package eolu.uchart;

import static java.lang.Math.max;
import static java.nio.file.Files.newInputStream;
import static java.util.stream.Collectors.joining;
import static java.util.stream.IntStream.range;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * A unicode chart generator. Jar it up then add this line to your .bashrc file:
 * 
 * alias chart="java -jar [path]/UnicodeChartGenerator.jar"
 * 
 * Then go to town piping output into it and getting nicely laid-out charts. Try
 * an 'ls -la | chart' as a quick test. Make sure your terminal is big enough
 * (it'll probably exceed 80 characters and it doesn't look right wrapped). You
 * can also build your own charts from stdin, just insert a blank line to
 * terminate reading and generate the chart.
 * 
 * Usage: chart [-h] [-d delimiter]... [FILE]
 */
public class UnicodeChartGenerator implements Iterable<String> {
    
    // Chart building block characters @formatter:off
    private static final String 
    
    // Frequent-use characters
    CHART_BLANKSPACE = " ",
    CHART_HORIZONTAL = "─",
    
    // Character used between each element
    CHART_VERTICAL   = "│",
    
    // Characters used on the top row only
    CHART_TOP_LEFT   = "┌",
    CHART_TOP_CENTER = "┬",
    CHART_TOP_RIGHT  = "┐",
    
    // Characters used between each intermediate row
    CHART_MID_LEFT   = "├",
    CHART_MID_CENTER = "┼",
    CHART_MID_RIGHT  = "┤",
    
    // Characters used on the bottom row only
    CHART_LOW_LEFT   = "└",
    CHART_LOW_CENTER = "┴",
    CHART_LOW_RIGHT  = "┘",
    
    // Bold or semi-bold characters used for the header
    BOLD_HORIZONTAL  = "━",
    BOLD_VERTICAL    = "┃",
    BOLD_TOP_LEFT    = "┏",
    BOLD_TOP_RIGHT   = "┓",
    BOLD_TOP_CENTER  = "┳",
    BOLD_MID_LEFT    = "┡",
    BOLD_MID_RIGHT   = "┩",
    BOLD_MID_CENTER  = "╇";

    // The default delimiter
    private static final String DEFAULT_DELIMITER = "\\s+";
    
    // Collectors
    private static final Collector<? super String, ?, String>
    TOP_ROW  = joining(CHART_TOP_CENTER, CHART_TOP_LEFT, CHART_TOP_RIGHT),
    TOP_BOLD = joining(BOLD_TOP_CENTER, BOLD_TOP_LEFT, BOLD_TOP_RIGHT),
    MID_ROW  = joining(CHART_VERTICAL, CHART_VERTICAL, CHART_VERTICAL),
    MID_BOLD = joining(BOLD_VERTICAL, BOLD_VERTICAL, BOLD_VERTICAL),
    SEP_ROW  = joining(CHART_MID_CENTER, CHART_MID_LEFT, CHART_MID_RIGHT),
    SEP_BOLD = joining(BOLD_MID_CENTER, BOLD_MID_LEFT, BOLD_MID_RIGHT),
    LOW_ROW  = joining(CHART_LOW_CENTER, CHART_LOW_LEFT, CHART_LOW_RIGHT);
    
    /**
     * Entry-point
     * 
     * @param args unused
     */
    public static void main(String... args) throws Exception
    {
        List<InputStream> fileInputStreams = new ArrayList<>();
        List<String> delimiters = new ArrayList<>();
        boolean header = false;

        // Handle args
        for (int i = 0; i < args.length; i++)
        {
            if (args[i].equals("-d") || args[i].equals("--delimiter"))
            {
                delimiters.add(args[++i]);
            }
            else if (args[i].startsWith("--delimiter="))
            {
                delimiters.add(args[i].replaceFirst("--delimiter=", ""));
            }
            else if (args[i].equals("-h") || args[i].equals("--header"))
            {
                header = true;
            }
            else
            {
                try
                {
                    fileInputStreams.add(newInputStream(Path.of(args[i])));
                }
                catch (IOException e)
                {
                    e.printStackTrace();
                }
            }
        }

        if (delimiters.isEmpty())
        {
            delimiters.add(DEFAULT_DELIMITER);
        }

        // Determine whether files were specified or standard input should be used.
        if (fileInputStreams.isEmpty())
        {
            new UnicodeChartGenerator(System.in, delimiters, header).forEach(System.out::println);
        }
        else for (InputStream in : fileInputStreams)
        {
            new UnicodeChartGenerator(in, delimiters, header).forEach(System.out::println);
        }
    }

    // Data from the parsed input
    private final List<Integer> colWidths = new ArrayList<>();
    private final List<String> chart = new ArrayList<>();
    private final boolean includeHeader;

    /**
     * Constructor. Generates a chart.
     * 
     * @param source The InputStream to read from.
     * @param delimiters The list of delimiters to use in element splitting.
     */
    public UnicodeChartGenerator(InputStream source, List<String> delimiters, boolean header)
    {
        includeHeader = header;
        List<String[]> parsed = parse(source, delimiters.stream().collect(Collectors.joining("|")));
        if (!parsed.isEmpty()) {
            generateChart(parsed);
        }
    }

    /**
     * Parse input
     * 
     * @param The source input stream.
     */
    private List<String[]> parse(InputStream source, String delimiter)
    {
        return new BufferedReader(new InputStreamReader(source))
            .lines()
            .takeWhile(s -> !s.isEmpty())
            .map(line -> line.split(delimiter))
            .peek(tokens -> range(colWidths.size(), tokens.length).map(i -> 0).forEach(colWidths::add))
            .peek(tokens -> range(0, tokens.length).forEach(i -> colWidths.set(i, max(colWidths.get(i), tokens[i].length()))))
            .collect(Collectors.toList());
    }
    
    /**
     * Create the chart.
     * 
     * @param parseList This list of parsed text.
     * @return The chart, as a list of strings.
     */
    private void generateChart(List<String[]> parseList)
    {
        // Create the top
        final Collector<? super String, ?, String> topCollector = includeHeader ? TOP_BOLD : TOP_ROW;
        chart.add(range(0, colWidths.size()).map(i -> colWidths.get(i))
                .mapToObj(i -> (includeHeader ? BOLD_HORIZONTAL : CHART_HORIZONTAL).repeat(i + 2))
                .collect(topCollector));

        // Create the separator row
        final String separatorRow = range(0, colWidths.size()).map(i -> colWidths.get(i))
                .mapToObj(i -> CHART_HORIZONTAL.repeat(i + 2))
                .collect(SEP_ROW);

        // Generate the top row. If it's a header, use bold
        chart.add(generateRow(parseList.get(0), includeHeader ? MID_BOLD : MID_ROW));
        chart.add(!includeHeader ? separatorRow : range(0, colWidths.size())
                                                 .map(i -> colWidths.get(i))
                                                 .mapToObj(i -> BOLD_HORIZONTAL.repeat(i + 2))
                                                 .collect(SEP_BOLD));
        
        // Generate every row.
        range(1, parseList.size()).mapToObj(parseList::get)
                                  .map(tokens -> generateRow(tokens, MID_ROW))
                                  .flatMap(row -> Stream.of(separatorRow, row))
                                  .skip(1)
                                  .forEachOrdered(chart::add);

        // Create the bottom
        chart.add(range(0, colWidths.size()).map(i -> colWidths.get(i))
                                            .mapToObj(i -> CHART_HORIZONTAL.repeat(i + 2))
                                            .collect(LOW_ROW));
    }

    /**
     * @brief Generate a single row from an array of tokens.
     * @param tokens The tokens containing the elements to fill the row with.
     * @return The generated row.
     */
    private String generateRow(String[] tokens, Collector<? super String, ?, String> collector)
    {
        return IntStream.range(0, colWidths.size())
                        .mapToObj(i -> generateCell(i, tokens))
                        .collect(collector);
    }

    /**
     * @brief Generate a single cell.
     * @param index The column index of the cell to generate.
     * @param tokens The list of tokens for the given row.
     * @return The generated cell, as a String.
     */
    private String generateCell(int index, String[] tokens)
    {
        if (index < tokens.length) 
        {
            final int cellPadding = 2 + (colWidths.get(index) - tokens[index].length());
            return CHART_BLANKSPACE.repeat(cellPadding / 2) 
                    + tokens[index]
                    + CHART_BLANKSPACE.repeat(cellPadding / 2 + cellPadding % 2);
        } 
        else 
        {
            return CHART_BLANKSPACE.repeat(colWidths.get(index) + 2);
        }
    }

    /**
     * @return This entire chart as a single string.
     */
    @Override
    public String toString()
    {
        return chart.stream().collect(Collectors.joining("\n"));
    }

    /**
     * @return An iterator containing the lines of the generated chart.
     */
    @Override
    public Iterator<String> iterator() {
        return chart.iterator();
    }

}
