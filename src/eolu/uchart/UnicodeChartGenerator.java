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
    BOLD_TOP_CENTER  = "┳",
    BOLD_TOP_RIGHT   = "┓",
    BOLD_MID_LEFT    = "┡",
    BOLD_MID_RIGHT   = "┩",
    BOLD_MID_CENTER  = "╇",
    BOLD_LOW_LEFT   = "┗",
    BOLD_LOW_CENTER = "┻",
    BOLD_LOW_RIGHT  = "┛";

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
    LOW_ROW  = joining(CHART_LOW_CENTER, CHART_LOW_LEFT, CHART_LOW_RIGHT),
    LOW_BOLD = joining(BOLD_LOW_LEFT, BOLD_LOW_CENTER, BOLD_LOW_RIGHT);
    
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
            gen(System.in, delimiters, header).forEach(System.out::println);
        }
        else for (InputStream in : fileInputStreams)
        {
            gen(in, delimiters, header).forEach(System.out::println);
        }
    }

    // Data from the parsed input
    private final List<Integer> colWidths = new ArrayList<>();
    private final List<String> chart = new ArrayList<>();
    private final boolean includeHeader;

    /**
     * Factory to generate a chart.
     * 
     * @param source The InputStream to read from.
     * @param delimiters The list of delimiters to use in element splitting.
     * @param header If true, will embolden the top row.
     */
    public static UnicodeChartGenerator gen(InputStream source, List<String> delimiters, boolean header)
    {
        return new UnicodeChartGenerator(source, delimiters, header);
    }
    
    /**
     * Constructor. Generates a chart.
     * 
     * @param source The InputStream to read from.
     * @param delimiters The list of delimiters to use in element splitting.
     * @param header If true, will embolden the top row.
     */
    private UnicodeChartGenerator(InputStream source, List<String> delimiters, boolean header)
    {
        includeHeader = header;
        List<String[]> parsed = parse(source, delimiters.stream().collect(Collectors.joining("|")));
        if (!parsed.isEmpty()) {
            genChart(parsed);
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
    private void genChart(List<String[]> parseList)
    {
        // Create the non-value rows
        final String topRow = genEdge(CHART_HORIZONTAL, TOP_ROW);
        final String topBold = includeHeader ? genEdge(BOLD_HORIZONTAL, TOP_BOLD) : null;
        final String separatorRow = genSeparator(CHART_HORIZONTAL, SEP_ROW);
        final String separatorBold = includeHeader ? genSeparator(BOLD_HORIZONTAL, SEP_BOLD) : null;
        final String bottomRow = genEdge(CHART_HORIZONTAL, LOW_ROW);
        final String bottomBold = includeHeader ? genEdge(BOLD_HORIZONTAL, LOW_BOLD) : null;
        
        // Create the top
        chart.add(includeHeader ? topBold : topRow);

        // Generate the top row. If it's a header, use bold
        chart.add(genValueRow(parseList.get(0), includeHeader ? MID_BOLD : MID_ROW));
        
        // Generate a separator row
        if (parseList.size() > 1)
        {
            chart.add(includeHeader ? separatorBold : separatorRow);
        }
        
        // Generate each value row
        for (int i = 1; i < parseList.size(); i++)
        {
            if (i > 1)
            {
                chart.add(separatorRow);
            }
            chart.add(genValueRow(parseList.get(i), MID_ROW));
        }

        // Create the bottom
        chart.add(parseList.size() < 2 && includeHeader ? bottomBold : bottomRow);
    }
    
    /**
     * @brief Generate a top or bottom row.
     * @param fill The character to fill empty space with.
     * @param collector Collector to define cell boundaries.
     * @return The generated row.
     */
    private String genEdge(String fill, Collector<? super String, ?, String> collector) {
        return IntStream.range(0, colWidths.size())
                        .map(i -> colWidths.get(i))
                        .mapToObj(i -> fill.repeat(i + 2))
                        .collect(collector);
    }
    
    /**
     * @brief Generate a single separator row.
     * @param fill The character to fill empty space with.
     * @param collector Collector to define cell boundaries.
     * @return The generated separator row.
     */
    private String genSeparator(String fill, Collector<? super String, ?, String> collector) {
        return IntStream.range(0, colWidths.size())
                        .map(i -> colWidths.get(i))
                        .mapToObj(i -> fill.repeat(i + 2))
                        .collect(collector);
    }

    /**
     * @brief Generate a single row from an array of tokens.
     * @param tokens The tokens containing the elements to fill the row with.
     * @param collector Collector to define cell boundaries.
     * @return The generated row.
     */
    private String genValueRow(String[] tokens, Collector<? super String, ?, String> collector)
    {
        return IntStream.range(0, colWidths.size())
                        .mapToObj(i -> genCell(i, tokens))
                        .collect(collector);
    }

    /**
     * @brief Generate a single cell.
     * @param index The column index of the cell to generate.
     * @param tokens The list of tokens for the given row.
     * @return The generated cell, as a String.
     */
    private String genCell(int index, String[] tokens)
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
    public Iterator<String> iterator() 
    {
        return chart.iterator();
    }
}
