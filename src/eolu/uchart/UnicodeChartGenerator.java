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
 * Usage: chart [-d delimiter]... [FILE]
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

    // The default delimiter
    DEFAULT_DELIMITER = "\\s+";

    /**
     * Entry-point
     * 
     * @param args unused
     */
    public static void main(String... args) throws Exception
    {
        List<InputStream> fileInputStreams = new ArrayList<>();
        List<String> delimiters = new ArrayList<>();

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
            new UnicodeChartGenerator(System.in, delimiters).forEach(System.out::println);
        }
        else for (InputStream in : fileInputStreams)
        {
            new UnicodeChartGenerator(in, delimiters).forEach(System.out::println);
        }
    }

    // Data from the parsed input
    private final List<Integer> colWidths = new ArrayList<>();
    private final List<String> chart = new ArrayList<>();

    /**
     * Constructor. Generates a chart.
     * 
     * @param source The InputStream to read from.
     * @param delimiters The list of delimiters to use in element splitting.
     */
    public UnicodeChartGenerator(InputStream source, List<String> delimiters)
    {
        generateChart(parse(source, delimiters.stream().collect(Collectors.joining("|"))));
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
        chart.add(range(0, colWidths.size()).map(i -> colWidths.get(i))
                .mapToObj(i -> CHART_HORIZONTAL.repeat(i + 2))
                .collect(joining(CHART_TOP_CENTER, CHART_TOP_LEFT, CHART_TOP_RIGHT)));

        // Create the separator row
        final String separatorRow = range(0, colWidths.size()).map(i -> colWidths.get(i))
                .mapToObj(i -> CHART_HORIZONTAL.repeat(i + 2))
                .collect(joining(CHART_MID_CENTER, CHART_MID_LEFT, CHART_MID_RIGHT));

        // Generate every row
        range(0, parseList.size()).mapToObj(parseList::get).map(this::generateRow)
                .flatMap(row -> Stream.of(separatorRow, row)).skip(1).forEachOrdered(chart::add);

        // Create the bottom
        chart.add(range(0, colWidths.size()).map(i -> colWidths.get(i))
                .mapToObj(i -> CHART_HORIZONTAL.repeat(i + 2))
                .collect(joining(CHART_LOW_CENTER, CHART_LOW_LEFT, CHART_LOW_RIGHT)));
    }

    /**
     * @brief Generate a single row from an array of tokens.
     * @param tokens The tokens containing the elements to fill the row with.
     * @return The generated row.
     */
    private String generateRow(String[] tokens)
    {
        return IntStream.range(0, colWidths.size())
                        .mapToObj(i -> generateCell(i, tokens))
                        .collect(joining(CHART_VERTICAL, CHART_VERTICAL, CHART_VERTICAL));
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
