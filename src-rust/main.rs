use std::io::{self, Read, Error, ErrorKind};
use std::env::{args};
use std::fs;
use std::cmp::{max};
use InputInfo::{StdIn,File};
use itertools::Itertools;

/// Chart building block characters
#[derive(Debug)]
struct Font
{
    blankspace: char, horizontal: char, vertical:   char,
    top_left:   char, mid_left:   char, low_left:   char,
    top_center: char, mid_center: char, low_center: char,
    top_right:  char, mid_right:  char, low_right:  char
}

/// The basic chart font
const BASE_FONT : Font = Font{ blankspace: ' ', horizontal: '─', vertical:   '│',
                               top_left:   '┌', mid_left:   '├', low_left:   '└',
                               top_center: '┬', mid_center: '┼', low_center: '┴',
                               top_right:  '┐', mid_right:  '┤', low_right:  '┘' };

/// Bold chart font
const BOLD_FONT : Font = Font{ blankspace: ' ', horizontal: '━', vertical:   '┏',
                               top_left:   '┏', mid_left:   '┡', low_left:   '┗',
                               top_center: '┳', mid_center: '╇', low_center: '┻',
                               top_right:  '┓', mid_right:  '┩', low_right:  '┛' };

const USAGE : &str = "usage: Usage: chart [-h] [-d delimiter]... [FILE]";

/// Result type created after parsing args.
enum InputInfo 
{
    /// # Args
    /// * bool True if a header is included, false otherwise.
    /// * Vec<String> The delimiters
    StdIn(bool, Vec<String>),
    /// # Args
    /// * bool True if a header is included, false otherwise.
    /// * Vec<String> The delimiters
    /// * file The file to read from.
    File(bool, Vec<String>, String)
}

// Useless function, but showing off the lifetime annotation ('a)
// fn split<'a>(string: &'a str, delimiters:  &'a str) -> impl Iterator<Item = &'a str>
// {
//     string.split(delimiters)
// }

/// Entry-point
fn main() -> io::Result<()>
{
    // Create structures from args
    let include_header: bool;
    let delimiters: Vec<String>;
    let input = match handle_args()?
    {
        StdIn(header, delims) => 
        {
            // Read stdin
            delimiters = delims;
            include_header = header;
            let mut result = String::new();
            io::stdin().lock().read_to_string(&mut result)?;
            result
        },
        File(header, delims, file) => 
        {
            // Read file
            delimiters = delims;
            include_header = header;
            fs::read_to_string(file)?
        }
    };
    println!("DELIMS: {:?}", delimiters);

    // Shadow input with the tokenized version
    let input = tokenize(input.as_str(), delimiters);

    // Get the widths of each column
    // TODO: This make a lot of vectors and is more inefficient than it should need to be
    let sizes = input.iter()
                     .map(|line| line.iter().map(|s| s.len() + 2).collect::<Vec<usize>>())
                     .fold1(|acc, lengths| acc.iter().zip(lengths).map(|i| -> usize{max(*i.0, i.1)}).collect())
                     .unwrap();

    // Allocate a string on the heap to contain output
    let mut output = String::new();

    // Generate chart
    gen_top(&sizes, include_header, &mut output);
    output.push('\n');
    for i in 0..input.len()
    {
        gen_row(input.get(i).unwrap(), &sizes, &mut output);
        output.push('\n');
        if i != input.len() - 1
        {
            gen_separator(&sizes, &mut output);
            output.push('\n');
        }
    }
    gen_bottom(&sizes, include_header && input.is_empty(), &mut output);
    output.push('\n');

    // Print output
    println!("{}", output);
    Ok(())
}

/// Generate the top row
fn gen_top<'a>(sizes: &Vec<usize>, bold: bool, output: &'a mut String)
{
    let font = if bold { BOLD_FONT } else { BASE_FONT };

    // Create the row
    output.push(font.top_left);
    for col in sizes
    {
        for _ in 0..*col { output.push(font.horizontal) }
        output.push(font.top_center)
    }
    output.pop();
    output.push(font.top_right);
}

/// Generate the bottom row
fn gen_bottom<'a>(sizes: &Vec<usize>, bold: bool, output: &'a mut String)
{
    let font = if bold { BOLD_FONT } else { BASE_FONT };

    // Create the row
    output.push(font.low_left);
    for col in sizes
    {
        for _ in 0..*col { output.push(font.horizontal) }
        output.push(font.low_center)
    }
    output.pop();
    output.push(font.low_right);
}

/// Generate a separator row
fn gen_separator<'a>(sizes: &Vec<usize>, output: &'a mut String)
{
    // Create the row
    output.push(BASE_FONT.mid_left);
    for col in sizes
    {
        for _ in 0..*col { output.push(BASE_FONT.horizontal) }
        output.push(BASE_FONT.mid_center)
    }
    output.pop();
    output.push(BASE_FONT.mid_right);
}

/// Generate a row with text
fn gen_row<'a>(input: &Vec<&str>, cols: &Vec<usize>, output: &'a mut String)
{
    output.push(BASE_FONT.vertical);
    for i in 0..input.len()
    {
        gen_cell(input.get(i).unwrap(), cols.get(i).unwrap(), output);
        output.push(BASE_FONT.vertical);
    }
    output.pop();
    output.push(BASE_FONT.vertical);
}

/// Generate a single cell. To be called from gen_row
fn gen_cell<'a>(input: &str, col: &usize, output: &'a mut String)
{
    let pads = col - input.len();
    let half_pads = pads / 2;
    for _ in 0..half_pads 
    { 
        output.push(BASE_FONT.blankspace) 
    }
    output.push_str(input);
    for _ in 0..(half_pads + (pads % 2)) 
    { 
        output.push(BASE_FONT.blankspace) 
    }
}

/// Convert an input string into tokens based on some delimiters
fn tokenize(parsed: &str, delimiters: Vec<String>) -> Vec<Vec<&str>>
{
    parsed.lines().map(|line| if delimiters.is_empty() 
    {
        line.split_whitespace().collect()
    } 
    else 
    {
        line.split(|c: char| delimiters.contains(&c.to_string())).collect()
    }).collect()
}

/// Handle args. Returns a file to parse
fn handle_args() -> Result<InputInfo, Error>
{
    // Keep track of delims
    let mut include_header : bool = false;
    let mut expect_delim : bool = false;
    let mut delimiters : Vec<String> = Vec::new();
    let mut file = String::new();

    // Handle args
    for arg in args().into_iter().skip(1)
    {
        if expect_delim
        {
            delimiters.push(arg);
            expect_delim = false
        }
        else 
        {
            match arg.as_str()
            {
                "-h" => include_header = true,
                "-d" => expect_delim = true,
                s    => 
                    if file.is_empty() 
                    { 
                        file.push_str(s) 
                    } 
                    else 
                    { 
                        return Err(Error::new(ErrorKind::InvalidInput, 
                            format!("{} {}", USAGE, "Multiple or invalid input files specified")))
                    }
            }
        }
    }

    // Error out on trailing delim
    if expect_delim
    {
        Err(Error::new(ErrorKind::InvalidInput, 
            format!("{} {}", USAGE, "Trailing -d argument with no value.")))
    }
    else if file.is_empty() 
    {
        Ok(StdIn(include_header, delimiters))
    }
    else 
    {
        Ok(File(include_header, delimiters, file))
    }
}

