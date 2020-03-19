use std::io::{self, Read, Error, ErrorKind};
use std::env::{args};
use std::fs;
use std::cmp::{max};
use InputInfo::{StdIn,File};
use itertools::Itertools;
use crate::chart::gen_chart;

pub mod chart;

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

    // Shadow input with the tokenized version
    let input = tokenize(input.as_str(), delimiters);

    // Get the widths of each column
    let sizes = input.iter()
                     .map(|line| Box::new(line.iter().map(|s| s.len() + 2)) as Box<dyn Iterator<Item = usize>>)
                     .fold1(|acc, lengths| Box::new(acc.zip(lengths).map(|i| -> usize{max(i.0, i.1)})))
                     .unwrap()
                     .collect();

    // Generate chart and print output
    println!("{}", gen_chart(input, sizes, include_header));

    Ok(())
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

