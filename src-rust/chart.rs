/// Chart building block characters
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
const BOLD_FONT : Font = Font{ blankspace: ' ', horizontal: '━', vertical:   '┃',
                               top_left:   '┏', mid_left:   '┡', low_left:   '┗',
                               top_center: '┳', mid_center: '╇', low_center: '┻',
                               top_right:  '┓', mid_right:  '┩', low_right:  '┛' };

/// Generate a chart
pub fn gen_chart(input: Vec<Vec<&str>>, sizes: Vec<usize>, include_header: bool) -> String
{
    // Allocate a string on the heap to contain output
    let mut output = String::new();

    gen_top(&sizes, include_header, &mut output);
    output.push('\n');
    for i in 0..input.len()
    {
        gen_row(input.get(i).unwrap(), &sizes, i == 0 && include_header, &mut output);
        output.push('\n');
        if i != input.len() - 1
        {
            gen_separator(&sizes, i == 0 && include_header, &mut output);
            output.push('\n');
        }
    }
    gen_bottom(&sizes, include_header && input.is_empty(), &mut output);
    output.push('\n');
    return output;
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
fn gen_separator<'a>(sizes: &Vec<usize>, bold: bool, output: &'a mut String)
{
    let font = if bold { BOLD_FONT } else { BASE_FONT };

    // Create the row
    output.push(font.mid_left);
    for col in sizes
    {
        for _ in 0..*col { output.push(font.horizontal) }
        output.push(font.mid_center)
    }
    output.pop();
    output.push(font.mid_right);
}

/// Generate a row with text
fn gen_row<'a>(input: &Vec<&str>, cols: &Vec<usize>, bold: bool, output: &'a mut String)
{
    let font = if bold { BOLD_FONT } else { BASE_FONT };

    // Create the row
    output.push(font.vertical);
    for i in 0..input.len()
    {
        gen_cell(input.get(i).unwrap(), cols.get(i).unwrap(), output);
        output.push(font.vertical);
    }
    output.pop();
    output.push(font.vertical);
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