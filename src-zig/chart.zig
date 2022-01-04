const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

/// Chart building block characters
const Font = struct
{
    blankspace: u8,         horizontal: []const u8, vertical:   []const u8,
    top_left:   []const u8, mid_left:   []const u8, low_left:   []const u8,
    top_center: []const u8, mid_center: []const u8, low_center: []const u8,
    top_right:  []const u8, mid_right:  []const u8, low_right:  []const u8
};

/// The basic chart font
const BASE_FONT = Font { .blankspace  = ' ', .horizontal  = "─", .vertical   = "│",
                         .top_left    = "┌", .mid_left    = "├", .low_left   = "└",
                         .top_center  = "┬", .mid_center  = "┼", .low_center = "┴",
                         .top_right   = "┐", .mid_right   = "┤", .low_right  = "┘" };

/// Bold chart font
const BOLD_FONT = Font { .blankspace  = ' ', .horizontal  = "━", .vertical    = "┃",
                         .top_left    = "┏", .mid_left    = "┡", .low_left    = "┗",
                         .top_center  = "┳", .mid_center  = "╇", .low_center  = "┻",
                         .top_right   = "┓", .mid_right   = "┩", .low_right   = "┛" };

pub fn genChart(allocator: *const Allocator, input: *[][][]const u8, widths: *[]const u8, include_header: bool) ![]const u8
{
    var output = ArrayList(u8).init(allocator.*); // Capacity: widths.*.len * input.*[0].len
    
    try genTop(widths, include_header, &output);
    try output.append('\n');
    for (input.*) |in, i|
    {
        try genRow(&in, widths, i == 0 and include_header, &output);
        try output.append('\n');
        if (i != input.len - 1)
        {
            try genSeparator(widths, i == 0 and include_header, &output);
            try output.append('\n');
        }
    }
    try genBottom(widths, include_header and input.len == 0, &output);
    try output.append('\n');
    return output.toOwnedSlice();
}

fn genTop(widths: *[]const u8, bold: bool, output: *ArrayList(u8)) !void
{
    const font = if (bold) BOLD_FONT else BASE_FONT;

    // Create the row
    try output.appendSlice(font.top_left);
    
    for (widths.*) |col|
    {
        var i: usize = 0;
        while (i < col) 
        {
            try output.appendSlice(font.horizontal);
            i += 1;
        }
        try output.appendSlice(font.top_center);
    }
    _ = output.shrinkAndFree(output.items.len - font.top_center.len);
    try output.appendSlice(font.top_right);
}

fn genBottom(widths: *[]const u8, bold: bool, output: *ArrayList(u8)) !void
{
    const font = if (bold) BOLD_FONT else BASE_FONT;

    // Create the row
    try output.appendSlice(font.low_left);
    for (widths.*) |col|
    {
        var i: usize = 0;
        while (i < col) 
        {
            try output.appendSlice(font.horizontal);
            i += 1;
        }
        try output.appendSlice(font.low_center);
    }
    _ = output.shrinkAndFree(output.items.len - font.low_center.len);
    try output.appendSlice(font.low_right);
}

fn genSeparator(widths: *[]const u8, bold: bool, output: *ArrayList(u8)) !void
{
    const font = if (bold) BOLD_FONT else BASE_FONT;

    // Create the row
    try output.appendSlice(font.mid_left);
    for (widths.*) |col|
    {
        var i: usize = 0;
        while (i < col) 
        {
            try output.appendSlice(font.horizontal);
            i += 1;
        }
        try output.appendSlice(font.mid_center);
    }
    _ = output.shrinkAndFree(output.items.len - font.mid_center.len);
    try output.appendSlice(font.mid_right);
}

fn genRow(input: *const [][]const u8, widths: *[]const u8, bold: bool, output: *ArrayList(u8)) !void
{
    const EMPTY: []const u8 = "";
    const font = if (bold) BOLD_FONT else BASE_FONT;

    // Create the row
    try output.appendSlice(font.vertical);
    for (widths.*) |_, i|
    {
        try genCell(if (i < input.*.len) &input.*[i] else &EMPTY, widths.*[i], output);
        try output.appendSlice(font.vertical);
    }
    _ = output.shrinkAndFree(output.items.len - font.vertical.len);
    try output.appendSlice(font.vertical);
}

fn genCell(input: *const []const u8, col: usize, output: *ArrayList(u8)) !void
{
    const pads = col - input.len;
    const half_pads = pads / 2;
    var i: usize = 0;
    while (i < half_pads) 
    {
        try output.append(BASE_FONT.blankspace);
        i += 1;
    }
    i = 0;
    try output.appendSlice(input.*);
    while (i < (half_pads + (pads % 2)) )
    {
        try output.append(BASE_FONT.blankspace);
        i += 1;
    }
}
