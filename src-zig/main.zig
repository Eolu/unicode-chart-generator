const std = @import("std");
const chart = @import("chart.zig");
const process = std.process;
const unicode = std.unicode;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const eql = std.mem.eql;
const max = std.math.max;

const ArgError = error
{
    MultipleInputFiles,
    MissingDelim
};

/// Whether to read from standard input or a particular file
const InputType = union(enum)
{
    stdin: void,
    file: []const u8,
};

/// Information derived from parsed arguments
const InputInfo = struct
{
    // Whether or not the top row should be bolded
    include_header: bool,
    // A list of valid delimiters
    delimiters: [][]const u8,
    // See InputType union
    in_type: InputType,
    
    /// Free all interior memory
    fn free(self: *InputInfo, allocator: *const Allocator) void
    {
        for (self.delimiters) |delim|
        {
            allocator.free(delim);
        }
        switch (self.in_type)
        {
            .stdin => |_| {},
            .file => |file|
            {
                allocator.free(file);
            }
        }
        allocator.free(self.delimiters);
    }
};

/// Entry-point
pub fn main() !void 
{
    // Init allocator
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Init and parse
    var input_info = try handleArgs(&allocator);
    var input = try readInput(&allocator, &input_info.in_type);
    var tokens = try tokenize(&allocator, &input_info.delimiters, &input);

    // Gen chart
    var widths = try getColumnWidths(&allocator, &tokens);
    const result = try chart.genChart(&allocator, &tokens, &widths, input_info.include_header);

    // Output resulting chart
    try std.io.getStdOut().writer().print("{s}", .{result});
}

fn getColumnWidths(allocator: *const Allocator, tokens: *[][][]const u8) ![]const u8
{
    var widths = try allocator.alloc(u8, tokens.*[0].len);
    for (widths) |_, i|
    {
        widths[i] = 0;
    }
    for (tokens.*) |line|
    {
        for (line) |tok, i|
        {
            widths[i] = max(widths[i], @intCast(u8, tok.len) + 2);
        }
    }
    return widths;
}

fn tokenize(allocator: *const Allocator, delimiters: *[][]const u8, input: *[][]const u8) ![][][]const u8
{
    var tokens = ArrayList([][]const u8).init(allocator.*);
    for (input.*) |string|
    {
        var sub_tokens = ArrayList([]const u8).init(allocator.*);
        var first_delim = true;
        for (delimiters.*) |delim|
        {
            if (first_delim)
            {
                first_delim = false;
                var iter = std.mem.split(u8, string, delim);
                while (iter.next()) |tok|
                {
                    try sub_tokens.append(tok);
                }
            }
            else
            {
                var line = sub_tokens.toOwnedSlice();
                for (line) |partial_line|
                {
                    var iter = std.mem.split(u8, partial_line, delim);
                    while (iter.next()) |tok|
                    {
                        try sub_tokens.append(tok);
                    }
                }
                allocator.free(line);
            }
        }
        try tokens.append(sub_tokens.toOwnedSlice());
    }
    return tokens.toOwnedSlice();
}

fn readInput(allocator: *const Allocator, inputType: *InputType) ![][]const u8
{
    var input = ArrayList([]const u8).init(allocator.*);
    switch (inputType.*)
    {
        .stdin => |_| 
        {
            var buffer = try allocator.alloc(u8, 1024);
            const stdin = std.io.getStdIn().reader();
            while (try stdin.readUntilDelimiterOrEof(buffer, '\n')) |line|
            {
                try input.append(try allocator.dupe(u8, line));
            }
            allocator.free(buffer);
        },
        .file => |path|
        {
            var buffer = try allocator.alloc(u8, 1024);
            var file = try std.fs.openFileAbsolute(path, .{});
            defer file.close();

            var buf_reader = std.io.bufferedReader(file.reader());
            var in_stream = buf_reader.reader();

            while (try in_stream.readUntilDelimiterOrEof(buffer, '\n')) |line|
            {
                try input.append(try allocator.dupe(u8, line));
            }
            allocator.free(buffer);
        }
    }
    return input.toOwnedSlice();
}

fn handleArgs(allocator: *const Allocator) !InputInfo
{
    var expect_delim = false;
    var args = process.args();

    var delimiters = ArrayList([]const u8).init(allocator.*);

    // Init return type
    var input_info = InputInfo
    {
        .include_header = false,
        .delimiters = undefined,
        .in_type = InputType { .stdin = undefined }
    };

    // Discard initial arg
    if (try args.next(allocator.*)) |arg|
    {
        allocator.free(arg);
    }

    // Iterate through each arg
    while (try args.next(allocator.*)) |arg| 
    {
        if (expect_delim)
        {
            expect_delim = false;
            try delimiters.append(arg);
        }
        else
        {
            if (eql(u8, "-h", arg))
            {
                input_info.include_header = true;
                allocator.free(arg);
            }
            else if (eql(u8, "-d", arg))
            {
                expect_delim = true;
                allocator.free(arg);
            }
            else
            {
                switch (input_info.in_type)
                {
                    .stdin => |_| 
                    {
                        input_info.in_type = InputType { .file = arg };
                    },
                    .file => |_|
                    {
                        allocator.free(arg);
                        input_info.free(allocator);
                        return ArgError.MultipleInputFiles;
                    }
                }
            }
        }
    }

    input_info.delimiters = delimiters.toOwnedSlice();

    if (expect_delim)
    {
        input_info.free(allocator);
        return ArgError.MissingDelim;
    }

    return input_info;
}