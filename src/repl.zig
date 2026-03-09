const std = @import("std");
const fs = std.fs;
const Io = std.Io;
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const heap = std.heap;
const ArenaAllocator = heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const posix = std.posix;

const Interpreter = @import("interpreter.zig").Interpreter;

const prompt_tmpl = "lang:{}> ";

pub fn readEvalPrintLoop(
    allocator: Allocator,
    interpreter: *Interpreter,
    stdin: *Io.Reader,
    stdout: *Io.Writer,
    stderr: *Io.Writer,
) ReplError!void {
    var n: usize = 1;
    var prompt_buf = [_]u8{0} ** 256;
    while (true) {
        defer n += 1;

        const prompt = try fmt.bufPrint(&prompt_buf, prompt_tmpl, .{n});

        const input = try edit(allocator, prompt, stdin, stdout);
        defer allocator.free(input);

        if (input.len == 0) {
            try stdout.writeByte('\n');
            continue;
        }

        if (interpreter.run(input)) |result|
            try stdout.print("\n{f}\n", .{result})
        else |err|
            try stderr.print("{}\n", .{err});

        try stdout.flush();
        try stderr.flush();
    }
}

fn edit(allocator: Allocator, prompt: []const u8, in: *Io.Reader, out: *Io.Writer) ReplError![]const u8 {
    const orig = try enableRawMode(posix.STDIN_FILENO);
    defer disableRawMode(posix.STDIN_FILENO, orig);

    var line = Line{};
    errdefer line.chars.deinit(allocator);

    try out.writeAll(prompt);
    try out.flush();

    while (true) {
        const char = try in.takeByte();

        // zig fmt: off
        switch (char) {
            '\r' => return try line.chars.toOwnedSlice(allocator),
            1 => line.moveStart(),           // Ctrl-A
            2 => line.moveLeft(),            // Ctrl-B
            3 => line.clear(),               // Ctrl-C
            4 => {                           // Ctrl-D
                if (line.chars.items.len == 0) return ReplError.EndOfInput;
                line.deleteForward();
            },
            5 => line.moveEnd(),             // Ctrl-E
            6 => line.moveRight(),           // Ctrl-F
            11 => line.killEnd(),            // Ctrl-K
            12 => {                          // Ctrl-L
                try out.writeAll("\x1b[2J\x1b[H");
            },
            20 => line.transpose(),          // Ctrl-T
            21 => line.killStart(),          // Ctrl-U
            23 => line.killWordBackward(),   // Ctrl-W
            127 => line.deleteBackward(),    // Backspace
            27 => {                          // Escape sequence
                const next = try in.takeByte();
                switch (next) {
                    'b' => line.moveWordBackward(),  // M-b
                    'd' => line.killWordForward(),   // M-d
                    'f' => line.moveWordForward(),   // M-f
                    '[' => switch (try in.takeByte()) {
                        'C' => line.moveRight(),     // Right arrow
                        'D' => line.moveLeft(),      // Left arrow
                        'H' => line.moveStart(),     // Home
                        'F' => line.moveEnd(),       // End
                        '3' => {                     // Delete key
                            if (try in.takeByte() == '~')
                                line.deleteForward();
                        },
                        else => {},
                    },
                    else => {},
                }
            },
            32...126 => try line.insert(allocator, char),
            else => {},
        }
        // zig fmt: on

        try out.writeAll("\r");
        try out.writeAll(prompt);
        try out.writeAll(line.render());
        try out.writeAll("\x1b[K");
        const cursor = line.pos + prompt.len;
        if (cursor == 0)
            try out.writeAll("\r")
        else
            try out.print("\r\x1b[{d}C", .{cursor});
        try out.flush();
    }
}

fn enableRawMode(fd: posix.fd_t) !posix.termios {
    var raw = try posix.tcgetattr(fd);
    const original = raw;

    raw.lflag.ECHO = false;
    raw.lflag.ICANON = false;
    raw.lflag.ISIG = false;
    raw.lflag.IEXTEN = false;

    raw.iflag.IXON = false;
    raw.iflag.ICRNL = false;

    try posix.tcsetattr(fd, .FLUSH, raw);
    return original;
}

fn disableRawMode(fd: posix.fd_t, original: posix.termios) void {
    posix.tcsetattr(fd, .FLUSH, original) catch {};
}

pub const ReplError = error{EndOfInput} || Allocator.Error ||
    Io.Writer.Error || Io.Reader.Error ||
    posix.TermiosGetError || posix.TermiosSetError ||
    fmt.BufPrintError;

const Line = struct {
    chars: ArrayList(u8) = .{},
    pos: usize = 0,

    fn insert(self: *Line, allocator: Allocator, char: u8) Allocator.Error!void {
        try self.chars.insert(allocator, self.pos, char);
        self.pos += 1;
    }

    fn deleteBackward(self: *Line) void {
        if (self.pos == 0) return;
        _ = self.chars.orderedRemove(self.pos - 1);
        self.pos -= 1;
    }

    fn deleteForward(self: *Line) void {
        if (self.pos >= self.chars.items.len) return;
        _ = self.chars.orderedRemove(self.pos);
    }

    fn moveRight(self: *Line) void {
        if (self.pos >= self.chars.items.len) return;
        self.pos += 1;
    }

    fn moveLeft(self: *Line) void {
        if (self.pos == 0) return;
        self.pos -= 1;
    }

    fn moveWordForward(self: *Line) void {
        const chars = self.chars.items;
        while (self.pos < chars.len and !isWordChar(chars[self.pos])) self.pos += 1;
        while (self.pos < chars.len and isWordChar(chars[self.pos])) self.pos += 1;
    }

    fn moveWordBackward(self: *Line) void {
        const chars = self.chars.items;
        while (self.pos > 0 and !isWordChar(chars[self.pos - 1])) self.pos -= 1;
        while (self.pos > 0 and isWordChar(chars[self.pos - 1])) self.pos -= 1;
    }

    fn moveStart(self: *Line) void {
        self.pos = 0;
    }

    fn moveEnd(self: *Line) void {
        self.pos = self.chars.items.len;
    }

    fn killEnd(self: *Line) void {
        if (self.chars.items.len == 0) return;
        self.chars.shrinkRetainingCapacity(self.pos);
    }

    fn killStart(self: *Line) void {
        if (self.pos == 0) return;
        var i: usize = 0;
        while (i < self.pos) : (i += 1)
            _ = self.chars.orderedRemove(0);
        self.pos = 0;
    }

    fn killWordBackward(self: *Line) void {
        const start = self.pos;
        const chars = self.chars.items;
        while (self.pos > 0 and !isWordChar(chars[self.pos - 1])) self.pos -= 1;
        while (self.pos > 0 and isWordChar(chars[self.pos - 1])) self.pos -= 1;
        var i: usize = 0;
        while (i < start - self.pos) : (i += 1)
            _ = self.chars.orderedRemove(self.pos);
    }

    fn killWordForward(self: *Line) void {
        const start = self.pos;
        const chars = self.chars.items;
        var end = start;
        while (end < chars.len and !isWordChar(chars[end])) end += 1;
        while (end < chars.len and isWordChar(chars[end])) end += 1;
        var i: usize = 0;
        while (i < end - start) : (i += 1)
            _ = self.chars.orderedRemove(self.pos);
    }

    fn transpose(self: *Line) void {
        if (self.chars.items.len < 2 or self.pos == 0) return;
        const p = if (self.pos == self.chars.items.len) self.pos - 1 else self.pos;
        const tmp = self.chars.items[p - 1];
        self.chars.items[p - 1] = self.chars.items[p];
        self.chars.items[p] = tmp;
        if (self.pos < self.chars.items.len) self.pos += 1;
    }

    fn clear(self: *Line) void {
        self.chars.clearRetainingCapacity();
        self.pos = 0;
    }

    fn render(self: *Line) []const u8 {
        return self.chars.items;
    }
};

fn isWordChar(char: u8) bool {
    return switch (char) {
        'a'...'z', 'A'...'Z', '0'...'0', '!', '?' => true,
        else => false,
    };
}
