const std = @import("std");
const tst = std.testing;
const mem = std.mem;
const ascii = std.ascii;

pub const Scanner = struct {
    input: []const u8,
    start: usize = 0,
    pos: usize = 0,
    line: u32 = 1,

    pub fn scanToken(self: *Scanner) ScanError!?Token {
        return while (!self.endOfFile()) {
            self.start = self.pos;
            defer self.pos += 1;

            switch (self.charAtPos()) {
                ' ', '\t' => {},
                '\n' => self.line += 1,
                '(' => break self.createToken(.lparen),
                ')' => break self.createToken(.rparen),
                '"' => break try self.scanString(),
                '0'...'9' => break self.scanNumber(),
                '+', '-', '.' => {
                    const next = self.peek() orelse return self.scanSymbol();
                    return if (ascii.isDigit(next) or next == '.') self.scanNumber() else self.scanSymbol();
                },
                else => |char| {
                    if (!isSymbolStartChar(char))
                        return ScanError.UnsupportedCharacter;

                    var tk = self.scanSymbol();

                    // Check if the symbol is a literal (e.g. nil, true, false)
                    if (tag_by_literal.get(tk.lexeme)) |tag|
                        tk.tag = tag;

                    return tk;
                },
            }
        } else null;
    }

    fn endOfFile(self: Scanner) bool {
        return self.pos >= self.input.len;
    }

    fn charAtPos(self: Scanner) u8 {
        return self.input[self.pos];
    }

    fn peek(self: Scanner) ?u8 {
        return if (self.pos + 1 < self.input.len)
            self.input[self.pos + 1]
        else
            null;
    }

    fn createToken(self: *Scanner, tag: Token.Tag) Token {
        return Token{
            .tag = tag,
            .lexeme = self.input[self.start .. self.pos + 1],
            .line = self.line,
        };
    }

    fn scanString(self: *Scanner) error{UnterminatedString}!Token {
        while (self.peek()) |next| {
            if (next == '"') break;
            if (next == '\n') self.line += 1;
            self.pos += 1;
        } else return ScanError.UnterminatedString;

        self.pos += 1; // consume trailing "

        return self.createToken(.string);
    }

    fn scanNumber(self: *Scanner) Token {
        if (isSign(self.charAtPos())) self.pos += 1;

        while (self.peek()) |next| {
            if (!ascii.isDigit(next)) break;
            self.pos += 1;
        }

        if (self.peek() == '.') {
            self.pos += 1;
            while (self.peek()) |next| {
                if (!ascii.isDigit(next)) break;
                self.pos += 1;
            }
        }

        return self.createToken(.number);
    }

    fn scanSymbol(self: *Scanner) Token {
        while (self.peek()) |next| {
            if (!isSymbolChar(next)) break;
            self.pos += 1;
        }
        return self.createToken(.symbol);
    }
};

const tag_by_literal = std.StaticStringMap(Token.Tag).initComptime(
    &.{
        .{ "nil", .nil },
        .{ "true", .boolean_true },
        .{ "false", .boolean_false },
    },
);

pub const ScanError = error{
    UnterminatedString,
    UnsupportedCharacter,
};

pub const Token = struct {
    const Tag = enum {
        lparen,
        rparen,
        string,
        number,
        symbol,
        nil,
        boolean_true,
        boolean_false,
    };
    tag: Tag,
    lexeme: []const u8,
    line: u32,
};

fn isSign(char: u8) bool {
    return char == '+' or char == '-';
}

fn isSymbolStartChar(char: u8) bool {
    return switch (char) {
        'a'...'z', 'A'...'Z', '*', '+', '!', '-', '_', '?', '<', '>', '=', '/', '&', '%', '$', '.' => true,
        else => false,
    };
}

fn isSymbolChar(char: u8) bool {
    return isSymbolStartChar(char) or switch (char) {
        '0'...'9' => true,
        else => false,
    };
}

fn expectToken(input: []const u8, expected: Token) !void {
    var scanner = Scanner{ .input = input };

    const actual = try scanner.scanToken() orelse {
        std.debug.print(
            "Expected token {}, received null.\n",
            .{expected.tag},
        );
        return error.TestExpectedEqual;
    };

    if (expected.tag != actual.tag or
        !mem.eql(u8, expected.lexeme, actual.lexeme) or
        expected.line != actual.line)
    {
        std.debug.print(
            \\Scanner returned wrong token.
            \\  expected: {} => {s}
            \\  actual:   {} => {s}
            \\
        , .{ expected, expected.lexeme, actual, actual.lexeme });
        return error.TestExpectedEqual;
    }
}

fn expectScanError(input: []const u8, expected: ScanError) !void {
    var scanner = Scanner{ .input = input };
    try tst.expectError(expected, scanner.scanToken());
}

test "Scan left parenthesis" {
    try expectToken(
        "(",
        Token{ .tag = .lparen, .lexeme = "(", .line = 1 },
    );
}

test "Scan left parenthesis with leading whitespace" {
    try expectToken(
        "    (",
        Token{ .tag = .lparen, .lexeme = "(", .line = 1 },
    );
}

test "Scan left parenthesis on third line" {
    try expectToken(
        "\n\n(",
        Token{ .tag = .lparen, .lexeme = "(", .line = 3 },
    );
}

test "Scan right parenthesis" {
    try expectToken(
        ")",
        Token{ .tag = .rparen, .lexeme = ")", .line = 1 },
    );
}

test "Scan string" {
    try expectToken(
        "\"hello\"",
        Token{ .tag = .string, .lexeme = "\"hello\"", .line = 1 },
    );
}

test "Scan multiline string increments line count" {
    const input =
        \\"Hello
        \\world
        \\!"
    ;

    var scanner = Scanner{ .input = input };
    _ = try scanner.scanToken();

    try tst.expectEqual(3, scanner.line);
}

test "Scan unterminated string" {
    try expectScanError(
        "\"hello",
        ScanError.UnterminatedString,
    );
}

test "Scan integer number" {
    const cases = [_][]const u8{ "123", "+123", "-123" };
    for (cases) |case| {
        try expectToken(
            case,
            Token{ .tag = .number, .lexeme = case, .line = 1 },
        );
    }
}

test "Scan float number" {
    const cases = [_][]const u8{ "3.14", "-3.14", "+3.14", "10.", "+10.", "-10.", ".5", "+.5", "-.5" };
    for (cases) |case| {
        try expectToken(
            case,
            Token{ .tag = .number, .lexeme = case, .line = 1 },
        );
    }
}

test "Scan symbol" {
    const cases = [_][]const u8{ "map", "reduce", "nil?", "+" };
    for (cases) |case| {
        try expectToken(
            case,
            Token{ .tag = .symbol, .lexeme = case, .line = 1 },
        );
    }
}

test "Scan nil" {
    try expectToken(
        "nil",
        Token{ .tag = .nil, .lexeme = "nil", .line = 1 },
    );
}

test "Scan true" {
    try expectToken(
        "true",
        Token{ .tag = .boolean_true, .lexeme = "true", .line = 1 },
    );
}

test "Scan false" {
    try expectToken(
        "false",
        Token{ .tag = .boolean_false, .lexeme = "false", .line = 1 },
    );
}

test "Scan multiple tokens" {
    var scanner = Scanner{ .input = "(+ 1 x)" };

    const expected = [_]Token{
        Token{ .tag = .lparen, .lexeme = "(", .line = 1 },
        Token{ .tag = .symbol, .lexeme = "+", .line = 1 },
        Token{ .tag = .number, .lexeme = "1", .line = 1 },
        Token{ .tag = .symbol, .lexeme = "x", .line = 1 },
        Token{ .tag = .rparen, .lexeme = ")", .line = 1 },
    };

    for (expected) |tk|
        try tst.expectEqualDeep(tk, try scanner.scanToken());

    try tst.expectEqualDeep(null, try scanner.scanToken());
}

test "Scan empty input" {
    var scanner = Scanner{ .input = "" };
    try tst.expectEqualDeep(null, try scanner.scanToken());
}
