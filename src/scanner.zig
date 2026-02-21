const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;
const tst = std.testing;

pub const Scanner = struct {
    input: []const u8,
    line: usize = 1,
    start: usize = 0,
    pos: usize = 0,

    pub fn init(input: []const u8) Scanner {
        return .{ .input = input };
    }

    pub fn readToken(self: *Scanner) ScanError!?Token {
        while (!self.endOfFile()) {
            self.start = self.pos;

            return switch (self.charAtPos()) {
                ' ', '\t', '\r' => {
                    self.consume();
                    continue;
                },
                '\n' => {
                    self.consume();
                    self.line += 1;
                    continue;
                },
                ';' => {
                    self.skipComment();
                    continue;
                },
                '(' => self.readLeftParenthesis(),
                ')' => self.readRightParenthesis(),
                '0'...'9', '+', '-' => self.readNumber() orelse self.readSymbol(),
                '"' => try self.readString(),
                'n' => self.readNil() orelse self.readSymbol(),
                'f' => self.readFalse() orelse self.readSymbol(),
                't' => self.readTrue() orelse self.readSymbol(),
                '\'' => self.readQuote(),
                '`' => self.readQuasiquote(),
                ',' => if (self.peek() == '@') self.readUnquoteSplicing() else self.readUnquote(),
                else => |char| {
                    if (isSymbolStartChar(char))
                        return self.readSymbol();

                    return ScanError.UnexpectedCharacter;
                },
            };
        }

        return null;
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

    fn consume(self: *Scanner) void {
        assert(!self.endOfFile());
        self.pos += 1;
    }

    fn consumeMany(self: *Scanner, n: usize) void {
        assert(self.pos + n <= self.input.len);
        self.pos += n;
    }

    fn consumeSlice(self: *Scanner, s: []const u8) bool {
        if (s.len > self.input.len - self.pos)
            return false;

        if (mem.eql(u8, s, self.input[self.pos .. self.pos + s.len])) {
            self.pos += s.len;
            return true;
        }

        return false;
    }

    fn consumeIf(self: *Scanner, pred: fn (u8) bool) bool {
        if (pred(self.charAtPos())) {
            self.consume();
            return true;
        }
        return false;
    }

    fn consumeWhile(self: *Scanner, pred: fn (u8) bool) usize {
        var n: usize = 0;
        while (!self.endOfFile() and pred(self.charAtPos())) {
            self.consume();
            n += 1;
        }
        return n;
    }

    fn createToken(self: *Scanner, tag: Token.Tag) Token {
        return .{
            .tag = tag,
            .lexeme = self.input[self.start..self.pos],
            .line = self.line,
        };
    }

    fn readLeftParenthesis(self: *Scanner) Token {
        self.consume();
        return self.createToken(.lparen);
    }

    fn readRightParenthesis(self: *Scanner) Token {
        self.consume();
        return self.createToken(.rparen);
    }

    fn readNumber(self: *Scanner) ?Token {
        if (isSign(self.charAtPos())) {
            const next = self.peek() orelse
                return null;

            if (!isDigit(next))
                return null;
        }

        _ = self.consumeIf(isSign);
        _ = self.consumeWhile(isDigit);

        if (!self.endOfFile() and self.charAtPos() == '.') {
            self.consume();
            _ = self.consumeWhile(isDigit);
        }

        return self.createToken(.number);
    }

    fn readSymbol(self: *Scanner) Token {
        _ = self.consumeIf(isSymbolStartChar);
        _ = self.consumeWhile(isSymbolChar);
        return self.createToken(.symbol);
    }

    fn readString(self: *Scanner) ScanError!Token {
        // Consume leading quote
        self.consume();

        while (!self.endOfFile() and self.charAtPos() != '"')
            self.consume();

        if (self.endOfFile())
            return ScanError.UnterminatedString;

        // Consume trailing quote
        self.consume();

        return self.createToken(.string);
    }

    fn readNil(self: *Scanner) ?Token {
        if (!self.consumeSlice("nil"))
            return null;

        if (!self.endOfFile() and isSymbolChar(self.charAtPos()))
            return null;

        return self.createToken(.nil);
    }

    fn readTrue(self: *Scanner) ?Token {
        if (!self.consumeSlice("true"))
            return null;

        if (!self.endOfFile() and isSymbolChar(self.charAtPos()))
            return null;

        return self.createToken(.boolean_true);
    }

    fn readFalse(self: *Scanner) ?Token {
        if (!self.consumeSlice("false"))
            return null;

        if (!self.endOfFile() and isSymbolChar(self.charAtPos()))
            return null;

        return self.createToken(.boolean_false);
    }

    fn readQuote(self: *Scanner) Token {
        self.consume();
        return self.createToken(.quote);
    }

    fn readQuasiquote(self: *Scanner) Token {
        self.consume();
        return self.createToken(.quasiquote);
    }

    fn readUnquote(self: *Scanner) Token {
        self.consume();
        return self.createToken(.unquote);
    }

    fn readUnquoteSplicing(self: *Scanner) Token {
        self.consumeMany(2);
        return self.createToken(.unquote_splicing);
    }

    fn skipComment(self: *Scanner) void {
        // Skip until end of line or end of file
        while (!self.endOfFile() and self.charAtPos() != '\n')
            self.consume();
    }
};

pub const ScanError = error{
    UnexpectedCharacter,
    UnterminatedString,
};

pub const Token = struct {
    pub const Tag = enum {
        lparen,
        rparen,
        symbol,
        number,
        string,
        nil,
        boolean_true,
        boolean_false,
        quote,
        quasiquote,
        unquote,
        unquote_splicing,
    };

    tag: Tag,
    lexeme: []const u8,
    line: usize,
};

fn isSign(char: u8) bool {
    return char == '+' or char == '-';
}

fn isDigit(char: u8) bool {
    return char >= '0' and char <= '9';
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

fn expectScansTo(input: []const u8, expected: Token) !void {
    var scanner = Scanner.init(input);
    const actual = try scanner.readToken() orelse
        return error.UnexpectedEndOfFile;

    try tst.expectEqual(actual.tag, expected.tag);
    try tst.expectEqualSlices(u8, actual.lexeme, expected.lexeme);
    try tst.expectEqual(actual.line, expected.line);
}

test "Scan number" {
    const cases = [_][]const u8{
        "-12",
        "+5",
        "124",
        "1.5",
        "10.",
        "0.333",
    };

    for (cases) |input| {
        try expectScansTo(
            input,
            Token{ .tag = .number, .lexeme = input, .line = 1 },
        );
    }
}

test "Scan symbol" {
    const cases = [_][]const u8{
        "add",
        "sym-1",
        "+",
        "nil?",
    };

    for (cases) |input| {
        try expectScansTo(
            input,
            Token{ .tag = .symbol, .lexeme = input, .line = 1 },
        );
    }
}

test "Scan string" {
    const cases = [_][]const u8{
        "\"Hello\"",
        "\"123\"",
    };

    for (cases) |input| {
        try expectScansTo(
            input,
            Token{ .tag = .string, .lexeme = input, .line = 1 },
        );
    }
}

test "Scan unterminated string" {
    var tokenizer = Scanner.init("\"Hello");
    try tst.expectError(ScanError.UnterminatedString, tokenizer.readToken());
}

test "Scan (" {
    try expectScansTo(
        "(",
        Token{ .tag = .lparen, .lexeme = "(", .line = 1 },
    );
}

test "Scan )" {
    try expectScansTo(
        ")",
        Token{ .tag = .rparen, .lexeme = ")", .line = 1 },
    );
}

test "Scan nil" {
    try expectScansTo(
        "nil",
        Token{ .tag = .nil, .lexeme = "nil", .line = 1 },
    );
}

test "Scan true" {
    try expectScansTo(
        "true",
        Token{ .tag = .boolean_true, .lexeme = "true", .line = 1 },
    );
}

test "Scan false" {
    try expectScansTo(
        "false",
        Token{ .tag = .boolean_false, .lexeme = "false", .line = 1 },
    );
}

test "Scan quote" {
    try expectScansTo(
        "'",
        Token{ .tag = .quote, .lexeme = "'", .line = 1 },
    );
}

test "Scan quasiquote" {
    try expectScansTo(
        "`",
        Token{ .tag = .quasiquote, .lexeme = "`", .line = 1 },
    );
}

test "Scan unquote" {
    try expectScansTo(
        ",",
        Token{ .tag = .unquote, .lexeme = ",", .line = 1 },
    );
}

test "Scan unquote splicing" {
    try expectScansTo(
        ",@",
        Token{ .tag = .unquote_splicing, .lexeme = ",@", .line = 1 },
    );
}

test "Skip whitespaces" {
    try expectScansTo(
        "  1",
        Token{ .tag = .number, .lexeme = "1", .line = 1 },
    );
}

test "Skip newlines" {
    try expectScansTo(
        "\n\n\n1",
        Token{ .tag = .number, .lexeme = "1", .line = 4 },
    );
}

test "Skip comment" {
    try expectScansTo(
        "; this is a comment\n42",
        Token{ .tag = .number, .lexeme = "42", .line = 2 },
    );
}

test "Skip comment at end of file" {
    var scanner = Scanner.init("; comment with no newline");
    const token = try scanner.readToken();
    try tst.expectEqual(null, token);
}

test "Skip comment with code after" {
    var scanner = Scanner.init("(+) ; add numbers\n(* 3 4)");
    _ = try scanner.readToken(); // (
    _ = try scanner.readToken(); // +
    _ = try scanner.readToken(); // )

    // Should get ( from next line
    const tk = try scanner.readToken();
    try tst.expectEqual(.lparen, tk.?.tag);
    try tst.expectEqual(2, tk.?.line);
}

test "Symbols starting with keyword-initial chars" {
    const cases = [_]struct { input: []const u8, tag: Token.Tag }{
        .{ .input = "nope", .tag = .symbol },
        .{ .input = "truth", .tag = .symbol },
        .{ .input = "falsy", .tag = .symbol },
        .{ .input = "nil?", .tag = .symbol },
        .{ .input = "true-ish", .tag = .symbol },
        .{ .input = "false?", .tag = .symbol },
    };

    for (cases) |case| {
        try expectScansTo(
            case.input,
            Token{ .tag = case.tag, .lexeme = case.input, .line = 1 },
        );
    }
}

test "Sign not followed by digit scans as symbol" {
    const cases = [_][]const u8{ "+", "-", "+.5" };

    for (cases) |input| {
        try expectScansTo(
            input,
            Token{ .tag = .symbol, .lexeme = input, .line = 1 },
        );
    }
}

test "Nil followed by symbol chars scans as symbol" {
    var scanner = Scanner.init("nil? 1");
    const first = try scanner.readToken();
    try tst.expectEqual(.symbol, first.?.tag);
    try tst.expectEqualSlices(u8, "nil?", first.?.lexeme);

    const second = try scanner.readToken();
    try tst.expectEqual(.number, second.?.tag);
    try tst.expectEqualSlices(u8, "1", second.?.lexeme);
}
