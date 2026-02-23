const std = @import("std");
const fmt = std.fmt;
const tst = std.testing;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;

const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;
const Token = scanner.Token;

pub const Parser = struct {
    arena: ArenaAllocator,
    scanner: Scanner,

    pub fn init(allocator: Allocator, input: []const u8) Parser {
        return .{
            .arena = ArenaAllocator.init(allocator),
            .scanner = Scanner{ .input = input },
        };
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
    }

    pub fn parse(self: *Parser) ParseError!AbstractSyntaxTree {
        const allocator = self.arena.allocator();

        var nodes = ArrayList(Node){};
        while (try self.scanner.scanToken()) |tk| {
            const node = try self.parseExpr(allocator, tk);
            try nodes.append(allocator, node);
        }

        return AbstractSyntaxTree{
            .nodes = try nodes.toOwnedSlice(allocator),
        };
    }

    fn parseExpr(self: *Parser, allocator: Allocator, tk: Token) ParseError!Node {
        return switch (tk.tag) {
            .number => .{ .number = .{ .val = try fmt.parseFloat(f64, tk.lexeme), .line = tk.line } },
            .boolean_true => .{ .boolean = .{ .val = true, .line = tk.line } },
            .boolean_false => .{ .boolean = .{ .val = false, .line = tk.line } },
            .nil => .{ .nil = .{ .line = tk.line } },
            .symbol => .{ .symbol = .{ .name = tk.lexeme, .line = tk.line } },
            .string => .{ .string = .{ .chars = tk.lexeme[1 .. tk.lexeme.len - 1], .line = tk.line } },
            .lparen => self.parseList(allocator, tk.line),
            .rparen => return ParseError.UnexpectedRightParen,
        };
    }

    fn parseList(self: *Parser, allocator: Allocator, line: u32) ParseError!Node {
        var items = ArrayList(Node){};
        while (try self.scanner.scanToken()) |tk| {
            if (tk.tag == .rparen) break;
            const item = try self.parseExpr(allocator, tk);
            try items.append(allocator, item);
        } else return ParseError.MissingRightParen;

        return Node{
            .list = Node.List{
                .items = try items.toOwnedSlice(allocator),
                .line = line,
            },
        };
    }
};

pub const ParseError = error{
    UnexpectedRightParen,
    MissingRightParen,
} || scanner.ScanError || fmt.ParseFloatError || Allocator.Error;

pub const AbstractSyntaxTree = struct { nodes: []const Node };

pub const Node = union(enum) {
    number: Number,
    boolean: Boolean,
    nil: Nil,
    string: String,
    symbol: Symbol,
    list: List,

    pub const Number = struct { val: f64, line: u32 };
    pub const Boolean = struct { val: bool, line: u32 };
    pub const Nil = struct { line: u32 };
    pub const String = struct { chars: []const u8, line: u32 };
    pub const Symbol = struct { name: []const u8, line: u32 };
    pub const List = struct { items: []const Node, line: u32 };

    pub fn getLine(self: Node) u32 {
        return switch (self) {
            inline .number, .boolean, .nil, .string, .symbol, .list => |obj| obj.line,
        };
    }
};

fn expectAstWithNodes(input: []const u8, expected: []const Node) !void {
    var parser = Parser.init(tst.allocator, input);
    defer parser.deinit();

    const ast = try parser.parse();
    try tst.expectEqualDeep(AbstractSyntaxTree{ .nodes = expected }, ast);
}

fn expectAstWithOneNode(input: []const u8, expected: Node) !void {
    try expectAstWithNodes(input, &.{expected});
}

fn expectParseError(input: []const u8, expected: ParseError) !void {
    var parser = Parser.init(tst.allocator, input);
    defer parser.deinit();

    try tst.expectError(expected, parser.parse());
}

test "Parse integer number" {
    try expectAstWithOneNode(
        "12",
        .{ .number = .{ .val = 12, .line = 1 } },
    );
}

test "Parse floating number" {
    try expectAstWithOneNode(
        "-.5",
        .{ .number = .{ .val = -0.5, .line = 1 } },
    );
}

test "Parse boolean true" {
    try expectAstWithOneNode(
        "true",
        .{ .boolean = .{ .val = true, .line = 1 } },
    );
}

test "Parse boolean false" {
    try expectAstWithOneNode(
        "false",
        .{ .boolean = .{ .val = false, .line = 1 } },
    );
}

test "Parse nil" {
    try expectAstWithOneNode(
        "nil",
        .{ .nil = .{ .line = 1 } },
    );
}

test "Parse symbol" {
    try expectAstWithOneNode(
        "reduce",
        .{ .symbol = .{ .name = "reduce", .line = 1 } },
    );
}

test "Parse string" {
    try expectAstWithOneNode(
        "\"Hello World!\"",
        .{ .string = .{ .chars = "Hello World!", .line = 1 } },
    );
}

test "Parse list" {
    try expectAstWithOneNode(
        "(+ 1 2)",
        .{ .list = .{
            .items = &.{
                .{ .symbol = .{ .name = "+", .line = 1 } },
                .{ .number = .{ .val = 1, .line = 1 } },
                .{ .number = .{ .val = 2, .line = 1 } },
            },
            .line = 1,
        } },
    );
}

test "Parse nested list" {
    try expectAstWithOneNode(
        "(+ 1 (+ 2 3))",
        .{ .list = .{
            .items = &.{
                .{ .symbol = .{ .name = "+", .line = 1 } },
                .{ .number = .{ .val = 1, .line = 1 } },
                .{
                    .list = .{
                        .items = &.{
                            .{ .symbol = .{ .name = "+", .line = 1 } },
                            .{ .number = .{ .val = 2, .line = 1 } },
                            .{ .number = .{ .val = 3, .line = 1 } },
                        },
                        .line = 1,
                    },
                },
            },
            .line = 1,
        } },
    );
}

test "Parse empty list" {
    try expectAstWithOneNode(
        "()",
        .{ .list = .{ .items = &.{}, .line = 1 } },
    );
}

test "Parse error on unbalanced parens" {
    try expectParseError("())", ParseError.UnexpectedRightParen);
    try expectParseError("(()", ParseError.MissingRightParen);
}

test "Parse multiple nodes" {
    try expectAstWithNodes("1\n2", &.{
        .{ .number = .{ .val = 1, .line = 1 } },
        .{ .number = .{ .val = 2, .line = 2 } },
    });
}

test "Parse empty input" {
    try expectAstWithNodes("", &.{});
}
