const std = @import("std");
const tst = std.testing;
const fmt = std.fmt;
const ArrayList = std.ArrayListUnmanaged;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const scanner = @import("scanner.zig");
const Token = scanner.Token;
const Scanner = scanner.Scanner;

pub const Parser = struct {
    arena: ArenaAllocator,
    scanner: Scanner,

    pub fn init(allocator: Allocator, input: []const u8) Parser {
        return .{
            .arena = ArenaAllocator.init(allocator),
            .scanner = Scanner.init(input),
        };
    }

    pub fn deinit(self: Parser) void {
        self.arena.deinit();
    }

    pub fn parse(self: *Parser) ParseError!Ast {
        const allocator = self.arena.allocator();

        var nodes = ArrayList(Node){};
        while (try self.scanner.readToken()) |tk| {
            const node = try self.parseExpr(allocator, tk);
            try nodes.append(allocator, node);
        }

        return Ast{
            .nodes = try nodes.toOwnedSlice(allocator),
        };
    }

    fn parseExpr(self: *Parser, allocator: Allocator, tk: Token) ParseError!Node {
        return switch (tk.tag) {
            .number => Node{
                .number = Node.Number{
                    .val = try fmt.parseFloat(f64, tk.lexeme),
                    .line = tk.line,
                },
            },
            .boolean_true => Node{
                .boolean = Node.Boolean{
                    .val = true,
                    .line = tk.line,
                },
            },
            .boolean_false => Node{
                .boolean = Node.Boolean{
                    .val = false,
                    .line = tk.line,
                },
            },
            .nil => Node{
                .nil = Node.Nil{
                    .line = tk.line,
                },
            },
            .symbol => Node{
                .symbol = Node.Symbol{
                    .name = tk.lexeme,
                    .line = tk.line,
                },
            },
            .string => Node{
                .string = Node.String{
                    .chars = tk.lexeme[1 .. tk.lexeme.len - 1],
                    .line = tk.line,
                },
            },
            .quote => self.wrapExpr(allocator, tk.line, "quote"),
            .quasiquote => self.wrapExpr(allocator, tk.line, "quasiquote"),
            .unquote => self.wrapExpr(allocator, tk.line, "unquote"),
            .unquote_splicing => self.wrapExpr(allocator, tk.line, "unquote-splicing"),
            .lparen => self.parseList(allocator, tk.line),
            .rparen => return ParseError.UnexpectedRparen,
        };
    }

    fn wrapExpr(self: *Parser, allocator: Allocator, line: usize, sym: []const u8) ParseError!Node {
        const tk = try self.scanner.readToken() orelse
            return ParseError.OutOfTokens;

        const expr = try self.parseExpr(allocator, tk);

        const items = try allocator.dupe(Node, &[_]Node{
            .{ .symbol = .{ .name = sym, .line = line } },
            expr,
        });

        return Node{
            .list = Node.List{
                .items = items,
                .line = line,
            },
        };
    }

    fn parseList(self: *Parser, allocator: Allocator, line: usize) ParseError!Node {
        var items = ArrayList(Node){};

        while (try self.scanner.readToken()) |tk| {
            if (tk.tag == .rparen) {
                return Node{
                    .list = Node.List{
                        .items = try items.toOwnedSlice(allocator),
                        .line = line,
                    },
                };
            }
            const item = try self.parseExpr(allocator, tk);
            try items.append(allocator, item);
        }

        return ParseError.UnclosedList;
    }
};

pub const ParseError = error{
    OutOfTokens,
    UnclosedList,
    UnexpectedRparen,
} || scanner.ScanError || fmt.ParseFloatError || Allocator.Error;

pub const Ast = struct { nodes: []const Node };

pub const Node = union(enum) {
    number: Number,
    boolean: Boolean,
    nil: Nil,
    string: String,
    symbol: Symbol,
    list: List,

    pub const Number = struct { val: f64, line: usize };
    pub const Boolean = struct { val: bool, line: usize };
    pub const Nil = struct { line: usize };
    pub const String = struct { chars: []const u8, line: usize };
    pub const Symbol = struct { name: []const u8, line: usize };
    pub const List = struct { items: []const Node, line: usize };

    pub fn getLine(self: Node) usize {
        return switch (self) {
            inline .number, .boolean, .nil, .string, .symbol, .list => |obj| obj.line,
        };
    }
};

fn expectParsesNode(input: []const u8, expected: Node) !void {
    var parser = Parser.init(tst.allocator, input);
    defer parser.deinit();

    const ast = try parser.parse();
    try tst.expectEqual(1, ast.nodes.len);
    try tst.expectEqualDeep(expected, ast.nodes[0]);
}

test "Parse number" {
    try expectParsesNode(
        "12.3",
        Node{ .number = Node.Number{ .val = 12.3, .line = 1 } },
    );
}

test "Parse boolean true" {
    try expectParsesNode(
        "true",
        Node{ .boolean = Node.Boolean{ .val = true, .line = 1 } },
    );
}

test "Parse boolean false" {
    try expectParsesNode(
        "false",
        Node{ .boolean = Node.Boolean{ .val = false, .line = 1 } },
    );
}

test "Parse nil" {
    try expectParsesNode(
        "nil",
        Node{ .nil = Node.Nil{ .line = 1 } },
    );
}

test "Parse symbol" {
    try expectParsesNode(
        "filter",
        Node{ .symbol = Node.Symbol{ .name = "filter", .line = 1 } },
    );
}

test "Parse string" {
    try expectParsesNode(
        "\"Hello World!\"",
        Node{ .string = Node.String{ .chars = "Hello World!", .line = 1 } },
    );
}

test "Parse list" {
    var parser = Parser.init(tst.allocator, "(1 2 3)");
    defer parser.deinit();

    const ast = try parser.parse();
    const expected = &.{
        Node{
            .list = Node.List{
                .items = &.{
                    Node{ .number = Node.Number{ .val = 1, .line = 1 } },
                    Node{ .number = Node.Number{ .val = 2, .line = 1 } },
                    Node{ .number = Node.Number{ .val = 3, .line = 1 } },
                },
                .line = 1,
            },
        },
    };
    try tst.expectEqualDeep(expected, ast.nodes);
}

test "Parse multiple forms" {
    var parser = Parser.init(tst.allocator, "1 2\n3");
    defer parser.deinit();

    const ast = try parser.parse();
    const expected = &.{
        Node{ .number = Node.Number{ .val = 1, .line = 1 } },
        Node{ .number = Node.Number{ .val = 2, .line = 1 } },
        Node{ .number = Node.Number{ .val = 3, .line = 2 } },
    };
    try tst.expectEqual(expected.len, ast.nodes.len);
    try tst.expectEqualDeep(expected, ast.nodes);
}

test "Parse quoted form" {
    var parser = Parser.init(tst.allocator, "'(1 2)");
    defer parser.deinit();

    const ast = try parser.parse();
    const expected = &.{
        Node{
            .list = .{
                .items = &.{
                    Node{ .symbol = Node.Symbol{ .name = "quote", .line = 1 } },
                    Node{
                        .list = Node.List{
                            .items = &.{
                                Node{ .number = Node.Number{ .val = 1, .line = 1 } },
                                Node{ .number = Node.Number{ .val = 2, .line = 1 } },
                            },
                            .line = 1,
                        },
                    },
                },
                .line = 1,
            },
        },
    };
    try tst.expectEqual(expected.len, ast.nodes.len);
    try tst.expectEqualDeep(expected, ast.nodes);
}

test "Parse quasiquoted form" {
    var parser = Parser.init(tst.allocator, "`(1 2)");
    defer parser.deinit();

    const ast = try parser.parse();
    const expected = &.{
        Node{
            .list = .{
                .items = &.{
                    Node{ .symbol = Node.Symbol{ .name = "quasiquote", .line = 1 } },
                    Node{
                        .list = Node.List{
                            .items = &.{
                                Node{ .number = Node.Number{ .val = 1, .line = 1 } },
                                Node{ .number = Node.Number{ .val = 2, .line = 1 } },
                            },
                            .line = 1,
                        },
                    },
                },
                .line = 1,
            },
        },
    };
    try tst.expectEqual(expected.len, ast.nodes.len);
    try tst.expectEqualDeep(expected, ast.nodes);
}

test "Parse unquoted symbol" {
    var parser = Parser.init(tst.allocator, ",x");
    defer parser.deinit();

    const ast = try parser.parse();
    const expected = &.{
        Node{
            .list = Node.List{
                .items = &.{
                    Node{ .symbol = Node.Symbol{ .name = "unquote", .line = 1 } },
                    Node{ .symbol = Node.Symbol{ .name = "x", .line = 1 } },
                },
                .line = 1,
            },
        },
    };

    try tst.expectEqual(expected.len, ast.nodes.len);
    try tst.expectEqualDeep(expected, ast.nodes);
}

test "Parse unquote-splicing form" {
    var parser = Parser.init(tst.allocator, ",@(1 2)");
    defer parser.deinit();

    const ast = try parser.parse();
    const expected = &.{
        Node{
            .list = .{
                .items = &.{
                    Node{ .symbol = Node.Symbol{ .name = "unquote-splicing", .line = 1 } },
                    Node{
                        .list = Node.List{
                            .items = &.{
                                Node{ .number = Node.Number{ .val = 1, .line = 1 } },
                                Node{ .number = Node.Number{ .val = 2, .line = 1 } },
                            },
                            .line = 1,
                        },
                    },
                },
                .line = 1,
            },
        },
    };
    try tst.expectEqual(expected.len, ast.nodes.len);
    try tst.expectEqualDeep(expected, ast.nodes);
}

test "Unclosed list returns error" {
    var parser = Parser.init(tst.allocator, "(1 2 3");
    defer parser.deinit();

    try tst.expectError(ParseError.UnclosedList, parser.parse());
}

test "Unexpected rparen returns error" {
    var parser = Parser.init(tst.allocator, ")");
    defer parser.deinit();

    try tst.expectError(ParseError.UnexpectedRparen, parser.parse());
}
