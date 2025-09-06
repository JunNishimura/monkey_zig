const std = @import("std");
const testing = std.testing;
const ast = @import("ast");
const Object = @import("object").Object;
const Lexer = @import("lexer").Lexer;
const Parser = @import("parser").Parser;

pub fn eval(node: ast.Node) ?Object {
    return switch (node.nodeType()) {
        .Program => {
            const program_node: *ast.Program = @ptrCast(@alignCast(node.ptr));
            return evalStatements(program_node.statements.items);
        },
        .ExpressionStatement => {
            const expr_stmt_node: *ast.ExpressionStatement = @ptrCast(@alignCast(node.ptr));
            return eval(expr_stmt_node.expression.?.node);
        },
        .PrefixExpression => {
            const prefix_node: *ast.PrefixExpression = @ptrCast(@alignCast(node.ptr));
            const right = eval(prefix_node.right.?.node);
            return evalPrefixExpression(prefix_node.operator, right);
        },
        .IntegerLiteral => {
            const int_node: *ast.IntegerLiteral = @ptrCast(@alignCast(node.ptr));
            return Object{ .integer = int_node.value orelse 0 };
        },
        .Boolean => {
            const bool_node: *ast.Boolean = @ptrCast(@alignCast(node.ptr));
            return Object{ .boolean = bool_node.value };
        },
        else => return null,
    };
}

fn evalPrefixExpression(operator: []const u8, right: ?Object) ?Object {
    if (right == null) return null;

    if (std.mem.eql(u8, operator, "!")) {
        return evalBangOperatorExpression(right.?);
    } else {
        return null;
    }
}

fn evalBangOperatorExpression(right: Object) Object {
    return switch (right) {
        .boolean => |bool_obj| {
            return Object{ .boolean = !bool_obj };
        },
        .null => Object{ .boolean = true },
        else => Object{ .boolean = false },
    };
}

fn evalStatements(statements: []ast.Statement) ?Object {
    var result: ?Object = null;

    for (statements) |stmt| {
        result = eval(stmt.node);
    }

    return result;
}

test "eval integer expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
    };

    for (tests) |tt| {
        const evaluated = try testEval(tt.input);
        try testing.expect(testIntegerObject(evaluated.?, tt.expected));
    }
}

fn testEval(input: []const u8) !?Object {
    const allocator = std.testing.allocator;
    const l = try Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    try p.parseProgram();

    return eval(p.program.node());
}

fn testIntegerObject(obj: Object, expected: i64) bool {
    switch (obj) {
        .integer => |int_obj| {
            return int_obj == expected;
        },
        else => return false,
    }
}

test "eval boolean expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
    };

    for (tests) |tt| {
        const evaluated = try testEval(tt.input);
        try testing.expect(testBooleanObject(evaluated.?, tt.expected));
    }
}

fn testBooleanObject(obj: Object, expected: bool) bool {
    switch (obj) {
        .boolean => |bool_obj| {
            return bool_obj == expected;
        },
        else => return false,
    }
}

test "test bang operator" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "!true", .expected = false },
        .{ .input = "!false", .expected = true },
        .{ .input = "!5", .expected = false },
        .{ .input = "!!true", .expected = true },
        .{ .input = "!!false", .expected = false },
        .{ .input = "!!5", .expected = true },
    };

    for (tests) |tt| {
        const evaluated = try testEval(tt.input);
        try testing.expect(testBooleanObject(evaluated.?, tt.expected));
    }
}
