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
        .BlockStatement => {
            const block_node: *ast.BlockStatement = @ptrCast(@alignCast(node.ptr));
            return evalStatements(block_node.statements.items);
        },
        .PrefixExpression => {
            const prefix_node: *ast.PrefixExpression = @ptrCast(@alignCast(node.ptr));
            if (eval(prefix_node.right.?.node)) |right| {
                return evalPrefixExpression(prefix_node.operator, right);
            }
            return null;
        },
        .InfixExpression => {
            const infix_node: *ast.InfixExpression = @ptrCast(@alignCast(node.ptr));
            if (eval(infix_node.left.node)) |left| {
                if (eval(infix_node.right.?.node)) |right| {
                    return evalInfixExpression(infix_node.operator, left, right);
                }
            }
            return null;
        },
        .IfExpression => {
            return evalIfExpression(node);
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

fn evalIfExpression(node: ast.Node) ?Object {
    const if_node: *ast.IfExpression = @ptrCast(@alignCast(node.ptr));
    if (eval(if_node.condition.?.node)) |condition| {
        if (isTruthy(condition)) {
            return eval(if_node.consequence.?.statement().node);
        } else if (if_node.alternative) |alt| {
            return eval(alt.statement().node);
        }
    }
    return Object{ .null = {} };
}

fn isTruthy(obj: Object) bool {
    return switch (obj) {
        .null => false,
        .boolean => |bool_obj| bool_obj,
        else => true,
    };
}

fn evalInfixExpression(operator: []const u8, left: Object, right: Object) ?Object {
    if (left.getType() == .integer and right.getType() == .integer) {
        return evalIntegerInfixExpression(operator, left, right);
    } else if (std.mem.eql(u8, operator, "==")) {
        return Object{ .boolean = left.boolean == right.boolean };
    } else if (std.mem.eql(u8, operator, "!=")) {
        return Object{ .boolean = left.boolean != right.boolean };
    }
    return null;
}

fn evalIntegerInfixExpression(operator: []const u8, left: Object, right: Object) ?Object {
    if (std.mem.eql(u8, operator, "+")) {
        return Object{ .integer = left.integer + right.integer };
    } else if (std.mem.eql(u8, operator, "-")) {
        return Object{ .integer = left.integer - right.integer };
    } else if (std.mem.eql(u8, operator, "*")) {
        return Object{ .integer = left.integer * right.integer };
    } else if (std.mem.eql(u8, operator, "/")) {
        return Object{ .integer = @divExact(left.integer, right.integer) };
    } else if (std.mem.eql(u8, operator, "<")) {
        return Object{ .boolean = left.integer < right.integer };
    } else if (std.mem.eql(u8, operator, ">")) {
        return Object{ .boolean = left.integer > right.integer };
    } else if (std.mem.eql(u8, operator, "==")) {
        return Object{ .boolean = left.integer == right.integer };
    } else if (std.mem.eql(u8, operator, "!=")) {
        return Object{ .boolean = left.integer != right.integer };
    }
    return null;
}

fn evalPrefixExpression(operator: []const u8, right: Object) ?Object {
    if (std.mem.eql(u8, operator, "!")) {
        return evalBangOperatorExpression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return evalMinusPrefixOperatorExpression(right);
    }
    return null;
}

fn evalMinusPrefixOperatorExpression(right: Object) ?Object {
    return switch (right) {
        .integer => |int_obj| {
            return Object{ .integer = -int_obj };
        },
        else => return null,
    };
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
        .{ .input = "-5", .expected = -5 },
        .{ .input = "-10", .expected = -10 },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = 10 },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = 32 },
        .{ .input = "-50 + 100 + -50", .expected = 0 },
        .{ .input = "5 * 2 + 10", .expected = 20 },
        .{ .input = "5 + 2 * 10", .expected = 25 },
        .{ .input = "20 + 2 * -10", .expected = 0 },
        .{ .input = "50 / 2 * 2 + 10", .expected = 60 },
        .{ .input = "2 * (5 + 10)", .expected = 30 },
        .{ .input = "3 * 3 * 3 + 10", .expected = 37 },
        .{ .input = "3 * (3 * 3) + 10", .expected = 37 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
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
        .{ .input = "1 < 2", .expected = true },
        .{ .input = "1 > 2", .expected = false },
        .{ .input = "1 < 1", .expected = false },
        .{ .input = "1 > 1", .expected = false },
        .{ .input = "1 == 1", .expected = true },
        .{ .input = "1 != 1", .expected = false },
        .{ .input = "1 == 2", .expected = false },
        .{ .input = "1 != 2", .expected = true },
        .{ .input = "true == true", .expected = true },
        .{ .input = "false == false", .expected = true },
        .{ .input = "true == false", .expected = false },
        .{ .input = "true != false", .expected = true },
        .{ .input = "false != true", .expected = true },
        .{ .input = "(1 < 2) == true", .expected = true },
        .{ .input = "(1 < 2) == false", .expected = false },
        .{ .input = "(1 > 2) == true", .expected = false },
        .{ .input = "(1 > 2) == false", .expected = true },
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

test "test if-else expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: ?i64,
    }{
        .{ .input = "if (true) { 10 }", .expected = 10 },
        .{ .input = "if (false) { 10 }", .expected = null },
        .{ .input = "if (1) { 10 }", .expected = 10 },
        .{ .input = "if (1 < 2) { 10 }", .expected = 10 },
        .{ .input = "if (1 > 2) { 10 }", .expected = null },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = 20 },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = 10 },
    };

    for (tests) |tt| {
        const evaluated = try testEval(tt.input);
        if (tt.expected) |expected| {
            try testing.expect(testIntegerObject(evaluated.?, expected));
        } else {
            try testing.expect(testNullObject(evaluated.?));
        }
    }
}

fn testNullObject(obj: Object) bool {
    return switch (obj) {
        .null => true,
        else => false,
    };
}
