const std = @import("std");
const testing = std.testing;
const ast = @import("ast");
const Object = @import("object").Object;
const Lexer = @import("lexer").Lexer;
const Parser = @import("parser").Parser;

fn newError(allocator: std.mem.Allocator, comptime format: []const u8, args: anytype) !Object {
    const error_message = try std.fmt.allocPrint(allocator, format, args);
    return Object{ .error_obj = error_message };
}

fn isError(obj: ?Object) bool {
    if (obj) |o| {
        return o.getType() == .error_obj;
    }
    return false;
}

const EvalError = error{OutOfMemory};

pub fn eval(allocator: std.mem.Allocator, node: ast.Node) EvalError!?Object {
    return switch (node.nodeType()) {
        .Program => {
            const program_node: *ast.Program = @ptrCast(@alignCast(node.ptr));
            return try evalProgram(allocator, program_node.statements.items);
        },
        .ExpressionStatement => {
            const expr_stmt_node: *ast.ExpressionStatement = @ptrCast(@alignCast(node.ptr));
            return try eval(allocator, expr_stmt_node.expression.?.node);
        },
        .BlockStatement => {
            const block_node: *ast.BlockStatement = @ptrCast(@alignCast(node.ptr));
            return try evalBlockStatement(allocator, block_node.statements.items);
        },
        .ReturnStatement => {
            const return_node: *ast.ReturnStatement = @ptrCast(@alignCast(node.ptr));
            var return_value = try eval(allocator, return_node.return_value.?.node);
            if (isError(return_value)) {
                return return_value;
            }
            return Object{ .return_obj = &(return_value.?) };
        },
        .PrefixExpression => {
            const prefix_node: *ast.PrefixExpression = @ptrCast(@alignCast(node.ptr));
            if (try eval(allocator, prefix_node.right.?.node)) |right| {
                if (isError(right)) {
                    return right;
                }
                return try evalPrefixExpression(allocator, prefix_node.operator, right);
            }
            return null;
        },
        .InfixExpression => {
            const infix_node: *ast.InfixExpression = @ptrCast(@alignCast(node.ptr));
            if (try eval(allocator, infix_node.left.node)) |left| {
                if (isError(left)) {
                    return left;
                }
                if (try eval(allocator, infix_node.right.?.node)) |right| {
                    if (isError(right)) {
                        return right;
                    }
                    return try evalInfixExpression(allocator, infix_node.operator, left, right);
                }
            }
            return null;
        },
        .IfExpression => {
            return try evalIfExpression(allocator, node);
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

fn evalProgram(allocator: std.mem.Allocator, statements: []ast.Statement) !?Object {
    var result: ?Object = null;

    for (statements) |stmt| {
        result = try eval(allocator, stmt.node);

        switch (result.?) {
            .return_obj => |val| {
                return val.*;
            },
            .error_obj => {
                return result;
            },
            else => {},
        }
    }

    return result;
}

fn evalIfExpression(allocator: std.mem.Allocator, node: ast.Node) !?Object {
    const if_node: *ast.IfExpression = @ptrCast(@alignCast(node.ptr));
    if (try eval(allocator, if_node.condition.?.node)) |condition| {
        if (isError(condition)) {
            return condition;
        }
        if (isTruthy(condition)) {
            return try eval(allocator, if_node.consequence.?.statement().node);
        } else if (if_node.alternative) |alt| {
            return try eval(allocator, alt.statement().node);
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

fn evalInfixExpression(allocator: std.mem.Allocator, operator: []const u8, left: Object, right: Object) !?Object {
    if (left.getType() == .integer and right.getType() == .integer) {
        return try evalIntegerInfixExpression(allocator, operator, left, right);
    } else if (std.mem.eql(u8, operator, "==")) {
        return Object{ .boolean = left.boolean == right.boolean };
    } else if (std.mem.eql(u8, operator, "!=")) {
        return Object{ .boolean = left.boolean != right.boolean };
    } else if (left.getType() != right.getType()) {
        return try newError(allocator, "type mismatch: {s} {s} {s}", .{ @tagName(left.getType()), operator, @tagName(right.getType()) });
    }
    return try newError(allocator, "unknown operator: {s} {s} {s}", .{ @tagName(left.getType()), operator, @tagName(right.getType()) });
}

fn evalIntegerInfixExpression(allocator: std.mem.Allocator, operator: []const u8, left: Object, right: Object) !?Object {
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
    return try newError(allocator, "unknown operator: integer {s} integer", .{operator});
}

fn evalPrefixExpression(allocator: std.mem.Allocator, operator: []const u8, right: Object) !?Object {
    if (std.mem.eql(u8, operator, "!")) {
        return evalBangOperatorExpression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return try evalMinusPrefixOperatorExpression(allocator, right);
    }
    return try newError(allocator, "unknown operator: {s}{any}", .{ operator, right.getType() });
}

fn evalMinusPrefixOperatorExpression(allocator: std.mem.Allocator, right: Object) !?Object {
    switch (right) {
        .integer => |int_obj| {
            return Object{ .integer = -int_obj };
        },
        else => return try newError(allocator, "unknown operator: -{s}", .{@tagName(right.getType())}),
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

fn evalBlockStatement(allocator: std.mem.Allocator, statements: []ast.Statement) !?Object {
    var result: ?Object = null;

    for (statements) |stmt| {
        result = try eval(allocator, stmt.node);

        if (result) |r| {
            if (r.getType() == .return_obj or r.getType() == .error_obj) {
                return r;
            }
        }
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
        const allocator = std.testing.allocator;
        const evaluated = try testEval(allocator, tt.input);
        defer evaluated.?.deinit(allocator);
        try testing.expect(testIntegerObject(evaluated.?, tt.expected));
    }
}

fn testEval(allocator: std.mem.Allocator, input: []const u8) !?Object {
    const l = try Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    try p.parseProgram();

    return try eval(allocator, p.program.node());
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
        const allocator = std.testing.allocator;
        const evaluated = try testEval(allocator, tt.input);
        defer evaluated.?.deinit(allocator);
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
        const allocator = std.testing.allocator;
        const evaluated = try testEval(allocator, tt.input);
        defer evaluated.?.deinit(allocator);
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
        const allocator = std.testing.allocator;
        const evaluated = try testEval(allocator, tt.input);
        defer evaluated.?.deinit(allocator);
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

test "test return statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "return 10;", .expected = 10 },
        .{ .input = "return 10; 9;", .expected = 10 },
        .{ .input = "return 2 * 5; 9;", .expected = 10 },
        .{ .input = "9; return 2 * 5; 9;", .expected = 10 },
        .{ .input = "if (10 > 1) { if (10 > 1) { return 10; } return 1; }", .expected = 10 },
    };

    for (tests) |tt| {
        const allocator = std.testing.allocator;
        const evaluated = try testEval(allocator, tt.input);
        defer evaluated.?.deinit(allocator);
        try testing.expect(testIntegerObject(evaluated.?, tt.expected));
    }
}

test "test error handling" {
    const tests = [_]struct {
        input: []const u8,
        expected_message: []const u8,
    }{
        .{ .input = "5 + true;", .expected_message = "type mismatch: integer + boolean" },
        .{ .input = "5 + true; 5;", .expected_message = "type mismatch: integer + boolean" },
        .{ .input = "-true", .expected_message = "unknown operator: -boolean" },
        .{ .input = "true + false;", .expected_message = "unknown operator: boolean + boolean" },
        .{ .input = "5; true + false; 5", .expected_message = "unknown operator: boolean + boolean" },
        .{ .input = "if (10 > 1) { true + false; }", .expected_message = "unknown operator: boolean + boolean" },
        .{ .input = "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }", .expected_message = "unknown operator: boolean + boolean" },
        // .{ .input = "foobar", .expected_message = "identifier not found: foobar" },
    };

    for (tests) |tt| {
        const allocator = std.testing.allocator;
        const evaluated = try testEval(allocator, tt.input);
        defer evaluated.?.deinit(allocator);
        switch (evaluated.?) {
            .error_obj => |err| {
                try testing.expect(std.mem.eql(u8, err, tt.expected_message));
            },
            else => try testing.expect(false),
        }
    }
}
