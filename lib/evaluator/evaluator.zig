const std = @import("std");
const testing = std.testing;
const ast = @import("ast");
const obj = @import("object");
const Environment = obj.Environment;
const Function = obj.Function;
const Lexer = @import("lexer").Lexer;
const Parser = @import("parser").Parser;

const EvalError = error{OutOfMemory};

pub const Evaluator = struct {
    allocator: std.mem.Allocator,
    result: ?obj.Object,
    evaluated: std.ArrayList(obj.Object),

    pub fn init(allocator: std.mem.Allocator) !*Evaluator {
        const evaluator = try allocator.create(Evaluator);
        evaluator.* = .{
            .allocator = allocator,
            .result = null,
            .evaluated = std.ArrayList(obj.Object).init(allocator),
        };
        return evaluator;
    }

    pub fn eval(self: *Evaluator, node: ast.Node, env: *Environment) EvalError!?obj.Object {
        switch (node.nodeType()) {
            .Program => {
                const program_node: *ast.Program = @ptrCast(@alignCast(node.ptr));
                self.result = try self.evalProgram(program_node.statements.items, env);
                return self.result;
            },
            .BlockStatement => {
                const block_node: *ast.BlockStatement = @ptrCast(@alignCast(node.ptr));
                return try self.evalBlockStatement(block_node.statements.items, env);
            },
            .ExpressionStatement => {
                const expr_stmt_node: *ast.ExpressionStatement = @ptrCast(@alignCast(node.ptr));
                return try self.eval(expr_stmt_node.expression.?.node, env);
            },
            .ReturnStatement => {
                const return_node: *ast.ReturnStatement = @ptrCast(@alignCast(node.ptr));

                const return_value = try self.eval(return_node.return_value.?.node, env);
                if (return_value.?.isError()) {
                    return return_value;
                }

                return (try obj.Return.init(self.allocator, return_value.?)).object();
            },
            .LetStatement => {
                const let_node: *ast.LetStatement = @ptrCast(@alignCast(node.ptr));
                const value = try self.eval(let_node.value.?.node, env);
                if (value.?.isError()) {
                    return value;
                }
                try self.addEvaluated(value.?);
                try env.set(let_node.name.?.value, value.?);
            },
            .FunctionLiteral => {
                const func_node: *ast.FunctionLiteral = @ptrCast(@alignCast(node.ptr));
                const params = func_node.parameters.items;
                const body = func_node.body.?;
                return (try Function.init(self.allocator, params, body, env)).object();
            },
            .IfExpression => {
                return try self.evalIfExpression(node, env);
            },
            .InfixExpression => {
                const infix_node: *ast.InfixExpression = @ptrCast(@alignCast(node.ptr));
                const left = try self.eval(infix_node.left.node, env);
                if (left) |l| {
                    try self.addEvaluated(l);
                    const right = try self.eval(infix_node.right.?.node, env);
                    if (right) |r| {
                        try self.addEvaluated(r);
                        return try self.evalInfixExpression(infix_node.operator, l, r);
                    }
                }
                return null;
            },
            .PrefixExpression => {
                const prefix_node: *ast.PrefixExpression = @ptrCast(@alignCast(node.ptr));
                const right = try self.eval(prefix_node.right.?.node, env);
                if (right) |r| {
                    try self.addEvaluated(r);
                    return try self.evalPrefixExpression(prefix_node.operator, r);
                }
                return null;
            },
            .Identifier => {
                return try self.evalIdentifier(node, env);
            },
            .IntegerLiteral => {
                const int_node: *ast.IntegerLiteral = @ptrCast(@alignCast(node.ptr));
                const int_obj = try obj.Integer.init(self.allocator, int_node.value orelse 0);
                return int_obj.object();
            },
            .Boolean => {
                const bool_node: *ast.Boolean = @ptrCast(@alignCast(node.ptr));
                const bool_obj = try obj.Boolean.init(self.allocator, bool_node.value);
                return bool_obj.object();
            },
            else => {
                return null;
            },
        }
        return null;
    }

    fn evalProgram(self: *Evaluator, statements: []ast.Statement, env: *Environment) EvalError!?obj.Object {
        var result: ?obj.Object = null;
        for (statements) |stmt| {
            result = try self.eval(stmt.node, env);

            if (result) |r| {
                try self.addEvaluated(r);

                switch (r.getType()) {
                    .return_obj => {
                        const return_obj: *obj.Return = @ptrCast(@alignCast(r.ptr));

                        return return_obj.value;
                    },
                    .error_obj => {
                        return r;
                    },
                    else => {},
                }
            }
        }

        return result;
    }

    fn evalBlockStatement(self: *Evaluator, statements: []ast.Statement, env: *Environment) EvalError!?obj.Object {
        var result: ?obj.Object = null;

        for (statements) |stmt| {
            if (result) |r| {
                r.deinit();
            }

            result = try self.eval(stmt.node, env);
            if (result) |r| {
                if (r.getType() == .return_obj or r.getType() == .error_obj) {
                    return r;
                }
            }
        }

        return result;
    }

    fn evalIfExpression(self: *Evaluator, node: ast.Node, env: *Environment) EvalError!?obj.Object {
        const if_node: *ast.IfExpression = @ptrCast(@alignCast(node.ptr));

        const cond_obj = try self.eval(if_node.condition.?.node, env);
        if (cond_obj) |cond| {
            try self.addEvaluated(cond);

            if (cond.isError()) {
                return cond;
            }
            if (isTruthy(cond)) {
                return try self.eval(if_node.consequence.?.statement().node, env);
            } else if (if_node.alternative) |alt| {
                return try self.eval(alt.statement().node, env);
            }
        }

        return (try obj.Null.init(self.allocator)).object();
    }

    fn isTruthy(object: obj.Object) bool {
        switch (object.getType()) {
            .null => return false,
            .boolean => {
                const bool_obj: *obj.Boolean = @ptrCast(@alignCast(object.ptr));
                return bool_obj.value;
            },
            else => return true,
        }
    }

    fn evalPrefixExpression(self: *Evaluator, operator: []const u8, right: obj.Object) EvalError!obj.Object {
        if (std.mem.eql(u8, operator, "!")) {
            return try self.evalBangOperatorExpression(right);
        } else if (std.mem.eql(u8, operator, "-")) {
            return try self.evalMinusPrefixOperatorExpression(right);
        }
        return try self.newError("unknown operator: {s}{any}", .{ operator, right.getType() });
    }

    fn evalBangOperatorExpression(self: *Evaluator, right: obj.Object) !obj.Object {
        switch (right.getType()) {
            .boolean => {
                const bool_obj: *obj.Boolean = @ptrCast(@alignCast(right.ptr));
                return (try obj.Boolean.init(self.allocator, !bool_obj.value)).object();
            },
            .null => {
                return (try obj.Boolean.init(self.allocator, true)).object();
            },
            else => {
                return (try obj.Boolean.init(self.allocator, false)).object();
            },
        }
    }

    fn evalMinusPrefixOperatorExpression(self: *Evaluator, right: obj.Object) !obj.Object {
        switch (right.getType()) {
            .integer => {
                const int_obj: *obj.Integer = @ptrCast(@alignCast(right.ptr));
                return (try obj.Integer.init(self.allocator, -int_obj.value)).object();
            },
            else => return try self.newError("unknown operator: -{s}", .{@tagName(right.getType())}),
        }
    }

    fn evalInfixExpression(self: *Evaluator, operator: []const u8, left: obj.Object, right: obj.Object) !?obj.Object {
        if (left.getType() == .integer and right.getType() == .integer) {
            const left_int: *obj.Integer = @ptrCast(@alignCast(left.ptr));
            const right_int: *obj.Integer = @ptrCast(@alignCast(right.ptr));
            return try self.evalIntegerInfixExpression(operator, left_int, right_int);
        } else if (std.mem.eql(u8, operator, "==")) {
            const left_bool: *obj.Boolean = @ptrCast(@alignCast(left.ptr));
            const right_bool: *obj.Boolean = @ptrCast(@alignCast(right.ptr));
            return (try obj.Boolean.init(self.allocator, left_bool.value == right_bool.value)).object();
        } else if (std.mem.eql(u8, operator, "!=")) {
            const left_bool: *obj.Boolean = @ptrCast(@alignCast(left.ptr));
            const right_bool: *obj.Boolean = @ptrCast(@alignCast(right.ptr));
            return (try obj.Boolean.init(self.allocator, left_bool.value != right_bool.value)).object();
        } else if (left.getType() != right.getType()) {
            return try self.newError("type mismatch: {s} {s} {s}", .{ @tagName(left.getType()), operator, @tagName(right.getType()) });
        }
        return try self.newError("unknown operator: {s} {s} {s}", .{ @tagName(left.getType()), operator, @tagName(right.getType()) });
    }

    fn evalIntegerInfixExpression(self: *Evaluator, operator: []const u8, left: *obj.Integer, right: *obj.Integer) !?obj.Object {
        if (std.mem.eql(u8, operator, "+")) {
            return (try obj.Integer.init(self.allocator, left.value + right.value)).object();
        } else if (std.mem.eql(u8, operator, "-")) {
            return (try obj.Integer.init(self.allocator, left.value - right.value)).object();
        } else if (std.mem.eql(u8, operator, "*")) {
            return (try obj.Integer.init(self.allocator, left.value * right.value)).object();
        } else if (std.mem.eql(u8, operator, "/")) {
            return (try obj.Integer.init(self.allocator, @divExact(left.value, right.value))).object();
        } else if (std.mem.eql(u8, operator, "<")) {
            return (try obj.Boolean.init(self.allocator, left.value < right.value)).object();
        } else if (std.mem.eql(u8, operator, ">")) {
            return (try obj.Boolean.init(self.allocator, left.value > right.value)).object();
        } else if (std.mem.eql(u8, operator, "==")) {
            return (try obj.Boolean.init(self.allocator, left.value == right.value)).object();
        } else if (std.mem.eql(u8, operator, "!=")) {
            return (try obj.Boolean.init(self.allocator, left.value != right.value)).object();
        }
        return try self.newError("unknown operator: integer {s} integer", .{operator});
    }

    fn evalIdentifier(self: *Evaluator, node: ast.Node, env: *Environment) EvalError!obj.Object {
        const ident_node: *ast.Identifier = @ptrCast(@alignCast(node.ptr));
        if (env.get(ident_node.value)) |val| {
            val.setIsIdent(true);
            return val;
        }
        return try self.newError("identifier not found: {s}", .{ident_node.value});
    }

    fn newError(self: *Evaluator, comptime format: []const u8, args: anytype) !obj.Object {
        const error_obj = try obj.Error.init(self.allocator, format, args);
        return error_obj.object();
    }

    pub fn deinit(self: *Evaluator) void {
        for (self.evaluated.items) |object| {
            object.deinit();
        }
        self.evaluated.deinit();
        self.allocator.destroy(self);
    }

    fn addEvaluated(self: *Evaluator, object: obj.Object) EvalError!void {
        if (object.isIdent()) {
            return;
        }
        try self.evaluated.append(object);
    }
};

// pub fn eval(allocator: std.mem.Allocator, node: ast.Node, env: *Environment) EvalError!?Object {
//     switch (node.nodeType()) {
//         .CallExpression => {
//             const call_node: *ast.CallExpression = @ptrCast(@alignCast(node.ptr));
//             const function = try eval(allocator, call_node.function.node, env);
//             if (isError(function)) {
//                 return function;
//             }
//             const args = try evalExpressions(allocator, call_node.arguments.items, env);
//             defer args.deinit();
//             const args_slice = args.items;
//             if (args_slice.len == 1 and isError(args_slice[0])) {
//                 return args_slice[0];
//             }

//             return try applyFunction(allocator, function.?, args_slice);
//         },
//     }
//     return null;
// }

// fn applyFunction(allocator: std.mem.Allocator, function: Object, args: []Object) !?Object {
//     switch (function) {
//         .function_obj => |func| {
//             const extended_env = try extendFunctionEnv(allocator, func, args);
//             defer extended_env.deinit(allocator);

//             const evaluated = try eval(allocator, func.body.statement().node, extended_env);
//             return unwrapReturnValue(evaluated);
//         },
//         else => return try newError(allocator, "not a function: {s}", .{@tagName(function.getType())}),
//     }
// }

// fn unwrapReturnValue(obj: ?Object) ?Object {
//     if (obj) |o| {
//         if (o.getType() == .return_obj) {
//             return o.return_obj.*;
//         }
//         return o;
//     }
//     return null;
// }

// fn extendFunctionEnv(allocator: std.mem.Allocator, func: *Function, args: []Object) !*Environment {
//     const env = try Environment.initEnclosed(allocator, func.env);

//     for (func.parameters, 0..) |param, i| {
//         try env.set(param.value, args[i]);
//     }

//     return env;
// }

// fn evalExpressions(allocator: std.mem.Allocator, expressions: []ast.Expression, env: *Environment) !std.ArrayList(Object) {
//     var result = std.ArrayList(Object).init(allocator);

//     for (expressions) |expr| {
//         const evaluated = try eval(allocator, expr.node, env);
//         if (isError(evaluated)) {
//             result.deinit();
//             try result.append(evaluated.?);
//             return result;
//         }
//         try result.append(evaluated.?);
//     }

//     return result;
// }

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
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        try p.parseProgram();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(p.program.node(), env);

        try testing.expect(testIntegerObject(evaluated.?, tt.expected));
    }
}

fn testIntegerObject(object: obj.Object, expected: i64) bool {
    switch (object.getType()) {
        .integer => {
            const int_obj: *obj.Integer = @ptrCast(@alignCast(object.ptr));
            return int_obj.value == expected;
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
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        try p.parseProgram();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(p.program.node(), env);
        try testing.expect(testBooleanObject(evaluated.?, tt.expected));
    }
}

fn testBooleanObject(object: obj.Object, expected: bool) bool {
    switch (object.getType()) {
        .boolean => {
            const bool_obj: *obj.Boolean = @ptrCast(@alignCast(object.ptr));
            return bool_obj.value == expected;
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
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        try p.parseProgram();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(p.program.node(), env);
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
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        try p.parseProgram();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(p.program.node(), env);
        if (tt.expected) |expected| {
            try testing.expect(testIntegerObject(evaluated.?, expected));
        } else {
            try testing.expect(testNullObject(evaluated.?));
        }
    }
}

fn testNullObject(object: obj.Object) bool {
    switch (object.getType()) {
        .null => return true,
        else => return false,
    }
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
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        try p.parseProgram();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(p.program.node(), env);
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
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        try p.parseProgram();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(p.program.node(), env);
        switch (evaluated.?.getType()) {
            .error_obj => {
                const err_obj: *obj.Error = @ptrCast(@alignCast(evaluated.?.ptr));
                try testing.expect(std.mem.eql(u8, err_obj.message, tt.expected_message));
            },
            else => try testing.expect(false),
        }
    }
}

test "test let statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let a = 5; a;", .expected = 5 },
        .{ .input = "let a = 5 * 5; a;", .expected = 25 },
        .{ .input = "let a = 5; let b = a; b;", .expected = 5 },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = 15 },
    };

    for (tests) |tt| {
        const allocator = std.testing.allocator;
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        try p.parseProgram();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(p.program.node(), env);
        try testing.expect(testIntegerObject(evaluated.?, tt.expected));
    }
}

test "test function object" {
    const input = "fn(x) { x + 2; };";

    const allocator = std.testing.allocator;

    const l = try Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    try p.parseProgram();

    const env = try Environment.init(allocator);
    defer env.deinit(allocator);

    const evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    const evaluated = try evaluator.eval(p.program.node(), env);
    switch (evaluated.?.getType()) {
        .function_obj => {
            const func: *Function = @ptrCast(@alignCast(evaluated.?.ptr));
            try testing.expect(func.parameters.len == 1);
            try testing.expect(std.mem.eql(u8, func.parameters[0].value, "x"));
            const body_str = try func.body.string();
            try testing.expect(std.mem.eql(u8, body_str, "(x + 2)"));
        },
        else => try testing.expect(false),
    }
}

// test "test function application" {
//     const tests = [_]struct {
//         input: []const u8,
//         expected: i64,
//     }{
//         .{ .input = "let identity = fn(x) { x; }; identity(5);", .expected = 5 },
//         .{ .input = "let identity = fn(x) { return x; }; identity(5);", .expected = 5 },
//         .{ .input = "let double = fn(x) { x * 2; }; double(5);", .expected = 10 },
//         .{ .input = "let add = fn(x, y) { x + y; }; add(5, 5);", .expected = 10 },
//         .{ .input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", .expected = 20 },
//         .{ .input = "fn(x) { x; }(5)", .expected = 5 },
//     };

//     for (tests) |tt| {
//         const allocator = std.testing.allocator;
//         const l = try Lexer.init(allocator, tt.input);
//         defer allocator.destroy(l);

//         const p = try Parser.init(allocator, l);
//         defer p.deinit();

//         try p.parseProgram();

//         const env = try Environment.init(allocator);
//         defer env.deinit(allocator);

//         const evaluated = try eval(allocator, p.program.node(), env);
//         defer evaluated.?.deinit(allocator);
//         try testing.expect(testIntegerObject(evaluated.?, tt.expected));
//     }
// }
