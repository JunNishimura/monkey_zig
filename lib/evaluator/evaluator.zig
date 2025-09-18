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
    builtins: std.StringHashMap(*obj.Builtin),

    pub fn init(allocator: std.mem.Allocator) !*Evaluator {
        const evaluator = try allocator.create(Evaluator);
        evaluator.* = .{
            .allocator = allocator,
            .result = null,
            .evaluated = std.ArrayList(obj.Object).init(allocator),
            .builtins = std.StringHashMap(*obj.Builtin).init(allocator),
        };
        try evaluator.initBuiltins();
        return evaluator;
    }

    fn initBuiltins(self: *Evaluator) EvalError!void {
        const len_builtin = try obj.Builtin.init(self.allocator, builtinLen);
        try self.builtins.put("len", len_builtin);
        const first_builtin = try obj.Builtin.init(self.allocator, builtinFirst);
        try self.builtins.put("first", first_builtin);
        const last_builtin = try obj.Builtin.init(self.allocator, builtinLast);
        try self.builtins.put("last", last_builtin);
        const rest_builtin = try obj.Builtin.init(self.allocator, builtinRest);
        try self.builtins.put("rest", rest_builtin);
        const push_builtin = try obj.Builtin.init(self.allocator, builtinPush);
        try self.builtins.put("push", push_builtin);
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
            .CallExpression => {
                const call_node: *ast.CallExpression = @ptrCast(@alignCast(node.ptr));
                const function = try self.eval(call_node.function.node, env);
                if (function.?.isError()) {
                    return function;
                }
                try self.addEvaluated(function.?);
                const args = try self.evalExpressions(self.allocator, call_node.arguments.items, env);
                defer args.deinit();

                const args_items = args.items;
                if (args_items.len == 1 and args_items[0].isError()) {
                    return args_items[0];
                }

                return try self.applyFunction(function.?, args_items);
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
            .StringLiteral => {
                const str_node: *ast.StringLiteral = @ptrCast(@alignCast(node.ptr));
                const str_obj = try obj.String.init(self.allocator, str_node.value);
                return str_obj.object();
            },
            .Boolean => {
                const bool_node: *ast.Boolean = @ptrCast(@alignCast(node.ptr));
                const bool_obj = try obj.Boolean.init(self.allocator, bool_node.value);
                return bool_obj.object();
            },
            .ArrayLiteral => {
                const array_node: *ast.ArrayLiteral = @ptrCast(@alignCast(node.ptr));
                const elements = try self.evalExpressions(self.allocator, array_node.elements.items, env);
                defer elements.deinit();

                if (elements.items.len == 1 and elements.items[0].isError()) {
                    return elements.items[0];
                }

                return (try obj.Array.init(self.allocator, elements.items)).object();
            },
            .IndexExpression => {
                const index_node: *ast.IndexExpression = @ptrCast(@alignCast(node.ptr));
                const left = try self.eval(index_node.left.node, env);
                if (left) |l| {
                    if (l.isError()) {
                        return l;
                    }
                    try self.addEvaluated(l);
                    const index = try self.eval(index_node.index.?.node, env);
                    if (index) |i| {
                        if (i.isError()) {
                            return i;
                        }
                        try self.addEvaluated(i);
                        return try self.evalIndexExpression(l, i);
                    }
                }
            },
            else => return null,
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
        } else if (left.getType() == .string and right.getType() == .string) {
            const left_str: *obj.String = @ptrCast(@alignCast(left.ptr));
            const right_str: *obj.String = @ptrCast(@alignCast(right.ptr));
            return try self.evalStringInfixExpression(operator, left_str, right_str);
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
        return try self.newError("unknown operator: {s} {s} {s}", .{ @tagName(left.getType()), operator, @tagName(right.getType()) });
    }

    fn evalStringInfixExpression(self: *Evaluator, operator: []const u8, left: *obj.String, right: *obj.String) !?obj.Object {
        if (std.mem.eql(u8, operator, "+")) {
            const new_str = try std.mem.concat(self.allocator, u8, &[_][]const u8{ left.value, right.value });
            defer self.allocator.free(new_str);
            return (try obj.String.init(self.allocator, new_str)).object();
        }
        return try self.newError("unknown operator: {s} {s} {s}", .{ @tagName(left.getType()), operator, @tagName(right.getType()) });
    }

    fn evalIdentifier(self: *Evaluator, node: ast.Node, env: *Environment) EvalError!obj.Object {
        const ident_node: *ast.Identifier = @ptrCast(@alignCast(node.ptr));
        if (env.get(ident_node.value)) |val| {
            val.setIsIdent(true);
            return val;
        }
        if (self.builtins.get(ident_node.value)) |builtin| {
            return builtin.object();
        }

        return try self.newError("identifier not found: {s}", .{ident_node.value});
    }

    fn evalExpressions(self: *Evaluator, allocator: std.mem.Allocator, expressions: []ast.Expression, env: *Environment) !std.ArrayList(obj.Object) {
        var result = std.ArrayList(obj.Object).init(allocator);

        for (expressions) |expr| {
            const evaluated = try self.eval(expr.node, env);
            if (evaluated.?.isError()) {
                result.deinit(); // cleanup before returning
                try result.append(evaluated.?);
                return result;
            }
            try result.append(evaluated.?);
            try self.addEvaluated(evaluated.?);
        }

        return result;
    }

    fn evalIndexExpression(self: *Evaluator, left: obj.Object, index: obj.Object) EvalError!?obj.Object {
        if (left.getType() == .array_obj and index.getType() == .integer) {
            const array: *obj.Array = @ptrCast(@alignCast(left.ptr));
            const int_index: *obj.Integer = @ptrCast(@alignCast(index.ptr));
            return try self.evalArrayIndexExpression(array, int_index);
        }
        return try self.newError("index operator not supported: {s}", .{@tagName(left.getType())});
    }

    fn evalArrayIndexExpression(self: *Evaluator, array: *obj.Array, index: *obj.Integer) EvalError!?obj.Object {
        const max: i64 = @intCast(array.elements.len);
        if (index.value < 0 or index.value >= max) {
            return (try obj.Null.init(self.allocator)).object();
        }

        const elem = array.elements[@intCast(index.value)];

        switch (elem.getType()) {
            .integer => {
                const int_obj: *obj.Integer = @ptrCast(@alignCast(elem.ptr));
                return (try obj.Integer.init(self.allocator, int_obj.value)).object();
            },
            .string => {
                const str_obj: *obj.String = @ptrCast(@alignCast(elem.ptr));
                return (try obj.String.init(self.allocator, str_obj.value)).object();
            },
            .boolean => {
                const bool_obj: *obj.Boolean = @ptrCast(@alignCast(elem.ptr));
                return (try obj.Boolean.init(self.allocator, bool_obj.value)).object();
            },
            .null => {
                return (try obj.Null.init(self.allocator)).object();
            },
            else => return try self.newError("unknown element type: {s}", .{@tagName(elem.getType())}),
        }
    }

    fn applyFunction(self: *Evaluator, function: obj.Object, args: []obj.Object) EvalError!?obj.Object {
        switch (function.getType()) {
            .function_obj => {
                const func: *Function = @ptrCast(@alignCast(function.ptr));
                const extended_env = try extendFunctionEnv(self.allocator, func, args);

                const evaluated = try self.eval(func.body.statement().node, extended_env);
                if (evaluated.?.getType() == .return_obj) {
                    try self.addEvaluated(evaluated.?);
                }
                if (evaluated.?.getType() == .function_obj) {
                    const func_obj: *Function = @ptrCast(@alignCast(evaluated.?.ptr));
                    func_obj.extended_env = extended_env;
                } else {
                    extended_env.deinit(self.allocator);
                }

                return unwrapReturnValue(evaluated);
            },
            .builtin_obj => {
                const builtin: *obj.Builtin = @ptrCast(@alignCast(function.ptr));
                return try builtin.func(self.allocator, args);
            },
            else => return try self.newError("not a function: {s}", .{@tagName(function.getType())}),
        }
    }

    fn extendFunctionEnv(allocator: std.mem.Allocator, func: *Function, args: []obj.Object) !*Environment {
        const env = try Environment.initEnclosed(allocator, func.env);

        for (func.parameters, 0..) |param, i| {
            try env.set(param.value, args[i]);
        }

        return env;
    }

    fn unwrapReturnValue(object: ?obj.Object) ?obj.Object {
        if (object) |o| {
            if (o.getType() == .return_obj) {
                var return_obj: *obj.Return = @ptrCast(@alignCast(o.ptr));
                const value = return_obj.value.?;
                return_obj.value = null; // Prevent double free
                return value;
            }
            return o;
        }
        return null;
    }

    fn newError(self: *Evaluator, comptime format: []const u8, args: anytype) !obj.Object {
        const error_obj = try obj.Error.init(self.allocator, format, args);
        return error_obj.object();
    }

    pub fn deinit(self: *Evaluator) void {
        for (self.evaluated.items) |object| {
            object.deinit();
        }

        var it = self.builtins.valueIterator();
        while (it.next()) |builtin| {
            builtin.*.deinit();
        }
        self.builtins.deinit();

        self.evaluated.deinit();
        self.allocator.destroy(self);
    }

    fn addEvaluated(self: *Evaluator, object: obj.Object) EvalError!void {
        if (object.isIdent() or object.getType() == .builtin_obj) {
            return;
        }
        try self.evaluated.append(object);
    }
};

fn builtinLen(allocator: std.mem.Allocator, args: []obj.Object) EvalError!?obj.Object {
    if (args.len != 1) {
        return (try obj.Error.init(allocator, "wrong number of arguments. got={d}, want=1", .{args.len})).object();
    }

    switch (args[0].getType()) {
        .string => {
            const str_obj: *obj.String = @ptrCast(@alignCast(args[0].ptr));
            return (try obj.Integer.init(allocator, @intCast(str_obj.value.len))).object();
        },
        .array_obj => {
            const array_obj: *obj.Array = @ptrCast(@alignCast(args[0].ptr));
            return (try obj.Integer.init(allocator, @intCast(array_obj.elements.len))).object();
        },
        else => {
            return (try obj.Error.init(allocator, "argument to `len` not supported, got {s}", .{@tagName(args[0].getType())})).object();
        },
    }
}

fn builtinFirst(allocator: std.mem.Allocator, args: []obj.Object) EvalError!?obj.Object {
    if (args.len != 1) {
        return (try obj.Error.init(allocator, "wrong number of arguments. got={d}, want=1", .{args.len})).object();
    }

    if (args[0].getType() != .array_obj) {
        return (try obj.Error.init(allocator, "argument to `first` must be ARRAY, got {s}", .{@tagName(args[0].getType())})).object();
    }

    const array: *obj.Array = @ptrCast(@alignCast(args[0].ptr));
    if (array.elements.len > 0) {
        const first = array.elements[0];
        switch (first.getType()) {
            .integer => {
                const int_obj: *obj.Integer = @ptrCast(@alignCast(first.ptr));
                return (try obj.Integer.init(allocator, int_obj.value)).object();
            },
            .string => {
                const str_obj: *obj.String = @ptrCast(@alignCast(first.ptr));
                return (try obj.String.init(allocator, str_obj.value)).object();
            },
            .boolean => {
                const bool_obj: *obj.Boolean = @ptrCast(@alignCast(first.ptr));
                return (try obj.Boolean.init(allocator, bool_obj.value)).object();
            },
            .null => {
                return (try obj.Null.init(allocator)).object();
            },
            else => return (try obj.Error.init(allocator, "unknown element type: {s}", .{@tagName(first.getType())})).object(),
        }
    }

    return (try obj.Null.init(allocator)).object();
}

fn builtinLast(allocator: std.mem.Allocator, args: []obj.Object) EvalError!?obj.Object {
    if (args.len != 1) {
        return (try obj.Error.init(allocator, "wrong number of arguments. got={d}, want=1", .{args.len})).object();
    }

    if (args[0].getType() != .array_obj) {
        return (try obj.Error.init(allocator, "argument to `last` must be ARRAY, got {s}", .{@tagName(args[0].getType())})).object();
    }

    const array: *obj.Array = @ptrCast(@alignCast(args[0].ptr));
    if (array.elements.len > 0) {
        const last = array.elements[array.elements.len - 1];
        switch (last.getType()) {
            .integer => {
                const int_obj: *obj.Integer = @ptrCast(@alignCast(last.ptr));
                return (try obj.Integer.init(allocator, int_obj.value)).object();
            },
            .string => {
                const str_obj: *obj.String = @ptrCast(@alignCast(last.ptr));
                return (try obj.String.init(allocator, str_obj.value)).object();
            },
            .boolean => {
                const bool_obj: *obj.Boolean = @ptrCast(@alignCast(last.ptr));
                return (try obj.Boolean.init(allocator, bool_obj.value)).object();
            },
            .null => {
                return (try obj.Null.init(allocator)).object();
            },
            else => return (try obj.Error.init(allocator, "unknown element type: {s}", .{@tagName(last.getType())})).object(),
        }
    }

    return (try obj.Null.init(allocator)).object();
}

fn builtinRest(allocator: std.mem.Allocator, args: []obj.Object) EvalError!?obj.Object {
    if (args.len != 1) {
        return (try obj.Error.init(allocator, "wrong number of arguments. got={d}, want=1", .{args.len})).object();
    }

    if (args[0].getType() != .array_obj) {
        return (try obj.Error.init(allocator, "argument to `rest` must be ARRAY, got {s}", .{@tagName(args[0].getType())})).object();
    }

    const array: *obj.Array = @ptrCast(@alignCast(args[0].ptr));
    if (array.elements.len > 0) {
        const new_elements = array.elements[1..];
        return (try obj.Array.init(allocator, new_elements)).object();
    }

    return (try obj.Null.init(allocator)).object();
}

fn builtinPush(allocator: std.mem.Allocator, args: []obj.Object) EvalError!?obj.Object {
    if (args.len != 2) {
        return (try obj.Error.init(allocator, "wrong number of arguments. got={d}, want=2", .{args.len})).object();
    }

    if (args[0].getType() != .array_obj) {
        return (try obj.Error.init(allocator, "argument to `push` must be ARRAY, got {s}", .{@tagName(args[0].getType())})).object();
    }

    const array: *obj.Array = @ptrCast(@alignCast(args[0].ptr));
    const new_elements = try std.mem.concat(allocator, obj.Object, &[_][]const obj.Object{ array.elements, args[1..] });
    defer allocator.free(new_elements);

    return (try obj.Array.init(allocator, new_elements)).object();
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
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(program.node(), env);

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

test "eval string expression" {
    const input = "\"hello world\"";

    const allocator = std.testing.allocator;

    const l = try Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();

    const env = try Environment.init(allocator);
    defer env.deinit(allocator);

    const evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    const evaluated = try evaluator.eval(program.node(), env);
    const str_obj: *obj.String = @ptrCast(@alignCast(evaluated.?.ptr));

    try testing.expect(std.mem.eql(u8, str_obj.value, "hello world"));
}

test "eval string concatenation" {
    const input = "\"hello\" + \" \" + \"world\"";

    const allocator = std.testing.allocator;
    const l = try Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();

    const env = try Environment.init(allocator);
    defer env.deinit(allocator);

    const evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    const evaluated = try evaluator.eval(program.node(), env);
    const str_obj: *obj.String = @ptrCast(@alignCast(evaluated.?.ptr));

    try testing.expect(std.mem.eql(u8, str_obj.value, "hello world"));
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

        const program = try p.parseProgram();
        defer program.deinit();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(program.node(), env);
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

        const program = try p.parseProgram();
        defer program.deinit();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(program.node(), env);
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

        const program = try p.parseProgram();
        defer program.deinit();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(program.node(), env);
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

        const program = try p.parseProgram();
        defer program.deinit();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(program.node(), env);
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
        .{ .input = "foobar", .expected_message = "identifier not found: foobar" },
        .{ .input = "\"Hello\" - \"World\"", .expected_message = "unknown operator: string - string" },
    };

    for (tests) |tt| {
        const allocator = std.testing.allocator;
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(program.node(), env);
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

        const program = try p.parseProgram();
        defer program.deinit();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(program.node(), env);
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

    const program = try p.parseProgram();
    defer program.deinit();

    const env = try Environment.init(allocator);
    defer env.deinit(allocator);

    const evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    const evaluated = try evaluator.eval(program.node(), env);
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

test "test function application" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let identity = fn(x) { x; }; identity(5);", .expected = 5 },
        .{ .input = "let identity = fn(x) { return x; }; identity(5);", .expected = 5 },
        .{ .input = "let double = fn(x) { x * 2; }; double(5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5, 5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", .expected = 20 },
        .{ .input = "fn(x) { x; }(5)", .expected = 5 },
    };

    for (tests) |tt| {
        const allocator = std.testing.allocator;
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(program.node(), env);
        try testing.expect(testIntegerObject(evaluated.?, tt.expected));
    }
}

const TestBuiltinFunction = union(enum) {
    int: i64,
    str: []const u8,
};

test "test builtin functions" {
    const tests = [_]struct {
        input: []const u8,
        expected: TestBuiltinFunction,
    }{
        .{ .input = "len(\"\")", .expected = .{ .int = 0 } },
        .{ .input = "len(\"four\")", .expected = .{ .int = 4 } },
        .{ .input = "len(\"hello world\")", .expected = .{ .int = 11 } },
        .{ .input = "len(1)", .expected = .{ .str = "argument to `len` not supported, got integer" } },
        .{ .input = "len(\"one\", \"two\")", .expected = .{ .str = "wrong number of arguments. got=2, want=1" } },
    };

    for (tests) |tt| {
        const allocator = std.testing.allocator;
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(program.node(), env);
        switch (tt.expected) {
            .int => {
                try testing.expect(testIntegerObject(evaluated.?, tt.expected.int));
            },
            .str => {
                switch (evaluated.?.getType()) {
                    .error_obj => {
                        const err_obj: *obj.Error = @ptrCast(@alignCast(evaluated.?.ptr));
                        try testing.expect(std.mem.eql(u8, err_obj.message, tt.expected.str));
                    },
                    else => try testing.expect(false),
                }
            },
        }
    }
}

test "test array literals" {
    const input = "[1, 2 * 2, 3 + 3]";

    const allocator = std.testing.allocator;

    const l = try Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();

    const env = try Environment.init(allocator);
    defer env.deinit(allocator);

    const evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    const evaluated = try evaluator.eval(program.node(), env);
    switch (evaluated.?.getType()) {
        .array_obj => {
            const array_obj: *obj.Array = @ptrCast(@alignCast(evaluated.?.ptr));
            try testing.expect(array_obj.elements.len == 3);
            try testing.expect(testIntegerObject(array_obj.elements[0], 1));
            try testing.expect(testIntegerObject(array_obj.elements[1], 4));
            try testing.expect(testIntegerObject(array_obj.elements[2], 6));
        },
        else => try testing.expect(false),
    }
}

test "test array index expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: ?i64,
    }{
        .{ .input = "[1, 2, 3][0]", .expected = 1 },
        .{ .input = "[1, 2, 3][1]", .expected = 2 },
        .{ .input = "[1, 2, 3][2]", .expected = 3 },
        .{ .input = "let i = 0; [1][i];", .expected = 1 },
        .{ .input = "[1, 2, 3][1 + 1];", .expected = 3 },
        .{ .input = "let myArray = [1, 2, 3]; myArray[2];", .expected = 3 },
        .{ .input = "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", .expected = 6 },
        .{ .input = "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", .expected = 2 },
        .{ .input = "[1, 2, 3][3]", .expected = null },
        .{ .input = "[1, 2, 3][-1]", .expected = null },
    };

    for (tests) |tt| {
        const allocator = std.testing.allocator;
        const l = try Lexer.init(allocator, tt.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();

        const env = try Environment.init(allocator);
        defer env.deinit(allocator);

        const evaluator = try Evaluator.init(allocator);
        defer evaluator.deinit();

        const evaluated = try evaluator.eval(program.node(), env);
        if (tt.expected) |expected| {
            try testing.expect(testIntegerObject(evaluated.?, expected));
        } else {
            try testing.expect(testNullObject(evaluated.?));
        }
    }
}
