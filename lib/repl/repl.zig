const std = @import("std");
const lexer = @import("lexer");
const parser = @import("parser");
const ast = @import("ast");
const Evaluator = @import("evaluator").Evaluator;
const Environment = @import("object").Environment;

const prompt = ">> ";

pub fn start(
    allocator: std.mem.Allocator,
) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const env = try Environment.init(allocator);
    defer env.deinit(allocator);

    const evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    var lines = std.ArrayList([]const u8).init(allocator);
    defer {
        for (lines.items) |line| {
            allocator.free(line);
        }
        lines.deinit();
    }

    var programs = std.ArrayList(*ast.Program).init(allocator);
    defer {
        for (programs.items) |program| {
            program.deinit();
        }
        programs.deinit();
    }

    var buf: [1024]u8 = undefined;
    while (true) {
        try stdout.print("{s}", .{prompt});
        if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            if (line.len == 0) break;

            const owned_line = try allocator.dupe(u8, line);
            try lines.append(owned_line);

            const l = try lexer.Lexer.init(allocator, owned_line);
            defer allocator.destroy(l);

            const p = try parser.Parser.init(allocator, l);
            defer p.deinit();

            const program = try p.parseProgram();
            try programs.append(program);

            if (p.errors.items.len > 0) {
                try stdout.print("Woops! We ran into some errors here!\n", .{});
                try stdout.print("Parser errors:\n", .{});
                for (p.errors.items) |err| {
                    try stdout.print("\t{s}\n", .{err});
                }
            }

            const eval_result = try evaluator.eval(program.node(), env);
            if (eval_result) |obj| {
                const inspect = try obj.inspect(allocator);
                defer allocator.free(inspect);
                try stdout.print("{s}\n", .{inspect});
            } else {
                try stdout.print("Evaluation returned null.\n", .{});
            }
        }
    }
}
