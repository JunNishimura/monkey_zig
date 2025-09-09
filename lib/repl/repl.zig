const std = @import("std");
const lexer = @import("lexer");
const parser = @import("parser");
const evaluator = @import("evaluator");
const Environment = @import("object").Environment;

const prompt = ">> ";

pub fn start(
    allocator: std.mem.Allocator,
) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const env = try Environment.init(allocator);
    defer env.deinit(allocator);

    while (true) {
        var buf: [1024]u8 = undefined;
        try stdout.print("{s}", .{prompt});
        if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            if (line.len == 0) break;

            const l = try lexer.Lexer.init(allocator, line);
            defer allocator.destroy(l);

            const p = try parser.Parser.init(allocator, l);
            defer p.deinit();

            try p.parseProgram();
            const program = p.program;

            if (p.errors.items.len > 0) {
                try stdout.print("Woops! We ran into some errors here!\n", .{});
                try stdout.print("Parser errors:\n", .{});
                for (p.errors.items) |err| {
                    try stdout.print("\t{s}\n", .{err});
                }
            }

            const eval_result = try evaluator.eval(allocator, program.node(), env);
            if (eval_result) |obj| {
                const inspect = try obj.inspect(allocator);
                defer allocator.free(inspect);
                defer obj.deinit(allocator);
                try stdout.print("{s}\n", .{inspect});
            } else {
                try stdout.print("Evaluation returned null.\n", .{});
            }
        }
    }
}
