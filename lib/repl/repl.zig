const std = @import("std");
const lexer = @import("lexer");

const prompt = ">> ";

pub fn start(
    allocator: std.mem.Allocator,
) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        var buf: [1024]u8 = undefined;
        try stdout.print("{s}", .{prompt});
        if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            if (line.len == 0) break;

            const l = try lexer.Lexer.init(allocator, line);
            defer allocator.destroy(l);

            while (true) {
                const token = l.NextToken();
                if (token.type == .Eof) break;

                try stdout.print("{any}\n", .{token});
            }
        }
    }
}
