const std = @import("std");
const repl = @import("repl");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    std.debug.print("Hello! This is the Monkey programming language!\n", .{});
    std.debug.print("Feel free to type in commands\n", .{});

    try repl.start(allocator);
}
