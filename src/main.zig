const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);

    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    const arg0 = args.next() orelse
        return error.NoArg0;

    var filename: ?[]const u8 = null;
    var logging = false;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help")) {
            printUsage(arg0, null) catch unreachable;
            return;
        } else if (std.mem.eql(u8, arg, "--log")) {
            if (!logging) {
                logging = true;
            } else {
                try printUsage(arg0, error.RepeatedOption);
            }
        } else {
            if (filename == null) {
                filename = arg;
            } else {
                try printUsage(arg0, error.TooManyFiles);
            }
        }
    }

    if (filename == null) {
        try printUsage(arg0, error.NoFileGiven);
    }

    const file = try std.fs.cwd().openFile(filename.?, .{});
    defer file.close();

    const waterclocks = try parseFile(file, allocator);
    defer deinitWaterclocks(waterclocks, allocator);

    try interpret(waterclocks, logging, allocator);
}

/// `allocator` must be the same allocator was given to `parseFile`
fn deinitWaterclocks(waterclocks: []Waterclock, allocator: std.mem.Allocator) void {
    for (waterclocks) |*waterclock| {
        waterclock.deinit(allocator);
    }

    allocator.free(waterclocks);
}

fn printUsage(arg0: []const u8, optional_err: ?anyerror) !void {
    const writer = if (optional_err == null)
        std.io.getStdOut().writer()
    else
        std.io.getStdErr().writer();

    const usage =
        \\Usage:
        \\{s} [--help] [--log] <file>
        \\
        \\Options:
        \\    --help          Print this message and exit
        \\    --log           Print each waterclock value every time one zeroes
        \\
        \\The given file must be in the matrix json format defined at
        \\https://esolangs.org/wiki/The_Waterfall_Model#Syntax
        \\
        \\
    ;

    try writer.print(usage, .{arg0});

    if (optional_err) |err| {
        return err;
    }
}

const Waterclock = struct {
    // Mutable is too annoying to work with
    value: std.math.big.int.Managed,
    zero_triggers: []const std.math.big.int.Const,

    fn deinit(self: *Waterclock, allocator: std.mem.Allocator) void {
        for (self.zero_triggers) |zero_trigger| {
            allocator.free(zero_trigger.limbs);
        }

        allocator.free(self.zero_triggers);
        allocator.free(self.value.limbs);

        // for catching use-after-free in debug
        self.value = undefined;
        self.zero_triggers = undefined;
        self.* = undefined;
    }
};

/// Caller owns returned Waterclocks as well as the slice and must deinit them
/// with `Waterclock.deinit` and free the returned slice
fn parseFile(file: std.fs.File, allocator: std.mem.Allocator) ![]Waterclock {
    var json_reader = std.json.reader(allocator, file.reader());
    defer json_reader.deinit();

    const json_matrix_form = try std.json.parseFromTokenSource([]const []const usize, allocator, &json_reader, .{});
    defer json_matrix_form.deinit();

    const matrix_form = json_matrix_form.value;
    try validateMatrix(matrix_form);

    const waterclock_count = matrix_form[0][1];
    const waterclocks = try allocator.alloc(Waterclock, waterclock_count);

    for (matrix_form[1..], 0..) |row, row_index| {
        const zero_triggers = try allocator.alloc(std.math.big.int.Const, waterclock_count);

        for (row[1..], 0..) |cell, cell_index| {
            var zero_cell = try std.math.big.int.Managed.initSet(allocator, cell);
            defer zero_cell.deinit();

            // reimpliment `Managed.toConst` because i need to dupe the limbs
            zero_triggers[cell_index] = .{
                .positive = zero_cell.isPositive(),
                .limbs = try allocator.dupe(std.math.big.Limb, zero_cell.limbs[0..zero_cell.len()]),
            };
        }

        const waterclock: Waterclock = .{
            .value = try std.math.big.int.Managed.initSet(allocator, row[0]),
            .zero_triggers = zero_triggers,
        };

        waterclocks[row_index] = waterclock;
    }
    return waterclocks;
}

const MatrixError = error{
    InvalidTopLeft,
    InvalidTopRow,
    InvalidRowCount,
    InvalidColumnCount,
    InvalidZeroTriggers,
    InvalidInitalValue,
};

/// returns an error from MatrixError if matrix does not fit the spec
fn validateMatrix(matrix: []const []const usize) MatrixError!void {
    var max_value: usize = matrix[1][0];

    for (matrix[1..]) |row| {
        for (row) |cell| {
            if (cell > max_value) {
                max_value = row[0];
            }
        }
    }

    if (matrix[0][0] <= max_value) {
        return MatrixError.InvalidTopLeft;
    }

    const top_row_value = matrix[0][1];
    // start from index 2 because we are comparing to index 1
    for (matrix[0][2..]) |cell| {
        if (cell != top_row_value) {
            return MatrixError.InvalidTopRow;
        }
    }

    if (matrix[1..].len != top_row_value) {
        return MatrixError.InvalidRowCount;
    }

    for (matrix) |row| {
        if (row[1..].len != top_row_value) {
            return MatrixError.InvalidColumnCount;
        }
    }

    for (matrix[0..], 0..) |row, index| {
        if (index == 0) {
            continue;
        }

        if (row[index] == 0) {
            for (row[1..]) |cell| {
                if (cell != 0) {
                    return MatrixError.InvalidZeroTriggers;
                }
            }
        }
    }

    for (matrix[1..]) |row| {
        if (row[0] == 0) {
            return MatrixError.InvalidInitalValue;
        }
    }
}

fn interpret(
    waterclocks: []Waterclock,
    logging: bool,
    allocator: std.mem.Allocator,
) !void {
    const stdout = std.io.getStdOut().writer();

    var output_counter = try std.math.big.int.Managed.init(allocator);
    defer output_counter.deinit();

    while (true) {
        var lowest_clock: ?Waterclock = null;
        var lowest_index: usize = undefined;

        defer if (lowest_clock) |*lowest| {
            lowest.value.deinit();
        };

        for (waterclocks, 0..) |waterclock, index| {
            if (lowest_clock == null or
                lowest_clock.?.value.toConst().order(waterclock.value.toConst()) == .gt)
            {
                if (lowest_clock) |*lowest| lowest.value.deinit();

                lowest_clock = Waterclock{
                    .value = try waterclock.value.clone(),
                    .zero_triggers = waterclock.zero_triggers,
                };
                lowest_index = index;
            }
        }

        if (lowest_clock.?.zero_triggers[lowest_index].eqlZero()) {
            // `validateMatrix` already made sure zero triggers are valid
            // so we do not have to check here

            return;
        }

        for (waterclocks, 0..) |waterclock, index| {
            if (index != lowest_index and
                !waterclock.zero_triggers[index].eqlZero())
            {
                break;
            }
        } else {
            const value = waterclocks[lowest_index].zero_triggers[lowest_index];
            if (value.orderAgainstScalar(7) == .eq) {
                try output_counter.addScalar(&output_counter, 1);
            } else if (value.orderAgainstScalar(8) == .eq) {
                const string_value = try output_counter.toString(allocator, 10, .lower);
                defer allocator.free(string_value);

                try stdout.print("{s}\n", .{string_value});

                try output_counter.set(0);
            } else if (value.orderAgainstScalar(9) == .eq) {
                if (output_counter.toConst().orderAgainstScalar(256) == .lt) {
                    try stdout.writeByte(output_counter.to(u8) catch unreachable);

                    try output_counter.set(0);
                } else {
                    return error.CharacterOver255;
                }
            }
        }

        for (waterclocks, 0..) |*waterclock, index| {
            try waterclock.value.sub(
                &waterclock.value,
                &lowest_clock.?.value,
            );

            var managed_zero_trigger = try lowest_clock.?.zero_triggers[index].toManaged(allocator);
            defer managed_zero_trigger.deinit();

            try waterclock.value.add(
                &waterclock.value,
                &managed_zero_trigger,
            );
        }

        if (logging) {
            try logWaterclocks(waterclocks, allocator);
        }
    }
}

fn logWaterclocks(waterclocks: []Waterclock, allocator: std.mem.Allocator) !void {
    const stderr = std.io.getStdErr().writer();

    for (waterclocks) |waterclock| {
        const string_value = try waterclock.value.toConst().toStringAlloc(allocator, 10, .lower);
        defer allocator.free(string_value);
        try stderr.print("{s} ", .{string_value});
    }
    try stderr.writeByte('\n');
}
