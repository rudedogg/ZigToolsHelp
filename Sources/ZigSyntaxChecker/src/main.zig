const std = @import("std");
const testing = std.testing;
const ast = std.zig.ast;
const zig = std.zig;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;
export fn add(a: i32, b: i32) i32 {
    return a + b;
}
// export fn parse(source: []const u8) i32 {
// export fn parse(source: [*]const u8, len: usize) i32 {
//     return 5;
// }

// const Data = extern struct { a: i32, b: u8, c: f32, d: bool, e: bool };
// const Data = extern struct { a: zig.Token.Tag, b: u8, c: f32, d: bool, e: bool };
// const ErrorData = extern struct { tag: ast.Error.Tag, line: usize, column: usize, line_start: usize, line_end: usize };
const MyData = extern struct { line_end: usize };

export fn parse2() MyData {
    const stderr = std.io.getStdErr().writer();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    const source =
        \\const x = "" ++
        \\    \\ hi
        \\;
        \\
    ;

    // st.zig.parse(gpa: allocator, source: []const u8)
    if (std.zig.parse(allocator, source)) |tree| {
        // defer tree.deinit(allocator);
        // var list = ArrayList(MyData).init(test_allocator);
        // defer list.deinit();
        // try list.append('H');
        // var errors: []ErrorData =[];
        for (tree.errors) |parse_error, index| {
            // try list.append(MyData{ .line_end = 5 });
            return MyData{ .line_end = 5 };
        }

        // const token_start = tree.tokens.items(.start)[parse_error.token];
        // const loc = tree.tokenLocation(0, parse_error.token);
        // try stderr.print("(memory buffer):{d}:{d}: error: ", .{ loc.line + 1, loc.column + 1 });
        // try tree.renderError(parse_error, stderr);
        // try stderr.print("\n{s}\n", .{source[loc.line_start..loc.line_end]});
        //     {
        //         var i: usize = 0;
        //         while (i < loc.column) : (i += 1) {
        //             try stderr.writeAll(" ");
        //         }
        //         try stderr.writeAll("^");
        //     }
        //     try stderr.writeAll("\n");
        // }
        // if (tree.errors.len != 0) {
        //     return error.ParseError;
        // }

        // const formatted = try tree.render(allocator);
        // anything_changed.* = !mem.eql(u8, formatted, source);
        // try stderr.writeAll(formatted);
        // return formatted;
        // return list;
        // return list.items[0];
    } else |myError| {
        return MyData{ .line_end = 5 };
    }
    return MyData{ .line_end = 5 };
}

export fn thing() Tag {
    return .test_decl;
}
export fn getLocation() Location {
    const location = Location{ .line = 5, .column = 5, .line_start = 0, .line_end = 10 };
    return location;
}

// test "parsing" {
//     const stderr = std.io.getStdErr().writer();
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//     var allocator = &arena.allocator;

//     const source =
//         \\const x = "" ++
//         \\    \\ hi
//         \\;
//         \\
//     ;

//     // st.zig.parse(gpa: allocator, source: []const u8)
//     var tree = try std.zig.parse(allocator, source);
//     defer tree.deinit(allocator);
//     var list = ArrayList(MyData).init(test_allocator);
//     defer list.deinit();
//     // try list.append('H');
//     // var errors: []ErrorData =[];
//     for (tree.errors) |parse_error, index| {
//         try list.append(ErrorData{ .line_end = 5 });
//         const token_start = tree.tokens.items(.start)[parse_error.token];
//         const loc = tree.tokenLocation(0, parse_error.token);
//         try stderr.print("(memory buffer):{d}:{d}: error: ", .{ loc.line + 1, loc.column + 1 });
//         try tree.renderError(parse_error, stderr);
//         try stderr.print("\n{s}\n", .{source[loc.line_start..loc.line_end]});
//         {
//             var i: usize = 0;
//             while (i < loc.column) : (i += 1) {
//                 try stderr.writeAll(" ");
//             }
//             try stderr.writeAll("^");
//         }
//         try stderr.writeAll("\n");
//     }
//     if (tree.errors.len != 0) {
//         return error.ParseError;
//     }

//     const formatted = try tree.render(allocator);
//     // anything_changed.* = !mem.eql(u8, formatted, source);
//     try stderr.writeAll(formatted);
//     // return formatted;
// }

const Location = extern struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

pub const Tag = extern enum {
    /// sub_list[lhs...rhs]
    root,
    /// `usingnamespace lhs;`. rhs unused. main_token is `usingnamespace`.
    @"usingnamespace",
    /// lhs is test name token (must be string literal), if any.
    /// rhs is the body node.
    test_decl,
    /// lhs is the index into extra_data.
    /// rhs is the initialization expression, if any.
    /// main_token is `var` or `const`.
    global_var_decl,
    /// `var a: x align(y) = rhs`
    /// lhs is the index into extra_data.
    /// main_token is `var` or `const`.
    local_var_decl,
    /// `var a: lhs = rhs`. lhs and rhs may be unused.
    /// Can be local or global.
    /// main_token is `var` or `const`.
    simple_var_decl,
    /// `var a align(lhs) = rhs`. lhs and rhs may be unused.
    /// Can be local or global.
    /// main_token is `var` or `const`.
    aligned_var_decl,
    /// lhs is the identifier token payload if any,
    /// rhs is the deferred expression.
    @"errdefer",
    /// lhs is unused.
    /// rhs is the deferred expression.
    @"defer",
    /// lhs catch rhs
    /// lhs catch |err| rhs
    /// main_token is the `catch` keyword.
    /// payload is determined by looking at the next token after the `catch` keyword.
    @"catch",
    /// `lhs.a`. main_token is the dot. rhs is the identifier token index.
    field_access,
    /// `lhs.?`. main_token is the dot. rhs is the `?` token index.
    unwrap_optional,
    /// `lhs == rhs`. main_token is op.
    equal_equal,
    /// `lhs != rhs`. main_token is op.
    bang_equal,
    /// `lhs < rhs`. main_token is op.
    less_than,
    /// `lhs > rhs`. main_token is op.
    greater_than,
    /// `lhs <= rhs`. main_token is op.
    less_or_equal,
    /// `lhs >= rhs`. main_token is op.
    greater_or_equal,
    /// `lhs *= rhs`. main_token is op.
    assign_mul,
    /// `lhs /= rhs`. main_token is op.
    assign_div,
    /// `lhs *= rhs`. main_token is op.
    assign_mod,
    /// `lhs += rhs`. main_token is op.
    assign_add,
    /// `lhs -= rhs`. main_token is op.
    assign_sub,
    /// `lhs <<= rhs`. main_token is op.
    assign_bit_shift_left,
    /// `lhs >>= rhs`. main_token is op.
    assign_bit_shift_right,
    /// `lhs &= rhs`. main_token is op.
    assign_bit_and,
    /// `lhs ^= rhs`. main_token is op.
    assign_bit_xor,
    /// `lhs |= rhs`. main_token is op.
    assign_bit_or,
    /// `lhs *%= rhs`. main_token is op.
    assign_mul_wrap,
    /// `lhs +%= rhs`. main_token is op.
    assign_add_wrap,
    /// `lhs -%= rhs`. main_token is op.
    assign_sub_wrap,
    /// `lhs = rhs`. main_token is op.
    assign,
    /// `lhs || rhs`. main_token is the `||`.
    merge_error_sets,
    /// `lhs * rhs`. main_token is the `*`.
    mul,
    /// `lhs / rhs`. main_token is the `/`.
    div,
    /// `lhs % rhs`. main_token is the `%`.
    mod,
    /// `lhs ** rhs`. main_token is the `**`.
    array_mult,
    /// `lhs *% rhs`. main_token is the `*%`.
    mul_wrap,
    /// `lhs + rhs`. main_token is the `+`.
    add,
    /// `lhs - rhs`. main_token is the `-`.
    sub,
    /// `lhs ++ rhs`. main_token is the `++`.
    array_cat,
    /// `lhs +% rhs`. main_token is the `+%`.
    add_wrap,
    /// `lhs -% rhs`. main_token is the `-%`.
    sub_wrap,
    /// `lhs << rhs`. main_token is the `<<`.
    bit_shift_left,
    /// `lhs >> rhs`. main_token is the `>>`.
    bit_shift_right,
    /// `lhs & rhs`. main_token is the `&`.
    bit_and,
    /// `lhs ^ rhs`. main_token is the `^`.
    bit_xor,
    /// `lhs | rhs`. main_token is the `|`.
    bit_or,
    /// `lhs orelse rhs`. main_token is the `orelse`.
    @"orelse",
    /// `lhs and rhs`. main_token is the `and`.
    bool_and,
    /// `lhs or rhs`. main_token is the `or`.
    bool_or,
    /// `op lhs`. rhs unused. main_token is op.
    bool_not,
    /// `op lhs`. rhs unused. main_token is op.
    negation,
    /// `op lhs`. rhs unused. main_token is op.
    bit_not,
    /// `op lhs`. rhs unused. main_token is op.
    negation_wrap,
    /// `op lhs`. rhs unused. main_token is op.
    address_of,
    /// `op lhs`. rhs unused. main_token is op.
    @"try",
    /// `op lhs`. rhs unused. main_token is op.
    @"await",
    /// `?lhs`. rhs unused. main_token is the `?`.
    optional_type,
    /// `[lhs]rhs`.
    array_type,
    /// `[lhs:a]b`. `ArrayTypeSentinel[rhs]`.
    array_type_sentinel,
    /// `[*]align(lhs) rhs`. lhs can be omitted.
    /// `*align(lhs) rhs`. lhs can be omitted.
    /// `[]rhs`.
    /// main_token is the asterisk if a pointer or the lbracket if a slice
    /// main_token might be a ** token, which is shared with a parent/child
    /// pointer type and may require special handling.
    ptr_type_aligned,
    /// `[*:lhs]rhs`. lhs can be omitted.
    /// `*rhs`.
    /// `[:lhs]rhs`.
    /// main_token is the asterisk if a pointer or the lbracket if a slice
    /// main_token might be a ** token, which is shared with a parent/child
    /// pointer type and may require special handling.
    ptr_type_sentinel,
    /// lhs is index into ptr_type. rhs is the element type expression.
    /// main_token is the asterisk if a pointer or the lbracket if a slice
    /// main_token might be a ** token, which is shared with a parent/child
    /// pointer type and may require special handling.
    ptr_type,
    /// lhs is index into ptr_type_bit_range. rhs is the element type expression.
    /// main_token is the asterisk if a pointer or the lbracket if a slice
    /// main_token might be a ** token, which is shared with a parent/child
    /// pointer type and may require special handling.
    ptr_type_bit_range,
    /// `lhs[rhs..]`
    /// main_token is the lbracket.
    slice_open,
    /// `lhs[b..c]`. rhs is index into Slice
    /// main_token is the lbracket.
    slice,
    /// `lhs[b..c :d]`. rhs is index into SliceSentinel
    /// main_token is the lbracket.
    slice_sentinel,
    /// `lhs.*`. rhs is unused.
    deref,
    /// `lhs[rhs]`.
    array_access,
    /// `lhs{rhs}`. rhs can be omitted.
    array_init_one,
    /// `lhs{rhs,}`. rhs can *not* be omitted
    array_init_one_comma,
    /// `.{lhs, rhs}`. lhs and rhs can be omitted.
    array_init_dot_two,
    /// Same as `array_init_dot_two` except there is known to be a trailing comma
    /// before the final rbrace.
    array_init_dot_two_comma,
    /// `.{a, b}`. `sub_list[lhs..rhs]`.
    array_init_dot,
    /// Same as `array_init_dot` except there is known to be a trailing comma
    /// before the final rbrace.
    array_init_dot_comma,
    /// `lhs{a, b}`. `sub_range_list[rhs]`. lhs can be omitted which means `.{a, b}`.
    array_init,
    /// Same as `array_init` except there is known to be a trailing comma
    /// before the final rbrace.
    array_init_comma,
    /// `lhs{.a = rhs}`. rhs can be omitted making it empty.
    /// main_token is the lbrace.
    struct_init_one,
    /// `lhs{.a = rhs,}`. rhs can *not* be omitted.
    /// main_token is the lbrace.
    struct_init_one_comma,
    /// `.{.a = lhs, .b = rhs}`. lhs and rhs can be omitted.
    /// main_token is the lbrace.
    /// No trailing comma before the rbrace.
    struct_init_dot_two,
    /// Same as `struct_init_dot_two` except there is known to be a trailing comma
    /// before the final rbrace.
    struct_init_dot_two_comma,
    /// `.{.a = b, .c = d}`. `sub_list[lhs..rhs]`.
    /// main_token is the lbrace.
    struct_init_dot,
    /// Same as `struct_init_dot` except there is known to be a trailing comma
    /// before the final rbrace.
    struct_init_dot_comma,
    /// `lhs{.a = b, .c = d}`. `sub_range_list[rhs]`.
    /// lhs can be omitted which means `.{.a = b, .c = d}`.
    /// main_token is the lbrace.
    struct_init,
    /// Same as `struct_init` except there is known to be a trailing comma
    /// before the final rbrace.
    struct_init_comma,
    /// `lhs(rhs)`. rhs can be omitted.
    /// main_token is the lparen.
    call_one,
    /// `lhs(rhs,)`. rhs can be omitted.
    /// main_token is the lparen.
    call_one_comma,
    /// `async lhs(rhs)`. rhs can be omitted.
    async_call_one,
    /// `async lhs(rhs,)`.
    async_call_one_comma,
    /// `lhs(a, b, c)`. `SubRange[rhs]`.
    /// main_token is the `(`.
    call,
    /// `lhs(a, b, c,)`. `SubRange[rhs]`.
    /// main_token is the `(`.
    call_comma,
    /// `async lhs(a, b, c)`. `SubRange[rhs]`.
    /// main_token is the `(`.
    async_call,
    /// `async lhs(a, b, c,)`. `SubRange[rhs]`.
    /// main_token is the `(`.
    async_call_comma,
    /// `switch(lhs) {}`. `SubRange[rhs]`.
    @"switch",
    /// Same as switch except there is known to be a trailing comma
    /// before the final rbrace
    switch_comma,
    /// `lhs => rhs`. If lhs is omitted it means `else`.
    /// main_token is the `=>`
    switch_case_one,
    /// `a, b, c => rhs`. `SubRange[lhs]`.
    /// main_token is the `=>`
    switch_case,
    /// `lhs...rhs`.
    switch_range,
    /// `while (lhs) rhs`.
    /// `while (lhs) |x| rhs`.
    while_simple,
    /// `while (lhs) : (a) b`. `WhileCont[rhs]`.
    /// `while (lhs) : (a) b`. `WhileCont[rhs]`.
    while_cont,
    /// `while (lhs) : (a) b else c`. `While[rhs]`.
    /// `while (lhs) |x| : (a) b else c`. `While[rhs]`.
    /// `while (lhs) |x| : (a) b else |y| c`. `While[rhs]`.
    @"while",
    /// `for (lhs) rhs`.
    for_simple,
    /// `for (lhs) a else b`. `if_list[rhs]`.
    @"for",
    /// `if (lhs) rhs`.
    /// `if (lhs) |a| rhs`.
    if_simple,
    /// `if (lhs) a else b`. `If[rhs]`.
    /// `if (lhs) |x| a else b`. `If[rhs]`.
    /// `if (lhs) |x| a else |y| b`. `If[rhs]`.
    @"if",
    /// `suspend lhs`. lhs can be omitted. rhs is unused.
    @"suspend",
    /// `resume lhs`. rhs is unused.
    @"resume",
    /// `continue`. lhs is token index of label if any. rhs is unused.
    @"continue",
    /// `break :lhs rhs`
    /// both lhs and rhs may be omitted.
    @"break",
    /// `return lhs`. lhs can be omitted. rhs is unused.
    @"return",
    /// `fn(a: lhs) rhs`. lhs can be omitted.
    /// anytype and ... parameters are omitted from the AST tree.
    /// main_token is the `fn` keyword.
    /// extern function declarations use this tag.
    fn_proto_simple,
    /// `fn(a: b, c: d) rhs`. `sub_range_list[lhs]`.
    /// anytype and ... parameters are omitted from the AST tree.
    /// main_token is the `fn` keyword.
    /// extern function declarations use this tag.
    fn_proto_multi,
    /// `fn(a: b) rhs linksection(e) callconv(f)`. `FnProtoOne[lhs]`.
    /// zero or one parameters.
    /// anytype and ... parameters are omitted from the AST tree.
    /// main_token is the `fn` keyword.
    /// extern function declarations use this tag.
    fn_proto_one,
    /// `fn(a: b, c: d) rhs linksection(e) callconv(f)`. `FnProto[lhs]`.
    /// anytype and ... parameters are omitted from the AST tree.
    /// main_token is the `fn` keyword.
    /// extern function declarations use this tag.
    fn_proto,
    /// lhs is the fn_proto.
    /// rhs is the function body block.
    /// Note that extern function declarations use the fn_proto tags rather
    /// than this one.
    fn_decl,
    /// `anyframe->rhs`. main_token is `anyframe`. `lhs` is arrow token index.
    anyframe_type,
    /// Both lhs and rhs unused.
    anyframe_literal,
    /// Both lhs and rhs unused.
    char_literal,
    /// Both lhs and rhs unused.
    integer_literal,
    /// Both lhs and rhs unused.
    float_literal,
    /// Both lhs and rhs unused.
    false_literal,
    /// Both lhs and rhs unused.
    true_literal,
    /// Both lhs and rhs unused.
    null_literal,
    /// Both lhs and rhs unused.
    undefined_literal,
    /// Both lhs and rhs unused.
    unreachable_literal,
    /// Both lhs and rhs unused.
    /// Most identifiers will not have explicit AST nodes, however for expressions
    /// which could be one of many different kinds of AST nodes, there will be an
    /// identifier AST node for it.
    identifier,
    /// lhs is the dot token index, rhs unused, main_token is the identifier.
    enum_literal,
    /// main_token is the string literal token
    /// Both lhs and rhs unused.
    string_literal,
    /// main_token is the first token index (redundant with lhs)
    /// lhs is the first token index; rhs is the last token index.
    /// Could be a series of multiline_string_literal_line tokens, or a single
    /// string_literal token.
    multiline_string_literal,
    /// `(lhs)`. main_token is the `(`; rhs is the token index of the `)`.
    grouped_expression,
    /// `@a(lhs, rhs)`. lhs and rhs may be omitted.
    /// main_token is the builtin token.
    builtin_call_two,
    /// Same as builtin_call_two but there is known to be a trailing comma before the rparen.
    builtin_call_two_comma,
    /// `@a(b, c)`. `sub_list[lhs..rhs]`.
    /// main_token is the builtin token.
    builtin_call,
    /// Same as builtin_call but there is known to be a trailing comma before the rparen.
    builtin_call_comma,
    /// `error{a, b}`.
    /// rhs is the rbrace, lhs is unused.
    error_set_decl,
    /// `struct {}`, `union {}`, `opaque {}`, `enum {}`. `extra_data[lhs..rhs]`.
    /// main_token is `struct`, `union`, `opaque`, `enum` keyword.
    container_decl,
    /// Same as ContainerDecl but there is known to be a trailing comma
    /// or semicolon before the rbrace.
    container_decl_trailing,
    /// `struct {lhs, rhs}`, `union {lhs, rhs}`, `opaque {lhs, rhs}`, `enum {lhs, rhs}`.
    /// lhs or rhs can be omitted.
    /// main_token is `struct`, `union`, `opaque`, `enum` keyword.
    container_decl_two,
    /// Same as ContainerDeclTwo except there is known to be a trailing comma
    /// or semicolon before the rbrace.
    container_decl_two_trailing,
    /// `union(lhs)` / `enum(lhs)`. `SubRange[rhs]`.
    container_decl_arg,
    /// Same as container_decl_arg but there is known to be a trailing
    /// comma or semicolon before the rbrace.
    container_decl_arg_trailing,
    /// `union(enum) {}`. `sub_list[lhs..rhs]`.
    /// Note that tagged unions with explicitly provided enums are represented
    /// by `container_decl_arg`.
    tagged_union,
    /// Same as tagged_union but there is known to be a trailing comma
    /// or semicolon before the rbrace.
    tagged_union_trailing,
    /// `union(enum) {lhs, rhs}`. lhs or rhs may be omitted.
    /// Note that tagged unions with explicitly provided enums are represented
    /// by `container_decl_arg`.
    tagged_union_two,
    /// Same as tagged_union_two but there is known to be a trailing comma
    /// or semicolon before the rbrace.
    tagged_union_two_trailing,
    /// `union(enum(lhs)) {}`. `SubRange[rhs]`.
    tagged_union_enum_tag,
    /// Same as tagged_union_enum_tag but there is known to be a trailing comma
    /// or semicolon before the rbrace.
    tagged_union_enum_tag_trailing,
    /// `a: lhs = rhs,`. lhs and rhs can be omitted.
    /// main_token is the field name identifier.
    /// lastToken() does not include the possible trailing comma.
    container_field_init,
    /// `a: lhs align(rhs),`. rhs can be omitted.
    /// main_token is the field name identifier.
    /// lastToken() does not include the possible trailing comma.
    container_field_align,
    /// `a: lhs align(c) = d,`. `container_field_list[rhs]`.
    /// main_token is the field name identifier.
    /// lastToken() does not include the possible trailing comma.
    container_field,
    /// `anytype`. both lhs and rhs unused.
    /// Used by `ContainerField`.
    @"anytype",
    /// `comptime lhs`. rhs unused.
    @"comptime",
    /// `nosuspend lhs`. rhs unused.
    @"nosuspend",
    /// `{lhs rhs}`. rhs or lhs can be omitted.
    /// main_token points at the lbrace.
    block_two,
    /// Same as block_two but there is known to be a semicolon before the rbrace.
    block_two_semicolon,
    /// `{}`. `sub_list[lhs..rhs]`.
    /// main_token points at the lbrace.
    block,
    /// Same as block but there is known to be a semicolon before the rbrace.
    block_semicolon,
    /// `asm(lhs)`. rhs is the token index of the rparen.
    asm_simple,
    /// `asm(lhs, a)`. `Asm[rhs]`.
    @"asm",
    /// `[a] "b" (c)`. lhs is 0, rhs is token index of the rparen.
    /// `[a] "b" (-> lhs)`. rhs is token index of the rparen.
    /// main_token is `a`.
    asm_output,
    /// `[a] "b" (lhs)`. rhs is token index of the rparen.
    /// main_token is `a`.
    asm_input,
    /// `error.a`. lhs is token index of `.`. rhs is token index of `a`.
    error_value,
    /// `lhs!rhs`. main_token is the `!`.
    error_union,

    pub fn isContainerField(tag: Tag) bool {
        return switch (tag) {
            .container_field_init,
            .container_field_align,
            .container_field,
            => true,

            else => false,
        };
    }
};

test "basic add functionality" {
    testing.expect(add(3, 7) == 10);
}
