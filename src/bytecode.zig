// zig fmt: off
pub const Op = enum {
    load_constant,   // <cidx>
    load_local,      // <sidx>

    // Encoded Literals
    load_true,
    load_false,
    load_nil,
    load_zero,
    load_one,
    load_empty_list,

    // Arithmetic
    add,             // <argc>
    sub,             // <argc>
    mul,             // <argc>
    div,             // <argc>
    neg,

    // Comparison
    eq,              // <argc>
    gt,              // <argc>
    ge,              // <argc>
    lt,              // <argc>
    le,              // <argc>

    // Logical operators
    not,

    // Jump
    jmp,             // <pos>
    jmpf,            // <pos>
    jmpt,            // <pos>

    // Stack manipulation
    pop,
    ppop_n,          // <n>

    // Calls
    call,            // <argc>
    ret,
};
// zig fmt: on
