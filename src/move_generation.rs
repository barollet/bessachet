extern crate array_init;
/*
0 size: 2295 start: 997 magic1: 9227877501035347968 magic2: 9223381395320111104
2 size: 1006 start: 1553 magic1: 9223935556830904320 magic2: 9223402823717355520
4 size: 1023 start: 3070 magic1: 9008299303370752 magic2: 2305878230093465600
6 size: 1011 start: 3084 magic1: 6755708680568832 magic2: 6631702142976
16 size: 839 start: 1649 magic1: 9223372071430848512 magic2: 9255463482801913857
18 size: 916 start: 2411 magic1: 4611686027029643266 magic2: 5845673416316706816
20 size: 923 start: 3124 magic1: 2314850277441671168 magic2: 2305843559506843648
22 size: 587 start: 1748 magic1: 9259400851384960000 magic2: 9367487225467623424
32 size: 990 start: 2993 magic1: 2882304719295873024 magic2: 2882304897267532800
34 size: 746 start: 1745 magic1: 9223372844308758592 magic2: 9241386574355111936
36 size: 683 start: 3412 magic1: 1729382456628355072 magic2: 1197957775884292096
38 size: 696 start: 1734 magic1: 9236886838651126784 magic2: 9227875652856717312
48 size: 2554 start: 1477 magic1: 576460889743425568 magic2: 288231477811088449
50 size: 3318 start: 617 magic1: 7007602119770898436 magic2: 720575946932880129
52 size: 2424 start: 839 magic1: 9223372041150660640 magic2: 4611686020843806724
54 size: 2940 start: 1147 magic1: 9223372051887423560 magic2: 38280596902397441
1 size: 1535 start: 2559 magic1: 4503668481327104 magic2: 3458782140380151808
3 size: 1023 start: 3070 magic1: 9009398345113600 magic2: 2305845213538582528
5 size: 1023 start: 1022 magic1: 9232414422629154816 magic2: 11745405562337103872
7 size: 2039 start: 2056 magic1: 1697784467062784 magic2: 4611721218502959104
17 size: 1011 start: 2572 magic1: 4611686035648708610 magic2: 2305844659152355328
19 size: 774 start: 3249 magic1: 2377900742839959552 magic2: 2342215953405739008
21 size: 513 start: 3310 magic1: 2305843079007406080 magic2: 2395915551786336256
23 size: 796 start: 1648 magic1: 9520609629537968128 magic2: 9224497937835598849
33 size: 1243 start: 1508 magic1: 9223374751274369024 magic2: 9223374047033692160
35 size: 798 start: 2753 magic1: 4611686142983569408 magic2: 4755802308028137984
37 size: 765 start: 2562 magic1: 4611686222438860800 magic2: 5764608073041711104
39 size: 857 start: 3158 magic1: 2378463561798240256 magic2: 2882303777153499136
49 size: 3036 start: 1059 magic1: 2449960396421136392 magic2: 21896429697
51 size: 2932 start: 123 magic1: 11529215596296273924 magic2: 4683743616907117825
53 size: 3059 start: 516 magic1: 9520609749951873028 magic2: 2630102215672465412
55 size: 2431 start: 432 magic1: 6917529027708977264 magic2: 11601272640419922945
Shared size found
Predicted offset 46786, size 374288
*/

macro_rules! bishop_table {
    () => {
        {
            unsafe {
                array_init::array_init(|square| {
                    MagicEntry {
                        magic: 0,
                        table: ATTACK_TABLE.as_ptr(),
                        black_mask: 0,
                        postmask: 0,
                    }
                })

            }
        }
    };
}

// unsafe attack table for rooks, bishops and queens
// this is a black magic fancy table with shared attacks
// See: https://www.chessprogramming.org/Magic_Bitboards
//
// This table is computed at runtime not to make the executable table too big
// hence the mut keyword
static mut ATTACK_TABLE: [u64; 46909] = [0; 46909];

#[derive(Copy, Clone)]
pub struct MagicEntry {
    magic: u64,
    table: *const u64, // Unsafe pointer not to use a safe bigger slice
    black_mask: u64,
    postmask: u64,
}

// The magic entries are computed at compile time using macros
static BISHOP_TABLE: [MagicEntry; 64] = bishop_table!();
static ROOK_TABLE: [MagicEntry; 64] = bishop_table!();

