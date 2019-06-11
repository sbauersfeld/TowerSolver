% this tower predicate uses finite domain solving to quickly find solutions to the tower puzzle
tower(N, T, C) :- 
    length(T, N), 
    gen_edges(N, C, TOP, BOTTOM, LEFT, RIGHT),
    check_length(T, N),
    uniq(T),
    limit(T, N),
    trans_mat(T, T_TRANS),
    uniq(T_TRANS),
    gen_and_check(T, LEFT, RIGHT),
    check_visible(TOP, T_TRANS), 
    revall(T_TRANS, T_TRANS_REV),
    check_visible(BOTTOM, T_TRANS_REV).

% this predicate ensures that the edge counts of the puzzle are well formed
gen_edges(N, counts(T, B, L, R), TOP, BOTTOM, LEFT, RIGHT) :- 
    length(T, N),
    TOP = T,
    length(B, N),
    BOTTOM = B,
    length(L, N),
    LEFT = L,
    length(R, N),
    RIGHT = R.

% this predicate checks that all rows and columns of the table are the correct length
check_length([], _).
check_length([HD|TL], N) :- length(HD, N), check_length(TL, N).

% this predicate uses finite domain solving to ensure that each member of a row and 
% column are unique
uniq([]).
uniq([HD|TL]) :- fd_all_different(HD), uniq(TL). 

% this predicate uses the finite domain solver to create a valid range of cell values
limit([], _).
limit([HD|TL], N) :- fd_domain(HD, 1, N), limit(TL, N). 

% this predicate reverses all of the rows/columns of the table
revall([], []).
revall([HD|TL], [REV|REST]) :- reverse(HD, REV), revall(TL, REST).

% this predicate transposes a matrix. It is inspired by the SWI transpose library availabe at: 
% https://github.com/SWI-Prolog/swipl-devel/blob/80cfdbf0bfc61f90aeb0e11f74496a1e6704e14a/library/clp/clpfd.pl#L6375-L6385
trans_mat([], []) :- !.
trans_mat([[]|_], []).
trans_mat(IN, [COL|COLS]) :- trans_vec(IN, COL, REST), trans_mat(REST, COLS).
trans_vec([], [], []).
trans_vec([[HD|T]|REST], [HD|HDS], [T|TS]) :- trans_vec(REST, HDS, TS).

% these predicates check that the number of visible towers from an edge equals the edge count.
visible([], _, 0).
visible([HD|TL], MAX, X) :- HD > MAX, !, visible(TL, HD, Y), X is Y + 1.
visible([HD|TL], MAX, X) :- HD < MAX, visible(TL, MAX, X).
check_visible([], _).
check_visible([C_HD|C_TL], [T_HD|T_TL]) :- visible(T_HD, 0, X), X=C_HD, check_visible(C_TL, T_TL).

% this predicate generates row values with the finite domain solver and checks that the
% rows match the left and right edge counts
gen_and_check([], [], []). 
gen_and_check([HD|TL], [L_HD|L_TL], [R_HD|R_TL]) :- 
    fd_labeling(HD),  
    visible(HD, 0, Z),
    Z = L_HD,
    reverse(HD, HD_REV),
    visible(HD_REV, 0, Y),
    Y = R_HD,
    gen_and_check(TL, L_TL, R_TL).

% this tower solver does not use finite domain solvers, so it takes longer to find solutions
plain_tower(N, T, C) :- 
    length(T, N), 
    gen_edges(N, C, TOP, BOTTOM, LEFT, RIGHT),
    check_length(T, N),
    gen_range(1, N, X),
    my_gen_and_check(X, T, [], LEFT, RIGHT),
    trans_mat(T, T_TRANS), 
    check_visible(TOP, T_TRANS), 
    revall(T_TRANS, T_TRANS_REV),
    check_visible(BOTTOM, T_TRANS_REV).

% this predicate generates the table entries by permuting each row and checking 
% that all cells in the same column are unique. It also matches left and right
% edge counts with the towers in each row.
my_gen_and_check(_, [], _, [], []).
my_gen_and_check(X, [HD|TL], PT, [L_HD|L_TL], [R_HD|R_TL]) :- 
    permutation(X, HD), 
    append(PT, [HD], CT), 
    trans_mat(CT, TT), 
    maplist(check_uniq, TT), 
    visible(HD, 0, Z),
    Z = L_HD,
    reverse(HD, HD_REV),
    visible(HD_REV, 0, Y),
    Y = R_HD,
    my_gen_and_check(X, TL, CT, L_TL, R_TL).

% this predicate ensures that all values in a row or column are unique
check_uniq([]).
check_uniq([HD|TL]) :- \+ member(HD, TL), check_uniq(TL).

% this predicate creates a list of numbers between 1 and N
gen_range(_, 0, []).
gen_range(HIGH, HIGH, [HIGH]).
gen_range(LOW, HIGH, [LOW|REST]) :- LOW < HIGH, NEXT is LOW + 1, gen_range(NEXT, HIGH, REST).

% this predicate compares the time it takes to solve all 4x4 tower puzzles for the
% plain_tower and tower predicates
speedup(TIME) :- 
    statistics(cpu_time, [_, _]),
    findall(C, tower(4, _, C), _),
    findall(C3, tower(4, _, C3), _),
    statistics(cpu_time, [_, TIME1]),
    findall(C2, plain_tower(4, _, C2), _),
    findall(C4, plain_tower(4, _, C4), _),
    statistics(cpu_time, [_, TIME2]),
    TIME is TIME2 / TIME1. 

% this predicate generates two solutions to a single NxN puzzle with the same edge counts
ambiguous(N, C, T1, T2) :- 
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.