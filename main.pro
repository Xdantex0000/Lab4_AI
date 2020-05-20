% Copyright

implement main
    open core, console, list, string, depth

class facts
    capacity : integer := 2.

domains
    magaOrPeri =
        k(integer);
        s(integer).
    state = tuple(loc, magaOrPeri*, magaOrPeri*).
    loc = left; right.

class predicates
    subset : (integer, A*, A* [out], A* [out]) nondeterm.
    move : move{state}.
    moveFromTo : (integer, magaOrPeri*, magaOrPeri*, magaOrPeri* [out], magaOrPeri* [out], magaOrPeri* [out]) nondeterm.
    unsafe : (magaOrPeri*) determ.
    f : (magaOrPeri*) -> string.
    g : (magaOrPeri) -> string.

clauses
    subset(0, L, [], L) :-
        !.
    subset(K, [A | L], [A | L1], L2) :-
        subset(K - 1, L, L1, L2).
    subset(K, [A | L], L1, [A | L2]) :-
        subset(K, L, L1, L2).

    move(tuple(left, L, R), Str) = tuple(right, L1, R1) :-
        K = std::downTo(capacity, 2),
        moveFromTo(K, L, R, L1, R1, B),
        Str = format("% с левого на правый", f(B)).
    move(tuple(right, L, R), Str) = tuple(left, L1, R1) :-
        K = std::fromTo(1, capacity),
        moveFromTo(K, R, L, R1, L1, B),
        Str = format("% с правого на левый", f(B)).

    moveFromTo(N, FromBef, ToBefore, FromAfter, ToAfter, Boat) :-
        subset(N, FromBef, Boat, FromAfter),
        not(unsafe(Boat)),
        not(unsafe(FromAfter)),
        ToAfter = sort(append(Boat, ToBefore)),
        not(unsafe(ToAfter)).

    unsafe(L) :-
        s(N) = getMember_nd(L),
        not(k(N) = getMember_nd(L)),
        k(_) = getMember_nd(L),
        !.

    f([S]) = format("% рухається", g(S)) :-
        !.
    f(L) = format("% рухається", concatWithDelimiter(map(L, { (X) = g(X) }), ", ")).

    g(k(N)) = concat("Магараджею ", toString(N)).
    g(s(N)) = concat("Пери ", toString(N)).

    run() :-
        L = sort([s(1), k(1), s(2), k(2), s(3), k(3)]),
        N = list::length(L) div 2,
        capacity := if N < 4 then 2 elseif N < 6 then 3 else 4 end if,
        Start = tuple(left, L, []),
        Goal = tuple(right, [], L),
        tuple([S0 | P], Moves) = depthSearch(move, Start, Goal, _W),
        V = varM::new(1),
        write("\t", S0),
        nl,
        forAll(zip(P, Moves),
            { (tuple(X, S)) :-
                writef("%. %\n\t%\n", V:value, S, X),
                V:value := V:value + 1
            }),
        nl,
        fail
        or
        _ = readLine().

end implement main

goal
    mainExe::run(main::run).
