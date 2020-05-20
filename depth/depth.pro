% Copyright

implement depth
    open core, list

class facts
    minweight : integer := 1.

class predicates
    depthSearch : (move{State}, State, State, tuple{State*, string*}, positive, positive [out]) -> tuple{State*, string*} nondeterm.
clauses
    depthSearch(Move, Start, Goal, minweight) = tuple(reverse(Path), reverse(Moves)) :-
        minweight := 11,
        V = varM::new([]),
        foreach tuple(P, Ms) = depthSearch(Move, Start, Goal, tuple([Start], []), 0, N) do
            if N > minweight then
                succeed()
            elseif N < minweight then
                V:value := [tuple(P, Ms)],
                minweight := N
            else
                V:value := [tuple(P, Ms) | V:value]
            end if
        end foreach,
        tuple(Path, Moves) = getMember_nd(V:value).

    depthSearch(_, Goal, Goal, Path, N, N) = Path :-
        !.
    depthSearch(Move, State, Goal, tuple(Path, Moves), C, N) = depthSearch(Move, NextState, Goal, tuple([NextState | Path], [Str | Moves]), C + 1, N) :-
        C < minweight,
        NextState = Move(State, Str),
        not(isMember(NextState, Path)).

end implement depth
