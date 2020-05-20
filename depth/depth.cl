% Copyright

class depth
    open core

domains
    move{State} = (State, string [out]) -> State nondeterm.

predicates
    depthSearch : (move{State}, State, State, positive [out]) -> tuple{State*, string*} nondeterm.

end class depth
