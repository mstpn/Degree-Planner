:- use_module(library(http/json)).

prereq_keys(['0','1','2','3','4','5','6','7','8','9']).

get_course_dict(Dict) :-
    open("../json/all_courses.json", read, Stream),
    json_read_dict(Stream, Dict, []),
    close(Stream).

get_prereqs(Course, Prereqs) :-
    get_course_dict(Dict),
    get_dict(Course, Dict, PrereqDict),
    prereq_keys(Keys),
    findall(Y,(member(M,Keys),Y=PrereqDict
        % .get(M,"none")),PrereqList),
        .get(M)),PrereqList),
    flt(PrereqList, Prereqs).

% flt/2 is a function to flatten a 2d list into a 1d list
% https://stackoverflow.com/questions/66627455/prolog-flatten-2-implementation
flt([], []).
flt([H|L], [H|X]):-
    not(is_list(H)),
    flt(L, X),
    !.
flt([H|L], X):-
    append(R, F, X),
    flt(H, R),
    !,
    flt(L, F).

