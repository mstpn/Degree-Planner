:- use_module(library(http/json)).
:- dynamic(my_dict/1). % dynamic predicate to hold the dict
:- initialization(get_course_dict()). % fetch dict on file load

prereq_keys(['0','1','2','3','4','5','6','7','8','9']).

% Load the json file into a dict
get_course_dict() :-
    open("../json/all_courses.json", read, Stream),
    json_read_dict(Stream, Dict, []),
    close(Stream), 
    assertz(my_dict(Dict)).

% get_prereqs/2 is a function to get the prereqs for a course
get_prereqs(Course, Prereqs) :-
    my_dict(Dict),
    % Course can be an atom or a string, make sure it's an atom
    (atom(Course)
    -> CourseKey = Course
    ; 
    atom_string(CourseKey, Course)),
    get_dict(CourseKey, Dict, PrereqDict),
    prereq_keys(Keys),
    findall(Y,(member(M,Keys),Y=PrereqDict
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


% Gets the full list of prereqs for a course, including prereqs of prereqs.
% Prereqs -> List of course names
get_all_prereqs(Course, Prereqs) :-
    get_prereqs(Course, CoursePrereqs),
    get_all_prereqs_helper(CoursePrereqs, Prereqs).

get_all_prereqs_helper([], []).
get_all_prereqs_helper([H|T], Prereqs) :-
    get_all_prereqs(H, HPrereqs),
    get_all_prereqs_helper(T, TPrereqs),
    append(HPrereqs, TPrereqs, HTPrereqs),
    list_to_set([H|HTPrereqs], Prereqs).
    % Prereqs = course(H, HTPrereqs, []).


% remove an item from a list
% https://stackoverflow.com/questions/66627455/prolog-flatten-2-implementation
remove_item(X, [X|T], T).
remove_item(X, [H|T], [H|T1]) :-
    remove_item(X, T, T1).



% datastructure for a class node in the graph (class, prereqs, postreqs)
% course(Name, Prereqs, Postreqs).

% prereq(Name, Prereq) :-
%     course(Name, Prereqs, Postreqs),
%     member(Prerew, Prereqs).

% postreq(Name, Postreq) :-
%     course(Name, Prereqs, Postreqs),
%     member(Postreq, Postreqs).


% % prereq_tree/2 is a function to get the prereq tree for a course
% prereq_tree(Course, Tree) :-
%     % course(Name, Prereqs, Postreqs),
%     get_all_prereqs(Course, AllPrereqs),
%     get_prereq_tree(Course, AllPrereqs, Prereqs),
%     Tree = course(Course, Prereqs, []).

% get_prereq_tree(Course, AllPrereqs, PrereqTree) :-
%     % for each prereq in allPrereqs, get the prereq tree for that prereq
%     findall(X, (member(Y, AllPrereqs), prereq_tree(Y, X)), PrereqTree).




% THIS ONE KINDA WORKS... BUT EMPTY LISTS instead of courses...
% get_prereq_tree/2 is a function to get the prereq tree for a course
% it uses the course/3 datastructure to represent the graph
% Course -> Course name
% PrereqTree -> List of course nodes
% get_prereq_tree(Course, PrereqTree) :-
%     get_prereqs(Course, Prereqs),
%     get_prereq_tree_helper(Course, Prereqs, PrereqTree).

% get_prereq_tree_helper(Course, Prereqs, PrereqTree) :-
%     get_all_prereqs(Course, AllPrereqs),
%     get_prereq_tree_helper_helper(Course, Prereqs, AllPrereqs, PrereqTree).

% get_prereq_tree_helper_helper(Course, Prereqs, AllPrereqs, PrereqTree) :-
%     findall(X, (member(Y, Prereqs), get_prereqs(Y, Z), 
%         get_prereq_tree_helper_helper(Y, Z, AllPrereqs, X)), PrereqTree).
