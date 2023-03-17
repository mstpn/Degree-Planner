:- initialization(get_course_dict()). % fetch dict on file load
:- dynamic(my_dict/1). % dynamic predicate to hold the dict
:- use_module(library(http/json)).

% Load the json file into a dict
get_course_dict() :-
    open("../json/simple_courses.json", read, Stream),
    json_read_dict(Stream, Dict, []),
    close(Stream), 
    assertz(my_dict(Dict)).

% convert a string to an atom if it is not already an atom
make_key(Key, Str) :-
(atom(Str)
-> Key = Str
; 
atom_string(Key, Str)).


% get components of a course from the dictionary
get_course_components(Course, Name, Start, Duration) :-
    my_dict(Dict),
    make_key(CourseKey, Course),
    get_dict(CourseKey, Dict, Components),
    Name = Components.name,
    Start = Components.start,
    Duration = Components.duration.


get_components(Course, Components) :-
    my_dict(Dict),
    make_key(CourseKey, Course),
    get_dict(CourseKey, Dict, Components).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedule_components(Schedule) :-
    my_dict(Courses),
    dict_keys(Courses, Keys),
    schedule_components(Keys, [], Schedule).
schedule_components([Key|Keys], Acc, Schedule) :-
    get_components(Key, Components),
    % write(Components), nl,
    append(Acc, [Components], Acc1),
    schedule_components(Keys, Acc1, Schedule).
schedule_components([], Schedule, Schedule).

% pretty print out the schedule
print_schedule([]).
print_schedule([Course|Courses]) :-
    write(Course.name), write(" "), write(Course.start), write(" "), write(Course.duration), nl,
    print_schedule(Courses).




















