:- initialization(get_course_dict()). % fetch dict on file load
:- dynamic(my_dict/1). % dynamic predicate to hold the dict
:- use_module(library(http/json)).


prereq_keys(['0','1','2','3','4','5','6','7','8','9']).

% Load the json file into a dict
get_course_dict() :-
    open("../json/simple_courses.json", read, Stream),
    json_read_dict(Stream, Dict, []),
    close(Stream), 
    assertz(my_dict(Dict)).


% Schedule maximum number of courses in a day
% course_schedule(CourseDict, Schedule)
% CourseDict: dictionary of courses with start times and durations
% Schedule: list of courses scheduled in a day
:- use_module(library(pairs)).

% Generate all possible schedules of courses
% all_schedules(Courses, Schedule) :-
all_schedules(Schedule) :-
    pairs_keys_values(Courses, Names, Durations),
    length(Names, NumCourses),
    length(Schedule, NumCourses),
    maplist(member, Names, Schedule),
    maplist(duration_constraint(Schedule, Durations), Schedule),
    maplist(no_overlap_constraint(Schedule), Schedule).

% Constraint that ensures that courses do not overlap in time
no_overlap_constraint(Schedule, Course) :-
    nth0(I, Schedule, Course),
    Start is I * 60, % Assume each class starts at the beginning of the hour
    End is Start + Course.duration,
    forall((nth0(J, Schedule, Other), J \= I),
           (OtherStart is J * 60,
            OtherEnd is OtherStart + Other.duration,
            ((Start >= OtherEnd) ; (OtherStart >= End)))).

% Constraint that ensures that the duration of each course is respected
duration_constraint(Schedule, Durations, Course) :-
    nth0(I, Schedule, Course),
    nth0(I, Durations, Duration),
    length(Slot, Duration),
    append(_, Slot, Course).

% Schedules the maximum number of courses in a day
schedule_courses(Courses, Schedule) :-
    % all_schedules(Courses, Schedule),
    all_schedules(Schedule),
    length(Schedule, NumCourses),
    between(1, NumCourses, MaxCourses),
    length(Selected, MaxCourses),
    sublist(Selected, Schedule),
    maplist(duration_constraint(Selected, Durations), Selected),
    maplist(no_overlap_constraint(Selected), Selected).

sublist([], _).
sublist([X|Xs], Ys) :-
    member(X, Ys),
    sublist(Xs, Ys).

% courses = _{comp101: course(comp101, 9, 2), math102: course(math102, 11, 3),
%     engl201: course(engl201, 14, 1), hist310: course(hist310, 15, 2),
%     biol401: course(biol401, 16, 3), phil501: course(phil501, 19, 2)}.