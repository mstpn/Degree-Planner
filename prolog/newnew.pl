:- use_module(library(http/json)).

offering(comp1631, fall, d100, mwf(11), mru).
offering(math1200, fall, d100, tr2(13), mru).
offering(math1203, fall, d100, mwf(16), mru).
offering(comp3309, fall, d100, tr2(10), mru).
offering(math1271, fall, d100, mwf(8), mru).
offering(comp2655, fall, d100, mwf(14), mru).
offering(comp2631, fall, d100, tr2(10), mru).
offering(comp2613, fall, d100, tr2(14), mru).
offering(comp3659, fall, d100, mwf(8), mru).

% TESTING
offering(comp1631, winter, d100, mwf(11), mru).
offering(math1200, winter, d100, tr2(13), mru).
offering(math1203, winter, d100, mwf(16), mru).
offering(comp3309, winter, d100, tr2(10), mru).
offering(math1271, winter, d100, mwf(8), mru).
offering(comp2655, winter, d100, mwf(14), mru).
offering(comp2631, winter, d100, tr2(10), mru).
offering(comp2613, winter, d100, tr2(14), mru).
offering(comp3659, winter, d100, mwf(8), mru).

offering(comp3659, winter, d100, mwf(80), mru).
offering(comp3659, fall, d100, mwf(80), mru).

offering(comp2659, winter, d100, mwf(15), mru).
offering(comp2633, winter, d100, tr2(8), mru).
offering(math2234, winter, d100, mwf(10), mru).
offering(comp1633, winter, d100, mwf(10), mru).
offering(math1271, winter, d100, mwf(8), mru).
offering(comp3309, winter, d100, tr2(11), mru).
offering(phil1179, winter, d100, mwf(16), mru).
offering(comp3614, winter, d100, t2r(14), mru).
offering(comp3649, winter, d100, tr2(16), mru).

offering(comp2659, winter, d100, mwf(16), mru).

offering(math2234, winter, d100, mwf(2), mru).
offering(comp1633, winter, d100, mwf(4), mru).
offering(math1271, winter, d100, mwf(6), mru).
offering(phil1179, winter, d100, mwf(8), mru).
offering(comp2633, winter, d100, tr2(2), mru).
offering(comp3309, winter, d100, tr2(6), mru).
offering(comp3649, winter, d100, tr2(10), mru).
offering(comp3614, winter, d100, t2r(14), mru).

%Testing
offering(math2234, fall, d100, mwf(2), mru).
offering(comp1633, fall, d100, mwf(4), mru).
offering(math1271, fall, d100, mwf(6), mru).
offering(phil1179, fall, d100, mwf(8), mru).
offering(comp2633, fall, d100, tr2(2), mru).
offering(comp3309, fall, d100, tr2(6), mru).
offering(comp3649, fall, d100, tr2(10), mru).
offering(comp3614, fall, d100, t2r(14), mru).

% OPTIONS
offering(comp2521, winter, d100, tr2(110), mru).
offering(comp2521, winter, d100, t2r(214), mru).
offering(comp2521, fall, d100, tr2(315), mru).
offering(comp3533, winter, d100, mwf(410), mru).
offering(comp3533, fall, d100, mwf(510), mru).
offering(comp3625, fall, d100, t2r(620), mru).
offering(comp3505, winter, d100, mwf(713), mru).
offering(comp4555, fall, d100, mwf(810), mru).
offering(comp4513, winter, d100, mwf(915), mru).
offering(comp4522, winter, d100, mwf(1110), mru).
offering(comp4630, winter, d100, mwf(2011), mru).
offering(comp4635, winter, d100, eve(tue), mru).
offering(comp5690, winter, d100, eve(any), mru).
offering(comp5690, fall, d100, eve(any), mru).
offering(comp3612, fall, d100, mwf(3013), mru).


offering(comp2521, fall, d100, tr2(110), mru).
offering(comp2521, fall, d100, t2r(214), mru).
offering(comp2521, winter, d100, tr2(315), mru).
offering(comp3533, fall, d100, mwf(410), mru).
offering(comp3533, winter, d100, mwf(510), mru).
offering(comp3625, winter, d100, t2r(620), mru).
offering(comp3505, fall, d100, mwf(713), mru).
offering(comp4555, winter, d100, mwf(810), mru).
offering(comp4513, fall, d100, mwf(915), mru).
offering(comp4522, fall, d100, mwf(1110), mru).
offering(comp4630, fall, d100, mwf(2011), mru).
offering(comp4635, fall, d100, eve(tue), mru).
offering(comp5690, fall, d100, eve(any), mru).
offering(comp5690, winter, d100, eve(any), mru).
offering(comp3612, winter, d100, mwf(3013), mru).

required(comp1631).
required(math1200).
required(math1203).
required(phil1179).
required(comp3309).
required(comp1633).
required(math1271).
required(math2234).
required(comp2655).
required(comp2631).
required(comp2633).
required(comp2613).
required(comp2659).
required(comp3614).
required(comp3649).
required(comp3659).

prerequisites(comp1631, []).
prerequisites(math1200, []).
prerequisites(math1203, []).
prerequisites(phil1179, []).
prerequisites(comp3309, []).
prerequisites(comp1633, [comp1631]).
prerequisites(math1271, [math1203]).
prerequisites(math2234, [math1200]).
prerequisites(comp2655, [comp1633]).
prerequisites(comp2631, [comp1633]).
prerequisites(comp2633, [comp2631]).
prerequisites(comp2613, [math1271,comp1633]).
prerequisites(comp2659, [phil1179,comp2655]).
prerequisites(comp3614, [comp2631,comp2613]).
prerequisites(comp3649, [comp2613,comp2631,phil1179]).
prerequisites(comp3659, [comp2631,comp2659]).

% OPTIONS
prerequisites(comp2521, [comp1631]). %Database 1 10 winter fall
prerequisites(comp3612, [comp1633]). %web cs
prerequisites(comp3533, [comp2655]). %network 8,16 mwf winter fqll
prerequisites(comp3625, [comp2631,comp2613]). %AI
prerequisites(comp3505, [comp1633,comp2631]). %testing mwf(13)

prerequisites(comp4555, [comp2659]). %Games
prerequisites(comp4513, [comp3612]). %Web 3 
prerequisites(comp4522, [comp2521]). %Database 2
prerequisites(comp4630, [comp3625]). %ml mwf(11)
prerequisites(comp4635, [comp3533]). %dist eve(tue)
prerequisites(comp5690, []). %sen eve(tue)



has_offering([]).
has_offering([X|XS]):- 
    % write(X),nl,
    offering(X, _, _, _, _),
    has_offering(XS).

jun_ops(Ops):- 
    Ops = [comp2521,comp3612,comp3533,comp3625,comp3505].

sen_ops(Ops):-
    Ops = [comp4555,comp4513,comp4522,comp4630,comp4635,comp5690].

has_three(SelectedOps):-
    length(SelectedOps,L),
    L >= 3.

check_jun_ops(SelectedJOps):- 
    jun_ops(Ops),
    subset(SelectedJOps,Ops),
    has_three(SelectedJOps).

check_sen_ops(SelectedSOps,Required):- 
    sen_ops(Ops),
    subset(SelectedSOps,Ops),
    has_three(SelectedSOps),
    prerequisites_satisfied(SelectedSOps,Required).

% TESTS
% PASSING 
% required_courses(D),append_options([comp2521,comp3612,comp3625],[comp4522,comp4513,comp4630],D,L).
% FAILING
% required_courses(D),append_options([comp2521],[comp4522],D,L).
% required_courses(D),append_options([comp2521,comp3612],[comp4522,comp3612],D,L).
append_options([],[],_,_):-false.
append_options(Jun,Sen,Required,NewRequired):- 
    check_jun_ops(Jun),
    append(Jun,Required,RequiredNow),
    check_sen_ops(Sen,RequiredNow),
    append(Sen,RequiredNow,NewRequired).


% append_options([],[],_,_):-false.
% append_options(Jun,Sen,Required,NewRequired):- 
%     check_jun_ops(Jun),
%     append(Jun,Required,RequiredNow),
%     check_sen_ops(Sen,RequiredNow),
%     append(Sen,RequiredNow,NewRequired).
student_append(student(_,_,_,Jun,Sen),Required):-
    required_courses(Reqs),
    append_options(Jun,Sen,Reqs,Required).

required_courses(C):-
    C=[
        comp1631,
        math1200,
        math1203, 
        phil1179, 
        comp3309, 
        comp1633, 
        math1271, 
        math2234, 
        comp2655, 
        comp2631, 
        comp2633,
        comp2613,
        comp2659,
        comp3614,
        comp3649,
        comp3659
    ].

schedule(mwf(N), Campus, [slot(mon, N, Campus), slot(wed, N, Campus), slot(fri, N, Campus)]).
schedule(t2r(N), Campus, [slot(tue, N, Campus), slot(tue, N1, Campus), slot(thu, N, Campus)]) :- N1 is N+1.
schedule(tr2(N), Campus, [slot(tue, N, Campus), slot(thu, N, Campus), slot(thu, N1, Campus)]) :- N1 is N-1.
schedule(eve(D), Campus, [slot(D, 17, Campus), slot(D, 18, Campus), slot(D, 19, Campus)]).

% Schedules are compatible if there are no conflicting time slots.
compatible_schedules([], _).
compatible_schedules(_, []).
compatible_schedules([Slot1 | M1], [Slot2 | M2]) :-
   conflict_free_slots(Slot1, Slot2), compatible_schedules([Slot1|M1], M2), compatible_schedules(M1, [Slot2|M2]), !.

% Slots are conflict-free if they are on different days, are on the same campus 
% but at different times, or they allow at least an hour travel time.
conflict_free_slots(slot(Day1, _, _), slot(Day2, _, _)) :- Day1 \== Day2.
conflict_free_slots(slot(Day, Hour1, Campus), slot(Day, Hour2, Campus)) :- Hour1 =\= Hour2.
conflict_free_slots(slot(Day, Hour1, _), slot(Day, Hour2, _)) :- TimeDiff is abs(Hour1-Hour2), TimeDiff > 1.

prerequisites_satisfied([], _).
prerequisites_satisfied([C|Cs], Taken) :-
   prerequisites(C, Prereqlist),
   subset(Prereqlist, Taken),
   prerequisites_satisfied(Cs, Taken).

filterTakeable([],_,[]).
filterTakeable([C|Cs],Taken,CanTake):-
    not(member(C,Taken)),
    prerequisites(C, Prereqlist),
    subset(Prereqlist, Taken),
    % write(C),nl,
    filterTakeable(Cs,Taken,CanTakeRest),
    append([C],CanTakeRest,CanTake)
    % write(C),nl
    ;
    % write(Cs),nl,
    filterTakeable(Cs,Taken,CanTake).

semester_schedule(_, [], [],[],[]).
semester_schedule(Sem, [Course|More], Schedule,Able,CourseSchedule) :-
  semester_schedule(Sem, More, Subschedule,SubAble,SubCourseSchedule),
%   write(Course),nl,
  offering(Course, Sem, _, SchedPattern, Campus),
%   write(Course),nl,
  schedule(SchedPattern, Campus, CourseSched),
%   write(Course),nl,
  compatible_schedules(CourseSched, Subschedule),
%   write(CourseSched),nl,
  append(CourseSched, Subschedule, Schedule),
%   write(Schedule),nl,
%   %write(idk),nl,
  append([Course], SubAble, Able),
%   write(Able),nl,
  append([course_instance(Course,CourseSched)],SubCourseSchedule,CourseSchedule)
    % write(CourseSchedule),nl
  ;
%   %write("Cant"),nl,
  semester_schedule(Sem, More, Schedule,Able,CourseSchedule).

prob_semester(_,0,_,_,[],[]).
prob_semester(Sem,Num,Required,Taken,Able,CourseSchedule):-
    subtract(Required,Taken,Crses),
    filterTakeable(Crses,Taken,Takeable),
    % write(Takeable),nl,
    % sort_on_prereqs(Takeable,Sorted),!,
    % pick(Num,Sorted,G),
    % write(G),nl,
    semester_schedule(Sem,Takeable,_,Able,CourseSchedule),
    Able \= [],
    length(Able,L),
    L =< Num.

even(X) :- 0 is mod(X, 2).
sem_type_select(Num,Sem):-
    even(Num),
    Sem = winter,!;
    Sem = fall,!.

generate_plan(T1,T2,0,_,_):-T1\=T2,false.%write("FAILING"),nl,false.
generate_plan(T,T,0,_,[]).
generate_plan(T,T,_,_,[]).
generate_plan(Taken,Required,Sems,CrsPer,Semesters):-
    sem_type_select(Sems,Sem),
    prob_semester(Sem,CrsPer,Required,Taken,Able,CourseSchedule),
    append(Able,Taken,NewTaken),
    A = semester_plan(Sem,CourseSchedule),
    NextSems is Sems - 1,
    NextSems >= 0, 
    sort(NewTaken,  TakenSorted),
    sort(Required,  RequiredSorted),
    % write("HERE"),nl,
    generate_plan(TakenSorted,RequiredSorted,NextSems,CrsPer,SubSemesters),
    append([A],SubSemesters,Semesters).
% TEST CODE
% PASSING
% required_courses(D),test_student_choices(student(1,1,D)).
% required_courses(D),test_student_choices(student(0,0,D)).
% equired_courses(D),[_|XS]=D,test_student_choices(student(1,1,XS)).
% FAILS
% required_courses(D),[_|XS]=D,test_student_choices(student(0,1,XS)).
% required_courses(D),[_|XS]=D,test_student_choices(student(0,0,XS)).
% test_student_choices(student(1,1,[])).
test_student_choices(Student,Req):-
    % required_courses(Req),
    test_student_courses_per(Student),
    test_student_semesters(Student),
    check_student_taken_is_sub(Student,Req),
    check_for_impossible_times(Student,Req).
% TEST CODE
% PASSING
% test_student_courses_per(student(_,0,_)).
% test_student_courses_per(student(_,10,_)).
% test_student_courses_per(student(_,-0,_)).
% FAILS
% test_student_courses_per(student(_,-10,_)).
% test_student_courses_per(student(_,-1,_)).
test_student_courses_per(student(_,CrsPer,_,_,_)):-
    (
        CrsPer > 0,!
        ;
        !,write("Error: The Provided Maximum Courses Taken in a Semester is incorrrect"),nl,
        tab(4),format("Message: Course Per Semester can not be negative, IE not (~w)",[CrsPer]),nl,
        fail
    ),
    (
        CrsPer =< 20,!
        ;
        !,write("Error: The Provided Maximum Courses Taken in a Semester is incorrrect"),nl,
        tab(4),format("Message: Course Per Semester can not be above 5 without permission, so above 20 is an issue, IE not (~w)",[CrsPer]),nl,
        fail
    ).

% TEST CODE
% PASSING
% test_student_semesters(student(-0,_,_)).
% test_student_semesters(student(10000000,_,_)).
% test_student_semesters(student(1,_,_)).
% FAILS
% test_student_semesters(student(-1,_,_)).
% test_student_semesters(student(-10,_,_)).
% test_student_semesters(student(-10000000,_,_)).
test_student_semesters(student(Sems,_,_,_,_)):-
    (
        Sems >= 0,!
        ;
        !,write("Error: The Provided Maximum Semesters is incorrect"),nl,
        tab(4),format("Message: Maximum Semester Count can not be negative, IE not (~w)",[Sems]),nl,
        fail
    ),
    (
        Sems =< 32,!
        ;
        !,write("Error: The Provided Maximum Semesters is incorrect"),nl,
        tab(4),format("Message: Maximum Semester Count can not be above 9 regularly, so 32 is a problem as in IE not (~w)",[Sems]),nl,
        fail
    ).

% TEST CODE
% PASSING
% required_courses(D),check_student_taken_is_sub(student(0,0,[comp1631]),D).
% FAILS
% required_courses(D),check_student_taken_is_sub(student(5,5,[comp10000]),D).
check_student_taken_is_sub(student(_,_,Taken,_,_),Req):-
    (
        is_set(Taken),
        subset(Taken,Req),!
        ;
        !,write("Error: The Provided Taken Courses are incorrect"),nl,
        tab(4),write("Message: Taken classes are not a subset of the core required courses"),nl,
        fail
    ).
% TEST CODE 
% PASSING
% required_courses(D),!,check_for_impossible_times(student(2,5,[comp1631,comp1633,comp2659,comp2655,math1200,math1203]),D).
% FAILS
% required_courses(D),!,check_for_impossible_times(student(2,5,[comp1631,comp1633,comp2659,comp2655]),D).
% required_courses(D),!,check_for_impossible_times(student(2,5,[comp1631,comp1633]),D).
% required_courses(D),!,check_for_impossible_times(student(2,5,[]),D).
% required_courses(D),!,check_for_impossible_times(student(1,5,[]),D).
% required_courses(D),!,check_for_impossible_times(student(0,5,[]),D).
check_for_impossible_times(student(Sems,CrsPer,Taken,_,_),Req):-
    TotalTakeableInTime is Sems * CrsPer,
    subtract(Req,Taken,Needs),
    length(Needs,NeedsLen),!,
    (
        TotalTakeableInTime >= NeedsLen,!
        ;
        !,write("Error: Not Enough Time To Complete Required Courses"),nl,
        tab(4),format("Message: The maximum semesters provided (~w) and maximum courses per semester (~w) multiplied equals: ~w",[Sems,CrsPer,TotalTakeableInTime]),
        tab(6),format("Which is larger then the number of still needed courses (~w): (~w >= ~w)",[NeedsLen,NeedsLen,TotalTakeableInTime]),
        fail
    ).


    % graduation_plan_ops(student(20,10,[],[comp2521,comp3612,comp3625],[comp4522,comp4513,comp4630]),L).
    % graduation_plan_ops(student(10,5,[comp1631,comp1633,comp2521,comp2613,comp2631,comp2633,comp2655,comp2659,comp3309,comp3614,comp3625,comp3649,comp4522,comp4630,math1200,math1203,math1271,math2234,phil1179],[comp2521,comp3612,comp3625],[comp4522,comp4513,comp4630]),L).
graduation_plan(student(Sems,CrsPer,Taken,Jun,Sen),Semesters):-
    required_courses(Req),
    append_options(Jun,Sen,Req,NewReq),
    % write(NewReq),nl,
    generate_plan(Taken,NewReq,Sems,CrsPer,Semesters).    

graduation_plan(student(Sems,CrsPer,Taken),Semesters):-
    required_courses(Req),
    % append_options(Jun,Sen,Req,NewReq),
    generate_plan(Taken,Req,Sems,CrsPer,Semesters).

graduation_plan(Taken,Sems,CrsPer,Semesters):-
    required_courses(Req),
    generate_plan(Taken,Req,Sems,CrsPer,Semesters).

write_lines(_,[]).
write_lines(Out,[L|Ls]):-
    tab(2),write(L),
    write(Out,L),
    write_lines(Out,Ls).

get_student_dict(JsonFile,Student) :-
    open(JsonFile, read, Stream),
    json_read_dict(Stream, Student),
    % write(Student),
    close(Stream).

get_student_w_ops(StudentDict,Student):-
    get_dict(taken, StudentDict, TakenStr),
    get_dict(jun, StudentDict, JunStr),
    get_dict(sen, StudentDict, SenStr),
    get_dict(semesters, StudentDict, Sems),
    get_dict(courses_for_semester, StudentDict, CrsPer),
    convert_taken_string_to_atom(TakenStr,Taken),
    convert_taken_string_to_atom(JunStr,Jun),
    convert_taken_string_to_atom(SenStr,Sen),
    Student = student(Sems,CrsPer,Taken,Jun,Sen).

get_student(StudentDict,Student):-
    get_dict(taken, StudentDict, TakenStr),
    get_dict(semesters, StudentDict, Sems),
    get_dict(courses_for_semester, StudentDict, CrsPer),
    convert_taken_string_to_atom(TakenStr,Taken),
    Student = student(Sems,CrsPer,Taken,_,_).

convert_taken_string_to_atom([],[]).
convert_taken_string_to_atom([C|Cs],Taken):-
    % write(C),
    (
        atom(C),L = C
    ; 
        % write(C),nl,
        string_to_atom(C,L)
        % write(L),nl
    ),
    convert_taken_string_to_atom(Cs,SubTaken),
    append([L],SubTaken,Taken).

write_plan(Lines):-
    open('output.csv',append,Out),!,
    write(Out,"\nNEW GRADUATION PLAN\n"),
    write_lines(Out,Lines),
    close(Out),!. 

print_graduation_plan(_,[],[]).
print_graduation_plan(SemNum,[Sem|More],Lines):-
    print_semester_plan(SemNum,Sem,Line),!,
    % write(Line),
    NextSem is SemNum + 1,!,
    print_graduation_plan(NextSem,More,SubLines),
    append(Line,SubLines,Lines).

print_semester_plan(_,semester_plan(_,[]),[]):-!.
print_semester_plan(SemNum,semester_plan(Sem,[C|Cs]),Lines):-
    print_course_instance(SemNum,Sem,C,S),!,
    print_semester_plan(SemNum,semester_plan(Sem,Cs),SubLines),!,
    % write(SubLines),nl,
    append([S],SubLines,Lines),!.

print_course_instance(SemNum,Semester,course_instance(Course,CourseSched),S):-
    [S1|[S2|[S3|[]]]] = CourseSched,!,
    decompose_time_slot(S1,DS1),
    decompose_time_slot(S2,DS2),
    decompose_time_slot(S3,DS3),
    swritef(S,"%w,%w,%w,%w,%w,%w,\n",[SemNum,Semester,Course,DS1,DS2,DS3]).

decompose_time_slot(slot(Day,Time,Site),R):-
    R = Day/Time/Site.

main:-
    main("default.json").

main(FileNameAndPath):-
    (
        exists_file(FileNameAndPath),
        get_student_dict(FileNameAndPath,StudentDict),
        get_student_w_ops(StudentDict,Student)
        
    ;
        tab(2),write("Error: Failed to Load Provided Json File."),nl,
        tab(4),write("- Ensure it is in the same directory or that the path is correct"),nl,
        tab(4),write("- Ensure it is in the correct format and has the following keys."),nl,
        tab(8),write("- taken:"),nl,
        tab(12),write("A list of taken courses."),nl,
        tab(8),write("- semesters:"),nl,
        tab(12),write("The max amount of semesters the student would like to take."),nl,
        tab(8),write("- courses_for_semester:"),nl,
        tab(12),write("The max amount of classes to taken in a semester."),nl,
        fail
    ),!,
    (
        student_append(Student,Required),
        test_student_choices(Student,Required),!,
        graduation_plan(Student,L),
        print_graduation_plan(0,L,Lines),
        nl,write("Generated a Plan"),nl,
        tab(2),write("Found plans are automatically appened to output.csv"),nl,nl,
        write_plan(Lines),nl,
        write("Press (;) to continue"),nl,
        write("Press (.) to stop"),nl,nl
        ;
        write("The provided file did not generate a plan"),nl,fail
    ).
