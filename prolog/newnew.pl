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

% OPTIONS
offering(comp2521, winter, d100, tr2(10), mru).
offering(comp2521, winter, d100, t2r(14), mru).
offering(comp2521, fall, d100, tr2(15), mru).
offering(comp3533, winter, d100, mwf(10), mru).
offering(comp3533, fall, d100, mwf(10), mru).
offering(comp3625, fall, d100, t2r(20), mru).
offering(comp3505, winter, d100, mwf(13), mru).
offering(comp4555, fall, d100, mwf(10), mru).
offering(comp4513, winter, d100, mwf(15), mru).
offering(comp4522, winter, d100, mwf(0), mru).
offering(comp4630, winter, d100, mwf(11), mru).
offering(comp4635, winter, d100, eve(tue), mru).
offering(comp5690, winter, d100, eve(any), mru).
offering(comp5690, fall, d100, eve(any), mru).

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
prerequisites(comp2521, []). %Database 1 10 winter fall
prerequisites(comp3612, [1633]). %web cs
prerequisites(comp3533, [comp2655]). %network 8,16 mwf winter fqll
prerequisites(comp3625, [comp2631,comp2613]). %AI
prerequisites(comp3505, [comp1633,comp2631]). %testing mwf(13)
prerequisites(comp4555, [comp2659]). %Games
prerequisites(comp4513, [comp3612]). %Web 3 
prerequisites(comp4522, [comp2521]). %Database 2
prerequisites(comp4630, [comp3625]). %ml mwf(11)
prerequisites(comp4635, [comp3533]). %dist eve(tue)
prerequisites(comp5690, []). %sen eve(tue)

jun_ops(Ops):- 
    Ops = [comp2521,comp3612,comp3533,comp3625,comp3505].

sen_ops(Ops):-
    Ops = [comp4555,comp4513,comp4522,comp4630,comp4635,comp5690].

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


remove_duplicates([],[]).

remove_duplicates([H | T], List) :-    
     member(H, T),
     remove_duplicates( T, List).

remove_duplicates([H | T], [H|T1]) :- 
      \+member(H, T),
      remove_duplicates( T, T1).

feasible_semester_schedule(_, [], []).
feasible_semester_schedule(Sem, [Course|More], Schedule) :-
  feasible_semester_schedule(Sem, More, Subschedule),
  offering(Course, Sem, _, SchedPattern, Campus),
  schedule(SchedPattern, Campus, CourseSched),
  compatible_schedules(CourseSched, Subschedule),
  append(CourseSched, Subschedule, Schedule);
  feasible_semester_schedule(Sem, More, Schedule).

feasible_semester_schedule_new(_, [], [],[]).
feasible_semester_schedule_new(Sem, [Course|More], Schedule,CourseSchedule) :-
    feasible_semester_schedule_new(Sem, More, Subschedule,SubCourseSchedule),
    offering(Course, Sem, _, SchedPattern, Campus),
    schedule(SchedPattern, Campus, CourseSched),
    compatible_schedules(CourseSched, Subschedule),
    append(CourseSched, Subschedule, Schedule),
    %write("wowy"),nl,
    append([course_instance(Course,CourseSched)],SubCourseSchedule,CourseSchedule)
    ;
    feasible_semester_schedule_new(Sem, More, Schedule,CourseSchedule).

prerequisites_satisfied([], _).
prerequisites_satisfied([C|Cs], Taken) :-
   prerequisites(C, Prereqlist),
   subset(Prereqlist, Taken),
   prerequisites_satisfied(Cs, Taken).

filterTakeable([],_,[]).
% filterTakeable([],[],[]).
filterTakeable([C|Cs],Taken,CanTake):-
    not(member(C,Taken)),
    prerequisites(C, Prereqlist),
    subset(Prereqlist, Taken),
    filterTakeable(Cs,Taken,CanTakeRest),
    append([C],CanTakeRest,CanTake);
    filterTakeable(Cs,Taken,CanTake).

choose(1, [H|_], [H]).
choose(N, [H|TL], [H|ST]) :- Less1 is N - 1, choose(Less1, TL, ST).
choose(N, [_|T], L) :- choose(N, T, L).

count(P,Count) :-
    findall(1,P,L),
    length(L,Count).

calcWeight(Pred,PW):-
    count(prerequisites(_, [Pred|_]),L),
    PW = Pred/L.

calcWeights(NonW,Sorted):-
    maplist(calcWeight, NonW, W),
    sort(2,  @>=, W,  Sorted).

strip_weight(S/_,S).

take_weight_off(W,NonW):-
    maplist(strip_weight, W,NonW).

sort_on_prereqs(NonW,Sorted):-
    calcWeights(NonW,W),
    take_weight_off(W,Sorted).

selectn(0, [], Rest, Rest).
selectn(N, [A|B], C, Rest) :-
    append(H, [A|T], C),
    M is N-1,
    selectn(M, B, T, S),
    append(H, S, Rest).

pick(Num,From,Too):-
    (
        choose(Num,From,TS);
        length(From,L),choose(L,From,TS)
    ),
    length(TS,Len),
    Len =< Num,
    Too = TS,!.


feasible_semester_plan(_, [], _).
feasible_semester_plan(Sem, Crses, Taken) :-
    feasible_semester_schedule(Sem, Crses, _),
    is_set(Crses),
    prerequisites_satisfied(Crses, Taken).



semester_schedule(_, [], [],[],[]).
semester_schedule(Sem, [Course|More], Schedule,Able,CourseSchedule) :-
  semester_schedule(Sem, More, Subschedule,SubAble,SubCourseSchedule),
  offering(Course, Sem, _, SchedPattern, Campus),
  schedule(SchedPattern, Campus, CourseSched),
  compatible_schedules(CourseSched, Subschedule),
  append(CourseSched, Subschedule, Schedule),
%   %write(idk),nl,
  append([Course], SubAble, Able),
  append([course_instance(Course,CourseSched)],SubCourseSchedule,CourseSchedule);
%   %write("Cant"),nl,
  semester_schedule(Sem, More, Schedule,Able,CourseSchedule).

prob_semester(_,0,_,_,[],[]).
prob_semester(Sem,Num,Required,Taken,Able,CourseSchedule):-
    (
        subtract(Required,Taken,Crses)
    ),
    filterTakeable(Crses,Taken,Takeable),
    sort_on_prereqs(Takeable,Sorted),!,
    pick(Num,Sorted,G),
    semester_schedule(Sem,G,_,Able,CourseSchedule).

even(X) :- 0 is mod(X, 2).
sem_type_select(Num,Sem):-
    even(Num),
    Sem = winter,!;
    Sem = fall,!.

generate_plan(T1,T2,0,_,_):-T1\=T2,false.
generate_plan(T,T,0,_,[]).
generate_plan(T,T,_,_,[]).
generate_plan(Taken,Required,Sems,CrsPer,Semesters):-
    sem_type_select(Sems,Sem),
    prob_semester(Sem,CrsPer,Required,Taken,Able,CourseSchedule),
    append(Able,Taken,NewTaken),
    A = semester_plan(Sem,CourseSchedule),
    sort(NewTaken,  TakenSorted),
    sort(Required,  RequiredSorted),
    NextSems is Sems - 1,
    NextSems >= 0, 
    generate_plan(TakenSorted,RequiredSorted,NextSems,CrsPer,SubSemesters),
    append([A],SubSemesters,Semesters).


graduation_plan(student(Sems,CrsPer,Taken),Semesters):-
    required_courses(Req),
    generate_plan(Taken,Req,Sems,CrsPer,Semesters).

graduation_plan(Taken,Sems,CrsPer,Semesters):-
    required_courses(Req),
    generate_plan(Taken,Req,Sems,CrsPer,Semesters).

write_lines(_,[]).
write_lines(Out,[L|Ls]):-
    write(Out,L),
    write_lines(Out,Ls).

get_student_dict(Student) :-
    open("soren.json", read, Stream),
    json_read_dict(Stream, Student),
    % write(Student),
    close(Stream).

get_student(StudentDict,Student):-
    get_dict(taken, StudentDict, TakenStr),
    get_dict(semesters, StudentDict, Sems),
    % nl,write("SEMS: "),write(SemsStr),nl,
    get_dict(courses_for_semester, StudentDict, CrsPer),
    convert_taken_string_to_atom(TakenStr,Taken),
    Student = student(Sems,CrsPer,Taken).

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


main:-
    get_student_dict(StudentDict),
    get_student(StudentDict,Student),
    (
        graduation_plan(Student,L),
        print_graduation_plan(0,L,Lines),
        write_plan(Lines)
        ;
        write("The provided file did not generate a plan"),nl,fail
    ).

write_plan(Lines):-
    open('output.csv',write,Out),!,
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

test_plan(L):-
    L = [
            semester_plan(fall, [course_instance(comp1631, [slot(mon, 11, mru), slot(wed, 11, mru), slot(fri, 11, mru)]), course_instance(math1200, [slot(tue, 13, mru), slot(thu, 13, mru), slot(thu, 12, mru)]), course_instance(math1203, [slot(mon, 16, mru), slot(wed, 16, mru), slot(fri, 16, mru)]), course_instance(comp3309, [slot(tue, 10, mru), slot(thu, 10, mru), slot(thu, 9, mru)])]), 
            semester_plan(winter, [course_instance(comp1633, [slot(mon, 4, mru), slot(wed, 4, mru), slot(fri, 4, mru)]), course_instance(math1271, [slot(mon, 8, mru), slot(wed, 8, mru), slot(fri, 8, mru)]), course_instance(phil1179, [slot(mon, 16, mru), slot(wed, 16, mru), slot(fri, 16, mru)]), course_instance(math2234, [slot(mon, 10, mru), slot(wed, 10, mru), slot(fri, 10, mru)])]), semester_plan(fall, [course_instance(comp2631, [slot(tue, 10, mru), slot(thu, 10, mru), slot(thu, 9, mru)]), course_instance(comp2613, [slot(tue, 14, mru), slot(thu, 14, mru), slot(thu, 13, mru)]), course_instance(comp2655, [slot(mon, 14, mru), slot(wed, 14, mru), slot(fri, 14, mru)])]), semester_plan(winter, [course_instance(comp2633, [slot(tue, 8, mru), slot(thu, 8, mru), slot(thu, 7, mru)]), course_instance(comp2659, [slot(mon, 16, mru), slot(wed, 16, mru), slot(fri, 16, mru)]), course_instance(comp3614, [slot(tue, 14, mru), slot(tue, 15, mru), slot(thu, 14, mru)]), course_instance(comp3649, [slot(tue, 16, mru), slot(thu, 16, mru), slot(thu, 15, mru)])]), semester_plan(fall, [course_instance(comp3659, [slot(mon, 8, mru), slot(wed, 8, mru), slot(fri, 8, mru)])])
        ].
