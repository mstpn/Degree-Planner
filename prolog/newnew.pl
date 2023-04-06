offering(comp1631, fall, d100, mwf(11), mru).
offering(math1200, fall, d100, tr2(13), mru).
offering(math1203, fall, d100, mwf(16), mru).
offering(comp3309, fall, d100, tr2(10), mru).
offering(math1271, fall, d100, mwf(8), mru).
offering(comp2655, fall, d100, mwf(14), mru).
offering(comp2631, fall, d100, tr2(10), mru).
offering(comp2613, fall, d100, tr2(14), mru).
offering(comp2659, winter, d100, mwf(15), mru).
offering(comp2633, winter, d100, tr2(8), mru).
offering(math2234, winter, d100, mwf(10), mru).
offering(comp1633, winter, d100, mwf(10), mru).
offering(math1271, winter, d100, mwf(8), mru).
offering(comp3309, winter, d100, tr2(11), mru).
offering(phil1179, winter, d100, mwf(16), mru).
offering(comp3614, winter, d100, t2r(14), mru).
offering(comp3649, winter, d100, tr2(16), mru).
offering(comp3659, fall, d100, mwf(8), mru).



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

pick(Num,From,Too):-
    (choose(Num,From,Too);length(From,L),choose(L,From,Too)).

% generate_grad_plan(Taken,Sops,Jops,Cog).

% student(Taken,Sops,Jops,Cog,Sems,CrsPerSem):-
%     is_set(Taken),
%     is_set(Sops),
%     is_set(Jops),
%     member(Cog,[geog,biol,geol,chem,math,phys]),
%     Sems > 0,
%     CrsPerSem > 3,
%     generate_grad_plan(Taken,Sops,Jops,Cog).

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

% prob_semester(Sem,Num,Taken,Able):-
%     findall(L,(offering(L, Sem, _, _, _),not(member(L,Taken))),RCrses),
%     remove_duplicates(RCrses,Crses),
%     % %write(Crses),nl,
%     filterTakeable(Crses,Taken,Takeable),
%     %write("prob_sem"),nl,
%     sort_on_prereqs(Takeable,Sorted),
%     pick(Num,Sorted,G),
%     semester_schedule(Sem,G,_,Able).
    %write("lol"),nl.

% prob_semester(Sem,Num,Required,Taken,RetReq,Able):-
%     (
%         % required_courses(C),
%         subtract(Required,Taken,Crses),
%         %write(Crses),nl,
%         Crses \= []
%     ),
%     filterTakeable(Crses,Taken,Takeable),
%     %write("probable_sem"),nl,
%     sort_on_prereqs(Takeable,Sorted),
%     pick(Num,Sorted,G),
%     semester_schedule(Sem,G,_,Able),
%     subtract(Crses,Able,RetReq).
    %write("lol probable"),nl.

prob_semester(Sem,Num,Required,Taken,Able,CourseSchedule):-
    (
        % required_courses(C),
        subtract(Required,Taken,Crses)
        %write("NEW REQUIRED CLASS: "),%write(Crses),nl
        % Crses \= []
    ),
    filterTakeable(Crses,Taken,Takeable),
    %write("TAKEABLE IN "),%write(Sem),%write(": "),%write(Takeable),nl,
    sort_on_prereqs(Takeable,Sorted),
    %write("SORTED: "),%write(Sorted),nl,
    pick(Num,Sorted,G),
    %write("PICKED "),%write(Num),%write(" FROM LIST: "),%write(Sorted),nl,
    semester_schedule(Sem,G,_,Able,CourseSchedule).
    % subtract(Crses,Able,RetReq),
    %write("ABLE TO BE TAKEN: "),%write(Able),nl;
    % Able = [].

even(X) :- 0 is mod(X, 2).
sem_type_select(Num,Sem):-
    even(Num),
    Sem = winter;
    Sem = fall.

% generate_plan(_,0,_,[]).
% generate_plan(Taken,Sems,CrsPer,Semesters):-
%     sem_type_select(Sems,Sem),
%     %write("Prob sem is the problem"),nl,
%     prob_semester(Sem,CrsPer,Taken,Able),
%     %write("PROBABLE SEMESTER: "),%write(Able),nl,
%     NextSems is Sems - 1,
%     NextSems >= 0, 
%     append(Able,Taken,NewTaken),
%     A = semester_plan(Sem,Able),
%     %write(NextSems),nl,
%     generate_plan(NewTaken,NextSems,CrsPer,SubSemesters),
%     length(SubSemesters,L),
%     %write("Sem is: "),%write(NextSems),%write(" and Schedule size is: "),%write(L),nl,
%     append([A],SubSemesters,Semesters).

    % generate_plan(Taken,Sems,CrsPer,Semesters).

% generate_plan(_,A,_,R,R,[]):-
%     A >= 0,
%     R = [].
% generate_plan(_,0,_,_,[],[]):- fail.
% generate_plan(_,A,_,_,G,[]):-
%     A >= 0,
%     G = [].


% generate_plan(_,_,_,[],[],[]).
% generate_plan(_,0,_,_,[],[]).
% generate_plan(_,0,_,[_|_],[_|_],_):-!, fail.
% generate_plan(Taken,Sems,CrsPer,Required,AfterRequired,Semesters):-
%     sem_type_select(Sems,Sem),
%     %write("Prob sem is the problem"),nl,
%     prob_semester(Sem,CrsPer,Required,Taken,RetReq,Able),
%     %write(RetReq),nl,
%     NextSems is Sems - 1,
%     NextSems >= 0, 
%     append(Able,Taken,NewTaken),
%     A = semester_plan(Sem,Able),
%     %write(Required),nl,
%     generate_plan(NewTaken,NextSems,CrsPer,RetReq,AfterRequired,SubSemesters),
%     length(SubSemesters,L),
%     % AfterRequired = [],
%     %write("Sem is: "),%write(NextSems),%write(" and Schedule size is: "),%write(L),nl,
%     append([A],SubSemesters,Semesters).
    % AfterRequired = RetReq. 

    % generate_plan(_,_,_,[],[],[]).
    % generate_plan(NewTaken,Required,NextSems,CrsPer,[[A]|SubSemesters]),


generate_plan(T1,T2,0,_,A):-
    T1=T2,A=[].
    %write("BASE CASE 2"),nl,
    %write("T1 "),%write(T1),nl,
    %write("T2 "),%write(T2),nl,
    %write("A "),%write(A),nl.

generate_plan(T1,T2,B,_,A):-
    %write("BASE CASE 1"),nl,
    B>0,T1=T2,A=[].
    %write("T1 "),%write(T1),nl,
    %write("T2 "),%write(T2),nl,
    %write("A "),%write(A),nl,
    %write("B "),%write(B),nl.
generate_plan(T1,T2,0,_,A):-
    T1\=T2,!,
    %write("BASE CASE FAIL"),nl,
    %write("T1 "),%write(T1),nl,
    %write("T2 "),%write(T2),nl,
    %write("A "),%write(A),nl,
    A=[],
    false.
generate_plan(Taken,Required,Sems,CrsPer,Semesters):-
    sem_type_select(Sems,Sem),
    % %write("Prob sem is the problem"),nl,
    %write("OLD REQUIRED"),%write(Required),nl,
    prob_semester(Sem,CrsPer,Required,Taken,Able,CourseSchedule),
    %write("ABLE COURSES AFTER SCH ARE: "),%write(Able),nl,
    %write("SEMS IS: "),%write(Sems),nl,
    append(Able,Taken,NewTaken),
    A = semester_plan(Sem,CourseSchedule),
    sort(NewTaken,  TakenSorted),
    sort(Required,  RequiredSorted),
        NextSems is Sems - 1,
        NextSems >= -1, 
        %write("SCHEDULING ABLE COURSES: "),%write(A),nl,
        %write("NEXT CALL"),nl,
        %write(" NEW TAKEN FOR NEXT "),%write(TakenSorted),nl,
        %write(" NEW REQUIRED FOR NEXT "),%write(Required),nl,
        %write(" NEW NEXTSEMS FOR NEXT "),%write(NextSems),nl,
        (generate_plan(TakenSorted,RequiredSorted,NextSems,CrsPer,SubSemesters),
        length(SubSemesters,L),
        %write("Sem is: "),%write(NextSems),%write(" and Schedule size is: "),%write(L),nl,
        append([A],SubSemesters,Semesters));false.
        % ;
        % generate_plan(NewTaken,RequiredSorted,Sems,CrsPer,Semesters)



% generate_plan(T,T,_,_,[]).
% generate_plan(T,T,0,_,[]).
% generate_plan(_,T,_,_,T,A).
% generate_plan(_,T,0,_,T,A).
% generate_plan(_,0,_,[_|_],_):-!, fail.
% generate_plan(Taken,Required,Sems,CrsPer,TakenAfter,Semesters):-
%     sem_type_select(Sems,Sem),
%     % %write("Prob sem is the problem"),nl,
%     %write("OLD REQUIRED"),%write(Required),nl,
%     prob_semester(Sem,CrsPer,Required,Taken,Able),
%     NextSems is Sems - 1,
%     NextSems >= 0, 
%     append(Able,Taken,NewTaken),
%     A = semester_plan(Sem,Able),
%     %write("SCHEDULING ABLE COURSES: "),%write(A),nl,
%     generate_plan(NewTaken,Required,NextSems,CrsPer,SubSemesters),
%     length(SubSemesters,L),
%     %write("Sem is: "),%write(NextSems),%write(" and Schedule size is: "),%write(L),nl,
%     append([A],SubSemesters,Semesters).
graduation_plan_(_, [],_).
graduation_plan_(Taken, [semester_plan(Sem, Crses)|More],Schedule) :-
    feasible_semester_schedule_new(Sem, Crses, _,SemSchedule),
    append(Crses, Taken, TakenAfterSem),
    graduation_plan_(TakenAfterSem, More,FutureSchedule),
    append([SemSchedule],FutureSchedule,Schedule).
    % graduation_plan_(Taken, More,Schedule).

% feasible_semester_plan4(_, [], _,_).
% feasible_semester_plan4(Sem, Crses, Taken,Able) :-
%     feasible_semester_schedule(Sem, Crses, _),
%     is_set(Crses),
%     prerequisites_satisfied(Crses, Taken).

% semester_plan(Sem, [Crs|Crses],Taken,Takeable) :-
%     feasible_semester_schedule_new(Sem, Crses, _,_),
%     is_set(Crses),
%     prerequisites_satisfied(Crses, Taken).


% semester_plan(Sem,[H|[]],Taken,Prop):-
%     prerequisites_satisfied(H,Taken),


% semester_plan(Sem,Crs,Taken,Prop):-
%     filterTakeable(Crse,Taken,Takeable),

   
is_ordered([]).
is_ordered([_]).
is_ordered([C1, C2|Cs]) :- C1 @< C2, is_ordered([C2|Cs]).

graduation_plan_new(_, _,0,_,_).
graduation_plan_new(_, [],_,_,_).
graduation_plan_new(Taken, [semester_plan(Sem, Crses)|More],Sems,CrsPer,Schedule) :-
    filterTakeable(Crses,Taken,Takeable),
    feasible_semester_plan(Sem, Takeable,Taken),
    pick(CrsPer,Takeable,Choosen),
    feasible_semester_schedule_new(Sem, Choosen, _,SemSchedule),
    append(Choosen, Taken, TakenAfterSem),
    SemsRest is Sems - 1,
    graduation_plan_new(TakenAfterSem, More,SemsRest,CrsPer,FutureSchedule),
    append([SemSchedule],FutureSchedule,Schedule);
    graduation_plan_new(Taken, More,Sems,CrsPer,Schedule).

graduation_plan(Taken, []) :-
    meets_cmpt_major_requirements(Taken).

graduation_plan(Taken, [semester_plan(Sem, Crses)|More]) :-
    feasible_semester_plan(Sem, Crses, Taken),
    append(Crses, Taken, TakenAfterSem),
    graduation_plan(TakenAfterSem, More).

meets_cmpt_major_requirements(Taken) :-
    member(cmpt300, Taken),
    member(cmpt307, Taken),
    member(cmpt320, Taken),
    member(macm316, Taken),
    breadth_areas(A1, A2, A3),
    area_covered(A1, Taken),
    area_covered(A2, Taken),
    area_covered(A3, Taken),
    %write("here"),nl,
    cmpt400courses(Taken, DepthCourses),
    length(DepthCourses, D),
    D >= 3,
    is_set(Taken),
    length(Taken, N),
    N >= 13.

area_covered(A, Taken) :-
    cmpt_concentration(A, Crses),
    has_one(Taken, Crses).

has_one([H|_], L) :- member(H, L), !.
has_one([_|T], L) :- has_one(T, L).


breadth_areas(A1, A2, A3) :- 
    append(_, [A1|L1], [ai, cgmm, infosys, pl]),
    append(_, [A2|L2], L1),
    append(_, [A3|_], L2).
 
cmpt_concentration(ai, [cmpt310, cmpt340, cmpt411, cmpt412, cmpt413, cmpt414, cmpt417, cmpt418, cmpt419]).
cmpt_concentration(cgmm, [cmpt361, cmpt363, cmpt365, cmpt461, cmpt464, cmpt466, cmpt467, cmpt468, cmpt469]).
cmpt_concentration(infosys, [cmpt301, cmpt354, cmpt370, cmpt441, cmpt454, cmpt459, cmpt470, cmpt474]).
cmpt_concentration(pl, [cmpt373, cmpt375, cmpt383, cmpt384, cmpt473, cmpt475, cmpt477, cmpt489]).
cmpt_concentration(sys-required, [cmpt300]).
cmpt_concentration(theory-required, [cmpt307]).
 
cmpt400level(Crse) :- atom_concat('cmpt4', _, Crse).
cmpt400courses([], []).
cmpt400courses([Crse|Cs], [Crse|Ds]) :- atom_concat('cmpt4', _, Crse), !, cmpt400courses(Cs, Ds).
cmpt400courses([_|Cs], Ds) :- cmpt400courses(Cs, Ds).