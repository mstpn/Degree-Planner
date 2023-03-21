:- include('courses.pl').
:- include('student.pl').
:- include('required.pl').

section(_,_,_).
slot(_,_,_).
last(L,R):- append(_,[R],L).

len([], LenResult):-
    LenResult is 0.

len([_|Y], LenResult):-
    len(Y, L),
    LenResult is L + 1.

if(Test,Then) :- if(Test,Then,true).
if(Test,Then,Else) :- Test, !, Then ; Else.

remove_list([], _, []).
remove_list([X|Tail], L2, Result):- member(X, L2), !, remove_list(Tail, L2, Result). 
remove_list([X|Tail], L2, [X|Result]):- remove_list(Tail, L2, Result).

remove_taken(_,[],[]).
remove_taken(C,L,L2):-
						member(C, L),
						delete(L,C,L2).

remove_duplicates([],[]).

remove_duplicates([H | T], List) :-    
     member(H, T),
     remove_duplicates( T, List).

remove_duplicates([H | T], [H|T1]) :- 
      \+member(H, T),
      remove_duplicates( T, T1).

filter_course_type(T,L):-
    findall(C,program_course(C,T),L).

% get_lecs_for_section(_,_,[]).
get_lecs_for_course(CourseName,Semester,LecList):- 
    (lecture(A,CourseName,Section,Comp,Del,Day,Start,Duration,Semester),
            lecture(A,CourseName,Section,Comp,Del,Day,Start,Duration,Semester),
            Lec),append(Lec,[],LecList).

% get_lecs_for_section(_,_,_,[]).
get_lecs_for_section(CourseName,Semester,Section,LecList):- 
    findall(lecture(A,CourseName,Section,Comp,Del,Day,Start,Duration,Semester),
            lecture(A,CourseName,Section,Comp,Del,Day,Start,Duration,Semester),
            Lec),append(Lec,[],LecList).

get_lecs_for_section(CourseName,Semester,Section,LecList):- 
    findall(lecture(A,CourseName,Section,_,_,Day,Start,Duration,Semester),
            lecture(A,CourseName,Section,_,_,Day,Start,Duration,Semester),
            LecList).

get_tuts_for_course(CourseName,Semester,Section,TutList) :-
    findall(tut(A,CourseName,Section,Comp,Del,Day,Start,Duration,Semester),
            tut(A,CourseName,Section,Comp,Del,Day,Start,Duration,Semester),
            Tut),append(Tut,[],TutList).

% get_labs_for_course(_,_,_,[]).
get_labs_for_course(CourseName,Semester,Section,LabList) :-
    findall(lab(A,CourseName,Section,Comp,Del,Day,Start,Duration,Semester),
            lab(A,CourseName,Section,Comp,Del,Day,Start,Duration,Semester),
            Lab),append(Lab,[],LabList).

gatherTimes([],_).
gatherTimes([X|XS],T):-
    X = lecture(_,_,_,_,_,Day,Start,Duration,_),
    Time = (Day,Start,Duration),
    gatherTimes(XS,Times),
    append([Time],Times,T).

gatherTimes([X|XS],T):-
    X = tut(_,_,_,_,_,Day,Start,Duration,_),
    Time = (Day,Start,Duration),
    gatherTimes(XS,Times),
    append([Time],Times,T).

gatherTimes([X|XS],T):-
    X = lab(_,_,_,_,_,Day,Start,Duration,_),
    Time = (Day,Start,Duration),
    gatherTimes(XS,Times),
    append([Time],Times,T).

course_time_table(CourseName,Semester,Section,Times):-
    get_lecs_for_section(CourseName,Semester,Section,LecList),
    gatherTimes(LecList,LecTimes),
    get_tuts_for_course(CourseName,Semester,Section,TutList),
    gatherTimes(TutList,TutTimes),
    write("TutTimes: "),write(TutTimes),nl,
    get_labs_for_course(CourseName,Semester,Section,LabList),
    gatherTimes(LabList,LabTimes),
    write("LabTimes: "),write(LabTimes),nl,
    if(nonvar(LecTimes),LecTimes=LecTimes,LecTimes=[]),
    if(nonvar(TutTimes),TutTimes=TutTimes,TutTimes=[]),
    if(nonvar(LabTimes),LabTimes=LabTimes,LabTimes=[]),
    append(LecTimes,TutTimes,LabTut),
    append(LabTimes,LabTut,Times),
    write("Times: "),write(Times),nl.
    
get_sections_for_course(CourseName,Semester,Sections):- 
    findall(section(CourseName,Section,Semester),
            lecture(Dept,CourseName,Section,Code,Del,Day,Start,Duration,Semester),
            SecList),
    remove_duplicates(SecList,Sec),
    append([],Sec,Sections).


build_sections([],_).
build_sections([X|XS],Sections):-
    X = section(CourseName,Section,Semester),
    course_time_table(CourseName,Semester,Section,Times),
    Sec=course(CourseName,Semester,Section,Times),
    build_sections(XS,NextSections),
    append([Sec],NextSections,Sections).

% get_sections_for_course("COMP2659","winter",L),build_sections(L,S).

sections(CourseName,Semester,Sections):-
    get_sections_for_course(CourseName,Semester,Secs),
    (
        (Secs = [],!,Sections=[])
        ;
        (build_sections(Secs,Sections))
    ).

% sections("COMP2659","winter",L)

time_overlaps(T1,T2):-
    T1 = (Day1,Start1,_),
    T2 = (Day2,Start2,_),
    (Day1 = Day2),
    (Start1 = Start2).
        
time_overlaps(T1,T2):-
    T1 = slot(Day1,Start1,Duration1),
    T2 = (Day2,Start2,_),
    (Day1 = Day2),
    End1 = Start1 + Duration1,
    (Start1 < Start2, End1 > Start2).

time_overlaps(T1,T2):-
    T1 = slot(Day1,Start1,Duration1),
    T2 = slot(Day2,Start2,Duration2),
    (Day1 = Day2),
    End1 = Start1 + Duration1,
    End2 = Start2 + Duration2,
    (Start1 < End2, End1 > End2).

time_overlaps(T1,T2):-
    T1 = (Day1,Start1,_),
    T2 = slot(Day2,Start2,Duration2),
    (Day1 = Day2),
    End2 = Start2 + Duration2,
    (Start2 < Start1,End2 > Start1).

   
time_overlaps(T1,T2):-
    T1 = slot(Day1,Start1,Duration1),
    T2 = slot(Day2,Start2,Duration2),
    (Day1 = Day2),
    End1 = Start1 + Duration1,
    End2 = Start2 + Duration2,
    (Start2 < End1, End2 > End1).



    
%(write(Day1),nl,write("T1 conflicts with T2 "),nl,StartDur1 is Start1 + Duration1,write(StartDur1),write(">"),write(Start1),StartDur1 > Start2+Duration2,!,true);(write("T2 conflicts with T1 "),nl,write(Day1),nl,StartDur2 is Start2 + Duration2,write(StartDur2),write(">"),write(Start2),StartDur2 > Start1+Duration2,!,true)



%A = [("Wednesday", 14.5, 1.0), ("Monday", 15.5, 1.0), ("Tuesday", 15.5, 1.0), ("Thursday", 15.5, 1.0), ("Friday", 8.5, 1.0)],B = [("Tuesday", 10.5, 1.0), ("Monday", 14.5, 1.0), ("Tuesday", 14.5, 1.0), ("Thursday", 14.5, 1.0)],times_conflict(A,B).

time_conflicts_against(_,[]):-false.
time_conflicts_against(T1,[T2|_]):- time_overlaps(T1,T2),!,true.
time_conflicts_against(T1,[_|T2S]):- time_conflicts_against(T1,T2S).

times_conflict([],_):- false.
times_conflict([X|_],Y) :- time_conflicts_against(X,Y),!,true.
times_conflict([_|XS],Y):- times_conflict(XS,Y).

course_compatiable(C1,C2):- 
    C1=course(CourseName1,Semester1,_,_),
    C2=course(CourseName2,Semester2,_,_),
    CourseName1 \= CourseName2,
    Semester1 = Semester2.

course_conflicts(C1,C2):-
    C1=course(CourseName1,Semester1,Sections1,Times1),
    C2=course(CourseName2,Semester2,Sections2,Times2),
    course_compatiable(C1,C2),
    times_conflict(Times1,Times2).


check_course_conflicts(_,[]).
check_course_conflicts(C1,[H|_]):-course_conflicts(C1,H).
check_course_conflicts(C1,[_|T]):-check_course_conflicts(C1,T).

check_courses(C1,A):-member(_,A),check_course_conflicts(C1,A).
check_courses(_,A):-not(member(_,A)).

%sections("COMP2659","winter",L),!,[X|XS]=L,[Y|_]=XS,course_conflicts(X,Y).
%A = course("COMP3649", "winter", 1, [("Thursday", 10.5, 2.0), ("Tuesday", 16.0, 1.5), ("Thursday", 16.0, 1.5)]),B = course("COMP3614", "winter", 1, [("Friday", 8.5, 1.0), ("Tuesday", 14.5, 1.5), ("Thursday", 14.5, 1.5)]),course_conflicts(A,B).
%sections("COMP3614","winter",L),sections("COMP2659","winter",G),!,[_|XS]=L,[X|_]=XS,[Y|_]=G,course_conflicts(X,Y).


%times_conflict([("Monday",10,5),("Tuesday",10,5)],[("Monday",10,5),("Tuesday",10,5)])
%times_conflict([("Monday",10,5)],[("Wednesday",10,5),("Tuesday",10,5)])

% A = course("COMP3649", "winter", 1, [("Thursday", 10.5, 2.0), ("Tuesday", 16.0, 1.5), ("Thursday", 16.0, 1.5)],course("COMP3614", "winter", 1, [("Friday", 8.5, 1.0), ("Tuesday", 14.5, 1.5), ("Thursday", 14.5, 1.5)],course_conflicts(A,B).

required_program_courses(L):-
    findall(C,program_course(C,_),L).

% needed_required_program_courses([],_,_).
% needed_required_program_courses([],NewReq,Needs):- Needs = NewReq.
% needed_required_program_courses([X|XS],Required,Needs):-
%     remove_taken(X,Required,NewReq),
%     needed_required_program_courses(XS,NewReq,Needs).

% %required_program_courses(L),needed_required_program_courses(["COMP1631","COMP1633"],L,N).
no_one_of(C):- 
    C = course(CourseName,_,_,_),
    findall(Req,program_one_of_prereq(CourseName,Req),List),
    (List = [],true);false.

student_has_one_of([],C):-
    (no_one_of(C),true);false.

student_has_one_of([X|XS],C):-
    C = course(CourseName,_,_,_),
    findall(Req,program_one_of_prereq(CourseName,Req),List),
    ((List=[],!,true);
    (
    (member(X,List),true)
    ;
    (student_has_one_of(XS,C))
    )).
% student_has_one_of(["COMP1633","COMP1631","GNED1301"],course("COMP3309", "winter", 1, [("Wednesday", 14.5, 1.0)])).

no_prereqs(C):- 
    C = course(CourseName,_,_,_),
    findall(Req,course_prereq(CourseName,Req),List),
    (List = [],true);false.

student_has_prereq([],C):-
    (no_prereqs(C),true)
    ;
    false.

student_has_prereq([X|XS],C):-
    C = course(CourseName,_,_,_),
    %write(C),nl,
    findall(Req,program_one_of_prereq(CourseName,Req),List),
    (
        (List=[],!,true);
        (
            (member(X,List),true)
        ;
            (student_has_prereq(XS,C))
        )
    ).

can_take([],C):- 
    no_prereqs(C),
    %write("HAS ON OF"),nl,
    no_one_of(C).
    %write("HAS PREREQ"),nl.

can_take(Taken,C):-
    student_has_one_of(Taken,C),
    %write("HAS ON OF"),nl,
    student_has_prereq(Taken,C).
    %write("HAS PREREQ"),nl.


findCanTake(_,[],_).
findCanTake(Taken,[X|XS],CanTake):-
    not(can_take(Taken,X)),
    findCanTake(Taken,XS,CanTake).
findCanTake(Taken,[X|XS],CanTake):-
    can_take(Taken,X),
    findCanTake(Taken,XS,Take),
    append([X],Take,CanTake).
    


%sections("COMP2631","winter",A),sections("COMP2633","winter",B),sections("COMP2613","winter",C),append(A,B,AB),append(AB,C,ABC),Taken=["COMP1631","COMP1633","MATH1200","MATH1271"],filter_can_take_from_needed(Taken,ABC,G).


get_semester_sections([],_,_).
get_semester_sections([X|XS],Semester,Sections):-
    sections(X,Semester,A),
    get_semester_sections(XS,Semester,Sec),
    append(A,Sec,Sections).

% required_program_courses(L),get_semester_sections(L,"winter",S).

available_courses(Semester,Taken,Available):-
    required_program_courses(Needed),
    get_semester_sections(Needed,Semester,S),
    findCanTake(Taken,S,Available).

picks(0,_,_,_).
picks(_,[],[],_).
picks(Amount,[Y|YS],XS,Schedule):-
    % write("Y"),nl,
    % write(Y),nl,
    % write("XS"),nl,
    % write(XS),nl,
    write("Checking course conflicts"),nl,
    % trace,
    not(check_course_conflicts(Y,XS)),
    write("HEY"),nl,
    Max = Amount - 1,
    % write(XS),nl,
    append([Y],XS,NewGen),
    picks(Max,YS,NewGen,New),
    append(NewGen,New,Schedule).

% picks(Amount,[Y|YS],Gen,Schedule):-
%     check_course_conflicts(Y,Gen),
%     % write(Y),nl,
%     % write(Gen),nl,
%     picks(Amount,YS,Gen,Schedule).  

  
% solve_year(Max,Taken,NewTaken,YearSchedule):-
%     available_courses("fall",Taken,FallAvailable),
%     pick(Max,FallAvailable,FallSchedule),
%     appendScheduleToTaken(FallSchedule,WinterTaken),
%     available_courses("winter",WinterTaken,WinterAvailable),
%     pick(Max,WinterAvailable,WinterSchedule),
%     appendScheduleToTaken(WinterSchedule,NewTaken),
%     append([FallSchedule],[WinterSchedule],YearSchedule).

% solve_years(0,_,_,_).
% solve_years(Max,MaxYears,Taken,NewTaken,Year):-
%     NewMax is MaxYears - 1,
%     solve_year(Max,Taken,New,Schedule),
%     solve_years(Max,NewMax,New,NewTaken,Years),
%     append(Schedule,)


%solve_semester(Max,Semester,Taken,Schedule):-
%    available_courses(Semester,Taken,Available),
%    pick(Amount,Available,Schedule).
    
%appendScheduleToTaken([],_).
%appendScheduleToTaken([X|XS],Taken):-
%    X = course(CourseName,_,_,_),
%    appendScheduleToTaken(XS,Picked),
%    append([CourseName],Picked,Taken).





print_sech([]).
print_sech([X|XS]):-
    write(X),nl,print_sech(XS).
