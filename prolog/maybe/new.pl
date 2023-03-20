:- include('courses.pl').
:- include('student.pl').
:- include('required.pl').



last(L,R):- append(_,[R],L).
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

print_offered([]):- write("DONE").
print_offered([H|T]):- write(H),nl,print_offered(T).




filter_course_type(T,L):-
    findall(C,program_course(C,T),L).

get_required_full_course_list([],T) :-
    filter_course_type(required,L),
    get_required_full_course_list(L,T).

get_required_full_course_list([X|XS],[]) :-
    generate_full_program_course(X,C),
    write(C),nl,
    append([C],[],TS),
    get_required_full_course_list(XS,TS).

get_required_full_course_list([X|XS],T) :-
    generate_full_program_course(X,C),
    write(C),
    append([C],T,TS),
    get_required_full_course_list(XS,TS),
    T=TS.




get_lecs_for_course(CourseName,Semester,LecList):- 
    findall(lecture(A,CourseName,Section,Comp,Del,Day,Start,Duration,Semester),lecture(A,CourseName,Section,Comp,Del,Day,Start,Duration,Semester),LecList).

get_lecs_for_section(CourseName,Semester,Section,LecList):- 
    findall(lecture(A,CourseName,Section,_,_,Day,Start,Duration,Semester),lecture(A,CourseName,Section,_,_,Day,Start,Duration,Semester),LecList).

get_tuts_for_course(CourseName,Semester,Section,TutList) :-
    findall(tut(A,CourseName,Section,_,_,Day,Start,Duration,Semester),tut(A,CourseName,Section,_,_,Day,Start,Duration,Semester),TutList).

get_labs_for_course(CourseName,Semester,Section,LabList) :-
    findall(lab(A,CourseName,Section,Code,_,Day,Start,Duration,Semester),lab(A,CourseName,Section,Code,_,Day,Start,Duration,Semester),LabList).

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
    get_labs_for_course(CourseName,Semester,Section,LabList),
    gatherTimes(LabList,LabTimes),
    append(LecTimes,TutTimes,LabTut),
    append(LabTimes,LabTut,Times),!.

get_sections_for_course(CourseName,Semester,Sections):- 
    findall(section(CourseName,Section,Semester),lecture(_,CourseName,Section,_,_,_,_,_,Semester),SecList),
    remove_duplicates(SecList,Sections).

build_sections([],_).
build_sections([X|XS],Sections):-
    X = section(CourseName,Section,Semester),
    course_time_table(CourseName,Semester,Section,Times),
    Sec=course(CourseName,Semester,Section,Times),
    build_sections(XS,NextSections),
    append([Sec],NextSections,Sections),!.

% get_sections_for_course("COMP2659","winter",L),build_sections(L,S).

sections(CourseName,Semester,Sections):-
    get_sections_for_course(CourseName,Semester,Secs),
    build_sections(Secs,Sections).

% sections("COMP2659","winter",L)


time_overlaps(T1,T2):-
    T1 = (Day1,Start1,Duration1),
    T2 = (Day2,Start2,Duration2),
    (Day1 = Day2),!,
        End1 = Start1 + Duration1,
        End2 = Start2 + Duration2,
    (  
        (Start1 = Start2,!,write("conflicts1"),nl,true);
        (Start1 < Start2, End1 > Start2,!,write("conflicts2"),nl,true);
        (Start1 < End2, End1 > End2,!,write(Day1),write(" conflicts3"),nl,true);
        (Start2 < Start1,End2 > Start1,!,write("conflicts4"),nl,true);
        (Start2 < End1, End2 > End1,!,write("conflicts5"),nl,true)
    )
    ;
    false.
    
%(write(Day1),nl,write("T1 conflicts with T2 "),nl,StartDur1 is Start1 + Duration1,write(StartDur1),write(">"),write(Start1),StartDur1 > Start2+Duration2,!,true);(write("T2 conflicts with T1 "),nl,write(Day1),nl,StartDur2 is Start2 + Duration2,write(StartDur2),write(">"),write(Start2),StartDur2 > Start1+Duration2,!,true)

times_conflict([],_) :- false.
times_conflict([X|XS],Y) :-
    time_conflicts_against(X,Y),!,true
    ;
    times_conflict(XS,Y).

%A = [("Wednesday", 14.5, 1.0), ("Monday", 15.5, 1.0), ("Tuesday", 15.5, 1.0), ("Thursday", 15.5, 1.0), ("Friday", 8.5, 1.0)],B = [("Tuesday", 10.5, 1.0), ("Monday", 14.5, 1.0), ("Tuesday", 14.5, 1.0), ("Thursday", 14.5, 1.0)],times_conflict(A,B).


time_conflicts_against(_,[]):-false.
time_conflicts_against(T1,[T2|_]):- time_overlaps(T1,T2),!,true.
time_conflicts_against(T1,[_|T2S]):- time_conflicts_against(T1,T2S).

course_compatiable(C1,C2):- 
    C1=course(CourseName1,Semester1,_,_),
    C2=course(CourseName2,Semester2,_,_),
    CourseName1 \= CourseName2,
    Semester1 = Semester2.

course_conflicts(C1,C2):-
    C1=course(_,_,_,Times1),
    C2=course(_,_,_,Times2),
    course_compatiable(C1,C2),
    %write(Times1),nl,
    %write(Times2),nl,
    times_conflict(Times1,Times2).



%sections("COMP2659","winter",L),!,[X|XS]=L,[Y|_]=XS,course_conflicts(X,Y).
%A = course("COMP3649", "winter", 1, [("Thursday", 10.5, 2.0), ("Tuesday", 16.0, 1.5), ("Thursday", 16.0, 1.5)]),B = course("COMP3614", "winter", 1, [("Friday", 8.5, 1.0), ("Tuesday", 14.5, 1.5), ("Thursday", 14.5, 1.5)]),course_conflicts(A,B).
%sections("COMP3649","winter",L),sections("COMP3614","winter",G),!,[X|_]=L,last(Y,G),course_conflicts(X,Y).
%StartDur2 is Start2 + Duration


%times_conflict([("Monday",10,5),("Tuesday",10,5)],[("Monday",10,5),("Tuesday",10,5)])
%times_conflict([("Monday",10,5)],[("Wednesday",10,5),("Tuesday",10,5)])

% A = course("COMP3649", "winter", 1, [("Thursday", 10.5, 2.0), ("Tuesday", 16.0, 1.5), ("Thursday", 16.0, 1.5)],course("COMP3614", "winter", 1, [("Friday", 8.5, 1.0), ("Tuesday", 14.5, 1.5), ("Thursday", 14.5, 1.5)],course_conflicts(A,B).
scheduleCourse().
scheduleSemester().


required_program_courses(L):-
    findall(C,program_course(C,_),L).

needed_required_program_courses([],[],_).
needed_required_program_courses([],NewReq,Needs):- Needs = NewReq.
needed_required_program_courses([X|XS],Required,Needs):-
    remove_taken(X,Required,NewReq),
    needed_required_program_courses(XS,NewReq,Needs).

%required_program_courses(L),needed_required_program_courses(["COMP1631","COMP1633"],L,N).

generate_full_program_course(C,T):-
    find_all_prereqs(C,ORS,ANDS),
    !,
    T = fullprogram_course(C,ORS,ANDS).

find_all_prereqs(C,O,A) :-
    findall(OR,program_one_of_prereq(C,OR),O),
    findall(AND,course_prereq(C,AND),A).

available_course([X|XS],Needed,Available) :-
    member(X,Needed).

still_required(Taken,N):-
    required_program_courses(L),
    needed_required_program_courses(Taken,L,N).
