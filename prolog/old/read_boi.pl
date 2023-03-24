
%rules 
%:- include(student.pl).
%:- include(courses.pl).

% GEOG COGNATE 

course(dep(geog),code(1101),or([]),and([])).
course(dep(geog),code(1105),or([]),and([])).
course(dep(geog),code(2553),or([]),and([course(dep(geog),code(1105),or([]),and([]))])).
course(dep(geog),code(3553),or([]),and([course(dep(geog),code(1105),or([]),and([])),course(dep(geog),code(2553),or([]),and([]))])).

% GNEDS 

course(dep(gned),code(1301),or([]),and([])).
course(dep(gned),code(1303),or([]),and([])).
course(dep(gned),code(1304),or([]),and([])).

% MATH/PHIL

course(dep(phil),code(1179),or([]),and([])).
course(dep(math),code(1200),or([]),and([])).
course(dep(math),code(1203),or([]),and([])).
course(dep(math),code(1271),or([]),and([])).
course(dep(math),code(2232),or([]),and(
    [
        course(dep(math),code(1200),or([]),and([]))
    ]
    )
    ).

% COMP REQUIRED 
course(dep(comp),code(1631),or([]),and([])).
course(dep(comp),code(1631),or([]),and([])).
course(dep(comp),code(1633),or([]),and(
    [
        course(dep(comp),code(1631),or([]),and([]))
    ]
    )
    ).
course(dep(comp),code(2613),or([]),and(
    [
        course(dep(math),code(1271),or([]),and([])),
        course(dep(comp),code(1633),or([]),and(
            [
                course(dep(comp),code(1631),or([]),and([]))
            ]
            )
            )
    ]
    )
    ).
course(dep(comp),code(2631),or([]),and(
    [
        course(dep(comp),code(1633),or([]),and(
            [
                course(dep(comp),code(1631),or([]),and([]))
            ]
            )
            )
    ]
    )
    ).
course(dep(comp),code(2633),or([]),and(
    [
        course(dep(comp),code(2631),or([]),and(
            [
                course(dep(comp),code(1633),or([]),and(
                    [
                        course(dep(comp),code(1631),or([]),and([]))
                    ]
                    )
                    )
            ]
            )
            )
    ]
    )
    ).
course(dep(comp),code(2655),or([]),and(
    [
        course(dep(comp),code(1633),or([]),and(
            [
                course(dep(comp),code(1631),or([]),and([]))
            ]
            )
            )
    ]
    )
    ).
course(dep(comp),code(2659),or([]),and(
    [
        course(dep(phil),code(1179),or([]),and([])),
        course(dep(comp),code(2655),or([]),and(
            [
                course(dep(comp),code(1633),or([]),and(
                    [
                        course(dep(comp),code(1631),or([]),and([]))
                    ]
                    )
                    )
            ]
            )
            )
    ]
    )
    ).
course(dep(comp),code(3309),or(
    [
        course(dep(gned),code(1301),or([]),and([])),
        course(dep(gned),code(1303),or([]),and([])),
        course(dep(gned),code(1304),or([]),and([]))
    ]
    ),and([])).
course(dep(comp),code(3614),or([]),and(
    [
        course(dep(math),code(1200),or([]),and([])),
        course(dep(comp),code(2613),or([]),and(
            [
                course(dep(math),code(1271),or([]),and([])),
                course(dep(comp),code(1633),or([]),and(
                    [
                        course(dep(comp),code(1631),or([]),and([]))
                    ]
                    )
                    )
            ]
            ),
            course(dep(comp),code(2631),or([]),and(
            [
                course(dep(comp),code(1633),or([]),and(
                    [
                        course(dep(comp),code(1631),or([]),and([]))
                    ]
                    )
                    )
            ]
            )
            )
            )
    ]
    )
    ).
course(dep(comp),code(3649),or([]),and(
    [
        course(dep(comp),code(2631),or([]),and(
            [
                course(dep(comp),code(1633),or([]),and(
                    [
                        course(dep(comp),code(1631),or([]),and([]))
                    ]
                    )
                    )
            ]
            )
            ),
        course(dep(phil),code(1179),or([]),and([]))
    ]
    )
    ).
course(dep(comp),code(3659),or([]),and(
    [
        course(dep(comp),code(2659),or([]),and(
            [
                course(dep(comp),code(2631),or([]),and(
                    [
                        course(dep(comp),code(1633),or([]),and(
                            [
                                course(dep(comp),code(1631),or([]),and([]))
                            ]
                            )
                            )
                    ]
                    ),
                course(dep(phil),code(1179),or([]),and([])),
                course(dep(comp),code(2655),or([]),and(
                    [
                        course(dep(comp),code(1633),or([]),and(
                            [
                                course(dep(comp),code(1631),or([]),and([]))
                            ]
                            )
                            )
                    ]
                    )
                    )
                    )
            ]
            )
            )
    ]
    )
    ).

% COMP JUN OPTIONS

course(dep(comp),code(2521),or([]),and([])).
course(dep(comp),code(3533),or([]),and([])).
course(dep(comp),code(3553),or([]),and([])).
course(dep(comp),code(3612),or([]),and([])).
course(dep(comp),code(3625),or([]),and([])).
course(dep(math),code(2101),or([]),and([])).
course(dep(math),code(2200),or([]),and([])).

% COMP SEN OPTIONS

course(dep(comp),code(4513),or([]),and([])).
course(dep(comp),code(4555),or([]),and([])).
course(dep(comp),code(4622),or([]),and([])).
course(dep(comp),code(4630),or([]),and([])).
course(dep(comp),code(4633),or([]),and([])).
course(dep(comp),code(4635),or([]),and([])).
course(dep(comp),code(5690),or([]),and([])).
course(dep(math),code(3101),or([]),and([])).

% STUDENT
student(
    name(soren),
    sem(fall),
    courses_taken(
            [
                course(dep(comp),code(1631),or([]),and([])),
                course(dep(phil),code(1179),or([]),and([])),
                course(dep(math),code(1200),or([]),and([])),
                course(dep(math),code(1203),or([]),and([])),
                course(dep(math),code(1271),or([]),and([]))
            ]
            ),
    chosen_jun_options(
            [
                course(dep(comp),code(2521),or([]),and([])),
                course(dep(comp),code(3533),or([]),and([])),
                course(dep(comp),code(3553),or([]),and([]))
            ]
            ),
    chosen_sen_options(            
            [
                course(dep(comp),code(4513),or([]),and([])),
                course(dep(comp),code(4555),or([]),and([])),
                course(dep(comp),code(4622),or([]),and([]))                  
            ]
        ),
    cog(geog),
    choosen_cog(
        [
            course(dep(geog),code(1101),or([]),and([])),
            course(dep(geog),code(1105),or([]),and([])),
            course(dep(geog),code(2553),or([]),and(
                [
                    course(dep(geog),code(1105),or([]),and([]))])),
                    course(dep(geog),code(3553),or([]),and(
                        [
                            course(dep(geog),code(1105),or([]),and([])),
                            course(dep(geog),code(2553),or([]),and([]))
                        ]
                        )
                    )
        ]
    ),
    years_to_grad(8),
    max_course(8)
    ).

write_course_info(D,C,O,A) :- course(dep(D),code(C),or(O),and(A)),!,format('Course: ~w ~10t Code: ~w ~10t ~1|~nOR: ~2t~w~nAND:~2t~w~n', [D,C,O,A]).

%c(D,C,O,A) :- course(dep(D),code(C),or(O),and(A)),!.
%course_prereqs(D,C,X) :- c(D,C,A,B),append(A,B,X).
%get_course(D,C,A,B,X) :- X = course(dep(D),code(C),or(A),and(B)).
%write_course(C) :- C = course(dep(D),code(C),O,A),

%write_prereqs(D,C) :- course_prereqs(D,C,X),X=[_|_],!,format('Course: ~w ~10t Code: ~w ~10t ~1|~n ~2t~w', [D,C,X]);write("No Prereqs").
%write_prereqs(D,C) :- write(D),write(C),course_prereqs(D,C,X),!,,write_course(H).
