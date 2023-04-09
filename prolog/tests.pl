% Import the the main program
:- ['schedule.pl'].
% Out of bounds error testing
% Wrapped in not since we expect the program to fail
:- begin_tests(oob).

test(zero_semesters) :-
    not(main('../data/input/test/prolog/1.json')).

test(neg_semesters) :-
    not(main('../data/input/test/prolog/2.json')).

test(zero_courses) :-
    not(main('../data/input/test/prolog/3.json')).

test(neg_courses) :-
    not(main('../data/input/test/prolog/4.json')).

:- end_tests(oob).


% Test for too many courses and other fail states
% Wrapped in not since we expect the program to fail
:- begin_tests(basic_fail).

test(overconstrained_1) :-
    not(main('../data/input/test/prolog/5.json')).

test(overconstrained_2) :-
    not(main('../data/input/test/prolog/6.json')).

test(overconstrained_3) :-
    not(main('../data/input/test/prolog/7.json')).

:- end_tests(basic_fail).


% Test basic empty degree cases
% Cut since we only care about getting a valid solution
:- begin_tests(basic_pass).

test(empty_easy) :-
    main('../data/input/test/prolog/8.json'),!.

test(empty_med) :-
    main('../data/input/test/prolog/9.json'),!.

test(empty_hard) :-
    main('../data/input/test/prolog/10.json'),!.

:- end_tests(basic_pass).


% Tests involving previously taken courses
:- begin_tests(previous_courses).

test(first_year_taken) :-
    % We expect <= 6 semesters
    main('../data/input/test/prolog/11.json'),!.

test(second_year_taken) :-
    % We expect <= 4 semesters
    main('../data/input/test/prolog/12.json'),!.

test(third_year_taken) :-
    % We expect <= 2 semesters
    main('../data/input/test/prolog/13.json'),!.

test(degree_done) :-
    % We expect <= 0 semesters
    main('../data/input/test/prolog/23.json'),!.

:- end_tests(previous_courses).


:- begin_tests(options).

test(junior_options) :-
    not(main('../data/input/test/prolog/14.json')).

test(senior_no_junior_short) :-
    not(main('../data/input/test/prolog/15.json')).

test(senior_no_junior_long) :-
    not(main('../data/input/test/prolog/16.json')).

test(all_options) :-
    main('../data/input/test/prolog/17.json'),!.

test(all_options_2) :-
    main('../data/input/test/prolog/18.json'),!.

test(invalid_junior) :-
    not(main('../data/input/test/prolog/19.json')).

test(invalid_senior) :-
    not(main('../data/input/test/prolog/20.json')).

test(invalid_junior_senior) :-
    not(main('../data/input/test/prolog/21.json')).

:- end_tests(options).

% Tests for other edge cases
:- begin_tests(pedantic).

test(one_per_semester) :-
    main('../data/input/test/prolog/22.json'),!.

:- end_tests(pedantic).