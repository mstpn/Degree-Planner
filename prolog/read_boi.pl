%this is real boi, he wants to be human
:- initialization(main, main).

:- use_module(library(csv)).
:- use_module(library(http/json)).

main :-
    (   current_prolog_flag(argv, [CSV_file|_])
    ->  csv_read_file(CSV_file, CSV, [])
    ;   csv_read_stream(current_input, CSV, [])
    ),
    CSV = [Colnames|Rows],
    Colnames =.. [row|Names],
    maplist(row_dict(Names), Rows, Dicts),
    json_write_dict(current_output, Dicts, [null('')]).

row_dict(Names, Row, Dict) :-
    Row =.. [row|Fields],
    pairs_keys_values(Data, Names, Fields),
    dict_create(Dict, _, Data).