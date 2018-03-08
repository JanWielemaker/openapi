:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

create_pets(List) :-
    setup_call_cleanup(
        http_open('http://localhost:5000/v1/pets',
                  In,
                  [ post(json(List)),
                    status_code(Status)
                  ]),
        (   Status == 201
        ->  Reply = true
        ;   json_read_dict(In, Reply, [])
        ),
        close(In)),
    writeln(Status),
    pp(Reply).
