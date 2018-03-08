:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

listPets(Limit, Reply) :-
    (   var(Limit)
    ->  URL = 'http://localhost:5000/v1/pets'
    ;   atom_concat('http://localhost:5000/v1/pets?limit=', Limit, URL)
    ),
    setup_call_cleanup(
        http_open(URL,
                  In,
                  [ status_code(Status)
                  ]),
        json_read_dict(In, Reply, []),
        close(In)),
    writeln(Status).

createPets(List) :-
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

showPetById(Id, Pet) :-
    atom_concat('http://localhost:5000/v1/pets/', Id, URL),
    setup_call_cleanup(
        http_open(URL,
                  In,
                  [ status_code(Status)
                  ]),
        json_read_dict(In, Pet, []),
        close(In)),
    writeln(Status).
