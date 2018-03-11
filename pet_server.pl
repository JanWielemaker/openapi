:- use_module(library(http/thread_httpd)).
:- use_module(prolog/openapi).

:- openapi_server('examples/petstore', []).

server(Port) :-
    http_server(openapi_dispatch,
                [ port(Port)
                ]).

:- dynamic
    pet/2.                                      % ?Id, ?Name

listPets(Limit, Pets) :-
    api_default(Limit, 100),
    once(findnsols(Limit, _{id:Id, name:Name}, pet(Id, Name), Pets)).

createPets(Pets, status(201)) :-
    forall(member(Pet, Pets),
           create_pet(Pet)).

create_pet(Pet) :-
    _{id:Id, name:Name} :< Pet,
    assert(pet(Id, Name)).

showPetById(IdA, [_{id:Id, name:Name}]) :-
    atom_number(IdA, Id),
    pet(Id, Name).
