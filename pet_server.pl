:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(prolog/openapi).
:- use_module(prolog/swagger_ui).

:- http_handler(root(.),
                http_redirect(see_other, root('swagger_ui')),
                []).
:- http_handler(root('swagger.yaml'),
                http_reply_file('examples/petstore.yaml', []),
                [id(swagger_config)]).

:- openapi_server('examples/petstore', []).

server(Port) :-
    http_server(dispatch,
                [ port(Port)
                ]).

dispatch(Request) :-
    openapi_dispatch(Request),
    !.
dispatch(Request) :-
    http_dispatch(Request).


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
