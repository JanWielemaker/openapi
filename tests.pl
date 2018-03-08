:- use_module(library(http/thread_httpd)).
:- use_module(prolog/openapi).

:- openapi('examples/petstore', []).

server(Port) :-
    http_server(openapi_dispatch,
                [ port(Port)
                ]).

listPets(_Limit, Pets) :-
    Pets = [p1, p2, p3].
