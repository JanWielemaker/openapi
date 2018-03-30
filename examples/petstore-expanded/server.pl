:- use_module(library(openapi)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(swagger_ui)).

:- http_handler(root(.),
                http_redirect(see_other, root('swagger_ui')),
                []).
:- http_handler(root('swagger.yaml'),
                http_reply_file('petstore-expanded.yaml', []),
                [id(swagger_config)]).

server(Port) :-
    http_server(dispatch,
                [ port(Port)
                ]).

dispatch(Request) :-
    openapi_dispatch(Request),
    !.
dispatch(Request) :-
    http_dispatch(Request).


:- openapi_server('petstore-expanded.yaml', []).

%! findPets(-Response, +Options) is det.
%
%  @arg Response array(Pet)
%       pet response
%  @arg Options
%       - tags(+array(string))
%         tags to filter by
%       - limit(+int32)
%         maximum number of results to return

findPets(Response, Options) :-
    option(limit(Limit), Options, 100),
    option(tags(Tags), Options, _),
    once(findnsols(Limit, Pet, find_pet(Tags, Pet), Response)).

%! addPet(+RequestBody, -Response) is det.
%
%  @arg RequestBody NewPet
%       Pet to add to the store
%  @arg Response Pet
%       pet response

addPet(RequestBody, Response) :-
    predicate_property(pet(_,_,_), number_of_clauses(Id)),
    (   Tag = RequestBody.get(tag)
    ->  assertz(pet(Id, RequestBody.name, [Tag]))
    ;   assertz(pet(Id, RequestBody.name, []))
    ),
    pet(Id, Response).

%! deletePet(+Id, -Response) is det.
%
%  @arg Id int64
%       ID of pet to delete
%  @arg Response -
%       pet deleted

deletePet(Id, status(204)) :-
    delete_pet(Id).

%! 'find pet by id'(+Id, -Response) is det.
%
%  @arg Id int64
%       ID of pet to fetch
%  @arg Response Pet
%       pet response

'find pet by id'(Id, Response) :-
    (   pet(Id, Response)
    ->  true
    ;   format(string(Msg), "Pet ~p does not exist", [Id]),
        Response = status(404, _{code:404, message:Msg})
    ).

		 /*******************************
		 *         ERROR MAPPING	*
		 *******************************/

:- multifile
    http:map_exception_to_http_status_hook/4.

http:map_exception_to_http_status_hook(
         error(existence_error(pet, Id), _),
         not_found(Location),
         [connection(close)],
         []) :-
    format(atom(Location), '/pets/~w', [Id]).


		 /*******************************
		 *        IMPLEMENTATION	*
		 *******************************/

:- dynamic
    pet/3.                                      % Id, Name, Tags

delete_pet(Id) :-
    retract(pet(Id, _, _)),
    !.
delete_pet(Id) :-
    existence_error(pet, Id).

pet(Id, Response) :-
    pet(Id, Name, Tags),
    (   Tags == []
    ->  Response = _{id:Id, name:Name}
    ;   Tags = [Tag]
    ->  Response = _{id:Id, name:Name, tag:Tag}
    ).

find_pet([], Pet) :-
    !,
    pet(_, Pet).
find_pet(Tags, Pet) :-
    pet(Id, _, PetTags),
    (   member(Tag, Tags),
        memberchk(Tag, PetTags)
    ->  pet(Id, Pet)
    ).
