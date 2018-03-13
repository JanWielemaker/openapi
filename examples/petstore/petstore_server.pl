:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(openapi)).
:- use_module(library(option)).
:- use_module(library(swagger_ui)).

:- http_handler(root(.),
                http_redirect(see_other, root('swagger_ui')),
                []).
:- http_handler(root('swagger.yaml'),
                http_reply_file('petstore.yaml', []),
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

:- openapi_server('petstore.yaml', []).

:- dynamic
    pet/2.                                      % ?Id, ?Name

%! listPets(-Response, +Options) is det.
%
%  List all pets
%
%  @arg Response Pets
%       An paged array of pets
%  @arg Options
%       - limit(+int32)
%         How many items to return at one time (max 100)

listPets(Response, Options) :-
   option(limit(Limit), Options, 100),
   Pet = _{id:Id, name:Name},
   once(findnsols(Limit, Pet, pet(Id, Name), Response)).

%! createPets(+RequestBody, -Response) is det.
%
%  Create a pet
%
%  @arg RequestBody Pets
%       Pet to add to the store
%  @arg Response -
%       Null response

createPets(RequestBody, status(201)) :-
    forall(member(Pet, RequestBody),
           create_pet(Pet)).

%! showPetById(+PetId, -Response) is det.
%
%  Info for a specific pet
%
%  @arg PetId string
%       The id of the pet to retrieve
%  @arg Response Pets
%       Expected response to a valid request

showPetById(PetId, [_{id:Id, name:Name}]) :-
   atom_number(PetId, Id),
   pet(Id, Name).


		 /*******************************
		 *             IMPL		*
		 *******************************/

create_pet(Pet) :-
    _{id:Id, name:Name} :< Pet,
    assert(pet(Id, Name)).
