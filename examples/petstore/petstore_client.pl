:- use_module(library(openapi)).

:- openapi_client('petstore.yaml', []).

%! listPets(-Response, +Options) is det.
%
%  List all pets
%
%  @arg Response Pets
%       An paged array of pets
%  @arg Options
%       - limit(+int32)
%         How many items to return at one time (max 100)

%! createPets(+RequestBody, -Response) is det.
%
%  Create a pet
%
%  @arg RequestBody Pets
%       Pet to add to the store
%  @arg Response -
%       Null response

%! showPetById(+PetId, -Response) is det.
%
%  Info for a specific pet
%
%  @arg PetId string
%       The id of the pet to retrieve
%  @arg Response Pets
%       Expected response to a valid request

fill_db :-
    read_file_to_terms('pets.db', Terms, []),
    foldl(add_pet, Terms, 1, _).

add_pet(pet(Name, _Sort, _Gender), N, N2) :-
    N2 is N + 1,
    createPets([_{id:N, name:Name}], _).

fill(N) :-
    forall(between(1, N, Id),
           (   atom_concat('Bello_', Id, Name),
               createPets([_{id:Id, name:Name}], _)
           )).
