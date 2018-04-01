:- use_module(library(openapi)).
:- use_module(library(option)).

:- openapi_client('petstore-expanded.yaml', []).

%! findPets(-Response, +Options) is det.
%
%  Returns all pets from the system that the user has access to
%
%  @arg Response array(Pet)
%       pet response
%  @arg Options
%       - tags(+array(string))
%         tags to filter by
%       - limit(+int32)
%         maximum number of results to return

%! addPet(+RequestBody, -Response) is det.
%
%  Creates a new pet in the store.  Duplicates are allowed
%
%  @arg RequestBody NewPet
%       Pet to add to the store
%  @arg Response Pet
%       pet response

%! deletePet(+Id) is det.
%
%  deletes a single pet based on the ID supplied
%
%  @arg Id int64
%       ID of pet to delete

%! 'find pet by id'(+Id, -Response) is det.
%
%  Returns a user based on a single ID, if the user does not have access to the pet
%
%  @arg Id int64
%       ID of pet to fetch
%  @arg Response Pet
%       pet response

fill :-
    read_file_to_terms('pets.db', Terms, []),
    forall(member(pet(Name, Class, Gender), Terms),
                  addPet(_{name:Name, tag:Class, gender:Gender}, _)).
