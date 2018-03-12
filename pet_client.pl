:- use_module(prolog/openapi).
:- use_module(library(http/http_open)).

:- openapi_client('examples/petstore', []).

%!  listPets(?Limit, -Pets)

%!  createPets(+Pets)

%!  showPetById(+Id, -Pet)


fill_db :-
    read_file_to_terms('pets.db', Terms, []),
    foldl(add_pet, Terms, 1, _).

add_pet(pet(Name, _Sort, _Gender), N, N2) :-
    N2 is N + 1,
    createPets([_{id:N, name:Name}], _).
