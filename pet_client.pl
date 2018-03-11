:- use_module(prolog/openapi).
:- use_module(library(http/http_open)).

:- openapi_client('examples/petstore', []).

%!  listPets(?Limit, -Pets)

%!  createPets(+Pets)

%!  showPetById(+Id, -Pet)
