:- use_module(library(openapi)).
:- use_module(library(option)).

:- openapi_client('petstore-expanded.yaml', []).

%! findPets(-Response, +Options) is det.
%
%  Returns all pets from the system that the user has access to
%  Nam sed condimentum est. Maecenas tempor sagittis sapien, nec rhoncus sem sagittis sit amet. Aenean at gravida augue, ac iaculis sem. Curabitur odio lorem, ornare eget elementum nec, cursus id lectus. Duis mi turpis, pulvinar ac eros ac, tincidunt varius justo. In hac habitasse platea dictumst. Integer at adipiscing ante, a sagittis ligula. Aenean pharetra tempor ante molestie imperdiet. Vivamus id aliquam diam. Cras quis velit non tortor eleifend sagittis. Praesent at enim pharetra urna volutpat venenatis eget eget mauris. In eleifend fermentum facilisis. Praesent enim enim, gravida ac sodales sed, placerat id erat. Suspendisse lacus dolor, consectetur non augue vel, vehicula interdum libero. Morbi euismod sagittis libero sed lacinia.
%
%  Sed tempus felis lobortis leo pulvinar rutrum. Nam mattis velit nisl, eu condimentum ligula luctus nec. Phasellus semper velit eget aliquet faucibus. In a mattis elit. Phasellus vel urna viverra, condimentum lorem id, rhoncus nibh. Ut pellentesque posuere elementum. Sed a varius odio. Morbi rhoncus ligula libero, vel eleifend nunc tristique vitae. Fusce et sem dui. Aenean nec scelerisque tortor. Fusce malesuada accumsan magna vel tempus. Quisque mollis felis eu dolor tristique, sit amet auctor felis gravida. Sed libero lorem, molestie sed nisl in, accumsan tempor nisi. Fusce sollicitudin massa ut lacinia mattis. Sed vel eleifend lorem. Pellentesque vitae felis pretium, pulvinar elit eu, euismod sapien.
%
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
