# SWI-Prolog OpenAPI (swagger) interface

## Design

There are two options to support OpenAPI:

  1. Use an OpenAPI file as a specification for the interface, only
     providing the body implementation in Prolog
  2. Specify the API in Prolog and generate an OpenAPI document for
     it.

We will first go for route (1).

## Using an OpenAPI specification

The OpenAPI is bound to Prolog using the directive below. This directive
generates the hooks into  the  HTTP   framework  and  the  wrappers that
extract the arguments. The wrapper  calls   the  body implementation. We
will provide a predicate that generates the predicate heads that must be
defined to make the service work.

    :- openapi(+OpenAPIFile, +Options).

## Parameters

Parameters for REST APIs come from multiple sources:

  - Path parameters
  - HTTP query parameters
    - Q: The HTTP protocol allows for repeating the same parameter.
      It is rearely used, but is it allowed by OpenAPI?
  - Header parameters

All are in the parameters declaration.

Q: How should we represent the parameters?  Options:

  - By position?
    - What position?  Path/Params/Header?  The generated skeleton
      could reveal that to the user.
    - How to deal with optional params?  Pass a variable?
  - As a single dict?
    - Provides access by name
    - Optional parameters are not in the dict

## Response

  - Returned object
  - Throw exception.
    - Need mapping of exceptions to error documents
    - Need mapping of errors from the body to above error documents

## Schema handling

  - Compile schemas to a test predicate
  - Provide schema validator against generated dict.

## Binding

  - openapi(:Request) provides the generic handler
