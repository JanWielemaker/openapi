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

## The handler

  - Predicate name from `operationId`
  - Followed by translated versions from `parameters`
  - If a POST or PUT, follow by translated content
  - Followed by response.  See below.

## Response

  - Returned object, one of
    - `status(+Status)`
    - `json(+Term)`
    - ...
  - Throw exception.
    - Need mapping of exceptions to error documents
    - Need mapping of errors from the body to above error documents

## Schema handling

  - Compile schemas to clauses for a validation predicate
  - Provide schema validator against generated dict.

## Binding

  - openapi_dispatch(:Request) provides the generic handler
