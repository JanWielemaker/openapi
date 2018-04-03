# SWI-Prolog OpenAPI (Swagger) interface

## Design

There are two options to support OpenAPI:

  1. Use an OpenAPI file as a specification for the interface, only
     providing the body implementation in Prolog.
  2. Specify the API in Prolog and generate an OpenAPI document for
     it.

This library implements the first option.  Future versions may reuse
a lot of the infrastructure to implement the second option.

## Components

The single library(openapi) implements the following components:

  1. A compiler that creates a Prolog friendly representation of
     the operations described in the swagger file.
  2. An HTTP request dispatcher that uses the above to
     - Find the operation
     - Extract the parameters (from path, query and request body)
     - Check the argument types
     - Call the (user defined) implementation
     - Check the response type
     - Return the reply document
  3. A compiler that creates the client predicates as wrappers
     around http_open/3.  The wrapper does
     - Type-check the parameters
     - Use the parameters to formulate a URL and optional
       request body for http_open/3.
     - Run the request
     - Type-check and return the returned answer

In addition, there is  a  script   `swi-openapi`  that  wraps  the above
library to create the  skeleton  server   and  client,  including  PlDoc
comments for the operations.

## Using this package

First of all, obtain a Swagger file using *Swagger version 3.0*. Now, to
generate a *server*, do

    swi-openapi --server=server.pl spec.yaml

This  creates  a  Prolog  file  `server.pl`  with  documented  predicate
skeletons that must be filled by  you.   We  advice  to write the actual
implementation of the  server  in  a   separate  module  such  that  the
implementation you have to  add  to  this   file  is  short.  This makes
upgrading and deploying multiple versions of the server API much easier.
There are additional options:

  - `--httpd` includes an HTTP server in `server.pl`.  For more
    complicated projects you probably want something more advanced.
    This quickly gets you started though.
  - `--ui` includes the swagger ui, so you can interactively explore
    the API in your browser.

To generate a *client* run

    swi-openapi --client=client.pl spec.yaml

This creates documented predicates that call the API. The server address
is extracted from `spec.yaml`.  Probably the best way to use this is to
include `client.pl` into your project using `include/1`.

## The predicate mapping

The `operationId` from the Swagger file  is   used  as the *name* of the
predicate, both for the server and client. The *arguments* are extracted
from the `parameters` specification  in   the  Swagger  file. *Required*
arguments use a normal Prolog argument. *Optional* parameters are passed
using a Prolog _option list_.  If  there   is  a  return  value, this is
positioned after the required argument and   before  the option list. On
the *client* side, normal responses are  returned as data. The `default`
is  mapped  to  an  _exception_   of  the  form  `error(rest_error(Code,
Data),_)`, where `Code` is the HTTP  status   code  and  `Data` the data
returned by the server. If  an  operation   only  defines  code 204 (_no
content_) and a default, the parameter is  missing in the client and the
predicate succeeds if the server replies 204   or throws an exception as
above otherwise.

### Server reply

The implementation skeleton for each  server   operation  has a variable
`Response`. The implementation must succeed and   bind `Response` to one
of the terms below.

  - `status(Code)`
  - `status(Code, Data)`

Currently, `Data` must  be  a   term  suitable  for `json_write_dict/3`.
Future versions will support a other replies   and  a hook to extend the
reply types.

## Examples

See the `examples` directory for two examples from the Swagger site.

# Prerequisites

SWI-Prolog 7.7.11 for basic operation. This  version   has  a bug in the
YAML parser and lacks an extension  to `http_parameters/2` that supports
more precise HTTP error reports.

