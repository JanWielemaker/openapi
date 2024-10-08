/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2024, VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(openapi,
          [ openapi_dispatch/1,                 % :Request
            openapi_server/2,                   % +File, +Options
            openapi_client/2,                   % +File, +Options

            openapi_doc/3,                      % +File, +Mode, +Options
            openapi_arg/4,                      % :PredName, ?Index, ?Name, ?Type
            openapi_response/2                  % :PredicateName, Responses:list
          ]).
:- use_module(library(apply)).
:- use_module(library(apply_macros), []).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(base64)).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(yaml)).
:- use_module(library(uri)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_header)).
:- use_module(library(listing), [portray_clause/2, portray_clause/1]).
:- use_module(library(pprint), [print_term/2]).
:- use_module(library(http/http_open)).
:- use_module(library(pcre), [re_match/3]).
:- use_module(library(dcg/high_order), [sequence/5]).

                                                % generated code.

/** <module> OpenAPI (Swagger) library

This library implements generating  server  and   client  code  from  an
OpenAPI  specification.  The  generated  code    generates  or  extracts
parameters from the path,  request  or   request  body  and  type-checks
parameters as well as responses.
*/

:- meta_predicate
    openapi_dispatch(:),
    openapi_arg(:, ?, ?, ?),
    openapi_response(:, -).

%!  openapi_server(+File, +Options)
%
%   Instantiate a REST server given the   OpenAPI specification in File.
%   Normally, use `swipl-openapi --server=server.pl spec.yaml` to create
%   a file that uses this directive  and generates documentation for the
%   server operations as well as a skeleton predicate.

openapi_server(File, Options) :-
    throw(error(context_error(nodirective, openapi_server(File, Options)), _)).

expand_openapi_server(File, Options,
                      [ (:- discontiguous((openapi_handler/11,
                                           openapi_doc/2))),
                        (:- multifile((openapi_error_hook/3)))
                      | Clauses
                      ]) :-
    read_openapi_spec(File, Spec, Options, Options1),
    phrase(server_clauses(Spec, Options1), Clauses).

%!  openapi_client(+File, +Options)
%
%   Instantiate a REST client given the   OpenAPI specification in File.
%   Normally use `swipl-openapi --client=client.pl  spec.yaml` to create
%   a file that uses this directive   and contains documentation for the
%   generated predicates.

openapi_client(File, Options) :-
    throw(error(context_error(nodirective,
                              openapi_client(File, Options)), _)).

%!  expand_openapi_client(+File, +Options, -Clauses)
%
%   Generate clauses for the client. Currently also generates the server
%   specification as this allows us to use  the same code for generating
%   the documentation.

expand_openapi_client(File, Options, AllClauses) :-
    AllClauses = [ (:- discontiguous(openapi_type/1))
                 | Clauses
                 ],
    read_openapi_spec(File, Spec, Options, Options1),
    phrase(client_clauses(Spec, Options1), Clauses).

%!  read_openapi_spec(+File, -Spec, +Options0, -Options) is det.

read_openapi_spec(File, Spec, Options0, [yaml(Spec)|Options]) :-
    (   prolog_load_context(directory, Dir)
    ->  true
    ;   Dir = '.'
    ),
    absolute_file_name(File, Path,
                       [ relative_to(Dir),
                         extensions(['',json,yaml]),
                         access(read)
                       ]),
    uri_file_name(BaseURI, Path),
    openapi_read(Path, Spec),
    merge_options(Options0, [base_uri(BaseURI)], Options).

%!  openapi_read(+File, -Term) is det.
%
%   Read an OpenAPI specification file.

openapi_read(File, Term) :-
    file_name_extension(_, yaml, File),
    !,
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        yaml_read(In, Term),
        close(In)).
openapi_read(File, Term) :-
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        json_read_dict(In, Term),
        close(In)).

		 /*******************************
		 *       SERVER COMPILER	*
		 *******************************/

%!  server_clauses(+JSONTerm, +Options)//
%
%   Grammar to generate clauses that control openapi/1.  Options
%   processed:
%
%     - base_uri(+URI)
%       Base URI for resolving types.
%     - type_check_response(+Boolean)
%       Check the response JSON against the schema (default `true`)
%     - format_response(+Boolean)
%       If `true` (default `false`), generate JSON with a nice
%       layout.

server_clauses(JSONTerm, Options) -->
    { dict_pairs(JSONTerm.paths, _, Paths)
    },
    root_clause(JSONTerm, Options),
    server_config_clauses(Options),
    server_path_clauses(Paths, Options),
    json_schema_clauses(JSONTerm, Options).

root_clause(_, Options) -->
    { option(server_url(ServerURL), Options),
      !,
      uri_components(ServerURL, Components),
      uri_data(path, Components, Root)
    },
    !,
    [ openapi_root(Root) ].
root_clause(Spec, _Options) -->
    { Spec.get(servers) = [Server|_],
      !,
      uri_components(Server.url, Components),
      uri_data(path, Components, Root)
    },
    [ openapi_root(Root) ].
root_clause(_Spec, _Options) -->
    [ openapi_root('') ].

server_config_clauses(Options) -->
    { findall(Clause, server_config(Clause, Options), Clauses) },
    string(Clauses).

server_config(openapi_server_config(type_check_response(Mode)), Options) :-
    option(type_check_response(Mode), Options, true).
server_config(openapi_server_config(reply_json_options(Opts)), Options) :-
    (   option(format_response(false), Options, false)
    ->  Opts = [width(0)]
    ;   Opts = []
    ).

server_path_clauses([], _) --> [].
server_path_clauses([H|T], Options) -->
    (   server_path_clause(H, Options)
    ->  []
    ;   { error(openapi(path_failed, H), Options),
          start_debugger
        }
    ),
    server_path_clauses(T, Options).

server_path_clause(Path-Spec, Options) -->
    { dict_pairs(Spec, _, Methods0),
      (   selectchk(parameters-Parms0, Methods0, Methods)
      ->  deref(Parms0, Parms, Options),
          Options1 = [parameters(Parms)|Options]
      ;   Methods = Methods0,
          Options1 = Options
      )
    },
    path_handlers(Methods, Path, Options1).

path_handlers([], _Path, _) --> [].
path_handlers([Method-Spec|T], Path, Options) -->
    { path_handler(Path, Method, Spec, Fact, Options),
      path_docs(Method, Path, Spec, Docs, Options)
    },
    [Fact, Docs],
    path_handlers(T, Path, Options).

%!  path_handler(+Path, +Method, +Spec, -Handler, +Options) is det.
%
%   Gather information about Method for  Path   from  the YAML term Spec
%   that describes this pair.

path_handler(Path, Method, Spec,
             openapi_handler(Method, PathList, SegmentMatches,
                             Request, HdrParams, AsOption, OptionParam,
                             Content, Responses, Security, Handler),
             Options) :-
    path_vars(Path, PathList, PathBindings),
    (   spec_parameters(Spec, ParamSpecs, Options)
    ->  server_parameters(ParamSpecs, PathBindings, SegmentMatches,
                          Request, AsOption, TypeAndArgs0, HdrParams,
                          [ path(Path),
                            method(Method)
                          | Options
                          ]),
        (   AsOption == []
        ->  OptionParams = []
        ;   OptionParams = [OptionParam]
        )
    ;   PathBindings == []
    ->  SegmentMatches = [],
        TypeAndArgs0 = [],
        HdrParams = [],
        Request = [],
        AsOption = [],
        OptionParams = []
    ;   error(openapi(not_covered_path_vars(Method, Path, PathBindings)),
              Options),
        fail
    ),
    content_parameter(Method, Spec, Content, TypeAndArgs0, TypeAndArgs, Options),
    maplist(arg(1), TypeAndArgs, Args),
    append(Args, [Result|OptionParams], AllArgs),
    dict_pairs(Spec.responses, _, ResPairs),
    maplist(response(Result, Options), ResPairs, Responses),
    spec_security(Spec, Security, Options),
    handler_predicate(Method, Path, Spec, PredName, Options),
    Handler =.. [PredName|AllArgs].

spec_parameters(Spec, Parameters, Options) :-
    option(parameters(Common), Options, []),
    (   Me0 = Spec.get(parameters)
    ->  deref(Me0, Me, Options)
    ;   Me = []
    ),
    append(Common, Me, Parameters),
    Parameters \== [].

%!  server_parameters(+ParamSpecs, +PathBindings,
%!                    -SegmentMatches, -RequestParams, -RequestOptions,
%!                    -HandlerArgs, -HeaderOptions, +Options) is det.
%
%   @arg HandlerArgs is a list of a(Var,Name,Type) for the positional
%        input arguments of the server handler predicates.

server_parameters([], _, [], [], [], [], [], _).
server_parameters([H|T], PathB, Segs, Request, AsOption, Args, HdrOpts, Options) :-
    _{name:NameS, in:"query"} :< H,
    !,
    phrase(http_param_options(H, Options), Opts),
    atom_string(Name, NameS),
    R0 =.. [Name,P0,Opts],
    (   Opts = [optional(true)|_],
        \+ option(optional(unbound), Options)
    ->  AsOption = [R0|AsOpts],
        server_parameters(T, PathB, Segs, Request, AsOpts, Args, HdrOpts, Options)
    ;   Request = [R0|Req],
        param_type(H, Type, Options),
        Args  = [a(P0,Name,Type)|MoreArgs],
        server_parameters(T, PathB, Segs, Req, AsOption, MoreArgs, HdrOpts, Options)
    ).
server_parameters([H|T], PathB, [segment(Type, Seg, A0, Name, Descr)|Segs],
                  Req, AsOption, [a(A0,Name,Type)|Args], HdrOpts, Options) :-
    _{name:NameS, in:"path"} :< H,
    !,
    atom_string(Name, NameS),
    (   memberchk(Name=Seg, PathB)
    ->  param_type(H, Type, Options),
        param_description(H, Descr)
    ;   option(path(Path), Options),
        option(method(Method), Options),
        error(openapi(missing_path_parameter(Method, Name, Path)), Options),
        fail
    ),
    server_parameters(T, PathB, Segs, Req, AsOption, Args, HdrOpts, Options).
server_parameters([H|T], PathB, Segs, Req, AsOption, Args, [R0|HdrOpts], Options) :-
    _{name:NameS, in:"header"} :< H,
    !,
    phrase(http_param_options(H, Options), Opts),
    atom_string(Name, NameS),
    R0 =.. [Name,A0,Opts],
    (   Opts = [optional(true)|_],
        \+ option(optional(unbound), Options)
    ->  AsOption = [R0|AsOpts],
        server_parameters(T, PathB, Segs, Req, AsOpts, Args, HdrOpts, Options)
    ;   param_type(H, Type, Options),
        Args  = [a(A0,Name,Type)|MoreArgs],
        server_parameters(T, PathB, Segs, Req, AsOption, MoreArgs, HdrOpts, Options)
    ).
server_parameters([H|T], PathB, Segs, Request, AsOption, Args, HdrOpts, Options) :-
    deref(H, Param, Options),
    !,
    server_parameters([Param|T], PathB, Segs, Request, AsOption, Args, HdrOpts, Options).
server_parameters([H|_], _PathB, _Segments, _Req, _AsOption, _, _HdrOpts, Options) :-
    error(openapi(parameter_failed(H)), Options),
    fail.

http_param_options(Spec, Options) -->
    hp_optional(Spec),
    hp_type(Spec, Options),
    hp_description(Spec).

hp_optional(Spec) -->
    { param_optional(Spec, optional) },
    !,
    [optional(true)].
hp_optional(_) --> [].

hp_type(Spec, Options) -->
    hp_schema(Spec.get(schema), Options),
    !.
hp_type(_, _) --> [].

hp_schema(Spec, Options) -->
    { json_type(Spec, Type, Options),
      json_param_type(Type, ParmType)
    },
    !,
    [ ParmType ].
hp_schema(_Spec, _Options) -->
    { start_debugger_fail }.

json_param_type(array(Type, _), list(openapi(Type))) :- !.
json_param_type(Type, openapi(Type)).

hp_description(Spec) -->
    { Descr = Spec.get(description) },
    !,
    [ description(Descr) ].
hp_description(_) --> [].

deref(Spec, Yaml, Options) :-
    is_dict(Spec),
    _{'$ref':URLS} :< Spec,
    sub_atom(URLS, 0, _, _, './'),
    !,
    option(base_uri(Base), Options),
    uri_normalized(URLS, Base, URL),
    url_yaml(URL, Yaml).
deref(Spec, Yaml, Options) :-
    is_dict(Spec),
    _{'$ref':Ref} :< Spec,
    atomic_list_concat(Segments, /, Ref),
    !,
    option(yaml(Doc), Options),
    yaml_subdoc(Segments, Doc, Yaml).
deref(Yaml, Yaml, _).

yaml_subdoc([], Doc, Doc).
yaml_subdoc([H|T], Doc, Sub) :-
    (   (H == '' ; H == '#')
    ->  Sub0 = Doc
    ;   Sub0 = Doc.H
    ),
    yaml_subdoc(T, Sub0, Sub).

%!  path_docs(+Method, +Path, +Spec, -Docs) is det.
%
%   Generate documentation clauses for an operationId

path_docs(Method, Path, Spec,
          openapi_doc(OperationID, [path(Path)|Docs]),
          Options) :-
    handler_predicate(Method, Path, Spec, OperationID, [warn(false)|Options]),
    phrase(path_doc(Spec), Docs).

%!  path_doc(+Spec)//
%
%   Generate a list of documentation properties for Path

path_doc(Spec) -->
    path_doc(summary, Spec),
    path_doc(description, Spec),
    path_doc(tags, Spec).

path_doc(Key, Spec) -->
    { Value = Spec.get(Key),
      !,
      Attr =.. [Key,Value]
    },
    [Attr].
path_doc(_, _) --> [].


%!  path_vars(+PathSpec, -Segments, -Bindings) is det.
%
%   Convert a path specification holding {Name} into a list of Segments,
%   where each Segment is an atom or a   variable. Bindings is a list of
%   `Name=Var`, e.g.
%
%       ?- path_vars('/aap/{noot}/mies', Segs, B).
%       Segs = ['/aap/', _A, '/mies'],
%       B = [noot=_A].

path_vars(PathSpec, Segments, Bindings) :-
    string_codes(PathSpec, Codes),
    phrase(path_vars(Segments, Bindings), Codes).

path_vars([Segment,Var|Segments], [VarName=Var|Bindings]) -->
    string(SegCodes), "{", string(VarCodes), "}",
    !,
    { atom_codes(Segment, SegCodes),
      atom_codes(VarName, VarCodes)
    },
    path_vars(Segments, Bindings).
path_vars(Segments, []) -->
    remainder(Codes),
    {   Codes == []
    ->  Segments = []
    ;   atom_codes(Segment, Codes),
        Segments = [Segment]
    }.

%!  match_path_list(+PathList, +Path) is semidet.
%
%   Where PathList is a list of atoms and variables and Path is an atom.
%   Bind the variables such that the   concatenation of PathList results
%   in Path. Each variable is bound to a string.

match_path_list([], "").
match_path_list([Path], Path) :-
    !.
match_path_list([Atom], String) :-
    !,
    atom_string(Atom, String).
match_path_list([H|T], Path) :-
    nonvar(H),
    !,
    string_concat(H, Rest, Path),
    match_path_list(T, Rest).
match_path_list([V,H|T], Path) :-
    assertion(nonvar(H)),
    sub_string(Path, B, _, A, H),
    sub_string(Path, 0, B, _, V),
    sub_string(Path, _, A, 0, Rest),
    match_path_list(T, Rest),
    !.

%! content_parameter(+Method, +Spec, -Content,
%!                   +ArgAndTypes0, -ArgAndTypes, +Options) is det.
%
%  If there is a request body, add it to the parameter list and return a
%  specification for openapi_dispatch/1 in Content.

content_parameter(Method, Spec, content(MediaType, Schema, Var, Descr),
                  Args, AllArgs, Options) :-
    has_content(Method),
    !,
    request_content_type(Spec, MediaType, Schema, Options),
    content_description(Spec, Descr),
    append(Args, [a(Var,'RequestBody',Schema)], AllArgs).
content_parameter(_, _, -, Args, Args, _).

has_content(post).
has_content(put).

content_description(JSON, Descr) :-
    Descr = JSON.get(requestBody).get(description),
    !.
content_description(_JSON, "").

request_content_type(Spec, MediaType, Schema, Options) :-
    (   Body = Spec.get(requestBody)
    ->  true
    ;   Body = _{}
    ),
    !,
    content_type(Body, MediaType, Schema, Options).

%!  response(+ResultVar, +Options, +ResponsePair, -Response) is det.
%
%   Describe the valid responses.  Response is a term
%
%     - response(Code, As, MediaType, Type, Result, Descr)
%       Where
%       - Code is the numeric HTTP status code or a variable for
%         `default`
%       - As describes how to handle the code.  Currently one of
%         `data` or `error`
%       - MediaType is the expected response type
%       - Type is the (JSON) schema describing a JSON result
%       - Result is the result variable
%       - Descr is the description of the response body.

response(Result, Options, CodeA-Spec,
         response(Code, As, MediaType, Type, Result, Descr)) :-
    response_code(CodeA, Code, As),
    response_description(Spec, Descr),
    content_type(Spec, MediaType, Type, Options).

response_code(default, _, error) :- !.
response_code(A, N, data) :-
    to_number(A, N).

response_description(Spec, Descr) :-
    Descr = Spec.get(description),
    !.
response_description(_, "") .

%!  content_type(+Sec, -BodyType, -Schema, +Options) is det.
%
%   Find the ContentType for the request   body  and, if applicable, its
%   schema.

content_type(_Spec, media(application/json, []), Type, Options) :-
    option(type_check_request(false), Options),
    !,
    Type = (-).
content_type(Spec, media(application/json, []), Type, Options) :-
    Content = Spec.get(content),
    Media = Content.get('application/json'),
    !,
    (   Schema = Media.get(schema)
    ->  json_type(Schema, Type, Options)
    ;   Type = (-)
    ).
content_type(_Spec, media(Type, []), -, Options) :-
    option(default_request_body_type(Type0), Options),
    !,
    to_content_type(Type0, Type).
content_type(_Spec, media(application/json, []), -, _).

to_content_type(Type0, Main/Sub) :-
    atomic(Type0),
    atomic_list_concat([Main,Sub], /, Type0),
    !.
to_content_type(Type, Type) :-
    Type = Main/Sub,
    must_be(atom, Main),
    must_be(atom, Sub).
to_content_type(Type, _) :-
    type_error(content_type, Type).


		 /*******************************
		 *       CLIENT COMPILER	*
		 *******************************/

%!  client_clauses(+JSONTerm, +Options)//
%
%   Generate clauses for the client.  The generated clauses are:
%
%     - openapi_server(URL)
%       One or more clauses describing the location of the server
%       as defined in the OpenAPI file.  If the option
%       server_url(ServerURL) is provided, this replaces the
%       locations found in the OpenAPI file.
%     - Clauses that call the REST methods.  The name is the
%       `operationId` described in the Swagger file.  The arguments
%       are defined by the parameters and response from the
%       OpenAPI specification.
%     - openapi:json_schema(URL, Type)
%       Clauses that define the JSON schema types.

client_clauses(JSONTerm, Options) -->
    { dict_pairs(JSONTerm.paths, _, Paths)
    },
    server_url_clauses(JSONTerm.servers, Options),
    client_path_clauses(Paths, Options),
    json_schema_clauses(JSONTerm, Options).

server_url_clauses(_Servers, Options) -->
    { option(server_url(ServerURL), Options)
    },
    !,
    [ openapi_server(ServerURL) ].
server_url_clauses(Servers, _Options) -->
    server_url_clauses(Servers).

server_url_clauses([]) --> [].
server_url_clauses([H|T]) --> server_url_clauses(H), server_url_clauses(T).

server_url_clauses(Server) -->
    [ openapi_server(Server.get(url)) ].

client_path_clauses([], _) --> [].
client_path_clauses([H|T], Options) -->
    (   client_path_clause(H, Options)
    ->  []
    ;   { error(openapi(path_failed, H), Options) }
    ),
    client_path_clauses(T, Options).

client_path_clause(Path-Spec, Options) -->
    { dict_pairs(Spec, _, Methods0),
      (   selectchk(parameters-Parms, Methods0, Methods)
      ->  Options1 = [parameters(Parms)|Options]
      ;   Methods = Methods0,
          Options1 = Options
      )
    },
    client_handlers(Methods, Path, Options1).

client_handlers([], _, _) --> [].
client_handlers([H|T], Path, Options) -->
    { client_handler(H, Path, Clause, TypeClause, Options) },
    [Clause, TypeClause],
    client_handlers(T, Path, Options).

:- det(client_handler/5).
client_handler(Method-Spec, PathSpec, (Head :- Body), openapi_type(TypeHead), Options) :-
    path_vars(PathSpec, PathList, PathBindings),
    handler_predicate(Method, PathSpec, Spec, PredName, Options),
    (   spec_parameters(Spec, ParamSpecs, Options)
    ->  client_parameters(ParamSpecs, PathBindings,
                          ArgAndTypes, HdrParams, Query, Optional,
                          CheckParams,
                          [ path(PathSpec),
                            method(Method)
                          | Options
                          ]),
        (   Optional == []
        ->  ClientOptionArgs = []
        ;   ClientOptionArgs = [ClientOptions]
        )
    ;   PathBindings == []
    ->  ArgAndTypes = [],
        Query = [],
        CheckParams = true,
        Optional = [],
        ClientOptionArgs = [],
        HdrParams = []
    ;   error(openapi(not_covered_path_vars(Method, PathSpec, PathBindings)),
              Options),
        fail
    ),
    content_parameter(Method, Spec, Content, ArgAndTypes, ArgAndTypes1, Options),
    maplist(arg(1), ArgAndTypes1, Args),
    maplist(client_arg, ArgAndTypes1, ClientArgs),
    TypeHead =.. [PredName|ClientArgs],
    request_body(Method, PathSpec, Module, Content, ContentGoal, RequestOptions),
    dict_pairs(Spec.responses, _, ResPairs),
    maplist(response(Result, Options), ResPairs, Responses),
    (   response_has_data(Responses)
    ->  ResultArgs = [Result]
    ;   ResultArgs = []
    ),
    append([ Args,
             ResultArgs,
             ClientOptionArgs
           ], AllArgs),
    spec_security(Spec, Security, Options),
    prolog_load_context(module, Module),
    (   PathBindings == []
    ->  Path = PathSpec,
        PathGoal = true
    ;   PathGoal = atomic_list_concat(PathList, Path)
    ),
    Head =.. [PredName|AllArgs],
    Body = ( CheckParams, PathGoal, ContentGoal,
             openapi:assemble_query(Module, Method, Path,
                                    HdrParams, Query, Optional, ClientOptions,
                                    URL, HdrOptions),
             context_module(CM),
             openapi:assemble_security(Security, CM, SecOptions),
             append([ SecOptions,
                      RequestOptions,
                      HdrOptions
                    ], OpenOptions),
             debug(openapi(client), '~w ~w', [Method, URL]),
             setup_call_cleanup(
                 openapi:http_open(URL, In,
                           [ status_code(Status),
                             method(Method),
                             header(content_type, ContentType),
                             request_header(accept = 'application/json')
                           | OpenOptions
                           ]),
                 openapi:openapi_read_reply(Status, ContentType, Responses,
                                            In, Result),
                 close(In))
           ).

:- det(client_arg/2).
client_arg(a(_, Name, Type), ArgName:Type) :-
    camel_case(Name, ArgName).

%!  handler_predicate(+Method, +Path, +Spec, -PredicateName, +Options) is det.
%
%   Generate  a  predicate  name  from   a  specification.  Prefers  the
%   `operationId`.

handler_predicate(_, _, Spec, PredicateName, _Options) :-
    uncamel_case(Spec.get(operationId), PredicateName),
    !.
handler_predicate(Method, Path, _Spec, PredicateName, Options) :-
    atomic_list_concat(Segments, /, Path),
    reverse(Segments, RevSegments),
    member(Segment, RevSegments),
    \+ sub_atom(Segment, _, _, _, '{'),
    !,
    file_name_extension(Name, _, Segment),
    atomic_list_concat([Method, '_', Name], PredicateName),
    (   option(warn(true), Options, true)
    ->  warning(openapi(no_operation_id, Method, Path, PredicateName), Options)
    ;   true
    ).


%!  response_has_data(+Responses) is semidet.
%
%   True if the request (may) return data. This   is not the case if the
%   only responses are 204 (no content) or   error codes that are mapped
%   to exceptions.

response_has_data(Responses) :-
    maplist(arg(1), Responses, Codes),
    member(Code, Codes),
    \+ code_has_no_data(Code), !.

code_has_no_data(Code) :-
    var(Code).                                  % errors
code_has_no_data(204).                          % No content

%!  client_parameters(+Spec, +PathBindings,
%!                    -ArgsAndTypes, -HdrParams, -Required, -Optional,
%!                    -Check:callable, +Options)
%
%   @arg Args is a list of pairs a(Var,Name,Type) for required arguments
%   of the client predicate.
%   @arg Required is a list of qparam(Name,P0,Type,Opt) used for
%   adding query parameters for required parameters to the URL
%   @arg Optional is a list of qparam(Name,P0,Type,optional) used for
%   adding query parameters for optional parameters to the URL
%   @arg Check is a callable term for validating the arguments,

client_parameters([], _, [], [], [], [], true, _).
client_parameters([H|T], PathBindings, [a(A0,Name,Type)|Args], HdrParams,
                  [qparam(Name,A0,Type,Opt)|Qs], Optional, Check, Options) :-
    _{name:NameS, in:"query"} :< H,
    param_optional(H, Opt),
    \+ ( Opt == optional,
         \+ option(optional(unbound), Options)
       ),
    !,
    param_type(H, Type, Options),
    atom_string(Name, NameS),
    client_parameters(T, PathBindings, Args, HdrParams, Qs, Optional, Check, Options).
client_parameters([H|T], PathBindings, [a(A0,Name,Type)|Args],
                  [hparam(Name,A0,Type,Opt)|HdrParams],
                  Query, Optional, Check, Options) :-
    _{name:NameS, in:"header"} :< H,
    param_optional(H, Opt),
    \+ ( Opt == optional,
         \+ option(optional(unbound), Options)
       ),
    !,
    param_type(H, Type, Options),
    atom_string(Name, NameS),
    client_parameters(T, PathBindings, Args, HdrParams, Query, Optional, Check, Options).
client_parameters([H|T], PathBindings,
                  Params, HdrParams, Query, [qparam(Name,_,Type,optional)|OptT],
                  Check, Options) :-
    _{name:NameS, in:"query"} :< H,
    !,
    param_type(H, Type, Options),
    atom_string(Name, NameS),
    client_parameters(T, PathBindings, Params, HdrParams,
                      Query, OptT, Check, Options).
client_parameters([H|T], PathBindings, Args,
                  [hparam(Name,_,Type,optional)|HdrParams], Query, Optional,
                  Check, Options) :-
    _{name:NameS, in:"header"} :< H,
    !,
    param_type(H, Type, Options),
    atom_string(Name, NameS),
    client_parameters(T, PathBindings, Args, HdrParams,
                      Query, Optional, Check, Options).
client_parameters([H|T], PathBindings, [a(A0,Name,Type)|Args],
                  HdrParams, Query, Opt, Check, Options) :-
    _{name:NameS, in:"path"} :< H,
    !,
    atom_string(Name, NameS),
    param_type(H, Type, Options),
    (   memberchk(Name=Segment, PathBindings)
    ->  Check1 = openapi:segment_value(Type, Segment, A0)
    ;   option(path(Path), Options),
        option(method(Method), Options),
        error(openapi(missing_path_parameter(Method, Name, Path)), Options),
        fail
    ),
    client_parameters(T, PathBindings, Args, HdrParams,
                      Query, Opt, Check0, Options),
    mkconj(Check0, Check1, Check).
client_parameters([H|T], PathBindings, Args, HdrParams,
                  Query, Opt, Check, Options) :-
    deref(H, Param, Options),
    !,
    client_parameters([Param|T], PathBindings, Args, HdrParams,
                      Query, Opt, Check, Options).

param_optional(Spec, Optional) :-
    (   Spec.get(required) == false
    ->  Optional = optional
    ;   _Default = Spec.get(schema).get(default)
    ->  Optional = optional
    ;   Optional = required
    ).

param_type(Spec, Type, Options) :-
    json_type(Spec.get(schema), Type, Options),
    !.
param_type(_Spec, any, _Options).

param_description(Spec, Description) :-
    Description = Spec.get(description),
    !.
param_description(_Spec, "").

mkconj(true, G, G) :- !.
mkconj(G, true, G) :- !.
mkconj(G1, G2,  (G1,G2)).

%!  request_body(+Method, +Path, +Module,
%!               +ContentSpec, -Goal, -HTTPOPenOptions) is det.
%
%   Translate the request body into options for http_open/3.

request_body(Method, Path, Module,
	     content(media(application/json,_), Schema, InVar, _Descr),
             openapi:assemble_content(Module, Method, Path,
                                      json, Schema, InVar, OutVar),
             [ post(json(OutVar))
             ]) :-
    !.
request_body(Method, Path, Module,
	     content(media(multipart/'form-data',_), Schema, InVar, _Descr),
             openapi:assemble_content(Module, Method, Path,
                                      form_data, Schema, InVar, OutVar),
             [ post(form_data(OutVar))
             ]) :-
    !.
request_body(_, _, _, content(MediaType, _Schema, _Var, _Descr), _, _) :-
    !,
    domain_error(openapi(content_type), MediaType).
request_body(_, _, _, _, true, []).


		 /*******************************
		 *           SECURITY		*
		 *******************************/

%!  spec_security(+MethodSpec, -Security:list, +Options) is det.
%
%   Decode the required authentication for   sending a request. Security
%   is a list of admissible authentication methods and has the following
%   possible values:
%
%     - public
%       No authentication needed. This is (with a warning) also emitted
%       for schemes we do not yet support.
%     - http(Scheme, Name, Args)
%       For http `basic` and http `bearer` authentications.  Name is
%       the name of the security scheme from the OpenAPI document.
%     - api_key(header(Header), Name, Args)
%       We need to provide an api key in an additional header named
%       Header. Name is the name of the security scheme from the OpenAPI
%       document.
%
%   @tbd Currently only deals with authorization we need in dealing with
%   the hypothesis API.

spec_security(Spec, Security, Options) :-
    maplist(security(Options), Spec.get(security), Security),
    Security \== [],
    !.
spec_security(_, [public], _).

security(Options, Sec, Security) :-
    dict_pairs(Sec, _, [Scheme-Args]),
    option(yaml(Doc), Options),
    yaml_subdoc([components, securitySchemes,Scheme], Doc, SchemeObj),
    security_scheme(Scheme, SchemeObj, Args, Security, Options).
security(_Options, Sec, public) :-
    dict_pairs(Sec, _, []),
    !.

security_scheme(SchemeName, Dict, Args,
                http(Scheme, SchemeName, Args), _Options) :-
    _{type: "http", scheme: SchemeS} :< Dict,
    !,
    atom_string(Scheme, SchemeS).
security_scheme(SchemeName, Dict, Args,
                api_key(header(Name), SchemeName, Args), _Options) :-
    _{type: "apiKey", in: "header", name: NameS} :< Dict,
    !,
    atom_string(Name, NameS).
security_scheme(SchemeName, Dict, _, public, Options) :-
    warning(openapi(unknown_security_scheme(SchemeName, Dict)), Options).


		 /*******************************
		 *       RUNTIME SUPPORT	*
		 *******************************/

:- public
    assemble_query/9,
    assemble_content/7.

%!  assemble_query(+Module, +Method, +Path, +HeaderParams, +QParams,
%!                 +QOptional, +QOptions, -URL, -OpenOptions) is det.
%
%   @arg QOptions is the option list of the client predicate.

assemble_query(Module, Method, Path, HeaderParams, QParams, QOptional, QOptions,
               URL, OpenOptions) :-
    call(Module:openapi_server(ServerBase)),
    convlist(client_query_param, QParams, QueryFromArgs),
    optional_query_params(QOptional, QOptions, QueryFromOptions),
    application_extra_query_parameters(Module, Method, Path, Extra),
    append([Extra, QueryFromArgs, QueryFromOptions], Query),
    (   Query == []
    ->  atomics_to_string([ServerBase, Path], URL)
    ;   phrase(array_query(Query), ArrayQuery),
        uri_query_components(QueryString, ArrayQuery),
        atomics_to_string([ServerBase, Path, "?", QueryString], URL)
    ),
    convlist(client_header_param(QOptions), HeaderParams, OpenOptions).

assemble_content(Module, Method, Path, Format, Schema, In, Content) :-
    (   Schema == (-)
    ->  Content0 = In
    ;   json_check(Schema, Content0, In)
    ),
    (   current_predicate(Module:extend_content/5),
        Module:extend_content(Method, Path, json, Content0, Content1)
    ->  true
    ;   Content1 = Content0
    ),
    output_format(Format, Content1, Content).

output_format(json, Content, Content).
output_format(form_data, Dict, Form) :-
    dict_pairs(Dict, _, FormPairs),
    maplist(form_entry, FormPairs, Form).

form_entry(Name-Value, Name=Value).

%!  application_extra_query_parameters(+Module, +Method, +Path, -Extra) is det.
%
%   Allow a client to specify additional   query  parameters that do not
%   appear in the OpenAPI  spec  but  apply   to  all  methods.  This is
%   sometimes used to supply credentials.

application_extra_query_parameters(Module, Method, Path, Extra) :-
    current_predicate(Module:extra_query_parameters/3),
    Module:extra_query_parameters(Method, Path, Extra),
    !,
    must_be(list, Extra).
application_extra_query_parameters(_, _, _, []).



%!  array_query(Query)//
%
%   Rewrite Name=List into Name=E1, Name=E2, ... to support array(Type,
%   Opts) for parameters passed as queries.

array_query([]) --> [].
array_query([Name=Value|T]) -->
    (   {is_list(Value)}
    ->  repeat_query(Value, Name)
    ;   [Name=Value]
    ),
    array_query(T).

repeat_query([], _) --> [].
repeat_query([H|T], Name) -->
    [ Name=H ],
    repeat_query(T, Name).

%!  client_query_param(+Spec, -QueryElement) is det.
%
%   Perform type validation and  transformation   for  the client Prolog
%   value to something suitable to pass onto uri_query_components/2.

client_query_param(qparam(Name, PlValue, Type, _Required),
                   Name = Value) :-
    nonvar(PlValue),
    !,
    (   Type == any
    ->  Value = PlValue
    ;   json_check(Type, Value, PlValue)
    ).
client_query_param(qparam(_Name, _PlValue, _Type, optional), _) :-
    !, fail.                                    % leave to convlist/3.
client_query_param(qparam(_Name, PlValue, Type, required), _) :-
    type_error(Type, PlValue).

optional_query_params([], _, []).
optional_query_params([qparam(Name, PlValue, Type, optional)|T0], Options, Q) :-
    Term =.. [Name,PlValue],
    option(Term, Options),
    !,
    json_check(Type, Value, PlValue),
    Q = [Name=Value|QT],
    optional_query_params(T0, Options, QT).
optional_query_params([_|T0], Options, Q) :-
    optional_query_params(T0, Options, Q).

%!  client_header_param(+QOptions, +HeaderParam, -Header) is semidet.
%
%

client_header_param(_QOptions, hparam(Name, PlValue, Type, _Required),
                    request_header(Name=Value)) :-
    nonvar(PlValue),
    !,
    (   Type == any
    ->  Value = PlValue
    ;   json_check(Type, Value, PlValue)
    ).
client_header_param(QOptions, hparam(Name, _PlValue, Type, _Required),
                    request_header(Name=Value)) :-
    Opt =.. [Name,PlValue],
    option(Opt, QOptions),
    !,
    json_check(Type, Value, PlValue).
client_header_param(_QOptions, hparam(Name, _PlValue, _Type, required),
                    _) :-
    existence_error(openapi_option, Name).

%!  segment_value(+Type, ?Segment, ?Prolog) is det.
%
%   Transform between a Segment string and the Prolog value according to
%   Type.

segment_value(Type, Segment, Prolog) :-
    nonvar(Segment),
    !,
    uri_encoded(segment, Value, Segment),
    json_check(Type, Value, Prolog).
segment_value(Type, Segment, Prolog) :-
    json_check(Type, Value, Prolog),
    uri_encoded(segment, Value, Segment).

%!  openapi_read_reply(+Code, +ContentType, +Responses, +In, -Result) is det.
%
%   Handle the reply at the client side.

:- public openapi_read_reply/5.

openapi_read_reply(Code, _ContentType, Responses, _In, Result) :-
    no_content(Code),
    !,
    (   memberchk(response(Code, _As, _ExpectedContentType, _Type, _Result, _Comment),
                  Responses)
    ->  Result = true
    ;   maplist(arg(1), Responses, ExCodes),
        throw(error(openapi_invalid_reply(Code, ExCodes, ""), _))
    ).
openapi_read_reply(Code, ContentType, Responses, In, Result) :-
    debug(openapi(reply), 'Got code ~p; type: ~p; response schemas: ~p',
          [Code, ContentType, Responses]),
    http_parse_header_value(content_type, ContentType, ParsedContentType),
    (   memberchk(response(Code, As, ExpectedContentType, Type, _Result, _Comment),
                  Responses)
    ->  true
    ;   read_reply(ParsedContentType, -, data, Code, In, Error),
        maplist(arg(1), Responses, ExCodes),
        throw(error(openapi_invalid_reply(Code, ExCodes, Error), _))
    ),
    content_matches(ExpectedContentType, ParsedContentType, ProcessType),
    read_reply(ProcessType, Type, As, Code, In, Result).

no_content(204).

content_matches(ContentType, ContentType, ContentType) :- !.
content_matches(media(Type, _), media(Type, Attrs), media(Type, Attrs)) :- !.
content_matches(Expected, Got, _) :-
    type_error(media(Expected), Got).

read_reply(media(application/json, _), Type, As, Code, In, Result) :-
    json_read_dict(In, Result0, []),
    (   debugging(openapi(reply_object))
    ->  print_term(Result0, [])
    ;   true
    ),
    (   Type = (-)
    ->  Result = Result0
    ;   json_check(Type, Result0, Result1)
    ),
    reply_result(As, Code, Result1, Result).

reply_result(data,  _Code, Result, Result).
reply_result(error, Code, Result, _ ) :-
    throw(error(rest_error(Code, Result), _)).

%!  assemble_security(+Security, +ClientModule, -HTTPOptions)
%
%   Assemble additional HTTP options from the security description.

:- public assemble_security/3.
assemble_security(Security, CM, SecOptions) :-
    current_predicate(CM:security_options/2),
    CM:security_options(Security, SecOptions), !.
assemble_security(Security, _, []) :-
    memberchk(public, Security),
    !.
assemble_security(Security, _, _) :-
    existence_error(security_data, Security).

%!  security_options(+Security:list, -SecOptions:list)
%
%   Multifile hook to provide additional HTTP   options  for realizing a
%   specific security/authentication. The application   must define this
%   hook for dealing with authentication.   The possible Security inputs
%   are described with spec_security/3. If this   hook fails __and__ the
%   API  handler  may  be  accessed   without  security  access  without
%   additional options is tried. If this   hook fails and authentication
%   is  required  the  client  call    raises   an  existence_error  for
%   `security_data`.


		 /*******************************
		 *          DISPATCHER		*
		 *******************************/

%!  openapi_dispatch(:Request) is semidet.
%
%   Generic HTTP handler to deal with OpenAPI REST requests.
%
%   @tbd - validate types
%	 - handle errors
%	 - different replies formats
%	 - different reply codes

openapi_dispatch(M:Request) :-
    memberchk(path(FullPath), Request),
    memberchk(method(Method), Request),
    M:openapi_root(Root),
    atom_concat(Root, Path, FullPath),
    M:openapi_handler(Method, PathList, Segments,
                      Required, HdrParams, AsOption, OptionParam, Content,
                      Responses, _Security,
                      Handler),
    match_path_list(PathList, Path),
    !,
    (   Error = error(_,_),
        catch(openapi_run(M:Request,
                          Segments,
                          Required, HdrParams, AsOption, OptionParam, Content,
                          Responses,
                          Handler),
              Error,
              openapi_error(M, Error, Responses))
    ->  true
    ;   openapi_error(M, failed, Responses)
    ).

openapi_run(Module:Request,
            Segments,
            Required, HdrParams, AsOption, OptionParam, Content,
            Responses,
            Handler) :-
    append(Required, AsOption, RequestParams),
    IE = error(_,_),
    catch(( maplist(segment_parameter, Segments),
            maplist(header_parameter(Request), HdrParams),
            http_parameters([method(get)|Request], RequestParams),
            request_body(Content, Request),
            server_handler_options(AsOption, OptionParam)
          ), IE, input_error(IE, RequestParams)),
    call(Module:Handler),
    OE = error(_,_),
    catch(openapi_reply(Module, Responses), OE,
          output_error(OE)).

%!  input_error(+Error, +RequestParams).
%!  output_error(+Error).
%
%   Handle errors while converting  the   input  and  output parameters.
%   Currently maps error context from   http_parameters/2 to rest(Param,
%   query, Type) context.

input_error(error(Formal, Context), RequestParams) :-
    subsumes_term(context(_, http_parameter(_)), Context),
    Context = context(_, http_parameter(Param)),
    debug(rest(error), 'Error in ~p; request = ~p', [Param, RequestParams]),
    member(ReqParam, RequestParams),
    ReqParam =.. [Param, _Value, Options],
    http_param_type(Options, Type),
    !,
    throw(error(Formal, rest(Param, request, Type))).
input_error(E, _RequestParams) :- throw(E).

http_param_type(Options, Type) :-
    memberchk(openapi(Type), Options),
    !.
http_param_type(Options, array(Type, _)) :-
    memberchk(list(openapi(Type)), Options),
    !.

output_error(E) :- throw(E).

:- meta_predicate
    add_error_context(0, +).

add_error_context(Goal, C) :-
    catch(Goal, error(Formal, _), throw(error(Formal, C))).

%!  segment_parameter(?Segment)
%
%   Fill a segment parameter

segment_parameter(segment(Type, Segment, Value, Name, _Description)) :-
    add_error_context(
        segment_value(Type, Segment, Value),
        rest(Name, path, Type)).

server_handler_options([], []).
server_handler_options([H|T], Options) :-
    arg(1, H, Value),
    (   var(Value)
    ->  server_handler_options(T, Options)
    ;   functor(H, Name, _),
        Opt =.. [Name,Value],
        Options = [Opt|OptT],
        server_handler_options(T, OptT)
    ).

%!  header_parameter(+Request, +HdrParam)
%
%   Extract a parameter through the header.
%   @tbd Deal with name normalization?  Deal with optional and
%   missing required values.

header_parameter(Request, HdrParam) :-
    HdrParam =.. [Name, Arg, _Opts],
    Header =.. [Name,Arg],
    (   memberchk(Header, Request)
    ->  true
    ;   print_message(warning, error(rest_error(missing_header(Name)), _))
    ).

%!  request_body(+ContentSpec, +Request) is det.
%
%   Read the specified request body.

request_body(-, _).
request_body(content(media(application/json,_), -, Body, _Descr), Request) :-
    !,
    add_error_context(
        http_read_json_dict(Request, Body),
        rest(body, request_body, json)).
request_body(content(media(application/json,_), Type, Body, _Descr), Request) :-
    add_error_context(
        http_read_json_dict(Request, Body0),
        rest(body, request_body, json)),
    add_error_context(
        json_check(Type, Body0, Body),
        rest(body, request_body, Type)).

%!  openapi_reply(+Module, +Responses) is det.
%
%   Formulate the HTTP request from a term.  The user handler binds the
%   response parameter to one of:
%
%     - status(Code)
%     Reply using an HTTP header with status Code and no body.
%     - status(Code, Data)
%     Use Code as HTTP status code and generate the body from Data.
%     Currently this only supports responses of the type
%     `application/json` and Data must be suitable for
%     json_write_dict/3.
%
%   @arg Responses is a  list   response(Code,  MediaType,  Type, Reply,
%   Description), where `Reply` is the  variable   that  is bound by the
%   user supplied handler.

:- det(openapi_reply/2).
openapi_reply(Module, Responses) :-
    Responses = [R0|_],
    arg(5, R0, Reply),
    reply_status(Reply, Code, Data),
    memberchk(response(Code, _As, MediaType, Type, _, _Descr), Responses),
    openapi_reply(Code, MediaType, Type, Data, Module).

reply_status(Var, _, _) :-
    var(Var), !,
    instantiation_error(Var).
reply_status(status(Code, Data), Code, Data) :- !.
reply_status(status(Code), Code, '') :- !.
reply_status(Data, 200, Data).

%!  openapi_reply(+HTTPCode, +MediaType, +Type, +Data, +Module) is det.

:- det(openapi_reply/5).
openapi_reply(Code, _, _, '', _) :-
    !,
    format('Status: ~d~n~n', [Code]).
openapi_reply(Code, media(application/json,_), -, Data, Module) :-
    !,
    Module:openapi_server_config(reply_json_options(Options)),
    reply_json_dict(Data, [status(Code)|Options]).
openapi_reply(Code, media(application/json,_), Type, Data, Module) :-
    !,
    (   Module:openapi_server_config(type_check_response(true))
    ->  json_check(Type, Out, Data)
    ;   Out = Data
    ),
    Module:openapi_server_config(reply_json_options(Options)),
    reply_json_dict(Out, [status(Code)|Options]).

%!  openapi_error(+Module, +Error, +Responses) is det.
%
%   An error happened while converting the  input arguments, running the
%   implementation or converting the output arguments.
%
%   @arg Module is the (server) module
%   @arg Error is the exception or the atom `failed` if the body
%        execution failed.
%   @arg Responses are the declared valid responses.

openapi_error(Module, Error, Responses) :-
    map_error(Module, Error, Responses, Reply),
    Responses = [R0|_],
    arg(5, R0, Reply),
    openapi_reply(Module, Responses),
    !.
openapi_error(_Module, Error, _Responses) :-
    throw(Error).

map_error(Module, Error, Responses, Reply) :-
    call(Module:openapi_error_hook(Error, Responses, Reply)),
    !.
map_error(_Module, Error, _Responses, Reply) :-
    Error = error(_, Context),
    nonvar(Context),
    http_error_status(Context, Error, Status),
    message_to_string(Error, Message),
    Reply = status(Status, _{code:Status, message:Message}).

http_error_status(rest(_,_,_), _, 400).

%!  openapi_error_hook(+Error, +Responses, -Reply) is semidet.
%
%   Hook called in the server module if   an error was encountered while
%   processing  the  REST  request.  If  the   error  was  thrown  while
%   extracting and converting the request   parameters, the _context_ of
%   the exception (2nd argument of the   error/2 term) has the following
%   shape:
%
%     - rest(Parameter, Location, Type)
%     Where Parameter is the parameter name or `body`, Location is
%     `path`, `query` or `request_body`, and Type is the translated
%     JSON schema type if the parameter.  The generated error is
%     typically a type_error, domain_error or syntax_error.
%
%   @arg Responses contains a description of the valid response types
%   and codes.
%   @arg Reply is typically bound to a term status(Code, Object), where
%   `Object` is a dict describing the error.


		 /*******************************
		 *            TYPES		*
		 *******************************/

%!  api_type(?Type, ?Format, ?TypeID) is semidet.
%
%

api_type(Type, Format, TypeID) :-
    api_type(_Name, Type, Format, TypeID), !.
api_type(string, Format, string) :-
    !,
    print_message(warning, openapi(unknown_string_format, Format)).
api_type(Type, Format, _TypeID) :-
    print_message(error, openapi(unknown_type, Type, Format)),
    fail.


%!  api_type(?Name, ?Type, ?Format, ?TypeID)
%
%   The formats defined by the OAS are:

api_type(integer,  integer,    int32,       int32).
api_type(long,     integer,    int64,       int64).
api_type(long,     integer,    -,           integer).
api_type(float,    number,     float,       float).
api_type(double,   number,     double,      float).
api_type(double,   number,     -,           float).
api_type(string,   string,     -,           string).
api_type(byte,     string,     byte,        base64).
api_type(binary,   string,     binary,      binary).
api_type(boolean,  boolean,    -,           boolean).
api_type(date,     string,     date,        date).
api_type(dateTime, string,     'date-time', date_time).
api_type(password, string,     password,    password).
api_type(string,   string,     string,      string). % Not in OAS
api_type(uri,      string,     uri,         uri).    % Not in OAS
api_type(uuid,     string,     uuid,        uuid).   % Not in OAS

%!  oas_type(+Type, ?In, ?Out) is det.
%
%   @arg Out is the _Prolog_ view
%   @arg In is the JSON dict view.

oas_type(int32, In, Out) :-
    cvt_integer(In, Out),
    must_be(between(-2147483648, 2147483647), Out).
oas_type(int64, In, Out) :-
    cvt_integer(In, Out),
    must_be(between(-9223372036854775808, 9223372036854775807), Out).
oas_type(integer, In, Out) :-
    cvt_integer(In, Out).
oas_type(number, In, Out) :-
    cvt_number(In, Out).
oas_type(float, In, Out) :-
    (   nonvar(In)
    ->  cvt_number(In, Out0),
        Out is float(Out0)
    ;   cvt_number(In0, Out),
        In is float(In0)
    ).
oas_type(string, In, Out) :-
    (   var(In)
    ->  to_string(Out, In)
    ;   to_atom(In, Out)
    ).
oas_type(uri, In, Out) :-
    (   var(In)
    ->  to_atom(Out, In)
    ;   to_atom(In, Out)
    ).
oas_type(uuid, In, Out) :-
    (   var(In)
    ->  to_atom(Out, In)
    ;   to_atom(In, Out)
    ).
oas_type(binary, In, Out) :-
    (   var(In)
    ->  to_string(Out, In)
    ;   to_string(In, Out)
    ).
oas_type(base64, In, Out) :-
    base64(In, Out).
oas_type(boolean, In, Out) :-
    (   var(In)
    ->  to_boolean(Out, In)
    ;   to_boolean(In, Out)
    ).
oas_type(date, In, Out) :-
    cvt_date_time(date, In, Out).
oas_type(date_time, In, Out) :-
    cvt_date_time(date_time, In, Out).
oas_type(password, In, Out) :-
    (   var(In)
    ->  to_string(Out, In)
    ;   to_string(In, Out)
    ).

%!  cvt_date_time(+Format, ?In, ?Out) is det.
%
%   Convert between wire (xsd) dateTime and   Prolog. As Prolog input we
%   accept a term (date(Y,M,D)  or   date_time(Y,M,D,H,Mn,S,0)),  a time
%   stamp or an xsd dateTime string.

cvt_date_time(Format, In, Out) :-
    (   var(In)
    ->  (   (   atom(Out)
            ->  to_string(Out, In)
            ;   string(Out)
            ->  In = Out
            )
        ->  valid_date_time(Format, In, _)
        ;   compound(Out)
        ->  valid_date_time(Format, In, Out)
        ;   number(Out)
        ->  stamp_date_time(Out, date(Y,M,D,H,Mn,S,0,_Tz,_Dst), 'UTC'),
            (   Format = date_time
            ->  valid_date_time(Format, In, date_time(Y,M,D,H,Mn,S,0))
            ;   valid_date_time(Format, In, date(Y,M,D))
            )
        )
    ;   valid_date_time(Format, In, Out) % creating a date/6 struct
    ).

valid_date_time(date, String, Date) :-
    xsd_time_string(Date,  % date(Y,M,D)
                    'http://www.w3.org/2001/XMLSchema#date',
                    String).
valid_date_time(date_time, String, DateTime) :-
    xsd_time_string(DateTime,  % date_time(Y,M,D,H,Mi,S[,TZ])
                    'http://www.w3.org/2001/XMLSchema#dateTime',
                    String).

cvt_integer(In, Out) :-
    cvt_number(In, Out),
    must_be(integer, Out).

cvt_number(In, Out) :- nonvar(In), !, to_number(In, Out).
cvt_number(N, N)    :- must_be(number, N).

to_number(In, Out) :-
    (   number(In)
    ->  Out = In
    ;   atom_number(In, Out0)
    ->  Out = Out0
    ;   type_error(number, In)
    ).

to_string(Val, String) :-
    atom_string(Val, String).

to_atom(Val, Atom) :-
    atom_string(Atom, Val).

to_boolean(Var, _) :-
    var(Var),
    !,
    instantiation_error(Var).
to_boolean(false,   false).
to_boolean(true,    true).
to_boolean('FALSE', false).
to_boolean('TRUE',  true).
to_boolean(0,       false).
to_boolean(1,       true).
to_boolean(no,      false).
to_boolean(yes,     true).
to_boolean('NO',    false).
to_boolean('YES',   true).
to_boolean(off,     false).
to_boolean(on,      true).
to_boolean('OFF',   false).
to_boolean('ON',    true).

%!  json_check(+Spec, ?JSONIn, ?JSONOut) is det.
%
%   Validate a JSON object.
%
%   @error type_error(Expected, Value)
%   @error existence_error(json_schema, URL)

json_check(url(URL), In, Out) :-
    !,
    (   json_schema(URL, Type)
    ->  json_check(Type, In, Out)
    ;   existence_error(json_schema, URL)
    ).
json_check(object, In, Out) :-
    !,
    In = Out,
    (   is_json_object(In)
    ->  true
    ;   type_error(object, In)
    ).
json_check(object(Properties), In, Out) :-
    !,
    (   nonvar(In)
    ->  json_object_pairs(In, InPairs),
        obj_properties_in(InPairs, Properties, OutPairs),
        dict_pairs(Out, _, OutPairs)
    ;   json_object_pairs(Out, OutPairs),
        obj_properties_out(OutPairs, Properties, InPairs),
        dict_pairs(In, _, InPairs)
    ).
json_check(array(Type, Opts), In, Out) :-
    !,
    (   is_list(In)
    ->  check_array_length(In, Opts),
        maplist(json_check(Type), In, Out)
    ;   is_list(Out)
    ->  check_array_length(Out, Opts),
        maplist(json_check(Type), In, Out)
    ;   must_be(list, In, Out)
    ),
    check_array_unique(In, Opts).
json_check(oneOf(Types), In, Out) :-
    !,
    Error = error(_,_),
    (   nonvar(In)
    ->  candidate_types(Types, In, Candidates, Best),
        (   Candidates = []                        % no candidate, best error
        ->  json_check(Best, In, Out)
        ;   Candidates = [Type]                    % one candidate, check
        ->  json_check(Type, In, Out)
        ;   append(_, [Type|Rest], Types),         % find type and verify no 2nd
            catch(json_check(Type, In, Out), Error, fail)
        ->  (   member(T2, Rest),
                catch(json_check(T2, In, _), Error, fail)
            ->  type_error(oneOf(Types), In)
            ;   true
            )
        ;   type_error(oneOf(Types), In)
        )
    ;   candidate_types(Types, Out, Candidates, Best),
        (   Candidates = []
        ->  json_check(Best, In, Out)
        ;   Candidates = [Type]
        ->  json_check(Type, In, Out)
        ;   append(_, [Type|Rest], Candidates),
            catch(json_check(Type, In, Out), Error, fail)
        ->  (   member(T2, Rest),
                catch(json_check(T2, _, Out), Error, fail)
            ->  type_error(oneOf(Types), Out)
            ;   true
            )
        ;   type_error(oneOf(Types), Out)
        )
    ).
json_check(allOf(Types), In, Out) :-
    !,
    (   nonvar(In)
    ->  maplist(json_check_in_out_type(In), Outs, Types),
        join_dicts(Outs, Out)
    ;   maplist(json_check_out_in_type(Out), Ins, Types),
        join_dicts(Ins, In)
    ).
json_check(anyOf(Types), In, Out) :-
    !,
    (   member(Type, Types),
        catch(json_check(Type, In, Out), _, fail)
    ->  true
    ;   nonvar(In)
    ->  type_error(oneOf(Types), In)
    ;   type_error(oneOf(Types), Out)
    ).
json_check(not(Type), In, Out) :-
    !,
    (   \+ catch(json_check(Type, In, Out), _, fail)
    ->  In = Out
    ;   (   nonvar(In)
        ->  type_error(not(Type), In)
        ;   type_error(not(Type), Out)
        )
    ).
json_check(enum(Values, CaseSensitive, Case), In, Out) :-
    Enum = enum(Values, CaseSensitive, Case),
    !,
    (   var(In)                                    % Out -> In
    ->  enum_find_ex(Out, Enum, Value),
        to_string(Value, In)
    ;   enum_find_ex(In, Enum, Value),
        enum_case(Case, Value, Out)
    ).
json_check(numeric(Type, Domain), In, Out) :-
    !,
    oas_type(Type, In, Out),
    (   number_in_domain(Domain, Out)
    ->  true
    ;   domain_error(Domain, Out)
    ).
json_check(any, In, Out) :-
    !,
    In = Out.
json_check(string(Restrictions), In, Out) :-
    !,
    oas_type(string, In, Out),
    maplist(check_string_restriction(In), Restrictions).
json_check(Type, In, Out) :-
    oas_type(Type, In, Out).

json_check_in_out_type(In, Out, Type) :- json_check(Type, In, Out).
json_check_out_in_type(Out, In, Type) :- json_check(Type, In, Out).

%!  candidate_types(+Types, +Data, -Candidates:list(type), -Best:type)
%
%   True when Candidates is a list of types   that may match and Best is
%   the closest matching candidate.

:- det(candidate_types/4).
candidate_types(Types, Data, Candidates, Best) :-
    maplist(candidate_type(Data), Types, Scores),
    pairs_keys_values(Best0, Types, Scores),
    sort(2, @>=, Best0, Best1),
    Best1 = [Best-_|_],
    convlist(is_candidate, Best1, Candidates).

is_candidate(Type-(_-0), Type).

%!  candidate_type(+Data, +Type, -Score:pair(Match,Mismatch)) is det.
%
%   Check whether Data may  satisfy  Type.   If  Mismatch  is  zero, all
%   required values are present and all enums are satisfied. Mismatch is
%   incremented on each missing required property or missing enum value.

candidate_type(Data, Type, Match-Mismatch) :-
    State = state(0,0),
    candidate_type_(Type, Data, State),
    State = state(Match, Mismatch).

candidate_type_(object(Props), Data, State) :-
    !,
    (   is_dict(Data)
    ->  (   member(p(Name, Type, Opts), Props),
            (   Field = Data.get(Name)
            ->  incr_match(State),
                candidate_type_(Type, Field, State)
            ;   memberchk(required, Opts)
            ->  incr_mismatch(State)
            ;   true
            ),
            fail
        ;   true
        )
    ;   incr_mismatch(State)
    ).
candidate_type_(Type, Data, State) :-
    Type = enum(_, _, _),
    !,
    (   (   atom(Data)
        ->  true
        ;   string(Data)
        ),
        (   enum_find(Data, Type, _Value)
        ->  incr_match(State)
        ;   incr_mismatch(State)
        )
    ;   incr_mismatch(State)
    ).
candidate_type_(_, _, _).

incr_match(State) :-
    arg(1, State, M0),
    M1 is M0+1,
    nb_setarg(1, State, M1).
incr_mismatch(State) :-
    arg(2, State, M0),
    M1 is M0-1,
    nb_setarg(2, State, M1).

%!  number_in_domain(+Domain, +Value) is semidet.
%
%   True if Value satisfies Domain.
%   @see type_restrictions/4 creates Domain

number_in_domain(Domain, Value) :-
    number_in_domain_(Domain, Value),
    arg(3, Domain, MultipleOf),
    (   MultipleOf == (-)
    ->  true
    ;   Times is Value/MultipleOf,
        round(Times) =:= Times
    ).

number_in_domain_(domain(between(Min, Max), ExclMin-ExclMax, _), Value) =>
    satisfies_min(Min, Value, ExclMin),
    satisfies_max(Max, Value, ExclMax).
number_in_domain_(domain(max(Max), ExclMax, _), Value) =>
    satisfies_max(Max, Value, ExclMax).
number_in_domain_(domain(min(Min), ExclMin, _), Value) =>
    satisfies_min(Min, Value, ExclMin).

satisfies_max(Max, Value, false) =>
    Value =< Max.
satisfies_max(Max, Value, true) =>
    Value < Max.

satisfies_min(Min, Value, false) =>
    Value >= Min.
satisfies_min(Min, Value, true) =>
    Value > Min.

%!  enum_find(+From, +EnumSpec, -Value:atom) is semidet.
%!  enum_find_ex(+From, +EnumSpec, -Value:atom) is det.
%
%   Find the intended enum value from  the   atom  or string From. Deals
%   with whether or not the enum is specified as case sensitive.
%
%   @error domain_error(oneof(Values))

enum_find(From, enum(Values, CaseSensitive, _Case), Value) :-
    to_atom(From, V0),
    (   memberchk(V0, Values)
    ->  Value = V0
    ;   CaseSensitive == false,
        downcase_atom(V0, VL),
        member(V1, Values),
        downcase_atom(V1, VL)
    ->  Value = V1
    ).

enum_find_ex(From, Enum, Value) :-
    (   enum_find(From, Enum, Value)
    ->  true
    ;   arg(1, Enum, Values),
        domain_error(oneof(Values), From)
    ).

enum_case(preserve, Out0, Out) => Out = Out0.
enum_case(lower,    Out0, Out) => downcase_atom(Out0, Out).
enum_case(upper,    Out0, Out) => upcase_atom(Out0, Out).

check_string_restriction(String, min_length(MinLen)) =>
    string_length(String, Len),
    (   Len >= MinLen
    ->  true
    ;   domain_error(string(minLength>=MinLen), String)
    ).
check_string_restriction(String, max_length(MaxLen)) =>
    string_length(String, Len),
    (   Len =< MaxLen
    ->  true
    ;   domain_error(string(maxLength=<MaxLen), String)
    ).
check_string_restriction(String, pattern(Pattern)) =>
    re_match(Pattern, String, []).

%!  is_json_object(@Term) is semidet.
%
%   True when Term can be used as a JSON object mapping.

is_json_object(Dict) :-
    is_dict(Dict, _), !.
is_json_object(json(Attrs)) :-
    is_list(Attrs),
    maplist(name_value, Attrs).

name_value(Name = _Value) :- atomic(Name).
name_value(Term) :- compound(Term), compound_name_arity(Term, _, 1).

json_object_pairs(Dict, Pairs) :-
    is_dict(Dict, _),
    !,
    dict_pairs(Dict, _, Pairs).
json_object_pairs(json(List), Pairs) :-
    is_list(List),
    maplist(name_value, List, Keys, Values),
    !,
    pairs_keys_values(Pairs0, Keys, Values),
    keysort(Pairs0, Pairs).
json_object_pairs(Obj, _) :-
    type_error(json_object, Obj).

name_value(Name - Value, Name, Value) :- !.
name_value(Name = Value, Name, Value) :- !.
name_value(Term, Name, Value) :- Term =.. [Name,Value].

%!  obj_properties_in(+InPairs, +Spec, -OutPairs) is det.
%
%   Type check the Name-Value pairs of an object against Spec. Spec is a
%   list of p(Name,Type,Opts). Input that does  not appear in the schema
%   is removed. If a Value is `null`   and the property is not required,
%   this is accepted. Should we delete the property instead?

obj_properties_in([], Spec, []) :-
    !,
    check_missing(Spec).
obj_properties_in(List, [], List) :-
    !.
obj_properties_in([NV|T0], PL, [NV|T]) :-
    PL = [p(P,_,_)|_],
    NV = N-_,
    N @< P,
    !,
    obj_properties_in(T0, PL, T).
obj_properties_in([N-V0|T0], [p(N,Type,Opts)|PT], [N-V|T]) :-
    !,
    (   V0 == null,
        (   memberchk(nullable, Opts)
        ;   \+ memberchk(required, Opts)
        )
    ->  V = V0
    ;   json_check(Type, V0, V)
    ),
    obj_properties_in(T0, PT, T).
obj_properties_in(T0, [p(N,_Type,Opts)|PT], T) :-
    (   memberchk(required, Opts)
    ->  existence_error(json_property, N)
    ;   obj_properties_in(T0, PT, T)
    ).

check_missing([]).
check_missing([p(N,_Type,Opts)|T]) :-
    (   memberchk(required, Opts)
    ->  existence_error(json_property, N)
    ;   check_missing(T)
    ).

%!  obj_properties_out(+OutPairs, +Spec, -InPairs)

obj_properties_out([], Spec, []) :-
    !,
    check_missing(Spec).
obj_properties_out(List, [], List) :-
    !.
obj_properties_out([NV|T0], PL, [NV|T]) :-
    PL = [p(P,_,_)|_],
    NV = N-_,
    N @< P,
    !,
    obj_properties_out(T0, PL, T).
obj_properties_out([N-V0|T0], [p(N,Type,_Opts)|PT], [N-V|T]) :-
    !,
    json_check(Type, V, V0),
    obj_properties_out(T0, PT, T).
obj_properties_out(T0, [p(N,_Type,Opts)|PT], T) :-
    (   memberchk(required, Opts)
    ->  existence_error(json_property, N)
    ;   obj_properties_out(T0, PT, T)
    ).

%!  join_dicts(+Dicts, -Dict) is det.
%
%   Create a dict from a list of   dicts, containing the joined keys. If
%   there are key duplicates, the last remains.

join_dicts([One], One) :- !.
join_dicts([H1,H2|T], Dict) :-
    H = H1.put(H2),
    join_dicts([H|T], Dict).

%!  must_be(+Type, ?In, ?Out) is det.
%
%   Support bi-directional type check for json_check/3.

must_be(Type, In, Out) :-
    (   nonvar(In)
    ->  must_be(Type, In)
    ;   must_be(Type, Out)
    ).

:- multifile
    http:convert_parameter/3.

http:convert_parameter(openapi(Type), In, Out) :-
    json_check(Type, In, Out).

%!  json_schema(?URL, ?Spec)
%
%   Spec is one of
%
%     - array(ItemType, Options)
%     - object(Properties)
%       Properties is an ordered list of
%       - p(Name, Type, Properties)
%         where Properties is a list of required(Bool), nullable(Bool)
%     - A type as defined by oas_type/3.
%     - url(URL)
%       Reference to another type.

:- multifile
    json_schema/2.

%!  json_schema_clauses(+JSONTerm, +Options)//

json_schema_clauses(JSONTerm, Options) -->
    { Schemas = JSONTerm.get(components).get(schemas),
      dict_pairs(Schemas, _, SchemaPairs)
    },
    !,
    schema_clauses(SchemaPairs, Options).
json_schema_clauses(_, _) --> [].


%!  schema_clauses(+Specs, +Options)//
%
%   Compile the OpenAPI schema definitions into json_schema/2 clauses.

schema_clauses([], _) --> [].
schema_clauses([H|T], Options) -->
    schema_clause(H, Options),
    schema_clauses(T, Options).

schema_clause(Schema-Spec, Options) -->
    { json_type(Spec, Type, Options),
      option(base_uri(Base), Options),
      file_directory_name(Base, Dir),
      atomic_list_concat([Dir, '#/components/schemas/', Schema], URL)
    },
    [ openapi:json_schema(URL, Type) ].

%!  json_type(+Spec, -Type, -TypeOpts, +Options) is det.
%
%   True when Type  is  the  type   representation  for  the  JSON  type
%   description Spec.
%
%   @arg Spec is an OpenAPI type specification as JSON or YAML term.
%   @arg Type is a term that is handled by json_check/3.
%   @arg TypeOpts is a list that may hold `nullable` or `required`.

json_type(Spec, Type, TypeOpts, Options) :-
    _{'$ref':URLS} :< Spec,
    !,
    option(base_uri(Base), Options),
    uri_normalized(URLS, Base, URL),
    (   url_yaml(URL, Spec2)
    ->  atom_string(NewBase, URL),
        json_type(Spec2, Type, TypeOpts, [base_uri(NewBase)|Options])
    ;   Type = url(URL),
        TypeOpts = []
    ).
json_type(Spec, Type, TypeOpts, Options) :-
    json_type(Spec, Type, Options),
    (   Spec.get(nullable) == true
    ->  TypeOpts = [nullable]
    ;   TypeOpts = []
    ).

%!  json_type(+Spec, -Type, +Options) is det.

json_type(Spec, Type, _) :-
    _{type:TypeS, format:FormatS} :< Spec,
    !,
    atom_string(Type0, TypeS),
    atom_string(Format, FormatS),
    api_type(Type0, Format, Type1),
    type_restrictions(Spec, Type0, Type1, Type).
json_type(Spec, object(Props), Options) :-
    _{properties:PropSpecs} :< Spec,
    !,
    dict_pairs(PropSpecs, _, Pairs),
    (   maplist(atom_string, Req, Spec.get(required))
    ->  true
    ;   Req = []
    ),
    maplist(schema_property(Req, Options), Pairs, Props0),
    sort(Props0, Props).
json_type(Spec, array(Type, Opts), Options) :-
    _{type:"array", items:IType} :< Spec,
    !,
    array_restrictions(Spec, Opts),
    json_type(IType, Type, Options).
json_type(Spec, oneOf(Types), Options) :-
    _{oneOf:List} :< Spec,
    !,
    maplist(opts_json_type(Options), List, Types).
json_type(Spec, allOf(Types), Options) :-
    _{allOf:List} :< Spec,
    !,
    maplist(opts_json_type(Options), List, Types).
json_type(Spec, anyOf(Types), Options) :-
    _{anyOf:List} :< Spec,
    !,
    maplist(opts_json_type(Options), List, Types).
json_type(Spec, not(Type), Options) :-
    _{not:NSpec} :< Spec,
    !,
    json_type(NSpec, Type, Options).
json_type(Spec, object, _Options) :-
    _{type:"object"} :< Spec,
    !.
json_type(Spec, enum(Values, CaseSensitive, Case), Options) :-
    _{type:"string", enum:ValuesS} :< Spec,
    !,
    option(enum_case_sensitive(CaseSensitive), Options, true),
    option(enum_case(Case), Options, preserve),
    maplist(atom_string, Values, ValuesS).
json_type(Spec, Type, _) :-
    _{type:TypeS} :< Spec,
    !,
    atom_string(Type0, TypeS),
    api_type(Type0, -, Type1),
    type_restrictions(Spec, Type0, Type1, Type).
json_type(Spec, Type, Options) :-
    _{'$ref':URLS} :< Spec,
    !,
    option(base_uri(Base), Options),
    uri_normalized(URLS, Base, URL),
    (   url_yaml(URL, Spec2)
    ->  atom_string(NewBase, URL),
        json_type(Spec2, Type, [base_uri(NewBase)|Options])
    ;   Type = url(URL)
    ).
json_type(_{properties:_{}}, Type, _Options) :-
    !,
    Type = (-).
json_type(_Spec, _Type, _Options) :-
    start_debugger_fail.

opts_json_type(Options, Spec, Type) :-
    json_type(Spec, Type, Options).

schema_property(Reqs, Options, Name-Spec, p(Name, Type, TypeOpts)) :-
    (   memberchk(Name, Reqs)
    ->  TypeOpts = [ required | TypeOpts1 ]
    ;   TypeOpts = TypeOpts1
    ),
    json_type(Spec, Type, TypeOpts1, Options).

%!  type_restrictions(+Spec, +Type0, +ApiType, -Type)

type_restrictions(Spec, Type0, Type1, Type) :-
    numeric_type(Type0),
    !,
    (   _{minimum:Min, maximum:Max} :< Spec
    ->  Type = numeric(Type1, domain(between(Min,Max), ExclMin-ExclMax, MultipleOf))
    ;   _{minimum:Min} :< Spec
    ->  Type = numeric(Type1, domain(min(Min), ExclMin, MultipleOf))
    ;   _{maximum:Max} :< Spec
    ->  Type = numeric(Type1, domain(max(Max), ExclMax, MultipleOf))
    ;   Type = Type1
    ),
    (   _{ exclusiveMinimum: ExclMin} :< Spec
    ->  true
    ;   ExclMin = false
    ),
    (   _{ exclusiveMaximum: ExclMax} :< Spec
    ->  true
    ;   ExclMax = false
    ),
    (   _{ multipleOf: MultipleOf} :< Spec
    ->  true
    ;   MultipleOf = (-)
    ).
type_restrictions(Spec, string, string, Type) :-
    setof(Restrict, string_restriction(Spec, Restrict), Restrictions),
    !,
    Type = string(Restrictions).
type_restrictions(_, _Type0, Type, Type).

numeric_type(integer).
numeric_type(number).

string_restriction(Spec, max_length(Len)) :-
    Len = Spec.get(maxLength).
string_restriction(Spec, min_length(Len)) :-
    Len = Spec.get(minLength).
string_restriction(Spec, pattern(Regex)) :-
    atom_string(Regex, Spec.get(pattern)).

array_restrictions(Spec, Options) :-
    findall(Opt, array_restriction(Spec, Opt), Options).

array_restriction(Spec, min_items(Min)) :-
    Min = Spec.get(minItems).
array_restriction(Spec, max_items(Max)) :-
    Max = Spec.get(minItems).
array_restriction(Spec, unique_items(true)) :-
    true == Spec.get(uniqueItems).

check_array_length(List, Opts) :-
    memberchk(max_items(Max), Opts),
    !,
    (   memberchk(min_items(Min), Opts)
    ->  true
    ;   Min = 0
    ),
    length(List, Len),
    (   between(Min, Max, Len)
    ->  true
    ;   domain_error(array_length(Min,Max), List)
    ).
check_array_length(List, Opts) :-
    memberchk(min_items(Min), Opts),
    !,
    length(List, Len),
    (   Len >= Min
    ->  true
    ;   domain_error(array_length(Min,infinite), List)
    ).
check_array_length(_, _).

check_array_unique(List, Opts) :-
    memberchk(unique_items(true), Opts),
    !,
    (   length(List, Len),
        sort(List, Sorted),
        length(Sorted, Len)
    ->  true
    ;   domain_error(unique_array, List)
    ).
check_array_unique(_, _).

%!  url_yaml(+URL, -Yaml:json) is semidet.
%
%   Assuming URL points to  a  local   file  and  fragment  thereof that
%   specifies a type, Type is the JSON/YAML representation of this type.

url_yaml(URL, Yaml) :-
    uri_components(URL, Components),
    uri_data(scheme, Components, file),
    uri_data(path, Components, FileEnc),
    uri_data(fragment, Components, Fragment),
    uri_encoded(path, File, FileEnc),
    openapi_read(File, Yaml0),
    (   var(Fragment)
    ->  Yaml = Yaml0
    ;   atomic_list_concat(Segments, /, Fragment),
        yaml_subdoc(Segments, Yaml0, Yaml)
    ).


		 /*******************************
		 *        DOC GENERATION	*
		 *******************************/

%!  openapi_doc(+File, +Mode, +Options) is det.
%
%   Write documentation to the current  output.   Options  are passed to
%   openapi_server/2. In addition, the following options are processed:
%
%     - file(+File)
%     Dump output to File.
%
%   This predicate is used by the `swipl-openapi` script to generate the
%   commented client or server code.

openapi_doc(File, Mode, Options) :-
    must_be(oneof([client,server]), Mode),
    read_openapi_spec(File, Spec, Options, Options1),
    phrase(server_clauses(Spec, Options1), Clauses),
    setup_call_cleanup(
        doc_output(Stream, Close, Options),
        doc_gen(Stream, File, Clauses, [mode(Mode)|Options]),
        Close).

doc_output(Stream, close(Stream), Options) :-
    option(file(File), Options),
    !,
    open(File, write, Stream).
doc_output(current_output, true, _).

doc_gen(Stream, File, Clauses, Options) :-
    findall(OperationId-Data,
            doc_data(Clauses, OperationId, Data, Options), Pairs),
    file_header(Stream, File, [operations(Pairs)|Options]),
    forall(member(OperationId-Data, Pairs),
           (   phrase(openapi_doc(OperationId, Data, Options), S)
           ->  format(Stream, '~s', [S])
           ;   warning(openapi(doc_failed, OperationId), Options)
           )).

file_header(Stream, File, Options) :-
    option(mode(client), Options),
    !,
    client_module(Stream, File, Options),
    findall(Opt, client_option(Opt, Options), ClientOptions),
    format(Stream, ':- use_module(library(openapi)).~n', []),
    format(Stream, ':- use_module(library(option)).~n~n', []),
    format(Stream, ':- use_module(library(debug)).~n~n', []),
    portray_clause(Stream, (:- openapi_client(File, ClientOptions))),
    nl(Stream).
file_header(Stream, File, Options) :-
    option(mode(server), Options),
    !,
    findall(Opt, server_option(Opt, Options), ServerOptions),
    format(Stream, ':- use_module(library(openapi)).~n', []),
    format(Stream, ':- use_module(library(option)).~n', []),
    format(Stream, ':- use_module(library(debug)).~n', []),
    server_header(Stream, File, Options),
    format(Stream, '~n', []),
    portray_clause(Stream, (:- openapi_server(File, ServerOptions))),
    nl(Stream).
file_header(_, _, _).

%!  client_module(+Stream, +SpecFile, +Options)
%
%   Emit a module  header  for  the   generated  client  if  the  option
%   module(Module) is present. If `Module` is  `true`, derive the module
%   from the client filename or the SpecFile.

client_module(Stream, SpecFile, Options) :-
    module_name(Module, SpecFile, Options),
    option(operations(Ops), Options),
    !,
    format(Stream, ':- module(~q,~n~t[ ~12|', [Module]),
    exports(Ops, Stream),
    format(Stream, '~t~10|]).~n', []).
client_module(_, _, _).

module_name(Module, SpecFile, Options) :-
    option(module(M), Options),
    (   M == true
    ->  option(file(File), Options, SpecFile),
        file_base_name(File, Base),
        file_name_extension(Module, _, Base)
    ;   Module = M
    ).

exports([], _).
exports([OperationId-Data|T], Stream) :-
    (   T == []
    ->  Sep = ''
    ;   Sep = ','
    ),
    export(Stream, OperationId, Data.arguments, Sep),
    exports(T, Stream).

export(Stream, OperationId, Args, Sep) :-
    length(Args, Arity),
    phrase(mode_args(Args), Codes),
    format(Stream, '~t~12|~q~w~t~48|% ~s~n',
           [OperationId/Arity, Sep, Codes]).

%!  client_option(-ClientOption, +Options) is nondet.
%
%   Pass options for generating the client at runtime.

client_option(warn(false), _Options).
client_option(type_check_request(Mode), Options) :-
    option(type_check_request(Mode), Options).
client_option(Option, Options) :-
    common_option(Option, Options).

%!  server_option(-ServerOption, +Options) is nondet.
%
%   Pass options for generating the server at runtime.

server_option(type_check_response(Bool), Options) :-
    option(type_check_response(Bool), Options),
    must_be(boolean, Bool).
server_option(format_response(Bool), Options) :-
    option(format_response(Bool), Options),
    must_be(boolean, Bool).
server_option(Option, Options) :-
    common_option(Option, Options).

common_option(server_url(URL), Options) :-
    option(server_url(URL), Options).
common_option(enum_case_sensitive(Bool), Options) :-
    option(enum_case_sensitive(Bool), Options).
common_option(enum_case(Case), Options) :-
    option(enum_case(Case), Options),
    must_be(oneof([lower,upper,preserve]), Case).

%!  server_header(+Stream, +File, +Options) is det.
%
%   Emit the header for generating a server.

server_header(Stream, File, Options) :-
    (   option(httpd(true), Options)
    ;   option(ui(true), Options)
    ),
    !,
    format(Stream, ':- use_module(library(http/thread_httpd)).~n', []),
    (   option(ui(true), Options)
    ->  server_ui(Stream, File, Options)
    ;   option(httpd(true), Options)
    ->  server_restonly(Stream, Options)
    ;   true
    ).
server_header(_,_,_).

server_ui(Stream, File, _Options) :-
    format(Stream, ':- use_module(library(http/http_dispatch)).~n', []),
    format(Stream, ':- use_module(library(swagger_ui)).~n', []),
    format(Stream, '
:- http_handler(root(.),
                http_redirect(see_other, root(\'swagger_ui\')),
                []).
:- http_handler(root(\'swagger.yaml\'),
                http_reply_file(~q, []),
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

', [File]).

server_restonly(Stream, _Options) :-
    format(Stream, '
server(Port) :-
    http_server(openapi_dispatch,
                [ port(Port)
                ]).

', []).

		 /*******************************
		 *        INTROSPECTION		*
		 *******************************/

%!  openapi_arg(:PredicateName, ?Index, ?Name, ?Type) is nondet.
%
%   True when PredicateName's one-based Index-th argument is named
%   Name and has type Type.

% Server clauses
openapi_arg(M:OperationId, Index, Name, Type) :-
    Clause = openapi_handler(_Method, _PathList, _SegmentMatches,
                             _Request, _HdrParams, _AsOption, _OptionParam,
                             _Content, _Responses, _Security, Handler),
    clause(M:Clause, true),
    functor(Handler, OperationId, _),
    clause_data(Clause, module(M), OperationId, Data, []),
    once(append(Inputs, [p(response, _, _)], Data.arguments)),
    nth1(Index, Inputs, p(Name, Type, _Description)).
% client clauses
openapi_arg(M:OperationId, ArgI, Arg, Type) :-
    Clause = openapi_type(Head),
    clause(M:Clause, true),
    functor(Head, OperationId, _),
    arg(ArgI, Head, Arg:Type).

%!  openapi_response(:PredicateName, Responses:list) is nondet.
%
%   True when Responses is  a  list   of  terms  describing the possible
%   responses for PredicateName. Each Response  is a term response(Code,
%   As, MediaType, Type), where
%
%     - Code is the numeric HTTP status code or a variable for
%       `default`
%     - As describes how to handle the code.  Currently one of
%       `data` or `error`
%     - MediaType is the expected response type
%     - Type is the (JSON) schema describing a JSON result

openapi_response(M:OperationId, Responses) :-
    Clause = openapi_handler(_Method, _PathList, _SegmentMatches,
                             _Request, _HdrParams, _AsOption, _OptionParam,
                             _Content, Responses0, _Security, Handler),
    clause(M:Clause, true),
    functor(Handler, OperationId, _),
    maplist(public_response, Responses0, Responses).

public_response(response(Code, As, MediaType, Type, _Result, _Descr),
                response(Code, As, MediaType, Type)).



		 /*******************************
		 *   DOCUMENTATION GENERATION	*
		 *******************************/

:- meta_predicate
    prefix(+, //, -, ?, ?).

%!  openapi_doc(+OperationID, +Data, +Options)// is det.

openapi_doc(OperationId, Data, Options) -->
    doc_mode(OperationId, Data.arguments),
    "\n%\n",
    doc_description(Data.doc),
    doc_security(Data.security),
    doc_args(Data.arguments),
    doc_path(Data.doc),
    "\n",
    server_skeleton(OperationId, Data.arguments, Options),
    "\n\n".

%!  server_skeleton(+OperationId, +Args:list, +Options)// is det.
%
%   Generate the skeleton clause for the operation.

:- det(server_skeleton//3).
server_skeleton(_OperationId, _Args, Options) -->
    { option(mode(client), Options) },
    !.
server_skeleton(OperationId, Args, Options) -->
    { option(mode(server), Options),
      maplist(server_arg_name, Args, ArgNames),
      Head =.. [OperationId|ArgNames],
      server_skeleton_clause(Head, Clause),
      (   string(Clause)
      ->  string_codes(Clause, Codes)
      ;   with_output_to(codes(Codes),
                         portray_clause(Clause))
      )
    },
    string(Codes).

%!  server_skeleton_clause(+Head, -Clause) is det.
%
%   Generate the server skeleton clause.  This predicate can be hooked
%   using user:openapi_server_clause/2 with the same signature.
%
%   @arg Head is a term OperationId(Arg, ...),   where  each arg has the
%   shape  '$VAR'(Name)  and  Name  is  the  CamelCase  version  of  the
%   parameter, a valid name for a Prolog variable.
%   @arg Clause is either a string, which is inserted into the output
%   untranslated or a term that is handed to portray_clause/1.

:- det(server_skeleton_clause/2).
:- multifile
    user:openapi_server_clause/2.
server_skeleton_clause(Head, Clause) :-
    user:openapi_server_clause(Head, Clause),
    !.
server_skeleton_clause(Head, Clause) :-
    functor(Head, _, Arity),
    arg(Arity, Head, Response),
    Clause = (Head :-
                  debug(openapi, "~p", [Head]),
                  Response = status(404)).


doc_mode(OperationId, Args) -->
    "%! ", quoted_atom(OperationId),
    "(", mode_args(Args), ") is det.".

mode_args([]) --> [].
mode_args([H|T]) -->
    mode_arg(H),
    (  {T==[]}
    -> []
    ;  ", ",
       mode_args(T)
    ).

mode_arg(p(Name, _Type, _Descr)) -->
    mode(Name), camel_case(Name).

mode(response) --> !, "-".
mode(_) --> "+".

server_arg_name(p(Param, _Type, _Descr), '$VAR'(ArgName)) :-
    camel_case(Param, ArgName).

quoted_atom(Atom, List, Tail) :-
    format(codes(List,Tail), '~q', [Atom]).

%!  camel_case(+Name)
%
%   Emit an identifier in CamelCase.

camel_case(Name) -->
    { camel_case(Name, Camel) },
    atom(Camel).

camel_case(Name, Camel) :-
    atom_codes(Name, Codes),
    phrase(camel(Codes), CamelCodes),
    atom_codes(Camel, CamelCodes).

camel([]) --> [].
camel([H|T]) -->
    { code_type(H, to_lower(U)) },
    [U],
    camel_skip(T).

camel_skip([]) --> [].
camel_skip([0'_|T]) --> !, camel(T).
camel_skip([0'-|T]) --> !, camel(T).
camel_skip([H|T]) --> !, [H], camel_skip(T).

%!  uncamel_case(+In:atom, -Out:atom)
%
%   Turn the commonly use CamelCase operationId   into a pleasant Prolog
%   identifier. This ensures the first character   is  lower case and lU
%   sequences are translated into l_l. lUU is changed into l_UU

uncamel_case(In, Out) :-
    atom_codes(In, Codes),
    phrase(uncamel(UnCamel), Codes),
    atom_codes(Out, UnCamel).

uncamel([H|T]) -->
    [U],
    { code_type(U, upper(H)) },
    !,
    uncamel_(T).
uncamel(List) -->
    uncamel_(List).

uncamel_([L,0'_,U1,U2|T]) -->
    [L,U1,U2],
    { code_type(L, lower),
      code_type(U1, upper),
      code_type(U2, upper)
    },
    !,
    uncamel_(T).
uncamel_([L,0'_,Lower|T]) -->
    [L,U],
    { code_type(L, lower),
      code_type(U, upper(Lower))
    },
    !,
    uncamel_(T).
uncamel_([0'_|T]) -->
    "-",
    !,
    uncamel_(T).
uncamel_([H|T]) -->
    [H],
    !,
    uncamel_(T).
uncamel_([]) -->
    [].

%!  doc_description(+Doc)//
%
%   Emit the summary and documentation

doc_description(Doc) -->
    { memberchk(summary(Summary), Doc),
      memberchk(description(Desc), Doc)
    }, !,
    multiline_comment(Summary),
    multiline_comment(Desc),
    "%\n".
doc_description(Doc) -->
    { memberchk(description(Desc), Doc)
    }, !,
    multiline_comment(Desc),
    "%\n".
doc_description(Doc) -->
    { memberchk(summary(Summary), Doc)
    }, !,
    multiline_comment(Summary),
    "%\n".
doc_description(_) -->  [].

multiline_comment(Text) -->
    { string_lines(Text, Lines)
    },
    lines(Lines, "%  ").

string_lines(String, Lines) :-
    split_string(String, "\n", "", Lines0),
    delete_empty_lines(Lines0, Lines1),
    reverse(Lines1, Lines2),
    delete_empty_lines(Lines2, Lines3),
    reverse(Lines3, Lines).

delete_empty_lines([Line|T0], T) :-
    empty_line(Line),
    !,
    delete_empty_lines(T0, T).
delete_empty_lines(T, T).

empty_line(Line) :-
    split_string(Line, " \t", " \t", [""]).

lines([], _) --> [].
lines([H|T], Prefix) --> atom(Prefix), atom(H), "\n", lines(T, Prefix).

doc_security([public]) -->
    !.
doc_security(List) -->
    "%  Authentication options:\n",
    doc_security_list(List),
    "%\n".

doc_security_list([]) -->
    [].
doc_security_list([H|T]) -->
    doc_security_option(H),
    doc_security_list(T).

doc_security_option(public) -->
    "%   - no authentication required\n".
doc_security_option(Term) -->
    { arg(2, Term, Name) },
    "%   - ", atom(Name), "\n".

doc_args([]) --> [].
doc_args([H|T]) --> doc_arg(H), doc_args(T).

doc_arg(p(Name, Type, Description)) -->
    indent(0),
    prefix(0, ("@arg ", camel_case(Name), " "), Indent),
    type(Type, Indent), "\n",
    arg_description(Description).

doc_path(Doc) -->
    { memberchk(path(Path), Doc) },
    !,
    "%\n%  @see Path = ", atom(Path), "\n".
doc_path(_) -->
    [].

arg_description(options(List)) -->
    !,
    arg_options(List).
arg_description(Description) -->
    { string_lines(Description, Lines) },
    lines(Lines, "%       ").

arg_options([]) --> [].
arg_options([H|T]) --> arg_option(H), arg_options(T).

arg_option(p(Name, Type, Description)) -->
    { string_lines(Description, Lines) },
    "%       - ", quoted_atom(Name), "(+", type(Type), ")", "\n",
    lines(Lines, "%         ").

%!  type(+Type)//

type(list(option)) --> !.
type(url(URL)) -->
    !,
    { file_base_name(URL, TypeName) },
    atom(TypeName).
type(Type) -->
    type(Type, 0).

type(array(Type, Opts), Indent) -->
    !,
    prefix(Indent, "array(", NewIndent),
    type(Type, NewIndent), ")",
    (   {Opts == []}
    ->  []
    ;   " [", sequence(array_attr, ",", Opts), "]"
    ).
type(string([pattern(Pattern)]), _Indent) -->
    !,
    "/", atom(Pattern), "/".
type(string(Attrs), _Indent) -->
    { select(pattern(Pattern), Attrs, Attrs1) },
    !,
    "/", atom(Pattern), "/ [", sequence(str_attr, ",", Attrs1), "]".
type(string(Attrs), _Indent) -->
    !,
    "string [", sequence(str_attr, ",", Attrs), "]".
type(enum(List,_,lower), _Indent) -->
    { maplist(downcase_atom, List, Lower) },
    sequence(atom, "|", Lower).
type(object(Properties), Indent) -->
    !,
    prefix(Indent, "{ ", NewIndent),
    sequence(obj_property(NewIndent), (",", nl(NewIndent)), Properties),
    nl(Indent), "}".
type(oneOf(List), Indent) -->
    !,
    prefix(Indent, "( ", NewIndent),
    sequence(itype(NewIndent), (nl(Indent),"| "), List),
    nl(Indent), ")".
type(Type, _Indent, List, Tail) :-
    format(codes(List, Tail), '~p', [Type]).

itype(Indent, Type) -->
    type(Type, Indent).

obj_property(Indent, p(Name, Type, Opts)) -->
    atom(Name), ": ",
    { atom_length(Name, NameL),
      NewIndent is Indent+NameL+2
    },
    type(Type, NewIndent),
    obj_property_attrs(Opts).

obj_property_attrs([]) -->
    !.
obj_property_attrs(Opts) -->
    " [", sequence(obj_property_attr, "",Opts), "]".

obj_property_attr(required) --> "R".
obj_property_attr(nullable) --> "N".

str_attr(min_length(Len)) --> format(">=~w", [Len]).
str_attr(max_length(Len)) --> format("=<~w", [Len]).

array_attr(min_items(Len)) --> format(">=~w", [Len]).
array_attr(max_items(Len)) --> format("=<~w", [Len]).
array_attr(unique_items(true)) --> "unique".

prefix(Indent, Prefix, NewIndent) -->
    here(Start),
    Prefix,
    here(End),
    { diff_len(Start, End, 0, PLen),
      NewIndent is Indent+PLen
    }.

diff_len(Here, End, Len, Len) :-
    Here == End,
    !.
diff_len([_|Here], End, Len0, Len) :-
    Len1 is Len0+1,
    diff_len(Here, End, Len1, Len).

here(List,List,List).

nl(Indent) -->
    "\n", indent(Indent).

indent(Indent) -->
    "%  ", spaces(Indent).

spaces(Indent) -->
    format('~t~*|', [Indent]).

format(Format, Args, List, Tail) :-
    format(codes(List, Tail), Format, Args).

%!  doc_data(:ServerClauses, -OperationID, -Data:dict, +Options) is nondet.
%
%   Get  a  dict  that  contains   all    information   to  produce  the
%   documentation.

doc_data(Clauses, OperationId, Data, Options) :-
    member(Clause, Clauses),
    clause_data(Clause, Clauses, OperationId, Data, Options).

clause_data(Clause, Clauses, OperationId, Data, Options) :-
    Clause = openapi_handler(_Method, _PathList, Segments,
                             Request, HdrParams, AsOption, OptionParam,
                             Content, Responses, Security, Handler),
    Data = #{arguments:Params, doc:Doc, security:Security},
    Handler =.. [OperationId|Args],
    (   (   Clauses = module(M)
        ->  M:openapi_doc(OperationId, Doc)
        ;   memberchk(openapi_doc(OperationId, Doc), Clauses)
        ),
        maplist(doc_param(from(Segments,
                               Request, HdrParams, AsOption, OptionParam,
                               Content, Responses), Options), Args, Params0),
        exclude(==(-), Params0, Params)
    ->  true
    ;   warning(openapi(doc_failed, OperationId), Options),
        fail
    ).

doc_param(from(Segments, Request, HdrParams, AsOption, OptionParam,
               Content, Responses), Options,
          Arg, Param) :-
    (   segment_param(Arg, Segments, Param)
    ;   request_param(Arg, Request, Param)
    ;   OptionParam == Arg,
        option_param(AsOption, Param)
    ;   content_param(Arg, Content, Param)
    ;   header_param(Arg, HdrParams, Param)
    ;   response_param(Arg, Responses, Param, Options)
    ;   start_debugger_fail
    ), !.

segment_param(Arg, Segments, p(Name, Type, Description)) :-
    member(segment(Type, _, Arg0, Name, Description), Segments),
    Arg == Arg0, !.

request_param(Arg, Requests, Param) :-
    member(R, Requests),
    arg(1, R, Arg0),
    Arg == Arg0, !,
    doc_request_param(R, Param).

param_json_type(Opts, Type) :-
    memberchk(openapi(Type), Opts),
    !.
param_json_type(Opts, Type) :-
    memberchk(list(openapi(Type0)), Opts),
    Type = array(Type0).

option_param(AsOption, p(options, list(option), options(Options))) :-
    phrase(doc_request_params(AsOption), Options).

doc_request_params([]) --> [].
doc_request_params([H|T]) -->
    { doc_request_param(H, Param) },
    [ Param ],
    doc_request_params(T).

doc_request_param(Request, p(Name,Type,Description)) :-
    Request =.. [Name,_Var,Options],
    (   param_json_type(Options, Type)
    ->  true
    ;   Type = string,
        warning(openapi(no_type, Name), [])
    ),
    (   memberchk(description(Description), Options)
    ->  true
    ;   Description = ""
    ).

content_param(Arg,
              content(_MediaType, Scheme, Arg0, Description),
              p(request_body, Scheme, Description)) :-
    Arg == Arg0, !.

header_param(Arg, HdrParams, Param) :-
    member(HdrParam, HdrParams),
    arg(1, HdrParam, Arg0),
    Arg == Arg0,
    !,
    doc_request_param(HdrParam, Param).

response_param(Arg, Responses, -, Options) :-
    is_reponse_arg(Arg, Responses),
    option(mode(client), Options),
    \+ response_has_data(Responses), !.
response_param(Arg, Responses, p(response, Scheme, Description), _Options) :-
    member(response(Code,_As,_MediaType, Scheme, Arg0, Description),
           Responses),
    Arg == Arg0,
    between(200, 399, Code), !.

is_reponse_arg(Arg, Responses) :-
    member(R, Responses),
    arg(5, R, Arg0),
    Arg == Arg0.


%!  error(+Term, +Options) is det.
%
%   Print an error message. If silent(true) is   an option, the error is
%   silently ignored.

error(_Term, Options) :-
    option(silent(true), Options),
    !.
error(Term, _Options) :-
    print_message(error, Term).

%!  warning(+Term, +Options) is det.
%
%   Print an warning message. If silent(true)  is an option, the warning
%   is silently ignored.

warning(_Term, Options) :-
    option(silent(true), Options),
    !.
warning(Term, _Options) :-
    print_message(warning, Term).

:- if(current_prolog_flag(gui, true)).
start_debugger :-
    current_prolog_flag(debug, true),
    !,
    gtrace.
:- endif.
start_debugger.

start_debugger_fail :-
    start_debugger,
    fail.


		 /*******************************
		 *        ENABLE EXPANSION	*
		 *******************************/

:- multifile
    system:term_expansion/2.

system:term_expansion((:- openapi_server(File, Options)), Clauses) :-
    \+ current_prolog_flag(xref, true),
    expand_openapi_server(File, Options, Clauses).
system:term_expansion((:- openapi_client(File, Options)), Clauses) :-
    \+ current_prolog_flag(xref, true),
    expand_openapi_client(File, Options, Clauses).


		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1,
    prolog:error_message//1,
    prolog:message_context//1.

prolog:message(openapi(path_failed, Path-_Spec)) -->
    [ 'OpenAPI: failed to generate clauses for path ~p'-[Path] ].
prolog:message(openapi(no_operation_id, Method, Path, PredicateName)) -->
    [ 'OpenAPI: no operationId for ~p ~p, using ~p'-
      [Method, Path, PredicateName] ].
prolog:message(openapi(doc_failed, OperationId)) -->
    [ 'OpenAPI: failed to generate documentation for operationId ~p'-
      [OperationId] ].
prolog:message(openapi(no_type, Param)) -->
    [ 'OpenAPI: no type for parameter ~p (assuming "string")'-[Param] ].
prolog:message(openapi(unknown_type, Type, -)) -->
    [ 'OpenAPI: unrecognized type `~p`'-[Type] ].
prolog:message(openapi(unknown_type, Type, Format)) -->
    [ 'OpenAPI: unrecognized type `~p` with format `~p`'-[Type, Format] ].
prolog:message(openapi(unknown_string_format, Format)) -->
    [ 'OpenAPI: Using plain "string" for string with format `~p`'-[Format] ].

prolog:error_message(rest_error(missing_header(Name))) -->
    [ 'REST error: missing header: ', ansi(code, '~p', [Name]) ].
prolog:error_message(rest_error(Code, Term)) -->
    [ 'REST error: code: ~p, data: ~p'-[Code, Term] ].
prolog:error_message(openapi_invalid_reply(Code, ExCodes, Error)) -->
    [ 'OpenAPI: request replied code ~p (expected one of ~p)'-[Code, ExCodes],
      nl,
      '  Document: ~p'-[Error]
    ].
prolog:message_context(rest(Name, Where, Type)) -->
    [ ' (REST '-[] ],
    rest_context(Name, Where, Type),
    [ ')'-[] ].

rest_context(body, request_body, json) -->
    [ 'invalid request body'-[] ].
rest_context(body, request_body, _Type) -->
    [ 'request body'-[] ].
rest_context(Name, Where, _Type) -->
    [ '~p parameter ~p'-[Where, Name] ].
