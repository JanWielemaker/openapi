/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
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

            openapi_doc/3                       % +File, +Mode, +Options
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
:- use_module(library(http/http_open)).         % http_open/3 is called by
                                                % generated code.

/** <module> OpenAPI (Swagger) library

This library implements generating  server  and   client  code  from  an
OpenAPI  specification.  The  generated  code    generates  or  extracts
parameters from the path,  request  or   request  body  and  type-checks
parameters as well as responses.
*/

:- meta_predicate
    openapi_dispatch(:).

%!  openapi_server(+File, +Options)
%
%   Instantiate a REST server given the   OpenAPI specification in File.
%   Normally, use `swipl-openapi --server=server.pl spec.yaml` to create
%   a file that uses this directive  and generates documentation for the
%   server operations as well as a skeleton predicate.

openapi_server(File, Options) :-
    throw(error(context_error(nodirective, openapi_server(File, Options)), _)).

expand_openapi_server(File, Options,
                      [ (:- discontiguous((openapi_handler/9,
                                           openapi_doc/2,
                                           openapi_error_hook/3)))
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

expand_openapi_client(File, Options, Clauses) :-
    read_openapi_spec(File, Spec, Options, Options1),
    phrase(client_clauses(Spec, Options1), Clauses).

%!  read_openapi_spec(+File, -Spec, +Options0, -Options) is det.

read_openapi_spec(File, Spec, Options0, Options) :-
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

server_clauses(JSONTerm, Options) -->
    { dict_pairs(JSONTerm.paths, _, Paths)
    },
    root_clause(JSONTerm.servers),
    server_path_clauses(Paths, Options),
    json_schema_clauses(JSONTerm, Options).

root_clause([Server|_]) -->
    { uri_components(Server.url, Components),
      uri_data(path, Components, Root)
    },
    [ openapi_root(Root) ].

server_path_clauses([], _) --> [].
server_path_clauses([H|T], Options) -->
    server_path_clause(H, Options),
    server_path_clauses(T, Options).

server_path_clause(Path-Spec, Options) -->
    { dict_pairs(Spec, _, Methods) },
    path_handlers(Methods, Path, Options).

path_handlers([], _Path, _) --> [].
path_handlers([Method-Spec|T], Path, Options) -->
    { path_handler(Path, Method, Spec, Fact, Options),
      path_docs(Spec, Docs)
    },
    [Fact, Docs],
    path_handlers(T, Path, Options).

%! path_handler(+Path, +Method, +Spec, -PathList, -Request, -Content,
%!		?Result, -Handler) is det.

path_handler(Path, Method, Spec,
             openapi_handler(Method, PathList, SegmentMatches,
                             Request, AsOption, OptionParam,
                             Content, Responses, Handler),
             Options) :-
    atomic_list_concat(Parts, '/', Path),
    path_vars(Parts, PathList, PathBindings),
    (   ParamSpecs = Spec.get(parameters)
    ->  server_parameters(ParamSpecs, PathBindings, SegmentMatches,
                          Request, AsOption, Params, Options),
        (   AsOption == []
        ->  OptionParams = []
        ;   OptionParams = [OptionParam]
        )
    ;   assertion(PathBindings == []),          % TBD: Proper message
        SegmentMatches = [],
        Params = [],
        Request = [],
        AsOption = [],
        OptionParams = []
    ),
    content_parameter(Method, Spec, Content, Params, Params1, Options),
    append(Params1, [Result|OptionParams], AllParams),
    dict_pairs(Spec.responses, _, ResPairs),
    maplist(response(Result, Options), ResPairs, Responses),
    atom_string(PredName, Spec.operationId),
    Handler =.. [PredName|AllParams].

%!  server_parameters(+ParamSpecs, +PathBindings,
%!                    -SegmentMatches, -RequestParams, -RequestOptions,
%!                    -HandlerParams, +Options) is det.

server_parameters([], _, [], [], [], [], _).
server_parameters([H|T], PathB, Segs, Request, AsOption, Params, Options) :-
    _{name:NameS, in:"query"} :< H,
    !,
    phrase(http_param_options(H, Options), Opts),
    atom_string(Name, NameS),
    R0 =.. [Name,P0,Opts],
    (   Opts = [optional(true)|_],
        \+ option(optional(unbound), Options)
    ->  AsOption = [R0|AsOpts],
        server_parameters(T, PathB, Segs, Request, AsOpts, Params, Options)
    ;   Request = [R0|Req],
        Params  = [P0|Ps],
        server_parameters(T, PathB, Segs, Req, AsOption, Ps, Options)
    ).
server_parameters([H|T], PathB, [segment(Type, Seg, P0, Name, Descr)|Segs],
                  Req, AsOption, [P0|Ps], Options) :-
    _{name:NameS, in:"path"} :< H,
    !,
    atom_string(Name, NameS),
    (   memberchk(Name=Seg, PathB)
    ->  param_type(H, Type, Options),
        param_description(H, Descr)
    ;   existence_error(path_parameter, Name)
    ),
    server_parameters(T, PathB, Segs, Req, AsOption, Ps, Options).
server_parameters([H|_], _PathB, _Segments, _Req, _AsOption, _, _) :-
    print_message(error, openapi(parameter_failed(H))),
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
    [ ParmType ].

json_param_type(array(Type), list(openapi(Type))) :- !.
json_param_type(Type, openapi(Type)).

hp_description(Spec) -->
    { Descr = Spec.get(description) },
    !,
    [ description(Descr) ].
hp_description(_) --> [].

%!  path_docs(+Spec, -Docs) is det.
%
%   Generate documentation clauses for an operationID

path_docs(Spec, openapi_doc(OperationID, Docs)) :-
    atom_string(OperationID, Spec.operationId),
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


%!  path_vars(+SegmentSpec, -Segments, -Bindings) is det.

path_vars([], [], []).
path_vars([H0|T0], [H|T], [Name=H|BT]) :-
    sub_atom(H0, 0, _, _, '{'),
    sub_atom(H0, _, _, 0, '}'),
    !,
    sub_atom(H0, 1, _, 1, Name),
    path_vars(T0, T, BT).
path_vars([H|T0], [H|T], Bindings) :-
    path_vars(T0, T, Bindings).

%! content_parameter(+Method, +Spec, -Content,
%!                   +Params0, -Params, +Options) is det.
%
%  If there is a request body, add it to the parameter list and return a
%  specification for openapi_dispatch/1 in Content.

content_parameter(Method, Spec, content(MediaType, Schema, Var, Descr),
                  Params, AllParams, Options) :-
    has_content(Method),
    !,
    request_content_type(Spec, MediaType, Schema, Options),
    content_description(Spec, Descr),
    append(Params, [Var], AllParams).
content_parameter(_, _, -, Params, Params, _).

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

content_type(Spec, media(application/json, []), Type, Options) :-
    Content = Spec.get(content),
    Media = Content.get('application/json'),
    !,
    (   Schema = Media.get(schema)
    ->  json_type(Schema, Type, Options)
    ;   Type = (-)
    ).
content_type(_Spec, media(application/json, []), -, _).

		 /*******************************
		 *       CLIENT COMPILER	*
		 *******************************/

%!  client_clauses(+JSONTerm, +Options)//
%
%   Generate clauses for the client.  The generated clauses are:
%
%     - openapi_server(URL)
%       One or more clauses describing the location of the server
%       as defined in the Swagger file.
%     - Clauses that call the REST methods.  The name is the
%       `operationId` described in the Swagger file.  The arguments
%       are defined by the parameters and response from the
%       Swagger file.
%     - Clauses that define the JSON schema types.

client_clauses(JSONTerm, Options) -->
    { dict_pairs(JSONTerm.paths, _, Paths)
    },
    server_clauses(JSONTerm.servers),
    client_path_clauses(Paths, Options),
    json_schema_clauses(JSONTerm, Options).

server_clauses([]) --> [].
server_clauses([H|T]) --> server_clause(H), server_clauses(T).

server_clause(Server) -->
    [ openapi_server(Server.get(url)) ].

client_path_clauses([], _) --> [].
client_path_clauses([H|T], Options) -->
    client_path_clause(H, Options),
    client_path_clauses(T, Options).

client_path_clause(Path-Spec, Options) -->
    { dict_pairs(Spec, _, Methods) },
    client_handlers(Methods, Path, Options).

client_handlers([], _, _) --> [].
client_handlers([H|T], Path, Options) -->
    { client_handler(H, Path, Clause, Options) },
    [Clause],
    client_handlers(T, Path, Options).

client_handler(Method-Spec, PathSpec, (Head :- Body), Options) :-
    atomic_list_concat(Parts, '/', PathSpec),
    path_vars(Parts, PathList, PathBindings),
    atom_string(PredName, Spec.operationId),
    (   ParamSpecs = Spec.get(parameters)
    ->  client_parameters(ParamSpecs, PathBindings,
                          Params, Query, Optional,
                          CheckParams, Options),
        (   Optional == []
        ->  ClientOptionArg = []
        ;   ClientOptionArg = [ClientOptions]
        )
    ;   assertion(PathBindings == []),          % TBD: Proper message
        Params = [],
        Query = [],
        CheckParams = true,
        Optional = [],
        ClientOptionArg = []
    ),
    content_parameter(Method, Spec, Content, Params, Params1, Options),
    request_body(Content, ContentGoal, OpenOptions),
    dict_pairs(Spec.responses, _, ResPairs),
    maplist(response(Result, Options), ResPairs, Responses),
    (   response_has_data(Responses)
    ->  append(Params1, [Result|ClientOptionArg], AllParams)
    ;   append(Params1, ClientOptionArg, AllParams)
    ),
    prolog_load_context(module, Module),
    (   PathBindings == []
    ->  Path = PathSpec,
        PathGoal = true
    ;   PathGoal = atomic_list_concat(PathList, '/', Path)
    ),
    Head =.. [PredName|AllParams],
    Body = ( CheckParams, PathGoal, ContentGoal,
             openapi:assemble_query(Module, Path,
                                    Query, Optional, ClientOptions,
                                    URL),
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

%!  response_has_data(Responses) is semidet.
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
%!                    -Params, -Required, -Optional,
%!                    -Check, +Options)

client_parameters([], _, [], [], [], true, _).
client_parameters([H|T], PathBindings, [P0|Ps],
                  [qparam(Name,P0,Type,Opt)|Qs], Optional, Check, Options) :-
    _{name:NameS, in:"query"} :< H,
    param_optional(H, Opt),
    \+ ( Opt == optional,
         \+ option(optional(unbound), Options)
       ),
    !,
    param_type(H, Type, Options),
    atom_string(Name, NameS),
    client_parameters(T, PathBindings, Ps, Qs, Optional, Check0, Options),
    mkconj(Check0, true, Check).
client_parameters([H|T], PathBindings,
                  Params, Query, [qparam(Name,_,Type,optional)|OptT],
                  Check, Options) :-
    _{name:NameS, in:"query"} :< H,
    !,
    param_type(H, Type, Options),
    atom_string(Name, NameS),
    client_parameters(T, PathBindings, Params, Query, OptT, Check0, Options),
    mkconj(Check0, true, Check).
client_parameters([H|T], PathBindings, [P0|Ps], Query, Opt, Check, Options) :-
    _{name:NameS, in:"path"} :< H,
    !,
    atom_string(Name, NameS),
    param_type(H, Type, Options),
    (   memberchk(Name=Segment, PathBindings)
    ->  Check1 = openapi:segment_value(Type, Segment, P0)
    ;   existence_error(path_parameter, Name)
    ),
    client_parameters(T, PathBindings, Ps, Query, Opt, Check0, Options),
    mkconj(Check0, Check1, Check).

param_optional(Spec, Optional) :-
    (   Spec.get(required) == false
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

%!  request_body(+ContentSpec, -Goal, -HTTPOPenOptions) is det.
%
%   Translate the request body into options for http_open/3.

request_body(content(media(application/json,_), Schema, InVar, _Descr),
             openapi:json_check(Schema, OutVar, InVar),
             [ post(json(OutVar))
             ]) :-
    !.
request_body(content(MediaType, _Schema, _Var, _Descr), _, _) :-
    !,
    domain_error(openapi(content_type), MediaType).
request_body(_, true, []).


:- public
    assemble_query/6.

assemble_query(Module, Path, QParams, QOptional, QOptions, URL) :-
    call(Module:openapi_server(ServerBase)),
    convlist(client_query_param, QParams, QueryFromArgs),
    optional_query_params(QOptional, QOptions, QueryFromOptions),
    append(QueryFromArgs, QueryFromOptions, Query),
    (   Query == []
    ->  atomics_to_string([ServerBase, Path], URL)
    ;   phrase(array_query(Query), ArrayQuery),
        uri_query_components(QueryString, ArrayQuery),
        atomics_to_string([ServerBase, Path, "?", QueryString], URL)
    ).

%!  array_query(Query)//
%
%   Rewrite Name=List into Name=E1, Name=E2,  ... to support array(Type)
%   for parameters passed as queries.

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

openapi_read_reply(Code, ContentType, Responses, In, Result) :-
    debug(openapi(reply), 'Got code ~p; type: ~p; response schemas: ~p',
          [Code, ContentType, Responses]),
    http_parse_header_value(content_type, ContentType, ParsedContentType),
    (   memberchk(response(Code, As, ExpectedContentType, Type, _Result, _Comment),
                  Responses)
    ->  true
    ;   maplist(arg(1), Responses, ExCodes),
        must_be(oneof(ExCodes), Code)
    ),
    content_matches(ExpectedContentType, ParsedContentType, ProcessType),
    read_reply(ProcessType, Type, As, Code, In, Result).

content_matches(ContentType, ContentType, ContentType) :- !.
content_matches(media(Type, _), media(Type, Attrs), media(Type, Attrs)) :- !.
content_matches(Expected, Got, _) :-
    type_error(media(Expected), Got).

read_reply(media(application/json, _), Type, As, Code, In, Result) :-
    json_read_dict(In, Result0, []),
    (   Type = (-)
    ->  Result = Result0
    ;   json_check(Type, Result1, Result0)
    ),
    reply_result(As, Code, Result1, Result).

reply_result(data,  _Code, Result, Result).
reply_result(error, Code, Result, _ ) :-
    throw(error(rest_error(Code, Result), _)).


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
    atomic_list_concat(Parts, '/', Path),
    M:openapi_handler(Method, Parts, Segments,
                      Required, AsOption, OptionParam, Content,
                      Responses,
                      Handler),
    !,
    (   catch(openapi_run(M:Request,
                          Segments,
                          Required, AsOption, OptionParam, Content,
                          Responses,
                          Handler),
              Error,
              openapi_error(M, Error, Responses))
    ->  true
    ;   openapi_error(M, failed, Responses)
    ).

openapi_run(Module:Request,
            Segments,
            Required, AsOption, OptionParam, Content,
            Responses,
            Handler) :-
    maplist(segment_parameter, Segments),
    append(Required, AsOption, RequestParams),
    http_parameters(Request, RequestParams),
    request_body(Content, Request),
    server_handler_options(AsOption, OptionParam),
    call(Module:Handler),
    openapi_reply(Responses).

segment_parameter(segment(Type, Segment, Value, _Name, _Description)) :-
    segment_value(Type, Segment, Value).

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

%!  request_body(+ContentSpec, +Request) is det.
%
%   Read the specified request body.

request_body(-, _).
request_body(content(media(application/json,_), -, Body, _Descr), Request) :-
    !,
    http_read_json_dict(Request, Body).
request_body(content(media(application/json,_), Type, Body, _Descr), Request) :-
    http_read_json_dict(Request, Body0),
    json_check(Type, Body0, Body).

%!  openapi_reply(+Responses) is det.
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

openapi_reply(Responses) :-
    Responses = [R0|_],
    arg(5, R0, Reply),
    reply_status(Reply, Code, Data),
    memberchk(response(Code, _As, MediaType, Type, _, _Descr), Responses),
    openapi_reply(Code, MediaType, Type, Data).

reply_status(Var, _, _) :-
    var(Var), !,
    instantiation_error(Var).
reply_status(status(Code, Data), Code, Data) :- !.
reply_status(status(Code), Code, '') :- !.
reply_status(Data, 200, Data).

openapi_reply(Code, media(application/json,_), -, Data) :-
    !,
    reply_json_dict(Data, [status(Code)]).
openapi_reply(Code, media(application/json,_), Type, Data) :-
    !,
    json_check(Type, Out, Data),
    reply_json_dict(Out, [status(Code)]).
openapi_reply(Code, media(MediaType, _Attrs), _, '') :-
    format('Status: ~d~n', [Code]),
    format('Content-type: ~w~n~n', [MediaType]).

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
    call(Module:openapi_error_hook(Error, Reply, Responses)),
    Responses = [R0|_],
    arg(4, R0, Reply),
    openapi_reply(Responses),
    !.
openapi_error(_Module, Error, _Responses) :-
    throw(Error).


		 /*******************************
		 *            TYPES		*
		 *******************************/

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

%!  oas_type(+Type, ?In, ?Out) is det.

oas_type(int32, In, Out) :-
    cvt_integer(In, Out),
    must_be(between(-2147483648, 2147483647), Out).
oas_type(int64, In, Out) :-
    cvt_integer(In, Out),
    must_be(between(-9223372036854775808, 9223372036854775807), Out).
oas_type(integer, In, Out) :-
    cvt_integer(In, Out).
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
oas_type(binary, In, Out) :-
    (   var(In)
    ->  to_string(Out, In)
    ;   to_string(In, Out)
    ).
oas_type(base64, In, Out) :-
    base64(In, Out).
oas_type(boolean, In, Out) :-
    (   nonvar(In)
    ->  to_boolean(Out, In)
    ;   to_boolean(In, Out)
    ).
oas_type(date, In, Out) :-
    xsd_time_string(In, 'http://www.w3.org/2001/XMLSchema#date', Out).
oas_type(date_time, In, Out) :-
    xsd_time_string(In, 'http://www.w3.org/2001/XMLSchema#dateTime', Out).
oas_type(password, In, Out) :-
    (   var(In)
    ->  to_string(Out, In)
    ;   to_string(In, Out)
    ).

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

%!  json_check(+Spec, ?JSONIn, ?JSONOut)
%
%   Validate a JSON object.

json_check(url(URL), In, Out) :-
    !,
    json_schema(URL, Type),
    json_check(Type, In, Out).
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
json_check(array(Type), In, Out) :-
    !,
    (   is_list(In)
    ->  maplist(json_check(Type), In, Out)
    ;   is_list(Out)
    ->  maplist(json_check(Type), In, Out)
    ;   must_be(list, In, Out)
    ).
json_check(oneOf(Types), In, Out) :-
    !,
    (   nonvar(In)
    ->  append(_, [Type|Rest], Types),
        catch(json_check(Type, In, Out), _, fail),
        (   member(T2, Rest),
            catch(json_check(T2, In, _), _, fail)
        ->  type_error(oneOf(Types), In)
        ;   true
        )
    ;   append(_, [Type|Rest], Types),
        catch(json_check(Type, In, Out), _, fail),
        (   member(T2, Rest),
            catch(json_check(T2, _, Out), _, fail)
        ->  type_error(oneOf(Types), Out)
        ;   true
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
json_check(anyof(Types), In, Out) :-
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
json_check(Type, In, Out) :-
    oas_type(Type, In, Out).

json_check_in_out_type(In, Out, Type) :- json_check(Type, In, Out).
json_check_out_in_type(Out, In, Type) :- json_check(Type, In, Out).

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
    dict_pairs(Dict, _, Pairs).
json_object_pairs(json(List), Pairs) :-
    is_list(List),
    maplist(name_value, List, Keys, Values),
    pairs_keys_values(Pairs0, Keys, Values),
    keysort(Pairs0, Pairs).

name_value(Name - Value, Name, Value) :- !.
name_value(Name = Value, Name, Value) :- !.
name_value(Term, Name, Value) :- Term =.. [Name,Value].

%!  obj_properties_in(+InPairs, +Spec, -OutPairs)

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
obj_properties_in([N-V0|T0], [p(N,Type,_Req)|PT], [N-V|T]) :-
    !,
    json_check(Type, V0, V),
    obj_properties_in(T0, PT, T).
obj_properties_in(T0, [p(N,_Type,Req)|PT], T) :-
    (   Req == false
    ->  obj_properties_in(T0, PT, T)
    ;   existence_error(json_property, N)
    ).

check_missing([]).
check_missing([p(N,_Type,Req)|T]) :-
    (   Req == false
    ->  check_missing(T)
    ;   existence_error(json_property, N)
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
obj_properties_out([N-V0|T0], [p(N,Type,_Req)|PT], [N-V|T]) :-
    !,
    json_check(Type, V, V0),
    obj_properties_out(T0, PT, T).
obj_properties_out(T0, [p(N,_Type,Req)|PT], T) :-
    (   Req == false
    ->  obj_properties_out(T0, PT, T)
    ;   existence_error(json_property, N)
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
%     - array(ItemType)
%     - object(Properties)
%       Properties is an ordered list of
%       - p(Name, Type, Required)
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

%!  json_type(+Spec, -Type, +Options) is det.
%
%   True when Type  is  the  type   representation  for  the  JSON  type
%   description Spec.

json_type(Spec, Type, _) :-
    _{type:TypeS, format:FormatS} :< Spec,
    !,
    atom_string(Type0, TypeS),
    atom_string(Format, FormatS),
    once(api_type(_, Type0, Format, Type)).
json_type(Spec, object(Props), Options) :-
    _{required:ReqS, properties:PropSpecs} :< Spec,
    !,
    dict_pairs(PropSpecs, _, Pairs),
    maplist(atom_string, Req, ReqS),
    maplist(schema_property(Req, Options), Pairs, Props0),
    sort(Props0, Props).
json_type(Spec, object, _Options) :-
    _{type:"object"} :< Spec,
    !.
json_type(Spec, array(Type), Options) :-
    _{type:"array", items:IType} :< Spec,
    !,
    json_type(IType, Type, Options).
json_type(Spec, oneOf(Types), Options) :-
    _{oneOf:List} :< Spec,
    !,
    maplist(opts_json_type(Options), List, Types).
json_type(Spec, allOf(Types), Options) :-
    _{allOf:List} :< Spec,
    !,
    maplist(opts_json_type(Options), List, Types).
json_type(Spec, oneOf(Types), Options) :-
    _{oneOf:List} :< Spec,
    !,
    maplist(opts_json_type(Options), List, Types).
json_type(Spec, not(Type), Options) :-
    _{not:NSpec} :< Spec,
    !,
    json_type(NSpec, Type, Options).
json_type(Spec, Type, _) :-
    _{type:TypeS} :< Spec,
    !,
    atom_string(Type0, TypeS),
    once(api_type(_, Type0, -, Type)).
json_type(Spec, url(URL), Options) :-
    _{'$ref':URLS} :< Spec,
    !,
    option(base_uri(Base), Options),
    file_directory_name(Base, Dir),
    atom_concat(Dir, URLS, URL).

opts_json_type(Options, Spec, Type) :-
    json_type(Spec, Type, Options).

schema_property(Reqs, Options, Name-Spec, p(Name, Type, Req)) :-
    (   memberchk(Name, Reqs)
    ->  Req = true
    ;   Req = false
    ),
    json_type(Spec, Type, Options).

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
    file_header(Stream, File, Options),
    forall(doc_data(Clauses, OperationId, Data, Options),
           (   phrase(openapi_doc(OperationId, Data, Options), S),
               format(Stream, '~s', [S]),
               fail
           ;   true
           )).

file_header(Stream, File, Options) :-
    option(mode(client), Options),
    !,
    format(Stream, ':- use_module(library(openapi)).~n', []),
    format(Stream, ':- use_module(library(option)).~n~n', []),
    format(Stream, ':- openapi_client(~q, []).~n~n', [File]).
file_header(Stream, File, Options) :-
    option(mode(server), Options),
    !,
    format(Stream, ':- use_module(library(openapi)).~n', []),
    format(Stream, ':- use_module(library(option)).~n', []),
    format(Stream, ':- use_module(library(debug)).~n', []),
    server_header(Stream, File, Options),
    format(Stream, '~n', []),
    format(Stream, ':- openapi_server(~q, []).~n~n', [File]).
file_header(_, _, _).

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

%!  openapi_doc(+OperationID, +Data, +Options)//

openapi_doc(OperationId, Data, Options) -->
    doc_mode(OperationId, Data.arguments),
    "\n%\n",
    doc_description(Data.doc),
    doc_args(Data.arguments),
    "\n",
    server_skeleton(OperationId, Data.arguments, Options).

server_skeleton(OperationId, Args, Options) -->
    { option(mode(server), Options) },
    !,
    server_head(OperationId, Args), " :-",
    "\n    debug(openapi, \"~p\", [",
		 server_head(OperationId, Args), "]),",
    "\n    Response = status(404).\n\n".
server_skeleton(_,_,_) --> [].

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

server_head(OperationId, Args) -->
    quoted_atom(OperationId),
    "(", arguments(Args), ")".

arguments([]) --> [].
arguments([H|T]) -->
    argument(H),
    (  {T==[]}
    -> []
    ;  ", ",
       arguments(T)
    ).

argument(p(Name, _Type, _Descr)) -->
    camel_case(Name).

quoted_atom(Atom, List, Tail) :-
    format(codes(List,Tail), '~q', [Atom]).

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
camel_skip([H|T]) --> !, [H], camel_skip(T).

doc_description(Doc) -->
    { memberchk(summary(Summary), Doc),
      memberchk(description(Desc), Doc),
      split_string(Desc, "\n", "", Lines)
    }, !,
    "%  ", atom(Summary), "\n",
    lines(Lines, "%  "),
    "%\n".
doc_description(Doc) -->
    { memberchk(description(Desc), Doc),
      split_string(Desc, "\n", "", Lines)
    }, !,
    lines(Lines, "%  "),
    "%\n".
doc_description(Doc) -->
    { memberchk(summary(Summary), Doc)
    }, !,
    "%  ", atom(Summary), "\n",
    "%\n".
doc_description(_) -->  [].

lines([], _) --> [].
lines([H|T], Prefix) --> atom(Prefix), atom(H), "\n", lines(T, Prefix).

doc_args([]) --> [].
doc_args([H|T]) --> doc_arg(H), doc_args(T).

doc_arg(p(Name, Type, Description)) -->
    "%  @arg ", camel_case(Name), " ", type(Type), "\n",
    arg_description(Description).

arg_description(options(List)) -->
    !,
    arg_options(List).
arg_description(Description) -->
    { split_string(Description, "\n", "", Lines) },
    lines(Lines, "%       ").

arg_options([]) --> [].
arg_options([H|T]) --> arg_option(H), arg_options(T).

arg_option(p(Name, Type, Description)) -->
    "%       - ", quoted_atom(Name), "(+", type(Type), ")", "\n",
    "%         ", atom(Description), "\n".

type(list(option)) --> !.
type(url(URL)) -->
    !,
    { file_base_name(URL, TypeName) },
    atom(TypeName).
type(array(Type)) --> !,
    "array(", type(Type), ")".
type(Type, List, Tail) :-
    format(codes(List, Tail), '~p', [Type]).


%!  doc_data(:ServerClauses, -OperationID, -Data:dict, +Options) is det.
%
%   Get  a  dict  that  contains   all    information   to  produce  the
%   documentation.

doc_data(Clauses, OperationId, _{arguments:Params, doc:Doc}, Options) :-
    member(openapi_handler(_Method, _PathList, Segments,
                           Request, AsOption, OptionParam,
                           Content, Responses, Handler), Clauses),
    Handler =.. [OperationId|Args],
    memberchk(openapi_doc(OperationId, Doc), Clauses),
    maplist(doc_param(from(Segments,
                           Request, AsOption, OptionParam,
                           Content, Responses), Options), Args, Params0),
    exclude(==(-), Params0, Params).

doc_param(from(Segments, Request, AsOption, OptionParam,
               Content, Responses), Options,
          Arg, Param) :-
    (   segment_param(Arg, Segments, Param)
    ;   request_param(Arg, Request, Param)
    ;   OptionParam == Arg,
        option_param(AsOption, Param)
    ;   content_param(Arg, Content, Param)
    ;   response_param(Arg, Responses, Param, Options)
    ), !.

segment_param(Arg, Segments, p(Name, Type, Description)) :-
    member(segment(Type, _, Arg0, Name, Description), Segments),
    Arg == Arg0, !.

request_param(Arg, Requests, p(Name, Type, Description)) :-
    member(R, Requests),
    R =.. [Name,Arg0,Opts],
    Arg == Arg0, !,
    param_json_type(Opts, Type),
    (   memberchk(description(Description), Opts)
    ->  true
    ;   Description = ""
    ).

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
    { H =.. [Name,_Var,Options],
      param_json_type(Options, Type),
      (   memberchk(description(Description), Options)
      ->  true
      ;   Description = ""
      )
    },
    [ p(Name,Type,Description) ],
    doc_request_params(T).

content_param(Arg,
              content(_MediaType, Scheme, Arg0, Description),
              p(request_body, Scheme, Description)) :-
    Arg == Arg0, !.

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
    prolog:error_message//1.

prolog:error_message(rest_error(Code, Term)) -->
    [ 'REST error: code: ~p, data: ~p'-[Code, Term] ].
