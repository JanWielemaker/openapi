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

            api_default/2,                      % Var, Default

            openapi_read/2                      % +File, -Term
          ]).
:- use_module(library(apply)).
:- use_module(library(apply_macros), []).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(base64)).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(uri)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).

:- meta_predicate
    openapi_dispatch(:).

%!  openapi_server(+File, +Options)
%
%   Instantiate a REST server given the OpenAPI specification in File.

openapi_server(File, Options) :-
    throw(error(context_error(nodirective, openapi_server(File, Options)), _)).

expand_openapi_server(File, Options, Clauses) :-
    read_openapi_spec(File, Spec, Options, Options1),
    phrase(server_clauses(Spec, Options1), Clauses).

%!  openapi_client(+File, +Options)
%
%   Instantiate a REST client given the OpenAPI specification in File.

openapi_client(File, Options) :-
    throw(error(context_error(nodirective, openapi_client(File, Options)), _)).

expand_openapi_client(File, Options, Clauses) :-
    read_openapi_spec(File, Spec, Options, Options1),
    phrase(client_clauses(Spec, Options1), Clauses).

%!  api_default(+Param, +Default) is det.

api_default(Param, Default) :-
    (   var(Param)
    ->  Param = Default
    ;   true
    ).

%!  read_openapi_spec(+File, -Spec, +Options0, -Options) is det.

read_openapi_spec(File, Spec, Options0, Options) :-
    prolog_load_context(directory, Dir),
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
%
%   @tbd:  we  need  a  native  YAML   parser  rather  than  relying  on
%   `yaml2json`.

openapi_read(File, Term) :-
    file_name_extension(_, yaml, File),
    !,
    setup_call_cleanup(
        process_create(path(yaml2json), [file(File)],
                       [ stdout(pipe(In))
                       ]),
        json_read_dict(In, Term),
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
    { path_handler(Path, Method, Spec, PathList, Request, Content, Result,
                   Handler, Options)
    },
    [ openapi_handler(Method, PathList, Request, Content, Result, Handler) ],
    path_handlers(T, Path, Options).

%! path_handler(+Path, +Method, +Spec, -PathList, -Request, -Content,
%!		?Result, -Handler) is det.

path_handler(Path, Method, Spec, PathList, Request,
             Content, Responses,
             Handler, Options) :-
    atomic_list_concat(Parts, '/', Path),
    path_vars(Parts, PathList, PathBindings),
    (   ParamSpecs = Spec.get(parameters)
    ->  parameters(ParamSpecs, PathBindings, Request, Params, Options)
    ;   assertion(PathBindings == []),          % TBD: Proper message
        Params = [],
        Request = []
    ),
    content_parameter(Method, Spec, Content, Params, Params1, Options),
    append(Params1, [Result], AllParams),
    dict_pairs(Spec.responses, _, ResPairs),
    maplist(response(Result, Options), ResPairs, Responses),
    atom_string(PredName, Spec.operationId),
    Handler =.. [PredName|AllParams].

parameters([], _, [], [], _).
parameters([H|T], PathB, [R0|Req], [P0|Ps], Options) :-
    _{name:NameS, in:"query"} :< H,
    !,
    phrase(http_param_options(H, Options), Opts),
    atom_string(Name, NameS),
    R0 =.. [Name,P0,Opts],
    parameters(T, PathB, Req, Ps, Options).
parameters([H|T], PathB, Req, [P0|Ps], Options) :-
    _{name:NameS, in:"path"} :< H,
    !,
    atom_string(Name, NameS),
    (   memberchk(Name=P0, PathB)
    ->  true                            % TBD: type conversion and encoding
    ;   existence_error(path_parameter, Name)
    ),
    parameters(T, PathB, Req, Ps, Options).
parameters([H|_], _PathB, _Req, _, _) :-
    print_message(error, openapi(parameter_failed(H))),
    fail.

http_param_options(Spec, Options) -->
    hp_optional(Spec),
    hp_type(Spec, Options).

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
    { json_type(Spec, Type, Options) },
    [ openapi(Type) ].

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
%   @tbd The post may contain requestBody that specifies the content
%   type and optional schema.

content_parameter(Method, Spec, content(MediaType, Schema, Var),
                  Params, AllParams, Options) :-
    has_content(Method),
    !,
    request_content_type(Spec, MediaType, Schema, Options),
    append(Params, [Var], AllParams).
content_parameter(_, _, -, Params, Params, _).

has_content(post).
has_content(put).

request_content_type(Spec, MediaType, Schema, Options) :-
    (   Body = Spec.get(requestBody)
    ->  true
    ;   Body = _{}
    ),
    !,
    content_type(Body, MediaType, Schema, Options).

%!  response(+ResultVar, +Options, +ResponsePair, -Response) is det.

response(Result, Options, CodeA-Spec, response(Code, MediaType, Type, Result)) :-
    response_code(CodeA, Code),
    content_type(Spec, MediaType, Type, Options).

response_code(default, _) :- !.
response_code(A, N) :-
    to_number(A, N).

content_type(Spec, 'application/json', Type, Options) :-
    Content = Spec.get(content),
    Media = Content.get('application/json'),
    !,
    (   Schema = Media.get(schema)
    ->  json_type(Schema, Type, Options)
    ;   Type = (-)
    ).
content_type(_Spec, 'application/json', -, _).

		 /*******************************
		 *       CLIENT COMPILER	*
		 *******************************/

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
    path_vars(Parts, PathList, PathBindings),   % TBD: deal with URL encoding
    atom_string(PredName, Spec.operationId),
    (   ParamSpecs = Spec.get(parameters)
    ->  client_parameters(ParamSpecs, PathBindings, Params0, Query,
                          CheckParams, Options),
        optional_client_params(Params0, Params, Optional, Options),
        (   Optional == []
        ->  ClientOptionArg = []
        ;   ClientOptionArg = [ClientOptions]
        )
    ;   assertion(PathBindings == []),          % TBD: Proper message
        Params = [],
        Query = [],
        CheckParams = true
    ),
    content_parameter(Method, Spec, Content, Params, Params1, Options),
    request_body(Content, ContentGoal, OpenOptions),
    append(Params1, [Result|ClientOptionArg], AllParams),
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
                 http_open(URL, In,
                           [ status_code(Status),
                             method(Method)
                           | OpenOptions
                           ]),
                 openapi:openapi_read_reply(Status, In, Result),
                 close(In))
           ).

client_parameters([], _, [], [], true, _).
client_parameters([H|T], PathBindings, [P0|Ps],
                  [qparam(Name,P0,Type,Opt)|Qs], Check, Options) :-
    _{name:NameS, in:"query"} :< H,
    !,
    param_optional(H, Opt),
    param_type(H, Type, Options),
    atom_string(Name, NameS),
    client_parameters(T, PathBindings, Ps, Qs, Check0, Options),
    mkconj(Check0, true, Check).
client_parameters([H|T], PathBindings, [P0|Ps], Query, Check, Options) :-
    _{name:NameS, in:"path"} :< H,
    !,
    atom_string(Name, NameS),
    param_type(H, Type, Options),
    (   memberchk(Name=Segment, PathBindings)
    ->  Check1 = openapi:segment_value(Type, Segment, P0)
    ;   existence_error(path_parameter, Name)
    ),
    client_parameters(T, PathBindings, Ps, Query, Check0, Options),
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

mkconj(true, G, G) :- !.
mkconj(G, true, G) :- !.
mkconj(G1, G2,  (G1,G2)).

optional_client_params(Params, Params, [], Options) :-
    option(optional(unbound), Options),
    !.
optional_client_params(Params, Args, Options, _) :-
    partition(is_optional, Params, Options, Args).

is_optional(qparam(_,_,_,optional)).

%!  request_body(+ContentSpec, -Goal, -HTTPOPenOptions) is det.

request_body(content('application/json', Schema, InVar),
             openapi:json_check(Schema, OutVar, InVar),
             [ post(json(OutVar))
             ]) :-
    !.
request_body(content(MediaType, _Schema, _Var), _, _) :-
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
    ;   uri_query_components(QueryString, Query),
        atomics_to_string([ServerBase, Path, "?", QueryString], URL)
    ).

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



%!  openapi_read_reply(+Code, +In, -Result) is det.
%
%   Handle the reply at the client side.

:- public openapi_read_reply/3.

openapi_read_reply(Code, In, Result) :-
    debug(openapi(reply), 'Got code ~p', [Code]),
    json_read_dict(In, Result, []).


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
    !,
    atom_concat(Root, Path, FullPath),
    atomic_list_concat(Parts, '/', Path),
    M:openapi_handler(Method, Parts, RequestParams, Content, Responses,
                      Handler),
    http_parameters(Request, RequestParams),
    request_body(Content, Request),
    call(M:Handler),
    openapi_reply(Responses).

%!  request_body(+ContentSpec, +Request) is det.
%
%   Read the specified request body.

request_body(-, _).
request_body(content('application/json', -, Body), Request) :-
    !,
    http_read_json_dict(Request, Body).
request_body(content('application/json', Type, Body), Request) :-
    http_read_json_dict(Request, Body0),
    json_check(Type, Body0, Body).

%!  openapi_reply(+Responses) is det.
%
%   Formulate the HTTP request from a term.
%
%   @arg Responses is a list response(Code, MediaType, Type, Reply),
%   where `Reply` is the variable that is bound by the use handler.

openapi_reply(Responses) :-
    Responses = [R0|_],
    arg(4, R0, Reply),
    reply_status(Reply, Code, Data),
    memberchk(response(Code, MediaType, Type, _), Responses),
    openapi_reply(Code, MediaType, Type, Data).

reply_status(Var, _, _) :-
    var(Var), !,
    instantiation_error(Var).
reply_status(status(Code, Data), Code, Data) :- !.
reply_status(status(Code), Code, '') :- !.
reply_status(Data, 200, Data).

openapi_reply(Code, 'application/json', -, Data) :-
    !,
    reply_json_dict(Data, [status(Code)]).
openapi_reply(Code, 'application/json', Type, Data) :-
    !,
    json_check(Type, Out, Data),
    reply_json_dict(Out, [status(Code)]).
openapi_reply(Code, MediaType, _, '') :-
    format('Status: ~d~n', [Code]),
    format('Content-type: ~w~n~n', [MediaType]).


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
json_check(object(Properties), In, Out) :-
    !,
    (   nonvar(In)
    ->  maplist(obj_property_in(In), Properties, Pairs0),
        exclude(==(-), Pairs0, Pairs),
        dict_pairs(Out, _, Pairs)
    ;   maplist(obj_property_out(Out), Properties, Pairs0),
        exclude(==(-), Pairs0, Pairs),
        dict_pairs(In, _, Pairs)
    ).
json_check(array(Type), In, Out) :-
    !,
    (   is_list(In)
    ->  maplist(json_check(Type), In, Out)
    ;   is_list(Out)
    ->  maplist(json_check(Type), In, Out)
    ;   must_be(list, In)
    ).
json_check(Type, In, Out) :-
    oas_type(Type, In, Out).

obj_property_in(In, p(Name, Type, true), Name-Out) :-
    !,
    json_check(Type, In.Name, Out).
obj_property_in(In, p(Name, Type, false), Out) :-
    (   InV = In.get(Name)
    ->  json_check(Type, InV, OutV),
        Out = (Name-OutV)
    ;   Out = (-)
    ).

obj_property_out(Out, p(Name, Type, true), Name-In) :-
    !,
    json_check(Type, In, Out.Name).
obj_property_out(Out, p(Name, Type, false), In) :-
    (   OutV = Out.get(Name)
    ->  json_check(Type, InV, OutV),
        In = (Name-InV)
    ;   In = (-)
    ).

:- multifile
    http:convert_parameter/3.

http:convert_parameter(openapi(Type), In, Out) :-
    json_check(Type, In, Out).

%!  json_schema(URL, Spec)
%
%   Spec is one of
%
%     - array(ItemType)
%     - object(Properties)
%       Properties is a list of
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
    { schema_type(Spec, Type, Options),
      option(base_uri(Base), Options),
      file_directory_name(Base, Dir),
      atomic_list_concat([Dir, '#/components/schemas/', Schema], URL)
    },
    [ openapi:json_schema(URL, Type) ].

schema_type(Spec, object(Props), Options) :-
    _{required:ReqS, properties:PropSpecs} :< Spec,
    !,
    dict_pairs(PropSpecs, _, Pairs),
    maplist(atom_string, Req, ReqS),
    maplist(schema_property(Req, Options), Pairs, Props).
schema_type(Spec, array(Type), Options) :-
    _{type:"array", items:IType} :< Spec,
    !,
    json_type(IType, Type, Options).
schema_type(Spec, Type, Options) :-
    json_type(Spec, Type, Options).

schema_property(Reqs, Options, Name-Spec, p(Name, Type, Req)) :-
    (   memberchk(Name, Reqs)
    ->  Req = true
    ;   Req = false
    ),
    json_type(Spec, Type, Options).

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
