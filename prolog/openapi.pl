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
            openapi/2,                          % +File, +Options

            api_default/2,                      % Var, Default

            openapi_read/2                      % +File, -Term
          ]).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(debug)).
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

%!  openapi(+File, +Options)
%
%

openapi(File, Options) :-
    throw(error(context_error(nodirective, openapi(File, Options)), _)).

expand_openapi(File, Options, Clauses) :-
    prolog_load_context(directory, Dir),
    absolute_file_name(File, Path,
                       [ relative_to(Dir),
                         extensions(['',json,yaml]),
                         access(read)
                       ]),
    openapi_read(Path, JSONTerm),
    phrase(openapi_clauses(JSONTerm, Options), Clauses).

%!  api_default(+Param, +Default) is det.

api_default(Param, Default) :-
    (   var(Param)
    ->  Param = Default
    ;   true
    ).

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
		 *            COMPILER		*
		 *******************************/

%!  openapi_clauses(+JSONTerm, +Options)//
%
%   Grammar to generate clauses that control openapi/1.

openapi_clauses(JSONTerm, _Options) -->
    { dict_pairs(JSONTerm.paths, _, Paths)
    },
    root_clause(JSONTerm.servers),
    path_clauses(Paths),
    (   { Schemas = JSONTerm.get(components).get(schemas),
          dict_pairs(Schemas, _, SchemaPairs)
        }
    ->  schema_clauses(SchemaPairs)
    ;   []
    ).

root_clause([Server|_]) -->
    { uri_components(Server.url, Components),
      uri_data(path, Components, Root)
    },
    [ openapi_root(Root) ].

path_clauses([]) --> [].
path_clauses([H|T]) --> path_clause(H), path_clauses(T).

path_clause(Path-Spec) -->
    { dict_pairs(Spec, _, Methods) },
    path_handlers(Methods, Path).

path_handlers([], _Path) --> [].
path_handlers([Method-Spec|T], Path) -->
    { path_handler(Path, Method, Spec, PathList, Request, Content, Result,
                   Handler)
    },
    [ openapi_handler(Method, PathList, Request, Content, Result, Handler) ],
    path_handlers(T, Path).

%! path_handler(+Path, +Method, +Spec, -PathList, -Request, -Content,
%!		?Result, -Handler) is det.

path_handler(Path, Method, Spec, PathList, Request, Content, Result,
             Handler) :-
    atomic_list_concat(Parts, '/', Path),
    path_vars(Parts, PathList, PathBindings),
    (   ParamSpecs = Spec.get(parameters)
    ->  parameters(ParamSpecs, PathBindings, Request, Params)
    ;   assertion(PathBindings == []),          % TBD: Proper message
        Params = [],
        Request = []
    ),
    content_parameter(Method, Spec, Content, Params, Params1),
    append(Params1, [Result], AllParams),
    atom_string(PredName, Spec.operationId),
    Handler =.. [PredName|AllParams].

parameters([], _, [], []).
parameters([H|T], PathB, [R0|Req], [P0|Ps]) :-
    _{name:NameS, in:"query"} :< H,
    !,
    phrase(http_param_options(H), Opts),
    atom_string(Name, NameS),
    R0 =.. [Name,P0,Opts],
    parameters(T, PathB, Req, Ps).
parameters([H|T], PathB, Req, [P0|Ps]) :-
    _{name:NameS, in:"path"} :< H,
    !,
    atom_string(Name, NameS),
    (   memberchk(Name=P0, PathB)
    ->  true                                    % TBD: type conversion
    ;   existence_error(path_parameter, Name)
    ),
    parameters(T, PathB, Req, Ps).
parameters([H|_], _PathB, _Req, _) :-
    print_message(error, openapi(parameter_failed(H))),
    fail.

http_param_options(Spec) -->
    hp_optional(Spec),
    hp_type(Spec).

hp_optional(Spec) -->
    { Spec.get(required) == false },
    !,
    [optional(true)].
hp_optional(_) --> [].

hp_type(Spec) -->
    hp_schema(Spec.get(schema)),
    !.
hp_type(_) --> [].

hp_schema(Spec) -->
    { _{type:Type,format:Format} :< Spec },
    hp_type_format(Type, Format).

hp_type_format(integer, _) -->
    [ integer ].

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

%! content_parameter(+Method, +Spec, -Content, +Params0, -Params) is
%! det.
%
%   @tbd The post may contain requestBody that specifies the content
%   type and optional schema.

content_parameter(Method, Spec, content(Type, Var), Params, AllParams) :-
    has_content(Method),
    !,
    body_content_type(Spec, Type),
    append(Params, [Var], AllParams).
content_parameter(_, _, -, Params, Params).

has_content(post).
has_content(put).

body_content_type(_Spec, 'application/json').


		 /*******************************
		 *          DISPATCHER		*
		 *******************************/

%!  openapi_dispatch(:Request) is det.
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
    M:openapi_handler(Method, Parts, RequestParams, Content, Result,
                      Handler),
    http_parameters(Request, RequestParams),
    request_body(Content, Request),
    call(M:Handler),
    openapi_reply(Result).

request_body(-, _).
request_body(content('application/json', Body), Request) :-
    http_read_json_dict(Request, Body).

%!  openapi_reply(+Reply) is det.
%
%   Formulate the HTTP request from a term.

openapi_reply(status(Status)) :-
    !,
    format('Status: ~d~n~n', [Status]).
openapi_reply(Result) :-
    reply_json_dict(Result).

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
to_boolean(false, false).
to_boolean(true,  true).
to_boolean(0,     false).
to_boolean(1,     true).
to_boolean(no,    false).
to_boolean(yes,   true).
to_boolean(off,   false).
to_boolean(on,    true).

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

obj_property_in(In, p(Name, Type, true), Out) :-
    json_check(Type, In.Name, Out).
obj_property_in(In, p(Name, Type, false), Out) :-
    (   InV = In.get(Name)
    ->  json_check(Type, InV, Out)
    ;   Out = (-)
    ).

obj_property_out(Out, p(Name, Type, true), In) :-
    json_check(Type, In, Out.Name).
obj_property_out(Out, p(Name, Type, false), In) :-
    (   OutV = Out.get(Name)
    ->  json_check(Type, In, OutV)
    ;   In = (-)
    ).

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

%!  schema_clauses(+Spec)//
%
%   Compile the OpenAPI schema definitions into json_schema/2 clauses.

schema_clauses([]) --> [].
schema_clauses([H|T]) --> schema_clause(H), schema_clauses(T).

schema_clause(Schema-Spec) -->
    { schema_type(Spec, Type) },
    [ openapi:json_schema(Schema, Type) ].

schema_type(Spec, object(Props)) :-
    _{required:ReqS, properties:PropSpecs} :< Spec,
    !,
    dict_pairs(PropSpecs, _, Pairs),
    maplist(atom_string, Req, ReqS),
    maplist(schema_property(Req), Pairs, Props).
schema_type(Spec, array(Type)) :-
    _{type:"array", items:IType} :< Spec,
    !,
    json_type(IType, Type).
schema_type(Spec, Type) :-
    json_type(Spec, Type).

schema_property(Reqs, Name-Spec, p(Name, Type, Req)) :-
    (   memberchk(Name, Reqs)
    ->  Req = true
    ;   Req = false
    ),
    json_type(Spec, Type).

json_type(Spec, Type) :-
    _{type:TypeS, format:FormatS} :< Spec,
    !,
    atom_string(Type0, TypeS),
    atom_string(Format, FormatS),
    once(api_type(_, Type0, Format, Type)).
json_type(Spec, Type) :-
    _{type:TypeS} :< Spec,
    !,
    atom_string(Type0, TypeS),
    once(api_type(_, Type0, -, Type)).
json_type(Spec, url(URL)) :-
    _{'$ref':URLS} :< Spec,
    !,
    atom_concat('#/components/schemas/', URL, URLS).


		 /*******************************
		 *        ENABLE EXPANSION	*
		 *******************************/

:- multifile
    system:term_expansion/2.

system:term_expansion((:- openapi(File, Options)), Clauses) :-
    \+ current_prolog_flag(xref, true),
    expand_openapi(File, Options, Clauses).
