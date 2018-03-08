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
            openapi_read/2                      % +File, -Term
          ]).
:- use_module(library(debug)).
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
    path_clauses(Paths).

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
    { path_handler(Path, Method, Spec, PathList, Request, Result, Handler) },
    [ openapi_handler(PathList, Request, Result, Handler) ],
    path_handlers(T, Path).

%! path_handler(+Path, +Method, +Spec, -PathList, -Request, ?Result,
%!              -Handler) is det.

path_handler(Path, Method, Spec, PathList, Request, Result, Handler) :-
    assertion(Method == get),
    atomic_list_concat(Parts, '/', Path),
    path_vars(Parts, PathList, PathBindings),
    parameters(Spec.parameters, PathBindings, Request, Params),
    append(Params, Result, AllParams),
    Handler =.. [Spec.operationId|AllParams].

parameters([], _, [], []).
parameters([H|T], PathB, [R0|Req], [P0|Ps]) :-
    _{name:Name, in:query} :< H,
    !,
    phrase(http_param_options(H), Opts),
    R0 =.. [Name,P0,Opts],
    parameters(T, PathB, Req, Ps).
parameters([H|T], PathB, Req, [P0|Ps]) :-
    _{name:Name, in:path} :< H,
    !,
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
    M:openapi_root(Root),
    atom_concat(Root, Path, FullPath),
    atomic_list_concat(Parts, '/', Path),
    M:openapi_handler(Parts, RequestParams, Result, Handler),
    http_parameters(Request, RequestParams),
    call(M:Handler),
    reply_json_dict(Result).



		 /*******************************
		 *           BIND HTTP		*
		 *******************************/


		 /*******************************
		 *            TYPES		*
		 *******************************/


:- multifile
    system:term_expansion/2.

system:term_expansion(openapi(File, Options), Clauses) :-
    expand_openapi(File, Options, Clauses).
