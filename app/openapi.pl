/*  Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2024, SWI-Prolog Solutions b.v.
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

:- use_module(library(main)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(openapi)).
:- use_module(library(edinburgh)).
:- use_module(library(gui_tracer)).

:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, [File], Options),
    (   option(client(_), Options)
    ;   option(server(_), Options)
    ), !,
    load_hook(Options),
    setup_debug(Options),
    generate(File, Options).
main(_Argv) :-
    usage,
    halt(1).

generate(File, Options) =>
    gen_client(File, Options),
    gen_server(File, Options).

gen_client(File, Options) :-
    option(client(Client), Options), !,
    output_options(client(Client), File, Out),
    append(Options, Out, GenOptions),
    openapi_doc(File, client, GenOptions).
gen_client(_, _).

gen_server(File, Options) :-
    option(server(Server), Options), !,
    output_options(server(Server), File, Out),
    append(Options, Out, GenOptions),
    openapi_doc(File, server, GenOptions).
gen_server(_, _).

output_options(client(-), _, []) :-
    !.
output_options(server(-), _, []) :-
    !.
output_options(client(true), From, [file(Out)]) :-
    !,
    file_name_extension(Base, _, From),
    atomic_list_concat([Base, '_client.pl'], Out).
output_options(server(true), From, [file(Out)]) :-
    !,
    file_name_extension(Base, _, From),
    atomic_list_concat([Base, '_server.pl'], Out).
output_options(client(File), _, [file(File)]).
output_options(server(File), _, [file(File)]).

load_hook(Options) :-
    forall(member(hook(File), Options),
           ensure_loaded(user:File)).

setup_debug(Options) :-
    forall(member(spy(Spy), Options),
           gspy(Spy)),
    (   option(trace(true), Options)
    ->  gtrace
    ;   true
    ),
    (   option(trap(true), Options)
    ->  gtrap(_)
    ;   true
    ).

		 /*******************************
		 *      COMMANDLINE OPTIONS	*
		 *******************************/

opt_type(client,              client,              (boolean|file(write))).
opt_type(server,              server,              (boolean|file(write))).
opt_type(type_check_request,  type_check_request,  boolean).
opt_type(type_check_response, type_check_response, boolean).
opt_type(format_response,     format_response,     boolean).
opt_type(enum_case_sensitive, enum_case_sensitive, boolean).
opt_type(enum_case,           enum_case,           oneof([lower,preserve])).
opt_type(module,              module,              (boolean|atom)).
opt_type(server_url,          server_url,          atom).
opt_type(ui,                  ui,                  boolean).
opt_type(http,                http,                boolean).
opt_type(hook,                hook,                file(read)).
opt_type(trap,                trap,                boolean).
opt_type(spy,                 spy,                 term).
opt_type(trace,               trace,              boolean).

opt_help(help(header),
         md("# OpenAPI -- generate REST servers and clients
             The OpenAPI app allows for using \c
             [OpenAPI](https://www.openapis.org/) (Swagger) descriptions \c
             to generate a REST HTTP server or a client for such a server. \c
             The specification may be provided as __JSON__ or __YAML__ file.
	    ")).
opt_help(help(usage),
         " [option ...] OpenAPI-file").

opt_help(client,
         "Generate the client (in FILE, default derived from OpenAPI file)").
opt_help(server,
         "Generate the server (in FILE, default derived from OpenAPI file)").
opt_help(type_check_request,
         "Type check the request body against its JSON schema").
opt_help(type_check_response,
         "Type check the response body against its JSON schema").
opt_help(format_response,
         "Emit the response as nicely formated JSON").
opt_help(enum_case_sensitive,
         "`enum` strings case handling (default `true`)").
opt_help(enum_case,
         "Case for enum constants on the Prolog end (default `preserve`)").
opt_help(module,
         "Generate client code as a module (with NAME, default derived \c
          from the generated file)").
opt_help(server_url,
         "Overrule the `servers` section, claiming given server").
opt_help(ui,
         "Include SWAGGER ui in server").
opt_help(http,
         "Include HTTP server when generating the server").
opt_help(hook,
         "Prolog file to load with customization code").
opt_help(trace,
         "Start the SWI-Prolog debugger").
opt_help(spy,
         "Start the SWI-Prolog debugger when calling predicate").
opt_help(trap,
         "Start the SWI-Prolog debugger on exception").

opt_meta(client,     'FILE').
opt_meta(server,     'FILE').
opt_meta(module,     'NAME').
opt_meta(server_url, 'URL').
opt_meta(spy,        'PREDICATE').

usage :-
    argv_usage(debug).

