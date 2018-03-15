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

:- module(test_openapi,
          [ test_openapi/0
          ]).
:- use_module(library(plunit)).
:- use_module('../prolog/openapi').

test_openapi :-
    run_tests([ openapi,
                json_schema
              ]).

:- begin_tests(openapi).

:- end_tests(openapi).


:- begin_tests(json_schema).

test(object, Out = _{}) :-
    json_check(object, _{}, Out).
test(object, Out = _{a:hello}) :-
    json_check(object([ p(a, string, true)
                      ]), _{a:"hello"}, Out).
test(object, Out = _{a:hello, b:1}) :-
    json_check(object([ p(a, string, true)
                      ]), _{a:"hello", b:1}, Out).
test(object, Out = _{c:hello, b:1}) :-
    json_check(object([ p(c, string, true)
                      ]), _{c:"hello", b:1}, Out).
test(object, Out = _{c:"hello", b:1}) :-        % Dubious
    json_check(object([ p(d, string, false)
                      ]), _{c:"hello", b:1}, Out).
test(object, Out = _{a:hello}) :-
    json_check(object([ p(a, string, true)
                      ]), json([a=hello]), Out).

json_check(Type, In, Out) :-
    openapi:json_check(Type, In, Out).

:- end_tests(json_schema).
