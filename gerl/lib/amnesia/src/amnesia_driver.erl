%%
%% amnesia_driver.erl
%%
%% ---------------------------------------------------------------------------
%%
%% AMNESIA Copyright (C) 2008-09 Corrado Santoro, Vincenzo Nicosia
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%   - Redistributions of source code must retain the above copyright notice,
%%     this list of conditions and the following disclaimer.
%%
%%   - Redistributions in binary form must reproduce the above copyright
%%     notice, this list of conditions and the following disclaimer in the
%%     documentation and/or other materials provided with the distribution.
%%
%% The name of the author may not be used to endorse or promote products
%% derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
%% NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%% ---------------------------------------------------------------------------
%%
-module (amnesia_driver).

-export ([behaviour_info/1]).

behaviour_info (callbacks) ->
  [{open, 2},
   {open, 3},
   {execute, 2},
   {close, 1},
   {drop_database, 2},
   {create_database, 2},
   {drop_user, 2},
   {create_user, 2},
   {sequence_field, 0},
   {table_create_options, 0},
   {field_option, 2},
   {field_type, 1},
   {insert, 5},
   {update, 6},
   {delete, 3},
   {has_join, 2},
   {begin_transaction, 1},
   {commit, 1},
   {rollback, 1}];
behaviour_info (_) ->
  undefined.

