%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 14. May 2018 3:25 PM
%%%-------------------------------------------------------------------
-module(xtt_client_utils).
-author("iguberman").

%% API
-export([generate_credentials/3]).
-define(SCRIPT_DIR, "scripts").
-define(CRED_SCRIPT, "generate_multi_daa_creds.sh").

%% TODO the script is for large scale testing and
%% also is good for adhoc manual generation of credentials for whatever purpose
%% for ct it would be nice to have this functionality using ecdaa-erlang library
generate_credentials(Start, End, BaseDir)->
  PrivDir = code:priv_dir(xaptum_client),
  CredScript = filename:join([PrivDir, ?SCRIPT_DIR, ?CRED_SCRIPT]),
  ExeCmd = CredScript ++ " " ++ integer_to_list(Start) ++ " " ++ integer_to_list(End) ++ " " ++ BaseDir,
  lager:info("Executing command: ~p", [ExeCmd]),
  os:cmd(ExeCmd).


