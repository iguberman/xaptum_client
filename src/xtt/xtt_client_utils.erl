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
-export([
  generate_credentials/3,
  bin_to_hex/1,
  hex_to_bin/1]).
-define(SCRIPT_DIR, "scripts").
-define(CRED_SCRIPT, "generate_multi_daa_creds.sh").

%% TODO the script is for large scale testing and
%% also is good for adhoc manual generation of credentials for whatever purpose
%% for ct it would be nice to have this functionality using ecdaa-erlang library
generate_credentials(Start, End, BaseDir)->
  AppPrivDir = code:priv_dir(xaptum_client),
  CredScript = filename:join([AppPrivDir, ?SCRIPT_DIR, ?CRED_SCRIPT]),
  ExeCmd = CredScript
    ++ " " ++ integer_to_list(Start) ++ " " ++ integer_to_list(End)
    ++ " " ++ BaseDir,
  lager:info("Executing command: ~p", [ExeCmd]),
  os:cmd(ExeCmd).


bin_to_hex(Bin)->
  lists:flatten([[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Bin ]]).

%% From http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html
hex_to_bin(HexBin) when is_binary(HexBin) ->
  hex_to_bin(binary_to_string(HexBin));
hex_to_bin(HexStr) when is_list(HexStr) ->
  hexstr_to_bin(HexStr, []).
hex_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hex_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]);
hex_to_bin([X|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", lists:flatten([X,"0"])),
  hexstr_to_bin(T, [V | Acc]).