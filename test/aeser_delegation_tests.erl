%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aeser_delegation_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aeser_delegation).

-define(ACCOUNT, aeser_id:create(account, <<1:256>>)).
-define(CONTRACT, aeser_id:create(contract, <<2:256>>)).
-define(NAME, aeser_id:create(name, <<3:256>>)).
-define(ORACLE, aeser_id:create(oracle, <<3:256>>)).

-define(NETWORK_ID, <<"my_fancy_network"/utf8>>).

encode_correct_test_() ->
    [{"Encode preclaim sig",
      fun() ->
          aeser_delegation:aens_preclaim_sig(?NETWORK_ID, ?ACCOUNT, ?CONTRACT)
      end},
     {"Encode name sig",
      fun() ->
          aeser_delegation:aens_name_sig(?NETWORK_ID, ?ACCOUNT, ?NAME, ?CONTRACT)
      end},
     {"Encode aens wildcard sig",
      fun() ->
          aeser_delegation:aens_sig(?NETWORK_ID, ?ACCOUNT, ?CONTRACT)
      end},
     {"Encode oracle sig",
      fun() ->
          aeser_delegation:oracle_sig(?NETWORK_ID, ?ACCOUNT, ?CONTRACT)
      end},
     {"Encode oracle response sig",
      fun() ->
          aeser_delegation:oracle_response_sig(?NETWORK_ID, ?ORACLE, ?CONTRACT)
      end}
    ].

encode_fail_test_() ->
    [{"Bad encoding preclaim sig",
      fun() ->
          ?assertError(_, aeser_delegation:aens_preclaim_sig(?NETWORK_ID, <<42:256>>, ?CONTRACT)),
          ?assertError(_, aeser_delegation:aens_preclaim_sig(?NETWORK_ID, ?CONTRACT, ?ACCOUNT))
      end},
     {"Bad encoding name sig",
      fun() ->
          ?assertError(_, aeser_delegation:aens_name_sig(?NETWORK_ID, ?ACCOUNT, <<42:256>>, ?CONTRACT)),
          ?assertError(_, aeser_delegation:aens_name_sig(?NETWORK_ID, ?NAME, ?ACCOUNT, ?CONTRACT))
      end},
     {"Bad encoding aens wildcard sig",
      fun() ->
          ?assertError(_, aeser_delegation:aens_sig(?NETWORK_ID, ?ACCOUNT, <<42:256>>)),
          ?assertError(_, aeser_delegation:aens_sig(?NETWORK_ID, ?CONTRACT, ?CONTRACT))
      end},
     {"Bad encoding oracle sig",
      fun() ->
          ?assertError(_, aeser_delegation:oracle_sig(?NETWORK_ID, <<42:256>>, ?CONTRACT)),
          ?assertError(_, aeser_delegation:oracle_sig(?NETWORK_ID, ?ACCOUNT, ?ACCOUNT))
      end},
     {"Bad encoding oracle response sig",
      fun() ->
          ?assertError(_, aeser_delegation:oracle_response_sig(?NETWORK_ID, <<42:256>>, ?CONTRACT)),
          ?assertError(_, aeser_delegation:oracle_response_sig(?NETWORK_ID, ?ORACLE, ?ORACLE))
      end}
    ].
