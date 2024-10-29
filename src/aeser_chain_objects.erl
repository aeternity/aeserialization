%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Functions for serializing chain objects to binary format.
%%% @end
%%%-------------------------------------------------------------------

-module(aeser_chain_objects).

-export([ serialize/4
        , deserialize/4
        , deserialize_type_and_vsn/1
        ]).

-type template() :: aeserialization:template().
-type fields()   :: aeserialization:fields().

%%%===================================================================
%%% API
%%%===================================================================

-spec serialize(atom(), non_neg_integer(), template(), fields()) -> binary().
serialize(Type, Vsn, Template, Fields) ->
    aeserialization:serialize(tag(Type), Vsn, Template, Fields).

deserialize_type_and_vsn(Binary) ->
    {Tag, Vsn, Fields} = aeserialization:deserialize_tag_and_vsn(Binary),
    {rev_tag(Tag), Vsn, Fields}.

-spec deserialize(atom(), non_neg_integer(), template(), binary()) -> fields().
deserialize(Type, Vsn, Template, Binary) ->
    aeserialization:deserialize(Type, tag(Type), Vsn, Template, Binary).

%%%===================================================================
%%% Internal functions
%%%===================================================================

tag(account) -> 10;
tag(signed_tx) -> 11;
tag(spend_tx) -> 12;
tag(oracle) -> 20;
tag(oracle_query) -> 21;
tag(oracle_register_tx) -> 22;
tag(oracle_query_tx) -> 23;
tag(oracle_response_tx) -> 24;
tag(oracle_extend_tx) -> 25;
tag(name) -> 30;
tag(name_commitment) -> 31;
tag(name_claim_tx) -> 32;
tag(name_preclaim_tx) -> 33;
tag(name_update_tx) -> 34;
tag(name_revoke_tx) -> 35;
tag(name_transfer_tx) -> 36;
tag(name_auction) -> 37;
tag(contract) -> 40;
tag(contract_call) -> 41;
tag(contract_create_tx) -> 42;
tag(contract_call_tx) -> 43;
tag(channel_create_tx) -> 50;
tag(channel_set_delegates_tx) -> 501;
tag(channel_deposit_tx) -> 51;
tag(channel_withdraw_tx) -> 52;
tag(channel_force_progress_tx) -> 521;
tag(channel_close_mutual_tx) -> 53;
tag(channel_close_solo_tx) -> 54;
tag(channel_slash_tx) -> 55;
tag(channel_settle_tx) -> 56;
tag(channel_offchain_tx) -> 57;
tag(channel_offchain_update_transfer) -> 570;
tag(channel_offchain_update_deposit) -> 571;
tag(channel_offchain_update_withdraw) -> 572;
tag(channel_offchain_update_create_contract) -> 573;
tag(channel_offchain_update_call_contract) -> 574;
tag(channel_offchain_update_meta) -> 576;
tag(channel_client_reconnect_tx) -> 575;
tag(channel) -> 58;
tag(channel_snapshot_solo_tx) -> 59;
tag(trees_poi) -> 60;
tag(trees_db) -> 61;
tag(state_trees) -> 62;
tag(mtree) -> 63;
tag(mtree_value) -> 64;
tag(contracts_mtree) -> 621;
tag(calls_mtree) -> 622;
tag(channels_mtree) -> 623;
tag(nameservice_mtree) -> 624;
tag(oracles_mtree) -> 625;
tag(accounts_mtree) -> 626;
tag(compiler_sophia) -> 70;
tag(ga_attach_tx) -> 80;
tag(ga_meta_tx) -> 81;
tag(ga_meta_tx_auth_data) -> 810;
tag(paying_for_tx) -> 82;
tag(hc_vote_tx) -> 83;
tag(key_block) -> 100;
tag(micro_block) -> 101;
tag(light_micro_block) -> 102;
tag(pof) -> 200.

rev_tag(10) -> account;
rev_tag(11) -> signed_tx;
rev_tag(12) -> spend_tx;
rev_tag(20) -> oracle;
rev_tag(21) -> oracle_query;
rev_tag(22) -> oracle_register_tx;
rev_tag(23) -> oracle_query_tx;
rev_tag(24) -> oracle_response_tx;
rev_tag(25) -> oracle_extend_tx;
rev_tag(30) -> name;
rev_tag(31) -> name_commitment;
rev_tag(32) -> name_claim_tx;
rev_tag(33) -> name_preclaim_tx;
rev_tag(34) -> name_update_tx;
rev_tag(35) -> name_revoke_tx;
rev_tag(36) -> name_transfer_tx;
rev_tag(37) -> name_auction;
rev_tag(40) -> contract;
rev_tag(41) -> contract_call;
rev_tag(42) -> contract_create_tx;
rev_tag(43) -> contract_call_tx;
rev_tag(50) -> channel_create_tx;
rev_tag(501) -> channel_set_delegates_tx;
rev_tag(51) -> channel_deposit_tx;
rev_tag(52) -> channel_withdraw_tx;
rev_tag(521) -> channel_force_progress_tx;
rev_tag(53) -> channel_close_mutual_tx;
rev_tag(54) -> channel_close_solo_tx;
rev_tag(55) -> channel_slash_tx;
rev_tag(56) -> channel_settle_tx;
rev_tag(57) -> channel_offchain_tx;
rev_tag(570) -> channel_offchain_update_transfer;
rev_tag(571) -> channel_offchain_update_deposit;
rev_tag(572) -> channel_offchain_update_withdraw;
rev_tag(573) -> channel_offchain_update_create_contract;
rev_tag(574) -> channel_offchain_update_call_contract;
rev_tag(576) -> channel_offchain_update_meta;
rev_tag(575) -> channel_client_reconnect_tx;
rev_tag(58) -> channel;
rev_tag(59) -> channel_snapshot_solo_tx;
rev_tag(60) -> trees_poi;
rev_tag(61) -> trees_db;
rev_tag(62) -> state_trees;
rev_tag(63) -> mtree;
rev_tag(64) -> mtree_value;
rev_tag(621) -> contracts_mtree;
rev_tag(622) -> calls_mtree;
rev_tag(623) -> channels_mtree;
rev_tag(624) -> nameservice_mtree;
rev_tag(625) -> oracles_mtree;
rev_tag(626) -> accounts_mtree;
rev_tag(70) -> compiler_sophia;
rev_tag(80) -> ga_attach_tx;
rev_tag(81) -> ga_meta_tx;
rev_tag(810) -> ga_meta_tx_auth_data;
rev_tag(82) -> paying_for_tx;
rev_tag(83) -> hc_vote_tx;
rev_tag(100) -> key_block;
rev_tag(101) -> micro_block;
rev_tag(102) -> light_micro_block;
rev_tag(200) -> pof.
