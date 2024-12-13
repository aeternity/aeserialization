%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity Anstalt
%%% @doc
%%% Module for various Hypoerchain-related encoding tasks
%%% @end
%%%-----------------------------------------------------------------------------

-module(aeser_hc).

-export([encode_parent_pin_payload/1,
         decode_parent_pin_payload/1,
         encode_child_pin_payload/1,
         decode_child_pin_payload/1]).

%%%=============================================================================
%%% Global Definitions
%%%=============================================================================

%%%=============================================================================
%%% Pinning
%%%=============================================================================

%%=============================================================================
%% Encode an Epoch info map to a <80 bytes binary to use in payload when pinning
%% Hyperchain state to the parent chain
%% =============================================================================
-spec encode_parent_pin_payload(#{epoch => integer(), height => integer(), block_hash => binary()}) -> binary().
encode_parent_pin_payload(#{epoch := Epoch, height := Height, block_hash := BlockHash}) ->
    EpochHex = list_to_binary(erlang:integer_to_list(Epoch, 16)),
    HeightHex = list_to_binary(erlang:integer_to_list(Height, 16)),
    EncBlockHash = aeser_api_encoder:encode(key_block_hash, BlockHash),
    <<EpochHex/binary, ":", HeightHex/binary, " ", EncBlockHash/binary>>.

%%=============================================================================
%% Decode a bionary payload to an Epoch info map when fetching pinning info
%% from the parent chain
%% =============================================================================
-spec decode_parent_pin_payload(binary()) ->
    {ok, #{epoch => integer(), height => integer(), block_hash => term()}} | {error, term()}.
decode_parent_pin_payload(Binary) ->
    try
        [HexEpoch, HexHeight, EncBlockHash] = binary:split(Binary, [<<":">>, <<" ">>], [global]),
        Epoch = erlang:list_to_integer(binary_to_list(HexEpoch), 16),
        Height = erlang:list_to_integer(binary_to_list(HexHeight), 16),
        {ok, BlockHash} = aeser_api_encoder:safe_decode(key_block_hash, EncBlockHash),
        {ok, #{epoch => Epoch, height => Height, block_hash => BlockHash}}
    catch
        _ -> {error, {bad_parent_pin_payload, Binary}}
    end.

%%=============================================================================
%% Encode the transaction hash returned when pinning to the parent chain to be
%% easily findable when notarized (in a spend tx to the leader of the last
%% epoch block) to the hyper chain.
%%=============================================================================
encode_child_pin_payload(TxHash) ->
    <<"pin", TxHash/binary>>.

%%=============================================================================
%% Decode a pinning transacation notarization payload
%% =============================================================================
decode_child_pin_payload(<<"pin", TxHash/binary>>) ->
    {ok, TxHash};
decode_child_pin_payload(BadTxHash) ->
    {error, {bad_child_pin_payload, BadTxHash}}.
