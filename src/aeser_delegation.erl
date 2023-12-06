%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, Aeternity Anstalt
%%% @doc
%%% Serialization of delegation signatures
%%% @end
%%%-------------------------------------------------------------------
-module(aeser_delegation).

-export([ aens_preclaim_sig/3
        , aens_name_sig/4
        , aens_sig/3

        , oracle_sig/3
        , oracle_response_sig/3
        ]).

%% Delegation signatures are prefixed with a unique tag, ensuring not to
%% collide with serialized transactions.
-define(DELEGATION_TAG, 16#1a01).

-define(TYPE_AENS, 1).
-define(TYPE_AENS_NAME, 2).
-define(TYPE_AENS_PRECLAIM, 3).
-define(TYPE_ORACLE, 4).
-define(TYPE_ORACLE_RESPONSE, 5).

-define(VSN, 1).

-type sig_data() :: binary().

-spec aens_preclaim_sig(binary(), aeser_id:id(), aeser_id:id()) -> sig_data().
aens_preclaim_sig(NetworkId, Account, Contract) ->
    assert_id(account, Account),
    assert_id(contract, Contract),
    Template = [{account, id}, {contract, id}],
    Fields   = [{account, Account}, {contract, Contract}],
    serialize(?TYPE_AENS_PRECLAIM, NetworkId, Template, Fields).

-spec aens_name_sig(binary(), aeser_id:id(), aeser_id:id(), aeser_id:id()) -> sig_data().
aens_name_sig(NetworkId, Account, Name, Contract) ->
    assert_id(account, Account),
    assert_id(name, Name),
    assert_id(contract, Contract),
    Template = [{account, id}, {name, id}, {contract, id}],
    Fields   = [{account, Account}, {name, Name}, {contract, Contract}],
    serialize(?TYPE_AENS_NAME, NetworkId, Template, Fields).

-spec aens_sig(binary(), aeser_id:id(), aeser_id:id()) -> sig_data().
aens_sig(NetworkId, Account, Contract) ->
    assert_id(account, Account),
    assert_id(contract, Contract),
    Template = [{account, id}, {contract, id}],
    Fields   = [{account, Account}, {contract, Contract}],
    serialize(?TYPE_AENS, NetworkId, Template, Fields).

-spec oracle_sig(binary(), aeser_id:id(), aeser_id:id()) -> sig_data().
oracle_sig(NetworkId, Account, Contract) ->
    assert_id(account, Account),
    assert_id(contract, Contract),
    Template = [{account, id}, {contract, id}],
    Fields   = [{account, Account}, {contract, Contract}],
    serialize(?TYPE_ORACLE, NetworkId, Template, Fields).

-spec oracle_response_sig(binary(), aeser_id:id(), aeser_id:id()) -> sig_data().
oracle_response_sig(NetworkId, QueryId, Contract) ->
    assert_id(oracle, QueryId),
    assert_id(contract, Contract),
    Template = [{query, id}, {contract, id}],
    Fields   = [{query, QueryId}, {contract, Contract}],
    serialize(?TYPE_ORACLE_RESPONSE, NetworkId, Template, Fields).

%% ------------------------------------------------------------------------
%% -- Internal functions
%% ------------------------------------------------------------------------

serialize(Type, NetworkId, Template, Fields) ->
    Data = aeserialization:serialize(Type, ?VSN, Template, Fields),
    <<?DELEGATION_TAG:16, NetworkId/binary, Data/binary>>.

assert_id(Type, AeserId) ->
    Type = aeser_id:specialize_type(AeserId).
