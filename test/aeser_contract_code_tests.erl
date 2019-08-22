-module(aeser_contract_code_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aeser_contract_code.hrl").

-define(DUMMY_CODE_MAP_1,
        #{ byte_code => <<"DUMMY CODE">>
         , type_info => [{<<>>, <<>>, <<>>, <<>>}]
         , contract_source => "contract Foo = ..." }).

-define(DUMMY_CODE_MAP_2,
        #{ byte_code => <<"DUMMY CODE">>
         , type_info => [{<<>>, <<>>, <<>>, <<>>}]
         , compiler_version => <<"3.1.4">>
         , source_hash => <<1, 2, 3, 4>> }).

-define(DUMMY_CODE_MAP_3,
        #{ byte_code => <<"DUMMY CODE">>
         , type_info => [{<<>>, <<>>, false, <<>>, <<>>}]
         , compiler_version => <<"3.1.4">>
         , contract_source => "contract Foo = ..."
         , payable => true} ).

vsn_1_test() ->
    aeser_contract_code:deserialize(
        aeser_contract_code:serialize(?DUMMY_CODE_MAP_1, ?SOPHIA_CONTRACT_VSN_1)).

vsn_2_test() ->
    aeser_contract_code:deserialize(
        aeser_contract_code:serialize(?DUMMY_CODE_MAP_2, ?SOPHIA_CONTRACT_VSN_2)).

vsn_3_test() ->
    aeser_contract_code:deserialize(
        aeser_contract_code:serialize(?DUMMY_CODE_MAP_3, ?SOPHIA_CONTRACT_VSN_3)).

