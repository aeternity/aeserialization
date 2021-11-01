%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% Serialization of contract code
%%% @end
%%%-------------------------------------------------------------------
-module(aeser_contract_code).

-include("aeser_contract_code.hrl").

-export([ deserialize/1
        , serialize/1
        , serialize/2 ]).

-spec serialize(map()) -> binary().
serialize(CodeMap) ->
    serialize(CodeMap, ?SOPHIA_CONTRACT_VSN_3).

-spec serialize(map(), non_neg_integer()) -> binary().
serialize(CodeMap = #{ byte_code := ByteCode
                     , type_info := TypeInfo }, SophiaContractVersion) ->
    %% Source hash
    SourceHash = case CodeMap of
                     #{ source_hash := SHash } -> SHash;
                     #{ contract_source := SrcStr } ->
                         SHash = enacl:generichash(32, list_to_binary(SrcStr)),
                         SHash
                 end,

    %% Compiler version
    Version    = maps:get(compiler_version, CodeMap, <<"unknown">>),
    BinVersion = if is_integer(Version) -> integer_to_binary(Version);
                    is_binary(Version)  -> Version
                 end,

    %% Payable
    Payable = maps:get(payable, CodeMap, true),

    Fields = [ {source_hash, SourceHash}
             , {type_info, TypeInfo}
             , {byte_code, ByteCode} ] ++
             [ {compiler_version, BinVersion}
               || SophiaContractVersion > ?SOPHIA_CONTRACT_VSN_1 ] ++
             [ {payable, Payable}
               || SophiaContractVersion > ?SOPHIA_CONTRACT_VSN_2 ],
    aeser_chain_objects:serialize(compiler_sophia,
                                  SophiaContractVersion,
                                  serialization_template(SophiaContractVersion),
                                  Fields).

-spec deserialize(binary()) -> map().
deserialize(Binary) ->
    case aeser_chain_objects:deserialize_type_and_vsn(Binary) of
        {compiler_sophia = Type, ?SOPHIA_CONTRACT_VSN_1 = Vsn, _Rest} ->
            Template = serialization_template(Vsn),
            [ {source_hash, Hash}
            , {type_info, TypeInfo}
            , {byte_code, ByteCode}
            ] = aeser_chain_objects:deserialize(Type, Vsn, Template, Binary),
            #{ source_hash => Hash
             , type_info => TypeInfo
             , byte_code => ByteCode
             , contract_vsn => Vsn
             , payable => true
             };
        {compiler_sophia = Type, ?SOPHIA_CONTRACT_VSN_2 = Vsn, _Rest} ->
            Template = serialization_template(Vsn),
            [ {source_hash, Hash}
            , {type_info, TypeInfo}
            , {byte_code, ByteCode}
            , {compiler_version, CompilerVersion}
            ] = aeser_chain_objects:deserialize(Type, Vsn, Template, Binary),
            #{ source_hash => Hash
             , type_info => TypeInfo
             , byte_code => ByteCode
             , compiler_version => CompilerVersion
             , contract_vsn => Vsn
             , payable => true
             };
        {compiler_sophia = Type, ?SOPHIA_CONTRACT_VSN_3 = Vsn, _Rest} ->
            Template = serialization_template(Vsn),
            [ {source_hash, Hash}
            , {type_info, TypeInfo}
            , {byte_code, ByteCode}
            , {compiler_version, CompilerVersion}
            , {payable, Payable}
            ] = aeser_chain_objects:deserialize(Type, Vsn, Template, Binary),
            #{ source_hash => Hash
             , type_info => TypeInfo
             , byte_code => ByteCode
             , compiler_version => CompilerVersion
             , contract_vsn => Vsn
             , payable => Payable
             };
        Other ->
            error({illegal_code_object, Other})
    end.

serialization_template(?SOPHIA_CONTRACT_VSN_1) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, binary, binary}]} %% {type hash, name, arg type, out type}
    , {byte_code, binary} ];
serialization_template(?SOPHIA_CONTRACT_VSN_2) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, binary, binary}]} %% {type hash, name, arg type, out type}
    , {byte_code, binary}
    , {compiler_version, binary} ];
serialization_template(?SOPHIA_CONTRACT_VSN_3) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, bool, binary, binary}]} %% {type hash, name, payable, arg type, out type}
    , {byte_code, binary}
    , {compiler_version, binary}
    , {payable, bool} ].

