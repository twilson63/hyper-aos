-module(aos_authorities_test).
-include_lib("eunit/include/eunit.hrl").

%% Test authorities parsing from process message
authorities_parsing_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Create process message with authorities
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    Authority1 = <<"Auth123456789012345678901234567890123456789">>,
    Authority2 = <<"Auth223456789012345678901234567890123456789">>,
    Authority3 = <<"Auth323456789012345678901234567890123456789">>,
    
    ProcessMsg = #{
        <<"id">> => <<"test-process">>,
        <<"type">> => <<"process">>,
        <<"authority">> => <<Authority1/binary, ",", Authority2/binary, ",", Authority3/binary>>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner
            }
        }
    },
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Verify authorities were parsed correctly by accessing them individually
    {ok, MetaTable} = luerl:get_table([<<"meta">>], LuaState2),
    {ok, Authorities} = luerl:get_table([<<"authorities">>], MetaTable, LuaState2),
    {ok, Auth1} = luerl:get_table([1], Authorities, LuaState2),
    {ok, Auth2} = luerl:get_table([2], Authorities, LuaState2),
    {ok, Auth3} = luerl:get_table([3], Authorities, LuaState2),
    ?assertEqual(Authority1, Auth1),
    ?assertEqual(Authority2, Auth2),
    ?assertEqual(Authority3, Auth3).

%% Test authorities with spaces and invalid entries
authorities_with_spaces_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Create process message with authorities containing spaces and invalid entries
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    Authority1 = <<"Auth123456789012345678901234567890123456789">>,
    Authority2 = <<"Auth223456789012345678901234567890123456789">>,
    
    ProcessMsg = #{
        <<"id">> => <<"test-process">>,
        <<"type">> => <<"process">>,
        %% Include spaces, invalid length entries
        <<"authority">> => <<"  ", Authority1/binary, " , ", Authority2/binary, "  ,InvalidShort,TooLongEntry123456789012345678901234567890123">>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner
            }
        }
    },
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Should only parse the valid 43-character entries
    {ok, MetaTable} = luerl:get_table([<<"meta">>], LuaState2),
    {ok, Authorities} = luerl:get_table([<<"authorities">>], MetaTable, LuaState2),
    {ok, Auth1} = luerl:get_table([1], Authorities, LuaState2),
    {ok, Auth2} = luerl:get_table([2], Authorities, LuaState2),
    {error, nil} = luerl:get_table([3], Authorities, LuaState2),
    ?assertEqual(Authority1, Auth1),
    ?assertEqual(Authority2, Auth2).

%% Test trusted message with matching authority
trusted_message_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize with authorities
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    Authority1 = <<"Auth123456789012345678901234567890123456789">>,
    Authority2 = <<"Auth223456789012345678901234567890123456789">>,
    
    ProcessMsg = #{
        <<"id">> => <<"test-process">>,
        <<"type">> => <<"process">>,
        <<"authority">> => <<Authority1/binary, ",", Authority2/binary>>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner
            }
        }
    },
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Create trusted message
    TrustedMsg = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'trusted message'">>,
        <<"from-process">> => <<"ProcessId123456789012345678901234567890123">>,
        <<"from">> => <<"ProcessId123456789012345678901234567890123">>, % Same as from-process
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Authority1  % Authority is the committer
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(TrustedMsg),
    
    %% Call compute and check the trusted field
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    
    %% Extract the message that was processed
    LuaState3 = element(2, Result),
    {ok, InboxTable} = luerl:get_table([<<"Inbox">>], LuaState3),
    {ok, MsgTable} = luerl:get_table([1], InboxTable, LuaState3),
    {ok, Trusted} = luerl:get_table([<<"trusted">>], MsgTable, LuaState3),
    ?assertEqual(true, Trusted).

%% Test untrusted message - from != from-process
untrusted_from_mismatch_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize with authorities
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    Authority1 = <<"Auth123456789012345678901234567890123456789">>,
    
    ProcessMsg = #{
        <<"id">> => <<"test-process">>,
        <<"type">> => <<"process">>,
        <<"authority">> => Authority1,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner
            }
        }
    },
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Create message where from != from-process
    UntrustedMsg = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'untrusted'">>,
        <<"from-process">> => <<"ProcessId123456789012345678901234567890123">>,
        <<"from">> => <<"DifferentId23456789012345678901234567890123">>, % Different from from-process
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Authority1
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(UntrustedMsg),
    
    %% Call compute and check the trusted field
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    
    %% Extract the message that was processed
    LuaState3 = element(2, Result),
    {ok, InboxTable} = luerl:get_table([<<"Inbox">>], LuaState3),
    {ok, MsgTable} = luerl:get_table([1], InboxTable, LuaState3),
    {ok, Trusted} = luerl:get_table([<<"trusted">>], MsgTable, LuaState3),
    ?assertEqual(false, Trusted).

%% Test untrusted message - committer not in authorities
untrusted_no_authority_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize with authorities
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    Authority1 = <<"Auth123456789012345678901234567890123456789">>,
    
    ProcessMsg = #{
        <<"id">> => <<"test-process">>,
        <<"type">> => <<"process">>,
        <<"authority">> => Authority1,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner
            }
        }
    },
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Create message with non-authority committer
    UntrustedMsg = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'untrusted'">>,
        <<"from-process">> => <<"ProcessId123456789012345678901234567890123">>,
        <<"from">> => <<"ProcessId123456789012345678901234567890123">>, % Same as from-process
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => <<"NotAnAuth234567890123456789012345678901234">>  % Not in authorities
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(UntrustedMsg),
    
    %% Call compute and check the trusted field
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    
    %% Extract the message that was processed
    LuaState3 = element(2, Result),
    {ok, InboxTable} = luerl:get_table([<<"Inbox">>], LuaState3),
    {ok, MsgTable} = luerl:get_table([1], InboxTable, LuaState3),
    {ok, Trusted} = luerl:get_table([<<"trusted">>], MsgTable, LuaState3),
    ?assertEqual(false, Trusted).

%% Test message without from-process is not trusted
no_from_process_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize with authorities
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    Authority1 = <<"Auth123456789012345678901234567890123456789">>,
    
    ProcessMsg = #{
        <<"id">> => <<"test-process">>,
        <<"type">> => <<"process">>,
        <<"authority">> => Authority1,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner
            }
        }
    },
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Create message without from-process
    NoFromProcessMsg = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'no from-process'">>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Authority1
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(NoFromProcessMsg),
    
    %% Call compute - message will get from from commitments but no from-process
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    
    %% Extract the message that was processed
    LuaState3 = element(2, Result),
    {ok, InboxTable} = luerl:get_table([<<"Inbox">>], LuaState3),
    {ok, MsgTable} = luerl:get_table([1], InboxTable, LuaState3),
    {ok, Trusted} = luerl:get_table([<<"trusted">>], MsgTable, LuaState3),
    ?assertEqual(false, Trusted).

%% Test process message without authorities field
no_authorities_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Create process message without authorities
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Verify authorities is empty
    {ok, MetaTable} = luerl:get_table([<<"meta">>], LuaState2),
    {ok, Authorities} = luerl:get_table([<<"authorities">>], MetaTable, LuaState2),
    %% An empty Lua table should look like []
    ?assertEqual([], Authorities).