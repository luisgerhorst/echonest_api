-module(echonest_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [].

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(echonest_api),
    [{apps_started, Started}|Config].

end_per_suite(Config) ->
    lists:map(fun application:stop/1, lists:reverse(?config(apps_started, Config))).

%% tasteprofiles_exist() ->
%%     [{timetrap, {seconds,10}}].

%% tasteprofiles_exist(_Config) ->
%%     {ok, #{<<"total">> := Count,
%%            <<"catalogs">> := TPs}} = list_tasteprofiles(),
%%     if Count > 0 ->
%%             io:format("Warning: ~p taste profiles that have not been deleted by their creator exist. Will delete them now.", [Count]),
%%             delete_tasteprofiles(TPs),
%%             io:format("Succuessfully deleted all taste profiles.");
%%        true ->
%%             io:format("No taste profiles exist.")
%%     end.

%% %% http://developer.echonest.com/docs/v4/tasteprofile.html
%% -define(EchonestTasteprofilesPerKeyLimit, 1000).

%% list_tasteprofiles() ->
%%     echonest_api:get("tasteprofile/list", [{<<"results">>, ?EchonestTasteprofilesPerKeyLimit}]).

%% delete_tasteprofiles(TPs) ->
%%     Delete = fun (#{<<"id">> := ID}) -> 
%%                      ct:timetrap({seconds, 5}),
%%                      {ok, _Result} = echonest_api:post("tasteprofile/delete", [{"id", ID}]) 
%%              end,
%%     lists:foreach(Delete, TPs).










