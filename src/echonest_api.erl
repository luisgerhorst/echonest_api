-module(echonest_api).

-define(MINUTE, 60*1000). %% ms

%% Request API
-export([post/2, get/2, % Plain.
         read_tasteprofile/2, wait_for_ticket_to_finish/1]). % High level.
%% For Supervisor
-export([start_link/0]).
%% Gen Rate Limiter
-behaviour(gen_rate_limiter).
-export([init/1, before_run/1, run/2, to_wait/1]).

-define(APPLICATION, echonest_api).

%% ===================================================================
%% API
%% ===================================================================

post(Path, Data) ->
    req_in_spot(post, Path, Data).

get(Path, Data) ->
    req_in_spot(get, Path, Data).

%% ===================================================================
%% Higher level API
%% ===================================================================

-define(ReadTasteProfileChunkSize, 300).

read_tasteprofile(TP, Params) ->
	read_tasteprofile(TP, Params, [], 0).

read_tasteprofile(TP, Params, OldItems, N) ->
	ChunkSize = ?ReadTasteProfileChunkSize,
	Data = [{"id", TP},
			{"start", N*ChunkSize},
			{"results", ChunkSize} | Params],
	{ok, #{<<"catalog">> := #{<<"items">> := ReceivedItems,
                              <<"total">> := TotalItems}}} = get("tasteprofile/read", Data),
	Items = ReceivedItems++OldItems,
	if length(Items) =:= TotalItems -> Items;
	   true -> read_tasteprofile(TP, Params, Items, N+1) end.

-define(WaitForTicketFinishRefreshDelay, 1000).

wait_for_ticket_to_finish(Ticket) ->
	{ok, #{<<"ticket_status">> := Status}} = get("tasteprofile/status", [{<<"ticket">>, Ticket}]),
	case Status of
		<<"complete">> ->
			ok;
		<<"pending">> ->
			timer:sleep(?WaitForTicketFinishRefreshDelay),
			wait_for_ticket_to_finish(Ticket)
	end.

%% ===================================================================
%% Rate Limiter
%% ===================================================================

start_link() ->
    gen_rate_limiter:start_link(?MODULE, undefined).

req_in_spot(Type, Path, Data) ->
    gen_rate_limiter:run(?MODULE, {Type, Path, Data}).

init(_State) ->
    register(?MODULE, self()),
    20. % Initial fallback rate limit.

before_run(RateLimit) ->
    {timestamp_ms(), RateLimit}.

run({Type, Path, Data}, {Started, _RecentRateLimit}) ->
    {ok, Return, RateLimit} = req(Type, Path, Data),
    {Return, {Started, RateLimit}}.

to_wait({Started, RateLimit}) ->
    {round(?MINUTE / RateLimit) - (timestamp_ms() - Started), RateLimit}.

timestamp_ms() ->
    {Mega, Sec, Micro} = now(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%% ===================================================================
%% Request
%% ===================================================================

req(Type, Path, Data) ->
    %% Because a failing request should not crash the rate limiter, this is
    %% executed in the calling process. That's why we have to manually specify
    %% the application.
    {ok, APIKey} = application:get_env(?APPLICATION, echonest_api_key), 
    DataStr = uri_params_encode([{"api_key", APIKey} | Data]),
	URL = "http://developer.echonest.com/api/v4/"++Path,
	Req = request_arg(Type, URL, DataStr),
    {ok, Response} = httpc:request(Type, Req, [], []),
    {{_HTTPVersion, Code, _State}, Head, Body} = Response,
    {RateLimit, _Rest} = string:to_integer(proplists:get_value("x-ratelimit-limit", Head)),
    case Code of
		200 ->
            #{<<"response">> := EchonestRes} = jiffy:decode(Body, [return_maps]),
		    {ok, {ok, EchonestRes}, RateLimit};
		%% Rate limit exceed, very uncommon but could happen if rate limit is
        %% reduced or if someone fails after performing a request but before
        %% returning the rate limit to echonest_api.
		429 ->
			%% Wait a little bit ... (there's nothing special about the choosen
            %% time, just wait a little longer if rate limit is smaller) 
			{RateLimit,_} = string:to_integer(proplists:get_value("x-ratelimit-limit", Head)),
		    timer:sleep(round(?MINUTE / RateLimit)),
			%% ... and retry.
			req(Type, Path, Data);
        _Other ->
            {RateLimit,_} = string:to_integer(proplists:get_value("x-ratelimit-limit", Head)),
            {ok, {error, Response}, RateLimit}
    end.

request_arg(post, URL, DataStr) ->
    {URL, [], "application/x-www-form-urlencoded", DataStr};
request_arg(get, URL, DataStr) ->
    {URL++"?"++DataStr, []}.

%% ===================================================================
%% Encode URI Params
%% ===================================================================

uri_params_encode(Params) ->
    intercalate("&", [uri_join([K, V], "=") || {K, V} <- Params]).

intercalate(Sep, Xs) ->
    lists:concat(intersperse(Sep, Xs)).

intersperse(_, []) ->
    [];
intersperse(_, [X]) ->
    [X];
intersperse(Sep, [X | Xs]) ->
    [X, Sep | intersperse(Sep, Xs)].

uri_join(Values, Separator) ->
    string:join(lists:map(fun encode/1, Values), Separator).

encode(Binary) when is_binary(Binary) ->
    http_uri:encode(binary_to_list(Binary));
encode(List) when is_list(List) ->
    http_uri:encode(List).
