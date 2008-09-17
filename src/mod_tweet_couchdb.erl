%%%-------------------------------------------------------------------
%%% File    : mod_tweet_couchdb.erl
%%% Author  : Stefan Strigler <zeank@jwchat.org>
%%% Description : couchDB adapter
%%%
%%% Created :  9 Sep 2008 by Stefan Strigler <zeank@jwchat.org>
%%%-------------------------------------------------------------------
-module(mod_tweet_couchdb).

-include("mod_tweet.hrl").

-export([
         init_db/1,
         log_tweet/2,
         get_tweets/0,
         get_tweets/1,
         get_tweets/2,
         get_tweets/3,
         get_tweets/4
        ]).

init_db(Opts) ->
    {Host, Port} = gen_mod:get_opt(couch_cfg, Opts, {"localhost", "5984"}),
    ecouch:start(permanent, {Host, Port}).

log_tweet(User, Body) ->
    DBName = couch_escape(User),
    case ecouch:db_info(DBName) of
        {ok,{obj,[{"error",<<"not_found">>},{"reason",<<"missing">>}]}} ->
            ecouch:db_create(DBName);
        _ ->
            ok
    end,
    {MSec, Sec, MuSec} = now(),
    Ts = MSec * 1000000000000 + Sec * 1000000 + MuSec,
    JSONDoc = {obj, [{"Type", <<"tweet">>},
                     {"timestamp", Ts}, 
                     {"body", list_to_binary(Body)}]},
    case ecouch:doc_create(DBName, JSONDoc) of 
        {ok, {obj, [{"error",_}, {"reason", R}]}} ->
            {error, R};
        {ok, {obj, _}} ->
            ok;
        _ ->
            {error, unknown}
    end.

get_tweets() ->
    get_tweets([], null, 0, 0).
get_tweets(User) ->
    get_tweets(User, null, 0, 0).
get_tweets(User, Date) ->
    get_tweets(User, Date, 0, 0).
get_tweets(User, Date, Len) ->
    get_tweets(User, Date, Len, 0).
get_tweets([],Date, Len, Offset) ->
    %% public timeline
    %% unsupported by now
    [];
get_tweets(User, Date, Len, Offset) ->
    lists:foldl(
      fun({obj, Obj}, Acc) ->
              {value, {"value", {obj, Doc}}} = lists:keysearch("value", 1, Obj),
              Acc ++ [make_tweet(User, Doc)]
      end,
      [],
      get_tweet_view_by_timestamp(User, Date, Len, Offset)).

get_tweet_view_by_timestamp(User, Date, Len, Offset) ->
    DBName = couch_escape(User),
    case ecouch:doc_get(DBName, "_view/tweets/by_timestamp") of
        {ok,{obj,[{"error",<<"not_found">>},
                  {"reason",<<"missing_named_view">>}]}} ->
            case create_tweet_view(DBName) of 
                {ok, {obj, Foo}} ->
                    io:format("~p~n", [Foo]),
                    get_tweet_view_by_timestamp(User, Date, Len, Offset);
                _E ->
                    io:format("~p~n", [_E]),
                    []
            end;
        {ok, {obj, Result}} ->
            case lists:keysearch("rows", 1, Result) of
                {value, {"rows", Rows}} ->
                    Rows;
                _ ->
                    []
            end
    end.

create_tweet_view(DBName) ->
    {error, not_implemented}.


couch_escape(List) ->
    %% replace '@' with url escape value of '/'
    lists:flatten(
      lists:map(
        fun(Char) -> 
                case Char of 
                    64 -> 
                        "%2F"; 
                    _ ->  
                        Char 
                end 
        end, 
        List)).

make_tweet(Jid, Doc) ->
    {value, {"body", Body}} = lists:keysearch("body", 1, Doc),
    {value, {"timestamp", Timestamp}} = lists:keysearch("timestamp", 1, Doc),
    CDate = calendar:gregorian_seconds_to_datetime(
              %% 62167219200 is the number of seconds from year 0 to 1970
              round(Timestamp/1000000)+62167219200),
    #tweet{id=Timestamp, jid=Jid, body=binary_to_list(Body), cdate=CDate}.

