%%%----------------------------------------------------------------------
%%% File    : mod_tweet_mnesia.erl
%%% Author  : Stefan Strigler <zeank@jwchat.org>
%%% Purpose : mnesia db backend for mod_tweet
%%% Created : Tue Sep 09 14:42:29 CET 2008
%%%----------------------------------------------------------------------

-module(mod_tweet_mnesia).
-author('zeank@jwchat.org').
-vsn('0.1'). 

-include("mod_tweet.hrl").
-export([init_db/0,
         log_tweet/2,
         get_tweets/0,
         get_tweets/1,
         get_tweets/2,
         get_tweets/3,
         get_tweets/4
        ]).

-record(sequence, {key, index}).

init_db() ->
    %% create table for all posts
    mnesia:create_table(tweet,
                        [{type, set},
                         {disc_only_copies, [node()]},
                         {attributes, record_info(fields, tweet)}]),
    %% add index to field jid 
    mnesia:add_table_index(tweet, jid),
    %% create sequence table
    mnesia:create_table(sequence, 
                        [{type, set},
                         {disc_copies, [node()]},
                         {attributes, record_info(fields, sequence)}]).


log_tweet(User,  Body) ->
    PostID = mnesia:dirty_update_counter(sequence, tweet, 1),
    mnesia:dirty_write(
      #tweet{id=PostID,
             jid=User,
             body=Body,
             cdate=erlang:universaltime()}).

get_tweets() ->
    get_tweets([], null, 0, 0).
get_tweets(User) ->
    get_tweets(User, null, 0, 0).
get_tweets(User, Date) ->
    get_tweets(User, Date, 0, 0).
get_tweets(User, Date, Len) ->
    get_tweets(User, Date, Len, 0).

get_tweets([], null, _Len, _Offset) ->
    case mnesia:dirty_read(sequence, tweet) of
        [{sequence, tweet, LastID} | _] ->
            lists:foldl(
              fun(ID, List) ->
                      List ++ 
                          mnesia:dirty_read(tweet, ID)
              end,
              [],
              lists:seq((LastID-?ITEMS_PER_PAGE*10)+1, LastID));
        [] ->
            []
    end;
get_tweets(User, {Year, Month, Day}, _Len, _Offset) ->
    mnesia:dirty_match_object(
      {tweet, '_', User, '_', '_', {{Year, Month, Day}, '_'}});
get_tweets(User, _, _Len, _Offset) ->
    mnesia:dirty_index_read(tweet, User, jid).
    
