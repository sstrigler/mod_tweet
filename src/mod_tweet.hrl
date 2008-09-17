%%%----------------------------------------------------------------------
%%% File    : mod_tweet.hrl
%%% Author  : Stefan Strigler <zeank@jwchat.org>
%%% Purpose : we tweet using (e)jabber(d)
%%% Created : Tue Sep 09 15:08:44 CET 2008
%%%----------------------------------------------------------------------

-define(ITEMS_PER_PAGE, 10).

-record(tweet, {id, jid, body, cdate}).
