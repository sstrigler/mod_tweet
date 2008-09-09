%%%----------------------------------------------------------------------
%%% File    : mod_tweet.erl
%%% Author  : Stefan Strigler <zeank@jwchat.org>
%%% Purpose : we tweet using (e)jabber(d)
%%% Created : Fri Feb 20 13:15:52 CET 2007
%%% Id      : 
%%%----------------------------------------------------------------------

-module(mod_tweet).
-author('zeank@jwchat.org').
-vsn('0.1'). 

-behaviour(gen_mod).
-behaviour(gen_server).

-include("mod_tweet.hrl").

%% gen_mod API
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% ejabberd http API
-export([process/2]).

-define(ejabberd_debug, true).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

-record(state, {host, db_mod}).

-define(PROCNAME, ?MODULE).

%%%----------------------------------------------------------------------
%%% gen_mod API
%%%----------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [Host, Opts]},
	 temporary,
	 1000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    ets:delete(mod_tweet_cfg),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    MyHost = gen_mod:get_opt(host, Opts, "tweet." ++ Host),
    Db_Mod = gen_mod:get_opt(db_mod, Opts, mod_tweet_mnesia),
    Db_Mod:init_db(),
    case catch ets:new(mod_tweet_cfg, [named_table, public]) of
        _ ->
            ok
    end,
    case gen_mod:get_opt(css_path, Opts, undefined) of
        undefined ->
            ok;
        CssPath ->
            ets:insert(mod_tweet_cfg, {css_path, CssPath})
    end,
    ets:insert(mod_tweet_cfg, {db_mod, Db_Mod}),
    ejabberd_router:register_route(MyHost),
    {ok, #state{host = MyHost, db_mod = Db_Mod}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, _To, 
             {xmlelement, "message", _, _}=P}, State) ->
    User = jlib:jid_to_string(
             jlib:jid_tolower(
               jlib:jid_remove_resource(From))),
    Subject = xml:get_path_s(P, [{elem, "subject"}, cdata]),
    Body= xml:get_path_s(P, [{elem, "body"}, cdata]),
    Db_Mod = State#state.db_mod,
    Db_Mod:log_tweet(User,
                     Subject,
                     Body),
    {noreply, State};
handle_info({route, From, To, {xmlelement, "iq", Attrs, _}=IQ}, State) ->
    ?DEBUG("got iq from ~s", [jlib:jid_to_string(From)]),

    %% TODO
    %% * store personal information (like real name)
    %% * allow to delete/edit an entry

    case xml:get_attr_s("type", Attrs) of
        "get" ->
            handle_iq_get({From, To, IQ}, State);
        "set" ->
            ejabberd_router:route(
              To, From, 
              jlib:make_error_reply(
                IQ, 
                ?ERR_FEATURE_NOT_IMPLEMENTED));
        _ ->
            nok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ejabberd_router:unregister_route(State#state.host),
    %% dirty fix

    %% if fixed hostname is set an instance of this module is launched
    %% for each vhost for some odd reason. so there is a route
    %% registered for each instance but with some
    %% hostname. unregister_route is called for the current process
    %% only thus deletes only one of numerous routes. so we're
    %% deleting all routes manually.

    mnesia:dirty_delete(route, State#state.host),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% ejabberd http API
%%--------------------------------------------------------------------

%% TODO
%% * search for dates

process(_, #request{method = 'GET',
                    path = Path,
                    q = Query,
                    data = []}=_R) ->
    ?DEBUG("got request: ~p", [_R]),
    Heading = "Ejabberd " ++ atom_to_list(?MODULE),
    [Prefix|_] = Path,
    {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"}],
     [{xmlelement, "head", [],
       [{xmlelement, "title", [], [{xmlcdata, Heading}]},
        case ets:lookup(mod_tweet_cfg, css_path) of
            [] ->
                {xmlcdata, ""};
            [{css_path, CssPath}] ->
                {xmlelement, "link", 
                 [{"rel", "stylesheet"},
                  {"href", CssPath}], []}
        end
       ]},
      {xmlelement, "body", [],
       [{xmlelement, "h1", [], 
         [{xmlelement, "a", [{"href", "/"++Prefix++"/"}],
           [
            {xmlcdata, Heading}
           ]}
         ]},
        serve(Path, Query)
        
       ]}]};
process(_, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, [], {xmlelement, "h1", [],
	       [{xmlcdata, "400 Bad Request"}]}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
serve([Prefix | []], Query) ->
    Db_Mod = get_db_mod(),
    EList = Db_Mod:get_posts([],[]),
    {xmlelement, "div", [{"class", "page"}], 
     [{xmlelement, "h2", [], 
       [{xmlcdata, "All Posts"}]},
      xml_tweets(EList, Prefix, Query)
     ]};
serve([_Prefix, "about", JID | _], _Query) ->
    %% TODO - get vCard information for user
    %% somehow need to deal with asynchronous nature of vCard requests
    %% ideas anyone?
    {xmlelement, "div", [{"class", "page"}], 
     [{xmlelement, "h2", [], 
       [{xmlcdata, "About "++JID}]}
     ]};
serve([Prefix, JID | []], Query) ->
    Db_Mod = get_db_mod(),
    Posts = Db_Mod:get_posts(JID, []),
    {xmlelement, "div", [{"class", "page"}], 
     [{xmlelement, "h2", [],
       [{xmlcdata, "Posts by "},
        {xmlelement, "a", [{"href","/"++Prefix++"/"++JID}],
         [{xmlcdata, JID}]}]},
      xml_tweets(Posts, Prefix, Query)
     ]};
serve([Prefix, JID | DatePath], Query) ->
    [{IYear, Year}, {IMonth, Month}, {IDay, Day} | _] = 
        lists:foldl(
          fun(El, IntList) ->
                  case catch list_to_integer(El) of
                      {'EXIT', _Reason} ->
                          IntList ++ [{'_',""}]; %% use wildcard
                      Int ->
                          IntList ++ [{Int,El}]
                  end
          end,
          [], 
          %% make sure we do have at least 3 elements in list
          DatePath++["a","b"]), 
    Db_Mod = get_db_mod(),
    Posts = Db_Mod:get_posts(JID, {IYear, IMonth, IDay}),
    {xmlelement, "div", [{"class", "page"}], 
     [{xmlelement, "h2", [],
       [{xmlcdata, "Posts by "},
        {xmlelement, "a", [{"href","/"++Prefix++"/"++JID}],
        [{xmlcdata, JID}]},
        {xmlcdata, " on "++Year++"-"++Month++"-"++Day}]},
      xml_tweets(Posts, Prefix, Query)
     ]}.

xml_tweets(EList, Prefix, Query) ->
    SortedList = 
        lists:sort(
          fun(E1, E2) ->
                  E1#tweet.id > E2#tweet.id
          end,
          EList),
    {ChoppedList, PrevPage, NextPage} = 
        case lists:keysearch("page",1, Query) of 
            false ->
                {lists:sublist(SortedList, 1, ?ITEMS_PER_PAGE), 0, 2};
            {value, {"page", Page}} ->
                %% convert Page to int and make sure we're non negative
                PageNum = case catch list_to_integer(Page) of
                              {'EXIT', _} ->
                                  1;
                              IPage ->
                                  lists:max([1,IPage])
                          end,
                Start = lists:min(
                          [(((PageNum-1)*?ITEMS_PER_PAGE)+1),
                           length(SortedList)+1]),
                {lists:sublist(SortedList, 
                               Start,
                               ?ITEMS_PER_PAGE),
                 PageNum-1,
                 PageNum+1}
        end,
    MaxPage = (length(SortedList) div ?ITEMS_PER_PAGE)+
        case length(SortedList) rem ?ITEMS_PER_PAGE of
            0 ->
                0;
            _ ->
                1
        end,
    Pager = [{xmlelement, "div", [{"id", "pager"}],
      [
       if 
           PrevPage > 0 ->
               {xmlelement, "a", 
                [{"href", "?page="++integer_to_list(PrevPage)},
                 {"id", "page_prev"},
                 {"title", "Previous Page"}],
                [{xmlcdata, "\302\253"}]};
           true ->
               {xmlcdata, "\302\253"}
       end,
       {xmlcdata, " "},
       if
           NextPage =< MaxPage ->
               {xmlelement, "a", 
                [{"href", "?page="++integer_to_list(NextPage)},
                 {"id", "page_next"},
                 {"title", "Next Page"}],
                [{xmlcdata, "\302\273"}]};
           true ->
               {xmlcdata, "\302\273"}
       end
      ]}],
    {xmlelement, "div", [{"id", "tweets"}], 
      Pager ++
     lists:foldl(
       fun(E, List) ->
               List ++ [xml_tweet(E, Prefix)]
       end,
       [],
       ChoppedList) ++ Pager
    }.

xml_tweet(E, Prefix) ->
    {{Year, Month, Day},_} = E#tweet.cdate,
    {xmlelement, "div", [{"class", "post"}],
     [{xmlelement, "h3", [{"class", "post_title"}], 
       [{xmlcdata, E#tweet.subject}]},
      {xmlelement, "div", [{"class", "post_content"}], 
       [{xmlcdata, E#tweet.body}]},
      {xmlelement, "div", [{"class", "post_meta"}],
       [{xmlcdata, "Posted by "},
        {xmlelement, "a", [{"href", "/"++Prefix++"/"++E#tweet.jid}], 
         [{xmlcdata, E#tweet.jid}]},
        {xmlcdata, " on "},
        {xmlelement, "a", 
         [{"href", "/"++Prefix++"/"++E#tweet.jid++
           "/"++integer_to_list(Year)++
           "/"++integer_to_list(Month)++
           "/"++integer_to_list(Day)++"/"}],
         [{xmlcdata, timestamp_to_iso_date(E#tweet.cdate)}]},
        {xmlcdata, " " ++ timestamp_to_iso_time(E#tweet.cdate)}]}
     ]}.

timestamp_to_iso_date({{Year, Month, Day}, _}) ->
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0w",
		    [Year, Month, Day])).

timestamp_to_iso_time({_, {Hour, Minute, Second}}) ->
    lists:flatten(
      io_lib:format("~2..0w:~2..0w:~2..0w",
		    [Hour, Minute, Second])).


handle_iq_get({From, To, IQ}, State) ->
    case jlib:get_iq_namespace(IQ) of 
        ?NS_DISCO_INFO ->
            {xmlelement, "iq", Attrs, _} = jlib:make_result_iq_reply(IQ),
            case jlib:iq_query_info(IQ) of
                #iq{sub_el = {xmlelement, "query", SubAttrs, _}} ->
                    Identity = {xmlelement, "identity", 
                                [{"category", "component"},
                                 {"name", "tweet service"},
                                 {"type", "log"}], []},
                    Feature1 = {xmlelement, "feature",
                               [{"var", ?NS_VERSION}], []},
                    Feature2 = {xmlelement, "feature",
                               [{"var", ?NS_VCARD}], []},
                    NewSubEl = {xmlelement, "query", SubAttrs, 
                                [Identity, Feature1, Feature2]},
                    ejabberd_router:route(
                      To, From,
                      {xmlelement, "iq", Attrs, [NewSubEl]});
                _ ->
                    none
            end;
        ?NS_DISCO_ITEMS ->
            ejabberd_router:route(
              To, From,
              jlib:make_result_iq_reply(IQ));
        ?NS_VERSION ->
            Reply = jlib:iq_query_info(IQ),
            NameEl = {xmlelement, "name", [], 
                      [{xmlcdata, atom_to_list(?MODULE)}]},
            VersionEl = case beam_lib:version(?MODULE) of 
                            {ok, {?MODULE, [VSN]}} ->
                                {xmlelement, "version", [],
                                 [{xmlcdata, atom_to_list(VSN)}]};
                            _ ->
                                {xmlelement, "version", [],
                                 [{xmlcdata, "unknown"}]}
                        end,
            OsEl = {xmlelement, "os", [], 
                    [{xmlcdata, erlang:system_info(system_version)}]},
            SubEl = {xmlelement, "query", [{"xmlns",?NS_VERSION}], 
                     [NameEl, VersionEl, OsEl]},
            ejabberd_router:route(
              To, From, jlib:make_result_iq_reply(
                          jlib:iq_to_xml(Reply#iq{sub_el=[SubEl]})));
        ?NS_VCARD ->
            {xmlelement, _, RAttrs, _} = jlib:make_result_iq_reply(IQ),
            FN = {xmlelement, "FN", [], 
                  [{xmlcdata, "mod_tweet"}]},
            URL = {xmlelement, "URL", [],
                   [{xmlcdata, "http://jabber.org/"}]},
            JID = {xmlelement, "JABBERID", [],
                   [{xmlcdata, State#state.host}]},
            DESC = {xmlelement, "DESC", [],
                    [{xmlcdata, "your personal microblogging service"}]},
            VCard = {xmlelement, "vCard", [{"xmlns", ?NS_VCARD}], 
                     [FN, URL, JID, DESC]},
            Reply = {xmlelement, "iq", RAttrs, [VCard]},
            ejabberd_router:route(To, From, Reply);
        _ ->
            ejabberd_router:route(
              To, From, 
              jlib:make_error_reply(
                IQ, 
                ?ERR_FEATURE_NOT_IMPLEMENTED))
    end.

get_db_mod() ->
    case ets:lookup(mod_tweet_cfg, db_mod) of
        [] ->
            mod_tweets_mnesia;
        [{db_mod, Db_Mod}] ->
            Db_Mod
    end.
