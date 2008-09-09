%%%----------------------------------------------------------------------
%%% File    : ejabberd_http.hrl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  4 Mar 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id: ejabberd_http.hrl 498 2006-02-03 03:28:15Z alexey $
%%%----------------------------------------------------------------------

-record(request, {method,
		  path,
		  q = [],
		  us,
		  auth,
		  lang = "",
		  data = "",
                  ip
		 }).


