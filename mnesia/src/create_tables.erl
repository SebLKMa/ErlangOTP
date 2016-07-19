%% @author LMA
%% @doc @todo Add description to create_tables.

-module(create_tables).

%% ====================================================================
%% API functions
%% NOTE:
%% start erl with mnesia path, schema must have already created in dir.
%% erl -mnesia dir '"c:/lma/git/erlangotp/mnesia_storage"' -name mynode
%% ====================================================================
-export([init_tables/0, insert_user/3, insert_project/2]).

-record(user, {id, name}).

-record(project, {title, description}).

-record(contributor, {user_id, project_title}).

init_tables() ->
	% note that record_info/2 is not a real function, it is resolved during compile time.
	% first field of the record is always the primary key.
	mnesia:create_table(user, [{attributes, record_info(fields, user)}]),
	mnesia:create_table(project, [{attributes, record_info(fields, project)}]),
	% type is bag to allow duplicate user_id in contributor table.
	mnesia:create_table(contributor, [{type, bag}, {attributes, record_info(fields, contributor)}]).

insert_user(Id, Name, ProjectTitles) when ProjectTitles =/= [] ->
	User = #user{id = Id, name = Name},
	Fun = fun() ->
			mnesia:write(User),
			lists:foreach(
			  fun(Title) ->
				[#project{title = Title}] = mnesia:read(project, Title),
				mnesia:write(#contributor{user_id = Id, project_title = Title})
			  end,
			  ProjectTitles)
		  end,
	mnesia:transaction(Fun).

insert_project(Title, Description) ->
	% no transaction or db lock, assuming at worst only description is dirty.
	mnesia:dirty_write(#project{title = Title, description = Description}).

%% ====================================================================
%% Internal functions
%% ====================================================================


