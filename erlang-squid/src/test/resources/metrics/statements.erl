-module(statements).
sayHello(A) ->

    lists:foreach(fun(Name) -> call(Name, stop) end,
		  checkpoints()), %+1 function call 
    Name = Cp#checkpoint_args.name, % +1 expression statement

    if %+1 if statement
		LocalWriter == true ->
		    {ok, node()}; %+1 expression statement
		Writers /= [] ->
			{ok, hd(Writers)} %+1 another expression statement
	end,

	
	case R#retainer.really_retain of %+1 case statement
		true ->
		    PendingTab = Cp#checkpoint_args.pending_tab, %+1 expression statement
		    case catch ?ets_lookup_element(PendingTab, Tid, 1) of %+1 nested case statement
			{'EXIT', _} ->
			    Store = R#retainer.store %+1 expression statement
		    end;
		false ->
		    ignore %+1 expression statement
	    end,

	try beam_disasm:file(Name) of %+1 try statement
		{error,beam_lib,Reason} -> [{beam_lib,Reason}]; %+1 return statement
		{beam_file,L} ->
		    {value,{code,Code0}} = lists:keysearch(code, 1, L), %+1 expression statement
		    Code = beam_file_1(Code0, []), %+1 expression statement
		    validate(Code) %+1 expression statement
	    catch _:_ -> [disassembly_failed] %+1 catch statement
    end,
    
	begin %+0 begin statement
    	start_servers() %+1 expression statement
	end.
