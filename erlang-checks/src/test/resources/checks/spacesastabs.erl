-module(spacesastabs).
     % wrong placed comment, does not matter
    %good
hello(A) -> 
   B  = {world, A}, % not 4
    case A of
        true ->
            no;
         false -> %not 8
           me %not 12
     end, %not 4
	{error}. % this starts with a tab

hallo() ->
        {hallo}. %starts with 8
