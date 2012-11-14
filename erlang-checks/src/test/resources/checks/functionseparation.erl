-module(noemacsstyle).
-export(hello/0).

hello()->
	Code;
hello(A) ->
    Code.
bello()-> %issue
    code;
bello(A) ->
    {A, code}.

%not issue
%not issue
hello2()->
	Code;

hello2(A) -> %issue
    Code.


bello2()-> %issue
    code;
%not issue
bello2(A) ->
    {A, code}.
