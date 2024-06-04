% This exercise asks you to build a collection of functions that manipulate arithmetical expressions.
% Start with an expression such as the following: ((2+3)-4), 4 and ~((2*3)+(3*4))
%   which is fully bracketed and where you use a tilde (~) for unary minus.
% First, write a parser for these, turning them into Erlang representations, such as the following:
%   {minus, {plus, {num, 2}, {num,3}}, {num, 4}} which represents ((2+3)-4).
%   We call these exp s. Now, write an evaluator, which takes an exp and returns its value.
% You can also extend the collection of expressions to add conditionals:
%   if ((2+3)-4) then 4 else ~((2*3)+(3*4))
%       where the value returned is the “then” value if the “if” expression evaluates to 0
%       and it is the “else” value otherwise.

-module(evaluating).
-export([parse/1, evaluate/1]).
-export([parse_test/0, evaluate_test/0]).
-define(IS_DIGIT(S), (S >= $0 andalso S =< $9)).
-define(IS_SPACE(S), (S =:= $\n orelse S =:= $\r orelse S =:= $\s orelse S =:= $\t)).
-define(IS_OPERATOR(S), (S =:= $+ orelse S =:= $- orelse S =:= $* orelse S =:= $/)).

get_number(Str) -> get_number(Str, []).
get_number([], Acc) ->
    {Acc, []};
get_number([Head | Rest], Acc) when ?IS_DIGIT(Head) ->
    get_number(Rest, Acc ++ [Head]);
get_number([Head | Rest], Acc) ->
    {Acc, [Head | Rest]}.

get_without_spaces([Head | Rest]) when ?IS_SPACE(Head) ->
    get_without_spaces(Rest);
get_without_spaces(Arr) ->
    Arr.

get_operator(Op) ->
    case Op of
        $+ -> plus;
        $- -> minus;
        $* -> multiply;
        $/ -> divide
    end.

parse(Str) ->
    {[Expr], _} = parse(string:tokens(Str, " "), []),
    Expr.

parse([], [Expr]) ->
    Expr;
% parenthesis case
parse([$( | Rest], []) ->
    % io:format("Doing the parenthesis case~n"),
    {[Expr], R} = parse(Rest, []),
    case get_without_spaces(R) of
        [] -> {[Expr], []};
        [$)] -> {[Expr], []};
        [$) | Rest2] -> parse(Rest2, [Expr])
    end;
parse([$)], [Expr]) ->
    {[Expr], []};
% number case
parse(S=[D | _], []) when ?IS_DIGIT(D) ->
    % io:format("Doing the number only case~n"),
    {N, Rest} = get_number(S),
    Num = erlang:list_to_integer(N),
    if
        length(Rest) > 0 ->
            First = lists:nth(1, Rest),
            if
                ?IS_OPERATOR(First) ->
                    parse(Rest, [{num, Num}]);
                true -> {[{num, Num}], Rest}
            end;
        true -> {[{num, Num}], Rest}
    end;
% operator case
parse([Op | Rest], [Left]) when ?IS_OPERATOR(Op) ->
    % io:format("Doing the operator case~n"),
    {[Right], R} = parse(Rest, []),
    {[{get_operator(Op), Left, Right}], R};
% negate case
parse([$~ | Rest], Expr) ->
    % io:format("Doing the negate case~n"),
    {[E], R} = parse(Rest, Expr),
    {[{negate, E}], R};
parse([Head | Rest], _) when is_list(Head) ->
    if
        Head == "if" ->
            [E | Rest1] = Rest,
            {[IfExpr], _} = parse(E, []),
            {[{[ThenExpr], _}], Rest2} = parse(Rest1, []),
            {[{[ElseExpr], _}], _} = parse(Rest2, []),
            {[{if_cond, IfExpr, ThenExpr, ElseExpr}], []};
        Head == "then" ->
            [E | Rest1] = Rest,
            {[parse(E, [])], Rest1};
        Head == "else" ->
            [E | Rest1] = Rest,
            {[parse(E, [])], Rest1};
        true ->
            parse(Head, [])
    end;
% space case
parse([S | Rest], Expr) when ?IS_SPACE(S) ->
    % io:format("Doing the space case~n"),
    parse(get_without_spaces(Rest), Expr).

parse_test() ->
    io:format("((2+3)-4) -> ~w~n", [parse("((2+3)-4)")]),
    io:format("4 -> ~w~n", [parse("4")]),
    io:format("~~((2*3)+(3*4)) -> ~w~n", [parse("~((2*3)+(3*4))")]),
    io:format("~~((20*3)+(12*50)) -> ~w~n", [parse("~((20*3)+(12*50))")]),
    io:format("if ((2+3)-4) then 4 else ~~((2*3)+(3*4)) -> ~w~n", [parse("if ((2+3)-4) then 4 else ~((2*3)+(3*4))")]),
    ok.

evaluate(Expr) ->
    case Expr of
        {negate, Rest} -> -evaluate(Rest);
        {plus, Num1, Num2} -> evaluate(Num1) + evaluate(Num2);
        {minus, Num1, Num2} -> evaluate(Num1) - evaluate(Num2);
        {multiply, Num1, Num2} -> evaluate(Num1) * evaluate(Num2);
        {divide, Num1, Num2} -> evaluate(Num1) / evaluate(Num2);
        {num, Num} -> Num;
        {if_cond, Cond, Then, Else} ->
            If_Val = evaluate(Cond),
            if
                If_Val == 0 -> evaluate(Then);
                true -> evaluate(Else)
            end
    end.

evaluate_test() ->
    io:format("((2+3)-4) -> ~w~n", [evaluate(parse("((2+3)-4)"))]),
    io:format("4 -> ~w~n", [evaluate(parse("4"))]),
    io:format("~~((2*3)+(3*4)) -> ~w~n", [evaluate(parse("~((2*3)+(3*4))"))]),
    io:format("~~((20*3)+(12*50)) -> ~w~n", [evaluate(parse("~((20*3)+(12*50))"))]),
    io:format("if ((2+3)-4) then 4 else ~~((2*3)+(3*4)) -> ~w~n", [evaluate(parse("if ((2+3)-4) then 4 else ~((2*3)+(3*4))"))]),
    ok.