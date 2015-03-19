(* ::Package:: *)

(* ::Section:: *)
(*函数说明*)


Poker24::usage = "\:7ecf\:5178\:95ee\:9898,4\:5f20\:724c\:7b9724\:70b9\n
	Poker[pList],\:4f7f\:7528\:5217\:8868pList\:4e2d\:7684\:6570\:5b57\:8ba1\:7b9724\:70b9\n
	\:9009\:9879 Number->24,\:6307\:5b9a\:8ba1\:7b9724\:70b9\n
	\:9009\:9879 Extension->Min,\:5141\:8bb8\:4f7f\:7528\:9636\:4e58,\:5bf9\:6570,\:5f00\:6839\:51d1\:914d\n
	\:9009\:9879 Extension->All,\:5141\:8bb8\:4f7f\:7528\:6240\:6709\:7684\:4e8c\:5143\:8fd0\:7b97\:51d1\:914d\n
";
Begin["`Poker24`"];


(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


Poker24One[__] = "\:8be5\:51fd\:6570\:672a\:5b8c\:6210";
Poker24::memb = "\:8ba1\:7b97 `1` \:7684\:8fc7\:7a0b\:4e2d\:4e0d\:80fd\:542b\:6709 `1` !";
Options[Poker24] = {Number -> 24, Extension -> Off, FindInstance -> False};
Poker24[input_, OptionsPattern[]] := Block[
	{goal, ans},
	goal = OptionValue[Number];
	If[TrueQ@OptionValue[FindInstance], Return@Poker24One[input, goal]];
	If[MemberQ[input, goal], Return@Message[Poker24::memb, goal]];
	ans = Quiet@Switch[OptionValue[Extension],
		Off, Poker24Off[input, goal],
		Min, Poker24Min[input, goal],
		Max, Poker24Max[input, goal],
		__, Poker24Off[input, goal, Rule -> OptionValue[Extension]]
	] /. opsName
];


(* ::Subsection:: *)
(*Main Functions*)


Options[Poker24Off] = {Rule -> {Plus, Subtract, Times, div}};
Poker24Off[nList_List, goal_Integer, OptionsPattern[]] := Block[
	{l = Length@nList, ops, filter, pts, cas, e},
	ops = OptionValue[Rule];
	filter = PokerFilter[l];
	pts = Outer[filter @@ Join[#1, #2]&, Tuples[ops, l - 1], Permutations[nList], 1];
	cas = Cases[pts, e_ /; ReleaseHold@e === goal, {3}];
	DeleteDuplicatesBy[cas, ReleaseHold[# /. Thread[nList -> CharacterRange[97, 96 + l]]]&]
];
Poker24Min[nList_List, goal_Integer] := Block[
	{l = Length@nList, ops, filter, pts, ext, mc},
	ops = {Plus, Subtract, Times, Divide, pow, log, root};
	filter = PokerFilter[l];
	pts = Outer[filter @@ Join[#1, #2]&, Tuples[ops, l - 1], Permutations[nList], 1] /. reduceRule;
	mc[pt_] := MemoryConstrained[Chop[pt - goal // N // ReleaseHold, 10^(-9)], 10^4];
	ext = AbortProtect@Extract[pts, Position[Map[mc, pts, {3}], 0, {3}]];
	DeleteDuplicatesBy[ext, ReleaseHold[# /. Thread[nList -> CharacterRange[97, 96 + l]]]&]
];
Poker24Max[nList_List, goal_Integer] := Block[
	{l = Length@nList, ops, filter, pts, ext, mc},
	ops = {Plus, Subtract, Times, Divide, pow, log, root, FactorialPower, Binomial};
	filter = PokerFilter[l];
	pts = Outer[filter @@ Join[#1, #2]&, Tuples[ops, l - 1], Permutations[nList], 1] /. reduceRule;
	mc[pt_] := MemoryConstrained[Chop[pt - goal // N // ReleaseHold, 10^(-9)], 10^4];
	ext = AbortProtect@Extract[pts, Position[Map[mc, pts, {3}], 0, {3}]];
	DeleteDuplicatesBy[ext, ReleaseHold[# /. Thread[nList -> CharacterRange[97, 96 + l]]]&]
];


(* ::Subsection:: *)
(*Auxiliary Functions*)


div[a_, 0] := ComplexInfinity;
log[a_, b_] := Log[a, b];
pow[a_, b_] := Power[a, b];
root[a_, b_] := pow[a, 1 / b];
reduceRule = log[a_, pow[b_, c_]] :> c + log[a, b];
plus[a_, b_] := Plus[a, b];
minus[a_, b_] := Subtract[a, b];
times[a_, b_] := Times[a, b];
div[a_, b_] := Divide[a, b];
pow[a_, b_] := Power[a, b];
log[a_, b_] := Log[a, b];
root[a_, b_] := pow[a, 1 / b];
opsName = Thread[
	{plus, minus, times, div, pow, log, root, aa, cc} ->
	{Plus, Subtract, Times, Divide, Power, Log, Surd, FactorialPower, Binomial}
];

treeR[1] = n;
treeR[n_] := treeR[n] = Table[o[treeR[a], treeR[n - a]], {a, 1, n - 1}];
treeC[n_] := Flatten[treeR[n] //. {o[a_List, b_] :> (o[#, b]& /@ a), o[a_, b_List] :> (o[a, #]& /@ b)}];
PokerFilter[l_Integer] := Block[
	{nn, oo, ff},
	nn = Array[ToExpression["n" <> ToString@#]&, l];
	oo = Array[ToExpression["o" <> ToString@#]&, l - 1];
	ff = ReplacePart[#, Thread[Position[#, n] -> nn] ~ Join ~ Thread[Position[#, o] -> oo]]&;
	Function[Evaluate@Join[oo, nn], Evaluate[HoldForm /@ Evaluate[ff /@ treeC[l]]]]
];


(* ::Section:: *)
(*附加设置*)
SetAttributes[
	{ },
	{Protected,ReadProtected}
];
End[];
