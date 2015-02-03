(* ::Package:: *)
(* ::Title:: *)
(*Example(样板包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:GalAster*)
(*Creation Date:2016-12-16*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["CardGames`"];
ShowHardQ::usage = "给出一个梭哈牌局的胜负向量";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Version$CardGames = "V0.1";
Updated$CardGames = "2016-12-16";
CardGames::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*梭哈*)
(* ::Text:: *)
(*Five Card Stud*)
(*Coded by Mr.Wizard*)
SetAttributes[c, Orderless];
a = Union@Differences[List @@ #1[[All, 1]]] == {1}&;
b = {#2, Max @@ #[[All, 1]]}&;
ShowHardRule = {
	w : c[{_, t_}..]?a :> b[w, 9],
	c[{a_, _}.., _] :> {8, a},
	w : c[{a_ | b_, _}..] :> b[w, 7],
	w : c[{_, t_}..] :> b[w, 6],
	w_c?a :> b[w, 5], c[{a_, _}.., _, _] :> {4, a},
	c[{a_ | b_, _}.., _] :> {3, a ~ Max ~ b},
	c[{a_, _}.., _, _, _] :> {2, a}, w_c :> b[w, 1]
};
ShowHardQ[case_] := Block[{num = Length@case / 5, par, rank},
	par = Characters@Partition[case, 5] /. Thread[Characters@"23456789TJQKA" -> Range[2, 14]];
	rank[i_, j_] := If[OrderedQ[Apply[FCS`core, {par[[i]], par[[j]]}, {1}] /. ShowHardRule], j, i];
	UnitVector[num, FoldPair[{rank[#1, #2], rank[#1, #2]}&, Range@num]]
];





(* ::Subsection::Closed:: *)
(*附加设置*)
End[];
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
EndPackage[]
