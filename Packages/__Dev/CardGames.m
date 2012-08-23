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
CardGames$Version="V0.1";
CardGames$Environment="V11.0+";
CardGames$LastUpdate="2016-12-16";
CardGames::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*梭哈*)
Begin["`FCS`"];
(*FCS=Five Card Stud
Coded by Mr.Wizard
Well.....It's so hard to understand.*)
SetAttributes[core,Orderless];
ass1=Union@Differences[List@@#1[[All,1]]]=={1}&;
ass2={#2,Max@@#[[All,1]]}&;
rule={
  w:core[{_,t_}..]?ass1:>ass2[w,9],
  core[{a_,_}..,_]:>{8,a},
  w:core[{a_|b_,_}..]:>ass2[w,7],
  w:core[{_,t_}..]:>ass2[w,6],
  w_core?ass1:>ass2[w,5],core[{a_,_}..,_,_]:>{4,a},
  core[{a_|b_,_}..,_]:>{3,a~Max~b},
  core[{a_,_}..,_,_,_]:>{2,a},w_core:>ass2[w,1]};
End[];
ShowHardQ[case_]:=Block[{num=Length@case/5,par,rank},
  par=Characters@Partition[case,5]/.Thread[Characters@"23456789TJQKA"->Range[2,14]];
  rank[i_,j_]:=If[OrderedQ[Apply[FCS`core,{par[[i]],par[[j]]},{1}]/.FCS`rule],j,i];
  UnitVector[num,FoldPair[{rank[#1,#2],rank[#1,#2]}&,Range@num]]];





(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];