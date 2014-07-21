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
(*Creation Date:2016-12-06*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["Sudoku`"];
GraphRPS::usage = "GraphRPS[n]给出n元猜拳的胜负判定图.";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Sudoku$Version = "V0.2";
Sudoku$LastUpdate = "2016-12-19";
OtherGames::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)


(* ::Subsubsection:: *)
(*Sudoku display*)
SudokuPlot[mat_?MatrixQ] := Module[
	{bg, color, foo},
	bg = If[EvenQ[Floor[(#2 - 1) / 3] + Floor[(#1 - 1) / 3] * 3], Lighter[Gray, 0.5], White]&;
	color = Switch[#, _?Positive, Style[#, Large, Black], _?Negative, Style[#, Red, Large], _, ""]&;
	foo[{i_, j_} -> k_] := {EdgeForm[Thin], bg[i, j], Rectangle[{i, j}, {i + 1, j + 1}], Text[color@k, {i + 0.5, j + 0.5}]};
	Graphics[foo /@ Thread[Flatten[Table[{i, j}, {i, 1, 9}, {j, 1, 9}], 1] -> Flatten[mat]]]
];
SudokuForm[mat_] := Module[
	{t = Length[mat], styles, bg},
	styles = {{Thickness[2], Sequence @@ Table[True, {Sqrt[t] - 1}]}};
	bg = Flatten[Table[{i, j} -> If[EvenQ[Plus @@ Floor[{i - 1, j - 1} / Sqrt[t]]], Darker[White, 0.3], White], {i, t}, {j, t}]];
	Style[Grid[mat /. {0 :> \[Placeholder]}, Dividers -> {styles, styles}, Background -> {Automatic, Automatic, bg}, ItemSize -> {1.8, 1.8}], 14, "Label"]
];
SudokuObject /: MakeBoxes[SudokuObject[expr_], StandardForm] := With[
	{r = ToBoxes@SudokuForm@expr},
	InterpretationBox[r, SudokuObject[expr]]
];
SudokuObject /: MakeBoxes[SudokuObject[expr_], TraditionalForm] := With[
	{r = ToBoxes@SudokuPlot@expr},
	InterpretationBox[r, SudokuObject[expr]]
];


(* ::Subsubsection:: *)
(*Todo: 完成数独程序块*)



(*SudokuSolverMethod["LinearProgramming"]*)
SetAttributes[{SudokuFX3, SudokuExc}, HoldAll];
(*数独的标准格式是一个9×9的矩阵,里面只能填0到9,0表示待解*)
(*SudokuLinked returns a list of the values at all locations that constrain the passed location,Delete is used*)
(*to ensure that the value of the passed location is not included in the list*)
SudokuLinked[m_][u_, v_, r_, c_] := {m[[u, All, r]] ~ Delete ~ {v, c}, m[[All, v, All, c]] ~ Delete ~ {u, r}, m[[u, v]] ~ Delete ~ {r, c}};
(*SudokuPof2 is 1|2|4|8|...|256*)
SudokuPof2 = Alternatives @@ (2^Range[0, 8]);
SudokuFX1 = BitOr @@ Cases[#, SudokuPof2, -1]&;
SudokuFX2 = BitOr[#, #2] - #&;
(*SudokuFX1 is used to find all the values that are already claimed by SudokuLinked locations*)
(*SudokuFX2 then used to subtract out those cases from the unsolved locations*)
(*note HoldAll attr above:SudokuFX3 and SudokuExc are the only two functions which directly modify the board*)
SudokuFX3[m_] := (m[[##]] = SudokuFX2[SudokuFX1@SudokuLinked[m][##], m[[##]]])&;
SudokuExc[m_][c__] := If[# != {}, {m[[c]]} = #, ""]&@Cases[SudokuFX2[BitOr @@ #, m[[c]]]& /@ Flatten /@ SudokuLinked[m]@c, Except[0], 1, 1];
(*between them SudokuLoop and SudokuSplit combine to solve the sudoku puzzle*)
(*each puzzle location is used as a bit vector to show possibles,i.e.511 means all are possible,*)
(*256 means only 8 is possible,1 means only 0 is possible (we're using 0-8,instead of 1-9)*)
(*FixedPoint applies SudokuFX3 and SudokuExc repeatedly to all locations that are not yet solved (=to a SudokuPof2) "good" is*)
(*a list of these positions,m (the modified board) is returned*)
SudokuLoop[board_] := Module[
	{m = board},
	(FixedPoint[Block[{good = Position[m, Except[SudokuPof2, _Integer], {-1}]}, SudokuFX3[m] @@@ good;SudokuExc[m] @@@ good]&, {}];m)
];
(*returns the position of the element {w,x,y,z} that is most nearly a power of 2,for example 48 has only*)
(*two possibles:4 or 5 (48=2^4+2^5)*)
SudokuNear = Position[#, Min@#, -1, 1][[1]]&[Map[# ~ Count ~ 1&, IntegerDigits[#, 2, 9], {-2}] /. {1 -> 10}]&;
(*bad puzzle boards (those with a 0=no possibles) are replaced with an empty sequence,which effectively*)
(*backtracks,going on to the next ReplacePart substitution (see below),solved boards are Throw'n,*)
(*and the rest are recursively solved.Solving involves working on locations that are nearly solved,*)
(*ReplacePart replaces those locations with all possible values,lowest to highest,for example*)
(*48 (possibles=4 or 5) would be replaced by 16 (2^4),then 32 (2^5)*)
SudokuSplit[board_] /; MemberQ[board, 0, {-1}] := Sequence[];
SudokuSplit[board_] /; MatchQ[Flatten@board, {SudokuPof2..}] := Throw[board] ;;
	SudokuSplit[board_] := With[
	{c = SudokuNear@board},
	SudokuSplit@SudokuLoop@ReplacePart[board, c -> #]& /@ First /@ (2^(Reverse@IntegerDigits[board ~ Extract ~ c, 2] ~ Position ~ 1 - 1))
];
(*0's become 511,everything else becomes 2^(n-1) and the puzzles are partitioned into blocks of 3*)
SudokuSolverMethod["BitOr"][problem_] := Block[
	{encoding = 2^(mat ~ Partition ~ {3, 3} - 1) /. {1 / 2 -> 511}, sol},
	sol = Log2@Catch[SudokuSplit@SudokuLoop@encoding] + 1;
	Partition[Flatten@sol, 9]
];
(*SudokuSolver is the main solving function,upon completion we have a list of lists of the digits to be summed*)
(*the base 2 Log and "+1" are there because all work is done with numbers 2^(n-1)*)
(*"Catch" should get the complete boards Throw'n in SudokuSplit above*)




(*SudokuSolverMethod["LinearProgramming"]*)
SudokuLinearBase = Module[
	{constrain, blocks, rows, cols},
	constrain[blk_] := Join @@ Table[Outer[Plus, Range[9], (Position[blk, k] - 1).{81, 9}], {k, Min[blk], Max[blk]}];
	blocks = Table[3 Quotient[i, 3] + Quotient[j, 3], {i, 0, 8}, {j, 0, 8}];
	rows = Transpose[cols = ConstantArray[Range@9, 9] - 1];
	Flatten[constrain /@ {blocks, rows, cols}, 1]
];
SudokuSolverMethod["LinearProgramming"][problem_] := Module[
	{problemConstraints, allConstraints, lpResult},
	problemConstraints = Map[List, Join @@ Table[Append[k] /@ Position[problem, k], {k, 9}].{81, 9, 1} - 90];
	allConstraints = Total[UnitVector[729, #]& /@ #]& /@ Join[
		SudokuLinearBase,
		Partition[Range@729, 9],
		problemConstraints
	];
	lpResult = Quiet@LinearProgramming[
		ConstantArray[0, 729],
		allConstraints,
		ConstantArray[{1, 0}, Length@allConstraints],
		ConstantArray[{0, 1}, 729],
		Integers
	];
	Partition[FirstPosition[#, 1][[1]]& /@ Partition[lpResult, 9], 9]
];

End[];
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
EndPackage[]
