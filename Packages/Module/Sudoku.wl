(* ::Package:: *)
(* ::Title:: *)
(*Sudoku(数独)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:酱紫君*)
(*Creation Date:2016-12-06*)
(*Copyright: Mozilla Public License Version 2.0*)
(* ::Program:: *)
(*1.软件产品再发布时包含一份原始许可声明和版权声明。*)
(*2.提供快速的专利授权。*)
(*3.不得使用其原始商标。*)
(*4.如果修改了源代码，包含一份代码修改说明。*)
(**)
(* ::Text:: *)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["Sudoku`"];
SudokuObject::usage = "";
SudokuForm::usage = "";
SudokuPlot::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Version$Sudoku = "V0.5";
Updated$Sudoku = "2018-8-11";
Sudoku::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
(* ::Subsubsection:: *)
(*Sudoku display*)
Options[SudokuPlot] = {FontSize -> Large, ImageSize -> Large};
(*Todo:修复SudokuPlot不兼容非三阶的bug*)
SudokuPlot[mat_?MatrixQ, OptionsPattern[]] := Module[
	{bg, color, foo},
	bg = If[EvenQ[Floor[(#2 - 1) / 3] + Floor[(#1 - 1) / 3] * 3], Lighter[Gray, 0.5], White]&;
	color = Switch[#, _?Positive, Style[#, Large, Black], _?Negative, Style[#, Red, OptionValue[FontSize]], _, ""]&;
	foo[{i_, j_} -> k_] := {EdgeForm[Thin], bg[i, j], Rectangle[{i, j}, {i + 1, j + 1}], Text[color@k, {i + 0.5, j + 0.5}]};
	Graphics[foo /@ Thread[Flatten[Table[{i, j}, {i, 1, 9}, {j, 1, 9}], 1] -> Flatten[mat]],
		ImageSize -> OptionValue[ImageSize]
	]
];
SudokuForm[mat_] := Module[
	{t = Length[mat], styles, bg, fmt},
	fmt = Map[If[NonPositive@#, \[Placeholder], #] &, mat, {2}];
	styles = {{Thickness[2], Sequence @@ Table[True, {Sqrt[t] - 1}]}};
	bg = Flatten[Table[{i, j} -> If[EvenQ[Plus @@ Floor[{i - 1, j - 1} / Sqrt[t]]], Darker[White, 0.3], White], {i, t}, {j, t}]];
	Style[Grid[fmt, Dividers -> {styles, styles}, Background -> {Automatic, Automatic, bg}, ItemSize -> {1.8, 1.8}], 14, "Label"]
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
(*Sudoku generation*)
Sudoku::lowRank = "数独的最低阶数为 2";
Sudoku::tooEmpty = "空格的数目不能高于 `1`";
Options[SudokuNew] = {MatrixForm -> False};
SudokuNew[num_Integer : 3, rm_Integer : 40, OptionsPattern[]] := Block[
	{n = num^2, seed, solution, game},
	If[n <= 1, Message[Sudoku::lowRank];Return[]];
	If[rm > n^2, Message[Sudoku::tooEmpty, n^2];Return[]];
	seed = ReplacePart[ConstantArray[0, {n, n}], Thread[RandomSample[Tuples[Range@n, 2], n] -> Range@n]];
	game = ReplacePart[SudokuSolverSeed[seed], Thread[RandomSample[Tuples[Range[n], 2], rm] -> 0]];
	If[TrueQ@Options[MatrixForm], game, SudokuForm[game]]
];




(* ::Subsubsection:: *)
(*数独求解代码*)
(* ::CodeText:: *)
(*参考资料*)
(*http://search.wolfram.com/?query=sudoku*)
(* ::Text:: *)
(*SudokuSolverMethod["Seed"]*)
blockPosition[{i_, j_}, size_] := blockPosition[{i, j}, size] = Sequence @@ Partition[Range[size], Sqrt[size]][[Ceiling /@ ({i, j} / Sqrt[size])]];
placeNumber[n_, {i_, j_}, extra_String : ""] := Block[
	{size = Length[choices]},
	If[
		MemberQ[choices[[i, j]], n],
		choices[[i, j]] = {};
		choices[[i]] = (DeleteCases[#1, n] & ) /@ choices[[i]];
		choices[[All, j]] = (DeleteCases[#1, n] & ) /@ choices[[All, j]];
		choices[[blockPosition[{i, j}, size]]] = Map[DeleteCases[#1, n] &, choices[[blockPosition[{i, j}, size]]], {2}];
		If[extra != "",
			If[
				(extra == "both" || extra == "diagonal") && i == j,
				choices = MapAt[DeleteCases[#1, n] & , choices, Table[{k, k}, {k, 1, size}]]
			];
			If[
				(extra == "both" || extra == "antidiagonal") && i + j == size + 1,
				choices = MapAt[DeleteCases[#1, n] & , choices, Table[{k, size + 1 - k}, {k, 1, size}]]
			]
		];
		result[[i, j]] = n,
		Throw[placedNumbers = size^2 + 1]
	]
];
singleNumber[v_] := With[
	{u = Flatten[Cases[Split[Sort[Flatten[v]]], {_}]]},
	If[u != {}, With[
		{w = ((Position[v, #1] & ) /@ u)[[All, 1, 1]]},
		If[
			Unequal @@ w,
			ReplacePart[v, List /@ u, List /@ w, List /@ Range[Length[u]]],
			Throw[placedNumbers = size^2 + 1]
		]
	], v]
];
reduceWith[rule_] := With[
	{t = Sqrt[Length[choices]]},
	choices = rule /@ choices;
	choices = Transpose[rule /@ Transpose[choices]];
	choices = rule /@ (Flatten[#1, 1] & ) /@ Flatten[Partition[choices, {t, t}], 1];
	choices = Flatten[(MapThread[Join, ##1] & ) /@ Partition[(Partition[#1, t] & ) /@ choices, t], 1];
	If[placedNumbers != Count[choices, {}, {-2}], Throw[placedNumbers = size^2 + 1]]
];
twins[v_] := With[
	{z = Cases[Split[Sort[Cases[v, {_, _}]]], {a_, a_} :> a]},
	If[z == {}, v, (If[MemberQ[z, #1], #1, Complement[#1, Flatten[z]]] & ) /@ v]
];
reduceFromBlocks := Block[
	{v, aux},
	aux = Partition[Range[size], Sqrt[size]];
	Do[
		v = choices[[blockPosition[{1, 1} + Sqrt[size] * {i - 1, j - 1}, size]]];
		Do[With[
			{w = Complement[Flatten[v[[k]]], Flatten[Drop[v, {k}]]]},
			If[w != {}, choices[[k + (i - 1) * Sqrt[size]]] = (Complement[#1, w] & ) /@ choices[[  k + (i - 1) * Sqrt[size]]]]
		], {k, 1, Sqrt[size]}
		];
		v = Transpose[v];
		Do[
			With[
				{w = Complement[Flatten[v[[k]]], Flatten[Drop[v, {k}]]]},
				If[w != {}, choices[[All, k + (j - 1) * Sqrt[size]]] = (Complement[#1, w] & ) /@
					choices[[All, k + (j - 1) * Sqrt[size]]]
				]
			],
			{k, 1, Sqrt[size]}
		];
		choices[[blockPosition[{1, 1} + Sqrt[size] * {i - 1, j - 1}, size]]] = Transpose[v],
		{i, 1, Sqrt[size]}, {j, 1, Sqrt[size]}
	];
	If[placedNumbers != Count[choices, {}, {-2}], Throw[placedNumbers = size^2 + 1]]
];
Options[SudokuSolverSeed] = {Number -> 1, Style -> ""};
SudokuSolverSeed[mat_, OptionsPattern[]] := Block[
	{size, choices, tobeDone, result, solutions, placedNumbers, extra},
	extra = OptionValue[Style];
	(* extra = "" | "diagonal" | "antidiagonal" | "both" *)
	size = Length[mat];
	choices = Array[Range[size] & , {size, size}];
	result = ConstantArray[0 , {size, size}];
	z = Position[mat, _Integer?Positive];
	placedNumbers = Length[z];
	Catch[MapThread[placeNumber[#1, #2, extra] & , {Extract[mat, z], z}];
	If[placedNumbers != Count[choices, {}, {-2}],
		placedNumbers = size^2 + 1]
	];
	solutions = {};
	tobeDone = If[placedNumbers <= size^2, {choices}, {}];
	splitCounter = 0;
	While[tobeDone != {} && Length[solutions] < OptionValue[Number],
		choices = tobeDone[[-1]];
		placedNumbers = Count[choices, {}, {-2}];
		Catch[reduceWith[singleNumber]];
		If[placedNumbers <= size^2, placedNumbers = Count[choices, {}, {-2}]];
		tobeDone = Most[tobeDone];
		While[placedNumbers < size^2, Catch[
			While[(z = Position[choices, {_}]) != {} && placedNumbers < size^2,
				placedNumbers = placedNumbers + Length[z];
				MapThread[placeNumber[#1, #2, extra] & , {Flatten[Extract[choices, z]], z}];
				reduceWith[singleNumber]
			];
			z = choices;
			reduceWith[twins];
			If[z != choices, Throw[reduceWith[singleNumber]]];
			reduceFromBlocks;
			If[z != choices, Throw[reduceWith[singleNumber]]];
			If[placedNumbers < size^2,
				splitCounter++;
				z = Min[Map[Length, choices, {2}] /. 0 -> size + 1];
				z = Position[choices, _?(Length[#1] == z & ), {-2}];
				pos = {{0, 0}, 3 * size};
				Do[With[
					{m = Count[{choices[[z[[i, 1]]]], choices[[All, z[[i, 2]]]], choices[[blockPosition[z[[i]], size]]]}, {}, {-2}]},
					If[m < pos[[2]], pos = {z[[i]], m}]
				], {i, 1, Length[z]}
				];
				pos = pos[[1]];
				AppendTo[tobeDone, ReplacePart[choices, Rest[choices[[pos[[1]], pos[[2]]]]], pos]];
				choices[[pos[[1]], pos[[2]]]] = Take[choices[[pos[[1]], pos[[2]]]], 1]
			]
		]];
		If[placedNumbers == size^2, AppendTo[solutions, result]]
	];
	If[OptionValue[Number] == 1 && solutions != {}, solutions[[1]], solutions]
];

(*SudokuSolverMethod["Nest"]*)
(*Todo:将算法扩展到高阶*)
SudokuSolverNest[sudoku_,upto_:16,lim_:1024]:=Block[
	{next,ans},
	next=Table[Table[ReplacePart[s,#1->n],{n,#2}]&@@First@SortBy[{#,Complement[Range@9,s[[First@#]],s[[;;,Last@#]],Catenate@Extract[Partition[s,{3,3}],Quotient[#,3,-2]]]}&/@Position[s,0,{2}],Length@Last@#&],{s,#}]&;
	ans=NestWhileList[Join@@Take[next@#,UpTo[lim]]&,{sudoku},!FreeQ[#,0]&,1,lim]
];


(*SudokuSolverMethod["BitOr"]*)
(*Todo:将算法扩展到高阶*)
SetAttributes[{SudokuFX3, SudokuExc}, HoldAll];
(* ::Text:: *)
(*SudokuLinked returns a list of the values at all locations that constrain the passed location,Delete is used*)
(*to ensure that the value of the passed location is not included in the list*)
SudokuLinked[m_][u_, v_, r_, c_] := {m[[u, All, r]] ~ Delete ~ {v, c}, m[[All, v, All, c]] ~ Delete ~ {u, r}, m[[u, v]] ~ Delete ~ {r, c}};
SudokuPof2 = Alternatives @@ (2^Range[0, 8]);
SudokuFX1 = BitOr @@ Cases[#, SudokuPof2, -1]&;
SudokuFX2 = BitOr[#, #2] - #&;
(* ::Text:: *)
(*SudokuPof2 is 1|2|4|8|...|256*)
(*SudokuFX1 is used to find all the values that are already claimed by SudokuLinked locations*)
(*SudokuFX2 then used to subtract out those cases from the unsolved locations*)
(*note HoldAll attr above:SudokuFX3 and SudokuExc are the only two functions which directly modify the board*)
SudokuFX3[m_] := (m[[##]] = SudokuFX2[SudokuFX1@SudokuLinked[m][##], m[[##]]])&;
SudokuExc[m_][c__] := If[# != {}, {m[[c]]} = #, ""]&@Cases[SudokuFX2[BitOr @@ #, m[[c]]]& /@ Flatten /@ SudokuLinked[m]@c, Except[0], 1, 1];
(* ::Text:: *)
(*between them SudokuLoop and SudokuSplit combine to solve the sudoku puzzle*)
(*each puzzle location is used as a bit vector to show possibles,i.e.511 means all are possible,*)
(*256 means only 8 is possible,1 means only 0 is possible (we're using 0-8,instead of 1-9)*)
(*FixedPoint applies SudokuFX3 and SudokuExc repeatedly to all locations that are not yet solved (=to a SudokuPof2) "good" is*)
(*a list of these positions,m (the modified board) is returned*)
SudokuLoop[board_] := Module[
	{m = board},
	(FixedPoint[Block[{good = Position[m, Except[SudokuPof2, _Integer], {-1}]}, SudokuFX3[m] @@@ good;SudokuExc[m] @@@ good]&, {}];m)
];
(* ::Text:: *)
(*returns the position of the element {w,x,y,z} that is most nearly a power of 2,for example 48 has only*)
(*two possibles:4 or 5 (48=2^4+2^5)*)
SudokuNear = Position[#, Min@#, -1, 1][[1]]&[Map[# ~ Count ~ 1&, IntegerDigits[#, 2, 9], {-2}] /. {1 -> 10}]&;
(* ::Text:: *)
(*bad puzzle boards (those with a 0=no possibles) are replaced with an empty sequence,which effectively*)
(*backtracks,going on to the next ReplacePart substitution (see below),solved boards are Throw'n,*)
(*and the rest are recursively solved.Solving involves working on locations that are nearly solved,*)
(*ReplacePart replaces those locations with all possible values,lowest to highest,for example*)
(*48 (possibles=4 or 5) would be replaced by 16 (2^4),then 32 (2^5)*)
SudokuSplit[board_] /; MemberQ[board, 0, {-1}] := Sequence[];
SudokuSplit[board_] /; MatchQ[Flatten@board, {SudokuPof2..}] := Throw[board];
SudokuSplit[board_] := With[
	{c = SudokuNear@board},
	SudokuSplit@SudokuLoop@ReplacePart[board, c -> #]& /@ First /@ (2^(Reverse@IntegerDigits[board ~ Extract ~ c, 2] ~ Position ~ 1 - 1))
];
(*0's become 511,everything else becomes 2^(n-1) and the puzzles are partitioned into blocks of 3*)
SudokuSolverBitOr[problem_] := Block[
	{encoding = 2^(mat ~ Partition ~ {3, 3} - 1) /. {1 / 2 -> 511}, sol},
	sol = Log2@Catch[SudokuSplit@SudokuLoop@encoding] + 1;
	Partition[Flatten@sol, 9]
];
(*SudokuSolver is the main solving function,upon completion we have a list of lists of the digits to be summed*)
(*the base 2 Log and "+1" are there because all work is done with numbers 2^(n-1)*)
(*"Catch" should get the complete boards Throw'n in SudokuSplit above*)





(*SudokuSolverMethod["LinearProgramming"]*)
(*Todo:将算法扩展到高阶*)
SudokuLinearBase = Module[
	{constrain, blocks, rows, cols},
	constrain[blk_] := Join @@ Table[Outer[Plus, Range[9], (Position[blk, k] - 1).{81, 9}], {k, Min[blk], Max[blk]}];
	blocks = Table[3 Quotient[i, 3] + Quotient[j, 3], {i, 0, 8}, {j, 0, 8}];
	rows = Transpose[cols = ConstantArray[Range@9, 9] - 1];
	Flatten[constrain /@ {blocks, rows, cols}, 1]
];
SudokuSolverLinearProgramming[problem_] := Module[
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





(* ::Text:: *)
(*SudokuSolverMethod["Logic"]*)
(*Todo:需要大幅修改,Print改Echo, 加入递归限制*)
(* ::Text:: *)
(*LineSimplify uses a "level 1" logic, in that it can eliminate values once a value is found to be one value in one place.*)
(*LineSimplify could be extended to eliminate when two values are known in two places eg {1,2},{1,2} allows us to  remove 1 and 2 from other entries.*)
(*And {1,2,3},{1,2,3},{1,2,3}, eliminates 1,2,3 from others. etc*)
LineSimplify[data_List] := Block[{known, d2},
	known = Cases[data, {n_} -> n];
	(*Find all positions with only one possible value*)
	If[Length[Union[known]] < Length[known] || (Count[data, {}] > 0), Throw[Contradiction]];
	(*If the same number is known twice, then we have a conflict*)
	(*If there are no remaining possible values for a position then we have an error*)
	d2 = Map[If[Length[#] === 1, #, Complement[#, known]]&, data];
	(*Eliminate the known values from each position, except those positions that are length 1 and thus known*)
	(*If a value only occurs in once place on a line then all other possibilities can be removed from that entry.*)
	(*Repeat for all numbers.*)
	d2 /. Map[{___, #, ___} -> {#}&, Cases[Frequencies[Flatten[d2]], {1, n_} -> n]]
];
(* ::Text:: *)
(*A function to apply all logic currently supported for a single line in isolation. *)
(*1. Eliminate values that are known from other positions *)
(*2. Search for numbers that occur only once and set them as known*)
SudokuProcess[data_] := FixedPoint[Block[
	{workingData = #},
	workingData = Map[LineSimplify, workingData];
	workingData = Transpose[Map[LineSimplify, Transpose[workingData]]];
	workingData = FromSubMatrices[Map[LineSimplify, FromSubMatrices[workingData]]]
] &, data];
(* ::Text:: *)
(*Apply the line simplification function to all rows, columns and sub-grids (rearranged into lines and back again).*)
SubMatrix3[data_, {i_, j_}] := Table[data[[i + m - 1, j + n - 1]], {m, 3}, {n, 3}]
FromSubMatrices[data_List] := Flatten[Table[Flatten[SubMatrix3[data, {i, j}], 1], {i, 1, 7, 3}, {j, 1, 7, 3}], 1]
Options[ApplySudokuLogic] = {};
ApplySudokuLogic[data_, opts___] := Block[
	{
		firstUnsolved, firstUnsolvedPosition, step,SudokuSolvedQ,
		workingData = Catch[FixedPoint[SudokuProcess, data]]
	(*Apply the logic steps until no more progres is made*)
	},
	SudokuSolvedQ[data_] := Dimensions[data] === {9, 9, 1};
	If[workingData === Contradiction, Throw[Contradiction]];
	(*If a contradiction is arrived at, throw the error state up a level in the recursion*)
	If[SudokuSolvedQ[workingData], workingData,
	(*If the problem is solved, stop*)
		firstUnsolved = First[Cases[workingData, {n_Integer, m__Integer}, {2}]];
		(*Otherwise find the first unsolved position possible values*)
		If[firstUnsolved === {}, Throw[Contradiction]];
		(* If there are no possible values, throw an error*)
		firstUnsolvedPosition = First[Position[workingData, firstUnsolved]];
		Print["Guessing ", First[firstUnsolved], " at ", firstUnsolvedPosition];
		step = Catch[ApplySudokuLogic[ReplacePart[workingData, Take[firstUnsolved, 1], firstUnsolvedPosition], opts]];
		(* Choose the first of the possible values and attempt to solve again*)
		If[step === Contradiction,
			Print["Guess ", First[firstUnsolved], " at ", firstUnsolvedPosition, " failed. Must be one of ", Rest[firstUnsolved]];
			Catch[ApplySudokuLogic[ReplacePart[workingData, Rest[firstUnsolved], firstUnsolvedPosition], opts]],
		(*If a contradiction results from the guess, eliminate this possibility and try to solve again*)
			step
		(*Otherwise return the result*)
		]
	]
];
SudokuSolveLogic[data_] := Block[
	{encoding},
	encoding = Map[If[Head[#] === Integer, {#}, Range[9]]&, data, {2}];
	Partition[Flatten@Catch[ApplySudokuLogic[encoding]], 9];
];





(* ::Subsection::Closed:: *)
(*附加设置*)
End[];
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
EndPackage[]
