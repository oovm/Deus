SudokuObject::usage = "";
SudokuForm::usage = "";
SudokuPlot::usage = "";
Version$Sudoku = "V0.5";
Updated$Sudoku = "2018-8-11";
Sudoku::usage = "程序包的说明,这里抄一遍";
Begin["`Sudoku`"];
Options[SudokuPlot] = {FontSize -> Large, ImageSize -> Large};
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
SudokuSolverNest[sudoku_,upto_:16,lim_:1024]:=Block[
	{next,ans},
	next=Table[Table[ReplacePart[s,#1->n],{n,#2}]&@@First@SortBy[{#,Complement[Range@9,s[[First@#]],s[[;;,Last@#]],Catenate@Extract[Partition[s,{3,3}],Quotient[#,3,-2]]]}&/@Position[s,0,{2}],Length@Last@#&],{s,#}]&;
	ans=NestWhileList[Join@@Take[next@#,UpTo[lim]]&,{sudoku},!FreeQ[#,0]&,1,lim]
];
SetAttributes[{SudokuFX3, SudokuExc}, HoldAll];
SudokuLinked[m_][u_, v_, r_, c_] := {m[[u, All, r]] ~ Delete ~ {v, c}, m[[All, v, All, c]] ~ Delete ~ {u, r}, m[[u, v]] ~ Delete ~ {r, c}};
SudokuPof2 = Alternatives @@ (2^Range[0, 8]);
SudokuFX1 = BitOr @@ Cases[#, SudokuPof2, -1]&;
SudokuFX2 = BitOr[#, #2] - #&;
SudokuFX3[m_] := (m[[##]] = SudokuFX2[SudokuFX1@SudokuLinked[m][##], m[[##]]])&;
SudokuExc[m_][c__] := If[# != {}, {m[[c]]} = #, ""]&@Cases[SudokuFX2[BitOr @@ #, m[[c]]]& /@ Flatten /@ SudokuLinked[m]@c, Except[0], 1, 1];
SudokuLoop[board_] := Module[
	{m = board},
	(FixedPoint[Block[{good = Position[m, Except[SudokuPof2, _Integer], {-1}]}, SudokuFX3[m] @@@ good;SudokuExc[m] @@@ good]&, {}];m)
];
SudokuNear = Position[#, Min@#, -1, 1][[1]]&[Map[# ~ Count ~ 1&, IntegerDigits[#, 2, 9], {-2}] /. {1 -> 10}]&;
SudokuSplit[board_] /; MemberQ[board, 0, {-1}] := Sequence[];
SudokuSplit[board_] /; MatchQ[Flatten@board, {SudokuPof2..}] := Throw[board];
SudokuSplit[board_] := With[
	{c = SudokuNear@board},
	SudokuSplit@SudokuLoop@ReplacePart[board, c -> #]& /@ First /@ (2^(Reverse@IntegerDigits[board ~ Extract ~ c, 2] ~ Position ~ 1 - 1))
];
SudokuSolverBitOr[problem_] := Block[
	{encoding = 2^(mat ~ Partition ~ {3, 3} - 1) /. {1 / 2 -> 511}, sol},
	sol = Log2@Catch[SudokuSplit@SudokuLoop@encoding] + 1;
	Partition[Flatten@sol, 9]
];
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
LineSimplify[data_List] := Block[{known, d2},
	known = Cases[data, {n_} -> n];
	If[Length[Union[known]] < Length[known] || (Count[data, {}] > 0), Throw[Contradiction]];
	d2 = Map[If[Length[#] === 1, #, Complement[#, known]]&, data];
	d2 /. Map[{___, #, ___} -> {#}&, Cases[Frequencies[Flatten[d2]], {1, n_} -> n]]
];
SudokuProcess[data_] := FixedPoint[Block[
	{workingData = #},
	workingData = Map[LineSimplify, workingData];
	workingData = Transpose[Map[LineSimplify, Transpose[workingData]]];
	workingData = FromSubMatrices[Map[LineSimplify, FromSubMatrices[workingData]]]
] &, data];
SubMatrix3[data_, {i_, j_}] := Table[data[[i + m - 1, j + n - 1]], {m, 3}, {n, 3}]
FromSubMatrices[data_List] := Flatten[Table[Flatten[SubMatrix3[data, {i, j}], 1], {i, 1, 7, 3}, {j, 1, 7, 3}], 1]
Options[ApplySudokuLogic] = {};
ApplySudokuLogic[data_, opts___] := Block[
	{
		firstUnsolved, firstUnsolvedPosition, step,SudokuSolvedQ,
		workingData = Catch[FixedPoint[SudokuProcess, data]]
	},
	SudokuSolvedQ[data_] := Dimensions[data] === {9, 9, 1};
	If[workingData === Contradiction, Throw[Contradiction]];
	If[SudokuSolvedQ[workingData], workingData,
		firstUnsolved = First[Cases[workingData, {n_Integer, m__Integer}, {2}]];
		If[firstUnsolved === {}, Throw[Contradiction]];
		firstUnsolvedPosition = First[Position[workingData, firstUnsolved]];
		Print["Guessing ", First[firstUnsolved], " at ", firstUnsolvedPosition];
		step = Catch[ApplySudokuLogic[ReplacePart[workingData, Take[firstUnsolved, 1], firstUnsolvedPosition], opts]];
		If[step === Contradiction,
			Print["Guess ", First[firstUnsolved], " at ", firstUnsolvedPosition, " failed. Must be one of ", Rest[firstUnsolved]];
			Catch[ApplySudokuLogic[ReplacePart[workingData, Rest[firstUnsolved], firstUnsolvedPosition], opts]],
			step
		]
	]
];
SudokuSolveLogic[data_] := Block[
	{encoding},
	encoding = Map[If[Head[#] === Integer, {#}, Range[9]]&, data, {2}];
	Partition[Flatten@Catch[ApplySudokuLogic[encoding]], 9];
];
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]