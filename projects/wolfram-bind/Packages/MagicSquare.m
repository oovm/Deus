Magic::usage = "\!\(\*StyleBox[RowBox[{\"Magic\", \"[\", StyleBox[\"n\", \"TI\"], \"]\"}], \"MR\"]\) 生成 \!\(\*StyleBox[RowBox[{\"n\", \"*\", \"n\"}], \"TI\"]\) 的幻方.\n\!\(\*StyleBox[RowBox[{\"Magic\", \"[\", StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"d\", \"TI\"], \"]\"}], \"MR\"]\) 生成 \!\(\*StyleBox[\"d\", \"TI\"]\) 维的 \!\(\*StyleBox[\"n\", \"TI\"]\) 阶幻方.";
MagicQ::usage = "MagicQ[n]检测一个n×n幻方.\r MagicQ[n,3D]检测一个n×n×n幻立方.";
MagicSquare::usage = "幻方包提供了一系列幻方的算法.";
Begin["`MagicSquare`"];
Version$MagicSquare = "V1.6";
Updated$Version = "2019-12-09";



Magic::nosol = "无解.";
Magic::nodef = "无定义.";
Magic::novpn = "数据库请求失败, 你可能需要VPN, 或者您要求的数据量太过巨大, 请可以使用 TimeConstraint 选项增加请求时长.";
Options[Magic] = {Method -> "Simple", TimeConstraint -> 5};
Magic[n_?Internal`PositiveIntegerQ, ops : OptionsPattern[]] := Magic[n, 2, ops];
Magic[n_?Internal`PositiveIntegerQ, d_?Internal`PositiveIntegerQ, OptionsPattern[]] := GeneralUtilities`Scope[
	If[
		OptionValue[Method] === "Simple",
		If[d == 2, Return@Magic2D[n]];
		If[d == 3, Return@Magic3D[n]];
	];
	TimeConstrained[
		MagicLinker[n, d, OptionValue[Method]],
		OptionValue[TimeConstraint],
		Message[Magic2D::novpn]
	]
];
(*Magic[n_, "3D"] := Magic3DShow@Magic3D[n];*)


MagicLinker[n_, d_, p_] := URLExecute["http://magichypercube.com/rest/hypercube/" <> p <> "/" <> ToString[n] <> "/" <> ToString[d] <> "/true", "CSV"];



Magic2D[n_?OddQ] := Block[
	{p = Range[n]},
	Outer[Plus, p, p - (n + 3) / 2] ~ Mod ~ n * n + Outer[Plus, p, 2p - 2] ~ Mod ~ n + 1
];
Magic2D[n_ /; n ~ Mod ~ 4 == 0] := Block[
	{J, K1, M},
	J = Floor[(Range[n] ~ Mod ~ 4) / 2.0];
	K1 = Abs@Outer[Plus, J, -J] ~ BitXor ~ 1;
	M = Outer[Plus, Range[1, n^2, n], Range[0, n - 1]];
	M + K1(n * n + 1 - 2M)
] // Experimental`CompileEvaluate;
Magic2D[2] := Message[Magic::nosol];
Magic2D[n_?EvenQ] := GeneralUtilities`Scope[
	p = n / 2;
	M = Magic2D[p];
	M = ArrayFlatten@{{M, M + 2p^2}, {M + 3p^2, M + p^2}};
	If[n == 2, Return[M]];
	i = Transpose@{Range@p};
	k = (n - 2) / 4;
	j = Range[k] ~ Join ~ Range[n - k + 2, n];
	M[[Flatten@{i, i + p}, j]] = M[[Flatten@{i + p, i}, j]];
	i = k + 1;j = {1, i};
	M[[Flatten@{i, i + p}, j]] = M[[Flatten@{i + p, i}, j]];
	M
];
Magic2D[x_] := Message[Magic::nodef];
Magic3D[n_?OddQ] := Table[n^2Mod[i - j + k - 1, n] + n Mod[i - j - k, n] + Mod[i + j + k - 2, n] + 1, {i, 1, n}, {j, 1, n}, {k, 1, n}];
Magic3D[n_ /; n ~ Mod ~ 4 == 0] := GeneralUtilities`Scope[
	QMagic[x_] := If[1 <= x <= n / 2, 0, 1];
	FMagic[i_, j_, k_] := Mod[i + j + k + QMagic[i] + QMagic[j] + QMagic[k], 2];
	Table[If[FMagic[i, j, k] == 1, (i - 1)n^2 + (j - 1)n + k, 1 - k + n(1 - j + n(1 - i + n))], {i, 1, n}, {j, 1, n}, {k, 1, n}]
];



Magic3D[2] := Message[Magic::nosol];
Magic3D[n_?EvenQ] := GeneralUtilities`Scope[
	QMagic[x_] := If[1 <= x <= n / 2, 0, 1];
	XMagic[x_] := Min[x, n + 1 - x];
	u = Mod[XMagic[i] - XMagic[j] + XMagic[k], n / 2] + 1;
	v = 4QMagic[i] + 2QMagic[j] + QMagic[k] + 1;
	d[1, v_] := {7, 3, 6, 2, 5, 1, 4, 0}[[v]];
	d[2, v_] := {3, 7, 2, 6, 1, 5, 0, 4}[[v]];
	d[3, v_] := {0, 1, 3, 2, 5, 4, 6, 7}[[v]];
	d[u_, v_] := If[Mod[u, 2] === 0, v - 1, 8 - v];
	upTab = Table[(n / 2)^3d[u, v], {i, 1, n}, {j, 1, n}, {k, 1, n}];
	downTab = Table[(n / 2)^2Mod[i - j + k - 1, n / 2] + (n / 2)Mod[i - j - k, n / 2] + Mod[i + j + k - 2, n / 2] + 1, {i, 1, n}, {j, 1, n}, {k, 1, n}];
	upTab + downTab
];
Magic3D[x_] := Message[Magic2D::nodef];
Magic3DShow[n_] := {
	Graph3D@GridGraph[{n, n, n}, VertexLabels -> Table[i -> Flatten[Magic3D@n][[i]], {i, n^3}]],
	MatrixForm /@ Magic3D[n]
};
MagicQ[input_, "3D"] := Magic3DQ[input];
MagicQ[matrix_] := GeneralUtilities`Scope[
	Echo["该矩阵所有数字总和为" <> ToString@Total[Total /@ matrix]];
	SRow = Total /@ matrix;
	Echo["该矩阵各行和分别为" <> ToString@SRow];
	If[SameQ @@ SRow, Echo["通过"], Return[False]];
	SCol = Total /@ (Transpose@matrix);
	Echo["该矩阵各列和分别为" <> ToString@SCol];
	If[SameQ @@ SCol, Echo["通过"], Return[False]];
	Echo["该矩阵主对角线和为" <> ToString@Tr@matrix <> ",该矩阵主副角线和为" <> ToString@Tr[Reverse /@ matrix]];
	If[SameQ[Tr@matrix, Tr[Reverse /@ matrix]], True, False]
];
Magic3DQ[x3d_] := GeneralUtilities`Scope[
	Echo["该立方矩阵所有数字总和为" <> ToString@Total@Flatten@x3d];
	{y3d, z3d} = {Transpose[x3d, {3, 1, 2}], Transpose[x3d, {2, 3, 1}]};
	SF = {Total@Flatten@#& /@ x3d, Total@Flatten@#& /@ y3d, Total@Flatten@#& /@ z3d};
	Echo["该立方矩阵各面和分别为" <> ToString@SF];
	If[SameQ @@ Flatten@SF, Echo["通过"], Return[False]];
	LF = {Map[Total, x3d, {2}], Map[Total, y3d, {2}], Map[Total, z3d, {2}]};
	Echo["该立方矩阵各列和分别为" <> ToString@LF];
	If[SameQ @@ Flatten@LF, Echo["通过"], Return[False]];
	TF = Tr /@ {x3d, y3d, z3d};
	Echo["该立方矩阵各对角线和分别为" <> ToString@TF];
	If[SameQ @@ TF, True, False]
];
SetAttributes[{Magic, Magic2D, Magic3D}, Listable];
SetAttributes[
	{},
	{ReadProtected}
];
End[]
