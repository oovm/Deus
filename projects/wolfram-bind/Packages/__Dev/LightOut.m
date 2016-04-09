(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: LightOut *)
(* :Context: LightOut` *)
(* :Author: 28059 *)
(* :Date: 2016-11-13 *)

(* :Package Version: 0.2 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["LightOut`"];


Begin["`Private`"]
(*啊,狗带,老子辛辛苦苦写的异或求解器
var[i_,j_]:=x[i,j];
variable[i_,j_,w_,h_]:=If[i>=1&&i<=w&&j>=1&&j<=h,var[i,j],0];
GetSquareValue[i_,j_,w_,h_,gs_]:=If[i>=1&&i<=w&&j>=1&&j<=h,gs[[i,j]],0];
GetShowValue[i_,j_,w_,h_,gs_]:=GetSquareValue[i,j,w,h,gs]+GetSquareValue[i,j+1,w,h,gs]+GetSquareValue[i,j-1,w,h,gs]+GetSquareValue[i-1,j,w,h,gs]+GetSquareValue[i+1,j,w,h,gs];
AllVariables[w_,h_]:=Flatten[Table[var[i,j],{i,1,w},{j,1,h}]];
OnSolver[M_?MatrixQ]:=Block[{w,h,OnEquation,OnSol},
  {w,h}=Length/@{M,Transpose@M};
  OnEquation[i_,j_,w_,h_,gs_]:=GetShowValue[i,j,w,h,gs]==1+variable[i,j,w,h]+variable[i-1,j,w,h]+ variable[i,j-1,w,h]+variable[i+1,j,w,h]+variable[i,j+1,w,h];
  OnSol=Quiet[Solve[Flatten[Table[OnEquation[i,j,w,h,M],{i,1,w},{j,1,h}]],AllVariables[w,h],Modulus->2]];
  Partition[Flatten[OnSol/.(x[a_,b_]->c_)->c],w]/.C[1]->0/.C[2]->0];
OffSolver[M_?MatrixQ]:=Block[{w,h,OffEquation,OffSol},
  {w,h}=Length/@{M,Transpose@M};
  OffEquation[i_,j_,w_,h_,gs_]:=GetShowValue[i,j,w,h,gs]==variable[i,j,w,h]+variable[i-1,j,w,h]+variable[i,j-1,w,h]+variable[i+1,j,w,h]+variable[i,j+1,w,h];
  OffSol=Quiet[Solve[Flatten[Table[OffEquation[i,j,w,h,M],{i,1,w},{j,1,h}]],AllVariables[w,h],Modulus->2]];
  Partition[Flatten[OffSol/.(x[a_,b_]->c_)->c],w]/.C[1]->0/.C[2]->0];*)
(*速度、可读性和可推广性能把上面的代码按在地上摩擦摩擦的通用求解器*)
M=AdjacencyMatrix@GridGraph[{n,n}]+SparseArray[{Band[{1,1}]->1},{n^2,n^2}]
ArrayPlot@Partition[LinearSolve[M,ConstantArray[1,n^2],Modulus->2],n]
res=RowReduce[Transpose@Join[Transpose@M,{ConstantArray[1,n^2]}],Modulus->2];
ArrayPlot@Partition[res[[All,-1]],n];



End[];

EndPackage[];