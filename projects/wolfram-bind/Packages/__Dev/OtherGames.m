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
BeginPackage["OtherGames`"];
GraphRPS::usage="GraphRPS[n]给出n元猜拳的胜负判定图.";
MatrixRPS::usage="MatrixRPS[n]可以输出n元猜拳的关系矩阵.";
RPSQ::usage="RPSQ[k,{x,n}]判定n元猜拳中{x,n}的胜负向量,1为胜,0为平,-1为败.";
GameRPS::usage="GameRPS[],与Mathematica AI玩猜拳!你觉得猜拳有策略吗?";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
OtherGames$Version="V0.2";
OtherGames$Environment="V11.0+";
OtherGames$LastUpdate="2016-12-19";
OtherGames::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*猜拳程序块*)
RPSQ[k_?OddQ,{x_,n_}]:=If[x===n,{0,0},If[Mod[n+1,k]<=x<=Mod[n+(k-1)/2,k],{-1,1},{1,-1}]];
RPSQ[k_?EvenQ,{x_,n_}]:=If[x===n||x===Mod[n+k/2,k],{0,0},If[Mod[n+1,k]<=x<=Mod[n+k/2-1,k],{-1,1},{1,-1}]];
MatrixRPS[2]:="请输入大于2的整数";
MatrixRPS[n_?OddQ]:=MapIndexed[RotateRight,Table[Join[ConstantArray[1,(n-1)/2],ConstantArray[0,(n+1)/2]],{n}]];
MatrixRPS[n_?EvenQ]:=MapIndexed[RotateRight,Table[Join[ConstantArray[1,n/2-1],ConstantArray[0,n/2+1]],{n}]];
MatrixRPS[n_]:=MatrixRPS[2];
GraphRPS[2]:=MatrixRPS[2];
GraphRPS[3,"预设"]:=Graph[{"石头","剪刀","布"},{SparseArray[Automatic,{3,3},0,{1,{{0,1,2,3},{{2},{3},{1}}},Pattern}],Null},
  VertexLabels->Placed["Name",Center],
  VertexLabelStyle->Directive[24,Lighter@Blue,Bold],
  VertexSize->0.3,EdgeShapeFunction->GraphElementData["CarvedArrow","ArrowSize"->0.1],
  GraphLayout->"CircularEmbedding"];
GraphRPS[5,"预设"]:=Graph[{"石头","剪刀","蜥蜴","布","史波克"},
  {SparseArray[Automatic,{5,5},0,{1,{{0,2,4,6,8,10},{{2},{3},{3},{4},{4},{5},{1},{5},{1},{2}}},Pattern}],Null},
  VertexLabels->Placed["Name",Center],VertexLabelStyle->Directive[24,Lighter@Blue,Bold],
  VertexSize->0.3,EdgeShapeFunction->GraphElementData["CarvedArrow","ArrowSize"->0.06],
  GraphLayout->"CircularEmbedding"];
GraphRPS[7,"预设"]:=Graph[{"天枢","天璇","天玑","天权","玉衡","开阳","瑶光"},{SparseArray[Automatic,{7,7},
  0,{1,{{0,3,6,9,12,15,18,21},{{2},{3},{4},{3},{4},{5},{4},{5},{6},{5},{6},{7},{1},{6},{7},{1},{2},{7},{1},{2},{3}}},Pattern}],Null},
  VertexLabels->Placed["Name",Center],
  VertexLabelStyle->Directive[24,Lighter@Blue,Bold],
  VertexSize->0.3,EdgeShapeFunction->GraphElementData["CarvedArrow","ArrowSize"->0.04],
  GraphLayout->"CircularEmbedding",ImageSize->Large];
GraphRPS[n_?IntegerQ,size_:18]:=AdjacencyGraph[MatrixRPS[n],
  VertexLabels->Placed["Index",Center],
  VertexLabelStyle->Directive[size,Lighter@Blue,Bold],
  VertexSize->0.3,EdgeShapeFunction->GraphElementData["CarvedArrow","ArrowSize"->0.03],
  GraphLayout->"CircularEmbedding",ImageSize->Large];
GraphRPS[n_]:=MatrixRPS[2];
Needs["ExData`"];
GameRPS=DynamicBlock[{history={},hLast=0,cLast=0,hScore=0,
  cScore=0,message=""},Panel@Column[{Dynamic@Grid[{{Column[MapIndexed[Button[#,cLast=chooseGo2[history];
hLast=#2[[1]];
message=Switch[winTest[hLast,cLast],"Win",hScore++;"Youwin","Lose",cScore++;"Youlose",_,"Draw"];
AppendTo[history,{hLast,cLast}];]&,{"Rock","Paper","Scissors"}]],displayPlay[hLast,"You"],Style[message,20],
  displayPlay[cLast,Style["Mathematica",Italic]]},
  {PieChart[{hScore,cScore},ChartLegends->SwatchLegend[{Row[{"You:",hScore}],Row[{Style["Mathematica:",Italic],cScore}]}],
    ChartStyle->{Darker@Green,Darker@Red},ImageSize->110],SpanFromLeft,
    ListLinePlot[Prepend[MeanFilter[(winTest@@@history)/.{"Win"->1,"Lose"->-1,"Draw"->0},
      Ceiling[Length[history]/5]],0],ImageSize->130,
      PlotLabel->"Winrate",AxesOrigin->{0,0},
      PlotRange->{-0.5,0.5}],SpanFromLeft}},ItemSize->{{{Scaled[0.24]}},Automatic},Background->White,Alignment->Center],
  Item[Row[{"",Button["Resethistory",history={};hScore=0;cScore=0;
  message="";cLast=0;hLast=0;]}],Alignment->Right]}],
  Initialization:>(displayPlay[play_,name_]:=Column[{Text[Style[name,18]],
    Show[If[name === "You", ImageReflect[#, Left], #] &@Switch[play,
      1, ExData`Private`石头,
      2, ExData`Private`剪刀,
      3, ExData`Private`布,
      _, Graphics[{}]],ImageSize->{109,70}]},Alignment->Center];
  winTest[p1_,p2_]:=Switch[Mod[p1-p2,3],
    0,"Draw",
    1,"Win",
    2,"Lose"];
  historySeek[history_,n_Integer,col_]:=If[n>Length[history]-1,{},If[#==={},{},#[[1,All,1]]]&
  [Reap[Do[If[history[[i;;(i+n-1),col]]===history[[-n;;,col]],Sow[history[[i+n]]]],
      {i,Length[history]-n}]][[2]]]];prediction[pastChoices_List]:={If[Length[pastChoices]<2,1,
    DistributionFitTest[pastChoices,DiscreteUniformDistribution[{1,3}]]],
    RandomChoice[Commonest[pastChoices/.{}->{1,2,3}]]};
  bestGuess[{}]:=RandomInteger[{1,3}];
  bestGuess[data_]:=Block[{max=Length[data]},Sort[Flatten[Outer[prediction[historySeek[data,#1,#2]]&,Range[max],{1,2,All}],{1,2}]]][[1,-1]];
  chooseGo2[data_]:=Mod[bestGuess[data]+1,3,1];)];


(* ::Subsubsection:: *)
(*扫雷程序块*)
MineLayout[{m_,n_,k_}]:=Block[{M,foo,bar,list},
  M=ConstantArray[0,{m+2,n+2}];
  foo[{x_,y_}]:=M[[x-1;;x+1,y-1;;y+1]]+=1;
  bar[{x_,y_}]:=M[[x,y]]=10;
  list=RandomSample[Tuples[{Range[2,m+1],Range[2,n+1]}]][[1;;k]];
  Do[foo@list[[i]],{i,k}];bar/@list;M[[2;;-2,2;;-2]]];
MineDistribution[m_,n_,k_,p_]:=Transpose@{Range[0,10],BinCounts[Flatten[MineLayout/@ConstantArray[{m,n,k},p]],{-0.5,10.5,1}]/p+0.0};


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
