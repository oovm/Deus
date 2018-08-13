Deus$ResourcesList::usage = "";

Begin["`Resources`"];
Deus$ResourcesList = <|
	"SudokuMega" -> <|
		"Remote" -> "https://m.vers.site/Resources/Deus/SudokuMega.wxf",
		"Local" -> FileNameJoin[{$UserBaseDirectory, "Applications", "Deus", "Resources", "exe", "Sudoku", "SudokuMega.wxf"}]
	|>
|>;

SetAttributes[taskProgress, HoldFirst];
taskProgress[manifest_][event_] := manifest = <|manifest, event["Task"] -> event|>;

SetAttributes[taskFinished, HoldFirst]
taskFinished[manifest_][event_] := manifest = <|manifest, event["Task"] -> event|>;

SetAttributes[startJob, HoldFirst];
startJob[manifest_][src_, dest_] := URLDownloadSubmit[
	src, dest,
	HandlerFunctions -> <|
		"ConnectionFailed" -> connectionFailed[manifest],
		"CookiesReceived" -> cookiesReceived[manifest],
		"HeadersReceived" -> headersReceived[manifest],
		"TaskFinished" -> taskFinished[manifest],
		"TaskProgress" -> taskProgress[manifest],
		"TaskStatusChanged" -> taskStatusChanged[manifest]
	|>,
	HandlerFunctionsKeys -> {
		"Task", "TaskStatus", "File",
		"ByteCountTotal", "ByteCountDownloaded", "FractionComplete"
	}
];
SetAttributes[abortDownload, HoldFirst]
abortDownload[manifest_, task_] := (
	TaskRemove /@ Select[Tasks[], #["TaskUUID"] === task["TaskUUID"] &];
	manifest = <|
		manifest,
		task -> <|manifest[task], "TaskStatus" -> "Aborted"|>
	|>
);

SetAttributes[visualizeManifest, HoldFirst]
visualizeManifest[manifest_] := TableForm[Join[
	{{"File", "Size (MB)", "Downloaded (MB)", "Fraction complete",
		"Status", ""}}, {
		FileNameTake[#File]
		, Floor[#ByteCountTotal / 10^6]
		, Floor[#ByteCountDownloaded / 10^6]
		, ProgressIndicator[#FractionComplete]
		, #TaskStatus
		, Button["Abort", abortDownload[manifest, #Task],
			Enabled -> (#TaskStatus =!= "Aborted")]
	} & /@ Values[manifest]
]];

(*Todo:local resource check*)
(*Todo:download from remote*)
Block[
	{},
	If[!AssociationQ[manifest],manifest = <||>];
	startJob[manifest][Deus$ResourcesList["SudokuMega","Remote"],Deus$ResourcesList["SudokuMega","Local"]];
	Dynamic@visualizeManifest[manifest]
];

End[]
