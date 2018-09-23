(* ::Package:: *)

BeginPackage["AdvancedCloudLogging`"];

CloudLoggingWrapper::usage = "CloudLoggingWrapper is used to perform detailed logging for the evaluation of an expression.";
ImportCloudLog::usage = "ImportCloudLog is used to read the data found in a log file created by CloudLoggingWrapper and returns a Dataset.";
MessageHistogram::usage = "MessageHistogram can either take a log file created by CloudLoggingWrapper or a Dataset returned by ImportCloudLog and returns a DateHistogram of the messages, grouped by Channel.";

Begin["`Private`"];

(* CloudLoggingWrapper is HoldFirst to allow the expression to be evaluated within the logging Block. *)
SetAttributes[CloudLoggingWrapper,HoldFirst];
(**
 * If no log is provided, a log file will be created. 
 * If the evaluation is in the cloud, then the log will be an unnamed CloudObject.
 * If the evaluation is in the desktop, then the log will be an unnamed file. (Used for testing purposes)
 *)
CloudLoggingWrapper[expr_] := CloudLoggingWrapper[expr, If[$CloudEvaluation, CloudObject[], CreateFile[]]]
CloudLoggingWrapper[expr_, log_] :=
	Module[{stream, streams, id=Hash[UUID`UUID[], "Expression", "Base64Encoding"],
		result, caller, name},
		(* Determine the name used for the stream *)
		name = Switch[log,
			(* CloudObjects need to be created and then return the path. *)
			_CloudObject,
			If[!FileExistsQ[log],
				CloudExport["", "text/plain", log];
			];
			FileNameDrop[CloudObjectInformation[log, "Path"], 1],
			(* Files will use just the path. *)
			_File,
			First[log],
			(* Strings will be assumed to be a path. *)
			_String,
			log,
			(* If the log is anything else, then it is invalid. *)
			_,
			Return[$Failed]
		];
		(* Construct the caller based on environment *)
		caller = Which[
			(* Cloud evaluations will use the $EvaluationCloudObject's path *)
			$CloudEvaluation,
			FileNameDrop[CloudObjectInformation[$EvaluationCloudObject, "Path"], 1],
			(* Notebook session will use the notebook's path. *)
			StringMatchQ[$EvaluationEnvironment, "Session"],
			NotebookFileName[],
			(* Anything else is termed "unknown" *)
			True,
			"unknown"
		];
		(* Create the main output stream. It is OpenAppend to allow for continuous logging. *)
		stream = OpenAppend[name, FormatType -> OutputForm, PageWidth -> Infinity, CharacterEncoding -> "UTF-8"];
		(* Create the sub-streams for the different channels *)
		streams = Map[createLoggingStream[name, id, #, caller]&, {"Output", "Messages", "Urgent"}];
		(* The sub-streams are Appended rather than Set in order to still allow for internal cloud logging. *)
		Block[{
			$Output = Append[$Output, streams[[1]]],
			$Messages = Append[$Messages, streams[[2]]],
			$Urgent = Append[$Urgent,streams[[3]]]},
			(* If this is a cloud evaluation, log the request data. *)
			If[$CloudEvaluation,
				Write[stream, constructLoggingEntry[HTTPRequestData[], id, "Info", caller]]
			];
			(* Save the result to be returned, this allows things like APIFunction to still behave normally. *)
			result = expr;
			(* Write the result to the log. *)
			Write[stream, constructLoggingEntry[StringJoin["Result: ", ToString[result]], id, "Info", caller]]
		];
		(* Close the main output stream. *)
		Close[stream];
		(* Close the sub-streams. *)
		Close/@streams;
		(* Return the result *)
		result
	]

(* Create an OutputStream using the "CloudLogging" method. *)
createLoggingStream[file_, id_, channel_] :=
	OpenWrite[StringRiffle[{file, id, channel}, ":"], Method -> "CloudLogging"]
createLoggingStream[file_, id_, channel_, caller_] :=
	OpenWrite[StringRiffle[{file, id, channel, caller},":"], Method -> "CloudLogging"]
	
(* Define the OutputStream method that uses a nicely formatted logging message. *)
DefineOutputStreamMethod[
	"CloudLogging",
	{
		"ConstructorFunction"->
			Function[{stream, isAppend, caller, opts},
				Module[{state=<||>, file, input},
					input=StringSplit[stream, ":"];
					file = input[[1]];
					state["uuid"] = input[[2]];
					state["channel"] = input[[3]];
					If[Length[input] == 4,
						state["caller"] = input[[4]],
						state["caller"] = "unknown"
					];
					state["stream"] = First[Cases[Streams[file], _OutputStream]];
					{True,state}
				]
			],
		"CloseFunction" -> Function[state, Quiet[Close[state["stream"]]];],
		"StreamPositionFunction" -> Function[state, {StreamPosition[state["stream"]], state}],
		"WriteFunction"->
			Function[{state, bytes},
				Module[{output, outBytes, result, nBytes, string},
					string = StringTrim[FromCharacterCode[bytes]];
					If[StringLength[string] > 0,
						output = constructLoggingEntry[FromCharacterCode[bytes], state["uuid"], state["channel"], state["caller"]];
						outBytes = ToCharacterCode[output];
						result = Write[state["stream"], output];
						nBytes = If[result === state["stream"], Length[outBytes], 0];
						{nBytes, state},
						{0, state}
					]
				]
			]
		}
	];

constructLoggingEntry[value_, uuid_String, channel_String] :=
	constructLoggingEntry[value, uuid, channel, "None"]
constructLoggingEntry[message_String, uuid_String, "Messages", caller_String] :=
	StringJoin["[", ToString[AccountingForm[Floor[AbsoluteTime[]]]], "][", uuid, "][Messages][", caller, "]", formatMessage[message]]
constructLoggingEntry[str_String, uuid_String, channel_String, caller_String] :=
	StringJoin["[", ToString[AccountingForm[Floor[AbsoluteTime[]]]], "][", uuid, "][", channel, "][", caller, "]", str]
constructLoggingEntry[value_, uuid_String, channel_String, caller_String] :=
	constructLoggingEntry[
		ToString[
			value,
			FormatType -> "OutputForm",
			CharacterEncoding -> "UTF-8",
			PageWidth -> \[Infinity]
		],
		uuid,
		channel,
		caller
	]
	
formatMessage[message_String] :=
	Quiet[
		With[{m = ToExpression[message]},
			StringJoin[
				m[[1,1,1]],
				"::",
				m[[1,1,2]],
				": ",
				formatText[m[[1,1,3]]]
			]
		]
	]

formatText[text_] :=
	StringReplace[
		text,
		{
			"\\!\\(\\*RowBox[{\\\"" ~~ Shortest[value__] ~~ "\\\"}]\\)" :> value,
			"\\\"" -> "",
			"\"" -> "",
			"\n" -> "",
			"\t" -> "",
			"     " -> ""
		}
	]
	
ImportCloudLog[log_] :=
	With[{content = Import[log, "String"]},
		importLogData[content]
	]

importLogData[data_String] := 
	Dataset[
			StringCases[
				data,
				StartOfLine~~"["~~Shortest[time:DigitCharacter..]~~"]["~~Shortest[id__]~~"]["~~Shortest[channel__]~~"]["~~Shortest[path__]~~"]"~~Shortest[value__]~~EndOfLine :>
					<|"Time" -> DateObject[FromDigits[time]], "Evaluation" -> id, "Channel" -> channel, "Object" -> path, "Message" -> value|>
			]
		]
		
MessageHistogram[data_Dataset] :=
	DateHistogram[
		Normal[data[GroupBy[#Channel&]][All, All, "Time"]],
		ChartLayout -> "Stacked",
		ImageSize -> Full,
		PlotTheme -> {"Wide", "Business"}
	]
MessageHistogram[log_] :=
	With[{data = Check[ImportCloudLog[log], Return[$Failed]]},
		MessageHistogram[data]
	]

End[];

EndPackage[];
