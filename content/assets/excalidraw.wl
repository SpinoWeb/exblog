(* ::Package:: *)

BeginPackage["excalidraw`"];

exdwRectangle::usage="exdwRectangle[e] return Rectangle"
exdwEllipse::usage="exdwEllipse[e] return Ellipse"
exdwLine::usage="exdwLine[e] return Line"

exdwType::usage="exdwType[e, type] return element"

Begin["`Private`"];

exdwRectangle[e_:<||>]:=Module[{default, key},
	Return[exdwType[e, "rectangle"]];
];

exdwEllipse[e_:<||>]:=Module[{default, key},
	Return[exdwType[e, "ellipse"]];
];

exdwLine[e_:<||>]:=Module[{default, key},
	Return[exdwType[e, "line"]];
];

exdwType[e_:<||>, type_:"rectangle"]:=Module[{elements, element, key},

	element = exdwTypes[type];
	
	Do[{
		key = Keys[e][[i]];
		element[[key]] = e[[key]];
	},{i, 1, Keys[e]//Length}];

	Return[element];
];

exdwTypes[type_]:=Module[{commons, elements, element},

	commons = <|
		"id" -> CreateUUID[],
		"x" -> 0,"y" -> 0,
		"width" -> 100,"height" -> 100, "angle" -> 0,
		"strokeColor" -> "#000000","backgroundColor" -> "transparent",
		"fillStyle"->"solid",
		"strokeWidth"->2,"strokeStyle"->"solid",
		"roughness"->0,
		"opacity"->100,
		"groupIds"->{},
		"roundness"->Null,
		"isDeleted"->False,
		"boundElements"->Null,
		"updated"->UnixTime[],
		"link"->Null,
		"locked"->False
	|>;

	elements = <|
		"rectangle" -> <|
			"type"->"rectangle"
		|>,		
		"ellipse" -> <|			
			"type"->"ellipse",
			"width" -> 10,"height" -> 10,
			"roundness"-> <|
				"type" -> 2
			|>
		|>,
		"line" -> <|			
			"type"->"line",
			"roundness"-> <|
				"type" -> 2
			|>,
			"points" -> {{0,0},{100,0}},
			"lastCommittedPoint"-> Null,
            "startBinding"-> Null,
            "endBinding"-> Null,
            "endArrowhead"-> Null
		|>,
		"arrow" -> <|			
			"type"->"arrow",
			"roundness"-> <|
				"type" -> 2
			|>,
			"points" -> {{0,0},{100,0}},
			"lastCommittedPoint"-> Null,
            "startBinding"-> Null,
            "endBinding"-> Null,
            "endArrowhead"-> "arrow"
		|>,
		"text" -> <|			
			"type" -> "text",
			"height" -> 50,		
            "fontSize"-> 28,
            "fontFamily"-> 1,
            "textAlign"-> "left",
            "verticalAlign"-> "top",
            "containerId"-> Null,
            "text" -> "text",
            "lineHeight"-> 1.25
		|>
	|>;	
	
	(* get element *)
	element = If[KeyExistsQ[elements, type], elements[type], <||>];
	
	(* join commons and element *)
	element = Join[commons, element];

	Return[element];
];

End[];
EndPackage[];
