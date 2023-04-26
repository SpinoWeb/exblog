(* ::Package:: *)

BeginPackage["utilities`"];

uDebug::usage="uDebug[s] quit evaluation after s seconds"
uPolygonCentroid::usage="uPolygonCentroid[vertex] return centroid of polygon"
uAppendTo::usage="uAppendTo[m, mi] append m to mi"

Begin["`Private`"];

uDebug[s_:1]:=Module[{},Pause[s];Quit[];Return[];];

uPolygonCentroid[vertex_]:=Module[{pts,first, last, j, i, twicearea=0, x=0, y=0, p1, p2, f},
	pts = vertex;
	If[0 == 1, Print["uPolygonCentroid > pts: ", pts]];

	first = First[pts];
	last = Last[pts];
	If[first[[1]]!=last[[1]], AppendTo[pts,first]];

	j = Length[pts];
	For[i=1,i<Length[pts],i++,{
		j=i+1;

		p1 = pts[[i]]; p2 = pts[[j]];
		f=p1[[1]] p2[[2]]-p2[[1]] p1[[2]];
		twicearea+=f;
		x+=(p1[[1]]+p2[[1]])f;
		y+=(p1[[2]]+p2[[2]])f;
	}];

	f = 3 twicearea;

	Return[{x/f, y/f, Abs[twicearea]/2 }];
];

uAppendTo[m_, a_, vars_]:=Module[{nm, mv, v, var, defaultValue, value},	
	If[0 == 1, Print["uAppendTo > m: ", m]];
	If[0 == 1, Print["uAppendTo > a: ", a]];
	If[0 == 1, Print["uAppendTo > vars: ", vars]];
		
	mv = {};
	Do[{
		var = vars[[v]];
		
		defaultValue = If[var == "GUID",  CreateUUID[], Null];
		
		value = If[KeyExistsQ[a, var], a[[var]], defaultValue]; 
		AppendTo[mv, value];
		
	},{v, 1, Length[vars]}];
	If[0 == 1, Print["uAppendTo > mv: ", mv]];
	
	nm = m;
	AppendTo[nm, mv];
	
	Return[nm];
];

End[];
EndPackage[];
