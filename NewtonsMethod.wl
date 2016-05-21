(* ::Package:: *)

BeginPackage["NewtonsMethod`"];


NewtonsMethodList::usage = "Returns n iterations of Newton's method on expr at x0.";
NewtonsMethodRoots::usage = "Returns a list of roots of expr.";


Begin["`Private`"];


(* http://mathworld.wolfram.com/NewtonsMethod.html *)
Clear[NewtonsMethodList]
NewtonsMethodList[f_, {x_, x0_}, n_] := 
	NestList[# - Function[x, f][#]/
		Derivative[1][Function[x, f]][#]& , x0, n];


(* This gives me an infinite loop somehow. *)
Clear[NewtonsMethodRoots]
NewtonsMethodRoots[f_, {x_,x0_}, n_] := Module[{roots={},expr=f, deg=Exponent[f,x]},
	While[expr=!=1,
		root = Last[NewtonsMethodList[expr, {x,x0}, n]];
		(*Print[root];*)
		roots = Append[roots, root];
		(*Print[roots];*)
		expr = PolynomialQuotient[expr, x-root, x]
	];
	roots//Round
];


End[];


EndPackage[];
