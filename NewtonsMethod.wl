(* ::Package:: *)

(* :Title: Newton's Method *)
(* :Context: NewtonsMethod` *)
(* :Author: Matt Gaikema *)
(* :Date: 5/19/16 *)
(* :Mathematica Version: 10.4 *)
(* :Keywords: Roots, Polynomials *)


BeginPackage["NewtonsMethod`"];


NewtonsMethodList::usage = "Returns n iterations of Newton's method on expr at x0.";
NewtonsMethodRoots::usage = "Returns a list of roots of expr.";


Begin["`Private`"];


(* http://mathworld.wolfram.com/NewtonsMethod.html *)
Clear[NewtonsMethodList]
NewtonsMethodList[f_, {x_, x0_}, n_] := 
	NestList[# - Function[x, f][#]/
		Derivative[1][Function[x, f]][#]& , x0, n];


(* If n is too large (n>5), this becomes reallllly slow. *)
Clear[NewtonsMethodRoots]
NewtonsMethodRoots[f_, {x_,x0_}, n_] := Module[{roots={},expr=f,root},
	While[expr=!=1,
		root = Last[NewtonsMethodList[expr, {x,x0}, n]];
		(*Print[root];*)
		roots = Append[roots, root];
		(*Print[roots];*)
		expr = PolynomialQuotient[expr, x-root, x];
	];
	roots//Round
];


End[];


EndPackage[];
