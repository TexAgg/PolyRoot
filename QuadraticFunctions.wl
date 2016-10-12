(* ::Package:: *)

(* :Title: Quadratic Functions *)
(* :Context: QuadraticFunctions` *)
(* :Author: Matt Gaikema *)
(* :Date: 5/28/2016 *)
(* :Mathematica Version: 10.4 *)
(* :Keywords: Roots, Quadratic Polynomials *)


BeginPackage["QuadraticFunctions`"]


QuadraticRoots::usage = "Find the roots of a quadratic polynomial using the quadratic formula."


Begin["`Private`"]


QuadraticRoots[f_,x_] := Module[{coef, a, b, c, roots},
	roots = {};
	coef = CoefficientList[f,x]//Reverse;
	a = coef[[1]];
	b = coef[[2]];
	c = coef[[3]];
	
	AppendTo[roots,
		(-b + Sqrt[b^2 - 4 a c])/(2 a)];
	AppendTo[roots, 
		(-b - Sqrt[b^2 - 4 a c])/(2 a)];
	roots
]


End[]


EndPackage[]
