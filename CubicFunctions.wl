(* ::Package:: *)

(* :Title: Cubic Functions *)
(* :Context: CubicFunctions` *)
(* :Author: Matt Gaikema *)
(* :Mathematica Version: 10.4 *)
(* :Keywords: Roots, Cubic Polynomials *)


BeginPackage["CubicFunctions`"]


CubicRoot::usage = "Calculates the roots of a cubic function using the formual."


Begin["`Private`"]


Clear[CubicRoot]
CubicRoot[f_,x_] := Module[
{
	expr=f,
	(* Roots of unity. *)
	uk = {
		1,
		(-1+I Sqrt[3])/2,
		(-1-I Sqrt[3])/2
	}
},
	(* coef[[1]]=a, coef[[2]]=b, coeff[[3]]=c, coeff[[4]]=d. *)
	coef = CoefficientList[expr,x]//Reverse;
	
	\[CapitalDelta]0 = coef[[2]]^2-3 coef[[1]] coef[[3]];
	\[CapitalDelta]1 = 2 coef[[2]]^3 - 9 coef[[1]] coef[[2]] coef[[3]] + 27 coef[[1]]^2 coef[[4]];
	
	c = Power[
		(\[CapitalDelta]1 + Sqrt[\[CapitalDelta]1^2-4\[CapitalDelta]0^3])/2
	,1/3]//N;
	
	xk = -1/(3 coef[[1]]) (coef[[2]] + uk c + \[CapitalDelta]0/(uk c))
];


End[]


EndPackage[]
