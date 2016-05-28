(* ::Package:: *)

(* :Title: Graeffe's Method *)
(* :Context: GraeffesMethod` *)
(* :Author: Matt Gaikema *)
(* :Date: 5/27/2016 *)
(* :Mathematica Version: 10.4 *)
(* :Keywords: Roots, Polynomials *)


BeginPackage["GraeffesMethod`"]


GraeffeIteration::usage = "Compute the rth Graeffe Iteration of a polynomial."
GraeffeFindRoots::usage = "Find the roots of a polynomial using r Graeffe Iterations."


Begin["`Private`"]


(* https://reference.wolfram.com/language/ref/SymmetricPolynomial.html *)


Clear[GraeffeIteration]

GraeffeIteration[p0_,x_,1] := Module[{p,n},
	p = Function[x, Evaluate[p0]];
	n = Exponent[p0,x];
	(-1)^n p[Sqrt[x]] p[-Sqrt[x]] 
]

GraeffeIteration[p0_,x_,r_] := Module[{p,n},
	p = Function[x, Evaluate[p0]];
	n = Exponent[p,x];
	(-1)^n GraeffeIteration[p[Sqrt[x]], x, r-1] GraeffeIteration[p[-Sqrt[x]], x, r-1]
]


Clear[GraeffeFindRoots]
GraeffeFindRoots[p0_,x_,r_] := Module[{pr, ar,\[Zeta]},
	pr = GraeffeIteration[p0,x,r];
	ar = CoefficientList[pr,x];
	\[Zeta] = {};
	For[i=1,i<Length[ar],i++,
		AppendTo[\[Zeta], Abs[ar[[i]]/ar[[i+1]]]^(1/(2^r))];
	];
	\[Zeta]
]


End[]


EndPackage[]
