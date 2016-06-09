(* ::Package:: *)

(* :Title: Durand-Kerner Method *)
(* :Context: DurandKernerMethod` *)
(* :Author: Matt Gaikema *)
(* :Date: 5/31/2016 *)
(* :Mathematica Version: 10.4 *)
(* :Keywords: Roots, Polynomials *)


BeginPackage["DurandKernerMethod`"]


DKFindRoots::usage = "Perform n iterations of the Durand-Kerner method to find the roots of a function."


(*Begin["`Private`"]*)


(* ::Section:: *)
(*Initialization functions*)


(* ::Text:: *)
(*These functions are used in the first iteration of the method.*)


Clear[RootRadius]
RootRadius[p0_, x_] := Module[{n, coef, ak, tk, elem},
	n = Exponent[expr,x];
	coef = CoefficientList[expr,x];
	ak = Last[coef]//Abs;
	tk = Take[coef,n]//Abs;
	elem = Total[tk]*(1/ak);
	Max[1,elem]
]


Clear[InitialGuess]
InitialGuess[p0_, x_] := Module[{n, \[Theta], R},
	n = Exponent[expr,x];
	\[Theta] = Range[0,n-1] * 2 Pi/n;
	R = RootRadius[expr,x];
	R*(Cos[\[Theta]]+I Sin[\[Theta]])
]


(* ::Section:: *)
(*Iterative functions*)


(* ::Text:: *)
(*These functions are called at each iteration.*)


Clear[QList]
QList[p0_, x_, z0_] := Module[{qj, n},
	n = Exponent[p0, x];
	qj = {};
	For[j=1,j<=n,j++,
		AppendTo[qj,Times@@Delete[z0,j]];
	];
	qj
]


(* ::Section:: *)
(*Root-finding*)


Clear[DKFindRoots]

DKFindRoots[p0_, x_, 0] := Module[{z0},
	z0 = InitialGuess[p0, x]
]

DKFindRoots[p0_, x_, n_] := Module[{f, zj},
	f = Function[x, Evaluate[p0]];
	zj = DKFindRoots[p0, x, n-1];
	zj - f[zj]/QList[p0, x, zj]
]


(*End[]*)


EndPackage[]
