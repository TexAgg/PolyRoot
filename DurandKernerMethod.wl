(* ::Package:: *)

(* :Title: Durand-Kerner Method *)
(* :Context: DurandKernerMethod` *)
(* :Author: Matt Gaikema *)
(* :Date: 5/31/2016 *)
(* :Mathematica Version: 10.4 *)
(* :Keywords: Roots, Polynomials *)


(* ::Section:: *)
(*Initialization functions*)


RootRadius[p0_,x_] := Module[{n, coef, ak, tk, elem},
	n = Exponent[expr,x];
	coef = CoefficientList[expr,x];
	ak = Last[coef]//Abs;
	tk = Take[coef,n]//Abs;
	elem = Total[tk]*(1/ak);
	Max[1,elem]
]


InitialGuess[p0_,x_] := Module[{n, \[Theta], R},
	n = Exponent[expr,x];
	\[Theta] = Range[0,n-1] * 2 Pi/n;
	R = RootRadius[expr,x];
	R*(Cos[\[Theta]]+I Sin[\[Theta]])
]


(* ::Section:: *)
(*Root-finding*)


(*
DKFindRoots[p0_,x_] := Module[{},
]
*)
