(* ::Package:: *)

(* ::Title:: *)
(*Finding Roots of Polynomials*)


(* ::Subtitle:: *)
(*Matt Gaikema*)


(* Clear variables. *)
ClearAll["Global`*"]
(* Limit variable scope to just this document. *)
SetOptions[EvaluationNotebook[], CellContext -> Notebook]
(* Set working directory to this one. *)
SetDirectory[NotebookDirectory[]]
Import["NewtonsMethod.wl"]


(* ::Section:: *)
(*Newton's Method*)


expr = (x-5)(x+6)(x+1)//Expand


NewtonsMethodList[expr,{x,0},10];
rt = N[%]//Last


(* ::Subsection:: *)
(*Iterative test*)


rts = {};
rts = Append[rts,rt]


q = PolynomialQuotient[expr,x-rt,x]
rt = NewtonsMethodList[%,{x,0},10]//Last
rts = Append[rts,rt]


q = PolynomialQuotient[q,x-rt,x]
rt = NewtonsMethodList[q,{x,0},10]//Last
rts = Append[rts,rt]


q = PolynomialQuotient[q,x-rt,x]


(* ::Subsection:: *)
(*NewtonsMethodRoots test*)


NewtonsMethodRoots[expr,{x,0},10]


Solve[expr==0,x];
N[%]
