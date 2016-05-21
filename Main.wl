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


expr===1


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
q=!=1


(* ::Subsection:: *)
(*NewtonsMethodRoots test*)


expr
NewtonsMethodRoots[expr,{x,0},10]


(* It works here but not in the package. *)
Clear[rt,q,rts]
q = expr;
rts = {};
While[q=!=1,
	rt = Last[NewtonsMethodList[q, {x,0}, 5]];
	(*Print[rt];*)
	rts = Append[rts, rt];
	(*Print[rts];*)
	q = PolynomialQuotient[q, x-rt, x]
]
rts//Round


(* This works too! *)
Module[{rts={},q=expr},
	While[q=!=1,
		rt = Last[NewtonsMethodList[q, {x,0}, 5]];
		(*Print[rt];*)
		rts = Append[rts, rt];
		(*Print[rts];*)
		q = PolynomialQuotient[q, x-rt, x]
	];
	rts//Round
]


Solve[expr==0,x];
N[%]
