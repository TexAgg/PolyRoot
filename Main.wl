(* ::Package:: *)

(* ::Title:: *)
(*Finding Roots of Polynomials*)


(* ::Subtitle:: *)
(*Matt Gaikema*)


(* https://en.wikipedia.org/wiki/Root-finding_algorithm *)


(* Clear variables. *)
ClearAll["Global`*"]
(* Limit variable scope to just this document. *)
SetOptions[EvaluationNotebook[], CellContext -> Notebook]
(* Set working directory to this one. *)
SetDirectory[NotebookDirectory[]]
Import["NewtonsMethod.wl"]
Import["CubicFunctions.wl"]


(* ::Section:: *)
(*Cubic Functions*)


expr = 2x^3-3x^2-3x+2


CubicRoot[expr,x]


Solve[expr==0,x]


(* ::Section::Closed:: *)
(*Newton's Method*)


(* ::Text:: *)
(*Newton's Method works on a real-valued function, but can be extended to the complex plane. *)
(*It does not actually find the precise roots, it only approximates them.*)


expr = (x-5)(x+6)(x+1)//Expand


NewtonsMethodList[expr,{x,0},10];
rt = N[%]//Last


expr===1


(* ::Subsection::Closed:: *)
(*NewtonsMethodRoots test*)


expr
NewtonsMethodRoots[expr,{x,0},5]


(* ::Section::Closed:: *)
(*Durand-Kerner Method*)


(* https://en.wikipedia.org/wiki/Durand%E2%80%93Kerner_method *)


(* ::Section:: *)
(*Analytic Functions*)


(* ::Subsection:: *)
(*Argument Principle*)


Clear[expr]
expr = z^5-2 z+3
(* Find the roots. *)
NSolve[expr==0,z];
(* All of the roots are within a circle of radius 1.5. *)
roots = z/.%
Abs[%]


c = Circle[{0,0},1.5]


Clear[n]
n = 0;
integrand = z^n D[expr,z]/expr
(* Find where the denominator is 0, AKA the poles. *)
Solve[Denominator[Simplify[%]]==0,z];
poles = z/.%;
Select[poles,Abs[#]<1.5&];
Residue[integrand,{z,#}]&/@%;
(* This is the number of roots in the region. *)
Total[%]


(* ::Subsection:: *)
(*Numerically finding roots*)


(* ::Section::Closed:: *)
(*Misc*)


(* https://reference.wolfram.com/language/ref/format/LaTeX.html *)
(*Import["report.tex","OutputFile"\[Rule]"Main.nb"]*)
