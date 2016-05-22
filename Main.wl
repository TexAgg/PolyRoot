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


(* ::Section:: *)
(*Durand-Kerner Method*)


(* https://en.wikipedia.org/wiki/Durand%E2%80%93Kerner_method *)
