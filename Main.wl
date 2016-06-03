(* ::Package:: *)

(* ::Title:: *)
(*Finding Roots of Polynomials*)


(* ::Subtitle:: *)
(*Matt Gaikema*)


(* ::Text:: *)
(*Various root-finding algorithms are tested here.*)
(*These are implimented in other packages.*)
(**)
(*https://bitbucket.org/gaikema/polyroot/src*)
(*https://github.com/TexAgg/PolyRoot*)


(* https://en.wikipedia.org/wiki/Root-finding_algorithm *)


(* Clear variables. *)
ClearAll["Global`*"]
(* Limit variable scope to just this document. *)
SetOptions[EvaluationNotebook[], CellContext -> Notebook]
(* Set working directory to this one. *)
SetDirectory[NotebookDirectory[]]
Import["NewtonsMethod.wl"]
Import["CubicFunctions.wl"]
Import["GraeffesMethod.wl"]
Import["QuadraticFunctions.wl"]


(* ::Section:: *)
(*General Formulas*)


(* ::Subsection:: *)
(*Quadratic Functions*)


expr = x^2+2x+1
QuadraticRoots[expr,x]


(* ::Subsection::Closed:: *)
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


(* ::Subsection:: *)
(*NewtonsMethodRoots test*)


expr
NewtonsMethodRoots[expr,{x,0},5]


(* ::Section::Closed:: *)
(*Graeffe's Method*)


(* https://en.wikipedia.org/wiki/Graeffe%27s_method *)
(* http://mathworld.wolfram.com/GraeffesMethod.html *)
(* Also done here in Mathematica: 
http://mathfaculty.fullerton.edu/mathews/n2003/GraeffeMethodMod.html *)


Clear[x,expr]
expr = 24 + 14x - 13x^2 -2x^3 + x^4


(* The 1st and second Graeffe Iterations. *)
GraeffeIteration[expr,x,1]//Expand
GraeffeIteration[expr,x,2]//Expand


Solve[expr==0,x]
GraeffeFindRoots[expr,x,2]//N


(* ::Text:: *)
(*The signs of the roots can be found by evaluating p at the approximations.*)
(*The higher the number of iterations, the more accurate Graeffe's Method is.*)


(* ::Section:: *)
(*Durand-Kerner Method*)


(* https://en.wikipedia.org/wiki/Durand%E2%80%93Kerner_method *)
(* https://ezekiel.encs.vancouver.wsu.edu/~cs330/projects/dk/DK-method.pdf *)


expr = x^3-3x^2+3x-5


(* These are the real roots. *)
Solve[expr==0,x]//N


CoefficientList[expr,x]//Last


(* ::Subsection:: *)
(*Public Test*)


(* Use Rouche's Theorem to find a disk containing all the roots. 
https://en.wikipedia.org/wiki/Rouch%C3%A9%27s_theorem 
https://en.wikipedia.org/wiki/Properties_of_polynomial_roots#Based_on_the_Rouch.C3.A9_theorem *)

Module[{n,coef,ak,tk,elem,R,z0,Q,f},
	f = Function[x,Evaluate[expr]];
	Print[f[x]];
	n = Exponent[expr,x];
	coef = CoefficientList[expr,x];
	ak = Last[coef]//Abs;
	tk = Take[coef,n]//Abs;
	elem = Total[tk]*(1/ak);
	R = Max[1,elem];
	Print[R];
	\[Theta] = Range[0,n-1] * 2 Pi/n;
	Print[\[Theta]];
	(* These are the initial guesses. *)
	z0 = R*(Cos[\[Theta]]+I Sin[\[Theta]]);

	Q = {};
	For[j=1,j<=n,j++,
		AppendTo[Q,Times@@Delete[z0,j]];
	];
	Print[Q];
	z1 = z0 - f[z0]/Q
]


N[%]


(* ::Section::Closed:: *)
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


(* Convert TeX document to Mathematica notebook because why not.
https://reference.wolfram.com/language/ref/format/LaTeX.html *)
(* Import["report.tex","OutputFile"\[Rule]"report.nb"]; *)
