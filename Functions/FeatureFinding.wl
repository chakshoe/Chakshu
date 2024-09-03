(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


XIntercept::usage = "Finds the x intercepts of a function. Input is xIntercept[function,{x,domain}]";

YIntercept::usage = "Finds the y intercepts of a function. Input is yIntercept[function,{x,domain}]";

statPoints::usage = "\!\(\*
StyleBox[\"statPoints\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)
Finds coordinates and nature of stationary points of a given function.
f: Name of a defined function
Output: Coordinates of stationary point and nature (Max, Min or Inconclusive)

Note: Further work is required if the second derivative is 0 at any of the stationary points.";

statPointsDom::usage = "\!\(\*
StyleBox[\"statPoints\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"a\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"b\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)
Finds coordinates and nature of stationary points of a given function.
f: Name of a defined function
a: Lower bound of restricted area/domain
b: Upper bound of restricted area/domain
Output: Coordinates of stationary point and nature (Max, Min or Inconclusive)

Note: Further work is required if the second derivative is 0 at any of the stationary points.";



(* ::Section:: *)
(*Begin*)


Begin["`Private`"]


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*Feature Finding*)


(* ::Subsubsection:: *)
(*XIntercept*)


XIntercept[exp_, {x_, domain_}] :=
    Module[{},
        Chop /@ Thread[{ForceSolve[{exp == 0, domain}, x], 0}]
    ]

XIntercept[exp_, x_] :=
    Module[{},
        Chop /@ Thread[{ForceSolve[{exp == 0}, x], 0}]
    ]


(* ::Subsubsection:: *)
(*YIntercept*)


YIntercept[exp_, {x_, domain_}] :=
    Module[{},
        Solve[y == exp && x == 0 && domain, {x, y}, Reals][[All, All,
             2]]
    ]


(* ::Subsubsection:: *)
(*statPoints*)


statPoints[f_]:=Module[{x},
{x,f[x],If[f''[x]>0,"Min",If[f''[x]<0,"Max","Inconclusive"]]}
/.Solve[f'[x]==0,x]
]
statPointsDom[f_,a_,b_]:=Module[{x},
{x,f[x],If[f''[x]>0,"Min",If[f''[x]<0,"Max","Inconclusive"]]}
/.Solve[{f'[x]==0,a<=x<=b},x]
]


(* ::Section:: *)
(*End*)


End[]
