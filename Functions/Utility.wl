(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


LinearSystem::usage = "Given a set of simultaneous equations, LinearSystem finds instances where there are infinite or no intersections. Usage is LinearSystem[{equations},{x,y},param]";

FTest::usage = "Tests a function against given statements about the function. Usage is FTest[f[x],{prop1,prop2...},{f,x}]";

RFTest::usage = "Tests various functions against one statement to check if it is true. Usage is RFTest[prop,{f1[x],f2[x]...},f,x]";

RatDenom::usage = "Rationalizes an expression.";

ToDeg::usage = "Converts radians to degrees.";

ToRad::usage = "Converts degrees to radians.";

Transform::usage = "Applies a transformation to a function. Transform[function,{x,y},M,b] where M is the 2x2 dilation matrix and b is the 2x1 translation column.";

aveROC::usage = "\!\(\*
StyleBox[\"aveROC\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"a\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"b\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)
Calculate the Average Rate of Change of a function on the interval x\[Element][a,b]
f: Name of a defined function
a: Left endpoint of interval
b: Right endpoint of interval
Output: Average rate of change of f over the interval. Exact if exact input given.";

averageValue::usage = "\!\(\*
StyleBox[\"averageValue\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"a\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"b\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)
Calculate the Average Value of a function on the interval x\[Element][a,b]
f: Name of a defined function
a: Left endpoint of interval
b: Right endpoint of interval
Output: Average Value of f over the interval. Exact if exact input given.";

riemannLEE::usage = "\!\(\*
StyleBox[\"riemannLEE\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"a\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"b\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[IndentingNewLine]Computes the Riemann Sum for the Left Endpoint Estimate of the signed area between a function and the x-axis over the interval x\[Element][a,b].\[IndentingNewLine]f: Name of a defined function
a: Left endpoint of interval
b: Right endpoint of interval
n: Number of rectangles required
Output: Left Endpoint Estimate of Area. Exact if exact input given";

riemannREE::usage = "\!\(\*
StyleBox[\"riemannREE\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"a\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"b\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[IndentingNewLine]Computes the Riemann Sum for the Right Endpoint Estimate of the signed area between a function and the x-axis over the interval x\[Element][a,b].\[IndentingNewLine]f: Name of a defined function
a: Left endpoint of interval
b: Right endpoint of interval
n: Number of rectangles required
Output: Right Endpoint Estimate of Area. Exact if exact input given";

riemannTEE::usage = "\!\(\*
StyleBox[\"riemannTE\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"a\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"b\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[IndentingNewLine]Computes the Riemann Sum for the Trapezoidal Estimate of the signed area between a function and the x-axis over the interval x\[Element][a,b].\[IndentingNewLine]f: Name of a defined function
a: Left endpoint of interval
b: Right endpoint of interval
n: Number of rectangles required
Output: Trapezoidal Estimate of Area. Exact if exact input given";

compSqu::usage = "\!\(\*
StyleBox[\"compSqu\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"x\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)
Returns the completed square version of a given quadratic
f: Name of a defined quadratic function
x: Variable to use in the output
Output: A quadratic in turning point form\[IndentingNewLine]
Note: There is no input error checking. Must input a quadratic and a cleared variable.
";


(* ::Section:: *)
(*Begin*)


Begin["`Private`"]


(* ::Section:: *)
(*Definitions*)


(* ::Subsection::Closed:: *)
(*Transform*)


(* ::Subsubsection::Closed:: *)
(*Transform*)


Transform[f_, {x_, y_}, m_, b_] :=
    Module[{tvec, xd, yd},
        tvec = Solve[{xd, yd} == m . {x, y} + b, {x, y}][[1, All, 2]]
            ;
        Solve[tvec[[2]] == f /. x -> tvec[[1]], yd][[1, 1, 2]] /. {yd
             -> y, xd -> x}
    ]

Transform[f_, {x_, y_}, matrix_] :=
    Module[{tvec, xd, yd},
        tvec = Solve[{xd, yd} == matrix, {x, y}][[1, All, 2]];
        Solve[tvec[[2]] == f /. x -> tvec[[1]], yd][[1, 1, 2]] /. {yd
             -> y, xd -> x}
    ]


(* ::Subsection::Closed:: *)
(*averageValue and aveROC*)


(* ::Subsubsection:: *)
(*averageValue*)


averageValue[f_,a_,b_]:=Module[{max,min,x},
min=Min[{a,b}];
max=Max[{a,b}];
1/(max-min) \!\(
\*SubsuperscriptBox[\(\[Integral]\), \(min\), \(max\)]\(f[x] \[DifferentialD]x\)\)
]


(* ::Subsubsection:: *)
(*aveROC*)


aveROC[f_,a_,b_]:=Module[{},
If[b!=a,(f[Max[{a,b}]]-f[Min[{a,b}]])/(Max[{a,b}]-Min[{a,b}]),"Error"]
]


(* ::Subsection::Closed:: *)
(*LinearSystem*)


(* ::Subsubsection::Closed:: *)
(*LinearSystem*)


LinearSystem[eqs_, {x_, y_}, par_] :=
    Module[{det, InfOrNone},
        det = Det[Normal[CoefficientArrays[eqs, {x, y}][[-1]]]];
        InfOrNone = Solve[det == 0, par][[All, 1, 2]];
        Table[
                If[Quiet @ Solve[eqs /. par -> InfOrNone[[i]], {x, y}
                    ] == {},
                    ToString[par] <> "=" <> ToString[InfOrNone[[i]], 
                        TraditionalForm] <> " - No Solution"
                    ,
                    ToString[par] <> "=" <> ToString[InfOrNone[[i]], 
                        TraditionalForm] <> " - Infinite solutions"
                ]
                ,
                {i, 1, Length[InfOrNone]}
            ] // TableForm
    ];


(* ::Subsection::Closed:: *)
(*FTest*)


(* ::Subsubsection:: *)
(*FTest*)


FTest[fx_, prop_, {f_:f, x_:x}] :=
    Module[{refx, g, reprop, dom},
        g[y_] := fx /. {x -> y};
        reprop = prop /. f[var_] :> g[var];
        reprop = FullSimplify /@ reprop;
        TableForm @
            Table[
                If[BooleanQ[reprop[[i]]] && TrueQ[reprop[[i]]],
                    ToString[prop[[i]], TraditionalForm] <> " is true"
                        
                    ,
                    ToString[prop[[i]], TraditionalForm] <> " is NOT true"
                        
                ]
                ,
                {i, 1, Length[prop]}
            ]
    ];


(* ::Subsection::Closed:: *)
(*RFTest*)


(* ::Subsubsection:: *)
(*RFTest*)


RFTest[prop_, fx_, {f_, x_}] :=
    Module[{doms, sub, result, variables},
        variables = DeleteDuplicates @ Cases[prop, _Symbol, \[Infinity]];
        doms = Table[FunctionDomain[# /. x -> variables[[i]], variables
            [[i]]], {i, 1, Length[variables]}]& /@ fx;
        sub = Table[prop //. f[var_] :> (fx[[i]] /. x -> var), {i, 1,
             Length[fx]}];
        result = Table[FullSimplify[sub[[i]], doms[[i]]], {i, 1, Length[
            sub]}];
        TableForm @
            Table[
                If[BooleanQ[result[[i]]] && TrueQ[result[[i]]],
                    ToString[fx[[i]], TraditionalForm] <> " satisfies the property."
                        
                    ,
                    ToString[fx[[i]], TraditionalForm] <> " DOES NOT satisfies the property."
                        
                ]
                ,
                {i, 1, Length[fx]}
            ]
    ]


(* ::Subsection::Closed:: *)
(*RiemannSum*)


(* ::Subsubsection::Closed:: *)
(*riemannREE*)


riemannREE[f_,a_,b_,n_]:=Module[{dx},
dx=(b-a)/n;
Sum[f[k] dx,{k,a+dx,b,dx}]
]


(* ::Subsubsection::Closed:: *)
(*riemannLEE*)


riemannLEE[f_,a_,b_,n_]:=Module[{dx},
dx=(b-a)/n;
Sum[f[k] dx,{k,a,b-dx,dx}]
]


(* ::Subsubsection::Closed:: *)
(*riemannTE*)


riemannTE[f_,a_,b_,n_]:=Module[{dx},
dx=(b-a)/n;
(f[a]dx)/2+Sum[f[k] dx,{k,a+dx,b-dx,dx}]+(f[b] dx)/2
]


(* ::Subsection::Closed:: *)
(*ToDeg*)


ToDeg[Radian_] :=
    N @ Radian * 180 / Pi;


(* ::Subsection::Closed:: *)
(*ToRad*)


ToRad[Degree_] :=
    N @ Degree * Pi / 180;


(* ::Subsection::Closed:: *)
(*RatDenom*)


RatDenom[x_?NumericQ] :=
    Module[{y, nn, dd, f, g, c, k, blah},
        (
            y = Together[x];
            nn = Numerator[y];
            dd = Denominator[y];
            f = MinimalPolynomial[dd, t];
            c = f /. t -> 0;
            g = Factor[(c - f) / t];
            {k, blah} = FactorTermsList[Expand[nn * (g /. t -> dd)]];
                
            Sign[c] ((k / GCD[k, c]) * blah) / HoldForm[Evaluate @ Abs[
                c / GCD[k, c]]]
        )
    ]

RatDenom[x__] :=
    RatDenom[#]& /@ x


(* ::Subsection::Closed:: *)
(*compSqu*)


compSqu[f_,x_]:=Module[{a,b,c},
a=Coefficient[f[x],x,2];
b=Coefficient[f[x],x,1];
c=Coefficient[f[x],x,0];
a (x+b/(2a))^2+c-b^2/(4a)//TraditionalForm
]


(* ::Section:: *)
(*End*)


End[]
