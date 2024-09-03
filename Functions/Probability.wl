(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


(* ::Subsection:: *)
(*Usage - <Probability>*)


(*Usage - Probability*)

HGD::usage = "Shortcut for HypergeometricDistribution. Input is number of trials, defectives and total population";

ND::usage = "Shortcut for NormalDistribution. Input is \[Mu] and \[Sigma].";

BD::usage = "Shortcut for BinomialDistribution. Input is n and p.";

SD::usage = "Computes the standard deviation of a random variable.";

Var::usage = "Computes the variance of a random variable.";

RVAdd::usage = "Adds two random variables.";

Pr::usage = "Shortcut for NProbability. Input is the Probability input.";

PD::usage = "Defines a ProbabilityDistribution.";

ED::usage = "EmpiricalDistribution shortcut.";

samplePropCI::usage= "\!\(\*
StyleBox[\"samplePropCI\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"C\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)
Calculates the Approximate Confidence Interval for the Sample Proportion from given sample statistics.\[IndentingNewLine]p: Sample Proportion \[IndentingNewLine]n: Sample size \[IndentingNewLine]C: Level of confidence as an integer (i.e. C=95 for a 95% CI)\[IndentingNewLine]Output: {Confidence Interval Lower Bound, Confidence Interval Upper Bound}";

invSamplePropCI::usage = "\!\(\*
StyleBox[\"invSamplePropCI\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"lCI\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"rCI\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"C\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)
Calculates the relevant sample statistics from a given Confidence Interval for the Population Proportion.\[IndentingNewLine]lCI: Confidence Interval Lower Bound\[IndentingNewLine]rCI: Confidence Interval Upper Bound\[IndentingNewLine]C: Confidence level of interval as an integer (i.e. C=95 for a 95% CI)\[IndentingNewLine]Output: {Sample Proportion, Calculated Sample Size (not rounded)}";

TProbability::usage = "TProbability[{success,failure},{succesname,failurename},initialstate,{possible states}]";

DProbability::usage = "DProbability[{success,failure},{successname,failurename},initialstate,combo]";

sampleMeanCI::usage = "Returns the confidence interval of the lower bound and the upper bound .sampleMeanCI[sampmean,samplepop,n,clevel]";

invSampleMeanCI::usage = "Returns the sample mean and the sample population. invSampleMeanCI[lowercint,uppercint,n,clevel]";

generatePDF::usage = "\!\(\*
StyleBox[\"generatePDF\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"val1\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"val2\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Bold\"]\)Generates a Probability Density Function from a given function over a given domain and then reports important values.\[IndentingNewLine]f: The function to be used as a PDF. Must be non-negative over the given domain
val1: Minimum of the domain of the PDF
val2: Maximum of the domain of the PDF
Output: Plot, scaling constant and statistical properties of the PDF

Note: \!\(\*
StyleBox[\"If\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"the\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"function\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"is\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"negative\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"anywhere\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"in\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"the\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"given\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"domain\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"then\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"the\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"output\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"is\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"not\",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\" \",\nFontVariations->{\"Underline\"->True}]\)\!\(\*
StyleBox[\"valid\",\nFontVariations->{\"Underline\"->True}]\). 
This is easily checked with the given plot. 
There is a large volume of output.";


(* ::Section:: *)
(*Begin*)


Begin["`Private`"]


(* ::Section:: *)
(*Definitions*)


(* ::Subsection::Closed:: *)
(*TProbability*)


(* ::Subsubsection::Closed:: *)
(*TProbability*)


TProbability[{out1_, out2_}, {succ_, fail_}, init_, dess_] :=
    Total[DProbability[{out1, out2}, {succ, fail}, init, #]& /@ dess]


(* ::Subsubsection::Closed:: *)
(*DProbability*)


DProbability[{out1_, out2_}, {succ_, fail_}, init_, des_] :=
    Module[{a, b, c = 1, i, t, chk = {}},
        For[i = 0, i <= StringLength[des], ++i,
            If[i == 0,
                If[init == succ,
                    b = out1
                    ,
                    b = out2
                ]
                ,
                If[StringPart[des, i] == succ,
                    If[i == 1,
                        If[StringPart[des, i] == init,
                            c = c * b;
                            AppendTo[chk, b]
                            ,
                            c = c * (1 - b);
                            b = out1
                            ,
                            AppendTo[chk, b];
                        ]
                        ,
                        If[StringPart[des, i] == StringPart[des, i - 
                            1],
                            b = out1;
                            c = c * b;
                            AppendTo[chk, b]
                            ,
                            c = c * (1 - b);
                            AppendTo[chk, 1 - b];
                            b = out1;
                        ]
                    ]
                    ,
                    If[i == 1,
                        If[StringPart[des, i] == init,
                            b = out2;
                            c = c * b;
                            AppendTo[chk, b]
                            ,
                            c = c * (1 - b);
                            AppendTo[chk, 1 - b];
                            b = out2;
                        ]
                        ,
                        If[StringPart[des, i] == StringPart[des, i - 
                            1],
                            b = out2;
                            c = c * b;
                            AppendTo[chk, b]
                            ,
                            c = c * (1 - b);
                            AppendTo[chk, 1 - b];
                            b = out2;
                        ]
                    ]
                ]
            ]
        ];
        c
    ]


(* ::Subsubsection:: *)
(*Attributes and Options*)


(* ::Subsection::Closed:: *)
(*PropCI & MeanCI*)


sampleMeanCI[xbar_,sx_,n_,C_]:=Module[{z,a},
z=a/.Solve[Probability[-a<Z<a,Z\[Distributed]NormalDistribution[]]==C/100,a][[1]];
{xbar-z sx/Sqrt[n],xbar+z sx/Sqrt[n]}//N
]
invSampleMeanCI[lCI_,rCI_,n_,C_]:=Module[{z,a,xbar,sx},
z=a/.Solve[Probability[-a<Z<a,Z\[Distributed]NormalDistribution[]]==C/100,a][[1]];
{xbar,sx}/.Solve[{xbar-z sx/Sqrt[n],xbar+z sx/Sqrt[n]}=={lCI,rCI},{xbar,sx}][[1]]
]
samplePropCI[p_,n_,C_]:=Module[{z,a},
z=a/.Solve[Probability[-a<Z<a,Z\[Distributed]NormalDistribution[]]==C/100,a][[1]];
{p-z Sqrt[(p(1-p))/n],p+z Sqrt[(p(1-p))/n]}//N
]
invSamplePropCI[lCI_,rCI_,C_]:=Module[{p,me,z,a,n},
z=a/.Solve[Probability[-a<Z<a,Z\[Distributed]NormalDistribution[]]==C/100,a][[1]];
{p,n}/.Solve[{p-z Sqrt[(p(1-p))/n],p+z Sqrt[(p(1-p))/n]}=={lCI,rCI},{p,n}][[1]]
]        


(* ::Subsection::Closed:: *)
(*Various Shortcuts*)


(* ::Subsubsection:: *)
(*Probability*)


HGD[n_, D_, N_] :=
    HypergeometricDistribution[n, D, N];

ND[\[Mu]_:0, \[Sigma]_:1] :=
    NormalDistribution[\[Mu], \[Sigma]];

BD[n_, p_] :=
    BinomialDistribution[n, p];

SD[x_] :=
    StandardDeviation[x];

Var[x_] :=
    Variance[x];

RVAdd[a_, b_] :=
    TransformedDistribution[a, b];

Pr[x_, y_] :=
    Probability[x, y];

PD[f_, {x_, a_, b_}] :=
    ProbabilityDistribution[f, {x, a, b}];

ED[a_, b_] :=
    EmpiricalDistribution[a -> b]


(* ::Subsection::Closed:: *)
(*GeneratePDF*)


generatePDF[f_,val1_,val2_]:=Module[{distf,a,b,x,\[Mu],var,m,width},
a=Min[{val1,val2}];
b=Max[{val1,val2}];
width=Abs[b-a];
distf=ProbabilityDistribution[f[x]/\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(a\), \(b\)]\(f[x] \[DifferentialD]x\)\),{x,a,b}];
\[Mu]=\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(-\[Infinity]\), \(\[Infinity]\)]\(x\ PDF[distf, x] \[DifferentialD]x\)\);
var=\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(-\[Infinity]\), \(\[Infinity]\)]\(
\*SuperscriptBox[\(x\(\ \)\), \(2\)] PDF[distf, x] \[DifferentialD]x\)\)-\[Mu]^2;
m=m/.Solve[{\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(a\), \(m\)]\(f[x] \[DifferentialD]x\)\)==1/2 \!\(
\*SubsuperscriptBox[\(\[Integral]\), \(a\), \(b\)]\(f[x] \[DifferentialD]x\)\),a<m<b},m,Reals][[1]];
Column[{
Plot[PDF[distf,x],{x,a-width/10,b+width/10},Filling->Axis,PlotLabel->"PDF of given function, f(x)"],
StringForm["The scaling constant is k=`` or k=``",1/\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(a\), \(b\)]\(f[x] \[DifferentialD]x\)\),1/\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(a\), \(b\)]\(f[x] \[DifferentialD]x\)\)//N],
StringForm["The mean is \[Mu]=`` or \[Mu]=``",\[Mu],\[Mu]//N],
StringForm["The median is m=`` or m=``",m,m//N],(*Remove the median if it causes errors*)
StringForm["The variance is \!\(\*SuperscriptBox[\(\[Sigma]\), \(2\)]\)=`` or \!\(\*SuperscriptBox[\(\[Sigma]\), \(2\)]\)=``",var,var//N],
StringForm["The standard deviation is \[Sigma]=`` or \[Sigma]=``",Sqrt[var],Sqrt[var]//N]}]
]


(* ::Section:: *)
(*End*)


End[]
