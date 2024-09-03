(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


WiggaPlot::usage = "The Wigga way to plot and basically find anything
Usage:
WiggaPlot[\!\(\*StyleBox[\"f\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\" \",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"/\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\" \",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"{\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[SubscriptBox[\"f\", \"1\"],FontSlant->\"Italic\"]\)\!\(\*StyleBox[\",\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\" \",FontSlant->\"Italic\"]\)\!\(\*StyleBox[SubscriptBox[\"f\", \"2\"],FontSlant->\"Italic\"]\)\!\(\*StyleBox[\",\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\" \",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"...\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"}\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\",\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\" \",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"{\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"var\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\",\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\" \",FontSlant->\"Italic\"]\)\!\(\*SubscriptBox[StyleBox[\"var\",FontSlant->\"Italic\"], StyleBox[\"min\",FontSlant->\"Italic\"]]\),\!\(\*StyleBox[\" \",FontSlant->\"Italic\"]\)\!\(\*StyleBox[SubscriptBox[\"var\", \"max\"],FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"}\",FontSlant->\"Italic\"]\), \!\(\*StyleBox[\"{\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[SubscriptBox[StyleBox[\"range\",FontSlant->\"Italic\"], \"min\"],FontSlant->\"Italic\"]\)\!\(\*StyleBox[\",\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\" \",FontSlant->\"Italic\"]\)\!\(\*StyleBox[SubscriptBox[\"range\", \"max\"],FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"}\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\",\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\" \",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"Optional\",FontSlant->\"Italic\"]\)]
Arg 1 must be list if more than one function
Arg 2 optional arg, if not inputted will default to {x, -10, 10} as variable and domain
Arg 3 optional args, if not inputted will default to {-10, 10} as range
Options: 
- Asy -> \"N\"/\"F\", \"N\" for normal way to find asymptote, \"F\" to use function resource (generally slower and use only for fatter equations)
- BackGround -> \"Base\"/\"Chad\"/\"GigaChad\"/False, False gives no background, \"Base\" gives nice background, \"Chad\" shows a picture of chad, \"GigaChad\" shows a picture of a Premium Wigga
  -> chad teachers for background:
  -> \"Charalambous\", for Mr Charalambous
  -> \"MrG\", for Mr Gavrilescu (This is so \!\(\*StyleBox[\"easyyy\",FontSlant->\"Italic\"]\))
  -> \"Kermond\", for Mr Kermond
  -> \"Corkill\", for the ultimate teacher Mr Corkill
  -> \"May\", for Mr May although he wont be impressed he is in this
- BoundArea -> \"I\"/\"N\", shows area bound by first 2 graphs or first graph with x axis
- DecimalPoints -> \!\(\*StyleBox[\"num\",FontSlant->\"Italic\"]\), converts any root objects into a numerical value with that many decimal points
- ExpandFunctions -> True/False, expands RealAbs into all of its piecewise functions
- Int -> True/False, also graphs integral and derivative of first graph, 
- Inv -> True/False, graphs inverse of first graph
- KermondTP -> True/False, generates tunring points with kermonds because who doesn't want it
- Labels -> \"N\"/\"B\"/False, \"N\" for normal tooltips, \"B\" for basic tooltips, False to not have it at all
- Parallel -> True/False, find important points using subkernels (multithreading, but slower with smaller and less equations)
- TN -> (Number), graphs tangent and normal at that x value
- Everything else Plot has too

Other important notes:
Now supports piecewise functions or functions with extra restrictions eg. \!\(\*SuperscriptBox[\(x\), \(2\)]\)&&x>1
Does not support Abs functions well, use RealAbs for better results

This WiggaPlot is originally created by Joseph and helped by others";

lineTangent::usage = "\!\(\*
StyleBox[\"lineTangent\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"a\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)
Calculates the equation of the tangent line to function f at point x=a
f: Name of a defined function (The rule itself will not work)
a: x value of the point at which to take the tangent (number or parameter)
Output: Tangent line as a function of x";

lineNormal::usage = "\!\(\*
StyleBox[\"lineNormal\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"f\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"a\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)
Calculates the equation of the normal (perpendicular) line to function f at point x=a
f: Name of a defined function (The rule itself will not work)
a: x value of the point at which to take the normal (number or parameter)
Output: Normal line as a function of x";

line2Point::usage = "\!\(\*
StyleBox[\"line2Point\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"{\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"x1\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"y1\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"}\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"{\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"x2\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"y2\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"}\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[IndentingNewLine]Calculates the line passing through two given points {x1,y1} and {x2,y2}\[IndentingNewLine]{x1,y1}: Coordinates of first point 
{x2,y2}: Coordinates of second point\[IndentingNewLine]Output: Line as a function of x\[IndentingNewLine]Note: This will fail if the two points have the same x-value";

plotinator::usage = "Plots a graph between the given domain and finds the turning points, x & y-intercepts";


(* ::Section:: *)
(*Begin*)


Begin["`Private`"]

(*Requires FeatureFinding to be loaded in first.*) 


(* ::Section:: *)
(*Definitions*)


(* ::Subsection::Closed:: *)
(*Line Construction*)


(* ::Subsubsection:: *)
(*lineTangent*)


lineTangent[f_,a_]:=Module[{},
Clear[x];
Collect[f[a]+f'[a](x-a),x]//Evaluate
]


(* ::Subsubsection:: *)
(*lineNormal*)


lineNormal[f_,a_]:=Module[{},
Clear[x];
Collect[f[a]-1/f'[a] (x-a),x]//Evaluate
]


(* ::Subsubsection:: *)
(*line2Point*)


line2Point[{x1_,y1_},{x2_,y2_}]:=Module[{},
Clear[x];
Collect[y1+(y2-y1)/(x2-x1) (x-x1),x]//Evaluate
]


(* ::Subsection::Closed:: *)
(*BinomialDistribution Plotting*)


plotBinomialDistributionBetweenInclusive[n_,p_,a_Integer,b_Integer]:=Module[{i,plot1,plots},
plot1=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,0,n},ExtentSize->Right,Filling->Axis,FillingStyle->Red];
plots={};
For[i=a,i<=b,i++,
AppendTo[plots,DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,i,i},ExtentSize->Right,Filling->Axis,FillingStyle->Blue]];
];
Return[Show[plot1,plots]];
];
plotBinomialDistributionBetweenExclusive[n_,p_,a_Integer,b_Integer]:=Module[{i,plot1,plots},
plot1=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,0,n},ExtentSize->Right,Filling->Axis,FillingStyle->Red];
plots={};
For[i=a+1,i<=b-1,i++,
AppendTo[plots,DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,i,i},ExtentSize->Right,Filling->Axis,FillingStyle->Blue]];
];
Return[Show[plot1,plots]];
];
plotBinomialDistributionGreaterThan[n_,p_,a_Integer]:=Module[{i,plot1,plots},
plot1=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,0,n},ExtentSize->Right,Filling->Axis,FillingStyle->Red];
plots={};
For[i=a+1,i<=n,i++,
AppendTo[plots,DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,i,i},ExtentSize->Right,Filling->Axis,FillingStyle->Blue]];
];
Return[Show[plot1,plots]];
];
plotBinomialDistributionLessThan[n_,p_,a_Integer]:=Module[{i,plot1,plots},
plot1=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,0,n},ExtentSize->Right,Filling->Axis,FillingStyle->Red];
plots={};
For[i=0,i<=a-1,i++,
AppendTo[plots,DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,i,i},ExtentSize->Right,Filling->Axis,FillingStyle->Blue]];
];
Return[Show[plot1,plots]];
];
plotBinomialDistributionEquals[n_,p_,a_Integer]:=Module[{i,plot1,plots},
plot1=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,0,n},ExtentSize->Right,Filling->Axis,FillingStyle->Red];
plots=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,a,a},ExtentSize->Right,Filling->Axis,FillingStyle->Blue];
Return[Show[plot1,plots]];
];
probabilityBinomialDistributionBetweenInclusive[n_,p_,a_Integer,b_Integer]:=Module[{i,plot1,plots},
plot1=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,0,n},ExtentSize->Right,Filling->Axis,FillingStyle->Red];
plots={};
For[i=a,i<=b,i++,
AppendTo[plots,DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,i,i},ExtentSize->Right,Filling->Axis,FillingStyle->Blue]];
];
Print[Show[plot1,plots]];
Return[Probability[a<=x<=b,x\[Distributed]BinomialDistribution[5,0.3]]];
];
probabilityBinomialDistributionBetweenExclusive[n_,p_,a_Integer,b_Integer]:=Module[{i,plot1,plots},
plot1=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,0,n},ExtentSize->Right,Filling->Axis,FillingStyle->Red];
plots={};
For[i=a+1,i<=b-1,i++,
AppendTo[plots,DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,i,i},ExtentSize->Right,Filling->Axis,FillingStyle->Blue]];
];
Print[Show[plot1,plots]];
Return[Probability[a<x<b,x\[Distributed]BinomialDistribution[5,0.3]]];
];
probabilityBinomialDistributionGreaterThan[n_,p_,a_Integer]:=Module[{i,plot1,plots},
plot1=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,0,n},ExtentSize->Right,Filling->Axis,FillingStyle->Red];
plots={};
For[i=a+1,i<=n,i++,
AppendTo[plots,DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,i,i},ExtentSize->Right,Filling->Axis,FillingStyle->Blue]];
];
Print[Show[plot1,plots]];
Return[Probability[x>a,x\[Distributed]BinomialDistribution[5,0.3]]];
];
probabilityBinomialDistributionLessThan[n_,p_,a_Integer]:=Module[{i,plot1,plots},
plot1=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,0,n},ExtentSize->Right,Filling->Axis,FillingStyle->Red];
plots={};
For[i=0,i<=a-1,i++,
AppendTo[plots,DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,i,i},ExtentSize->Right,Filling->Axis,FillingStyle->Blue]];
];
Print[Show[plot1,plots]];
Return[Probability[x<a,x\[Distributed]BinomialDistribution[5,0.3]]];
];
probabilityBinomialDistributionEquals[n_,p_,a_Integer]:=Module[{i,plot1,plots},
plot1=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,0,n},ExtentSize->Right,Filling->Axis,FillingStyle->Red];
plots=DiscretePlot[PDF[BinomialDistribution[n,p],x],{x,a,a},ExtentSize->Right,Filling->Axis,FillingStyle->Blue];
Print[Show[plot1,plots]];
Return[Probability[x==a,x\[Distributed]BinomialDistribution[5,0.3]]];
]



(* ::Subsection::Closed:: *)
(*NormalDistribution Plotting*)


probabilityNormalDistributionBetween[mean_,stdev_,a_,b_,numberOfStdevToShow_:4]:=Module[{plot1,plot2},
plot1=Plot[PDF[NormalDistribution[mean,stdev],x],{x,mean-stdev*numberOfStdevToShow,mean+stdev*numberOfStdevToShow},AxesLabel->{"Random Variable (standard deviation)", "Probability X=x"}];
plot2=Plot[PDF[NormalDistribution[mean,stdev],x],{x,a,b},Filling->Axis,PlotRange->PlotRange[plot1]];
Print[Show[plot1,plot2]];
Return[NProbability[a<x<b,x\[Distributed]NormalDistribution[mean,stdev]]];
];
probabilityNormalDistributionGreaterThan[mean_,stdev_,a_,numberOfStdevToShow_:4]:=Module[{plot1,plot2},
plot1=Plot[PDF[NormalDistribution[mean,stdev],x],{x,mean-stdev*numberOfStdevToShow,mean+stdev*numberOfStdevToShow},AxesLabel->{"Random Variable (standard deviation)", "Probability X=x"}];
plot2=Plot[PDF[NormalDistribution[mean,stdev],x],{x,a,mean+stdev*numberOfStdevToShow},Filling->Axis,PlotRange->PlotRange[plot1]];
Print[Show[plot1,plot2]];
Return[NProbability[a<x,x\[Distributed]NormalDistribution[mean,stdev]]];
];
probabilityNormalDistributionLessThan[mean_,stdev_,a_,numberOfStdevToShow_:4]:=Module[{plot1,plot2},
plot1=Plot[PDF[NormalDistribution[mean,stdev],x],{x,mean-stdev*numberOfStdevToShow,mean+stdev*numberOfStdevToShow},AxesLabel->{"Random Variable (standard deviation)", "Probability X=x"}];
plot2=Plot[PDF[NormalDistribution[mean,stdev],x],{x,mean-stdev*numberOfStdevToShow,a},Filling->Axis,PlotRange->PlotRange[plot1]];
Print[Show[plot1,plot2]];
Return[NProbability[x<a,x\[Distributed]NormalDistribution[mean,stdev]]];
];
plotNormalDistributionBetween[mean_,stdev_,a_,b_,numberOfStdevToShow_:4]:=Module[{plot1,plot2},
plot1=Plot[PDF[NormalDistribution[mean,stdev],x],{x,mean-stdev*numberOfStdevToShow,mean+stdev*numberOfStdevToShow},AxesLabel->{"Random Variable (standard deviation)", "Probability X=x"}];
plot2=Plot[PDF[NormalDistribution[mean,stdev],x],{x,a,b},Filling->Axis,PlotRange->PlotRange[plot1]];
Return[Show[plot1,plot2]];
];
plotNormalDistributionGreaterThan[mean_,stdev_,a_,numberOfStdevToShow_:4]:=Module[{plot1,plot2},
plot1=Plot[PDF[NormalDistribution[mean,stdev],x],{x,mean-stdev*numberOfStdevToShow,mean+stdev*numberOfStdevToShow},AxesLabel->{"Random Variable (standard deviation)", "Probability X=x"}];
plot2=Plot[PDF[NormalDistribution[mean,stdev],x],{x,a,mean+stdev*numberOfStdevToShow},Filling->Axis,PlotRange->PlotRange[plot1]];
Return[Show[plot1,plot2]];
];
plotNormalDistributionLessThan[mean_,stdev_,a_,numberOfStdevToShow_:4]:=Module[{plot1,plot2},
plot1=Plot[PDF[NormalDistribution[mean,stdev],x],{x,mean-stdev*numberOfStdevToShow,mean+stdev*numberOfStdevToShow},AxesLabel->{"Random Variable (standard deviation)", "Probability X=x"}];
plot2=Plot[PDF[NormalDistribution[mean,stdev],x],{x,mean-stdev*numberOfStdevToShow,a},Filling->Axis,PlotRange->PlotRange[plot1]];
Return[Show[plot1,plot2]];
];


(* ::Subsection::Closed:: *)
(*Plotinator*)


plotinator[f_,dom_,showObliqueAsymptotes_:False]:=Module[{i,j,keyPoints,plot,absoluteRange,epilogPoints,epologTextOffset,epilogs,hAsymptotes,vAsymptotes,oAsymptotes,asymptotes,solveDomain,plotEquations,oAsymptoteHolder,plotStyle,pointObjects,numberOfEquations},
keyPoints={};
plot=0;
absoluteRange=0;
epilogPoints=0;
epologTextOffset=0;
epilogs={};
solveDomain=dom[[2]]<=dom[[1]]<=dom[[3]];
plotEquations={f};
oAsymptoteHolder={};
plotStyle={};
pointObjects={};
numberOfEquations=0;

If[(f)[[0]]==List,
plotEquations=f;
];

For[i=1,i<=Length[plotEquations],i++,
AppendTo[plotStyle,{plotColors[i]}]
];
For[i=1,i<=Length[plotEquations],i++,
keyPoints=Join[keyPoints,movingPointsOfInflection[plotEquations[[i]],dom[[1]],solveDomain]];
keyPoints=Join[keyPoints,stationaryPointsOfInflection[plotEquations[[i]],dom[[1]],solveDomain]];
keyPoints=Join[keyPoints,turningPoints[plotEquations[[i]],dom[[1]],solveDomain]];
keyPoints=Join[keyPoints,roots[plotEquations[[i]],dom[[1]],solveDomain]];
keyPoints=Join[keyPoints,yIntercepts[plotEquations[[i]],dom[[1]],solveDomain]];
];
keyPoints=DeleteDuplicates[keyPoints];

plot=Plot[f,dom];
absoluteRange=Abs[PlotRange[plot][[2,2]]-PlotRange[plot][[2,1]]];
For[j=1,j<=Length[plotEquations],j++,
For[i=1,i<=Length[keyPoints],i++,
epologTextOffset=1/10*absoluteRange*concaviteAtPointAsInteger[plotEquations[[j]],dom[[1]],keyPoints[[i,1]]];
AppendTo[epilogs,Text[keyPoints[[i]],{keyPoints[[i,1]],keyPoints[[i,2]]+epologTextOffset}]];
];
];

asymptotes={};
For[j=1,j<=Length[plotEquations],j++,
hAsymptotes=horizontalAsymptotes[plotEquations[[j]],dom[[1]]];
vAsymptotes=verticleAsymptotes[plotEquations[[j]],dom[[1]]];
oAsymptotes=obliqueAsymptote[plotEquations[[j]],dom[[1]]];
For[i=1,i<=Length[hAsymptotes],i++,
AppendTo[asymptotes,Tooltip[Line[{{dom[[2]],hAsymptotes[[i]]},{dom[[3]],hAsymptotes[[i]]}}],y==hAsymptotes[[i]]]];
];
For[i=1,i<=Length[vAsymptotes],i++,
AppendTo[asymptotes,Tooltip[Line[{{vAsymptotes[[i]],PlotRange[plot][[2,1]]},{vAsymptotes[[i]],PlotRange[plot][[2,2]]}}],x==vAsymptotes[[i]]]];
];
For[i=1,i<=Length[oAsymptotes],i++,
AppendTo[oAsymptoteHolder,oAsymptotes[[i]]];
];
];

If[showObliqueAsymptotes,
Print["showing oblique asymptotes"];
oAsymptoteHolder=DeleteDuplicates[oAsymptoteHolder];
For[i=1,i<=Length[oAsymptoteHolder],i++,
AppendTo[plotEquations,oAsymptoteHolder[[i]]];
];

For[i=1,i<=(Length[oAsymptoteHolder]),i++,
AppendTo[plotStyle,{Red,Dashed}];
];
];

For[i=1,i<=Length[keyPoints],i++,
AppendTo[pointObjects,Tooltip[Point[keyPoints[[i]]],{keyPoints[[i,1]],keyPoints[[i,2]]}]];
];

plot=Plot[plotEquations,dom,Epilog->{PointSize[Large],pointObjects,(*epilogs,*)Directive[Red,Dashed],asymptotes},PlotStyle->plotStyle,PlotLegends->"Expressions"];
Return[plot];
];


(* ::Subsubsection::Closed:: *)
(*PlotinatorBase*)


roots[f_,x_,sdom_:True]:=Module[{i,rawSol,root,output},
rawSol=Solve[f==0&&sdom,x,Reals];
root={};
output={};
For[i=1,i<=Length[rawSol],i++,
If[rawSol[[i,1,2,0]]==Root,
AppendTo[root,N[rawSol[[i]],2]];
Continue[];
];
AppendTo[root,rawSol[[i]]];
];
If[Or[root=={{}},root=={}],
Return[{}],
For[i=1,i<=Length[root],i++,
AppendTo[output, {root[[i,1,2]],0}];
];
Return[output]
];
];
approximateRoots[solutions_]:=Module[{i, output},
output={};
For[i=1,i<=Length[solutions],i++,
If[solutions[[i,1,2,0]]==Root,
AppendTo[output, N[solutions[[i]],2]];
Continue[];
];
AppendTo[output, solutions[[i]]];
];
Return[output];
];
turningPoints[f_,x_,sdom_:True]:=Module[{solveResult,skippedIfStatment},
skippedIfStatment=True;
If[D[f,x]==0,
skippedIfStatment=False;
solveResult={{}};
];
If[skippedIfStatment,
solveResult=approximateRoots[Solve[D[f,x]==0&&sdom,x,Reals]];
];
If[Or[solveResult=={{}},solveResult=={}],
Return[{}]
,
Return[Zip[solveResult[[All,1,2]],f/.x->solveResult[[All,1,2]]]]
];
Print["The answer is liable to contain errors"];
Return[{}]
];
maximumTurningPoints[f_,x_,sdom_:True]:=Module[{i,TPs,output},
TPs=turningPoints[f,x,sdom];
output={};
If[Or[TPs=={{}},TPs=={},TPs=={}],
Return[{}];
];
For[i=1,i<=Length[TPs],i++,
If[(D[f,{x,2}]/.x->TPs[[i,1]])<0,
AppendTo[output,TPs[[i]]];
];
];
Return[output];
];
minimumTurningPoints[f_,x_,sdom_:True]:=Module[{i,TPs,output},
TPs=turningPoints[f,x,sdom];
output={};
If[Or[TPs=={{}},TPs=={},TPs=={}],
Return[{}];
];
For[i=1,i<=Length[TPs],i++,
If[(D[f,{x,2}]/.x->TPs[[i,1]])>0,
AppendTo[output,TPs[[i]]];
];
];
Return[output];
];
stationaryPointsOfInflection[f_,x_,sdom_:True]:=Module[{i,j,secondDSolutions,thirdDSolutions,output,inflections,shouldAdd,skippedIfStatment},
output={};
inflections={};
shouldAdd=True;
skippedIfStatment=True;
If[D[f,{x,2}]==0,
secondDSolutions={{}};
skippedIfStatment=False;
];
If[skippedIfStatment,
secondDSolutions=approximateRoots[Solve[D[f,{x,2}]==0&&sdom,x,Reals]];
];
skippedIfStatment=True;
If[D[f,{x,3}]==0,
thirdDSolutions={{}};
skippedIfStatment=False;
];
If[skippedIfStatment,
thirdDSolutions=approximateRoots[Solve[D[f,{x,3}]==0&&sdom,x,Reals]];
];
If[Or[secondDSolutions=={{}},secondDSolutions=={}],
Return[{}];
,
If[Or[thirdDSolutions=={{}},thirdDSolutions=={}],
Return[{}];
,
For[i=1,i<=Length[secondDSolutions],i++,
shouldAdd=False;
For[j=1,j<=Length[thirdDSolutions],j++,
If[secondDSolutions[[i]]==thirdDSolutions[[j]],
shouldAdd=True;
];
];
If[shouldAdd,
AppendTo[inflections,secondDSolutions[[i]]];
];
];
Return[Zip[inflections[[All,1,2]],f/.x->inflections[[All,1,2]]]];
];
];
Print["The answer is liable to contain errors"];
Return[{}];
];
movingPointsOfInflection[f_,x_,sdom_:True]:=Module[{i,j,secondDSolutions,thirdDSolutions,output,inflections,shouldAdd,skippedIfStatment},
output={};
inflections={};
shouldAdd=True;
skippedIfStatment=True;
If[D[f,{x,2}]==0,
secondDSolutions={{}};
skippedIfStatment=False;
];
If[skippedIfStatment,
secondDSolutions=approximateRoots[Solve[D[f,{x,2}]==0&&sdom,x,Reals]];
];
skippedIfStatment=True;
If[D[f,{x,3}]==0,
thirdDSolutions={{}};
skippedIfStatment=False;
];
If[skippedIfStatment,
thirdDSolutions=approximateRoots[Solve[D[f,{x,3}]==0&&sdom,x,Reals]];
];
If[Or[secondDSolutions=={{}},secondDSolutions=={}],
Return[{}];
,
If[Or[thirdDSolutions=={{}},thirdDSolutions=={}],
Return[Zip[secondDSolutions[[All,1,2]],f/.x->secondDSolutions[[All,1,2]]]];
,
For[i=1,i<=Length[secondDSolutions],i++,
shouldAdd=True;
For[j=1,j<=Length[thirdDSolutions],j++,
If[secondDSolutions[[i]]==thirdDSolutions[[j]],
shouldAdd=False;
];
];
If[shouldAdd,
AppendTo[inflections,secondDSolutions[[i]]];
];
];
Return[Zip[inflections[[All,1,2]],f/.x->inflections[[All,1,2]]]];
];
];
Print["The answer is liable to contain errors"];
Return[{}];
];
concaviteAtPointAsInteger[f_,x_,p_]:=Module[{secondD},
secondD=D[f,{x,2}];
If[(secondD/.x->p)<=0,
Return[1]
,
Return[-1]
];
Return["Error"];
];
yIntercepts[f_,x_,sdom_:True]:=Module[{},
If[(f&&sdom/.x->0)==False,
Return[{}];
];
Return[{{0,(f&&sdom/.x->0)}}];
];
evalulateValue[val_]:=val
plotColors[n_]:=ColorData[97,"ColorList"][[Mod[n,15]]]
obliqueAsymptote[f_,x_]:=Module[{i,expandedForm,temp,output,hasOAsymptote},
expandedForm=Expand[f];
If[expandedForm[[0]]==Plus,
temp={};
output={};
hasOAsymptote=False;
For[i=1,i<=Length[expandedForm],i++,
If[Limit[expandedForm[[i]],x->\[Infinity]]!=0,
AppendTo[temp,expandedForm[[i]]];
,
hasOAsymptote=True;
];
];
hasOAsymptote=True;
If[hasOAsymptote,
temp=Total[temp];
AppendTo[output,temp];
If[temp==0,output=Drop[output,-1]];
temp={};
];
hasOAsymptote=False;
For[i=1,i<=Length[expandedForm],i++,
If[Limit[expandedForm[[i]],x->-\[Infinity]]!=0,
AppendTo[temp,expandedForm[[i]]];
,
hasOAsymptote=True;
];
];
hasOAsymptote=True;
If[hasOAsymptote,
temp=Total[temp];
AppendTo[output,temp];
If[temp==0,output=Drop[output,-1]];
temp={};
];
output=DeleteDuplicates[output];
Return[output];
];
Return[{}];
];
valueOfInequality[inequ_,x_]:=Module[{},
If[inequ[[2]]==x,Return[inequ[[1]]]];
If[inequ[[1]]==x,Return[inequ[[2]]]];
]
verticleAsymptotes[f_,x_]:=Module[{i,j,dom,asymptotes,output},
dom=FunctionDomain[f,x];
asymptotes={};
output={};
For[i=1,i<=Length[dom],i++,
If[dom[[i,0]]==Less,
AppendTo[asymptotes,valueOfInequality[dom[[i]],x]];
];
If[dom[[i,0]]==Greater,
AppendTo[asymptotes,valueOfInequality[dom[[i]],x]];
];
If[dom[[i,0]]==Inequality,
For[j=1,j<=Length[dom[[i]]]-2,j=j+2,
AppendTo[asymptotes,valueOfInequality[dom[[i,j;;j+2]],x]];
];
];
];
asymptotes=DeleteDuplicates[asymptotes];
For[i=1,i<=Length[asymptotes],i++,
If[Or[Limit[f,x->asymptotes[[i]],Direction->"FromAbove"]==\[Infinity],Limit[f,x->asymptotes[[i]],Direction->"FromAbove"]==-\[Infinity]],
AppendTo[output,asymptotes[[i]]];
];
If[Or[Limit[f,x->asymptotes[[i]],Direction->"FromBelow"]==\[Infinity],Limit[f,x->asymptotes[[i]],Direction->"FromBelow"]==-\[Infinity]],
AppendTo[output,asymptotes[[i]]];
];
];
output=DeleteDuplicates[output];
Return[output];
];
horizontalAsymptotes[f_,x_]:=Module[{output,val},
output={};
val=0;
val=Limit[f,x->\[Infinity]];
If[And[val!=\[Infinity],val!=-\[Infinity]],
AppendTo[output,val];
];
val=Limit[f,x->-\[Infinity]];
If[And[val!=\[Infinity],val!=-\[Infinity]],
AppendTo[output,val];
];
output=DeleteDuplicates[output];
Return[output];
]


(* ::Subsection::Closed:: *)
(*WiggaPlot*)


WiggaPlot[funclist_,var:{_,_?NumericQ,_?NumericQ}:{x,-10,10},plotRange:{_,_}:{-15,15},opts:OptionsPattern[]]:=Module[{tooltip},tooltip[coords_,string_]:=If[OptionValue[Labels]===True,string,coords];Module[{functions,graphs={},graphsFinal={},thicc=Thickness[0.0055],exp,btninfo={},inv,xint,yint,tp,sol,num,tempEP,infliction,lines={},kermonds={},numerator,background=White,allIP,loop,denominator,range,allHeads,tempExpr,tempExprCond,cond,asy=<|"Horizontal"->{},"Vertical"->{},"Slant"->{},"Hole"->{}|>,points={},corner={},poidone={},rplot,btns={},
numForm,realChecker,convertRoots,singleArgExtracter,tempExp,integrate,getAllArgs,testHeads
},numForm[number_]:=NumberForm[number,Abs[IntegerPart[Re[Log[10,number]]]]+OptionValue[DecimalPoints]];

realChecker[number_]:=If[Head[number]===Real,numForm[number],number];

convertRoots[list_]:=Flatten[{N[Cases[list,_Root]],Cases[list,Except[_Root]]}];

singleArgExtracter[arg_]:=arg;

getAllArgs[expr_]:=Module[{getArg,headList={}},getArg[expression_]:=Table[If[Length[expression[[loop1]]]>0,AppendTo[headList,expression[[loop1]]];getArg[expression[[loop1]]]],{loop1,Length[expression]}];getArg[expr];Flatten[headList]];

testHeads[expr_,head_]:=If[Length[DeleteDuplicates[Cases[expr,head[x_]->True]]]===1,True,False];

range=plotRange;

If[OptionValue[PlotRange]=!={Full,Automatic},range=OptionValue[PlotRange]];

If[Head[funclist]=!=List,functions={funclist},functions=funclist];

(* plotting optional graphs with a few less characters *)
(* Int for intergral/derivative, Inv for inverse, TN for tangent and normal at a point you must give *)

If[OptionValue[Int],functions=Join[functions,{Evaluate[D[functions[[1]],var[[1]]]],Evaluate[Integrate[functions[[1]],var[[1]]]]}]];

If[OptionValue[Inv],functions=Join[functions,y/.Solve[(functions[[1]]/.var[[1]]->y)==var[[1]],y]]];

If[NumericQ[OptionValue[TN]],functions=Join[functions,{# var[[1]]+cons/.(Flatten[Solve[functions[[1]]==# var[[1]]+cons,cons]]/.var[[1]]->OptionValue[TN]),((-1)/#)var[[1]]+cons/.(Flatten[Solve[functions[[1]]==((-1)/#)var[[1]]+cons,cons]]/.var[[1]]->OptionValue[TN])}]&[(D[functions[[1]],var[[1]]]/.var[[1]]->OptionValue[TN])]];

If[#==={True},Print["Abs detected, recommended to use RealAbs instead"]]&[DeleteDuplicates[Flatten[Table[testHeads[getAllArgs[loop1],Abs],{loop1,functions}]]]];

Module[{tempList={},tempCond},
functions=Flatten[Table[If[Head[loop1]===Piecewise,Table[loop2[[1]]&&loop2[[2]],{loop2,loop1[[1]]}],
If[(testHeads[getAllArgs[loop1],RealAbs])&&OptionValue[ExpandFunctions],{Table[AppendTo[tempList,FunctionDomain[{loop2[[1]],loop2[[2]]},var[[1]]]];loop2[[1]]&&FunctionDomain[{loop2[[1]],loop2[[2]]},var[[1]]],{loop2,PiecewiseExpand[loop1][[1]]}],tempCond=tempList[[1]];Table[tempCond=tempCond||tempList[[loop3+1]],{loop3,Length[tempList]-1}];(PiecewiseExpand[loop1][[2]]&&Simplify[Not[tempCond]&&FunctionDomain[loop1,var[[1]]]])},loop1]],{loop1,functions}]]];

(* changes the background... yes very usefull*)

If[OptionValue[BackGround]=!="Base"&&OptionValue[BackGround]=!="Vibrant",
AppendTo[graphs,Plot[functions,var,PlotRange->range,Evaluate[FilterRules[{opts},Options[Plot]]]]]];

If[OptionValue[BackGround]==="Chad",AppendTo[graphs,Graphics[Inset[chad]]]];

If[OptionValue[BackGround]==="GigaChad",AppendTo[graphs,Graphics[Inset[gigaChad]]]];

If[OptionValue[BackGround]==="Chara",AppendTo[graphs,Graphics[Inset[charalambous]]]];

If[OptionValue[BackGround]==="MrG",AppendTo[graphs,Graphics[Inset[mrG]]]];

If[OptionValue[BackGround]==="Kermond",AppendTo[graphs,Graphics[Inset[kermond]]]];

If[OptionValue[BackGround]==="Corkill",AppendTo[graphs,Graphics[Inset[corkill]]]];

If[OptionValue[BackGround]==="May",AppendTo[graphs,Graphics[Inset[may]]]];

If[OptionValue[BackGround]==="Base",background=Polygon[{ImageScaled[{0,0}],ImageScaled[{1,0}],ImageScaled[{1,1}],ImageScaled[{0,1}]},VertexColors->{LightBlue,LightGreen,LightYellow,LightRed}]];

If[OptionValue[BackGround]==="Vibrant",background=Polygon[{ImageScaled[{0,0}],ImageScaled[{1,0}],ImageScaled[{1,1}],ImageScaled[{0,1}]},VertexColors->{RGBColor[0,0.5,1],RGBColor[0,1,0.5],Yellow,RGBColor[1,0.2,0.2]}]];

(* base graphs creation *)

AppendTo[graphs,Plot[Evaluate[Table[Tooltip[functions[[loop1]],tooltip[functions[[1]],StringForm["Graph ``: ``",loop1,functions[[loop1]]]]],{loop1,Length[functions]}]],var,PlotRange->range,Evaluate[FilterRules[{opts},Options[Plot]]/.If[OptionValue[PlotLegends]==="Expressions",OptionValue[PlotLegends]->"AllExpressions",OptionValue[PlotLegends]->OptionValue[PlotLegends]]],
Prolog->background]];

(* absolute values of the graph for automatic plot range etc etc *)

range=Rationalize[Round[PlotRange/.AbsoluteOptions[graphs[[1]],PlotRange],0.0001]][[2]];

(* getting the area bound by the curve and x axis or another graph*)

(* Use "N" for faster but numerical values (no fractions) use "I" for slower but exact values (usually)*)

If[OptionValue[BoundArea]==="I"||OptionValue[BoundArea]==="N",

If[OptionValue[BoundArea]==="I",integrate=Integrate,integrate=NIntegrate];

If[Length[functions]==1,

(* if only one function find area bound by axis and graph and in given domain *)

AppendTo[graphs,{RegionPlot[Tooltip[{functions[[1]]>=y>=0,functions[[1]]<=y<=0},

StringForm["Total Area: `` units\nTotal Signed Area: `` \!\(\*SuperscriptBox[\(units\), \(2\)]\)",integrate[Abs[functions[[1]]],var],integrate[functions[[1]],var]]],var,#2,BoundaryStyle->None,PlotStyle->Directive[LightGray,Opacity->0.1]],
If[#1[[2]]!=#1[[3]],RegionPlot[Tooltip[{functions[[1]]>=y>=0,functions[[1]]<=y<=0},StringForm["Bound Area: `` \!\(\*SuperscriptBox[\(units\), \(2\)]\)\nSigned Bound Area: `` \!\(\*SuperscriptBox[\(units\), \(2\)]\)",integrate[Abs[functions[[1]]],#1],integrate[functions[[1]],#]]],#1,#2,BoundaryStyle->None,PlotStyle->Directive[LightYellow,Opacity->0.1]],Nothing]}],If[Length[functions]==2&&#3[[2]]!=#3[[3]],
(* 2 graph bound area *)
AppendTo[graphs,RegionPlot[Tooltip[{functions[[1]]>=y>=functions[[2]],functions[[1]]<=y<=functions[[2]]},
StringForm["Total Area: `` \!\(\*SuperscriptBox[\(units\), \(2\)]\)",integrate[Abs[functions[[1]]-functions[[2]]],#3]]],#3,#2,BoundaryStyle->None,PlotStyle->Directive[LightGray,Opacity->0.1]]]]]&[

(* using # because mathematica region plot is very picky with its inputs *)

{var[[1]],Sort[#,Less][[1]],Sort[#,Less][[-1]]}&[DeleteDuplicates[var[[1]]/.Solve[functions[[1]]==0&&var[[2]]<=x<=var[[3]],Reals]]],{y,range[[1]],range[[2]]},
If[Length[functions]==2,{var[[1]],#[[1]],#[[-1]]}&[Sort[If[#=!={},var[[1]]/.#,{0}]&[DeleteDuplicates[Solve[functions[[2]]==functions[[1]]&&var[[2]]<=var[[1]]<=var[[3]],var[[1]],Reals]]],Less]]]]];

AppendTo[graphs,#[exp=functions[[loop1]];

If[Head[exp]===And,cond=Rest[exp];exp=exp[[1]],cond=True];

tempExp=var[[1]];

If[testHeads[getAllArgs[exp],RealAbs],tempExp=Replace[exp,RealAbs[x_]->\[Sqrt]Power[x,2],All]];

tempExprCond=Quiet[var[[1]]/.Solve[System`FunctionDiscontinuities[{tempExp,var[[2]]<=var[[1]]<=var[[3]]},var[[1]]]]];

corner={};


If[tempExprCond=!=var[[1]],Table[If[Quiet[NumericQ[exp/.var[[1]]->loop2]],AppendTo[corner,loop2]],{loop2,tempExprCond}]];
(* resetting variables *)

points={};

Table[AppendTo[points,Tooltip[#,tooltip[#,StringForm["(``, ``) : Corner for ``",#[[1]],#[[2]],exp]]]&[{loop2,exp/.var[[1]]->loop2}]],{loop2,corner}];

allIP={};

kermonds={};

(* x intercept finding *)

sol=Table[If[Head[var[[1]]/.#[[loop2]]]===Root,(N[#[[loop2]]]),#[[loop2]]],{loop2,Length[#]}]&[DeleteDuplicates[Flatten[Solve[exp==0&&var[[2]]<=var[[1]]<=var[[3]]&&cond,Reals]]]];

xint=sol;

(* x intercept point creation *)

AppendTo[points,Table[Tooltip[{var[[1]]/.sol[[loop2]],0},tooltip[{var[[1]]/.sol[[loop2]],0},StringForm["(`1`, 0) : `3`-int for graph `2`",realChecker[var[[1]]/.sol[[loop2]]],exp,var[[1]]]]],{loop2,Length[sol]}]];

(* inflection point finding *)

AppendTo[points,Table[Tooltip[{loop3,exp/.var[[1]]->loop3},tooltip[{loop3,exp/.var[[1]]->loop3},StringForm["(``, ``) : Point of inflection for ``",realChecker[loop3],realChecker[exp/.var[[1]]->loop3],exp]]],{loop3,#}]]&[

Table[If[Head[var[[1]]/.loop2]===Root,N[var[[1]]/.loop2],var[[1]]/.loop2],{loop2,#}]&[

Flatten[Solve[D[exp,{var[[1]],2}]==0&&var[[2]]<=var[[1]]<=var[[3]]&&cond,Reals]]]];

(* y intercept finding *) 

If[NumericQ[#]&&Quiet[(cond/.var[[1]]->0)],yint=#;AppendTo[points,{Tooltip[{0,#},tooltip[{0,#},StringForm["(0, ``) : y-int for graph ``",realChecker[#],exp]]]}],yint="none existent"]&[Quiet[exp/.var[[1]]->0]];

(* turning point solving *)

sol=Table[If[Head[var[[1]]/.#[[loop2]]]===Root,N[#[[loop2]]],#[[loop2]]],{loop2,Length[#]}]&[DeleteDuplicates[Flatten[Solve[D[exp,var[[1]]]==0&&var[[2]]<=var[[1]]<=var[[3]]&&cond]]]];

tp=sol;

(* ... kermonds, if you want them *)

If[OptionValue[KermondTP]===True,AppendTo[kermonds,Table[Graphics[Tooltip[Inset[miniK,{(var[[1]]/.sol[[loop2]]) -((var[[3]]-var[[2]])/20),((exp/.sol[[loop2]])+1.2 *((range[[2]]-range[[1]])/10))}],StringForm["Mr Kermond is looking at the turning point (``, ``)",realChecker[var[[1]]/.sol[[loop2]]],realChecker[exp/.sol[[loop2]]]]]],{loop2,Length[sol]}]]];

(* point creation for the turning points and local min/max and station point of inflection checking *)

AppendTo[points,Table[Tooltip[{var[[1]]/.sol[[loop2]],exp/.sol[[loop2]]},tooltip[{var[[1]]/.sol[[loop2]],exp/.sol[[loop2]]},StringForm["(`1`, `2`) : TP (`4`) for graph `3`",realChecker[var[[1]]/.sol[[loop2]]],

realChecker[exp/.sol[[loop2]]],exp,If[(exp/.sol[[loop2]])<exp/.var[[1]]->(sol[[loop2,2]]+1/1000000),

If[(exp/.sol[[loop2]])<exp/.var[[1]]->(sol[[loop2,2]]-1/1000000),"Local min","SPOI Down -> up"],

If[(exp/.sol[[loop2]])<exp/.var[[1]]->(sol[[loop2,2]]-1/1000000),"SPOI Up -> down","Local max"],"Bruh"]]]],{loop2,Length[sol]}]];

(* end point finding and point creation *)
tempEP={};

Quiet[(If[(#/.var[[1]]->var[[2]])===True,AppendTo[tempEP,{var[[2]],exp/.var[[1]]->var[[2]]}],If[(#[[1]]=!=var[[1]])===True,AppendTo[tempEP,{#[[1]],exp/.var[[1]]->#[[1]]}],AppendTo[tempEP,{#[[2]],exp/.var[[1]]->#[[2]]}]]];If[#/.var[[1]]->var[[3]],AppendTo[tempEP,{var[[3]],exp/.var[[1]]->var[[3]]}],If[#[[2]]===var[[1]],AppendTo[tempEP,{#[[3]],exp/.var[[1]]->#[[3]]}],AppendTo[tempEP,{#[[2]],exp/.var[[1]]->#[[2]]}]]])&[FunctionDomain[{exp,cond},var[[1]]]];

(*endpoints that might occur in the middle and not the ends *)

tempEP=Join[tempEP,If[#=!=var[[1]],Table[If[NumericQ[Limit[exp,var[[1]]->ep]],{ep,exp/.var[[1]]->ep},Nothing],{ep,#}],{}]&[var[[1]]/.Solve[System`FunctionDiscontinuities[exp&&var[[2]]<=var[[1]]<=var[[3]]&&cond,var[[1]],Reals]&&var[[2]]<=var[[1]]<=var[[3]]&&cond,var[[1]],Reals]]];

tempEP=DeleteDuplicates[Table[If[Length[Select[Select[ep,FreeQ[#,_Complex]&],NumericQ]]==2,ep,Nothing],{ep,tempEP}]];

(* intercepts between different graphs and plotting them *)

Table[If[loop1!=loop2,sol=Table[If[Head[var[[1]]/.#[[j]]]===Root,var[[1]]->N[var[[1]]/.#[[loop4]]],#[[loop4]]],{loop4,Length[#]}]&[DeleteDuplicates[Flatten[Solve[#1==exp&&var[[2]]<=var[[1]]<=var[[3]]&&cond&&#2,var[[1]],Reals]]]&[If[Head[functions[[loop2]]]===And,functions[[loop2]][[1]],functions[[loop2]]],If[Head[functions[[loop2]]]===And,functions[[loop2,2]],True]]];

AppendTo[allIP,Table[{var[[1]]/.sol[[loop3]],exp/.sol[[loop3]]},{loop3,Length[sol]}]];

AppendTo[points,Table[Tooltip[{var[[1]]/.sol[[loop3]],exp/.sol[[loop3]]},tooltip[{var[[1]]/.sol[[loop3]],exp/.sol[[loop3]]},StringForm["(`1`, `2`) : intersection point between `` and ``",realChecker[var[[1]]/.sol[[loop3]]],realChecker[exp/.sol[[loop3]]],functions[[loop1]],functions[[loop2]]]]],{loop3,Length[sol]}]]],{loop2,Length[functions]}]];

Table[If[allIP[[loop2]]==={},allIP[[loop2]]={{"None"}}],{loop2,Length[allIP]}];

lines={};

(* finding asymptotes only if the user wants them *)

Quiet[If[OptionValue[Asy]=="N",Quiet[inv=y/.Normal[Solve[(exp/.var[[1]]->y)==var[[1]],y]][[1]]];

(* finding asymptotes with an x exponent degree of larger than 0 *)

asy["Horizontal"]={If[NumericQ[#],#,Nothing]&[Limit[exp,var[[1]]->\[Infinity]]],If[NumericQ[#],#,Nothing]&[Limit[exp,var[[1]]->-\[Infinity]]]};

If[Head[exp]===RealAbs||Head[exp]===Abs,tempExp=exp/.{RealAbs->singleArgExtracter,Abs->singleArgExtracter},tempExp=exp];

numerator=Numerator[Together[tempExp]];

denominator=Denominator[Together[tempExp]];

If[NumericQ[#]===True,If[NumericQ[#2],If[#=!=0,AppendTo[asy["Slant"],# var[[1]]+#2],AppendTo[asy["Horizontal"],#2]]]&[#,Limit[tempExp-# var[[1]],var[[1]]->-\[Infinity]]]]&[Limit[tempExp/var[[1]],var[[1]]->-\[Infinity]]];

If[NumericQ[#]===True,If[NumericQ[#2],If[#=!=0,AppendTo[asy["Slant"],# var[[1]]+#2],AppendTo[asy["Horizontal"],#2]]]&[#,Limit[tempExp-# var[[1]],var[[1]]->\[Infinity]]]]&[Limit[tempExp/var[[1]],var[[1]]->\[Infinity]]];

If[Exponent[numerator,var[[1]]]>Exponent[denominator,var[[1]]],

AppendTo[asy["Slant"],Quiet[Check[PolynomialQuotient[numerator,denominator,var[[1]]],Null]]];

If[Expand[exp]=!=Expand[tempExp],AppendTo[asy["Slant"],RealAbs[asy["Slant"][[-1]]]];asy["Slant"]=ReplacePart[asy["Slant"],-2->Nothing]]

];

(* graph intersection with asymptote *)

asy["Slant"]=DeleteDuplicates[asy["Slant"]];

asy["Slant"]=DeleteCases[(Expand[#]&/@asy["Slant"]),Expand[exp]];

Table[If[slantAsy=!=tempExp,AppendTo[points,Table[Tooltip[{loop2,slantAsy/.var[[1]]->loop2},tooltip[{loop2,slantAsy/.var[[1]]->loop2},StringForm["(``,``) : `` asymptopic intersection ``",loop2,slantAsy/.var[[1]]->loop2,exp,slantAsy]]],{loop2,#}]]&[convertRoots[var[[1]]/.Solve[asy["Slant"]==exp&&var[[2]]<=var[[1]]<=var[[3]]&&cond,Reals]]],asy["Slant"]=(asy["Slant"]/.slantAsy->Nothing)]
,{slantAsy,asy["Slant"]}];

(* using function discon to find the breaks in the graph and assuming they are all asymptotes *)

asy["Vertical"]=DeleteDuplicates[var[[1]]/.Solve[System`FunctionDiscontinuities[exp&&var[[2]]<=var[[1]]<=var[[3]]&&cond,var[[1]]]&&var[[2]]<=var[[1]]<=var[[3]]&&cond,var[[1]]]];

If[Head[asy["Vertical"]]=!=List,asy["Vertical"]={}];

asy["Vertical"]=DeleteCases[asy["Vertical"],Alternatives@@(#[[1]]&/@tempEP)];

Table[If[True===NumericQ[#[[2]]]===!NumericQ[Quiet[exp/.var[[1]]->asy["Vertical"][[vert]]]],

(AppendTo[asy["Hole"],#];AppendTo[points,Tooltip[#,tooltip[#,StringForm["(``, ``) : Hole in ``",#[[1]],#[[2]],exp]]]])

]&[{asy["Vertical"][[vert]],Limit[exp,var[[1]]->asy["Vertical"][[vert]],Direction->"FromAbove"]}];

If[True===NumericQ[#[[2]]]===!NumericQ[Quiet[exp/.var[[1]]->asy["Vertical"][[vert]]]],

(AppendTo[asy["Hole"],#];AppendTo[points,Tooltip[#,tooltip[#,StringForm["(``, ``) : Hole in ``",#[[1]],#[[2]],exp]]]])

]&[{asy["Vertical"][[vert]],Limit[exp,var[[1]]->asy["Vertical"][[vert]],Direction->"FromBelow"]}];,

{vert,Length[asy["Vertical"]]}];



asy["Vertical"]=DeleteCases[Select[DeleteDuplicates[Flatten[asy["Vertical"]]],FreeQ[#,_Complex]&],_Infinity];

asy["Vertical"]=DeleteCases[asy["Vertical"],Alternatives@@Table[hole[[1]],{hole,asy["Hole"]}]];

asy["Horizontal"]=DeleteCases[Select[DeleteDuplicates[Flatten[asy["Horizontal"]]],FreeQ[#,_Complex]&],_Infinity];

AppendTo[points,Table[Table[Tooltip[{If[Head[var[[1]]/.loop3]===Root,N[var[[1]]/.loop3],var[[1]]/.loop3],loop2},tooltip[{If[Head[var[[1]]/.loop3]===Root,N[var[[1]]/.loop3],var[[1]]/.loop3],loop2},StringForm["(``, ``) : `` and y = `` horizontal asymptote intersection",realChecker[var[[1]]/.loop3],loop2,exp,loop2]]],{loop3,#}]&[Flatten[Solve[exp==loop2&&var[[2]]<=var[[1]]<=var[[3]]&&cond,Reals]]],{loop2,asy["Horizontal"]}]];

(* asymptote line creation to put onto the graph *)

AppendTo[lines,Table[Tooltip[Line[{{asy["Vertical"][[loop2]],range[[1]]},{asy["Vertical"][[loop2]],range[[2]]}}],tooltip[StringForm["`` = ``",var[[1]],asy["Vertical"][[loop2]]],StringForm["`` = `` : Vertical asymptote for graph  ``",var[[1]],asy["Vertical"][[loop2]],exp]]],{loop2,Length[asy["Vertical"]]}]];

AppendTo[lines,Table[Tooltip[Line[{{var[[2]],asy["Horizontal"][[loop2]]},{var[[3]],asy["Horizontal"][[loop2]]}}],tooltip[StringForm["y = ``",asy["Horizontal"][[loop2]]],StringForm["y = `` : Horizontal asymptote for graph  ``",asy["Horizontal"][[loop2]],exp]]],{loop2,Length[asy["Horizontal"]]}]];

]];

(* if the thing above doesnt work use this? tbh it doesnt work sometimes too tho *)

If[OptionValue[Asy]=="F",asy=ResourceFunction["Asymptotes"][exp,var[[1]],y];

(* lots of lines creation and checking if the key exists in the list *)

If[KeyExistsQ[asy,"Vertical"],

AppendTo[lines,Table[If[NumericQ[var[[1]]/.asy["Vertical"][[loop2]]],

Tooltip[Line[{{var[[1]]/.asy["Vertical"][[loop2]],range[[1]]},{var[[1]]/.asy["Vertical"][[loop2]],range[[2]]}}],tooltip[StringForm["`` = ``",var[[1]],var[[1]]/.asy["Vertical"][[loop2]]],StringForm["`` = `` : Vertical Asymptote for graph ``",var[[1]],var[[1]]/.asy["Vertical"][[loop2]],exp]]]],{loop2,Length[asy["Vertical"]]}]]];

If[KeyExistsQ[asy,"Horizontal"],

AppendTo[lines,Table[If[NumericQ[y/.asy["Horizontal"][[loop2]]],

Tooltip[Line[{{var[[2]],y/.asy["Horizontal"][[loop2]]},{var[[3]],y/.asy["Horizontal"][[loop2]]}}],

tooltip[StringForm["y = ``",y/.asy["Horizontal"][[loop2]]],StringForm["y = `` : Horzontal Asymptote for graph ``",y/.asy["Horizontal"][[loop2]],exp]]]],{loop2,Length[asy["Horizontal"]]}]]]];

(* the big returning stuff so all the stuff found above is actually used *)

{Graphics[{Red,thicc,Dashed,Flatten[lines]}],Flatten[kermonds],If[KeyExistsQ[asy,"Slant"]&&asy["Slant"]=!=Null,

(* asy sant plot code here com back*)
Plot[Evaluate[Table[Tooltip[slantAsy,tooltip[slantAsy,StringForm["`` : Oblique asymptote for ``",slantAsy,exp]]],{slantAsy,asy["Slant"]}]],var,PlotRange->range,PlotStyle->Directive[{Red,Dashed,thicc}]],Nothing],

If[KeyExistsQ[asy,"Oblique"],Plot[Tooltip[y/.asy["Oblique"][[1]],tooltip[y/.asy["Oblique"][[1]],StringForm["`` Slant asymptote for graph ``",y/.asy["Oblique"][[1]],exp]]],var,PlotRange->range,PlotStyle->Directive[{Red,Dashed,thicc}]],Nothing],

AppendTo[points,Table[Tooltip[ep,tooltip[ep,StringForm["(``, ``) : End Point for graph ``",ep[[1]],ep[[2]],exp]]],{ep,tempEP}]];ListPlot[Flatten[points],PlotStyle->RGBColor[Random[],Random[],Random[]]],

(* stuff for the button to know otherwise whats the point of the button? *)

AppendTo[btninfo,TableForm[Table[#[[loop3,2]],{loop3,Length[#]}],TableDepth->1,TableHeadings->{Table[#[[loop3,1]],{loop3,Length[#]}]},TableAlignments->Left,TableSpacing->{2,2}]&[{{"Equation: ",exp},{"Derivative: ",D[exp,var[[1]]]},{"Integral: " ,Integrate[exp,var[[1]]]},{"Largest dom: ",If[#,var[[1]]\[Element]Reals,"No real dom...",#]&[FunctionDomain[exp,var[[1]],Reals]]},{"Largest Range: ",If[#,y\[Element]Reals,"No real range...",#]&[FunctionRange[exp,var[[1]],y,Reals]]},{StringForm["`` ints: ",var[[1]]],xint},{"y int:",y->yint},{"TP: ",Table[{var[[1]]/.loop3,exp/.loop3},{loop3,tp}]},{"End Points: ",tempEP},If[OptionValue[Asy]==="N",{"Vertical Asy: ",asy["Vertical"]},Nothing],If[OptionValue[Asy]==="N",{"Horizontal Asy: ",asy["Horizontal"]},Nothing],If[OptionValue[Asy]==="N",{"Oblique Asy: ",asy["Slant"]},Nothing],If[OptionValue[Asy]==="N",{"Holes: ",asy["Hole"]},Nothing],If[OptionValue[Asy]==="F",{"Vertical Asy: ",If[KeyExistsQ[asy,"Vertical"],x/.asy["Vertical"],"None"]},Nothing],If[OptionValue[Asy]==="F",{"Horizontal Asy: ",If[KeyExistsQ[asy,"Horizontal"],y/.asy["Horizontal"],"None"]},Nothing],If[OptionValue[Asy]==="F",{"Oblique Asy: ",If[KeyExistsQ[asy,"Oblique"],y/.asy["Oblique"],"None"]},Nothing],If[Length[functions]>1,Join[{"Graphs:"},Table[If[loop1!=loop2,StringForm["``",functions[[loop2]]],Nothing],{loop2,Length[functions]}]],Nothing],If[Length[functions]>1,Join[{"POI"},allIP],Nothing]}]];

},{loop1,Length[functions]}]]&[(* if for some reason you really love multiprocessing which theres really no point most of the time *)If[Length[functions]<5||OptionValue[Parallel]===False,Table,ParallelTable]];

graphsFinal=DeleteCases[DeleteCases[Flatten[graphs],Null],Graphics[White]];

(* button creation with all in the info *)

btns=Prepend[Append[Table[With[{i=i,info=btninfo},Button[functions[[i]],CreateDialog[{info[[i]],DefaultButton[]},WindowTitle->"Graph: "<>ToString[i],WindowElements->{"VerticalScrollBar","HorizontalScrollBar"}]]],{i,Length[functions]}],Button["All graphs",CreateDialog[{Table[With[{i=i,info=btninfo},info[[i]]],{i,Length[functions]}],DefaultButton[]},WindowTitle->"All Graphs",WindowElements->{"HorizontalScrollBar"}]]],Button["Features/Help",CreateDialog[{Style[StringForm[Information[WiggaPlot,"Usage"]],13],DefaultButton[]},WindowTitle->"WiggaPlot Help",WindowFrameElements->{"CloseBox","ResizeArea","MinimizeBox","ZoomBox"},WindowElements->{"VerticalScrollBar","HorizontalScrollBar"}]]];
(* showing the graphs and everything *)
Labeled[Labeled[Show[Flatten[{graphsFinal}]],{"The WiggaPlot","Extra info on these graphs:"},{Top,Bottom}],Grid[{btns},ItemSize->All]]]/.{Tooltip->(If[OptionValue[Labels]===False,#&,Tooltip])}]


(* ::Subsubsection::Closed:: *)
(*Options -> WiggaPlot*)


Options[WiggaPlot]=Join[Options[Plot],{Asy->"N",BoundArea->"N",Int->False,TN->Null,Inv->False,Labels->True,BackGround->"Base",Parallel->False,KermondTP->False,DecimalPoints->6,ExpandFunctions->False}];


(* ::Section:: *)
(*End*)


End[]
