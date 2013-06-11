(* ::Package:: *)

(* Created 11-June-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`StatisticsTools`"]


StatisticsTools::usage="A collection of statistical utilities.";


chiSquareProbability::usage=
"chiSquareProbability[chisq,dof] returns the probability of obtaining a chisq less
than the specified value for the specified number of degrees of freedom.";


Begin["Private`"]


chiSquareProbability[chisq_,dof_]:=N[GammaRegularized[dof/2,0,chisq/2]]


End[]


EndPackage[]
