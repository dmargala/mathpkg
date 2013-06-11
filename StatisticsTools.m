(* ::Package:: *)

(* Created 11-June-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`StatisticsTools`"]


StatisticsTools::usage="A collection of statistical utilities.";


chiSquareProbability::usage=
"chiSquareProbability[chisq,dof] returns the probability of obtaining a chisq less
than the specified value for the specified number of degrees of freedom.";


gaussianChiSquareContourLevel::usage=
"gaussianChiSquareContourLevel[coverage,ndim] returns the change in chisq that
encloses the specified coverage fraction for a Gaussian in ndim dimensions.";


Begin["Private`"]


chiSquareProbability[chisq_,dof_]:=N[GammaRegularized[dof/2,0,chisq/2]]


gaussianChiSquareContourLevel[coverage_,ndim_]:=
gaussianChiSquareContourLevel[coverage,ndim]=
Module[{dchisq},dchisq/.FindRoot[chiSquareProbability[dchisq,ndim]==coverage,{dchisq,1}]]
SetAttributes[gaussianChiSquareContourLevel,Listable]


End[]


EndPackage[]
