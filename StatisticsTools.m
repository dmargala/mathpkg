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


generateGaussianSamples::usage=
"generateGaussianSamples[cov,n] generates a list of n random samples of a
multidimensional with the specified covariance. The following options are supported:
  - mean : mean vector to use, with same dimension as cov (default is zero vector).";


Begin["Private`"]


chiSquareProbability[chisq_,dof_]:=N[GammaRegularized[dof/2,0,chisq/2]]
SetAttributes[chiSquareProbability,Listable]


gaussianChiSquareContourLevel[coverage_,ndim_]:=
gaussianChiSquareContourLevel[coverage,ndim]=
Module[{dchisq},dchisq/.FindRoot[chiSquareProbability[dchisq,ndim]==coverage,{dchisq,1}]]
SetAttributes[gaussianChiSquareContourLevel,Listable]


Clear[generateGaussianSamples]
generateGaussianSamples::badmean="Mean vector and covariance have different dimensions.";
generateGaussianSamples[cov_,n_,OptionsPattern[]]:=
With[{
  m=Length[cov],
  gauss=NormalDistribution[0,1],
  meanOption=OptionValue["mean"]
},
Module[{mean,cholesky},
  mean=If[meanOption===None,ConstantArray[0,m],meanOption];
  If[Length[mean]!=m,
    Message[generateGaussianSamples::badmean];
    Return[$Failed]
  ];
  cholesky=Transpose[CholeskyDecomposition[cov]];
  Thread[mean + cholesky.RandomVariate[gauss,{m,n}]]
]]
Options[generateGaussianSamples]={"mean"->None};


End[]


EndPackage[]
