(* ::Package:: *)

(* Created 11-June-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`StatisticsTools`"]


StatisticsTools::usage="A collection of statistical utilities.";


chiSquareProbability::usage=
"chiSquareProbability[chisq,dof] returns the probability of obtaining a chisq greater
than the specified value for the specified number of degrees of freedom.";


gaussianChiSquareContourLevel::usage=
"gaussianChiSquareContourLevel[coverage,ndim] returns the change in chisq that
encloses the specified coverage fraction for a Gaussian in ndim dimensions.";


generateGaussianSamples::usage=
"generateGaussianSamples[cov,n] generates a list of n random samples of a
multidimensional with the specified covariance. The following options are supported:
  - mean : mean vector to use, with same dimension as cov (default is zero vector).";


correlationMatrix::usage=
"correlationMatrix[cov] returns the symmetric matrix of correlation coefficients
cov(i,j)/sqrt(cov(i)cov(j)), whose diagonal elements are all one and whose
off-diagonal elements are all between -1 and +1.";


Begin["Private`"]


chiSquareProbability[chisq_,dof_]:=N[1-GammaRegularized[dof/2,0,chisq/2]]
SetAttributes[chiSquareProbability,Listable]


gaussianChiSquareContourLevel[coverage_,ndim_]:=
gaussianChiSquareContourLevel[coverage,ndim]=
Module[{dchisq},dchisq/.FindRoot[chiSquareProbability[dchisq,ndim]==1-coverage,{dchisq,1}]]
SetAttributes[gaussianChiSquareContourLevel,Listable]


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


correlationMatrix::notsymm="Covariance matrix is not symmetric.";
correlationMatrix::notposdef="Covariance matrix is not positive definite.";
correlationMatrix[cov_]:=
Module[{sigvec,sigmat},
  If[!SymmetricMatrixQ[cov],
    Message[correlationMatrix::notsymm];
    Return[$Failed]
  ];
  If[!PositiveDefiniteMatrixQ[cov],
    Message[correlationMatrix::notposdef];
    Return[$Failed]
  ];
  sigvec=N[Sqrt[Diagonal[cov]]];
  sigmat=ConstantArray[sigvec,Length[sigvec]];
  cov/(sigmat Transpose[sigmat])
]


End[]


EndPackage[]
