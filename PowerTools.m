(* ::Package:: *)

(* Created 4-Jan-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`PowerTools`"]


PowerTools::usage=
"A collection of utilities for working with cosmological power spectra.";


makePower::usage=
"makePower[data] uses a list of tabulated values {k,P(k)} in data to create a function that
interpolates in log(k) and P(k) and extrapolates using power laws in k. The following options
are supported:
  verbose (False) - set True to print out kmin, kmax, and the number of points being used.
  extrapolateBelow (True) - set False to print a message if extrapolation below kmin is attempted.
  extrapolateAbove (True) - set False to print a message if extrapolation above kmax is attempted.";


createDistortionModel::usage=
"createDistortionModel[name] associates the following definitions with the symbol name:
 - redshiftSpaceDistortion[name][k,mu]
 - nonlinearDistortion[name][k,mu]
 - transformedCoordinates[name][kp,mup]
Separate help is available for each of these definitions. Use the following options
(defaults in parentheses) to customize the created distortion model:
 - bias (1) tracer-1 bias
 - beta (0) tracer-1 redshift-space linear distortion parameter
 - bias2(Automatic) tracer-2 bias or same as tracer-1 bias if Automatic
 - beta2(Automatic) tracer-2 beta or same as tracer-1 beta if Automatic
 - sigL (0) longitudinal component of non-linear broadening sigma
 - sigT (0) transverse component of non-linear broadening sigma
 - sigS (0) fingers of god sigma
 - \[Alpha]L   (1) rescaling of (k,\[Mu]) along \[Mu]=1 axis
 - \[Alpha]T   (1) rescaling of (k,\[Mu]) along \[Mu]=0 axis
Use OptionValue[name,opt] to get option values associated with a named distortion model.
To clear a previously defined model, use Clear[name].";


redshiftSpaceDistortion::usage=
"redshiftSpaceDistortion[model][k,mu] calculates the redshift-space distortion factor.";


nonlinearDistortion::usage=
"nonlinearDistortion[model][k,mu] calculates the non-linear distortion factor.";


transformedCoordinates::usage=
"transformedCoordinates[name][k,mu] returns the transformed coordinates {k',mu'}.";


distortionMultipole::usage=
"distortionMultipole[model,k,ell] calculates the specified multipole of the specified distortion model at the specified k.";


sbTransform::usage=
"sbTransform[plfunc,rmin,rmax,ell,veps] calculates the spherical Bessel transform
of the specified function for multipole ell. Returns an interpolating function
defined for r in [rmin,rmax] that is free of any aliasing artifacts.
Use the veps parameter to control the numerical accuracy of the result.
sbTransform[plfunc,rmin,rmax,ell,veps,True] displays the k range and
sampling that is being used, which can be useful in building a suitable plfunc.";


Begin["Private`"]


powerLaw[{{k1_,p1_},{k2_,p2_}}]:=
Module[{a,c},
	a=Log[p2/p1]/Log[k2/k1];
	c=p1/k1^a;
	Function[k,c k^a]
]


Clear[makePower]
makePower[tabulated_,OptionsPattern[]]:=
With[{verbose=OptionValue["verbose"],extrapolateBelow=OptionValue["extrapolateBelow"],extrapolateAbove=OptionValue["extrapolateAbove"]},
Module[{interpolator,kmin,kmax,plo,phi},
	interpolator=Interpolation[tabulated/.{k_,Pk_}:>{Log[k],Pk}];
	kmin=tabulated[[1,1]];
	kmax=tabulated[[-1,1]];
    If[verbose===True,Print["makePower using ",Length[tabulated]," points covering ",kmin," <= k <= ",kmax]];
	plo=If[extrapolateBelow===True,powerLaw[tabulated[[;;2]]],Message[makePower::ExtrapolationDisabled,#1,"<",kmin]&];
	phi=If[extrapolateAbove===True,powerLaw[tabulated[[-2;;]]],Message[makePower::ExtrapolationDisabled,#1,">",kmax]&];
	Function[k,
		Which[
			k<=kmin,plo[k],
			k>=kmax,phi[k],
			True,interpolator[Log[k]]
		]
	]
]]
Options[makePower]={"verbose"->False,"extrapolateBelow"->True,"extrapolateAbove"->True};
makePower::ExtrapolationDisabled="k = `1` is `2` `3`.";


Clear[createDistortionModel]
createDistortionModel[name_,OptionsPattern[]]:=
With[{
    bias=OptionValue["bias"],
    beta=OptionValue["beta"],
    bias2Option=OptionValue["bias2"],
    beta2Option=OptionValue["beta2"],
    sigL=OptionValue["sigL"],
    sigT=OptionValue["sigT"],
    sigS=OptionValue["sigS"],
    \[Alpha]L=OptionValue["\[Alpha]L"],
    \[Alpha]T=OptionValue["\[Alpha]T"]
},
Module[{bias2,beta2},
    bias2=If[bias2Option===Automatic,bias,bias2Option];
    beta2=If[beta2Option===Automatic,beta,beta2Option];
    Options[name]^={ "bias"->bias,"beta"->beta,"bias2"->bias,"beta2"->beta2,"sigL"->sigL,"sigT"->sigT,"sigS"->sigS,"\[Alpha]L"->\[Alpha]L,"\[Alpha]T"->\[Alpha]T };
    redshiftSpaceDistortion[name]^=Function[{k,mu},Evaluate[Simplify[bias bias2(1+beta mu^2)(1+beta2 mu^2)]]];
    nonlinearDistortion[name]^=Function[{k,mu},Evaluate[Simplify[
        Exp[-(mu^2 sigL^2+(1-mu^2)sigT^2)k^2/2]/(1+(mu sigS k)^2)^2]]];
    transformedCoordinates[name]^=Function[{k,mu},Evaluate[PowerExpand[Simplify[
        With[{\[Alpha]=Sqrt[\[Alpha]L^2 mu^2 + \[Alpha]T^2 (1-mu^2)]},{\[Alpha] k,\[Alpha]L/\[Alpha] mu}]]]]];
]]
SetAttributes[createDistortionModel,HoldFirst]
Options[createDistortionModel]={
    "bias"->1,"beta"->0,"bias2"->Automatic,"beta2"->Automatic,"sigL"->0,"sigT"->0,"sigS"->0,"\[Alpha]L"->1,"\[Alpha]T"->1
};


distortionMultipole[name_,k_,ell_]:=
  (2 ell+1)/2 NIntegrate[redshiftSpaceDistortion[name][k,mu]nonlinearDistortion[name][k,mu]LegendreP[ell,mu],{mu,-1,+1}]


epsApprox[veps_,ell_]:=
With[{c=ds[ell,1]},
Module[{L0,L1,L2,tmp},
L0=veps/c//N;
Assert[L0<0.35];
L1=Log[L0];
L2=Log[-L1];
tmp=-L0/(6L1^3)(6 L1^4+6L1^2L2(L1+1)-3L1 L2(L2-2)+L2(2L2^2-9L2+6));
tmp^((ell+1)/2)
]]


kr0[ell_]:=2^(-((-1 - ell)/(1 + ell))) \[Pi]^(-(1/(2 + 2 ell)))Gamma[3/2 + ell]^(1/(1 + ell))


nds[ell_,eps_,aligned_:True]:=
Module[{kr,Y,\[Delta],nds0},
kr=kr0[ell];
Y=(kr/(2\[Pi]))eps^(-2/(ell+1));
\[Delta]=Log[Ceiling[Y]/Y];
nds0=-2/(ell+1)Log[eps];
If[aligned,nds0+\[Delta],nds0]
]


ds[ell_,eps_]:=1/2 eps^(2/(1 + ell)) \[Pi]^(1 + 1/(2 + 2 ell))Gamma[3/2 + ell]^(-(1/(1 + ell)))


ff[s_,ell_,kr0_,\[Alpha]_]:=Exp[\[Alpha] s]SphericalBesselJ[ell,kr0 Exp[s]]


gg[s_,plfunc_,ell_,k0_,\[Alpha]_]:=I^ell/(2\[Pi]^2)Exp[(3-\[Alpha])s]k0^3 plfunc[k0 Exp[s]]


wrap[n_,nmax_]:=If[n<nmax,n,n-2nmax]


getndsf[veps_,ell_]:=Module[{eps,ndsf,dsfmax,nsf,dsf},
  eps=epsApprox[veps,ell];
  ndsf=nds[ell,eps,True];
  dsfmax=Min[ds[ell,eps],Log[10]/40];
  nsf=Ceiling[ndsf/dsfmax];
  dsf=ndsf/nsf;
  {nsf,dsf}
]


Clear[sbTransformWork]
sbTransformWork[plfunc_,rmin_,rmax_,ell_,veps_,verbose_]:=
Module[{nsf,dsf,kr,k0,r0,nsg,ntot,n,\[Alpha],fdata,fnorm,gdata,fgdata,rgrid,xigrid,rzoom,xizoom,popts},
{nsf,dsf}=getndsf[veps,ell];
(*eps=epsApprox[veps,ell];
ndsf=nds[ell,eps,True];
dsfmax=Min[ds[ell,eps],Log[10]/40];
nsf=Ceiling[ndsf/dsfmax];
dsf=ndsf/nsf;*)
kr=kr0[ell]//N;
r0=Sqrt[rmin rmax];
k0=kr/r0;
(* Calculate the number of samples needed to cover (rmin,rmax) *)
nsg=Ceiling[Log[rmax/rmin]/(2dsf)];
ntot=nsf+nsg;
If[verbose,Print[k0 Exp[-ntot dsf]," \[LessEqual] k \[LessEqual] ",k0 Exp[+ntot dsf]," is covered with ",2 ntot," samples (",1/dsf," per logint)."]];
\[Alpha]=(1-ell)/2;
fdata=Table[
n=wrap[m,nsg+nsf];
If[Abs[n]<=nsf,ff[n dsf,ell,kr,\[Alpha]]dsf,0],
{m,0,2(nsg+nsf)-1}
];
(* Note that we use -s here ! *)
gdata=Table[gg[-n dsf,plfunc,ell,k0,\[Alpha]]dsf,{n,-ntot,ntot-1}];
fgdata=Re[Fourier[
Fourier[fdata,FourierParameters->{1,-1}]Fourier[gdata,FourierParameters->{1,-1}]/(2ntot),
FourierParameters->{1,+1}
]];
rgrid=Table[r0 Exp[n dsf],{n,-ntot,ntot-1}];
xigrid=fgdata (rgrid/r0)^(-\[Alpha])/dsf;
{fdata,gdata,fgdata,rgrid,xigrid,nsf}
]


Clear[sbTransform]
sbTransform[plfunc_,rmin_,rmax_,ell_,veps_,verbose_:False]:=
Module[{fdata,gdata,fgdata,rgrid,xigrid,nsf,rzoom,xizoom,interpolator},
{fdata,gdata,fgdata,rgrid,xigrid,nsf}=sbTransformWork[plfunc,rmin,rmax,ell,veps,verbose];
rzoom=rgrid[[nsf+1;;-nsf-1]];
xizoom=xigrid[[nsf+1;;-nsf-1]];
interpolator=Interpolation[Transpose[{Log[rzoom], xizoom}]];
Function[r,interpolator[Log[r]]]
]


End[]


EndPackage[]
