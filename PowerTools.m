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
Use OptionValue[name,opt] to get option values associated with a named distortion model.
To clear a previously defined model, use Clear[name].";


redshiftSpaceDistortion::usage=
"redshiftSpaceDistortion[model][k,mu] calculates the redshift-space distortion factor.";


nonlinearDistortion::usage=
"nonlinearDistortion[model][k,mu] calculates the non-linear distortion factor.";


projectMultipole::usage=
"projectMultipole[f,ell] evaluates the multipole projection integral of f(mu)P(ell,mu) over {-1,+1}, scaled by (2 ell+1)/2.";


distortionMultipoleFunction::usage=
"distortionMultipoleFunction[model,kmin,kmax,ell,nPtsPerDecade] returns an interpolated function for kmin < k < kmax for the
specified distortion model multipole.";


sbTransform::usage=
"sbTransform[plfunc,rmin,rmax,ell,veps] calculates the spherical Bessel transform
of the specified function for multipole ell. Returns an interpolating function
defined for r in [rmin,rmax] that is free of any aliasing artifacts.
Use the veps parameter to control the numerical accuracy of the result.
sbTransform[plfunc,rmin,rmax,ell,veps,True] displays the k range and
sampling that is being used, which can be useful in building a suitable plfunc.";


createCorrelationFunction::usage=
"createCorrelationFunction[linearPower,rmin,rmax] returns a function of (r,mu) that evaluates the correlation function
corresponding to the specified linear theory power spectrum.";


createCorrelationMultipole::usage=
"createCorrelationMultipole[xi,rmin,rmax,ell] returns an interpolation function for the specified multipole of the specified
xi(r,mu) function, valid over the specified range of r. Options include:
  - alphaP : rescaling parallel to the line of sight (default 1)
  - alphaT : rescaling transverse to the line of sight (default 1)
Note that alphaP or alphaT different from 1 will generally require that xi(r,mu) be valid outside [rmin,rmax].";


generateGaussianRandomField::usage=
"generateGaussianRandomField[power,length,size] returns a 3D delta field sampled from
the specified power spectrum on a grid of the specified length and size.";


growthVarianceFunction::usage=
"growthVarianceFunction[power,kmin,kmax] returns a function that evaluates the variance of growth fluctuations
integrated from kmin to k with kmin <= k <= kmax. Options are:
 - pointsPerDecade (20) number of interpolation points to use per decade.
 - inverted (False) return inverse function k(var) instead of var(k) when True.";


constrainedXi2::usage=
"constrainedXi2[xi0,rmin,rmax,xi2rmin] returns an interpolated function valid on rmin <= r <= rmax
for the quadrupole derived from the specified monopole using eqn (2.14) of Kirkby 2013, with
xi2[rmin]=xi2rmin.";


constrainedXi4::usage=
"constrainedXi4[xi0,xi2,rmin,rmax,xi4rmin] returns an interpolated function valid on rmin <= r <= rmax
for the hexadecapole derived from the specified monopole and quadrupole using eqn (2.14) of Kirkby 2013, with
xi2[rmin]=xi2rmin.";


getXiPeak::usage=
"smoothedXi[xi,rvec,rpvec,modelTerms] returns an interpolated function for the peak in the specified correlation
function using value (rvec) and derivative (rpvec) matching constraints at the specified r values and
assuming a model of the smooth (peak-substracted shape) consisting of linear combination of the specified
terms. The peak is obtained by first calculating the linear coefficients of the smooth model S(r) using
constraints S(r)=xi(r) for all r in rvec and S'(r)=xi'(r) for all r in rpvec. The total number of constraint
points must match the number of model terms so that the solution is unique. The input xi must be defined at
all constraint points, which are assumed to all lie outside the peak region. The resulting peak model will be
zero except on the constraint r interval containing rpeak (see below). The following options are supported:
 - rpeak: the peak model is assumed non-zero in the constraint interval containing this r value (default 105).
 - plot: generates a diagnostic plot if True (default = True).
 - plotRMin: lower bound of the diagnostic plot.
 - plotRMax: upper bound of the diagnostic plot.
 - plotRPow: r-weighting power for the diagnostic plot.";


Begin["Private`"]


powerLaw[{{k1_,p1_},{k2_,p2_}}]:=
Module[{a,c},
	a=Log[p2/p1]/Log[k2/k1];
	c=p1/k1^a;
	Function[k,c k^a]
]


Clear[makePower]
makePower[tabulated_,OptionsPattern[]]:=
With[{
  verbose=OptionValue["verbose"],
  extrapolateBelow=OptionValue["extrapolateBelow"],
  extrapolateAbove=OptionValue["extrapolateAbove"],
  maxRelError=OptionValue["maxRelError"]
},
Module[{interpolator,kmin,kmax,plo,phi,relerr},
	interpolator=Interpolation[tabulated/.{k_,Pk_}:>{Log[k],Pk}];
	kmin=tabulated[[1,1]];
	kmax=tabulated[[-1,1]];
    If[verbose===True,Print["makePower using ",Length[tabulated]," points covering ",kmin," <= k <= ",kmax]];
	If[extrapolateBelow===True,
      plo=powerLaw[tabulated[[{1,3}]]];
      relerr=Abs[plo[tabulated[[2,1]]]/tabulated[[2,2]]-1];
      If[verbose,Print["Relative error for extrapolation below is ",relerr]];
      If[relerr > maxRelError,Message[makePower::UnreliableBelow]; Return[$Failed]]
      ,
      plo=Message[makePower::ExtrapolationDisabled,#1,"<",kmin]&
    ];
	If[extrapolateAbove===True,
      phi=powerLaw[tabulated[[{-3,-1}]]];
      relerr=Abs[phi[tabulated[[-2,1]]]/tabulated[[-2,2]]-1];
      If[verbose,Print["Relative error for extrapolation above is ",relerr]];
      If[relerr > maxRelError,Message[makePower::UnreliableAbove]; Return[$Failed]]
      ,
      phi=Message[makePower::ExtrapolationDisabled,#1,">",kmax]&
    ];
	Function[k,
		Which[
			k<=kmin,plo[k],
			k>=kmax,phi[k],
			True,interpolator[Log[k]]
		]
	]
]]
Options[makePower]={
  "verbose"->False,"extrapolateBelow"->True,"extrapolateAbove"->True,"maxRelError"->0.001
};
makePower::ExtrapolationDisabled="k = `1` is `2` `3`.";
makePower::UnreliableBelow="Cannot reliably extrapolate below kmin.";
makePower::UnreliableAbove="Cannot reliably extrapolate above kmax.";


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
    multipoleRescalingOption=OptionValue["multipoleRescaling"]
},
Module[{bias2,beta2},
    Clear[name];
    bias2=If[bias2Option===Automatic,bias,bias2Option];
    beta2=If[beta2Option===Automatic,beta,beta2Option];
    Options[name]^={ "bias"->bias,"beta"->beta,"bias2"->bias,"beta2"->beta2,"sigL"->sigL,"sigT"->sigT,"sigS"->sigS };
    redshiftSpaceDistortion[name]^=Function[{k,mu},Evaluate[Simplify[bias bias2(1+beta mu^2)(1+beta2 mu^2)]]];
    nonlinearDistortion[name]^=Function[{k,mu},Evaluate[Simplify[
        Exp[-(mu^2 sigL^2+(1-mu^2)sigT^2)k^2/2]/(1+(mu sigS k)^2)^2]]];
    multipoleRescaling[name]^=multipoleRescalingOption;
]]
SetAttributes[createDistortionModel,HoldFirst]
Options[createDistortionModel]={
    "bias"->1,"beta"->0,"bias2"->Automatic,"beta2"->Automatic,"sigL"->0,"sigT"->0,"sigS"->0,"multipoleRescaling"->None
};


projectMultipole[f_,ell_]:=(2 ell+1)/2 NIntegrate[f[mu]LegendreP[ell,mu],{mu,-1,+1},AccuracyGoal->12]


distortionMultipoleFunction[name_,kmin_,kmax_,ell_,OptionsPattern[]]:=
With[{
  nPtsPerDecade=OptionValue["nPtsPerDecade"],
  padFraction=OptionValue["padFraction"]
},
Module[{index,rescale,nk,dk,k,pts,interpolator},
  index=1+ell/2;
  rescale=Which[multipoleRescaling[name]===None,1,index<=Length[multipoleRescaling[name]],multipoleRescaling[name][[index]],True,1];
  nk=Max[10,Ceiling[Log[10,kmax/kmin]nPtsPerDecade]];
  dk=padFraction^2(kmax/kmin)^(1/(nk-1));
  pts=Table[
    k=(kmin/padFraction) dk^(i-1);
    {Log[k],projectMultipole[rescale redshiftSpaceDistortion[name][k,##] nonlinearDistortion[name][k,##]&,ell]},
    {i,nk}
  ];
  interpolator=Interpolation[pts];
  Function[k,interpolator[Log[k]]]
]]
Options[distortionMultipoleFunction]={"nPtsPerDecade"->5,"padFraction"->1.05};


kr0[ell_,hankel_:False]:=
If[hankel,
2 \[Pi]^(1/(-1-2 ell)) Gamma[1+ell]^(2/(1+2 ell)),
2 \[Pi]^(-(1/(2 + 2 ell)))Gamma[3/2 + ell]^(1/(1 + ell))]


s0[ell_,hankel_:False]:=If[hankel,4/(2 ell + 1), 2/(ell + 1)]


f0[ell_,hankel_:False]:=With[{kr=kr0[ell,hankel]},If[hankel,(2/(\[Pi] kr))^(1/2),1/kr]]


nds[ell_,eps_,aligned_:True,hankel_:False]:=
Module[{kr,s,Y,\[Delta],nds0},
kr=kr0[ell,hankel];
s=s0[ell,hankel];
Y=(kr/(2\[Pi]))eps^(-s);
\[Delta]=Log[If[hankel,(Ceiling[1/8+Y/4]-1/8)/Y,Ceiling[Y]/Y]];
nds0=-s Log[eps];
If[aligned,nds0+\[Delta],nds0]
]


ds[ell_,eps_,hankel_:False]:= \[Pi]/kr0[ell,hankel] eps^(s0[ell,hankel])


ff[s_,ell_,kr0_,\[Alpha]_]:=Exp[\[Alpha] s]SphericalBesselJ[ell,kr0 Exp[s]]


gg[s_,plfunc_,ell_,k0_,\[Alpha]_]:=I^ell/(2\[Pi]^2)Exp[(3-\[Alpha])s]k0^3 plfunc[k0 Exp[s]]


ffH[s_,ell_,kr0_,\[Alpha]_]:=Exp[\[Alpha] s]BesselJ[ell,kr0 Exp[s]]


ggH[s_,plfunc_,ell_,k0_,\[Alpha]_]:=Exp[(2-\[Alpha])s]k0^2 plfunc[k0 Exp[s]]


wrap[n_,nmax_]:=If[n<nmax,n,n-2nmax]


epsApprox[veps_,ell_,hankel_:False]:=
With[{c=\[Pi]/kr0[ell,hankel]},
Module[{L0,L1,L2,tmp},
L0=veps/c//N;
Assert[L0<0.35];
L1=Log[L0];
L2=Log[-L1];
tmp=-L0/(6L1^3)(6 L1^4+6L1^2L2(L1+1)-3L1 L2(L2-2)+L2(2L2^2-9L2+6));
tmp^(1/s0[ell,hankel])
]]


getndsf[veps_,ell_,hankel_]:=Module[{eps,ndsf,dsfmax,nsf,dsf},
  eps=epsApprox[veps,ell,hankel];
  ndsf=nds[ell,eps,True,hankel];
  dsfmax=Min[ds[ell,eps,hankel],Log[10]/40];
  nsf=Ceiling[ndsf/dsfmax];
  dsf=ndsf/nsf;
  {nsf,dsf}
]


Clear[sbTransformWork]
sbTransformWork[callback_,rmin_,rmax_,ell_,veps_,hankel_,verbose_]:=
Module[{nsf,dsf,kr,k0,r0,nsg,ntot,n,\[Alpha],ffunc,gfunc,fdata,fnorm,plfunc,gdata,fgdata,rgrid,xigrid,rzoom,xizoom,popts},
{nsf,dsf}=getndsf[veps,ell,hankel];
kr=kr0[ell,hankel]//N;
r0=Sqrt[rmin rmax];
k0=kr/r0;
(* Calculate the number of samples needed to cover (rmin,rmax) *)
nsg=Ceiling[Log[rmax/rmin]/(2dsf)];
ntot=nsf+nsg;
(* Get the Pl(k) function to use *)
plfunc=callback[k0 Exp[-ntot dsf],k0 Exp[+ntot dsf],2 ntot];
(* Start tabulating... *)
\[Alpha]=If[hankel,(1-2ell)/4,(1-ell)/2];
ffunc=If[hankel,ffH,ff];
fdata=Table[
  n=wrap[m,ntot];
  If[Abs[n]<=nsf,ffunc[n dsf,ell,kr,\[Alpha]]dsf,0],
  {m,0,2ntot-1}
];
(* Note that we use -s here ! *)
gfunc=If[hankel,ggH,gg];
gdata=Table[gfunc[-n dsf,plfunc,ell,k0,\[Alpha]]dsf,{n,-ntot,ntot-1}];
fgdata=Re[Fourier[
  Fourier[fdata,FourierParameters->{1,-1}] Fourier[gdata,FourierParameters->{1,-1}]/(2ntot),
  FourierParameters->{1,+1}
]];
rgrid=Table[r0 Exp[n dsf],{n,-ntot,ntot-1}];
xigrid=fgdata (rgrid/r0)^(-\[Alpha])/dsf;
{fdata,gdata,fgdata,rgrid,xigrid,nsf}
]


Clear[sbTransform]
sbTransform[plfunc_,rmin_,rmax_,ell_,veps_,OptionsPattern[]]:=
With[{
  verboseOption=OptionValue["verbose"],
  distortionOption=OptionValue["distortion"],
  distortionSamplingOption=OptionValue["distortionSampling"],
  padFractionOption=OptionValue["padFraction"],
  hankelOption=OptionValue["hankel"]
},
Module[{fdata,gdata,fgdata,rgrid,xigrid,nsf,rzoom,xizoom,interpolator,callback,distortion},
  callback=Function[{kmin,kmax,nk},
    If[verboseOption===True,Print[kmin " \[LessEqual] k \[LessEqual] ",kmax," is covered with ",nk," samples (",nk/Log[kmax/kmin]," per logint)."]];
    If[distortionOption===None,
      plfunc,
      If[verboseOption===True,Print["Sampling distortion model at ",distortionSamplingOption," points per decade."]];
      distortion=distortionMultipoleFunction[distortionOption,kmin,kmax,ell,"nPtsPerDecade"->distortionSamplingOption];
      Function[k,plfunc[k]distortion[k]]
    ]
  ];
  {fdata,gdata,fgdata,rgrid,xigrid,nsf}=sbTransformWork[callback,rmin/padFractionOption,rmax padFractionOption,ell,veps,hankelOption,verboseOption];
  rzoom=rgrid[[nsf+1;;-nsf-1]];
  xizoom=xigrid[[nsf+1;;-nsf-1]];
  interpolator=Interpolation[Transpose[{Log[rzoom], xizoom}]];
  Function[r,interpolator[Log[r]]]
]]
Options[sbTransform]={
"verbose"->False,"distortion"->None,"distortionSampling"->5,"padFraction"->1.05,"hankel"->False
};


createCorrelationFunction[linearPower_,rmin_,rmax_,OptionsPattern[]]:=
With[{
  verboseOption=OptionValue["verbose"],
  lmaxOption=OptionValue["lmax"],
  distortionOption=OptionValue["distortion"],
  distortionSamplingOption=OptionValue["distortionSampling"],
  vepsOption=OptionValue["veps"]
},
Module[{lvec,xi},
  lvec=Range[0,lmaxOption,2];
  xi=Table[
    sbTransform[linearPower,rmin,rmax,ell,vepsOption,
      "distortion"->distortionOption,"distortionSampling"->distortionSamplingOption,"verbose"->verboseOption],
    {ell,lvec}
  ];
  Function[{r,mu},Sum[xi[[j]][r]LegendreP[lvec[[j]],mu],{j,Length[lvec]}]]
]]
Options[createCorrelationFunction]={
  "verbose"->False,"lmax"->4,"distortion"->None,"distortionSampling"->5,"veps"->0.001
};


createCorrelationMultipole[xi_,rmin_,rmax_,ell_,OptionsPattern[]]:=
With[{
  alphaPOption=OptionValue["alphaP"],
  alphaTOption=OptionValue["alphaT"],
  npointsOption=OptionValue["npoints"]
},
Module[{alphaP,alphaT,npoints,rvec,f,pts},
  alphaP=alphaPOption;
  alphaT=alphaTOption;
  npoints=If[npointsOption===Automatic,Ceiling[rmax-rmin],npointsOption];
  rvec=Table[rmin+(j-1)(rmax-rmin)/(npoints-1),{j,npoints}];
  f=Function[{r,mu},With[{alpha=Sqrt[alphaP^2 mu^2 + alphaT^2 (1-mu^2)]},xi[alpha r,(alphaP/alpha)mu]]];
  pts=Table[{r,projectMultipole[f[r,##]&,ell]},{r,rvec}];
  Interpolation[pts]
]]
Options[createCorrelationMultipole]={
  "alphaP"->1,"alphaT"->1,"npoints"->Automatic
};


Clear[generateGaussianRandomField]
generateGaussianRandomField[power_,OptionsPattern[]]:=
With[{
  gridPoints=OptionValue["gridPoints"],
  gridLength=OptionValue["gridLength"],
  seed=OptionValue["seed"]
},
Module[{deltak,offset,dk,L3,rms},
  (* Set the random seed if requested *)
  If[!(seed===None),SeedRandom[seed]];
  (* Generate random phases at each grid point *)
  deltak=Exp[I Pi RandomReal[{0,1},{gridPoints,gridPoints,gridPoints}]];
  Print[{ByteCount[deltak],Developer`PackedArrayQ[deltak]}];
  (* Multiply by unit Gaussian random amplitudues at each grid point *)
  deltak*=RandomVariate[NormalDistribution[0,1],{gridPoints,gridPoints,gridPoints}];
  Print[{ByteCount[deltak],Developer`PackedArrayQ[deltak]}];
  (* Build the k grid along one axis using FFT indexing *)
  offset=Floor[(gridPoints-1)/2];
  dk=N[(2\[Pi])/gridLength];
  L3=N[2 gridLength^3];
  With[{
    kgrid=dk RotateLeft[Range[gridPoints]-offset-1,offset],
    rmsF=Compile[{{kx,_Real},{ky,_Real},{kz,_Real}},Sqrt[power[Sqrt[kx^2+ky^2+kz^2]]/L3]]
    },
    Print[Developer`PackedArrayQ[kgrid]];
    rms=Outer[List,kgrid,kgrid,kgrid];
    Print[Developer`PackedArrayQ[rms]];
    rms=Apply[rmsF,rms,{3}];
    Print[Developer`PackedArrayQ[rms]];
  ];
  Print[{ByteCount[deltak],Developer`PackedArrayQ[deltak]}];
  deltak*=rms;
  Re[InverseFourier[deltak]]
]]
Options[generateGaussianRandomField]={
  "gridPoints"->256,"gridLength"->100,"seed"->None
};


(* Builds a function that evaluates f[k]=scale*transform[k,Integral[integrand[kk],{kk,kmin,k}]] using interpolation
in log k with the specified number of points per decade. The default scale=1 and transform[k,f]=f.
With inverted->True, evaluates k[f] instead of f[k]. Based on the similar function in CosmoTools. *)
Clear[buildKFunction]
buildKFunction[integrand_,kmin_,kmax_,OptionsPattern[]]:=
With[{pointsPerDecade=OptionValue["pointsPerDecade"],inverted=OptionValue["inverted"],
scale=OptionValue["scale"],transform=OptionValue["transform"]},
Module[{npts,ds,sval,partials,tabulated,interpolator},
	npts=Ceiling[N[Log[kmax/kmin]pointsPerDecade/Log[10]]];
    (* Integrate over equally spaced intervals in s = log(k) *)
    ds=Log[kmax/kmin]/(npts-1);
	sval=Log[kmin]+Table[n ds,{n,0,npts-1}];
	partials=Table[NIntegrate[integrand[Exp[s]]Exp[s],{s,sval[[n]],sval[[n+1]]}],{n,1,npts-1}];
    (* Accumulate the partials and add the boundary condition that the integral is zero at k = kmin *)
	tabulated=Prepend[Accumulate[partials],0];
    (* Apply the scale and transform[k,f] to the cummulative integrals *)
    tabulated=scale Apply[transform,Transpose[{Exp[sval]-1,tabulated}],1];
    (* Create the requested interpolation f(k) or k(f) *)
    If[inverted===True,
        interpolator=Interpolation[Transpose[{tabulated,sval}]];
	        Function[f,Exp[interpolator[f]]],
    	interpolator=Interpolation[Transpose[{sval,tabulated}]];
            Function[k,interpolator[Log[k]]]
    ]
]]
Options[buildKFunction]={"pointsPerDecade"->20,"scale"->1,"transform"->(#2&),"inverted"->False};


growthVarianceFunction[power_,kmin_,kmax_,options:OptionsPattern[{buildKFunction}]]:=
  buildKFunction[##^2/(2\[Pi]^2) power[##]&,kmin,kmax,options]


constrainedXi2[xi0_,rmin_,rmax_,xi2rmin_]:=
With[{xi0rmin=xi0[rmin]},
Module[{integral,F,r},
  integral=F/.First[NDSolve[{F[rmin]==0,F'[r]==r^2 xi0[r]},F,{r,rmin,rmax}]];
  Function[r,xi0[r]+(rmin/r)^3(xi2rmin-xi0rmin)-(3/r^3)integral[r]]
]]


constrainedXi4[xi0_,xi2_,rmin_,rmax_,xi4rmin_]:=
With[{xi0rmin=xi0[rmin]},
Module[{integral,F,r},
  integral=F/.First[NDSolve[{F[rmin]==0,F'[r]==r^4 (xi0[r]+xi2[r])},F,{r,rmin,rmax}]];
  Function[r,xi0[r]+(rmin/r)^5(xi4rmin-xi0rmin)-(5/r^5)integral[r]]
]]


getXiPeak::badn="Number of constraints (`1`) does not match number of coefficients (`2`).";
getXiPeak::badpk="Peak at r=`1` is not bracketed by r values of constraints.";
getXiPeak::nonzero="Bracketing values `1` should both be in rvec to avoid steps in peak function.";
getXiPeak[xi_,rvec_,rpvec_,model_,OptionsPattern[]]:=
With[{
  nr=Length[rvec],
  nrp=Length[rpvec],
  ncoef=Length[model],
  rpeakOption=OptionValue["rpeak"],
  plotOption=OptionValue["plot"],
  plotRMin=OptionValue["plotRMin"],
  plotRMax=OptionValue["plotRMax"],
  plotRPow=OptionValue["plotRPow"]
},
Module[{M,r,f,fp,xip,b,coefs,smooth,rsort,ipeak,rlo,rhi},
  (* Check that the number of constraints matches the number of unknown coefficients *)
  If[nr+nrp!=ncoef,
    Message[getXiPeak::badn,nr+nrp,ncoef];
    Return[$Failed]
  ];
  (* Points in rvec and rpvec are all assumed to be in the smooth regions on either side
  of the peak. Figure out which r interval between these points contains the peak. *)
  rsort=Sort[Join[rvec,rpvec,{rpeakOption}]];
  ipeak=Flatten[Position[rsort,rpeakOption]];
  If[Length[ipeak]!=1||ipeak==1||ipeak==Length[rsort],
    Message[getXiPeak::badpk,rpeakOption];
    Return[$Failed]
  ];
  ipeak=First[ipeak];
  {rlo,rhi}=rsort[[{ipeak-1,ipeak+1}]];
  If[!MemberQ[rvec,rlo]||!MemberQ[rvec,rhi],
    Message[getXiPeak::nonzero,{rlo,rhi}];
    Return[$Failed]
  ];
  (* Build the coefficient matrix for the system of linear equations we need to solve *)
  M=ConstantArray[0,{ncoef,ncoef}];
  Do[
    f=Function[r,Evaluate[Simplify[model[[i]][r]]]];
    fp=Function[r,Evaluate[Simplify[D[f[r],r]]]];
    M[[;;nr,i]]=f/@rvec;
    M[[nr+1;;,i]]=fp/@rpvec;
    ,{i,ncoef}
  ];
  (* Build the corresponding vector of RHS values *)
  xip=Function[r,xi'[r]];
  b=Join[xi/@rvec,xip/@rpvec];
  coefs=LinearSolve[M,b];
  smooth=Function[r,Evaluate[Simplify[coefs.Through[model[r]]]]];
  If[plotOption===True,
    Print[Show[{
      Plot[r^plotRPow xi[r],{r,plotRMin,plotRMax},PlotStyle->{Thick,Blue},Frame->True],
      Plot[r^plotRPow smooth[r],{r,rlo,rhi},PlotStyle->{Opacity[0.5],Thick,Dashed,Red}],
      ListPlot[{#,#^plotRPow xi[#]}&/@rpvec,PlotStyle->{Black,Opacity[0.3],PointSize[0.04]}],
      ListPlot[{#,#^plotRPow xi[#]}&/@rsort,PlotStyle->{Red,Opacity[0.8],PointSize[0.02]}]
    }]]
  ];
  (* Return a peak function that is defined for all r and only non-zero on [rlo,rhi] *)
  Function[r,UnitStep[(r-rlo)(rhi-r)](xi[Clip[r,{rlo,rhi}]]-smooth[Clip[r,{rlo,rhi}]])]
]]
Options[getXiPeak]={"rpeak"->105,"plot"->True,"plotRMin"->50,"plotRMax"->190,"plotRPow"->2};


End[]


EndPackage[]
