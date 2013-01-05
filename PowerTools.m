(* ::Package:: *)
(* Created 4-Jan-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`PowerTools`"]


PowerTools::usage=
"A collection of utilities for working with cosmological power spectra."


loadPower::usage=
"Loads a tabulated power spectrum from a text file containing one value of k and P(k)
separated by white space on each line. Returns a function that interpolates
in log(k) and log(P(k)) and extrapolates using power laws in k."


sbTransform::usage=
"Calculates the spherical Bessel transform of the specified function for multipole ell.
Returns a function defined for r in [rmin,rmax] that is free of any aliasing artifacts.
Use the veps parameter to control the numerical accuracy of the result."


Begin["Private`"]


powerLaw[{{k1_,p1_},{k2_,p2_}}]:=
Module[{a,c},
	a=Log[p2/p1]/Log[k2/k1];
	c=p1/k1^a;
	Function[k,c k^a]
]


loadPower[filename_]:=
With[{tabulated=Import[filename]},
	Module[{interpolator,kmin,kmax,plo,phi},
		interpolator=Interpolation[Log[tabulated]];
		kmin=tabulated[[1,1]];
		kmax=tabulated[[-1,1]];
		plo=powerLaw[tabulated[[;;2]]];
		phi=powerLaw[tabulated[[-2;;]]];
		Function[k,
			Which[
				k<=kmin,plo[k],
				k>=kmax,phi[k],
				True,Exp[interpolator[Log[k]]]
			]
		]
	]
]


epsApprox[veps_,ell_]:=
With[{c=ds[ell,1]},
Module[{L0,L1,L2,tmp},
L0=veps/c//N;
Assert[L0<0.35];
L1=Log[L0];
L2=Log[-L1];
tmp=-L0/(6L1^3)(6 L1^4+6L1^2L2(L1+1)-3L1 L2(L2-2)+L2(2L2^2-9L2+6));
tmp^((ell+1)/2)
]
]


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


sbTransformWork[plfunc_,rmin_,rmax_,ell_,veps_]:=
Module[{eps,ndsf,nsf,dsfmax,dsf,kr,k0,r0,nsg,ntot,n,\[Alpha],fdata,fnorm,gdata,fgdata,rgrid,xigrid,rzoom,xizoom,popts},
eps=epsApprox[veps,ell];
ndsf=nds[ell,eps,True];
dsfmax=Min[ds[ell,eps],Log[10]/40];
nsf=Ceiling[ndsf/dsfmax];
dsf=ndsf/nsf;
kr=kr0[ell]//N;
r0=Sqrt[rmin rmax];
k0=kr/r0;
(* Calculate the number of samples needed to cover (rmin,rmax) *)
nsg=Ceiling[Log[rmax/rmin]/(2dsf)];
ntot=nsf+nsg;
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


sbTransform[plfunc_,rmin_,rmax_,ell_,veps_]:=
Module[{fdata,gdata,fgdata,rgrid,xigrid,nsf,rzoom,xizoom,interpolator},
{fdata,gdata,fgdata,rgrid,xigrid,nsf}=sbTransformWork[plfunc,rmin,rmax,ell,veps];
rzoom=rgrid[[nsf+1;;-nsf-1]];
xizoom=xigrid[[nsf+1;;-nsf-1]];
interpolator=Interpolation[Transpose[{Log[rzoom], xizoom}]];
Function[r,interpolator[Log[r]]]
]


End[]


EndPackage[]
