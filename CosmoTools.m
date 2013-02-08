(* ::Package:: *)

(* Created 4-Jan-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

(* As of version 9, PhysicalConstants has been replaced by the built-in Quantity[],
but we keep using it here for backwards compatibility *)
Off[General::obspkg]
BeginPackage["DeepZot`CosmoTools`",{"Units`","PhysicalConstants`"}]


CosmoTools::usage=
"A collection of utilities for cosmological calculations."


criticalDensityToday::usage=
"criticalDensityToday[hValue] calcualtes the present critical density in J/m^3."


radiationDensity::usage=
"radiationDensity[Tcmb,Nnu] calculates the Stefan-Boltzmann radiation energy density
in J/m^3 for a CMB temperature Tcmb in Kelvin and Nnu massless neutrinos."


zstar::usage=
"zstar[\[CapitalOmega]mh2,\[CapitalOmega]bh2] calculates the redshift of last scattering using equation (E-1)
of Hu & Sugiyama 1996 (astro-ph/9510117)."


zeq::usage=
"zeq[\[CapitalOmega]mh2,Tcmb] calculates the redshift of matter-radiation equality using equation (2)
of Eistenstein & Hu 1998 (astro-ph/9709112)."


zdrag::usage=
"zdrag[\[CapitalOmega]mh2,\[CapitalOmega]bh2] calculates the redshift at which the baryon-photon fluid separates,
defined as the moment when the Compton drag experienced by the baryons falls below
some threshold, using equation (4) of Eistenstein & Hu 1998 (astro-ph/9709112)."


createCosmology::usage=
"createCosmology[name] associates the following definitions with the symbol name:
 - \[CapitalOmega]rad[name][z]
 - \[CapitalOmega]de[name][z]
 - \[CapitalOmega]mat[name][z]
 - Hratio[name][z]
 - zstar[name]
 - zeq[name]
 - zdrag[name]
Separate help is available for each of these definitions, e.g., ?\[CapitalOmega]rad.
Use the following options to customize the cosmology that is created:
 - h (0.7) H0/(100 km/s/Mpc).
 - \[CapitalOmega]\[Phi] (0.73) present-day fraction of dark energy.
 - \[CapitalOmega]bh2 (0.0227) present-day physical baryon fraction.
 - w0 (-1) dark-energy equation parameter state present-day value.
 - wa (0) dark-energy equation of state parameter derivative wrt scale a.
 - \[CapitalOmega]k (0) present-day curvature fraction.
 - Tcmb (2.725) present-day CMB temperature in Kelvin.
 - Nnu (3.046) effective number of massless neutrinos.
Use OptionValue[name,opt] to get option values associated with a cosmology.
To clear a previously defined cosmology, use Clear[name]. Use the name provided
here to identify the created cosmology in functions like comovingDistanceFunction."


\[CapitalOmega]rad::usage=
"\[CapitalOmega]rad[name][z] returns the radiation energy density relative to the critical density at the specified redshift."


\[CapitalOmega]de::usage=
"\[CapitalOmega]de[name][z] returns the dark-energy density relative to the critical density at the specified redshift."


\[CapitalOmega]mat::usage=
"\[CapitalOmega]mat[name][z] returns the matter energy density relative to the critical density at the specified redshift."


H0::usage=
"H0[name] returns H0 in (km/s)/Mpc."


Hratio::usage=
"Hratio[name][z] returns H(z)/H0."


comovingDistanceFunction::usage=
"comovingDistanceFunction[cosmology,zmax] returns a function that evaluates the comoving distance
to a redshift z <= zmax for the named cosmology. Possible options are:
 - physical (False) units are Mpc (True) or Mpc/h (False).
 - pointsPerDecade (20) number of interpolation points to use per decade."


angularDiameterDistanceFunction::usage
"angularDiameterDistanceFunction[cosmology,zmax] returns a function that evaluates the comoving
angular diameter distance to a redshift z <= zmax for the named cosmology. Options are the same
as for comovingDistanceFunction."


ageOfUniverse::usage=
"ageOfUniverse[cosmology] returns the age of the universe in Gyr for the named cosmology."


lookbackTimeFunction::usage=
"lookbackTimeFunction[cosmology,zmax] returns a function that evaluates the lookback time
to a redshift z <= zmax for the named cosmology. Possible options are:
 - physical (True) units Gyr (True) or Gyr/h (False).
 - pointsPerDecade (20) number of interpolation points to use per decade."


conformalTimeFunction::usage=
"conformalTimeFunction[cosmology,zmax] returns a function that evaluates the conformal time
at redshift z <= zmax for the named cosmology. Options are the same as for lookbackTimeFunction."


Begin["Private`"]


criticalDensityToday[hvalue_]:=
Units`Convert[
	3 PhysicalConstants`SpeedOfLight^2/
	(8 \[Pi] PhysicalConstants`GravitationalConstant)
	(100 hvalue Units`Kilo Units`Meter/Units`Second/(Units`Mega Units`Parsec))^2/
	(Units`Joule/Units`Meter^3),1
]


radiationDensity[Tcmb_,Nnu_]:=
Units`Convert[
    (\[Pi]^2/15/(PhysicalConstants`PlanckConstantReduced PhysicalConstants`SpeedOfLight)^3
    (PhysicalConstants`BoltzmannConstant Tcmb Units`Kelvin)^4(1+7/8(4/11)^(4/3)Nnu))/
    (Units`Joule/Units`Meter^3),1
]


zstar[\[CapitalOmega]mh2_,\[CapitalOmega]bh2_]:=
With[{g1=0.0783 \[CapitalOmega]bh2^(-0.238)/(1+39.5 \[CapitalOmega]bh2^(0.763)),g2=0.560/(1+21.1 \[CapitalOmega]bh2^(1.81))},
    1048(1+0.00124 \[CapitalOmega]bh2^(-0.738))(1+g1 \[CapitalOmega]mh2^g2)
]


zeq[\[CapitalOmega]mh2_,Tcmb_]:=25000 \[CapitalOmega]mh2 (2.7/Tcmb)^4


zdrag[\[CapitalOmega]mh2_,\[CapitalOmega]bh2_]:=
With[{b1=0.313 \[CapitalOmega]mh2^(-0.419)(1+0.607 \[CapitalOmega]mh2^(0.674)),b2=0.238 \[CapitalOmega]mh2^(0.223)},
    1291 \[CapitalOmega]mh2^(0.251)/(1+0.659 \[CapitalOmega]mh2^(0.828))(1+b1 \[CapitalOmega]bh2^b2)
]


createCosmology[name_,OptionsPattern[]]:=
With[{
    h=OptionValue["h"],
    \[CapitalOmega]\[CapitalLambda]=OptionValue["\[CapitalOmega]\[CapitalLambda]"],
    \[CapitalOmega]bh2=OptionValue["\[CapitalOmega]bh2"],
    w0=OptionValue["w0"],
    wa=OptionValue["wa"],
    \[CapitalOmega]k=OptionValue["\[CapitalOmega]k"],
    Tcmb=OptionValue["Tcmb"],
    Nnu=OptionValue["Nnu"]
},
    name/: Options[name]= { "h"->h,"\[CapitalOmega]\[CapitalLambda]"->\[CapitalOmega]\[CapitalLambda],"\[CapitalOmega]bh2"->\[CapitalOmega]bh2,"w0"->w0,"wa"->wa,"\[CapitalOmega]k"->\[CapitalOmega]k,"Tcmb"->Tcmb,"Nnu"->Nnu };
    name/: \[CapitalOmega]rad[name]=Function[z,Evaluate[Simplify[radiationDensity[Tcmb,Nnu]/criticalDensityToday[h](1+z)^4]]];
    name/: \[CapitalOmega]de[name]=Function[z,Evaluate[Simplify[\[CapitalOmega]\[CapitalLambda] Exp[3(-((wa z)/(1 + z)) + (1 + w0 + wa) Log[1 + z])]]]];
    name/: \[CapitalOmega]mat[name]=Function[z,Evaluate[Simplify[(1-\[CapitalOmega]de[name][0]-\[CapitalOmega]rad[name][0]-\[CapitalOmega]k)(1+z)^3]]];
    name/: Hratio[name]=Function[z,Evaluate[Sqrt[Simplify[\[CapitalOmega]de[name][z]+\[CapitalOmega]k (1+z)^2+\[CapitalOmega]mat[name][z]+\[CapitalOmega]rad[name][z]]]]];
    With[{\[CapitalOmega]mh2=\[CapitalOmega]mat[name][0]h^2},
        name/: zstar[name]=zstar[\[CapitalOmega]mh2,\[CapitalOmega]bh2];
        name/: zeq[name]=zeq[\[CapitalOmega]mh2,Tcmb];
        name/: zdrag[name]=zdrag[\[CapitalOmega]mh2,\[CapitalOmega]bh2];
    ]
]
SetAttributes[createCosmology,HoldFirst]
Options[createCosmology]={
    "h"->0.7,"\[CapitalOmega]\[CapitalLambda]"->0.73,"\[CapitalOmega]bh2"->0.0227,"w0"->-1,"wa"->0,"\[CapitalOmega]k"->0,"Tcmb"->2.725,"Nnu"->3.046
};


(* Builds a function that evaluates the integral of I(z) from 0 to zmax with interpolation in log(1+z) *)
buildFunction[integrand_,zmax_,ptsPerDecade_]:=
Module[{npts,ds,sval,partials,tabulated,interpolator},
	npts=Ceiling[N[Log[1+zmax]ptsPerDecade/Log[10]]];
    (* Integrate over equally spaced intervals in s = log(1+z) *)
    ds=Log[1+zmax]/(npts-1);
	sval=Table[n ds,{n,0,npts-1}];
	partials=Table[NIntegrate[integrand[Exp[s]-1]Exp[s],{s,sval[[n]],sval[[n+1]]}],{n,1,npts-1}];
    (* Add the boundary condition that the integral is zero at z = 0 *)
	tabulated=Prepend[Accumulate[partials],0];
	interpolator=Interpolation[Transpose[{sval,tabulated}]];
	Function[z,interpolator[Log[1+z]]]
]


(* Returns numerator/H0 in the specified units *)
hubbleScale[numerator_,hValue_,units_]:=
Units`Convert[numerator/(100 hValue Units`Kilo Units`Meter/Units`Second/(Units`Mega Units`Parsec))/units,1]


comovingDistanceFunction[cosmology_,zmax_,options:OptionsPattern[]]:=
With[{physical=OptionValue["physical"],pointsPerDecade=OptionValue["pointsPerDecade"]},
Module[{h,scale},
    h=If[physical===True,OptionValue[cosmology,"h"],1];
    scale=hubbleScale[PhysicalConstants`SpeedOfLight,h,Units`Mega Units`Parsec];
	buildFunction[scale/Hratio[cosmology][#1]&,zmax,pointsPerDecade]
]]
Options[comovingDistanceFunction]={"physical"->False,"pointsPerDecade"->20};


curvatureFunction[\[CapitalOmega]k_]:=Which[
\[CapitalOmega]k>0,Function[Dc,Sinh[Sqrt[\[CapitalOmega]k]Dc]/Sqrt[\[CapitalOmega]k]],
\[CapitalOmega]k<0,Function[Dc,Sin[Sqrt[-\[CapitalOmega]k]Dc]/Sqrt[-\[CapitalOmega]k]],
True,Function[Dc,Dc]]


angularDiameterDistanceFunction[cosmology_,zmax_,options:OptionsPattern[]]:=
With[{physical=OptionValue["physical"],pointsPerDecade=OptionValue["pointsPerDecade"]},
Module[{h,scale,func,curved},
    h=If[physical===True,OptionValue[cosmology,"h"],1];
    scale=hubbleScale[PhysicalConstants`SpeedOfLight,h,Units`Mega Units`Parsec];
	func=buildFunction[1/Hratio[cosmology][#1]&,zmax,pointsPerDecade];
    curved=curvatureFunction[OptionValue[cosmology,"\[CapitalOmega]k"]];
    Function[z,scale curved[func[z]]]
]]
Options[angularDiameterDistanceFunction]=Options[comovingDistanceFunction];


ageOfUniverse[cosmology_]:=
Module[{scale},
    scale=hubbleScale[1,OptionValue[cosmology,"h"],Units`Giga Units`Year];
	scale NIntegrate[1/Hratio[cosmology][Exp[s]-1],{s,0,Infinity}]
]


lookbackTimeFunction[cosmology_,zmax_,options:OptionsPattern[]]:=
With[{physical=OptionValue["physical"],pointsPerDecade=OptionValue["pointsPerDecade"]},
Module[{h,scale},
    h=If[physical===True,OptionValue[cosmology,"h"],1];
    scale=hubbleScale[1,h,Units`Giga Units`Year];
	buildFunction[scale/(1+#1)/Hratio[cosmology][#1]&,zmax,pointsPerDecade]
]]
Options[lookbackTimeFunction]={"physical"->True,"pointsPerDecade"->20};


conformalTimeFunction[cosmology_,zmax_,options:OptionsPattern[]]:=
With[{physical=OptionValue["physical"],pointsPerDecade=OptionValue["pointsPerDecade"]},
Module[{h,scale,eta0,func},
    h=If[physical===True,OptionValue[cosmology,"h"],1];
    scale=hubbleScale[1,h,Units`Giga Units`Year];
    eta0=scale NIntegrate[1/Hratio[cosmology][Exp[s]-1]Exp[s],{s,0,Infinity}];
    func=buildFunction[scale/Hratio[cosmology][#1]&,zmax,pointsPerDecade];
    Function[z,eta0-func[z]]
]]
Options[conformalTimeFunction]=Options[lookbackTimeFunction];


(*soundHorizonFunction[hubble_,zmax_,options:OptionsPattern[]]:=
With[{hValue=OptionValue["hValue"],pointsPerDecade=OptionValue["pointsPerDecade"]},
Module[{scale,rs0,func},
    scale=hubbleScale[PhysicalConstants`SpeedOfLight,hValue,Units`Mega Units`Parsec];
    rs0=scale NIntegrate[...,{s,0,Infinity}];
    func=buildFunction[scale ...,zmax,pointsPerDecade];
    Function[z,rs0-func[z]]
]]
Options[soundHorizonFunction]=Options[comovingDistanceFunction];*)


End[]


EndPackage[]
