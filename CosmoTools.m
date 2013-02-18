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
 - H0[name]
 - Hratio[name][z]
 - curvatureTransform[name][x]
 - zstar[name]
 - zeq[name]
 - zdrag[name]
 - betas[name][z]
Separate help is available for each of these definitions, e.g., ?\[CapitalOmega]rad.
Use the following options (defaults in parentheses) to customize the
created cosmology:
 - h (0.7) H0/(100 km/s/Mpc).
 - \[CapitalOmega]\[Phi] (0.73) present-day fraction of dark energy.
 - \[CapitalOmega]bh2 (0.0227) present-day physical baryon fraction.
 - w0 (-1) dark-energy equation parameter state present-day value.
 - wa (0) dark-energy equation of state parameter derivative wrt scale a.
 - \[CapitalOmega]k (0) present-day curvature fraction.
 - Tcmb (2.725) present-day CMB temperature in Kelvin.
 - Nnu (3.046) effective number of massless neutrinos.
Use OptionValue[name,opt] to get option values associated with a named
cosmology. To clear a previously defined cosmology, use Clear[name]. Use
the name provided here to identify the created cosmology in functions
like comovingDistanceFunction."


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


hubbleDistance::usage=
"hubbleDistance[name] returns the Hubble distance c/H0 in Mpc (not Mpc/h)."


curvatureTransform::usage=
"curvatureTransform[name][x] returns the comoving transverse distance DM corresponding to the
scaled comoving line-of-sight distance x = DC/(c/H0)."


betas::usage=
"betas[name][z] returns the sound speed in the bayron-photon fluid relative to the speed of light."


comovingDistanceFunction::usage=
"comovingDistanceFunction[cosmology,zmax] returns a function that evaluates the comoving distance
along the line of sight to a redshift z <= zmax for the named cosmology. Possible options (with
defaults in parentheses) are:
 - physical (False) units are Mpc (True) or Mpc/h (False).
 - inverted (False) return inverse function z(Dc) instead of Dc(z) when True.
 - pointsPerDecade (20) number of interpolation points to use per decade."


transverseDistanceFunction::usage
"transverseDistanceFunction[cosmology,zmax] returns a function that evaluates the comoving distance
transverse to the line of sight at a redshift z <= zmax for the named cosmology. Options are the same
as for comovingDistanceFunction."


angularDiameterDistanceFunction::usage
"angularDiameterDistanceFunction[cosmology,zmax] returns a function that evaluates the
angular diameter distance to a redshift z <= zmax for the named cosmology. Options are the same
as for comovingDistanceFunction."


luminosityDistanceFunction::usage
"angularDiameterDistanceFunction[cosmology,zmax] returns a function that evaluates the
luminosity distance to a redshift z <= zmax for the named cosmology. Options are the same
as for comovingDistanceFunction."


ageOfUniverse::usage=
"ageOfUniverse[cosmology] returns the age of the universe in Gyr for the named cosmology.
The result is cached after first evaluation."


lookbackTimeFunction::usage=
"lookbackTimeFunction[cosmology,zmax] returns a function that evaluates the lookback time
to a redshift z <= zmax for the named cosmology. Possible options are:
 - physical (True) units Gyr (True) or Gyr/h (False).
 - pointsPerDecade (20) number of interpolation points to use per decade.
 - inverted (False) return inverse function z(tLB) instead of tLB(z) when True."


conformalTimeFunction::usage=
"conformalTimeFunction[cosmology,zmax] returns a function that evaluates the conformal time
at redshift z <= zmax for the named cosmology. Options are the same as for lookbackTimeFunction."


soundHorizonFunction::usage=
"soundHorizonFunction[cosmology,zmax] returns a function that evaluates the sound horizon at
redshift z <= zmax for the named cosmology. Options are the same as for comovingDistanceFunction."


rsdrag::usage=
"rsdrag[cosmology] returns the sound horizon in Mpc (not Mpc/h) at zdrag for the named cosmology.
The result is cached after the first evaluation."


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


(* Returns numerator/H0 in the specified units *)
hubbleScale[numerator_,hValue_,units_]:=
Units`Convert[numerator/(100 hValue Units`Kilo Units`Meter/Units`Second/(Units`Mega Units`Parsec))/units,1]


Clear[createCosmology]
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
    name/: H0[name]=100 h;
    name/: Hratio[name]=Function[z,Evaluate[Sqrt[Simplify[\[CapitalOmega]de[name][z]+\[CapitalOmega]k (1+z)^2+\[CapitalOmega]mat[name][z]+\[CapitalOmega]rad[name][z]]]]];
    name/: hubbleDistance[name]=hubbleScale[PhysicalConstants`SpeedOfLight,h,Units`Mega Units`Parsec];
    name/: curvatureTransform[name]=Function[x,Evaluate[Simplify[Which[
        \[CapitalOmega]k>0,Sinh[Sqrt[\[CapitalOmega]k]x]/Sqrt[\[CapitalOmega]k],\[CapitalOmega]k<0,Sin[Sqrt[-\[CapitalOmega]k]x]/Sqrt[-\[CapitalOmega]k],True,x]]]];
    With[{\[CapitalOmega]mh2=\[CapitalOmega]mat[name][0]h^2},
        name/: zstar[name]=zstar[\[CapitalOmega]mh2,\[CapitalOmega]bh2];
        name/: zeq[name]=zeq[\[CapitalOmega]mh2,Tcmb];
        name/: zdrag[name]=zdrag[\[CapitalOmega]mh2,\[CapitalOmega]bh2];
    ];
    With[{\[CapitalOmega]\[Gamma]=radiationDensity[Tcmb,0]/criticalDensityToday[h],\[CapitalOmega]b=\[CapitalOmega]bh2/h^2},
        name/: betas[name]=Function[z,Evaluate[Simplify[1/Sqrt[3(1+(3\[CapitalOmega]b)/(4\[CapitalOmega]\[Gamma])/(1+z))]]]];
    ]
]
SetAttributes[createCosmology,HoldFirst]
Options[createCosmology]={
    "h"->0.7,"\[CapitalOmega]\[CapitalLambda]"->0.73,"\[CapitalOmega]bh2"->0.0227,"w0"->-1,"wa"->0,"\[CapitalOmega]k"->0,"Tcmb"->2.72548,"Nnu"->3.03761
};


(* Builds a function that evaluates f[z]=scale*transform[z,Integral[integrand[zz],{zz,0,z}]] using interpolation
in log(1+z) with the specified number of points per decade. The default scale=1 and transform[z,f]=f.
With inverted\[Rule]True, evaluates z[f] instead of f[z]. *)
Clear[buildFunction]
buildFunction[integrand_,zmax_,OptionsPattern[]]:=
With[{pointsPerDecade=OptionValue["pointsPerDecade"],inverted=OptionValue["inverted"],
scale=OptionValue["scale"],transform=OptionValue["transform"]},
Module[{npts,ds,sval,partials,tabulated,interpolator},
	npts=Ceiling[N[Log[1+zmax]pointsPerDecade/Log[10]]];
    (* Integrate over equally spaced intervals in s = log(1+z) *)
    ds=Log[1+zmax]/(npts-1);
	sval=Table[n ds,{n,0,npts-1}];
	partials=Table[NIntegrate[integrand[Exp[s]-1]Exp[s],{s,sval[[n]],sval[[n+1]]}],{n,1,npts-1}];
    (* Accumulate the partials and add the boundary condition that the integral is zero at z = 0 *)
	tabulated=Prepend[Accumulate[partials],0];
    (* Apply the scale and transform[z,f] to the cummulative integrals *)
    tabulated=scale Apply[transform,Transpose[{Exp[sval]-1,tabulated}],1];
    (* Create the requested interpolation f(z) or z(f) *)
    If[inverted===True,
        interpolator=Interpolation[Transpose[{tabulated,sval}]];
	        Function[f,Exp[interpolator[f]]-1],
    	interpolator=Interpolation[Transpose[{sval,tabulated}]];
            Function[z,interpolator[Log[1+z]]]
    ]
]]
Options[buildFunction]={"pointsPerDecade"->20,"scale"->1,"transform"->(#2&),"inverted"->False};


(* Builds a distance function using options physical (Mpc vs Mpc/h), transverse (apply curvatureTransform), and
multiplying result by (1+z)^zpower *)
Clear[buildDistanceFunction]
buildDistanceFunction[cosmology_,zmax_,options:OptionsPattern[{buildDistanceFunction,buildFunction}]]:=
With[{physical=OptionValue["physical"],transverse=OptionValue["transverse"],zpower=OptionValue["zpower"]},
Module[{h,scale,transform},
    h=If[physical===True,OptionValue[cosmology,"h"],1];
    scale=hubbleScale[PhysicalConstants`SpeedOfLight,h,Units`Mega Units`Parsec];
    transform=If[transverse===True,curvatureTransform[cosmology][#2](1+#1)^zpower,#2 (1+#1)^zpower]&;
	buildFunction[1/Hratio[cosmology][#1]&,zmax,"scale"->scale,"transform"->transform,
        FilterRules[{options},Options[buildFunction]]
    ]
]]
Options[buildDistanceFunction]={"physical"->False,"transverse"->False,"zpower"->0};


Clear[comovingDistanceFunction]
comovingDistanceFunction[cosmology_,zmax_,options:OptionsPattern[]]:=
buildDistanceFunction[cosmology,zmax,"transverse"->False,"zpower"->0,FilterRules[{options},Options[buildDistanceFunction]]]
Options[comovingDistanceFunction]={"physical"->False,"inverted"->False,"pointsPerDecade"->20};


Clear[transverseDistanceFunction]
transverseDistanceFunction[cosmology_,zmax_,options:OptionsPattern[comovingDistanceFunction]]:=
buildDistanceFunction[cosmology,zmax,"transverse"->True,"zpower"->0,FilterRules[{options},Options[buildDistanceFunction]]]


Clear[angularDiameterDistanceFunction]
angularDiameterDistanceFunction[cosmology_,zmax_,options:OptionsPattern[comovingDistanceFunction]]:=
buildDistanceFunction[cosmology,zmax,"transverse"->True,"zpower"->-1,FilterRules[{options},Options[buildDistanceFunction]]]


Clear[luminosityDistanceFunction]
luminosityDistanceFunction[cosmology_,zmax_,options:OptionsPattern[comovingDistanceFunction]]:=
buildDistanceFunction[cosmology,zmax,"transverse"->True,"zpower"->+1,FilterRules[{options},Options[buildDistanceFunction]]]


Clear[ageOfUniverse]
ageOfUniverse[cosmology_]:=ageOfUniverse[cosmology]^=
Module[{scale},
    scale=hubbleScale[1,OptionValue[cosmology,"h"],Units`Giga Units`Year];
	scale NIntegrate[1/Hratio[cosmology][Exp[s]-1],{s,0,Infinity}]
]


Clear[lookbackTimeFunction]
lookbackTimeFunction[cosmology_,zmax_,options:OptionsPattern[{lookbackTimeFunction,buildFunction}]]:=
With[{physical=OptionValue["physical"]},
Module[{h,scale},
    h=If[physical===True,OptionValue[cosmology,"h"],1];
    scale=hubbleScale[1,h,Units`Giga Units`Year];
	buildFunction[1/(1+#1)/Hratio[cosmology][#1]&,zmax,"scale"->scale,FilterRules[{options},Options[buildFunction]]]
]]
Options[lookbackTimeFunction]={"physical"->True};


Clear[conformalTimeFunction]
conformalTimeFunction[cosmology_,zmax_,options:OptionsPattern[{conformalTimeFunction,buildFunction}]]:=
With[{physical=OptionValue["physical"]},
Module[{h,scale,eta0},
    h=If[physical===True,OptionValue[cosmology,"h"],1];
    scale=hubbleScale[1,h,Units`Giga Units`Year];
    eta0=NIntegrate[1/Hratio[cosmology][Exp[s]-1]Exp[s],{s,0,Infinity}];
    buildFunction[1/Hratio[cosmology][#1]&,zmax,"scale"->scale,"transform"->((eta0-#2)&),FilterRules[{options},Options[buildFunction]]]
]]
Options[conformalTimeFunction]={"physical"->True};


Clear[soundHorizonFunction]
soundHorizonFunction[cosmology_,zmax_,options:OptionsPattern[{soundHorizonFunction,buildFunction}]]:=
With[{physical=OptionValue["physical"]},
Module[{h,scale,r0},
    h=If[physical===True,OptionValue[cosmology,"h"],1];
    scale=hubbleScale[PhysicalConstants`SpeedOfLight,h,Units`Mega Units`Parsec];
    r0=NIntegrate[betas[cosmology][Exp[s]-1]/Hratio[cosmology][Exp[s]-1]Exp[s],{s,0,Infinity}];
    buildFunction[betas[cosmology][#1]/Hratio[cosmology][#1]&,zmax,"scale"->scale,"transform"->((r0-#2)&),FilterRules[{options},Options[buildFunction]]]
]]
Options[soundHorizonFunction]={"physical"->False};


Clear[rsdrag]
rsdrag[cosmology_]:=rsdrag[cosmology]^=
Module[{scale,sdrag},
    scale=hubbleScale[PhysicalConstants`SpeedOfLight,OptionValue[cosmology,"h"],Units`Mega Units`Parsec];
    sdrag=Log[1+zdrag[cosmology]];
    scale NIntegrate[betas[cosmology][Exp[s]-1]/Hratio[cosmology][Exp[s]-1]Exp[s],{s,sdrag,Infinity}]
]


End[]


EndPackage[]
