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


darkEnergyEvolution::usage=
"darkEnergyEvolution[z,w0,wa] calcualtes the dark energy evolution with redshift (eqn 22 of Weinberg 2012)
as a function of the equation of state parameters w0,wa."


createCosmology::usage=
"createCosmology[name] associates the following definitions with the symbol name:
 - \[CapitalOmega]rad[name]
 - hubbleFunction[name]
Separate help is available for each of these definitions, e.g., ?\[CapitalOmega]rad.
Use the following options to customize the cosmology that is created:
 - hValue (0.7) only matters if radiation is included.
 - \[CapitalOmega]\[Phi] (0.73) present-day fraction of dark energy.
 - w0 (-1) dark-energy equation parameter state present-day value.
 - wa (0) dark-energy equation of state parameter derivative wrt scale a.
 - \[CapitalOmega]k (0) present-day curvature fraction.
 - Tcmb (2.725) present-day CMB temperature in Kelvin.
 - Nnu (3.046) effective number of massless neutrinos.
To clear a previously defined cosmology, use Clear[name]."


(* ::InheritFromParent:: *)
(*"createCosmology[name] creates the following definitions associated with the symbol name:\n - name[hubbleFunction][z] evaluates H(z)/H0\n - ...\nUse the following options to customize the cosmology that is created:\n - hValue (0.7) only matters if radiation is included.\n - \[CapitalOmega]\[CapitalLambda] (0.73) present-day fraction of dark energy.\n - w0 (-1) dark-energy equation parameter state present-day value.\n - wa (0) dark-energy equation of state parameter derivative wrt scale a.\n - \[CapitalOmega]k (0) present-day curvature fraction.\n - Tcmb (2.725) present-day CMB temperature in Kelvin.\n - Nnu (3.046) effective number of massless neutrinos."*)


\[CapitalOmega]rad::usage=
"\[CapitalOmega]rad[name][z] returns the radiation energy density relative to the critical density at the specified redshift."


\[CapitalOmega]de::usage=
"\[CapitalOmega]de[name][z] returns the dark-energy density relative to the critical density at the specified redshift."


\[CapitalOmega]mat::usage=
"\[CapitalOmega]mat[name][z] returns the matter energy density relative to the critical density at the specified redshift."


hubble::usage=
"hubble[name][z] returns H(z)/H0."


hubbleFunction::usage=
"Returns a function that evalutes H(z)/H(0) for the specified cosmology. Use the following
options to customize the cosmology:
 - hValue (0.7) only matters if radiation is included.
 - \[CapitalOmega]\[Phi] (0.73) present-day fraction of dark energy.
 - w0 (-1) dark-energy equation parameter state present-day value.
 - wa (0) dark-energy equation of state parameter derivative wrt scale a.
 - \[CapitalOmega]k (0) present-day curvature fraction.
 - Tcmb (2.725) present-day CMB temperature in Kelvin.
 - Nnu (3.046) effective number of massless neutrinos."


comovingDistanceFunction::usage=
"Returns a function that evaluates the comoving distance to a redshift
z <= zmax for the specified Hubble function. If hValue is specified,
results are in Mpc, otherwise they are in Mpc/h."


angularDiameterDistanceFunction::usage
"Returns a function that evaluates the comoving angular diameter distance to a redshift
z <= zmax for the specified Hubble function and curvature \[CapitalOmega]k. If hValue is specified,
results are in Mpc, otherwise they are in Mpc/h."


lookbackTimeFunction::usage=
"Returns a function that evaluates the lookback time to a redshift
z <= zmax for the specified Hubble function. If hValue is specified,
results are in Gyr, otherwise they are in Gyr/h."


conformalTimeFunction::usage=
"Returns a function that evaluates the conformal time at redshift z <= zmax
for the specified Hubble function. If hValue is specified, results are in Gyr,
otherwise they are in Gyr/h."


ageOfUniverse::usage=
"Returns the age of the universe for the specified Hubble function. If hValue is specified,
the result is in Gyr, otherwise it is in Gyr/h."


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


darkEnergyEvolution[z_,w0_,wa_]:=
Exp[3(-((wa z)/(1 + z)) + (1 + w0 + wa) Log[1 + z])]


Clear[createCosmology]
createCosmology[name_,OptionsPattern[]]:=
With[{
    hValue=OptionValue["hValue"],
    \[CapitalOmega]\[CapitalLambda]=OptionValue["\[CapitalOmega]\[CapitalLambda]"],
    w0=OptionValue["w0"],
    wa=OptionValue["wa"],
    \[CapitalOmega]k=OptionValue["\[CapitalOmega]k"],
    Tcmb=OptionValue["Tcmb"],
    Nnu=OptionValue["Nnu"]
},
    name/: \[CapitalOmega]rad[name]=Function[z,Evaluate[Simplify[radiationDensity[Tcmb,Nnu]/criticalDensityToday[hValue](1+z)^4]]];
    name/: \[CapitalOmega]de[name]=Function[z,Evaluate[Simplify[\[CapitalOmega]\[CapitalLambda] Exp[3(-((wa z)/(1 + z)) + (1 + w0 + wa) Log[1 + z])]]]];
    name/: \[CapitalOmega]mat[name]=Function[z,Evaluate[Simplify[(1-\[CapitalOmega]de[name][0]-\[CapitalOmega]rad[name][0]-\[CapitalOmega]k)(1+z)^3]]];
    name/: hubble[name]=Function[z,Evaluate[Sqrt[Simplify[\[CapitalOmega]de[name][z]+\[CapitalOmega]k (1+z)^2+\[CapitalOmega]mat[name][z]+\[CapitalOmega]rad[name][z]]]]];
]
SetAttributes[createCosmology,HoldFirst]
Options[createCosmology]={
    "hValue"->0.7,"\[CapitalOmega]\[CapitalLambda]"->0.73,"w0"->-1,"wa"->0,"\[CapitalOmega]k"->0,"Tcmb"->2.725,"Nnu"->3.046
};


hubbleFunction[options:OptionsPattern[]]:=
With[{
    hValue=OptionValue["hValue"],
    \[CapitalOmega]\[CapitalLambda]=OptionValue["\[CapitalOmega]\[CapitalLambda]"],
    w0=OptionValue["w0"],
    wa=OptionValue["wa"],
    \[CapitalOmega]k=OptionValue["\[CapitalOmega]k"],
    Tcmb=OptionValue["Tcmb"],
    Nnu=OptionValue["Nnu"]
},
Module[{
    \[CapitalOmega]rad=radiationDensity[Tcmb,Nnu]/criticalDensityToday[hValue]
},
	Function[z,Evaluate[
		With[{de=\[CapitalOmega]\[CapitalLambda] darkEnergyEvolution[z,w0,wa]},
			Sqrt[Simplify[de+\[CapitalOmega]k (1+z)^2+(1-\[CapitalOmega]\[CapitalLambda]-\[CapitalOmega]rad-\[CapitalOmega]k)(1+z)^3+\[CapitalOmega]rad (1+z)^4]]
		]
	]]
]]
Options[hubbleFunction]={
    "hValue"->0.7,"\[CapitalOmega]\[CapitalLambda]"->0.73,"w0"->-1,"wa"->0,"\[CapitalOmega]k"->0,"Tcmb"->2.725,"Nnu"->3.046
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


hubbleScale[numerator_,hValue_,units_]:=
Units`Convert[numerator/(100 hValue Units`Kilo Units`Meter/Units`Second/(Units`Mega Units`Parsec))/units,1]


comovingDistanceFunction[hubble_,zmax_,options:OptionsPattern[]]:=
With[{hValue=OptionValue["hValue"],pointsPerDecade=OptionValue["pointsPerDecade"]},
Module[{scale},
    scale=hubbleScale[PhysicalConstants`SpeedOfLight,hValue,Units`Mega Units`Parsec];
	buildFunction[scale/hubble[#1]&,zmax,pointsPerDecade]
]]
Options[comovingDistanceFunction]={"hValue"->1,"pointsPerDecade"->20};


curvatureFunction[\[CapitalOmega]k_]:=Which[
\[CapitalOmega]k>0,Function[Dc,Sinh[Sqrt[\[CapitalOmega]k]Dc]/Sqrt[\[CapitalOmega]k]],
\[CapitalOmega]k<0,Function[Dc,Sin[Sqrt[-\[CapitalOmega]k]Dc]/Sqrt[-\[CapitalOmega]k]],
True,Function[Dc,Dc]]


angularDiameterDistanceFunction[hubble_,zmax_,\[CapitalOmega]k_,options:OptionsPattern[]]:=
With[{hValue=OptionValue["hValue"],pointsPerDecade=OptionValue["pointsPerDecade"]},
Module[{scale,func,curved},
    scale=hubbleScale[PhysicalConstants`SpeedOfLight,hValue,Units`Mega Units`Parsec];
	func=buildFunction[1/hubble[#1]&,zmax,pointsPerDecade];
    curved=curvatureFunction[\[CapitalOmega]k];
    Function[z,scale curved[func[z]]]
]]
Options[angularDiameterDistanceFunction]=Options[comovingDistanceFunction];


lookbackTimeFunction[hubble_,zmax_,options:OptionsPattern[]]:=
With[{hValue=OptionValue["hValue"],pointsPerDecade=OptionValue["pointsPerDecade"]},
Module[{scale},
    scale=hubbleScale[1,hValue,Units`Giga Units`Year];
	buildFunction[scale/(1+#1)/hubble[#1]&,zmax,pointsPerDecade]
]]
Options[lookbackTimeFunction]=Options[comovingDistanceFunction];


ageOfUniverse[hubble_,hValue_:1]:=
Module[{scale},
    scale=hubbleScale[1,hValue,Units`Giga Units`Year];
	scale NIntegrate[1/hubble[Exp[s]-1],{s,0,Infinity}]
]


conformalTimeFunction[hubble_,zmax_,options:OptionsPattern[]]:=
With[{hValue=OptionValue["hValue"],pointsPerDecade=OptionValue["pointsPerDecade"]},
Module[{scale,eta0,func},
    scale=hubbleScale[1,hValue,Units`Giga Units`Year];
    eta0=scale NIntegrate[1/hubble[Exp[s]-1]Exp[s],{s,0,Infinity}];
    func=buildFunction[scale/hubble[#1]&,zmax,pointsPerDecade];
    Function[z,eta0-func[z]]
]]
Options[conformalTimeFunction]=Options[comovingDistanceFunction];


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
