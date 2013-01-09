(* ::Package:: *)

(* Created 4-Jan-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

(* As of version 9, PhysicalConstants has been replaced by the built-in Quantity[],
but we keep using it here for backwards compatibility *)
Off[General::obspkg]
BeginPackage["DeepZot`CosmoTools`",{"Units`","PhysicalConstants`"}]


CosmoTools::usage=
"A collection of utilities for cosmological calculations."


criticalDensityToday::usage=
"Calcualtes the present critical density in J/\!\(\*SuperscriptBox[\(m\), \(3\)]\)."


radiationDensity::usage=
"Calculates the Stefan-Boltzmann radiation energy density in J/\!\(\*SuperscriptBox[\(m\), \(3\)]\)."


darkEnergyEvolution::usage=
"Calcualtes the dark energy evolution with redshift as a function of the
equation of state parameters w0,wa."


hubbleFunction::usage=
"Returns a function that evalutes H(z)/H(0) for the specified cosmology. The hValue
only matters if radiation is included."


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
	(100 hvalue Units`Kilo Units`Meter/Units`Second/(Units`Mega Units`Parsec))^2,
	Units`Joule/Units`Meter^3
]


radiationDensity[Tcmb_,Nnu_]:=
(\[Pi]^2/15/(PhysicalConstants`PlanckConstantReduced PhysicalConstants`SpeedOfLight)^3
(PhysicalConstants`BoltzmannConstant Tcmb Units`Kelvin)^4(1+7/8(4/11)^(4/3)Nnu))


darkEnergyEvolution[z_,w0_,wa_]:=
Exp[3(-((wa z)/(1 + z)) + (1 + w0 + wa) Log[1 + z])]


hubbleFunction[hValue_:0.7,\[CapitalOmega]\[CapitalLambda]_:0.73,w0_:-1,wa_:0,\[CapitalOmega]k_:0,Tcmb_:2.725,Nnu_:3.046]:=
With[{\[CapitalOmega]rad=radiationDensity[Tcmb,Nnu]/criticalDensityToday[hValue]},
	Function[z,Evaluate[
		With[{de=\[CapitalOmega]\[CapitalLambda] darkEnergyEvolution[z,w0,wa]},
			Sqrt[Simplify[de+\[CapitalOmega]k (1+z)^2+(1-\[CapitalOmega]\[CapitalLambda]-\[CapitalOmega]rad-\[CapitalOmega]k)(1+z)^3+\[CapitalOmega]rad (1+z)^4]]
		]
	]]
]


buildFunction[integrand_,zmax_,ptsPerDecade_]:=
Module[{npts,ds,sval,partials,tabulated,interpolator},
	npts=Ceiling[N[Log[1+zmax]ptsPerDecade/Log[10]]];
    (* Integrate and interpolate in s = log(z) *)
    ds=Log[1+zmax]/(npts-1);
	sval=Table[n ds,{n,0,npts-1}];
	partials=Table[NIntegrate[integrand[Exp[s]-1]Exp[s],{s,sval[[n]],sval[[n+1]]}],{n,1,npts-1}];
	tabulated=Prepend[Accumulate[partials],0];
	interpolator=Interpolation[Transpose[{sval,tabulated}]];
	Function[z,interpolator[Log[1+z]]]
]


hubbleScale[numerator_,hValue_,units_]:=
Units`Convert[numerator/(100 hValue Units`Kilo Units`Meter/Units`Second/(Units`Mega Units`Parsec))/units,1]


comovingDistanceFunction[hubble_,zmax_,hValue_:1,ptsPerDecade_:20]:=
Module[{scale},
    scale=hubbleScale[PhysicalConstants`SpeedOfLight,hValue,Units`Mega Units`Parsec];
	buildFunction[scale/hubble[#1]&,zmax,ptsPerDecade]
]


curvatureFunction[\[CapitalOmega]k_]:=Which[
\[CapitalOmega]k>0,Function[Dc,Sinh[Sqrt[\[CapitalOmega]k]Dc]/Sqrt[\[CapitalOmega]k]],
\[CapitalOmega]k<0,Function[Dc,Sin[Sqrt[-\[CapitalOmega]k]Dc]/Sqrt[-\[CapitalOmega]k]],
True,Function[Dc,Dc]]


angularDiameterDistanceFunction[hubble_,zmax_,\[CapitalOmega]k_,hValue_:1,ptsPerDecade_:20]:=
Module[{scale,func,curved},
    scale=hubbleScale[PhysicalConstants`SpeedOfLight,hValue,Units`Mega Units`Parsec];
	func=buildFunction[1/hubble[#1]&,zmax,ptsPerDecade];
    curved=curvatureFunction[\[CapitalOmega]k];
    Function[z,scale curved[func[z]]]
]


lookbackTimeFunction[hubble_,zmax_,hValue_:1,ptsPerDecade_:20]:=
Module[{scale},
    scale=hubbleScale[1,hValue,Units`Giga Units`Year];
	buildFunction[scale/(1+#1)/hubble[#1]&,zmax,ptsPerDecade]
]


ageOfUniverse[hubble_,hValue_:1]:=
Module[{scale},
    scale=hubbleScale[1,hValue,Units`Giga Units`Year];
	scale NIntegrate[1/hubble[Exp[s]-1],{s,0,Infinity}]
]


conformalTimeFunction[hubble_,zmax_,hValue_:1,ptsPerDecade_:20]:=
Module[{scale,eta0,func},
    scale=hubbleScale[1,hValue,Units`Giga Units`Year];
    eta0=scale NIntegrate[1/hubble[Exp[s]-1]Exp[s],{s,0,Infinity}];
    func=buildFunction[scale/hubble[#1]&,zmax,ptsPerDecade];
    Function[z,eta0-func[z]]
]


End[]


EndPackage[]
