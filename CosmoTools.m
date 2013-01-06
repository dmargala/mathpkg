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
"Returns a function that evalutes H(z)/H(0) for the specified cosmology."


comovingDistanceFunction::usage=
"Returns a function that evaluates the comoving distance to a redshift
z <= zmax for the specified Hubble function. If hValue is specified,
results are in Mpc, otherwise they are in Mpc/h."


lookbackTimeFunction::usage=
"Returns a function that evaluates the lookback time to a redshift
z <= zmax for the specified Hubble function. If hValue is specified,
results are in Gyr, otherwise they are in Gyr/h."


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


hubbleFunction[hValue_,\[CapitalOmega]\[CapitalLambda]_,w0_,wa_,\[CapitalOmega]k_,Tcmb_,Nnu_]:=
With[{\[CapitalOmega]rad=radiationDensity[Tcmb,Nnu]/criticalDensityToday[hValue]},
	Function[z,
		With[{de=\[CapitalOmega]\[CapitalLambda] darkEnergyEvolution[z,w0,wa]},
			Sqrt[de+\[CapitalOmega]k (1+z)^2+(1-de-\[CapitalOmega]rad-\[CapitalOmega]k)(1+z)^3+\[CapitalOmega]rad (1+z)^4]
		]
	]
]


buildFunction[integrand_,zmax_,ptsPerDecade_]:=
Module[{npts,zval,partials,tabulated,interpolator},
	npts=Ceiling[N[Log[1+zmax]ptsPerDecade/Log[10]]];
	zval=Table[(1+zmax)^(n/(npts-1))-1,{n,0,npts-1}]//N;
	partials=Table[NIntegrate[integrand[zz],{zz,zval[[n]],zval[[n+1]]}],{n,1,npts-1}];
	tabulated=Prepend[Accumulate[partials],0];
	interpolator=Interpolation[Transpose[{Log[1+zval],tabulated}]];
	Function[z,interpolator[Log[1+z]]]
]


comovingDistanceFunction[hubble_,zmax_,hValue_:1,ptsPerDecade_:20]:=
Module[{scale},
	scale=Units`Convert[PhysicalConstants`SpeedOfLight/(100 hValue Units`Kilo Units`Meter/Units`Second),1];
	buildFunction[scale/hubble[#1]&,zmax,ptsPerDecade]
]


lookbackTimeFunction[hubble_,zmax_,hValue_:1,ptsPerDecade_:20]:=
Module[{scale},
	scale=Units`Convert[1/(100 hValue Units`Kilo Units`Meter/Units`Second/(Units`Mega Units`Parsec)),Units`Year]/(10^9 Units`Year);
	buildFunction[scale/(1+#1)/hubble[#1]&,zmax,ptsPerDecade]
]


End[]


EndPackage[]
