(* ::Package:: *)

(* Created 4-Jan-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)
BeginPackage["DeepZot`AstroTools`"]


AstroTools::usage=
"A collection of utilities for astronomical calculations.";


raDecUnitVector::usage=
"raDecUnitVector[ra,dec] returns a unit vector on the sphere in the direction (ra,dec)
where the angles are measured in decimal degrees.";


Begin["Private`"]


raDecUnitVector[ra_,dec_]:=
With[{raRad=ra \[Pi]/180,decRad=dec \[Pi]/180},
  N[{Cos[raRad]Cos[decRad],Sin[raRad]Cos[decRad],Sin[decRad]}]
]


End[]


EndPackage[]
