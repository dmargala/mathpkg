(* ::Package:: *)
(* Created 5-Jan-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`PlotTools`"]


PlotTools::usage=
"A collection of plotting utilities."


temperatureMap::usage=
"A replacement for the builtin TemperatureMap that is pure white at its midpoint."


rasterize::usage=
"Converts graphics output into a rasterized format with anti-aliasing.
The raster dimensions will be magnification*ImageSize and anti-aliasing will
be performed using the specified oversampling factor."


Begin["Private`"]


temperatureMap[z_]:=With[{
r=Clip[2 z,{0,1}],
g=1-4(z-0.5)^2,
b=Clip[3-4z,{0,1}]
},RGBColor[r,g,b]
]


rasterize[graphics_,magnification_:2,oversampling_:2]:=
With[{size=ImageSize/.Options[graphics,ImageSize]},
Rasterize[
	Magnify[graphics,magnification],
	ImageSize->magnification size,
	RasterSize->oversampling magnification size]
]


End[]


EndPackage[]
