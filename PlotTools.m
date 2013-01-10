(* ::Package:: *)

(* Created 5-Jan-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`PlotTools`"]


PlotTools::usage=
"A collection of plotting utilities."


createFrame::usage=
"Creates an empty plot frame of the specified type (Plot,LogPlot,LogLogPlot,LogLinearPlot),
with the specified axis limits and options."


temperatureMap::usage=
"A replacement for the builtin TemperatureMap that is pure white at its midpoint."


drawEllipse::usage=
"Draws an ellipse centered at (x,y) whose bounding box dimensions are 2(dx,dy),
with tangents at (dx,rho dy) and (rho dx,dy). Use dx = \!\(\*SubscriptBox[\(n\[Sigma]\), \(x\)]\), dy = \!\(\*SubscriptBox[\(n\[Sigma]\), \(y\)]\) to draw the
n-sigma contour of a likelihood. For a Gaussian likelihood, use n = 1.51517 for
68% CL and n = 2.44775 for 95% CL."


rasterize::usage=
"Converts graphics output into a rasterized format with anti-aliasing.
The raster dimensions will be magnification*ImageSize and anti-aliasing will
be performed using the specified oversampling factor."


coverageContourPlot::usage=
"Plots contours of the specified coverage fractions for a dataset."


Begin["Private`"]


createFrame[type_:Plot,{xmin_,xmax_},{ymin_,ymax_},options___]:=
Module[{method,prototype,defaults},
    method=ToExpression["List"<>ToString[type]];
    defaults={Frame->True,LabelStyle->Medium};
    If[StringPosition[ToString[type],"Log"]!={},AppendTo[defaults,{GridLines->Automatic,GridLinesStyle->LightGray}]];
    prototype=method[{{1,1}},PlotRange->{{xmin,xmax},{ymin,ymax}},options,defaults];
    DeleteCases[prototype,{__,_Point},Infinity]
]
HoldFirst[createFrame]


temperatureMap[z_]:=With[{
r=Clip[2 z,{0,1}],
g=1-4(z-0.5)^2,
b=Clip[3-4z,{0,1}]
},RGBColor[r,g,b]
]


(* See http://pdg.lbl.gov/2012/reviews/rpp2012-rev-statistics.pdf and http://users.tkk.fi/mvermeer/uncertainty.pdf *)
drawEllipse[x_,y_,dx_,dy_,rho_,fillStyle_:{Opacity[0.3]},edgeStyle_:{}]:=
Module[{avg,dlam,dab,\[Lambda]1,\[Lambda]2,\[Phi]},
    avg=(dx^2+dy^2)/2;
    dab=rho dx dy;
    dlam=Sqrt[(dx^2-dy^2)^2/4+dab^2];
    \[Lambda]1=avg+dlam;
    \[Lambda]2=avg-dlam;
    \[Phi]=ArcTan[(dx^2-dy^2),2 dab]/2;
    Graphics[{Directive[fillStyle],EdgeForm[edgeStyle],Rotate[Disk[{x,y},{Sqrt[\[Lambda]1],Sqrt[\[Lambda]2]}],\[Phi],{x,y}]}]
]


rasterize[graphics_,magnification_:2,oversampling_:2]:=
With[{size=ImageSize/.Options[graphics,ImageSize]},
Rasterize[
	Magnify[graphics,magnification],
	ImageSize->magnification size,
	RasterSize->oversampling magnification size]
]


coverageContourPlot[data_,{plotXmin_,plotXmax_,dx_},{plotYmin_,plotYmax_,dy_},fractions_:{0.6827,0.9545},plotOptions_:{}]:=
Module[{kde,xmin,xmax,ymin,ymax,hist,sorted,cummulative,contourLevels},
    (* Build a kernel density estimate of the PDF *)
    kde=SmoothKernelDistribution[data];
    (* Calculate the binning to use *)
    xmin=Ceiling[Min[data[[;;,1]]]-dx,dx];
    ymin=Ceiling[Min[data[[;;,2]]]-dy,dy];
    xmax=Floor[Max[data[[;;,1]]]+dx,dx];
    ymax=Floor[Max[data[[;;,2]]]+dy,dy];
    (* Estimate the KDE integral over each bin *)
    hist=Table[PDF[kde,{x,y}]dx dy,{x,xmin+dx/2,xmax-dx/2,dx},{y,ymin+dy/2,ymax-dy/2,dy}];
    (* calculate the cummulative bin counts when bins are in descending contents order *)
    sorted=Sort[Flatten[hist],Greater];
    cummulative=Accumulate[sorted];
    (* calculate the contour levels for each fraction *)
    contourLevels=Map[sorted[[Position[cummulative,First[Nearest[cummulative,#1]],1,1][[1,1]]]]/(dx dy)&,fractions];
    (* plot the contours *)
    ContourPlot[PDF[kde,{x,y}],{x,plotXmin,plotXmax},{y,plotYmin,plotYmax},
        plotOptions,Contours->contourLevels,PlotRange->{{plotXmin,plotXmax},{plotYmin,plotYmax},All},ContourShading->None
    ]
]


End[]


EndPackage[]
