(* ::Package:: *)

(* Created 5-Jan-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`PlotTools`"]


PlotTools::usage=
"A collection of plotting utilities."


createFrame::usage=
"Creates an empty plot frame of the specified type (Plot,LogPlot,LogLogPlot,LogLinearPlot),
with the specified axis limits and options."


pointWithError::usage=
"Creates a point with a vertical error bar."


valueWithFixedPrecision::usage=
"valueWithFixedPrecision[val,prec] returns a string representation of val rounded to 10^(-prec)."


autoRangeValue::usage=
"autoRangeValue[val] returns a string representation of val using an automatically chosen
SI multiplier. The default format is \"<ranged> <SI-mult>\", e.g. \"12.3 k\" for val=12345.
The following options are supported:
  - digits (4): number of digits of precision to display, must be integer.
  - breakpoint (-1): ranged value is between 10^b and 10^(b+3), must be integer.
  - separator (\" \"): appears between <ranged> and <SI-mult>.
  - unit (None): appears after <SI-mult>.
  - prefixes ({\"y\",\"z\",...,\"Z\",\"Y\"}): prefix labels to use for E-24 through E+24."


temperatureMap::usage=
"A replacement for the builtin TemperatureMap that is pure white at its midpoint."


drawEllipse::usage=
"drawEllipse[x,y,dx,dy,rho] draws an ellipse centered at (x,y) whose bounding box
dimensions are 2(dx,dy), with tangents at (dx, rho dy) and (rho dx, dy).
Use dx = n \[Sigma]x, dy = n \[Sigma]y to draw the n-sigma contour of a likelihood.
For a Gaussian likelihood, use n = 1.51517 for 68% CL and n = 2.44775 for 95% CL.
Add options fillStyle and edgeStyle to customize the appearance."


rasterize::usage=
"rasterize[g] converts graphics to a bitmap with anti-aliasing.
rasterize[g,oversampling->os] increases anti-aliasing quality but takes longer.
The default os=2 is usually ok.
rasterize[g,magnification->mag] magnifies the generated bitmap relative to its
on-screen size. The default mag=1 is best for exporting to a presentation, but
mag=2 or higher is necessary when preparing latex figures."


coverageContourPlot::usage=
"coverageContourPlot[dataset,{x1,x2,dx},{y1,y2,dy}] plots contours containing 68% and 95% of the dataset points
over the ranges (x1,x2) and (y1,y2) using a cell size (dx,dy) for kernel density estimation. Additional options
supported are coverageFractions, which specifies the contour levels to draw, and any options of ContourPlot."


dataRange::usage=
"dataRange[dataset] returns a range {lo,hi} for the specified dataset. The default range
includes all elements but the following options can be use:
  - clipLoFraction: clip the lo limit at the specified quantile in the flattened dataset.
  - clipHiFraction: clip the hi limit at the specified quantile in the flattened dataset.
  - clipLoValue: set the lo limit to the specified value (overrides clipLoFraction).
  - clipHiValue: set the hi limit to the specified value (overrides clipHiFraction).
  - centerValue: expands the range, if necessary, to be symmetric about this value.
  - padFraction: expands the range by this fraction on both sides (default is zero).
  - printRange: prints the returned range if True (default is False)."


pixelImage::usage=
"Draws an image of pixel data stored in a 2D rectangular list {{row1},{row2},...}
using ArrayPlot. The first row of data is drawn at the bottom of the image by default,
but this can be overridden with DataReversed->False. All options of ArrayPlot and
dataRange are supported, in addition to:
  - map: function that maps array values to display values (default is Identity).
  - magnification: relative amount to increase the image size by.
  - zoom: amount to zoom the image about its center, clipping the borders.
The default color map is grayscale with white representing the maximum value. Use
the ColorFunction option to change this. When using dataRange options to clip some
data, the ClippingStyle->{loStyle,hiStyle} option can be used to control how
clipped pixels are displayed. The default image size matches the dimensions of the
pixel data, but can be changed with the ImageSize (absolute) or magnification
(relative) options."


Begin["Private`"]


createFrame[type_:Plot,{xmin_,xmax_},{ymin_,ymax_},options___]:=
Module[{method,prototype,defaults},
    method=ToExpression["List"<>ToString[type]];
    defaults={Frame->True,LabelStyle->Medium};
    If[StringPosition[ToString[type],"Log"]!={},AppendTo[defaults,{GridLines->Automatic,GridLinesStyle->LightGray}]];
    prototype=method[{{1,1}},PlotRange->{{xmin,xmax},{ymin,ymax}},options,defaults];
    DeleteCases[prototype,{__,_Point},Infinity]
]
HoldFirst[createFrame];


errorInterval[{plus_,minus_}]:={plus,minus} /; NumericQ[plus]&&NumericQ[minus]&&plus>=0&&minus<=0
errorInterval[plusMinus_]:=errorInterval[{+plusMinus,-plusMinus}] /; NumericQ[plusMinus]&&plusMinus>=0


errorIntervals[dy_]:={errorInterval[0],errorInterval[dy]}
errorIntervals[{dx_,dy_}]:={errorInterval[dx],errorInterval[dy]}


errorLine[xy1_,xy2_,size_,withTicks_]:=
Module[{graphics},
    graphics=Line[{xy1,xy2}];
]


pointWithError[point_,error_,options:OptionsPattern[]]:=
With[{theSize=OptionValue[size],theStyle=OptionValue[style],theError=errorIntervals[error]},
Module[{mapper,graphics,xy1,xy2},
    mapper={OptionValue[xMap][#1[[1]]],OptionValue[yMap][#1[[2]]]}&;
    graphics={PointSize[theSize],Point[mapper[point]]};
    If[!(style==={}),PrependTo[graphics,Directive[theStyle]]];
    AppendTo[graphics,Thickness[theSize/4]];
    If[!(theError[[1]]==={0,0}),
        xy1=mapper[point+{theError[[1,1]],0}];
        xy2=mapper[point+{theError[[1,2]],0}];
        AppendTo[graphics,Line[{xy1,xy2}]];
        If[OptionValue[xTicks],
            AppendTo[graphics,Line[{Scaled[{0,theSize/2},xy1],Scaled[{0,-theSize/2},xy1]}]];
            AppendTo[graphics,Line[{Scaled[{0,theSize/2},xy2],Scaled[{0,-theSize/2},xy2]}]];
        ]
    ];
    If[!(theError[[2]]==={0,0}),
        xy1=mapper[point+{0,theError[[2,1]]}];
        xy2=mapper[point+{0,theError[[2,2]]}];
        AppendTo[graphics,Line[{xy1,xy2}]];
        If[OptionValue[yTicks],
            AppendTo[graphics,Line[{Scaled[{theSize/2,0},xy1],Scaled[{-theSize/2,0},xy1]}]];
            AppendTo[graphics,Line[{Scaled[{theSize/2,0},xy2],Scaled[{-theSize/2,0},xy2]}]];
        ]
    ];
    graphics
]]
Options[pointWithError]={
    size->0.02,style->{},xMap->Identity, yMap->Identity,xTicks->True,yTicks->True
};


valueWithFixedPrecision[value_,precision_Integer]:=
With[{rounded=Round[value,10^(-precision)]},
Module[{leftSize,digits,formatted},
    leftSize=1+Max[0,Floor[Log[10,Abs[rounded]]]];
    digits=Map[ToString,RealDigits[rounded,10,leftSize+precision,leftSize-1][[1]]];
    If[precision<0,digits=Join[digits,ConstantArray["0",-precision]]];
    formatted=digits[[;;leftSize]];
    If[rounded<0,PrependTo[formatted,"-"]];
    If[precision>0,formatted=Join[AppendTo[formatted,"."],digits[[-precision;;]]]];
    StringJoin[formatted]
]]


autoRangeValue[value_,options:OptionsPattern[]]:=
With[{
    digits=OptionValue["digits"],
    breakpoint=OptionValue["breakpoint"],
    prefixes=OptionValue["prefixes"],
    separator=OptionValue["separator"],
    unit=OptionValue["unit"]
},
Module[{range,rangedValue,size,prefix},
    Assert[IntegerQ[digits]];
    Assert[IntegerQ[breakpoint]];
    range=Quotient[Floor[Log[10,Abs[value]]],3,breakpoint];
    rangedValue=value 10^(-3range);
    size=1+Floor[Log[10,Abs[rangedValue]]];
    prefix=If[Abs[range]>8,"E"<>ToString[3 range],prefixes[[range+9]]];
    valueWithFixedPrecision[rangedValue,digits-size]<>
        If[separator===None,"",separator]<>prefix<>If[unit===None,"",unit]
]]
Options[autoRangeValue]={
    "digits"->4,"breakpoint"->-1,"separator"->" ","unit"->None,
    "prefixes"->{"y","z","a","f","p","n","\[Mu]","m","","k","M","G","T","P","E","Z","Y"}
};


temperatureMap[z_]:=With[{
r=Clip[2 z,{0,1}],
g=1-4(z-0.5)^2,
b=Clip[3-4z,{0,1}]
},RGBColor[r,g,b]
]


(* See http://pdg.lbl.gov/2012/reviews/rpp2012-rev-statistics.pdf and http://users.tkk.fi/mvermeer/uncertainty.pdf *)
Clear[drawEllipse]
drawEllipse[x_,y_,dx_,dy_,rho_,OptionsPattern[]]:=
With[{fillStyle=OptionValue["fillStyle"],edgeStyle=OptionValue["edgeStyle"]},
Module[{avg,dlam,dab,\[Lambda]1,\[Lambda]2,\[Phi]},
    avg=(dx^2+dy^2)/2;
    dab=rho dx dy;
    dlam=Sqrt[(dx^2-dy^2)^2/4+dab^2];
    \[Lambda]1=avg+dlam;
    \[Lambda]2=avg-dlam;
    \[Phi]=ArcTan[(dx^2-dy^2),2 dab]/2;
    Graphics[{Directive[fillStyle],EdgeForm[edgeStyle],Rotate[Disk[{x,y},{Sqrt[\[Lambda]1],Sqrt[\[Lambda]2]}],\[Phi],{x,y}]}]
]]
Options[drawEllipse]={"fillStyle"->{Opacity[0.5]},"edgeStyle"->{Thick}};


rasterize[graphics_,options:OptionsPattern[]]:=
With[{size=ImageSize/.Options[graphics,ImageSize],mag=OptionValue[magnification],os=OptionValue[oversampling]},
    Rasterize[Magnify[graphics,mag],ImageSize->mag size,RasterSize->os mag size]
]
Options[rasterize]={ magnification->1,oversampling->2 };


Clear[coverageContourPlot]
coverageContourPlot[data_,{plotXmin_,plotXmax_,dx_},{plotYmin_,plotYmax_,dy_},options:OptionsPattern[{coverageContourPlot,ContourPlot}]]:=
With[{plotOptions=FilterRules[{options},Options[ContourPlot]],fractions=OptionValue["coverageFractions"]},
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
        Sequence[plotOptions], (* following options can be overridden in plotOptions *)
        Contours->contourLevels,PlotRange->{{plotXmin,plotXmax},{plotYmin,plotYmax},All},ContourShading->None
    ]
]]
Options[coverageContourPlot]={"coverageFractions"->{0.6827,0.9545}};


dataRange[data_,OptionsPattern[]]:=
With[{
    clipLoFraction=OptionValue["clipLoFraction"],
    clipHiFraction=OptionValue["clipHiFraction"],
    clipLoValue=OptionValue["clipLoValue"],
    clipHiValue=OptionValue["clipHiValue"],
    centerValue=OptionValue["centerValue"],
    padFraction=OptionValue["padFraction"],
    printRange=OptionValue["printRange"]
},
Module[{lo,hi,size},
    {lo,hi}=If[clipLoFraction>0||clipHiFraction<1,
        Quantile[Flatten[data],{clipLoFraction,clipHiFraction}],
        {Min[data],Max[data]}
    ];
    If[!(clipLoValue===Automatic),lo=clipLoValue];
    If[!(clipHiValue===Automatic),hi=clipHiValue];
    If[!(centerValue===Automatic),
        size=Max[hi-centerValue,centerValue-lo];
        hi=centerValue+size;
        lo=centerValue-size;
    ];
    If[padFraction>0,
        size=padFraction*(hi-lo);
        lo -= size;
        hi += size;
    ];
    If[printRange===True,Print["dataRange: ",{lo,hi}]];
    {lo,hi}
]]
Options[dataRange]={
  "clipLoValue"->Automatic,"clipHiValue"->Automatic,
  "clipLoFraction"->0,"clipHiFraction"->1,
  "centerValue"->Automatic,"padFraction"->0,"printRange"->False
};


Clear[pixelImage]
pixelImage[data_,options:OptionsPattern[{ArrayPlot,dataRange,pixelImage}]]:=
With[{
    map=OptionValue["map"],
    magnification=OptionValue["magnification"],
    zoom=OptionValue["zoom"]
},
Module[{w,h,dw,dh,zoomed,min,max,mmin,mmax,lo,hi,in,mdata},
    zoomed=data;
    (* zoom about the image center by the specified factor *)
    If[zoom>1,
        {w,h}=Dimensions[data];
        dw=Round[w (zoom-1)/zoom/2];
        dh=Round[h (zoom-1)/zoom/2];
        zoomed=data[[1+dw;;w-dw,1+dh;;h-dh]];
    ];
    {w,h}=Dimensions[zoomed];
    (* increase the image dimensions by the specified factor, if requested *)
    If[!(magnification===None),{w,h}=magnification {w,h}];
    (* calculate range to use after zooming but before mapping *)
    {min,max}=dataRange[zoomed,FilterRules[{options},Options[dataRange]]];
    {mmin,mmax}={map[0],map[1]};
    (* only apply the map to min \[LessEqual] z \[LessEqual] max, rescaled to 0-1 *)
    lo=UnitStep[min-zoomed];
    hi=UnitStep[zoomed-max];
    in=UnitStep[(zoomed-min)(max-zoomed)];
    mdata=((mmin-1.)lo+(mmax+1.)hi)(1-in)+Map[map,(zoomed-min)/(max-min),{2}]in;
    ArrayPlot[mdata,FilterRules[{options},Options[ArrayPlot]],
        DataReversed->True,ColorFunction->(GrayLevel[1-#]&),ImageSize->{w,h},
        AspectRatio->Full,Frame->True,PlotRangePadding->0,
        PlotRange->{mmin,mmax},ClippingStyle->{None,None}
    ]
]]
Options[pixelImage]={"map"->Identity,"magnification"->None,"zoom"->None};


End[]


EndPackage[]
