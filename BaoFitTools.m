(* ::Package:: *)

(* Created 16-May-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`BaoFitTools`",{"DeepZot`PlotTools`"}]


BaoFitTools::usage=
"A collection of utilities for analyzing and displaying baofit outputs."


loadFitMatrix::usage=
"loadFitMatrix[filename] constructs a symmetric matrix from a sequence of lines read
from the specified file of the form 'i j matrix(i,j)' where i,j are indices starting
from zero. Because of the assumed symmetry, only one of (i,j) and (j,i) are needed in
the file, but redundant entries are harmless. Missing diagonal elements are assumed to
be one and missing off-diagonal elements are assumed to be zero. The following options
are supported:
    - path : directory containing the file to read (default is '.')
    - size : size of the matrix to return (Automatic or integer value)
      (default is Automatic, which uses the largest index found)
    - sparse : should result be returned as a SparseArray? (Automatic,True,False)
      (default is Automatic, which decides based on the space saving)
    - sparseThreshold : prefers sparse matrix if sparseSize < threshold * denseSize
      (default is 1)";


loadFitResiduals::usage=
"loadResiduals[tag,prefix] loads the residuals output file for the specified
output prefix and associates the results with the specified tag. The following
options are supported:
    - path: directory containing the residuals file (default is '.')
Use the following keys to access the loaded residuals:
  tag[\"INDEX\"] = global bin index
  tag[\"USER\"] = bin center in the user-defined binning variables
  tag[\"RMUZ\"] = bin center in (r,mu,z)
  tag[\"PRED\"] = model prediction for this bin
  tag[\"DATA\"] = data for this bin
  tag[\"ERROR\"] = diagonal error for this bin
  tag[\"GRADS\"] = gradients of the model prediction in this bin
  tag[\"ZVEC\"] = sorted list of redshifts with data";


fitDensityPlot::usage=
"fitDensityPlot[tag] uses ListDensityPlot to plot quantities related to fit residuals
previously loaded and associated with the specified tag.";


Begin["Private`"]


Clear[loadFitResiduals]
loadFitResiduals[tag_,prefix_,OptionsPattern[loadFitResiduals]]:=
With[{
    pathOption=OptionValue["path"]
},
Module[{raw,ncols,nbins,ngrads},
    Clear[tag];
    (* load the residuals file into memory *)
    raw=Transpose[Developer`ToPackedArray[
        ReadList[pathOption<>$PathnameSeparator<>prefix<>"residuals.dat",
            Real,RecordLists->True]
    ]];
    {ncols,nbins}=Dimensions[raw];
    ngrads=ncols-11;
    (* associate each of the filled arrays with the tag using a string key *)
    tag["INDEX"]=IntegerPart[raw[[1]]];
    tag["USER"]=Transpose[raw[[2;;4]]];
    tag["RMUZ"]=Transpose[raw[[5;;7]]];
    tag["PRED"]=raw[[8]];
    tag["DATA"]=raw[[9]];
    tag["ERROR"]=raw[[10]];
    tag["GRADS"]=If[ngrads>0,Transpose[raw[[11;;]]],None];
    tag["ZVEC"]=Union[raw[[7]]];
]]
SetAttributes[loadFitResiduals,HoldFirst]
Options[loadFitResiduals]={
    "path"->"."
};


(* See http://mathematica.stackexchange.com/questions/7887/best-way-to-create-symmetric-matrices *)
Clear[loadFitMatrix]
loadFitMatrix[filename_,OptionsPattern[loadFitMatrix]]:=
With[{
    pathOption=OptionValue["path"],
    sizeOption=OptionValue["size"],
    sparseOption=OptionValue["sparse"],
    sparseThresholdOption=OptionValue["sparseThreshold"]
},
Module[{raw,size,sparse,packed},
    raw=ReadList[pathOption<>$PathnameSeparator<>filename,{Number,Number,Number}];
    size=If[sizeOption===Automatic,Max[raw[[;;,1]]]+1,sizeOption];
    With[{values=N[raw[[;;,3]]]},
        sparse=N[SparseArray[{
            1+raw[[;;,{1,2}]]->values,
            1+raw[[;;,{2,1}]]->values,
            {i_,i_}->1
        },{size,size}]];
    ];
    If[sparseOption===True,Return[sparse]];
    (* Convert the matrix to a dense packed array *)
    packed=Developer`ToPackedArray[Normal[sparse]];
    If[sparseOption===Automatic&&ByteCount[sparse]<sparseThresholdOption ByteCount[packed],
        Return[sparse],
        Return[packed]
    ];
]]
Options[loadFitMatrix]={ "path"->".", "size"->Automatic, "sparse"->Automatic, "sparseThreshold"->1 };


(* Returns a tuple { rT, rP, r^rpow value} given a value and an offset to use into tag[COORDS] *)
Clear[rxyTuple]
rxyTuple[offset_,value_,tag_,rpow_:0]:=
With[{r=tag["RMUZ"][[offset,1]],mu=tag["RMUZ"][[offset,2]]},
    {r Sqrt[1-mu^2],r mu,r^rpow value}
]


(* Returns a list of offsets corresponding to the specified values of the specified bin
coordinates, with _ representing a wildcard in the pattern *)
Clear[binSlice]
binSlice[tag_,pattern_,key_:"RMUZ"]:=Flatten[Position[tag[key],pattern]]


(* Returns sorted lists of the user coordinate values used for the first two axes at the specified
value of the third redshift axis, suitable for use with the Mesh plotting option *)
userGrid[tag_,zval_]:=With[{bins=tag["USER"][[binSlice[tag,{_,_,zval},"USER"]]]},
{Union[bins[[;;,1]]],Union[bins[[;;,2]]]}
]


Clear[fitDensityPlot]
fitDensityPlot::badtag="Invalid tag `1`.";
fitDensityPlot::badkey="Invalid key `1`.";
fitDensityPlot::badvec="Invalid vector for plotting.";
fitDensitPlot::badgrid="Invalid grid option, should be None, Automatic, \"cartesian\" or \"polar\".";
fitDensityPlot[tag_,options:OptionsPattern[{fitDensityPlot,dataRange,ListDensityPlot}]]:=
With[{
    rpow=OptionValue["rpow"],
    zindex=OptionValue["zindex"],
    key=OptionValue["key"],
    vector=OptionValue["vector"],
    grid=OptionValue["grid"],
    rmin=OptionValue["rmin"],
    rmax=OptionValue["rmax"]
},
Module[{vec,zval,points,rTmax,rPmax,range,zmin,zmax,gridOptions,regionFunction},
    If[!ValueQ[tag["ZVEC"]],
        Message[fitDensityPlot::badtag,ToString[tag]];
        Return[$Failed]
    ];
    vec=If[vector===None,tag[key],vector];
    If[vector===None&&!ValueQ[tag[key]],
        Message[fitDensityPlot::badkey,key];
        Return[$Failed]
    ];
    If[Length[vec]<Length[tag["INDEX"]],
        Message[fitDensityPlot::badvec];
        Return[$Failed]
    ];
    zval=tag["ZVEC"][[zindex]];
    points=Table[rxyTuple[k,vec[[k]],tag,rpow],{k,binSlice[tag,{_,_,zval},"RMUZ"]}];
    rTmax=Max[points[[;;,1]]];
    rPmax=Max[points[[;;,2]]];
    range=dataRange[points[[;;,3]],FilterRules[{options},Options[dataRange]]];
    {zmin,zmax}=range;
    gridOptions=Which[
        grid===None,{},
        grid===Automatic,{MeshStyle->Opacity[0.25],Mesh->All},
        grid==="cartesian",{
            MeshStyle->Opacity[0.25],Mesh->userGrid[tag,zval],MeshFunctions->{#1&,#2&}
        },
        grid==="polar",{
            MeshStyle->Opacity[0.25],Mesh->userGrid[tag,zval],MeshFunctions->{Sqrt[#1^2+#2^2]&,#2/Sqrt[#1^2+#2^2]&}
        },
        True,Message[fitDensityPlot::badgrid];Return[$Failed]
    ];
    regionFunction=Which[
        NumberQ[rmin]&&NumberQ[rmax]&&rmin>0&&rmax>rmin,{RegionFunction->(rmin^2<=#1^2+#2^2<=rmax^2&)},
        NumberQ[rmax]&&rmax>0,{RegionFunction->(#1^2+#2^2<=rmax^2&)},
        NumberQ[rmin]&&rmin>0,{RegionFunction->(#1^2+#2^2>=rmin^2&)},
        True,{}
    ];
    ListDensityPlot[points,FilterRules[{options},Options[ListDensityPlot]],
        gridOptions,regionFunction,
        InterpolationOrder->0,PlotRange->{{0,rTmax},{0,rPmax},range},
        LabelStyle->Medium,ColorFunctionScaling->False,
        ColorFunction->(temperatureMap[(##-zmin)/(zmax-zmin)]&),
        FrameLabel->{"\!\(\*SubscriptBox[\(r\), \(\[Perpendicular]\)]\)(Mpc/h)","\!\(\*SubscriptBox[\(r\), \(||\)]\)(Mpc/h)"}
    ]
]]
Options[fitDensityPlot]={"rpow"->0,"zindex"->1,"key"->"PRED","vector"->None,"grid"->Automatic,"rmin"->None,"rmax"->None};


End[]


EndPackage[]
