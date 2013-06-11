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
    - path : directory to prepend to filename (default is None)
    - size : size of the matrix to return (Automatic or integer value)
      (default is Automatic, which uses the largest index found)
    - sparse : should result be returned as a SparseArray? (Automatic,True,False)
      (default is Automatic, which decides based on the space saving)
    - sparseThreshold : prefers sparse matrix if sparseSize < threshold * denseSize
      (default is 1)";


loadFitResiduals::usage=
"loadFitResiduals[tag,prefix] loads the residuals output file for the specified
output prefix and associates the results with the specified tag. The following
options are supported:
    - verbose: be verbose about what we are doing (default is True)
    - path: directory containing the residuals file (default is None)
    - cov: load fit input covariance matrix from this file under path (default is None)
    - icov: load fit input inverse covariance matrix from this file under path (default is None)
    - nlargest: if cov or icov is specified and verbose is True, prints this number of modes
      with largest contribution to chisq (default 10)
Use the following keys to access the loaded residuals:
  tag[\"INDEX\"] = global bin index
  tag[\"USER\"] = bin center in the user-defined binning variables
  tag[\"RMUZ\"] = bin center in (r,mu,z)
  tag[\"PRED\"] = model prediction for this bin
  tag[\"DATA\"] = data for this bin
  tag[\"ERROR\"] = diagonal error for this bin
  tag[\"GRADS\"] = gradients of the model prediction in this bin
  tag[\"ZVEC\"] = sorted list of redshifts with data
If the cov or icov options are provided, the following extra keys are available:
  tag[\"ICOV\"] = inverse covariance pruned to the bins used in the fit.
  tag[\"EVAL\"] = eigenvalues of the pruned inverse covariance in descending order.
  tag[\"EVEC\"] = corresponding matrix of eigenvectors, with vectors stored in successive rows.
  tag[\"CHIJK\"] = matrix of chisq contributions by mode and bin.
  tag[\"CHIJ\"] = vector of chisq contributions by mode.";


loadFitAnalysis::usage=
"loadFitAnalysis[tag,name] loads the analysis output file for the specified analysis
name.";


fitDensityPlot::usage=
"fitDensityPlot[tag] uses ListDensityPlot to plot quantities related to fit residuals
previously loaded and associated with the specified tag.";


fitModePlot::usage=
"fitModePlot[tag,mode] plots the per-bin weights (left) and per-bin chisq contributions (right) of the specified eigenmode.";


Begin["Private`"]


makePath::nosuchpath="No such path \"`1`\".";
makePath::filenotfound="File not found \"`1`\".";
makePath[filename_,pathOption_]:=
Module[{path},
    path=If[!(pathOption===None)&&StringLength[ToString[pathOption]]>0,
        If[!DirectoryQ[ToString[pathOption]],
            Message[makePath::nosuchpath,pathOption];
            Return[$Failed]
        ];
        FileNameJoin[{pathOption,filename}],
        filename
    ];
    If[!FileExistsQ[path],
        Message[makePath::filenotfound,path];
        Return[$Failed]
    ];
    path
]


makeSample[line_,npar_,ndump_,nfit_]:=
With[{size=npar+1},
Table[{
  (* fit chisq *)
  line[[size(fit-1)+npar+1]],
  (* fit parameter values *)
  Table[line[[size(fit-1)+par]],{par,1,npar}],
  (* multipole dumps *)
  Table[line[[nfit size+3ndump(fit-1)+ndump(ell-1)+dump]],{ell,1,3},{dump,1,ndump}]
},{fit,1,nfit}
]]


Clear[loadFitAnalysis]
loadFitAnalysis::badheader1="Analysis output file has badly formatted header line 1.";
loadFitAnalysis::badheader2="Analysis output file has badly formatted header line 2.";
loadFitAnalysis::badshape="Analysis output file has badly shaped samples.";
loadFitAnalysis::badncol="Analysis output file has wrong number of sample columns.";
loadFitAnalysis[tag_,name_,OptionsPattern[loadFitAnalysis]]:=
With[{
    verboseOption=OptionValue["verbose"],
    pathOption=OptionValue["path"]
},
Module[{path,raw,npar,ndump,nfit,nrow,ncol},
  Clear[tag];
  (* load the analysis file into memory *)
  path=makePath[name<>".dat",pathOption];
  raw=ReadList[path,Number,RecordLists->True];
  (* parse the first header line *)
  If[Length[raw[[1]]]!=3,
    Message[loadFitAnalysis::badheader1];
    Return[$Failed]
  ];
  {npar,ndump,nfit}=raw[[1]];
  tag["NPAR"]=npar;
  tag["NDUMP"]=ndump;
  tag["NFIT"]=nfit;
  (* parse the second header line *)
  If[Length[raw[[2]]]!=nfit npar,
    Message[loadFitAnalysis::badheader2];
    Return[$Failed]
  ];
  tag["FITERR"]=Table[raw[[2,(fit-1)npar+par]],{fit,1,nfit},{par,1,npar}];
  (* parse the sample data *)
  If[Length[Dimensions[raw[[3;;]]]]!=2,
    Message[loadFitAnalysis::badshape];
    Return[$Failed]
  ];
  {nrow,ncol}=Dimensions[raw[[3;;]]];
  If[ncol!=nfit(1+npar+3ndump),
    Message[loadFitAnalysis::badncol];
    Return[$Failed]
  ];
  tag["NSAMPLE"]=nrow-1;
  tag["BASELINE"]=makeSample[raw[[3]],npar,ndump,nfit];
  tag["SAMPLE"]=Map[makeSample[##,npar,ndump,nfit]&,raw[[4;;]]];
  (* all done *)
  If[verboseOption===True,
    Print["Loaded ",nrow-1," samples with npar = ",npar,", ndump = ",ndump,", nfit = ",nfit]
  ];
]]
SetAttributes[loadFitAnalysis,HoldFirst]
Options[loadFitAnalysis]={
    "verbose"->True,"path"->None
};


Clear[loadFitResiduals]
loadFitResiduals[tag_,prefix_,OptionsPattern[loadFitResiduals]]:=
With[{
    verboseOption=OptionValue["verbose"],
    pathOption=OptionValue["path"],
    covOption=OptionValue["cov"],
    icovOption=OptionValue["icov"],
    nlargestOption=OptionValue["nlargest"]
},
Module[{path,raw,ncols,nbins,ngrads,cov,keep,chij,nlargest,largest},
    Clear[tag];
    (* load the residuals file into memory *)
    path=makePath[prefix<>"residuals.dat",pathOption];
    raw=Transpose[Developer`ToPackedArray[ReadList[path,Real,RecordLists->True]]];
    {ncols,nbins}=Dimensions[raw];
    ngrads=ncols-11;
    If[verboseOption===True,
        Print["Read residuals for ",nbins," bins."];
        If[ngrads>0,
            Print["Found gradients for ",ngrads," parameters."],
            Print["No gradients available."]
        ]
    ];
    (* associate each of the filled arrays with the tag using a string key *)
    tag["INDEX"]=IntegerPart[raw[[1]]];
    tag["USER"]=Transpose[raw[[2;;4]]];
    tag["RMUZ"]=Transpose[raw[[5;;7]]];
    tag["PRED"]=raw[[8]];
    tag["DATA"]=raw[[9]];
    tag["ERROR"]=raw[[10]];
    tag["GRADS"]=If[ngrads>0,Transpose[raw[[11;;]]],None];
    tag["ZVEC"]=Union[raw[[7]]];
    (* Read cov or icov if available. If both are provided, cov takes precedence. *)
    cov=None;
    If[!(covOption===None),
        cov=loadFitMatrix[covOption,"path"->pathOption,"verbose"->verboseOption];
    ];
    If[cov===None&&!(icovOption===None),
        cov=Inverse[loadFitMatrix[icovOption,"path"->pathOption,"verbose"->verboseOption]];
    ];
    If[!(cov===None),
        (* Prune the covariance to the bins used in the fit *)
        keep=1+tag["INDEX"];
        (* Save the corresponding inverse covariance *)
        tag["ICOV"]=Inverse[cov[[keep,keep]]];
        (* Calculate and save the eigenvalues and eigenvectors *)
        {tag["EVAL"],tag["EVEC"]}=Eigensystem[tag["ICOV"]];
        (* Calculate and save the matrix of chisq contributions to this fit *)
        tag["CHIJK"]=tag["EVEC"]Outer[Times,Sqrt[tag["EVAL"]],tag["DATA"]-tag["PRED"]];
        tag["CHIJ"]=Total/@tag["CHIJK"];
        If[verboseOption===True,
            Print["chi^2/dof = ",Total[tag["CHIJ"]^2]," / (",nbins," - ",ngrads,")"];
            nlargest=Min[nbins,nlargestOption];
            largest=Ordering[tag["CHIJ"]^2,nlargest,Greater];
            Print[nlargest," modes with largest chi^2 contributions:"];
            Print[TableForm[{largest,(tag["CHIJ"]^2)[[largest]]}]];
        ];
    ];
]]
SetAttributes[loadFitResiduals,HoldFirst]
Options[loadFitResiduals]={
    "verbose"->True,"path"->None,"cov"->None,"icov"->None,"nlargest"->10
};


(* See http://mathematica.stackexchange.com/questions/7887/best-way-to-create-symmetric-matrices *)
Clear[loadFitMatrix]
loadFitMatrix[filename_,OptionsPattern[loadFitMatrix]]:=
With[{
    verboseOption=OptionValue["verbose"],
    pathOption=OptionValue["path"],
    sizeOption=OptionValue["size"],
    sparseOption=OptionValue["sparse"],
    sparseThresholdOption=OptionValue["sparseThreshold"]
},
Module[{path,raw,size,sparse,packed},
    path=makePath[filename,pathOption];
    raw=ReadList[path,{Number,Number,Number}];
    size=If[sizeOption===Automatic,Max[raw[[;;,1]]]+1,sizeOption];
    If[verboseOption===True,Print["Read matrix with size = ",size,"."]];
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
        If[verboseOption===True,Print["Automatically selected sparse matrix format."]]; Return[sparse],
        If[verboseOption===True,Print["Automatically selected packed matrix format."]]; Return[packed]
    ];
]]
Options[loadFitMatrix]={ "verbose"->False,"path"->None, "size"->Automatic, "sparse"->Automatic, "sparseThreshold"->1 };


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


Clear[fitModePlot]
fitModePlot[tag_,mode_,options:OptionsPattern[{fitModePlot,fitDensityPlot,dataRange,ListDensityPlot}]]:=
With[{
  densityPlotOptions=FilterRules[{options},{Options[fitDensityPlot],Options[dataRange],Options[ListDensityPlot]}]
},
Print["Mode ",mode," of ",Length[tag["EVAL"]]," contributes ",tag["CHIJ"][[mode]]^2," to total chisq = ",Total[tag["CHIJ"]^2]];
GraphicsRow[{
    fitDensityPlot[tag,"vector"->tag["EVEC"][[mode]],densityPlotOptions,"centerValue"->0],
    fitDensityPlot[tag,"vector"->tag["CHIJK"][[mode]],densityPlotOptions,"centerValue"->0]
},Spacings->4]
]
Options[fitModePlot]={};


End[]


EndPackage[]
