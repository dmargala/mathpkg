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
      (default is 1)"


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
  tag[\"ZVEC\"] = sorted list of redshifts with data"


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
    raw=ReadList[pathOption<>$PathnameSeparator<>filename,{Number,Number,Real}];
    size=If[sizeOption===Automatic,Max[raw[[;;,1]]]+1,sizeOption];
    With[{values=raw[[;;,3]]},
        sparse=N[SparseArray[{
            1+raw[[;;,{1,2}]]->values,
            1+raw[[;;,{2,1}]]->values,
            {i_,i_}->1
        },{size,size}]];
    ];
    If[sparseOption===True,Return[sparse]];
    (* Convert the matrix to a dense packed array *)
    packed=Developer`ToPackedArray[N[Normal[sparse]]];
    If[sparseOption===Automatic&&ByteCount[sparse]<sparseThresholdOption ByteCount[packed],
        Return[sparse],
        Return[packed]
    ];
]]
Options[loadFitMatrix]={ "path"->".", "size"->Automatic, "sparse"->Automatic, "sparseThreshold"->1 };


End[]


EndPackage[]
