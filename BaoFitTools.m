(* ::Package:: *)

(* Created 16-May-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`BaoFitTools`"]


BaoFitTools::usage=
"A collection of utilities for analyzing and displaying baofit outputs."


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
  tag[\"ZVEC\"] = sorted list of redshifts with data
"


Begin["Private`"]


Clear[loadFitResiduals]
loadFitResiduals[tag_,prefix_,OptionsPattern[loadFitResiduals]]:=
With[{
    path=OptionValue["path"]
},
Module[{raw,ncols,nbins,ngrads},
    Clear[tag];
    (* load the residuals file into memory *)
    raw=Transpose[Developer`ToPackedArray[
        ReadList[path<>$PathnameSeparator<>prefix<>"residuals.dat",
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


End[]


EndPackage[]
