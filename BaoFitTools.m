(* ::Package:: *)

(* Created 16-May-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`BaoFitTools`"]


BaoFitTools::usage=
"A collection of utilities for analyzing and displaying baofit outputs."


loadResiduals::usage=
"loadResiduals[tag,prefix] loads the residuals output file for the specified
output prefix and associates the results with the specified tag. The following
options are supported:
    - path: directory containing the residuals file (default is .)"


Begin["Private`"]


Clear[loadResiduals]
loadResiduals[tag_,prefix_,OptionsPattern[loadResiduals]]:=
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
    tag["BIN"]=Transpose[raw[[2;;4]]];
    tag["COORDS"]=Transpose[raw[[5;;7]]];
    tag["PRED"]=raw[[8]];
    tag["DATA"]=raw[[9]];
    tag["ERROR"]=raw[[10]];
    tag["GRADS"]=If[ngrads>0,Transpose[raw[[11;;]]],None];
]]
SetAttributes[loadResiduals,HoldFirst]
Options[loadResiduals]={
    "path"->"."
};


End[]


EndPackage[]
