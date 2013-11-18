(* ::Package:: *)

(* Created 18-Nov-2013 by David Kirkby (University of California, Irvine) <dkirkby@uci.edu> *)

BeginPackage["DeepZot`StampTools`"]


StampTools::usage=
"A collection of utilities for working with postage stamp images.";


fftshift::usage=
"fftshift[x] shifts the zero-frequency component to the center of the spectrum.
Equivalent to the numpy.fft.fftshift function.";


ifftshift::usage=
"ifftshift[x] is the inverse of fftshift(x).
Equivalent to the numpy.fft.ifftshift function.";


fft2::usage=
"fft2[x] returns the 2D discrete Fourier transform of x. Uses FourierParameters->{1,-1}.
Equivalent to the numpy.fft.fft2 function.";


ifft2::usage=
"ifft2[x] returns the 2D discrete inverse Fourier transform of x. Uses FourierParameters->{1,-1}.
Equivalent to the numpy.fft.ifft2 function.";


fftfreq::usage=
"fftfreq[n,d] returns the frequencies of the discrete Fourier transform with length
n and spacing d (which defaults to one). Equivalent to the numpy.fft.fftfreq function.";


Begin["Private`"]


shiftAmount[n_]:=Ceiling[n/2]


fftshift[x_]:=RotateLeft[x,Map[shiftAmount,Dimensions[x]]]


ifftshift[x_]:=RotateRight[x,Map[shiftAmount,Dimensions[x]]]


fft2[x_]:=Fourier[x,FourierParameters->{1,-1}]


ifft2[x_]:=InverseFourier[x,FourierParameters->{1,-1}]


fftfreq[n_,d_:1]:=fftshift[Range[-Floor[n/2],+Floor[(n-1)/2]]]/(n d)


End[]


EndPackage[]
