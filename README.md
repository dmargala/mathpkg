mathpkg
=======

Reusable packages for Mathematica.

Clone this package into a subfolder called DeepZot of your existing $BaseDirectory/Applications (system wide) or $UserBaseDirectory/Applications, e.g.

cd /Library/Mathematica/Applications
git clone https://github.com/deepzot/mathpkg.git DeepZot

To load a package from within a notebook, use the Needs[] command, e.g.

Needs["DeepZot`PowerTools`"]
