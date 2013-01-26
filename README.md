mathpkg
=======

Reusable packages for Mathematica.

Clone this package into a subfolder called DeepZot of your existing $BaseDirectory/Applications (system wide) or $UserBaseDirectory/Applications, e.g.

    cd /Library/Mathematica/Applications
    git clone https://github.com/deepzot/mathpkg.git DeepZot

To load a package from within a notebook, use the Needs[] command, e.g.

    Needs["DeepZot`PowerTools`"]

To get a list of the functions available in a package and browse their help, use e.g.

    ?DeepZot`PowerTools`*

Many of the help strings are not very detailed yet so, as a last resort, you may need to look up the function definition to figure out the order of its expected parameters, e.g.

    ??sbTransform
