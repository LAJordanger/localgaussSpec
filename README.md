[![Build Status](https://travis-ci.org/LAJordanger/localgaussSpec.svg?branch=master)](https://travis-ci.org/LAJordanger/localgaussSpec)

# localgaussSpec
Local Gaussian spectral densities, estimation and interactive
visualisation using a `shiny`-application


You can downlad this package by the help of the `remotes`-package.


The help files do not contain much information (yet) about how the
package should be used — but you can use the function
`LG_extract_script` to extract a collection of scripts which shows how
to investigate real and simulated data.  These scripts are included in
order to ensure the replicability of the univariate and multivariate
examples investigated in our local Gaussian spectral density papers
(https://arxiv.org/abs/1708.02166 and
https://arxiv.org/abs/1708.02495), and it will hopefully be a simple
task to adjust these scripts to fit other use-cases.


**Notice:** A major has been done on the univariate paper, see
https://arxiv.org/abs/1708.02166, and it is now possible to also
investigate the local Gaussian spectral densities by means of
heatmap-based and distance-based plots.  These new plots are not yet
implemented in the interactive `shiny`-application, but the interested
user can produce them by modifying the code in the relevant scripts.
