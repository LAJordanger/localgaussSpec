# localgaussSpec
Local Gaussian spectral densities, estimation and interactive
visualisation using a `shiny`-application


You can download this package by the help of the `remotes`-package.


The help files do not contain much information (yet) about how the
package should be used — but you can use the function
`LG_extract_script` to extract a collection of scripts which shows how
to investigate real and simulated data.  These scripts are included in
order to ensure the replicability of the univariate and multivariate
examples and figures investigated in the two local Gaussian spectral
density papers _Nonlinear spectral analysis: A local Gaussian
approach_ (published in the Journal of the American Statistical
Association, https://doi.org/10.1080/01621459.2020.1840991, see also
https://arxiv.org/abs/1708.02166) and _Local Gaussian Cross-Spectrum
Analysis_ (publised in Econometrics,
https://doi.org/10.3390/econometrics11020012), which respectively
deals with univariate and multivariate time series.  These two papers
build upon Paper 1 and Paper 2 in the PhD-dissertation _Nonlinear
Spectrum Analysis based on the Local Gaussian Correlation and Model
Selection for Copulas_ (https://bora.uib.no/handle/1956/16950).

Note that these scripts also are intended to be used as templates for
those that would like to investigate their own time series by means of
the local Gaussian spectral densities.


**Notice:** New theory and new plots where developed during the
review-process related to the paper _Nonlinear spectral analysis: A
local Gaussian approach_, and it is now possible to also investigate
the local Gaussian spectral densities by means of heatmap-based and
distance-based plots.  These new plots are not yet implemented in the
interactive `shiny`-application, but the interested user can produce
them by the help of the code in the relevant scripts.
