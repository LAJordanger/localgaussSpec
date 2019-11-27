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


**Notice:** A collection of new plots are under development (as a part
of a revision of https://arxiv.org/abs/1708.02166), and it is likely
that the implementation of these plots also might require some minor
adjustments with regard to the way the data will be stored on disk (in
order to reduce the time the `shiny`-application need to process the
data).  This adjustment will hopefully not require data computed under
the present version of the package to be recomputed, but this can not
be completely ruled out yet.  If you are interested in testing this
package, but would like to wait for the aforementioned update, then
please send me an email (Lars.Arne.Jordanger@hvl.no) and I will inform
you when I have had the time to iron out the required details.
