% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LG_extract_scripts.R
\name{LG_extract_scripts}
\alias{LG_extract_scripts}
\title{Extraction of scripts}
\usage{
LG_extract_scripts(target_dir, overwrite = FALSE)
}
\arguments{
\item{target_dir}{The path to the directory where the scripts are
to be stored.  The user will be asked for permission to create
the directory if it does not already exist.}

\item{overwrite}{Logical value, default \code{FALSE}.  The default
behaviour will be that this function stops if it encounters a
non-empty directory.  This is done in order to avoid that
previously extracted (and potentially modified) versions of the
scripts are overwritten.  Set \code{overwrite} to \code{TRUE}
in order to override this requirement.}
}
\value{
The result of this function is that copies of the scripts
    occur in \code{target_dir}, and the user can then run them (or
    modify them) as desired.  Read the \code{ReadMe.txt}-file in
    the extracted folder for further details about the structure of
    the extracted scripts.
}
\description{
This function extracts the scripts from the internal
    file hierarchy of the \code{localgaussSpec}-package to a target
    directory selected by the user.
}
\details{
If the \code{target_dir}-argument is missing, then the
    function will print a message that points to this help-page.
    If \code{target_dir} is a non-existing directory, then the user
    will be asked for permission to create it.  If
    \code{target_dir} exists but are non-empty, then it is required
    that the \code{overwrite}-argument must be \code{TRUE} in order
    for the scripts to be extracted.
}
