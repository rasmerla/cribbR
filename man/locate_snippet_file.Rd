% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cribbR.R
\name{locate_snippet_file}
\alias{locate_snippet_file}
\title{Locate where the snippet file is hiding, and if it exists}
\usage{
locate_snippet_file(
  be_my_guess = NULL,
  win_user_name = NULL,
  rstudio_path = NULL,
  snippet_path = NULL,
  match_argument = "r.snippets",
  robust_mode = F
)
}
\arguments{
\item{be_my_guess}{Looks for user folders on Windows if TRUE. Set to TRUE if no arguments are specified.}

\item{win_user_name}{The name of window user folder to use.}

\item{rstudio_path}{Path to windows folder containing the R-studio installation ("c:/user/AppData/Roaming/Rstudio/")}

\item{snippet_path}{Path pointing to your snippet file ("r.snippets")}

\item{match_argument}{Not used, but allows you to check for other names of the snippet file (for development use.)}

\item{robust_mode}{Default, a probably more robust way to find the path for the snippet file.}
}
\value{
Path pointing to snippet file if it exists. Otherwise instructions on how to create it.
}
\description{
Locate where the snippet file is hiding, and if it exists
}
