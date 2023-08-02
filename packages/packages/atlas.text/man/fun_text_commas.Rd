\name{fun_text_commas}
\alias{fun_text_commas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
fun_text_commas
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
An auxiliary function to separate arguments with commas, and also wrap them in quotes, if so required.
}
\usage{
fun_text_commas(
    ...
    , chr_sep = ','
    , chr_last_sep = ', and'
    , lgc_quote = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
  Any number of \code{\link[base::character]{character}} elements or any data type that is coercible to a character.
  }
  \item{chr_sep}{
  A \code{\link[base::character]{character}} string to separate the terms. Defaults to a comma.
  }
  \item{chr_last_sep}{
  A \code{\link[base::character]{character}} string to separate the last terms. Defaults to ", and".
  }
  \item{lgc_quote}{
  A \code{\link[base::logical]{logical}} element indicating whether or not to wrap each text element in quotes.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A character string with the \code{...} terms separated by \code{chr_sep} and \code{chr_last_sep}. Additionally, if \code{lgc_quote} is \code{TRUE}, the terms are wrapped in quotes.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Cao Bittencourt Ferreira
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
fun_text_commas('1', '2'){}
fun_text_commas(1, 2){}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
