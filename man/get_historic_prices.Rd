% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_historic_prices.R
\name{get_historic_prices}
\alias{get_historic_prices}
\title{Functions to call and retrieve financial data from remote databases, by using company tickers (symbols). It uses publicly available API to download all data.}
\usage{
get_historic_prices(symbol)
}
\arguments{
\item{symbol}{Specifies the ticker or symbol the user wants to download the data of, in the form of a string. Example: 'MSFT'}
}
\value{
Returns a tibble
}
\description{
Functions to call and retrieve financial data from remote databases, by using company tickers (symbols). It uses publicly available API to download all data.
}
\examples{

prices <- get_historic_prices('MSFT')
}
\author{
Alejandro Jiménez Rico \email{aljrico@gmail.com}, \href{https://aljrico.github.io}{Personal Website}
}
