% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oblicz_utrata_etatu.R
\name{oblicz_utrata_etatu}
\alias{oblicz_utrata_etatu}
\title{oblicza zmienne UP_E, UP_EL oraz UP_ENL}
\usage{
oblicz_utrata_etatu(okienko, utrataPracy)
}
\arguments{
\item{okienko}{dane wygenerowane za pomocą funkcji
\code{\link{oblicz_okienko}} na danych miesięcznych (tzn. danych
wygenerowanych wcześniej funkcją \code{\link{agreguj_do_miesiecy}})}

\item{utrataPracy}{dane wygenerowane za pomocą funkcji
\code{\link{przygotuj_utrata_pracy}}}
}
\value{
data.frame wyliczone zmienne
}
\description{
oblicza zmienne UP_E, UP_EL oraz UP_ENL
}
