% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wczytaj_do_sparka.R
\name{wczytaj_do_sparka}
\alias{wczytaj_do_sparka}
\title{Wczytuje i rejestruje podany plik CSV w Sparku}
\usage{
wczytaj_do_sparka(polSparka, nazwaPliku, katalog = NULL)
}
\arguments{
\item{polSparka}{połączenie ze sparkiem uzyskane za pomocą
\code{\link[sparklyr]{spark_connect}}}

\item{nazwaPliku}{nazwa pliku do wczytania (bez rozszerzenia)}

\item{katalog}{katalog katalog, w którym znajduje się plik (gdy nie podany,
funkcja zakłada, że plik znajduje się w podkatalogu "cache" katalogu
roboczego)}
}
\value{
ramka danych Sparka
}
\description{
Wczytuje i rejestruje podany plik CSV w Sparku
}
