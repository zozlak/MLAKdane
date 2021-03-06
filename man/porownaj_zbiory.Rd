% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/porownaj_zbiory.R
\name{porownaj_zbiory}
\alias{porownaj_zbiory}
\title{Porównuje ze sobą wskazane zbiory}
\usage{
porownaj_zbiory(z1, z2, wspolne = c("ID_ZDAU", "id_zdau", "ID", "id",
  "OKRES", "okres", "id_zdu3"), pominNumInt = TRUE)
}
\arguments{
\item{z1}{pierwszy zbiór do porównania}

\item{z2}{drugi zbiór do porównania}

\item{wspolne}{lista kolumn, po których porównywane zbiory będą łączone}

\item{pominNumInt}{nie zgłaszaj różnic typów gdy zmienna w \code{z1} jest
typu \code{numeric}, a zmienna w \code{z2} typu \code{integer}}
}
\description{
Porównuje ze sobą wskazane zbiory
}
