#' tworzy obiekt opisujący okienko czasu
#' @param offsetMin pierwszy miesiąc okienka względem wartości zmiennej
#'   wskazywanej przez parametr \code{zmiennaMin}
#' @param offsetMax ostatni miesiąc okienka względem wartości zmiennej
#'   wskazywanej przez parametr \code{zmiennaMax}
#' @param zmiennaMin nazwa zmiennej, względem której obliczany jest początek
#'   okienka
#' @param zmiennaMax nazwa zmiennej, względem której obliczany jest koniec
#'   okienka
#' @param sufiks sufiks nazw zmiennych tego okienka czasu
#' @param dataMin początek okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2014-01-01')
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2015-09-30')
#' @return [okienko] obiekt opisujący okienko czasu
okienko = function(offsetMin, offsetMax, zmiennaMin, zmiennaMax, sufiks, dataMin, dataMax) {
  stopifnot(
    is.vector(offsetMin), is.numeric(offsetMin), length(offsetMin) == 1, all(!is.na(offsetMin)),
    is.vector(offsetMax), is.numeric(offsetMax), length(offsetMax) == 1, all(!is.na(offsetMax)),
    is.vector(zmiennaMin), is.character(zmiennaMin), length(zmiennaMin) == 1, all(!is.na(zmiennaMin)),
    is.vector(zmiennaMax), is.character(zmiennaMax), length(zmiennaMax) == 1, all(!is.na(zmiennaMax)),
    is.vector(sufiks), is.character(sufiks), length(sufiks) == 1, all(!is.na(sufiks)),
    is.vector(dataMin), is.character(dataMin), length(dataMin) == 1, all(!is.na(dataMin)),
    is.vector(dataMax), is.character(dataMax), length(dataMax) == 1, all(!is.na(dataMax))
  )
  o = list(
    offsetMin = offsetMin,
    offsetMax = offsetMax,
    zmiennaMin = zmiennaMin,
    zmiennaMax = zmiennaMax,
    sufiks = sufiks,
    dataMin = dataMin,
    dataMax = dataMax
  )
  class(o) = 'okienko'
  return(o)
}