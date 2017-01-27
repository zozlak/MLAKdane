#' Odfiltrowuje obserwacje na podstawie zmiennej \code{etap}
#' @description
#' Wartość zmiennej etap do odfiltrowania pobierana jest z sufiksu okienka
#' poprzez usunięcie znaków podkreślenia i zamianę pisowni na wielkie litery.
#' @param dane dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_publikacje}}, \code{\link{przygotuj_pomoc}} lub
#'   \code{\link{przygotuj_projekty}}
#' @param okienko obiekt opisujący okienko stworzony za pomocą funkcji
#'   \code{\link{okienko}}
#' @return data.frame odfiltrowane dane
#' @export
#' @import dplyr
ustaw_okienko_etap = function(dane, okienko) {
  stopifnot(
    methods::is(dane, 'publikacje_df') | methods::is(dane, 'projekty_df') | methods::is(dane, 'pomoc_df'),
    methods::is(okienko, 'okienko')
  )
  klasy = class(dane)

  etapFiltr = sub('_', '', toupper(okienko[['sufiks']]))
  zmEtap = grep('_etap$', names(dane), value = TRUE)
  dane = dane %>%
    rename_(etap = zmEtap) %>%
    filter_(
      ~etap %in% etapFiltr
    ) %>%
    rename_(.dots = stats::setNames('etap', zmEtap))

  class(dane) = c('okienko_df', klasy)
  return(dane)
}