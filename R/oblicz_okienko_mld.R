#' wylicza zadane okienko danych w zakresie specyficznym dla monitorowania losów
#' doktorantów
#' @param okienko obiekt okienka utworzony za pomocą funkcji
#'   \code{\link{okienko}}
#' @param zdau dane zwrócone przez funkcję \code{\link{przygotuj_zdau}}
#' @param publikacje dane zwrócone przez funkcję
#'   \code{\link{przygotuj_publikacje}}
#' @param projekty dane miesieczne zwrócone przez funkcję
#'   \code{\link{przygotuj_projekty}}
#' @param pomoc dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_pomoc}}
#' @return [data.frame] dane obliczone dla wskazanego okienka czasu
#' @import dplyr
oblicz_okienko_mld = function(okienko, zdau, publikacje, projekty, pomoc) {
  stopifnot(
    methods::is(okienko, 'okienko'),
    methods::is(zdau, 'zdau_df'),
    methods::is(publikacje, 'publikacje_df'),
    methods::is(projekty, 'projekty_df'),
    methods::is(pomoc, 'pomoc_df')
  )

  okienkoPub = ustaw_okienko_etap(publikacje, okienko)
  okienkoProj = ustaw_okienko_etap(projekty, okienko)
  okienkoPom = ustaw_okienko_etap(pomoc, okienko)

  pub = oblicz_publikacje(okienkoPub, zdau, F)
  proj = oblicz_projekty(okienkoProj, zdau, F)
  pom = oblicz_pomoc(okienkoPom, zdau, F)

  razem = zdau %>%
    full_join(pub) %>%
    full_join(proj) %>%
    full_join(pom) %>%

  colnames(razem) = sub('^id_zdau.*$', 'id_zdau', paste0(colnames(razem), okienko[['sufiks']]))
  return(razem)
}
