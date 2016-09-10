#' oblicza zmienne generowane z poziomu (zus x zdau) przez poziom (id x okres)
#' na poziom (zdau)
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_zdau = function(dane, zdau, multidplyr = TRUE){
  stopifnot(
    is(dane, 'okienko_df'),
    is(zdau, 'zdau_df')
  )

  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max)
  if(multidplyr){
    dane = multidplyr::partition(dane, id)
  }
  dane = dane %>%
    select_('id', 'id_zdau', 'okres') %>%
    distinct() %>%
    group_by_('id', 'okres') %>%
    summarize_(n = ~ n())

}