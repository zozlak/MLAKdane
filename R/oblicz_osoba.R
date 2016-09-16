#' oblicza zmienne generowane z poziomu (zus x zdau) na poziom (id) (dla danego
#' okienka czasu)
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_osoba = function(dane, multidplyr = TRUE){
  stopifnot(
    is(dane, 'okienko_df')
  )

  dane = dane %>%
    filter_(~ okres >= okres_min & okres <= okres_max)
  zdau = dane %>%
    select_('id', 'id_zdau') %>%
    distinct()
  if(multidplyr){
    dane = multidplyr::partition(dane, id)
  }
  dane = dane %>%
    filter_(~ okres >= data_rozp & okres <= data_zak) %>%
    group_by_('id') %>%
    summarize_(
      nmstud = ~ length(unique(okres))
    ) %>%
    collect()
  dane = zdau %>%
    left_join(dane) %>%
    mutate_(
      nmstud = ~ ifelse(is.na(nmstud), 0, nmstud)
    ) %>%
    select_('-id')
  return(dane)
}