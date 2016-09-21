#' oblicza zmienne generowane z poziomu (zus x zdau) na poziom (id) (dla danego
#' okienka czasu)
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_osoba = function(dane){
  stopifnot(
    is(dane, 'okienko_df')
  )

  studia = dane %>%
    filter_(~ okres >= data_rozp & okres <= data_zak) %>%
    select_('id', 'okres') %>%
    distinct()
    
  zdau = dane %>%
    select_('id_zdau', 'id', 'okres_min', 'okres_max') %>%
    distinct()
  
  dane = zdau %>%
    left_join(studia) %>%
    filter_(~ okres >= okres_min & okres <= okres_max) %>%
    group_by_('id_zdau') %>%
    summarize_(
      nmstud = ~ length(unique(okres))
    )
  dane = zdau %>%
    left_join(dane) %>%
    mutate_(
      nmstud = ~ ifelse(is.na(nmstud), 0, nmstud)
    )
  
  return(dane)
}