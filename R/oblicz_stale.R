#' oblicza proste zmienne niezwiazane z okienkami czasu
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{polacz_zus_zdau}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_stale = function(dane){
  stopifnot(
    is(dane, 'baza_df')
  )
  dane = dane %>%
    select_('id_zdau', 'id_platnika') %>% 
    group_by_('id_zdau') %>%
    summarize_(ifzus = ~ as.numeric(any(!is.na(id_platnika)))) %>%
    ungroup()
  return(dane)
}