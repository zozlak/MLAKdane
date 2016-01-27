#' oblicza zmienne NMLE oraz NMLEP
#' @param okienko dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param utrataEtatu dane wygenerowane za pomocą funkcji \code{\link{oblicz_utrata_etatu}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_utrata_etatu = function(okienko, utrataEtatu, multidplyr = TRUE){
  stopifnot(
    is(okienko, 'okienko_df'),
    is(utrataEtatu, 'utrata_etatu_df')
  )

  nmle = okienko %>%
    filter_(~ okres >= okres_min & okres <= okres_max) %>%
    select_('id_zdau', 'id', 'okres') %>%
    distinct() %>%
    left_join(utrataEtatu)
  if(multidplyr){
    nmle = multidplyr::partition(nmle, id_zdau)
  }else{
    nmle = group_by_(nmle, 'id_zdau')
  }
  nmle = nmle %>%
    summarize_(
      nmle   = ~ sum(utretatu, na.rm = TRUE),
      nmlenp = ~ sum(utretatu_v2, na.rm = TRUE)
    ) %>%
    mutate_(
      nmlep = ~ nmle - nmlenp
    ) %>%
    collect()

  class(nmle) = c('absolwent_df', class(nmle))
  return(nmle)
}