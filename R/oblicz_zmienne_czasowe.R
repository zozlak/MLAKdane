#' oblicza zmienne związane z czasem, który upłynął od momentu uzyskania dyplomu
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @param utrataEtatu dane wygenerowane za pomocą funkcji \code{\link{oblicz_utrata_etatu}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_zmienne_czasowe = function(dane, utrataEtatu, multidplyr = TRUE){
  stopifnot(
    is(dane, 'baza_df')
  )

  # liczone do dupy, to trzeba liczyć w podziale na pracodawców, bo tylko tam ma sens utratapracy!

  dane = dane %>%
    filter_(~ okres >= data_zak) %>%
    left_join(utrataEtatu) %>%
    mutate_(
      roznica   = ~ okres - data_zak
    )
  if(multidplyr){
    dane = multidplyr::partition(dane, id_zdau)
  }else{
    dane = group_by_(dane, 'id_zdau')
  }
  dane = dane %>%
    summarize_(
      tmuna  = ~ min(ifelse((roznica == 0 & is.na(utrmundur)  | roznica > 0) & mundur > 0, roznica, NA_integer_), na.rm = TRUE),
      tprawa = ~ min(ifelse((roznica == 0 & is.na(utrprawnik) | roznica > 0) & prawnik > 0, roznica, NA_integer_), na.rm = TRUE),
      tprda  = ~ min(ifelse((roznica == 0 & is.na(utrpracy)   | roznica > 0) & etat + netat + samoz > 0, roznica, NA_integer_), na.rm = TRUE),
      tprsda = ~ min(ifelse((roznica == 0 & is.na(utrpracy)   | roznica > 0) & samoz > 0, roznica, NA_integer_), na.rm = TRUE),
      tpruda = ~ min(ifelse((roznica == 0 & is.na(utretatu)   | roznica > 0) & etat > 0, roznica, NA_integer_), na.rm = TRUE),
      tzata  = ~ min(ifelse((roznica == 0 & is.na(utrzatr)    | roznica > 0) & etat + netat > 0, roznica, NA_integer_), na.rm = TRUE)
    ) %>%
    mutate_(
      tmuna  = ~ ifelse(is.infinite(tmuna),  NA_integer_, tmuna),
      tprawa = ~ ifelse(is.infinite(tprawa), NA_integer_, tprawa),
      tprda  = ~ ifelse(is.infinite(tprda),  NA_integer_, tprda),
      tprsda = ~ ifelse(is.infinite(tprsda), NA_integer_, tprsda),
      tpruda = ~ ifelse(is.infinite(tpruda), NA_integer_, tpruda),
      tzata  = ~ ifelse(is.infinite(tzata),  NA_integer_, tzata)
    ) %>%
    mutate_(
      munbaz = ~ ifelse(is.na(tmuna), 0, 1),
      tmun   = ~ ifelse(is.na(tmuna),  NA_integer_, ifelse(tmuna  > 0, tmuna  - 1, 0)),
      tpraw  = ~ ifelse(is.na(tprawa), NA_integer_, ifelse(tprawa > 0, tprawa - 1, 0)),
      tprd   = ~ ifelse(is.na(tprda),  NA_integer_, ifelse(tprda  > 0, tprda  - 1, 0)),
      tprsd  = ~ ifelse(is.na(tprsda), NA_integer_, ifelse(tprsda > 0, tprsda - 1, 0)),
      tprud  = ~ ifelse(is.na(tpruda), NA_integer_, ifelse(tpruda > 0, tpruda - 1, 0)),
      tzat   = ~ ifelse(is.na(tzata),  NA_integer_, ifelse(tzata  > 0, tzata  - 1, 0))
    ) %>%
    collect()
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}