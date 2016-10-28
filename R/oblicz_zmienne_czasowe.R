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
    filter_(~okres >= data_zak) %>%
    left_join(utrataEtatu) %>%
    mutate_(
      roznica =~ okres - data_zak
    )
  if(multidplyr){
    dane = multidplyr::partition(dane, id_zdau)
  }else{
    dane = group_by_(dane, 'id_zdau')
  }
  dane = dane %>%
    summarize_(
      tp_m2 = ~dplyr::na_if(min(ifelse((roznica == 0 & is.na(utrmundur)  | roznica > 0) & mundur > 0, roznica, NA_integer_), na.rm = TRUE), Inf),
      tp_j2 = ~dplyr::na_if(min(ifelse((roznica == 0 & is.na(utrprawnik) | roznica > 0) & prawnik > 0, roznica, NA_integer_), na.rm = TRUE), Inf),
      tp_p2 = ~dplyr::na_if(min(ifelse((roznica == 0 & is.na(utrpracy)   | roznica > 0) & etat + netat + samoz > 0, roznica, NA_integer_), na.rm = TRUE), Inf),
      tp_s2 = ~dplyr::na_if(min(ifelse((roznica == 0 & is.na(utrpracy)   | roznica > 0) & samoz > 0, roznica, NA_integer_), na.rm = TRUE), Inf),
      tp_e2 = ~dplyr::na_if(min(ifelse((roznica == 0 & is.na(utretatu)   | roznica > 0) & etat > 0, roznica, NA_integer_), na.rm = TRUE), Inf),
      tp_z2 = ~dplyr::na_if(min(ifelse((roznica == 0 & is.na(utrzatr)    | roznica > 0) & etat + netat > 0, roznica, NA_integer_), na.rm = TRUE), Inf)
    ) %>%
    mutate_(
      tp_m = ~ifelse(is.na(tp_m2), NA_integer_, ifelse(tp_m2 > 0, tp_m2 - 1, 0)),
      tp_j = ~ifelse(is.na(tp_j2), NA_integer_, ifelse(tp_j2 > 0, tp_j2 - 1, 0)),
      tp_p = ~ifelse(is.na(tp_p2), NA_integer_, ifelse(tp_p2 > 0, tp_p2 - 1, 0)),
      tp_s = ~ifelse(is.na(tp_s2), NA_integer_, ifelse(tp_s2 > 0, tp_s2 - 1, 0)),
      tp_e = ~ifelse(is.na(tp_e2), NA_integer_, ifelse(tp_e2 > 0, tp_e2 - 1, 0)),
      tp_z = ~ifelse(is.na(tp_z2), NA_integer_, ifelse(tp_z2 > 0, tp_z2 - 1, 0))
    ) %>%
    collect()
  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}