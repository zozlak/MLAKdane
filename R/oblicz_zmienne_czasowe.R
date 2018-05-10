#' oblicza zmienne związane z czasem, który upłynął od momentu uzyskania dyplomu
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{polacz_zus_zdau}}
#' @param utrataPracy dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_utrata_pracy}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_zmienne_czasowe = function(dane, utrataPracy){
  stopifnot(
    methods::is(dane, 'tbl_spark') # Spark inaczej ewaluuje niektóre funkcje (np. n_distinct), co prowadziłoby do różnych wyników
  )

  dane = dane %>%
    filter_(~okres >= data_do) %>%
    left_join(utrataPracy, copy = TRUE) %>%
    mutate_(
      roznica = ~okres - data_do
    ) %>%
    group_by_('id_zdau') %>%
    summarize_(
      data_do = ~min(data_do, na.rm = TRUE),
      tp_m2 = ~min(if_else((roznica == 0L & is.na(utrmundur)  | roznica > 0L) & mundur > 0L, roznica, NA_integer_), na.rm = TRUE),
      tp_j2 = ~min(if_else((roznica == 0L & is.na(utrprawnik) | roznica > 0L) & prawnik > 0L, roznica, NA_integer_), na.rm = TRUE),
      tp_p2 = ~min(if_else((roznica == 0L & is.na(utrpracy)   | roznica > 0L) & etat + netat + samoz > 0L, roznica, NA_integer_), na.rm = TRUE),
      tp_s2 = ~min(if_else((roznica == 0L & is.na(utrpracy)   | roznica > 0L) & samoz > 0L, roznica, NA_integer_), na.rm = TRUE),
      tp_e2 = ~min(if_else((roznica == 0L & is.na(utretatu)   | roznica > 0L) & etat > 0L, roznica, NA_integer_), na.rm = TRUE),
      tp_z2 = ~min(if_else((roznica == 0L & is.na(utrzatr)    | roznica > 0L) & etat + netat > 0L, roznica, NA_integer_), na.rm = TRUE)
    ) %>%
    mutate_(
      tp_m = ~if_else(is.na(tp_m2), NA_integer_, if_else(tp_m2 > 0L, tp_m2 - 1, 0L)),
      tp_j = ~if_else(is.na(tp_j2), NA_integer_, if_else(tp_j2 > 0L, tp_j2 - 1, 0L)),
      tp_p = ~if_else(is.na(tp_p2), NA_integer_, if_else(tp_p2 > 0L, tp_p2 - 1, 0L)),
      tp_s = ~if_else(is.na(tp_s2), NA_integer_, if_else(tp_s2 > 0L, tp_s2 - 1, 0L)),
      tp_e = ~if_else(is.na(tp_e2), NA_integer_, if_else(tp_e2 > 0L, tp_e2 - 1, 0L)),
      tp_z = ~if_else(is.na(tp_z2), NA_integer_, if_else(tp_z2 > 0L, tp_z2 - 1, 0L)),
      data_od_e  = ~data_do + tp_e2,
      data_od_es = ~data_do + if_else(tp_e2 < tp_s2  & !is.na(tp_e2) | is.na(tp_s2), tp_e2, tp_s2)
    ) %>%
    select_('-data_do') %>%
    collect() %>%
    mutate_(
      data_od_e  = ~okres2data(data_od_e),
      data_od_es = ~okres2data(data_od_es)
    )
  return(dane)
}