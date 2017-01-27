#' przypisuje odpowiedni etap kariery naukowej oraz jego długość
#' @description
#' przypisuje odpowiedni etap kariery naukowej oraz jego długość
#' @param dane ramka danych zawierająca zmienną \code{data_od}
#' @param kariera dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_kariere}}
#' @param dataMax koniec badanego okresu (jako łańcuch znaków, np. '2015-09-30')
#' @return data.frame ramka danych z dołączonymi zmiennymi \code{etap} oraz
#'   \code{len}
#' @import dplyr
oblicz_etap = function(dane, kariera, dataMax) {
  dataMax = data2okres(dataMax) + 1L
  kolumny = names(dane)

  dane = dane %>%
    left_join(
      kariera %>%
        select_('id', 'data_od_slic', 'data_od_smgr', 'data_od_sdr', 'data_od_dr', 'data_od_hab', 'data_od_prof')
    )

  dane = dane %>%
    mutate_(
    etap = ~case_when(
      .$data_od < .$data_od_slic & !is.na(.$data_od_slic) ~ NA_character_,
      .$data_od < .$data_od_smgr & !is.na(.$data_od_smgr) & !is.na(.$data_od_slic) ~ 'SLIC',
      .$data_od < .$data_od_sdr  & !is.na(.$data_od_sdr)  ~ 'SMGR',
      .$data_od < .$data_od_dr   & !is.na(.$data_od_dr)   ~ 'SDR',
      .$data_od < .$data_od_hab  & !is.na(.$data_od_hab)  ~ 'DR',
      .$data_od < .$data_od_prof & !is.na(.$data_od_prof) ~ 'HAB',
      .$data_od >= .$data_od_prof & !is.na(.$data_od_prof) ~ 'PROF',
      TRUE ~ NA_character_
    ),
    dataMax = ~dataMax
  )

  dane = dane %>%
    mutate_(
      len = ~case_when(
        is.na(.$etap) ~ NA_integer_,
        .$etap %in% 'SLIC' ~ coalesce(.$data_od_smgr, .$dataMax) - .$data_od_slic,
        .$etap %in% 'SMGR' ~ coalesce(.$data_od_sdr, .$dataMax) - .$data_od_smgr,
        .$etap %in% 'SDR'  ~ coalesce(.$data_od_dr, .$dataMax) - .$data_od_sdr,
        .$etap %in% 'DR'   ~ coalesce(.$data_od_hab, .$dataMax) - .$data_od_dr,
        .$etap %in% 'HAB'  ~ coalesce(.$data_od_prof, .$dataMax) - .$data_od_dr,
        .$etap %in% 'PROF' ~ coalesce(.$dataMax, .$dataMax) - .$data_od_prof,
        TRUE ~ NA_integer_
      )
    )

  dane = dane %>%
    select_(.dots = c(kolumny, 'etap', 'len'))

  return(dane)
}