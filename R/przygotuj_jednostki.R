#' Przygotowuje słownik jednostek
#' @description
#' Zwraca słownik jednostek
#' @param katZr katalog, w którym znajduje się plik sl_instytucje.xlsx
#' @param zmiennaRok nazwa zmiennej z rokiem rekordu
#' @return [data.frame] ramka danych opisująca jednostki
#' @export
#' @import dplyr
przygotuj_jednostki = function(katZr, zmiennaRok = 'rok'){
  jednostki = as.tbl(openxlsx::readWorkbook(paste0(katZr, '/JEDNOSTKI.xlsx')))
  colnames(jednostki) = tolower(colnames(jednostki))

  lata = data_frame(rok = 2014L:as.integer(format(Sys.Date(), '%Y'))) %>%
    mutate_(
      x = 1L,
      f_do = ~paste0(rok, '-12-31')
    )

  jednostki = jednostki %>%
    rename_(
      teryt = 'jednostka_teryt',
      jednostka_nazwa = 'nazwa_pelna'
    ) %>%
    mutate_(
      teryt = ~ floor(as.numeric(teryt) / 10),
      jednostka_nazwa = ~gsub('"', '', jednostka_nazwa),
      x = 1L
    ) %>%
    inner_join(lata) %>%
    group_by_('jednostka_id') %>%
    # zapewnijmy, że każda jednostka będzie miała rekord dla każdego roku, nawet jeśli w teorii wtedy nie istniała (bo kto wie, co siedzi w zbiorze ZDAU)
    mutate_(data_od = ~coalesce(data_od, '1900-01-01')) %>%
    mutate_(data_od = ~if_else(data_od == min(data_od, na.rm = TRUE), '1900-01-01', data_od)) %>%
    ungroup() %>%
    filter_(~data_od <= f_do) %>%
    group_by_('rok', 'jednostka_id') %>%
    arrange_('desc(data_od)') %>%
    filter_(~row_number(jednostka_id) == 1) %>%
    ungroup() %>%
    select_('-data_od', '-f_do', '-x', '-zmiana_nazwy_jednostki')

  if (any(is.na(jednostki$teryt))) {
    warning(sum(is.na(jednostki$teryt)), ' jednostek bez kodu TERYT')
  }
  stopifnot(jednostki %>% group_by_('rok', 'jednostka_id') %>% filter_(~n() > 1) %>% nrow() == 0)

  class(jednostki) = c('jednostki_df', class(jednostki))
  return(jednostki %>% rename_(.dots = stats::setNames(list('rok'), zmiennaRok)))
}