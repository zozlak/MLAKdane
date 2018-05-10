#' Przygotowuje słownik kierunków
#' @description
#' Zwraca słownik kierunków studiów
#' @param katZr katalog, w którym znajduje się plik sl_instytucje.xlsx
#' @param agregujDoKierunku czy agregować dane tak by {rok, kierunek_id} były
#'   unikalne (jeśli FALSE, agregacja nastapi do unikalności na poziomie {rok,
#'   kierunek_id, jednostka_id})
#' @param jednostki ramka danych opisująca jednostki (gdy NULL, zostanie
#'   wczytana automatycznie)
#' @return [data.frame] ramka danych opisująca kierunki studiów
#' @export
#' @import dplyr
przygotuj_kierunki = function(katZr, agregujDoKierunku = TRUE, jednostki = NULL){
  kierunki = as.tbl(openxlsx::readWorkbook(paste0(katZr, '/KIERUNKI.xlsx')))
  colnames(kierunki) = tolower(colnames(kierunki))
  stopifnot(
    all(!is.na(kierunki$data_od)),
    all(!is.na(kierunki$data_do))
  )

  slFormy = c('niestacjonarne' = 'niestac.', 'stacjonarne' = 'stac.', 'stacjonarne_i_niestacjonarne' = 'stac./niestac.')
  slPoziomy = c('i_stopnia' = '1', 'ii_stopnia' = '2', 'jednolite_magisterskie' = 'JM')

  lata = data_frame(rok = 2014L:max(as.integer(substr(kierunki$data_do, 1, 4)))) %>%
    mutate_(
      x = 1L,
      f_od = ~paste0(rok, '-01-01'),
      f_do = ~paste0(rok, '-12-31')
    )

  kierunki = kierunki %>%
    rename_(
      kierunek_id  = 'studia_kier_id',
      jednostka_id = 'instytucja_id'
    ) %>%
    group_by_('kierunek_id') %>%
    #<-- zapewnijmy, że każdy kierunek będzie miał rekord dla każdego roku, nawet jeśli w teorii wtedy nie istniał (bo kto wie, co siedzi w zbiorze ZDAU)
    mutate_(
      x = 1L,
      dysc_na = ~is.na(dyscyplina_kod),
      data_od = ~if_else(data_od == min(data_od, na.rm = TRUE), '1900-01-01', data_od),
      data_do = ~if_else(data_do == max(data_do, na.rm = TRUE), as.character(Sys.Date()), data_do)
    ) %>%
    inner_join(lata) %>%
    group_by_('rok', 'kierunek_id') %>%
    arrange_('rok', 'kierunek_id', 'dysc_na') %>%
    mutate_(
      f = ~sum(data_od <= f_do & data_do >= f_od),
      n = ~row_number(kierunek_id)
    ) %>%
    ungroup() %>%
    filter_(~data_od <= f_do & data_do >= f_od | f == 0 & n == 1) %>%
    select_('-f', '-n')
    #-->
  if (agregujDoKierunku) {
    kierunki = kierunki %>%
      group_by_('rok', 'kierunek_id') %>%
      arrange_('dysc_na', 'desc(data_od)', 'desc(data_do)') %>%
      summarize_(
        jednostka_id      = ~first(jednostka_id),
        kierunek_nazwa    = ~paste0(unique(nazwa), collapse = '/'),
        poziom            = ~first(poziom_ksz),
        forma_ksztalcenia = ~if_else(n_distinct(formy_ksztalcenia) > 1, 'stacjonarne_i_niestacjonarne', first(formy_ksztalcenia)),
        miedzywydz        = ~n_distinct(jednostka_id) > 1,
        obsz_kod          = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', first(obszar_kod)))),
        obsz              = ~first(obszar_nazwa),
        dzie_kod          = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', first(dziedzina_kod)))),
        dzie              = ~first(dziedzina_nazwa),
        dysc_kod          = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', first(dyscyplina_kod)))),
        dysc              = ~first(dyscyplina_nazwa)
      )
  } else {
    kierunki = kierunki %>%
      group_by_('rok', 'kierunek_id', 'jednostka_id') %>%
      arrange_('dysc_na', 'desc(data_od)', 'desc(data_do)') %>%
      summarize_(
        kierunek_nazwa    = ~paste0(unique(nazwa), collapse = '/'),
        poziom            = ~first(poziom_ksz),
        forma_ksztalcenia = ~if_else(n_distinct(formy_ksztalcenia) > 1, 'stacjonarne_i_niestacjonarne', first(formy_ksztalcenia)),
        miedzywydz        = ~n_distinct(jednostka_id) > 1,
        obsz_kod          = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', first(obszar_kod)))),
        obsz              = ~first(obszar_nazwa),
        dzie_kod          = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', first(dziedzina_kod)))),
        dzie              = ~first(dziedzina_nazwa),
        dysc_kod          = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', first(dyscyplina_kod)))),
        dysc              = ~first(dyscyplina_nazwa)
      )
  }
  if (is.null(jednostki)) {
    jednostki = przygotuj_jednostki(katZr)
  }
  kierunki = kierunki %>%
    ungroup() %>%
    mutate_(
      poziom = ~slPoziomy[tolower(poziom)],
      forma_ksztalcenia = ~slFormy[tolower(forma_ksztalcenia)],
      dzie_kod = ~as.integer(sub('^([0-9])0([0-9])$', '\\1\\2', dzie_kod)),
      dysc_kod = ~as.integer(sub('^([0-9])0([0-9]{3})$', '\\1\\2', dysc_kod))
    ) %>%
    left_join(jednostki %>% select_('jednostka_id', 'rok', 'jednostka_nazwa')) %>%
    mutate_(
      kierunek_nazwa = ~sub('^.*; ?', '', paste0(jednostka_nazwa, ', ', kierunek_nazwa, ', ', forma_ksztalcenia, ' (POLon: ', kierunek_id, ')'))
    ) %>%
    select_('-jednostka_nazwa')

  if (any(is.na(kierunki$obsz_kod))) {
    warning('brak informacji o obszarach/dziedzinach/dyscyplinach dla ', sum(is.na(kierunki$obsz_kod)), ' rok-kierunków')
  }
  if (agregujDoKierunku) {
    stopifnot(kierunki %>% group_by_('rok', 'kierunek_id') %>% filter_(~n() > 1) %>% nrow() == 0)
  } else {
    stopifnot(kierunki %>% group_by_('rok', 'kierunek_id', 'jednostka_id') %>% filter_(~n() > 1) %>% nrow() == 0)
  }

  return(kierunki)
}