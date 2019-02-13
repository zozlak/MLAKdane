#' Przygotowuje słownik kierunków
#' @description
#' Zwraca słownik kierunków studiów
#' @param katZr katalog, w którym znajduje się plik KIERUNKI.xlsx
#' @param agregujDoKierunku czy agregować dane tak by {rok, kierunek_id} były
#'   unikalne (jeśli FALSE, agregacja nastapi do unikalności na poziomie {rok,
#'   kierunek_id, jednostka_id})
#' @param jednostki ramka danych opisująca jednostki (gdy NULL, zostanie
#'   wczytana automatycznie)
#' @param zmiennaRok nazwa zmiennej z rokiem rekordu
#' @return [data.frame] ramka danych opisująca kierunki studiów
#' @export
#' @import dplyr
przygotuj_kierunki = function(katZr, agregujDoKierunku = TRUE, jednostki = NULL, zmiennaRok = 'rok'){
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
      dysc_nna = ~as.integer(!is.na(dyscyplina_kod)),
      data_od =  ~if_else(data_od == min(data_od, na.rm = TRUE), '1900-01-01', data_od),
      data_do =  ~if_else(data_do == max(data_do, na.rm = TRUE), as.character(Sys.Date()), data_do)
    ) %>%
    inner_join(lata) %>%
    group_by_('rok', 'kierunek_id') %>%
    arrange_('rok', 'kierunek_id', 'desc(dysc_nna)') %>%
    mutate_(
      f = ~sum(data_od <= f_do & data_do >= f_od),
      n = ~row_number(kierunek_id)
    ) %>%
    ungroup() %>%
    filter_(~data_od <= f_do & data_do >= f_od | f == 0 & n == 1) %>%
    select_('-f', '-n')
    #-->
  # przypiszmy obsz/dzie/dysc jeśli tylko się da
  kierunki = kierunki %>%
    rename_(
      obsz_kod = 'obszar_kod',
      obsz     = 'obszar_nazwa',
      dzie_kod = 'dziedzina_kod',
      dzie     = 'dziedzina_nazwa',
      dysc_kod = 'dyscyplina_kod',
      dysc     = 'dyscyplina_nazwa'
    ) %>%
    mutate_(
      obsz_kod = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', obsz_kod))),
      dzie_kod = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', dzie_kod))),
      dysc_kod = ~suppressWarnings(as.integer(sub('^[A-Z]+', '', dysc_kod)))
    ) %>%
    group_by_('kierunek_id') %>%
    arrange_('kierunek_id', 'data_od', 'data_do', 'desc(dysc_nna)') %>%
    mutate_(x = ~cumsum(dysc_nna)) %>%
    mutate_(y = ~x - dysc_nna) %>%
    group_by_('kierunek_id', 'y') %>%
    mutate_(
      obsz_kod = ~last(obsz_kod),
      obsz     = ~last(obsz),
      dzie_kod = ~last(dzie_kod),
      dzie     = ~last(dzie),
      dysc_kod = ~last(dysc_kod),
      dysc     = ~last(dysc)
    ) %>%
    group_by_('kierunek_id', 'x') %>%
    mutate_(
      obsz_kod          = ~coalesce(obsz_kod, first(obsz_kod)),
      obsz              = ~coalesce(obsz, first(obsz)),
      dzie_kod          = ~coalesce(dzie_kod, first(dzie_kod)),
      dzie              = ~coalesce(dzie, first(dzie)),
      dysc_kod          = ~coalesce(dysc_kod, first(dysc_kod)),
      dysc              = ~coalesce(dysc, first(dysc))
    ) %>%
    select_('-x', '-y')

  #-->
  if (agregujDoKierunku) {
    kierunki = kierunki %>%
      group_by_('rok', 'kierunek_id') %>%
      arrange_('desc(data_od)', 'desc(data_do)') %>%
      summarize_(
        miedzywydz        = ~as.integer(n_distinct(jednostka_id) > 1), # musi być na początku, aby brał właściwą "jednostka_id"
        jednostka_id      = ~first(jednostka_id),
        kierunek_nazwa    = ~paste0(unique(nazwa), collapse = '/'),
        poziom            = ~first(poziom_ksz),
        forma_ksztalcenia = ~if_else(n_distinct(formy_ksztalcenia) > 1, 'stacjonarne_i_niestacjonarne', first(formy_ksztalcenia)),
        obsz_kod          = ~first(obsz_kod),
        obsz              = ~first(obsz),
        dzie_kod          = ~first(dzie_kod),
        dzie              = ~first(dzie),
        dysc_kod          = ~first(dysc_kod),
        dysc              = ~first(dysc)
      )
  } else {
    kierunki = kierunki %>%
      group_by_('rok', 'kierunek_id', 'jednostka_id') %>%
      arrange_('desc(data_od)', 'desc(data_do)') %>%
      summarize_(
        miedzywydz        = ~as.integer(n_distinct(jednostka_id) > 1),
        kierunek_nazwa    = ~paste0(unique(nazwa), collapse = '/'),
        poziom            = ~first(poziom_ksz),
        forma_ksztalcenia = ~if_else(n_distinct(formy_ksztalcenia) > 1, 'stacjonarne_i_niestacjonarne', first(formy_ksztalcenia)),
        obsz_kod          = ~first(obsz_kod),
        obsz              = ~first(obsz),
        dzie_kod          = ~first(dzie_kod),
        dzie              = ~first(dzie),
        dysc_kod          = ~first(dysc_kod),
        dysc              = ~first(dysc)
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
      dysc_kod = ~as.integer(sub('^([0-9])0([0-9]{3})$', '\\1\\2', dysc_kod)),
      kierunek_nazwa = ~gsub('"', '', kierunek_nazwa)
    ) %>%
    left_join(jednostki %>% select_('jednostka_id', 'rok', 'jednostka_nazwa')) %>%
    mutate_(
      kierunek_nazwa_pelna = ~sub('^.*; ?', '', paste0(if_else(miedzywydz > 0L, 'Studia międzywydziałowe', jednostka_nazwa), ', ', kierunek_nazwa, ', ', forma_ksztalcenia, ' (POLon: ', kierunek_id, ')'))
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

  return(kierunki %>% rename_(.dots = stats::setNames(list('rok'), zmiennaRok)))
}