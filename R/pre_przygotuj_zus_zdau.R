#' Złącza dane ZUS z eskportów dla wielu roczników absolwentów w jeden zestaw
#' danych
#' @description OPI eksportuje oddzielny zbiór ZDAU dla każdego rocznika
#' absolwentów uwzględniając w nim informacje o innych kierunkach studiów
#' jedynie jeśli okres studiowania przypadał na ten sam rok, co rok uzyskania
#' dyplomu. Uniemożliwia to poprawne śledzenie kontynuacji nauki na innych
#' kierunkach studiów. Drugą niedogodnością jest wielokrotne przetwarzanie
#' informacji z ZUS dotyczących tych samych osób będących absolwentami (różnych
#' kierunków) w różnych rocznikach.
#'
#' Aby rozwiazać obydwie niedogodności należy złączyć zbiory opisujące
#' poszczególne roczniki absolwentów w jeden zestaw danych, do czego służy ta
#' funkcja.
#' @param katalog lokalizacja zawierająca katalogi z danymi dla poszczególnych
#'   roczników absolwentów (ich nazwy powinny sortować się w takiej kolejności,
#'   jak roczniki absolwentów)
#' @export
#' @import dplyr
pre_przygotuj_zus_zdau = function(katalog) {
  ## wczytaj dane
  zdau = list()
  zdu1 = list()
  zdu2 = list()
  zdu3 = list()
  zdu4 = list()
  for (i in list.dirs(katalog, TRUE, FALSE)) {
    if (i == katalog) {
      next
    }
    zdau[[length(zdau) + 1]] = readr::read_csv2(paste0(i, '/ZDAU.csv'), col_names = c('id', 'pesel', 'typ', 'cudz', 'n', 'uczelnia_id', 'jednostka_id', 'kierunek_id', 'ism', 'forma', 'poziom', 'profil', 'rok_od', 'mies_od', 'rok_do', 'mies_do'), col_types = 'iiciiiiiiccciiii') %>%
      mutate_(zbior = basename(i))
    zdu1[[length(zdu1) + 1]] = readr::read_csv2(paste0(i, '/ZDU1.csv'), col_names = c('id', 'a1', 'a2', 'a3', 'rok_ur', 'plec', 'kon_r', 'kon_m'), col_types = 'iiciicii') %>%
      mutate_(zbior = basename(i))
    zdu2[[length(zdu2) + 1]] = readr::read_csv2(paste0(i, '/ZDU2.csv'), col_names = c('id', 'data_od', 'data_do', 'pna_mel', 'pna_zam', 'pna_kor', 'zm_mel', 'zm_zam', 'zm_kor'), col_types = 'iccccciii') %>%
      mutate_(zbior = basename(i))
    zdu3[[length(zdu3) + 1]] = readr::read_csv2(paste0(i, '/ZDU3.csv'), col_names = c('id', 'id_platnika', 'okres', 'kod', 'chor', 'wyp', 'emer', 'zdr', 'limit', 'rsa'), col_types = 'iiciddddcc') %>%
      mutate_(zbior = basename(i))
    zdu4[[length(zdu4) + 1]] = readr::read_csv2(paste0(i, '/ZDU4.csv'), col_names = c('id_platnika', 'pkd', 'kon_r', 'kon_m'), col_types = 'icii') %>%
      mutate_(zbior = basename(i))
  }
  zdau = bind_rows(zdau)
  zdu1 = bind_rows(zdu1)
  zdu2 = bind_rows(zdu2)
  zdu3 = bind_rows(zdu3)
  zdu4 = bind_rows(zdu4)

  ## przekoduj identyfikatory
  noweId = zdau %>%
    select_('id', 'zbior') %>%
    distinct() %>%
    mutate_(id2 = ~if_else(nchar(id) >= 7, id, -(1:nrow(.))))
  save(noweId, file = paste0(katalog, '/mapowanieId.RData'))
  zdau = zdau %>% select_('id', 'zbior') %>% left_join(noweId) %>% select_('-id') %>% rename_(id = 'id2')
  tmp = zdu1 %>% select_('id', 'zbior') %>% left_join(noweId) %>% select_('-id') %>% rename_(id = 'id2')
  tmp = zdu2 %>% select_('id', 'zbior') %>% left_join(noweId) %>% select_('-id') %>% rename_(id = 'id2')
  tmp = zdu3 %>% select_('id', 'zbior') %>% left_join(noweId) %>% select_('-id') %>% rename_(id = 'id2')

  ## uporządkuj ZDAU

  # potraktuj rekordy o takim samym {kierunek_id, forma, rok_od, mies_od} jako jedne studia i sprowadź do jednego wpisu
  zdau = zdau %>%
    group_by_('id', 'kierunek_id', 'forma', 'rok_od', 'mies_od') %>%
    arrange_('id', 'kierunek_id', 'forma', 'rok_od', 'mies_od', 'typ', 'desc(rok_do)', 'desc(mies_do)') %>%
    filter_(~row_number(id) == 1)
  # złącz okresy ciągłej nauki na tym samym {kierunek_id, forma} w jeden rekord
  zdau = zdau %>%
    group_by_('id', 'kierunek_id', 'forma') %>%
    arrange_('id', 'kierunek_id', 'forma', 'rok_od', 'mies_od') %>%
    mutate_(zlacz = ~cumsum(coalesce(rok_od * 12L + mies_od - lag(rok_do * 12L + mies_do) > 1, FALSE))) %>%
    group_by_('id', 'kierunek_id', 'forma', 'rok_od', 'mies_od', 'zlacz') %>%
    mutate_(
      typ = ~last(typ),
      rok_do = ~last(rok_do),
      mies_do = ~last(mies_do)
    ) %>%
    filter_(~row_number(id) == 1) %>%
    ungroup() %>%
    select_('-zlacz')
  zdau = zdau %>%
    group_by_('id') %>%
    mutate_(n = ~n()) %>%
    ungroup() %>%
    select_('-zbior')

  ## sprawdź i złącz pozostałe zbiory

  zdu1 = zdu1 %>%
    select_('-zbior') %>%
    distinct()
  niespojnosci = zdu1 %>% group_by_('id') %>% summarize_(n = ~n()) %>% filter_(~n > 1) %>% nrow()
  if (niespojnosci > 0) {
    warning('niespójności w zbiorach ZDU1 dla ', niespojnosci, ' rekordów')
  }

  # nie sprawdzamy, bo nawet w zbiorach z jednego roku zdarzają się nieścisłości i przygotuj_zus() jest na to gotowe
  zdu2 = zdu2 %>%
    select_('-zbior') %>%
    distinct()

  niespojnosci = zdu3 %>%
    mutate_(skladka = ~paste(id_platnika, kod, chor, wyp, emer, zdr, limit, rsa)) %>%
    select_('zbior', 'id', 'okres', 'skladka') %>%
    group_by_('id', 'okres') %>%
    filter_(~n_distinct(zbior) > 1) %>%
    mutate_(l_zbiorow = ~n_distinct(zbior)) %>%
    group_by_('id', 'okres', 'skladka') %>%
    filter_(~n() != l_zbiorow)
  if (nrow(niespojnosci) > 1) {
    warning('niespójności w zbiorach ZDU3 dla ', nrow(niespojnosci), ' rekordów')
  }
  zdu3 = zdu3 %>%
    select_('-zbior') %>%
    distinct()

  niespojnosci = zdu4 %>% group_by_('id_platnika') %>% summarize_all(n_distinct) %>% filter_(~pkd + kon_r + kon_m > 3) %>% nrow()
  if (niespojnosci > 0) {
    warning('niespójności w zbiorach ZDU4 dla ', niespojnosci, ' rekordów')
  }
  zdu4 = zdu4 %>%
    group_by_('id_platnika') %>%
    arrange_('id_platnika', 'zbior') %>%
    summarize_all(last) %>%
    select_('-zbior')

  ## zapisz
  utils::write.table(zdau, paste0(katalog, '/ZDAU.csv'), sep = ';', dec = '.', na = '', row.names = FALSE, col.names = FALSE)
  utils::write.table(zdu1, paste0(katalog, '/ZDU1.csv'), sep = ';', dec = '.', na = '', row.names = FALSE, col.names = FALSE)
  utils::write.table(zdu2, paste0(katalog, '/ZDU2.csv'), sep = ';', dec = '.', na = '', row.names = FALSE, col.names = FALSE)
  utils::write.table(zdu3, paste0(katalog, '/ZDU3.csv'), sep = ';', dec = '.', na = '', row.names = FALSE, col.names = FALSE)
  utils::write.table(zdu4, paste0(katalog, '/ZDU4.csv'), sep = ';', dec = '.', na = '', row.names = FALSE, col.names = FALSE)
}