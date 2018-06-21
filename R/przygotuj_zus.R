#' Przygotowuje dane z ZUS
#' @description
#' Do danych o składkach dołączane są dane o miejscu zamieszkania, płatniku,
#' tytułach ubezpieczeń, itp.
#' @param katZr katalog, w którym znajduje się plik ZDUx.csv
#' @param dataMin pierwszy uwzględniany okres składkowy
#' @param dataMax ostatni uwzględniany okres składkowy
#' @param pna ramka danych z dozwolonymi kodami pna
#' @param polSparka połączenie ze Sparkiem
#' @param cacheTestow czy cache-ować dane przed testami poprawności (jeśli tylko
#'   dane mieszczą się w RAM-ie, znacznie przyspiesza wykonywani testów
#'   poprawności)
#' @export
#' @import dplyr
#' @import readr
przygotuj_zus = function(katZr, dataMin, dataMax, pna, polSparka, cacheTestow = TRUE){
  zus_tytuly_ubezp = openxlsx::readWorkbook('dane/ZUS_tytuly_ubezp.xlsx')[-1, ] %>%
    select_('-OPIS', '-OD', '-DO', '-ZAGRANIC', '-CUDZOZ') %>%
    rename_(id_tytulu = 'KOD') %>%
    mutate_all(funs_('as.integer'))
  colnames(zus_tytuly_ubezp) = tolower(colnames(zus_tytuly_ubezp))

  # dane ewidencyjne osoby
  zdu1 = przygotuj_zdu1(katZr)

  # dane adresowe w poszczególnych okresach
  kolZdu2 = c('id', 'data_od', 'data_do', 'pna_mel', 'pna_zam', 'pna_kor', 'zm_mel', 'zm_zam', 'zm_kor')
  sciezka = paste0('file://', if_else(grepl('^/', katZr), katZr, paste0(getwd(), '/', katZr)), '/ZDU2.csv')
  zdu2 = sparklyr::spark_read_csv(polSparka, 'zdu2', sciezka, header = FALSE, columns = kolZdu2, delimiter = ';')
  zdu2 = zdu2 %>%
    mutate_(
      data_do = ~substr(coalesce(if_else(data_do <= dataMax, data_do, dataMax), data_do, dataMax), 1, 10),
      pna_mel = ~as.integer(pna_mel),
      pna_zam = ~as.integer(pna_zam),
      pna_kor = ~as.integer(pna_kor)
    )
  zdu2 = sparklyr::sdf_bind_rows(
    zdu2 %>% select_('id', 'data_od', 'data_do', 'pna_mel') %>% rename_(pna = 'pna_mel') %>% filter_(~!is.na(pna)) %>% mutate_(pna_typ = 3L),
    zdu2 %>% select_('id', 'data_od', 'data_do', 'pna_zam') %>% rename_(pna = 'pna_zam') %>% filter_(~!is.na(pna)) %>% mutate_(pna_typ = 2L),
    zdu2 %>% select_('id', 'data_od', 'data_do', 'pna_kor') %>% rename_(pna = 'pna_kor') %>% filter_(~!is.na(pna)) %>% mutate_(pna_typ = 1L)
  ) %>%
    group_by_('id', 'data_od', 'data_do') %>%
    filter_(~row_number(pna_typ) == 1) %>%
    ungroup() %>%
    select_('-pna_typ') %>%
    mutate_(
      data_od = ~ substring(as.character(data_od), 1, 7),
      data_do = ~ substring(as.character(data_do), 1, 7)
    )

  # dane z rozliczeń
  kolZdu3 = c('id', 'id_platnika', 'okres', 'id_tytulu', 'podst_chor', 'podst_wyp', 'podst_em', 'podst_zdr', 'limit', 'rsa')
  sciezka = paste0('file://', if_else(grepl('^/', katZr), katZr, paste0(getwd(), '/', katZr)), '/ZDU3.csv')
  zdu3 = sparklyr::spark_read_csv(polSparka, 'zdu3', sciezka, header = FALSE, columns = kolZdu3, delimiter = ';')
  zdu3 = zdu3 %>%
    filter_(~okres >= substr(dataMin, 1, 7) & okres <= substr(dataMax, 1, 7)) %>%
    mutate_(
      id_zdu3 = ~row_number(id)
    )
  # zdu3 %>% group_by(id, okres) %>% summarize(n = n()) %>% group_by(n) %>% summarize(nn = n()) %>% arrange(nn)
  # zdu3 %>% group_by(id, id_platnika, okres) %>% summarize(n = n()) %>% group_by(n) %>% summarize(nn = n()) %>% arrange(nn)
  # zdu3 %>% group_by(id, id_platnika, okres, id_tytulu) %>% summarize(n = n()) %>% group_by(n) %>% summarize(nn = n()) %>% arrange(nn)
  zdu3tmp = zdu3 %>%
    select_('id_zdu3', 'id', 'id_platnika', 'okres', 'id_tytulu')
  zdu3 = sparklyr::sdf_bind_rows(
    zdu3 %>% select_('id_zdu3', 'podst_chor') %>% rename_(podst = 'podst_chor') %>% mutate_(podst_typ = '"chorobowe"'),
    zdu3 %>% select_('id_zdu3', 'podst_wyp') %>% rename_(podst = 'podst_wyp') %>% mutate_(podst_typ = '"wypadkowe"'),
    zdu3 %>% select_('id_zdu3', 'podst_em') %>% rename_(podst = 'podst_em') %>% mutate_(podst_typ = '"emerytalne"'),
    zdu3 %>% select_('id_zdu3', 'podst_zdr') %>% rename_(podst = 'podst_zdr') %>% mutate_(podst_typ = '"zdrowotne"')
  ) %>%
    group_by_('id_zdu3') %>%
    mutate_(podst = ~as.numeric(regexp_replace(podst, ',', '.'))) %>%
    summarize_(podst = ~max(podst, na.rm = TRUE)) %>%
    inner_join(zdu3tmp)
  # zdu3 %>% group_by(id, okres) %>% summarize(n = n()) %>% group_by(n) %>% summarize(nn = n())

  # dane płatników
  kolZdu4 = c('id_platnika', 'pkd', 'platnik_koniec_r', 'platnik_koniec_m')
  sciezka = paste0('file://', if_else(grepl('^/', katZr), katZr, paste0(getwd(), '/', katZr)), '/ZDU4.csv')
  zdu4 = sparklyr::spark_read_csv(polSparka, 'zdu4', sciezka, header = FALSE, columns = kolZdu4, delimiter = ';')
  zdu4 = zdu4 %>%
    mutate_(
      platnik_kon = ~as.character(as.Date(paste0(platnik_koniec_r, '-', platnik_koniec_m, '-01')))
    ) %>%
    mutate_(
      platnik_kon = ~substr(if_else(platnik_kon > dataMax, NA_character_, platnik_kon), 1, 7)
    )

  zusA = zdu3 %>%
    left_join(zdu2) %>%
    group_by_('id_zdu3') %>%
    filter_(~data_do >= okres | okres > max(data_do, na.rm = TRUE) & data_do == max(data_do, na.rm = TRUE) | is.na(pna)) %>%
    # jedyny sens uwzględnienia "pna" w filtrze poniżej to zapewnienie stabilnych wyników, mniejszy kod pocztowy nie jest oczywiście w żaden sposób "lepszy" od większego
    arrange_('data_od', 'pna') %>%
    filter_(~row_number() == 1) %>%
    ungroup() %>%
    select_('-data_od', '-data_do')
  if (cacheTestow) {
    zusA = sparklyr::sdf_register(zusA, 'zus')
    tbl_cache(polSparka, 'zus')
  }
  # {id, id_platnika, okres, id_tytulu} nie jest unikalne!
  stopifnot(
    # unikalność {id, okres, pna}
    unlist(zusA %>% select_('id', 'okres', 'pna') %>% distinct() %>% summarize(n = n()) %>% collect()) == unlist(zusA %>% select_('id', 'okres') %>% distinct() %>% summarize(n = n()) %>% collect()),
    # nie zgubiliśmy żadnego okresu składkowego
    unlist(zusA %>% summarize(n = n()) %>% collect()) == unlist(zdu3 %>% summarize(n = n()) %>% collect()),
    # wszyscy płatnicy są znani
    unlist(zusA %>% filter(is.na(id_platnika)) %>% summarise(n = n()) %>% collect()) == 0
  )
  #zdu3 %>% anti_join(zus %>% select(id, id_platnika, okres, id_tytulu)) %>% head()

  zusB = zusA %>%
    left_join(zdu4 %>% select_('id_platnika', 'pkd', 'platnik_kon')) %>%
    left_join(zdu1 %>% select_('id', 'koniec'), copy = TRUE) %>%
    left_join(zus_tytuly_ubezp, copy = TRUE) %>%
    mutate_(
      okres       = ~as.integer(substr(okres, 1, 4)) * 12L + as.integer(substr(okres, 6, 7)),
      platnik_kon = ~as.integer(substr(platnik_kon, 1, 4)) * 12L + as.integer(substr(platnik_kon, 6, 7)),
      rok         = ~as.integer(substr(okres, 1, 4)),
      id_platnika = ~if_else(samoz > 0L, -1L , id_platnika), # zmiana firmy na samozatrudnieniu nie jest dla nas zmiana platnika
      pna         = ~coalesce(pna, -1L)
    )
  stopifnot(
    unlist(zusB %>% filter_(~is.na(etat)) %>% summarise(n = n()) %>% collect()) == 0
  )

  # weryfikacja pna
  zusC = zusB %>%
    left_join(
      pna %>%
        select_('pna') %>%
        distinct() %>%
        mutate_(popr_pna = TRUE),
      copy = TRUE
    )
  test = zusC %>% filter(is.na(popr_pna)) %>% select_('pna') %>% collect()
  if (nrow(test) > 0) {
    test = test %>% distinct() %>% arrange_('pna')
    warning('Błędne kody pocztowe w ', nrow(test), ' rekordach: ', paste0(test$pna, collapse = ', '))
  }
  zusD = zusC %>%
    mutate_(
      pna = ~if_else(!is.na(popr_pna), pna, -1L)
    ) %>%
    select_('-popr_pna')

  return(zusD)
}
