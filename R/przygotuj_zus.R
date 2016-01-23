#' Przygotowuje dane z ZUS
#' @description
#' Do danych o składkach dołączane są dane o miejscu zamieszkania, płatniku,
#' tytułach ubezpieczeń, itp.
#' @param dataMin pierwszy uwzględniany okres składkowy
#' @param dataMax ostatni uwzględniany okres składkowy
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu
#'   multidplyr
#' @export
#' @import dplyr
przygotuj_zus = function(dataMin, dataMax, multidplyr = TRUE){
  zus_tytuly_ubezp = openxlsx::readWorkbook('dane/ZUS_tytuly_ubezp.xlsx')[-1, ] %>%
    select_('-OPIS', '-OD', '-DO', '-ZAGRANIC', '-CUDZOZ') %>%
    rename_(id_tytulu = 'KOD') %>%
    mutate_each(funs_('as.numeric'))
  colnames(zus_tytuly_ubezp) = tolower(colnames(zus_tytuly_ubezp))

  # dane ewidencyjne osoby
  zdu1 = read.csv2('dane/ZDU1.csv', header = F, fileEncoding = 'Windows-1250', stringsAsFactors = FALSE)[, c(1, 5:8)]
  colnames(zdu1) = c('id', 'rok_ur', 'plec', 'koniec_r', 'koniec_m')

  # dane adresowe w poszczególnych okresach
  zdu2 = read.csv2('dane/ZDU2.csv', header = F, fileEncoding = 'Windows-1250', stringsAsFactors = FALSE)
  colnames(zdu2) = c('id', 'data_od', 'data_do', 'pna_mel', 'pna_zam', 'pna_kor', 'zm_mel', 'zm_zam', 'zm_kor')
  zdu2 = zdu2 %>%
    mutate_(
      data_od = ~ as.Date(data_od),
      data_do = ~ as.Date(ifelse(data_do == '', dataMax, data_do))
    )
  zdu2$data_do[zdu2$data_do > Sys.Date()] = Sys.Date()
  zdu2 = bind_rows(
    zdu2 %>% select_('id', 'data_od', 'data_do', 'pna_mel') %>% rename_(pna = 'pna_mel') %>% mutate_(pna_typ = '3', pna = ~suppressWarnings(as.numeric(pna))),
    zdu2 %>% select_('id', 'data_od', 'data_do', 'pna_zam') %>% rename_(pna = 'pna_zam') %>% mutate_(pna_typ = '2', pna = ~suppressWarnings(as.numeric(pna))),
    zdu2 %>% select_('id', 'data_od', 'data_do', 'pna_kor') %>% rename_(pna = 'pna_kor') %>% mutate_(pna_typ = '1', pna = ~suppressWarnings(as.numeric(pna)))
  ) %>%
    filter_(~!is.na(pna)) %>%
    group_by_('id', 'data_od', 'data_do') %>%
    arrange_('pna_typ') %>%
    summarize_(pna = ~first(pna)) %>%
    ungroup() %>%
    mutate_(
      pna = ~ifelse(is.na(pna), 0, pna),
      data_od = ~ substring(as.character(data_od), 1, 7),
      data_do = ~ substring(as.character(data_do), 1, 7)
    ) %>%
    collect()

  # dane z rozliczeń
  zdu3 = read.csv2('dane/ZDU3.csv', header = F, fileEncoding = 'Windows-1250', stringsAsFactors = FALSE)
  colnames(zdu3) = c('id', 'id_platnika', 'okres', 'id_tytulu', 'podst_chor', 'podst_wyp', 'podst_em', 'podst_zdr', 'limit', 'rsa')
  zdu3 = zdu3 %>%
    filter_(~ okres >= substr(dataMin, 1, 7) & okres <= substr(dataMax, 1, 7)) %>%
    mutate_(id_zdu3 = ~ row_number())
  # zdu3 %>% group_by(id, okres) %>% summarize(n = n()) %>% group_by(n) %>% summarize(nn = n()) %>% arrange(nn)
  # zdu3 %>% group_by(id, id_platnika, okres) %>% summarize(n = n()) %>% group_by(n) %>% summarize(nn = n()) %>% arrange(nn)
  # zdu3 %>% group_by(id, id_platnika, okres, id_tytulu) %>% summarize(n = n()) %>% group_by(n) %>% summarize(nn = n()) %>% arrange(nn)
  zdu3tmp = zdu3 %>%
    select_('id_zdu3', 'id', 'id_platnika', 'okres', 'id_tytulu')
  zdu3 = bind_rows(
    zdu3 %>% select_('id_zdu3', 'id_platnika', 'okres', 'id_tytulu', 'podst_chor') %>% rename_(podst = 'podst_chor') %>% mutate_(podst_typ = '"chorobowe"'),
    zdu3 %>% select_('id_zdu3', 'podst_wyp') %>% rename_(podst = 'podst_wyp') %>% mutate_(podst_typ = '"wypadkowe"'),
    zdu3 %>% select_('id_zdu3', 'podst_em') %>% rename_(podst = 'podst_em') %>% mutate_(podst_typ = '"emerytalne"'),
    zdu3 %>% select_('id_zdu3', 'podst_zdr') %>% rename_(podst = 'podst_zdr') %>% mutate_(podst_typ = '"zdrowotne"')
  )
  if(multidplyr){
    zdu3 = multidplyr::partition(zdu3, id_zdu3)
  }else{
    zdu3 = group_by_(zdu3, 'id_zdu3')
  }
  zdu3 = zdu3 %>%
    summarize_(podst = ~ max(podst, na.rm = TRUE)) %>%
    collect() %>%
    inner_join(zdu3tmp)
  rm(zdu3tmp)
  # zdu3 %>% group_by(id, okres) %>% summarize(n = n()) %>% group_by(n) %>% summarize(nn = n())

  # dane płatników
  zdu4 = read.csv2('dane/ZDU4.csv', header = F, fileEncoding = 'Windows-1250', stringsAsFactors = FALSE)
  colnames(zdu4) = c('id_platnika', 'pkd', 'platnik_koniec_r', 'platnik_koniec_m')
  zdu4 = zdu4 %>%
    mutate_(
      platnik_kon = ~ sprintf('%04d-%02d', platnik_koniec_r, platnik_koniec_m)
    ) %>%
    mutate_(
      platnik_kon = ~ ifelse(platnik_kon > dataMax, NA, platnik_kon)
    )

  zus = zdu3 %>%
    left_join(zdu2) %>%
    arrange_('data_od')
  if(multidplyr){
    zus = multidplyr::partition(zus, id_zdu3)
  }else{
    zus = group_by_('id_zdu3')
  }
  zus = zus %>%
    filter_(~ data_do >= okres | okres > max(data_do) & data_do == max(data_do) | is.na(pna)) %>%
    filter_(~ row_number() == 1) %>%
    collect() %>%
    ungroup() %>%
    select_('-data_od', '-data_do') %>%
    distinct()
  stopifnot(
    # {id, id_platnika, okres, id_tytulu} nie jest unikalne!
    zus %>% select_('id', 'okres', 'pna') %>% distinct() %>% nrow() == zus %>% select_('id', 'okres') %>% distinct() %>% nrow(), # unikalność {id, okres, pna}
    nrow(zus) == nrow(zdu3), # nie zgubiliśmy żadnego okresu składkowego,
    all(!is.na(zus$id_platnika)) # wszyscy płatnicy są znani
  )
  #zdu3 %>% anti_join(zus %>% select(id, id_platnika, okres, id_tytulu)) %>% head()

  zus = zus %>%
    left_join(zdu4 %>% select_('id_platnika', 'pkd', 'platnik_kon')) %>%
    left_join(zdu1 %>% select_('id', 'rok_ur', 'plec')) %>%
    left_join(zus_tytuly_ubezp) %>%
    mutate_(
      okres       = ~ data2okres(okres),
      platnik_kon = ~ data2okres(platnik_kon)
    )

  class(zus) = c('zus_df', class(zus))
  return(zus)
}