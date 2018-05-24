devtools::load_all(".")
library(dplyr)
library(sparklyr)
options(scipen = 100)

dataMin = '2014-01-01'
dataMax = '2017-09-30'  # 2015-03-31 / 2015-09-30 / 2016-09-30 / 2017-09-30
katZr = 'dane/ZUS_2017-09/'
okienka = list(
  okienko(  1, 12, 'data_do', 'data_do', '_p1', dataMin, dataMax),
  okienko( 13, 24, 'data_do', 'data_do', '_p2', dataMin, dataMax),
  okienko( 25, 36, 'data_do', 'data_do', '_p3', dataMin, dataMax),
  okienko(  1, 99, 'data_do', 'data_do', ''   , dataMin, dataMax)
)
probka = 1
pominCache = FALSE

sc = spark_connect(
  master = 'local',
  config = list(
    sparklyr.cores.local = 7, 'sparklyr.shell.driver-memory' = '30G', spark.memory.fraction = 0.8, spark.executor.memory = '8G',
    spark.local.dir = '/home/zozlak/roboty/Jasiński/MLAKdane/cache/',
    spark.sql.crossJoin.enabled = 'true'
  )
)

####################
# 1. Przygotowanie danych ZUS i GUS
####################
plikCache = nazwa_pliku('utrataPracy', '.csv')
if (!file.exists(plikCache) | pominCache) {
  zdau = przygotuj_zdau(katZr, probka)
  zapisz_dla_sparka(zdau, 'zdau')
  pnaPowiaty = przygotuj_pna_powiaty_mb(dataMin, dataMax)
  zapisz_dla_sparka(pnaPowiaty, 'pnaPowiaty')
  zus = przygotuj_zus(katZr, dataMin, dataMax, pnaPowiaty)
  zapisz_dla_sparka(zus, 'zus')
  utrataPracy = przygotuj_utrata_pracy(zus, dataMax)
  zapisz_dla_sparka(utrataPracy, 'utrataPracy')
}
zdau = wczytaj_do_sparka(sc, 'zdau')
pnaPowiaty = wczytaj_do_sparka(sc, 'pnaPowiaty')
utrataPracy = wczytaj_do_sparka(sc, 'utrataPracy')

####################
# 2. Przygotowujemy zbiory potrzebne do wyliczenia okienek
####################
plikCache = nazwa_pliku('miesieczne', '.csv')
if (!file.exists(plikCache) | pominCache) {
  zus = wczytaj_do_sparka(sc, 'zus')

  baza = polacz_zus_zdau(zus, zdau, pnaPowiaty, dataMin, dataMax)
  zapisz_ze_sparka(baza, 'baza')

  miesieczne = agreguj_do_miesiecy(baza, zdau)
  zapisz_ze_sparka(miesieczne, 'miesieczne')
}
baza = wczytaj_do_sparka(sc, 'baza')
miesieczne = wczytaj_do_sparka(sc, 'miesieczne')

####################
# 3. Wyliczamy zmienne w poszczególnych okienkach czasu
####################
jednostki = przygotuj_jednostki(katZr)
#jednostki = przygotuj_jednostki_old(katZr, 2015) %>% mutate(x = 1L) %>% inner_join(data_frame(rok = 2014L:2016L, x = c(1L, 1L, 1L))) %>% select(-x)
zdauAbs = zdau %>%
  filter_(~typ == 'A') %>%
  select_('id_zdau')
for (i in seq_along(okienka)) {
  okienko = okienka[[i]]
  okienkoMies = oblicz_okienko(miesieczne, okienko)
  okienkoBaza = oblicz_okienko(baza, okienko)

  abs = agreguj_do_okresu(okienkoMies)
  up = oblicz_utrata_etatu(okienkoMies, utrataPracy)
  np = oblicz_pracodawcy(okienkoBaza)

  zam0 = zam1 = zdauAbs
  if (okienko[['offsetMax']] == 99) {
    zam0  = oblicz_zamieszkanie(okienkoBaza, jednostki, TRUE) %>%
      rename_all(~sub('^id_zdau.*$', 'id_zdau', paste0(.x, '0')))
    zam1  = oblicz_zamieszkanie(okienkoBaza, jednostki, FALSE) %>%
      rename_all(~sub('^id_zdau.*$', 'id_zdau', paste0(.x, '1')))
  }

  razem = zdauAbs %>%
    full_join(abs) %>%
    full_join(np) %>%
    full_join(up) %>%
    mutate_(len = ~coalesce(len, 0L)) %>%
    rename_all(~sub('^id_zdau.*$', 'id_zdau', paste0(.x, okienko[['sufiks']]))) %>%
    full_join(zam0, copy = TRUE) %>%
    full_join(zam1, copy = TRUE) %>%
    collect()

  n1 = razem %>%
    summarize_(
      nz = ~n_distinct(id_zdau),
      n = ~n()
    )
  stopifnot(
    zdauAbs %>% summarize_(n = ~n()) %>% collect() == n1$n,
    n1$nz == n1$n
  )
  save(razem, file = nazwa_pliku(paste0('razem_', i), '.RData'), compress = TRUE)
  # zapisz_dla_sparka(razem, paste0('razem_', i))
}

####################
# 4. Wyliczamy zmienne niezależne od okienka czasu (STUDYP*, TP_*, *_K, *_R, itp.)
####################

kierunki = przygotuj_kierunki(katZr, FALSE)
#kierunki = przygotuj_kierunki_old(katZr) %>% mutate(rok = 2015)
plikCache = nazwa_pliku('stale', '.RData')
if (!file.exists(plikCache) | pominCache) {
  studyp = oblicz_studyp(zdau, kierunki)
  czas = oblicz_zmienne_czasowe(baza, utrataPracy)
  stale = oblicz_stale(baza, zdau)
  save(studyp, czas, stale, file = plikCache, compress = TRUE)
} else {
  load(plikCache)
}

##########
# Złączamy wszystko, cośmy policzyli i zapisujemy
kierunki = przygotuj_kierunki(katZr, TRUE)
wszystko = zdau %>%
  collect() %>%
  mutate_(rok = ~okres2rok(data_do)) %>%
  oblicz_stale_czasowe(dataMax) %>%
  filter_(~typ %in% 'A') %>%
  full_join(studyp) %>%
  full_join(czas) %>%
  full_join(stale) %>%
  left_join(kierunki %>% select_('rok', 'kierunek_id', 'jednostka_id', 'kierunek', 'obsz_kod', 'obsz', 'dzie_kod', 'dzie', 'dysc_kod', 'dysc')) %>%
  left_join(przygotuj_jednostki(katZr) %>% select('rok', 'jednostka_id', 'jednostka_nazwa')) %>%
  left_join(przygotuj_uczelnie(katZr) %>% select_('uczelnia_id', 'uczelnia_nazwa')) %>%
  rename_(kieruneknazwa = 'kierunek_nazwa_pelna', kierunek = 'kierunek_id', uczelnia = 'uczelnia_id', uczelnianazwa = 'uczelnia_nazwa')
for (i in seq_along(okienka)) {
  load(nazwa_pliku(paste0('razem_', i), '.RData'))
  wszystko = full_join(wszystko, razem)
  rm(razem)
}
stopifnot(
  nrow(wszystko) == zdauAbs %>% collect() %>% nrow()
)
colnames(wszystko) = toupper(colnames(wszystko))
save(wszystko, file = nazwa_pliku('dane', '.RData'), compress = TRUE)
tmp = wszystko
for (r in unique(wszystko$ROK)) {
  wszystko = tmp %>% filter(ROK == r)
  save(wszystko, file = nazwa_pliku('dane', '.RData', katZr, r), compress = TRUE)
}
rm(tmp, wszystko)

# load(nazwa_pliku('dane', '.RData'))
# wszystko2 = wszystko
# load('dane/ZUS_2016-09/2015/2015_dane.RData')
# wszystko1 = wszystko
# d = porownaj_zbiory(wszystko1, wszystko2)

##########
# Zbiór danych miesięcznych
okienkoMies = oblicz_okienko(miesieczne, okienko(-60, 60, 'data_do', 'data_do', '', dataMin, dataMax)) %>%
  filter(okres >= okres_min & okres <= okres_max) %>%
  mutate(
    rok = as.integer((data_do - 1) / 12),
    okres = printf('%04d-%02d', as.integer((okres - 1) / 12), if_else(okres %% 12L == 0L, 12L, okres %% 12L))
  ) %>%
  select(rok, id_zdau, id, okres, if_x_s, if_x_stprg, wzg_ez_e, wzg_ez_z, wzg_ryzbez, ez_z, ez_e, if_p, if_e, if_s, status)
zapisz_ze_sparka(okienkoMies, 'daneMies')

tmp = read_csv_spark(nazwa_pliku('daneMies', '.csv'), 'iiiciidddddiiid')
names(tmp) = toupper(paste0(names(tmp), '_M'))
for (r in unique(tmp$ROK)) {
  miesieczne = tmp %>% filter(ROK_M == r)
  save(miesieczne, file = nazwa_pliku('dane_mies', '.RData', katZr, r), compress = TRUE)
}
rm(okienkoMies)

##########
# Zbiór danych kwartalnych (+0, +3, +6, itd. miesiąc od dyplomu)
kwartalne = miesieczne %>%
  filter(okres >= data_do & (okres - data_do) %% 3L == 0L) %>%
  mutate(
    rok = as.integer((data_do - 1) / 12),
    kwartal = as.integer((okres - data_do) / 3L),
    data = printf('%04d-%02d', as.integer((okres - 1) / 12), if_else(okres %% 12L == 0L, 12L, okres %% 12L)),
    nm_es = if_else(nm_e > 0L | nm_s > 0L, 1L, 0L)
  ) %>%
  left_join(
    baza %>%
      select_('id_zdau', 'okres', 'klaszam2') %>%
      group_by_('id_zdau', 'okres') %>%
      summarize_(klasz = ~min(klaszam2, na.rm = TRUE))
  )
zapisz_ze_sparka(kwartalne, 'daneKwart')

kwartalne = read_csv_spark(nazwa_pliku('daneKwart', '.csv'), 'iiciidddddiiii')
names(kwartalne) = toupper(names(kwartalne))
rm(kwartalne)

##########
# PKD
load(nazwa_pliku('baza', 'cache', rocznik))
load(nazwa_pliku('stale', 'cache', rocznik))

pkdKwartalne = agreguj_pkd(baza, 3)
pkdPoDyplomie = agreguj_pkd(baza, 99) %>% select(-kwartal)
pkdAbsolwenci = zdau %>%
  filter_(~typ == 'A') %>%
  select_('id_zdau', 'uczelnia_id', 'jednostka_id', 'kierunek_id', 'forma', 'poziom') %>%
  left_join(przygotuj_kierunki(katZr, c())) %>%
  left_join(stale)
names(pkdKwartalne) = toupper(names(pkdKwartalne))
names(pkdAbsolwenci) = toupper(names(pkdAbsolwenci))
names(pkdPoDyplomie) = toupper(names(pkdPoDyplomie))
save(pkdAbsolwenci, pkdKwartalne, pkdPoDyplomie, file = nazwa_pliku('pkd', katZr, rocznik), compress = TRUE)
