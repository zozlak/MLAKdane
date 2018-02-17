devtools::load_all(".")
library(dplyr)
options(scipen = 100)

dataMin = '2014-01-01'
dataMax = '2016-09-30'  # 2015-03-31 / 2015-09-30 / 2016-09-30
rocznik = 2015          # 2014 / 2015
katZr = 'dane/ZUS_2016-09/2015/'
okienka = list(
  okienko(  1, 12, 'data_do', 'data_do', '_p1', dataMin, dataMax),
#  okienko( 13, 24, 'data_do', 'data_do', '_p2', dataMin, dataMax),
  okienko(  1, 99, 'data_do', 'data_do', ''   , dataMin, dataMax)
)
kierZmDod = c()
probka = 1
krok = 100000
pominCache = FALSE

####################
# 1. Przygotowanie danych ZUS i GUS
####################
plikCache = nazwa_pliku('ZUS', 'cache', rocznik)
if (!file.exists(plikCache) | pominCache) {
  # pnaPowiaty = polacz_pna_powiaty(przygotuj_pna(), przygotuj_powiaty(), dataMin, dataMax)
  pnaPowiaty = przygotuj_pna_powiaty_mb(dataMin, dataMax)
  zus = przygotuj_zus(katZr, dataMin, dataMax, pnaPowiaty)
  utrataPracy = przygotuj_utrata_pracy(zus, dataMax)
  save(zus, utrataPracy, pnaPowiaty, file = plikCache, compress = TRUE)
} else {
  load(plikCache)
}

####################
# 2. Przygotowujemy zbiory potrzebne do wyliczenia okienek
####################
plikCache = nazwa_pliku('baza', 'cache', rocznik)
if (!file.exists(plikCache) | pominCache) {
  zdau = przygotuj_zdau(katZr, probka)
  jednostki = przygotuj_jednostki(katZr, rocznik)
  baza = polacz_zus_zdau(zus, zdau, pnaPowiaty, dataMin, dataMax)
  miesieczne = agreguj_do_miesiecy(baza, zdau)
  save(zdau, jednostki, baza, miesieczne, utrataPracy, file = plikCache, compress = TRUE)
  rm(zus)
} else {
  suppressWarnings(rm(zus))
  load(plikCache)
}

####################
# 3. Wyliczamy zmienne w poszczególnych okienkach czasu
####################
for (i in seq_along(okienka)) {
  unlink(nazwa_pliku(paste0('razem_', i), 'cache', rocznik))
  razem = okienko_ela(okienka[[i]], zdau, baza, miesieczne, utrataPracy, jednostki, krok, TRUE)
  save(razem, file = nazwa_pliku(paste0('razem_', i), 'cache', rocznik), compress = TRUE)
  rm(razem)
}

####################
# 4. Wyliczamy zmienne niezależne od okienka czasu (STUDYP*, TP_*, *_K, *_R, itp.)
####################

plikCache = nazwa_pliku('stale', 'cache', rocznik)
if (!file.exists(plikCache) | pominCache) {
  studyp = oblicz_studyp(zdau)
  czas = oblicz_zmienne_czasowe(baza, utrataPracy)
  stale = oblicz_stale(baza, zdau)
  save(studyp, czas, stale, file = plikCache, compress = TRUE)
} else {
  load(plikCache)
}

##########
# Złączamy wszystko, cośmy policzyli i zapisujemy
wszystko = oblicz_stale_czasowe(zdau, dataMax) %>%
  filter_(~typ %in% 'A') %>%
  full_join(studyp) %>%
  full_join(czas) %>%
  full_join(stale) %>%
  left_join(przygotuj_kierunki(katZr, kierZmDod)) %>%
  left_join(przygotuj_jednostki(katZr, rocznik) %>% select(jednostka_id, jednostka, uczelnianazwa)) %>%
  rename_(kierunek = 'kierunek_id', uczelnia = 'uczelnia_id')
for (i in seq_along(okienka)) {
  load(nazwa_pliku(paste0('razem_', i), 'cache', rocznik))
  wszystko = full_join(wszystko, razem)
}
stopifnot(
  nrow(wszystko) == nrow(zdau %>% filter_(~typ %in% 'A'))
)
colnames(wszystko) = toupper(colnames(wszystko))
save(wszystko, file = nazwa_pliku('dane', katZr, rocznik), compress = TRUE)

##########
# Zbiór danych miesięcznych
okienkoMies = oblicz_okienko(miesieczne, okienko(-60, 60, 'data_do', 'data_do', '', dataMin, dataMax)) %>%
  filter_(~okres >= okres_min & okres <= okres_max) %>%
  select_('id_zdau', 'id', 'okres', 'if_x_s', 'if_x_stprg', 'wzg_ez_e', 'wzg_ez_z', 'wzg_ryzbez', 'ez_z', 'ez_e', 'if_p', 'if_e', 'if_s', 'status')
okienkoMies = okienkoMies %>%
  mutate_(okres = ~okres2data(okres))

names(okienkoMies) = toupper(paste0(names(okienkoMies), '_M'))
save(okienkoMies, file = nazwa_pliku('dane_mies', katZr, rocznik), compress = TRUE)

##########
# Zbiór danych kwartalnych (+0, +3, +6, itd. miesiąc od dyplomu)
kwartalne = miesieczne %>%
  filter(okres >= data_do & (okres - data_do) %% 3L == 0L) %>%
  mutate(
    kwartal = as.integer((okres - data_do) / 3L),
    data = okres2data(okres),
    nm_es = pmin(nm_e + nm_s, 1)
  ) %>%
  left_join(
    baza %>%
      select_('id_zdau', 'okres', 'klaszam2') %>%
      group_by_('id_zdau', 'okres') %>%
      summarize_(klasz = ~min(klaszam2))
  )

names(kwartalne) = toupper(names(kwartalne))
save(kwartalne, file = nazwa_pliku('dane_kwart', katZr, rocznik), compress = TRUE)

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
