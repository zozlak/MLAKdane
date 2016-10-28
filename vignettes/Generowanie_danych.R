##########
# Ładujemy pakiety i ustawiamy stałe
# Zmienne "okienkaMin", "okienkaMax" i "okienkaSufiksy" określają możliwe okienka czasowe,
#   np. kombinacja wartości -11 z okienkaMin, 0 z okienkaMax i "_m1" z okienkaSufiksy
#   oznacza okienko czasowe od 11 miesięcy przed dyplomem do miesiąca dyplomu (włącznie)
#   oraz że zmienne wyliczone dla tego okienka dostaną sufiks "_m1".
# Zmienna "okienkaIter" oznacza natomiast, które z możliwych okienek mają zostać wyliczone
devtools::load_all(".")
library(dplyr)

dataMin = '2014-01-01'
dataMax = '2015-09-30' # 2015-09-30/2015-03-31 dla nowych/starych danych
okienkaMin = c(-11, 1, 13, 1)
okienkaMax = c(0, 12, 24, 1000)
okienkaSufiksy = c('_m1', '_p1', '_p2', '')
okienkaIter = c(2, 4)
plikZapisu = 'dane/nowe/nowe'

##########
# Przygotowujemy dane zus, statystyki z BDL przypisane do PNA, dane OPI (zbiór ZDAU), itp.
# pnaPowiaty = polacz_pna_powiaty(przygotuj_pna(), przygotuj_powiaty(), dataMin, dataMax)
pnaPowiaty = przygotuj_pna_powiaty_mb(dataMin, dataMax)
jednostki = przygotuj_jednostki()
zdau = przygotuj_zdau() 
zdauAbs = zdau %>%
  filter_(~typ %in% 'A') %>%
  select_('id_zdau')
zus = przygotuj_zus(dataMin, dataMax)
save(zus, file = 'cache/ZUS.RData', compress = TRUE)
# load('cache/ZUS.RData')

##########
# Wyliczamy pomocniczy zbiór utrataEtatu
utrataEtatu = przygotuj_utrata_pracy(zus, dataMax)
save(utrataEtatu, file = 'cache/utrataEtatu.RData', compress = TRUE)
# load('cache/utrataEtatu.RData')

##########
# złączamy dane ZUS z danymi OPI i statystykami powiatów z BDL
baza = polacz_zus_zdau(zus, zdau, pnaPowiaty, dataMin, dataMax)
save(baza, file = 'cache/baza.RData', compress = TRUE)
# load('cache/baza.RData')
miesieczne = agreguj_do_miesiecy(baza, zdau)
save(miesieczne, file = 'cache/miesieczne.RData', compress = TRUE)
# load('cache/miesieczne.RData')

##########
# Wyliczamy zmienne niezależne od okienka czasu (STUDYP*, TP_*, *_K, *_R, itp.)
studyp = oblicz_studyp(zdau)
czas = oblicz_zmienne_czasowe(baza, utrataEtatu)
stale = oblicz_stale(baza, zdau)

##########
# Wyliczamy zmienne w poszczególnych okienkach czasu
for (i in okienkaIter) {
  okienkoMin = okienkaMin[i]
  okienkoMax = okienkaMax[i]
  cat(okienkoMin, '-', okienkoMax)

  okienkoMies = oblicz_okienko(miesieczne, okienkoMin, okienkoMax, dataMin, dataMax)
  okienkoBaza = oblicz_okienko(baza, okienkoMin, okienkoMax, dataMin, dataMax)

  abs = agreguj_do_okresu(okienkoMies)
  np = oblicz_pracodawcy(okienkoBaza)
  up = oblicz_utrata_etatu(okienkoMies, utrataEtatu)

  razem = zdauAbs %>%
    full_join(abs) %>%
    full_join(np) %>%
    full_join(up) %>%
    mutate_(len = ~coalesce(as.integer(len), 0L))
  stopifnot(
    nrow(zdauAbs) == nrow(razem),
    length(unique(razem$id_zdau)) == nrow(razem)
  )
  rm(abs, np, up);gc()

  colnames(razem) = sub('^id_zdau.*$', 'id_zdau', paste0(colnames(razem), okienkaSufiksy[i]))

  if (okienkoMax == 1000) {
    zam0  = oblicz_zamieszkanie(okienkoBaza, jednostki, TRUE)
    colnames(zam0) = sub('^id_zdau.*$', 'id_zdau', paste0(colnames(zam0), '0'))
    zam1  = oblicz_zamieszkanie(okienkoBaza, jednostki, FALSE)
    colnames(zam1) = sub('^id_zdau.*$', 'id_zdau', paste0(colnames(zam1), '1'))
    razem = razem %>%
      inner_join(zam0) %>%
      inner_join(zam1)
    rm(zam0, zam1); gc()
  }

  save(razem, file = paste0('cache/razem', i, '.RData'), compress = TRUE)
  rm(razem);gc()
}

##########
# Złączamy wszystko, cośmy policzyli i zapisujemy
wszystko = oblicz_stale_czasowe(zdau, dataMax) %>%
  filter_(~typ %in% 'A') %>%
  full_join(studyp) %>%
  full_join(czas) %>%
  full_join(stale) %>%
  left_join(przygotuj_kierunki()) %>%
  left_join(jednostki %>% select(jednostka_id, jednostka, uczelnianazwa)) %>%
  rename_(kierunek = 'kierunek_id', uczelnia = 'uczelnia_id')
for (i in okienkaIter) {
  load(paste0('cache/razem', i, '.RData'))
  wszystko = full_join(wszystko, razem)
}
stopifnot(
  nrow(wszystko) == nrow(zdau %>% filter_(~typ %in% 'A'))
)
colnames(wszystko) = toupper(colnames(wszystko))
save(wszystko, file = paste0(plikZapisu, '.RData'), compress = TRUE)

##########
# Zbiór danych miesięcznych
okienkoMies = oblicz_okienko(miesieczne, -60, 60, dataMin, dataMax) %>%
  filter_(~okres >= okres_min & okres <= okres_max) %>%
  select_('id_zdau', 'okres', 'if_st', 'if_stprg', 'wzg_ez_e', 'wzg_ez_e2', 'wzg_ez_z', 'wzg_ez_z2', 'wzg_ryzbez')
names(okienkoMies) = toupper(sub('^(id_zdau|okres).*$', '\\1', paste0(names(okienkoMies), '_M')))
save(okienkoMies, file = paste0(plikZapisu, '_mies.RData'), compress = TRUE)

Sys.time()
