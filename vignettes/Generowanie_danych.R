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
okienkaSufiksy = c('m1', 'p1', 'p2', '')
okienkaIter = c(2, 4)
plikZapisu = 'dane/nowe/nowe'

##########
# Przygotowujemy dane zus, statystyki z BDL przypisane do PNA, dane OPI (zbiór ZDAU), itp.
# pnaPowiaty = polacz_pna_powiaty(przygotuj_pna(), przygotuj_powiaty(), dataMin, dataMax)
pnaPowiaty = przygotuj_pna_powiaty_mb(dataMin, dataMax)
jednostki = przygotuj_jednostki()
zdau = przygotuj_zdau()
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

##########
# Wyliczamy zmienne w poszczególnych okienkach czasu
for(i in okienkaIter){
  okienkoMin = okienkaMin[i]
  okienkoMax = okienkaMax[i]
  cat(okienkoMin, '-', okienkoMax)

  okienko = oblicz_okienko(baza, okienkoMin, okienkoMax, dataMin, dataMax)
  len = okienko %>%
    select(id_zdau, len) %>%
    distinct()

  abs1 = oblicz_absolwent(okienko)
  abs2 = oblicz_absolwent_okres(okienko)
  nnn  = oblicz_nowi_pracodawcy(okienko)
  nmle = oblicz_utrata_etatu(okienko, utrataEtatu)
  osoba = oblicz_osoba(okienko)
  razem = zdau %>%
    filter_(~ typ %in% 'A') %>%
    select_('id_zdau') %>%
    full_join(len) %>%
    full_join(abs1) %>%
    full_join(abs2) %>%
    full_join(nnn) %>%
    full_join(nmle) %>%
    full_join(osoba)
  rm(abs1, abs2, nnn, nmle, osoba)
  gc()
  razem = oblicz_zmienne_pochodne(razem)

  colnames(razem) = sub('^id_zdau.*$', 'id_zdau', paste0(colnames(razem), okienkaSufiksy[i]))

  if(okienkoMax == 1000){
    zam0  = oblicz_zamieszkanie(okienko, jednostki, TRUE)
    colnames(zam0) = sub('^id_zdau.*$', 'id_zdau', paste0(colnames(zam0), '0'))
    zam1  = oblicz_zamieszkanie(okienko, jednostki, FALSE)
    colnames(zam1) = sub('^id_zdau.*$', 'id_zdau', paste0(colnames(zam1), '1'))
    razem = razem %>%
      full_join(zam0) %>%
      full_join(zam1)
    rm(zam0, zam1)
    gc()
  }

  save(razem, file = paste0('cache/razem', i, '.RData'), compress = TRUE)
  rm(razem)
  gc()
}

##########
# Wyliczamy zmienne niezależne od okienka czasu (KONT, STUDYP* oraz CZAS*)
studyp = oblicz_studyp(zdau)
czas = oblicz_zmienne_czasowe(baza, utrataEtatu)
stale = oblicz_stale(baza, zdau)

##########
# Złączamy wszystko, cośmy policzyli i zapisujemy
wszystko = oblicz_stale_czasowe(zdau, dataMax) %>%
  filter_(~ typ %in% 'A') %>%
  full_join(studyp) %>%
  full_join(czas) %>%
  full_join(stale) %>%
  left_join(przygotuj_kierunki()) %>%
  left_join(jednostki %>% select(jednostka_id, jednostka, uczelnianazwa)) %>%
  rename_(kierunek = 'kierunek_id', uczelnia = 'uczelnia_id')
for(i in okienkaIter){
  load(paste0('cache/razem', i, '.RData'))
  wszystko = full_join(wszystko, razem)
}
stopifnot(
  nrow(wszystko) == nrow(zdau %>% filter_(~ typ %in% 'A'))
)
colnames(wszystko) = toupper(colnames(wszystko))
save(wszystko, file = paste0(plikZapisu, '.RData'), compress = TRUE)

##########
# Zbiór danych miesięcznych
okienko = oblicz_okienko(baza, -60, 60, dataMin, dataMax)
miesieczne = oblicz_zmienne_miesieczne(okienko)
names(miesieczne) = toupper(names(miesieczne))
save(miesieczne, file = paste0(plikZapisu, '_mies.RData'), compress = TRUE)
