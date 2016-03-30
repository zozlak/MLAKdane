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
katalogZapisu = 'dane/nowe'

##########
# Przygotowujemy dane zus, statystyki z BDL przypisane do PNA, dane OPI (zbiór ZDAU), itp.
# pnaPowiaty = polacz_pna_powiaty(przygotuj_pna(), przygotuj_powiaty(), dataMin, dataMax) # t1
pnaPowiaty = przygotuj_pna_powiaty_mb(dataMin, dataMax) # t1
jednostki = przygotuj_jednostki() # t2
zdau = przygotuj_zdau() # t5
zus = przygotuj_zus(dataMin, dataMax) # t3
save(zus, file = 'cache/ZUS.RData', compress = TRUE)
# load('cache/ZUS.RData')

##########
# Wyliczamy pomocniczy zbiór utrataEtatu
utrataEtatu = przygotuj_utrata_pracy(zus, dataMax) # t4
save(utrataEtatu, file = 'cache/utrataEtatu.RData', compress = TRUE)
# load('cache/utrataEtatu.RData')

##########
# złączamy dane ZUS z danymi OPI i statystykami powiatów z BDL
baza = polacz_zus_zdau(zus, zdau, pnaPowiaty, dataMin, dataMax) # t6
save(baza, file = 'cache/baza.RData', compress = TRUE)
# load('cache/baza.RData')

##########
# Wyliczamy zmienne w poszczególnych okienkach czasu
for(i in okienkaIter){
  okienkoMin = okienkaMin[i]
  okienkoMax = okienkaMax[i]
  cat(okienkoMin, '-', okienkoMax)

  okienko = oblicz_okienko(baza, okienkoMin, okienkoMax, dataMin, dataMax) # t9
  len = okienko %>%
    select(id_zdau, len) %>%
    distinct()

  abs1 = oblicz_absolwent(okienko) # t10
  abs2 = oblicz_absolwent_okres(okienko) # t11
  nnn  = oblicz_nowi_pracodawcy(okienko) # t12
  nmle = oblicz_utrata_etatu(okienko, utrataEtatu) # t13
  zam  = oblicz_zamieszkanie(okienko, jednostki, okienkoMax == 1000) # t14
  razem = len %>%
    right_join(abs1) %>%
    full_join(abs2) %>%
    full_join(nnn) %>%
    full_join(nmle) %>%
    full_join(zam)
  rm(abs1, abs2, nnn, nmle, zam)
  gc()
  razem = oblicz_zmienne_pochodne(razem) # t16

  colnames(razem) = sub('^id_zdau.*$', 'id_zdau', paste0(colnames(razem), okienkaSufiksy[i]))
  save(razem, file = paste0('cache/razem', i, '.RData'), compress = TRUE)
  rm(razem)
  gc()
}

##########
# Wyliczamy zmienne niezależne od okienka czasu (KONT, STUDYP* oraz CZAS*)
studyp = oblicz_studyp(zdau) # t7
czas = oblicz_zmienne_czasowe(baza, utrataEtatu) # t8

##########
# Złączamy wszystko, cośmy policzyli i zapisujemy
wszystko = zdau %>%
  filter_(~ typ %in% 'A') %>%
  full_join(studyp) %>%
  full_join(czas) %>%
  left_join(przygotuj_kierunki()) %>%
  left_join(jednostki %>% select(jednostka_id, jednostka, uczelnia))
for(i in okienkaIter){
  load(paste0('cache/razem', i, '.RData'))
  wszystko = full_join(wszystko, razem)
}
stopifnot(
  nrow(wszystko) == nrow(zdau %>% filter_(~ typ %in% 'A'))
)
colnames(wszystko) = toupper(colnames(wszystko))
save(wszystko, file = paste0(katalogZapisu, '/zlaczone.RData'), compress = TRUE)
write.csv2(wszystko, paste0(katalogZapisu, '/zlaczone.csv'), row.names = FALSE, fileEncoding = 'Windows-1250')

