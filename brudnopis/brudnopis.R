dataMin = '2014-01-01'
dataMax = '2015-09-30'
pnaPowiaty = polacz_pna_powiaty(przygotuj_pna(), przygotuj_powiaty(), dataMin, dataMax)
jednostki = przygotuj_jednostki()
# zus = przygotuj_zus(dataMin, dataMax)
# save(zus, file = 'cache/ZUS.RData', compress = TRUE)
load('cache/ZUS.RData')
# utrataEtatu = przygotuj_utrata_pracy(zus, dataMax)
# save(utrataEtatu, file = 'cache/utrataEtatu.RData', compress = TRUE)
load('cache/utrataEtatu.RData')
zdau = przygotuj_zdau()
baza = polacz_zus_zdau(zus, zdau, pnaPowiaty)

studyp = oblicz_studyp(zdau)

okienkoMin = 0
okienkoMax = 1000
# okienko = oblicz_okienko(baza, okienkoMin, okienkoMax, dataMin, dataMax)
# save(okienko, file = 'cache/okienko.RData', compress = TRUE)
load('cache/okienko.RData')

czas = oblicz_zmienne_czasowe(okienko, utrataEtatu)
abs1 = oblicz_absolwent(okienko)
abs2 = oblicz_absolwent_okres(okienko)
nnn = oblicz_nowi_pracodawcy(okienko)
nmle = oblicz_utrata_etatu(okienko, utrataEtatu)
zam  = oblicz_zamieszkanie(okienko, jednostki, okienkoMin)
razem = abs2 %>%
  full_join(abs2) %>%
  full_join(nnn) %>%
  full_join(nmle) %>%
  full_join(zam) %>%
  full_join(czas)
rm(abs1, abs2, nnn, nmle, zam, czas)
razem = oblicz_zmienne_pochodne(razem)



