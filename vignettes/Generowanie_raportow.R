library(MLAK)
library(openxlsx)
devtools::load_all(".")

def = read.xlsx('parametry_do_csv.xlsx')

#### 2015
dane = wczytajDane('2015_dane_20170428.RData')
daneMies = wczytajDane('2015_dane_mies_20170428.RData')

## programy
odbiorcy = wczytajDane('MLA1_odbiorcy_program_15_20170518.csv')
generujRaporty('R3-program_ELA2_v10_OPI1-1rok.Rmd', odbiorcy, list(dane, daneMies), 'R3-program_ELA2_v8_OPI1-1rok')
write.csv2(oblicz_wartosci(odbiorcy, list(dane, daneMies), def$program.1.rok[-(1:2)], def$program.1.rok[2], def$program.1.rok[1]), 'P1.csv', row.names = F, na = '')

## uczelnie
odbiorcy = wczytajDane('MLA1_odbiorcy_uczelnie_15_20170126.csv')
generujRaporty('R3-uczelnia_ELA2_v10_OPI1-1rok.Rmd', odbiorcy, list(dane, daneMies), 'R3-uczelnia_ELA2_v9_OPI1-1rok')
write.csv2(oblicz_wartosci(odbiorcy, list(dane, daneMies), def$uczelnia.1.rok[-(1:2)], def$uczelnia.1.rok[2], def$uczelnia.1.rok[1]), 'U1.csv', row.names = F, na = '')

## Polska
odbiorcy = data_frame(VROKDYP = rep(2015, 3), VSTOPIEN = c('1', '2', 'JM'))
write.csv2(oblicz_wartosci(odbiorcy, list(dane, daneMies), def$Polska.1.rok[-(1:2)], def$Polska.1.rok[2], def$Polska.1.rok[1]), 'Pl1.csv', row.names = F, na = '')

### 2014
dane = wczytajDane('2014_dane_20170428.RData')
daneMies = wczytajDane('2014_dane_mies_20170428.RData')

## programy
odbiorcy = wczytajDane('MLA1_odbiorcy_program_14_20170518.csv')
generujRaporty('R3-program_ELA2_v10_OPI1-2lata.Rmd', odbiorcy, list(dane, daneMies), 'R3-program_ELA2_v8_OPI1-2lata')
write.csv2(oblicz_wartosci(odbiorcy, list(dane, daneMies), def$program.2lata[-(1:2)], def$program.2lata[2], def$program.2lata[1]), 'P2.csv', row.names = F, na = '')

## uczelnie
odbiorcy = wczytajDane('MLA1_odbiorcy_uczelnie_14_20170126.csv')
generujRaporty('R3-uczelnia_ELA2_v10_OPI1-2lata.Rmd', odbiorcy, list(dane, daneMies), 'R3-uczelnia_ELA2_v9_OPI1-2lata')
write.csv2(oblicz_wartosci(odbiorcy, list(dane, daneMies), def$uczelnia.2lata[-(1:2)], def$uczelnia.2lata[2], def$uczelnia.2lata[1]), 'U2.csv', row.names = F, na = '')

## Polska
odbiorcy = data_frame(VROKDYP = rep(2014, 3), VSTOPIEN = c('1', '2', 'JM'))
write.csv2(oblicz_wartosci(odbiorcy, list(dane, daneMies), def$Polska.2lata[-(1:2)], def$Polska.2lata[2], def$Polska.2lata[1]), 'Pl2.csv', row.names = F, na = '')
