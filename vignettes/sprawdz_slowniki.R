devtools::load_all(".")
library(dplyr)

katZr = 'dane/ZUS_2016-09/2014'
rocznik = 2014

##########

zdau = przygotuj_zdau(katZr)

#####
# Jednostki
#####
jednostki = przygotuj_jednostki(katZr, rocznik)

# Jeśli błąd, to w słowniku jednostek znajdują się zmienne o nazwach jednakowych, jak w zbiorze ZDAU
# Należy usunąć lub przezwać te zmienne
stopifnot(
  intersect(colnames(zdau), colnames(jednostki)) == 'jednostka_id'
)

# Kody jednostek występujących w zbiorze ZDAU i niewystępujących w słowniku jednostek
zdau %>%
  select(jednostka_id) %>%
  distinct() %>%
  anti_join(jednostki) %>%
  arrange(jednostka_id)

#####
# Kierunki
#####
kierunki = przygotuj_kierunki(katZr)

# Jeśli błąd, to w słowniku kierunków znajdują się zmienne o nazwach jednakowych, jak w zbiorze ZDAU
# Należy usunąć lub przezwać te zmienne
stopifnot(
  intersect(colnames(zdau), colnames(kierunki)) == c('uczelnia_id', 'jednostka_id', 'kierunek_id')
)

# Kody kierunków występujących w zbiorze ZDAU i niewystępujących w słowniku kierunków
zdau %>%
  select(kierunek_id) %>%
  distinct() %>%
  anti_join(kierunki) %>%
  arrange(kierunek_id)
