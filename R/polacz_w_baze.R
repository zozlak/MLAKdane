#' łączy dane zus, zdau, statystyki powiatów, informacje o jednostkach oraz
#' karierze
#' @description Aby poprawnie przyłączyć statystyki powiatów macierz danych
#' uzupełniana jest tak, aby dla każdego absolwenta zawierała informacje o
#' każdym okresie w zadanym przedziale czasu, ale nie później niż do "końca
#' obserwacji w zus" (zakładając zamieszkanie absolwenta w uzupełnianych
#' okresach "w Polsce", kodowane jako pna = -1, oraz wszelkie cechy tytułu
#' ubezpieczenia ZUS równe 0, w szczególności także bezrob = 0).
#'
#' Generowana jest również zmienna określająca, czy absolwent studiował w
#' poszczególnych okresach, wyznaczana na podstawie danych ZDAU
#' @param zus dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zus}}
#' @param zdau dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zdau}}
#' @param kariera dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_kariere}}
#' @param jednostki dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_jednostki}}
#' @param pnaPowiaty dane wygenerowane za pomocą funkcji
#'   \code{\link{polacz_pna_powiaty}}
#' @param dataMin początek okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2014-01-01')
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2015-09-30')
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
polacz_w_baze = function(zus, zdau, kariera, jednostki, pnaPowiaty, dataMin, dataMax){
  stopifnot(
    methods::is(zus, 'zus_df'),
    methods::is(zdau, 'zdau_df'),
    methods::is(pnaPowiaty, 'pna_powiaty_df')
  )

  okresy = data2okres(dataMin):data2okres(dataMax)
  absolw = zdau %>%
    filter_(~typ %in% 'A' | poziom %in% '3') # doktoranci jak leci, także nieukończeni
  wynik = data.frame(
    id_zdau      = rep(absolw$id_zdau,      each = length(okresy)),
    id           = rep(absolw$id,           each = length(okresy)),
    data_od      = rep(absolw$data_od,      each = length(okresy)),
    data_do      = rep(absolw$data_do,      each = length(okresy)),
    jednostka_id = rep(absolw$jednostka_id, each = length(okresy)),
    okres        = rep(okresy,              times = nrow(absolw))
  )
  wynik = wynik %>%
    left_join(
      zus %>%
        select_('-nspraw', '-pkd', '-platnik_kon', '-plec', '-rok_ur', '-rolnik', '-zlec', '-koniec')
    ) %>%
    left_join(
      zus %>%
        select_('id', 'koniec') %>%
        distinct()
    ) %>%
    filter_(~ (okres < koniec & data_do <= koniec) | is.na(koniec)) %>%
    left_join(
      jednostki %>%
        select_('regon') %>%
        mutate_(nauka = TRUE)
    )

  stud = zdau %>%
    select_('id', 'data_od', 'data_do') %>%
    left_join(
      wynik %>% select_('id', 'okres')
    ) %>%
    filter_(~ okres >= data_od & ((is.na(data_do) | okres <= data_do))) %>%
    select_('id', 'okres') %>%
    distinct() %>%
    mutate_(studopi = 1)

  wynik = wynik %>%
    left_join(stud) %>%
    mutate_(
      etat     = ~ifelse(is.na(etat), 0L, etat),
      netat    = ~ifelse(is.na(netat), 0L, netat),
      samoz    = ~ifelse(is.na(samoz), 0L, samoz),
      bezrob   = ~ifelse(is.na(bezrob), 0L, bezrob),
      rentemer = ~ifelse(is.na(rentemer), 0L, rentemer),
      studzus  = ~ifelse(is.na(student), 0L, student), # student wg zus
      studopi  = ~ifelse(is.na(studopi), 0L, studopi), # student wg opi
      if_b       = ~as.integer(bezrob > 0L), # niezbędne, aby poprawnie policzyć if_x_s zagregowane do miesięcy
      if_x_s     = ~as.integer(studzus + studopi > 0L), # student wg zus lub OPI
      if_x_stprg = ~as.integer(if_x_s == 1L & okres >= data_od & (okres <= data_do | is.na(data_do))), # student na kierunku studiów id_zdau
      prawnik  = ~ifelse(is.na(prawnik), 0L, prawnik),
      mundur   = ~ifelse(is.na(mundur), 0L, mundur),
      # zlec     = ~ifelse(is.na(zlec), 0L, zlec),
      # nspraw   = ~ifelse(is.na(nspraw), 0L, nspraw),
      # rolnik   = ~ifelse(is.na(rolnik), 0, rolnik),
      dziecko  = ~ifelse(is.na(dziecko), 0L, dziecko),
      podst    = ~ifelse(is.na(podst), 0L, podst),
      pna      = ~ifelse(is.na(pna), -1, pna)
    ) %>%
    select_('-student', '-studzus', '-studopi') %>%
    group_by_('id_zdau', 'id', 'okres') %>%
    mutate_(
      if_nb      = ~as.integer(all(if_b == 0L)),
      if_x_s     = ~as.integer(any(if_x_s > 0L) & if_nb), # student wg zus lub OPI
      if_x_stprg = ~as.integer(any(if_x_stprg > 0L) & if_nb) # student na kierunku studiów id_zdau
    ) %>%
    select_('-if_b', '-if_nb') %>%
    ungroup()

  wynik = left_join(wynik, pnaPowiaty)

  stopifnot(
    all(!is.na(wynik$powezar_teryt))
  )

  class(wynik) = c('baza_df', class(wynik))
  return(wynik)
}