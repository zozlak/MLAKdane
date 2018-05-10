#' łączy dane zus, zdau oraz statystyki powiatów
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
#' @param pnaPowiaty dane wygenerowane za pomocą funkcji
#'   \code{\link{polacz_pna_powiaty}}
#' @param dataMin początek okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2014-01-01')
#' @param dataMax koniec okresu uwzględnionego w danych ZUS (jako łańcuch
#'   znaków, np. '2015-09-30')
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
polacz_zus_zdau = function(zus, zdau, pnaPowiaty, dataMin, dataMax){
  absolw = zdau %>%
    filter_(~typ %in% 'A') %>%
    select_('id_zdau', 'id', 'data_od', 'data_do', 'jednostka_id') %>%
    mutate_(dummy = 1L)
  okresy = data_frame(dummy = 1L, okres = data2okres(dataMin):data2okres(dataMax))
  wynik1 = absolw %>%
    inner_join(okresy, copy = TRUE) %>%
    select_('-dummy') %>%
    left_join(
      zus %>%
        select_('-nspraw', '-platnik_kon', '-rolnik', '-zlec', '-koniec')
    ) %>%
    left_join(
      zus %>%
        select_('id', 'koniec') %>%
        distinct()
    ) %>%
    filter_(~(okres < koniec & data_do <= koniec) | is.na(koniec))

  stud = zdau %>%
    select_('id', 'data_od', 'data_do') %>%
    left_join(
      wynik1 %>% select_('id', 'okres')
    ) %>%
    filter_(~okres >= data_od & ((is.na(data_do) | okres <= data_do))) %>%
    select_('id', 'okres') %>%
    distinct() %>%
    mutate_(studopi = 1)

  wynik2 = wynik1 %>%
    left_join(stud) %>%
    mutate_(
      etat     = ~coalesce(etat, 0L),
      netat    = ~coalesce(netat, 0L),
      samoz    = ~coalesce(samoz, 0L),
      bezrob   = ~coalesce(bezrob, 0L),
      rentemer = ~coalesce(rentemer, 0L),
      studzus  = ~coalesce(student, 0L), # student wg zus
      studopi  = ~coalesce(studopi, 0L), # student wg opi
      prawnik  = ~coalesce(prawnik, 0L),
      mundur   = ~coalesce(mundur, 0L),
      # zlec     = ~coalesce(zlec, 0L),
      # nspraw   = ~coalesce(nspraw, 0L),
      # rolnik   = ~coalesce(rolnik, 0L),
      dziecko  = ~coalesce(dziecko, 0L),
      podst    = ~coalesce(podst, 0L),
      pna      = ~coalesce(pna, -200L)
    )  %>%
    mutate_(
      if_b   = ~as.integer(bezrob > 0L), # niezbędne, aby poprawnie policzyć if_x_s zagregowane do miesięcy
      if_x_s = ~as.integer(studzus + studopi > 0L) # student wg zus lub OPI
    ) %>%
    mutate_(
      if_x_stprg = ~as.integer(if_x_s == 1L & okres >= data_od & (okres <= data_do | is.na(data_do))) # student na kierunku studiów id_zdau
    ) %>%
    select_('-student', '-studzus', '-studopi') %>%
    group_by_('id_zdau', 'id', 'okres') %>%
    mutate_(
      if_nb      = ~as.integer(sum(as.integer(if_b == 0L), na.rm = TRUE) == n())
    ) %>%
    mutate_(
      if_x_s     = ~as.integer(sum(as.integer(if_x_s > 0L), na.rm = TRUE) > 0L & if_nb > 0L), # student wg zus lub OPI
      if_x_stprg = ~as.integer(sum(as.integer(if_x_stprg > 0L), na.rm = TRUE) > 0L & if_nb > 0L) # student na kierunku studiów id_zdau
    ) %>%
    select_('-if_b', '-if_nb') %>%
    ungroup() %>%
    left_join(pnaPowiaty)

  # stopifnot(
  #   all(!is.na(wynik$powezar_teryt))
  # )

  return(wynik2)
}