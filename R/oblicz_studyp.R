#' oblicza zmienne KONT oraz STUDYP*
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zdau}}
#' @param kierunki zbiór informacji o kierunkach studiów wygenerowany za pomocą
#'   funkcji \code{\link{przygotuj_kierunki}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_studyp = function(dane, kierunki){
  # upewnijmy się, że dane są w R
  dane = dane %>%
    collect() %>%
    mutate_(rok = ~coalesce(okres2rok(data_do), okres2rok(data_od))) %>%
    left_join(kierunki %>% select_('kierunek_id', 'jednostka_id', 'rok', 'obsz_kod', 'dzie_kod', 'dysc_kod'))

  skalaRelKier = function(ucz1, ucz2, jedn1, jedn2, forma2){
    zgUcz = ucz1 == ucz2 & !is.na(ucz2) & !is.na(ucz1)
    zgJedn = zgUcz & jedn1 == jedn2 & !is.na(jedn1) & !is.na(jedn2)
    kont = min((!zgUcz) * 4 + (!zgJedn) * 2 + (forma2 != 'S'), na.rm = TRUE)
    kont = if_else(is.infinite(kont), 0, if_else(kont > 5, kont - 1, kont + 1))
    return(as.integer(kont))
  }

  skalaRelDysc = function(ob1, ob2, dz1, dz2, dy1, dy2) {
    return(if_else(
      any(dy1 == dy2),
      1L,
      if_else(
        any(dz1 == dz2),
        2L,
        if_else(any(ob1 == ob2), 3L, 4L)
      )
    ))
  }

  kont = dane %>%
    filter_(~poziom == 1 & typ == 'A') %>%
    select_('id_zdau', 'id', 'uczelnia_id', 'jednostka_id', 'data_od', 'data_do', 'obsz_kod', 'dzie_kod', 'dysc_kod') %>%
    inner_join(
      dane %>%
        filter_(~poziom %in% 2) %>%
        select_('id', 'uczelnia_id', 'jednostka_id', 'forma', 'data_od', 'data_do', 'obsz_kod', 'dzie_kod', 'dysc_kod', 'typ') %>%
        rename_(
          data_od_ = 'data_od', data_do_ = 'data_do', typ_ = 'typ',
          uczelnia_id_ = 'uczelnia_id', jednostka_id_ = 'jednostka_id', forma_ = 'forma',
          obsz_kod_ = 'obsz_kod', dzie_kod_ = 'dzie_kod', dysc_kod_ = 'dysc_kod'
        )
    ) %>%
    filter_(~data_od_ >= data_do) %>%
    group_by_('id_zdau') %>%
    summarize_(
      kont_jedn = ~skalaRelKier(uczelnia_id, uczelnia_id_, jednostka_id, jednostka_id_, forma_),
      kont_dysc = ~skalaRelDysc(obsz_kod, obsz_kod_, dzie_kod, dzie_kod_, dysc_kod, dysc_kod_),
      kont_ukon = ~as.integer(any(typ_ == 'A')),
      kont = 1L
    ) %>%
    select_('id_zdau', 'kont_jedn', 'kont_dysc', 'kont_ukon', 'kont')

  kontDyplom = dane %>%
    filter_(~typ == 'A') %>%
    select_('id_zdau', 'id', 'data_do') %>%
    inner_join(
      dane %>%
        filter_(~typ == 'A') %>%
        select('id', 'data_do') %>%
        rename_(data_do_ = 'data_do')
    ) %>%
    filter_(~data_do_ > data_do) %>%
    select_('id_zdau') %>%
    distinct() %>%
    mutate_(kont_dyplom = 1L)

  polaczone = dane %>%
    filter_(~typ == 'A') %>%
    select_('id_zdau', 'id', 'data_od', 'data_do', 'uczelnia_id', 'jednostka_id') %>%
    left_join(
      dane %>%
        filter_(~typ == 'S') %>%
        select_('id', 'id_zdau', 'data_od', 'data_do', 'uczelnia_id', 'jednostka_id', 'forma', 'poziom') %>%
        rename_(id_zdau_ = 'id_zdau', data_od_ = 'data_od', data_do_ = 'data_do', uczelnia_id_ = 'uczelnia_id', jednostka_id_ = 'jednostka_id', forma_ = 'forma', poziom_ = 'poziom')
    ) %>%
    filter_(~data_od_ <= data_do, ~ is.na(data_do_) | data_do_ > data_do, ~ !is.na(id_zdau_), ~ id_zdau != id_zdau_)

  studyp = polaczone %>%
    group_by_('id_zdau') %>%
    summarize(studyp = n())

  studyp1 = polaczone %>%
    group_by_('id_zdau') %>%
    filter_(~poziom_ == '1' & row_number() == 1) %>%
    summarize_(
      studyp1 = ~skalaRelKier(uczelnia_id, uczelnia_id_, jednostka_id, jednostka_id_, forma_)
    )
  studyp2 = polaczone %>%
    group_by_('id_zdau') %>%
    filter_(~poziom_ == '2' & row_number() == 1) %>%
    summarize_(
      studyp2 = ~skalaRelKier(uczelnia_id, uczelnia_id_, jednostka_id, jednostka_id_, forma_)
    )
  studyp3 = polaczone %>%
    group_by_('id_zdau') %>%
    filter_(~poziom_ == 'JM' & row_number() == 1) %>%
    summarize_(
      studyp3 = ~skalaRelKier(uczelnia_id, uczelnia_id_, jednostka_id, jednostka_id_, forma_)
    )

  wynik = dane %>%
    filter_(~typ == 'A') %>%
    select_('id_zdau', 'poziom') %>%
    left_join(kont) %>%
    left_join(kontDyplom) %>%
    left_join(studyp) %>%
    left_join(studyp1) %>%
    left_join(studyp2) %>%
    left_join(studyp3) %>%
    mutate_(
      kont_jedn   = ~if_else(is.na(kont) & poziom == '1', 0L, kont_jedn),
      kont_dysc   = ~if_else(is.na(kont) & poziom == '1', 0L, kont_dysc),
      kont_ukon   = ~if_else(is.na(kont) & poziom == '1', 0L, kont_ukon),
      kont_dyplom = ~coalesce(kont_dyplom, 0L),
      studyp  = ~coalesce(studyp, 0L),
      studyp1 = ~coalesce(studyp1, 0L),
      studyp2 = ~coalesce(studyp2, 0L),
      studyp3 = ~coalesce(studyp3, 0L)
    ) %>%
    mutate_(
      kont_if = ~as.integer(kont_jedn > 0L)
    ) %>%
    select_('-poziom', '-kont')

  class(dane) = c('absolwent_df', class(wynik))
  return(wynik)
}