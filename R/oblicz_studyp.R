#' oblicza zmienne KONT oraz STUDYP*
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{oblicz_okienko}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_studyp = function(dane){
  stopifnot(
    is(dane, 'zdau_df')
  )

  skalaRelKier = function(ucz1, ucz2, jedn1, jedn2, forma2){
    zgUcz = ucz1 == ucz2 & !is.na(ucz2) & !is.na(ucz1)
    zgJedn = zgUcz & jedn1 == jedn2 & !is.na(jedn1) & !is.na(jedn2)
    kont = min((!zgUcz) * 4 + (!zgJedn) * 2 + (forma2 != 'S'), na.rm = TRUE)
    kont = ifelse(is.infinite(kont), 0, ifelse(kont > 5, kont - 1, kont + 1))
    return(kont)
  }

  kont = dane %>%
    filter_(~ poziom %in% 1 & typ %in% 'A') %>%
    select_('id_zdau', 'id', 'uczelnia_id', 'jednostka_id') %>%
    left_join(
      dane %>%
        filter_(~ poziom %in% 2) %>%
        select_('id', 'uczelnia_id', 'jednostka_id', 'forma') %>%
        rename_(uczelnia_id_ = 'uczelnia_id', jednostka_id_ = 'jednostka_id', forma_ = 'forma')
    ) %>%
    group_by_('id_zdau') %>%
    summarize_(
      kont = ~ min(skalaRelKier(uczelnia_id, uczelnia_id_, jednostka_id, jednostka_id_, forma_))
    ) %>%
    select_('id_zdau', 'kont')

  polaczone = dane %>%
    filter_(~ typ == 'A') %>%
    select_('id_zdau', 'id', 'data_rozp', 'data_zak', 'uczelnia_id', 'jednostka_id') %>%
    left_join(
      dane %>%
        filter_(~ typ == 'S') %>%
        select_('id', 'id_zdau', 'data_rozp', 'data_zak', 'uczelnia_id', 'jednostka_id', 'forma', 'poziom') %>%
        rename_(id_zdau_ = 'id_zdau', data_rozp_ = 'data_rozp', data_zak_ = 'data_zak', uczelnia_id_ = 'uczelnia_id', jednostka_id_ = 'jednostka_id', forma_ = 'forma', poziom_ = 'poziom')
    ) %>%
    filter_(~ data_rozp_ < data_zak, ~is.na(data_zak_) | data_zak_ > data_zak, ~!is.na(id_zdau_), ~id_zdau != id_zdau_)

  studyp = polaczone %>%
    group_by_('id_zdau') %>%
    summarize(studyp = n())

  studyp1 = polaczone %>%
    group_by_('id_zdau') %>%
    filter_(~ poziom_ == '1' & row_number() == 1) %>%
    summarize_(
      studyp1 = ~ min(skalaRelKier(uczelnia_id, uczelnia_id_, jednostka_id, jednostka_id_, forma_))
    )
  studyp2 = polaczone %>%
    group_by_('id_zdau') %>%
    filter_(~ poziom_ == '2' & row_number() == 1) %>%
    summarize_(
      studyp2 = ~ min(skalaRelKier(uczelnia_id, uczelnia_id_, jednostka_id, jednostka_id_, forma_))
    )
  studyp3 = polaczone %>%
    group_by_('id_zdau') %>%
    filter_(~ poziom_ == 'JM' & row_number() == 1) %>%
    summarize_(
      studyp3 = ~ min(skalaRelKier(uczelnia_id, uczelnia_id_, jednostka_id, jednostka_id_, forma_))
    )

  wynik = dane %>%
    select_('id_zdau') %>%
    left_join(kont) %>%
    left_join(studyp) %>%
    left_join(studyp1) %>%
    left_join(studyp2) %>%
    left_join(studyp3) %>%
    mutate_(
      kont    = ~ ifelse(is.na(kont), 0, kont),
      studyp  = ~ ifelse(is.na(studyp), 0, studyp),
      studyp1 = ~ ifelse(is.na(studyp1), 0, studyp1),
      studyp2 = ~ ifelse(is.na(studyp2), 0, studyp2),
      studyp3 = ~ ifelse(is.na(studyp3), 0, studyp3)
    )
  stopifnot(
    nrow(wynik) == nrow(dane)
  )

  class(dane) = c('absolwent_df', class(dane))
  return(dane)
}