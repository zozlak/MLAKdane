#' oblicza proste zmienne niezwiazane z okienkami czasu
#' @param dane dane wygenerowane za pomocą funkcji \code{\link{polacz_zus_zdau}}
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_stale = function(dane){
  stopifnot(
    is(dane, 'baza_df')
  )

  w1 = dane %>%
    select_('id_zdau', 'id_platnika') %>%
    group_by_('id_zdau') %>%
    summarize_(ifzus = ~ as.numeric(any(!is.na(id_platnika)))) %>%
    ungroup()

  # w trakcie studiów
  w2 = dane %>%
    select_('id_zdau', 'okres', 'data_rozp', 'data_zak', 'etat', 'samoz', 'podst') %>%
    filter_(~ okres >= data_rozp & okres <= data_zak) %>%
    group_by_('id_zdau') %>%
    summarize_(
      ezarstud = ~ sum(podst) / (first(data_zak) - first(data_rozp) + 1),
      ezustud  = ~ sum(podst[etat == 1]) / (first(data_zak) - first(data_rozp) + 1),
      pracaS   = ~ sum(etat + samoz) > 0
    )

  # przed studiami
  w3 = dane %>%
    select_('id_zdau', 'okres', 'data_rozp', 'data_zak', 'etat', 'samoz', 'podst') %>%
    filter_(~ okres < data_rozp) %>%
    group_by_('id_zdau') %>%
    summarize_(
      ezarrekr = ~ sum(podst) / length(unique(okres)),
      ezurekr  = ~ sum(podst[etat == 1]) / length(unique(okres[etat == 1])),
      pracaPS  = ~ sum(etat + samoz) > 0
    )

  dane = w1 %>%
    full_join(w2) %>%
    full_join(w3) %>%
    mutate_(
      ezarstud = ~ ifelse(is.na(ezarstud), 0, ezarstud),
      ezarrekr = ~ ifelse(is.na(ezarrekr), 0, ezarrekr),
      ezustud  = ~ ifelse(is.na(ezustud), 0, ezustud),
      ezurekr  = ~ ifelse(is.na(ezurekr), 0, ezurekr),
      pracaS   = ~ ifelse(is.na(pracaS), FALSE, pracaS),
      pracaPS  = ~ ifelse(is.na(pracaPS), FALSE, pracaPS),
      dosw_es  = ~ ifelse(pracaPS, 1, ifelse(pracaS, 2, 3))
    ) %>%
    select_('-pracaS', '-pracaPS')
  return(dane)
}