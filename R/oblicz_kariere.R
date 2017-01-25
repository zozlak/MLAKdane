#' oblicza przebieg kariery naukowej
#' @description oblicza przebieg kariery naukowej
#' @param zdau dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zdau}}
#' @param jednostki dane wygenerowane za pomocą funkcji
#'   \code{\link{przygotuj_jednostki}}
#' @return ramka danych opisująca karierę naukową (w postaci szerokiej)
#' @export
#' @import dplyr
oblicz_kariere = function(zdau, jednostki, kierunki){
  stopifnot(
    methods::is(zdau, 'zdau_df'),
    methods::is(jednostki, 'jednostki_df'),
    methods::is(kierunki, 'kierunki_df')
  )

  slEtapy = c('1' = 'slic', '2' = 'smgr', 'JM' = 'smgr', '3' = 'sdr', 'DR' = 'dr', 'HAB' = 'hab', 'PROF' = 'prof')
  dlugie = zdau %>%
    filter_(~typ == 'A') %>%
    mutate_(etap = ~slEtapy[poziom]) %>%
    select_('id', 'data_od', 'data_do', 'kierunek_id', 'jednostka_id', 'etap') %>%
    group_by_('id', 'etap') %>%
    arrange_('desc(data_od)') %>%
    filter_(~row_number() == 1) %>%
    left_join(
      jednostki %>%
        select_('jednostka_id')
    ) %>%
    left_join(
      kierunki %>%
        select_('kierunek_id', 'dysc_kod', 'dzie_kod', 'obsz_kod', 'liczba_semestrow')
    ) %>%
    mutate_(
      if_wterminie = ~(data_do - data_od) / 6 < liczba_semestrow + 1
    ) %>%
    select_('-liczba_semestrow')

  kariera = data.frame(id = numeric(0))
  for (i in unique(slEtapy)) {
    tmp = dlugie %>%
      filter_(~etap %in% i)
    names(tmp) = paste0(names(tmp), '_', i)
    names(tmp)[1] = 'id'
    kariera = kariera %>%
      full_join(tmp)
  }

  sdr = zdau %>%
    filter_(~poziom %in% '3' & typ %in% 'A') %>%
    group_by_('id', 'kierunek_id', 'jednostka_id') %>%
    arrange_('data_od') %>%
    filter_(~row_number() == 1) %>%
    select_('id', 'kierunek_id', 'jednostka_id', 'data_do') %>%
    rename_(
      data_pow_sdr = 'data_do',
      kierunek_id_dr = 'kierunek_id',
      jednostka_id_dr = 'jednostka_id'
    )

  dr = zdau %>%
    filter_(~poziom %in% 'DR') %>%
    group_by_('id', 'kierunek_id', 'jednostka_id') %>%
    arrange_('data_od') %>%
    filter_(~row_number() == 1) %>%
    select_('id', 'kierunek_id', 'jednostka_id', 'data_od') %>%
    rename_(
      data_pow_dr = 'data_od',
      kierunek_id_sdr = 'kierunek_id',
      jednostka_id_sdr = 'jednostka_id'
    )

  kariera = kariera %>%
    left_join(sdr) %>%
    left_join(dr) %>%
    mutate_(
      if_dr_po_sdr = ~ifelse(!is.na(data_pow_sdr) & !is.na(data_od_dr) & data_od_dr >= data_pow_sdr, TRUE, ifelse(!is.na(data_od_dr), FALSE, NA)),
      data_pow_dr  = ~ifelse(!is.na(data_pow_dr) & !is.na(data_do_sdr) & data_pow_dr >= data_do_sdr, data_pow_dr, NA_integer_)
    ) %>%
    select_('-data_pow_sdr')

  stopifnot(
    sum(duplicated(kariera$id)) == 0
  )

  class(kariera) = c('kariera_df', class(kariera))
  return(kariera)
}

#
#tmp = c(rep('1', 3), rep('2', 3), 'JM', rep('3', 4), rep('DR', 3), rep('HAB', 2),'PROF')
#zdau$poziom = tmp[runif(nrow(zdau), 1, 17)]
