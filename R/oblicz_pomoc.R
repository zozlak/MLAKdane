#' oblicza zmienne opisujące pomoc materialną
#' @param okienko dane wygenerowane za pomocą funkcji
#'   \code{\link{oblicz_okienko}} na danych jednostkowych (tzn. danych
#'   wygenerowanych wcześniej funkcją \code{\link{przygotuj_pomoc}})
#' @param zdau dane wygenerowane za pomocą funkcji \code{\link{przygotuj_zdau}}
#' @param multidplyr czy obliczać na wielu rdzeniach korzystając z pakietu
#'   multidplyr
#' @return data.frame wyliczone zmienne
#' @export
#' @import dplyr
oblicz_pomoc = function(okienko, zdau, multidplyr = TRUE) {
  stopifnot(
    methods::is(okienko, 'okienko_df') & methods::is(okienko, 'pomoc_df'),
    methods::is(zdau, 'zdau_df')
  )

  if (multidplyr) {
    okienko = multidplyr::partition(okienko, pom_id)
  } else {
    okienko = group_by_(okienko, 'pom_id')
  }

  okienko = okienko %>%
    summarize_(
      nmpom = ~n(),
      nmpom_dr      = ~sum(pom_rodz_pom %in% 1),
      nmpom_naj     = ~sum(pom_rodz_pom %in% 2),
      nmpom_nspraw  = ~sum(pom_rodz_pom %in% 3),
      nmpom_sdr     = ~sum(pom_rodz_pom %in% 4),
      nmpom_stypsoc = ~sum(pom_rodz_pom %in% 5),
      nmpom_wyb     = ~sum(pom_rodz_pom %in% 6),
      nmpom_zap     = ~sum(pom_rodz_pom %in% 7),
      nmpom_zsdr    = ~sum(pom_rodz_pom %in% 8),
      pom_len       = ~first(pom_len)
    ) %>%
    mutate_( # do poprawy - pmpom* powinny uwzględniać zachodzenie na siebie stypendiów
      pmpom         = ~nmpom / pom_len,
      pmpom_dr      = ~pmpom_dr / pom_len,
      pmpom_naj     = ~pmpom_naj / pom_len,
      pmpom_nspraw  = ~pmpom_nspraw / pom_len,
      pmpom_sdr     = ~pmpom_sdr / pom_len,
      pmpom_stypsoc = ~pmpom_stypsoc / pom_len,
      pmpom_wyb     = ~pmpom_wyb / pom_len,
      pmpom_zap     = ~pmpom_zap / pom_len,
      pmpom_zsdr    = ~pmpom_zsdr / pom_len
    ) %>%
    collect() %>%
    ungroup() %>%
    rename_(id = 'pom_id')

  okienko = zdau %>%
    select_('id', 'id_zdau') %>%
    left_join(okienko) %>%
    mutate_(
      nmpom         = ~coalesce(nmpom, 0L),
      nmpom_dr      = ~coalesce(nmpom_dr, 0L),
      nmpom_naj     = ~coalesce(nmpom_naj, 0L),
      nmpom_nspraw  = ~coalesce(nmpom_nspraw, 0L),
      nmpom_sdr     = ~coalesce(nmpom_sdr, 0L),
      nmpom_stypsoc = ~coalesce(nmpom_stypsoc, 0L),
      nmpom_wyb     = ~coalesce(nmpom_wyb, 0L),
      nmpom_zap     = ~coalesce(nmpom_zap, 0L),
      nmpom_zsdr    = ~coalesce(nmpom_zsdr, 0L),
      pom_len       = ~coalesce(pom_len, 0L),
      pmpom         = ~coalesce(pmpom, 0L),
      pmpom_dr      = ~coalesce(pmpom_dr, 0L),
      pmpom_naj     = ~coalesce(pmpom_naj, 0L),
      pmpom_nspraw  = ~coalesce(pmpom_nspraw, 0L),
      pmpom_sdr     = ~coalesce(pmpom_sdr, 0L),
      pmpom_stypsoc = ~coalesce(pmpom_stypsoc, 0L),
      pmpom_wyb     = ~coalesce(pmpom_wyb, 0L),
      pmpom_zap     = ~coalesce(pmpom_zap, 0L),
      pmpom_zsdr    = ~coalesce(pmpom_zsdr, 0L)
    )

  class(okienko) = c('pomoc_mies_df', class(okienko))
  return(okienko)
}
