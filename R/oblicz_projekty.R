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
oblicz_projekty = function(okienko, zdau, multidplyr = TRUE) {
  stopifnot(
    methods::is(okienko, 'okienko_df') & methods::is(okienko, 'projekty_df'),
    methods::is(zdau, 'zdau_df')
  )

  if (multidplyr) {
    okienko = multidplyr::partition(okienko, proj_id)
  } else {
    okienko = group_by_(okienko, 'proj_id')
  }

  okienko = okienko %>%
    summarize_(
      nproj_fin   = ~sum(proj_zaakc),
      nproj_rozl  = ~sum(proj_rozliczenie),
      nproj_rozp  = ~n(),
      # nproj_zak   = ,
      # nproj_zgl   = ,
      sfproj_rozp = ~sum(proj_kwota[proj_zaakc]),
      proj_len    = ~first(proj_len)
    ) %>%
    mutate_(
      eproj_fin   = ~nproj_fin / proj_len,
      eproj_rozl  = ~nproj_rozl / proj_len,
      eproj_rozp  = ~nproj_rozp / proj_len,
      # eproj_zak   = ,
      # eproj_zgl   = ,
      efproj_rozp = ~sfproj_rozp / proj_len
      # pfproj      =
    ) %>%
    select_('-proj_len') %>%
    collect() %>%
    ungroup() %>%
    rename_(id = 'proj_id')

  okienko = zdau %>%
    select_('id', 'id_zdau') %>%
    left_join(okienko) %>%
    mutate_(
      nproj_fin   = ~coalesce(nproj_fin, 0L),
      nproj_rozl  = ~coalesce(nproj_rozl, 0L),
      nproj_rozp  = ~coalesce(nproj_rozp, 0L),
      # nproj_zak   = ,
      # nproj_zgl   = ,
      sfproj_rozp = ~coalesce(sfproj_rozp, 0L),
      eproj_fin   = ~coalesce(eproj_fin, 0),
      eproj_rozl  = ~coalesce(eproj_rozl, 0),
      eproj_rozp  = ~coalesce(eproj_rozp, 0),
      # eproj_zak   = ,
      # eproj_zgl   = ,
      efproj_rozp = ~coalesce(efproj_rozp, 0)
      # pfproj      =
    )

  class(okienko) = c('projekty_mies_df', class(okienko))
  return(okienko)
}
