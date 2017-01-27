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
oblicz_publikacje = function(okienko, zdau, multidplyr = TRUE) {
  stopifnot(
    methods::is(okienko, 'okienko_df') & methods::is(okienko, 'publikacje_df'),
    methods::is(zdau, 'zdau_df')
  )

  if (multidplyr) {
    okienko = multidplyr::partition(okienko, pub_id)
  } else {
    okienko = group_by_(okienko, 'pub_id')
  }

  okienko = okienko %>%
    summarize_(
      pkt         = ~sum(pub_punkty),
      pkt_art     = ~sum(pub_punkty[pub_publ_typ %in% 'A']),
      pkt_art_a   = ~sum(pub_punkty[pub_publ_typ %in% 'A' & pub_segment %in% 'A']),
      pkt_art_b   = ~sum(pub_punkty[pub_publ_typ %in% 'A' & pub_segment %in% 'B']),
      pkt_art_c   = ~sum(pub_punkty[pub_publ_typ %in% 'A' & pub_segment %in% 'C']),
      n_art       = ~n(),
      n_art_a     = ~sum(pub_segment %in% 'A'),
      n_art_b     = ~sum(pub_segment %in% 'B'),
      n_art_c     = ~sum(pub_segment %in% 'C'),
      n_art_sam   = ~sum(pub_n_autor %in% 1),
      n_art_sam_a = ~sum(pub_n_autor %in% 1 & pub_segment %in% 'A'),
      n_art_sam_b = ~sum(pub_n_autor %in% 1 & pub_segment %in% 'B'),
      n_art_sam_c = ~sum(pub_n_autor %in% 1 & pub_segment %in% 'C'),
      n_ks        = ~sum(pub_publ_typ %in% 'K'),
      n_rozdz     = ~sum(pub_publ_typ %in% 'R')
    ) %>%
    collect() %>%
    ungroup() %>%
    rename_(id = 'pub_id')

  okienko = zdau %>%
    select_('id', 'id_zdau') %>%
    left_join(okienko) %>%
    mutate_(
      pkt         = ~coalesce(pkt, 0L),
      pkt_art     = ~coalesce(pkt_art, 0L),
      pkt_art_a   = ~coalesce(pkt_art_a, 0L),
      pkt_art_b   = ~coalesce(pkt_art_b, 0L),
      pkt_art_c   = ~coalesce(pkt_art_c, 0L),
      n_art       = ~coalesce(n_art, 0L),
      n_art_a     = ~coalesce(n_art_a, 0L),
      n_art_b     = ~coalesce(n_art_b, 0L),
      n_art_c     = ~coalesce(n_art_c, 0L),
      n_art_sam   = ~coalesce(n_art_sam, 0L),
      n_art_sam_a = ~coalesce(n_art_sam_a, 0L),
      n_art_sam_b = ~coalesce(n_art_sam_b, 0L),
      n_art_sam_c = ~coalesce(n_art_sam_c, 0L),
      n_ks        = ~coalesce(n_ks, 0L),
      n_rozdz     = ~coalesce(n_rozdz, 0L)
    )

  class(okienko) = c('publikacje_mies_df', class(okienko))
  return(okienko)
}
