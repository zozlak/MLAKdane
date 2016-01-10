uzupelnij_obserwacje = function(wynik, wzor, id = 'id_zdau'){
  stopifnot(
    is(wynik, 'data.frame'),
    is(wzor, 'data.frame')
  )
  wzor %>%
    select_(.dots = id) %>%
    distinct() %>%
    left_join(wynik) %>%
    return()
}