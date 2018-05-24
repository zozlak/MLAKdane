#' Porównuje ze sobą wskazane zbiory
#' @param z1 pierwszy zbiór do porównania
#' @param z2 drugi zbiór do porównania
#' @param wspolne lista kolumn, po których porównywane zbiory będą łączone
#' @param pominNumInt nie zgłaszaj różnic typów gdy zmienna w \code{z1} jest
#'   typu \code{numeric}, a zmienna w \code{z2} typu \code{integer}
#' @export
porownaj_zbiory = function(z1, z2, wspolne = c('ID_ZDAU', 'id_zdau', 'ID', 'id', 'OKRES', 'okres'), pominNumInt = TRUE) {
  zmienne = intersect(names(z1), names(z2))
  wspolne = intersect(wspolne, zmienne)

  tmp = setdiff(names(z1), zmienne)
  if (length(tmp) > 0) {
    cat('tylko z1\n')
    print(tmp)
  }
  tmp = setdiff(names(z2), zmienne)
  if (length(tmp) > 0) {
    cat('tylko z2\n')
    print(tmp)
  }

  names(z1) = paste0(names(z1), '_1')
  names(z2) = paste0(names(z2), '_2')
  for (i in wspolne) {
    names(z1) = sub(paste0('^', i, '_1$'), i, names(z1))
    names(z2) = sub(paste0('^', i, '_2$'), i, names(z2))
  }

  d = full_join(z1, z2)
  stopifnot(nrow(d) <= nrow(z1) + nrow(z2))

  for (zm in setdiff(zmienne, wspolne)) {
    zm1 = paste0(zm, '_1')
    zm2 = paste0(zm, '_2')
    numInt = class(unlist(d[, zm1]))[1] == 'numeric' & class(unlist(d[, zm2]))[1] == 'integer'
    if (class(unlist(d[, zm1])) != class(unlist(d[, zm2])) & (!numInt | !pominNumInt)) {
      print(paste(zm, class(unlist(d[, zm1]))[1],  class(unlist(d[, zm2])))[1])
    }
    tryCatch(
      {
        if (class(unlist(d[, zm1])) == 'numeric') {
          tmp = abs(d[, zm1] - d[, zm2]) < 0.000001 | is.na(d[, zm1]) & is.na(d[, zm2])
        } else {
          tmp = d[, zm1] == d[, zm2] | is.na(d[, zm1]) & is.na(d[, zm2])
        }
        tmp[is.na(tmp)] = FALSE
        if (!all(tmp)) {
          print(paste(zm, sum(!tmp)))
        }
      },
      error = function(e) {
        print(paste0(zm, ' exception'))
      }
    )
  }

  return(as.tbl(d))
}