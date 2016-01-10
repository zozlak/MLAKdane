#' Zamienia kody powiatów z kodów BDL na kody TERYT
#' @param id wektor kodów z BDL
bdl2teryt = function(id){
  return(
    as.numeric(
      paste0(
        substring(id, 2, 3),
        substring(id, 6, 7)
      )
    )
  )
}