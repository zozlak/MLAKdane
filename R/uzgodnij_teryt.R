#' uzgadnia TERYT
#' @description
#' W razie niezgodności zwraca najbliższą wspólną JST (powiat, województwo bądz
#' Polskę)
#' @param teryt wektor kodów TERYT
#' @return numeric uzgodniony kod TERYT
#' @export
uzgodnij_teryt = function(teryt){
  potega = 0
  teryt = teryt[!is.na(teryt) & teryt > 0]
  if(length(teryt) == 0){
    return(0)
  }
  while(length(unique(teryt)) > 1){
    teryt = floor(teryt / 100)
    potega = potega + 2
  }
  return(teryt[1] * 10^potega)
}