uzgodnij_teryt = function(teryt){
  potega = 0
  while(length(unique(teryt)) > 1){
    teryt = floor(teryt / 100)
    potega = potega + 2
  }
  return(teryt[1] * 10^potega)
}