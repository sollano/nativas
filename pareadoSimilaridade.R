p.similaridade=function(x, y, rotuloNI = "NI", indice = "both"){
  
  # Remover observações cuja espécie é desconhecida
  semNI1 = x[x != rotuloNI]
  
  # Encontrar o número de espéciue que ocorrem na parcela
  a = length(unique(semNI1))
  
  semNI2 = y[y != rotuloNI]
  
  b = length(unique(semNI2))
  
  c = length(intersect(unique(semNI1), unique(semNI2)))
  
  SJ = round(c / (a+b-c), 2)
  
  SO = round(2*c/(a+b), 2)
  
  if(indice == "both"){
    
    return(c(SJ, SO))
    
  } else if (indice == "Sorensen"){
    
    return(SO)
    
  } else if (indice == "Jaccard"){
    
    return(SJ)
  
  } else {
    
    return(c(SJ, SO))
  }
}