m.similaridade=function(data, col.especies, col.comparison, rotulo.NI = "NI", indice = "both"){
  
  # Remover NA
  data = data[!is.na(data[col.especies]),]
  data = data[!is.na(data[col.comparison]),]
  
  # converter rotulos NI (aplicativo)
  if(is.null(rotulo.NI)){rotulo.NI <- "NI"}
  
  # Remover observações cuja espécie é desconhecida
  # modifiquei para aceitar multiplas entradas
  semNI = data[! data[ ,col.especies] %in% rotulo.NI,]
  
  # Converter variaveis categoricas em fatores
  data[,col.especies] <- as.factor(data[,col.especies])
  data[,col.comparison] <- as.factor(data[,col.comparison])
  
  compair = levels(data[,col.comparison])
  
  SO = matrix(1, nrow = length(compair), ncol = length(compair))
  SJ = matrix(1, nrow = length(compair), ncol = length(compair))
  for (p in seq(1, length(compair)-1,1)){
    for (r in seq(p+1, length(compair),1)){
      # Encontrar o número de espéciue que ocorrem na parcela
      a = length(unique(semNI[semNI[,col.comparison] == compair[p], col.especies]))
    
      b = length(unique(semNI[semNI[,col.comparison] == compair[r], col.especies]))
      
      c = length(intersect(unique(semNI[semNI[,col.comparison] == compair[p], col.especies]),
                           unique(semNI[semNI[,col.comparison] == compair[r], col.especies])))
      
      SJ[p, r] = round(c / (a+b-c), 2)
      SJ[r, p] = round(c / (a+b-c), 2)
      
      SO[p, r] = round(2 * c / (a+b), 2)
      SO[r, p] = round(2 * c / (a+b), 2)
    }
  }
  if(indice == "both"){

    return(list(SJ, SO))
    
  } else if (indice == "Sorensen"){

    return(SO)
    
  } else if (indice == "Jaccard"){
    
    return(SJ)
  
  } else {
    
    return(list(SJ, SO))
    
  }
}