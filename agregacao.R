agregacao = function(data, col.especies, col.parcelas, rotulo.NI = "NI"){
  SPECIES = col.especies
  PLOTS = col.parcelas
  NI = rotulo.NI  
  
  # Remover NA
  data = data[!is.na(data[SPECIES]),]
  
  # converter rotulos NI (aplicativo)
  if(is.null(NI)){NI <- "NI"}

  # Remover NI
  # modifiquei para aceitar multiplas entradas
  data = data[! data[,SPECIES] %in% NI,]
  espList = levels(factor(data[,SPECIES]))
  
  # Constroi tabela de frequencia
  pivot = data.frame(table(data[SPECIES]))
  names(pivot) = c("especie", "sum")
  pivot = pivot[which(pivot$especie %in% espList),]
  
  # Calcula número de parcelas na área de estudo
  nplots = length(unique(data[,PLOTS]))
  
  # Qui-quadrado tabelado para indice de Hazen
  chisq75 = qchisq(0.75, nplots - 1)
  chisq99 = qchisq(0.99, nplots - 1)
  
  for (i in levels(data[,PLOTS])){
    tableFreq = data.frame(table(data[data[PLOTS] == i,SPECIES]))
    pivot = cbind(pivot, tableFreq[which(tableFreq[,1] %in% espList),2])
    names(pivot)[ncol(pivot)] = i
  } 
  
  agreg = pivot[1]
  if(nplots > 3){
    for (i in seq(1, length(pivot[,1]))){ 
      Si = var(as.numeric(pivot[i, seq(3, (2 + nplots), 1)]))
      Mi = mean(as.numeric(pivot[i, seq(3, (2 + nplots), 1)]))
      agreg[i,"Payandeh"] = round(Si/Mi, 1)
      if(round(Si/Mi, 1) == 1){
        agreg[i, "Pay.res"] = "Aleatório"
      } else if(round(Si/Mi, 1) < 1) {
        agreg[i, "Pay.res"] = "Regular"
      } else {
        agreg[i, "Pay.res"] = "Agregado"
      }
      
      agreg[i,"Hazen"] = round(Si/Mi * (nplots - 1), 1)
      if(round(Si/Mi * (nplots - 1), 1) > chisq99){
        agreg[i, "Haz.res"] = "Agregado"
      } else if(round(Si/Mi * (nplots - 1), 1) < chisq75) {
        agreg[i, "Haz.res"] = "Não agregado"
      } else {
        agreg[i, "Haz.res"] = "Tende ao agregado"
      }
      
      if ( (as.numeric(pivot[i, 2]) * (as.numeric(pivot[i, 2])-1)) != 0){
        agreg[i,"Morisita"] = round((sum(as.numeric(pivot[i, seq(3, (2 + nplots), 1)]) * (as.numeric(pivot[i, seq(3, (2 + nplots), 1)]) - 1))) / (as.numeric(pivot[i, 2]) * (as.numeric(pivot[i, 2])-1)) * nplots, 1)
      } else {
        agreg[i,"Morisita"] = round(0, 0)
      }
      if(agreg[i,"Morisita"] == 1){
        agreg[i, "Mor.res"] = "Aleatório"
      } else if(agreg[i,"Morisita"] < 1 & agreg[i,"Morisita"] > 0) {
        agreg[i, "Mor.res"] = "Regular"
      } else if(agreg[i,"Morisita"] == 0){
        agreg[i, "Mor.res"] = "Rara"
      } else {
        agreg[i, "Mor.res"] = "Agregado"
      }
    }
    return(agreg)
  } else {
   return("Baixo número de parcelas") 
  }
}