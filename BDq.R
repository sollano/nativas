bdq.meyer = function(data, col.parcelas, col.dap, area.parcela, intervalo.classe = 5, min.dap = 5, i.licourt = 1.3){

  INTERVALO.CLASSE = intervalo.classe
  DBH = col.dap
  DBH.MIN = min.dap
  PLOTS = col.parcelas
  # alterei aqui para areaplot poder ser uma coluna do data frame
  if(is.numeric(area.parcela) ){AREA.PLOT = area.parcela}else(AREA.PLOT = mean(data[,area.parcela],na.rm = T ) )
  LICOURT = i.licourt
  
  # Remover NA
  data = data[!is.na(data[DBH]),]
  
  # Calcula número de parcelas na área de estudo
  nplots = length(unique(data[,PLOTS]))
  
  # Estrutura diametrica
  
  data[,"Classe"] = ceiling(data[,DBH] /  INTERVALO.CLASSE)
  data[, "CentroClasse"] = data[,"Classe"] * INTERVALO.CLASSE - (INTERVALO.CLASSE / 2)
  
  freq = data.frame(table(data[,"Classe"]))
  DD = data.frame(Classe = as.numeric(freq[,1]))
  DD$CentroClasse = DD$Classe * INTERVALO.CLASSE - (INTERVALO.CLASSE / 2)
  DD$NumIndv = freq[,2]
  # Alterei aqui para a area poder ser inserida em m2
  DD$IndvHectare = round(DD$NumIndv / ((AREA.PLOT/10000) * nplots), 1)
  DD = DD[DD$CentroClasse >= DBH.MIN,]
  DD = DD[DD$IndvHectare > 0,]
  rm(freq)
  
  # Meyer
  meyer = lm(log(DD$IndvHectare) ~ DD$CentroClasse)
  DD$Meyer = round(exp(predict(meyer)), 0)
  
  # # Mervart
  # mervart = lm(log(DD$IndvHectare) ~ log(DD$CentroClasse))
  # DD$Mervart = round(exp(predict(mervart)), 0)
 
  # Licourt atual
  q = 0
  for (i in seq(1, length(DD$CentroClasse)-1,1)){
    q[i] = DD$IndvHectare[i] / DD$IndvHectare[i+1]
  }
  q[length(DD$CentroClasse)] = NA
  DD$q = round(q, 1)
  rm(q)
  
  # DBq base meyer
  
  # Calcula b1 do modelo de Meyer
  b1 = round(log(LICOURT)/(- INTERVALO.CLASSE), 6)
  
  # Calcula b0 do modelo de Meyer
  temp.b0 = DD$CentroClasse^2 * exp(b1 * DD$CentroClasse)
  sum.temp.b0 = sum(temp.b0)
  areaBasal = (DD$CentroClasse^2 * pi / 40000) * (DD$IndvHectare)
  b0 = log(40000 * sum(areaBasal) / (pi * sum.temp.b0))
  rm(temp.b0, sum.temp.b0, areaBasal)
  
  # Calcula a distribuição diamétrica balanceada com base no modelo de Meyer
  DD$MeyerBalan = round(exp(b0 + b1 * DD$CentroClasse), 0)
  
  result = list(DD, meyer, c(b0, b1))
  return(result)
}
