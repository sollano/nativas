diversidade = function(data, col.especies, rotulo.NI = "NI", indice = NA){
  
  # Remover NA
  data = data[!is.na(data[col.especies]),]

  # Remover NI
  semNI = data[data[,col.especies]!=rotulo.NI, col.especies]
  
  # Calcula tabela de frequencia
  tableFreq = table(semNI)
  tableP = data.frame(tableFreq)
  names(tableP) = c("especie", "freq")
  
  # Calcula número de indivíduos na amostra
  #N = length(semNI)
  N = sum(tableP$freq)
  
  # Calcula a proporção de cada espécie
  tableP$p = tableP$freq / N
  
  # Calcula o log da proporção de cada espécie
  tableP$lnp = log(tableP$p)
  tableP[tableP$lnp  == "-Inf", "lnp"] = 0
  
  # Número de espécies amostradas
  Sesp = length(tableP[tableP$freq > 0, "especie"])
  
  # Calcula Shannon
  H = round(- sum(tableP$p * tableP$lnp), 2)
  
  #Calcula Simpson
  S = round(1 - (sum(tableP$freq*(tableP$freq - 1))/(N*(N-1))), 2)
  
  # Diversidade Máxima
  Hmax = round(log(length(tableP$freq[tableP$freq>0])), 2)
  
  # Equabilidade de Pielou
  J = round(H / Hmax, 2)
  
  # Coeficiente de mistura de Jentsch
  QM = round(Sesp / N, 2)
  
  if (is.na(indice)){
    return(data.frame(Shannon = H, Simpson = S, EqMaxima = Hmax, Piellou = J, Jentsch = QM))
  } else if (indice == "H"){
    return(H)
  } else if (indice == "S"){
    return(S)
  } else if (indice == "Hmax"){
    return(Hmax)
  } else if (indice == "J"){
    return(J)
  } else if (indice == "QM"){  
    return(QM)
  } else {
    return(data.frame(Shannon = H, Simpson = S, EqMaxima = Hmax, Piellou = J, Jentsch = QM))
  }
}