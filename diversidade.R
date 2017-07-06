diversidade = function(data, col.especies, col.parcelas, rotulo.NI = "NI", indice){
  
  # Remover NA
  data <- data[!is.na(data[col.especies]),]
  
  # converter rotulos NI (aplicativo)
  if(is.null(rotulo.NI)){rotulo.NI <- "NI"}
  
  # Remover NI (modifiquei para aceitar multiplas)
  #semNI = data[ ! data[,col.especies] %in% rotulo.NI, col.especies]
  semNI = data[ ! data %in% rotulo.NI ]
  
  ESPECIES <- semNI[col.especies]
  
  # Condicional: Se o usuario nao fornecer a variavel parcela,
  # cria-se um vetor vazio com o mesmo numero de linhas chamado parcela
  if(missing(col.parcelas) || is.null(col.parcelas) || is.na(col.parcelas) || col.parcelas == ""){
    
    PARCELAS <- vector("character", nrow(ESPECIES) )
    
    # transformar argumento vazio em NA, para evitar erros
    col.parcelas <- NA
    
  }else{ # caso contrario, cria-se um objeto que contem a variavel parcela
    PARCELAS <- semNI[col.parcelas]
  }
  
  # Com a funcao by calcula-se os indices por PARCELAS;
  # caso col.parcelas nao tenha sido fornecido, PARCELAS sera um vetor vazio,
  # e o calculo sera feito considerando todo o dado.
  tab_indices <- by(ESPECIES, PARCELAS , function(x){
    
    tableFreq = table(x)
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
    
    tab_final <- data.frame(Shannon = H, Simpson = S, EqMaxima = Hmax, Piellou = J, Jentsch = QM)
    
    return(tab_final)
    
  } )
  
  # transforma-se o objeto de classe by criado em um dataframe
  tab_indices <- data.frame(do.call(rbind, tab_indices))
  
  
  # converter nomes das linhas em coluna, caso os calculos tenham sido feitos por PARCELAS
  if( !is.na(col.parcelas) ){
    
    tab_indices <- cbind(aux = row.names(tab_indices), tab_indices)
    
    names(tab_indices)[names(tab_indices) == "aux"] <- col.parcelas
    row.names(tab_indices) <- NULL
  }
  

  if (missing(indice)){
    return(tab_indices)
  } else if (indice == "H"){
    return( tab_indices$Shannon )
  } else if (indice == "S"){
    return(tab_indices$Simpson)
  } else if (indice == "Hmax"){
    return(tab_indices$EqMaxima)
  } else if (indice == "J"){
    return(tab_indices$Piellou)
  } else if (indice == "QM"){  
    return(tab_indices$Jentsch)
  } else {
    return(tab_indices)
  }
}