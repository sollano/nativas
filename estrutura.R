estrutura = function(data, col.especies, col.dap, col.parcelas, area.parcela, est.vertical = NA, est.interno = NA, nao.identificada = "NI"){
    SPECIES = col.especies
    DBH = col.dap
    PLOTS = col.parcelas
    # alterei aqui para areaplot poder ser uma coluna do data frame
    if(is.numeric(area.parcela) ){AREA.PLOT = area.parcela}else(AREA.PLOT = mean(data[,area.parcela],na.rm = T ) )
    
    # Coloquei estes dois if statements, para que o usuario possa deixar
    # de preencher a variavel, e a funcao continue rodando
    # (adicionei o "" por causa do app)
    if(missing(est.vertical)||is.null(est.vertical)||est.vertical==F||est.vertical==""){
       est.vertical = NA }
    
    if(missing(est.interno)||is.null(est.interno)||est.interno==F||est.interno==""){
      est.interno = NA }
    
    
    VERTICAL = est.vertical
    INTERNA = est.interno
    NI = nao.identificada
    
    # Converter variaveis categoricas em fatores
    data[,PLOTS] <- as.factor(data[,PLOTS])
    data[,SPECIES] <- as.factor(data[,SPECIES])
    
    # converter rotulos NI (aplicativo)
    if(is.null(NI)){NI <- "NI"}
    
    # Ajustar formato categórico
    
    # tive que colocar estes if statements aqui tambem,
    # para caso as variaveis opcionais nao sejam inseridas
    if(!is.na(est.vertical)){
      
      data[,VERTICAL] = as.factor(data[,VERTICAL])

    }
    
    if(!is.na(est.interno)){
      
      data[,INTERNA] = as.factor(data[,INTERNA])
      
    }
    
    # Remover NA
    data = data[!is.na(data[SPECIES]),]
    data = data[!is.na(data[DBH]),]
    
    # Remover NI
    # modifiquei para aceitar multiplas entradas
    data = data[!data[,SPECIES] %in% NI,]
    espList = levels(factor(data[,SPECIES]))
    
    # Constroi tabela de frequencia
    pivot = data.frame(table(data[SPECIES]))
    names(pivot) = c("especie", "sum")
    pivot = pivot[which(pivot$especie %in% espList),]
    
    # Calcula número de parcelas na área de estudo
    nplots = length(unique(data[,PLOTS]))
    
    # Estrutura horizontal
    # Calcula frequencia absoluta e relativa
    for (i in levels(data[,PLOTS])){
        tableFreq = data.frame(table(data[data[PLOTS] == i,SPECIES]))
        pivot = cbind(pivot, tableFreq[which(tableFreq[,1] %in% espList),2])
        names(pivot)[ncol(pivot)] = i
    }    
    
    AcFAi = 0
    FA = 0
    for (i in seq(1, nrow(pivot), 1)){
        contagem = pivot[i,-c(1,2)] > 0
        cplots = length(contagem[contagem == TRUE])
        FAi = cplots/nplots * 100
        AcFAi = AcFAi + FAi
        FA[i] = FAi
    }
    
    result = pivot[1]
    result["FA"] = round(FA, 4)

    FR = FA / AcFAi * 100
    result["FR"] = round(FR, 4)
    
    # Calcula densidade absoluta e relativa
    # Alterei aqui para a area poder ser inserida em m2
    DA = pivot[2] / (nplots * (AREA.PLOT/10000) )
    result["DA"] = round(DA, 4)

    AcDAi = sum(DA)    
    DR = DA / AcDAi * 100
    result["DR"] = round(DR, 4)
    
    # Calcula dominância absoluta e relativa
    
    data["AB"] = data[DBH]^2 * pi / 40000
    AB = tapply(data[,"AB"], data[,SPECIES], sum)
    AB = AB[which(names(AB) %in% espList)]
    
    # Alterei aqui para a area poder ser inserida em m2
    DoA = AB / (nplots * (AREA.PLOT/10000) )
    result["DoA"] = round(DoA, 6)
    
    AcDoAi = sum(DoA)
    DoR = DoA / AcDoAi * 100
    result["DoR"] = round(DoR, 6)
    rm(AB, AcDAi, AcDoAi, AcFAi, cplots, DoA, DoR, FA, FAi, FR, DA, DR, tableFreq, i, contagem)
    
    if (!is.na(est.vertical)){
      # Estrutura vertical
      
      vert = pivot["especie"]
      for (j in levels(data[,VERTICAL])){
          daVert = data.frame(table(data[data[VERTICAL] == j, SPECIES]))
          vert = cbind(vert, daVert[which(daVert[,1] %in% espList),2])
      }
      names(vert)[-1] = levels(data[,VERTICAL])
      
      VFj = data.frame()
      for (j in levels(data[,VERTICAL])){
        VFj[1,j] = sum(vert[, j]) / sum(vert[, seq(2,length(levels(data[,VERTICAL]))+1,1)]) * 100
      }
      
      for (j in levels(data[,VERTICAL])){
          for (i in seq(1, nrow(vert), 1)){
              vert[i, paste("VF", j, sep = "")] = vert[i, j] * VFj[1, j]
              result[i, paste("VF", j, sep = "")] = vert[i, j] * VFj[1, j]
          }
      }
  
      AcPSAi = 0
      for (i in seq(1, nrow(vert), 1)){
          PSAi = 0
          for (j in levels(data[,VERTICAL])){
              
              PSAi = PSAi + VFj[1, j] * vert[i, j] 
          }
          vert[i, "PSA"] = PSAi
          AcPSAi = AcPSAi + PSAi
      }
      
      result["PSA"] = vert["PSA"]
      result["PSR"] = vert["PSA"] / AcPSAi * 100
      rm(AcPSAi, i, j, PSAi, VFj, daVert, vert)
    }
    
    if (!is.na(est.interno)){
      
      # Estrutura Interna
      
      intern = pivot["especie"]
      for (j in levels((data[,INTERNA]))){
          daInter = data.frame(table(data[data[INTERNA] == j, SPECIES]))
          intern = cbind(intern, daInter[which(daInter[,1] %in% espList),2])
      }
      names(intern)[-1] = levels(data[,INTERNA])
      
      for (j in levels(data[,INTERNA])){
          for (i in seq(1, nrow(intern), 1)){
              intern[i, paste("QF", j, sep = "")] = intern[i, j] * (sum(intern[,j]) / sum(intern[, seq(2,length(levels(data[,INTERNA]))+1,1)]))
              result[i, paste("QF", j, sep = "")] = intern[i, j] * (sum(intern[,j]) / sum(intern[, seq(2,length(levels(data[,INTERNA]))+1,1)]))
          }
      }
      
      AcQFAi = 0
      for (i in seq(1, nrow(intern), 1)){
          intern[i, "QFA"] = sum(intern[i, seq(2+length(levels(data[,INTERNA])),2*length(levels(data[,INTERNA]))+1,1)])
          AcQFAi = AcQFAi + intern[i, "QFA"]
      }
      
      result["QFA"] = intern["QFA"]
      result["QFR"] = intern["QFA"] / AcQFAi * 100
      rm(daInter, AcQFAi, i, j, intern)
    }
    rm(pivot)
    return(result)
}    