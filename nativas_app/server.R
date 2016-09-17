
library(shiny)
library(DT)
#library(plyr)
library(tidyr)
library(dplyr)
library(lazyeval)
library(ggplot2)
library(ggdendro)
library(ggthemes)
library(xlsx)
library(xlsxjars)
library(markdown)

ex <- read.csv("examples/cauaxi_parc10000m2.csv")


# Funcoes Nativas ####

agregacao = function(data, col.especies, col.parcelas, rotulo.NI = "NI"){
  SPECIES = col.especies
  PLOTS = col.parcelas
  NI = rotulo.NI  
  
  # Remover NA
  data = data[!is.na(data[SPECIES]),]
  
  # Remover NI
  data = data[data[SPECIES] != NI,]
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
  data = data[data[SPECIES] != NI,]
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

p.similaridade=function(x, y, rotuloNI = "NI", indice = "both"){
  
  # Remover observações cuja espécie é desconhecida
  semNI1 = x[x != rotuloNI]
  semNI1 = x[!is.na(x)]
  
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

m.similaridade=function(data, col.especies, col.comparison, rotulo.NI = "NI", indice = "both"){
  
  # Remover NA
  data = data[!is.na(data[col.especies]),]
  data = data[!is.na(data[col.comparison]),]
  
  # Remover observações cuja espécie é desconhecida
  semNI = data[data[ ,col.especies] != rotulo.NI,]
  
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


# Funcoes Inventario ####
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

inv_summary <- function(df,DAP, HT, VCC, area_parcela, groups, area_total,idade,VSC,Hd) {
  
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  if( missing(df)||is.null(df)||df==F)
  {stop("please insert data frame")}
  
  if(missing(DAP)||is.null(DAP)||DAP==F||DAP=="")
  {stop("please insert diameter variable")}
  
  if(missing(HT)||is.null(HT)||HT==F||HT=="")
  {stop("please insert height variable")}
  
  if(missing(VCC)||is.null(VCC)||VCC==F||VCC=="")
  {stop("please insert volume variable")}
  
  if(missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("please insert sample area variable")}
  
  if(missing(groups)||is.null(groups)||groups==F||groups==""){groups<-NULL}else{df <-df[  apply(!is.na(df[groups ]) , MARGIN = 1, function(x) all(x) ), ] }
  
  
  # argumentos opcionais
  if(missing(area_total) || is.null(area_total) || area_total==F || area_total==""   ){df$area_total<-NA; area_total <- "area_total"}
  if(missing(idade)      || is.null(idade)      || idade==F      || idade==""        ){df$idade<-NA;      idade <- "idade"}
  if(missing(VSC)        || is.null(VSC)        || VSC==F        || VSC==""          ){df$VSC<-NA;        VSC <- "VSC"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  if(missing(Hd)||is.null(Hd)||Hd==F||Hd=="") { # calculo da altura dominante
    
    if(  "HD" %in% names(df) ){ df$HD <- NULL }
    
    if(is.null(groups)){
      x <-  suppressMessages(   # remove mensagens do dplyr
        df %>% 
          select_(ht = HT) %>% 
          top_n(2) %>% # seleciona as duas maiores arvores
          summarise(HD = mean(ht) ) %>% 
          cbind(df) # como nao ha grupos, usamos cbind
      )    }else{
        x <-  suppressMessages(
          
          df %>% 
            group_by_(.dots = groups) %>% 
            select_(ht = HT) %>% 
            top_n(2) %>% 
            summarise(HD = mean(ht) ) %>% 
            full_join(df) # como ha grupos, usamos join
          
        )
      }
    
  } else{ x <- df %>% rename_(HD = Hd) }
  
  
  x %>% 
    group_by_(.dots = groups) %>% 
    mutate_(.dots = setNames(list( interp(~ pi * DAP^2 / 40000, DAP = as.name(DAP) ) ), nm = "AS" ) ) %>% 
    summarise_(
      .dots =  
        setNames(  list(  
          interp(~ round(mean(as.numeric(idade), na.rm=T) ), idade = as.name(idade)) ,
          interp(~ mean(area_total, na.rm=T), area_total = as.name(area_total)) ,
          interp(~ mean(area_parcela, na.rm=T), area_parcela = as.name(area_parcela)) ,
          interp(~ round(mean(DAP, na.rm=T), 2), DAP = as.name(DAP) ) ,
          ~ round(sqrt(mean(AS, na.rm=T) * 40000 / pi), 2)  ,
          interp(~round(mean(HT, na.rm=T), 2), HT = as.name(HT) ),
          ~ round(mean(HD), 2),
          ~ round(sum(AS, na.rm=T) * 10000/AREA_PARCELA, 4),
          interp(~round(sum(VCC, na.rm=T) * 10000/ AREA_PARCELA, 4 ), VCC = as.name(VCC), AREA_PARCELA = as.name("AREA_PARCELA") ),
          interp(~round(sum(VSC, na.rm=T) * 10000/ AREA_PARCELA, 4 ), VSC = as.name(VSC), AREA_PARCELA = as.name("AREA_PARCELA") )
          
        ), #list 
        nm = c("IDADE","AREA_TOTAL","AREA_PARCELA", "DAP", "q", "HT", "HD","G", "VCC","VSC" ) 
        )#setnames 
    ) %>% #sumarise 
    na_if(0) %>% # substitui 0 por NA
    select_if(Negate(anyNA)) %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    as.data.frame
}

acs <- function(df, area_total, area_parcela, VCC, idade, grupos, alpha = 0.05, Erro = 10, casas_decimais=4, pop="inf",tidy=T){
  
  suppressPackageStartupMessages(require(dplyr))
  require(tidyr)
  require(lazyeval)
  
  if(is.null(grupos)||missing(grupos)||grupos==FALSE||grupos==""){grupos=NULL}
  
  if(is.null(df)||missing(df)||df==F||df=="")
  {stop("Escolha o data frame") }
  
  if(is.null(area_total)||missing(area_total)||area_total==F||area_total=="")
  {stop("Escolha a variavel Area total (ha) ") }
  
  if(is.null(area_parcela)||missing(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("Escolha a variavel Area da parcela (m2) ") }
  
  if(is.null(VCC)||missing(VCC)||VCC==F||VCC=="")
  {stop("Escolha a variavel Volume (m3)") }
  
  # argumentos opcionais
  if(is.null(idade)||missing(idade)||idade==F||idade==""){df$idade<-NA; idade <- "idade"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  
  x_ <-df %>%
    na_if(0) %>%
    group_by_(.dots = grupos) %>%
    summarise_(
      .dots = 
        setNames( 
          list( 
            interp(~ mean(idade), idade = as.name(idade) ),
            interp(~ n() ),
            interp(~ mean(area_total) / ( mean(area_parcela)/10000 ), area_total = as.name(area_total), area_parcela = as.name(area_parcela)  ),
            interp(~ sd(VCC) / mean(VCC) * 100, VCC = as.name(VCC) ),
            ~ qt(alpha/2, df = n-1, lower.tail = FALSE),
            ~ ifelse(pop=="inf", 
                     qt(alpha/2, df = ceiling( t^2 * CV^2 / Erro^2) - 1, lower.tail = FALSE)  ,
                     qt(alpha/2, df = ceiling( t^2 * CV^2 / ( Erro^2 +(t^2 * CV^2 / N) ) ) - 1, lower.tail = FALSE) ) ,
            ~ ifelse(pop=="inf",
                     ceiling( t_rec ^2 * CV^2 / Erro^2 ) ,
                     ceiling( t_rec ^2 * CV^2 / ( Erro^2 +(t_rec^2 * CV^2 / N) ) ) ),
            interp(~ mean(VCC, na.rm=T), VCC = as.name(VCC) ),
            interp(~ ifelse(pop=="inf", 
                            sqrt( var(VCC)/n ), 
                            sqrt( var(VCC)/n  * (1 - (n/N)) ) ) , 
                   VCC = as.name(VCC), n = as.name("n"), N = as.name("N") ),
            ~ Sy * t ,
            ~ Erroabs / Y * 100,
            ~ Y * N,
            ~ Erroabs * N,
            ~ Y - Erroabs,
            ~ Y + Erroabs,
            ~ Yhat - Erro_Total,
            ~ Yhat + Erro_Total
          ), 
          nm=c("idade", "n","N", "CV","t","t_rec","n_recalc" ,"Y","Sy","Erroabs" ,"Erroperc","Yhat", "Erro_Total","IC_ha_Inf" ,"IC_ha_Sup","IC_Total_inf","IC_Total_Sup")
        ) 
    ) %>%
    na_if(0) %>% # substitui 0 por NA
    select_if(Negate(anyNA) ) %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    round_df(casas_decimais)
  
  x <- x_ %>% 
    plyr::rename(c( "idade"        = "Idade (meses)"                  , 
                    "n"            = "Numero de Parcelas (n)"         ,
                    "N"            = "Numero de Parcelas cabiveis (N)", 
                    "CV"           = "Coeficiente de Variancia (CV)"  ,
                    "t"            = "t-student"                      ,
                    "t_rec"        = "t recalculado"                  ,
                    "n_recalc"     = "n recalculado"                  ,
                    "Y"            = "Media geral (Y)"                ,
                    "Sy"           = "Erro-Padrao da Media (Sy)"      ,
                    "Erroabs"      = "Erro Absoluto"                  ,
                    "Erroperc"     = "Erro Relativo (%)"              ,
                    "Yhat"         = "Volume total estimado (Yhat)"   , 
                    "Erro_Total"   = "Erro Total"                     ,
                    "IC_ha_Inf"    = "IC (m³/ha) Inferior"            ,
                    "IC_ha_Sup"    = "IC (m³/ha) Superior"            ,
                    "IC_Total_inf" = "IC Total (m³) inferior"         ,
                    "IC_Total_Sup" = "IC Total (m³) Superior")        , 
                 warn_missing = F) # nao gera erro mesmo quando se renomeia variaveis inexistentes
  
  if(tidy==F)
  {
    return(x_)
  } 
  else if(tidy==T & is.null(grupos) )
  {
    x <- data.frame(Variaveis = names(x), Valores = t(x) )
    rownames(x) <- NULL
    # ou
    # x <- tibble::rownames_to_column(data.frame("Valores"=t(x)) , "Variaveis" ) 
    
    return(x)
  }
  else
  {
    vec1 <- names(x)[! names(x) %in% grupos ]
    vec2 <- grupos[length(grupos)]
    vec3 <- grupos[grupos!=vec2]
    
    y <- x %>%
      gather_("Variaveis","value", vec1, factor_key=T ) %>% 
      arrange_( grupos ) %>% 
      spread_(vec2,"value",sep="")%>%
      group_by_(.dots=vec3)
    return(y)
    
  }
  
}

ace <- function(df, area_estrato, area_parcela, VCC, grupos, idade, alpha = 0.05, Erro = 10, casas_decimais = 4, pop="inf", tidy=T ){
  
  suppressPackageStartupMessages(require(dplyr))
  require(tidyr)
  require(lazyeval)
  
  if(missing(df)||is.null(df)||df==F||df=="")
  {stop("Escolha o data frame") }
  
  if(missing(area_estrato)||is.null(area_estrato)||area_estrato==F||area_estrato=="")
  {stop("Escolha a variavel Area do Estrato (ha)") }
  
  if(missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("Escolha a variavel Area da parcela (m2)") }
  
  if(missing(VCC)||is.null(VCC)||VCC==F||VCC=="")
  {stop("Escolha a variavel Volume (m3)") }
  
  if(missing(grupos)||is.null(grupos)||is.na(grupos)||grupos==F||grupos=="")
  {stop("Escolha a(s) variavel(is) de estratificacao") }
  
  # argumentos opcionais
  if(missing(idade)||is.null(idade)||idade==F||idade=="") {df$idade<-NA; idade<-"idade"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_estrato)){df$area_estrato <- area_estrato; area_estrato <- "area_estrato"}
  
  
  x_ <- df %>%
    na_if(0) %>%
    group_by_(.dots = grupos) %>%
    summarise_(.dots = setNames( list( interp( ~ mean(area_estrato) / (mean(area_parcela)/10000), area_estrato = as.name(area_estrato), area_parcela = as.name(area_parcela) ) ), nm="Nj" ) ) %>%
    summarise(N  = sum(Nj) ) %>% 
    ungroup %>%
    select(N) %>%
    cbind(data.frame(df),.) %>% 
    mutate_(.dots = setNames( list( interp( ~ area_estrato / ( area_parcela/10000 ), area_estrato = as.name(area_estrato), area_parcela = as.name(area_parcela) ) ), nm="Nj" ) ) %>%
    group_by_(.dots = grupos) %>%
    summarise_(.dots = 
                 setNames( 
                   list(
                     interp(~ mean(idade), idade = as.name(idade) ),
                     interp(~ n() ) ,
                     ~ mean(Nj),
                     ~ mean(N),
                     ~ Nj/N,
                     interp(~ sum(VCC), VCC= as.name(VCC) ),
                     interp(~ sum(VCC^2), VCC= as.name(VCC) ),
                     interp(~ mean(VCC, na.rm =T), VCC= as.name(VCC) ),
                     interp(~ Pj * var(VCC, na.rm=T), Pj = as.name("Pj"), VCC = as.name(VCC) ),
                     interp(~ Pj * sd(VCC, na.rm=T), Pj = as.name("Pj"), VCC = as.name(VCC) ),
                     ~ Pj * Yj
                   ), 
                   nm=c("IDADE", "nj", "Nj", "N", "Pj", "Eyj", "Eyj2", "Yj", "Pj_Sj2", "Pj_Sj", "Pj_Yj")
                 ) 
    ) %>%
    ungroup %>%
    mutate( EPj_Sj2  =   sum(Pj_Sj2), 
            EPj_Sj   =   sum(Pj_Sj), 
            Y        =   sum(Pj_Yj), # media estratificada (ponderada)     
            CV       = EPj_Sj / Y * 100, # Coeficiente de variancia
            t        = qt(alpha/2, df = sum(nj)-1, lower.tail = FALSE),     # a seguir, o t sera calculado utilizando o n calculado, de forma direta
            t_rec    = ifelse(pop=="inf",
                              qt(alpha/2, df = ceiling( t^2 * EPj_Sj^2 / (Erro*Y/100)^2 )-1, lower.tail = FALSE),
                              qt(alpha/2, df = ceiling( t^2 * EPj_Sj^2 / ( (Erro*Y/100)^2 + (t^2 * EPj_Sj2 / N )  ) )-1, lower.tail = FALSE)
            ),
            n_recalc = ifelse(pop=="inf",
                              ceiling( t_rec^2 * EPj_Sj^2 / (Erro*Y/100)^2 ),
                              ceiling( t_rec^2 * EPj_Sj^2 / ( (Erro*Y/100)^2 + (t_rec^2 * EPj_Sj2 / N )  ) ) 
            ), # agora fazemos o recalculo do n, utilizando o t correto
            nj_otimo = ceiling(n_recalc*Pj_Sj/EPj_Sj), # por estrato utilizando o metodo de Neyman
            n_otimo  = sum(nj_otimo), # n calculado total
            Yhatj    = Nj * Yj )  %>% # producao total por estrato
    na_if(0) %>% # substitui 0 por NA
    select_if(Negate(anyNA) ) %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    round_df(casas_decimais)  
  
  x <- x_ %>% 
    plyr::rename(
      c("IDADE" = "Idade (meses)",
        "nj" = "numero de amostras / estrato (nj)" ,
        "Nj" = "Numero de amostras cabiveis / estrato (Nj)",
        "N" = "Numero de amostras cabiveis (N)", 
        "Pj" = "Proporcao Nj/N (Pj)",
        "Eyj" = "Somatorio do volume por estrato (Eyj)", 
        "Eyj2" = "Soma quadratica do volume por estrato (Eyj2)", 
        "Yj" = "Media do volume por estrato (Yj)", 
        "Pj_Sj2" = "PjSj2", 
        "Pj_Sj" = "PjSj", 
        "Pj_Yj" = "PjYj",
        "EPj_Sj2" = "EPjSj2",
        "EPj_Sj" = "EPjSj", 
        "Y" = "Media Estratificada (Y)",
        "CV" = "Coeficiente de Variancia (CV)", 
        "t" = "t-student", 
        "t_rec" = "t recalculado", 
        "n_recalc" = "n recalculado",
        "nj_otimo" = "Numero otimo de parcelas por estrato (nj otimo)", 
        "n_otimo" = "numero otimo de parcelas (n otimo)", 
        "Yhatj" = "Producao total por estrato (Yhatj)"  ),
      warn_missing = F)
  
  
  
  
  y_ <- x_ %>%
    group_by_(.dots=grupos[-length(grupos)] ) %>%
    summarise(t            = mean(t),
              Sy           = ifelse(pop=="inf",
                                    sqrt(sum(Pj_Sj)^2 / sum(nj) ),
                                    sqrt(sum(Pj_Sj) ^2 / sum(nj) - (mean(EPj_Sj2) / mean(N) )  )
              ), # Erro-padrao da media
              Y            = sum(Pj_Yj), # media de vcc estratificada (ponderada) 
              Erroabs      = Sy * t, # Erro Absoluto
              Erroperc     = Erroabs / Y * 100, # Erro percentual
              Yhat         = sum(Nj) * Y, # Volume Total
              Erro_Total   = Erroabs * sum(Nj), # Erro Total
              IC_ha_Inf    = Y - Erroabs, # Intervalo de confianca por ha inferior
              IC_ha_Sup    = Y + Erroabs, # Intervalo de confianca por ha superior
              IC_Total_inf = Yhat - Erro_Total, # Intervalo de confianca total inferior
              IC_Total_Sup = Yhat + Erro_Total    ) %>% # Intervalo de confianca total superior)
    round_df(casas_decimais)  
  
  
  y <- y_ %>% 
    plyr::rename(
      c("t" = "t-student",
        "Sy" = "Erro-Padrao da Media (Sy)",
        "Y" = "Media estratificada (Y)",
        "Erroabs" = "Erro Absoluto" ,
        "Erroperc" = "Erro Relativo (%)",
        "Yhat" = "Volume total estimado (Yhat)", 
        "Erro_Total" = "Erro Total",
        "IC_ha_Inf" = "IC (m³/ha) Inferior" ,
        "IC_ha_Sup" = "IC (m³/ha) Superior",
        "IC_Total_inf" = "IC Total (m³) inferior",
        "IC_Total_Sup" = "IC Total (m³) Superior"),
      warn_missing = F)
  
  
  if(tidy==F){
    
    z <- list(Tabela1 = as.data.frame(x_), Tabela2 = as.data.frame(y_))
    
    return(z)
    
  }else{
    
    vec1 <- names(x)[! names(x) %in% grupos ]
    vec2 <- grupos[length(grupos)]
    vec3 <- grupos[grupos!=vec2]
    vec4 <- names(y)[! names(y) %in% vec3 ] 
    vec5 <- vec3[length(vec3)]
    vec6 <- vec3[vec3!=vec5]
    
    x <-  x %>%
      gather_("Variaveis","value", vec1, factor_key=T) %>% 
      arrange_(grupos) %>% 
      spread_(vec2,"value",sep="") %>%
      group_by_(.dots=vec3)
    
    if(length(grupos)!=1 ){
      
      y <- y %>%
        gather_("Variaveis","value", vec4, factor_key=T) %>% 
        arrange_(vec3) %>% 
        spread_(vec5,"value") %>%
        group_by_(.dots=vec6)
      
    } else{
      
      y <- y %>%
        gather_("Variaveis","value", vec4, factor_key=T)
      
      
    }
    
    z <- list(Tabela1 = as.data.frame(x), Tabela2 = as.data.frame(y))
    
    return(z) 
    
  }
  
  
  
}

as_diffs <- function(df, area_total, area_parcela, VCC, idade, grupos, alpha = 0.05, Erro = 10, casas_decimais=4, tidy=T ) {
  
  suppressPackageStartupMessages(require(dplyr))
  require(tidyr)
  require(lazyeval)
  
  if(missing(grupos) || is.null(grupos) || grupos==F || grupos==""){grupos=NULL}
  
  if(missing(df)||is.null(df)||df==F||df=="")
  {stop("Escolha o data frame") }
  
  if(missing(area_total)||is.null(area_total)||area_total==F||area_total=="")
  {stop("Escolha a variavel Area total (ha) ") }
  
  if(missing(area_parcela)||is.null(area_parcela)||area_parcela==F||area_parcela=="")
  {stop("Escolha a variavel Area da parcela (m2) ") }
  
  if(missing(VCC)||is.null(VCC)||VCC==F||VCC=="")
  {stop("Escolha a variavel Volume (m3)") }
  
  # argumentos opcionais
  if(missing(idade)||is.null(idade)||idade==F||idade==""){df$idade<-NA; idade <- "idade"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  x_ <- df %>%
    group_by_(.dots = grupos) %>%
    summarise_(
      .dots = 
        setNames( 
          list( 
            interp(~ mean(idade), idade = as.name(idade) ),
            interp(~ n() ),
            interp(~ mean(area_total) / ( mean(area_parcela)/10000 ), area_total = as.name(area_total), area_parcela = as.name(area_parcela)  ),
            interp(~ sd(VCC) / mean(VCC) * 100, VCC = as.name(VCC) ),
            ~ qt(alpha/2, df = n-1, lower.tail = FALSE),
            interp(~ mean(VCC, na.rm=T), VCC = as.name(VCC) ),
            interp(~ sqrt( (sum(diff(VCC)^2) / (2 * n * (n-1) ) ) * ((N-n)/N) ) , VCC = as.name(VCC), n = as.name("n"), N = as.name("N") ),
            ~ Sy * t ,
            ~ Erroabs / Y * 100,
            ~ Y * N,
            ~ Erroabs * N,
            ~ Y - Erroabs,
            ~ Y + Erroabs,
            ~ Yhat - Erro_Total,
            ~ Yhat + Erro_Total
          ), 
          nm=c("idade", "n","N", "CV","t","Y","Sy","Erroabs" ,"Erroperc","Yhat", "Erro_Total","IC_ha_Inf" ,"IC_ha_Sup","IC_Total_inf","IC_Total_Sup")
        ) 
    ) %>%
    na_if(0) %>% # substitui 0 por NA
    select_if(Negate(anyNA) ) %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    round_df(casas_decimais)
  
  
  x <- x_ %>% 
    plyr::rename(c( "idade"        = "Idade (meses)"                  , 
                    "n"            = "Numero de Parcelas (n)"         ,
                    "N"            = "Numero de Parcelas cabiveis (N)", 
                    "CV"           = "Coeficiente de Variancia (CV)"  ,
                    "t"            = "t-student"                      ,
                    "t_rec"        = "t recalculado"                  ,
                    "n_recalc"     = "n recalculado"                  ,
                    "Y"            = "Media geral (Y)"                ,
                    "Sy"           = "Erro-Padrao da Media (Sy)"      ,
                    "Erroabs"      = "Erro Absoluto"                  ,
                    "Erroperc"     = "Erro Relativo (%)"              ,
                    "Yhat"         = "Volume total estimado (Yhat)"   , 
                    "Erro_Total"   = "Erro Total"                     ,
                    "IC_ha_Inf"    = "IC (m³/ha) Inferior"            ,
                    "IC_ha_Sup"    = "IC (m³/ha) Superior"            ,
                    "IC_Total_inf" = "IC Total (m³) inferior"         ,
                    "IC_Total_Sup" = "IC Total (m³) Superior")        , 
                 warn_missing = F) # nao gera erro mesmo quando se renomeia variaveis inexistentes
  
  
  
  if(tidy==F)
  {
    return(x_)
  } 
  else if(tidy==T & is.null(grupos) )
  {
    x <- data.frame(Variaveis = names(x), Valores = t(x) )
    rownames(x) <- NULL
    # ou
    # x <- tibble::rownames_to_column(data.frame("Valores"=t(x)) , "Variaveis" ) 
    
    return(x)
  }
  else
  {
    
    vec1 <- names(x)[! names(x) %in% grupos ]
    vec2 <- grupos[length(grupos)]
    vec3 <- grupos[grupos!=vec2]
    
    y <- x %>%
      gather_("Variaveis","value", vec1, factor_key=T ) %>% 
      arrange_( grupos ) %>% 
      spread_(vec2,"value",sep="")%>%
      group_by_(.dots=vec3)
    
    return(y)
  }
  
  
  
}

# vectors for names ####

especies_names <- c("nome.cient","scientific.name","Scientific.Name","SCIENTIFIC.NAME" ,"scientific_name", "Scientific_Name","SCIENTIFIC_NAME","nome.cientifico", "Nome.Cientifico","NOME.CIENTIFICO","nome_cientifico", "Nome_Cientifico","NOME_CIENTIFICO")
parcelas_names <- c("transecto","transect", "Transect", "TRNASECT", "transect.code","Transect.Code","TRANSECT.CODE","transect_code","Transect_Code","TRANSECT_CODE","parcela", "Parcela","PARCELA","cod.parcela","Cod.Parcela","COD.PARCELA", "cod_parcela","Cod_Parcela","COD_PARCELA")
est.vertical_names <- c("pos.copa","canopy", "canopy_09")
est.interno_names <- c("luminosidade","light", "light_09")


DAP_names <- c("DAP","Dap","dap", "dbh", "Dbh","DBH","DBH_11")
HT_names <- c("HT_EST", "HT", "Ht", "ht","Htot","ALTURA","Altura","Altura_Total", "ALTURA_TOTAL")
VCC_names <- c("VCC","Vcc", "vcc", "VOL", "Vol", "vol" ,"VOLUME")
area_parcela_names <- c("trans.area","AREA_PARCELA","Area_Parcela","area_parcela", "AREAPARCELA", "areaparcela", "transect.area", "Transect.Area", "TRANSECT.AREA","transect_area","Transect_Area","TRANSECT_AREA")
area_total_names <- c("sub.area","AREA_TOTAL", "AREATOTAL", "area_total", "areatotal","AREA_TALHAO", "AREATALHAO", "area_talhao", "areatalhao","total.area","Total.Area","TOTAL.AREA","total_area","Total_Area","TOTAL_AREA")
idade_names <- c("IDADE", "Idade","idade")
VSC_names <- c("VSC","Vsc", "vsc")
HD_names <- c("HD", "Hd", "hd", "ALTURA_DOMINANTE", "ALT_DOM")
grupos_names <- c(c("TALHAO", "PARCELA"), c("area.code", "transect"), c("codigo", "transecto"))
estratos_names <- c("TALHAO", "Talhao", "talhao","COD_TALHAO","Cod_Talhao","cod_talhao", "COD.TALHAO", "Cod.Talhao","cod.talhao", "area.code", "Area.Code","AREA.CODE", "area_code","Area_Code","AREA_CODE")

# Server ####

shinyServer(function(input, output, session) {
  
  
  # Importar os dados ####

  
  output$upload <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload", "" )  )

    list(    
      
      fileInput( # input de arquivos
      inputId = "file1", # Id
      
      label = "Selecione o arquivo: (.csv, .txt ou .xlsx)", # nome que sera mostrado na UI
      
      accept=c('text/csv/xlsx','.csv', ".txt", ".xlsx")),
      
      checkboxInput(inputId = "excel",
                    label = "Excel (.xls ou .xslx) ?",
                    value = F),
      
      div("Recomendamos o uso do formato .csv", style = "color:blue"),
      
      radioButtons("df", 
                   "Tipo da base de dados:", 
                   choices = c("Dados em nivel de arvore",
                               "Dados em nivel de parcela"),
                   selected = "Dados em nivel de arvore"),
      
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='sep',  #Id
        label='Separador:', # nome que sera mostrado na UI
        choices=c(Virgula=',', "Ponto e Virgula"=';', Tab='\t'), # opcoes e seus nomes
        selected=','), # valor que sera selecionado inicialmente
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='dec', # Id
        label='Decimal:', # nome que sera mostrado na UI
        choices=c(Ponto=".", Virgula=","), # opcoes e seus nomes
        selected="."), # valor que sera selecionado inicialmente
      
      
      
      actionButton( # botao que o usuario clica, e gera uma acao no server
        "Load", # Id
        "Carregue o arquivo")
      
      
      
      
    )
    
    
  })
  
  upData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
    
    if(input$Load==0){return()} # se o botao load nao for pressionado(==0), retornar nada
    else(inFile <- input$file1) # caso contrario, salvar o caminho do arquivo carregado em inFile
    
    # input$file1 sera NULL inicialmente. apos o usuario selecionar
    # e upar um arquivo, ele sera um data frame com as colunas
    # 'size', 'type', e 'datapath' . A coluna 'datapath' 
    # ira conter os nomes dos arquivos locais onde o dado pode ser encontrado
    
    if (is.null(inFile)){return(NULL)} # se o arquivo nao for carregado, retornar null
    else if(input$excel == F)
    {
      raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote='"')
    } else {raw_data <- read.xlsx(inFile$datapath, 1)  }
    
    # Carregamos o arquivo em um objeto
    
    
    raw_data # tabela final a ser mostrada. 
    
  })
  
  rawData <- reactive({
    
    switch(input$df_select, 
           "Fazer o upload" = upData(),
           "Utilizar o dado de exemplo" = ex)
    
  })
  
  output$rawdata <- renderDataTable({ # renderizamos uma DT::DataTable
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    data <- rawData() 
    
    datatable(data) # Criamos uma DT::datatable com base no objeto
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
  })
  
  # Índices de diversidade ####
  
  # funcao diversidade
  tabdiversidade <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" )  )
    
    if(input$Loaddiv){
      
      dados <- rawData()
      
      x <- diversidade(data             = dados, 
                       col.especies     = input$col.especiesdiv,
                       rotulo.NI        = input$rotutuloNIdiv  ) %>% 
        gather("Índice", "Resultado") # transpor tabela
      
      x 
    }
    
  })
  
  # UI
  output$selec_especiesdiv <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.especiesdiv", # Id
      "Selecione a coluna de espécies:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = especies_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_rotuloNIdiv <- renderUI({
    
    dados <- rawData()
    
    switch(input$CBdiv,
           "Manualmente" = textInput("rotutuloNIdiv", 
                                     label = "Rotular:", 
                                     value = "NI"),
           
           "lista de especies" = selectizeInput("rotutuloNIdiv",
                                                label = "Rotular:",
                                                choices = levels(as.factor(dados[,input$col.especiesdiv])),
                                                options = list(
                                                  placeholder = 'Selecione uma espécie abaixo',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                ) # options    
           )# selectize
    )
    
    
  })
  
  # tabela
  output$div <- renderDataTable({
    
    if(input$Loaddiv)
    {
      divdt <- tabdiversidade() 
      
      datatable( divdt,
                 options = list(searching = FALSE,
                                paging=FALSE )  ) 
    }
    
  }) 
  
  # Matriz Similaridade ####
  
  # funcao m similaridade
  tabmsimilaridade1 <- reactive({
    
    if(input$Loadmsim){
      
      validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" )  )
      
      dados <- rawData()
      
      x <- m.similaridade(data             = dados, 
                          col.especies     = input$col.especiesmsim,
                          col.comparison   = input$col.parcelasmsim,
                          rotulo.NI        = input$rotutuloNImsim  )
      
      x <- as.data.frame(x[[1]])
      names(x) <- 1:length(x)
      x
    }
    
  })
  tabmsimilaridade2 <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" )  )
    
    if(input$Loadmsim){
      
      dados <- rawData()
      
      x <- m.similaridade(data             = dados, 
                          col.especies     = input$col.especiesmsim,
                          col.comparison   = input$col.parcelasmsim,
                          rotulo.NI        = input$rotutuloNImsim  )
      
      x <- as.data.frame(x[[2]])
      names(x) <- 1:length(x)
      x
    }
    
  })
  
  # UI
  
  output$selec_especiesmsim <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.especiesmsim", # Id
      "Selecione a coluna de espécies:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = especies_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_parcelasmsim <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelasmsim", # Id
      "Selecione a coluna da parcela:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_rotuloNImsim <- renderUI({
    
    dados <- rawData()
    
    switch(input$CBmsim,
           "Manualmente" = textInput("rotutuloNImsim", 
                                     label = "Rotular:", 
                                     value = "NI"),
           
           "lista de especies" = selectizeInput("rotutuloNImsim",
                                                label = "Rotular:",
                                                choices = levels(
                                                  as.factor(
                                                    dados[,input$col.especiesmsim])),
                                                options = list(
                                                  placeholder = 'Selecione uma espécie abaixo',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                ) # options    
           )# selectize
    )
    
    
  })
  
  
  # tabela
  output$msim1 <- renderDataTable({
    
    if(input$Loadmsim)
    {
      msimdt1 <- tabmsimilaridade1() 
      
      datatable( msimdt1,
                 options = list(searching = FALSE,
                                paging=FALSE )  ) 
    }
    
  }) 
  output$msim2 <- renderDataTable({
    
    if(input$Loadmsim)
    {
      msimdt2 <- tabmsimilaridade2() 
      
      datatable( msimdt2,
                 options = list(searching = FALSE,
                                paging=FALSE )  ) 
    }
    
  }) 
  
  # graficos 
  msim1_graph <- reactive({
    
    if(input$Loadmsim)
    {
      dados <- rawData()
      ms <- as.data.frame(tabmsimilaridade1() ) 
      
      names(ms) <- levels( as.factor( dados[,input$col.parcelasmsim] ) )
      
      ms_hclust <- hclust(as.dist(ms) ) #?
      
      x <- ggdendrogram(ms_hclust)
      
      x
      
      }
    
    
    
  })
  
  output$msim1_graph_ <- renderPlot({
    
    
      
    g <- msim1_graph()
    
    g
    
  })
  
  
  msim2_graph <- reactive({
    
    if(input$Loadmsim)
    {
      dados <- rawData()
      ms <- as.data.frame(tabmsimilaridade2() ) 
      
      names(ms) <- levels( as.factor( dados[,input$col.parcelasmsim] ) )
      
      ms_hclust <- hclust(as.dist(ms) ) #?
      
      x <- ggdendrogram(ms_hclust)
      
      x
    }
    
    
    
  })
  
  output$msim2_graph_ <- renderPlot({
    
   g <- msim2_graph()
   
   g
    
    
  })
  
  # Pareado Similaridade ####
  # funcao p similaridade
  tabpsimilaridade <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" )  )
    
    if(input$Loadpsim){
      
      dados <- rawData()
      
      #inv %>% 
      #filter_(.dots = interp(~ transect == "T01", transect = as.name("transect") ) ) %>% 
      #select_("scientific.name")
      
      x <- dados %>% 
        filter_(.dots = interp(~ transect == input$psimselec_parc1, transect = as.name(input$col.parcelaspsim) ) ) %>% 
        select_(input$col.especiespsim)
      
      y <- dados %>% 
        filter_(.dots = interp(~ transect == input$psimselec_parc2, transect = as.name(input$col.parcelaspsim) ) ) %>% 
        select_(input$col.especiespsim)
      
      x <- p.similaridade( 
        x         = x[,1],
        y         = y[,1],
        rotuloNI = input$rotutuloNIpsim  )
      
      x <- data.frame( "Índices" = c("Jaccard", "Sorensen")  ,
                       "Resultado" = c( x[1], x[2] )  )
      x
      
    }
    
  })
  
  # UI
  
  output$selec_especiespsim <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.especiespsim", # Id
      "Selecione a coluna de espécies:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = especies_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_parcelaspsim <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelaspsim", # Id
      "Selecione a coluna da parcela:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_psimselec_parc1 <- renderUI({
    
    dados <- rawData()
    
    parcelas <- levels(
      as.factor(
        dados[,input$col.parcelaspsim]))
    
    selectizeInput("psimselec_parc1",
                   label = "Selecione a Parcela 1:",
                   choices = parcelas,
                   options = list(
                     placeholder = 'Selecione uma espécie abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )
    
  })
  
  output$selec_psimselec_parc2 <- renderUI({
    
    dados <- rawData()
    
    parcelas <- levels(
      as.factor(
        dados[,input$col.parcelaspsim]))
    
    selectizeInput("psimselec_parc2",
                   label = "Selecione a Parcela 2:",
                   choices = parcelas,
                   options = list(
                     placeholder = 'Selecione uma espécie abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )
    
  })
  
  output$selec_rotuloNIpsim <- renderUI({
    
    dados <- rawData()
    
    switch(input$CBpsim,
           "Manualmente" = textInput("rotutuloNIpsim", 
                                     label = "Rotular:", 
                                     value = "NI"),
           
           "lista de especies" = selectizeInput("rotutuloNIpsim",
                                                label = "Rotular:",
                                                choices = levels(
                                                  as.factor(
                                                    dados[,input$col.especiespsim])),
                                                options = list(
                                                  placeholder = 'Selecione uma espécie abaixo',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                ) # options    
           )# selectize
    )
    
    
  })
  
  # tabela
  output$psim <- renderDataTable({
    
    if(input$Loadpsim)
    {
      psimdt <- tabpsimilaridade() 
      
      datatable( psimdt,
                 options = list(searching = FALSE,
                                paging=FALSE )  ) 
    }
    
  }) 
 
  # Índices de agregacao ####
  
  tabagregate <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" )  )
    
    if(input$Loadagreg){
      
    dados <- rawData()
    
    x <- agregacao(data         =  dados, 
                   col.especies = input$col.especiesagreg, 
                   col.parcelas = input$col.parcelasagreg, 
                   rotulo.NI    = input$rotutuloNIagreg  )
    
    x
    }
    
  })
  
  output$selec_especiesagreg <- renderUI({
    
    data <- rawData()
    
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.especiesagreg", # Id
      "Selecione a coluna de espécies:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = especies_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
      )
    
  })
  
  output$selec_parcelasagreg <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelasagreg", # Id
      "Selecione a coluna das parcelas:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
      )
    
  })
  
  output$selec_rotuloNIagreg <- renderUI({
    
    dados <- rawData()
    
    switch(input$CBagreg,
           "Manualmente" = textInput("rotutuloNIagreg", 
                                   label = "Rotular:", 
                                   value = "NI"),
           
           "lista de especies" = selectizeInput("rotutuloNIagreg",
                                                label = "Rotular:",
                                                choices = levels(as.factor(dados[,input$col.especiesagreg])),
                                                options = list(
                                                  placeholder = 'Selecione uma espécie abaixo',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                ) # options    
                                                )# selectize
           )
    
    
  })
  
  output$agreg <- renderDataTable({
    
    if(input$Loadagreg)
    {
      agregdt <- tabagregate() 
      
      datatable( agregdt,
                 options = list(searching = T,
                                paging=T )  ) 
    }
    
  }) 
  
  # Estrutura ####
  
  # funcao estrutura
  tabestrutura <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" )  )
    
    if(input$Loadestr){
      
      dados <- rawData()
      
      x <- estrutura(data             = dados, 
                     col.especies     = input$col.especiesestr,
                     col.dap          = input$col.dapestr,
                     col.parcelas     = input$col.parcelasestr,
                     area.parcela     = input$area.parcelaestr,
                     est.vertical     = input$est.verticalestr,
                     est.interno      = input$est.internoestr,
                     nao.identificada = input$rotutuloNIestr  )
      
      as.tbl(x)
    }
    
  })
  
  # UI
  output$selec_especiesestr <- renderUI({
    
    data <- rawData()

    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.especiesestr", # Id
      "Selecione a coluna de espécies:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = especies_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_dapestr <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.dapestr", # Id
      "Selecione a coluna do DAP (cm):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = DAP_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_parcelasestr <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelasestr", # Id
      "Selecione a coluna da parcela:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_area.parcelaestr <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "area.parcelaestr", # Id
      "Selecione a coluna da área da parcela (m²):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_parcela_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_rotuloNIestr <- renderUI({
    
    dados <- rawData()
    
    switch(input$CBestr,
           "Manualmente" = textInput("rotutuloNIestr", 
                                     label = "Rotular:", 
                                     value = "NI"),
           
           "lista de especies" = selectizeInput("rotutuloNIestr",
                                                label = "Rotular:",
                                                choices = levels(as.factor(dados[,input$col.especiesestr])),
                                                options = list(
                                                  placeholder = 'Selecione uma espécie abaixo',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                ) # options    
           )# selectize
    )
    
    
  })
  
  output$selec_est.verticalestr <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "est.verticalestr", # Id
      "Selecione a coluna estrutura vertical:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      #selected = est.vertical_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_est.internoestr <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "est.internoestr", # Id
      "Selecione a coluna estrutura interna:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      #selected = est.interno_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  # tabela
  output$estr <- renderDataTable({
    
    if(input$Loadestr)
    {
      estrdt <- round_df( tabestrutura(), input$cdestr )
      
      datatable( as.tbl(estrdt),
                 options = list(searching = T,
                                paging=T )  ) 
    }
    
  }) 
  
  
  # BDq Meyer ####
  
  # funcao BDq Meyer
  tabBDq1 <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" )  )
    
    if(input$LoadBDq){
      
      dados <- rawData()
      
      x <- bdq.meyer(data             = dados, 
                     col.parcelas     = input$col.parcelasBDq,
                     col.dap          = input$col.dapBDq,
                     area.parcela     = input$area.parcelaBDq,
                     intervalo.classe = input$intervalo.classeBDq,
                     min.dap          = input$min.dapBDq,
                     i.licourt        = input$i.licourtBDq  )
      
      x[[1]]
    }
    
  })
  tabBDq3 <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" )  )
    
    if(input$LoadBDq){
      
      dados <- rawData()
      
      x <- bdq.meyer(data             = dados, 
                     col.parcelas     = input$col.parcelasBDq,
                     col.dap          = input$col.dapBDq,
                     area.parcela     = input$area.parcelaBDq,
                     intervalo.classe = input$intervalo.classeBDq,
                     min.dap          = input$min.dapBDq,
                     i.licourt        = input$i.licourtBDq  )
      
      x <-data.frame( "Coeficientes" = c("b0", "b1")  ,
                      "Valor"        = c( x[[3]][1], x[[3]][2] )  )
      x
    }
    
  })
  
  # UI
  
  output$selec_parcelasBDq <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelasBDq", # Id
      "Selecione a coluna da parcela:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_dapBDq <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.dapBDq", # Id
      "Selecione a coluna do DAP (cm):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = DAP_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  output$selec_area.parcelaBDq <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "area.parcelaBDq", # Id
      "Selecione a coluna da área da parcela (m²):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_parcela_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  # tabela
  output$BDq1 <- renderDataTable({
    
    if(input$LoadBDq)
    {
      BDqdt <- tabBDq1()
      
      datatable( as.data.frame(BDqdt),
                 options = list(searching = T,
                                paging=T )  ) 
    }
    
  }) 
  output$BDq3 <- renderDataTable({
    
    if(input$LoadBDq)
    {
      BDqdt <- tabBDq3()

      datatable(BDqdt,
                 options = list(searching = FALSE,
                                paging=FALSE )  ) 
    }
    
  }) 

  # grafico
  
  BDq_graph <- reactive({
    
    if(input$LoadBDq){
      
      data <- tabBDq1()
      
      graph_bdq <- data %>% 
        select("x"                       = CentroClasse, 
               "Distribuição observada"  = IndvHectare , 
               "Distribuição balanceada" = MeyerBalan  ) %>% 
        gather(class, y, -x, factor_key = T) %>% 
        arrange(x) %>% 
        mutate(x = as.factor(x) )
      
      g <-  ggplot(graph_bdq, aes(x = x, y = y) ) + 
        geom_bar(aes(fill = class), stat = "identity",position = "dodge") +
        labs(x = "Classe de diâmetro (cm)", y = "Número de indivíduos (ha)", fill = NULL) + 
        scale_fill_manual(values =c("firebrick2", "cyan3") ) +
        theme_hc(base_size = 14) 
      #theme_igray(base_size = 14)
      
      g
    }
    
    
  })

  output$BDq_graph_ <- renderPlot({
    
   g <- BDq_graph()
   
   g 
   
  })
  
  # Inventario ####
  
  # Dado utilizado no inventario ####
  
  # switch que muda o dado a ser utilizado
  invData <- reactive({
    
    switch(input$df, 
           "Dados em nivel de arvore" = newData(),
           "Dados em nivel de parcela" = rawData() )
    
  })
  
  
  # Totalização de Parcelas ####
  
  # dados / funcao inv_summary
  newData <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" )  )
    
    if(input$Loadnew){    
      
      dados <- rawData()
      
      x <- inv_summary(df           = dados, 
                       DAP          = input$DAPnew, 
                       HT           = input$HTnew,
                       VCC          = input$VCCnew,
                       area_parcela = input$area_parcelanew,
                       groups       = input$gruposnew,
                       area_total   = input$area_totalnew,
                       idade        = input$idadenew,
                       VSC          = input$VSCnew,
                       Hd           = input$Hdnew)
      
      x
      
    }
    
  })
  
  # UI
  output$selec_DAPnew <- renderUI({
    
  
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'DAPnew', # Id
      "Selecione a coluna do DAP (cm):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = DAP_names,
      options = list(
        placeholder = 'selecione uma coluna abaixo'# ,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_HTnew <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'HTnew', # Id
      "Selecione a coluna da altura (cm):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = HT_names,
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_VCCnew <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'VCCnew', # Id
      "Selecione a coluna do volume com casca (m³):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = VCC_names,
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_area_parcelanew <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'area_parcelanew', # Id
      "Selecione a coluna da área da parcela (m²):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_parcela_names,
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_gruposnew <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'gruposnew', # Id
      "selecione as variáveis pivô:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      multiple = TRUE,
      selected = grupos_names,
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_area_totalnew <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'area_totalnew', # Id
      "Selecione a coluna da área total (ha):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_total_names,
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_idadenew <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'idadenew', # Id
      "Selecione a coluna da idade:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      #selected = idade_names,
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_VSCnew <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'VSCnew', # Id
      "selecione a coluna do volume com casca (m³):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
     # selected = VSC_names,
      options = list(
        placeholder = 'selecione uma coluna abaixo',
         onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_HDnew <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'Hdnew', # Id
      "Selecione a coluna da altura dominante (m):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
     # selected = HD_names,
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  # tabela
  output$newdata <- renderDataTable({ # renderizamos uma DT::DataTable
    
    data <- newData() 
    
    if(input$Loadnew)
    {
      datatable(data) # Criamos uma DT::datatable com base no objeto
    }
    
  })
  
  
  # ACS ####
  
  # funcao acs aplicada em invData
  tabacs <- reactive({
    
    if(input$Loadacs){
      
      dados <- invData()
      
      x <-     acs(df             = dados,
                   area_total     = input$area_totalacs, 
                   area_parcela   = input$area_parcelaacs,
                   VCC            = input$VCCacs,
                   idade          = input$idadeacs,
                   grupos         = input$gruposacs, 
                   alpha          = input$alphaacs, 
                   Erro           = input$erroacs, 
                   casas_decimais = input$cdacs, 
                   pop            = input$popacs, 
                   tidy           = input$tidyacs)
      
      x}
    
  })
  
  # UI: as opcoes (choices) sao os nomes de invData
  
  output$selec_area_totalacs <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'area_totalacs', # Id
      "Selecione a coluna da área total (ha):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_total_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_area_parcelaacs <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'area_parcelaacs', # Id
      "Selecione a coluna da área da parcela (m²):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_parcela_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_VCCacs <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'VCCacs', # Id
      "Selecione a coluna do volume (m³):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = VCC_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_idadeacs <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'idadeacs', # Id
      "Selecione a coluna da idade:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      #selected = idade_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo',
         onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_gruposacs <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'gruposacs', # Id
      "Selecione as variáveis pivô:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      multiple = TRUE,  # permite mais de uma opcao ser selecionada
      selected = NULL,     
      options = list(
        placeholder = 'Selecione as variaveis abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  # tabela
  output$acs <- renderDataTable({
    
    acsdt <- tabacs() 
    
    if(input$Loadacs)
    {
      datatable( acsdt,
                 options = list(searching = FALSE,
                                paging=FALSE
                 )   
                 
      )
    } 
    
  })
  
  # ACE ####
  
  # resultado 1 da funcao ace aplicada em invData
  tabace1 <- reactive({
    
    if(input$Loadace){
      
      dados <- invData()
      
      x <- ace(df             = dados, 
               area_estrato   = input$area_estratoace, 
               area_parcela   = input$area_parcelaace, 
               VCC            = input$VCCace, 
               grupos         = input$gruposace, 
               idade          = input$idadeace, 
               alpha          = input$alphaace, 
               Erro           = input$erroace, 
               casas_decimais = input$cdace, 
               pop            = input$popace, 
               tidy           = input$tidyace)[[1]]
      x
    }
    
  })
  
  # resultado 2 da funcao ace aplicada em invData
  tabace2 <- reactive({
    
    if(input$Loadace){ 
      
      dados <- invData()
      
      x <- ace(df = dados, 
               area_estrato   = input$area_estratoace, 
               area_parcela   = input$area_parcelaace , 
               VCC            = input$VCCace, 
               grupos         = input$gruposace, 
               idade          = input$idadeace, 
               alpha          = input$alphaace, 
               Erro           = input$erroace, 
               casas_decimais = input$cdace, 
               pop            = input$popace, 
               tidy           = input$tidyace)[[2]]
      
      x
    }
    
  })
  
  # UI: as opcoes (choices) sao os nomes de invData
  
  output$selec_area_totalace <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'area_estratoace', # Id
      "Selecione a coluna da área total (ha):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_total_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  }) 
  
  output$selec_area_parcelaace <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'area_parcelaace', # Id
      "Selecione a coluna da área da parcela (m²):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_parcela_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_VCCace <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'VCCace', # Id
      "Selecione a coluna do volume (m³):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = VCC_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_gruposace <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'gruposace', # Id
      "Selecione a(s) coluna(s) para estratificação:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      multiple = TRUE,  # permite mais de uma opcao ser selecionada
      selected = estratos_names,     
      options = list(
        placeholder = 'Selecione as variaveis abaixo'#,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_idadeace <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'idadeace', # Id
      "Selecione a coluna da idade:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
     # selected = idade_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo',
         onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  # tabela ace1
  output$ace1 <- renderDataTable({
    
    ace1dt <- tabace1() 
    
    if(input$Loadace)
    {
      datatable( ace1dt,
                 options = list(searching = FALSE,
                                paging=FALSE
                 )   
                 
      )
    } 
    
  })
  
  # tabela ace2
  output$ace2 <- renderDataTable({
    
    ace2dt <- tabace2() 
    
    if(input$Loadace)
    {
      datatable( ace2dt,
                 options = list(searching = FALSE,
                                paging=FALSE
                 )   
                 
      )
    } 
    
  })
  
  # AS ####
  
  # funcao as aplicado em invData
  tabas <- reactive({
    
    if(input$Loadas){ 
      
      dados <- invData()
      
      x <- as_diffs(df             = dados, 
                    area_total     = input$area_totalas,
                    area_parcela   = input$area_parcelaas ,
                    VCC            = input$VCCas,
                    idade          = input$idadeas,
                    grupos         = input$gruposas,
                    alpha          = input$alphaas,
                    Erro           = input$erroas,
                    casas_decimais = input$cdas,
                    tidy           = input$tidyas)
      
      x
    }
    
  }) 
  
  # UI: as opcoes (choices) sao os nomes de invData
  
  output$selec_area_totalas <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'area_totalas', # Id
      "Selecione a coluna da área total (ha):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_total_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_area_parcelaas <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'area_parcelaas', # Id
      "Selecione a coluna da área da parcela (m²):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_parcela_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_VCCas <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'VCCas', # Id
      "Selecione a coluna do volume (m³):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = VCC_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo'#,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_idadeas <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'idadeas', # Id
      "Selecione a coluna da idade:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      #selected = idade_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  output$selec_gruposas <- renderUI({
    
    data <- invData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'gruposas', # Id
      "Selecione as variáveis pivô:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      multiple = TRUE,  # permite mais de uma opcao ser selecionada
      options = list(
        placeholder = 'Selecione as variaveis abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
  })
  
  # tabela as
  output$as <- renderDataTable({
    
    asdt <- tabas() 
    
    if(input$Loadas)
    {
      datatable( asdt,
                 options = list(searching = FALSE,
                                paging=FALSE
                 )   
                 
      )
    } 
    
  })
  
  # Download tabelas ####
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Agregar"                           = tabagregate(),
           "Estrutura"                         = tabestrutura(),
           "Diversidade"                       = tabdiversidade(),
           "BDq Meyer"                         = tabBDq1(),
           "BDq Meyer - Coeficientes"          = tabBDq3(),
           "Matriz Similaridade - Jaccard"     = tabmsimilaridade1(),
           "Matriz Similaridade - Sorensen"    = tabmsimilaridade2(),
           "Pareado Similaridade"              = tabpsimilaridade(),
           "Amostragem Casual Simples"         = tabacs(),
           "Amostragem Casual Estratificada 1" = tabace2(),
           "Amostragem Casual Estratificada 2" = tabace1(),
           "Amostragem Sistematica"            = tabas(),
           "Nivel Parcela"                     = newData() )
  })
  
  output$table <- renderDataTable({
    
    datadownload <- datasetInput()
    
    datatable( datadownload,
               options = list(searching = FALSE,
                              paging=FALSE
               )  )
    
  }) 
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      
      if(input$datasetformat==".csv")
      {
        paste(input$dataset, '.csv', sep='') 
      }
      else if(input$datasetformat==".xlsx")
      {
        paste(input$dataset, '.xlsx', sep='') 
      }
    },
    
    content = function(file) {
      if(input$datasetformat==".csv")
      {
        write.csv(datasetInput(), file, row.names = F)
      }
      else if(input$datasetformat==".xlsx")
      {
        xlsx::write.xlsx2(as.data.frame( datasetInput() ), file, row.names = F)
      }
      
      
      
    }
  )
  
  # Download graficos ####
  
  graphInput <- reactive({
    switch(input$graph_d,
           "Distribuicao - BDq Meyer"  = BDq_graph(),
           "Dendrograma - Jaccard"     = msim1_graph(),
           "Dendrograma - Sorensen"    = msim2_graph() )
  })
  
  output$graph_d_out <- renderPlot({
    
    g <- graphInput()
    
    g

    
  }) 
  
  output$downloadGraph <- downloadHandler(
    filename = function() { 
      
      if(input$graphformat==".png")
      {
        paste(input$graph_d, '.png', sep='') 
      }
      else if(input$graphformat==".jpg")
      {
        paste(input$graph_d, '.jpg', sep='') 
      }
      else if(input$graphformat==".pdf")
      {
        paste(input$graph_d, '.pdf', sep='') 
      }
      
    },
    
    content = function(file) {

        ggsave(file, graphInput(), width = 12, height = 6 )

      
    }
  )
  
  
})
