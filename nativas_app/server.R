
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(lazyeval)
library(xlsx)
library(xlsxjars)
library(markdown)


round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

acs <- function(df, grupos, idade, area_total, area_parcela, VCC, alpha = 0.05, Erro = 10, casas_decimais=4, pop="inf",tidy=T)
{
  suppressPackageStartupMessages(require(dplyr))
  require(tidyr)
  require(lazyeval)
  
  if(missing(grupos) || grupos==FALSE || is.null(grupos) ){grupos=NULL}
  
  if(is.null(df)||missing(df) )
  {stop("Escolha o data frame") }
  
  if(is.null(idade)||missing(idade) )
  {stop("Escolha a variavel Idade (meses) ") }
  
  if(is.null(area_total)||missing(area_total) )
  {stop("Escolha a variavel Area total (ha) ") }
  
  if(is.null(area_parcela)||missing(area_parcela) )
  {stop("Escolha a variavel Area da parcela (m2) ") }
  
  if(is.null(VCC)||missing(VCC) )
  {stop("Escolha a variavel Volume (m3)") }
  
  
  
  
  x <-df %>%
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
    round_df(casas_decimais)
  
  
  
  
  
  if(tidy==F)
  {
    return(x)
  } 
  else if(tidy==T & is.null(grupos) )
  {
    x <- as.data.frame(t(x))
    colnames(x) <- "Valores"
    
    x <- x %>%
      mutate(Variaveis=c("Idade (meses)", "Numero de Parcelas (n)","Numero de parcelas cabiveis (N)", "Coeficiente de Variancia (CV)","t-student","t recalculado","n recalculado" ,"Media geral (Y)","Erro-Padrao da Media (Sy)","Erro Absoluto" ,"Erro Relativo (%)","Volume total estimado (Yhat)", "Erro Total","IC (m³/ha) Inferior" ,"IC (m³/ha) Superior","IC Total (m³) inferior","IC Total (m³) Superior") ) %>%
      select(Variaveis, Valores)
    
    return(x)
  }
  else
  {
    
    vec1 <- names(x)[! names(x) %in% grupos ]
    vec2 <- grupos[length(grupos)]
    vec3 <- grupos[grupos!=vec2]
    
    y<- x %>%
      gather_("Variaveis","value", vec1  ) %>% 
      arrange_( grupos ) %>% 
      spread_(vec2,"value",sep="")%>%
      group_by_(.dots=vec3) %>%
      do(.[c(9,10,11,1,14,15,12,16,13,3,4,17,2,5,6,7,8) , ] ) %>%
      mutate(Variaveis=c("Idade (meses)", "Numero de Parcelas (n)","Numero de parcelas cabiveis (N)", "Coeficiente de Variancia (CV)","t-student","t recalculado","n recalculado" ,"Media geral (Y)","Erro-Padrao da Media (Sy)","Erro Absoluto" ,"Erro Relativo (%)","Volume total estimado (Yhat)", "Erro Total","IC (m³/ha) Inferior" ,"IC (m³/ha) Superior","IC Total (m³) inferior","IC Total (m³) Superior") )
    
    return(y)
  }
  
}

ace <- function(df, grupos, idade, area_estrato, area_parcela, VCC, alpha = 0.05, Erro = 10, casas_decimais = 4, pop="inf", tidy=T )
{
  
  if(is.null(df)||missing(df) )
  {stop("Escolha o data frame") }
  
  if(is.null(idade)||missing(idade) )
  {stop("Escolha a variavel Idade (meses) ") }
  
  if(is.null(grupos)||is.na(grupos)||missing(grupos)  )
  {stop("Escolha a(s) variavel(is) de estratificacao") }
  
  if(is.null(area_estrato)||missing(area_estrato) )
  {stop("Escolha a variavel Area do Estrato (ha) ") }
  
  if(is.null(area_parcela)||missing(area_parcela) )
  {stop("Escolha a variavel Area da parcela (m2) ") }
  
  if(is.null(VCC)||missing(VCC) )
  {stop("Escolha a variavel Volume (m3)") }
  
  
  
  
  
  
  suppressPackageStartupMessages(require(dplyr))
  require(tidyr)
  require(lazyeval)
  
  x <- df %>%
    na_if(0) %>%
    group_by_(.dots = grupos) %>%
    summarise_(.dots = setNames( list( interp( ~ mean(area_estrato) / (mean(area_parcela)/10000), area_estrato = as.name(area_estrato), area_parcela = as.name(area_parcela) ) ), nm="Nj" ) ) %>%
    summarise(N  = sum(Nj) ) %>% 
    ungroup %>%
    select(N) %>%
    cbind(df,.) %>% 
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
    round_df(casas_decimais)
  
  y <- x %>%
    group_by_(.dots=grupos[-length(grupos)] ) %>%
    summarise(t            = mean(t),
              Sy           = ifelse(pop=="inf",
                                    sqrt(sum(Pj_Sj)^2 / sum(nj) ),
                                    sqrt(sum(Pj_Sj) ^2 / sum(nj) - (mean(EPj_Sj2) / mean(N) )  )
              ), # Erro-padrao da media
              Y            = sum(Pj_Yj), # media de vcc estratificada (ponderada) 
              Erroabs      = Sy * mean(t), # Erro Absoluto
              Erroperc     = Erroabs / Y * 100, # Erro percentual
              Yhat         = sum(Nj) * Y, # Volume Total
              Erro_Total   = Erroabs * sum(Nj), # Erro Total
              IC_ha_Inf    = Y - Erroabs, # Intervalo de confianca por ha inferior
              IC_ha_Sup    = Y + Erroabs, # Intervalo de confianca por ha superior
              IC_Total_inf = Yhat - Erro_Total, # Intervalo de confianca total inferior
              IC_Total_Sup = Yhat + Erro_Total    ) %>% # Intervalo de confianca total superior)
    round_df(casas_decimais)  
  
  
  
  if(tidy==T)
  {
    vec1 <- names(x)[! names(x) %in% grupos ]
    vec2 <- grupos[length(grupos)]
    vec3 <- grupos[grupos!=vec2]
    vec4 <-  names(y)[! names(y) %in% vec3 ] 
    vec5 <- vec3[length(vec3)]
    vec6 <- vec3[vec3!=vec5]
    
    x <-  x %>%
      gather_("Variaveis","value", vec1) %>% 
      arrange_(grupos) %>% 
      spread_(vec2,"value",sep="") %>%
      group_by_(.dots=vec3) %>%
      do(.[c( 6,10,11,7,13,4,5, 21, 15,14,16,3,2,19,1,17,18,9,12,8,20) , ] ) %>%
      mutate(Variaveis = c("Idade (meses)","numero de amostras / estrato (nj)" ," Numero de amostras cabiveis / estrato (Nj)","Numero de amostras cabiveis (N)", "Proporcao Nj/N (Pj)","Somatorio do volume por estrato (Eyj)", "Soma quadratica do volume por estrato (Eyj2)", "Media do volume por estrato (Yj)", "PjSj2", "PjSj", "PjYj","EPjSj2","EPjSj", "Media Estratificada (Y)", "Coeficiente de Variancia (CV)", "t-student", "t recalculado", "n recalculado","Numero otimo de parcelas por estrato (nj otimo)", "numero otimo de parcelas (n otimo)", "Producao total por estrato (Yhatj)"  ) )
    
    if(length(grupos)!=1 )
    {
      y <- y %>%
        gather_("Variaveis","value", vec4) %>% 
        arrange_(vec3) %>% 
        spread_(vec5,"value") %>%
        group_by_(.dots=vec6) %>%
        do(.[c(9,8,10,2,3,11,1,4,5,6,7 ) , ] ) %>%
        mutate(Variaveis = c("t-student","Erro-Padrao da Media (Sy)","Media estratificada (Y)","Erro Absoluto" ,"Erro Relativo (%)","Volume total estimado (Yhat)", "Erro Total","IC (m³/ha) Inferior" ,"IC (m³/ha) Superior","IC Total (m³) inferior","IC Total (m³) Superior") ) 
      
    }
    else
    {
      
      y <- y %>%
        gather_("Variaveis","value", vec4) %>% 
        do(.[c(9,8,10,2,3,11,1,4,5,6,7 ) , ] ) %>%
        mutate(Variaveis = c("t-student","Erro-Padrao da Media (Sy)","Media estratificada (Y)","Erro Absoluto" ,"Erro Relativo (%)","Volume total estimado (Yhat)", "Erro Total","IC (m³/ha) Inferior" ,"IC (m³/ha) Superior","IC Total (m³) inferior","IC Total (m³) Superior") ) 
      
    }
    
  }
  
  
  z <- list(Tabela1 = x, Tabela2 = y)
  
  return(z)
}

as_diffs <- function(df, grupos, idade, area_total, area_parcela, VCC, alpha = 0.05, Erro = 10, casas_decimais=4, tidy=T )
{
  suppressPackageStartupMessages(require(dplyr))
  require(tidyr)
  require(lazyeval)
  
  if(missing(grupos) || grupos==FALSE || is.null(grupos) ){grupos=NULL}
  
  if(is.null(df)||missing(df) )
  {stop("Escolha o data frame") }
  
  if(is.null(idade)||missing(idade) )
  {stop("Escolha a variavel Idade (meses) ") }
  
  if(is.null(area_total)||missing(area_total) )
  {stop("Escolha a variavel Area total (ha) ") }
  
  if(is.null(area_parcela)||missing(area_parcela) )
  {stop("Escolha a variavel Area da parcela (m2) ") }
  
  if(is.null(VCC)||missing(VCC) )
  {stop("Escolha a variavel Volume (m3)") }
  
  x <- df %>%
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
            interp(~ (sum(diff(VCC)^2) / (2 * n * (n-1) ) ) * ((N-n)/N) , VCC = as.name(VCC), n = as.name("n"), N = as.name("N") ),
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
    round_df(casas_decimais)
  
  
  if(tidy==F)
  {
    return(x)
  } 
  else if(tidy==T & is.null(grupos) )
  {
    x <- as.data.frame(t(x))
    colnames(x) <- "Valores"
    
    x <- x %>%
      mutate(Variaveis=c("Idade (meses)", "Numero de Parcelas (n)","Numero de parcelas cabiveis (N)", "Coeficiente de Variancia (CV)","t-student","Media geral (Y)","Erro-Padrao da Media (Diferencas Sucessivas) (Sy)","Erro Absoluto" ,"Erro Relativo (%)","Volume total estimado (Yhat)", "Erro Total","IC (m³/ha) Inferior" ,"IC (m³/ha) Superior","IC Total (m³) inferior","IC Total (m³) Superior") ) %>%
      select(Variaveis, Valores)
    
    return(x)
  }
  else
  {
    
    vec1 <- names(x)[! names(x) %in% grupos ]
    vec2 <- grupos[length(grupos)]
    vec3 <- grupos[grupos!=vec2]
    
    y<- x %>%
      gather_("Variaveis","value", vec1  ) %>% 
      arrange_( grupos ) %>% 
      spread_(vec2,"value",sep="")%>%
      group_by_(.dots=vec3) %>%
      do(.[c(9,10,11,1,13,14,12,3,4,15,2,5,6,7,8) , ] ) %>%
      mutate(Variaveis=c("Idade (meses)", "Numero de Parcelas (n)","Numero de parcelas cabiveis (N)", "Coeficiente de Variancia (CV)","t-student" ,"Media geral (Y)","Erro-Padrao da Media (Diferencas Sucessivas) (Sy)","Erro Absoluto" ,"Erro Relativo (%)","Volume total estimado (Yhat)", "Erro Total","IC (m³/ha) Inferior" ,"IC (m³/ha) Superior","IC Total (m³) inferior","IC Total (m³) Superior") )
    
    return(y)
  }
  
  
  
}

agregacao <- function(data, col.especies, col.parcelas, rotulo.NI = "NI"){
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

shinyServer(function(input, output, session) {
  
  
  outVar <- reactive({ # iremos salvar os nomes das variaveis do objeto carregado em uma funcao reativa
    
    if(input$Load == 0){return()} # se o botao load nao for pressionado, retornar nada
    inFile <- input$file1 
    if(is.null(inFile)){return(NULL)} # se o arquivo nao for carregado, retornar null
    
    # Carregar o arquivo com base em input
    if(input$excel==F)
    {
      mydata <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec)
      
    }else {
      mydata <- read.xlsx(inFile$datapath, 1)
      
    }
    names(mydata) # nomes das variaveis do arquivo carregado
  })  
  
  observe({ # Com observe iremos atualizar a lista de variaveis em selectizeInput
    
    #agregacao ####
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "col.especies", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "col.parcelas", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    
    # ACS ####
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "idadeacs", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "area_totalacs", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "area_parcelaacs", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "VCCacs", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "gruposacs", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    # ACE ####
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "idadeace", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "area_estratoace", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "area_parcelaace", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "VCCace", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "gruposace", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    
    
    
    
    # ACS ####
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "idadeas", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "area_totalas", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "area_parcelaas", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "VCCas", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "gruposas", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    
  })
  
  newData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
    
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
  
  tabagregate <- reactive({
    
    dados <- newData()
    
    x <- agregacao(dados, input$col.especies, input$col.parcelas, "NI")
    
    x
  })
  
  tabacs <- reactive({
    
    dados <- newData() 
    
    x <-     acs(dados, grupos = input$gruposacs, input$idadeacs, input$area_totalacs, input$area_parcelaacs , input$VCCacs, pop=input$popacs, alpha = input$alphaacs, Erro = input$erroacs, casas_decimais = input$cdacs, tidy=input$tidyacs)
    
    x
    
  })
  
  tabace1 <- reactive({
    
    dados <- newData() 
    x <- ace(dados, grupos = input$gruposace, input$idadeace, input$area_estratoace, input$area_parcelaace , input$VCCace, pop=input$popace, alpha = input$alphaace, Erro = input$erroace, casas_decimais = input$cdace, tidy=input$tidyace)[[1]]
    x
    
  })
  
  tabace2 <- reactive({
    
    dados <- newData() 
    x <- ace(dados, grupos = input$gruposace, input$idadeace, input$area_estratoace, input$area_parcelaace , input$VCCace, pop=input$popace, alpha = input$alphaace, Erro = input$erroace, casas_decimais = input$cdace, tidy=input$tidyace)[[2]]
    x
    
  })
  
  tabas <- reactive({
    
    
    dados <- newData()
    
    x <- as_diffs(dados, grupos = input$gruposas, input$idadeas, input$area_totalas, input$area_parcelaas , input$VCCas, alpha = input$alphaas, Erro = input$erroas, casas_decimais = input$cdas, tidy=input$tidyas)
    
    x
  }) 
  
  output$data <- renderDataTable({ # renderizamos uma DT::DataTable
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    data <- newData() 
    
    datatable(data) # Criamos uma DT::datatable com base no objeto
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
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
  
  output$acs <- renderDataTable({
    
    acsdt <- tabacs() 
    
    if(input$Loadacs)
    {
      datatable( acsdt,
                 options = list(searching = FALSE,
                                paging=FALSE )   )
    } 
    
  })
  
  output$ace1 <- renderDataTable({
    
    ace1dt <- tabace1() 
    
    if(input$Loadace)
    {
      datatable( ace1dt,
                 options = list(searching = FALSE,
                                paging=FALSE )  )
    } 
    
  })
  
  output$ace2 <- renderDataTable({
    
    ace2dt <- tabace2() 
    
    if(input$Loadace)
    {
      datatable( ace2dt,
                 options = list(searching = FALSE,
                                paging=FALSE ) )
    } 
    
  })
  
  output$as <- renderDataTable({
    
    asdt <- tabas() 
    
    if(input$Loadas)
    {
      datatable( asdt,
                 options = list(searching = FALSE,
                                paging=FALSE )    )
    } 
    
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Amostragem Casual Simples" = tabacs(),
           "Amostragem Casual Estratificada 1" = tabace2(),
           "Amostragem Casual Estratificada 2" = tabace1(),
           "Amostragem Sistematica" = tabas())
  })
  
  output$table <- renderTable({
    datasetInput()
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
  
  
})
