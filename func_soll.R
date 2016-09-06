# Pacotes ####

suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(lazyeval)

# Modelos standard evaluation ####

# group_by_(.dots = )
# mutate_(.dots    = setNames( list( interp(~ var, var=as.name(var) ) ), nm=names ) )
# summarise_(.dots = setNames( list( interp(~ var, var=as.name(var) ) ), nm=names ) )
# transmute_(.dots = setNames( list( interp(~ var, var=as.name(var) ) ), nm=names ) )
# filter_(interp(~  ,.values = list( ) ) )

# Funcoes ####


round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

smaliancc <- function(df, grupos, di, hi, names = c("AS_CC", "VCC")){
  
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  df %>% 
    group_by_(.dots = grupos) %>% 
    mutate_(
      .dots = 
        setNames(
          list( 
            interp( ~ ( (di^2* pi) / 40000) , di = as.name(di)), 
            interp( ~ ((AS + lead(AS) )/2 ) * (lead(hi) - hi) , AS = as.name("AS_CC"),  hi = as.name(hi))
          ),
          nm=names
        )
    )
}

smaliansc <- function(df, grupos, di, hi, es, names = c("di_sc","AS_SC", "VSC") ){
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  df %>% 
    group_by_(.dots = grupos) %>% 
    mutate_(
      .dots = 
        setNames(
          list(
            interp( ~ di-2*(e_casca/10) , di= as.name(di), es = as.name(es) ),
            interp( ~  (di_sc^2* pi) / 40000 , di_sc = as.name("di_sc")), 
            interp( ~ ((AS + lead(AS) )/2 ) * (lead(hi) - hi) , AS = as.name("AS_SC"),  hi = as.name(hi))
          ),
          nm=names
        )
    )
}

hubercc <- function(df, grupos, di, hi, names = c("secao","AS_CC", "VCC")){
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  df %>% 
    group_by_(.dots = grupos) %>% 
    mutate_(
      .dots = 
        setNames(
          list( 
            interp( ~row_number(grupos[length(grupos) ]), secao = as.name( grupos[length(grupos) ] ) ),
            interp( ~ ( (di^2* pi) / 40000) , di = as.name(di)), 
            interp( ~ ifelse(secao %% 2 == 0, AS * (lead(hi) - lag(hi) )  , NA) , AS = as.name("AS_CC"),  hi = as.name(hi))
          ),
          nm=names
        )
    )
}

hubersc <- function(df, grupos, di, hi, es, names = c("secao","di_sc","AS_SC", "VSC") ){
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  df %>% 
    group_by_(.dots = grupos) %>% 
    mutate_(
      .dots = 
        setNames(
          list(
            interp( ~row_number(grupos[length(grupos) ]), secao = as.name(grupos[length(grupos) ] ) ),
            interp( ~ di-2*(e_casca/10) , di= as.name(di), es = as.name(es) ),
            interp( ~  (di_sc^2* pi) / 40000 , di_sc = as.name("di_sc")), 
            interp( ~ ifelse(secao %% 2 == 0, AS * (lead(hi) - lag(hi) )  , NA) , AS = as.name("AS_SC"),  hi = as.name(hi))
          ),
          nm=names
        )
    )
}

hdjoin <- function(df, grupos, HT, DAP, OBS, dom, names="HD"){
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  x <- df %>%
    group_by_(.dots = grupos) %>%
    filter_( 
      .dots =
        interp(~ !is.na(HT), HT = as.name(HT), .values =  list( HT = as.name(HT) ) ) ,
      interp(~ !is.na(DAP), DAP = as.name(DAP), .values = list( DAP = as.name(DAP) )  ),
      interp(~ OBS == dom, OBS = as.name(OBS), .values = list(OBS = as.name(OBS) ) )
    ) %>%
    summarise_(
      .dots = 
        setNames(
          list(
            interp( ~mean(HT), HT = as.name(HT) )
          ),
          nm=names
        )
    ) %>%
    ungroup
  
  
  df %>%
    filter_( 
      .dots =
        interp(~ !is.na(DAP), DAP = as.name(DAP), .values = list( DAP = as.name(DAP) )  )
    ) %>%
    left_join(x, by = grupos)
  
}

hd <- function(df, grupos, HT, DAP, OBS, dom, names="HD"){
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  df %>%
    group_by_(.dots = grupos) %>%
    filter_( 
      .dots =
        interp(~ !is.na(HT), HT = as.name(HT), .values =  list( HT = as.name(HT) ) ) ,
      interp(~ !is.na(DAP), DAP = as.name(DAP), .values = list( DAP = as.name(DAP) )  ),
      interp(~ OBS == dom, OBS = as.name(OBS), .values = list(OBS = as.name(OBS) ) )
    ) %>%
    summarise_(
      .dots = 
        setNames(
          list(
            interp( ~mean(HT), HT = as.name(HT) )
          ),
          nm=names
        )
    ) %>%
    ungroup
}

nv_parcela <- function(df, grupos, var_medias=c("IDADE", "AREA_TALHAO", "AREA_PARCELA", "HT_EST", "HD"), var_somas_ext=c("AS","VCC", "VSC"), ap = "AREA_PARCELA" ){
  
  var_medias[var_medias==ap] <- "area_parcela"
  names(df)[names(df)==ap] <- "area_parcela"
  
  df %>% 
    group_by_(.dots = grupos) %>% 
    do(cbind( 
      summarise_each_(., funs(mean), var_medias ) ,
      summarise_each_(.,  funs(round(sum(.) * 10000/ mean(area_parcela), 4 ) ) , var_somas_ext  )   )
    )
  
}

inv_summary <- function(df,DAP, HT, VCC, area_parcela, groups, area_total,idade,VSC,Hd) {
  
  require(dplyr)
  require(lazyeval)
  
  if(missing(df)){stop("please insert data frame")}
  if(missing(DAP)){stop("please insert diameter variable")}
  if(missing(HT)){stop("please insert height variable")}
  if(missing(VCC)){stop("please insert volume variable")}
  if(missing(area_parcela)){stop("please insert sample area variable")}
  
  if(missing(groups) || groups==FALSE || is.null(groups) ){groups<-NULL}
  
  # argumentos opcionais
  if(missing(area_total) || area_total==FALSE || is.null(area_total) ){df$area_total<-NA; area_total <- "area_total"}
  if(missing(idade)      || idade==FALSE      || is.null(idade)      ){df$idade<-NA;      idade <- "idade"}
  if(missing(VSC)        || VSC==FALSE        || is.null(VSC)        ){df$VSC<-NA;        VSC <- "VSC"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  
  if(missing(Hd)) { # calculo da altura dominante
    
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
    select_if(Negate(anyNA)) # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
  
}

acs <- function(df, grupos, idade, area_total, area_parcela, VCC, alpha = 0.05, Erro = 10, casas_decimais=4, pop="inf",tidy=T){
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

ace <- function(df, grupos, idade, area_estrato, area_parcela, VCC, alpha = 0.05, Erro = 10, casas_decimais = 4, pop="inf", tidy=T ){
  
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

as_diffs <- function(df, grupos, idade, area_total, area_parcela, VCC, alpha = 0.05, Erro = 10, casas_decimais=4, tidy=T ){
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

