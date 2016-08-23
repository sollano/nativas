inv_summary <- function(df,DAP, HT, VCC, area_parcela, grupos, area_total,idade,VSC,Hd) {
  
  require(dplyr)
  require(lazyeval)
  
  if(missing(df)){stop("please insert data frame")}
  if(missing(DAP)){stop("please insert diameter variable")}
  if(missing(HT)){stop("please insert height variable")}
  if(missing(VCC)){stop("please insert volume variable")}
  if(missing(area_parcela)){stop("please insert sample area variable")}

  if(missing(grupos) || grupos==FALSE || is.null(grupos) ){grupos=NULL}
  
  # argumentos opcionais
  if(missing(area_total) || area_total==FALSE || is.null(area_total) ){df$area_total=NA; area_total <- "area_total"}
  if(missing(idade)      || idade==FALSE      || is.null(idade)      ){df$idade=NA;      idade <- "idade"}
  if(missing(VSC)        || VSC==FALSE        || is.null(VSC)        ){df$VSC=NA;        VSC <- "VSC"}
  
  # argumentos de area podem ser numericos
  if(is.numeric(area_parcela)){df$area_parcela <- area_parcela; area_parcela <- "area_parcela"}
  if(is.numeric(area_total  )){df$area_total   <- area_total; area_total     <- "area_total"}
  
  
  if(missing(Hd)) # calculo da altura dominante
  {
    if(is.null(grupos))
    {
      x <-  
        
      suppressMessages(   # remove mensagens do dplyr
        df %>% 
          select_(ht = HT) %>% 
          top_n(2) %>% # seleciona as duas maiores arvores
          summarise(HD = mean(ht) ) %>% 
          cbind(df) # como nao ha grupos, usamos cbind
      )    }
    else
    {
      x <-  suppressMessages(
        
        df %>% 
          group_by_(.dots = grupos) %>% 
          select_(ht = HT) %>% 
          top_n(2) %>% 
          summarise(HD = mean(ht) ) %>% 
          full_join(df) # como ha grupos, usamos join
        
      )
      }
    
  } else
  {
    x <- df %>% rename_(HD = Hd)
  }
  
  
  x %>% 
    group_by_(.dots = grupos) %>% 
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
          interp(~round(sum(AS,  na.rm=T) * 10000/ mean(area_parcela, na.rm=T), 4 ), AS = as.name("AS"), area_parcela = as.name(area_parcela) ),
          interp(~round(sum(VCC, na.rm=T) * 10000/ mean(area_parcela, na.rm=T), 4 ), VCC = as.name(VCC), area_parcela = as.name(area_parcela) ),
          interp(~round(sum(VSC, na.rm=T) * 10000/ mean(area_parcela, na.rm=T), 4 ), VSC = as.name(VSC), area_parcela = as.name(area_parcela) )
          
        ), #list 
        nm = c("IDADE","AREA_TOTAL","AREA_PARCELA", "DAP", "q", "HT", "HD","G", "VCC","VSC" ) 
        )#setnames 
    ) %>% #sumarise 
    na_if(0) %>% # substitui 0 por NA
    select_if(Negate(anyNA)) # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
  
  }


