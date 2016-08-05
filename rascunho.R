require(dplyr)
require(lazyeval)

if(missing(grupos) || grupos==FALSE || is.null(grupos) ){grupos=NULL}

df %>% 
  group_by_(.dots = grupos) %>% 
  mutate_(.dots  = setNames( list( interp(~ 0.000503 * DAP^2.187162, DAP=as.name(DAP) ) ), nm="VOL" ) ) %>% 
  summarise_(
    .dots =  
      setNames(  list(  
        interp(~ round(mean(as.numeric(idade) ), 1), idade = as.name(idade)  ),
        interp(~ mean(area_talhao), area_talhao = as.name(area_talhao))    ,
        interp(~ mean(area_parcela), area_parcela = as.name(area_parcela)) ,
        interp(~ mean(DAP), DAP = as.name(DAP) ) ,
        ~ round(sqrt(mean(AS) * 40000 / pi), 2)  ,
        interp(~mean(HT), HT = as.name(HT) ),
        interp(~mean(HD), HD = as.name(HD) ),
        interp(~round(sum(AS) * 10000/ mean(area_parcela), 4 ), AS = as.name("AS"), area_parcela = as.name(area_parcela) ),
        interp(~round(sum(VCC) * 10000/ mean(area_parcela), 4 ), VCC = as.name(VCC), area_parcela = as.name(area_parcela) ),
        interp(~round(sum(VSC) * 10000/ mean(area_parcela), 4 ), VSC = as.name(VSC), area_parcela = as.name(area_parcela) )
        
      ), #list 
      nm = c("IDADE", "AREA_TALHAO", "AREA_PARCELA_", "DAP", "q", "HT", "HD", "G", "VCC", "VSC" ) 
      )#setnames 
  )#sumarise
