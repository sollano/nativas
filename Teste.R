source("diversidade.R")
source("pareadoSimilaridade.R")
source("matrizSimilaridade.R")
source("estrutura.R")
source("BDq.R")
source("agregacao.R")
source("inv_summary.R")

inv = read.csv("cauaxi 2012.csv")
#inv2 = read.csv("cauaxi_parc10000m2.csv")
head(estrutura(inv, "scientific.name", "DBH", "transect", 1000)) # horizontal
head(estrutura(inv, "scientific.name", "DBH", "transect", 1000, "canopy")) # vertical
head(estrutura(inv, "scientific.name", "DBH", "transect", 1000, "canopy", "light")) # vertical + interna
diversidade(inv, "scientific.name", indice = "H")
diversidade(inv, "scientific.name", indice = "S")
diversidade(inv, "scientific.name", indice = "Hmax")
diversidade(inv, "scientific.name", indice = "J")
diversidade(inv, "scientific.name")
p.similaridade(inv[inv$transect == "T01","scientific.name"], inv[inv$transect == "T02","scientific.name"])
m.similaridade(inv, "scientific.name", "transect")
bdq.meyer(inv, "transect", "DBH", 1000)[[1]]
agregacao(inv, "scientific.name", "transect")

inv = read.csv("ducke.csv")
estrutura(inv, "scientific_name", "DBH_11", "transect", 1000, "canopy_11", "light_11")
diversidade(inv, "scientific_name", indice = "H")
diversidade(inv, "scientific_name", indice = "S")
diversidade(inv, "scientific_name", indice = "Hmax")
diversidade(inv, "scientific_name", indice = "J")
diversidade(inv, "scientific_name")
m.similaridade(inv, "scientific_name", "transect")
bdq.meyer(inv, "transect", "DBH_11", 1000)[1]
agregacao(inv, "scientific_name", "transect")

# Totalizacao de Parcelas ####

# calculo do volume e add da coluna de area (em m²)
library(dplyr)
library(lazyeval)

inv <- read.csv("cauaxi 2012.csv") %>% 
  mutate(
    VOL           = 0.000503 * DBH^2.187162,
    total.area    = 50,
    transect.area = 1000        )

#write.csv(inv, "cauaxi 2012_.csv", row.names = F)

# obs: a funcao a seguir requer as versoes mais recentes dos pacotes dplyr & lazyeval;
# A funcao inv_summary gera valores de dap, diametro quadratico e altura e altura dominante medias;
# e somas de area basal e volume;
# Melhor utilizada para passar tabelas em nivel de arvore para nivel de parcela;

# a altura dominante pode ser fornecida como nome de coluna pelo argumento "Hd"; 
# caso contrario, sera calculada com base nas duas maiores arvores;

# argumentos adicionais calculam a media da area total, media da idade, e media do volume sem casca
# outro argumento adicional e o "grupos".
# caso nao seja fornecido, ou seja falso, o sumario ira considerar o df como um todo
# caso ele seja fornecido, o inventario sera feito por n grupo(s)

# Inv geral, sem grupos
inv_summary(inv, "DBH","Htot", "VOL", "transect.area", groups = F)

# Inv geral, por parcela
inv_summary(inv,"DBH", "Htot", "VOL", "transect.area", groups = "transect")

# Inv geral, por especie
inv_summary(inv,"DBH", "Htot", "VOL", "transect.area", groups = "scientific.name")

# Inv geral, por parcela e especie
inv_summary(inv, "DBH", "Htot", "VOL", "transect.area", groups = c("transect", "scientific.name") )

# Argumento de area pode ser numerico
inv_summary(inv, "DBH", "Htot", "VOL", 1000, groups = c("transect", "scientific.name") )

# testando mensagens de erro 
inv_summary()
inv_summary(inv)
inv_summary(inv, "DBH")
inv_summary(inv, "DBH", "Htot")
inv_summary(inv, "DBH", "Htot", "VOL")
inv_summary(inv, "DBH", "Htot", "VOL", "transect.area")


inv2 <- read.csv("ducke.csv") %>% 
  mutate(
    VOL           = 0.000503 * DBH_11^2.187162,
    total.area    = 50,
    transect.area = 1000        )

# Inv geral, sem grupos
inv_summary(inv2, "DBH_11","Htot_11", "VOL", "transect.area", groups = F)

# Inv geral, por parcela
inv_summary(inv2,"DBH_11", "Htot_11", "VOL", "transect.area", groups = "transect")

# Inv geral, por parcela e especie
inv_summary(inv2, "DBH_11", "Htot_11", "VOL", "transect.area", groups = c("transect", "scientific.name") )

# Argumento de area pode ser numerico
inv_summary(inv2, "DBH_11", "Htot_11", "VOL", 1000, groups = c("transect", "scientific.name") )


