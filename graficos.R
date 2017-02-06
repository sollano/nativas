# BDq Meyer Graficos ####

library(tidyverse)
library(ggthemes)
library(ggdendro)

inv <- read.csv("cauaxi_parc10000m2.csv")

res_bdq <- bdq.meyer(inv, "transecto", "dap", 1000)[[1]]

graph_bdq <- res_bdq %>% 
  select("x"                = CentroClasse, 
         "Distribuição observada"  = IndvHectare , 
         "Distribuição balanceada" = MeyerBalan  ) %>% 
  gather(class, y, -x) %>% 
  arrange(x) %>% 
  mutate(x = as.factor(x) )

ggplot(graph_bdq, aes(x = x, y = y) ) + 
  geom_bar(aes(fill = class), stat = "identity",position = "dodge") +
  labs(x = "Centro de classe de DAP (cm)", y = "Individuos por hectare", fill = NULL) + 
  scale_fill_manual(values =c("firebrick2", "cyan3") ) +
  theme_hc(base_size = 14) 
#theme_igray(base_size = 14)



# M Similaridade Graficos ####
library(vegan)

library(tidyverse)
library(ggthemes)
library(ggdendro)

inv <- read.csv("cauaxi_parc10000m2.csv")


ms1 <- as.data.frame(m.similaridade(inv, "nome.cient", "transecto")[[1]])
rownames(ms1) <- levels( inv[,"transecto"] )

ms1_hclust <- hclust( as.dist(1-ms1), method = "complete" ) #?

ggdendrogram(ms1_hclust)
plot(ms1_hclust)
rect.hclust(ms1_hclust, k=3)

ms1k <- kmeans(ms1, 3)

#inv["nome.cient"]
#inv["transecto"]

ms1$cluster <- factor(ms1k$cluster)

# Transforma centers em um dataframe
ms1k_centers <- as.data.frame(ms1k$centers)
ms1k_centers

ggplot(data=ms1, aes(x=T01, y=T02, color=cluster )) + 
  geom_point() + 
  geom_point(data=ms1k_centers, aes(x=T01,y=T02, color='Center')) +
  geom_point(data=ms1k_centers, aes(x=T01,y=T02, color='Center'), size=40, alpha=.3)


# Agrupando as variaveis ####

test <- gather(centers,var, vol) # gather transpoe a tabela
df1 <- gather(df,var,vol, -cluster,-PARCELAS)


# dados em funcao de C1G1
df2 <- cbind(df1 %>%
               filter(!var=="C1G1") ,
             
             df1 %>%
               filter(var=="C1G1") %>%
               select("C1G1"=vol) )

test2 <- cbind(test %>%
                 filter(!var=="C1G1") ,
               
               test %>%
                 filter(var=="C1G1") %>%
                 select("C1G1"=vol) )

View(df2)
View(test2)

# Grafico de C1G1 em funcao dos demais
ggplot(data=df2, aes(x=vol, y=C1G1, color=cluster )) +
  geom_point() +
  geom_point(data=test2, aes(x=vol,y=C1G1, color='Center')) +
  geom_point(data=test2, aes(x=vol,y=C1G1, color='Center'), size=52, alpha=.3) +
  facet_wrap(~var)


