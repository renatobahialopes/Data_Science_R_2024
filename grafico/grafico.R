
install.packages(c("summarytools", "fdth", "ggplot2",
                   "readxl", "readr","dplyr"))

install.packages(c("Rtools"))
stall.packages("lubridate")
library(lubridate)

require(summarytools)
require(fdth)
require(ggplot2)
require(readxl)
require(readr)
require(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(cowplot)

install.packages("jsonlite", type = "source")
install.packages("psych", type = "source")
install.packages("geosphere", type = "source")
library(jsonlite)
library(psych)
library(geosphere)

install.packages("devtools")
library(devtools)
install.packages("Rcmdr", dependencies = T)
library(Rcmdr)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage")
library(EBImage)

install.packages("h2o")
library(h2o)

install.packages("tinytex")
tinytex::install_tinytex()
########################################################################################
install.packages("tidyverse")
library(tidyverse)
require(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(cowplot)

################ importar dados ###########################################################
dados <- read.csv('/dados/si-veic-2022.csv',fileEncoding='latin1',check.names=F,sep=';')
View(dados)
dados["data"]<-as.Date(dados$data_hora_boletim, format="%d/%m/%Y")
dados["ano"]<- year(dados$data)
dados["mes"]<-month(dados$data, label = T)
View(dados)

dados_env <- read.csv('/dados/si-env-2022.csv',fileEncoding='latin1',check.names=F,sep=';')
View(dados_env)
dados_env["data"]<-as.Date(dados_env$data_hora_boletim, format="%d/%m/%Y")
dados_env["ano"]<- year(dados_env$data)
dados_env["mes"]<-month(dados_env$data, label = T)
dados_env <-filter(dados_env,sexo>0)
View(dados_env)
######agrupamento ######################################################################################
grouped_env <-dados_env %>% 
  group_by(desc_severidade, .drop = FALSE) %>% count()
grouped_env <- rename(grouped_env, c("severidade" = "desc_severidade", "ocorrencias" = "n"))
View(grouped_env)

grouped_env_idade <-dados_env %>% 
  group_by(Idade, .drop = FALSE) %>% count()
grouped_env_idade <- rename(grouped_env_idade, c("ocorrencias" = "n"))
grouped_env_idade <-filter(grouped_env_idade,Idade>0)
View(grouped_env_idade)

grouped_dados <-dados %>% 
  group_by(descricao_especie, .drop = FALSE) %>% count()
grouped_dados <- rename(grouped_dados, c("especie" = "descricao_especie", "ocorrencias" = "n"))
View(grouped_dados)

socorro <-dados %>% 
  group_by(desc_tipo_socorro, .drop = FALSE) %>% count()
socorro <- rename(socorro, c("socorro" = "desc_tipo_socorro", "ocorrencias" = "n"))
View(socorro)

movimento <-dados %>% 
  group_by(desc_situacao, .drop = FALSE) %>% count()
movimento <- rename(movimento, c("tipo" = "desc_situacao", "ocorrencias" = "n"))
View(movimento)

dados_mes <-dados %>% 
  group_by(mes, .drop = FALSE) %>% count()
dados_mes <- rename(dados_mes, c("ocorrencias" = "n"))
View(dados_mes)

dados_ano <-dados %>% 
  group_by(ano) %>% 
    summarise(n = n())
dados_ano <- rename(dados_ano, c("ocorrencias" = "n"))
View(dados_ano)
################################ gerar graficos ###########################################

##barplot(table(grouped_dados))
##barplot((table(grouped_dados)/length(grouped_dados)*100), xlab = "refrigerante", ylab = "% entrevistados", col="lightgreen", ylim=c(0,40))
##ploot <- ggplot(grouped_dados,aes(x ="descricao_especie",y = "qtdoc"))
##view(ploot)

#ggplot(ocorrencias, aes(ocorrencias,especie,size=ocorrencias,color = especie))+
ggplot(grouped_dados, aes(especie,ocorrencias,color = especie))+
  geom_point()

ggplot(grouped_env, aes(severidade,ocorrencias,color = severidade))+
  geom_point()

ggplot(grouped_env, aes(severidade,ocorrencias,label = ocorrencias))+
  geom_bar(stat = "identity",fill = "green")+
  geom_label()+
  coord_flip()

ggplot(dados_env,aes(x=sexo))+geom_bar()+
  labs(title='Ocorrencias de Acidentes 2022',x='sexo',y='Contagem')

ggplot(dados_env,aes(x=cinto_seguranca))+geom_bar()+
  labs(title='Ocorrencias de Acidentes 2022',x='Cinto Seguranca',y='Contagem')

ggplot(dados_env,aes(x=Embreagues))+geom_bar()+
  labs(title='Ocorrencias de Acidentes 2022',x='Embreagues',y='Contagem')
  +theme_minimal()

ggplot(grouped_env_idade, aes(Idade,ocorrencias,label = ocorrencias))+
  geom_bar(stat = "identity",fill = "green")+
  geom_label()
#  +coord_flip()

ggplot(graf_ocorrencias, aes(especie,ocorrencias,label = ocorrencias))+
  geom_bar(stat = "identity",fill = "green")+
  geom_label()+
  coord_flip()

ggplot(dados_mes, aes(mes,ocorrencias,label = ocorrencias))+
  geom_bar(stat = "identity",fill = "green")+
  geom_label()+
  coord_flip()

ggplot(socorro, aes(socorro,ocorrencias,label = ocorrencias))+
  geom_bar(stat = "identity",fill = "green")+
  geom_label()+
  coord_flip()

ggplot(movimento, aes(tipo,ocorrencias,label = ocorrencias))+
  geom_bar(stat = "identity",fill = "green")+
  geom_label()+
  coord_flip()

ggplot(socorro)+ aes(socorro,ocorrencias)+
    geom_line(col="#00AFBB",lwd=.5)+
    geom_line(aes(y=socorro),col="read")
    +theme_bw()

#######################mostrar##############################
graf_mes = ggplot(dados_mes, aes(mes,ocorrencias,label = ocorrencias))+
  geom_bar(stat = "identity",fill = "green")+
  #geom_label()+
  coord_flip()

graf_idade = ggplot(grouped_env_idade, aes(Idade,ocorrencias,label = ocorrencias))+
  geom_bar(stat = "identity",fill = "green")
#+  geom_label()
#  +coord_flip()

graf_sexo = ggplot(dados_env,aes(x=sexo))+geom_bar()+
  labs(title='Ocorrencias de Acidentes 2022',x='sexo',y='Contagem',,fill = "green")

graf_condutor = ggplot(dados_env,aes(x=condutor))+geom_bar()+
  labs(title='Ocorrencias de Acidentes 2022',x='Condutor',y='Contagem',,fill = "green")


plot_grid(graf_sexo,graf_condutor,graf_mes,graf_idade)




