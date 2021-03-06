library(dplyr)
library(readr)
library(gravity)
library(tidyverse)

yeardata <- list.files(path="C:\\Users\\vinic\\Desktop\\Mono\\Dados agrupar", full.names = TRUE, pattern = '*csv')%>%lapply(read_csv)%>%bind_rows




d_anos4=d_anos%>%
  mutate(k_4 = substr(as.character(k),1,4))%>%
  group_by(year,i,j,k_4,iso_i,iso_j)%>%
  summarise(v=sum(v))
names(d_anos4)=c("year","i","j","k_4","iso_i","iso_j", "v")

#NESTE CASO DEBAIXO ESTAMOS DESAGRAGANDO, POR ISSO TEMOS UM BANCO DE DADOS MAIOR.

#Limpar memoria
gc()
memory.limit()
memory.limit(size=56000)


##############################################
setwd("C:\\Users\\vinic\\Desktop\\Mono")
dir()
setwd("C:\\Users\\vinic\\Desktop\\Mono\\Dados agrupar")
dir()


#LENDO OS DADOS GRAVITACIONAIS PARA PAR DE PA�SES DE 1996 A 2019
dg=readRDS("dados gravitacionais")

#LENDO BASE DE DADOS BACI (1996-2019) PARA TODAS AS NCMS E FILTRANDO PARA NCM 87
#Ve�culos autom�veis, tratores, ciclos e outros ve�culos terrestres,
#suas partes e acess�rios
d_anos=readRDS("dados dos anos")

#JOIN D_ANOS COM BASE COUNTRY_CODES PARA PEGAR C�DIGO ISO 3 D�GITOS
country_codes=read.csv("country_codes_V202102.csv")


d_anos=left_join(d_anos,country_codes%>% dplyr::select(country_code,iso_3digit_alpha),
                 by=c("i"="country_code"))
d_anos=left_join(d_anos,country_codes%>% dplyr::select(country_code,iso_3digit_alpha),
                 by=c("j"="country_code"))

#RENOMEANDO VARI�VEIS
names(d_anos)=c("year","i","j","k","v","q","iso_i","iso_j")

#SALVANDO EM CSV
path <- 
  file.path(path.expand("C:\\Users\\vinic\\Desktop\\Mono"),
            "db2.csv")
write.csv(db2,path)
tail(db2)
##############################################

#FILTRANDO PARA NCM 87 E CRIANDO VARI�VEL K_2 (NCM 87),
#AGRUPANDO POR ANO, PA�S EXPORTADOR, IMPORTADOR, NCM,
#C�DIGO PA�S EXPORTADOR, C�GIDO PA�S IMPORTADOR
#E PEGANDO O VALOR EXPORTADO PARA PAR DE PA�SES
#NAM 87: Ve�culos autom�veis, tratores, ciclos e outros ve�culos terrestres,
#suas partes e acess�rios
d_anos2=d_anos%>%
  mutate(k_2 = substr(as.character(k),1,2))%>%
  group_by(year,i,j,k_2,iso_i,iso_j)%>%summarise(v=sum(v))
names(d_anos2)=c("year","i","j","k_2","iso_i","iso_j", "v")


#REALIZANDO UM JOIN COM OS DADOS GRAVITACIONAIS
db2=left_join(d_anos2, dg, by = c("iso_i"="iso3_o","year","iso_j"="iso3_d"))

##############################################
apply(db2, 2, function(col)sum(is.na(col))/length(col))

#apply � uma fun��o para aplicar qualquer fun��o dentro de um banco de dados
#primeiro coloca o banco "," 1 para linha e 2 para coluna "," "function" e escreve a fun��o
#neste caso pegamos todos os NA Values para as colunas e dividimos pela quantidade de valores de coluna

#dist                  distw                distcap 
#0.2794438           0.2836185              0.2794438

##############################################
#TIRANDO NA�S QUE COMPROMETEM A AN�LISE
db2=db2 %>% drop_na(dist)


#PASSANDO VARI�VEIS PARA FATOR E LOG
db2_b<-db2 %>% mutate(
  iso3o_year=factor(paste0(iso_i,year)),
  iso3d_year=factor(paste0(iso_j,year)),
  lngpdo=log(gdp_o),
  lngpdd=log(gdp_d),
  fyear=factor(year),
  fiso3o=factor(iso_i),
  fiso3d=factor(iso_j),
  fsector=factor(k_2))

path <- 
  file.path(path.expand("C:\\Users\\vinic\\Desktop\\Mono"),
            "db2_b.csv")
write.csv(db2_b,path)


#RODANDO UM MODELO QUE NOS RETORNASSE OS VALORES ESPERADOS DE EXPORTA��O
#DOS PRODUTOS PERTENCENTES A NCM 87, LEVANDO EM CONTA AS VARI�VEIS DIST�NCIA EM KM
#DAS CAPITAIS DOS PARES DE PA�SES PIB ORIGEM, PIB DESTINO, FATOR (ANO),
#BIN�RIA (SE OS PA�SES POSSUEM O MESMO IDOMA) E �NDICE DE PROXIMIDADE RELIGIOSA.

#UTILIZANDO O EFEITO FIXO
#efeito fixo serve justamente para capturar,
#em um �nico coeficiente,
#todas as especificidades da unidade, neste caso o ano.
fit <- fixed_effects(
  dependent_variable = "v",
  distance = "dist",
  additional_regressors = c("comrelig","comlang_off", "lngpdo", "lngpdd", "fyear"),
  code_origin = "fiso3o",
  code_destination = "fiso3d",
  robust = FALSE,
  data = db2_b
)

summary(fit)


