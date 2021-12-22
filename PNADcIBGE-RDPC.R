##########################################################################

# Limpando arquivos armazenados na memória
rm(list=ls(all=TRUE))

# Definindo limite de memória para compilação do programa
aviso <- getOption("warn")
options(warn=-1)
memory.limit(size=50000)
options(warn=aviso)
rm(aviso)

# Definindo tempo de espera para obtenção de resposta do servidor
aviso <- getOption("warn")
options(warn=-1)
options(timeout=600)
options(warn=aviso)
rm(aviso)

# Definindo opção de codificação dos caracteres e linguagem
aviso <- getOption("warn")
options(warn=-1)
options(encoding="latin1")
options(warn=aviso)
rm(aviso)

# Definindo opção de exibição de números sem representação em exponencial
aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)

# Definindo opção de repositório para instalação dos pacotes necessários
aviso <- getOption("warn")
options(warn=-1)
options(repos=structure(c(CRAN="https://cran.r-project.org/")))
options(warn=aviso)
rm(aviso)

# Definindo diretório de trabalho
caminho <- getwd()
setwd(caminho)

# Carregando pacotes necessários para obtenção da estimativa desejada
if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("PNADcIBGE", dependencies=TRUE)
}
library("PNADcIBGE")
if("dplyr" %in% rownames(installed.packages())==FALSE)
{
  install.packages("dplyr", dependencies=TRUE)
}
library("dplyr")
if("tibble" %in% rownames(installed.packages())==FALSE)
{
  install.packages("tibble", dependencies=TRUE)
}
library("tibble")
if("survey" %in% rownames(installed.packages())==FALSE)
{
  install.packages("survey", dependencies=TRUE)
}
library("survey")

# Obtendo microdados anuais por visita da PNAD Contínua (PNADcIBGE >= 0.6.0)
pnadc_anual_visita <- get_pnadc(year=2019, interview=1, design=FALSE)

# Criando variáveis auxiliares para obtenção da estimativa desejada
pnadc_anual_visita <- transform(pnadc_anual_visita, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc_anual_visita <- transform(pnadc_anual_visita, Pais=as.factor("Brasil"))
pnadc_anual_visita$Pais <- factor(pnadc_anual_visita$Pais, levels=c("Brasil"))
pnadc_anual_visita <- transform(pnadc_anual_visita, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_visita$GR <- factor(pnadc_anual_visita$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Realizando processo de obtenção da estimativa do rendimento domiciliar real
pnadc_anual_visita <- transform(pnadc_anual_visita, V2001_rendimento=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,1))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4019real_proprioano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO1))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_proprioano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO1e))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4019real_ultimoano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO2))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_ultimoano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO2e))
pnadc_anual_visita_rendimento <- pnadc_anual_visita %>% group_by(ID_DOMICILIO) %>% summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),
                                                                                       rendimento_todos_trabalhos_proprioano=sum(VD4019real_proprioano, na.rm=TRUE),
                                                                                       rendimento_outras_fontes_proprioano=sum(VD4048real_proprioano, na.rm=TRUE),
                                                                                       rendimento_todos_trabalhos_ultimoano=sum(VD4019real_ultimoano, na.rm=TRUE),
                                                                                       rendimento_outras_fontes_ultimoano=sum(VD4048real_ultimoano, na.rm=TRUE))
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_proprioano=rendimento_todos_trabalhos_proprioano+rendimento_outras_fontes_proprioano)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_proprioano=VD5007real_proprioano/moradores_rendimento)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_ultimoano=rendimento_todos_trabalhos_ultimoano+rendimento_outras_fontes_ultimoano)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_ultimoano=VD5007real_ultimoano/moradores_rendimento)
pnadc_anual_visita <- pnadc_anual_visita[,!(names(pnadc_anual_visita) %in% c("V2001_rendimento","VD4019real_proprioano","VD4048real_proprioano","VD4019real_ultimoano","VD4048real_ultimoano"))]
pnadc_anual_visita_rendimento <- pnadc_anual_visita_rendimento[,!(names(pnadc_anual_visita_rendimento) %in% c("moradores_rendimento","rendimento_todos_trabalhos_proprioano","rendimento_outras_fontes_proprioano","rendimento_todos_trabalhos_ultimoano","rendimento_outras_fontes_ultimoano"))]
pnadc_anual_visita <- merge(x=pnadc_anual_visita, y=pnadc_anual_visita_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)
rm(pnadc_anual_visita_rendimento)
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_proprioano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_proprioano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ultimoano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ultimoano))
pnadc_anual_visita <- as_tibble(pnadc_anual_visita)
pnadc_anual_visita <- pnadc_design(data_pnadc=pnadc_anual_visita)

# Calculando a massa do rendimento mensal real domiciliar a preços médios do ano
print(rendimento_domiciliar_total_proprioano <- svybys(formula=~VD5007real_proprioano, bys=~Pais+GR+UF, design=subset(pnadc_anual_visita, V2005=="Pessoa responsável pelo domicílio"), FUN=svytotal, na.rm=TRUE))
print(list(cv(rendimento_domiciliar_total_proprioano[[1]]),cv(rendimento_domiciliar_total_proprioano[[2]]),cv(rendimento_domiciliar_total_proprioano[[3]])))

# Calculando o rendimento médio mensal real domiciliar a preços médios do ano
print(rendimento_domiciliar_media_proprioano <- svybys(formula=~VD5007real_proprioano, bys=~Pais+GR+UF, design=subset(pnadc_anual_visita, V2005=="Pessoa responsável pelo domicílio"), FUN=svymean, na.rm=TRUE))
print(list(cv(rendimento_domiciliar_media_proprioano[[1]]),cv(rendimento_domiciliar_media_proprioano[[2]]),cv(rendimento_domiciliar_media_proprioano[[3]])))

# Calculando a massa do rendimento mensal real domiciliar per capita a preços médios do ano (SIDRA - Tabela 7428)
print(rendimento_domiciliar_per_capita_total_proprioano <- svybys(formula=~VD5008real_proprioano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svytotal, na.rm=TRUE))
print(list(cv(rendimento_domiciliar_per_capita_total_proprioano[[1]]),cv(rendimento_domiciliar_per_capita_total_proprioano[[2]]),cv(rendimento_domiciliar_per_capita_total_proprioano[[3]])))

# Calculando o rendimento médio mensal real domiciliar per capita a preços médios do ano (SIDRA - Tabela 7531)
print(rendimento_domiciliar_per_capita_media_proprioano <- svybys(formula=~VD5008real_proprioano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svymean, na.rm=TRUE))
print(list(cv(rendimento_domiciliar_per_capita_media_proprioano[[1]]),cv(rendimento_domiciliar_per_capita_media_proprioano[[2]]),cv(rendimento_domiciliar_per_capita_media_proprioano[[3]])))

# Calculando a massa do rendimento mensal real domiciliar a preços médios do último ano
print(rendimento_domiciliar_total_ultimoano <- svybys(formula=~VD5007real_ultimoano, bys=~Pais+GR+UF, design=subset(pnadc_anual_visita, V2005=="Pessoa responsável pelo domicílio"), FUN=svytotal, na.rm=TRUE))
print(list(cv(rendimento_domiciliar_total_ultimoano[[1]]),cv(rendimento_domiciliar_total_ultimoano[[2]]),cv(rendimento_domiciliar_total_ultimoano[[3]])))

# Calculando o rendimento médio mensal real domiciliar a preços médios do último ano
print(rendimento_domiciliar_media_ultimoano <- svybys(formula=~VD5007real_ultimoano, bys=~Pais+GR+UF, design=subset(pnadc_anual_visita, V2005=="Pessoa responsável pelo domicílio"), FUN=svymean, na.rm=TRUE))
print(list(cv(rendimento_domiciliar_media_ultimoano[[1]]),cv(rendimento_domiciliar_media_ultimoano[[2]]),cv(rendimento_domiciliar_media_ultimoano[[3]])))

# Calculando a massa do rendimento mensal real domiciliar per capita a preços médios do último ano (SIDRA - Tabela 7427)
print(rendimento_domiciliar_per_capita_total_ultimoano <- svybys(formula=~VD5008real_ultimoano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svytotal, na.rm=TRUE))
print(list(cv(rendimento_domiciliar_per_capita_total_ultimoano[[1]]),cv(rendimento_domiciliar_per_capita_total_ultimoano[[2]]),cv(rendimento_domiciliar_per_capita_total_ultimoano[[3]])))

# Calculando o rendimento médio mensal real domiciliar per capita a preços médios do último ano (SIDRA - Tabela 7533)
print(rendimento_domiciliar_per_capita_media_ultimoano <- svybys(formula=~VD5008real_ultimoano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svymean, na.rm=TRUE))
print(list(cv(rendimento_domiciliar_per_capita_media_ultimoano[[1]]),cv(rendimento_domiciliar_per_capita_media_ultimoano[[2]]),cv(rendimento_domiciliar_per_capita_media_ultimoano[[3]])))

##########################################################################
