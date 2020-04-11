### Gui Russo e equipe CepespData
## Quantos candidatos a vereador teremos nas eleições de 2020?
# 3 de Março, 2019

##### DO REGISTRO DE CANDIDATOS ----------------------------------- 
# Fonte: http://www.tse.jus.br/legislacao/codigo-eleitoral

# Art. 10. Cada partido ou coligação poderá registrar candidatos para a Câmara dos Deputados, 
# a Câmara Legislativa, as assembleias legislativas e as câmaras municipais no total de até 150% (cento e cinquenta por cento)
# do número de lugares a preencher, salvo:

#  Caput com redação dada pelo art. 2º da Lei nº 13.165/2015.
#CF/1988, art. 29, IV e alíneas, com redação dada pela EC nº 58/2009: critérios para fixação do número de vereadores; Ac.-STF, de 24.3.2004, no RE nº 197.917: aplicação de critério aritmético rígido no cálculo do número de vereadores.
#LC nº 78/1993: "Disciplina a fixação do número de deputados, nos termos do art. 45, § 1º, da Constituição Federal".
#I – nas unidades da Federação em que o número de lugares a preencher para a Câmara dos Deputados não exceder a doze, 
# nas quais cada partido ou coligação poderá registrar candidatos a deputado federal e a deputado estadual ou distrital
# no total de até 200% (duzentos por cento) das respectivas vagas;

#II – nos municípios de até cem mil eleitores, nos quais cada coligação poderá registrar candidatos no total 
# de até 200% (duzentos por cento) do número de lugares a preencher.

#Incisos I e II acrescidos pelo art. 2º da Lei nº 13.165/2015.

rm(list=ls()); cat("\014")

# Pacote do CepespData (cepespR) -----------------------------------
library(cepespR)
library(stringr)
library(tidyverse)
library(WriteXLS)

# Candidatos na eleições para vereador de 2016 (não vamos nos importar se deferido ou não)
df<-get_candidates(2016, "Vereador", cached=T, 
                   columns_list=c("DESCRICAO_ELEICAO", "SIGLA_UE", "NOME_MUNICIPIO",
                                  "SIGLA_UF", "DESCRICAO_UE", "NUMERO_CANDIDATO", "NOME_CANDIDATO","CPF_CANDIDATO", 
                                  "NUMERO_PARTIDO", "CODIGO_LEGENDA", "COMPOSICAO_LEGENDA", 
                                  "CODIGO_SEXO", "DESC_SIT_TOT_TURNO", "DES_SITUACAO_CANDIDATURA"))

table(df$DESCRICAO_ELEICAO) # Verificando que estamos apenas olhando para as eleições ordinárias
nrow(df)

table(df$DES_SITUACAO_CANDIDATURA)
#df<-df %>% filter(DES_SITUACAO_CANDIDATURA=="DEFERIDO" | 
#                  DES_SITUACAO_CANDIDATURA=="DEFERIDO COM RECURSO" | 
#                  DES_SITUACAO_CANDIDATURA=="INDEFERIDO COM RECURSO" |
#                  DES_SITUACAO_CANDIDATURA=="CANCELADO COM RECURSO" |
#                  DES_SITUACAO_CANDIDATURA=="CASSADO COM RECURSO" |
#                  DES_SITUACAO_CANDIDATURA=="PENDENTE DE JULGAMENTO")
nrow(df)  # 437940=~ quatroscentos trinta e oito mil
nrow(df[df$SIGLA_UF=="SP",])

# Agora precisamos saber qual é o número de vagas em cada município?
vagas<-readr::read_csv2("http://cepespdata.io/static/docs/vagas_vereadores.csv")
head(vagas)

# Vamos tratar a sigla do TSE para podermos fazer o join com nosso banco mais na frente
vagas$SIGLA_UE<-str_pad(vagas$cod_tse, width = 5, pad = 0)
head(vagas$SIGLA_UE)

# Vamos usar apenas 2016 para nossa simulação
vagas$vagas<-vagas$`2016`
sum(vagas$vagas) # Total de vagas de vereador no Brasil em 2016
sum(vagas$vagas[vagas$uf=="SP"])
table(vagas$vagas) # Número de municípios com X cadeiras nas Câmaras Municipais

# Podemos 'dropar' os outros anos e variáveis
vagas<- vagas[, c("uf", "SIGLA_UE", "vagas")]
head(vagas)

# Número de cands por número de vagas:
nrow(df)/sum(vagas$vagas, na.rm=T)
nrow(df[df$SIGLA_UF=="SP",])/sum(vagas$vagas[vagas$uf=="SP"])

head(df)

### Cenário máximo ### ---------------------------------------------------------------

# Regra: Cada Partido pode registrar até 150% do número de lugares a preencher.
# 150% de 9 = 13,5. Como a fração é igual a meio, aproxima-se para 14, podendo o
# partido ou a coligação, neste caso, registrar até 14 candidatos.

# Hoje, são 32 partidos registrados no TSE: http://www.tse.jus.br/partidos/partidos-politicos/registrados-no-tse
vagas$max<-ceiling(vagas$vagas*1.5)*32 # ceiling é uma função que arredonda para cima 
# número de vagas x 151%(regra do TSE) x 32 partidos registrados
sum(vagas$max) # 2869120, dois milhões oitocentos sessenta nove mil
sum(vagas$max[vagas$uf=="SP"]) 

sum(vagas$max)/sum(vagas$vagas, na.rm=T)
sum(vagas$max[vagas$uf=="SP"])/sum(vagas$vagas[vagas$uf=="SP"])

plot(jitter(vagas$vagas), jitter(vagas$max),
     main="Cenário- Máximo",
     xlab="Tamanho da Câmara", ylab="Número de candidatos a vereador", yaxt='n')
axis(2, las=2)

### Cenário mantendo partidos ### ----------------------------------------------------

# Porém, sabemos que a quantidade de partidos competindo em cada município é diferente
# Por exemplo,

unique(df$NUMERO_PARTIDO[df$DESCRICAO_UE=="BRAGANÇA PAULISTA"]) # 27 partidos lançaram candidatx em Bragança, 
unique(df$NUMERO_PARTIDO[df$DESCRICAO_UE=="VARGEM"]) # mas só 11 partidos lançaram na vizinha Vargem

# Por isso vamos criar um data.frame com a quantidade de partidos que competiram por município em 2016
partidos_competindo_df<-data.frame(SIGLA_UE=unique(df$SIGLA_UE), partidos_competindo_2016=NA)
head(partidos_competindo_df)

table(df$NUMERO_PARTIDO) # Quantos candidatos cada partido lançou.

# 3 partidos deixaram de existir desde 2016: PHS, PRB, PPL

nrow(df)
df<-df %>% filter(NUMERO_PARTIDO!=c(31, 44, 54))
nrow(df)

for(i in partidos_competindo_df$SIGLA_UE){ # para cada município
  partidos_competindo_df$partidos_competindo_2016[partidos_competindo_df$SIGLA_UE==i]<- # atribua à variável partidos
    length( # o número
      unique(df$NUMERO_PARTIDO[df$SIGLA_UE==i])
    )
}

# Vamos analisar como é a distribuição entre os municípios
summary(partidos_competindo_df$partidos_competindo_2016) # De 2 a 35 partidos por município em 2016
hist(partidos_competindo_df$partidos_competindo_2016, main="Municípios por # de Partidos Competindo",
     xlab="Número de Partidos", yaxt='n')
axis(2, las=2)

# Atualmente, há 32 partidos registrados no TSE, então vamos criar uma variável equivalente,
# mas que coloque um teto de 32

partidos_competindo_df$partidos_competindo_2020<-partidos_competindo_df$partidos_competindo_2016
partidos_competindo_df$partidos_competindo_2020[partidos_competindo_df$partidos_competindo_2020>32]<-32
table(partidos_competindo_df$partidos_competindo_2020)

# Agora, vamos juntar o banco de vagas com o banco de partidos competindo
template_analise<-merge(vagas, partidos_competindo_df, by="SIGLA_UE")
head(template_analise)

# E calcular o número máximo de candidatos levando em consideração o número de partidos em 2016
# Número de vagas x 150% x número de partidos que competiram em 2016
template_analise$max_partidos_competindo<-
  ceiling(template_analise$vagas*1.5)*template_analise$partidos_competindo_2020 
sum(template_analise$max_partidos_competindo)
sum(template_analise$max_partidos_competindo[template_analise$uf=="SP"])

sum(template_analise$max_partidos_competindo)/sum(vagas$vagas, na.rm=T)
sum(template_analise$max_partidos_competindo[template_analise$uf=="SP"])/sum(vagas$vagas[vagas$uf=="SP"])  # 1327412

plot(jitter(template_analise$vagas), jitter(template_analise$max_partidos_competindo),
     main="Cenário- Partidos em 2016 Máximo",
     xlab="Tamanho da Câmara", ylab="Número de candidatos a vereador", yaxt='n')
axis(2, las=2)

# Incorporando "algum" nível de coordenação:
# Primeiro vamos descobrir quantos candidatos concorreram por municipio em 2016
head(df)
df$cand2016<-1 # todo candidato vale um
cand2016<-aggregate(cand2016~SIGLA_UE, df, sum) # quanto é a soma da variável cand2016 por SIGLA_UE
head(cand2016)

# Adicionar no template de analise a quantidade de candidatos de 2016
template_analise<-merge(template_analise, cand2016, by="SIGLA_UE")
head(template_analise)

# Aproximação de 2016 com cenário
# Meio a meio
template_analise$cands_2020_metade2016<-(template_analise$cand2016+template_analise$max_partidos_competindo)/2
sum(template_analise$cands_2020_metade2016)
sum(template_analise$cands_2020_metade2016[template_analise$uf=="SP"])

sum(template_analise$cands_2020_metade2016)/sum(vagas$vagas, na.rm=T)
sum(template_analise$cands_2020_metade2016[template_analise$uf=="SP"])/sum(vagas$vagas[vagas$uf=="SP"], na.rm=T)

# Um terço de coordenação
template_analise$max_1terco_coord<-(2*template_analise$cand2016+template_analise$max_partidos_competindo)/3 # Mais coordenação
sum(template_analise$max_1terco_coord)
sum(template_analise$max_1terco_coord[template_analise$uf=="SP"])

sum(template_analise$max_1terco_coord)/sum(vagas$vagas, na.rm=T)
sum(template_analise$max_1terco_coord[template_analise$uf=="SP"])/sum(vagas$vagas[vagas$uf=="SP"], na.rm=T)

template_analise$max_twothird16<-(template_analise$cand2016+2*template_analise$max_partidos_competindo)/3 # Menos coordenação
sum(template_analise$max_twothird16)
sum(template_analise$max_twothird16[template_analise$uf=="SP"])

sum(template_analise$max_twothird16)/sum(vagas$vagas, na.rm=T)
sum(template_analise$max_twothird16[template_analise$uf=="SP"])/sum(vagas$vagas[vagas$uf=="SP"], na.rm=T)

# Entretanto, é altamente improvável que TODOS os partidos
# lancem o número máximo de candidatos em TODOS os municípios.

# Podemos observar até que ponto os partidos já faziam isso no passado, 
# e depois estimar quanto de coordenação há por município 

head(template_analise)

template_analise$coord<-template_analise$cand2016/template_analise$max_partidos_competindo
template_analise$ncand_coord<-template_analise$coord*template_analise$max  

hist(template_analise$coord, main="Distribuição da taxa de coordenação",
     xlab="Taxa de Coordenação", yaxt='n')
axis(2, las=2)

plot(jitter(template_analise$vagas), jitter(template_analise$coord), 
     xlab="Número de Vagas", ylab="Taxa de Coordenação",
     main="Taxa de coordenação por Número de Vagas")

plot(jitter(template_analise$vagas), jitter(template_analise$ncand_coord),
     main="Cenário- Coordenação igual a 2016",
     xlab="Tamanho da Câmara", ylab="Número de candidatos a vereador", yaxt='n')
axis(2, las=2)

plot(jitter(template_analise$vagas), jitter(template_analise$ncand_coord),
     main="Cenário- Coordenação igual a 2016",
     xlab="Tamanho da Câmara", ylab="Número de candidatos a vereador", yaxt='n', 
     xlim=c(8, 14), ylim=c(0, 500))
axis(2, las=2)

sum(template_analise$ncand_coord)
sum(template_analise$ncand_coord[template_analise$uf=="SP"])

sum(template_analise$ncand_coord)/sum(vagas$vagas, na.rm=T)
sum(template_analise$ncand_coord[template_analise$uf=="SP"])/sum(vagas$vagas[vagas$uf=="SP"], na.rm=T)

head(template_analise)

WriteXLS(template_analise, "N_Cand_Vereador_20162020.XLS")
