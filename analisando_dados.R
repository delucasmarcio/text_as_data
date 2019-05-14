## Carregando Bibliotecas
library(tidyverse)
library(data.table)
library(tidytext)
library(tm)
library(glue)
library(stringr)
library(stringi)
library(rvest)
library(readr)
library(ptstem)
library(wordcloud2)
library(quanteda)
library(igraph)
library(dotwhisker)
library(broom)
library(dplyr)

# Stopwords
sw <- read.csv('~/portfolio/discursos/stop_words.txt', 
               sep = ';')

## Dados de Discursos
df <- fread('~/portfolio/discursos/data/dados_discursos_deputados_tratados.csv',
            sep = '\t',header = T, encoding = 'UTF-8')

df <- as.data.frame(df)

df <- df[c('id','text')]

## Dados de Políticos
cd1 = fread('~/portfolio/militaristas/data/dados_candidatos_02_18.txt', sep = ';')
cd2 = fread('~/portfolio/militaristas/data/dados_candidatos_98.txt', sep = ';')

df_cad = rbind(cd1,cd2)

# Populista de Profissão
seguranca <- c('POLICIAL MILITAR','MILITAR','MILITAR REFORMADO',
               'POLICIAL CIVIL','MEMBRO DAS FORÇAS ARMADAS',
               'MEMBRO DAS FORCAS ARMADAS','DELEGADO DE POLICIA')

df_cad$prof_seg <- ifelse(df_cad$profissao %in% seguranca & 
                            df_cad$resultado %in% c('ELEITO',
                                                    'ELEITO POR MÉDIA',
                                                    'ELEITO POR QP',
                                                    'MÉDIA'), 1, 0)

# Populista de Designação
desig_militar <- c('SOLDADO','CABO','SARGENTO','CADETE','CAPITAO',
                   'MAJOR','TENENTE','CORONEL','GENERAL','MARECHAL',
                   'POLICIAL','GUARDA','MILITAR','COMANDANTE',
                   'ALMIRANTE','SGT')

Find_Security <- function(x){
  ll <- strsplit(x,' ')
  ll <- unlist(ll)
  out = 0
  if(length(x) < length(ll)){
    for(k in ll){
      if(k %in% desig_militar)
        out = 1
    }
  }
  return(out)
}

df_cad$nome_urna <- as.character(df_cad$nome_urna)
df_cad$seguranca <- apply(as.matrix(df_cad$nome_urna),1, Find_Security)
df_cad$seguranca <- ifelse(df_cad$seguranca == 1 & 
                             df_cad$resultado %in% c('ELEITO',
                                                     'ELEITO POR MÉDIA',
                                                     'ELEITO POR QP',
                                                     'MÉDIA'), 1, 0)

## Nomes dos Políticos
bolsonaro = c('JAIR BOLSONARO','FLAVIO BOLSONARO','EDUARDO BOLSONARO')

pop_prof = levels(as.factor(df_cad$nome_urna[df_cad$prof_seg == 1 &
                                               df_cad$ano >= 2008]))

pol_prof = c(pop_prof, 
             levels(as.factor(df_cad$nome[df_cad$prof_seg == 1 &
                                            df_cad$ano >= 2008])))

pop_prof = pop_prof[!(pop_prof %in% bolsonaro)]

pop_desi = levels(as.factor(df_cad$nome_urna[df_cad$seguranca == 1 &
                                               df_cad$ano >= 2008]))

pop_desi = c(pop_desi, 
             levels(as.factor(df_cad$nome[df_cad$seguranca == 1 &
                                            df_cad$ano >= 2008])))

pop_desi = pop_desi[!(pop_desi %in% bolsonaro)]

outros = c(levels(as.factor(df_cad$nome_urna)),
           levels(as.factor(df_cad$nome)))

outros = outros[!(outros %in% c(bolsonaro,pop_prof,pop_desi))]

## Definindo ID
df$siglaPartido[df$siglaPartido == 'PP**'] <- 'PP'
df$siglaPartido[df$siglaPartido == 'MDB'] <- 'PMDB'
df$siglaPartido[df$siglaPartido == 'SOLIDARIEDADE'] <- 'SD'
df$siglaPartido[df$siglaPartido == 'AVANTE'] <- 'PTdoB'
df$siglaPartido[df$siglaPartido == 'PATRI'] <- 'PEN'
df$siglaPartido[df$siglaPartido == 'PODE'] <- 'PTN'

df$id = ifelse(df$nome %in% c(pop_prof, pop_desi), 'CFS',
               ifelse(df$nome %in% bolsonaro, 'Bolsonaro',
                             df$siglaPartido))

df$legis = ifelse(df$ano < 2012, '53',
                  ifelse(df$ano < 2014, '54', '55'))

xx = table(grepl('CRÍTICA',df$keywords),df$id)
xx = rbind(xx, xx[1,] + xx[2,])
xx[3,] = xx[2,] * 100 / xx[3,]
round(xx,2)

dt = df[nchar(df$transcricao) > 0,]

dt$text = dt$transcricao
dt$docid = paste(dt$id, dt$legis, sep = ' ')
dt$docid = dt$id

dt = dt[c('docid','text')]

## Word Fish
dfC <- corpus(dt)

dfM <- dfm(dfC,groups = 'docid')

wfish = textmodel_wordfish(dfM)

wf = textplot_scale1d(wfish, highlighted = c('CFS', 'Bolsonaro'))

wfd = wf$data
wfd$std.error = (wfd$upper - wfd$theta) / 1.96
wfd$statistic = wfd$theta / wfd$std.error
wfd$p.value = 1.96 * pnorm(-abs(wfd$statistic))

wfd = wfd[c('doclabels','theta','std.error','statistic','p.value')]

colnames(wfd)[1:2] = c('term','estimate')

wfd = wfd[order(wfd$estimate,decreasing = T),]

dwplot(wfd, 
       dot_args = list(size = 3, pch = 19, 
                       colour = c(rep('black',20),
                                  'red',rep('black',2),
                                  'red',rep('black',4)))) + 
  theme_classic(base_size = 18,
                base_family = 'serif') +
  theme(legend.position="none")

## Análise Fatorial de Correspondência
wca = textmodel_ca(dfM)

df_ca <- data.frame(dim1 = coef(wca, doc_dim = 1)$coef_document, 
                    dim2 = coef(wca, doc_dim = 2)$coef_document)

# Plot
plot(x = df_ca$dim1, 
     y = df_ca$dim2, 
     type = 'p',
     pch = 20,
     xlab = 'Dimensão 1', 
     ylab = 'Dimensão 2',
     cex = 1.4,
     cex.lab = 1.6)

grid()

text(df_ca$dim1, 
     df_ca$dim2, 
     labels = rownames(df_ca), 
     cex = 1.2, 
     col = c(rep('red',2),rep('black',26)))

