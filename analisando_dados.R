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

## Diretório
setwd('source')

## Dados de Discursos
df <- fread('dados_discursos_deputados_tratados.csv',
            sep = '\t',header = T, encoding = 'UTF-8')

df <- as.data.frame(df)

df <- df[c('id','text')]

## Nomes dos Políticos
# Família Bolsonaro
bolsonaro = c('JAIR BOLSONARO','FLAVIO BOLSONARO','EDUARDO BOLSONARO')

# CFS por Profissão (Excluindo membros da família Bolsonaro)
pop_prof = read.table('deputados_CFS_profissão_2008_2018.txt')
pop_prof = str_trim(pop_prof$x)

# CFS por Designação
pop_desi = read.table('deputados_CFS_nome_urna_2008_2018.txt')
pop_desi = str_trim(pop_desi$x)

# Outros Deputados (Excluindo CFS e membros da família Bolsonaro)
# CFS por Profissão (Excluindo membros da família Bolsonaro)
outros = read.table('~/portfolio/text_as_data/data/deputados_outros_2008_2018.txt',
                    sep = ';')
outros = str_trim(outros$V1)

## Definindo ID
df$siglaPartido[df$siglaPartido == 'PP**'] <- 'PP'
df$siglaPartido[df$siglaPartido == 'MDB'] <- 'PMDB'
df$siglaPartido[df$siglaPartido == 'SOLIDARIEDADE'] <- 'SD'
df$siglaPartido[df$siglaPartido == 'AVANTE'] <- 'PTdoB'
df$siglaPartido[df$siglaPartido == 'PATRI'] <- 'PEN'
df$siglaPartido[df$siglaPartido == 'PODE'] <- 'PTN'

df$id = ifelse(grepl('BOLSONARO',df$nome) == T, 'Bolsonaro',
               ifelse(df$nome %in% c(pop_prof, pop_desi), 'CFS',
                      df$siglaPartido))

df$legis = ifelse(df$ano < 2012, '53',
                  ifelse(df$ano < 2014, '54', '55'))

dt = df[nchar(df$transcricao) > 0,]

dt$text = dt$transcricao
dt$docid = paste(dt$id, dt$legis, sep = ' ')

dt = dt[c('docid','text')]

## Word Fish
# Corupus e Matrix de Texto
dfC <- corpus(dt)

dfM <- dfm(dfC,groups = 'docid')

# Processando Algoritmo
wfish = textmodel_wordfish(dfM)

# Coletando Dados
wf = textplot_scale1d(wfish)

wfd = wf$data
wfd$std.error = (wfd$upper - wfd$theta) / 1.96
wfd$statistic = wfd$theta / wfd$std.error
wfd$p.value = 1.96 * pnorm(-abs(wfd$statistic))

wfd = wfd[c('doclabels','theta','std.error','statistic','p.value')]

colnames(wfd)[1:2] = c('term','estimate')

wfd = wfd[order(wfd$estimate,decreasing = T),]

# Inserindo Labels
wfd$model = rep('Outros', nrow(wfd))
wfd$model[grepl('Bolsonaro',wfd$term) == T] <- 'Bolsonaro'
wfd$model[grepl('CFS',wfd$term) == T] <- 'CFS'

# Definindo Cores
color = rep('black', nrow(wfd))
color[grepl('Bolsonaro',wfd$term) == T] <- 'blue'
color[grepl('CFS',wfd$term) == T] <- 'green'

# Agregando por Legislatura
ordem = as.character(wfd$term)
ordem = substr(ordem, nchar(ordem) - 1, nchar(ordem))
ordem = as.character(wfd$term)[order(ordem)]

brackets = list(c("53º Legislatura", "PV 53", "PSOL 53"),
                c("54º Legislatura", "PV 54", "PSOL 54"),
                c("55º Legislatura", "PV 55", "PSOL 55"))

# Plot
{dwplot(wfd, 
        order_vars = ordem,
        dot_args = list(size = 3, pch = 19)) + 
    labs(x = 'theta estimado', y = '', color = 'Grupo') +
    theme_classic(base_size = 18,
                  base_family = 'serif')} %>%
  add_brackets(brackets)

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
     cex.lab = 1.6,
     family = 'serif')

grid()

colour = rep('black',nrow(wfd))
colour[grepl('Bolsonaro',rownames(df_ca)) == T] <- 'blue'
colour[grepl('CFS',rownames(df_ca)) == T] <- 'green'

text(df_ca$dim1, 
     df_ca$dim2, 
     labels = rownames(df_ca), 
     cex = 1.2, 
     col = colour)
