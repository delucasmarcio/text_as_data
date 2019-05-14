# -*- coding: utf-8 -*-
"""
Created on Mon May 13 23:06:03 2019

@author: Marcio
"""

import nltk
from nltk.stem import RSLPStemmer
from nltk.tokenize import sent_tokenize, word_tokenize
from nltk.corpus import stopwords
import pandas as pd
from unidecode import unidecode
import re
    
ps = RSLPStemmer()
    
## Carregando Dados
path = "\\Users\\Marcio\\Documents\\portfolio\\discursos\\data\\"

data = pd.read_csv(path + 'discursos_deputados_ate_2018.csv',
                  sep = '\t', header = 0,
                  encoding = 'utf-8')

## Tratando dados
text = data['transcricao']
text = list(text)

sw = nltk.corpus.stopwords.words('portuguese')

for j in range(len(data)):
    wd = text[j]
    
    if str(wd) != 'nan':
    
        wd = wd.lower()
        wd = unidecode(wd)
        wd = " ".join(re.findall("[a-zA-Z]+", wd))
        wd = word_tokenize(wd) 
        for k in range(len(wd)):
            wdd = wd[k]
            
            if wdd in sw:
                wdd = ''
            elif len(wdd) == 1:
                wdd = ''
            else:
                wdd = ps.stem(wdd)
                wd[k] = wdd
                
        wd = ' '.join(wd)
            
        text[j] = wd

data['transcricao'] = text

dt = data[['uriEvento','titulo','tipoDiscurso',
           'urlTexto','keywords','transcricao',
           'ano','nome','siglaPartido','siglaUf',
           'urlFoto','uri']]

dt.to_csv(path + 'dados_discursos_deputados_tratados.csv', 
          sep='\t', encoding='utf-8')