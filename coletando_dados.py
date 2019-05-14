# -*- coding: utf-8 -*-
"""
Created on Mon May 13 17:06:54 2019

@author: Marcio
"""

import re
import requests 
import pandas as pd

path = '/Users/Marcio/Documents/portfolio/discursos/data/'

var_dep = ['dados','id','uri','nome','siglaPartido',
           'siglaUf','idLegislatura','urlFoto','ano']

data_dep = pd.DataFrame(columns=var_dep, dtype='str')

var_dis = ['dados','dataHoraInicio','dataHoraFim','uriEvento',
           'faseEvento','titulo','dataHoraInicio','dataHoraFim',
           'tipoDiscurso','urlTexto','urlAudio','urlVideo',
           'keywords','sumario','transcricao','ano','id',
           'nome','siglaPartido','siglaUf','idLegislatura',
           'urlFoto','uri']

data_dis = pd.DataFrame(columns=var_dis, dtype='str')  

api = 'https://dadosabertos.camara.leg.br/api/v2'

anos = range(2008,2019)

for a in range(len(anos)):
    
    ini = 'dataInicio="' + str(anos[a]) + '-01-01"'
    fim = '&dataFim="' + str(anos[a]) + '-12-31"'
    busca_dep = api + '/deputados?' + ini + fim
    
    try:
        rd = requests.get(busca_dep, stream = True)
        txd = rd.text
        rd = re.findall('"([^"]*)"', txd)
            
    except:
        rd = ''
        print('error: ' + str(anos[a]))
    
    df_dep = pd.DataFrame(columns=var_dep, dtype='str',
                          index = range(rd.count('uri') + 1))
        
    if len(df_dep) > 0:
        
        df_dep['ano'] = str(anos[a])
                            
        j = 0
        for i in range(len(rd)):
            
            vd = rd[i]
            
            if vd in var_dep and i < len(rd):
            
                if rd[i + 1] not in var_dep:
                    df_dep.xs(j)[vd] = rd[i + 1]
                    
                    if vd == "urlFoto":
                        j = j + 1

        data_dep = pd.concat([data_dep, df_dep])
                
for a in range(len(anos)):
    
    ano = str(anos[a])
    dp = data_dep.loc[data_dep['ano'] == ano,]
    
    arqv = path + 'discursos_deputados_ate_' + ano + '.csv'
        
    for k in range(len(dp)):
        
        uri = dp['uri'].values[k]
        idd = dp['id'].values[k]
        nome = dp['nome'].values[k]
        siglaPartido = dp['siglaPartido'].values[k]
        siglaUf = dp['siglaUf'].values[k]
        idLegislatura = dp['idLegislatura'].values[k]
        urlFoto = dp['urlFoto'].values[k]
        
        busca_disc = str(uri) + '/discursos?' + ini + fim
        
        if uri != 'nan':
        
            try:
                rs = requests.get(busca_disc, stream = True)
                txs = rs.text
                rs = re.findall('"([^"]*)"', txs)
                
            except:
                rs = ''
                print('error: ' + str(nome) + str(anos[a]))
                
            if rs.count('transcricao') > 0:
                
                df_dis = pd.DataFrame(columns=var_dis, dtype='str',
                                      index = range(rs.count('transcricao') + 1))
                    
                df_dis['ano'] = ano
                df_dis['uri'] = uri
                df_dis['id'] = idd
                df_dis['nome'] = nome
                df_dis['siglaPartido'] = siglaPartido
                df_dis['siglaUf'] = siglaUf
                df_dis['idLegislatura'] = idLegislatura
                df_dis['urlFoto'] = urlFoto
                
                u = 0
                for l in range(len(rs)):
                    
                    vs = rs[l]
                    
                    if vs in var_dis and l < len(rs):
                    
                        if rs[l + 1] not in var_dis:
                            nc = var_dis.index(vs)
                            df_dis.iloc[u,nc] = rs[l + 1]
                            
                            if vs == "transcricao":
                                u = u + 1
    
                data_dis = pd.concat([data_dis, df_dis])
            
        print((k + 1) * 100 / len(dp))
        
    data_dis.to_csv(arqv, sep='\t', encoding='utf-8')