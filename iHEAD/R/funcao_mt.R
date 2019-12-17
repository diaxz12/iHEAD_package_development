####################################################################################################
####################################################################################################
###                                                                                              ###
###Projeto HEAD                                                                                  ###
###Package responsavel por calcular as variaveis utilizadas na analise das linhas                ###
###Empresa: INESCTEC                                                                             ###
### Autores: Alexandra Vaz, Armando Leitao, Leonel Carvalho,                                     ###
### Luis Dias, Luis Guimaraes, Miguel Ribeiro                                                    ###
###                                                                                              ###
####################################################################################################
####################################################################################################

#'Create_MT_Data
#'
#'@description Criacao dos segmentos MT e das respectivas variaveis utilizadas na analise dos mesmos
#'@usage Create_MT_Data(data_mt_sit, data_mt_incidentes, data_dc_trocos_mt, data_meteo_anual,ano_inicio_dados,ano_inicio_estimativa,ano_fim_estimativa)
#'@param data_mt_sit Base de dados extraida do SIT, que contem informacoes tecnicas das linhas MT (para ver qual a estrutura que deve ter o ficheiro, correr o exemplo)
#'@param data_mt_incidentes Base de dados extraida do CRI, que contem informacoes sobre os incidentes que ocorreram nas linhas MT (para ver qual a estrutura que deve ter o ficheiro, correr o exemplo)
#'@param data_dc_trocos_mt Base de dados que contem informacao sobre a localizacao dos trocos MT e respectivas variaveis associadas a essa mesma localizacao (para ver qual a estrutura que deve ter o ficheiro, correr o exemplo)
#'@param data_meteo_anual Base de dados que contem informacao sobre dados meteorologicos anuais por distrito
#'@param ano_inicio_dados Ano mais antigo relativamente ao qual temos informacao tecnica e dos incidentes que ocorreram nas linhas
#'@param ano_inicio_estimativa Ano mais antigo em relacao ao qual se pretende estimar as falhas
#'@param ano_fim_estimativa Ano mais recente em relacao ao qual se pretende estimar as falhas
#'@return Lista de 4 datasets com a informação dos segmentos e variaveis utilizadas na analise dos mesmos. O elemento da lista com a denominacao "Total" contem todos os segmentos MT, o elemento com a denomincao "Mistos" contem todos os segmentos mistos MT, o elemento com a denominacao "Aereos" contem todos os segmentos aereos MT e o elemento com a denominacao "Subterraneos" contem todos os segmentos subterraneos MT
#'@examples
#'#Ler os dados a partir de um dado ficheiro
#'data_mt_sit <- iHEAD::data_mt_sit
#'data_mt_incidentes <- iHEAD::data_mt_incidentes
#'data_dc_trocos_mt <- iHEAD::data_dc_trocos_mt
#'data_meteo_anual <- iHEAD::data_meteo_anual
#'ano_inicio_dados <- 2009
#'ano_inicio_estimativa <- 2012
#'ano_fim_estimativa <- 2018
#'
#'#Chamar a funcao de calculo
#'resultados <- Create_MT_Data(data_mt_sit, data_mt_incidentes, data_dc_trocos_mt, data_meteo_anual, ano_inicio_dados, ano_inicio_estimativa, ano_fim_estimativa)
#'
#'@export

Create_MT_Data <- function(data_mt_sit,data_mt_incidentes,data_dc_trocos_mt,data_meteo_anual,ano_inicio_dados,ano_inicio_estimativa, ano_fim_estimativa){

  #instala as librarias necessarias para correr a funcao
  packages <- c("stringr", "lubridate")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }else{

    library(stringr)
    library(lubridate)
  }

  #retirar linhas que não estejam em exploraçao, que sejam particulares e que nao tenham codigo circuito
  data_mt_sit <- data_mt_sit[which(data_mt_sit$Situacao=="Em exploração" & data_mt_sit$Situacao.Linha!="Rede Particular" & data_mt_sit$Codigo.Circuito!=""),]

  #minutos em numerico
  data_mt_incidentes$X.Qtd.Minutos.Duracao.Incidente <- as.numeric(levels(data_mt_incidentes$X.Qtd.Minutos.Duracao.Incidente)[data_mt_incidentes$X.Qtd.Minutos.Duracao.Incidente])

  #comprimento por km
  data_mt_sit$Comprimento.Troco..m. <- data_mt_sit$Comprimento.Troco..m./1000

  #tensao em numerico
  data_mt_sit$Tensao.De.Servico..2. <- as.numeric(data_mt_sit$Tensao.De.Servico..2.)

  #distrito da raiz de circuito
  data_mt_sit$Distrito <- substr(data_mt_sit$Codigo.Circuito, 1,2)

  #concelho da raiz de circuito
  data_mt_sit$Concelho <- substr(data_mt_sit$Codigo.Circuito, 1,4)

  #cria tipo material do troco
  data_mt_sit$Tipo_Material <- str_match(data_mt_sit$LineCode, "_(.*?)_")[,2]

  #cria tipo de seccao do troco
  data_mt_sit$Tipo_Seccao <- str_match(data_mt_sit$LineCode, paste(str_match(data_mt_sit$LineCode, "_(.*?)_")[,1],"(.*?)_", sep=""))[,2]

  data_mt_sit$Tipo_Seccao <- str_match(gsub("\\(be)",".be.", data_mt_sit$LineCode), paste(str_match(gsub("\\(be)",".be.", data_mt_sit$LineCode), "_(.*?)_")[,1],"(.*?)_", sep=""))[,2]


  #criacao de config, tem que segmentar os dados em aereos ou subterraneos e depois vais buscar os ultimos 2 caracteres do line code, tem q transformar o line code de factor para string
  # tenho ainda de adaptar a diferentes configs ex: SGG40, H12
  data_mt_sit$Tipo_Config <- ""
  data_mt_sit$Tipo_Config[which(data_mt_sit$Tipo.instalacao=="Aéreo" & data_mt_sit$Tipo_Material!="LXHIOV" & data_mt_sit$Tipo_Material!="LXHIOZ1(be)")] <- substr(data_mt_sit$LineCode[which(data_mt_sit$Tipo.instalacao=="Aéreo" & data_mt_sit$Tipo_Material!="LXHIOV" & data_mt_sit$Tipo_Material!="LXHIOZ1(be)")], nchar(as.character(levels(data_mt_sit$LineCode[which(data_mt_sit$Tipo.instalacao=="Aéreo" & data_mt_sit$Tipo_Material!="LXHIOV" & data_mt_sit$Tipo_Material!="LXHIOZ1(be)")]))[data_mt_sit$LineCode[which(data_mt_sit$Tipo.instalacao=="Aéreo" & data_mt_sit$Tipo_Material!="LXHIOV" & data_mt_sit$Tipo_Material!="LXHIOZ1(be)")]])-1, nchar(as.character(levels(data_mt_sit$LineCode[which(data_mt_sit$Tipo.instalacao=="Aéreo" & data_mt_sit$Tipo_Material!="LXHIOV" & data_mt_sit$Tipo_Material!="LXHIOZ1(be)")]))[data_mt_sit$LineCode[which(data_mt_sit$Tipo.instalacao=="Aéreo" & data_mt_sit$Tipo_Material!="LXHIOV" & data_mt_sit$Tipo_Material!="LXHIOZ1(be)")]]))
  data_mt_sit$Tipo_Config[which(data_mt_sit$Tipo_Config=="_S")] <- ""
  data_mt_sit$Tipo_Config[which(data_mt_sit$Tipo_Config=="40")] <- substr(data_mt_sit$LineCode[which(data_mt_sit$Tipo_Config=="40")], nchar(as.character(levels(data_mt_sit$LineCode[which(data_mt_sit$Tipo_Config=="40")]))[data_mt_sit$LineCode[which(data_mt_sit$Tipo_Config=="40")]])-4, nchar(as.character(levels(data_mt_sit$LineCode[which(data_mt_sit$Tipo_Config=="40")]))[data_mt_sit$LineCode[which(data_mt_sit$Tipo_Config=="40")]]))
  data_mt_sit$Tipo_Config[which(data_mt_sit$Tipo_Config=="12")] <- substr(data_mt_sit$LineCode[which(data_mt_sit$Tipo_Config=="12")], nchar(as.character(levels(data_mt_sit$LineCode[which(data_mt_sit$Tipo_Config=="12")]))[data_mt_sit$LineCode[which(data_mt_sit$Tipo_Config=="12")]])-2, nchar(as.character(levels(data_mt_sit$LineCode[which(data_mt_sit$Tipo_Config=="12")]))[data_mt_sit$LineCode[which(data_mt_sit$Tipo_Config=="12")]]))

  #organizacao do dataset final
  names <- c("Segmento", "Codigo.Circuito", "Data_Presente","Anos_Historico","Comprimento_Total", "Comprimento_total_y_mais_1", "Comprimento_total_y_mais_2", "Comprimento_total_y_mais_3", "Tensao", "Distrito_RC", "Concelho_RC",
             "Setubal_por_km", "Lisboa_por_km", "Leiria_por_km", "Guarda_por_km", "Viseu_por_km","Santarem_por_km", "Coimbra_por_km", "Aveiro_por_km", "Porto_por_km", "Evora_por_km", "Braga_por_km", "Vila_Real_por_km", "Faro_por_km", "Portalegre_por_km", "Castelo_Branco_por_km", "Viana_do_Castelo_por_km", "Beja_por_km", "Braganca_por_km",
             "Acima_600m_por_km", "Litoral", "Aceleracao_Sismica_Solo_TipoB", "Valorizacao_sismica", "Valorizacao_ceraunico", "P.TIEMT..0.2_Simulado", "Valorizacao_Atmosfericos_extremos",
             "Resinosas", "Povoamentos_mistos", "Especies_arbustivas", "Folhosas", "Zonas_agro_florestais", "Outras_zonas", "Zonas_ardidas", "Sem_especie_florestal", "Densidade_Florestal_alta", "Densidade_Florestal_media", "Densidade_Florestal_baixa", "Densidade_Florestal_desconhecida", "Sem_Densidade_Florestal",
             "AA_por_km", "AM_por_km", "CU_por_km", "LXHIOV_por_km", "LXHIOZ1.be_por_km", "PHCA_por_km","LEHIV_por_km","PCIA_por_km","Outros_Materiais_por_km", "Idade_Media", "Idade_Max",
             "Idade_Min", "Idade_N_Trocos_Sem_por_km", "Seccao_menor_50_por_Km", "Seccao_maior_igual_50_menor_90_por_Km", "Seccao_maior_igual_90_menor_120_por_Km", "Seccao_maior_igual_120_menor_180_por_Km", "Seccao_maior_igual_180_por_Km", "Seccao_Outros_por_Km" ,
             "Config_SG", "Config_SGG40", "Config_RT", "Config_H12", "Config_V2","Outras_Config", "Principal_por_km", "Line.code.Rating..c...a.", "Line.code.Rating..h...a.","Em_Exploracao",
             "media_horas_seguidas_temp_maior_3Q", "media_horas_seguidas_temp_menor_1Q", "media_horas_seguidas_mod_maior_3Q", "cfh", "cfl", "cfm", "cft", "Count_hours_sem_cloud", "Mean_cloud", "Max_cloud",
             "Count_hours_mod_maior_3Q", "Mean_mod_wind", "Max_mod_wind", "Mean_Temp", "Max_Temp", "Min_Temp", "Count_hours_Temp_maior_3Q", "Count_hours_Temp_menor_1Q", "Mean_Swflx", "Max_Swflx",
             "Mean_u", "Max_u_pos", "Max_u_neg", "Mean_v", "Max_v_pos", "Max_v_neg", "Aerea_por_km", "Subterranea_por_km", "Duracao_Manutencoes_e_Previstas", "Contagem_1ano_Manutencoes_e_Previstas", "Contagem_Manutencoes_e_Previstas",
             "Incipiente_Duracao_Forca_Maior", "Incipiente_Duracao_Material_Equipamento", "Incipiente_Duracao_Outros", "Incipiente_Duracao_Atmosferica",
             "Incipiente_Duracao_Origem_Interna", "Incipiente_Duracao_Humanos", "Incipiente_Duracao_Tecnicas",
             "Incipiente_Duracao_Naturais", "Incipiente_Duracao_Entidade_Exterior", "Incipiente_Duracao_Razoes_Seguranca", "Incipiente_Duracao_Redes_Instalacoes",
             "Incipiente_Contagem_1ano_Forca_Maior", "Incipiente_Contagem_1ano_Material_Equipamento", "Incipiente_Contagem_1ano_Outros", "Incipiente_Contagem_1ano_Atmosferica",
             "Incipiente_Contagem_1ano_Origem_Interna", "Incipiente_Contagem_1ano_Humanos", "Incipiente_Contagem_1ano_Tecnicas",
             "Incipiente_Contagem_1ano_Naturais", "Incipiente_Contagem_1ano_Entidade_Exterior", "Incipiente_Contagem_1ano_Razoes_Seguranca","Incipiente_Contagem_1ano_Redes_Instalacoes",
             "Incipiente_Contagem_Forca_Maior", "Incipiente_Contagem_Material_Equipamento", "Incipiente_Contagem_Outros", "Incipiente_Contagem_Atmosferica",
             "Incipiente_Contagem_Origem_Interna", "Incipiente_Contagem_Humanos", "Incipiente_Contagem_Tecnicas",
             "Incipiente_Contagem_Naturais", "Incipiente_Contagem_Entidade_Exterior", "Incipiente_Contagem_Razoes_Seguranca", "Incipiente_Contagem_Redes_Instalacoes",
             "Catastrofica_Duracao_Forca_Maior", "Catastrofica_Duracao_Material_Equipamento", "Catastrofica_Duracao_Outros", "Catastrofica_Duracao_Atmosferica",
             "Catastrofica_Duracao_Origem_Interna", "Catastrofica_Duracao_Humanos", "Catastrofica_Duracao_Tecnicas",
             "Catastrofica_Duracao_Naturais", "Catastrofica_Duracao_Entidade_Exterior", "Catastrofica_Duracao_Razoes_Seguranca", "Catastrofica_Duracao_Redes_Instalacoes",
             "Catastrofica_Contagem_Forca_Maior", "Catastrofica_Contagem_Material_Equipamento", "Catastrofica_Contagem_Outros", "Catastrofica_Contagem_Atmosferica",
             "Catastrofica_Contagem_Origem_Interna", "Catastrofica_Contagem_Humanos", "Catastrofica_Contagem_Tecnicas",
             "Catastrofica_Contagem_Naturais", "Catastrofica_Contagem_Entidade_Exterior", "Catastrofica_Contagem_Razoes_Seguranca", "Catastrofica_Contagem_Redes_Instalacoes",
             "Catastrofica_Contagem_1ano_Forca_Maior", "Catastrofica_Contagem_1ano_Material_Equipamento", "Catastrofica_Contagem_1ano_Outros", "Catastrofica_Contagem_1ano_Atmosferica",
             "Catastrofica_Contagem_1ano_Origem_Interna", "Catastrofica_Contagem_1ano_Humanos", "Catastrofica_Contagem_1ano_Tecnicas",
             "Catastrofica_Contagem_1ano_Naturais", "Catastrofica_Contagem_1ano_Entidade_Exterior", "Catastrofica_Contagem_1ano_Razoes_Seguranca","Catastrofica_Contagem_1ano_Redes_Instalacoes",
             "Incipientes_Variacao_ultimos2anos", "Falhas_Totais_Variacao_ultimos2anos", "Falhas_Totais_por_km_y_menos_1", "Falhas_Totais_por_km_y_menos_2_menos_1", "Falhas_Totais_por_km_y_menos_3_menos_2_menos_1", "Falhas_Totais_por_km_y_mais_1", "Falhas_Totais_por_km_y_mais_2", "Falhas_Totais_por_km_y_mais_3")



  data_seg_mt <- data.frame(matrix(ncol=length(names), nrow=0))
  colnames(data_seg_mt) <- names



  ## definir datas da analise
  date_i <- paste(ano_inicio_dados,"/01/01",sep="")
  date_f <- paste(ano_fim_estimativa,"/01/01",sep="")
  date_ini <- paste(ano_inicio_estimativa,"/01/01",sep="")
  date_pre <- date_ini
  anos <- year(as.Date(date_f))-year(as.Date(date_ini))+1
  date_i_meteo <- min(data_meteo_anual$Ano)

  #criacao de variaveis referentes a um dado segmento num determinado ano (dataset final)
  for(n in 1:anos){

    #criacao das datas utilizadas na analise
    date_pre_y_mais_1 <- date_pre
    substr(date_pre_y_mais_1, 1, 4) <- as.character(as.numeric(substr(date_pre_y_mais_1, 1, 4))+1)
    date_pre_y_mais_2 <- date_pre
    substr(date_pre_y_mais_2, 1, 4) <- as.character(as.numeric(substr(date_pre_y_mais_2, 1, 4))+2)
    date_pre_y_mais_3 <- date_pre
    substr(date_pre_y_mais_3, 1, 4) <- as.character(as.numeric(substr(date_pre_y_mais_3, 1, 4))+3)

    date_pre_y_menos_1 <- date_pre
    substr(date_pre_y_menos_1, 1, 4) <- as.character(as.numeric(substr(date_pre_y_menos_1, 1, 4))-1)
    date_pre_y_menos_2 <- date_pre
    substr(date_pre_y_menos_2, 1, 4) <- as.character(as.numeric(substr(date_pre_y_menos_2, 1, 4))-2)
    date_pre_y_menos_3 <- date_pre
    substr(date_pre_y_menos_3, 1, 4) <- as.character(as.numeric(substr(date_pre_y_menos_3, 1, 4))-3)

    anos_historico <- year(as.Date(date_pre))-year(as.Date(date_i))

    #filtra os dados do sit para conter so os segmentos que pertencem a um determinado periodo tempora ou que nao tem data de criacao definida
    data_mt_sit_aux <- data_mt_sit[which(as.Date(levels(data_mt_sit$Data.De.Entrada.Em.Servico)[data_mt_sit$Data.De.Entrada.Em.Servico], format = "%d/%m/%Y")<as.Date(date_pre) | data_mt_sit$Data.De.Entrada.Em.Servico=="" ),]
    data_mt_sit_aux_y_mais_1 <- data_mt_sit[which(as.Date(levels(data_mt_sit$Data.De.Entrada.Em.Servico)[data_mt_sit$Data.De.Entrada.Em.Servico], format = "%d/%m/%Y")<as.Date(date_pre_y_mais_1) | data_mt_sit$Data.De.Entrada.Em.Servico=="" ),]
    data_mt_sit_aux_y_mais_2 <- data_mt_sit[which(as.Date(levels(data_mt_sit$Data.De.Entrada.Em.Servico)[data_mt_sit$Data.De.Entrada.Em.Servico], format = "%d/%m/%Y")<as.Date(date_pre_y_mais_2) | data_mt_sit$Data.De.Entrada.Em.Servico=="" ),]
    data_mt_sit_aux_y_mais_3 <- data_mt_sit[which(as.Date(levels(data_mt_sit$Data.De.Entrada.Em.Servico)[data_mt_sit$Data.De.Entrada.Em.Servico], format = "%d/%m/%Y")<as.Date(date_pre_y_mais_3) | data_mt_sit$Data.De.Entrada.Em.Servico=="" ),]

    #reduz o tamanho dos datasets filtrados para conter so a informacao necessaria (codigo do circuito e o seu comprimento)
    data_mt_sit_aux_y_mais_1 <- subset(data_mt_sit_aux_y_mais_1,select=c(Codigo.Circuito, Comprimento.Troco..m.))
    data_mt_sit_aux_y_mais_2 <- subset(data_mt_sit_aux_y_mais_2,select=c(Codigo.Circuito, Comprimento.Troco..m.))
    data_mt_sit_aux_y_mais_3 <- subset(data_mt_sit_aux_y_mais_3,select=c(Codigo.Circuito, Comprimento.Troco..m.))

    #calculo da idade
    data_mt_sit_aux$Idade <- as.numeric(format(as.Date(date_pre), "%Y")) - 1 - (as.numeric(format(as.Date(levels(data_mt_sit_aux$Data.De.Entrada.Em.Servico)[data_mt_sit_aux$Data.De.Entrada.Em.Servico], format = "%d/%m/%Y"), "%Y")))

    #criacao de minidataset final para conter os segmentos no periodo temporal da iteracao
    data_seg_mt_aux <- data.frame(matrix(ncol=length(names), nrow=length(unique(paste(data_mt_sit_aux$Codigo.Circuito, data_mt_sit_aux$Concelho, sep="")))))
    colnames(data_seg_mt_aux) <- names

    data_seg_mt_aux$Segmento <- unique(paste(data_mt_sit_aux$Codigo.Circuito, data_mt_sit_aux$Concelho, sep=""))

    data_seg_mt_aux$Codigo.Circuito <- substr(data_seg_mt_aux$Segmento, 1, nchar(data_seg_mt_aux$Segmento)-4)

    data_seg_mt_aux <- data_seg_mt_aux[which(data_seg_mt_aux$Segmento!=""),]

    #calcula as variaveis para cada segmento
    for (i in 1:nrow(data_seg_mt_aux)){


      aux <- data_mt_sit_aux[which(data_mt_sit_aux$Codigo.Circuito==data_seg_mt_aux$Codigo.Circuito[i]),]
      aux_y_mais_1 <- data_mt_sit_aux_y_mais_1[which(data_mt_sit_aux_y_mais_1$Codigo.Circuito==data_seg_mt_aux$Codigo.Circuito[i]),]
      aux_y_mais_2 <- data_mt_sit_aux_y_mais_2[which(data_mt_sit_aux_y_mais_2$Codigo.Circuito==data_seg_mt_aux$Codigo.Circuito[i]),]
      aux_y_mais_3 <- data_mt_sit_aux_y_mais_3[which(data_mt_sit_aux_y_mais_3$Codigo.Circuito==data_seg_mt_aux$Codigo.Circuito[i]),]

      #filtra as incidencias para conterem so as incidencias que aconteceram ao segmento
      aux_f <- data_mt_incidentes[which(as.character(levels(data_mt_incidentes$Correspondencia.codigo.SIT))[data_mt_incidentes$Correspondencia.codigo.SIT]==data_seg_mt_aux$Codigo.Circuito[i]),]

      #filtra os dados relativos a localizacao dos trocos para conterem os relativos ao segmento num determinado periodo
      aux_c <- data_dc_trocos_mt[which(as.character(levels(data_dc_trocos_mt$Codigo.Circuito))[data_dc_trocos_mt$Codigo.Circuito]==data_seg_mt_aux$Codigo.Circuito[i] & (as.Date(levels(data_dc_trocos_mt$Data.De.Entrada.Em.Servico)[data_dc_trocos_mt$Data.De.Entrada.Em.Servico], format="%d/%m/%Y")<as.Date(date_pre) | data_dc_trocos_mt$Data.De.Entrada.Em.Servico=="")),]

      as.numeric(levels(data_mt_incidentes$X.Qtd.Minutos.Duracao.Incidente)[data_mt_incidentes$X.Qtd.Minutos.Duracao.Incidente])

      comprimento_total <- sum(aux$Comprimento.Troco..m.)
      comprimento_total_y_menos_1 <- sum(aux$Comprimento.Troco..m.[which(as.Date(levels(aux$Data.De.Entrada.Em.Servico)[aux$Data.De.Entrada.Em.Servico], format = "%d/%m/%Y")<=(as.Date(date_pre_y_menos_1)) | aux$Data.De.Entrada.Em.Servico=="" )])
      comprimento_total_y_menos_2 <- sum(aux$Comprimento.Troco..m.[which(as.Date(levels(aux$Data.De.Entrada.Em.Servico)[aux$Data.De.Entrada.Em.Servico], format = "%d/%m/%Y")<=(as.Date(date_pre_y_menos_2)) | aux$Data.De.Entrada.Em.Servico=="" )])
      comprimento_total_y_menos_3 <- sum(aux$Comprimento.Troco..m.[which(as.Date(levels(aux$Data.De.Entrada.Em.Servico)[aux$Data.De.Entrada.Em.Servico], format = "%d/%m/%Y")<=(as.Date(date_pre_y_menos_3)) | aux$Data.De.Entrada.Em.Servico=="" )])
      comprimento_total_y_mais_1 <- sum(aux_y_mais_1$Comprimento.Troco..m.)
      comprimento_total_y_mais_2 <- sum(aux_y_mais_2$Comprimento.Troco..m.)
      comprimento_total_y_mais_3 <- sum(aux_y_mais_2$Comprimento.Troco..m.)

      data_seg_mt_aux$Data_Presente[i] <- date_pre

      data_seg_mt_aux$Anos_Historico[i] <- anos_historico

      #comprimento
      data_seg_mt_aux$Comprimento_Total[i] <- comprimento_total

      #Distrito

      #distrito da raiz do circuito
      data_seg_mt_aux$Distrito_RC[i] <- aux$Distrito[1]

      #distribuicao dos distritos da linha
      data_seg_mt_aux$Setubal_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==15)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Lisboa_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==11)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Leiria_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==10)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Guarda_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==9)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Viseu_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==18)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Santarem_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==14)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Coimbra_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==6)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Aveiro_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==1)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Porto_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==13)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Evora_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==7)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Braga_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==3)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Vila_Real_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==17)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Faro_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==8)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Portalegre_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==12)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Castelo_Branco_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==5)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Viana_do_Castelo_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==16)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Beja_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==2)])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Braganca_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Distrito==4)])/sum(aux_c$Comprimento_do_sistema_.km.)


      #Concelho

      #concelho da raiz do circuito
      data_seg_mt_aux$Concelho_RC[i] <- aux$Concelho[1]

      #distribuicao dos dados relativos a localizacao no segmento
      data_seg_mt_aux$Acima_600m_por_km[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Acima_600m=="S")])/sum(aux_c$Comprimento_do_sistema_.km.)

      data_seg_mt_aux$Litoral[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(aux_c$Litoral=="S")])/sum(aux_c$Comprimento_do_sistema_.km.)

      data_seg_mt_aux$Aceleracao_Sismica_Solo_TipoB[i] <- sum(aux_c$Aceleracao_Sismica_Solo_TipoB * aux_c$Comprimento_do_sistema_.km.)/sum(aux_c$Comprimento_do_sistema_.km.)

      data_seg_mt_aux$Valorizacao_sismica[i] <- sum(aux_c$Valorizacao_sismica * aux_c$Comprimento_do_sistema_.km.)/sum(aux_c$Comprimento_do_sistema_.km.)

      data_seg_mt_aux$Valorizacao_ceraunico[i] <- sum(aux_c$Valorizacao_ceraunico * aux_c$Comprimento_do_sistema_.km.)/sum(aux_c$Comprimento_do_sistema_.km.)

      data_seg_mt_aux$P.TIEMT..0.2_Simulado[i] <- sum(aux_c$P.TIEMT..0.2_Simulado * aux_c$Comprimento_do_sistema_.km.)/sum(aux_c$Comprimento_do_sistema_.km.)

      data_seg_mt_aux$Valorizacao_Atmosfericos_extremos[i] <- sum(aux_c$Valorizacao_Atmosfericos_extremos * aux_c$Comprimento_do_sistema_.km.)/sum(aux_c$Comprimento_do_sistema_.km.)

      data_seg_mt_aux$Resinosas[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Especie_Florestal)[aux_c$Especie_Florestal])=="Resinosas")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Povoamentos_mistos[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Especie_Florestal)[aux_c$Especie_Florestal])=="Povoamentos mistos")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Especies_arbustivas[i] <-sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Especie_Florestal)[aux_c$Especie_Florestal])=="Espécies arbustivas")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Folhosas[i] <-sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Especie_Florestal)[aux_c$Especie_Florestal])=="Folhosas")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Zonas_agro_florestais[i] <- sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Especie_Florestal)[aux_c$Especie_Florestal])=="Zonas agro-florestais")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Outras_zonas[i] <-sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Especie_Florestal)[aux_c$Especie_Florestal])=="Outras zonas")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Zonas_ardidas[i] <-sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Especie_Florestal)[aux_c$Especie_Florestal])=="Zonas ardidas")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Sem_especie_florestal[i] <- 1 - (data_seg_mt_aux$Resinosas[i]+data_seg_mt_aux$Povoamentos_mistos[i]+data_seg_mt_aux$Especies_arbustivas[i]+data_seg_mt_aux$Folhosas[i]+data_seg_mt_aux$Zonas_agro_florestais[i]+data_seg_mt_aux$Outras_zonas[i]+data_seg_mt_aux$Zonas_ardidas[i])

      data_seg_mt_aux$Densidade_Florestal_alta[i] <-sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Densidade_Florestal)[aux_c$Densidade_Florestal])=="Alta")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Densidade_Florestal_media[i] <-sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Densidade_Florestal)[aux_c$Densidade_Florestal])=="Média")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Densidade_Florestal_baixa[i] <-sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Densidade_Florestal)[aux_c$Densidade_Florestal])=="Baixa")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Densidade_Florestal_desconhecida[i] <-sum(aux_c$Comprimento_do_sistema_.km.[which(as.character(levels(aux_c$Densidade_Florestal)[aux_c$Densidade_Florestal])=="Desconhecido")])/sum(aux_c$Comprimento_do_sistema_.km.)
      data_seg_mt_aux$Sem_Densidade_Florestal[i] <- 1 - (data_seg_mt_aux$Densidade_Florestal_alta[i]+data_seg_mt_aux$Densidade_Florestal_media[i]+data_seg_mt_aux$Densidade_Florestal_baixa[i]+data_seg_mt_aux$Densidade_Florestal_desconhecida[i])

      #tensao
      data_seg_mt_aux$Tensao[i] <- round(sum(aux$Tensao.De.Servico..2.*aux$Comprimento.Troco..m./sum(aux$Comprimento.Troco..m.*!is.na(aux$Tensao.De.Servico..2.)), na.rm=T), digits=0)

      #distribuicao dos materiais do segmento
      data_seg_mt_aux$AA_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.de.Material.da.Linha=="AA")])/comprimento_total
      data_seg_mt_aux$AM_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.de.Material.da.Linha=="AM")])/comprimento_total
      data_seg_mt_aux$CU_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.de.Material.da.Linha=="CU")])/comprimento_total
      data_seg_mt_aux$LXHIOV_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.de.Material.da.Linha=="LXHIOV")])/comprimento_total
      data_seg_mt_aux$LXHIOZ1.be_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.de.Material.da.Linha=="LXHIOZ1(be)")])/comprimento_total
      data_seg_mt_aux$PHCA_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.de.Material.da.Linha=="PHCA#")])/comprimento_total
      data_seg_mt_aux$LEHIV_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.de.Material.da.Linha=="LEHIV")])/comprimento_total
      data_seg_mt_aux$PCIA_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.de.Material.da.Linha=="PCIA#")])/comprimento_total
      data_seg_mt_aux$Outros_Materiais_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.de.Material.da.Linha!="AA" & aux$Tipo.de.Material.da.Linha!="CU" & aux$Tipo.de.Material.da.Linha!="AM" & aux$Tipo.de.Material.da.Linha!="LXHIOV" & aux$Tipo.de.Material.da.Linha!="LXHIOZ1(be)" & aux$Tipo.de.Material.da.Linha!="PHCA#" & aux$Tipo.de.Material.da.Linha!="LEHIV" & aux$Tipo.de.Material.da.Linha!="PCIA#")])/comprimento_total

      #calculo das variaveis relativas a idade
      data_seg_mt_aux$Idade_Media[i] <- sum(aux$Idade*aux$Comprimento.Troco..m./sum(aux$Comprimento.Troco..m.*!is.na(aux$Idade)), na.rm=T)
      data_seg_mt_aux$Idade_Max[i] <- max(aux$Idade, na.rm=T)
      data_seg_mt_aux$Idade_Min[i] <- min(aux$Idade, na.rm=T)
      data_seg_mt_aux$Idade_N_Trocos_Sem_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(is.na(aux$Idade))])/comprimento_total

      #distribuicao de tipos de seccao
      data_seg_mt_aux$Seccao_menor_50_por_Km[i] <- sum(aux$Comprimento.Troco..m.[which(as.numeric(aux$Tipo_Seccao)<50)])/comprimento_total
      data_seg_mt_aux$Seccao_maior_igual_50_menor_90_por_Km[i] <- sum(aux$Comprimento.Troco..m.[which(as.numeric(aux$Tipo_Seccao)>=50 & as.numeric(aux$Tipo_Seccao)<90)])/comprimento_total
      data_seg_mt_aux$Seccao_maior_igual_90_menor_120_por_Km[i] <- sum(aux$Comprimento.Troco..m.[which(as.numeric(aux$Tipo_Seccao)>=90 & as.numeric(aux$Tipo_Seccao)<120)])/comprimento_total
      data_seg_mt_aux$Seccao_maior_igual_120_menor_180_por_Km[i] <- sum(aux$Comprimento.Troco..m.[which(as.numeric(aux$Tipo_Seccao)>=120 & as.numeric(aux$Tipo_Seccao)<180)])/comprimento_total
      data_seg_mt_aux$Seccao_maior_igual_180_por_Km[i] <- sum(aux$Comprimento.Troco..m.[which(as.numeric(aux$Tipo_Seccao)>=180)])/comprimento_total
      data_seg_mt_aux$Seccao_Outros_por_Km[i] <- sum(aux$Comprimento.Troco..m.[which(as.character(aux$Tipo_Seccao)=="D240" | as.character(aux$Tipo_Seccao)=="D185" | as.character(aux$Tipo_Seccao)=="D185" | as.character(aux$Tipo_Seccao)=="CAV" | as.character(aux$Tipo_Seccao)=="D120" | as.character(aux$Tipo_Seccao)=="D235" | is.na(aux$Tipo_Seccao))])/comprimento_total

      #distribuicao de configuracoes
      data_seg_mt_aux$Config_SG[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo_Config=="SG")])/comprimento_total
      data_seg_mt_aux$Config_SGG40[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo_Config=="SGG40")])/comprimento_total
      data_seg_mt_aux$Config_RT[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo_Config=="RT")])/comprimento_total
      data_seg_mt_aux$Config_H12[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo_Config=="H12")])/comprimento_total
      data_seg_mt_aux$Config_V2[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo_Config=="V2")])/comprimento_total
      data_seg_mt_aux$Outras_Config[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo_Config!="SG" & aux$Tipo_Config!="SGG40" & aux$Tipo_Config!="RT"  & aux$Tipo_Config!="H12" & aux$Tipo_Config!="V2")])/comprimento_total

      #distribuicao Line.code.Rating
      data_seg_mt_aux$Line.code.Rating..c...a.[i] <- sum(aux$Line.code.Rating..c...a.*aux$Comprimento.Troco..m./sum(aux$Comprimento.Troco..m.*!is.na(aux$Line.code.Rating..c...a.)), na.rm=T)
      data_seg_mt_aux$Line.code.Rating..h...a.[i] <- sum(aux$Line.code.Rating..h...a.*aux$Comprimento.Troco..m./sum(aux$Comprimento.Troco..m.*!is.na(aux$Line.code.Rating..h...a.)), na.rm=T)

      #distribuicao de classificacao do tipo: principal
      data_seg_mt_aux$Principal_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Principal=="Sim")])/comprimento_total

      #distribuicao de classificacao do tipo: Em exploracao. atraves da variave situacao e nao da situacao.linha
      data_seg_mt_aux$Em_Exploracao[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Situacao=="Em exploração")])/comprimento_total

      #metereologicas

      #filtra os dados meteorologicos para conterem so os referentes ao periodo em analise
      aux_meteo_2 <- data_meteo_anual[which(data_meteo_anual$Ano < year(as.Date(date_pre))),]

      #determina que informacao tem que ser tida em conta mediante a localizacao do segmento
      aux_meteo_2$Comprimento <- ""
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==1)] <- data_seg_mt_aux$Aveiro_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==2)] <- data_seg_mt_aux$Beja_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==3)] <- data_seg_mt_aux$Braga_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==4)] <- data_seg_mt_aux$Braganca_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==5)] <- data_seg_mt_aux$Castelo_Branco_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==6)] <- data_seg_mt_aux$Coimbra_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==7)] <- data_seg_mt_aux$Evora_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==8)] <- data_seg_mt_aux$Faro_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==9)] <- data_seg_mt_aux$Guarda_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==10)] <- data_seg_mt_aux$Leiria_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==11)] <- data_seg_mt_aux$Lisboa_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==12)] <- data_seg_mt_aux$Portalegre_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==13)] <- data_seg_mt_aux$Porto_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==14)] <- data_seg_mt_aux$Santarem_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==15)] <- data_seg_mt_aux$Setubal_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==16)] <- data_seg_mt_aux$Viana_do_Castelo_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==17)] <- data_seg_mt_aux$Vila_Real_por_km[i]
      aux_meteo_2$Comprimento[which(aux_meteo_2$Distrito==18)] <- data_seg_mt_aux$Viseu_por_km[i]

      anos_historico_meteo <- year(as.Date(date_pre))-date_i_meteo

      data_seg_mt_aux$media_horas_seguidas_temp_maior_3Q[i] <- sum(aux_meteo_2$horas_seguidas_temp_maior_3Q*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo

      data_seg_mt_aux$media_horas_seguidas_temp_menor_1Q[i] <- sum(aux_meteo_2$horas_seguidas_temp_menor_1Q*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo

      data_seg_mt_aux$media_horas_seguidas_mod_maior_3Q[i] <- sum(aux_meteo_2$horas_seguidas_mod_maior_3Q*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo


      data_seg_mt_aux$cfh[i] <- sum(aux_meteo_2$cfh*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$cfl[i] <- sum(aux_meteo_2$cfl*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$cfm[i] <- sum(aux_meteo_2$cfm*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$cft[i] <- sum(aux_meteo_2$cft*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo

      data_seg_mt_aux$Mean_cloud[i] <- sum(aux_meteo_2$Mean_cloud*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Max_cloud[i] <- sum(aux_meteo_2$Max_cloud*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Count_hours_sem_cloud[i] <- sum(aux_meteo_2$Count_hours_sem_cloud*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo

      data_seg_mt_aux$Mean_mod_wind[i] <- sum(aux_meteo_2$Mean_mod_wind*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Max_mod_wind[i] <-sum(aux_meteo_2$Max_mod_wind*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Count_hours_mod_maior_3Q[i] <- sum(aux_meteo_2$Count_hours_mod_maior_3Q*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo


      data_seg_mt_aux$Mean_Temp[i] <- sum(aux_meteo_2$Mean_Temp*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Max_Temp[i] <- sum(aux_meteo_2$Max_Temp*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Min_Temp[i] <- sum(aux_meteo_2$Min_Temp*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Count_hours_Temp_maior_3Q[i] <- sum(aux_meteo_2$Count_hours_Temp_maior_3Q*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Count_hours_Temp_menor_1Q[i] <- sum(aux_meteo_2$Count_hours_Temp_menor_1Q*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo


      data_seg_mt_aux$Mean_Swflx[i] <- sum(aux_meteo_2$Mean_Swflx*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Max_Swflx[i] <- sum(aux_meteo_2$Max_Swflx*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo

      data_seg_mt_aux$Mean_u[i] <- sum(aux_meteo_2$Mean_u*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Max_u_pos[i] <-  sum(aux_meteo_2$Max_u_pos*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Max_u_neg[i] <- sum(aux_meteo_2$Max_u_neg*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo

      data_seg_mt_aux$Mean_v[i] <- sum(aux_meteo_2$Mean_v*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Max_v_pos[i] <- sum(aux_meteo_2$Max_u_pos*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo
      data_seg_mt_aux$Max_v_neg[i] <- sum(aux_meteo_2$Max_u_neg*as.numeric(aux_meteo_2$Comprimento))/anos_historico_meteo

      #criacao das variaveis relativas a incidentes
      if(nrow(aux_f)==0){

        data_seg_mt_aux$Duracao_Manutencoes_e_Previstas[i] <- 0

        data_seg_mt_aux$Contagem_Manutencoes_e_Previstas[i] <- 0

        data_seg_mt_aux$Contagem_1ano_Manutencoes_e_Previstas[i] <- 0

        data_seg_mt_aux$Incipiente_Duracao_Forca_Maior[i] <- 0
        data_seg_mt_aux$Incipiente_Duracao_Material_Equipamento[i] <- 0
        data_seg_mt_aux$Incipiente_Duracao_Outros[i] <- 0
        data_seg_mt_aux$Incipiente_Duracao_Atmosferica[i] <- 0
        data_seg_mt_aux$Incipiente_Duracao_Origem_Interna[i] <- 0
        data_seg_mt_aux$Incipiente_Duracao_Humanos[i] <- 0
        data_seg_mt_aux$Incipiente_Duracao_Tecnicas[i] <- 0
        data_seg_mt_aux$Incipiente_Duracao_Naturais[i] <- 0
        data_seg_mt_aux$Incipiente_Duracao_Entidade_Exterior[i] <- 0
        data_seg_mt_aux$Incipiente_Duracao_Razoes_Seguranca[i] <- 0
        data_seg_mt_aux$Incipiente_Duracao_Redes_Instalacoes[i] <- 0

        data_seg_mt_aux$Incipiente_Contagem_Forca_Maior[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_Material_Equipamento[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_Outros[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_Atmosferica[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_Origem_Interna[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_Humanos[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_Tecnicas[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_Naturais[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_Entidade_Exterior[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_Razoes_Seguranca[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_Redes_Instalacoes[i] <- 0

        data_seg_mt_aux$Incipiente_Contagem_1ano_Forca_Maior[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_1ano_Material_Equipamento[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_1ano_Outros[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_1ano_Atmosferica[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_1ano_Origem_Interna[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_1ano_Humanos[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_1ano_Tecnicas[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_1ano_Naturais[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_1ano_Entidade_Exterior[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_1ano_Razoes_Seguranca[i] <- 0
        data_seg_mt_aux$Incipiente_Contagem_1ano_Redes_Instalacoes[i] <- 0

        data_seg_mt_aux$Catastrofica_Duracao_Forca_Maior[i] <- 0
        data_seg_mt_aux$Catastrofica_Duracao_Material_Equipamento[i] <- 0
        data_seg_mt_aux$Catastrofica_Duracao_Outros[i] <- 0
        data_seg_mt_aux$Catastrofica_Duracao_Atmosferica[i] <- 0
        data_seg_mt_aux$Catastrofica_Duracao_Origem_Interna[i] <- 0
        data_seg_mt_aux$Catastrofica_Duracao_Humanos[i] <- 0
        data_seg_mt_aux$Catastrofica_Duracao_Tecnicas[i] <- 0
        data_seg_mt_aux$Catastrofica_Duracao_Naturais[i] <- 0
        data_seg_mt_aux$Catastrofica_Duracao_Entidade_Exterior[i] <- 0
        data_seg_mt_aux$Catastrofica_Duracao_Razoes_Seguranca[i] <- 0
        data_seg_mt_aux$Catastrofica_Duracao_Redes_Instalacoes[i] <- 0

        data_seg_mt_aux$Catastrofica_Contagem_Forca_Maior[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_Material_Equipamento[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_Outros[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_Atmosferica[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_Origem_Interna[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_Humanos[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_Tecnicas[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_Naturais[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_Entidade_Exterior[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_Razoes_Seguranca[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_Redes_Instalacoes[i] <- 0

        data_seg_mt_aux$Catastrofica_Contagem_1ano_Forca_Maior[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Material_Equipamento[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Outros[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Atmosferica[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Origem_Interna[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Humanos[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Tecnicas[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Naturais[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Entidade_Exterior[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Razoes_Seguranca[i] <- 0
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Redes_Instalacoes[i] <- 0

        data_seg_mt_aux$Incipientes_Variacao_ultimos2anos[i] <- 0
        data_seg_mt_aux$Falhas_Totais_Variacao_ultimos2anos[i] <- 0

      } else {

        data_seg_mt_aux$Duracao_Manutencoes_e_Previstas[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which((aux_f$Causa=="Prevista" | aux_f$Causa=="Previstas" | aux_f$Causa=="ManutenÃ§Ãµes") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico

        data_seg_mt_aux$Contagem_Manutencoes_e_Previstas[i] <- sum((aux_f$Causa=="Prevista" | aux_f$Causa=="Previstas" | aux_f$Causa=="ManutenÃ§Ãµes") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico

        data_seg_mt_aux$Contagem_1ano_Manutencoes_e_Previstas[i] <- sum((aux_f$Causa=="Prevista" | aux_f$Causa=="Previstas" | aux_f$Causa=="ManutenÃ§Ãµes") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total

        data_seg_mt_aux$Incipiente_Duracao_Forca_Maior[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="ForÃ§a maior" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Duracao_Material_Equipamento[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Material/Equipamento" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Duracao_Outros[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Outros" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Duracao_Atmosferica[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="AtmosfÃ©rica" | aux_f$Causa=="Atmosfericos") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Duracao_Origem_Interna[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Origem_Interna" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Duracao_Humanos[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Humanos" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Duracao_Tecnicas[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="TÃ©nicas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Duracao_Naturais[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Naturais" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Duracao_Entidade_Exterior[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="Entidade Exterior" | aux_f$Causa=="Entidades_exteriores") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Duracao_Razoes_Seguranca[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="RazÃµes de SeguranÃ§a" | aux_f$Causa=="Razoes_seguranca") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Duracao_Redes_Instalacoes[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="Redes_Instalacoes" | aux_f$Causa=="Redes_Instalações") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico


        data_seg_mt_aux$Incipiente_Contagem_Forca_Maior[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="ForÃ§a maior" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Contagem_Material_Equipamento[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Material/Equipamento" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Contagem_Outros[i] <- sum(aux_f$Causa=="Outros" & aux_f$Tipo.de.falha=="Incipiente" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Contagem_Atmosferica[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="AtmosfÃ©rica" | aux_f$Causa=="Atmosfericos") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Contagem_Origem_Interna[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Origem_Interna" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Contagem_Humanos[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Humanos" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Contagem_Tecnicas[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="TÃ©nicas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Contagem_Naturais[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Naturais" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Contagem_Entidade_Exterior[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="Entidade Exterior" | aux_f$Causa=="Entidades_exteriores") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Contagem_Razoes_Seguranca[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="RazÃµes de SeguranÃ§a" | aux_f$Causa=="Razoes_seguranca") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Incipiente_Contagem_Redes_Instalacoes[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="Redes_Instalacoes" | aux_f$Causa=="Redes_Instalações") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico


        data_seg_mt_aux$Incipiente_Contagem_1ano_Forca_Maior[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="ForÃ§a maior" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Incipiente_Contagem_1ano_Material_Equipamento[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Material/Equipamento" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Incipiente_Contagem_1ano_Outros[i] <- sum(aux_f$Causa=="Outros" & aux_f$Tipo.de.falha=="Incipiente" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Incipiente_Contagem_1ano_Atmosferica[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="AtmosfÃ©rica" | aux_f$Causa=="Atmosfericos") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Incipiente_Contagem_1ano_Origem_Interna[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Origem_Interna" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Incipiente_Contagem_1ano_Humanos[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Humanos" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Incipiente_Contagem_1ano_Tecnicas[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="TÃ©nicas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Incipiente_Contagem_1ano_Naturais[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa=="Naturais" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Incipiente_Contagem_1ano_Entidade_Exterior[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="Entidade Exterior" | aux_f$Causa=="Entidades_exteriores") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Incipiente_Contagem_1ano_Razoes_Seguranca[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="RazÃµes de SeguranÃ§a" | aux_f$Causa=="Razoes_seguranca") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Incipiente_Contagem_1ano_Redes_Instalacoes[i] <- sum(aux_f$Tipo.de.falha=="Incipiente" & (aux_f$Causa=="Redes_Instalacoes" | aux_f$Causa=="Redes_Instalações") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total


        data_seg_mt_aux$Catastrofica_Duracao_Forca_Maior[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="ForÃ§a maior" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Duracao_Material_Equipamento[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Material/Equipamento" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Duracao_Outros[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Outros" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Duracao_Atmosferica[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="AtmosfÃ©rica" | aux_f$Causa=="Atmosfericos") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Duracao_Origem_Interna[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Origem_Interna" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Duracao_Humanos[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Humanos" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Duracao_Tecnicas[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="TÃ©nicas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Duracao_Naturais[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Naturais" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Duracao_Entidade_Exterior[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="Entidade Exterior" | aux_f$Causa=="Entidades_exteriores") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Duracao_Razoes_Seguranca[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="RazÃµes de SeguranÃ§a" | aux_f$Causa=="Razoes_seguranca") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Duracao_Redes_Instalacoes[i] <- sum(aux_f$X.Qtd.Minutos.Duracao.Incidente[which(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="Redes_Instalacoes" | aux_f$Causa=="Redes_Instalações") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))])/comprimento_total / anos_historico


        data_seg_mt_aux$Catastrofica_Contagem_Forca_Maior[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="ForÃ§a maior" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Contagem_Material_Equipamento[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Material/Equipamento" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Contagem_Outros[i] <- sum(aux_f$Causa=="Outros" & aux_f$Tipo.de.falha=="CatastrÃ³fica" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Contagem_Atmosferica[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="AtmosfÃ©rica" | aux_f$Causa=="Atmosfericos") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Contagem_Origem_Interna[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Origem_Interna" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Contagem_Humanos[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Humanos" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Contagem_Tecnicas[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="TÃ©nicas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Contagem_Naturais[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Naturais" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Contagem_Entidade_Exterior[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="Entidade Exterior" | aux_f$Causa=="Entidades_exteriores") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Contagem_Razoes_Seguranca[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="RazÃµes de SeguranÃ§a" | aux_f$Causa=="Razoes_seguranca") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico
        data_seg_mt_aux$Catastrofica_Contagem_Redes_Instalacoes[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="Redes_Instalacoes" | aux_f$Causa=="Redes_Instalações") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total / anos_historico


        data_seg_mt_aux$Catastrofica_Contagem_1ano_Forca_Maior[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="ForÃ§a maior" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Material_Equipamento[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Material/Equipamento" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Outros[i] <- sum(aux_f$Causa=="Outros" & aux_f$Tipo.de.falha=="CatastrÃ³fica" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Atmosferica[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="AtmosfÃ©rica" | aux_f$Causa=="Atmosfericos") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Origem_Interna[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Origem_Interna" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Humanos[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Humanos" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Tecnicas[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="TÃ©nicas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Naturais[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa=="Naturais" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Entidade_Exterior[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="Entidade Exterior" | aux_f$Causa=="Entidades_exteriores") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Razoes_Seguranca[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="RazÃµes de SeguranÃ§a" | aux_f$Causa=="Razoes_seguranca") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total
        data_seg_mt_aux$Catastrofica_Contagem_1ano_Redes_Instalacoes[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & (aux_f$Causa=="Redes_Instalacoes" | aux_f$Causa=="Redes_Instalações") & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>(as.Date(date_pre_y_menos_1)) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total

        aux_var_inc <- ((sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_1) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre)))+(sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_2) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_menos_1))))
        if(aux_var_inc==0){
          data_seg_mt_aux$Incipientes_Variacao_ultimos2anos[i] <- 0
        }else{
          data_seg_mt_aux$Incipientes_Variacao_ultimos2anos[i] <- ((sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_1) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total)-(sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_2) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_menos_1))/comprimento_total_y_menos_1)) /(sum(aux_f$Tipo.de.falha=="Incipiente" & aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_2) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_menos_1))/comprimento_total_y_menos_1)
        }

        aux_var_cat <-((sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_1) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre)))+(sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_2) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_menos_1))))
        if(aux_var_cat==0){
        data_seg_mt_aux$Falhas_Totais_Variacao_ultimos2anos[i] <-0
        }else{
          data_seg_mt_aux$Falhas_Totais_Variacao_ultimos2anos[i] <- ((sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_1) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total)-(sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_2) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_menos_1))/comprimento_total_y_menos_1)) /(sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" & aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_2) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_menos_1))/comprimento_total_y_menos_1)
        }

      }

      #falhas catastroficas no ultimo ano
      data_seg_mt_aux$Falhas_Totais_por_km_y_menos_1[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" &  aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_1) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total

      #media anual das falhas catastroficas nos ultimos 2 anos
      data_seg_mt_aux$Falhas_Totais_por_km_y_menos_2_menos_1[i] <- ((sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" &  aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_1) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total)+(sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" &  aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_2) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_menos_1))/comprimento_total_y_menos_1))/2

      #media anual das falhas catastroficas nos ultimos 3 anos
      data_seg_mt_aux$Falhas_Totais_por_km_y_menos_3_menos_2_menos_1[i] <- ((sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" &  aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_1) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre))/comprimento_total)+(sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" &  aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_2) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_menos_1))/comprimento_total_y_menos_1) + (sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" &  aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_menos_3) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_menos_2))/comprimento_total_y_menos_2))/3

      #comprimentos dos proximos 3 anos
      data_seg_mt_aux$Comprimento_total_y_mais_1[i] <- comprimento_total_y_mais_1
      data_seg_mt_aux$Comprimento_total_y_mais_2[i] <- comprimento_total_y_mais_2
      data_seg_mt_aux$Comprimento_total_y_mais_3[i] <- comprimento_total_y_mais_3

      #falhas catastroficas no proximo ano
      data_seg_mt_aux$Falhas_Totais_por_km_y_mais_1[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" &  aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_mais_1))/comprimento_total_y_mais_1

      #falhas catastroficas daqui a 2 anos
      data_seg_mt_aux$Falhas_Totais_por_km_y_mais_2[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" &  aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_mais_1) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_mais_2))/comprimento_total_y_mais_2

      #falhas catastroficas daqui a 3 anos
      data_seg_mt_aux$Falhas_Totais_por_km_y_mais_3[i] <- sum(aux_f$Tipo.de.falha=="CatastrÃ³fica" &  aux_f$Causa!="ManutenÃ§Ãµes" & aux_f$Causa!="Prevista" & aux_f$Causa!="Previstas" & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")>=as.Date(date_pre_y_mais_2) & as.Date(levels(aux_f$Data.Deteccao.Incidente)[aux_f$Data.Deteccao.Incidente], format = "%d/%m/%Y")<as.Date(date_pre_y_mais_3))/comprimento_total_y_mais_3

      #distribuicao de aerea ou subterranea
      data_seg_mt_aux$Aerea_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.instalacao=="Aéreo")])/comprimento_total
      data_seg_mt_aux$Subterranea_por_km[i] <- sum(aux$Comprimento.Troco..m.[which(aux$Tipo.instalacao=="Subterrâneo")])/comprimento_total


    }

    #adiciona a informacao gerada a base de dados
    if(nrow(data_seg_mt)<1){
      data_seg_mt <- data_seg_mt_aux
    }else{
      data_seg_mt <- rbind(data_seg_mt,data_seg_mt_aux)
    }

    #actualiza a data
    date_pre <- date_ini
    substr(date_pre, 1, 4) <- as.character(as.numeric(substr(date_pre, 1, 4))+n)

    print(n)
  }

  #divide os resultados pelas diferentes categorias de segmentos
  #data_aereas <- data_seg_mt[which(data_seg_mt$Aerea_por_km>=0.95),]
  #data_subterraneas <- data_seg_mt[which(data_seg_mt$Subterranea_por_km>=0.95),]
  #data_mistos <- data_seg_mt[which(data_seg_mt$Aerea_por_km<0.95 & data_seg_mt$Subterranea_por_km<0.95),]
  #Data_MT <- list("Todos"=data_seg_mt,"Aereos"=data_aereas, "Subterraneos"=data_subterraneas, "Mistos"=data_mistos)
  Data_MT <- data_seg_mt

  #Path para guardar
  pack <- "iHEAD"
  path <- find.package(pack)

  #guardar resultado na package
  save(Data_MT, file = paste(path,"/data/Data_MT.RDA",sep = ""))
  return(Data_MT)
}
