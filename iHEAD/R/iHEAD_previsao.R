    ####################################################################################################
    ####################################################################################################
    ###                                                                                              ###
    ###Projeto HEAD                                                                                  ###
    ###Package responsavel por prever o numero de avarias para as linhas AT e MT,                    ###
    ###como também calcular a probabilidade de falha.                                                ###
    ###Empresa: INESCTEC                                                                             ###
    ###Autores: Alexandra Oliveira, Armando Leitao, Leonel Carvalho,                                 ###
    ### Luis Dias, Luis Guimaraes,Luis Miguel                                                        ###
    ###                                                                                              ###
    ####################################################################################################
    ####################################################################################################

    #Funcao para criar as pastas necessarias aquando a instalacao do package
    criar_pastas_iHEAD <- function(){
      pack <- "iHEAD"
      path <- find.package(pack)
      dir.create(paste0(path,"/Resultados"))

      #update da variavel path
      path<-paste0(path,"/Resultados")

      #criar pastas para resultados da análise Curto prazo
      dir.create(paste0(path,"/AT_CurtoPrazo"))
      dir.create(paste0(path,"/MT_Aereas_CurtoPrazo"))
      dir.create(paste0(path,"/MT_Mistas_CurtoPrazo"))
      dir.create(paste0(path,"/MT_Subterraneas_CurtoPrazo"))

      #criar pastas para resultados da análise Curto prazo
      dir.create(paste0(path,"/AT_LongoPrazo"))
      dir.create(paste0(path,"/MT_Aereas_LongoPrazo"))
      dir.create(paste0(path,"/MT_Mistas_LongoPrazo"))
      dir.create(paste0(path,"/MT_Subterraneas_LongoPrazo"))
    }

    #criar as pastas necessarias aquando da verificacao dos packages
    criar_pastas_iHEAD()

    # Funcao para instalar e ler packages
    verificar_packages <- function(){

      #carregar os packages que permitem ler, analisar e transformar dados respetivamente
      Required_Packages=c("openxlsx","readxl","plyr","pracma","zoo", "dplyr", "xgboost", "Metrics", "moments", "MASS","caret","ggplot2","glue","ModelMetrics","readr","vtreat","OpenMPController","caret","stringr", "naniar")

      Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];

      if(length(Remaining_Packages))
      {
        install.packages(Remaining_Packages);
      }
      for(package_name in Required_Packages)
      {
        library(package_name,character.only=TRUE,quietly=TRUE);
      }

    }

    #'Funcao para efetuar a analise no curto-prazo ou no longo-prazo
    #'
    #'@description Esta funcao agrega todas as funcoes desenvolvidas. Ao chamar esta funcao e possivel efetuar
    #'a analise de curto prazo ou longo prazo conforme o especificado pelo utilizador.
    #'@usage iHEAD_master_algorithm(flag_tipoLinha=c("AT","MT_Aereas","MT_Mistas","MT_Subterraneas"),flag_prazo=c(1,2), niter,flag_versao=c(1,2), horizonte,
    #'anosTeste, ano_inicio_estimativa, atualizar_modelo=c("sim","nao"), afinamento=c("sim","nao"))
    #'@param flag_tipoLinha Identificacao do tipo de linha a analisar
    #' "AT": Linhas AT
    #' "MT_Aereas": Linhas aereas MT
    #' "MT_Mistas": Linhas mistas MT
    #' "MT_Subterraneas": Linhas subterraneas MT
    #' Passar a flag entre ""
    #'@param flag_prazo Identifica o tipo de analise que se pretende
    #' 1: Curto-prazo: seleciona as variaveis para diferentes tipos de teste para no final o modelo escolhido ser aquele com melhor performance
    #' 2: Longo-prazo: seleciona as variaveis mais explicativas
    #'@param niter Numero de variaveis que se pretende obter na analise a longo prazo; Para curto prazo equivale ao numero de iteracoes que pretender correr o programa, sendo que o numero selecionado sera de acordo com um criterio
    #'@param flag_versao Identifica o tipo de versao que se pretende correr, completa ou resumida
    #'1: versao resumida: imprime lista de variaveis seleccionadas
    #'2: versao completa: imprime lista de variaveis seleccionadas e erros das previsoes
    #'@param horizonte Numero de anos que se pretende prever, minimo 1 e maximo 3
    #'@param anosTeste O numero de anos a definir para o teste, minimo 1 ano de teste e maximo 3 anos de teste
    #'@param ano_inicio_estimativa O ano em que comeca o teste, com um minimo fixado em 2016
    #'@param atualizar_modelo Permite utilizar um modelo previamente gerado para a previsão de falhas. Caso contrário o modelo será atualizado com os dados disponiveis.
    #'@param afinamento Permite selecionar o nivel de afinamento desejado no modelo gerado. Ao ativar o afinamento o modelo irá demorar mais tempo a atualizar os resultados.
    #'@return Excel com as variaveis selecionadas e os erros aos diferentes niveis geograficos
    #'@examples
    #'#Utilizar a funcao para efetuar a analise a longo-prazo, numa versao completa
    #'lista_resultados<-iHEAD_master_algorithm("MT_Aereas",2, 10, flag_versao=1, 1, 1, 2016, atualizar_modelo="sim", afinamento="sim")
    #'#Utilizar a funcao para efetuar a analise a curto-prazo, numa versao resumida
    #'lista_resultados<-iHEAD_master_algorithm("AT",1, 10, flag_versao=1, 1, 1, 2017, atualizar_modelo="sim", afinamento="nao")
    #'@export
    iHEAD_master_algorithm <- function(flag_tipoLinha, flag_prazo, niter, flag_versao=1, horizonte, anosTeste, ano_inicio_estimativa, atualizar_modelo="nao", afinamento="nao"){

      #Path package
      pack <- "iHEAD"
      path <- find.package(pack)

      #processar dados
      lista_dados<-processamento_dados(flag_tipoLinha, horizonte, anosTeste, ano_inicio_estimativa)

      #selecionar as variaveis significativas
      selecao_variaveis(lista_dados,flag_tipoLinha,flag_prazo, niter, flag_versao, horizonte, anosTeste, ano_inicio_estimativa,'log')

      if(flag_prazo==1){
        if(flag_tipoLinha=="AT"){
          resultados<-prever_numero_avarias_AT(lista_dados,atualizar_modelo,afinamento,'log')
          load(paste(path,"/Resultados/AT_CurtoPrazo/variaveis_melhor_modelo_xgboost_AT.rda",sep = ""))
        }else{
          resultados<-prever_numero_avarias_MT(lista_dados,atualizar_modelo,afinamento,str_sub(flag_tipoLinha,4),'log')
          load(paste(path,"/Resultados/",flag_tipoLinha,"_CurtoPrazo/variaveis_melhor_modelo_xgboost_",flag_tipoLinha,".rda",sep = ""))
        }
      }else if(flag_prazo==2){
        if(flag_tipoLinha=="AT"){
          resultados<-probabilidade_avaria_AT(lista_dados,atualizar_modelo,afinamento)
          load(paste(path,"/Resultados/AT_LongoPrazo/variaveis_melhor_modelo_xgboost_AT.rda",sep = ""))
        }else{
          resultados<-probabilidade_avaria_MT(lista_dados,atualizar_modelo,afinamento,str_sub(flag_tipoLinha,4))
          load(paste(path,"/Resultados/",flag_tipoLinha,"_LongoPrazo/variaveis_melhor_modelo_xgboost_",flag_tipoLinha,".rda",sep = ""))
        }
      }else{
        break
      }

      lista_Resultados<-list("Resultados"=resultados,"Variaveis_explicativas"=variaveis)
      return(lista_Resultados)
    }

    #retirar a definicao do log utilizado no modelo
    retrieve_log <- function(string){
      n<-3
      sapply(string, function(xx)
        substr(xx, (nchar(xx)-n+1), nchar(xx))
      )
    }

    #'Processamento dos dados juntamente com feature engineering
    #'
    #'@description Esta funcao ira fazer o pre-processamento de dados que incluiu conversao, padronizacao, remocao e definicao de variaveis.
    #'A funcao tambem dividie o conjunto de dados para treino e teste
    #'@usage pre_processamento_dados(flag_tipoLinha=c("AT","MT_Aereas","MT_Mistas","MT_Subterraneas"), horizonte=c(1,2,3), anosTeste=c(1,2,3), ano_inicio_estimativa=c(2016,sem limite))
    #'Esta funcao so deve ser corrida depois de correr (sem chamar) a funcao xxxxx DO MIGUEL.
    #'@param flag_tipoLinha Identificacao do tipo de linha a analisar
    #' "AT": Linhas AT
    #' "MT_Aereas": Linhas aereas MT
    #' "MT_Mistas": Linhas mistas MT
    #' "MT_Subterraneas": Linhas subterraneas MT
    #' Passar a flag entre ""
    #'@param horizonte Numero de anos que se pretende prever, minimo 1 e maximo 3
    #'@param anosTeste O numero de anos a definir para o teste, minimo 1 ano de teste e maximo 3 anos de teste
    #'@param ano_inicio_estimativa O ano em que comeca o teste, com um minimo fixado em 2016
    #'@return
    #'Dataset relativo ao treino e ao teste com os dados processados devidamente processados
    #'O vetor das variaveis numericas que contem as variaveis candidatas na selecao, util apenas para alimentar a funcao 'selecao_variaveis';
    #'A flag que indica o tipo de linha, util apenas para alimentar a funcao 'selecao_variaveis'.
    #'@examples
    #'#Utilizar a funcao para processar os dados relativos as linhas subterraneas MT, para um horizonte de um ano, com dois anos de teste comecar em 2017.
    #'Isto siginifica que se pretende prever as falhas em 2017 e em 2018 para linhas subterraneas MT.
    #'dados<-processamento_dados("MT_Subterraneas",1,2,2017)
    #'@export
    processamento_dados<-function(flag_tipoLinha, horizonte, anosTeste, ano_inicio_estimativa){

      #instalar as packages necessarios caso o utilizador ainda nao as tenha disponiveis
      verificar_packages()

      #Path package
      pack <- "iHEAD"
      path <- find.package(pack)

      ##############################Verificacao dos parametros inseridos pelo utilizador################

      #Verificar se o numero tem correspondencia com algum tipo de linha
      if("AT"!= flag_tipoLinha && "MT_Aereas" != flag_tipoLinha && "MT_Mistas" != flag_tipoLinha && "MT_Subterraneas" != flag_tipoLinha){
        return("Por favor selecione uma sigla valido para identificar o tipo linha: AT, MT_Aeras, MT_Mistas ou MT_Subterraneas.")
      }
      #Verificar se o horizonte escolhido esta dentro dos limites
      if(3 < horizonte || 1 > horizonte){
        return("Por favor selecione um horizonte de previsao entre 1 e 3.")
      }
      #verificar se os anos escolhidos para teste estao dentro dos limites
      if (3 < anosTeste || 1 > anosTeste){
        return("Por favor selecione um numero de anos para teste entre 1 e 3.")
      }
      #Verificar se o ano de inicio escolhido para teste esta dentro dos limites
      if (2016 > ano_inicio_estimativa){
        return("Por favor escolha o ano de inicio do teste a partir de 2016")
      }else if ((3 == horizonte | 2 == horizonte) & 2016==ano_inicio_estimativa){
        return("Nao e possivel comecar o teste em 2016 com um horizonte de previsao de 2 ou 3 anos")
      }

      #################################################################################################

      #Ler o dataset
      ##############!
      ##############!
      if("AT" == flag_tipoLinha){ #Se forem linhas AT
        load(paste(path,"/data/Data_AT.rda",sep = ""))
        dados <-Data_AT
        dados <-  replace_with_na_all(dados, condition = ~.x %in% c(-Inf, Inf))
      }else{ #Se forem linhas MT
        load(paste(path,"/data/Data_MT.rda",sep = ""))
        dados <-Data_MT
        dados <- replace_with_na_all(dados, condition = ~.x %in% c(-Inf, Inf))


      }

      #dados$Todos[sapply(dados$Todos, is.infinite)] <- NA

      if(ano_inicio_estimativa+anosTeste-1 > 2018){
        diferenca<-(ano_inicio_estimativa+anosTeste-1)-2018
        if((9+diferenca > max(dados$Anos_Historico))){
          return(paste0("Atencao que nao existem dados a partir do ano ",ano_inicio_estimativa+anosTeste-1," (inclusive), logo nao sera possivel utilizar como teste esse ano. Escolha por favor outro ano para iniciar o teste."))
        }
      }

      #Remover variaveis consoante o horizonte de previsao escolhido
      switch(horizonte,
             "1"={
               dados<-dados[,-grep("Falhas_Totais_por_km_y_mais_2",colnames(dados))]
               dados<-dados[,-grep("Falhas_Totais_por_km_y_mais_3",colnames(dados))]
               dados<-dados[,-grep("Comprimento_total_y_mais_2",colnames(dados))]
               dados<-dados[,-grep("Comprimento_total_y_mais_3",colnames(dados))]},

             "2"={
               dados<-dados[,-grep("Falhas_Totais_por_km_y_mais_1",colnames(dados))]
               dados<-dados[,-grep("Falhas_Totais_por_km_y_mais_3",colnames(dados))]
               dados<-dados[,-grep("Comprimento_total_y_mais_1",colnames(dados))]
               dados<-dados[,-grep("Comprimento_total_y_mais_3",colnames(dados))]},

             "3"={
               dados<-dados[,-grep("Falhas_Totais_por_km_y_mais_2",colnames(dados))]
               dados<-dados[,-grep("Falhas_Totais_por_km_y_mais_1",colnames(dados))]
               dados<-dados[,-grep("Comprimento_total_y_mais_2",colnames(dados))]
               dados<-dados[,-grep("Comprimento_total_y_mais_1",colnames(dados))]}
      )

      #Remover colunas totalmente vazias
      colunas_vazias<-sapply(dados, function (k) all(is.na(k)))
      dados<-dados[,!colunas_vazias]

      #Se se tratar de linhas MT, ? necess?rio descarregar as ?reas dos distritos e concelhos para efeitos de c?lculo
      #Ler dados: Areas de cada distrito e concelho
      if("AT" != flag_tipoLinha){

        areas<-iHEAD::areas
        #Filtrar as colunas necessarias
        areas <- areas[, c("Codigo Concelho",
                           "Area do concelho",
                           "Codigo distritos unicos",
                           "Area distritos"
        )]

        #Mudar o nome das colunas
        colnames(areas) <- c("Codigo_Concelho",
                             "Area_Concelho",
                             "Codigo_Distrito",
                             "Area_Distrito"
        )

        #Colocar um D ou um C antes do codigo do distrito ou concelho para serem identificados como tal
        for(i in areas$Codigo_Distrito)areas$Codigo_Distrito[which(areas$Codigo_Distrito==i)]<-paste("D",areas$Codigo_Distrito[which(areas$Codigo_Distrito==i)],sep = "")
        for(i in areas$Codigo_Concelho)areas$Codigo_Concelho[which(areas$Codigo_Concelho==i)]<-paste("C",areas$Codigo_Concelho[which(areas$Codigo_Concelho==i)],sep = "")
        #Conversao para dados numericos
        areas$Area_Concelho<-as.numeric(areas$Area_Concelho)
        areas$Area_Distrito<-as.numeric(areas$Area_Distrito)
      }

      names(dados)[names(dados) == "Distrito_RC"] <- "Distrito"
      names(dados)[names(dados) == "Concelho_RC"] <- "Concelho"



      #Convers?o das vari?veis categ?ricas (factor) em vari?veis num?ricas
      #Variaveis a converter
      cols = c('Tensao','Em_Exploracao','Acima_600m_por_km','Distrito','Concelho')
      #Criar dummies para distrito e concelho (so para efeitos de calculo)
      for(i in unique(dados$Distrito))dados$Distrito[which(dados$Distrito==i)]<-paste("D",dados$Distrito[which(dados$Distrito==i)],sep = "")
      for(i in unique(dados$Concelho))dados$Concelho[which(dados$Concelho==i)]<-paste("C",dados$Concelho[which(dados$Concelho==i)],sep = "")

      if( "AT" != flag_tipoLinha){ #Para linhas MT
        Tensao=c('6','10','15','30')
      }else{ #Para linhas AT
        Tensao=c('60','130')
      }

      #Se a linha se encontra num concelho acima dos 600m. Se for 0,n?o se encontra.
      dados$Acima_600m_por_km[which(dados$Acima_600m_por_km==0)]<-"Nao"
      dados$Acima_600m_por_km[which(dados$Acima_600m_por_km==1)]<-"Sim"
      Acima_600m_por_km=c("Nao","Sim")

      #N?veis
      levels=list(Tensao,Acima_600m_por_km) #definir nivel ordenados
      i=1
      for (c in cols) {
        print(c)
        if (c == "Tensao" | c == "Acima_600m_por_km") {
          dados[, c] = as.numeric(factor(unlist(dados[, c]),ordered = TRUE, levels = levels[[i]])) #fator para passar numerico ordenado
          i = i + 1
        } else {
          dados[,c]<-as.factor(unlist(dados[,c])) #para passar para dummy porque s?o n?o mum?ricas
        }
      }

      #Lista com as variaveis a remover, a transformar e as apenas informativas
      #1 - variaveis informativas (cor azul)
      #2 - variaveis a remover (cor vermelha)
      #3 - variaveis a incluir (cor verde)
      #4 - variaveis a remover para o caso de linhas subterraneas MT (cor roxa)

      #Leitura do ficheiro excel
     # if("AT" == flag_tipoLinha){
      #  lista_variaveis<-iHEAD::Lista_variaveis_AT
      #}else{
      #  lista_variaveis<-iHEAD::Lista_variaveis_MT
      #}

      if("AT" == flag_tipoLinha){
        lista_variaveis<-read.csv2(file="D:/LuisDias/final_deliverable_iHEAD/old results/Lista_variaveis_AT_final.csv")
      }else{
        lista_variaveis<-read.csv2(file="D:/LuisDias/final_deliverable_iHEAD/old results/Lista_variaveis_MT_final.csv")
      }
      colnames(lista_variaveis)[1] <- "Lista Variaveis"

      #Criacao de uma lista com duas colunas identificadas
      lista_variaveis<-lista_variaveis[,c("Lista Variaveis", "Tipo")]
      colnames(lista_variaveis)<- c("Variaveis", "Tipo")

      if ("MT_Aereas" == flag_tipoLinha){ #Linhas aereas

        dados<-dados[which(dados$Aerea_por_km>=0.95),]
        dados<-dados[ , !(names(dados) %in% "Subterranea_por_km")]
        dados<-dados[ , !(names(dados) %in% "Aerea_por_km")]

      }else if ("MT_Mistas" == flag_tipoLinha){ #Linhas mistas

        dados<-dados[which(dados$Aerea_por_km<0.95 & dados$Aerea_por_km>0),]
        dados<-dados[ , !(names(dados) %in% "Subterranea_por_km")]
        dados<-dados[ , !(names(dados) %in% "Aerea_por_km")]

      }else if ("MT_Subterraneas" ==flag_tipoLinha){ #Linhas subterraneas

        dados<-dados[which(dados$Aerea_por_km==0),]
        variaveis_remover_cabos<-lista_variaveis$Variaveis[which(lista_variaveis$Tipo==4)] #Se forem cabos existem mais variaveis que devem ser removidas
        dados<-dados[,!(colnames(dados) %in% variaveis_remover_cabos)]
        dados<-dados[ , !(names(dados) %in% "Subterranea_por_km")]
        dados<-dados[ , !(names(dados) %in% "Aerea_por_km")]

      }

      #Depois de selecionados os dados, remover variaveis comuns
      variaveis_remover<-lista_variaveis$Variaveis[which(lista_variaveis$Tipo==2)] #vetor com as variaiveis a remover para a analise
      dados<-dados[,!(colnames(dados) %in% variaveis_remover)]

      #Converter variaveis em texto para numericas
      for(i in names(dados)){
        if(i!="Distrito" && i != "Concelho" && i!="Em_Exploracao" && i!="Acima_600m_por_km" && i!="Tensao" && i!= "Codigo.da.linha" && i!="Codigo.Circuito" && !is.numeric(unlist(dados[,i]))){
          print(i)
          dados[,i]<-as.factor(unlist(dados[,i]))
          dados[,i]<-as.numeric(unlist(dados[,i]))
        }
      }

      # Retirar variaveis informativas (Segmento, Codigo Circuito e data presente) dado que nao vamos precisar das mesmas no modelo gerado
      variaveis_remover<-c("Segmento", "Data_Presente")
      dados<-dados[,!colnames(dados) %in% variaveis_remover]

      #Retirar linhas com comprimento inferior a 1km
      dados<-dados[which(dados$Comprimento_Total>=1),]

      # Preencher os campos vazios pelo valor medio da coluna
      for(i in 1:ncol(dados)){
        if(sum(is.na(unlist(dados[,i])))>0 && is.numeric(unlist(dados[,i]))){
          dados[which(is.na(unlist(dados[,i]))),i]<-sum(unlist(dados[which(!is.na(unlist(dados[,i]))),i]))/length(unlist(dados[which(!is.na(unlist(dados[,i]))),i]))
        }
      }

      # Remover variaveis com zero de variancia, ou seja cuja coluna contem o mesmo valor em todas as linhas
      if(length(as.numeric(which(apply(dados, 2, var) == 0)))>0){
        dados <- dados[, - as.numeric(which(apply(dados, 2, var) == 0))]
        oldw <- getOption("warn")
        options(warn = -1)
      }


      #Verificar se existem NaN ou NA
      if(0 != length(which(apply(dados, 2, function(x) any(is.nan(x))))) | 0 != length(which(apply(dados, 2, function(x) any(is.na(x)))))){
        return("Existem NAs")
      }

      # Encontrar o tipo de dados para cada uma das variaveis
      feature_classes <- sapply(names(dados),function(x){class(dados[[x]])})
      #Nome da colunas cuja variaveis sao numericas
      numeric_feats <-names(feature_classes[feature_classes == "numeric"])
      #Retirar das variaveis numericas as variaveis informativas que nao poderao entrar na selecao de variaveis
      numeric_feats<-numeric_feats[!numeric_feats %in% c("Anos_Historico", "Comprimento_Total")]
      #Assim como a variavel falhas que e o output a prever
      aux<-c()
      for(i in 1:length(numeric_feats)){
        if(grepl("Falhas_Totais_por_km_y_mais",numeric_feats[i])){
          aux[1]<-numeric_feats[i]
        }
      }
      for(i in 1:length(numeric_feats)){
        if(grepl("Comprimento_total_y_mais",numeric_feats[i])){
          aux[2]<-numeric_feats[i]
        }
      }
      numeric_feats<-numeric_feats[!(numeric_feats %in% aux)]

      #Nome da colunas cuja variaveis sao categoricas
      # if(flag_tipoLinha == "AT"){
      #   categorical_feats <- names(feature_classes[feature_classes != "numeric" & names(dados)!="Codigo.da.linha"])
      # }else{
      #   categorical_feats <- names(feature_classes[feature_classes != "numeric" & names(dados)!="Codigo.Circuito"])
      #
      # }

      # Verificar se a distribuicao de cada variavel esta enviesada ou nao
      skewed_feats <- sapply(numeric_feats,function(x){skewness(dados[[x]],na.rm=TRUE)}) #calcula a media de cada um dos valores de cada coluna diferentes de NA
      skewed_feats[is.na(skewed_feats)]<-0

      #Padronizar variaveis numericas apenas
      for(x in names(skewed_feats)) {
        if(x!= "Falhas_Totais_por_km_y_mais_1" & x!="Falhas_Totais_por_km_y_mais_2" & x!="Falhas_Totais_por_km_y_mais_3"){
          media<-mean(unlist(dados[,x]), na.rm=TRUE) #media de cada coluna
          dp<-sd(unlist(dados[,x]), na.rm=TRUE) #desvio-padrao de cada coluna
          dados[,x]<-(dados[,x]-media)/dp
        }
      }

      #Definir treino e teste
      dif<-ano_inicio_estimativa - 2016 #diferenca entre o ano incio escolhido (que no minimo e igual a 2016) e o ano de 2016


      if("AT" != flag_tipoLinha){
        if(1 == anosTeste){
          teste<-dados[which(dados$Anos_Historico==(7+dif)),]
          teste_Codigo_Circuito<-dados$Codigo.Circuito[which(dados$Anos_Historico==(7+dif))]
          treino<-dados[which(dados$Anos_Historico<=((7+dif)-horizonte)),]
          treino_Codigo_Circuito<-dados$Codigo.Circuito[which(dados$Anos_Historico<=((7+dif)-horizonte))]
        }else{
          teste<-dados[which(dados$Anos_Historico>=(7+dif) & dados$Anos_Historico<(7+dif+anosTeste)),]
          teste_Codigo_Circuito<-dados$Codigo.Circuito[which(dados$Anos_Historico>=(7+dif) & dados$Anos_Historico<(7+dif+anosTeste))]
          treino<-dados[which(dados$Anos_Historico<=((7+dif)-horizonte)),]
          treino_Codigo_Circuito<-dados$Codigo.Circuito[which(dados$Anos_Historico<=((7+dif)-horizonte))]
        }
      }else{
        if(1 == anosTeste){
          teste<-dados[which(dados$Anos_Historico==(7+dif)),]
          teste_Codigo_Circuito<-dados$Codigo.da.linha[which(dados$Anos_Historico==(7+dif))]
          treino<-dados[which(dados$Anos_Historico<=((7+dif)-horizonte)),]
          treino_Codigo_Circuito<-dados$Codigo.da.linha[which(dados$Anos_Historico<=((7+dif)-horizonte))]
        }else{
          teste<-dados[which(dados$Anos_Historico>=(7+dif) & dados$Anos_Historico<(7+dif+anosTeste)),]
          teste_Codigo_Circuito<-dados$Codigo.da.linha[which(dados$Anos_Historico>=(7+dif) & dados$Anos_Historico<(7+dif+anosTeste))]
          treino<-dados[which(dados$Anos_Historico<=((7+dif)-horizonte)),]
          treino_Codigo_Circuito<-dados$Codigo.da.linha[which(dados$Anos_Historico<=((7+dif)-horizonte))]
        }
      }

      #Guardar o comprimento das linhas pertencentes ao treino e ao teste
      switch (horizonte,
              "1" = {colnames(treino)[which(names(treino) == "Falhas_Totais_por_km_y_mais_1")] <- "Falhas_Totais_porKm"
              colnames(treino)[which(names(treino) == "Comprimento_total_y_mais_1")] <- "Comprimento_total"
              colnames(teste)[which(names(teste) == "Falhas_Totais_por_km_y_mais_1")] <- "Falhas_Totais_porKm"
              colnames(teste)[which(names(teste) == "Comprimento_total_y_mais_1")] <- "Comprimento_total"
              },

              "2" = {colnames(treino)[which(names(treino) == "Falhas_Totais_por_km_y_mais_2")] <- "Falhas_Totais_porKm"
              colnames(treino)[which(names(treino) == "Comprimento_total_y_mais_2")] <- "Comprimento_total"
              colnames(teste)[which(names(teste) == "Falhas_Totais_por_km_y_mais_2")] <- "Falhas_Totais_porKm"
              colnames(teste)[which(names(teste) == "Comprimento_total_y_mais_2")] <- "Comprimento_total"},

              "3" = {colnames(treino)[which(names(treino) == "Falhas_Totais_por_km_y_mais_3")] <- "Falhas_Totais_porKm"
              colnames(treino)[which(names(treino) == "Comprimento_total_y_mais_3")] <- "Comprimento_total"
              colnames(teste)[which(names(teste) == "Falhas_Totais_por_km_y_mais_3")] <- "Falhas_Totais_porKm"
              colnames(teste)[which(names(teste) == "Comprimento_total_y_mais_3")] <- "Comprimento_total"}
      )

      # if("AT" != flag_tipoLinha){
      #
      #   numeric_feats<-c(numeric_feats, "Falhas_Totais_porKm", "Comprimento_total", "Anos_Historico")
      #
      #   # Criar as vari?veis dummy: Distrito e concelho, para calcular o erro pesado ao nivel do concelho e do distrito
      #   dummies <- dummyVars(~.,treino[categorical_feats])
      #   dummies_teste <- dummyVars(~.,teste[categorical_feats])
      #   categorical_1_hot <- predict(dummies,treino[categorical_feats])
      #   categorical_1_hot_teste <- predict(dummies_teste,teste[categorical_feats])
      #   # Separar os distritos e concelhos no teste e treino
      #   #Treino
      #   Distrito_treino<-treino$Distrito
      #   Distrito_treino<-unlist(lapply(unlist(Distrito_treino), as.character))
      #   Concelho_treino<-treino$Concelho
      #   Concelho_treino<-unlist(lapply(unlist(Concelho_treino), as.character))
      #   treino <- cbind(treino[numeric_feats],categorical_1_hot)
      #   treino <- cbind(treino,"Codigo.Circuito"=as.character(treino_Codigo_Circuito))
      #
      #   #Teste
      #   Distrito_teste<-teste$Distrito
      #   Distrito_teste<-unlist(lapply(unlist(Distrito_teste), as.character))
      #   Concelho_teste<-teste$Concelho
      #   Concelho_teste<-unlist(lapply(unlist(Concelho_teste), as.character))
      #   teste <- cbind(teste[numeric_feats],categorical_1_hot_teste)
      #   teste <- cbind(teste,"Codigo.Circuito"=as.character(teste_Codigo_Circuito))
      #
      #   numeric_feats<-numeric_feats[!(numeric_feats %in% c("Falhas_Totais_porKm", "Comprimento_total", "Anos_Historico"))]
      #
      # }
      if("AT" != flag_tipoLinha){
        return(list("treino"=treino, "teste"=teste,"areas"=areas,
                    "flag_tipoLinha"=flag_tipoLinha,"var_numericas"=numeric_feats))
      }else{
        return(list("treino"=treino, "teste"=teste,
                    "flag_tipoLinha"=flag_tipoLinha,"var_numericas"=numeric_feats))

      }

    }
    transformacao<-function(transformacao_var,Falhas){

      t <- c()
      if('log'== transformacao_var){
        t <- c(t,log(Falhas+1))
        return(t)
      }

      else if ('square'== transformacao_var){
        t <- c(t,(Falhas)^2)
        return(t)
      }
      else if ("reciprocal" == transformacao_var){
        t <- c(t,1/(Falhas+1))
        return(t)
      }

    }

    transformacao_inversa<-function(transformacao_var,comprimento,pred){

      it <- c()
      pred[pred<0]<-0

      if('log'== transformacao_var){
         it <- c(it,(exp(pred)-1)*comprimento)
         return (it)
      }
      else if ('square'== transformacao_var){
        it <- c(it,(sqrt(pred)*comprimento))
        return(it)

      }
      else if ("reciprocal" == transformacao_var){
       it <- c(it,(1/pred-1)*comprimento)
       return (it)
      }

    }

    #'Selecao_variaveis de variaveis significativas
    #'
    #'@description Esta funcao ira fazer a selecao das variaveis mais significativas consoante o tipo de analise selecionada
    #'@usage selecao_variaveis(flag_prazo=c(1,2), niter=c(1, sem limite))
    #'A funcao so pode ser corrido depois de se correr (nao chamar) a funcao "processar_dados"
    #'@param lista_dados Dados a utilizar no processo de selecao de variaveis
    #'@param flag_tipoLinha Identificacao do tipo de linha a analisar
    #' "AT": Linhas AT
    #' "MT_Aereas": Linhas aereas MT
    #' "MT_Mistas": Linhas mistas MT
    #' "MT_Subterraneas": Linhas subterraneas MT
    #' Passar a flag entre ""
    #'@param flag_prazo Identifica o tipo de analise que se pretende
    #' 1: Curto-prazo: seleciona as variaveis para diferentes tipos de teste para no final o modelo escolhido ser aquele com melhor performance
    #' 2: Longo-prazo: seleciona as variaveis mais explicativas
    #'@param niter Numero de variaveis que se pretende obter na analise a longo prazo; Para curto prazo equivale ao numero de iteracoes que pretender correr o programa, sendo que o numero selecionado sera de acordo com um criterio
    #'@param flag_versao Identifica o tipo de versao que se pretende correr, completa ou resumida
    #'1: versao resumida: imprime lista de variaveis seleccionadas
    #'2: versao completa: imprime lista de variaveis seleccionadas e erros das previsoes
    #'@param horizonte Numero de anos que se pretende prever, minimo 1 e maximo 3
    #'@param anosTeste O numero de anos a definir para o teste, minimo 1 ano de teste e maximo 3 anos de teste
    #'@param ano_inicio_estimativa O ano em que comeca o teste, com um minimo fixado em 2016
    #'@return Excel com as variaveis selecionadas e os erros aos diferentes niveis geograficos
    #'@examples
    #'#Utilizar a funcao para selecionar as variaveis numa analise a longo-prazo, numa versao completa
    #'selecao_variaveis(2,15,2)
    #'#Utilizar a funcao para selecionar as variaveis numa analise a curto-prazo, numa versao resumida
    #'selecao_variaveis(1,10,1)
    #'@export
    selecao_variaveis<-function(lista_dados,flag_tipoLinha,flag_prazo, niter, flag_versao, horizonte, anosTeste, ano_inicio_estimativa,transformacao_var){

      #Path package
      pack <- "iHEAD"
      path <- find.package(pack)
      #Chamar funcao de processar os dados
      output<-lista_dados
      #Extrair outputs da funcao
      var_numericas<-output$var_numericas #Vetor com as variaveis candidatas a selecao
      treino<-output$treino #Dataset de treino
      teste<-output$teste #Dataset de teste

      if("AT" != flag_tipoLinha){
        areas<-output$areas #Dataset com as areas dos distritos e concelhos
        #Identifica??o do distrito e concelho de cada linha pertencete ao dataset do treino ou do teste
        Distrito_treino<-output$Distrito_treino
        Distrito_teste<-output$Distrito_teste
        Concelho_treino<-output$Concelho_treino
        Concelho_teste<-output$Concelho_teste

        treino<-treino[, -grep("Codigo.Circuito",colnames(treino))]
        teste<-teste[, -grep("Codigo.Circuito",colnames(teste))]

        variaveis_remover<-c("Codigo.Circuito")
        var_numericas<-var_numericas[!(var_numericas %in% variaveis_remover)]
      }else{
        treino<-treino[, -grep("Codigo.da.linha",colnames(treino))]
        teste<-teste[, -grep("Codigo.da.linha",colnames(teste))]

        variaveis_remover<-c("Codigo.da.linha")
        var_numericas<-var_numericas[!(var_numericas %in% variaveis_remover)]
      }

      #Extrair valores
      comprimento_treino<-treino$Comprimento_total
      comprimento_teste<-teste$Comprimento_total

      #Verificar se a flag_prazo esta dentro dos limites
      if(2<flag_prazo | 1>flag_prazo){
        return("O valor da flag_prazo pode ser 1 ou 2. Por favor insira um valor valido")
      }
      if(1 > niter){
        return("Por favor selecione um numero de iteracoes valido")
      }
      #Variaveis relativas a localizacao
      var_localizacao<-c("Aveiro_por_km", "Beja_por_km", "Braga_por_km",
                         "Bragan?a_por_km", "Castelo_Branco_por_km", "Coimbra_por_km",
                         "Evora_por_km", "Faro_por_km", "Guarda_por_km",
                         "Leiria_por_km", "Lisboa_por_km", "Portalegre_por_km",
                         "Porto_por_km", "Santarem_por_km", "Setubal_por_km",
                         "Viana_do_Castelo_por_km", "Vila_Real_por_km", "Viseu_por_km")
      #Variaveis associadas a causas de falhas catastroficas
      a<-1
      var_catastroficas<-c()
      for(i in 1:length(var_numericas)){
        if(grepl("Catastrofica",var_numericas[i])){
          var_catastroficas[a]<-var_numericas[i]
          a<-a+1
        }
      }
      #Variaveis de registos historico de falhas
      var_historicoFalhas<-c("Falhas_Totais_por_km_y_menos_1",
                             "Falhas_Totais_por_km_y_menos_2_menos_1",
                             "Falhas_Totais_por_km_y_menos_3_menos_2_menos_1"
                             )

      var_longoPrazo<-var_numericas[!(var_numericas %in% var_localizacao)]
      var_longoPrazo<-var_longoPrazo[!(var_longoPrazo %in% var_catastroficas)]
      #vetor sem as variaveis de causas de falhas catatroficas, sem localizacao e sem registo historico de falhas
      var_longoPrazo<-var_longoPrazo[!(var_longoPrazo %in% var_historicoFalhas)]

      if(2==flag_prazo & length(var_longoPrazo)<niter){#Confirmar o numero de variaveis candidatas numa analise a longo prazo para fazer a ultima verificacao: se o niter escolhido nao excede o numero de variaveis disponiveis
        return(paste0("O numero de variaveis que pretende selecionar excede o numero de variaveis disponiveis, por favor selecione um numero menor ou igual a ", length(var_longoPrazo)))
      }

      #vetor sem as variaveis de localizacao
      var_semLoc<-var_numericas[!(var_numericas %in% var_localizacao)]
      #vetor sem as variaveis de causas de falhas catatroficas
      var_semCat<-var_numericas[!(var_numericas %in% var_catastroficas)]
      #vetor sem as variaveis de causas de falhas catatroficas e sem localizacao
      var_semLocCat<-var_semLoc[!(var_semLoc %in% var_catastroficas)]

      #################################Inicio do algoritmo de selecao##########################

      if(1 ==flag_prazo){#Se foi escolhida uma analise a curto prazo
        ntestes<-6 #O numero de testes a realizar sao 6
        #Construir a lista com as diferentes tipos de variaveis
        z1 <- zoo(var_semLoc)
        z2 <- zoo(var_semCat)
        z3 <- zoo(var_semLocCat)
        nm <- list("z1", "z2", "z3")
        matriz_variaveis <- zoo()
        for(i in 1:length(nm)) matriz_variaveis <- merge(matriz_variaveis, get(nm[[i]]))
        names(matriz_variaveis) <- unlist(nm)
        #Vetor com o nome dos testes a realizar
        vetor_testes<-c("SemLoc", "SemCausasCat", "SemLoc_SemCausasCat")
      }else{
        ntestes<-2 #Caso seja a longo prazo entao sao somente 2 testes
      }


      if("AT" != flag_tipoLinha){
        #Nomes dos distritos e concelhos unicos
        nctr<-unique(treino$Concelho)
        nct<-unique(teste$Concelho)
        ndtr<-unique(treino$Distrito)
        ndt<-unique(teste$Distrito)

        #Calcular o peso de cada concelho do treino -> normalizado
        peso_concelho_treino <- setNames(data.frame(matrix(ncol = 2, nrow = length(nctr))), c("Codigo_concelho", "Area"))
        areas_concelhos_treino<-sum(areas$Area_Concelho[match(nctr,areas$Codigo_Concelho)])
        for(i in 1:length(nctr)){
          posicao<-match(nctr[i], areas$Codigo_Concelho)
          peso_concelho_treino[i,1]<-areas$Codigo_Concelho[posicao]
          peso_concelho_treino[i,2]<-areas$Area_Concelho[posicao]/areas_concelhos_treino
        }

        #Calcular o peso de cada concelho do teste -> normalizado
        peso_concelho_teste<-setNames(data.frame(matrix(ncol = 2, nrow = length(nct))), c("Codigo_concelho", "Area"))
        areas_concelhos_teste<-sum(areas$Area_Concelho[match(nct, areas$Codigo_Concelho)])
        for(i in 1:length(nct)){
          posicao<-match(nct[i], areas$Codigo_Concelho)
          peso_concelho_teste[i,1]<-areas$Codigo_Concelho[posicao]
          peso_concelho_teste[i,2]<-areas$Area_Concelho[posicao]/areas_concelhos_teste
        }

        #Calcular peso de cada distrito do teste -> normalizado
        peso_distrito_teste<-setNames(data.frame(matrix(ncol = 2, nrow = length(ndt))), c("Codigo_distrito", "Area"))
        areas_distritos_teste<-sum(areas$Area_Distrito[match(ndt,areas$Codigo_Distrito)])
        for(i in 1:length(ndt)){
          posicao<-match(ndt[i], areas$Codigo_Distrito)
          peso_distrito_teste[i,1]<-areas$Codigo_Distrito[posicao]
          peso_distrito_teste[i,2]<-areas$Area_Distrito[posicao]/areas_distritos_teste
        }

        #Calcular peso de cada distrito do treino -> normalizado
        peso_distrito_treino<-setNames(data.frame(matrix(ncol = 2, nrow = length(ndtr))), c("Codigo_distrito", "Area"))
        areas_distritos_treino<-sum(areas$Area_Distrito[match(ndtr,areas$Codigo_Distrito)])
        for(i in 1:length(ndtr)){
          posicao<-match(ndtr[i], areas$Codigo_Distrito)
          peso_distrito_treino[i,1]<-areas$Codigo_Distrito[posicao]
          peso_distrito_treino[i,2]<-areas$Area_Distrito[posicao]/areas_distritos_treino
        }
      }

      b<-1 #Para passar de coluna em coluna na matriz das variqaveis para os diferentes testes
      for(z in 1:ntestes){

        #Inicializacao de variaveis e matrizes a cada iteracao de ntestes#####################
        if(0 == (z%%2)){
          flag_log<-0 #teste sem logaritmo na variavel a prever
          if(1==flag_prazo){
            v<-matriz_variaveis[,b-1]
            v<-as.vector(v)
            v<- v[!is.na(v)]
            tipoTeste<-vetor_testes[b-1]
          }else{
            v<-var_longoPrazo
          }
        }else{
          flag_log<-1 #teste com logaritmo na variavel a prever
          if(1 == flag_prazo){
            v<-matriz_variaveis[,b]
            v<-as.vector(v)
            v<- v[!is.na(v)]
            tipoTeste<-vetor_testes[b]
            b<-b+1
          }else{
            v<-var_longoPrazo
          }
        }

       # newv<-c("Anomalia_A_Contagem",  "Anomalia_A_Contagem_1ano",
        #        "Anomalia_A_Cabos_Contagem", "Anomalia_A_Isoladores_Contagem", "Anomalia_A_Fundacoes_Contagem", "Anomalia_A_Faixa_Contagem", "Anomalia_A_Corte_Protecao_Cabos_Guarda_Contagem", "Anomalia_A_Outros_Contagem",
         #       "Anomalia_A_Cabos_Contagem_1ano", "Anomalia_A_Isoladores_Contagem_1ano", "Anomalia_A_Fundacoes_Contagem_1ano", "Anomalia_A_Faixa_Contagem_1ano", "Anomalia_A_Corte_Protecao_Cabos_Guarda_Contagem_1ano", "Anomalia_A_Outros_Contagem_1ano") #vetor que vai conter as variaveis seleccionadas
        newv<-c("Anomalia_A_Contagem",  "Anomalia_A_Contagem_1ano")
        #newv<-c()
        v <- v[!(v %in% newv)]
        j<-0 #Numero de variaveis
        #Matriz falhas reais do treino
        falhas_reais_treino<- as.data.frame(cbind(treino$Anos_Historico,treino$Falhas_Totais_porKm*comprimento_treino))
        colnames(falhas_reais_treino)<-c("Anos","Falhas")
        #Matriz falhas reais do teste
        falhas_reais_teste<- as.data.frame(cbind(teste$Anos_Historico,teste$Falhas_Totais_porKm*comprimento_teste))
        colnames(falhas_reais_teste)<-c("Anos","Falhas")
        #Numero de anos do treino e do teste
        anosTreino<-(unique(falhas_reais_treino$Anos))
        anosTeste<-(unique(falhas_reais_teste$Anos))
        #Vetores que cont?m os erros aos diferentes n?veis para o dataset do treino e do teste
        eam_segmento_treino_final<-c() #erro absoluto m?dio ao n?vel do segmento
        eam_segmento_teste_final<-c() #erro absoluto m?dio ao n?vel do segmento
        eam_pais_treino<-c() #erro absoluto m?dio ao n?vel do pa?s
        eam_pais_teste<-c() #erro absoluto m?dio ao n?vel do pa?s
        #Para as linhas MT ser?o calculados erros ao nivel do distrito e concelho porque n?o s?o t?o longas como as linhas AT, logo os erros n?o ser?o t?o elevados
        if("AT" != flag_tipoLinha){
          eam_distrito_treino<-c() #erro absoluto m?dio ao n?vel do pa?s
          eam_distrito_teste<-c() #erro absoluto m?dio ao n?vel do pa?s
          eam_concelho_treino<-c() #erro absoluto m?dio ao n?vel do pa?s
          eam_concelho_teste<-c() #erro absoluto m?dio ao n?vel do pa?s
        }

        ################################################################################

        #Verificar se o teste sera feito com ou sem logaritmo na variavel a prever
        if (1 == flag_log){
          treino$Falhas_Totais_porKm <- transformacao(transformacao_var,treino$Falhas_Totais_porKm)
          teste$Falhas_Totais_porKm <- transformacao(transformacao_var,teste$Falhas_Totais_porKm)

          #treino$Falhas_Totais_porKm <- log(treino$Falhas_Totais_porKm+1)
          #teste$Falhas_Totais_porKm <- log(teste$Falhas_Totais_porKm+1)
          log<-"ComLog"
        }else{
          log<-"SemLog"
        }

        #Ciclo de selecao
        while(j < niter){


          j<-j+1
          #Variaveis auxiliares inicializadas a cada iteracao
          i<-1
          eam_segmento_treino<-c() #vetor dos erros medios absolutos do concelho no treino
          eam_segmento_teste<-c() #vetor dos erros medios absolutos do concelho no teste
          falhas_previstas_treino<-matrix(data=NA,nrow=nrow(treino),ncol=length(v))
          falhas_previstas_teste<-matrix(data=NA,nrow=nrow(teste),ncol=length(v))

          for(i in 1:length(v)){

            #Modelo com treino
            if(0==length(newv)){
              Aux<-treino[,c(v[i],"Falhas_Totais_porKm")]
            }else{

              Aux<-treino[,c(newv,v[i],"Falhas_Totais_porKm")]
            }
            model<-lm(Falhas_Totais_porKm ~., data =Aux )

            #Previsao com treino
            f<-grep("Falhas_Totais_porKm", colnames(Aux))
            Aux<-Aux[,-f, drop=FALSE] #Tirar falhas
            preds<-predict(model,newdata=Aux)

            if(0 == flag_log){
              falhas_previstas_treino[,i]<-preds*comprimento_treino
            }
            else{ # Se flag_log==1 o teste e com logaritmo, logo no final este tem de ser retirado
              #falhas_previstas_treino[,i]<-(exp(preds)-1)*comprimento_treino
              falhas_previstas_treino[,i]<- transformacao_inversa(transformacao_var,comprimento_treino,preds)

            }
            ############################################# Erro absoluto medio do treino ao segmento ############################
            a<-0
            eam_segmento_treino[i]<-0
            for(k in anosTreino){
              a<- sum(abs(falhas_previstas_treino[which(falhas_reais_treino$Anos==k),i] - falhas_reais_treino$Falhas[which(falhas_reais_treino$Anos==k)]))
              a<-a/nrow(falhas_reais_treino[which(falhas_reais_treino$Anos==k),])
              eam_segmento_treino[i]<- eam_segmento_treino[i] + a
            }
            eam_segmento_treino[i]<-eam_segmento_treino[i]/length(anosTreino)
            eam_segmento_treino<-c(eam_segmento_treino,eam_segmento_treino[i])

            #################################################################################################################

            #Previs?o teste
            if(0==length(newv)){
              Aux2<-teste[,v[i], drop=FALSE]
            }else{
              Aux2<-teste[,c(newv,v[i])]
            }

            preds2<-predict(model,newdata=Aux2)

            if(0 == flag_log){
              falhas_previstas_teste[,i]<-preds2*comprimento_teste
            }
            else{ # Se flag_log==1 o teste e com logaritmo, logo no final este tem de ser retirado
              #falhas_previstas_teste[,i]<-(exp(preds2)-1)*comprimento_teste
              falhas_previstas_teste[,i]<-transformacao_inversa(transformacao_var,comprimento_teste,preds2)
            }

            ############################################# Erro ao segmento teste ############################################
            a<-0
            eam_segmento_teste[i]<-0
            for(k in anosTeste){
              a<-sum(abs(falhas_previstas_teste[which(falhas_reais_teste$Anos==k),i] - falhas_reais_teste$Falhas[which(falhas_reais_teste$Anos==k)]))
              a<-a/nrow(falhas_reais_teste[which(falhas_reais_teste$Anos==k),])
              eam_segmento_teste[i]<-eam_segmento_teste[i]+a
            }
            eam_segmento_teste[i]<-eam_segmento_teste[i]/length(anosTeste)
            eam_segmento_teste<-c(eam_segmento_teste,eam_segmento_teste[i])

          }
          #################################################################################################################
          # Guardar menor erro do treino ao nivel do segmento
          pmin<- which.min(eam_segmento_treino)
          eam_segmento_treino_final<-c(eam_segmento_treino_final,eam_segmento_treino[pmin])
          eam_segmento_teste_final<-c(eam_segmento_teste_final,eam_segmento_teste[pmin])

          newv<-c(newv,v[pmin]) #acrescentar variavel com a qual foi obtido o menor erro
          v<-v[-pmin] #eliminar variavel do vetor


          ######################################### Erro ao pais  #########################################################
          #Treino
          a<-0
          eam_pais_treino[j]<-0
          for(k in anosTreino){
            a<- abs(sum(falhas_previstas_treino[which(falhas_reais_treino$Anos==k),pmin])-sum(falhas_reais_treino$Falhas[which(falhas_reais_treino$Anos==k)]))
            eam_pais_treino[j]<-eam_pais_treino[j]+a
          }
          eam_pais_treino[j]<-eam_pais_treino[j]/length(anosTreino)
          eam_pais_treino<-c(eam_pais_treino,eam_pais_treino[j])

          #Teste
          a<-0
          eam_pais_teste[j]<-0
          for(k in anosTeste){
            a<- abs(sum(falhas_previstas_teste[which(falhas_reais_teste$Anos==k),pmin])-sum(falhas_reais_teste$Falhas[which(falhas_reais_teste$Anos==k)]))
            eam_pais_teste[j]<-eam_pais_teste[j]+a
          }
          eam_pais_teste[j]<- eam_pais_teste[j]/length(anosTeste)
          eam_pais_teste<-c(eam_pais_teste,eam_pais_teste[j])

          if("AT" != flag_tipoLinha){
            ######################################## Erro ao distrito #######################################################
            #Treino
            a<-0
            eam_distrito_treino[j]<-0
            for(k in anosTreino){
              for (z in ndtr) {
                auxiliar_real<-falhas_reais_treino[Distrito_treino==z,]
                auxiliar_previstas<-falhas_previstas_treino[Distrito_treino==z,pmin]
                pcodigo<-match(z,peso_distrito_treino[,1])
                a<-a + (peso_distrito_treino[pcodigo,2]*abs(sum(auxiliar_real$Falhas[which(auxiliar_real$Anos==k)])-sum(auxiliar_previstas[which(auxiliar_real$Anos==k)])))
              }
              eam_distrito_treino[j]<-eam_distrito_treino[j]+a
            }
            eam_distrito_treino[j]<-eam_distrito_treino[j]/length(anosTreino)
            eam_distrito_treino<-c(eam_distrito_treino, eam_distrito_treino[j])

            #Teste
            a<-0
            eam_distrito_teste[j]<-0
            for(k in anosTeste){
              for (z in ndt) {
                auxiliar_real<-falhas_reais_teste[Distrito_teste==z,]
                auxiliar_previstas<-falhas_previstas_teste[Distrito_teste==z,pmin]
                pcodigo<-match(z,peso_distrito_teste[,1])
                a<-a + (peso_distrito_teste[pcodigo,2]*abs(sum(auxiliar_real$Falhas[which(auxiliar_real$Anos==k)])-sum(auxiliar_previstas[which(auxiliar_real$Anos==k)])))
              }
              eam_distrito_teste[j]<-eam_distrito_teste[j]+a
            }
            eam_distrito_teste[j]<-eam_distrito_teste[j]/length(anosTeste)
            eam_distrito_teste<-c(eam_distrito_teste, eam_distrito_teste[j])

            ######################################### Erro ao concelho ######################################################
            #Treino
            a<-0
            eam_concelho_treino[j]<-0
            for(k in anosTreino){
              for (z in nctr) {
                auxiliar_real<-falhas_reais_treino[Concelho_treino==z,]
                auxiliar_previstas<-falhas_previstas_treino[Concelho_treino==z,pmin]
                pcodigo<-match(z,peso_concelho_treino[,1])
                a<-a + (peso_concelho_treino[pcodigo,2]*abs(sum(auxiliar_real$Falhas[which(auxiliar_real$Anos==k)])-sum(auxiliar_previstas[which(auxiliar_real$Anos==k)])))
              }
              eam_concelho_treino[j]<-eam_concelho_treino[j]+a
            }
            eam_concelho_treino[j]<-eam_concelho_treino[j]/length(anosTreino)
            eam_concelho_treino<-c(eam_concelho_treino, eam_concelho_treino[j])

            #Teste
            a<-0
            eam_concelho_teste[j]<-0
            for(k in anosTeste){
              for (z in nct) {
                auxiliar_real<-falhas_reais_teste[Concelho_teste==z,]
                auxiliar_previstas<-falhas_previstas_teste[Concelho_teste==z,pmin]
                pcodigo<-match(z,peso_concelho_teste[,1])
                a<-a + (peso_concelho_teste[pcodigo,2]*abs(sum(auxiliar_real$Falhas[which(auxiliar_real$Anos==k)])-sum(auxiliar_previstas[which(auxiliar_real$Anos==k)])))
              }
              eam_concelho_teste[j]<-eam_concelho_teste[j]+a
            }
            eam_concelho_teste[j]<-eam_concelho_teste[j]/length(anosTeste)
            eam_concelho_teste<-c(eam_concelho_teste, eam_concelho_teste[j])
          }
        }

        Variaveis_seleccionadas<-array(data=0, dim=length(newv)+1)
        Variaveis_seleccionadas[1]<-"Variaveis selecionadas"
        Variaveis_seleccionadas[2:length(Variaveis_seleccionadas)]<-newv

        eam_pais_teste<-eam_pais_teste[-niter]
        eam_pais_treino<-eam_pais_treino[-niter]
        eam_segmento_teste_final<-eam_segmento_teste_final
        eam_segmento_treino_final<-eam_segmento_treino_final

        if("AT" != flag_tipoLinha){
          eam_distrito_teste<-eam_distrito_teste[-niter]
          eam_distrito_treino<-eam_distrito_treino[-niter]
          eam_concelho_teste<-eam_concelho_teste[-niter]
          eam_concelho_treino<-eam_concelho_treino[-niter]

          Erros<-as.data.frame(cbind(eam_segmento_teste_final, eam_segmento_treino_final,
                                     eam_pais_teste, eam_pais_treino,
                                     eam_distrito_teste, eam_distrito_treino,
                                     eam_concelho_teste, eam_concelho_treino))
          colnames(Erros)<-c("Erro absoluto medio do teste ao nivel do segmento", "Erro absoluto medio do treino ao nivel do segmento",
                             "Erro absoluto medio do teste ao nivel do pais", "Erro absoluto medio do treino ao nivel do pais",
                             "Erro absoluto medio do teste ao nivel do distrito", "Erro absoluto medio do treino ao nivel do distrito",
                             "Erro absoluto medio do teste ao nivel do concelho", "Erro absoluto medio do treino ao nivel do concelho")
        }else{
          Erros<-as.data.frame(cbind(eam_segmento_teste_final, eam_segmento_treino_final,
                                     eam_pais_teste, eam_pais_treino))
          colnames(Erros)<-c("Erro absoluto medio do teste ao nivel do segmento", "Erro absoluto medio do treino ao nivel do segmento",
                             "Erro absoluto medio do teste ao nivel do pais", "Erro absoluto medio do treino ao nivel do pais")
        }


        if(1!=z){
          if(1 == flag_prazo){
            diretorio<-paste(path,"/Resultados/",flag_tipoLinha,"_CurtoPrazo/VariaveisSelecionadas_",paste0(tipoTeste,"_",log),".csv", sep="")
            #write.xlsx(Variaveis_seleccionadas[1:11],file = diretorio, col.names=FALSE, row.names=FALSE, append = TRUE, sheetName = paste0(tipoTeste,"_",log))
            write.csv(Variaveis_seleccionadas[1:11],file = diretorio,col.names = TRUE, row.names = FALSE)
            if(2 == flag_versao){
              diretorio<-paste(path,"/Resultados/",flag_tipoLinha,"_CurtoPrazo/Erros_",paste0(tipoTeste,"_",log),".csv", sep="")
              #write.xlsx(Erros,file = diretorio, col.names=TRUE, row.names=FALSE, append = TRUE, sheetName = paste0(tipoTeste,"_",log))
              write.csv(Erros,file = diretorio,col.names = TRUE, row.names = FALSE)
            }

          }else{
            diretorio<-paste(path,"/Resultados/",flag_tipoLinha,"_LongoPrazo/VariaveisSelecionadas_",log,".csv", sep="")
            #write.xlsx(Variaveis_seleccionadas,file = diretorio, col.names=FALSE, row.names=FALSE, append = TRUE, sheetName = log)
            write.csv(Variaveis_seleccionadas,file = diretorio, col.names = TRUE, row.names = FALSE)
            if(2 == flag_versao){
              diretorio<-paste(path,"/Resultados/",flag_tipoLinha,"_LongoPrazo/Erros_",log,".csv", sep="")
              #write.xlsx(Erros,file = diretorio, col.names=TRUE, row.names=FALSE, append = TRUE, sheetName = log)
              write.csv(Erros,file = diretorio)
            }
          }

        }else{
          if(1 == flag_prazo){
            diretorio<-paste(path,"/Resultados/",flag_tipoLinha,"_CurtoPrazo/VariaveisSelecionadas_",paste0(tipoTeste,"_",log),".csv", sep="")
            #write.xlsx(Variaveis_seleccionadas[1:11],file = diretorio, col.names=FALSE, row.names=FALSE, append = FALSE, sheetName = paste0(tipoTeste,"_",log))
            write.csv(Variaveis_seleccionadas[1:11],file = diretorio,col.names = TRUE, row.names = FALSE)
            if(2 == flag_versao){
              diretorio<-paste(path,"/Resultados/",flag_tipoLinha,"_CurtoPrazo/Erros_",paste0(tipoTeste,"_",log),".csv", sep="")
              #write.xlsx(Erros,file = diretorio, col.names=TRUE, row.names=FALSE, append = FALSE, sheetName = paste0(tipoTeste,"_",log))
              write.csv(Erros,file = diretorio, col.names = TRUE, row.names = FALSE)
            }

          }else{
            diretorio<-paste(path,"/Resultados/",flag_tipoLinha,"_LongoPrazo/VariaveisSelecionadas_",log,".csv", sep="")
            #write.xlsx(Variaveis_seleccionadas,file = diretorio_pessoal, col.names=FALSE, row.names=FALSE, append = FALSE, sheetName = log)
            write.csv(Variaveis_seleccionadas,file = diretorio, col.names = TRUE, row.names = FALSE)
            if(2 == flag_versao){
              diretorio<-paste(path,"/Resultados/",flag_tipoLinha,"_LongoPrazo/Erros_",log,".csv", sep="")
              #write.xlsx(Erros,file = diretorio_pessoal, col.names=TRUE, row.names=FALSE, append = FALSE, sheetName = log)
              write.csv(Erros,file = diretorio_pessoal)
            }

          }
        }
      }

    }

    #'Prever o numero de avarias para as linhas AT
    #'
    #'@description Esta funcão permite prever o número de avarias para o horizonte (anos) especificado,
    #'tendo por base as linhas AT guardadas na base de dados
    #'@usage prever_numero_avarias_AT(...,atualizar_modelo=c("nao","sim"),afinamento=c("nao","sim"))
    #'
    #'@param lista_dados Dados a utilizar na previsao das avarias para as Linhas AT
    #'@param atualizar_modelo Permite utilizar um modelo previamente gerado para a previsão de falhas. Caso contrário o modelo será atualizado com os dados disponiveis.
    #'@param afinamento Permite selecionar o nivel de afinamento desejado no modelo gerado. Ao ativar o afinamento o modelo irá demorar mais tempo a atualizar os resultados.
    #'@return Previsões do número de avarias para cada linha AT
    #'@examples #Ler os dados apartir de um dado ficheiro
    #'
    #'prever_numero_avarias_AT(Dados_AT,atualizar_modelo="sim",afinamento="nao")
    #'
    #'@export
    #Funcao para ler e prever as avarias no curto prazo para as linhas AT************************************************************************************************************************************************************
    prever_numero_avarias_AT<-function(lista_dados,atualizar_modelo="nao",afinamento="nao",transformacao_var){

      #path para o diretorio
      pack <- "iHEAD"
      path <- find.package(pack)

      #escolher se queremos a versao com log!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!
      #gerar modelos com/sem transformacao logaritmica da variavel a prever
      opcao_log<-c("sim","nao")

      #contador do progresso do calculo do iHEAD
      progresso<-0

      #Guardar as previsoes geradas caso exista o afinamento do modelo, como também os resultados
      #guardar modelos xgboost
      lista_modelo_xgboost<-list("SemLoc_ComLog_opcao_log_sim"=NA,"SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_sim"=NA,"SemLoc_SemLog_opcao_log_sim"=NA,"SemCausasCat_SemLog_opcao_log_sim"=NA,
                                 "SemLoc_ComLog_opcao_log_nao"=NA,"SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_nao"=NA,"SemLoc_SemLog_opcao_log_nao"=NA,"SemCausasCat_SemLog_opcao_log_nao"=NA)

      #guardar previsoes xgboost
      lista_previsoes_xgboost<-list("SemLoc_ComLog_opcao_log_sim"=NA,"SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_sim"=NA,"SemLoc_SemLog_opcao_log_sim"=NA,"SemCausasCat_SemLog_opcao_log_sim"=NA,
                                    "SemLoc_ComLog_opcao_log_nao"=NA,"SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_nao"=NA,"SemLoc_SemLog_opcao_log_nao"=NA,"SemCausasCat_SemLog_opcao_log_nao"=NA)

      #guardar o root mean squared error (RMSE)
      lista_erros<-c("SemLoc_ComLog_opcao_log_sim"=NA,"SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_sim"=NA,"SemLoc_SemLog_opcao_log_sim"=NA,"SemCausasCat_SemLog_opcao_log_sim"=NA,
                     "SemLoc_ComLog_opcao_log_nao"=NA,"SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_nao"=NA,"SemLoc_SemLog_opcao_log_nao"=NA,"SemCausasCat_SemLog_opcao_log_nao"=NA)

      #guardar a lista de variaveis selecionadas
      lista_variaveis<-c("SemLoc_ComLog_opcao_log_sim"=NA,"SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_sim"=NA,"SemLoc_SemLog_opcao_log_sim"=NA,"SemCausasCat_SemLog_opcao_log_sim"=NA,
                         "SemLoc_ComLog_opcao_log_nao"=NA,"SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_nao"=NA,"SemLoc_SemLog_opcao_log_nao"=NA,"SemCausasCat_SemLog_opcao_log_nao"=NA)


      if(atualizar_modelo!="nao"){
        for(opcao_log_aux in opcao_log){
          #Gerar modelo e validar modelo************************************************************************************************************************************************************

          #Preparar tipos de testes a realizar
          nome_lista<-c("SemLoc_ComLog","SemCausasCat_ComLog","SemLoc_SemCausasCat_ComLog","SemLoc_SemCausasCat_SemLog","SemLoc_SemLog","SemCausasCat_SemLog")
          Matriz_resultados<-matrix(data=NA,nrow = 6,ncol = length(nome_lista))
          colnames(Matriz_resultados)<-nome_lista
          rownames(Matriz_resultados)<-c("Erro absoluto Pais","Erro percentual absoluto Pais","Erro absoluto ponderado Distrito",
                                         "Erro percentual absoluto ponderado Distrito","SMAPE Distrito","RMSE")

          #Guardas as falhas na label que queremos analisar
          print(opcao_log_aux)
          if(opcao_log_aux=="sim"){
            #Falhas_treino<-log(lista_dados$treino$Falhas_Totais_porKm+1)
            #Falhas_teste<-log(lista_dados$teste$Falhas_Totais_porKm+1)
            Falhas_treino<-transformacao(transformacao_var,lista_dados$treino$Falhas_Totais_porKm)
            Falhas_teste<-transformacao(transformacao_var,lista_dados$teste$Falhas_Totais_porKm)
            lista_dados_aux<-lista_dados
          }else{
            lista_dados<-lista_dados_aux
            Falhas_treino<-lista_dados$treino$Falhas_Totais_porKm
            Falhas_teste<-lista_dados$teste$Falhas_Totais_porKm
          }

          #retirar variavel de resposta do treino e teste
          index_falhas<-grep("Falhas_Totais_porKm", colnames(lista_dados$treino))
          lista_dados$treino<-lista_dados$treino[,-index_falhas]
          lista_dados$teste<-lista_dados$teste[,-index_falhas]

          #Geral modelo e avaliar
          for(nome_lista_aux in nome_lista){

            #Atualizar o progresso do calculo
            progresso<-progresso+1
            print(paste("Progresso ",round(progresso*100/length(lista_erros),0),"%",sep = ""))

            #versao do modelo a realizar
            versao_modelo<-paste(nome_lista_aux,"_opcao_log_",opcao_log_aux,sep = "")

            #lista de variaveis que foram escolhidas para o modelo em questao
            lista_variaveis[[versao_modelo]]<-read_csv(paste(path,"/Resultados/AT_CurtoPrazo/VariaveisSelecionadas_",nome_lista_aux,".csv",sep = ""))
            lista_variaveis[[versao_modelo]]<-unlist(lista_variaveis[[versao_modelo]]$x[2:length(lista_variaveis[[versao_modelo]]$x)])

            #selecionar  a lista de variaveis a utilizar para os algoritmos
            Training<-lista_dados$treino[,lista_variaveis[[versao_modelo]]]
            teste<-lista_dados$teste[,lista_variaveis[[versao_modelo]]]

            ##selecionar o periodo que queremos analisar e testar
            #guardar variaveis teste
            Distrito_teste<-lista_dados$teste$Distrito
            Concelho_teste<-lista_dados$teste$Concelho
            comprimento_teste<-lista_dados$teste$Comprimento_total
            codigo_circuito_teste<-lista_dados$teste$Codigo.da.linha
            Anos_Historico_teste<-lista_dados$teste$Anos_Historico

            #guardar variaveis treino
            Distrito_treino<-lista_dados$treino$Distrito
            Concelho_treino<-lista_dados$treino$Concelho
            comprimento_treino<-lista_dados$treino$Comprimento_total
            codigo_circuito_treino<-lista_dados$treino$Codigo.da.linha
            Anos_Historico_treino<-lista_dados$treino$Anos_Historico

            ## Modelos a construir depois do processar os dados. Os modelos são avaliados através do RMSE
            ## Linear regression - Stepwise --> utilizado para avaliar o efeito das variaveis e o nivel de importancia
            modelo_lm<-lm(Falhas_treino ~., data = Training)

            #caso queiramos guardar no excel
            output<-data.frame("Variaveis"=modelo_lm$coefficients)
            write.csv(output,file = paste(path,"/Resultados/AT_CurtoPrazo/",nome_lista_aux,"_comLog_log_",opcao_log_aux,".csv",sep = ""))

            ## XGBOOST - Extreme gradient boosting.**
            if(afinamento=="nao"){
              xgb_model<-xgboost(data=as.matrix(Training),nfold=5,label=as.matrix(Falhas_treino),nrounds=2200,verbose=FALSE,objective='reg:linear',eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,max_depth=6,min_child_weight=1.7817,subsample=0.5213,colsample_bytree=0.4603)
            }else{
              #Melhoramento do xgboost
              train_control <- caret::trainControl(
                method = "none",
                verboseIter = FALSE,
                allowParallel = TRUE
              )

              #Numero de iteracoes e o learning rate
              nrounds <- 1000

              #Grid com os parametros a treinar
              tune_grid <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = c(0.025, 0.05, 0.1, 0.3),
                max_depth = c(2, 3, 4, 5, 6),
                gamma = 0,
                colsample_bytree = 1,
                min_child_weight = 1,
                subsample = 1
              )

              #Regras de avaliacao do erro na fase de treino
              tune_control <- caret::trainControl(
                method = "cv",
                number = 5,
                verboseIter = FALSE,
                allowParallel = TRUE
              )

              #Avaliar parametros
              xgb_tune <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid,
                method = "xgbTree",
                verbose = TRUE
              )

              #Grid com os parametros a treinar
              tune_grid2 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                                   c(xgb_tune$bestTune$max_depth:4),
                                   xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
                gamma = 0,
                colsample_bytree = 1,
                min_child_weight = c(1, 2, 3),
                subsample = 1
              )

              #Avaliar parametros
              xgb_tune2 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid2,
                method = "xgbTree",
                verbose = TRUE
              )

              #Column and Row Sampling
              #Grid com os parametros a treinar
              tune_grid3 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = 0,
                colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = c(0.5, 0.75, 1.0)
              )

              #Avaliar parametros
              xgb_tune3 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid3,
                method = "xgbTree",
                verbose = TRUE
              )

              #Gamma
              #Grid com os parametros a treinar
              tune_grid4 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
                colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = xgb_tune3$bestTune$subsample
              )

              #Avaliar parametros
              xgb_tune4 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid4,
                method = "xgbTree",
                verbose = TRUE
              )

              #Reduzir o learning rate
              #Grid com os parametros a treinar
              tune_grid5 <- expand.grid(
                nrounds = seq(from = 200, to = 10000, by = 200),
                eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = xgb_tune4$bestTune$gamma,
                colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = xgb_tune3$bestTune$subsample
              )

              #Avaliar parametros
              xgb_tune5 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid5,
                method = "xgbTree",
                verbose = TRUE
              )

              #Guardar os melhores parametros do modelo XGBOOST
              (final_grid <- expand.grid(
                nrounds = xgb_tune5$bestTune$nrounds,
                eta = xgb_tune5$bestTune$eta,
                max_depth = xgb_tune5$bestTune$max_depth,
                gamma = xgb_tune5$bestTune$gamma,
                colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
                min_child_weight = xgb_tune5$bestTune$min_child_weight,
                subsample = xgb_tune5$bestTune$subsample
              ))

              #Gerar o modelo com os melhores parametros
              (xgb_model <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = train_control,
                tuneGrid = final_grid,
                method = "xgbTree",
                verbose = TRUE
              ))

            }

            ##resultados para o dataset de teste+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            falhas_previstas <- predict(xgb_model,as.matrix(teste))

            if(opcao_log_aux=="sim"){
              falhas_previstas<- transformacao_inversa(transformacao_var,comprimento_teste,falhas_previstas)
              falhas_reais<- transformacao_inversa(transformacao_var,comprimento_teste,Falhas_teste)
              #falhas_previstas<-(exp(falhas_previstas)-1)*comprimento_teste
              #falhas_reais<-(exp(Falhas_teste)-1)*comprimento_teste
            }else{
              falhas_previstas<-falhas_previstas*comprimento_teste
              falhas_reais<-(Falhas_teste)*comprimento_teste
            }

            #guardar rmse
            lista_erros[versao_modelo]<-rmse(unlist(falhas_reais),falhas_previstas)

            #guardar previsoes apos processamento do log
            lista_previsoes_xgboost[[versao_modelo]] <- falhas_previstas

            #calcular erros ao nivel do pais
            EAM_pais_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            EPAM_pais_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            #indice utilizado para calcular o erro do ano
            l<-0

            #percorrer os anos de treino
            for(a in unique(Anos_Historico_teste)){
              l<-l+1
              EAM_pais_aux[l]<-abs(sum(falhas_reais[Anos_Historico_teste==a])-sum(falhas_previstas[Anos_Historico_teste==a]))
              EPAM_pais_aux[l]<-abs(sum(falhas_reais[Anos_Historico_teste==a])-sum(falhas_previstas[Anos_Historico_teste==a]))/sum(falhas_reais[Anos_Historico_teste==a])
            }
            EAM_pais<-mean(EAM_pais_aux)
            EPAM_pais<-mean(EPAM_pais_aux)

            #calcular erros ao nivel do Distrito
            #indice utilizado para calcular o erro do ano
            #renicializar o indice
            l<-0
            EAM_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            EPAM_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            SMAPE_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))

            #percorrer os anos de treino
            for(a in unique(Anos_Historico_teste)){

              pesos_distrito<-iHEAD::Pesos_AT_distrito
              Distrito_teste_aux<-pesos_distrito$Codigo_distrito

              #vetores auxiliares de calculo dos erros
              resultado_abs<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              resultado_abs_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              resultado_rel<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              falhas_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              smape_falhas_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              smape_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              #indice do distrito
              k<-0

              #calculo do peso total no dataset de forma a normalizar os pesos quando nao sao utilizados todos os distritos. Casos em que temos um distrito com falha 0.
              for(i in unique(Distrito_teste_aux)){
                k<-k+1
                falhas_aux[k]<-sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])
                smape_falhas_aux[k]<-sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])+sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a])
              }
              pesos_distrito$Peso_linha[which(falhas_aux==0)]<-0
              pesos_distrito$Peso_linha<-pesos_distrito$Peso_linha/sum(pesos_distrito$Peso_linha)
              l<-l+1

              #renicializar indice do distrito
              k<-0
              #calculo dos erros
              for (i in unique(Distrito_teste_aux)) {
                k<-k+1
                resultado_abs[k]<-(pesos_distrito$Peso_linha[pesos_distrito$Codigo_distrito==i])*abs(sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a]))
                resultado_abs_aux[k]<-abs(sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a]))
                smape_aux[k]<-(pesos_distrito$Peso_linha[pesos_distrito$Codigo_distrito==i])*2*resultado_abs_aux[k]/sum(smape_falhas_aux*pesos_distrito$Peso_linha)
              }
              resultado_rel<-resultado_abs/sum(falhas_aux*pesos_distrito$Peso_linha)
              EAM_Distrito_aux[l]<-sum(resultado_abs)
              EPAM_Distrito_aux[l]<-sum(resultado_rel[which(falhas_aux!=0)])

              #SMAPE
              SMAPE_Distrito_aux[l]<-sum(smape_aux[which(falhas_aux!=0)])
            }

            #resultado guardar distrito
            EAM_Distrito<-mean(EAM_Distrito_aux)
            EPAM_Distrito<-mean(EPAM_Distrito_aux)
            SMAPE_Distrito<-mean(SMAPE_Distrito_aux)

            #imprimir resultados para a matriz
            Matriz_resultados[1,nome_lista_aux]<-EAM_pais
            Matriz_resultados[2,nome_lista_aux]<-EPAM_pais
            Matriz_resultados[3,nome_lista_aux]<-EAM_Distrito
            Matriz_resultados[4,nome_lista_aux]<-EPAM_Distrito
            Matriz_resultados[5,nome_lista_aux]<-SMAPE_Distrito
            Matriz_resultados[6,nome_lista_aux]<-lista_erros[versao_modelo]

            #guardar o resultado das falhas previstas na lista de previsoes para cada modelo
            lista_modelo_xgboost[[versao_modelo]] <- xgb_model

            #Guardar no excel os resultados obtidos para os indicadores desenvolvidos
            write.csv(Matriz_resultados,file = paste(path,"/Resultados/AT_CurtoPrazo/Resutados_indicadores_opcao_log_",opcao_log_aux,".csv",sep = ""))
          }
        }

        #imprimir as previsões utilizando o melhor modelo
        nome_melhor_modelo<-names(lista_erros[which(lista_erros==min(lista_erros))])
        previsoes<-data.frame("Codigo Circuito"=codigo_circuito_teste,"Anos Historico"=Anos_Historico_teste,"Distrito"=Distrito_teste,"Concelho"=Concelho_teste,
                              "Comprimento Total"=comprimento_teste,"Falhas Reais"=falhas_reais,"Falhas Previstas"=lista_previsoes_xgboost[[nome_melhor_modelo]])

        #guardar melhor modelo do xgboost
        melhor_xgb_model<-lista_modelo_xgboost[[nome_melhor_modelo]]
        save(melhor_xgb_model,file=paste(path,"/Resultados/AT_CurtoPrazo/melhor_modelo_xgboost_AT.rda",sep = ""))

        #guardar definicao do melhor modelo
        save(nome_melhor_modelo,file=paste(path,"/Resultados/AT_CurtoPrazo/nome_melhor_modelo_xgboost_AT.rda",sep = ""))

        #guardar a lista de variaveis selecionadas no melhor modelo
        variaveis<-lista_variaveis[[nome_melhor_modelo]]
        save(variaveis,file=paste(path,"/Resultados/AT_CurtoPrazo/variaveis_melhor_modelo_xgboost_AT.rda",sep = ""))

      }else{

        #carregar modelo e fazer a previsao
        load(paste(path,"/Resultados/AT_CurtoPrazo/melhor_modelo_xgboost_AT.rda",sep = ""))

        #carregar definicao do log
        load(paste(path,"/Resultados/AT_CurtoPrazo/nome_melhor_modelo_xgboost_AT.rda",sep = ""))

        #carregar lista de variaveis
        load(paste(path,"/Resultados/AT_CurtoPrazo/variaveis_melhor_modelo_xgboost_AT.rda",sep = ""))

        #ler e processar dados
        #Guardas as falhas na label que queremos analisar
        if(retrieve_log(nome_melhor_modelo)=="sim"){
          Falhas_teste<-transformacao(transformacao_var,lista_dados$teste$Falhas_Totais_porKm)
          #Falhas_teste<-log(lista_dados$teste$Falhas_Totais_porKm+1)
        }else{
          Falhas_teste<-lista_dados$teste$Falhas_Totais_porKm
        }

        #retirar variavel de resposta do treino e teste
        index_falhas<-grep("Falhas_Totais_porKm", colnames(lista_dados$teste))
        lista_dados$teste<-lista_dados$teste[,-index_falhas]

        ##selecionar o periodo que queremos analisar e testar
        #guardar variaveis teste
        Distrito_teste<-lista_dados$teste$Distrito
        Concelho_teste<-lista_dados$teste$Concelho
        comprimento_teste<-lista_dados$teste$Comprimento_total
        codigo_circuito_teste<-lista_dados$teste$Codigo.da.linha
        Anos_Historico_teste<-lista_dados$teste$Anos_Historico

        #selecionar colunas de acordo com as variaveis
        teste<-lista_dados$teste[,variaveis]

        #prever o numero de falhas
        melhores_previsoes <- predict(melhor_xgb_model,as.matrix(teste))

        #converter para o formato correto
        if(retrieve_log(nome_melhor_modelo)=="sim"){
          melhores_previsoes<- transformacao_inversa(transformacao_var,comprimento_teste,melhores_previsoes)
          falhas_reais<- transformacao_inversa(transformacao_var,comprimento_teste,Falhas_teste)
          #melhores_previsoes<-(exp(melhores_previsoes)-1)*comprimento_teste
          #falhas_reais<-(exp(Falhas_teste)-1)*comprimento_teste
        }else{
          melhores_previsoes<-melhores_previsoes*comprimento_teste
          falhas_reais<-(Falhas_teste)*comprimento_teste
        }

        previsoes<-data.frame("Codigo Circuito"=codigo_circuito_teste,"Anos Historico"=Anos_Historico_teste,"Distrito"=Distrito_teste,"Concelho"=Concelho_teste,
                              "Comprimento Total"=comprimento_teste,"Falhas Reais"=falhas_reais,"Falhas Previstas"=melhores_previsoes)
      }

      return(previsoes)
    }

    #'Prever o numero de avarias para as linhas MT
    #'
    #'@description Esta funcão permite prever o número de avarias para o horizonte (anos) especificado,
    #'tendo por base as linhas MT guardadas na base de dados
    #'@usage prever_numero_avarias_MT(...,atualizar_modelo=c("nao","sim"),afinamento=c("nao","sim"),Tipo_linha_MT=c("Aereas","Subterraneas","Mistas"))
    #'
    #'@param lista_dados Dados a utilizar na previsao das avarias para as Linhas MT e classificacao desejada
    #'@param atualizar_modelo Permite utilizar um modelo previamente gerado para a previsão de falhas. Caso contrário o modelo será atualizado com os dados disponiveis.
    #'@param afinamento Permite selecionar o nivel de afinamento desejado no modelo gerado. Ao ativar o afinamento o modelo irá demorar mais tempo a atualizar os resultados.
    #'@param Tipo_linha_MT Permite selecionar qual o tipo de linha MT para a qual pretendemos prever o número de avarias (i.e. Aereas, Subterraneas ou Mistas).
    #'@return Previsões do número de avarias para cada linha MT, tendo por base a classificação solicitada.
    #'@examples #Ler os dados apartir de um dado ficheiro
    #'
    #'prever_numero_avarias_MT(Dados_MT,atualizar_modelo="sim",afinamento="nao",Tipo_linha_MT="Subterraneas")
    #'
    #'@export
    #Funcao para ler e prever as avarias no curto prazo para as linhas MT com a classificacao desejada************************************************************************************************************************************************************
    prever_numero_avarias_MT<-function(lista_dados,atualizar_modelo="nao",afinamento="nao",Tipo_linha_MT,transformacao_var){

      #path para o diretorio
      pack <- "iHEAD"
      path <- find.package(pack)

      #escolher se queremos a versao com log!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!
      #gerar modelos com/sem transformacao logaritmica da variavel a prever
      opcao_log<-c("sim","nao")

      #contador do progresso do calculo do iHEAD
      progresso<-0

      #Guardar as previsoes geradas caso exista o afinamento do modelo, como também os resultados
      #guardar modelos xgboost
      lista_modelo_xgboost<-list("SemLoc_ComLog_opcao_log_sim"=NA,"SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_sim"=NA,"SemLoc_SemLog_opcao_log_sim"=NA,"SemCausasCat_SemLog_opcao_log_sim"=NA,
                                 "SemLoc_ComLog_opcao_log_nao"=NA,"SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_nao"=NA,"SemLoc_SemLog_opcao_log_nao"=NA,"SemCausasCat_SemLog_opcao_log_nao"=NA)

      #guardar previsoes xgboost
      lista_previsoes_xgboost<-list("SemLoc_ComLog_opcao_log_sim"=NA,"SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_sim"=NA,"SemLoc_SemLog_opcao_log_sim"=NA,"SemCausasCat_SemLog_opcao_log_sim"=NA,
                                    "SemLoc_ComLog_opcao_log_nao"=NA,"SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_nao"=NA,"SemLoc_SemLog_opcao_log_nao"=NA,"SemCausasCat_SemLog_opcao_log_nao"=NA)

      #guardar o root mean squared error (RMSE)
      lista_erros<-c("SemLoc_ComLog_opcao_log_sim"=NA,"SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_sim"=NA,"SemLoc_SemLog_opcao_log_sim"=NA,"SemCausasCat_SemLog_opcao_log_sim"=NA,
                     "SemLoc_ComLog_opcao_log_nao"=NA,"SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_nao"=NA,"SemLoc_SemLog_opcao_log_nao"=NA,"SemCausasCat_SemLog_opcao_log_nao"=NA)

      #guardar a lista de variaveis selecionadas
      lista_variaveis<-c("SemLoc_ComLog_opcao_log_sim"=NA,"SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_sim"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_sim"=NA,"SemLoc_SemLog_opcao_log_sim"=NA,"SemCausasCat_SemLog_opcao_log_sim"=NA,
                         "SemLoc_ComLog_opcao_log_nao"=NA,"SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_ComLog_opcao_log_nao"=NA,"SemLoc_SemCausasCat_SemLog_opcao_log_nao"=NA,"SemLoc_SemLog_opcao_log_nao"=NA,"SemCausasCat_SemLog_opcao_log_nao"=NA)


      if(atualizar_modelo!="nao"){
        for(opcao_log_aux in opcao_log){
          #Gerar modelo e validar modelo************************************************************************************************************************************************************

          #Preparar tipos de testes a realizar
          nome_lista<-c("SemLoc_ComLog","SemCausasCat_ComLog","SemLoc_SemCausasCat_ComLog","SemLoc_SemCausasCat_SemLog","SemLoc_SemLog","SemCausasCat_SemLog")
          Matriz_resultados<-matrix(data=NA,nrow = 9,ncol = length(nome_lista))
          colnames(Matriz_resultados)<-nome_lista
          rownames(Matriz_resultados)<-c("Erro absoluto Pais","Erro percentual absoluto Pais","Erro absoluto ponderado Distrito","Erro percentual absoluto ponderado Distrito",
                                         "SMAPE Distrito","Erro absoluto ponderado Concelho","Erro percentual absoluto ponderado Concelho",
                                         "SMAPE Concelho","RMSE")

          #Guardas as falhas na label que queremos analisar
          if(opcao_log_aux=="sim"){
            #Falhas_treino<-log(lista_dados$treino$Falhas_Totais_porKm+1)
            #Falhas_teste<-log(lista_dados$teste$Falhas_Totais_porKm+1)
            Falhas_treino<-transformacao(transformacao_var,lista_dados$treino$Falhas_Totais_porKm)
            Falhas_teste<-transformacao(transformacao_var,lista_dados$teste$Falhas_Totais_porKm)
            lista_dados_aux<-lista_dados
          }else{
            lista_dados<-lista_dados_aux
            Falhas_treino<-lista_dados$treino$Falhas_Totais_porKm
            Falhas_teste<-lista_dados$teste$Falhas_Totais_porKm
          }

          #retirar variavel de resposta do treino e teste
          index_falhas<-grep("Falhas_Totais_porKm", colnames(lista_dados$treino))
          lista_dados$treino<-lista_dados$treino[,-index_falhas]
          lista_dados$teste<-lista_dados$teste[,-index_falhas]

          #Geral modelo e avaliar
          for(nome_lista_aux in nome_lista){

            #Atualizar o progresso do calculo
            progresso<-progresso+1
            print(paste("Progresso ",round(progresso*100/length(lista_erros),0),"%",sep = ""))

            #versao do modelo a realizar
            versao_modelo<-paste(nome_lista_aux,"_opcao_log_",opcao_log_aux,sep = "")

            #lista de variaveis que foram escolhidas para o modelo em questao
            #lista_variaveis[[versao_modelo]]<-read_csv('C:/Users/IOSRV174/Documents/R/win-library/3.6/iHEAD/Resultados/MT_Aereas_CurtoPrazo/VariaveisSelecionadas_SemLoc_ComLog.csv')
            lista_variaveis[[versao_modelo]]<-read_csv(paste(path,"/Resultados/MT_",Tipo_linha_MT,"_CurtoPrazo/VariaveisSelecionadas_",nome_lista_aux,".csv",sep = ""))
            lista_variaveis[[versao_modelo]]<-unlist(lista_variaveis[[versao_modelo]]$x[2:length(lista_variaveis[[versao_modelo]]$x)])

            #selecionar  a lista de variaveis a utilizar para os algoritmos
            Training<-lista_dados$treino[,lista_variaveis[[versao_modelo]]]
            teste<-lista_dados$teste[,lista_variaveis[[versao_modelo]]]

            ##selecionar o periodo que queremos analisar e testar
            #guardar variaveis teste
            Distrito_teste<-lista_dados$teste$Distrito
            Concelho_teste<-lista_dados$teste$Concelho
            comprimento_teste<-lista_dados$teste$Comprimento_Total
            codigo_circuito_teste<-lista_dados$teste$Codigo.Circuito
            Anos_Historico_teste<-lista_dados$teste$Anos_Historico

            #guardar variaveis treino
            Distrito_treino<-lista_dados$treino$Distrito
            Concelho_treino<-lista_dados$treino$Concelho
            comprimento_treino<-lista_dados$treino$Comprimento_Total
            codigo_circuito_treino<-lista_dados$treino$Codigo.Circuito
            Anos_Historico_treino<-lista_dados$treino$Anos_Historico

            ## Modelos a construir depois do processar os dados. Os modelos são avaliados através do RMSE
            ## Linear regression - Stepwise --> utilizado para avaliar o efeito das variaveis e o nivel de importancia
            modelo_lm<-lm(Falhas_treino ~., data = Training)

            #caso queiramos guardar no excel
            output<-data.frame("Variaveis"=modelo_lm$coefficients)
            write.csv(output,file = paste(path,"/Resultados/MT_",Tipo_linha_MT,"_CurtoPrazo/",nome_lista_aux,"_comLog_log_",opcao_log_aux,".csv",sep = ""))

            ## XGBOOST - Extreme gradient boosting.**
            if(afinamento=="nao"){
              xgb_model<-xgboost(data=as.matrix(Training),nfold=5,label=as.matrix(Falhas_treino),nrounds=2200,verbose=FALSE,objective='reg:linear',eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,max_depth=6,min_child_weight=1.7817,subsample=0.5213,colsample_bytree=0.4603)
            }else{
              #Melhoramento do xgboost
              train_control <- caret::trainControl(
                method = "none",
                verboseIter = FALSE,
                allowParallel = TRUE
              )

              #Numero de iteracoes e o learning rate
              nrounds <- 1000

              #Grid com os parametros a treinar
              tune_grid <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = c(0.025, 0.05, 0.1, 0.3),
                max_depth = c(2, 3, 4, 5, 6),
                gamma = 0,
                colsample_bytree = 1,
                min_child_weight = 1,
                subsample = 1
              )

              #Regras de avaliacao do erro na fase de treino
              tune_control <- caret::trainControl(
                method = "cv",
                number = 5,
                verboseIter = FALSE,
                allowParallel = TRUE
              )

              #Avaliar parametros
              xgb_tune <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid,
                method = "xgbTree",
                verbose = TRUE
              )

              #Grid com os parametros a treinar
              tune_grid2 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                                   c(xgb_tune$bestTune$max_depth:4),
                                   xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
                gamma = 0,
                colsample_bytree = 1,
                min_child_weight = c(1, 2, 3),
                subsample = 1
              )

              #Avaliar parametros
              xgb_tune2 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid2,
                method = "xgbTree",
                verbose = TRUE
              )

              #Column and Row Sampling
              #Grid com os parametros a treinar
              tune_grid3 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = 0,
                colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = c(0.5, 0.75, 1.0)
              )

              #Avaliar parametros
              xgb_tune3 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid3,
                method = "xgbTree",
                verbose = TRUE
              )

              #Gamma
              #Grid com os parametros a treinar
              tune_grid4 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
                colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = xgb_tune3$bestTune$subsample
              )

              #Avaliar parametros
              xgb_tune4 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid4,
                method = "xgbTree",
                verbose = TRUE
              )

              #Reduzir o learning rate
              #Grid com os parametros a treinar
              tune_grid5 <- expand.grid(
                nrounds = seq(from = 200, to = 10000, by = 200),
                eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = xgb_tune4$bestTune$gamma,
                colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = xgb_tune3$bestTune$subsample
              )

              #Avaliar parametros
              xgb_tune5 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid5,
                method = "xgbTree",
                verbose = TRUE
              )

              #Guardar os melhores parametros do modelo XGBOOST
              (final_grid <- expand.grid(
                nrounds = xgb_tune5$bestTune$nrounds,
                eta = xgb_tune5$bestTune$eta,
                max_depth = xgb_tune5$bestTune$max_depth,
                gamma = xgb_tune5$bestTune$gamma,
                colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
                min_child_weight = xgb_tune5$bestTune$min_child_weight,
                subsample = xgb_tune5$bestTune$subsample
              ))

              #Gerar o modelo com os melhores parametros
              (xgb_model <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = train_control,
                tuneGrid = final_grid,
                method = "xgbTree",
                verbose = TRUE
              ))

            }

            ##resultados para o dataset de teste+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            falhas_previstas <- predict(xgb_model,as.matrix(teste))

            if(opcao_log_aux=="sim"){
              falhas_previstas<- transformacao_inversa(transformacao_var,comprimento_teste,falhas_previstas)
              falhas_reais<- transformacao_inversa(transformacao_var,comprimento_teste,Falhas_teste)
              #falhas_previstas<-(exp(falhas_previstas)-1)*comprimento_teste
              #falhas_reais<-(exp(Falhas_teste)-1)*comprimento_teste
            }else{
              falhas_previstas<-falhas_previstas*comprimento_teste
              falhas_reais<-(Falhas_teste)*comprimento_teste
            }

            #guardar rmse
            lista_erros[versao_modelo]<-rmse(unlist(falhas_reais),falhas_previstas)

            #guardar previsoes apos processamento do log
            lista_previsoes_xgboost[[versao_modelo]] <- falhas_previstas

            #calcular erros ao nivel do pais
            EAM_pais_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            EPAM_pais_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            #indice utilizado para calcular o erro do ano
            l<-0

            #percorrer os anos de treino
            for(a in unique(Anos_Historico_teste)){
              l<-l+1
              EAM_pais_aux[l]<-abs(sum(falhas_reais[Anos_Historico_teste==a])-sum(falhas_previstas[Anos_Historico_teste==a]))
              EPAM_pais_aux[l]<-abs(sum(falhas_reais[Anos_Historico_teste==a])-sum(falhas_previstas[Anos_Historico_teste==a]))/sum(falhas_reais[Anos_Historico_teste==a])
            }
            EAM_pais<-mean(EAM_pais_aux)
            EPAM_pais<-mean(EPAM_pais_aux)

            #calcular erros ao nivel do Distrito
            #indice utilizado para calcular o erro do ano
            #renicializar o indice
            l<-0
            EAM_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            EPAM_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            SMAPE_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))

            #calcular erros ao nivel do distrito
            for(a in unique(Anos_Historico_teste)){

              #pesos para cada distrito calculado com base na area
              pesos_distrito<-iHEAD::Pesos_MT_distrito

              Distrito_teste_aux<-pesos_distrito$Codigo_distrito

              #vetores auxiliares de calculo dos erros
              resultado_abs<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              resultado_abs_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              resultado_rel<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              falhas_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              smape_falhas_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              smape_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              #indice do distrito
              k<-0

              #calculo do peso total no dataset de forma a normalizar os pesos quando nao sao utilizados todos os distritos. Casos em que temos um distrito com falha 0.
              for(i in unique(Distrito_teste_aux)){
                k<-k+1
                falhas_aux[k]<-sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])
                smape_falhas_aux[k]<-sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])+sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a])
              }
              pesos_distrito$Peso_linha[which(falhas_aux==0)]<-0
              pesos_distrito$Peso_linha<-pesos_distrito$Peso_linha/sum(pesos_distrito$Peso_linha)
              l<-l+1

              #renicializar indice do distrito
              k<-0
              #calculo dos erros
              for (i in unique(Distrito_teste_aux)) {
                k<-k+1
                resultado_abs[k]<-(pesos_distrito$Peso_linha[pesos_distrito$Codigo_distrito==i])*abs(sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a]))
                resultado_abs_aux[k]<-abs(sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a]))
                smape_aux[k]<-(pesos_distrito$Peso_linha[pesos_distrito$Codigo_distrito==i])*2*resultado_abs_aux[k]/sum(smape_falhas_aux*pesos_distrito$Peso_linha)
              }
              resultado_rel<-resultado_abs/sum(falhas_aux*pesos_distrito$Peso_linha)
              EAM_Distrito_aux[l]<-sum(resultado_abs)
              EPAM_Distrito_aux[l]<-sum(resultado_rel[which(falhas_aux!=0)])

              #SMAPE
              SMAPE_Distrito_aux[l]<-sum(smape_aux[which(falhas_aux!=0)])
            }

            #resultado guardar distrito
            EAM_Distrito<-mean(EAM_Distrito_aux)
            EPAM_Distrito<-mean(EPAM_Distrito_aux)
            SMAPE_Distrito<-mean(SMAPE_Distrito_aux)

            #calcular erros ao nivel do concelho
            #indice utilizado para calcular o erro do ano
            #renicializar o indice

            EAM_Concelho_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            EPAM_Concelho_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            SMAPE_Concelho_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            l<-0

            for(a in unique(Anos_Historico_teste)){
              #pesos dos concelhos
              pesos_concelhos<-iHEAD::Pesos_MT_concelho
              Concelho_teste_aux<-pesos_concelhos$`Codigo concelho do teste`

              #vetores auxiliares
              resultado_abs<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              resultado_abs_aux<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              resultado_abs_falhas_0<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              resultado_rel<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              falhas_aux<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              smape_falhas_aux<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              smape_aux<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              j<-0

              #calculo do peso total no dataset de forma a normalizar os pesos quando nao sao utilizados todos os distritos
              for(i in unique(Concelho_teste_aux)){
                j<-j+1
                falhas_aux[j]<-sum(falhas_reais[Concelho_teste==i & Anos_Historico_teste==a])
                smape_falhas_aux[j]<-sum(falhas_reais[Concelho_teste==i & Anos_Historico_teste==a])+sum(falhas_previstas[Concelho_teste==i & Anos_Historico_teste==a])
              }

              pesos_concelhos$Peso_linha[which(falhas_aux==0)]<-0
              pesos_concelhos$Peso_linha<-pesos_concelhos$Peso_linha/sum(pesos_concelhos$Peso_linha)
              l<-l+1
              j<-0
              for (i in unique(Concelho_teste_aux)) {
                j<-j+1
                resultado_abs[j]<-(pesos_concelhos$Peso_linha[pesos_concelhos$`Codigo concelho do teste`==i])*abs(sum(falhas_reais[Concelho_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Concelho_teste==i & Anos_Historico_teste==a]))
                resultado_abs_aux[j]<-abs(sum(falhas_reais[Concelho_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Concelho_teste==i & Anos_Historico_teste==a]))
                smape_aux[j]<-(pesos_concelhos$Peso_linha[pesos_concelhos$`Codigo concelho do teste`==i])*2*resultado_abs_aux[j]/sum(smape_falhas_aux*pesos_concelhos$Peso_linha)
              }
              resultado_rel<-resultado_abs/sum(falhas_aux*pesos_concelhos$Peso_linha)

              EAM_Concelho_aux[l]<-sum(resultado_abs)
              EPAM_Concelho_aux[l]<-sum(resultado_rel[which(falhas_aux!=0)])

              #smape
              SMAPE_Concelho_aux[l]<-sum(smape_aux[which(falhas_aux!=0)])
            }

            #resultado guardar concelho
            EAM_Concelho<-mean(EAM_Concelho_aux)
            EPAM_Concelho<-mean(EPAM_Concelho_aux)
            SMAPE_Concelho<-mean(SMAPE_Concelho_aux)

            #imprimir resultados para a matriz
            Matriz_resultados[1,nome_lista_aux]<-EAM_pais
            Matriz_resultados[2,nome_lista_aux]<-EPAM_pais
            Matriz_resultados[3,nome_lista_aux]<-EAM_Distrito
            Matriz_resultados[4,nome_lista_aux]<-EPAM_Distrito
            Matriz_resultados[5,nome_lista_aux]<-SMAPE_Distrito
            Matriz_resultados[6,nome_lista_aux]<-EAM_Concelho
            Matriz_resultados[7,nome_lista_aux]<-EPAM_Concelho
            Matriz_resultados[8,nome_lista_aux]<-SMAPE_Concelho
            Matriz_resultados[9,nome_lista_aux]<-lista_erros[versao_modelo]

            #guardar o resultado das falhas previstas na lista de previsoes para cada modelo
            lista_modelo_xgboost[[versao_modelo]] <- xgb_model

            #Guardar no excel os resultados obtidos para os indicadores desenvolvidos
            write.csv(Matriz_resultados,file = paste(path,"/Resultados/MT_",Tipo_linha_MT,"_CurtoPrazo/Resutados_indicadores_opcao_log_",opcao_log_aux,".csv",sep = ""))
          }
        }

        #imprimir as previsões utilizando o melhor modelo
        nome_melhor_modelo<-names(lista_erros[which(lista_erros==min(lista_erros))])
        previsoes<-data.frame("Codigo Circuito"=codigo_circuito_teste,"Anos Historico"=Anos_Historico_teste,"Distrito"=Distrito_teste,"Concelho"=Concelho_teste,
                              "Comprimento Total"=comprimento_teste,"Falhas Reais"=falhas_reais,"Falhas Previstas"=lista_previsoes_xgboost[[nome_melhor_modelo]])

        #guardar melhor modelo do xgboost
        melhor_xgb_model<-lista_modelo_xgboost[[nome_melhor_modelo]]
        save(melhor_xgb_model,file=paste(path,"/Resultados/MT_",Tipo_linha_MT,"_CurtoPrazo/melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

        #guardar definicao do melhor modelo
        save(nome_melhor_modelo,file=paste(path,"/Resultados/MT_",Tipo_linha_MT,"_CurtoPrazo/nome_melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

        #guardar a lista de variaveis selecionadas no melhor modelo
        variaveis<-lista_variaveis[[nome_melhor_modelo]]
        save(variaveis,file=paste(path,"/Resultados/MT_",Tipo_linha_MT,"_CurtoPrazo/variaveis_melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

      }else{

        #carregar modelo e fazer a previsao
        load(paste(path,"/Resultados/MT_",Tipo_linha_MT,"_CurtoPrazo/melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

        #carregar definicao do log
        load(paste(path,"/Resultados/MT_",Tipo_linha_MT,"_CurtoPrazo/nome_melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

        #carregar lista de variaveis
        load(paste(path,"/Resultados/MT_",Tipo_linha_MT,"_CurtoPrazo/variaveis_melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

        #ler e processar dados
        #Guardas as falhas na label que queremos analisar
        if(retrieve_log(nome_melhor_modelo)=="sim"){
          Falhas_teste<-log(lista_dados$teste$Falhas_Totais_porKm+1)
        }else{
          Falhas_teste<-lista_dados$teste$Falhas_Totais_porKm
        }

        #retirar variavel de resposta do treino e teste
        index_falhas<-grep("Falhas_Totais_porKm", colnames(lista_dados$teste))
        lista_dados$teste<-lista_dados$teste[,-index_falhas]

        ##selecionar o periodo que queremos analisar e testar
        #guardar variaveis teste
        Distrito_teste<-lista_dados$Distrito_teste
        Concelho_teste<-lista_dados$Concelho_teste
        comprimento_teste<-lista_dados$teste$Comprimento_total
        codigo_circuito_teste<-lista_dados$teste$Codigo.Circuito
        Anos_Historico_teste<-lista_dados$teste$Anos_Historico

        #selecionar colunas de acordo com as variaveis
        teste<-lista_dados$teste[,variaveis]

        #prever o numero de falhas
        melhores_previsoes <- predict(melhor_xgb_model,as.matrix(teste))

        #converter para o formato correto
        if(retrieve_log(nome_melhor_modelo)=="sim"){
          melhores_previsoes<-(exp(melhores_previsoes)-1)*comprimento_teste
          falhas_reais<-(exp(Falhas_teste)-1)*comprimento_teste
        }else{
          melhores_previsoes<-melhores_previsoes*comprimento_teste
          falhas_reais<-(Falhas_teste)*comprimento_teste
        }

        previsoes<-data.frame("Codigo Circuito"=codigo_circuito_teste,"Anos Historico"=Anos_Historico_teste,"Distrito"=Distrito_teste,"Concelho"=Concelho_teste,
                              "Comprimento Total"=comprimento_teste,"Falhas Reais"=falhas_reais,"Falhas Previstas"=melhores_previsoes)
      }

      return(previsoes)
    }

    #'Estimar probabilidade de avaria para as linhas AT
    #'
    #'@description Esta funcão permite estimar a probabilidade de falha,
    #'tendo por base as linhas AT guardadas na base de dados
    #'@usage probabilidade_avaria_AT(...,atualizar_modelo=c("nao","sim"),afinamento=c("nao","sim"))
    #'
    #'@param lista_dados Dados a utilizar no calculo da probabilidade de avaria para as Linhas AT
    #'@param atualizar_modelo Permite utilizar um modelo previamente gerado parao calculo da probabilidade de falha. Caso contrário o modelo será atualizado com os dados disponiveis.
    #'@param afinamento Permite selecionar o nivel de afinamento desejado no modelo gerado. Ao ativar o afinamento o modelo irá demorar mais tempo a atualizar os resultados.
    #'@return Calculo da probabilidade de falha para cada linha AT
    #'@examples #Ler os dados apartir de um dado ficheiro
    #'
    #'probabilidade_avaria_AT(Dados_AT,atualizar_modelo="sim",afinamento="nao")
    #'
    #'@export
    #Funcao para ler e calcular a probabilidade de avaria para as linhas AT************************************************************************************************************************************************************
    probabilidade_avaria_AT<-function(lista_dados,atualizar_modelo="nao",afinamento="nao"){

      #path para o diretorio
      pack <- "iHEAD"
      path <- find.package(pack)

      #escolher se queremos a versao com log!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!
      #gerar modelos com/sem transformacao logaritmica da variavel a prever
      opcao_log<-c("sim","nao")

      #contador do progresso do calculo do iHEAD
      progresso<-0

      #Guardar as previsoes geradas caso exista o afinamento do modelo, como também os resultados
      #guardar modelos xgboost
      lista_modelo_xgboost<-list("ComLog_opcao_log_sim"=NA,"SemLog_opcao_log_sim"=NA,
                                 "ComLog_opcao_log_nao"=NA,"SemLog_opcao_log_nao"=NA)

      #guardar previsoes xgboost
      lista_previsoes_xgboost<-list("ComLog_opcao_log_sim"=NA,"SemLog_opcao_log_sim"=NA,
                                    "ComLog_opcao_log_nao"=NA,"SemLog_opcao_log_nao"=NA)

      #guardar o root mean squared error (RMSE)
      lista_erros<-c("ComLog_opcao_log_sim"=NA,"SemLog_opcao_log_sim"=NA,
                     "ComLog_opcao_log_nao"=NA,"SemLog_opcao_log_nao"=NA)

      #guardar a lista de variaveis selecionadas
      lista_variaveis<-c("ComLog_opcao_log_sim"=NA,"SemLog_opcao_log_sim"=NA,
                         "ComLog_opcao_log_nao"=NA,"SemLog_opcao_log_nao"=NA)


      if(atualizar_modelo!="nao"){
        for(opcao_log_aux in opcao_log){
          #Gerar modelo e validar modelo************************************************************************************************************************************************************

          #Preparar tipos de testes a realizar
          nome_lista<-c("ComLog","SemLog")
          Matriz_resultados<-matrix(data=NA,nrow = 6,ncol = length(nome_lista))
          colnames(Matriz_resultados)<-nome_lista
          rownames(Matriz_resultados)<-c("Erro absoluto Pais","Erro percentual absoluto Pais","Erro absoluto ponderado Distrito",
                                         "Erro percentual absoluto ponderado Distrito","SMAPE Distrito","RMSE")

          #Guardas as falhas na label que queremos analisar
          if(opcao_log_aux=="sim"){
            Falhas_treino<-log(lista_dados$treino$Falhas_Totais_porKm+1)
            Falhas_teste<-log(lista_dados$teste$Falhas_Totais_porKm+1)
            lista_dados_aux<-lista_dados
          }else{
            lista_dados<-lista_dados_aux
            Falhas_treino<-lista_dados$treino$Falhas_Totais_porKm
            Falhas_teste<-lista_dados$teste$Falhas_Totais_porKm
          }

          #retirar variavel de resposta do treino e teste
          index_falhas<-grep("Falhas_Totais_porKm", colnames(lista_dados$treino))
          lista_dados$treino<-lista_dados$treino[,-index_falhas]
          lista_dados$teste<-lista_dados$teste[,-index_falhas]

          #Geral modelo e avaliar
          for(nome_lista_aux in nome_lista){

            #Atualizar o progresso do calculo
            progresso<-progresso+1
            print(paste("Progresso ",round(progresso*100/length(lista_erros),0),"%",sep = ""))

            #versao do modelo a realizar
            versao_modelo<-paste(nome_lista_aux,"_opcao_log_",opcao_log_aux,sep = "")

            #lista de variaveis que foram escolhidas para o modelo em questao
            lista_variaveis[[versao_modelo]]<-read_csv(paste(path,"/Resultados/AT_LongoPrazo/VariaveisSelecionadas_",nome_lista_aux,".csv",sep = ""))
            lista_variaveis[[versao_modelo]]<-unlist(lista_variaveis[[versao_modelo]]$x[2:length(lista_variaveis[[versao_modelo]]$x)])

            #selecionar  a lista de variaveis a utilizar para os algoritmos
            Training<-lista_dados$treino[,lista_variaveis[[versao_modelo]]]
            teste<-lista_dados$teste[,lista_variaveis[[versao_modelo]]]

            ##selecionar o periodo que queremos analisar e testar
            #guardar variaveis teste
            Distrito_teste<-lista_dados$teste$Distrito
            Concelho_teste<-lista_dados$teste$Concelho
            comprimento_teste<-lista_dados$teste$Comprimento_total
            codigo_circuito_teste<-lista_dados$teste$Codigo.da.linha
            Anos_Historico_teste<-lista_dados$teste$Anos_Historico

            #guardar variaveis treino
            Distrito_treino<-lista_dados$treino$Distrito
            Concelho_treino<-lista_dados$treino$Concelho
            comprimento_treino<-lista_dados$treino$Comprimento_total
            codigo_circuito_treino<-lista_dados$treino$Codigo.da.linha
            Anos_Historico_treino<-lista_dados$treino$Anos_Historico

            ## Modelos a construir depois do processar os dados. Os modelos são avaliados através do RMSE
            ## Linear regression - Stepwise --> utilizado para avaliar o efeito das variaveis e o nivel de importancia
            modelo_lm<-lm(Falhas_treino ~., data = Training)

            #caso queiramos guardar no excel
            output<-data.frame("Variaveis"=modelo_lm$coefficients)
            write.csv(output,file = paste(path,"/Resultados/AT_LongoPrazo/",nome_lista_aux,"_comLog_log_",opcao_log_aux,".csv",sep = ""))

            ## XGBOOST - Extreme gradient boosting.**
            if(afinamento=="nao"){
              xgb_model<-xgboost(data=as.matrix(Training),nfold=5,label=as.matrix(Falhas_treino),nrounds=2200,verbose=FALSE,objective='reg:linear',eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,max_depth=6,min_child_weight=1.7817,subsample=0.5213,colsample_bytree=0.4603)
            }else{
              #Melhoramento do xgboost
              train_control <- caret::trainControl(
                method = "none",
                verboseIter = FALSE,
                allowParallel = TRUE
              )

              #Numero de iteracoes e o learning rate
              nrounds <- 1000

              #Grid com os parametros a treinar
              tune_grid <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = c(0.025, 0.05, 0.1, 0.3),
                max_depth = c(2, 3, 4, 5, 6),
                gamma = 0,
                colsample_bytree = 1,
                min_child_weight = 1,
                subsample = 1
              )

              #Regras de avaliacao do erro na fase de treino
              tune_control <- caret::trainControl(
                method = "cv",
                number = 5,
                verboseIter = FALSE,
                allowParallel = TRUE
              )

              #Avaliar parametros
              xgb_tune <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid,
                method = "xgbTree",
                verbose = TRUE
              )

              #Grid com os parametros a treinar
              tune_grid2 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                                   c(xgb_tune$bestTune$max_depth:4),
                                   xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
                gamma = 0,
                colsample_bytree = 1,
                min_child_weight = c(1, 2, 3),
                subsample = 1
              )

              #Avaliar parametros
              xgb_tune2 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid2,
                method = "xgbTree",
                verbose = TRUE
              )

              #Column and Row Sampling
              #Grid com os parametros a treinar
              tune_grid3 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = 0,
                colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = c(0.5, 0.75, 1.0)
              )

              #Avaliar parametros
              xgb_tune3 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid3,
                method = "xgbTree",
                verbose = TRUE
              )

              #Gamma
              #Grid com os parametros a treinar
              tune_grid4 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
                colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = xgb_tune3$bestTune$subsample
              )

              #Avaliar parametros
              xgb_tune4 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid4,
                method = "xgbTree",
                verbose = TRUE
              )

              #Reduzir o learning rate
              #Grid com os parametros a treinar
              tune_grid5 <- expand.grid(
                nrounds = seq(from = 200, to = 10000, by = 200),
                eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = xgb_tune4$bestTune$gamma,
                colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = xgb_tune3$bestTune$subsample
              )

              #Avaliar parametros
              xgb_tune5 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid5,
                method = "xgbTree",
                verbose = TRUE
              )

              #Guardar os melhores parametros do modelo XGBOOST
              (final_grid <- expand.grid(
                nrounds = xgb_tune5$bestTune$nrounds,
                eta = xgb_tune5$bestTune$eta,
                max_depth = xgb_tune5$bestTune$max_depth,
                gamma = xgb_tune5$bestTune$gamma,
                colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
                min_child_weight = xgb_tune5$bestTune$min_child_weight,
                subsample = xgb_tune5$bestTune$subsample
              ))

              #Gerar o modelo com os melhores parametros
              (xgb_model <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = train_control,
                tuneGrid = final_grid,
                method = "xgbTree",
                verbose = TRUE
              ))

            }

            ##resultados para o dataset de teste+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            falhas_previstas <- predict(xgb_model,as.matrix(teste))

            if(opcao_log_aux=="sim"){
              falhas_previstas<-(exp(falhas_previstas)-1)*comprimento_teste
              falhas_reais<-(exp(Falhas_teste)-1)*comprimento_teste
            }else{
              falhas_previstas<-falhas_previstas*comprimento_teste
              falhas_reais<-(Falhas_teste)*comprimento_teste
            }

            #guardar rmse
            lista_erros[versao_modelo]<-rmse(unlist(falhas_reais),falhas_previstas)

            #guardar previsoes apos processamento do log
            lista_previsoes_xgboost[[versao_modelo]] <- falhas_previstas

            #calcular erros ao nivel do pais
            EAM_pais_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            EPAM_pais_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            #indice utilizado para calcular o erro do ano
            l<-0

            #percorrer os anos de treino
            for(a in unique(Anos_Historico_teste)){
              l<-l+1
              EAM_pais_aux[l]<-abs(sum(falhas_reais[Anos_Historico_teste==a])-sum(falhas_previstas[Anos_Historico_teste==a]))
              EPAM_pais_aux[l]<-abs(sum(falhas_reais[Anos_Historico_teste==a])-sum(falhas_previstas[Anos_Historico_teste==a]))/sum(falhas_reais[Anos_Historico_teste==a])
            }
            EAM_pais<-mean(EAM_pais_aux)
            EPAM_pais<-mean(EPAM_pais_aux)

            #calcular erros ao nivel do Distrito
            #indice utilizado para calcular o erro do ano
            #renicializar o indice
            l<-0
            EAM_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            EPAM_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            SMAPE_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))

            #percorrer os anos de treino
            for(a in unique(Anos_Historico_teste)){

              #pesos para cada distrito calculado com base na area
              pesos_distrito<-iHEAD::Pesos_AT_distrito
              Distrito_teste_aux<-pesos_distrito$Codigo_distrito

              #vetores auxiliares de calculo dos erros
              resultado_abs<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              resultado_abs_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              resultado_rel<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              falhas_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              smape_falhas_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              smape_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              #indice do distrito
              k<-0

              #calculo do peso total no dataset de forma a normalizar os pesos quando nao sao utilizados todos os distritos. Casos em que temos um distrito com falha 0.
              for(i in unique(Distrito_teste_aux)){
                k<-k+1
                falhas_aux[k]<-sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])
                smape_falhas_aux[k]<-sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])+sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a])
              }
              pesos_distrito$Peso_linha[which(falhas_aux==0)]<-0
              pesos_distrito$Peso_linha<-pesos_distrito$Peso_linha/sum(pesos_distrito$Peso_linha)
              l<-l+1

              #renicializar indice do distrito
              k<-0
              #calculo dos erros
              for (i in unique(Distrito_teste_aux)) {
                k<-k+1
                resultado_abs[k]<-(pesos_distrito$Peso_linha[pesos_distrito$Codigo_distrito==i])*abs(sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a]))
                resultado_abs_aux[k]<-abs(sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a]))
                smape_aux[k]<-(pesos_distrito$Peso_linha[pesos_distrito$Codigo_distrito==i])*2*resultado_abs_aux[k]/sum(smape_falhas_aux*pesos_distrito$Peso_linha)
              }
              resultado_rel<-resultado_abs/sum(falhas_aux*pesos_distrito$Peso_linha)
              EAM_Distrito_aux[l]<-sum(resultado_abs)
              EPAM_Distrito_aux[l]<-sum(resultado_rel[which(falhas_aux!=0)])

              #SMAPE
              SMAPE_Distrito_aux[l]<-sum(smape_aux[which(falhas_aux!=0)])
            }

            #resultado guardar distrito
            EAM_Distrito<-mean(EAM_Distrito_aux)
            EPAM_Distrito<-mean(EPAM_Distrito_aux)
            SMAPE_Distrito<-mean(SMAPE_Distrito_aux)

            #imprimir resultados para a matriz
            Matriz_resultados[1,nome_lista_aux]<-EAM_pais
            Matriz_resultados[2,nome_lista_aux]<-EPAM_pais
            Matriz_resultados[3,nome_lista_aux]<-EAM_Distrito
            Matriz_resultados[4,nome_lista_aux]<-EPAM_Distrito
            Matriz_resultados[5,nome_lista_aux]<-SMAPE_Distrito
            Matriz_resultados[6,nome_lista_aux]<-lista_erros[versao_modelo]

            #guardar o resultado das falhas previstas na lista de previsoes para cada modelo
            lista_modelo_xgboost[[versao_modelo]] <- xgb_model

            #Guardar no excel os resultados obtidos para os indicadores desenvolvidos
            write.csv(Matriz_resultados,file = paste(path,"/Resultados/AT_LongoPrazo/Resutados_indicadores_opcao_log_",opcao_log_aux,".csv",sep = ""))
          }
        }

        #imprimir as previsões utilizando o melhor modelo
        nome_melhor_modelo<-names(lista_erros[which(lista_erros==min(lista_erros))])
        previsoes<-data.frame("Codigo Circuito"=codigo_circuito_teste,"Anos Historico"=Anos_Historico_teste,"Distrito"=Distrito_teste,"Concelho"=Concelho_teste,
                              "Comprimento Total"=comprimento_teste,"Falhas Reais"=falhas_reais,"Taxa Falhas por km"=lista_previsoes_xgboost[[nome_melhor_modelo]]/comprimento_teste,
                              "Probabilidade de falha"=(1-ppois(0,lista_previsoes_xgboost[[nome_melhor_modelo]]/comprimento_teste)))

        #guardar melhor modelo do xgboost
        melhor_xgb_model<-lista_modelo_xgboost[[nome_melhor_modelo]]
        save(melhor_xgb_model,file=paste(path,"/Resultados/AT_LongoPrazo/melhor_modelo_xgboost_AT.rda",sep = ""))

        #guardar definicao do melhor modelo
        save(nome_melhor_modelo,file=paste(path,"/Resultados/AT_LongoPrazo/nome_melhor_modelo_xgboost_AT.rda",sep = ""))

        #guardar a lista de variaveis selecionadas no melhor modelo
        variaveis<-lista_variaveis[[nome_melhor_modelo]]
        save(variaveis,file=paste(path,"/Resultados/AT_LongoPrazo/variaveis_melhor_modelo_xgboost_AT.rda",sep = ""))

      }else{

        #carregar modelo e fazer a previsao
        load(paste(path,"/Resultados/AT_LongoPrazo/melhor_modelo_xgboost_AT.rda",sep = ""))

        #carregar definicao do log
        load(paste(path,"/Resultados/AT_LongoPrazo/nome_melhor_modelo_xgboost_AT.rda",sep = ""))

        #carregar lista de variaveis
        load(paste(path,"/Resultados/AT_LongoPrazo/variaveis_melhor_modelo_xgboost_AT.rda",sep = ""))

        #ler e processar dados
        #Guardas as falhas na label que queremos analisar
        if(retrieve_log(nome_melhor_modelo)=="sim"){
          Falhas_teste<-log(lista_dados$teste$Falhas_Totais_porKm+1)
        }else{
          Falhas_teste<-lista_dados$teste$Falhas_Totais_porKm
        }

        #retirar variavel de resposta do treino e teste
        index_falhas<-grep("Falhas_Totais_porKm", colnames(lista_dados$teste))
        lista_dados$teste<-lista_dados$teste[,-index_falhas]

        ##selecionar o periodo que queremos analisar e testar
        #guardar variaveis teste
        Distrito_teste<-lista_dados$teste$Distrito
        Concelho_teste<-lista_dados$teste$Concelho
        comprimento_teste<-lista_dados$teste$Comprimento_Total
        codigo_circuito_teste<-lista_dados$teste$Codigo.da.linha
        Anos_Historico_teste<-lista_dados$teste$Anos_Historico

        #selecionar colunas de acordo com as variaveis
        teste<-lista_dados$teste[,variaveis]

        #prever o numero de falhas
        melhores_previsoes <- predict(melhor_xgb_model,as.matrix(teste))

        #converter para o formato correto
        if(retrieve_log(nome_melhor_modelo)=="sim"){
          melhores_previsoes<-(exp(melhores_previsoes)-1)*comprimento_teste
          falhas_reais<-(exp(Falhas_teste)-1)*comprimento_teste
        }else{
          melhores_previsoes<-melhores_previsoes*comprimento_teste
          falhas_reais<-(Falhas_teste)*comprimento_teste
        }

        previsoes<-data.frame("Codigo Circuito"=codigo_circuito_teste,"Anos Historico"=Anos_Historico_teste,"Distrito"=Distrito_teste,"Concelho"=Concelho_teste,
                              "Comprimento Total"=comprimento_teste,"Falhas Reais"=falhas_reais,"Taxa Falhas por km"=melhores_previsoes/comprimento_teste,
                              "Probabilidade de falha"=(1-ppois(0,melhores_previsoes/comprimento_teste)))
      }

      return(previsoes)
    }

    #'Estimar probabilidade de avaria para as linhas MT
    #'
    #'@description Esta funcão permite estimar a probabilidade de falha,
    #'tendo por base as linhas MT guardadas na base de dados
    #'@usage probabilidade_avaria_MT(...,atualizar_modelo=c("nao","sim"),afinamento=c("nao","sim"),Tipo_linha_MT=c("Aereas","Subterraneas","Mistas"))
    #'
    #'@param lista_dados Dados a utilizar no calculo da probabilidade de avaria para as Linhas MT e classificacao desejada
    #'@param atualizar_modelo Permite utilizar um modelo previamente gerado parao calculo da probabilidade de falha. Caso contrário o modelo será atualizado com os dados disponiveis.
    #'@param afinamento Permite selecionar o nivel de afinamento desejado no modelo gerado. Ao ativar o afinamento o modelo irá demorar mais tempo a atualizar os resultados.
    #'@param Tipo_linha_MT Permite selecionar qual o tipo de linha MT para a qual pretendemos estimar a probabilidade de avaria (i.e. Aereas, Subterraneas ou Mistas).
    #'@return Calculo da probabilidade de falha para cada linha MT
    #'@examples #Ler os dados apartir de um dado ficheiro
    #'
    #'probabilidade_avaria_MT(Dados_MT,atualizar_modelo="sim",afinamento="nao",Tipo_linha_MT="Subterraneas")
    #'
    #'@export
    #Funcao para ler e alcular a probabilidade de avaria para as linhas MT com a classificacao desejada************************************************************************************************************************************************************
    probabilidade_avaria_MT<-function(lista_dados,atualizar_modelo="nao",afinamento="nao",Tipo_linha_MT){

      #path para o diretorio
      pack <- "iHEAD"
      path <- find.package(pack)

      #escolher se queremos a versao com log!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!!!**!!**!!**!!
      #gerar modelos com/sem transformacao logaritmica da variavel a prever
      opcao_log<-c("sim","nao")

      #contador do progresso do calculo do iHEAD
      progresso<-0

      #Guardar as previsoes geradas caso exista o afinamento do modelo, como também os resultados
      #guardar modelos xgboost
      lista_modelo_xgboost<-list("ComLog_opcao_log_sim"=NA,"SemLog_opcao_log_sim"=NA,
                                 "ComLog_opcao_log_nao"=NA,"SemLog_opcao_log_nao"=NA)

      #guardar previsoes xgboost
      lista_previsoes_xgboost<-list("ComLog_opcao_log_sim"=NA,"SemLog_opcao_log_sim"=NA,
                                    "ComLog_opcao_log_nao"=NA,"SemLog_opcao_log_nao"=NA)

      #guardar o root mean squared error (RMSE)
      lista_erros<-c("ComLog_opcao_log_sim"=NA,"SemLog_opcao_log_sim"=NA,
                     "ComLog_opcao_log_nao"=NA,"SemLog_opcao_log_nao"=NA)

      #guardar a lista de variaveis selecionadas
      lista_variaveis<-c("ComLog_opcao_log_sim"=NA,"SemLog_opcao_log_sim"=NA,
                         "ComLog_opcao_log_nao"=NA,"SemLog_opcao_log_nao"=NA)


      if(atualizar_modelo!="nao"){
        for(opcao_log_aux in opcao_log){
          #Gerar modelo e validar modelo************************************************************************************************************************************************************

          #Preparar tipos de testes a realizar
          nome_lista<-c("ComLog","SemLog")
          Matriz_resultados<-matrix(data=NA,nrow = 9,ncol = length(nome_lista))
          colnames(Matriz_resultados)<-nome_lista
          rownames(Matriz_resultados)<-c("Erro absoluto Pais","Erro percentual absoluto Pais","Erro absoluto ponderado Distrito","Erro percentual absoluto ponderado Distrito",
                                         "SMAPE Distrito","Erro absoluto ponderado Concelho","Erro percentual absoluto ponderado Concelho",
                                         "SMAPE Concelho","RMSE")

          #Guardas as falhas na label que queremos analisar
          if(opcao_log_aux=="sim"){
            Falhas_treino<-log(lista_dados$treino$Falhas_Totais_porKm+1)
            Falhas_teste<-log(lista_dados$teste$Falhas_Totais_porKm+1)
            lista_dados_aux<-lista_dados
          }else{
            lista_dados<-lista_dados_aux
            Falhas_treino<-lista_dados$treino$Falhas_Totais_porKm
            Falhas_teste<-lista_dados$teste$Falhas_Totais_porKm
          }

          #retirar variavel de resposta do treino e teste
          index_falhas<-grep("Falhas_Totais_porKm", colnames(lista_dados$treino))
          lista_dados$treino<-lista_dados$treino[,-index_falhas]
          lista_dados$teste<-lista_dados$teste[,-index_falhas]

          #Geral modelo e avaliar
          for(nome_lista_aux in nome_lista){

            #Atualizar o progresso do calculo
            progresso<-progresso+1
            print(paste("Progresso ",round(progresso*100/length(lista_erros),0),"%",sep = ""))

            #versao do modelo a realizar
            versao_modelo<-paste(nome_lista_aux,"_opcao_log_",opcao_log_aux,sep = "")

            #lista de variaveis que foram escolhidas para o modelo em questao
            lista_variaveis[[versao_modelo]]<-read_csv(paste(path,"/Resultados/MT_",Tipo_linha_MT,"_LongoPrazo/VariaveisSelecionadas_",nome_lista_aux,".csv",sep = ""))
            lista_variaveis[[versao_modelo]]<-unlist(lista_variaveis[[versao_modelo]]$x[2:length(lista_variaveis[[versao_modelo]]$x)])

            #selecionar  a lista de variaveis a utilizar para os algoritmos
            Training<-lista_dados$treino[,lista_variaveis[[versao_modelo]]]
            teste<-lista_dados$teste[,lista_variaveis[[versao_modelo]]]

            ##selecionar o periodo que queremos analisar e testar
            #guardar variaveis teste
            Distrito_teste<-lista_dados$Distrito_teste
            Concelho_teste<-lista_dados$Concelho_teste
            comprimento_teste<-lista_dados$teste$Comprimento_total
            codigo_circuito_teste<-lista_dados$teste$Codigo.Circuito
            Anos_Historico_teste<-lista_dados$teste$Anos_Historico

            #guardar variaveis treino
            Distrito_treino<-lista_dados$Distrito_treino
            Concelho_treino<-lista_dados$Concelho_treino
            comprimento_treino<-lista_dados$treino$Comprimento_total
            codigo_circuito_treino<-lista_dados$treino$Codigo.Circuito
            Anos_Historico_treino<-lista_dados$treino$Anos_Historico

            ## Modelos a construir depois do processar os dados. Os modelos são avaliados através do RMSE
            ## Linear regression - Stepwise --> utilizado para avaliar o efeito das variaveis e o nivel de importancia
            modelo_lm<-lm(Falhas_treino ~., data = Training)

            #caso queiramos guardar no excel
            output<-data.frame("Variaveis"=modelo_lm$coefficients)
            write.csv(output,file = paste(path,"/Resultados/MT_",Tipo_linha_MT,"_LongoPrazo/",nome_lista_aux,"_comLog_log_",opcao_log_aux,".csv",sep = ""))

            ## XGBOOST - Extreme gradient boosting.**
            if(afinamento=="nao"){
              xgb_model<-xgboost(data=as.matrix(Training),nfold=5,label=as.matrix(Falhas_treino),nrounds=2200,verbose=FALSE,objective='reg:linear',eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,max_depth=6,min_child_weight=1.7817,subsample=0.5213,colsample_bytree=0.4603)
            }else{
              #Melhoramento do xgboost
              train_control <- caret::trainControl(
                method = "none",
                verboseIter = FALSE,
                allowParallel = TRUE
              )

              #Numero de iteracoes e o learning rate
              nrounds <- 1000

              #Grid com os parametros a treinar
              tune_grid <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = c(0.025, 0.05, 0.1, 0.3),
                max_depth = c(2, 3, 4, 5, 6),
                gamma = 0,
                colsample_bytree = 1,
                min_child_weight = 1,
                subsample = 1
              )

              #Regras de avaliacao do erro na fase de treino
              tune_control <- caret::trainControl(
                method = "cv",
                number = 5,
                verboseIter = FALSE,
                allowParallel = TRUE
              )

              #Avaliar parametros
              xgb_tune <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid,
                method = "xgbTree",
                verbose = TRUE
              )

              #Grid com os parametros a treinar
              tune_grid2 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                                   c(xgb_tune$bestTune$max_depth:4),
                                   xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
                gamma = 0,
                colsample_bytree = 1,
                min_child_weight = c(1, 2, 3),
                subsample = 1
              )

              #Avaliar parametros
              xgb_tune2 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid2,
                method = "xgbTree",
                verbose = TRUE
              )

              #Column and Row Sampling
              #Grid com os parametros a treinar
              tune_grid3 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = 0,
                colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = c(0.5, 0.75, 1.0)
              )

              #Avaliar parametros
              xgb_tune3 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid3,
                method = "xgbTree",
                verbose = TRUE
              )

              #Gamma
              #Grid com os parametros a treinar
              tune_grid4 <- expand.grid(
                nrounds = seq(from = 200, to = nrounds, by = 200),
                eta = xgb_tune$bestTune$eta,
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
                colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = xgb_tune3$bestTune$subsample
              )

              #Avaliar parametros
              xgb_tune4 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid4,
                method = "xgbTree",
                verbose = TRUE
              )

              #Reduzir o learning rate
              #Grid com os parametros a treinar
              tune_grid5 <- expand.grid(
                nrounds = seq(from = 200, to = 10000, by = 200),
                eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
                max_depth = xgb_tune2$bestTune$max_depth,
                gamma = xgb_tune4$bestTune$gamma,
                colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
                min_child_weight = xgb_tune2$bestTune$min_child_weight,
                subsample = xgb_tune3$bestTune$subsample
              )

              #Avaliar parametros
              xgb_tune5 <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = tune_control,
                tuneGrid = tune_grid5,
                method = "xgbTree",
                verbose = TRUE
              )

              #Guardar os melhores parametros do modelo XGBOOST
              (final_grid <- expand.grid(
                nrounds = xgb_tune5$bestTune$nrounds,
                eta = xgb_tune5$bestTune$eta,
                max_depth = xgb_tune5$bestTune$max_depth,
                gamma = xgb_tune5$bestTune$gamma,
                colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
                min_child_weight = xgb_tune5$bestTune$min_child_weight,
                subsample = xgb_tune5$bestTune$subsample
              ))

              #Gerar o modelo com os melhores parametros
              (xgb_model <- caret::train(
                x = Training,
                y = Falhas_treino,
                trControl = train_control,
                tuneGrid = final_grid,
                method = "xgbTree",
                verbose = TRUE
              ))

            }

            ##resultados para o dataset de teste+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            falhas_previstas <- predict(xgb_model,as.matrix(teste))

            if(opcao_log_aux=="sim"){
              falhas_previstas<-(exp(falhas_previstas)-1)*comprimento_teste
              falhas_reais<-(exp(Falhas_teste)-1)*comprimento_teste
            }else{
              falhas_previstas<-falhas_previstas*comprimento_teste
              falhas_reais<-(Falhas_teste)*comprimento_teste
            }

            #guardar rmse
            lista_erros[versao_modelo]<-rmse(unlist(falhas_reais),falhas_previstas)

            #guardar previsoes apos processamento do log
            lista_previsoes_xgboost[[versao_modelo]] <- falhas_previstas

            #calcular erros ao nivel do pais
            EAM_pais_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            EPAM_pais_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            #indice utilizado para calcular o erro do ano
            l<-0

            #percorrer os anos de treino
            for(a in unique(Anos_Historico_teste)){
              l<-l+1
              EAM_pais_aux[l]<-abs(sum(falhas_reais[Anos_Historico_teste==a])-sum(falhas_previstas[Anos_Historico_teste==a]))
              EPAM_pais_aux[l]<-abs(sum(falhas_reais[Anos_Historico_teste==a])-sum(falhas_previstas[Anos_Historico_teste==a]))/sum(falhas_reais[Anos_Historico_teste==a])
            }
            EAM_pais<-mean(EAM_pais_aux)
            EPAM_pais<-mean(EPAM_pais_aux)

            #calcular erros ao nivel do Distrito
            #indice utilizado para calcular o erro do ano
            #renicializar o indice
            l<-0
            EAM_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            EPAM_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            SMAPE_Distrito_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))

            #calcular erros ao nivel do distrito
            for(a in unique(Anos_Historico_teste)){

              #pesos para cada distrito calculado com base na area
              pesos_distrito<-iHEAD::Pesos_MT_distrito
              Distrito_teste_aux<-pesos_distrito$Codigo_distrito

              #vetores auxiliares de calculo dos erros
              resultado_abs<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              resultado_abs_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              resultado_rel<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              falhas_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              smape_falhas_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              smape_aux<-array(data=NA,dim = length(unique(Distrito_teste_aux)))
              #indice do distrito
              k<-0

              #calculo do peso total no dataset de forma a normalizar os pesos quando nao sao utilizados todos os distritos. Casos em que temos um distrito com falha 0.
              for(i in unique(Distrito_teste_aux)){
                k<-k+1
                falhas_aux[k]<-sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])
                smape_falhas_aux[k]<-sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])+sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a])
              }
              pesos_distrito$Peso_linha[which(falhas_aux==0)]<-0
              pesos_distrito$Peso_linha<-pesos_distrito$Peso_linha/sum(pesos_distrito$Peso_linha)
              l<-l+1

              #renicializar indice do distrito
              k<-0
              #calculo dos erros
              for (i in unique(Distrito_teste_aux)) {
                k<-k+1
                resultado_abs[k]<-(pesos_distrito$Peso_linha[pesos_distrito$Codigo_distrito==i])*abs(sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a]))
                resultado_abs_aux[k]<-abs(sum(falhas_reais[Distrito_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Distrito_teste==i & Anos_Historico_teste==a]))
                smape_aux[k]<-(pesos_distrito$Peso_linha[pesos_distrito$Codigo_distrito==i])*2*resultado_abs_aux[k]/sum(smape_falhas_aux*pesos_distrito$Peso_linha)
              }
              resultado_rel<-resultado_abs/sum(falhas_aux*pesos_distrito$Peso_linha)
              EAM_Distrito_aux[l]<-sum(resultado_abs)
              EPAM_Distrito_aux[l]<-sum(resultado_rel[which(falhas_aux!=0)])

              #SMAPE
              SMAPE_Distrito_aux[l]<-sum(smape_aux[which(falhas_aux!=0)])
            }

            #resultado guardar distrito
            EAM_Distrito<-mean(EAM_Distrito_aux)
            EPAM_Distrito<-mean(EPAM_Distrito_aux)
            SMAPE_Distrito<-mean(SMAPE_Distrito_aux)

            #calcular erros ao nivel do concelho
            #indice utilizado para calcular o erro do ano
            #renicializar o indice

            EAM_Concelho_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            EPAM_Concelho_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            SMAPE_Concelho_aux<-array(data=NA, dim = length(unique(Anos_Historico_teste)))
            l<-0

            for(a in unique(Anos_Historico_teste)){
              #pesos dos concelhos
              pesos_concelhos<-iHEAD::Pesos_MT_concelho
              Concelho_teste_aux<-pesos_concelhos$`Codigo concelho do teste`

              #vetores auxiliares
              resultado_abs<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              resultado_abs_aux<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              resultado_abs_falhas_0<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              resultado_rel<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              falhas_aux<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              smape_falhas_aux<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              smape_aux<-array(data=NA,dim = length(unique(Concelho_teste_aux)))
              j<-0

              #calculo do peso total no dataset de forma a normalizar os pesos quando nao sao utilizados todos os distritos
              for(i in unique(Concelho_teste_aux)){
                j<-j+1
                falhas_aux[j]<-sum(falhas_reais[Concelho_teste==i & Anos_Historico_teste==a])
                smape_falhas_aux[j]<-sum(falhas_reais[Concelho_teste==i & Anos_Historico_teste==a])+sum(falhas_previstas[Concelho_teste==i & Anos_Historico_teste==a])
              }

              pesos_concelhos$Peso_linha[which(falhas_aux==0)]<-0
              pesos_concelhos$Peso_linha<-pesos_concelhos$Peso_linha/sum(pesos_concelhos$Peso_linha)
              l<-l+1
              j<-0
              for (i in unique(Concelho_teste_aux)) {
                j<-j+1
                resultado_abs[j]<-(pesos_concelhos$Peso_linha[pesos_concelhos$`Codigo concelho do teste`==i])*abs(sum(falhas_reais[Concelho_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Concelho_teste==i & Anos_Historico_teste==a]))
                resultado_abs_aux[j]<-abs(sum(falhas_reais[Concelho_teste==i & Anos_Historico_teste==a])-sum(falhas_previstas[Concelho_teste==i & Anos_Historico_teste==a]))
                smape_aux[j]<-(pesos_concelhos$Peso_linha[pesos_concelhos$`Codigo concelho do teste`==i])*2*resultado_abs_aux[j]/sum(smape_falhas_aux*pesos_concelhos$Peso_linha)
              }
              resultado_rel<-resultado_abs/sum(falhas_aux*pesos_concelhos$Peso_linha)

              EAM_Concelho_aux[l]<-sum(resultado_abs)
              EPAM_Concelho_aux[l]<-sum(resultado_rel[which(falhas_aux!=0)])

              #smape
              SMAPE_Concelho_aux[l]<-sum(smape_aux[which(falhas_aux!=0)])
            }

            #resultado guardar concelho
            EAM_Concelho<-mean(EAM_Concelho_aux)
            EPAM_Concelho<-mean(EPAM_Concelho_aux)
            SMAPE_Concelho<-mean(SMAPE_Concelho_aux)

            #imprimir resultados para a matriz
            Matriz_resultados[1,nome_lista_aux]<-EAM_pais
            Matriz_resultados[2,nome_lista_aux]<-EPAM_pais
            Matriz_resultados[3,nome_lista_aux]<-EAM_Distrito
            Matriz_resultados[4,nome_lista_aux]<-EPAM_Distrito
            Matriz_resultados[5,nome_lista_aux]<-SMAPE_Distrito
            Matriz_resultados[6,nome_lista_aux]<-EAM_Concelho
            Matriz_resultados[7,nome_lista_aux]<-EPAM_Concelho
            Matriz_resultados[8,nome_lista_aux]<-SMAPE_Concelho
            Matriz_resultados[9,nome_lista_aux]<-lista_erros[versao_modelo]

            #guardar o resultado das falhas previstas na lista de previsoes para cada modelo
            lista_modelo_xgboost[[versao_modelo]] <- xgb_model

            #Guardar no excel os resultados obtidos para os indicadores desenvolvidos
            write.csv(Matriz_resultados,file = paste(path,"/Resultados/MT_",Tipo_linha_MT,"_LongoPrazo/Resutados_indicadores_opcao_log_",opcao_log_aux,".csv",sep = ""))
          }
        }

        #imprimir as previsões utilizando o melhor modelo
        nome_melhor_modelo<-names(lista_erros[which(lista_erros==min(lista_erros))])
        previsoes<-data.frame("Codigo Circuito"=codigo_circuito_teste,"Anos Historico"=Anos_Historico_teste,"Distrito"=Distrito_teste,"Concelho"=Concelho_teste,
                              "Comprimento Total"=comprimento_teste,"Falhas Reais"=falhas_reais,"Taxa Falhas por km" = lista_previsoes_xgboost[[nome_melhor_modelo]]/comprimento_teste,
                              "Probabilidade de falha" = (1 - ppois(0, lista_previsoes_xgboost[[nome_melhor_modelo]]/comprimento_teste)))

        #guardar melhor modelo do xgboost
        melhor_xgb_model<-lista_modelo_xgboost[[nome_melhor_modelo]]
        save(melhor_xgb_model,file=paste(path,"/Resultados/MT_",Tipo_linha_MT,"_LongoPrazo/melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

        #guardar definicao do melhor modelo
        save(nome_melhor_modelo,file=paste(path,"/Resultados/MT_",Tipo_linha_MT,"_LongoPrazo/nome_melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

        #guardar a lista de variaveis selecionadas no melhor modelo
        variaveis<-lista_variaveis[[nome_melhor_modelo]]
        save(variaveis,file=paste(path,"/Resultados/MT_",Tipo_linha_MT,"_LongoPrazo/variaveis_melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

      }else{

        #carregar modelo e fazer a previsao
        load(paste(path,"/Resultados/MT_",Tipo_linha_MT,"_LongoPrazo/melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

        #carregar definicao do log
        load(paste(path,"/Resultados/MT_",Tipo_linha_MT,"_LongoPrazo/nome_melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

        #carregar lista de variaveis
        load(paste(path,"/Resultados/MT_",Tipo_linha_MT,"_LongoPrazo/variaveis_melhor_modelo_xgboost_MT_",Tipo_linha_MT,".rda",sep = ""))

        #ler e processar dados
        #Guardas as falhas na label que queremos analisar
        if(retrieve_log(nome_melhor_modelo)=="sim"){
          Falhas_teste<-log(lista_dados$teste$Falhas_Totais_porKm+1)
        }else{
          Falhas_teste<-lista_dados$teste$Falhas_Totais_porKm
        }

        #retirar variavel de resposta do treino e teste
        index_falhas<-grep("Falhas_Totais_porKm", colnames(lista_dados$teste))
        lista_dados$teste<-lista_dados$teste[,-index_falhas]

        ##selecionar o periodo que queremos analisar e testar
        #guardar variaveis teste
        Distrito_teste<-lista_dados$Distrito_teste
        Concelho_teste<-lista_dados$Concelho_teste
        comprimento_teste<-lista_dados$teste$Comprimento_total
        codigo_circuito_teste<-lista_dados$teste$Codigo.Circuito
        Anos_Historico_teste<-lista_dados$teste$Anos_Historico

        #selecionar colunas de acordo com as variaveis
        teste<-lista_dados$teste[,variaveis]

        #prever o numero de falhas
        melhores_previsoes <- predict(melhor_xgb_model,as.matrix(teste))

        #converter para o formato correto
        if(retrieve_log(nome_melhor_modelo)=="sim"){
          melhores_previsoes<-(exp(melhores_previsoes)-1)*comprimento_teste
          falhas_reais<-(exp(Falhas_teste)-1)*comprimento_teste
        }else{
          melhores_previsoes<-melhores_previsoes*comprimento_teste
          falhas_reais<-(Falhas_teste)*comprimento_teste
        }

        previsoes<-data.frame("Codigo Circuito"=codigo_circuito_teste,"Anos Historico"=Anos_Historico_teste,"Distrito"=Distrito_teste,"Concelho"=Concelho_teste,
                              "Comprimento Total"=comprimento_teste,"Falhas Reais"=falhas_reais,"Taxa Falhas por km" = melhores_previsoes/comprimento_teste,
                              "Probabilidade de falha" = (1 - ppois(0, melhores_previsoes/comprimento_teste)))

      }

      return(previsoes)
    }


