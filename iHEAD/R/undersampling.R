source("iHEAD_previsao.r")

path <- "D:/Alexandra Oliveira/HEAD"
list_data <- processamento_dados("AT", 1, 2, 2017)
training <- list_data$treino
#Failure distribution
original_hist <- hist(training$Falhas_Totais_porKm)
#Nr. of training years
number_training_years <- length(unique(training$Anos_Historico))

#No undersampling
selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'square')
results_square <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'square')
write.csv(results_square, file = paste(path,"/No_undersampling_transformations/AT_results_square.csv", sep=""))
selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'reciprocal')
results_reciprocal <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'reciprocal')
write.csv(results_reciprocal, file = paste(path,"/No_undersampling_transformations/AT_results_reciprocal.csv", sep=""))


#All recrods with failures equal to 0 : Experience 1
x <- training$Codigo.da.linha[which(training$Falhas_Totais_porKm==0)]
#Table of freuqnecies
table <- as.data.frame(table(x))
#Records that never have failed during the training years
no_failures <- as.character(table$x[which(table$Freq==number_training_years)])
#Update dataframe by removing records correspondent to no_failures
training <- training[!(training$Codigo.da.linha %in% no_failures),]
#Check new failure distribution
new_hist <- hist(training$Falhas_Totais_porKm)
list_data$treino <- training

selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'log')
results_log <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'log')
write.csv(results_log, file = paste(path,"/Undersampling_transformations_exp1/AT_results_log.csv", sep=""))
selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'square')
results_square <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'square')
write.csv(results_square, file = paste(path,"/Undersampling_transformations_exp1/AT_results_square.csv", sep=""))
selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'reciprocal')
results_reciprocal <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'reciprocal')
write.csv(results_reciprocal, file = paste(path,"/Undersampling_transformations_exp1/AT_results_reciprocal.csv", sep=""))


#Experiment 2
#Remove records that have failures less than 0.5 during the training years
xx<-training$Codigo.da.linha[which(training$Falhas_Totais_porKm <= 0.5)]
table_2 <- as.data.frame(table(xx))
no_failures <- as.character(table_2$xx[which(table_2$Freq==number_training_years)])
#Update dataframe
training_exp2 <- training[!(training$Codigo.da.linha %in% no_failures),]
#Failure distributios
new_hist_exp2<-hist(training_exp2$Falhas_Totais_porKm)
list_data$treino <- training_exp2

selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'log')
results_log <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'log')
write.csv(results_log, file =paste(path,"/Undersampling_transformations_exp2/AT_results_log.csv", sep=""))
selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'square')
results_square <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'square')
write.csv(results_square, file = paste(path,"/Undersampling_transformations_exp2/AT_results_square.csv", sep=""))
selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'reciprocal')
results_reciprocal <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'reciprocal')
write.csv(results_reciprocal, file = paste(path,"/Undersampling_transformations_exp2/AT_results_reciprocal.csv", sep=""))


#Experiment 3
#Remove records that have failures less than 1 during the training years
xx<-training$Codigo.da.linha[which(training$Falhas_Totais_porKm <= 1)]
table_2 <- as.data.frame(table(xx))
no_failures <- as.character(table_2$xx[which(table_2$Freq==number_training_years)])
#Update dataframe
training_exp3 <- training[!(training$Codigo.da.linha %in% no_failures),]
#Failure distributios
new_hist_exp3<-hist(training_exp3$Falhas_Totais_porKm)
list_data$treino <- training_exp3


selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'log')
results_log <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'log')
write.csv(results_log, file = paste(path,"/Undersampling_transformations_exp3/AT_results_log.csv", sep=""))
selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'square')
results_square <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'square')
write.csv(results_square, file = paste(path,"/Undersampling_transformations_exp3/AT_results_square.csv", sep=""))
selecao_variaveis(list_data,"AT",1, 10, 1, 1, 2, 2017,'reciprocal')
results_reciprocal <- prever_numero_avarias_AT(list_data,atualizar_modelo="sim",afinamento="nao",'reciprocal')
write.csv(results_reciprocal, file = paste(path,"/Undersampling_transformations_exp3/AT_results_reciprocal.csv", sep=""))
