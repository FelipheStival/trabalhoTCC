#=================================================================
# Funcao para carregar depedencias
#=================================================================
load.packages = function(){
  
  require(dplyr)
  require(randomForest)
  require(e1071)
  require(caTools)
  require(randomForest)
  require(caret)
  require(plumber)
  require(GA)
  
}

#================================================================
# Funcao para gerar data.frame aleatorio
#================================================================
load.data = function(){
  
  # Criando data.frame com valores aleatorios
  dados = data.frame(
    PROD_GEN_1 = round(runif(5000,min = 4000,max = 5000)),
    FLO_GEN_1 = round(runif(5000,min = 80,max = 90)),
    ALT_GEN_1 = round(runif(5000,min = 100,max = 110)),
    BP = round(runif(5000,min = 2,max = 3),1),
    ESC = round(runif(5000,min = 3,max = 4),1),
    PROD_GEN_2 = round(runif(5000,min = 4000,max = 5000)),
    FLO_GEN_2 = round(runif(5000,min = 80,max = 90),1),
    ALT_GEN_2 = round(runif(5000,min = 100,max = 110)),
    BP_GEN_2 = round(runif(5000,min = 2,max = 3),1),
    ESC_GEN_2 = round(runif(5000,min = 3,max = 4),1),
    resultado = floor(runif(5000,min = 0,max = 3))
  )
  
  # Criando variavel previsao
  dados = dados %>%
    mutate(resultado = case_when(dados$PROD_GEN_1 > 4800 & dados$PROD_GEN_2 > 4800 ~ "Otima",
                                 dados$PROD_GEN_1 > 4500 & dados$PROD_GEN_2 < 4800 ~ "Boa",
                                 dados$PROD_GEN_1 > 4300 & dados$PROD_GEN_2 < 4500 ~ "media",
                                 dados$PROD_GEN_1 >= 4000 & dados$PROD_GEN_2 < 4300 ~ "ruim"
    ))
  
  # Removendo valores NA
  dados = dados[!is.na(dados$resultado),]
  
  return(dados)
  
}

#==================================================================
# Funcao para criar o modelo
# @param dados
#==================================================================
load.model = function(dados, prever){
  
  # Escalonando dados
  dados$resultado = factor(dados$resultado)
  dados[,1:10] = scale(dados[,1:10])
  
  #Dividindo data.frame em teste e treinamento
  divisao = sample.split(dados$PROD_GEN_1,SplitRatio = 0.75)
  treinamento = subset(dados,divisao == TRUE)
  teste = subset(dados,divisao == FALSE)
  
  # Treinando modelo SVM
  classificador = svm(formula = resultado ~ . ,data = treinamento,type = "C-classification",kernel = "linear",cost = 0.001)
  previsao = predict(classificador, newdata = teste[-11])
  
  # Gerando matriz de confusao
  confusionMatrix(table(teste[,11],previsao))
  
  # Tunagem de parametros SVM
  tunnedModel = tune.svm(resultado ~., data=treinamento, kernel="linear", cost= c(0.001, 0.01, 0.1, 1,5,10))
  summary(tunnedModel)
  
  # Rodando modelo com tunning
  classificador = tunnedModel$best.model
  
  # Executando previsao
  previsao = predict(classificador, newdata = prever[-11])
  
  return(previsao)
}

#==================================================================
# Funcao de avaliacao algoritmo genetico
# @param dados
#==================================================================
encontrarComb = function(cromossomo,...){
  
  # Obtendo dados
  dados =  list(...)[[1]]
  
  # Combinando linhas
  temp = NULL
  for(i in seq(2,length(cromossomo),2)){
    
    # Obtendo linhas
    tempData = dados[c(cromossomo[i - 1],cromossomo[i]),]
    
    # Criando data.frame para previsao
    dataPrevisao = data.frame(
      PROD_GEN_1 = tempData[1,'PROD_GEN'],
      FLO_GEN_1 = tempData[1,'FLO_GEN'],
      ALT_GEN_1 = tempData[1,'ALT_GEN'],
      BP = tempData[1,'BP'],
      ESC = tempData[1,'ESC'],
      PROD_GEN_2 = tempData[2,'PROD_GEN'],
      FLO_GEN_2 =  tempData[2,'FLO_GEN'],
      ALT_GEN_2 = tempData[2,'ALT_GEN'],
      BP_GEN_2 = tempData[2,'BP'],
      ESC_GEN_2 = tempData[2,'ESC'],
      GENOTIPOS = paste(tempData[1,"ID_GEN"],tempData[2,'ID_GEN'],sep = "-")
    )
    
    # rbindo
    temp = rbind(temp, dataPrevisao)
  }
  
  
  # Relizando previsao
  previsao = load.model(load.data(), temp)
  previsao = as.character(previsao)
  
  # Adicionando resultado previsao
  temp$RESULTADO = previsao
  
  # Somando notas
  previsao = 
    case_when(previsao == "Otima" ~ 1,
              previsao == "Boa" ~ 2,
              previsao == "Media" ~ 3,
              previsao == "Ruim" ~ 4
    )
  
  return(sum(previsao))
  
}

#==================================================================
# Funcao para extrair o resultado
# @param dados
#==================================================================
load.extract = function(dados){
  
}

#==================================================================
# Funcao para encontrar combinacao
# @param dados
#==================================================================
load.combinacao = function(GADATE){
  
  # rodando algoritmo genetico 
  resultado = ga(
    "permutation",
    fitness = encontrarComb,
    lower = 1,
    upper = nrow(GADATE),
    maxiter = 1,
    popSize = 5,
    ... = GADATE
  )
  
  solucao = resultado@solution[1,]
  dataResult = NULL
  
  #Percorrendo linhas resultado
  for(i in seq(2,length(solucao), 2)){
    
    #Obtendo genotipo
    CombSelect = data.frame(
      PROD_GEN_1 = GADATE[i - 1,'PROD_GEN'],
      FLO_GEN_1 = GADATE[i - 1,'FLO_GEN'],
      ALT_GEN_1 = GADATE[i - 1,'ALT_GEN'],
      BP = GADATE[i - 1,'BP'],
      ESC = GADATE[i - 1,'ESC'],
      PROD_GEN_2 = GADATE[i,'PROD_GEN'],
      FLO_GEN_2 =  GADATE[i,'FLO_GEN'],
      ALT_GEN_2 = GADATE[i,'ALT_GEN'],
      BP_GEN_2 = GADATE[i,'BP'],
      ESC_GEN_2 = GADATE[i,'ESC'],
      GENOTIPOS = paste(GADATE[i - 1,"ID_GEN"],GADATE[i,'ID_GEN'],sep = "-")
    )
    
    #Construindo possivel solucao
    dataResult = rbind(dataResult,CombSelect)
  }
  
  # Obtendo resultado modelo
  dataResult$RESULTADO = as.character(load.model(load.data(), dataResult))
  dataResult = dataResult[,c("GENOTIPOS","RESULTADO")]
  
  return(dataResult)
}
