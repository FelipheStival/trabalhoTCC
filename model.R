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
  
  load.packages()
  
  # Criando data.frame com valores aleatorios
  dados = data.frame(
    PROD_GEN_1 = round(runif(100,min = 4000,max = 5000)),
    FLO_GEN_1 = round(runif(100,min = 80,max = 90)),
    ALT_GEN_1 = round(runif(100,min = 100,max = 110)),
    PROD_GEN_2 = round(runif(100,min = 4000,max = 5000)),
    FLO_GEN_2 = round(runif(100,min = 80,max = 90),1),
    ALT_GEN_2 = round(runif(100,min = 100,max = 110)),
    resultado = floor(runif(100,min = 0,max = 3))
  )
  
  dados = dados %>%
    mutate(PROD_GEN_1 = case_when(dados$PROD_GEN_1 > 4800 & dados$PROD_GEN_1 > 4800 ~ "Otima",
                                  dados$PROD_GEN_1 > 4500 & dados$PROD_GEN_1 < 4800 ~ "Boa",
                                  dados$PROD_GEN_1 > 4300 & dados$PROD_GEN_1 < 4500 ~ "media",
                                  dados$PROD_GEN_1 >= 4000 & dados$PROD_GEN_1 < 4300 ~ "ruim"),
           
           FLO_GEN_1 = case_when(dados$FLO_GEN_1 >= 80 & dados$FLO_GEN_1 <= 82 ~ "Ruim",
                                 dados$FLO_GEN_1 >= 82 & dados$FLO_GEN_1 <= 90 ~ "Boa" ),
           
           ALT_GEN_1 = case_when(dados$ALT_GEN_1 >= 100 & dados$ALT_GEN_1 <= 102 ~ "Ruim",
                                 dados$ALT_GEN_1 >= 102 & dados$ALT_GEN_1 <= 105 ~ "Media" ,
                                 dados$ALT_GEN_1 >= 105 & dados$ALT_GEN_1 <= 110 ~ "Otima"),
           
           PROD_GEN_2 = case_when(dados$PROD_GEN_2 > 4800 & dados$PROD_GEN_2 > 4800 ~ "Otima",
                                  dados$PROD_GEN_2 > 4500 & dados$PROD_GEN_2 < 4800 ~ "Boa",
                                  dados$PROD_GEN_2 > 4300 & dados$PROD_GEN_2 < 4500 ~ "media",
                                  dados$PROD_GEN_2 >= 4000 & dados$PROD_GEN_2 < 4300 ~ "ruim"),
           
           FLO_GEN_2 = case_when(dados$FLO_GEN_2 >= 80 & dados$FLO_GEN_2 <= 82 ~ "Ruim",
                                 dados$FLO_GEN_2 >= 82 & dados$FLO_GEN_2 <= 90 ~ "Boa" ),
           
           ALT_GEN_2 = case_when(dados$ALT_GEN_2 >= 100 & dados$ALT_GEN_2 <= 102 ~ "Ruim",
                                 dados$ALT_GEN_2 >= 102 & dados$ALT_GEN_2 <= 105 ~ "Media" ,
                                 dados$ALT_GEN_2 >= 105 & dados$ALT_GEN_2 <= 110 ~ "Otima"),
           
           PROD_GEN_2 = case_when(dados$PROD_GEN_2 > 4800 & dados$PROD_GEN_2 > 4800 ~ "Otima",
                                  dados$PROD_GEN_2 > 4500 & dados$PROD_GEN_2 < 4800 ~ "Boa",
                                  dados$PROD_GEN_2 > 4300 & dados$PROD_GEN_2 < 4500 ~ "media",
                                  dados$PROD_GEN_2 >= 4000 & dados$PROD_GEN_2 < 4300 ~ "ruim"),
    )
  
  # Removendo valores NA
  dados = dados[!is.na(dados$resultado),]
  
  return(dados)
  
}

#==================================================================
# Funcao para criar o modelo
# @param dados
#==================================================================
load.model = function(dados, prever){
  
  # Tratando dados
  prever = prever %>%
    mutate(PROD_GEN_1 = case_when(prever$PROD_GEN_1 > 3000 & prever$PROD_GEN_1 > 4800 ~ "Otima",
                                  prever$PROD_GEN_1 > 2000 & prever$PROD_GEN_1 < 3000 ~ "Boa",
                                  prever$PROD_GEN_1 > 1000 & prever$PROD_GEN_1 < 2000 ~ "media",
                                  prever$PROD_GEN_1 >= 0 & prever$PROD_GEN_1 < 1000 ~ "ruim"),
           
           FLO_GEN_1 = case_when(prever$FLO_GEN_1 >= 80 & prever$FLO_GEN_1 <= 82 ~ "Ruim",
                                 prever$FLO_GEN_1 >= 82 & prever$FLO_GEN_1 <= 90 ~ "Boa" ,
                                 prever$FLO_GEN_1 >= 0 & prever$FLO_GEN_1 <= 82 ~ "Boa" ),
           
           ALT_GEN_1 = case_when(prever$ALT_GEN_1 >= 0 & prever$ALT_GEN_1 <= 102 ~ "Ruim",
                                 prever$ALT_GEN_1 >= 102 & prever$ALT_GEN_1 <= 105 ~ "Media" ,
                                 prever$ALT_GEN_1 >= 105 & prever$ALT_GEN_1 <= 110 ~ "Otima"),
           
           PROD_GEN_2 = case_when(prever$PROD_GEN_1 > 3000 & prever$PROD_GEN_1 > 4800 ~ "Otima",
                                  prever$PROD_GEN_1 > 2000 & prever$PROD_GEN_1 < 3000 ~ "Boa",
                                  prever$PROD_GEN_1 > 1000 & prever$PROD_GEN_1 < 2000 ~ "media",
                                  prever$PROD_GEN_1 >= 0 & prever$PROD_GEN_1 < 1000 ~ "ruim"),
           
           FLO_GEN_2 = case_when(prever$FLO_GEN_2 >= 80 & prever$FLO_GEN_2 <= 82 ~ "Ruim",
                                 prever$FLO_GEN_2 >= 82 & prever$FLO_GEN_2 <= 90 ~ "Boa" ,
                                 prever$FLO_GEN_2 >= 0 & prever$FLO_GEN_2 <= 82 ~ "Boa" ),
           
           ALT_GEN_2 = case_when(prever$ALT_GEN_2 >= 0 & prever$ALT_GEN_2 <= 102 ~ "Ruim",
                                 prever$ALT_GEN_2 >= 102 & prever$ALT_GEN_2 <= 105 ~ "Media" ,
                                 prever$ALT_GEN_2 >= 105 & prever$ALT_GEN_2 <= 110 ~ "Otima"),
    )
  
  load.packages()
  
  # Escalonando dados
  dados$resultado = factor(dados$resultado)
  
  #Dividindo data.frame em teste e treinamento
  divisao = sample.split(dados$PROD_GEN_1,SplitRatio = 0.75)
  treinamento = subset(dados,divisao == TRUE)
  teste = subset(dados,divisao == FALSE)
  
  # Treinando modelo Naive bayes
  classificador = naiveBayes(resultado ~ ., data = treinamento)
  previsao = predict(classificador, newdata = teste[-7])
  
  # Matrix de confusao
  cm = table(teste$resultado, previsao)
  
  # Fazendo previsao
  previsao = predict(classificador, newdata = prever[-7])
  
  # Taxa de porcentagem
  confusionMatrix(cm)
  
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
      #BP = tempData[1,'BP'],
      #ESC = tempData[1,'ESC'],
      PROD_GEN_2 = tempData[2,'PROD_GEN'],
      FLO_GEN_2 =  tempData[2,'FLO_GEN'],
      ALT_GEN_2 = tempData[2,'ALT_GEN'],
      #BP_GEN_2 = tempData[2,'BP'],
      #ESC_GEN_2 = tempData[2,'ESC'],
      GENOTIPOS = paste(tempData[1,"ID_GEN"],tempData[2,'ID_GEN'],sep = "-")
    )
    
    # rbindo
    temp = rbind(temp, dataPrevisao)
  }
  
  # Relizando previsao
  previsao = load.model(dadosGerados, temp)
  previsao = as.character(previsao)
  
  # Adicionando resultado previsao
  temp$RESULTADO = previsao
  
  # Somando notas
  return(sum(as.numeric(previsao)))
  
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
  
  load.packages()
  
  # Gerando dados aleatorios baseados em dados reais
  dadosGerados <<- load.data()
  
  # rodando algoritmo genetico 
  resultado = ga(
    "permutation",
    fitness = encontrarComb,
    lower = 1,
    upper = nrow(GADATE),
    maxiter = 100,
    popSize = 5,
    ... = GADATE
  )
  
  solucao = resultado@solution[1,]
  dataResult = NULL
  
  #Percorrendo linhas resultado
  for(i in seq(2,length(solucao), 2)){
    
    #Obtendo genotipo
    CombSelect = data.frame(
      PROD_GEN_1 = GADATE[solucao[i - 1],'PROD_GEN'],
      FLO_GEN_1 = GADATE[solucao[i - 1],'FLO_GEN'],
      ALT_GEN_1 = GADATE[solucao[i - 1],'ALT_GEN'],
      PROD_GEN_2 = GADATE[solucao[i],'PROD_GEN'],
      FLO_GEN_2 =  GADATE[solucao[i],'FLO_GEN'],
      ALT_GEN_2 = GADATE[solucao[i],'ALT_GEN'],
      GENOTIPOS = paste(GADATE[solucao[i - 1],"ID_GEN"],GADATE[solucao[i],'ID_GEN'],sep = "-")
    )
    
    #Construindo possivel solucao
    dataResult = rbind(dataResult,CombSelect)
  }
  
  # Obtendo resultado modelo
  dataResult$RESULTADO = as.character(load.model(load.data(), dataResult))
  dataResult = dataResult[,c("GENOTIPOS","RESULTADO")]
  dataResult$RESULTADO = case_when(dataResult$RESULTADO == 0 ~ "Ruim",
                                   dataResult$RESULTADO == 1 ~ "Media",
                                   dataResult$RESULTADO == 2 ~ "Boa",
                                   dataResult$RESULTADO == 3 ~ "Muito boa")
  
  return(dataResult)
}

