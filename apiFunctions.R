# pacotes
require(plumber)
require(rjson)

# Carregando depedencias
source("model.R")


# plumber.R

# Enable CORS Filtering
#' @filter cors
cors <- function(req, res) {
  safe_domains <- c("http://localhost:8100")
  
  if (any(grepl(pattern = paste0(safe_domains,collapse="|"), req$HTTP_REFERER,ignore.case=T))) {
    res$setHeader("Access-Control-Allow-Origin", sub("/$","",req$HTTP_REFERER)) #Have to remove last slash, for some reason
    
    if (req$REQUEST_METHOD == "OPTIONS") {
      res$setHeader("Access-Control-Allow-Methods","GET,HEAD,PUT,PATCH,POST,DELETE") #This is how node.js does it
      res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
      res$status <- 200
      return(list())
    } else {
      plumber::forward()
    }
  } else {
    plumber::forward()
  }
} 

#* Obter dados da API e rodar modelo
#* @post /
function(jsonDados = NULL) {
  # Convertendo dados
  #dados = rjson::fromJSON(jsonDados)
  #dados = do.call(rbind.data.frame, dados)
  # Obtendo resultado modelo
  
  # Criando colunas faltantes
  colunas = c('ID_GEN','PROD_GEN','FLO_GEN','ALT_GEN')
  for(coluna in colunas){
    if(coluna %in% colnames(jsonDados)){
      
    } else {
      jsonDados[,coluna] = 0
    }
  }
  
  resultado = load.combinacao(jsonDados)
  
  return(resultado)
  
}

