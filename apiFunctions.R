# pacotes
require(plumber)
require(rjson)

# Carregando depedencias
source("model.R")

# plumber.R

#* Obter dados da API e rodar modelo
#* @param dados data.frame com dados 
#* @get /combinacao
function(jsonDados = NULL) {
  # Convertendo dados
  dados = rjson::fromJSON(jsonDados)
  dados = do.call(rbind.data.frame, dados)
  # Obtendo resultado modelo
  resultado = load.combinacao(dados)
  list(msg = resultado)
}

