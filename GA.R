setClass (
  "Produto",
  slots = c(
    nome = "character",
    espaco = "numeric",
    valor = "numeric"
  )
)

setClass (
  "Individuo",
  slots = c(
    espacos = "numeric",
    valores = "numeric",
    limiteEspacos = "numeric",
    notaAvaliacao = "numeric",
    espacoUsado = "numeric",
    geracao = "numeric",
    cromossomo = "character"
  ),
  
  prototype = list(
    espacos = 0,
    valores = 0,
    limiteEspacos = 0,
    notaAvaliacao = 0,
    espacoUsado = 0,
    geracao = 0
  )
)

gerarCromossomo = function(tamanhoCromossomo) {
  cromossomo = sample(x = c("0", "1"), size = tamanhoCromossomo, replace = TRUE)
  return(cromossomo)
}

avaliacao = function(individuo) {
  nota = 0
  somaEspacos = 0
  for (i in 1:length(individuo@cromossomo)) {
    if (individuo@cromossomo[i] == '1') {
      nota = nota + individuo@valores[i]
      somaEspacos = somaEspacos + individuo@espacos[i]
    }
  }
  if (somaEspacos > individuo@limiteEspacos) {
    nota = 1
  }
  
  individuo@notaAvaliacao = nota
  individuo@espacoUsado = somaEspacos
  
  return(individuo)
}

crossover = function(individuoA, individuoB) {
  indices = 1:length(individuoA@cromossomo)
  corte = sample(indices, 1)
  if (corte == length(individuoA@cromossomo)) {
    filho1 = individuoB@cromossomo[1:corte]
    filho2 = individuoA@cromossomo[1:corte]
  } else {
    filho1 = c(individuoB@cromossomo[1:corte], individuoA@cromossomo[(corte + 1):length(individuoA@cromossomo)])
    filho2 = c(individuoA@cromossomo[1:corte], individuoB@cromossomo[(corte + 1):length((individuoB@cromossomo))])
  }
  
  filhos = list(
    new("Individuo", espacos = individuoA@espacos, valores = individuoA@valores, 
        limiteEspacos = individuoA@limiteEspacos, geracao = individuoA@geracao + 1),
    new("Individuo", espacos = individuoB@espacos, valores = individuoB@valores, 
        limiteEspacos = individuoB@limiteEspacos, geracao = individuoB@geracao + 1)
  )
  
  filhos[[1]]@cromossomo = filho1
  filhos[[2]]@cromossomo = filho2
  return(filhos)
}

mutacao = function(individuo, taxaMutacao) {
  for (i in 1:length(individuo@cromossomo)) {
    if (runif(n = 1, min = 0, max = 1) < taxaMutacao) {
      if (individuo@cromossomo[i] == '1') {
        individuo@cromossomo[i] = '0'
      } else {
        individuo@cromossomo[i] = '1'
      }
    }
  }
  return (individuo)
}

setClass (
  "algoritmoGenetico",
  slots = c(
    tamanhoPopulacao = "numeric",
    populacao = "list",
    geracao = "numeric",
    melhorSolucao = "Individuo"
  )
)

inicializaPopulacao = function(algoritmoGenetico, espacos, valores, limite) {
  for (i in 1:algoritmoGenetico@tamanhoPopulacao) {
    algoritmoGenetico@populacao[[i]] = new("Individuo", espacos = espacos, valores = valores, 
                                           limiteEspacos = limite)
    algoritmoGenetico@populacao[[i]]@cromossomo = gerarCromossomo(length(espacos))
  }
  return(algoritmoGenetico)
}

ordenaPopulacao = function(populacao) {
  populacaoOrdenada = c()
  notasAvaliacao = c()
  for (individuo in populacao) {
    notasAvaliacao = c(notasAvaliacao, individuo@notaAvaliacao)  
  }
  listaPosicao = order(notasAvaliacao, decreasing = TRUE)
  for (i in 1:length(listaPosicao)) {
    populacaoOrdenada = c(populacaoOrdenada, populacao[[listaPosicao[i]]])
  }
  return(populacaoOrdenada)
}

melhorIndividuo = function(algoritmoGenetico, individuo) {
  if (individuo@notaAvaliacao > algoritmoGenetico@melhorSolucao@notaAvaliacao) {
    algoritmoGenetico@melhorSolucao = individuo
  }
  return(algoritmoGenetico)
}

somaAvaliacoes = function(algoritmoGenetico) {
  soma = 0
  for (individuo in algoritmoGenetico@populacao) {
    soma = soma + individuo@notaAvaliacao
  }
  return(soma)
}

selecionaPai = function(algoritmoGenetico, somaAvaliacoes) {
  pai = 0
  valorSorteado = runif(1, 0, 1) * somaAvaliacoes
  soma = 0
  i = 1
  while (i < length(algoritmoGenetico@populacao) && soma < valorSorteado) {
    soma = soma + algoritmoGenetico@populacao[[i]]@notaAvaliacao
    pai = pai + 1
    i = i + 1
  }
  return(pai)
}

visualizaGeracao = function(algoritmoGenetico) {
  melhor = algoritmoGenetico@populacao[[1]]
  cat("\nG: ", melhor@geracao, " Valor: ", melhor@notaAvaliacao,
      " Espaço: ", melhor@espacoUsado, " Cromossomo: ", melhor@cromossomo)
}

resolver = function(algoritmoGenetico, taxaMutacao, numeroGeracoes, espacos, valores, limiteEspaco) {
  ag = algoritmoGenetico
  ag = inicializaPopulacao(algoritmoGenetico = ag, espacos = espacos, valores = valores, limite = limiteEspaco)
  for (i in 1:ag@tamanhoPopulacao) {
    ag@populacao[[i]] = avaliacao(ag@populacao[[i]])
  }
  ag@populacao = ordenaPopulacao(ag@populacao)
  ag@melhorSolucao = ag@populacao[[1]]
  visualizaGeracao(algoritmoGenetico = ag)
  
  for (geracao in 1:numeroGeracoes) {
    soma = somaAvaliacoes(algoritmoGenetico = ag)
    novaPopulacao = c()
    for (individuosGerados in 1:(ag@tamanhoPopulacao / 2)) {
      pai1 = selecionaPai(algoritmoGenetico = ag, somaAvaliacoes = soma)
      pai2 = selecionaPai(algoritmoGenetico = ag, somaAvaliacoes = soma)
      
      filhos = crossover(individuoA = ag@populacao[[pai1]], individuoB = ag@populacao[[pai2]])
      novaPopulacao = c(novaPopulacao, mutacao(individuo = filhos[[1]], taxaMutacao = taxaMutacao))
      novaPopulacao = c(novaPopulacao, mutacao(individuo = filhos[[2]], taxaMutacao = taxaMutacao))
    }
    ag@populacao = novaPopulacao
    for (i in 1:ag@tamanhoPopulacao) {
      ag@populacao[[i]] = avaliacao(ag@populacao[[i]])
    }
    ag@populacao = ordenaPopulacao(ag@populacao)
    visualizaGeracao(algoritmoGenetico = ag)
    ag = melhorIndividuo(algoritmoGenetico = ag, individuo = ag@populacao[[1]])
  }
  
  cat("\nMelhor solução - G: ", ag@melhorSolucao@geracao, " Valor: ", ag@melhorSolucao@notaAvaliacao,
      " Espaço: ", ag@melhorSolucao@espacoUsado, " Cromossomo: ", ag@melhorSolucao@cromossomo)
  return(ag)
}

listaProdutos = c(new("Produto", nome = "Geladeira Dako", espaco = 0.751, valor = 999.90),
                  new("Produto", nome = "Iphone 6", espaco = 0.0000899, valor = 2911.12),
                  new("Produto", nome = "TV 55' ", espaco = 0.400, valor = 4346.99),
                  new("Produto", nome = "TV 50' ", espaco = 0.290, valor = 3999.90),
                  new("Produto", nome = "TV 42' ", espaco = 0.200, valor = 2999.00),
                  new("Produto", nome = "Notebook Dell", espaco = 0.00350, valor = 2499.90),
                  new("Produto", nome = "Ventilador Panasonic", espaco = 0.496, valor = 199.90),
                  new("Produto", nome = "Microondas Electrolux", espaco = 0.0424, valor = 308.66),
                  new("Produto", nome = "Microondas LG", espaco = 0.0544, valor = 429.90),
                  new("Produto", nome = "Microondas Panasonic", espaco = 0.0319, valor = 299.29),
                  new("Produto", nome = "Geladeira Brastemp", espaco = 0.635, valor = 849.00),
                  new("Produto", nome = "Geladeira Consul", espaco = 0.870, valor = 1199.89),
                  new("Produto", nome = "Notebook Lenovo", espaco = 0.498, valor = 1999.90),
                  new("Produto", nome = "Notebook Asus", espaco = 0.527, valor = 3999.00)
)

espacos = c()
valores = c()
nomes = c()
for (produto in listaProdutos) {
  espacos = c(espacos, produto@espaco)
  valores = c(valores, produto@valor)
  nomes = c(nomes, produto@nome)
}
limite = 3
tamanho = 20
probabilidadeMutacao = 0.05
numeroGeracoes = 100
ag = new("algoritmoGenetico", tamanhoPopulacao = tamanho)
ag = resolver(algoritmoGenetico = ag, taxaMutacao = probabilidadeMutacao, numeroGeracoes = numeroGeracoes,
              espacos = espacos, valores = valores, limiteEspaco = limite)
for (i in 1:length(listaProdutos)) {
  if (ag@melhorSolucao@cromossomo[i] == '1') {
    cat("\nNome: ", nomes[i], " R$: ", valores[i])
  }
}












