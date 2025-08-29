# ===========================================
#        AMOSTRAGEM ESTRATIFICADA NO R
# ===========================================

# -------------------------------------------------------
# Função: clean_env
# Objetivo: limpar completamente o ambiente de trabalho do R
# Parâmetros:
#   - hidden   : se TRUE, remove também objetos ocultos (que começam com ".")
#   - packages : se TRUE, descarrega pacotes não-essenciais (cuidado!)
#   - graphics : se TRUE, fecha todos os dispositivos gráficos
# -------------------------------------------------------
clean_env <- function(hidden = TRUE, packages = FALSE, graphics = FALSE) {
  cat("🧹 Iniciando limpeza do ambiente...\n")
  
  # 1. Limpar objetos do ambiente global
  if (hidden) {
    rm(list = ls(all.names = TRUE), envir = .GlobalEnv)
    cat("✅ Objetos visíveis e ocultos removidos\n")
  } else {
    rm(list = ls(all.names = FALSE), envir = .GlobalEnv)
    cat("✅ Objetos visíveis removidos\n")
  }
  
  # 2. Limpar console (opcional - depende do ambiente)
  tryCatch({
    cat("\014") # Limpa console no RStudio
    cat("✅ Console limpo\n")
  }, error = function(e) {
    cat("⚠️  Não foi possível limpar o console\n")
  })
  
  # 3. Fechar dispositivos gráficos
  if (graphics) {
    while (!is.null(dev.list())) {
      dev.off()
    }
    cat("✅ Dispositivos gráficos fechados\n")
  }
  
  # 4. Descarregar pacotes não-essenciais
  if (packages) {
    essential_pkgs <- c("base", "utils", "stats", "graphics", "grDevices", "methods")
    all_pkgs <- .packages()
    pkgs_to_unload <- setdiff(all_pkgs, essential_pkgs)
    
    if (length(pkgs_to_unload) > 0) {
      for (pkg in pkgs_to_unload) {
        tryCatch({
          detach(paste0("package:", pkg), character.only = TRUE, unload = TRUE)
        }, error = function(e) {
          # Alguns pacotes não podem ser descarregados
        })
      }
      cat("✅ Pacotes não-essenciais descarregados\n")
    }
  }
  
  # 5. Coletar lixo da memória
  gc(verbose = FALSE)
  cat("✅ Coleta de lixo realizada\n")
  
  cat("🎉 Ambiente limpo com sucesso!\n")
  invisible(TRUE)
}

# -------------------------------------------------------
# Função: eh_inteiro
# Objetivo: verificar se um número é inteiro
# Entrada : número
# Saída   : TRUE se for inteiro, FALSE caso contrário
# -------------------------------------------------------
eh_inteiro <- function(numero) {
  resultado <- (numero %% 1) == 0
  return(resultado)
}

# -------------------------------------------------------
# Função: arredondar_para_cima
# Objetivo: arredondar cada elemento de um vetor para cima
# Entrada : lista numérica
# Saída   : vetor de inteiros arredondados
# -------------------------------------------------------
arredondar_para_cima <- function(lista) {
  resultado <- numeric(length(lista))
  
  for (i in seq_along(lista)) {
    valor <- lista[i]
    if (eh_inteiro(valor)) {
      resultado[i] <- as.integer(valor) + 1
    } else {
      resultado[i] <- ceiling(valor)
    }
  }
  return(resultado)
}


# ======================================================
# Inicialização
# ======================================================
cat("============================================================ \n",
    "Limpando o ambiente de trabalho do R \n",
    "============================================================")
clean_env()

# ATENÇÃO:
# Para evitar conflito entre STRATA() dos pacotes sampling e survey,
# o pacote survey só deve ser carregado depois da seleção da amostra.

# -------------------------------------------------------
# Instalação e carregamento do pacote sampling
# -------------------------------------------------------
install.packages("sampling")
library(sampling)

# -------------------------------------------------------
# Carregar base de dados MU284 (municípios da Suécia)
# -------------------------------------------------------
data(MU284)

# -------------------------------------------------------
# Construir tabela de frequências da variável de estratificação
# -------------------------------------------------------
freq_estratos <- table(MU284$REG)  # Tabela de frequência por região
dados_populacao <- MU284           # Dados originais
nome_dataset <- "MU284"            # Nome do dataset como string
print(dados_populacao)

# -------------------------------------------------------
# Definições iniciais
# -------------------------------------------------------
N <- nrow(MU284)                   # Tamanho total da população
tamanhos <- list(80, 80)           # Tamanhos de amostra possíveis
n <- tamanhos[[1]]                 # Primeiro tamanho de amostra
qtd_estratos <- length(freq_estratos) # Número de estratos

# Definir semente para reprodutibilidade
semente <- 3
set.seed(semente)

# Variável usada para estratificação
variavel_estrato <- "REG"

# Variável numérica de interesse (estimativa média e total)
variavel_estimacao <- "P85"   # População em 1985
formula_estimacao <- as.formula(paste("~", variavel_estimacao))


# ======================================================
# Diagnóstico dos dados de entrada
# ======================================================
cat("============================================================ \n",
    "DADOS DE ENTRADA\n",
    "Tabela : ", nome_dataset, "\n",
    "População total (N) = ", N, "\n",
    "Tamanho da amostra (n) = ", n, "\n",
    "Qtd. de estratos = ", qtd_estratos, "\n",
    "Semente = ", semente, "\n",
    "Variável de estratificação = ", variavel_estrato, "\n",
    "Variável de estimação = ", variavel_estimacao, "\n",
    "Fórmula = ", deparse(formula_estimacao), "\n",
    "============================================================")

# -------------------------------------------------------
# Cálculo de pesos amostrais proporcionais
# -------------------------------------------------------
cat("Peso amostral (proporção em cada estrato): ", prop.table(freq_estratos), "\n")

cat("============================================================ \n",
    "Explicação de como foram obtidos pesos amostrais:\n",
    "Proporção de cada estrato em relação ao total populacional.\n",
    "============================================================")

total_municipios <- sum(freq_estratos)
for (i in seq_along(freq_estratos)) {
  cat("Estrato", i, ": ", freq_estratos[i], "/", total_municipios,
      "=", freq_estratos[i] / total_municipios, "\n")
}

# -------------------------------------------------------
# Alterando tamanho da amostra para o segundo valor
# -------------------------------------------------------
n <- tamanhos[[2]]

cat("============================================================ \n",
    "NOVOS DADOS DE ENTRADA (com n alterado)\n",
    "Tabela : ", nome_dataset, "\n",
    "População total (N) = ", N, "\n",
    "Tamanho da amostra (n) = ", n, "\n",
    "Qtd. de estratos = ", qtd_estratos, "\n",
    "Semente = ", semente, "\n",
    "Variável Estrato = ", variavel_estrato, "\n",
    "Variável Estimação = ", variavel_estimacao, "\n",
    "Fórmula = ", deparse(formula_estimacao), "\n",
    "============================================================")

# -------------------------------------------------------
# Cálculo da alocação proporcional teórica
# -------------------------------------------------------
alocacao_teorica <- n * prop.table(freq_estratos)

cat("============================================================ \n",
    "Alocação proporcional teórica (sem arredondamento)\n",
    "============================================================")
for (i in 1:qtd_estratos) {
  cat("Estrato", i, ": ", prop.table(freq_estratos)[i], "x", n,
      "=", prop.table(freq_estratos)[i] * n, "\n")
}

# -------------------------------------------------------
# Ajuste da alocação (sempre arredondando para cima)
# -------------------------------------------------------
alocacao_final <- arredondar_para_cima(alocacao_teorica)
cat("Alocação ajustada (com arredondamento para cima): ", alocacao_final, "\n")

# -------------------------------------------------------
# Probabilidades de seleção em cada estrato
# -------------------------------------------------------
cat("============================================================ \n",
    "Probabilidades de seleção em cada estrato:\n",
    "============================================================")
for (i in seq_along(freq_estratos)) {
  cat("Estrato", i, ": ", alocacao_final[i], "/", freq_estratos[i],
      "=", alocacao_final[i] / freq_estratos[i], "\n")
}

# -------------------------------------------------------
# Seleção da amostra estratificada
# -------------------------------------------------------
amostra_ids <- strata(dados_populacao,
                      stratanames = variavel_estrato,
                      size = alocacao_final,
                      method = "srswor")

# Amostra estratificada (IDs)
head(amostra_ids, 10)

# Probabilidades de inclusão das unidades sorteadas
amostra_ids$Prob

# Extração dos dados completos da amostra
amostra <- getdata(dados_populacao, amostra_ids)
head(amostra, 8)

# -------------------------------------------------------
# Fator de correção para população finita (FPC)
# -------------------------------------------------------
fpc <- rep(freq_estratos, alocacao_final)
fpc

# -------------------------------------------------------
# Estimações com o pacote survey
# -------------------------------------------------------
library(survey)

cat("============================================================ \n",
    "CÁLCULOS COM FPC (Finite Population Correction)\n",
    "============================================================")

# Criar plano amostral com FPC
plano_amostral <- svydesign(
  id = ~1,
  strata = ~Stratum,
  probs = ~amostra_ids$Prob,
  data = amostra,
  fpc = ~fpc
)

# Estimar média e total populacional da variável de interesse
media_populacional <- svymean(formula_estimacao, plano_amostral)
total_populacional <- svytotal(formula_estimacao, plano_amostral)

media_populacional
total_populacional

# Estimativas por estrato (com FPC)
svyby(formula_estimacao, by = ~Stratum, design = plano_amostral, FUN = svymean)
svyby(formula_estimacao, by = ~Stratum, design = plano_amostral, FUN = svytotal)

cat("============================================================ \n",
    "CÁLCULOS SEM FPC (maior erro padrão esperado)\n",
    "============================================================")

# Criar plano amostral sem FPC
plano_amostral <- svydesign(
  id = ~1,
  strata = ~Stratum,
  probs = ~amostra_ids$Prob,
  data = amostra
)

# Estimar média e total populacional (sem FPC)
media_populacional <- svymean(formula_estimacao, plano_amostral)
total_populacional <- svytotal(formula_estimacao, plano_amostral)

media_populacional
total_populacional

# Estimativas por estrato (sem FPC)
svyby(formula_estimacao, by = ~Stratum, design = plano_amostral, FUN = svymean)
svyby(formula_estimacao, by = ~Stratum, design = plano_amostral, FUN = svytotal)
