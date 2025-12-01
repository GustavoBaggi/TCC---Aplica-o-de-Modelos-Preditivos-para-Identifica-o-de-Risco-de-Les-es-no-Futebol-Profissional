################################################################################
# TCC: Aplicação de Modelos Preditivos para Identificação de Risco de Lesões no 
# Futebol Profissional
# Autor: Gustavo Marques de Matos Baggi
# Data: 23/10/2025
# 
# SCRIPT MESTRE CONSOLIDADO
# 
# Estrutura:
#   0.0. Preparação e Carregamento
#   2.0. Coleta e Tratamento de Dados (Web Scraping + CORREÇÃO DE ERROS)
#   3.0. Análise Descritiva e Exploratória
#   4.0. Modelagem Preditiva
#   5.0. Resultados e Discussão
################################################################################

# ==============================================================================
# 0.0. PREPARAÇÃO E CARREGAMENTO DE PACOTES
# ==============================================================================

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  # 1. Manipulação e Web Scraping
  tidyverse, rvest, httr, lubridate, readxl, writexl,
  
  # 2. Visualização e Estatística
  skimr, ggcorrplot, patchwork, gridExtra, grid, scales,
  
  # 3. Modelagem (Tidymodels)
  tidymodels, vip,
  
  # 4. Motores (Engines)
  poissonreg, MASS, glmnet, rpart, ranger, xgboost, kernlab, kknn, nnet,
  
  # 5. Ensemble
  stacks
)

# Configuração Global de Tema
theme_set(theme_minimal(base_size = 14) + 
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.title = element_text(face = "bold"),
              legend.text = element_text(size = 12)
            ))

# Cores Personalizadas
cor_azul     <- "#619cff"
cor_vermelho <- "#f87067"

# Chave para rodar o scraping (TRUE) ou carregar Excel pronto (FALSE)
RODAR_SCRAPING <- FALSE 


# ==============================================================================
# CAPÍTULO 2: COLETA E TRATAMENTO DOS DADOS
# ==============================================================================

print("--- Iniciando Capítulo 2: Coleta e Tratamento ---")

# --- 2.1. Definição das Funções de Extração ---

extrair_dados_perfil <- function(link) {
  perfil <- tryCatch(read_html(link), error = function(e) return(NULL))
  if (is.null(perfil)) {
    return(tibble(altura=NA_real_, idade=NA_integer_, pe_preferido=NA_character_, 
                  posicao=NA_character_, nacionalidade=NA_character_))
  }
  altura <- perfil %>% html_elements(xpath = "//span[contains(text(), 'Height')]/following-sibling::span[1]") %>%
    html_text(trim = TRUE) %>% str_replace(",", ".") %>% str_replace_all("[[:space:]]", "") %>% str_remove("m$") %>% as.numeric()
  idade <- perfil %>% html_elements(xpath = "//span[contains(text(), 'Date of birth')]/following-sibling::span[1]") %>%
    html_text(trim = TRUE) %>% str_extract("\\(\\d+\\)") %>% str_remove_all("[\\(\\)]") %>% as.integer()
  pe_preferido <- perfil %>% html_elements(xpath = "//span[contains(text(), 'Foot')]/following-sibling::span[1]") %>% html_text(trim = TRUE)
  posicao <- perfil %>% html_elements(xpath = "//span[contains(text(), 'Position')]/following-sibling::span[1]") %>% html_text(trim = TRUE)
  nacionalidade <- perfil %>% html_elements(xpath = "//span[contains(text(), 'Citizenship')]/following-sibling::span[1]//img") %>%
    html_attr("title") %>% paste(collapse = ", ")
  tibble(altura, idade, pe_preferido, posicao, nacionalidade)
}

extrair_lesoes_jogador <- function(link, temporada) {
  link_lesoes <- str_replace(link, "/profil/", "/verletzungen/")
  pagina <- tryCatch(read_html(link_lesoes), error = function(e) return(NULL))
  if (is.null(pagina)) return(tibble(n_lesoes=NA_integer_, dias_lesionado=NA_real_, jogos_perdidos=NA_integer_))
  tabela_node <- pagina %>% html_element(xpath = '//table[contains(@class, "items")]')
  if (is.null(tabela_node)) return(tibble(n_lesoes=0, dias_lesionado=0, jogos_perdidos=0))
  tabela <- tryCatch(html_table(tabela_node, fill = TRUE), error = function(e) return(NULL))
  if (is.null(tabela)) return(tibble(n_lesoes=0, dias_lesionado=0, jogos_perdidos=0))
  colnames(tabela) <- make.unique(make.names(colnames(tabela)))
  temp_str <- paste0(substr(temporada, 3, 4), "/", substr(temporada + 1, 3, 4))
  filtrada <- tabela %>% filter(grepl(temp_str, Season)) %>%
    mutate(Days = as.numeric(str_extract(Days, "\\d+")), GamesMissed = as.numeric(str_extract(Games.missed, "\\d+")))
  tibble(n_lesoes = nrow(filtrada), dias_lesionado = sum(filtrada$Days, na.rm = TRUE), jogos_perdidos = sum(filtrada$GamesMissed, na.rm = TRUE))
}

extrair_dados_jogador_estatisticas <- function(link, temporada) {
  nome_jogador <- str_extract(link, "com/(.*?)/") %>% str_remove_all("com/|/")
  id_jogador <- str_extract(link, "spieler/\\d+") %>% str_remove("spieler/")
  extrair_stats <- function(season) {
    url <- paste0("https://www.transfermarkt.com/", nome_jogador, "/leistungsdaten/spieler/", id_jogador, "/plus/0?saison=", season)
    pagina <- tryCatch(read_html(url), error = function(e) return(NULL))
    if (is.null(pagina)) return(list(jogos=0, minutos=0))
    tabela <- tryCatch(html_table(pagina %>% html_element("//table[contains(@class, 'items')]"), fill = TRUE), error = function(e) NULL)
    if (is.null(tabela)) return(list(jogos=0, minutos=0))
    linha <- tabela %>% filter(str_detect(.[[1]], regex("Total", ignore_case = TRUE)))
    if (nrow(linha) > 0) {
      list(jogos = as.numeric(gsub("[^0-9]", "", linha[[4]][1])), minutos = as.numeric(gsub("[^0-9]", "", linha[[ncol(linha)]][1])))
    } else { list(jogos=0, minutos=0) }
  }
  s_ant <- extrair_stats(temporada - 1); s_atual <- extrair_stats(temporada)
  tibble(jogos_temporada_anterior = replace_na(s_ant$jogos, 0), minutos_temporada_anterior = replace_na(s_ant$minutos, 0),
         jogos_temporada_atual = replace_na(s_atual$jogos, 0), minutos_temporada_atual = replace_na(s_atual$minutos, 0))
}

teve_lesao_ano_passado <- function(link, temporada) {
  temp_ant <- temporada - 1
  link_lesoes <- str_replace(link, "/profil/", "/verletzungen/")
  pagina <- tryCatch(read_html(link_lesoes), error = function(e) return(NULL))
  if (is.null(pagina)) return(NA)
  tabela <- tryCatch(html_table(pagina %>% html_element('//table[contains(@class, "items")]'), fill = TRUE), error = function(e) NULL)
  if (is.null(tabela)) return(FALSE)
  temp_str <- paste0(substr(temp_ant, 3, 4), "/", substr(temp_ant + 1, 3, 4))
  any(grepl(temp_str, tabela$Season))
}

extrair_dados_elenco <- function(team_id, season, team_name) {
  url <- paste0("https://www.transfermarkt.com/", team_name, "/kader/verein/", team_id, "/saison_id/", season, "/plus/1")
  page <- read_html(url)
  players <- page %>% html_nodes(xpath = "//table[contains(@class, 'items')]/tbody/tr") %>%
    map_df(function(row) {
      node <- row %>% html_node(".hauptlink a")
      tibble(nome = node %>% html_text(trim = TRUE), link = paste0("https://www.transfermarkt.com", node %>% html_attr("href")))
    })
  print(paste("Extraindo", nrow(players), "jogadores do time:", team_name))
  dados <- players %>% mutate(
    perfil = map(link, ~ { Sys.sleep(30); extrair_dados_perfil(.x) }),
    lesoes = map(link, ~ { Sys.sleep(10); extrair_lesoes_jogador(.x, season) }),
    stats  = map(link, ~ { Sys.sleep(10); extrair_dados_jogador_estatisticas(.x, season) })
  ) %>% unnest(c(perfil, lesoes, stats)) %>% mutate(team = team_name, season = season)
  return(dados)
}

# --- FUNÇÕES DE CORREÇÃO (ADICIONADAS) ---

filtrar_jogadores_sem_lesoes <- function(df) {
  idx <- which(df$n_lesoes == 0)
  tibble(linha = idx, link = df$link[idx])
}

corrigir_lesoes_zeradas <- function(df, temporada) {
  idx <- which(df$n_lesoes == 0 | is.na(df$n_lesoes))
  for (i in idx) {
    cat("Corrigindo lesões - linha", i, "->", df$nome[i], "\n")
    lesoes <- extrair_lesoes_jogador(df$link[i], temporada)
    if (nrow(lesoes) == 1) {
      df[i, c("n_lesoes", "dias_lesionado", "jogos_perdidos")] <- lesoes
    } else {
      cat("  Nenhuma informação de lesão encontrada para:", df$nome[i], "\n")
    }
    Sys.sleep(30)
  }
  return(df)
}


# --- 2.2. Execução da Extração (20 Times) ---

if (RODAR_SCRAPING) {
  print("--- Iniciando Scraping dos 20 times ---")
  
  # Extração Time a Time
  d_arsenal   <- extrair_dados_elenco(11, 2023, "arsenal-fc")
  d_mancity   <- extrair_dados_elenco(281, 2023, "manchester-city")
  d_chelsea   <- extrair_dados_elenco(631, 2023, "chelsea-fc")
  d_liverpool <- extrair_dados_elenco(31, 2023, "liverpool-fc")
  d_tottenham <- extrair_dados_elenco(148, 2023, "tottenham-hotspur")
  d_manutd    <- extrair_dados_elenco(985, 2023, "manchester-united")
  d_newcastle <- extrair_dados_elenco(762, 2023, "newcastle-united")
  d_villa     <- extrair_dados_elenco(405, 2023, "aston-villa")
  d_westham   <- extrair_dados_elenco(379, 2023, "west-ham-united")
  d_brighton  <- extrair_dados_elenco(1237, 2023, "brighton-amp-hove-albion")
  d_fulham    <- extrair_dados_elenco(931, 2023, "fulham-fc")
  d_palace    <- extrair_dados_elenco(873, 2023, "crystal-palace")
  d_wolves    <- extrair_dados_elenco(543, 2023, "wolverhampton-wanderers")
  d_brentford <- extrair_dados_elenco(1148, 2023, "brentford-fc")
  d_bournemouth <- extrair_dados_elenco(989, 2023, "afc-bournemouth")
  d_everton   <- extrair_dados_elenco(29, 2023, "everton-fc")
  d_forest    <- extrair_dados_elenco(703, 2023, "nottingham-forest")
  d_luton     <- extrair_dados_elenco(1031, 2023, "luton-town")
  d_burnley   <- extrair_dados_elenco(1132, 2023, "burnley-fc")
  d_sheffield <- extrair_dados_elenco(350, 2023, "sheffield-united")
  
  # --- ETAPA DE CORREÇÃO (NOVO) ---
  print("Aplicando verificação de lesões zeradas...")
  
  d_arsenal   <- corrigir_lesoes_zeradas(d_arsenal, 2023)
  d_mancity   <- corrigir_lesoes_zeradas(d_mancity, 2023)
  d_chelsea   <- corrigir_lesoes_zeradas(d_chelsea, 2023)
  d_liverpool <- corrigir_lesoes_zeradas(d_liverpool, 2023)
  d_tottenham <- corrigir_lesoes_zeradas(d_tottenham, 2023)
  d_manutd    <- corrigir_lesoes_zeradas(d_manutd, 2023)
  d_newcastle <- corrigir_lesoes_zeradas(d_newcastle, 2023)
  d_villa     <- corrigir_lesoes_zeradas(d_villa, 2023)
  d_westham   <- corrigir_lesoes_zeradas(d_westham, 2023)
  d_brighton  <- corrigir_lesoes_zeradas(d_brighton, 2023)
  d_fulham    <- corrigir_lesoes_zeradas(d_fulham, 2023)
  d_palace    <- corrigir_lesoes_zeradas(d_palace, 2023)
  d_wolves    <- corrigir_lesoes_zeradas(d_wolves, 2023)
  d_brentford <- corrigir_lesoes_zeradas(d_brentford, 2023)
  d_bournemouth <- corrigir_lesoes_zeradas(d_bournemouth, 2023)
  d_everton   <- corrigir_lesoes_zeradas(d_everton, 2023)
  d_forest    <- corrigir_lesoes_zeradas(d_forest, 2023)
  d_luton     <- corrigir_lesoes_zeradas(d_luton, 2023)
  d_burnley   <- corrigir_lesoes_zeradas(d_burnley, 2023)
  d_sheffield <- corrigir_lesoes_zeradas(d_sheffield, 2023)
  
  # Unificação
  print("Unificando base...")
  dados_completos_2023 <- bind_rows(
    d_arsenal, d_mancity, d_chelsea, d_liverpool, d_tottenham,
    d_manutd, d_newcastle, d_villa, d_westham, d_brighton,
    d_fulham, d_palace, d_wolves, d_brentford, d_bournemouth,
    d_everton, d_forest, d_luton, d_burnley, d_sheffield
  )
  
  # Adicionar variável 'teve_lesao_ano_passado'
  print("Calculando histórico de lesões (passo final)...")
  dados_completos_2023$teve_lesao_ano_passado <- NA
  for (i in 1:nrow(dados_completos_2023)) {
    dados_completos_2023$teve_lesao_ano_passado[i] <- teve_lesao_ano_passado(dados_completos_2023$link[i], 2023)
  }
  
  write_xlsx(dados_completos_2023, "dados_completos_2023_raw.xlsx")
  dados_brutos <- dados_completos_2023
  
} else {
  print("Pulo do Scraping: Carregando base de dados local...")
  # Ajuste o caminho se necessário
  dados_brutos <- read_excel("C:/Users/bagGi/OneDrive/Documentos/TCC/dados_completos_2023_atualizado.xlsx")
}

# --- 2.3. Tratamento Final dos Dados ---

print("Finalizando tratamento dos dados...")

dados <- dados_brutos %>%
  dplyr::select(
    altura, idade, nacionalidade, posicao, pe_preferido,
    jogos_temporada_anterior, minutos_temporada_anterior,
    teve_lesao_ano_passado, n_lesoes
  ) %>%
  mutate(
    nacionalidade = str_trim(str_split_fixed(nacionalidade, ",", 2)[, 1]),
    posicao = case_when(
      posicao == "Goalkeeper" ~ "Goleiro",
      posicao == "Defender - Centre-Back" ~ "Zagueiro",
      posicao %in% c("Defender - Left-Back", "Defender - Right-Back") ~ "Lateral",
      str_detect(posicao, "Midfield") ~ "Meio-Campo",
      str_detect(posicao, "Attack") ~ "Atacante",
      TRUE ~ "Outro"
    ),
    nacionalidade = as.factor(nacionalidade),
    posicao = as.factor(posicao),
    pe_preferido = as.factor(pe_preferido),
    # Converte lógico para inteiro
    teve_lesao_ano_passado = as.integer(as.logical(teve_lesao_ano_passado))
  )

write_xlsx(dados, "dados_completos_2023_selecionados.xlsx")
print(paste("Base pronta com", nrow(dados), "jogadores."))


# ==============================================================================
# CAPÍTULO 3: ANÁLISE DESCRITIVA E EXPLORATÓRIA
# ==============================================================================

# --- 3.1. Caracterização da Base ---
print(paste("Número total de jogadores:", nrow(dados)))
skim(dados)

# --- 3.2. Visualização Gráfica ---

# 3.2.1. Resposta
p_resp <- ggplot(dados, aes(x = factor(n_lesoes), fill = factor(n_lesoes))) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 5) +
  labs(title = "Distribuição da Variável Resposta (Número de Lesões)",
       x = "Número de Lesões na Temporada 2023/24",
       y = "Número de Jogadores")

# 3.2.2. Numéricas
vars_num <- c("altura", "idade", "jogos_temporada_anterior", "minutos_temporada_anterior")
p_num <- dados %>% dplyr::select(all_of(vars_num)) %>%
  pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 20, fill = cor_azul, alpha = 0.8) +
  facet_wrap(~ variavel, scales = "free") +
  labs(title = "Distribuição das Variáveis Preditivas Numéricas", x = "Valor", y = "Nº Jogadores") +
  theme(strip.text = element_text(face = "bold"))

# 3.2.3. Categóricas
p1 <- dados %>% count(posicao) %>% mutate(perc = n/sum(n)) %>%
  ggplot(aes(x = fct_reorder(posicao, n, .desc=TRUE), y = n, fill = posicao)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(perc, 0.1)), vjust = 1.5, color = "white", size = 4) +
  labs(x = "Posição", y = NULL) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- dados %>% count(pe_preferido) %>% mutate(perc = n/sum(n)) %>%
  ggplot(aes(x = fct_reorder(pe_preferido, n, .desc=TRUE), y = n, fill = pe_preferido)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(perc, 0.1)), vjust = 1.5, color = "white", size = 4) +
  labs(x = "Pé Preferido", y = NULL)

p3 <- dados %>% count(teve_lesao_ano_passado) %>% mutate(perc = n/sum(n)) %>%
  ggplot(aes(x = teve_lesao_ano_passado, y = n, fill = teve_lesao_ano_passado)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(perc, 0.1)), vjust = 1.5, color = "white", size = 4) +
  labs(x = "Lesão Anterior", y = NULL) + scale_fill_manual(values = c("TRUE"=cor_vermelho, "FALSE"=cor_azul))

dados_pais <- dados %>% mutate(pais_principal = str_split_i(nacionalidade, ",", 1) %>% str_trim())
p4 <- dados_pais %>% 
  mutate(pais_lump = fct_lump_n(pais_principal, n = 12)) %>% count(pais_lump) %>% mutate(perc = n/sum(n)) %>%
  ggplot(aes(x = fct_reorder(pais_lump, n, .desc=TRUE), y = n, fill = pais_lump)) +
  geom_col(show.legend = FALSE) +
  geom_text(data = . %>% slice_max(n, n=2), aes(label = scales::percent(perc, 0.1)), 
            vjust = 0.5, hjust = 1.25, color = "white", size = 4, angle = 90) +
  labs(x = "Nacionalidade", y = NULL) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

shared_y <- textGrob("Número de Jogadores", rot = 90, vjust = 0.5, gp = gpar(fontsize = 14, fontface = "bold"))
shared_title <- textGrob("Distribuição das Variáveis Preditivas Categóricas", hjust = 0.5, gp = gpar(fontsize = 16, fontface = "bold"))
g_categ <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2, left = shared_y, top = shared_title)

# 3.2.4. Correlação
p_corr <- ggcorrplot(cor(dados %>% dplyr::select(all_of(vars_num), n_lesoes), use = "complete.obs"),
                     method = "square", type = "lower", lab = TRUE, lab_size = 5, colors = c(cor_vermelho, "white", cor_azul)) +
  labs(title = "Matriz de Correlação") + theme(plot.title = element_text(size=16))

# 3.2.5. Relação
p_scatter <- dados %>% dplyr::select(all_of(vars_num), n_lesoes) %>%
  pivot_longer(cols = -n_lesoes) %>%
  ggplot(aes(x = value, y = n_lesoes)) +
  geom_jitter(alpha = 0.3, width = 0.1) + geom_smooth(method = "lm", se = FALSE, color = cor_vermelho) +
  facet_wrap(~ name, scales = "free_x") + labs(title = "Preditores Numéricos vs. Lesões")

p_box_pos <- ggplot(dados, aes(x = fct_reorder(posicao, n_lesoes, .fun = median), y = n_lesoes, fill = posicao)) +
  geom_boxplot(show.legend = FALSE) + labs(title = "Lesões por Posição") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_box_hist <- ggplot(dados, aes(x = factor(teve_lesao_ano_passado), y = n_lesoes, fill = factor(teve_lesao_ano_passado))) +
  geom_boxplot(show.legend = FALSE) + labs(title = "Lesões por Histórico") + scale_fill_manual(values = c("1"=cor_vermelho, "0"=cor_azul))

p_box_pe <- ggplot(dados, aes(x = pe_preferido, y = n_lesoes, fill = pe_preferido)) + geom_boxplot(show.legend = FALSE) + labs(title = "Lesões por Pé")

p_box_nac <- ggplot(dados_pais, aes(x = fct_reorder(fct_lump_n(pais_principal, 12), n_lesoes), y = n_lesoes, fill = fct_lump_n(pais_principal, 12))) +
  geom_boxplot(show.legend = FALSE) + labs(title = "Lesões por Nacionalidade") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 3.3. Salvamento ---
if (!dir.exists("imagens_tcc")) dir.create("imagens_tcc")
ggsave("imagens_tcc/fig1.pdf", p_resp, width=8, height=6); ggsave("imagens_tcc/fig2.pdf", p_num, width=10, height=8)
ggsave("imagens_tcc/fig3.pdf", g_categ, width=12, height=10); ggsave("imagens_tcc/fig4.pdf", p_corr, width=8, height=8)
ggsave("imagens_tcc/fig5.pdf", p_scatter, width=10, height=8); ggsave("imagens_tcc/fig6.pdf", p_box_pos, width=10, height=6)
ggsave("imagens_tcc/fig7.pdf", p_box_hist, width=6, height=6); ggsave("imagens_tcc/fig8.pdf", p_box_pe, width=6, height=6)
ggsave("imagens_tcc/fig9.pdf", p_box_nac, width=12, height=6)


# ==============================================================================
# CAPÍTULO 4: MODELAGEM PREDITIVA
# ==============================================================================

# --- 4.1. Divisão ---
set.seed(793127) 
split <- initial_split(dados, prop = 3/4, strata = n_lesoes)
treino <- training(split); teste <- testing(split)
cv <- vfold_cv(treino, v = 10, strata = n_lesoes)

# --- 4.2. Lasso ---
rec <- recipe(n_lesoes ~ ., data = treino) %>%
  step_other(nacionalidade, threshold = 0.05) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% step_zv(all_predictors())

spec_lasso <- poisson_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet")
wf_lasso <- workflow() %>% add_recipe(rec) %>% add_model(spec_lasso)
res_lasso <- tune_grid(wf_lasso, resamples = cv, grid = 10, metrics = metric_set(rmse))
best_lambda <- select_best(res_lasso, "rmse")
final_lasso <- finalize_workflow(wf_lasso, best_lambda) %>% fit(data = treino)
vars_lasso <- final_lasso %>% extract_fit_parsnip() %>% tidy() %>% filter(estimate != 0, term != "(Intercept)") %>% pull(term)

# --- 4.3. Modelos (4 Selecionados) ---
rec_final <- rec %>% step_select(all_outcomes(), any_of(vars_lasso), matches("^idade$"), starts_with("posicao"))

mod_poisson <- poisson_reg() %>% set_engine("glm") %>% set_mode("regression")
mod_tree <- decision_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>% set_engine("rpart") %>% set_mode("regression")
mod_rf <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>% set_engine("ranger", importance = "permutation") %>% set_mode("regression")
mod_knn <- nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>% set_engine("kknn") %>% set_mode("regression")

wf_set <- workflow_set(preproc = list(rec = rec_final), models = list(poisson=mod_poisson, arvore=mod_tree, rf=mod_rf, knn=mod_knn), cross = TRUE)

# --- 4.4. Treinamento ---
ctrl <- control_grid(save_pred = TRUE)
print("Treinando modelos...")
res_treino <- wf_set %>% workflow_map("tune_grid", resamples = cv, metrics = metric_set(rmse, mae), grid = 10, control = ctrl, verbose = TRUE)


# ==============================================================================
# CAPÍTULO 5: RESULTADOS E DISCUSSÃO
# ==============================================================================

# --- 5.1. Ranking (CV) ---
rank <- rank_results(res_treino, rank_metric = "rmse", select_best = TRUE) %>%
  dplyr::select(model, .metric, mean, std_err, rank) %>% filter(.metric == "rmse") %>%
  mutate(mean = round(mean, 4))
print(rank)

# Tabela Hiperparâmetros
params <- collect_metrics(res_treino)
garantir <- function(df, c) { if(!c %in% names(df)) df[[c]] <- NA; df }
params <- params %>% garantir("cost_complexity") %>% garantir("tree_depth") %>% garantir("min_n") %>%
  garantir("mtry") %>% garantir("neighbors") %>% garantir("dist_power")

best_params <- rank_results(res_treino, rank_metric="rmse", select_best=TRUE) %>%
  filter(.metric=="rmse") %>%
  left_join(params, by=c("wflow_id", ".config", ".metric")) %>%
  dplyr::select(model, mean, std_err, contains(c("cost", "tree", "min", "mtry", "neigh", "dist")))
print(best_params)

# Gráfico Comparativo
graf_dados <- rank %>% mutate(model = case_when(
  str_detect(model, "poisson") ~ "Poisson", str_detect(model, "decision") ~ "Árvore",
  str_detect(model, "rand") ~ "Random Forest", TRUE ~ "KNN")) %>%
  mutate(model = fct_reorder(model, mean))

p_comp <- ggplot(graf_dados, aes(x = mean, y = model, color = model)) +
  geom_point(size = 4) + geom_errorbarh(aes(xmin=mean-std_err, xmax=mean+std_err), height=0.2) +
  geom_text(aes(label=mean), vjust=-1.5) + labs(title="Melhores Modelos (RMSE)", x="RMSE", y=NULL) +
  theme(legend.position="none")
print(p_comp); ggsave("imagens_tcc/fig_cap5_comp.pdf", p_comp, width=8, height=5)

# --- 5.2. Teste Final ---
best_id <- rank$wflow_id[1]
final_wf <- extract_workflow(res_treino, best_id) %>%
  finalize_workflow(select_best(extract_workflow_set_result(res_treino, best_id), "rmse"))

last_fit_res <- last_fit(final_wf, split, metrics = metric_set(rmse, mae, rsq))
print(collect_metrics(last_fit_res))

# --- 5.3. Diagnóstico ---
preds <- collect_predictions(last_fit_res)
p_diag <- ggplot(preds, aes(x = factor(n_lesoes), y = .pred, fill = factor(n_lesoes))) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  labs(title = "Predito vs Observado (Teste)", x = "Real", y = "Previsto")
print(p_diag); ggsave("imagens_tcc/fig_cap5_diag.pdf", p_diag, width=7, height=6)

# --- 5.4. VIP ---
if (str_detect(best_id, "rf|arvore")) {
  vip_plot <- vip(extract_fit_parsnip(last_fit_res), num_features = 15, geom = "col", aesthetics = list(fill = cor_azul)) +
    labs(title = "Importância das Variáveis")
  print(vip_plot); ggsave("imagens_tcc/fig_cap5_vip.pdf", vip_plot, width=8, height=6)
}

print("--- FIM ---")
