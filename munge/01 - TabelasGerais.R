info( logger, "CARTOLA_2017::obtem informacoes gerais dos jogadores" )

informacoes_jogadores <- dados_mercado$atletas[1:13] %>%
  data.frame() %>%
  select( -foto,
          -nome,
          -preco_num,
          -variacao_num )


info( logger, "CARTOLA_2017::obtem precos dos jogadores" )

precos_jogadores <- dados_mercado$atletas[1:13] %>%
  data.frame() %>%
  select( atleta_id,
          apelido,
          rodada_id,
          status_id,
          preco_num,
          variacao_num ) %>%
  mutate( rodada_id = rodada_id + 1,
          preco_num_anterior = preco_num - variacao_num )


info( logger, "CARTOLA_2017::obtem labels posicoes" )

label_posicoes <- dados_mercado$posicoes %>%
  unlist() %>%
  matrix(., 3, 6) %>%
  t() %>%
  data.frame(., stringsAsFactors = FALSE ) %>%
  rename( posicao_id = X1,
          posicao = X3 ) %>%
  mutate( posicao_id = as.integer( posicao_id ) ) %>%
  select( -X2 )


info( logger, "CARTOLA_2017::obtem labels status" )

label_status <- dados_mercado$status %>%
  unlist() %>%
  matrix(., 2, 5) %>%
  t() %>%
  data.frame(., stringsAsFactors = FALSE ) %>%
  rename( status_id = X1,
          status = X2 ) %>%
  mutate( status_id = as.integer(status_id) )


info( logger, "CARTOLA_2017::obtem labels clubes" )

label_clubes <- dados_mercado[2] %>%
  unlist() %>%
  matrix(., 7, 20) %>%
  t() %>%
  data.frame(., stringsAsFactors = FALSE ) %>%
  rename( clube_id = X1,
          clube = X2,
          classificacao = X4) %>%
  mutate( clube_id = as.integer(clube_id),
          classificacao = as.integer(classificacao),
          rodada_id = informacoes_jogadores$rodada_id[1] + 1 ) %>%
  select( clube_id,
          clube,
          rodada_id,
          classificacao )


info( logger, "CARTOLA_2017::obtem scouts" )

scouts_jogadores <- dados_mercado$atletas$scout
scouts_jogadores[ is.na(scouts_jogadores ) ] <- 0

# # enquanto a API nao arruma esta merda
# scouts_jogadores %<>%
#   mutate( GC = 0 )

info( logger, "CARTOLA_2017::obtem partidas" )

informacoes_partidas <- dados_partidas[1] %>%
  data.frame() %>%
  select( -partidas.url_transmissao,
          -partidas.url_confronto )
rm(dados_partidas)
