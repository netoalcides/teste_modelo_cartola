info( logger, "CARTOLA_2017::modelo treinadores" )

projecoes <- bind_rows(
  list.files("./reports", pattern = "atacantes_", full.names=TRUE) %>%
    lapply(., function(x) get( load(x) ) ) %>%
    bind_rows() %>%
    arrange( rodada ) %>%
    left_join(., y = dados_cartola_2017_por_rodada, by = "atleta_id" ) %>%
    filter( rodada == rodada_id ) %>% 
    select( atleta_id, apelido, clube_id, clube, posicao, rodada_id, preco_num, preco_num_anterior, pontos_num, pontos_num_pred ),
  list.files("./reports", pattern = "meiocampo_", full.names=TRUE) %>%
    lapply(., function(x) get( load(x) ) ) %>%
    bind_rows() %>%
    arrange( rodada ) %>%
    left_join(., y = dados_cartola_2017_por_rodada, by = "atleta_id" ) %>%
    filter( rodada == rodada_id ) %>% 
    select( atleta_id, apelido, clube_id, clube, posicao, rodada_id, preco_num, preco_num_anterior, pontos_num, pontos_num_pred ),
  list.files("./reports", pattern = "laterais_", full.names=TRUE) %>%
    lapply(., function(x) get( load(x) ) ) %>%
    bind_rows() %>%
    arrange( rodada ) %>%
    left_join(., y = dados_cartola_2017_por_rodada, by = "atleta_id" ) %>%
    filter( rodada == rodada_id ) %>% 
    select( atleta_id, apelido, clube_id, clube, posicao, rodada_id, preco_num, preco_num_anterior, pontos_num, pontos_num_pred ),
  list.files("./reports", pattern = "zagueiros_", full.names=TRUE) %>%
    lapply(., function(x) get( load(x) ) ) %>%
    bind_rows() %>%
    arrange( rodada ) %>%
    left_join(., y = dados_cartola_2017_por_rodada, by = "atleta_id" ) %>%
    filter( rodada == rodada_id ) %>% 
    select( atleta_id, apelido, clube_id, clube, posicao, rodada_id, preco_num, preco_num_anterior, pontos_num, pontos_num_pred ),
  list.files("./reports", pattern = "goleiros_", full.names=TRUE) %>%
    lapply(., function(x) get( load(x) ) ) %>%
    bind_rows() %>%
    arrange( rodada ) %>%
    left_join(., y = dados_cartola_2017_por_rodada, by = "atleta_id" ) %>%
    filter( rodada == rodada_id ) %>% 
    select( atleta_id, apelido, clube_id, clube, posicao, rodada_id, preco_num, preco_num_anterior, pontos_num, pontos_num_pred )
)


info( logger, "CARTOLA_2017::obtém projeções para treinadores" )

tecnicos <- dados_cartola_2017_por_rodada %>% 
  filter( posicao == "tec" ) %>% 
  arrange( clube, rodada_id ) %>% 
  select( atleta_id, apelido, clube, clube_id, posicao, rodada_id, preco_num, preco_num_anterior, pontos_num ) %>% 
  left_join(., y = projecoes %>% 
              group_by( clube_id, rodada_id ) %>% 
              summarise( pontos_num_pred = median(pontos_num_pred) ) %>% 
              data.frame(), 
            by = "clube_id") %>% 
  filter( rodada_id.x == rodada_id.y ) %>% 
  mutate( rodada_id = rodada_id.x ) %>% 
  select( -rodada_id.y,
          -rodada_id.x ) %>% 
  arrange( rodada_id, desc(pontos_num_pred) )

save( tecnicos, file = "reports/tecnicos_por_rodada.RData")

projecoes <- bind_rows( projecoes, tecnicos)

rm(tecnicos)
