
periodos <- projecoes %>%
  count( rodada_id ) %>%
  select( rodada_id ) %>%
  data.frame

resultados_times <- list()

for( i in periodos$rodada_id ){
  
  times_sugeridos <- bind_rows(
    bind_rows(
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "tec") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "gol") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "zag") %>% 
        head(3),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "mei") %>% 
        head(4),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "ata") %>% 
        head(3) ) %>% 
      mutate( escalacao = "esc_3_4_3" ),
    bind_rows(
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "tec") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "gol") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "zag") %>% 
        head(3),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "mei") %>% 
        head(5),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "ata") %>% 
        head(2) ) %>% 
      mutate( escalacao = "esc_3_5_2" ),
    bind_rows(
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "tec") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "gol" ) %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "zag" ) %>% 
        head(2),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "lat" ) %>% 
        head(2),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "mei" ) %>% 
        head(3),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "ata" ) %>% 
        head(3) ) %>% 
      mutate( escalacao = "esc_4_3_3" ),
    bind_rows(
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "tec") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "gol") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "zag") %>% 
        head(2),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "lat") %>% 
        head(2),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "mei") %>% 
        head(4),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "ata") %>% 
        head(2) ) %>% 
      mutate( escalacao = "esc_4_4_2" ),
    bind_rows(
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "tec") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "gol") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "zag") %>% 
        head(2),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "lat") %>% 
        head(2),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "mei") %>% 
        head(5),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "ata") %>% 
        head(1) ) %>% 
      mutate( escalacao = "esc_4_5_1" ),
    bind_rows(
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "tec") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "gol") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "zag") %>% 
        head(3),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "lat") %>% 
        head(2),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "mei") %>% 
        head(3),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "ata") %>% 
        head(2) ) %>% 
      mutate( escalacao = "esc_5_3_2" ),
    bind_rows(
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "tec") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "gol") %>% 
        head(1),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "zag") %>% 
        head(3),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "lat") %>% 
        head(2),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "mei") %>% 
        head(4),
      projecoes %>% 
        filter( rodada_id == i,
                posicao == "ata") %>% 
        head(1) ) %>% 
      mutate( escalacao = "esc_5_4_1" )
  )
  
  
  resultados_times[[length(resultados_times)+1]] <- times_sugeridos %>% 
    group_by( escalacao ) %>% 
    summarise( pontuacao_esperada = sum( pontos_num_pred ),
               pontuacao_real = sum( pontos_num ),
               valor_time_antes = sum( preco_num_anterior ),
               valor_time_apos = sum( preco_num ),
               rodada = max(rodada_id) ) %>% 
      data.frame
   
}

resultados_times <- bind_rows( resultados_times )

save( resultados_times, file = "reports/resultados_times.RData")

file.remove( list.files("./reports",
                        pattern = "rodada",
                        full.names = TRUE) )


