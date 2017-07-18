load( "reports/resultados_times.RData" )

# Avaliacao dos resultados

resultados_times %>% 
  group_by( escalacao ) %>% 
  summarise( pontuacao_esperada = sum(pontuacao_esperada),
             pontuacao_real = sum(pontuacao_real),
             min_rodada = min(rodada),
             max_rodada = max(rodada) ) %>% 
  arrange( desc(pontuacao_real) )


resultados_times %>% 
  arrange( escalacao, rodada )

resultados_times %>% 
  distinct( escalacao )

resultados_times %>% 
  filter( escalacao == "esc_4_3_3" )

resultados_times %>% 
  distinct( rodada )

resultados_times %>% 
  filter( rodada == 13) %>% 
  arrange( desc(pontuacao_real) )


resultados_times %>% 
  ggplot( aes( x = rodada, 
               y = pontuacao_real, 
               group = escalacao, 
               colour = escalacao ) ) +
  geom_line()


resultados_times %>% 
  ggplot( aes( x = rodada, 
               y = pontuacao_esperada, 
               group = escalacao, 
               colour = escalacao ) ) +
  geom_line()


resultados_times %>% 
  filter( escalacao == "esc_3_4_3" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")

resultados_times %>% 
  filter( escalacao == "esc_3_5_2" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")

bind_rows( resultados_times ) %>% 
  filter( escalacao == "esc_4_3_3" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) )

resultados_times %>% 
  filter( escalacao == "esc_4_4_2" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")

resultados_times %>% 
  filter( escalacao == "esc_4_5_1" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")

resultados_times %>% 
  filter( escalacao == "esc_5_3_2" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")

resultados_times %>% 
  filter( escalacao == "esc_5_4_1" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")


