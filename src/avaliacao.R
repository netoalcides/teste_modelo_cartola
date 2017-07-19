#############################################################################
###################### Carrega Resultados das Análises ######################
#############################################################################

load( "reports/resultados_times.RData" )

######################################################################
###################### Avaliacao dos resultados ######################
######################################################################

# Resultados gerais

resultados_times %>% 
  group_by( escalacao ) %>% 
  summarise( pontuacao_esperada = sum(pontuacao_esperada),
             pontuacao_real = sum(pontuacao_real),
             min_rodada = min(rodada),
             max_rodada = max(rodada) ) %>% 
  arrange( desc(pontuacao_real) )


# Resultados por escalação e rodada

resultados_times %>% 
  arrange( escalacao, rodada )


# Resultados por escalação específica

resultados_times %>% 
  distinct( escalacao )

resultados_times %>% 
  filter( escalacao == "esc_4_3_3" )

# Resultados por rodada específica

resultados_times %>% 
  distinct( rodada )

resultados_times %>% 
  filter( rodada == 13) %>% 
  arrange( desc(pontuacao_real) )


# Visão pontuação esperada

resultados_times %>% 
  ggplot( aes( x = rodada, 
               y = pontuacao_esperada, 
               group = escalacao, 
               colour = escalacao ) ) +
  geom_line()

# Visão pontuação esperada

resultados_times %>% 
  ggplot( aes( x = rodada, 
               y = pontuacao_real, 
               group = escalacao, 
               colour = escalacao ) ) +
  geom_line()


# Visão pontuação esperada x real 3-4-3

resultados_times %>% 
  filter( escalacao == "esc_3_4_3" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")


# Visão pontuação esperada x real 3-5-2

resultados_times %>% 
  filter( escalacao == "esc_3_5_2" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")


# Visão pontuação esperada x real 4-3-3

bind_rows( resultados_times ) %>% 
  filter( escalacao == "esc_4_3_3" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) )


# Visão pontuação esperada x real 4-4-2

resultados_times %>% 
  filter( escalacao == "esc_4_4_2" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")


# Visão pontuação esperada x real 4-5-1

resultados_times %>% 
  filter( escalacao == "esc_4_5_1" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")

# Visão pontuação esperada x real 5-3-2

resultados_times %>% 
  filter( escalacao == "esc_5_3_2" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")

# Visão pontuação esperada x real 5-4-1

resultados_times %>% 
  filter( escalacao == "esc_5_4_1" ) %>% 
  ggplot( aes( x = rodada ) ) +
  geom_line( aes( y = pontuacao_esperada, colour = "pontuacao_esperada" ) ) +
  geom_line( aes( y = pontuacao_real, colour = "pontuacao_real" ) ) +
  ylab("pontuação")


