info( logger, "CARTOLA_2017::ajusta dados do dataset" )

dados_cartola_2017_por_rodada %<>%
  arrange( atleta_id, rodada_id ) %>%
  select( atleta_id, FC, PE, RB, SG, CA, FD, FS, FF, I, G, DD, GS, A, CV, FT, GC, DP, PP ) %>%
  group_by( atleta_id ) %>%
  mutate_all( funs( ajuste_lag(.) ) ) %>%
  bind_cols( dados_cartola_2017_por_rodada %>%
               arrange( atleta_id, rodada_id ) %>%
               select( -atleta_id, -FC, -PE, -RB, -SG, -CA, -FD, -FS, -FF, -I, -G, -DD, -GS,
                       -A, -CV, -FT, -GC, -DP, -PP ), .) %>%
  mutate( apelido = replace( apelido, apelido == "Patrick", "Patrick Correia" ) ) %>%
  mutate( apelido = replace( apelido, apelido == "Vinicius Júnior", "Vinicius Junior" ) ) %>%
  mutate( apelido = replace( apelido, apelido == "Matheus Sávio", "Matheus Savio" ) ) %>%
  mutate( apelido = replace( apelido, apelido == "Mateus Pet", "Mateus Vital" ) ) %>%
  data.frame()


info( logger, "CARTOLA_2017::cache informacoes dos dados por rodada" )

cache( "dados_cartola_2017_por_rodada" )

rm(dados_cartola_2017_por_rodada)
