#######################################################################
############################ Carrega dados ############################
#######################################################################

rodada <- 14

load( paste0( "cache/label_clubes_rodada_", rodada, ".RData" ) )
load( "cache/dados_cartola_2017_por_rodada.RData" )
load( paste0( "cache/informacoes_partidas_rodada_", rodada, ".RData" ) )

##########################################################################
####################### Salva csv rodadas passadas #######################
##########################################################################

write.csv( dados_cartola_2017_por_rodada, 
           file = paste0( "dados_ate_rodada_", rodada, ".csv" ) )


#########################################################################
####################### Prepara para rodada atual #######################
#########################################################################

dados_cartola_2017_por_rodada %<>%
  filter( rodada_id == rodada -1 ) %>% 
  select( atleta_id, apelido, clube_id, clube, posicao, status,
          pontos_num, media_num, jogos_num, preco_num, variacao_num, preco_num_anterior,
          GS, FC, FF, FS, I, PE, SG, CA, RB, DD, G, FD, A, FT, CV, GC, DP, PP )

informacoes_partidas %<>%
  right_join(., x = label_clubes, by = c("clube_id" = "partidas.clube_visitante_id") ) %>%
  rename(., partidas.clube_visitante_id = clube_id ) %>% 
  rename(., partidas.clube_visitante_nome = clube ) %>%
  right_join(., x = label_clubes, by = c("clube_id" = "partidas.clube_casa_id") ) %>%
  rename(., partidas.clube_casa_id = clube_id ) %>% 
  rename(., partidas.clube_casa_nome = clube ) %>%
  select( partidas.clube_casa_id,
          partidas.clube_casa_nome,
          partidas.clube_casa_posicao,
          partidas.clube_visitante_id,
          partidas.clube_visitante_nome,
          partidas.clube_visitante_posicao,
          partidas.partida_data,
          partidas.local )

casa  <- dados_cartola_2017_por_rodada %>% 
  right_join(., y = informacoes_partidas, 
             by = c( "clube_id" = "partidas.clube_casa_id" ) ) %>%
  mutate( partidas.clube_casa_id = clube_id )

fora <- dados_cartola_2017_por_rodada %>% 
  right_join(., y = informacoes_partidas, 
             by = c( "clube_id" = "partidas.clube_visitante_id" ) ) %>% 
  mutate( partidas.clube_visitante_id = clube_id )

dados_cartola_2017_por_rodada <- bind_rows(casa, fora) %>% 
  mutate( mandante = ifelse( clube_id == partidas.clube_casa_id, "S", "N") )

dados_cartola_2017_por_rodada %<>%
  select( atleta_id, apelido, clube_id, clube, posicao, status,
          partidas.clube_casa_nome, partidas.clube_casa_posicao,
          partidas.clube_visitante_id,partidas.clube_visitante_nome,
          partidas.clube_visitante_posicao, partidas.partida_data,
          partidas.local, partidas.clube_casa_id, mandante )


######################################################################
####################### Salva csv rodada atual #######################
######################################################################

write.csv( dados_cartola_2017_por_rodada, 
           file = paste0( "informacoes_partidas_", rodada, ".csv" ) )
