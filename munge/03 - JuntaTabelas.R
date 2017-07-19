if ( atualiza == 1 ) {
  
  info( logger, "CARTOLA_2017::carrega informacoes para juntar as tabelas" )
  
  load( paste0( "cache/label_clubes_rodada_", rodada_atual - 1, ".RData") )
  
  load( paste0( "cache/informacoes_partidas_rodada_", rodada_atual - 1, ".RData") )
  
  label_clubes <- label_clubes %>%
    select( -rodada_id )
  
  precos_jogadores <- precos_jogadores %>%
    select( preco_num,
            variacao_num,
            preco_num_anterior )
  
  
  info( logger, "CARTOLA_2017::junta tabelas de informacoes dos jogadores" )
  
  informacoes_jogadores %<>%
    right_join(., x = label_posicoes, by = "posicao_id" ) %>%
    right_join(., x = label_status, by = "status_id" ) %>%
    right_join(., x = label_clubes, by = "clube_id" ) %>%
    bind_cols(., precos_jogadores ) %>%
    bind_cols(., scouts_jogadores )
  
  rm(label_status, label_posicoes, scouts_jogadores)
  
  
  info( logger, "CARTOLA_2017::junta tabelas de informacoes das partidas" )
  
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
  
  
  info( logger, "CARTOLA_2017::junta tabelas de informacoes das partidas e jogadores" )
  
  casa  <- informacoes_jogadores %>%
    right_join(., y = informacoes_partidas, by = c( "clube_id" = "partidas.clube_casa_id" ) ) %>%
    mutate( partidas.clube_casa_id = clube_id )
  
  fora <- informacoes_jogadores %>%
    right_join(., y = informacoes_partidas, by = c( "clube_id" = "partidas.clube_visitante_id" ) ) %>%
    mutate( partidas.clube_visitante_id = clube_id )
  
  dados_cartola_2017 <- bind_rows(casa, fora) %>%
    mutate( mandante = ifelse( clube_id == partidas.clube_casa_id, "S", "N") )
  
  
  info( logger, "CARTOLA_2017::cache informacoes partidas" )
  
  cache( "dados_cartola_2017" )
  file.rename( from = "cache/dados_cartola_2017.RData",
               to = paste0( "cache/dados_rodada_", rodada_atual, "_cartola_2017.RData") )
  file.rename( from = "cache/dados_cartola_2017.hash",
               to = paste0( "cache/dados_rodada_", rodada_atual, "_cartola_2017.hash") )
  
  rm(casa, fora, informacoes_jogadores, informacoes_partidas, label_clubes,
     precos_jogadores, rodada_atual, dados_mercado, dados_cartola_2017)
  
  
  info( logger, "CARTOLA_2017::combina os dataset por rodada" )
  
  dados_cartola_2017_por_rodada <- list.files("./cache", pattern = "_cartola_2017.RData", full.names=TRUE) %>%
    lapply(., function(x) get( load(x) ) ) %>%
    bind_rows()
  
}

