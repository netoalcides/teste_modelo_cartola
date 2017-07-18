rodada_atual <- precos_jogadores$rodada_id[1]

info( logger, "CARTOLA_2017::cache precos dos jogadores da rodada atual" )

cache( "precos_jogadores" )
file.rename( from = "cache/precos_jogadores.RData",
             to = paste0( "cache/precos_jogadores_rodada_", rodada_atual, ".RData") )
file.rename( from = "cache/precos_jogadores.hash",
             to = paste0( "cache/precos_jogadores_rodada_", rodada_atual, ".hash") )

info( logger, "CARTOLA_2017::cache labels clubes" )

cache( "label_clubes" )
file.rename( from = "cache/label_clubes.RData",
             to = paste0( "cache/label_clubes_rodada_", rodada_atual, ".RData") )
file.rename( from = "cache/label_clubes.hash",
             to = paste0( "cache/label_clubes_rodada_", rodada_atual, ".hash") )
rm(label_clubes)


info( logger, "CARTOLA_2017::cache informacoes partidas" )

cache( "informacoes_partidas" )
file.rename( from = "cache/informacoes_partidas.RData",
             to = paste0( "cache/informacoes_partidas_rodada_", rodada_atual, ".RData") )
file.rename( from = "cache/informacoes_partidas.hash",
             to = paste0( "cache/informacoes_partidas_rodada_", rodada_atual, ".hash") )
rm(informacoes_partidas)