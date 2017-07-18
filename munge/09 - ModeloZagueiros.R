info( logger, "CARTOLA_2017::modelo zagueiros" )

info( logger, "CARTOLA_2017::filtra zagueiros" )

zagueiros <- dados_cartola_2017_por_rodada %>%
  filter( posicao == "zag" ) %>%
  mutate( fin = FF + FD + FT,
          RB_FC = ifelse( is.infinite(RB/FC) == TRUE, 0, RB/FC ),
          A_PE = ifelse( is.infinite(A/PE) == TRUE, 0, A/PE ),
          FS_FC = ifelse( is.infinite(FS/FC) == TRUE, 0, FS/FC ) ) %>%
  replace_na( list( RB_FC = 0,
                    A_PE = 0,
                    FS_FC = 0 ) ) %>%
  mutate( fez = FC + PE + RB + SG + CA + FD + FS + FF + I + G + DD + GS + A + CV + FT + GC + DP + PP ) %>%
  filter( fez != 0 & variacao_num != 0 )


info( logger, "CARTOLA_2017::variaveis da forca do adversario" )

zagueiros %<>%
  left_join(., y = zag_data %>%
              select( clube_id, zag_ptos_clube, mandante, rodada_id ),
            by = "clube_id" ) %>%
  filter( mandante.x == mandante.y,
          rodada_id.x == rodada_id.y ) %>%
  left_join(., y = gol_data %>%
              select( clube_id, gol_ptos_clube, mandante, rodada_id ),
            by = "clube_id" ) %>%
  filter( mandante.x == mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante, -rodada_id ) %>%
  left_join(., y = ata_data %>%
              select( clube_id, ata_ptos_clube, mandante, rodada_id ),
            by = c( "partidas.clube_visitante_id" = "clube_id" ) ) %>%
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante, -rodada_id ) %>%
  left_join(., y = meio_data %>%
              select( clube_id, meio_ptos_clube, mandante, rodada_id ),
            by = c( "partidas.clube_visitante_id" = "clube_id" ) ) %>%
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -fez, -mandante.x, -mandante,
          -rodada_id.x, -rodada_id.y ) %>%
  rename(., ata_ptos_clube_adv = ata_ptos_clube,
            meio_ptos_clube_adv = meio_ptos_clube ) %>%
  mutate( mandante = ifelse( mandante.y == "S", 1, 0 ) ) %>% 
  select( -mandante.y )


info( logger, "CARTOLA_2017::cria defasagens" )


zagueiros %<>%
  group_by( atleta_id ) %>%
  mutate( lfin = lag(fin, n = 1),
          lfin2 = lag(fin, n = 2),
          lRB_FC = lag(RB_FC, n = 1),
          lRB_FC2 = lag(RB_FC, n = 2),
          lA_PE = lag(A_PE, n = 1),
          lA_PE2 = lag(A_PE, n = 2),
          lFS_FC = lag(FS_FC, n = 1),
          lFS_FC2 = lag(FS_FC, n = 2),
          lG = lag(G, n = 1),
          lG2 = lag(G, n = 2),
          lFC = lag(FC, n = 1),
          lFC2 = lag(FC, n = 2),
          lCA = lag(CA, n = 1),
          lCA2 = lag(CA, n = 2),
          lSG = lag(SG, n = 1),
          lSG2 = lag(SG, n = 2) ) %>%
  na.omit() %>%
  select( atleta_id, apelido,
          partidas.clube_casa_nome,
          partidas.clube_visitante_nome,
          pontos_num,
          lfin, lRB_FC, lA_PE, lFS_FC, lG, lFC, lCA, lSG,
          lfin2, lRB_FC2, lA_PE2, lFS_FC2, lG2, lFC2, lCA2, lSG2,
          zag_ptos_clube,
          gol_ptos_clube,
          ata_ptos_clube_adv,
          meio_ptos_clube_adv,
          mandante,
          rodada_id ) %>%
  data.frame()


info( logger, "CARTOLA_2017::divide periodos para treino e teste" )

periodos <- zagueiros %>%
  count( rodada_id ) %>%
  select( rodada_id ) %>%
  data.frame

janela <- 3
Resultados <- list()


info( logger, "CARTOLA_2017::loop avaliacao dos modelos" )

for( i in 1:(nrow(periodos) - janela) ){


  info( logger, paste0("CARTOLA_2017::separando periodos para analise ", i) )

  tempoTreino <- periodos[1:(janela + i - 1), ]
  tempoTeste <- periodos[(janela + i), ]

  dadosTreino <- zagueiros %>%
    filter( between( rodada_id, min(tempoTreino), max(tempoTreino) ) ) %>%
    select( -apelido,
            -partidas.clube_casa_nome,
            -partidas.clube_visitante_nome,
            -rodada_id )

  dadosTeste <- zagueiros %>%
    filter( rodada_id ==  tempoTeste ) %>%
    select( -apelido,
            -partidas.clube_casa_nome,
            -partidas.clube_visitante_nome,
            -rodada_id )


  info( logger, "CARTOLA_2017::etapa de clustering iniciada" )

  set.seed(54321)
  cluster_model <- dadosTreino %>%
    group_by( atleta_id ) %>%
    summarise_at( vars( lSG, lfin, lRB_FC, lG, lA_PE, lFS_FC ),
                  funs( sum ) ) %>%
    select( -atleta_id ) %>%
    mutate_all( funs(rescale) ) %>%
    kcca(., 4, family = kccaFamily("kmeans") )

  grupos_dadosTreino <- dadosTreino %>%
    group_by( atleta_id ) %>%
    summarise_at( vars( lSG, lfin, lRB_FC, lG, lA_PE, lFS_FC ),
                  funs( sum ) ) %>%
    mutate_at( vars( lSG, lfin, lRB_FC, lG, lA_PE, lFS_FC ),
               funs(rescale) ) %>%
    mutate( grupos = predict(cluster_model) ) %>%
    select( atleta_id, grupos ) %>%
    mutate( grupos = paste( "gr_kmeans_m1", grupos, sep = "_" ) ) %>%
    spread( key = grupos, value = grupos ) %>%
    mutate_at( .vars = vars(-atleta_id),
               .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) )

  grupos_cluster <- zagueiros %>%
    filter( rodada_id <= tempoTeste ) %>%
    group_by( atleta_id ) %>%
    summarise_at( vars(lSG, lfin, lRB_FC, lG, lA_PE, lFS_FC), funs(sum) ) %>%
    mutate_at( vars(lSG, lfin, lRB_FC, lG, lA_PE, lFS_FC), funs(rescale) ) %>%
    mutate( grupos = predict( cluster_model, .[-1] ) ) %>%
    select( atleta_id, grupos )
  
  grupos_dadosTeste <- dadosTeste %>%
    group_by( atleta_id ) %>%
    summarise_at( vars(lSG),
                  funs(sum) ) %>%
    left_join(., y = grupos_cluster, by = "atleta_id" ) %>% 
    select( atleta_id, grupos ) %>%
    mutate( grupos = paste( "gr_kmeans_m1", grupos, sep = "_" ) ) %>%
    spread( key = grupos, value = grupos ) %>%
    mutate_at( .vars = vars(-atleta_id),
               .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) )
  
  info( logger, "CARTOLA_2017::etapa de clustering finalizada" )

  
  dadosTreino %<>%
    right_join(., y = grupos_dadosTreino, by = "atleta_id" ) %>%
    select( -atleta_id, -gr_kmeans_m1_1 )

  dadosTeste %<>%
    right_join(., y = grupos_dadosTeste, by = "atleta_id" ) %>%
    select( -gr_kmeans_m1_1 )

  modelo <- lm( pontos_num ~., data = dadosTreino )

  dadosTeste$pontos_num_pred <- predict( modelo, dadosTeste )

  # dadosTeste %<>%
  #   filter( between( pontos_num_pred,
  #                    quantile( pontos_num_pred, 0.05 ),
  #                    quantile( pontos_num_pred, 0.95 ) ) )
  
  Resultados[[length(Resultados) + 1]] <- dadosTeste %>%
    summarise( rmse = sqrt( mean( (pontos_num - pontos_num_pred)^2) ),
               mae = mean( abs(pontos_num - pontos_num_pred) ) )

  cat( paste0( "zagueiros_teste ", i), "\n" )
  show( dadosTeste %>% 
          select( atleta_id, pontos_num, pontos_num_pred) %>% 
          arrange( desc(pontos_num_pred) ) %>% 
          head( 10 ) )
  
  show( dadosTeste %>%
          ggplot( aes( x = pontos_num_pred, y = pontos_num ) ) +
          geom_point( shape = 1 ) +
          geom_smooth( method = lm ) +
          ggtitle( paste0("zagueiros_teste ", i) ) )

  zaga <- dadosTeste %>% 
    select( atleta_id, pontos_num_pred) %>%
    mutate( rodada = tempoTeste ) %>%
    arrange( desc(pontos_num_pred) )
  
  save( zaga, file = paste0( "reports/zagueiros_rodada_", tempoTeste, ".RData") )
  
  rm(zaga)
  
}

show( bind_rows( Resultados ) )