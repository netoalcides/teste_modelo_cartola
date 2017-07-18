info( logger, "CARTOLA_2017::modelo goleiros" )

info( logger, "CARTOLA_2017::filtra goleiros" )

goleiros <- dados_cartola_2017_por_rodada %>%
  filter( posicao == "gol" ) %>%
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

goleiros %<>%
  left_join(., y = gol_data %>%
              select( clube_id, gol_ptos_clube, mandante, rodada_id ),
            by = "clube_id" ) %>%
  filter( mandante.x == mandante.y,
          rodada_id.x == rodada_id.y ) %>%
  left_join(., y = zag_data %>%
              select( clube_id, zag_ptos_clube, mandante, rodada_id ),
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

goleiros %<>%
  group_by( atleta_id ) %>%
  mutate( lPE = lag(PE, n = 1),
          lPE2 = lag(PE, n = 2),
          lSG = lag(SG, n = 1),
          lSG2 = lag(SG, n = 2),
          lCA = lag(CA, n = 1),
          lCA2 = lag(CA, n = 2),
          lFS = lag(FS, n = 1),
          lFS2 = lag(FS, n = 2),
          lDD = lag(DD, n = 1),
          lDD2 = lag(DD, n = 2),
          lGS = lag(GS, n = 1),
          lGS2 = lag(GS, n = 2),
          lDP = lag(DP, n = 1),
          lDP2 = lag(DP, n = 2) ) %>%
  na.omit() %>%
  select( atleta_id, apelido,
          partidas.clube_casa_nome,
          partidas.clube_visitante_nome,
          pontos_num,
          lPE, lSG, lCA, lFS, lDD, lGS, lDP,
          lPE2, lSG2, lCA2, lFS2, lDD2, lGS2, lDP2,
          gol_ptos_clube,
          zag_ptos_clube,
          ata_ptos_clube_adv,
          meio_ptos_clube_adv,
          mandante,
          rodada_id ) %>%
  data.frame()


info( logger, "CARTOLA_2017::divide periodos para treino e teste" )

periodos <- goleiros %>%
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

  dadosTreino <- goleiros %>%
    filter( between( rodada_id, min(tempoTreino), max(tempoTreino) ) ) %>%
    select( -apelido,
            -partidas.clube_casa_nome,
            -partidas.clube_visitante_nome,
            -rodada_id )

  dadosTeste <- goleiros %>%
    filter( rodada_id ==  tempoTeste ) %>%
    select( -apelido,
            -partidas.clube_casa_nome,
            -partidas.clube_visitante_nome,
            -rodada_id )


  info( logger, "CARTOLA_2017::etapa de clustering iniciada" )

  set.seed(54321)
  cluster_model <- dadosTreino %>%
    group_by( atleta_id ) %>%
    summarise_at( vars( lPE, lSG, lCA, lFS, lDD, lGS, lDP ),
                  funs( sum ) ) %>%
    select( -atleta_id ) %>%
    mutate_all( funs(rescale) ) %>%
    kcca(., 3, family = kccaFamily("kmeans") )

  grupos_dadosTreino <- dadosTreino %>%
    group_by( atleta_id ) %>%
    summarise_at( vars( lPE, lSG, lCA, lFS, lDD, lGS, lDP ),
                  funs( sum ) ) %>%
    mutate_at( vars( lPE, lSG, lCA, lFS, lDD, lGS, lDP ),
               funs(rescale) ) %>%
    mutate( grupos = predict(cluster_model) ) %>%
    select( atleta_id, grupos ) %>%
    mutate( grupos = paste( "gr_kmeans_m1", grupos, sep = "_" ) ) %>%
    spread( key = grupos, value = grupos ) %>%
    mutate_at( .vars = vars(-atleta_id),
               .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) )

  grupos_cluster <- goleiros %>%
    filter( rodada_id <= tempoTeste ) %>%
    group_by( atleta_id ) %>%
    summarise_at( vars(lPE, lSG, lCA, lFS, lDD, lGS, lDP), funs(sum) ) %>%
    mutate_at( vars(lPE, lSG, lCA, lFS, lDD, lGS, lDP), funs(rescale) ) %>%
    mutate( grupos = predict( cluster_model, .[-1] ) ) %>%
    select( atleta_id, grupos )
    
  
  grupos_dadosTeste <- dadosTeste %>%
    group_by( atleta_id ) %>%
    summarise_at( vars(lPE),
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

  cat( paste0( "goleiros_teste ", i), "\n" )
  show( dadosTeste %>% 
          select( atleta_id, pontos_num, pontos_num_pred) %>% 
          arrange( desc(pontos_num_pred) ) %>% 
          head( 10 ) )
  
  show( dadosTeste %>%
          ggplot( aes( x = pontos_num_pred, y = pontos_num ) ) +
          geom_point( shape = 1 ) +
          geom_smooth( method = lm ) +
          ggtitle( paste0("goleiros_teste ", i) ) )
  
  goleiro <- dadosTeste %>% 
    select( atleta_id, pontos_num_pred) %>%
    mutate( rodada = tempoTeste ) %>%
    arrange( desc(pontos_num_pred) )
  
  save( goleiro, file = paste0( "reports/goleiros_rodada_", tempoTeste, ".RData") )
  
  rm(goleiro)

}

show( bind_rows( Resultados ) )