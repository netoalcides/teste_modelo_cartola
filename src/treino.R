
load( "cache/dados_cartola_2017_por_rodada.RData" )


# dataset só de atacantes

ataque <- dados_cartola_2017_por_rodada %>%
  filter( posicao == "ata" ) %>%
  mutate( fin = FF + FD + FT,
          RB_FC = ifelse( is.infinite(RB/FC) == TRUE, 0, RB/FC ),
          A_PE = ifelse( is.infinite(A/PE) == TRUE, 0, A/PE ),
          FS_FC = ifelse( is.infinite(FS/FC) == TRUE, 0, FS/FC ) ) %>%
  replace_na( list( RB_FC = 0,
                    A_PE = 0,
                    FS_FC = 0 ) ) %>% 
  mutate( fez = FC + PE + RB + SG + CA + FD + FS + FF + I + G + DD + GS + A + CV + FT + GC + DP + PP ) %>% 
  filter( fez != 0 & variacao_num != 0 )


# variaveis da forca do adversario

ataque %<>% 
  left_join(., y = ata_data %>% 
              select( clube_id, ata_ptos_clube, mandante, rodada_id ), 
            by = "clube_id" ) %>% 
  filter( mandante.x == mandante.y,
          rodada_id.x == rodada_id.y ) %>%
  left_join(., y = zag_data %>% 
              select( clube_id, zag_ptos_clube, mandante, rodada_id ), 
            by = c( "partidas.clube_visitante_id" = "clube_id" ) ) %>% 
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante, -rodada_id ) %>% 
  left_join(., y = lat_data %>% 
              select( clube_id, lat_ptos_clube, mandante, rodada_id ), 
            by = c( "partidas.clube_visitante_id" = "clube_id" ) ) %>% 
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante, -rodada_id ) %>% 
  left_join(., y = gol_data %>% 
              select( clube_id, gol_ptos_clube, mandante, rodada_id ), 
            by = c( "partidas.clube_visitante_id" = "clube_id" ) ) %>% 
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -fez, -mandante.x, -mandante.y, 
          -rodada_id.x, -rodada_id.y ) %>% 
  rename(., zag_ptos_clube_adv = zag_ptos_clube,
            lat_ptos_clube_adv = lat_ptos_clube,
            gol_ptos_clube_adv = gol_ptos_clube ) %>% 
  mutate( mandante = ifelse( mandante == "S", 1, 0 ) )

# variaveis defasagens

ataque %<>%
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
          lI = lag(I, n = 1),
          lI2 = lag(I, n = 2),
          lPE = lag(PE, n = 1),
          lPE2 = lag(PE, n = 2),
          lFC = lag(FC, n = 1),
          lFC2 = lag(FC, n = 2),
          lCA = lag(CA, n = 1),
          lCA2 = lag(CA, n = 2) ) %>%
  na.omit() %>% 
  select( atleta_id, apelido, 
          partidas.clube_casa_nome,
          partidas.clube_visitante_nome, 
          pontos_num, 
          lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA,
          lfin2, lRB_FC2, lA_PE2, lFS_FC2, lG2, lI2, lPE2, lFC2, lCA2, 
          ata_ptos_clube,
          zag_ptos_clube_adv,
          lat_ptos_clube_adv,
          gol_ptos_clube_adv,
          mandante,
          rodada_id ) %>% 
  data.frame()


# periodos para treino e teste

periodos <- ataque %>%
  count( rodada_id ) %>%
  select( rodada_id ) %>%
  data.frame

janela <- 3

#Previsoes_modelo_XG_propor <- list()

Resultados <- list()

for( i in 1:(nrow(periodos) - janela) ){
  
  info( logger, paste0("MASSMEDIAPOC:separando periodos para analise ", i) )
  
  tempoTreino <- periodos[1:(janela + i - 1), ]
  tempoTeste <- periodos[(janela + i), ]
  
  dadosTreino <- ataque %>%
    filter( between( rodada_id, min(tempoTreino), max(tempoTreino) ) ) %>% 
    select( -apelido, 
            -partidas.clube_casa_nome, 
            -partidas.clube_visitante_nome,
            -rodada_id )
  
  dadosTeste <- ataque %>%
    filter( rodada_id ==  tempoTeste ) %>% 
    select( -apelido, 
            -partidas.clube_casa_nome, 
            -partidas.clube_visitante_nome,
            -rodada_id )
  
  # clustering 
  
  set.seed(54321)
  cluster_model <- dadosTreino %>% 
    group_by( atleta_id ) %>% 
    summarise_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA ),
                  funs( sum ) ) %>% 
    select( -atleta_id ) %>% 
    mutate_all( funs(rescale) ) %>% 
    kcca(., 4, family = kccaFamily("kmeans") )
  
  grupos_dadosTreino <- dadosTreino %>% 
    group_by( atleta_id ) %>% 
    summarise_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA ),
                  funs( sum ) ) %>% 
    mutate_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA ),
               funs(rescale) ) %>% 
    mutate( grupos = predict(cluster_model) ) %>% 
    select( atleta_id, grupos ) %>% 
    mutate( grupos = paste( "gr_kmeans_m1", grupos, sep = "_" ) ) %>% 
    spread( key = grupos, value = grupos ) %>% 
    mutate_at( .vars = vars(-atleta_id),
               .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) )
  
  grupos_cluster <- dadosTeste %>% 
    group_by( atleta_id ) %>% 
    summarise_at( vars(lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA),
                  funs(sum) ) %>% 
    mutate_at( vars(lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA),
                  funs(rescale) ) %>% 
    select( -atleta_id ) %>% 
    mutate( grupos = predict(cluster_model, .) ) %>% 
    select( grupos )
  
  grupos_dadosTeste <- dadosTeste %>% 
    group_by( atleta_id ) %>% 
    summarise_at( vars(lfin),
                  funs(sum) ) %>%
    bind_cols(., grupos_cluster) %>% 
    select( atleta_id, grupos ) %>% 
    mutate( grupos = paste( "gr_kmeans_m1", grupos, sep = "_" ) ) %>% 
    spread( key = grupos, value = grupos ) %>% 
    mutate_at( .vars = vars(-atleta_id),
               .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) )
  
  ###
  
  dadosTreino %<>% 
    right_join(., y = grupos_dadosTreino, by = "atleta_id" ) %>% 
    select( -atleta_id, -gr_kmeans_m1_1 )
  
  dadosTeste %<>% 
    right_join(., y = grupos_dadosTeste, by = "atleta_id" ) %>% 
    select( -gr_kmeans_m1_1 )
    
  modelo <- lm( pontos_num ~., data = dadosTreino )
  
  dadosTeste$pontos_num_pred <- predict( modelo, dadosTeste )
  
  Resultados[[length(Resultados) + 1]] <- dadosTeste %>% 
    summarise( rmse = sqrt( mean( (pontos_num - pontos_num_pred)^2) ),
               mae = mean( abs(pontos_num - pontos_num_pred) ) )
  
  show( dadosTeste %>% 
    ggplot( aes( x = pontos_num_pred, y = pontos_num ) ) +
    geom_point( shape = 1 ) +
    geom_smooth( method = lm ) )

}



