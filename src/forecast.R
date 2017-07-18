#######################################################################
############################ Carrega dados ############################
#######################################################################

rodada <- 11
janela <- 6

load( paste0( "cache/label_clubes_rodada_", rodada, ".RData" ) )
load( "cache/dados_cartola_2017_por_rodada.RData" )
load( paste0( "cache/informacoes_partidas_rodada_", rodada, ".RData" ) )

###########################################################################
############################ Ajusta defasagens ############################
###########################################################################

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
  filter( fez != 0 & variacao_num != 0 ) %>% 
  group_by( atleta_id ) %>% 
  mutate( lfin = fin,
          lfin2 = lag(fin, n = 1),
          lRB_FC = RB_FC,
          lRB_FC2 = lag(RB_FC, n = 1),
          lA_PE = A_PE,
          lA_PE2 = lag(A_PE, n = 1),
          lFS_FC = FS_FC,
          lFS_FC2 = lag(FS_FC, n = 1),
          lG = G,
          lG2 = lag(G, n = 1),
          lI = I,
          lI2 = lag(I, n = 1),
          lPE = PE,
          lPE2 = lag(PE, n = 1),
          lFC = FC,
          lFC2 = lag(FC, n = 1),
          lCA = CA,
          lCA2 = lag(CA, n = 1) ) %>% 
  na.omit() %>% 
  data.frame()
          
#########################################################################
####################### Prepara para rodada atual #######################
#########################################################################

ataque %<>%
  filter( rodada_id == rodada - 1 ) %>% 
  select( -starts_with("partidas") )

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

casa  <- ataque %>% 
  right_join(., y = informacoes_partidas, 
             by = c( "clube_id" = "partidas.clube_casa_id" ) ) %>%
  mutate( partidas.clube_casa_id = clube_id )

fora <- ataque %>% 
  right_join(., y = informacoes_partidas, 
             by = c( "clube_id" = "partidas.clube_visitante_id" ) ) %>% 
  mutate( partidas.clube_visitante_id = clube_id )

ataque <- bind_rows(casa, fora) %>% 
  mutate( mandante = ifelse( clube_id == partidas.clube_casa_id, "S", "N"),
          rodada_id = rodada )


###################################################################
####################### Feature Engeneering #######################
###################################################################

ata_data <- dados_cartola_2017_por_rodada %>% 
  filter( posicao == "ata",
          between( rodada_id, min(rodada-janela), max(rodada-1) ) ) %>% 
  group_by( clube_id, clube, mandante ) %>% 
  summarise( ata_ptos_clube = mean(pontos_num),
             ata_FC = mean(FC),
             ata_PE = mean(PE),
             ata_RB = mean(RB),
             ata_CA = mean(CA),
             ata_FD = mean(FD),
             ata_FS = mean(FS),
             ata_FF = mean(FF),
             ata_I = mean(I),
             ata_G = mean(G),
             ata_A = mean(A),
             ata_FT = mean(FT) ) %>% 
  data.frame()
  
meio_data <- dados_cartola_2017_por_rodada %>% 
  filter( posicao == "mei",
          between( rodada_id, min(rodada-janela), max(rodada-1) ) ) %>% 
  group_by( clube_id, clube, mandante ) %>% 
  summarise( meio_ptos_clube = mean(pontos_num),
             meio_FC = mean(FC),
             meio_PE = mean(PE),
             meio_RB = mean(RB),
             meio_CA = mean(CA),
             meio_FD = mean(FD),
             meio_FS = mean(FS),
             meio_FF = mean(FF),
             meio_I = mean(I),
             meio_G = mean(G),
             meio_A = mean(A),
             meio_FT = mean(FT) ) %>% 
  data.frame()

lat_data <- dados_cartola_2017_por_rodada %>% 
  filter( posicao == "lat",
          between( rodada_id, min(rodada-janela), max(rodada-1) ) ) %>% 
    group_by( clube_id, clube, mandante ) %>% 
    summarise( lat_ptos_clube = mean(pontos_num),
               lat_FC = mean(FC),
               lat_PE = mean(PE),
               lat_RB = mean(RB),
               lat_SG = mean(SG),
               lat_CA = mean(CA),
               lat_FD = mean(FD),
               lat_FS = mean(FS),
               lat_FF = mean(FF),
               lat_I = mean(I),
               lat_G = mean(G),
               lat_A = mean(A) ) %>% 
    data.frame()

zag_data <- dados_cartola_2017_por_rodada %>% 
  filter( posicao == "zag",
          between( rodada_id, min(rodada-janela), max(rodada-1) ) ) %>% 
    group_by( clube_id, clube, mandante ) %>% 
    summarise( zag_ptos_clube = mean(pontos_num),
               zag_FC = mean(FC),
               zag_PE = mean(PE),
               zag_RB = mean(RB),
               zag_SG = mean(SG),
               zag_CA = mean(CA),
               zag_FD = mean(FD),
               zag_FS = mean(FS),
               zag_FF = mean(FF) ) %>% 
    data.frame()

gol_data <- dados_cartola_2017_por_rodada %>% 
  filter( posicao == "gol",
          between( rodada_id, min(rodada-janela), max(rodada-1) ) ) %>% 
  group_by( clube_id, clube, mandante ) %>% 
  summarise( gol_ptos_clube = mean(pontos_num),
             gol_PE = mean(PE),
             gol_SG = mean(SG),
             gol_CA = mean(CA),
             gol_FS = mean(FS),
             gol_DD = mean(DD),
             gol_GS = mean(GS),
             gol_DP = mean(DP) ) %>% 
    data.frame()


##################################################################
####################### Juntando as coisas #######################
##################################################################

# variaveis da forca do adversario

ataque %<>% 
  left_join(., y = ata_data %>% 
              select( clube_id, ata_ptos_clube, mandante ), 
            by = "clube_id" ) %>% 
  filter( mandante.x == mandante.y ) %>%
  left_join(., y = zag_data %>% 
              select( clube_id, zag_ptos_clube, mandante ), 
            by = c( "partidas.clube_visitante_id" = "clube_id" ) ) %>% 
  filter( mandante.x != mandante ) %>% 
  select( -mandante ) %>% 
  left_join(., y = lat_data %>% 
              select( clube_id, lat_ptos_clube, mandante ), 
            by = c( "partidas.clube_visitante_id" = "clube_id" ) ) %>% 
  filter( mandante.x != mandante ) %>%
  select( -mandante ) %>% 
  left_join(., y = gol_data %>% 
              select( clube_id, gol_ptos_clube, mandante ), 
            by = c( "partidas.clube_visitante_id" = "clube_id" ) ) %>% 
  filter( mandante.x != mandante ) %>%
  select( -fez, -mandante.x, -mandante.y ) %>% 
  rename(., zag_ptos_clube_adv = zag_ptos_clube,
         lat_ptos_clube_adv = lat_ptos_clube,
         gol_ptos_clube_adv = gol_ptos_clube ) %>% 
  mutate( mandante = ifelse( mandante == "S", 1, 0 ) )

ataque$pontos_num_pred <- predict( modelo, ataque )

cat("\014")

ataque %>% 
  filter( status == "Provável" ) %>% 
  select( atleta_id, apelido, clube, posicao, pontos_num_pred, preco_num ) %>% 
  arrange( desc(pontos_num_pred) )

