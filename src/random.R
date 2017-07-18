# dados

# load( "cache/dados_cartola_2017_por_rodada.RData" )
# 
# dados_cartola_2017_por_rodada %<>%
#   arrange( atleta_id, rodada_id ) %>%
#   select( atleta_id, FC, PE, RB, SG, CA, FD, FS, FF, I, G, DD, GS, A, CV, FT, GC, DP, PP ) %>% 
#   group_by( atleta_id ) %>%
#   mutate_all( funs( ajuste_lag(.) ) ) %>% 
#   bind_cols( dados_cartola_2017_por_rodada %>%
#                arrange( atleta_id, rodada_id ) %>% 
#                select( -atleta_id, -FC, -PE, -RB, -SG, -CA, -FD, -FS, -FF, -I, -G, -DD, -GS, 
#                        -A, -CV, -FT, -GC, -DP, -PP ), .) %>% 
#   data.frame()


# ataque

ata_data <- NULL

for( rod in 1:max(dados_cartola_2017_por_rodada$rodada_id) ){
  
  temp_data <- dados_cartola_2017_por_rodada %>% 
    filter( posicao == "ata",
            rodada_id < rod ) %>% 
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
    data.frame() %>% 
    mutate( rodada_id = rod )
  
  ata_data <- bind_rows( ata_data, temp_data )
  rm( temp_data )
  gc()
}


# meio

meio_data <- NULL

for( rod in 1:max(dados_cartola_2017_por_rodada$rodada_id) ){
  
  temp_data <- dados_cartola_2017_por_rodada %>% 
    filter( posicao == "mei",
            rodada_id < rod ) %>% 
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
    data.frame() %>% 
    mutate( rodada_id = rod )
  
  meio_data <- bind_rows( meio_data, temp_data )
  rm( temp_data )
  gc()
}


# lat

lat_data <- NULL

for( rod in 1:max(dados_cartola_2017_por_rodada$rodada_id) ){
  
  temp_data <- dados_cartola_2017_por_rodada %>% 
    filter( posicao == "lat",
            rodada_id < rod ) %>% 
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
    data.frame() %>% 
    mutate( rodada_id = rod )
  
  lat_data <- bind_rows( lat_data, temp_data )
  
}



# zag

zag_data <- NULL

for( rod in 1:max(dados_cartola_2017_por_rodada$rodada_id) ){
  
  temp_data <- dados_cartola_2017_por_rodada %>% 
    filter( posicao == "zag",
            rodada_id < rod ) %>% 
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
    data.frame() %>% 
    mutate( rodada_id = rod )
  
  zag_data <- bind_rows( zag_data, temp_data )
  
}


# gol

gol_data <- NULL

for( rod in 1:max(dados_cartola_2017_por_rodada$rodada_id) ){
  
  temp_data <- dados_cartola_2017_por_rodada %>% 
    filter( posicao == "gol",
            rodada_id < rod ) %>% 
    group_by( clube_id, clube, mandante ) %>% 
    summarise( gol_ptos_clube = mean(pontos_num),
               gol_PE = mean(PE),
               gol_SG = mean(SG),
               gol_CA = mean(CA),
               gol_FS = mean(FS),
               gol_DD = mean(DD),
               gol_GS = mean(GS),
               gol_DP = mean(DP) ) %>% 
    data.frame() %>% 
    mutate( rodada_id = rod )
  
  gol_data <- bind_rows( gol_data, temp_data )
  
}
