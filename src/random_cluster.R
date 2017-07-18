load( "cache/dados_cartola_2017_por_rodada.RData" )

########## ataque ##########

temp_data <- dados_cartola_2017_por_rodada %>%
  filter( posicao == "ata",
          rodada_id < rod ) %>%
  group_by( atleta_id, apelido ) %>%
  summarise( FC = sum(FC),
             PE = sum(PE),
             RB = sum(RB),
             CA = sum(CA),
             FD = sum(FD),
             FS = sum(FS),
             FF = sum(FF),
             I = sum(I),
             G = sum(G),
             A = sum(A),
             FT = sum(FT),
             fin = sum(FF)+sum(FD)+sum(FT),
             RB_FC = RB/FC,
             A_PE = A/PE,
             FS_FC = FS/FC ) %>%
  replace_na( list( RB_FC = 0,
                    A_PE = 0,
                    FS_FC = 0 ) ) %>%
  data.frame() %>% 
  mutate( RB_FC = ifelse( is.infinite(RB_FC) == TRUE, 0, RB_FC ),
          A_PE = ifelse( is.infinite(A_PE) == TRUE, 0, A_PE ),
          FS_FC = ifelse( is.infinite(FS_FC) == TRUE, 0, FS_FC ) )

find_cluster_solution <- temp_data %>%
  select( fin, RB_FC, A_PE, FS_FC, G, CA, I ) %>%
  mutate_all( funs(rescale) ) %>% # normalizando
  kcca(., 5, family = kccaFamily("kmeans") )

for( rod in 3:max(dados_cartola_2017_por_rodada$rodada_id) ){
  
  temp_data <- dados_cartola_2017_por_rodada %>%
    filter( posicao == "ata",
            rodada_id < rod ) %>%
    group_by( atleta_id, apelido ) %>%
    summarise( FC = sum(FC),
               PE = sum(PE),
               RB = sum(RB),
               CA = sum(CA),
               FD = sum(FD),
               FS = sum(FS),
               FF = sum(FF),
               I = sum(I),
               G = sum(G),
               A = sum(A),
               FT = sum(FT),
               fin = sum(FF)+sum(FD)+sum(FT),
               RB_FC = RB/FC,
               A_PE = A/PE,
               FS_FC = FS/FC ) %>%
    replace_na( list( RB_FC = 0,
                      A_PE = 0,
                      FS_FC = 0 ) ) %>%
    data.frame() %>% 
    mutate( RB_FC = ifelse( is.infinite(RB_FC) == TRUE, 0, RB_FC ),
            A_PE = ifelse( is.infinite(A_PE) == TRUE, 0, A_PE ),
            FS_FC = ifelse( is.infinite(FS_FC) == TRUE, 0, FS_FC ) )
  
  clusters <- predict( find_cluster_solution, temp_data %>%
                         select( fin, RB_FC, A_PE, FS_FC, G, CA, I ) %>% 
                         mutate_all( funs(rescale) ) )
  
  clusters <- temp_data %>%
    select( fin, RB_FC, A_PE, FS_FC, G, CA, I ) %>% 
    mutate_all( funs(rescale) ) %>% 
    predict( find_cluster_solution, .)
  
  
  clusters <- data.frame( atleta_id = temp_data$atleta_id, 
                          grupo = paste( "gr_kmeans_m1", 
                                         clusters, 
                                         sep = "_" ) )
  
  temp_data %<>% 
    right_join(., x = clusters, by = "atleta_id" ) %>% 
    select( atleta_id, grupo ) %>% 
    spread( key = grupo, value = grupo ) %>% 
    mutate_at( .vars = vars(-atleta_id),
               .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) ) %>% 
    mutate( rodada_id = rod )
  
  ata_resumo <- bind_rows( ata_resumo, temp_data )
  rm( temp_data )
  gc()
  
  
}

# ata_resumo %>% 
#   group_by( rodada_id ) %>% 
#   summarise_at( vars(-atleta_id), funs(sum) )

########## meio ##########

mei_resumo <- NULL

for( rod in 2:max(dados_cartola_2017_por_rodada$rodada_id) ){
 
  temp_data <- dados_cartola_2017_por_rodada %>%
    filter( posicao == "mei",
            rodada_id < rod ) %>%
    group_by( atleta_id, apelido ) %>%
    summarise( FC = sum(FC),
               PE = sum(PE),
               RB = sum(RB),
               CA = sum(CA),
               FD = sum(FD),
               FS = sum(FS),
               FF = sum(FF),
               I = sum(I),
               G = sum(G),
               A = sum(A),
               FT = sum(FT),
               fin = FF+FD+FT,
               RB_FC = RB/FC,
               A_PE = A/PE,
               FS_FC = FS/FC ) %>%
    replace_na( list( RB_FC = 0,
                      A_PE = 0,
                      FS_FC = 0 ) ) %>%
    data.frame() %>% 
    mutate( RB_FC = ifelse( is.infinite(RB_FC) == TRUE, 0, RB_FC ),
            A_PE = ifelse( is.infinite(A_PE) == TRUE, 0, A_PE ),
            FS_FC = ifelse( is.infinite(FS_FC) == TRUE, 0, FS_FC ) )
  
  find_cluster_solution <- temp_data %>%
    select( fin, RB_FC, A_PE, FS_FC, G, CA, I ) %>%
    mutate_all( funs(rescale) ) %>% # normalizando
    kmeans( centers = 5, nstart = 25 )
  
  clusters <- data.frame( atleta_id = temp_data$atleta_id, 
                          grupo = paste( "gr_kmeans_m1", 
                                         find_cluster_solution$cluster, sep = "_" ) )
  
  temp_data %<>% 
    right_join(., x = clusters, by = "atleta_id" ) %>%
    select( atleta_id, grupo ) %>% 
    spread( key = grupo, value = grupo ) %>% 
    mutate_at( .vars = vars(-atleta_id),
               .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) ) %>% 
    mutate( rodada_id = rod )
  
  mei_resumo <- bind_rows( mei_resumo, temp_data )
  rm( temp_data )
  gc()
  
}

# mei_resumo %>%
#   group_by( rodada_id ) %>%
#   summarise_at( vars(-atleta_id), funs(sum) )

########## lateral ##########

lat_resumo <- NULL

for( rod in 2:max(dados_cartola_2017_por_rodada$rodada_id) ){
  
  temp_data <- dados_cartola_2017_por_rodada %>%
    filter( posicao == "lat",
            rodada_id < rod ) %>%
    group_by( atleta_id, apelido ) %>%
    summarise( FC = sum(FC),
               PE = sum(PE),
               RB = sum(RB),
               SG = sum(SG),
               CA = sum(CA),
               FD = sum(FD),
               FS = sum(FS),
               FF = sum(FF),
               I = sum(I),
               G = sum(G),
               A = sum(A),
               A_PE = A/PE,
               RB_FC = RB/FC,
               FS_FC = FS/FC,
               fin = FF+FD ) %>%
    replace_na( list( A_PE = 0,
                      RB_FC = 0,
                      FS_FC = 0 ) ) %>% 
    data.frame() %>% 
    mutate( RB_FC = ifelse( is.infinite(RB_FC) == TRUE, 0, RB_FC ),
            A_PE = ifelse( is.infinite(A_PE) == TRUE, 0, A_PE ),
            FS_FC = ifelse( is.infinite(FS_FC) == TRUE, 0, FS_FC ) )
  
  find_cluster_solution <- temp_data %>%
    select( SG, CA, I, G, A_PE, RB_FC, FS_FC, fin ) %>%
    mutate_all( funs(rescale) ) %>% # normalizando
    kmeans( centers = 6, nstart = 25 )
  
  clusters <- data.frame( atleta_id = temp_data$atleta_id, 
                          grupo = paste( "gr_kmeans_m1", 
                                         find_cluster_solution$cluster, sep = "_" ) )
  temp_data %<>% 
    right_join(., x = clusters, by = "atleta_id" ) %>%
    select( atleta_id, grupo ) %>% 
    spread( key = grupo, value = grupo ) %>% 
    mutate_at( .vars = vars(-atleta_id),
               .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) ) %>% 
    mutate( rodada_id = rod )
  
  lat_resumo <- bind_rows( lat_resumo, temp_data )
  rm( temp_data )
  gc()
  
}

# lat_resumo %>%
#   group_by( rodada_id ) %>%
#   summarise_at( vars(-atleta_id), funs(sum) )

########## zagueiro ##########

zag_resumo <- NULL

for( rod in 2:max(dados_cartola_2017_por_rodada$rodada_id) ){
  
  temp_data <- dados_cartola_2017_por_rodada %>%
    filter( posicao == "zag",
            rodada_id < rod ) %>%
    group_by( atleta_id, apelido ) %>%
    summarise( FC = sum(FC),
               PE = sum(PE),
               RB = sum(RB),
               SG = sum(SG),
               CA = sum(CA),
               FD = sum(FD),
               FS = sum(FS),
               FF = sum(FF),
               G = sum(G),
               A = sum(A),
               fin = FD+FF,
               RB_FC = RB/FC,
               FS_FC = FS/FC,
               A_PE = A/PE) %>%
    replace_na( list( RB_FC = 0,
                      A_PE = 0,
                      FS_FC = 0 ) ) %>% 
    data.frame() %>% 
    mutate( RB_FC = ifelse( is.infinite(RB_FC) == TRUE, 0, RB_FC ),
            A_PE = ifelse( is.infinite(A_PE) == TRUE, 0, A_PE ),
            FS_FC = ifelse( is.infinite(FS_FC) == TRUE, 0, FS_FC ) )
  
  find_cluster_solution <- temp_data %>%
    select( SG, fin, RB_FC, G, A_PE, FS_FC ) %>%
    mutate_all( funs(rescale) ) %>% # normalizando
    kmeans( centers = 4, nstart = 25 )
  
  clusters <- data.frame( atleta_id = temp_data$atleta_id, 
                          grupo = paste( "gr_kmeans_m1", 
                                         find_cluster_solution$cluster, sep = "_" ) )
  temp_data %<>% 
    right_join(., x = clusters, by = "atleta_id" ) %>%
    select( atleta_id, grupo ) %>% 
    spread( key = grupo, value = grupo ) %>% 
    mutate_at( .vars = vars(-atleta_id),
               .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) ) %>% 
    mutate( rodada_id = rod )
  
  zag_resumo <- bind_rows( zag_resumo, temp_data )
  rm( temp_data )
  gc()
  
}

# zag_resumo %>%
#   group_by( rodada_id ) %>%
#   summarise_at( vars(-atleta_id), funs(sum) )


########## goleiro ##########


gol_resumo <- NULL

for( rod in 2:max(dados_cartola_2017_por_rodada$rodada_id) ){
  
  temp_data <- dados_cartola_2017_por_rodada %>%
    filter( posicao == "gol",
            rodada_id < rod ) %>%
    group_by( atleta_id, apelido ) %>%
    summarise( PE = sum(PE),
               SG = sum(SG),
               CA = sum(CA),
               FS = sum(FS),
               DD = sum(DD),
               GS = sum(GS),
               DP = sum(DP) ) %>%
    data.frame()
  
  find_cluster_solution <- temp_data %>%
    select( -atleta_id, -apelido ) %>%
    mutate_all( funs(rescale) ) %>% # normalizando
    kmeans( centers = 4, nstart = 25 )
  
  clusters <- data.frame( atleta_id = temp_data$atleta_id, 
                          grupo = paste( "gr_kmeans_m1", 
                                         find_cluster_solution$cluster, sep = "_" ) )
  temp_data %<>% 
    right_join(., x = clusters, by = "atleta_id" ) %>%
    select( atleta_id, grupo ) %>% 
    spread( key = grupo, value = grupo ) %>% 
    mutate_at( .vars = vars(-atleta_id),
               .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) ) %>% 
    mutate( rodada_id = rod )
  
  gol_resumo <- bind_rows( gol_resumo, temp_data )
  rm( temp_data )
  gc()
  
}

# gol_resumo %>%
#   group_by( rodada_id ) %>%
#   summarise_at( vars(-atleta_id), funs(sum) )
