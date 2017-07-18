info( logger, "CARTOLA_2017::obtem dados das partidas" )

dados_partidas <- fromJSON( 
                   paste( 
                     readLines( "https://api.cartolafc.globo.com/partidas" ), 
                     collapse = "" ) 
                   )
