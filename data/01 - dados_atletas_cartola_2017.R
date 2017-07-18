info( logger, "CARTOLA_2017::iniciado" )

info( logger, "CARTOLA_2017::obtem dados do mercado" )

dados_mercado <- fromJSON( 
                   paste( 
                     readLines( "https://api.cartolafc.globo.com/atletas/mercado" ), 
                     collapse = "" )
                   )
