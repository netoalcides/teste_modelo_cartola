dados_parciais <- fromJSON( 
  paste( 
    readLines( "https://api.cartolafc.globo.com/atletas/pontuados" ), 
    collapse = "" )
)

#"https://api.cartolafc.globo.com/atletas/pontuados"