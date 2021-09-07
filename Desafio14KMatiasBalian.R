require( "data.table")

set.seed( 300331 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

#defino los jugadores
mejor      <-  0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( mejor, peloton )

#veo que tiene el vector
jugadores

#hago que los 100 jugadores tiren 100 veces cada uno
#Aqui usamos los 10000 primeros lanzamientos que pide la consigna
jugadores1 <- mapply(  ftirar, jugadores, 100 )

#vemos contenido de jugadores1
jugadores1

#hago simulacion Montecarlo para verificar que siempre el primero (nuestro mejor) esta en puntaje mayor a 65
#en todos las 10000 iteraciones. En este momento no me interesa que sea efectivamente el mejor. Solo que este dentro
#de los 10 mejores

primero_ganador <- 0

for( i in 1:10000 )  #diez mil experimentos
{
jugadores1  <- mapply( ftirar, jugadores, 100 )  #100 tiros libres cada jugador
  
  mejor1  <- all( jugadores1[1] >= 65)
  if( (mejor1)[1]>='65' )  primero_ganador <- primero_ganador + 1
}

print(  primero_ganador ) #en las 10000 iteraciones siempre el mejor jugador esta seleccionado entre los 10.



#selecciono los jugadores indice que tengan score >= a 65 (corte verificado arriba). Se asume que el mejor jugador rara vez
#tendra un score menor a 65 en los 100 tiros anteriores. (si eso pasa, no era muy habil en definitiva)
#en la simulacion anterior se verifico que siempre supera los 65 puntos.
jugadores2 <- which(jugadores1 >= 65)

#veo el contenido de jugadores2
jugadores2

#selecciono los indices que arrojo jugadores2 para seleccionar los mejores con las probabilidades de jugadores original
#aqui tengo seleccionado los mejores (maximo 10) de la tanda de 100 anterior
jugadores3 <- jugadores[c(jugadores2[1:10])]

#veo contenido de jugadores3
jugadores3
  
#ahora los jugadores seleccionados, lanzan  100,200 y 300 tiros libres. Asi se completan los lanzamientos restantes(maximo 4000).

for(  tiros_libres  in c( 100,200,300 ) )
{
  
  primero_ganador1  <- 0
  
  for( i in 1:10000 )  #diez mil experimentos
  {
    vaciertos <- mapply( ftirar, jugadores3, tiros_libres ) 
    mejor2  <- which.max( vaciertos )
    if( mejor2 == 1 )  primero_ganador1 <- primero_ganador1 + 1
  }
  
  cat( tiros_libres, primero_ganador1/10000, "\n" )
}

#Conclusion: en la primer tanda cada jugador lanzo 100 tiros. Aqui se usaron los primeros 10000 lanzamientos. 
#luego, se seleccionan los mejores de esos 100. Los seleccionados lanzan hasta 400 tiros y llegamos al index==1
#como el mejor en el 99% o mas de los casos.
