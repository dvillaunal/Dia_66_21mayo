## ---- eval=FALSE, include=TRUE-----------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: Cree funciones que manejen matrices en R  (realice al menos dos ejercicios que requieran cargar archivos externos *.csv cada uno con al menos 50 filas y tres datos por fila, lea y procese la información del archvo leído, y guarde las respuestas a los ejercicios  en archivos independientes tipo *.txt)
## 
##  4. Fuentes:
##     https://www.generatedata.com"


## ----------------------------------------------------------------------------------
# Importamos la base:
sandl <- read.csv(file = "sandl.csv", header = T, sep = ",")

# Ahora convertimos en matriz a la base:
msandl <- as.matrix(sandl)

## Ahora haremos una función que sume los terminos y saque la columna ganadora de snake and ladder:

ganador <- function(x){
  "Recibe una matriz de tres juagadores de snakes and ladder y saca el ganador según el que tenaga mayor suma"
  a <- sum(x[,1])
  b <- sum(x[,2])
  c <- sum(x[,3])
  v <- c(a,b,c)
  count = 0
  for (i in v) {
    count = count + 1
    if(i == max(v)){
      g <- paste("El Ganador de Snakes and Ladder es el Jugador numero: ", count)
      return(g)
      break
    }
  }
}

## Miramos el resulado:
ganador(msandl)

## Exportamos el resultado:
win <- ganador(msandl)
write.table(win, file = "GanadorSnake&Ladder.txt", row.names = F)


## ----------------------------------------------------------------------------------
# Importamos la base:
base2 <- read.csv(file = "base2.csv", sep = ",", header = T)

#pasamos a matriz:
cantidad <- as.matrix(base2)

## Creamos la matriz de precio en delos 3 negocios:
vidrio <- c(1500,2000,1900)
personal <- c(2500,2000,2000)
mega <- c(6500,5500,6000)

catalogo <- as.matrix(rbind(vidrio, personal, mega))

#ahora creamos la funcion:
precio <- function(x){
  "Calcula el mejor precio por cada presentación de gaseosa, según el catalogo"
  p <- x %*% catalogo
  v <- c(min(p[,1]), min(p[,2]), min(p[,3]))
  count <- 0
  for (i in v) {
    count = count + 1
    if(i == min(v)){
      price <- paste("El mejor precio es: ", min(v), " | la que se llevo el negocio es la distribuidora numero: ", count)
      return(price)
      break
    }
  }
}

precio(cantidad)

# exportamos el resultado:
write.table(precio(cantidad), file = "MejorPrecio.txt", row.names = F)