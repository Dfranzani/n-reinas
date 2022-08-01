# Problema de las n-reinas

El problema de las reinas de ajedrez o mejor conocido como el de las $n$ - reinas, consiste en poner \$n\$ reinas en un tablero de ajedrez de dimensión \$nxn\$ casillas (un grilla o matriz cuadrada) de tal manera que ninguna reina pueda "capturar" a otra (una reina puede capturar a otra si se encuentra en su misma fila, columna o diagonal). Por ende, la finalidad de problema es encontrar la (s) distribución (es) de las \$n\$ reinas en el tablero.

Abordaremos el problema bajo la perspectiva de búsqueda con un enfoque desinformado, es decir, no conocemos característica alguna de la solución (estado final). Esto implica, que debemos usar las condiciones descritas en problema para poder evaluar si una solución es factible. Para llevar a cabo la búsqueda, realizaremos una del tipo en profundidad y otra del tipo en paralelo.

Puedes revisar en mi [blog](https://dfranzani.netlify.app/posts/2021-09-19-busqueda/) el desarrollo de problema paso a paso, además encontrarás documentación de apoyo.
