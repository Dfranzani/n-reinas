# Primera opción: búsqueda en paralelo de la cantidad de soluciónes
option1 = function(){
  
  n = as.integer(readline("Number of Queens?: "))
  solutions = list()
  
  Queens = function(positions) {
    
    # Guarda la solución al momento de obtener 8 posiciones que cumplen
    # las condiciones
    if (length(positions) == n) {
      solutions[[length(solutions)+1]] <<- positions
    }
    
    # Definimos las columnas posibles en donde puede ir una reina
    possible.colums = setdiff(1:n, positions)
    
    # Determinamos las diagonales matriciales en donde no puede ir una reina
    out.diags = seq(length(positions), 1)
    diags = c(positions + out.diags, positions - out.diags)
    diags = diags[diags >= 1 & diags <= n]
    
    # Eliminamos las casillas diagonales, de las casillas posibles en donde
    # ubicar a una reina
    possible.colums = setdiff(possible.colums, diags)
    
    # Para cada posibilidad se evalua nuevamente la función
    if(length(possible.colums > 0)){
      apply(matrix(possible.colums), 1, FUN = function(p){
        Queens(c(positions,p)) # Se añaden las opciones posibles
        # a cada iteración
      })
    }
  }
  
  Queens(c())
  
  # Imprimimos la cantidad de soluciones posibles
  print(paste("All solutions: ",length(solutions)))
  
  # Transformar soluciones en matrices
  solutions = lapply(solutions, function(sol){
    M = matrix(0, ncol = n, nrow = n)
    for (i in 1:n) {
      M[i,sol[i]] = 1
    }
    return(M)
  })
  
  # Retorna las matrices solución
  return(solutions)
}

r1 = option1()

# Segunda opción: búsqueda en profundidad de las soluciones
option2 = function(queens = 4){
  
  queens = as.integer(readline("Number of Queens?: "))
  possible = function(aux.square,i,j,valor){
    
    aux.square[i,j] = valor # Colocamos la reina el la posición a probar
    
    # Criterio de suma de columnas
    aux.col = apply(aux.square, 2, FUN = function(colum){
      if(sum(colum) <= 1) return(TRUE)
      else return(FALSE)
    })
    aux.col = ifelse(sum(!aux.col) == 0, TRUE, FALSE)
    
    #Criterio de suma de filas
    aux.row = apply(aux.square, 1, FUN = function(rows){
      if(sum(rows) <= 1) return(TRUE)
      else return(FALSE)
    })
    aux.row = ifelse(sum(!aux.row) == 0, TRUE, FALSE)
    
    # Diagonales principaes del tablero
    diag.principals = c(0,0) 
    
    for (i in 1:queens) {
      diag.principals[1] = sum(diag.principals[1],
                               aux.square[i,i])
      diag.principals[2] = sum(diag.principals[2],
                               aux.square[i,(queens - i + 1)])
    }
    
    diag.principals = ifelse(diag.principals <= 1, TRUE, FALSE)
    diag.principals = ifelse(sum(diag.principals) == 2, TRUE, FALSE)
    
    # Las diagonales secundarias están determinadas por las
    # coordenadas de la primera fila, que están entre la
    # primera y última casilla de dicha fila.
    
    # Primero determinamos los puntos clave en la primer fila
    key.points = matrix(c(rep(1,(queens - 2)), seq(2,(queens - 1))),
                        ncol = 2,
                        nrow = (queens - 2))
    
    # Función que determina las diagonales secundarias
    # mediante los puntos clave
    
    diags = function(aux.key.points){
      
      # Suma de los valores de la diagonal
      suma.diag = function(M){
        x = M[1,]; y = M[2,]
        # Cantidad de puntos entre los extremos de la diagonal
        n = abs(y[1] -  x[1]) - 1
        suma = aux.square[x[1],x[2]] + aux.square[y[1],y[2]]
        if(n >0){
          for (i in 1:n) {
            if(x[1] < y[1] & x[2] > y[2]) suma = suma + aux.square[(x[1]+i),
                                                                   (x[2]-i)]
            if(x[1] < y[1] & x[2] < y[2]) suma = suma + aux.square[(x[1]+i),
                                                                   (x[2]+i)]
            if(x[1] > y[1] & x[2] < y[2]) suma = suma + aux.square[(x[1]-i),
                                                                   (x[2]+i)]
            if(x[1] > y[1] & x[2] > y[2]) suma = suma + aux.square[(x[1]-i),
                                                                   (x[2]-i)]
          }
        }
        
        if(suma <= 1) return(TRUE)
        else return(FALSE)
      }
      
      report.diag = list()
      
      # Búsqueda de todas las diagonales.
      # Cada punto clave genera 4 diagonales
      for (i in 1:dim(aux.key.points)[1]) {
        first.diag = rbind(aux.key.points[i,],
                           c(aux.key.points[i,2],aux.key.points[i,1]))
        second.diag = rbind(first.diag[2,],
                            first.diag[2,]+c(1,1)*(queens - aux.key.points[i,2]))
        third.diag = rbind(second.diag[2,], c(second.diag[2,2],second.diag[2,1])) 
        fourth.diag = rbind(third.diag[2,], first.diag[1,])
        aux.diags = list(first.diag, second.diag,third.diag, fourth.diag)
        
        # Se verifica la suma en todas las diagonales
        aux.sumas = lapply(aux.diags, function(x){ 
          return(suma.diag(x))
        })
        report.diag[[i]] = unlist(aux.sumas)
      }
      return(report.diag)
    }
    
    diag.secundarias = diags(key.points)
    # Condición general sobre todas las diagonales
    diag.secundarias = ifelse(sum(!unlist(diag.secundarias)) == 0, TRUE, FALSE)
    diagonals = ifelse(diag.principals == TRUE &
                         diag.secundarias == TRUE, TRUE, FALSE)
    if(aux.col == TRUE & aux.row == TRUE & diagonals == TRUE) return(TRUE) 
    else return(FALSE)
    
  }
  
  # Función recursiva que evalua todos los caminos posibles 1 a 1
  solve2 = function(board){
    for (i in 1:queens) {
      for (j in 1:queens) {
        if(board[i,j] == 0){
          if(possible(board,i,j,1)){
            board[i,j] = 1
            if(sum(board) == queens){
              solutions[[length(solutions)+1]] <<- board
              break()
            }
            solve2(board)
            board[i,j] = 0
          }
        }
      }
      if(sum(board[i,]) == 0) return()
    }
  }
  
  solutions = list()
  square = matrix(0,ncol = queens, nrow = queens)
  solve2(square)
  
  print(paste("All solutions: ",length(solutions)))
  return(solutions) # Retorna las matrices solución
}

r2 = option2()
