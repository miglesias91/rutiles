#' Exporto csv para subir a kaggle
#'
#' @param data dataframe con las columas 'numero_de_cliente' y 'estimulos'
#' @param clientes vector con los numeros de clientes
#' @param estimulos vector con los estimulos de cada cliente
#' @param path path del archivo de salida. Si existe, se crea un 'input_kaggle_YYYYMMDD_HHmmss.csv' por default, en la carpeta del usuario
#' @import data.table
#' @export
kaggle_csv = function(data = NULL, clientes = NULL, estimulos = NULL, path='~/input_kaggle.csv') {

  if (is.null(data) && is.null(clientes) && is.null(estimulos))
    stop ('No pueden ser todos nulos: o hay "data" o hay "clientes" + "estimulos".')

  if ((is.null(clientes) || is.null(estimulos)) && is.null(data) )
    stop('Si "data" es nulo entonces "clientes" y "estimulos" tienen que tener info.')

  #genero el archivo de salida
  if( file.exists(path) ) {
    path = sprintf('~/input_kaggle_%s.csv',format(Sys.time(),'%Y%m%d_%H%M%S'))
  }

  if (is.null(data)) {
    data = data.table('numero_de_cliente'=clientes, 'estimulos'=estimulos)
  }

  fwrite(data, sep=',',  file=path)
}

#' Calcula ganancia de la prediccion
#'
#' @param data dataframe con las columas 'real' y 'prediccion'
#' @param real vector con los valores reales de los BAJA+2
#' @param prediccion vector con los valores predichos de los BAJA+2
#' @param imprimir T = imprime por pantalla, F = no imprime
#' @param devolver T = return ganancia, F = no devuelve nada
#' @import data.table
#' @export
ganancia = function(data = NULL, real = NULL, prediccion = NULL, imprimir = T, devolver = F) {

  if (is.null(data) && is.null(real) && is.null(prediccion))
    stop ('No pueden ser todos nulos: o hay "data" o hay "clientes" + "estimulos".')

  if ((is.null(real) || is.null(prediccion)) && is.null(data) )
    stop('Si "data" es nulo entonces "clientes" y "estimulos" tienen que tener info.')

  if (is.null(data)) {
    data = data.table('real'=real, 'prediccion'=prediccion)
  }

  g = sum( data[,ifelse(prediccion==1,ifelse(real==1, 29250, -750),0)])
  cat( 'ganancia: ',  g, '\n')

  if (devolver) {
    g
  }
}

