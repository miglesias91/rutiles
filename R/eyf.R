#' Levanta dataset y arma clase binaria
#'
#' @param dataset dataset a modificar -TIENE QUE SER EL DE LA MATERIA-
#' @param path path donde esta el csv con los clientes
#' @param nombre nombre de la clase binaria
#' @param positivo valor positivo -factor-
#' @param negativo valor negativo -factor-
#' @param fix_nulos corregir nulos
#' @import data.table
#' @import randomForest
#' @export
levantar_clientes = function(path, nombre_clase_binaria = 'baja', positivo = 'si', negativo = 'no', fix_nulos = F) {
  dataset = fread(path, stringsAsFactors= TRUE)
  dataset[, c(nombre_clase_binaria) := as.factor(ifelse(clase_ternaria == 'BAJA+2', 'si', 'no'))]

  # y elimino la clase ternaria
  dataset[, clase_ternaria := NULL]

  # corrijo nulos para correr ranger
  dataset = na.roughfix(dataset)

  return(dataset)
}

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

#' Train y test random forest usando 'ranger'
#'
#' @param a_predecir string con la variable a predecir -en función del resto de las variables-
#' @param positivo string con el valor positivo de la variable a predecir
#' @param n_split numero de variables posible a dividir en cada nodo
#' @param n_arboles numero de árboles
#' @param nodo_min tamaño minimo del nodo
#' @param profundidad_max profundidad máxima de los árboles
#' @param entrenamiento dataset de entrenamiento
#' @param evaluacion dataset de evaluación
#' @import data.table
#' @import ranger
#' @import randomForest
#' @export
rf_ranger = function(a_predecir, positivo, n_arboles, n_split, nodo_min, profundidad_max, entrenamiento, evaluacion, salida_csv, ver_ganancia = T, kagglecsv = T) {
  # corro random forest
  set.seed(200)
  modelo = ranger( formula = paste0(a_predecir, ' ~ .'),
                   data = entrenamiento,
                   probability =   TRUE,  #para que devuelva las probabilidades
                   num.trees =     n_arboles,
                   mtry =          n_split,
                   min.node.size = nodo_min,
                   max.depth =     profundidad_max
  )

  # hago la prediccion y la guardo en una data.table
  set.seed(200)
  prediccion = predict(modelo, evaluacion)
  predicciones = as.data.table(prediccion$predictions)

  # armo la columna 'estimulo':[1,0]
  predicciones[, estimulo := ifelse(si > 0.025, as.integer(1), as.integer(0))]

  if (ver_ganancia) {
    # armo data para calcular ganancia.
    dganancia = data.table(real=evaluacion[, ifelse(a_predecir==as.factor(positivo),as.integer(1),as.integer(0))], prediccion=predicciones[, estimulo])
    rutiles::ganancia(data = dganancia) # calculo ganancia
  }

  if (kagglecsv) {
    # armo data para escribir output para kaggle
    dkaggle = data.frame(numero_de_cliente=evaluacion[, numero_de_cliente], estimulo=predicciones[, estimulo])
    rutiles::kaggle_csv(dkaggle, path = salida_csv) # escribo output para kaggle
  }
}
