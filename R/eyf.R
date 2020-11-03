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

  if (nombre_clase_binaria %in% colnames(dataset) == F) {
    dataset[, c(nombre_clase_binaria) := as.factor(ifelse(clase_ternaria == 'BAJA+2', positivo, negativo))]

    # y elimino la clase ternaria
    dataset[, clase_ternaria := NULL]
  }

  # corrijo nulos para correr ranger
  if (fix_nulos) {
    dataset = na.roughfix(dataset)
  }

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
    data = data.table('numero_de_cliente'=clientes, 'estimulo'=estimulos)
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
rf_ranger = function(a_predecir, positivo, n_arboles, n_split, nodo_min, profundidad_max, entrenamiento, evaluacion, salida_csv = '~/rf_ranger.csv', ver_ganancia = T, kagglecsv = T) {
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

  positivos = prediccion[, ..positivo]
  # armo la columna 'estimulo':[1,0]
  predicciones[, estimulo := ifelse(positivos > 0.025, as.integer(1), as.integer(0))]

  if (ver_ganancia) {
    # armo data para calcular ganancia.
    dganancia = data.table(real=as.integer(evaluacion[, ..a_predecir] == positivo), prediccion=predicciones[, estimulo])
    rutiles::ganancia(data = dganancia) # calculo ganancia
  }

  if (kagglecsv) {
    # armo data para escribir output para kaggle
    dkaggle = data.frame(numero_de_cliente=evaluacion[, numero_de_cliente], estimulo=predicciones[, estimulo])
    rutiles::kaggle_csv(dkaggle, path = salida_csv) # escribo output para kaggle
  }

  return(modelo)
}


#' Train y test árbol de regresión usando 'rpart'
#'
#' @param a_predecir string con la variable a predecir -en función del resto de las variables-
#' @param positivo string con el valor positivo de la variable a predecir
#' @param complejidad complejidad utilizada para partir los nodos
#' @param n_split numero de variables posible a dividir en cada nodo
#' @param nodo_min tamaño minimo del nodo
#' @param profundidad_max profundidad máxima de los árboles
#' @param entrenamiento dataset de entrenamiento
#' @param evaluacion dataset de evaluación
#' @param n_cv número de veces a realizar cross validation
#' @import data.table
#' @import rpart
#' @export
rt_rpart = function(a_predecir, positivo, complejidad, n_split, nodo_min, profundidad_max, entrenamiento, evaluacion, n_cv = 0,
                    salida_csv = '~/rt_rpart.csv', ver_ganancia = T, kagglecsv = T, devolver_modelo = F) {
  # corro rpart
  set.seed(2020)
  modelo = rpart(formula = paste0(a_predecir, ' ~ .'),
                 data= entrenamiento,
                 xval= n_cv,
                 cp=         complejidad,
                 maxdepth=  profundidad_max,
                 minsplit=  n_split,
                 minbucket=  nodo_min
  )

  # hago la prediccion y la guardo en una data.table
  set.seed(200)
  prediccion = predict(modelo, evaluacion)
  predicciones = as.data.table(prediccion)

  positivos = predicciones[, ..positivo]
  # armo la columna 'estimulo':[1,0]

  predicciones[, estimulo := ifelse(positivos > 0.025, as.integer(1), as.integer(0))]

  if (ver_ganancia) {
    # armo data para calcular ganancia.
    dganancia = data.table(real=as.integer(evaluacion[, ..a_predecir] == positivo), prediccion=predicciones[, estimulo])
    rutiles::ganancia(data = dganancia) # calculo ganancia
  }

  if (kagglecsv) {
    # armo data para escribir output para kaggle
    dkaggle = data.frame(numero_de_cliente=evaluacion[, numero_de_cliente], estimulo=predicciones[, estimulo])
    rutiles::kaggle_csv(dkaggle, path = salida_csv) # escribo output para kaggle
  }

  if (devolver_modelo){
    return(modelo)
  }
}

#' Ingeniería de features para el dataset crudo de clientes.
#'
#' @param d dataset clientes original -se devuelve copia-
#' @param historico_desde mes desde el cual se empieza el procesamiento -luego, se procesan los meses anteriores-
#' @param ventana_historico cantidad de meses para calcular valores históricos
#' @param max_tarjetas si calcula o no el máximo de las tarjetas
#' @param min_tarjetas si calcula o no el mínimo de las tarjetas
#' @param borrar_originales_de_tarjetas si elimina las variables originales de tarjetas
#' @param correccion_catedra si aplica la correción de la catedra
#' @param acum_historico si calcula el acumulado en los meses historicos
#' @param var_historico si calcula la varianza en los meses historicos
#' @param diff_historico si calcula la diferencia en los meses historicos
#' @import data.table
#' @import stringr
#' @import future.apply
#' @import future
#' @export
feature_eng = function(d, historico_desde = 202001, ventana_historico = 2,
                       max_tarjetas = T, min_tarjetas = T,
                       borrar_originales_de_tarjetas = T, correccion_catedra = T,
                       acum_historico = T, var_historico = T, diff_historico = T,
                       log = T) {

  plan(multiprocess)

  # CORRECCIÓN DE GUSTAVO
  if (correccion_catedra) {
    if (log) cat('corrección de catedra\n')

    dataset[ foto_mes==201701,  ccajas_consultas   := NA ]
    dataset[ foto_mes==201702,  ccajas_consultas   := NA ]

    dataset[ foto_mes==201801,  internet   := NA ]
    dataset[ foto_mes==201801,  thomebanking   := NA ]
    dataset[ foto_mes==201801,  chomebanking_transacciones   := NA ]
    dataset[ foto_mes==201801,  tcallcenter   := NA ]
    dataset[ foto_mes==201801,  ccallcenter_transacciones   := NA ]
    dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
    dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
    dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
    dataset[ foto_mes==201801,  ccajas_transacciones   := NA ]
    dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
    dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
    dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
    dataset[ foto_mes==201801,  ccajas_otras   := NA ]

    dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
    dataset[ foto_mes==201806,  ccallcenter_transacciones   :=  NA ]

    dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
    dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos  := NA ]

    dataset[ foto_mes==201905,  mrentabilidad     := NA ]
    dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
    dataset[ foto_mes==201905,  mcomisiones       := NA ]
    dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
    dataset[ foto_mes==201905,  mactivos_margen  := NA ]
    dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
    dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
    dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

    dataset[ foto_mes==201910,  mpasivos_margen  := NA ]
    dataset[ foto_mes==201910,  mactivos_margen  := NA ]
    dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
    dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
    dataset[ foto_mes==201910,  mcomisiones       := NA ]
    dataset[ foto_mes==201910,  mrentabilidad     := NA ]
    dataset[ foto_mes==201910,  mrentabilidad_annual     := NA ]
    dataset[ foto_mes==201910,  chomebanking_transacciones   := NA ]
    dataset[ foto_mes==201910,  ctarjeta_visa_descuentos   := NA ]
    dataset[ foto_mes==201910,  ctarjeta_master_descuentos   := NA ]
    dataset[ foto_mes==201910,  mtarjeta_visa_descuentos   := NA ]
    dataset[ foto_mes==201910,  mtarjeta_master_descuentos    := NA ]
    dataset[ foto_mes==201910,  ccajeros_propios_descuentos   := NA ]
    dataset[ foto_mes==201910,  mcajeros_propios_descuentos   := NA ]

    dataset[ foto_mes==202001,  cliente_vip   := NA ]
  }

  # VARIABLES DE TARJETAS
  # hago la suma y el minimo de las variables de las tarjetas.
  renombradas = c(paste0('Master_', sub('_master_', '_', str_subset(names(dataset), '_master_'))), paste0('Visa_', sub('_visa_', '_', str_subset(names(dataset), '_visa_'))))
  a_renombrar = c(str_subset(names(dataset), '_master_'), str_subset(names(dataset), '_visa_'))
  setnames(dataset, a_renombrar, renombradas)

  variables_tarjetas = substring(str_subset(names(dataset), 'Visa'), str_length('Visa_') + 1)

  if (log) cat('procesando variables de tarjeta\n')
  for (v in variables_tarjetas) {
    visa_y_master = c(paste0('Visa_',v), paste0('Master_',v))

    max = paste0('max_', v)
    min = paste0('min_', v)

    if (log) cat('procesando:', v, '\n')

    # maximio
    if (max_tarjetas) {
      dataset[,
              (max) := list(do.call(pmax, c(.SD, list(na.rm = T)))),
              .SDcols = visa_y_master]
    }

    # mininimo
    if (min_tarjetas) {
      dataset[,
              (min) := list(do.call(pmin, c(.SD, list(na.rm = T)))),
              .SDcols = visa_y_master]
    }
  }

  if (borrar_originales_de_tarjetas) {
    if (log) cat('borrando originales de tarjeta\n')
    de_tarjetas = str_subset(names(dataset), 'Visa|Master')
    data.table::set(dataset, i = NULL, j = de_tarjetas, value = NULL)
  }

  # en cada fila, calculamos para las primeras N variables:
  # - ACUMULADO en los últimos M meses, empezando del mes PRESENTE
  # - DIFERENCIA en los últimos M meses, empezando del mes PRESENTE

  if (log) cat('procesando historicos\n')
  if (ventana_historico != 0) {

    variables = names(dataset)
    variables = variables[!( variables %in% c('numero_de_cliente', 'foto_mes', 'baja'))]

    n_acum = paste0('acum_', variables)
    n_var = paste0('var_', variables)
    n_diff = paste0('diff_', variables)
    n_todo = c(n_acum, n_var, n_diff)

    # me qedo con los meses que voy a recorrer
    meses = unique(dataset$foto_mes)
    meses = meses[1:match(historico_desde, meses)]

    # doy vuelta la lista de meses para arrancar por el último
    meses = rev(meses)

    # empiezo a procesar para cada mes
    for (mes in meses) {
      if (log) cat('procesando historicos de mes', mes, '\n')

      # calculo suma y varianza para cada una de las N variables
      desde = meses[match(mes, meses) + ventana_historico]
      # si es NA, entonces seteo en más pequeño
      if (is.na(desde)) {
        if (log) cat('alcanzado el ultimo mes. Saliendo del for y saliendo del script.\n')

        dataset[foto_mes < mes, (n_acum) := NA]
        dataset[foto_mes < mes, (n_var) := NA]
        dataset[foto_mes < mes, (n_diff) := NA]
        break
      }
      hasta = mes

      # TODO JUNTO
      if (log) cat('procesando TODO JUNTO acum, var y diff historica de mes', mes, '\n')
      dataset[desde <= foto_mes & foto_mes <= hasta,
              (n_todo) := c(future_lapply(.SD, function(x) {return (sum(x, na.rm =T) / (max(x) - min(x)))} ),
                            future_lapply(.SD, function(x) { return( sd(x, na.rm = T) / (max(x) - min(x)))} ),
                            future_lapply(.SD, function(x) { return (sum(diff(x), na.rm = T) / mean(x))} )),
              by = numero_de_cliente,
              .SDcols = variables
              ]

      # acumulado
      # if (acum_historico) {
      #   if (log) cat('procesando acumulacion historica de mes', mes, '\n')
      #
      #   dataset[desde <= foto_mes & foto_mes <= hasta,
      #           # (n_acum) := c(lapply(.SD, sum, na.rm = T)),
      #           (n_acum) := c(lapply(.SD, function(x) {return (sum(x, na.rm =T) / (max(x) - min(x)))} )),
      #           # PROBAR: function(x) {sum(x, na.rm =T) / (max(x) - min(x))}
      #           by = numero_de_cliente,
      #           .SDcols = variables
      #           ]
      # }
      #
      # # varianza
      # if (var_historico) {
      #   if (log) cat('procesando varianza historica de mes', mes, '\n')
      #
      #   dataset[desde <= foto_mes & foto_mes <= hasta,
      #           # (n_var) := c(lapply(.SD, var, na.rm = T)),
      #           (n_var) := c(lapply(.SD, function(x) { return( sd(x, na.rm = T) / (max(x) - min(x)))} )),
      #           by = numero_de_cliente,
      #           .SDcols = variables
      #           ]
      # }
      #
      # # diferencia
      # if (diff_historico) {
      #   if (log) cat('procesando diferencia historica de mes', mes, '\n')
      #
      #   dataset[desde <= foto_mes & foto_mes <= hasta,
      #           # (n_diff) := c(lapply(.SD, function(x) {return (sum(diff(x), na.rm = T))} )),
      #           (n_diff) := c(lapply(.SD, function(x) { return (sum(diff(x), na.rm = T) / mean(x))} )),
      #           by = numero_de_cliente,
      #           .SDcols = variables
      #           ]
      # }
    }
  }

  if (log) cat('FIN. devuelvo dataset y termino ejecucion.\n')

  return(dataset)
}
