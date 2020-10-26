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
rt_rpart = function(a_predecir, positivo, complejidad, n_split, nodo_min, profundidad_max, entrenamiento, evaluacion, n_cv = 0, salida_csv = '~/rt_rpart.csv', ver_ganancia = T, kagglecsv = T) {
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

  return(modelo)
}

#' Ingeniería de features para el dataset crudo de clientes.
#'
#' @param d dataset clientes original -se devuelve copia-
#' @param desde mes desde el cual se empieza el procesamiento -luego, se procesan los meses anteriores-
#' @param top_variables_importantes número de variables importantes a tener en cuenta para el cálculo de históricos
#' @param historico_de número de meses anteriores que usa para calcular el histórico del mes actual
#' @import data.table
#' @export
feature_eng = function(d, historico_desde = 202001, top_variables_importantes = 10, historico_de = 7,
                       combinar_tarjetas = T, historicos_de_tarjetas = T, borrar_originales_de_tarjetas = F, correccion_catedra = T) {
  # CORRECCIÓN DE GUSTAVO
  if (correccion_catedra) {
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

  if (combinar_tarjetas) {
    variables_tarjetas = substring(str_subset(names(dataset), 'Visa'), str_length('Visa_') + 1)

    for (v in variables_tarjetas) {
      visa = paste0('Visa_',v)
      master = paste0('Master_',v)
      visa_y_master = c(visa,master)

      suma = paste0('suma_', v)
      min = paste0('min_', v)
      suma_min = c(suma,min)

      dataset[,
              (suma_min) := list(rowSums(.SD, na.rm = T), do.call(pmin, c(.SD, list(na.rm = T)))),
              .SDcols = visa_y_master]
    }

    if (borrar_originales_de_tarjetas) {
      de_tarjetas = str_subset(names(dataset), 'Visa|Master')
      data.table::set(dataset, i = NULL, j = de_tarjetas, value = NULL)
    }
  }

  # HISTÓRICOS DE LAS VARIABLES IMPORTANTES
  # Establecemos una lista con las variables más importantes, entrenando rpart para los meses [201906, 201909].
  # Se sacaron las variables pertenecientes a las tarjetas porque ya se procesaron antes.
  # Para cada una de las elegidas, se calculan valores históricos.

  variables_importantes =
    c('mcuentas_saldo'
      , 'mcuenta_corriente'
      , 'mdescubierto_preacordado'
      , 'ctrx_quarter'
      , 'mcaja_ahorro'
      , 'mrentabilidad'
      , 'mactivos_margen'
      , 'cproductos'
      , 'ctarjeta_visa'
      , 'mprestamos_personales'
      , 'mrentabilidad_annual'
      , 'ctarjeta_master'
      , 'cprestamos_personales'
      , 'mpasivos_margen'
      # , 'numero_de_cliente' no la tengo en cuenta porque no varía
      , 'ccomisiones_otras'
      , 'cseguro_vida'
      , 'mcomisiones'
      , 'cliente_edad'
      , 'cliente_antiguedad'
      , 'thomebanking'
      , 'cextraccion_autoservicio'
      , 'mcomisiones_otras'
      , 'mcaja_ahorro_dolares'
      , 'mextraccion_autoservicio'
      , 'catm_trx'
      , 'internet'
      , 'active_quarter'
      , 'mttarjeta_visa_debitos_automaticos'
      # , 'foto_mes' no la tengo en cuenta porque claramente siempre aumenta en 1, salvo cuando cambia de año.
      , 'cseguro_accidentes_personales'
      , 'ctarjeta_visa_transacciones'
      , 'mtarjeta_visa_consumo'
      , 'ctarjeta_visa_debitos_automaticos'
      , 'cseguro_vivienda'
      , 'ccaja_ahorro'
      , 'mpayroll'
      , 'matm'
      , 'cpayroll_trx'
      , 'ccajas_depositos'
      , 'tpaquete3'
      , 'mautoservicio'
      , 'ccajas_consultas'
      , 'ccajas_otras'
      , 'mcomisiones_mantenimiento'
      , 'mtarjeta_master_consumo'
      , 'ctarjeta_debito_transacciones'
      , 'ccheques_depositados_rechazados'
      , 'mcheques_depositados_rechazados'
      , 'mcuenta_debitos_automaticos'
      , 'tcuentas'
      , 'mtransferencias_recibidas'
      , 'mcheques_emitidos_rechazados'
      , 'ccheques_emitidos'
      , 'mcheques_emitidos'
      , 'ccuenta_debitos_automaticos'
      , 'ctransferencias_recibidas'
      , 'mttarjeta_master_debitos_automaticos'
      , 'chomebanking_transacciones'
      , 'ccheques_emitidos_rechazados'
      , 'mplazo_fijo_dolares'
      , 'catm_trx_other'
      , 'matm_other'
      , 'ccajas_transacciones'
      , 'mcaja_ahorro_adicional'
      , 'cpagomiscuentas'
      , 'mpagomiscuentas'
      , 'cinversion2'
      , 'ccajas_extracciones'
      , 'mtransferencias_emitidas'
      , 'cinversion1'
      , 'minversion2'
      , 'ctarjeta_debito'
      , 'ccallcenter_transacciones'
      , 'cplazo_fijo'
      , 'mforex_buy'
      , 'ccaja_seguridad'
      , 'tpaquete4'
      , 'ctarjeta_master_transacciones'
      , 'cforex_buy')

  # en cada fila, calculamos para las primeras N variables:
  # - ACUMULADO en los últimos M meses, empezando del mes PRESENTE
  # - VARIANZA en los últimos M meses, empezando del mes PRESENTE

  if (historico_de != 0) {
    presente = desde
    para_atras = historico_de # los últimos M meses
    n = top_variables_importantes # N variables más importantes a procesar

    n_importantes = variables_importantes[1:n]

    if (combinar_tarjetas && historicos_de_tarjetas) {
      n_importantes = c(n_importantes, paste0('suma_', variables_tarjetas), paste0('min_', variables_tarjetas))
    }

    n_sumas = paste0('acum_', n_importantes)
    n_var = paste0('var_', n_importantes)
    n_todas = c(n_sumas, n_var)

    # me qedo con los meses que voy a recorrer
    meses = unique(dataset$foto_mes)[1:match(presente, unique(dataset$foto_mes))]

    # doy vuelta la lista de meses para arrancar por el último
    meses = rev(meses)

    # empiezo a procesar para cada mes
    for (mes in meses) {

      # calculo suma y varianza para cada una de las N variables
      desde = meses[match(mes, meses) + para_atras]
      # si es NA, entonces seteo en más pequeño
      if (is.na(desde)) {
        desde = min(meses)
      }
      hasta = mes
      dataset[desde <= foto_mes & foto_mes <= hasta,
              (n_todas) := c(lapply(.SD, sum, na.rm = T),lapply(.SD, var, na.rm = T)),
              by = numero_de_cliente,
              .SDcols = n_importantes
              ]
    }
  }
  return(dataset)
}
