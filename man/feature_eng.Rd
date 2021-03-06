% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eyf.R
\name{feature_eng}
\alias{feature_eng}
\title{Ingeniería de features para el dataset crudo de clientes.}
\usage{
feature_eng(
  d,
  historico_desde = 202001,
  ventana_historico = 2,
  max_tarjetas = T,
  min_tarjetas = T,
  borrar_originales_de_tarjetas = T,
  correccion_catedra = T,
  acum_historico = T,
  var_historico = T,
  diff_historico = T,
  log = T,
  grabar_temp = T,
  temp = "-"
)
}
\arguments{
\item{d}{dataset clientes original -se devuelve copia-}

\item{historico_desde}{mes desde el cual se empieza el procesamiento -luego, se procesan los meses anteriores-}

\item{ventana_historico}{cantidad de meses para calcular valores históricos}

\item{max_tarjetas}{si calcula o no el máximo de las tarjetas}

\item{min_tarjetas}{si calcula o no el mínimo de las tarjetas}

\item{borrar_originales_de_tarjetas}{si elimina las variables originales de tarjetas}

\item{correccion_catedra}{si aplica la correción de la catedra}

\item{acum_historico}{si calcula el acumulado en los meses historicos}

\item{var_historico}{si calcula la varianza en los meses historicos}

\item{diff_historico}{si calcula la diferencia en los meses historicos}
}
\description{
Ingeniería de features para el dataset crudo de clientes.
}
