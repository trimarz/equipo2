#' Esta función descarga desde una url datos .zip o .gz, los descomprime y finalmente crea un data frame que devuelve
#'
#' @param input.url   Es la URL desde donde se obtendrán los datos
#' @param input.file  Es el nombre del fichero comprimido (.gz o .zip)
#' @param dest.file   Es el nombre del fichero en el que quedarán los datos descomprimidos
#' @param dir.data    Es el nombre de la carpeta donde se guardarán los datos descomprimidos
#' @param trace       Es el indicador de mostrar o no la traza
#' @return Un data frame obtenido de lo anterior
#' @examples
#' ReadRawData("mydatas/resultados.zip", "resultados.zip", "DataSource.csv", TRUE)
#' @export
ReadRawData <- function(input.url, input.file, dest.file, dir.data, trace = FALSE) {

  if (trace) print(paste("[*] Descarga desde ", input.url, ", descomprime el fichero y lo guarda como ", input.file))

  if (endswith(input.file,'.zip',ignore.case=TRUE)) extension <- 'zip'
  else if (endswith(input.file,'.gz',ignore.case=TRUE)) extension <- 'gz'
  else extension <- ''

  download.file(url = input.url, file.name, destfile = input.file)

  if (extension == 'zip') {
    zipfiles <- unzip(zipfile = input.file, list = T)
    input.dest.file <- zipfiles$Name[grep(pattern = dest.file, x = zipfiles$Name)]
    unzip(zipfile = input.file, exdir = dir.data, files = input.dest.file)
    input.dest.file <- file.path(getwd(), "data", input.dest.file)
    rm(zipfiles)
  }
  if (extension == 'gz')
    R.utils::gunzip(input.file)

  rm(input.file)
  return <- read.csv(input.dest.file, stringsAsFactors = FALSE)
}
