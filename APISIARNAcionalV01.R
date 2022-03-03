# Environment Variables
urlRoot_SIARNacional <- "https://servicio.mapama.gob.es/apisiar/API/v1"
if(exists("API_KEY_SIAR_MAPA"))
{
	token_SIARNacional <- API_KEY_SIAR_MAPA # From Passwords file 
}else
{
	stop("Para usar este procedimiento es necesario contar con una variable llamada: \"API_KEY_SIAR_MAPA\" que contenga la API_KEY que da acceso a los datos del SIAR NAcional")
}

DEBUG = FALSE # sin uso

Sys.setenv(tz = 'UTC')


# Load packages
library(httr)
#require(RCurl)
require(rjson)
library(lubridate)
library(xts)

# Inicio del proceso
mensajeLog <- "Carga del cliente API de acceso al SIAR Nacional"
print(mensajeLog)


add_token <- function(url, info = FALSE) # La API v1 tiene una inconsistencia entre los dos apartados al añadir el token, se añaden de formas diferentes
{
	if(info)
	{
		url <- paste0(url, "?ClaveAPI=", token_SIARNacional )
	}else
	{
		url <- paste0(url, "&ClaveAPI=", token_SIARNacional )
	}
	return(url)
}
#Test
#add_token("hola")



get_SIARNacional_DatosHorarios <- function(tipoDato = 'Horarios', CCAA = NULL, Estacion = NULL, Provincia = NULL, FechaInicial = NULL, FechaFinal = NULL, checkType = TRUE)
{
	if(!is.null(CCAA))
	{
		url =  paste0(urlRoot_SIARNacional, '/datos/', tipoDato, '/CCAA?Id=', CCAA)
	}else if(!is.null(Estacion))
	{
		url =  paste0(urlRoot_SIARNacional, '/datos/', tipoDato, '/Estacion?Id=', Estacion)
	} else if(!is.null(Provincia))
	{
			url =  paste0(urlRoot_SIARNacional, '/datos/', tipoDato, '/Provincia?Id=', Provincia)
	}

	if(is.null(FechaInicial)) FechaInicial = Sys.Date()-1
	if(is.null(FechaFinal)) FechaFinal = Sys.Date()

	url = paste0(url, '&FechaInicial=', strftime(FechaInicial, '%Y-%m-%d'), '&FechaFinal=', strftime(FechaFinal, '%Y-%m-%d'))

	url <- add_token(url)
	resp <- GET(url)
	
	if(resp$status_code != 200)
	{
		stop(message = paste("En get_SIARNacional_DatosSemiHorarios API http: response not 200. URL:", url, '\nMensaje:', resp$headers$mensajerespuesta), call. = FALSE) # content(resp, type="text", encoding = 'UTF-8')
	}
	if(http_type(resp) != "application/json" && checkType)
	{
		stop(message = "En get_SIARNacional_DatosSemiHorarios API did not return json", call. = FALSE)
	}
	
	datosBrutos = fromJSON(content(resp, type="text"))
	
# > str(datosBrutos$Datos[[49]],1)
# List of 11
 # $ Fecha        : chr "2020-06-02T00:00:00"
 # $ HoraMin      : num 30
 # $ TempMedia    : num 14.9
 # $ HumedadMedia : num 85.1
 # $ VelViento    : num 0.739
 # $ DirViento    : num 159
 # $ Radiacion    : num 0
 # $ Precipitacion: num 0
 # $ TempSuelo1   : NULL
 # $ TempSuelo2   : NULL
 # $ Estacion     : chr "SE03"
							# Los datos brutos pueden incluir NULLs y son listas de listas.
	
	
	extraeDatos <- function(Datos)
	{
		res <- do.call(rbind, lapply(Datos, rbind)) # https://stackoverflow.com/questions/22870198/is-there-a-more-efficient-way-to-replace-null-with-na-in-a-list
		nullToNA <- function(x) 
		{
			x[sapply(x, is.null)] <- NA
			return(x)
		}
		res <- nullToNA(res)
	
		res <- data.frame(res)
	
		for(elemento in 1:length(res))
		{
			res[,elemento] <- unlist(res[,elemento])
		}
		return(res)
	}
	
	datos = extraeDatos(datosBrutos$Datos)
	
# >str(datos)

# 'data.frame':   1440 obs. of  11 variables:
 # $ Fecha        : chr  "2020-06-01T00:00:00" "2020-06-01T00:00:00" "2020-06-01T00:00:00" "2020-06-01T00:00:00" ...
 # $ HoraMin      : num  30 100 130 200 230 300 330 400 430 500 ...
 # $ TempMedia    : num  15.1 15 14.7 14.3 15.2 ...
 # $ HumedadMedia : num  73.5 73.4 75.2 80.8 78.9 79.6 78.6 81.4 82.9 84.7 ...
 # $ VelViento    : num  0.53 1.154 1.297 0.966 0.992 ...
 # $ DirViento    : num  198 214 194 141 147 ...
 # $ Radiacion    : num  0 0 0 0 0 0 0 0 0 0.36 ...
 # $ Precipitacion: num  0 0 0 0 0 0 0 0 0 0 ...
 # $ TempSuelo1   : logi  NA NA NA NA NA NA ...
 # $ TempSuelo2   : logi  NA NA NA NA NA NA ...
 # $ Estacion     : chr  "SE03" "SE03" "SE03" "SE03" ...
								# Las fechas tienen un formato extraño, parece herencia del CR10X
								
	formateaFecha <- function(datos)
	{
		Fecha = datos$Fecha
		HoraMin = datos$HoraMin
		Fecha = as.POSIXct(Fecha, format = '%Y-%m-%dT00:00:00')
		hora = ifelse(nchar(HoraMin) < 3, 0, as.integer(substr(HoraMin, 1, nchar(HoraMin) - 2)))
		minuto = as.integer(substr(HoraMin, nchar(HoraMin) - 1, nchar(HoraMin)))
		
		datos$Fecha = Fecha + hora*3600 + minuto * 60
		
		return(datos[,-2]) # se elimina el HoraMin
		
	}
	
	datos <- formateaFecha(datos)
  
	return(list(datosBrutos = datosBrutos, datos = datos))
   	
}  

get_SIARNacional_DatosSemiHorarios <- get_SIARNacional_DatosHorarios # retrocompatibilidad, a eliminar en futuras versiones
# # Test
#a <- get_SIARNacional_DatosSemiHorarios(CCAA = "NAV", FechaInicial = Sys.Date()-2, FechaFinal = Sys.Date()-1)
#b <- get_SIARNacional_DatosSemiHorarios(Estacion = "SE03", FechaInicial = as.Date('2020-06-01'), FechaFinal = as.Date('2020-06-30')) # Estación de Lebrija-1 Identificador: 'SE03'



get_SIARNacional_stationName <- function(stationCode = NA, checkType = TRUE)
{
	url =  paste0(urlRoot_SIARNacional, '/Info/Estaciones')
	
	url <- add_token(url, info = TRUE)
	
	resp <- GET(url)
	
	if(resp$status_code != 200)
	{
		stop(message = paste("Function get_SIARNacional_stationName API http: response not 200. URL:", url), call. = FALSE)
	}
	if(http_type(resp) != "application/json" && checkType)
	{
		stop(message = "Function get_SIARNacional_stationName API did not return json", call. = FALSE)
	}
	
	datosBrutos = fromJSON(content(resp, type="text"))
	
	extraeDatos <- function(Datos)
	{
		res <- do.call(rbind, lapply(Datos, rbind)) # https://stackoverflow.com/questions/22870198/is-there-a-more-efficient-way-to-replace-null-with-na-in-a-list
		nullToNA <- function(x) 
		{
			x[sapply(x, is.null)] <- NA
			return(x)
		}
		res <- nullToNA(res)
	
		res <- data.frame(res)
	
		for(elemento in 1:length(res))
		{
			res[,elemento] <- unlist(res[,elemento])
		}
		return(res)
	}
	
	datos = extraeDatos(datosBrutos$Datos)
	
# > str(datos, 1)
# 'data.frame':   571 obs. of  11 variables:
 # $ Estacion         : chr  "Villena" "Camp de Mirra" "Vila Joiosa" "Ondara" ...
 # $ Codigo           : chr  "A01" "A02" "A03" "A04" ...
 # $ Termino          : chr  "Villena" "Campo de Mirra/Camp de Mirra, el" "Villajoyosa/Vila Joiosa, la" "Ondara" ...
 # $ Longitud         : chr  "005304000W" "004622000W" "001522000W" "000023000E" ...
 # $ Latitud          : chr  "384035000N" "384045000N" "383140000N" "384906000N" ...
 # $ Altitud          : num  519 589 73 38 86 629 244 259 73 58 ...
 # $ XUTM             : num  684017 693720 739183 761033 767731 ...
 # $ YUTM             : num  4283000 4283550 4267960 4300940 4298290 ...
 # $ Huso             : num  30 30 30 31 31 30 30 30 30 30 ...
 # $ Fecha_Instalacion: chr  "1999-11-10T00:00:00" "1999-11-10T00:00:00" "1999-11-11T00:00:00" "1999-11-11T00:00:00" ...
 # $ Fecha_Baja       : chr  "2011-09-14T00:00:00" NA NA NA ...
# >
	
	if(is.na(stationCode))
	{
		return(datos)
	}else
	{
		posicion = which(datos$Codigo == stationCode)
		stationName = if(length(posicion) > 0) datos$Estacion[posicion] else NA
		return(stationName)
	}	
}

#Test
# get_SIARNacional_stationName("SE03")
# get_SIARNacional_stationName(501)


mensajeLog <- "Funciones cargadas: get_SIARNacional_DatosSemiHorarios() y get_SIARNacional_stationName()"
print(mensajeLog)
df.EstacionesSIARNacional <- get_SIARNacional_stationName()
print("Adicionalmente se genera data.frame \"df.EstacionesSIARNacional\" contiene el listado completo de todas las estaciones que ofrece el servicio del SIAR Nacional")
print(paste("A partir de un Codigo de estacion, por ejemplo \"SE03\" se puede obtener el nombre de la estacion usando la función: get_SIARNacional_stationName(\"SE03\") cuyo resultado es:", get_SIARNacional_stationName("SE03")))

mensajeLog <- "Fin del proceso de carga del cliente API de acceso al SIAR Nacional"

print(mensajeLog)


# Métodos pendientes de desarrollar para poder realizar filtros

# Listado de CCAA
	#https://servicio.mapama.gob.es/apisiar/API/v1/Info/CCAA?ClaveAPI=APIKEYMAPAMA
# Límites de acceso
	#https://servicio.mapama.gob.es/apisiar/API/v1/Info/Accesos?ClaveAPI=APIKEYMAPAMA
# ¿?
	#https://servicio.mapama.gob.es/apisiar/API/v1/Info/Estaciones?ClaveAPI=APIKEYMAPAMA

# Consulta para definir medias de temperaturas nocturnas y diurnas

ObtenTemperaturasMediasNocturnasDiurnas <-function(Estacion = "SE03", FechaInicial = as.Date('2020-06-01'), FechaFinal = as.Date('2020-06-30')) # Incompleto
{
	res <- get_SIARNacional_DatosSemiHorarios(Estacion = Estacion, FechaInicial = FechaInicial, FechaFinal = FechaFinal) 
	
	datosClimaticos <- data.frame(fecha = as.POSIXct(res[1:nrow(res),1], format = '%Y-%m-%dT00:00:00'),
								hora = ifelse(nchar(res[1:nrow(res),2]) < 3, 0, as.integer(substr(res[1:nrow(res),2], 1, nchar(res[1:nrow(res),2]) - 2))),
								minuto = as.integer(substr(res[1:nrow(res),2], nchar(res[1:nrow(res),2]) - 1, nchar(res[1:nrow(res),2]))),
							#hora <- as.integer(substr(res[1:nrow(res),2], 1, nchar(res[1:nrow(res),2]) - 2))
							#fecha <- fecha + hora * 3600 + minuto * 60
								TempMd = as.numeric(res[1:nrow(res),3]))
	
	datosClimaticos$esDia <- ifelse(datosClimaticos$hora >= 9 & datosClimaticos$hora <21, TRUE, FALSE)
	datosClimaticos$esNoche <- !datosClimaticos$esDia
	names(res) <- nombres.df
	

	list(TAirDiurna <- mean(datosClimaticos$TempMd[datosClimaticos$esDia]), 
		TAirNocturna <- mean(datosClimaticos$TempMd[!datosClimaticos$esDia])
		)
	 
}
#Test
#ObtenTemperaturasMediasNocturnasDiurnas(Estacion = "SE03", FechaInicial = as.Date('2020-06-01'), FechaFinal = as.Date('2020-06-30'))
#######################################################################
