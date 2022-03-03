# ClienteAPI_SIAR_NACIONAL
Acceso a la API del Servicio de Información al Regante (SIAR) para extraer información de sus redes de estaciones

Para su funcionamiento requiere la existencia de la variable API_KEY_SIAR_MAPA = "API KEY PROPORCIODADA POR EL RESPONSABLE DEL SIAR"

Proporciona dos funciones para el acceso a la información que proporciona esta API:

get_SIARNacional_DatosHorarios(tipoDato = 'Horarios', CCAA = NULL, Estacion = NULL, Provincia = NULL, FechaInicial = NULL, FechaFinal = NULL, checkType = TRUE)

Que genera una data.frame con los datos horarios de las estaciones.

get_SIARNacional_stationName(stationCode) que es un ejemplo de uso de uno de los métodos de la API para ofrecer el nombre de la estación

