TÍTULO GITHUB: Analisis-datos-Mobula-mobular-a-partir-de-vuelos-de-aeronave

UTILIZADO EN TRABAJO: DISTRIBUCIÓN ESPACIAL DE LA POBLACIÓN DE MOBULA MOBULAR EN EL MEDITERRÁNEO OCCIDENTAL Y SU RELACIÓN CON VARIABLES AMBIENTALES

AUTOR: Josep Alba Campins

FECHA Y VERSIÓN: 15/08/2025, versión 5

DESCRIPCIÓN: 
El código divide los vuelos en transectos regulares de 15 millas náuticas y los relaciona con los datos de avistamientos. Para la delimitación final de los transectos, inserta el transecto sobrante de la división (<15nm) en el intervalo con menor cantidad de observaciones para minimizar la pérdida de información. Finalmente se eliminan los transectos sobrantes.
A cada transecto se asocia un valor de clorofila y profundidad relacionando el punto medio de los transectos con los datos obtenidos de Copernicus y EMODnet.
Se realizan dos modelos gam (uno sin posición y otro con la posición) de tipo binomial (1 si el transecto contiene observaciones y 0 si no las contiene) y enlace logit para evaluar la relación de las variables estudiadas (hora, clorofila, profundidad y posición) con la probabilidad de presencia de Mobula mobular.
Cada modelo tiene los resultados de relación, validación de los modelos y figuras gráficas asociadas.

IMPUTS NECESARIOS:
-	Archivo Excel con una hoja “Transectos” con datos de vuelos de aeronave. Los vuelos han sido procesados anteriormente y solo siguen líneas rectilíneas. Para cada vuelo solo tiene que haber un punto inicial y un punto final (sin puntos intermedios). El Excel debe contener las siguientes columnas (cada fila es un vuelo):
    o	Día = día donde se ha realizado cada vuelo
    o	Año = año donde se ha realizado el vuelo (para datos de varios años)
    o	ID_T = identificación de cada vuelo
    o	hora0 = hora inicial de cada vuelo (formato HH:MM:SS)
    o	horaf = hora final de cada vuelo (formato HH:MM:SS)
    o	Lat0 = coordenada latitud inicial de cada vuelo (formato decimal)
    o	Lon0 = coordenada longitud inicial de cada vuelo (formato decimal)
    o	Latf = coordenada latitud final de cada vuelo (formato decimal)
    o	Lonf = coordenada longitud final de cada vuelo (formato decimal)
    o	Adicionalmente puede contener columnas de tiempo de vuelo, distancia vuelo, etc.

-	Archivo Excel con una hoja “Avistamientos” que contenga las siguientes columnas respecto a los avistamientos (cada fila es un avistamiento):
    o	Día = día donde se ha realizado el avistamiento
    o	Año = año del avistamiento (para datos de distintos años)
    o	Hora = hora del avistamiento (formato HH:MM:SS)
    o	Lat = coordenada latitud del avistamiento (formato decimal)
    o	Long = coordenada longitud del avistamiento (formato decimal)
    o	Ind = número de individuos observados en el avistamiento.

-	Shapefiles de costa y batimetría de la zona de estudio. El código utiliza archivos de la zona marítima alrededor de las Islas Baleares propiedad del Instituto Español de Oceanografía (IEO).

-	Datos de concentración de clorofila superficial obtenidos a través de Copernicus en formato .nc (se utiliza modelo de observación satelital L4).

-	Datos de batimetría obtenidos a través de EMODnet en formato .csv

OUTPUTS OBTENIDOS:
-	Un dataframe (convertible en archivo Excel para futuras ampliaciones) que contiene la siguiente información por fila:
    o	Transectos regulares de únicamente 15 millas náuticas de longitud (nm).
    o	Horas iniciales y finales de cada transecto
    o	Coordenadas iniciales y finales de cada transecto
    o	Observaciones que contiene el transecto (puede ser > 1)
    o	Suma de individuos que contiene el transecto
    o	Hora del punto medio del transecto
    o	Coordenadas del punto medio del transecto
    o	Concentración de clorofila (mg/m3) asociada al punto medio de cada transecto
    o	Profundidad (-m) asociada al punto medio de cada transecto
    o	Columna carácter binomial según el nº de observaciones (1 si hay observaciones y 0 si no contiene observaciones).

-	Mapa de la distribución de los puntos medios de cada transecto separados según si contienen observaciones (puntos verdes) o no (puntos rojos). El radio de los puntos verdes es proporcional al número de individuos relacionados.

-	Resultados de la existencia o no de las variables estudiadas con la probabilidad de presencia (resultados modelos gam), con la validación de los modelos.

-	Mapa en la zona de estudio de como varia la probabilidad de presencia (según modelo gam con posición). 
