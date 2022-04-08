#https://developer.spotify.com/documentation/web-api/reference/#/operations/get-several-audio-features

### Cargar y limpiar datos ###

#Cargamos librerias
pacman::p_load(tidyverse,dplyr,stats)
#Lectura de datos para la base de datos.
df <- readRDS(file="/Users/vaguirre/Desktop/UAI/2022/1er/MINERIA DE DATOS/Proyectos/Proyecto 1/beats.rds") %>%
  filter(!(is.na(duration_ms))) %>% #Chequeamos que hayan canciones validas.
  as.tibble()

count(df)          

# Crear una nueva base de datos con las columnas necesarias para el analisis y revisamos que los atributos no tengan NA (valores nulos).
# Mantuvimos solamente el nombre y artista, ya que de esta manera es mas facil evidenciar las canciones al momento de chequear.
df1 <- df %>%
  filter(!(is.na(danceability) | is.na(energy)| is.na(key)| is.na(loudness)| is.na(mode)| is.na(speechiness)| is.na(acousticness)| is.na(instrumentalness)| is.na(liveness)| is.na(valence)| is.na(tempo)| is.na(duration_ms))) %>%
  select(track_name,artist_name,danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,duration_ms)

#Revisamos el dataframe
df1 %>% glimpse()

### Pre-procesamiento ###

#Estandarizar datos para luego realizar PCA
df_est <- df1 %>% mutate_at(c("key","loudness","mode","tempo","duration_ms"), ~(scale(.)%>%as.vector))



# Tomamos una muestra para graficar, para no saturar el grafico.
sample <- sample_n(df_est,10000)

# Canciones que duran menos de 15 min.
sample1 <- sample %>% filter(duration_ms < 900000)

ggplot(sample1, aes(duration_ms, danceability)) + 
  geom_point()

summary(df1$duration_ms)


#Resumen Kmeans: Dado un numero de K clusters, cada cluster esta asociado a un centroide
#y a cada entidad es asignado al clusters que tenga el centroide mas cerca.


modelo_kmeans = kmeans(sample1 %>% select(energy,loudness),10)


wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
    plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(filter(sample1,-track_name,-artist_name), nc = 20)




