install.packages("datos")

install.packages(c("nycflights13", "gapminder", "Lahman", "nasaweather", "babynames", "fueleconomy"))
install.packages("devtools")
devtools::session_info(c("tidyverse"))
install.packages("RcppRoll")
install.packages("ggpubr")
install.packages("maps")
install.packages("ggstance")
install.packages("lvplot")
install.packages("data.table")
install.packages("seriation")
install.packages("reshape2")
install.packages("RColorBrewer")

library(RColorBrewer)
library(reshape2)
library(seriation)
library(data.table)
library(RcppRoll)
library(ggpubr)
library(maps)
library(ggstance)
library(lvplot)
library(tidyverse)
library(ggplot2)
library(datos)
library(modelr)
library(data.table)
###Alteracion de variables visuales según clase

ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista))

ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista, color = clase)) 

ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista, size = clase))

# Izquierda
ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista, alpha = clase))

# Derecha
ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista, shape = clase))


ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista ),color = "blue")

class(millas)
summary(millas)
?millas

ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista, color=clase<8,shape = clase,stroke = 2))

?geom_point

ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista, color=clase)) +
  facet_wrap(clase ~ .)

ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista, color=clase)) +
  facet_grid(. ~ clase)

ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista, color=clase)) +
  facet_grid(. ~ cilindros)

ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista, color=clase)) +
  facet_grid(traccion ~ .)

# izquierda
ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista))

ggplot(data = millas) +
  geom_smooth(mapping = aes(x = motor, y = autopista, linetype = traccion))

ggplot(data = millas, mapping = aes(x = motor, y = autopista)) +
  geom_point(mapping = aes(color = clase)) +
  geom_smooth(data = filter(millas, clase == "subcompacto"), se = FALSE)

ggplot(data = millas, mapping = aes(x = motor, y = autopista, color = traccion)) +
  geom_point(size=2)+
  geom_smooth(se=FALSE,aes(linetype = traccion))


ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte))

demo <- tribble(
  ~corte,     ~freq,
  "Regular",   1610,
  "Bueno",     4906,
  "Muy Bueno", 12082,
  "Premium",   13791,
  "Ideal",     21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = corte, y = freq), stat = "identity")

ggplot(data = demo) +
  geom_histogram(mapping = aes(x = corte, y = freq), stat = "identity")

ggplot(data = demo) +
  geom_col(mapping = aes(x = corte, y = freq))

ggplot(data = demo,mapping = aes(x = corte, y = freq)) +
  geom_smooth()

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, y = ..prop.., group = 1))

ggplot(data = diamantes) +
  stat_summary(
    mapping = aes(x = corte, y = profundidad),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

?stat_bin
?stat_summary()
?ggplot2


ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, y = ..prop.., group = 3))

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = color, y = ..prop.., group = 3))


ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, colour = corte))

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = corte))

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = claridad))

ggplot(data = diamantes, mapping = aes(x = corte, fill = claridad)) +
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamantes, mapping = aes(x = corte, fill = claridad)) +
  geom_bar(position = "identity")

ggplot(data = diamantes, mapping = aes(x = corte, colour = claridad)) +
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamantes, mapping = aes(x = corte, fill = claridad)) +
  geom_bar(position = "dodge")

ggplot(data = diamantes, mapping = aes(x = corte, fill = claridad)) +
  geom_bar(position = "fill")

ggplot(data = diamantes, mapping = aes(x = corte, fill = claridad)) +
  geom_bar(position = "stack")


ggplot(data = millas) +
  geom_point(mapping = aes(x = motor, y = autopista), position = "jitter")

ggplot(data = millas, mapping = aes(x = ciudad, y = autopista)) +
  geom_point(position="jitter",seed = 4)

?geom_jitter
?position_jitter


ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_boxplot()

ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_boxplot() +
  coord_flip()

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()


bar <- ggplot(data = diamantes) +
  geom_bar(
    mapping = aes(x = corte, fill = corte),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar + coord_polar()

bar + coord_flip()

ggplot(data = millas, mapping = aes(x = ciudad, y = autopista)) +
  geom_point(position="jitter") +
  geom_abline() +
  coord_fixed()

seq(1, 10, length.out = 7)

mi_variable <- 10
mi_variable

library(tidyverse)

ggplot(data = millas) + 
  geom_point(mapping = aes(x = motor, y = autopista))

filter(millas, cilindros == 8)
filter(diamantes, quilate > 3)

near(sqrt(2)^2, 2)



filter(vuelos, mes == 11 | mes == 12)
nov_dic <- filter(vuelos, mes %in% c(11, 12))

a1=filter(vuelos, !(atraso_llegada > 120 | atraso_salida > 120))
a2=filter(vuelos  , atraso_llegada <= 120, atraso_salida <= 120)
a1==a2
head(vuelos)

df <- tibble(x = c(5, 2, NA))
arrange(df, x)

transmute(vuelos,
          horario_salida,
          hora = horario_salida %/% 100,
          minuto = horario_salida %% 100
)

(x <- 1:10)
lag(x)

cumsum(x)
cummean(x)

roll_medianr(x, n = 1L, weights = NULL, by = 1L, fill = NA,
             partial = FALSE, align = "right", normalize = TRUE, na.rm = FALSE)

library(RcppRoll)
df %>% mutate(media_movil = roll_mean(num, n = 4, fill = NA, align = "right"))

atrasos <- no_cancelados %>% 
  group_by(codigo_cola) %>% 
  summarise(
    atraso = mean(atraso_llegada)
  )

no_cancelados <- vuelos %>% 
  filter(!is.na(atraso_salida), !is.na(atraso_llegada))

no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(mean = mean(atraso_salida))

atrasos <- no_cancelados %>% 
  group_by(codigo_cola) %>% 
  summarise(
    atraso = mean(atraso_llegada)
  )

ggplot(data = atrasos, mapping = aes(x = atraso)) + 
  geom_freqpoly(binwidth = 10)

atrasos <- no_cancelados %>% 
  group_by(codigo_cola) %>% 
  summarise(
    atraso = mean(atraso_llegada, na.rm = TRUE),
    n = n()
  )

ggplot(data = atrasos, mapping = aes(x = n, y = atraso)) + 
  geom_point(alpha = 1/10)


atrasos %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = atraso)) + 
  geom_point(alpha = 1/10)

bateo <- as_tibble(datos::bateadores)

rendimiento_bateadores <- bateo %>% 
  group_by(id_jugador) %>% 
  summarise(
    pb = sum(golpes, na.rm = TRUE) / sum(al_bate, na.rm = TRUE),
    ab = sum(al_bate, na.rm = TRUE)
  )

rendimiento_bateadores %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = pb)) +
  geom_point() +
  geom_smooth(se = FALSE)

rendimiento_bat_desc <- rendimiento_bateadores %>% 
  arrange(desc(pb))


library(Lahman)
career2 <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

career <- Master %>%
  tbl_df() %>%
  select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID") %>%
  select(-playerID)

no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(
    primera_salida = first(horario_salida), 
    ultima_salida = last(horario_salida)
  )

no_cancelados %>% 
  count(codigo_cola, wt = distancia)

###Agrupaciones

diario <- group_by(vuelos, anio, mes, dia)
(por_dia   <- summarise(diario, vuelos = n()))
(por_mes <- summarise(por_dia, vuelos = sum(vuelos)))
(por_anio  <- summarise(por_mes, vuelos = sum(vuelos)))
diario %>% 
  ungroup() %>%             # ya no está agrupado por fecha
  summarise(vuelos = n())   

atrasos <- vuelos %>% 
  group_by(destino) %>% 
  summarise(
    conteo = n(),
    distancia = mean(distancia, na.rm = TRUE),
    atraso = mean(atraso_llegada, na.rm = TRUE)
  ) %>% 
  filter(conteo > 20)


no_cancelados %>% group_by (vuelo) %>% 
  summarise(atraso=median(atraso_llegada,na.rm=TRUE)) %>% 
  mutate(menos_15=n(atraso[atraso< -15]),
         mas_15=n(atraso[atraso>15])) %>% 

  ###EJERCICIO1  
 ### Un vuelo llega 15 minutos antes 50% del tiempo, y 15 minutos tarde 50% del tiempo.
  
vuelos %>% group_by(vuelo) %>% count()

E1=no_cancelados %>% 
  group_by(vuelo) %>% 
  summarise(menos_15 = median(atraso_llegada[atraso_llegada < -15]),
            mas_15 = median(atraso_llegada[atraso_llegada > 15])) %>% 
  filter(!is.na(menos_15)|!is.na(mas_15))

### Un vuelo llega siempre 10 minutos tarde.

no_cancelados %>% 
  group_by(codigo_cola) %>% 
  summarise(
    atraso = mean(atraso_llegada, na.rm = TRUE),
    n = n()
  ) %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = atraso)) + 
  geom_point(alpha = 1/10)

E2=no_cancelados %>% group_by(vuelo) %>% 
  summarise(media_atraso=mean(atraso_llegada)) %>% 
  filter(media_atraso==10)

E5= no_cancelados %>% group_by(vuelo) %>% 
  summarise(media_atraso_sal=round(mean(atraso_salida,na.rm=TRUE)),
            media_atraso_lleg=round(mean(atraso_llegada,na.rm=TRUE)))

no_cancelados %>% ggplot(mapping=aes(x=vuelo))+
  geom_bar(aes(x=atraso_salida,color="red"))+
  geom_bar(aes(x=atraso_llegada,color="blue"))

no_cancelados %>% group_by(vuelo) %>% 
  summarise(q1=quantile(atraso_llegada,0.99,na.rm=TRUE),
            q2=quantile(atraso_llegada,0.01,na.rm=TRUE)) %>% 
  filter(q2>120)

###Ejercicio2
no_cancelados %>% count(destino)
no_cancelados %>% group_by(destino) %>% summarise(n=n())
no_cancelados %>% count(codigo_cola, wt = distancia)
no_cancelados %>% group_by(codigo_cola) %>% summarise(n=sum(distancia))

###Ejercicio3-4

##media cancelaciones dia / mes
  vuelos %>% group_by(anio,mes,dia) %>% 
  filter(is.na(atraso_salida),is.na(atraso_llegada)) %>% 
    count() %>%
    ggplot(aes(x=dia,y=n,color=mes))+
    geom_smooth(se=TRUE)+
    facet_wrap(~ mes)

  ###Media de cancelaciones por dia
  vuelos %>% group_by(anio,mes,dia) %>% 
    filter(is.na(atraso_salida),is.na(atraso_llegada)) %>% 
    count() %>% summarise(media_dia=mean(n)) %>% 
    ggplot(aes(x=dia,y=media_dia))+
    geom_smooth(se=TRUE)
  
  ###Media de cancelaciones por mes
  vuelos %>% group_by(anio,mes,dia) %>% 
    filter(is.na(atraso_salida),is.na(atraso_llegada)) %>% 
    count() %>% summarise(media_mes=mean(n)) %>% 
    ggplot(aes(x=mes,y=media_mes))+
    geom_smooth(se=TRUE)

  vuelos %>% group_by(aerolinea, destino) %>% summarise(n())

  ###Relacion de retrasos con cnacelaciones  
  vuelos %>% group_by(anio,mes,dia) %>% 
    mutate(canc=ifelse(is.na(atraso_llegada),1,0)) %>% 
    summarise(media_retraso=(mean(atraso_llegada,na.rm=TRUE)),
              cancelacion=sum(canc)) %>% 
        ggplot(aes(x=dia,color=mes))+
    geom_smooth(aes(y=media_retraso),se=TRUE)+
    geom_smooth(aes(y=cancelacion),se=FALSE, color="red")+
    facet_wrap(~ mes)
  
  ###Retrasos por compañia y aeropuerto
  vuelos %>% group_by(aerolinea,origen) %>% 
    summarise(media_atraso=mean(atraso_llegada,na.rm=TRUE))  %>% 
    ggplot(aes(x = reorder(origen,-media_atraso), y=media_atraso)) +
    geom_col()+
    facet_grid(~ aerolinea)
  
  vuelos %>% group_by(aerolinea,origen) %>% 
    summarise(media_atraso=mean(atraso_llegada,na.rm=TRUE))  %>% 
    ggplot(aes(x = reorder(aerolinea,-media_atraso), y=media_atraso)) +
    geom_col()+
    facet_grid(~ origen)

  vuelos %>% group_by(aerolinea, destino) %>% summarise(n())
  vuelos %>% group_by(aerolinea, destino) %>% count(sort=TRUE)
  
  no_cancelados %>% 
    group_by(anio, mes, dia) %>% 
    summarise(hora_perc = mean(atraso_llegada > 60))

  ####Transformaciones agrupadas
  vuelos_sml= vuelos %>% select(anio,mes,dia,atraso_salida,atraso_llegada,distancia,tiempo_vuelo)

  
  vuelos_sml %>% 
    group_by(anio, mes, dia) %>%
    filter(rank(desc(atraso_llegada)) < 10)
  
  popular_destinos <- vuelos %>% 
    group_by(destino) %>% 
    filter(n() > 365)
  
  popular_destinos %>% 
    filter(atraso_llegada > 0) %>% 
    mutate(prop_atraso = atraso_llegada / sum(atraso_llegada)) %>% 
    select(anio:dia, destino, atraso_llegada, prop_atraso)
  
  
  ##¿Qué avión (codigo_cola) tiene el peor registro de tiempo?
  
  vuelos %>% group_by(codigo_cola) %>% 
    summarise(peor_llegada=max(atraso_llegada,na.rm=TRUE),
              peor_salida=mean(atraso_salida,na.rm=TRUE)) %>% 
    arrange(desc(peor_llegada))
           
  ##Para cada destino, calcula los minutos totales de demora. 
  
  vuelos %>% group_by(destino) %>%
    filter(atraso_llegada>0) %>% 
    summarise(total_demora=sum(atraso_llegada,na.rm=TRUE)) %>% 
    arrange(desc(total_demora))

  #Para cada vuelo, calcula la proporción de la demora total para su destino.
  
    EX=vuelos %>% group_by(vuelo) %>%
      filter(atraso_llegada>0) %>% 
      mutate(total_demora=sum(atraso_llegada),
             prop_demora=(atraso_llegada/total_demora)*100) %>% 
      arrange(vuelo,mes,dia,horario_salida) %>% 
      select(-anio,-salida_programada,-llegada_programada,-hora,-minuto,-fecha_hora,-total_demora)


 E5= vuelos %>% arrange(origen,anio,mes,dia,horario_salida) %>%
   mutate(lead_salida=lead(atraso_salida),lag_salida=lag(atraso_salida)) %>% 
   select(origen,mes,dia,horario_salida,lead_salida,atraso_salida,lag_salida)

 E6=vuelos %>% group_by(origen,destino) %>% 
   mutate(tiempo_mas_corto=mean(tiempo_vuelo,na.rm=TRUE),
          prop_tiempos=((tiempo_vuelo/tiempo_mas_corto))) %>% 
   arrange(origen,destino) %>%  
   filter(between(horario_salida,1000,1500)) 
 
 E6 %>% ggplot(aes(x=horario_salida,y=prop_tiempos))+
   geom_point()+
   facet_wrap(~ mes)

###Encuentra todos los destinos que son volados por al menos dos operadores. Usa esta información para clasificar a las aerolíneas.
 
 E7=vuelos %>% group_by(destino,aerolinea) %>% 
   summarise(number=n_distinct(aerolinea)) %>% 
   summarise(total=sum(number)) %>% 
   filter(total>1) %>% select(destino) %>%
   unlist(, use.names=FALSE) %>% 
   as.character()


E8= vuelos %>% group_by(aerolinea) %>% 
  filter(grepl(paste(E7, collapse="|"), destino))  %>% 
  summarise(n=n_distinct(destino)) %>% 
  arrange(desc(n))


vuelos %>% group_by(codigo_cola) %>% 
  filter(atraso_llegada<60) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


###################################
###################################
#####ANALISIS EXPLORATORIO#########
###################################
###################################

library(dplyr)
library(datos)
no_cancelado <- vuelos %>% 
  filter(!is.na(atraso_salida), !is.na(atraso_llegada))
no_cancelado %>% 
  group_by(anio, mes, dia) %>% 
  summarise(media = mean(atraso_salida))

ggplot(data = diamantes) +
  geom_histogram(mapping = aes(x = quilate), binwidth = 0.5)

diamantes %>% 
  count(cut_width(quilate, 0.5))

pequenos <- diamantes %>% 
  filter(quilate < 3)

ggplot(data = pequenos, mapping = aes(x = quilate)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = pequenos, mapping = aes(x = quilate, colour = corte)) +
  geom_freqpoly(binwidth = 0.01)

ggplot(data = pequenos, mapping = aes(x = quilate, fill = corte)) +
  geom_histogram(binwidth = 0.01)

####Valores inusuales

ggplot(diamantes) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamantes) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

###Quitamos valores inusuales
inusual <- diamantes %>% 
  filter(y < 3 | y > 20) %>% 
  select(precio, x, y, z) %>%
  arrange(y)
inusual

ggplot(inusual) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

###EDDA
ggplot(diamantes) + 
  geom_histogram(mapping = aes(x = x), binwidth = 0.01, fill="blue")+
  geom_histogram(mapping = aes(x = y), binwidth = 0.01,fill="darkgreen")+
  #geom_histogram(mapping = aes(x = z), binwidth = 0.01,fill="pink")+
  xlim(3.5,9.5)

q1=ggplot(diamantes) + 
  geom_histogram(mapping = aes(x = x), binwidth = 0.01,)+
  xlim(3,10)
q2=ggplot(diamantes) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.01)+
  xlim(3,10)
q3=ggplot(diamantes) + 
  geom_histogram(mapping = aes(x = z), binwidth = 0.01)+
  xlim(3,10)
ggarrange(q1,q2,q3,nrow=3)

diamantes %>% 
  ggplot()+
  geom_point(position="jitter",aes(x=quilate,y=precio,color=corte))+
  geom_smooth(aes(x=quilate,y=precio,se=TRUE))

####Como tratar valores faltantes
diamantes2 <- diamantes %>% 
  filter(between(y, 3, 20))

diamantes2 <- diamantes %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamantes2, mapping = aes(x = x, y = y)) + 
  geom_point()

########ANALISIS INTERESANTE CON VUELOS

datos::vuelos %>% 
  mutate(
    cancelados = is.na(horario_salida),
    hora_programada = salida_programada %/% 100,
    minuto_programado = salida_programada %% 100,
    salida_programada = hora_programada + minuto_programado / 60
  ) %>% 
  ggplot(mapping = aes(salida_programada)) + 
  geom_freqpoly(mapping = aes(colour = cancelados), binwidth = 1/4)

ggplot(data = diamantes, mapping = aes(x = precio)) + 
  geom_freqpoly(mapping = aes(colour = corte), binwidth = 500)

ggplot(diamantes) + 
  geom_bar(mapping = aes(x = corte))

ggplot(data = diamantes, mapping = aes(x = precio, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = corte), binwidth = 500)

##BOXPLOTS

ggplot(data = diamantes, mapping = aes(x = corte, y = precio)) +
  geom_boxplot()

###En estos, al igual quel el grafico de geom_freqpoly, arroja el resultado de 
##que los diamantes con mejor corte son mas baratos de promedio. Cçómo?

ggplot(data = diamantes, mapping = aes(x = corte, y = precio)) +
  geom_col()
ggplot(data = diamantes, mapping = aes(x = corte, y = precio)) +
  geom_point(position="jitter")

########

ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_boxplot()

ggplot(data = millas) +
  geom_boxplot(mapping = aes(x = reorder(clase, autopista, FUN = median), y = autopista))+
  coord_flip()

###########Ejercicios:

vuelos %>% 
  mutate(
    cancelados = is.na(horario_salida),
    hora_programada = salida_programada %/% 100,
    minuto_programado = salida_programada %% 100,
    salida_programada = hora_programada + minuto_programado / 60) %>% 
  ggplot(mapping = aes(x=salida_programada,y=..density..)) + 
  geom_freqpoly(mapping = aes(colour = cancelados), binwidth = 1/4)

vuelos %>% 
  mutate(
    cancelados = is.na(horario_salida),
    hora_programada = salida_programada %/% 100,
    minuto_programado = salida_programada %% 100,
    salida_programada = hora_programada + minuto_programado / 60) %>% 
  ggplot(mapping = aes(x=cancelados,y=salida_programada)) + 
  geom_boxplot()

#####Diamantes
Diam <- diamantes

Diam$volumen = diamantes$x*diamantes$y*diamantes$z

Diam %>% filter(volumen<1000) %>% ggplot(mapping = aes(x = corte, y = volumen)) +
  geom_boxplot()

Diam %>% filter(volumen<1000) %>% ggplot(mapping = aes(x = corte, y = profundidad)) +
  geom_boxplot()

Diam %>% filter(volumen<1000) %>% ggplot(mapping = aes(x = corte, y = precio)) +
  geom_boxplot()

ggplot(Diam, mapping=aes(x=profundidad,y=precio,color=corte))+
  geom_point()+coord_flip()

ggplot(Diam, mapping=aes(x=profundidad,y=precio,color=claridad))+
  geom_point()+coord_flip()

ggplot(Diam, mapping=aes(x=volumen,y=precio,color=corte))+
  geom_point()+ xlim(0,500)

ggplot(Diam, mapping=aes(x=volumen,y=precio,color=claridad))+
  geom_point()+ xlim(0,500)

Diam %>% filter(volumen <1000) %>% 
  ggplot( mapping=aes(x=profundidad,y=volumen,color=corte))+
  geom_point()

##Claridad y corte

s1=Diam %>% ggplot( mapping = aes(x = claridad, y = precio)) +
  geom_boxplot()

s2=Diam %>% ggplot( mapping = aes(x = corte, y = precio)) +
  geom_boxplot()
ggarrange(s1,s2,nrow=2)

w1=ggplot(data = diamantes, mapping = aes(x = precio,y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = claridad), binwidth = 500)

w2=ggplot(data = diamantes, mapping = aes(x = precio, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = corte), binwidth = 500)

ggarrange(w1,w2,nrow=2)

s3=ggplot(data = diamantes, mapping = aes(x = corte, fill = claridad)) +
  geom_bar(position = "dodge")

ggarrange(s1,s2,s3,nrow=3)

s4= Diam %>% filter(corte=="Regular") %>% 
  ggplot(mapping = aes(x = corte, fill = claridad))+
  geom_bar(position = "dodge")

s5= Diam %>% filter(corte=="Ideal") %>% 
  ggplot(mapping = aes(x = corte, fill = claridad))+
  geom_bar(position = "dodge")
ggarrange(s1,s2,s4,s5,ncol=2,nrow=2)

Diam %>% filter(volumen<1000) %>% ggplot(, mapping = aes(x = corte, y = volumen)) +
  geom_boxplot()

ggplot(data = millas, mapping = aes(x = autopista, y = clase)) +
  geom_boxploth()

###Probar nuevos plots

d1=Diam %>% ggplot( mapping = aes(x = corte, y = precio)) +
  geom_boxplot()
d2=Diam %>% ggplot( mapping = aes(x = corte, y = precio)) +
  geom_lv(outlier.colour = "blue",position="identity")
ggarrange(d1,d2,nrow=2)

ggplot(data = Diam, mapping = aes(x = precio))+
  geom_histogram(binwidth = 0.1)
####-----
ggplot(data = Diam, mapping = aes(x = quilate, colour = corte,y=..density..)) +
  geom_freqpoly(binwidth = 0.01)


ggplot(data = Diam, mapping = aes(x = quilate, colour = claridad,y=..density..)) +
  geom_freqpoly(binwidth = 0.01)

ggplot(data = Diam, mapping = aes(x = quilate,y=..density..)) +
  geom_histogram(binwidth = 0.1)+xlim(0,3)

ggplot(data = Diam, mapping = aes(x = quilate, fill = corte,y=precio)) +
  geom_violin(binwidth = 1)+xlim(0,4)

ggplot(data = diamantes) +
  geom_count(mapping = aes(x = corte, y = color))


###Patrones

ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))


####EDA con patrones y modelaje

library(modelr)

##Relacion quilate-precio

diamantes %>% ggplot(aes(x=quilate,y=precio,color=corte))+ 
  geom_point(position="jitter")+
  geom_smooth(se=TRUE)

    ggplot(data = diamantes) + 
      geom_point(mapping = aes(x = quilate, y = precio), alpha = 1 / 50)
    ggplot(data = diamantes) +
      geom_bin2d(mapping = aes(x = quilate, y = precio))
    ggplot(data = diamantes) +
      geom_hex(mapping = aes(x = quilate, y = precio))
    
    
ggplot(data = diamantes, mapping = aes(x = quilate, colour = corte,y=..density..)) +
  geom_freqpoly(binwidth = 0.01)

ggplot(data = diamantes, mapping = aes(x = quilate, y = precio)) + 
  geom_boxplot(mapping = aes(group = cut_width(quilate, 0.1)))

##Relacion quilate-corte

Diam %>% ggplot(mapping = aes(x = quilate, color = corte)) +
  geom_freqpoly(binwidth = 0.01)+xlim(0,3)

Diam %>% filter(quilate <3) %>% ggplot(mapping = aes(x = quilate, fill = corte)) +
  geom_bar(binwidth = 0.1)+
  coord_polar()

Diam %>% filter(quilate <3) %>% ggplot(mapping = aes(x = quilate, fill = corte)) +
  geom_bar(binwidth = 0.1, position="dodge")

###Relacion corte-precio

Diam %>% filter(volumen<1000) %>% ggplot(mapping = aes(x = corte, y = precio)) +
  geom_boxplot()

ggplot(data = diamantes, mapping = aes(x = precio, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = corte), binwidth = 500)

###Relacion corte-color

ggplot(data = diamantes) +
  geom_count(mapping = aes(x = corte, y = color))

            diamantes %>% 
              count(color, corte)

diamantes %>% 
  count(color, corte) %>%  
  ggplot(mapping = aes(x = color, y = corte)) +
  geom_tile(mapping = aes(fill = n))

#####2Relacion x-y

ggplot(data = diamantes) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))


#####¿Cómo podrías cambiar la escala del conjunto de datos anterior para mostrar de manera más clara la distribución 
##del corte dentro del color, o del color dentro de la variable corte?

?dcast
?seriate
myPalette <- colorRampPalette(rev(brewer.pal(11, "PuOr")))
oo = diamantes %>% 
  count(color, corte) %>% 
  as.data.table()

o=as.matrix(dcast(oo,color~corte,value.var="n"),rownames = "color")

typeof(o)
  
o_serial=seriate(o, method="PCA")
####Se usa el medio

longData=melt(o)

ggplot(longData,aes(x=Var1,y=Var2,fill=value))+geom_tile()

longData2=longData
longData2$Var1 <- factor(longData2$Var1, names(unlist(o_serial[[1]][])))
longData2$Var2 <- factor(longData2$Var2, names(unlist(o_serial[[2]][])))

ggplot(longData2,aes(x=Var1,y=Var2,fill=value))+geom_tile()
ggplot(longData2,aes(x=Var1,y=Var2,fill=value))+ theme(axis.text.x=element_text(angle=45, hjust = 1, size = 5))

zp1 <- ggplot(longData2,
              aes(x = Var1, y = Var2, fill = value))
zp1 <- zp1 + geom_tile()
zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(50))
zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
zp1 <- zp1 + theme(axis.text.x=element_text(angle=45, hjust = 1, size = 5))
print(zp1)

###Usa geom_tile() junto con dplyr para explorar la variación del retraso promedio de los vuelos en relación al 
##destino y mes del año. ¿Qué hace que este gráfico sea difícil de leer? ¿Cómo podrías mejorarlo?

V=vuelos
Z =V %>% group_by(destino,mes) %>% filter(!is.na(destino)) %>% 
  summarise(retraso=mean(atraso_llegada,na.rm=TRUE)) %>%
  filter(!retraso=="NaN") %>% as.data.table()

Y=dcast(Z,destino~mes,value.var="retraso")
Y[is.na(Y)]=0
X = as.matrix(Y,rownames = "destino")
o_serial=seriate(X, method="BEA_TSP")
longData=melt(X)
ggplot(longData,aes(x=Var1,y=Var2,fill=value))+geom_tile()

longData2=longData
longData2$Var1 <- factor(longData2$Var1, names(unlist(o_serial[[1]][])))
longData2$Var2 <- factor(longData2$Var2, names(unlist(o_serial[[2]][])))

ggplot(longData2,aes(x=Var1,y=Var2,fill=value))+geom_tile()

###########

diamantes %>% 
  count(color, corte) %>%  
  ggplot(mapping = aes(x = corte, y = color)) +
  geom_tile(mapping = aes(fill = n))

diamantes %>% 
  count(color, corte) %>%  
  ggplot(mapping = aes(x = color, y = corte)) +
  geom_tile(mapping = aes(fill = n))

#####Modelaje

mod <- lm(log(precio) ~ log(quilate), data = diamantes)

###Lo que sería logaritmizar los datos
Diam$logprecio = log(Diam$precio)
Diam$logquilate = log(Diam$quilate)

ggplot(data = Diam) + 
  geom_point(mapping = aes(x = logquilate, y = logprecio))
#####


diamantes2 <- diamantes %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamantes2) + 
  geom_point(mapping = aes(x = quilate, y = resid))

ggplot(data = diamantes2) + 
  geom_boxplot(mapping = aes(x = corte, y = resid))




