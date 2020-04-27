library(readr)        #Para leer e importar datos
library(dplyr)        #Análisis exploratorio
library(ggplot2)      #Visualizaciones
library(tidyverse)    #Utilizada para ordenar dataset
library(gsheet)       #Leer google sheets
library(readxl)       
library(knitr)        #Varios propósito
library(DT)           #libreria DataTables
library(caret)        #Variables para Correlation Matrix
library(grid)         #Plotear
library(gridExtra)    #Plotear
library(rpart)        #Decision Tree


########INICIO

temp <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00356/student.zip",temp, mode="wb")
unzip(temp, "student-mat.csv")
ST <- read.table("student-mat.csv",sep= ";", header= T)
unlink(temp)

#FILTRADO Y VARIABLES

#Primeras visualizaciones
data[,1:5] %>% head() %>% kable()
summary(ST)
names(ST)

#Compruebo si hay NA
ST %>% is.na() %>% all()

#Consulto la codificacion de la DT de la pagina de google
COL_LIST <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1mDsF0aMNgODx7063l2mxV1_zP32fAe_P55SBmtG72G8')
kable(COL_LIST)
kable(COL_LIST[,1:3])

#Escogemos 18 variables como predictores
###7 variables categóricas
###11 variables numericas
#3 variables dependientes y susceptibles de predicción, correspondientes a las notas
#Finalmente añadimos columna PASS, como se sugeria en el tema del master, para boolear el resultado final
#En base a lo que observo en nuestro ST, y dadas las características de las variables actuales, eliminaré diversas variables entre las que se encuentran "famsize" "reason" o "guardian" o "school", ya que serán irrelevantes en nuestro análisis con select()

ST <- ST %>%as_tibble()%>%select(sex, age, address,Pstatus, Medu, Fedu, Mjob, Fjob,studytime,traveltime,failures,higher,internet, 
         goout, Dalc,Walc,health, absences,G1,G2,G3)
ST$PASS <- ifelse((ST$G3>9, 1, 0)
#Total: 22 variables o columnas
dim(ST)

ST %>% glimpse()
ST %>% summary()


#ANALISIS EXPLORATORIO DE DATOS

#Analizaremos los siguientes indicadores
#1. Según género
#2. Consumo de alcohol (W/D)
#3. Costumbres, objetivos y origen del alumno
#4. Salud y asistencia a clase
#5. Acceso a internet
#6. Relación asistencia a clase con las notas

###Según Genero

(ANA_1<-ST%>%
    mutate(pass=ifelse(G3>=10,1,0), fail= ifelse(G3<10,1,0))%>%
    filter(sex=="F"|sex=="M")%>%
    group_by(sex)%>%
    summarise(Pass=sum(pass), 
              Fail=sum(fail)))

ANA_1%>%
  ggplot(aes(x=sex,y=Fail))+
  geom_bar(stat="identity")
ANA_1

###Observamos que hay diferencias notables entre mujeres y hombres 

#2. Consumo de alcohol

ANA_2a <- ST%>%
  group_by(Walc)%>%
  aggregate(G3~Walc, data=., mean)%>%
  arrange(desc(G3))
ANA_2a

ANA_2b <- ST%>%
  group_by(Dalc)%>%
  aggregate(G3~Dalc, data=., mean)%>%
  arrange(desc(G3))
ANA_2b

#Vemos que disminuye el promedio a medida que aumenta el consumo de alcohol entre semana, pero no hay un patron para fin de semana

#Cruce de datos para visualizacion de aspectos que relacionen datos relacionados con el género y el consumo de alcohol

DUM <- dummyVars("~.", data=ST)
GEN_ALC <- data.frame(predict(DUM, newdata=ST))
correl1 <-cor(GEN_ALC[,c("G3","sex.F","sex.M","Walc","Dalc")])

correl1 %>%
  ggcorr(label = TRUE)+ ggtitle("Correlaciones entre genero, consumo de alcohol y las notas")

#Pasamos 
ST$Dalc <- as.factor(ST$Dalc)
ST$Walc <- as.factor(ST$Walc)
P1a<-ST %>%
  ggplot(aes(x=Dalc, y=G3, fill= Dalc))+
  geom_boxplot()+
  coord_flip()+
  xlab("Consumo de alcohol entre semana")+
  ylab("Notas")+
  facet_grid(~sex)
P1b<-ST %>%
  ggplot(aes(x=Walc, y=G3, fill= Walc))+
  geom_boxplot()+
  coord_flip()+
  xlab("Consumo de alcohol entresemana")+
  ylab("Notas")+
  facet_grid(~sex)
grid.arrange(P1a,P1b,ncol=2)

#Básicamente podemos predecir que el consumo de alcohol tiene un impacto mucho mayor que el género en las notas

#3. Costumbres, objetivos y origen del alumno

class(ST$goout)
ST$goout <- as.factor(ST$goout)
#Convertimos a factor para que me acepte el algoritmo
ANA_3 <- ST%>%
  group_by(goout)%>%
  summarise(Avr= mean(G3,na.rm=TRUE))%>%
  arrange(desc(Avr))
ANA_3

#Vemos que hay un decrecimiento en las notas en cuanto que el alumno supera el factor de salida de 4

P2a<-ST %>% 
  group_by(address)%>%
  ggplot(aes(x=factor(Dalc), y= G3))+
  geom_jitter(alpha=0.6)+
  scale_x_discrete("Alcohol entresemana")+
  scale_y_continuous("Notas")+
  facet_grid(~address)
P2b<-ST %>% 
  group_by(address)%>%
  ggplot(aes(x=factor(Walc), y= G3))+
  geom_jitter(alpha=0.6)+
  scale_x_discrete("Alcohol en fin de semana")+
  scale_y_continuous("Notas")+
  facet_grid(~address)
grid.arrange(P2a,P2b,ncol=2)

#Otro ejemplo paradigmático de la relacion del decrecimiento de notas respecto al consumo de alcohol. Se observa que se bebe menos en las zonas rurales

ST%>%
  ggplot(aes(x=higher, y=G3))+
  geom_boxplot()+
  facet_grid(~sex)

#Los alumnos que aspiran a mejor educacion, tienen mejores notas. Los hombres son mejores que las mujeres también

#4. Salud y asistencia a clase

ST%>%
  group_by(sex)%>%
  ggplot(aes(x=factor(health), y=absences, color=sex))+
  geom_smooth(aes(group=sex), method="lm", se=FALSE)

#Este interesante gráfico nos muestra que existe una relación lineal decreciente entre la salud del alumno y las ausencias en clase
#Ademas de ser, de nuevo, las mujeres quienes faltan más clase, siendo sus ausencias menos dependientes con la salud

#5. Acceso a internet

ST%>%
  group_by(internet)%>%
  ggplot(aes(x=G3, fill=internet))+
  geom_density( alpha=0.8)

#el uso de internet afecta a las notas, aunque no en exceso

#6. Relación asistencia a clase con las notas

P3 <- ggplot(ST, aes(absences, G3))
P3 + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(y="G3", 
       x="abcenses", 
       title="Comparativa ausencias con notas") 
       
ggplot(ST, aes(x=absences, y=G3)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(y="G3", 
       x="absences", 
    title="Ausencias vs Notas")


######CLUSTERING DE DATOS
###CREAMOS ST_MOD COMO REFERENCIA CON EL G3 REESCALADO DE 1:10

ST_MOD=ST
ST_MOD$G3=as.integer(ST_MOD$G3)

ST_MOD$G3=rescale(ST_MOD$G3,to=c(1,10))

###convertimos a integer

ST_KM= ST[c("age","Medu","Fedu","studytime","traveltime","failures","goout","Dalc","Walc","health","absences","G1","G2")]
ST_factors = ST_KM %>% select_if(is.factor) %>% colnames()
ST_KM[,ST_factors] = lapply(ST_KM[,ST_factors], as.integer)

#Limpiamos variables de valores dispersos
#abcenses
plot(ST_KM$absences)
ST_KM$absences<- floor(rescale(ifelse(ST_KM$absences>3*mean(ST_KM$absences),
                                      3*mean(ST_KM$absences),ST_KM$absences),
                               ,to=c(1,5)))
###plot(ST_KM$absences)
#age
plot(ST_KM$age)
ST_KM$age<- floor(rescale(ifelse(ST_KM$age>19,
                                 19,ST_KM$age),
                          ,to=c(1,5)))
plot(ST_KM$age)

##Se reajusta todo el sistema a escala 1:5

#ST_KM$absences<- floor(rescale(ST_KM$absences,to=c(1,5)))
ST_KM[, c(1:13)] <- lapply(ST_KM[, c(1:13)], function(x) rescale(x,to=c(1,5)))
ST_KM=floor(ST_KM)
ST_factors = ST_KM %>% select_if(is.factor) %>% colnames()
ST_KM[,ST_factors] = lapply(ST_KM[,ST_factors], as.integer)

glimpse(ST_KM)
summary(ST_KM)


mydata <- ST_KM
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Numero de Clusters",
     ylab="Sumas de cuadrados dentro de los grupos",
     main="Num de clusters óptimo según Elbow",
     pch=20, cex=2)

##se deciden 5 centros 

KM=kmeans(ST_KM,5)
KM

table(ST_MOD$G3, KM$cluster)

plot(ST_KM$age, col=KM$cluster)

plot(KM$centers)
radial.plot(KM$centers[1,],
            labels=names(KM$centers[1,]),
            rp.type="s",
            radial.lim=c(0,8),
            point.symbols=13,
            point.col="red",
            mar = c(2,1,5,2))


plot(ST_KM %>% select(age, absences), col = KM$cluster)
points(as.data.frame(KM$centers) %>% select(age, absences), col = 1:3, pch = 8, cex = 2)


ST_FIN <- ST_MOD %>% mutate(cluster_id = KM$cluster)
kable(head(ST_FIN))


#Aunque es posible que haber normalizado los valores de las variables entre 1:5 puede que nos haya limitado la
#vision grafica de las dispersiones, en general vemos, tras un proceso iterativo en la busqueda de centros
#Todo apunta a que aquellas variables como G1,G2,"studytime", "traveltime" se encuentran en el grueso de valores máximos asociados a notas altas
#Y las variables de tipo Walc,Dalc,"goout" Tienen mayor influencia en el sentido inverso, y están asociados a la caida de notas.
##Mis fuentes principales son el 'table(ST_MOD$G3, KM$cluster)' y el esquema de centros radial.

  
################USO DE ALGORITMOS PREDICTIVOS

library(caret)
ST_new<- ST%>%select(sex, age, address,Pstatus, Medu, Fedu, Mjob, Fjob,studytime,traveltime,failures,higher,internet, 
                        goout, Dalc,Walc,health, absences,G1, G2, G3)
ARBOL <- rpart(G3 ~ .,
              data = ST_new,
              method = "class")
PRIN <- varImp(ARBOL)
rownames(PRIN)[order(PRIN$Overall, decreasing=TRUE)]

printcp(ARBOL)
plotcp(ARBOL)

##Encontramos que G1 y G2 examen son predictores clave seguidos por niveles de asistencia, consumo de alcohol y trabajos de los padres.

#La lógica del árbol consiste en sólo utilizar "attendance, Fjob, G1 y G2" como variables basadas en la correlación y la colinealidad entre algunas de las otras variables.

##############
