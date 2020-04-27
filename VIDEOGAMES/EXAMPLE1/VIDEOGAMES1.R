library(readr)        #Para leer e importar datos
library(dplyr)        #Análisis exploratorio
library(ggplot2)      #Visualizaciones
library(tidyverse)    #Utilizada para ordenar dataset
library(gsheet)       #Leer google sheets
library(readxl)       
library(knitr)        #Varios propósitos
library(DT)           #libreria DataTables
library(caret)        #Variables para Correlation Matrix
library(grid)         #Plotear
library(gridExtra)    #Plotear
library(corrr)        #Var estadisticas
library(ggpubr)
library(qcc)
library(data.table)
library(janitor)
library(arules)
library(arulesViz)
library(lubridate)
library(stats)
library(samplingbook)
library(rmarkdown)

####Apertura de archivos

getwd()
list.files()
setwd("R/WORKS/GENERA")

setwd()

DT <- read.csv("event_levels.csv")
View(head(DT))

##################################
##########DATA ANALYSIS###########
##################################


##Calculamos previamente algunos totales

Num_Usr = as.integer(DT %>% distinct(user_id, .keep_all = TRUE)
                     %>% summarise(n=n()))
#Numero total de usuarios que han participado en el evento

Num_Tot = as.integer(DT %>% summarise(n=n())) ###Numero total de entradas

Num_Suc = as.integer(DT %>%  filter(level==5,action=="action.success") %>% 
                             distinct(user_id, .keep_all = TRUE) 
                                 %>% summarise(n=n()))

Num_PowT =as.integer(DT %>% filter(action=="action.use_powerup")%>% #Numero de veces que se usa un powerup
        summarise(n=n()))

Num_Start = as.integer(DT %>% filter(action=="action.game_start") #Recuento de Starts
                       %>% summarise(n=n()))

Num_Fail = as.integer(DT %>% filter(action=="action.fail") #Recuento de Fails
                      %>% summarise(n=n()))

Num_SucT = as.integer(DT %>% filter(action=="action.success") #Recuento de Success totales
                      %>% summarise(n=n()))


##1## ¿Cuál es el porcentaje de usuarios que han completado el evento respecto de los que han participado?
###############

SOL_SUCC =(Num_Suc/Num_Usr)*100

##El 17,5927% de los usuarios lo ha superado completamente

##ANALYSIS
###############

RATE_START = Num_Start/Num_Usr

RATE_SUCC = Num_SucT/Num_Usr

RATE_FAIL = Num_Fail/Num_Usr

RATE_TABLE = S2 =  DT %>% group_by(level) %>% 
            mutate(Total=n(),
                   Start=ifelse(action=="action.game_start",1,0),
                   Success=ifelse(action=="action.success",1,0),
                   Fail=ifelse(action=="action.fail",1,0)) %>% 
            summarise(RATE_START=sum(Start)/Num_Usr,
                      RATE_SUCC=sum(Success)/Num_Usr,
                      RATE_FAIL=sum(Fail)/Num_Usr)

summary(RATE_TABLE)

##En una primera visual vemos que el ratio de Succ bruto y por level es mayor que el de Fail
##Solo el fail tiene una mediana mayor, lo que suguiere más equilibrio y menos dependencia de factores externos.

S2 =  DT %>% group_by(level) %>% 
        mutate(Total=n(),
               Start=ifelse(action=="action.game_start",1,0),
               Success=ifelse(action=="action.success",1,0),
               Fail=ifelse(action=="action.fail",1,0),
               PowerUp=ifelse(action=="action.use_powerup",1,0)) %>% 
        summarise(Success=sum(Success),
                  Fail=sum(Fail),
                  PowerUp=sum(PowerUp),
                  xSuccess=sum(Success)/sum(Start)*100,
                  xFail=sum(Fail)/sum(Start)*100)# %>% 
        #adorn_totals("row")

#Evolución de cantidades netas

S2gA <- ggplot(S2, aes(x=level)) +                    
        geom_line(aes(y=Success), colour="green") + 
        geom_line(aes(y=Fail), colour="red") +
        geom_line(aes(y=PowerUp), colour="yellow")

lm(Success~level,data=S2)

S2.R2 <- summary(lm(Success~level,data=S2))$r.squared

##En esta grafica se observa una caida decreciente de la cantidad de partidas ganadas y perdidas (basandonos en la porporcion relativa a partidas comenzadas), las cuales difieren
##principalmente en el primer nivel y después se ajustan hasta encontrarse en valores similares, lo que sugiere equidad
##Su decrecimiento es relativamente lineal (S2.R2=0.84). Una baja dispersion sugiere alta fiabilidad

#Evolución de cantidades relativas

S2gB <-ggplot(S2, aes(level)) +                    
        geom_line(aes(y=xSuccess), colour="green") + 
        geom_line(aes(y=xFail), colour="red") +
        ylim(0,100)

##En esa grafica se puede confirmar que existe cierta equidad en la proporcion de exito y fallo durante los niveles intermedios, lo que sugiere un juego asequible
##La caida de las proporciones de fallo en el ultimo nivel sugiere que los powerups acumulados durante todo el evento son usados principalmente en el nivel 5 del evento

ggarrange(S2gA,S2gB)

##DIFFICULT

##Visualizamos totales

SOL_DIF = round(Num_Fail/Num_SucT,2)

##El valor de este indicador está por debajo de 1, lo que significa que el juego es asesquible en general, en tanto que se aleje de 1 más facil será

S3 = as.data.table(
        DT %>% group_by(level) %>% 
                mutate(Total=n(),
                       Start=ifelse(action=="action.game_start",1,0),
                       Success=ifelse(action=="action.success",1,0),
                       Fail=ifelse(action=="action.fail",1,0),
                       PowerUp=ifelse(action=="action.use_powerup",1,0)) %>% 
                summarise(Success=sum(Success),
                          Difficulty=sum(Fail)/sum(Success)))
mean(S3$Difficulty)

ggplot(S3, aes(level))+
        geom_line(aes(y=Difficulty), colour="blue")+
        geom_hline(yintercept=1, linetype="dashed", 
                   color = "red", size=2)

##La dificultad nos sale de media 0.89, En este grafico es muy interesante visualizar la evolución de la dificultad según nivel:
##La dificultad del juego se eleva durante los niveles 3 y 4 y cae en picado en el nivel 5. Tal como hemos visto con SOL_DIF en general el juego es asequible

##Este juego no parece complicado ya que el ratio Fail/Success es bastante bajo en general. En los videojuegos generalmente este indicador adopta valores
##Desde (0.1-10) y donde un 1 marca un 50% de Success/Fail, de manera que podemos suponer que el juego es medio-fácil en el sentido de que solo en los niveles
##intermedios subre del valor 1, además de que en el análisis de videojuegos generalmente la dificultad se mide de forma relativa a la dificultad hallada en 
##eventos anteriores del mismo videojuego

##POWERUPS

POWERUP_TABLE0 = DT %>%  filter(action=="action.use_powerup") %>% group_by(level) %>% summarise(n())

plot(POWERUP_TABLE0)

##El uso de powerups aumenta de forma geométrica, aunque no exponencial

SOL_PU = (Num_PowT/Num_Usr)

##Se han usado 2.66 pu por cada user en el evento
###Con una visual completa de los jugadores que han comenzado la partida, ganado en cada nivel, y el uso de powerups,



##Haremos lo mismo pero valorando la dependencia con los powerups:
##Como no existen variables que asocien directamente el success o el fail con el uso de powerups, crearemos un data table
##Que nos ayude a clasificar ambos casos estudiando y filtrando los casos de coincidencia de variables:

POWERUP_TABLE = data.frame(level=c(1),Success_pu=c(1),Success_nopu=c(1),Fail_pu=c(1),Fail_nopu=c(1),SucFail_pu=c(1),SucFail_nopu=c(1))

POWERUP_TABLE = POWERUP_TABLE[-1,]

y=1

for(y in 1:5){

        Success_pu = as.integer(DT %>% filter(level==y) %>% 
                        group_by(user_id) %>% 
                        mutate(PowerUp=ifelse(action=="action.use_powerup",1,0),
                               Success=ifelse(action=="action.success",1,0),
                               Fail=ifelse(action=="action.fail",1,0))  %>% 
                        summarise(PowerUp=sum(PowerUp),Success=sum(Success),Fail=sum(Fail)) %>%
                        filter(PowerUp>0,Success>0,Fail<1) %>%
                        summarise(n=n()))
        
        Success_nopu = as.integer(DT %>% filter(level==y) %>% 
                                        group_by(user_id) %>% 
                                        mutate(PowerUp=ifelse(action=="action.use_powerup",1,0),
                                               Success=ifelse(action=="action.success",1,0),
                                               Fail=ifelse(action=="action.fail",1,0))  %>% 
                                        summarise(PowerUp=sum(PowerUp),Success=sum(Success),Fail=sum(Fail)) %>%
                                        filter(PowerUp<1,Success>0,Fail<1) %>%
                                        summarise(n=n()))
        
        Fail_pu = as.integer(DT %>% filter(level==y) %>% 
                                          group_by(user_id) %>% 
                                          mutate(PowerUp=ifelse(action=="action.use_powerup",1,0),
                                                 Success=ifelse(action=="action.success",1,0),
                                                 Fail=ifelse(action=="action.fail",1,0))  %>% 
                                          summarise(PowerUp=sum(PowerUp),Success=sum(Success),Fail=sum(Fail)) %>%
                                          filter(PowerUp>0,Success<1,Fail>0) %>%
                                          summarise(n=n()))

        
        Fail_nopu = as.integer(DT %>% filter(level==y) %>% 
                                          group_by(user_id) %>% 
                                          mutate(PowerUp=ifelse(action=="action.use_powerup",1,0),
                                                 Success=ifelse(action=="action.success",1,0),
                                                 Fail=ifelse(action=="action.fail",1,0))  %>% 
                                          summarise(PowerUp=sum(PowerUp),Success=sum(Success),Fail=sum(Fail)) %>%
                                          filter(PowerUp<1,Success<1,Fail>0) %>%
                                          summarise(n=n()))
    
        SucFail_pu = as.integer(DT %>% filter(level==y) %>% 
                                     group_by(user_id) %>% 
                                     mutate(PowerUp=ifelse(action=="action.use_powerup",1,0),
                                            Success=ifelse(action=="action.success",1,0),
                                            Fail=ifelse(action=="action.fail",1,0))  %>% 
                                     summarise(PowerUp=sum(PowerUp),Success=sum(Success),Fail=sum(Fail)) %>%
                                     filter(PowerUp>0,Success>0,Fail>0) %>%
                                     summarise(n=n()))
        SucFail_nopu = as.integer(DT %>% filter(level==y) %>% 
                                        group_by(user_id) %>% 
                                        mutate(PowerUp=ifelse(action=="action.use_powerup",1,0),
                                               Success=ifelse(action=="action.success",1,0),
                                               Fail=ifelse(action=="action.fail",1,0))  %>% 
                                        summarise(PowerUp=sum(PowerUp),Success=sum(Success),Fail=sum(Fail)) %>%
                                        filter(PowerUp<1,Success>0,Fail>0) %>%
                                        summarise(n=n()))
        
                H = data.table(t(c(y,Success_pu,Success_nopu,Fail_pu,Fail_nopu,SucFail_pu,SucFail_nopu)))
                
                names(H)<-names(POWERUP_TABLE)
                
                POWERUP_TABLE = data.table(rbind(POWERUP_TABLE,H))
                
                #POWERUP_TABLEx = data.table(mapply(c, POWERUP_TABLEx,G))
    
        }

POWERUP_TABLE = POWERUP_TABLE %>% adorn_totals("row")
POWERUP_TABLE$level = as.integer(POWERUP_TABLE$level)

glimpse(POWERUP_TABLE)

ggplot(POWERUP_TABLE,aes(x=level))+
        geom_line(aes(y=Success_pu), colour="blue4")+
        geom_line(aes(y=Success_nopu), colour="dodgerblue2")+
        geom_line(aes(y=Fail_pu), colour="red4")+
        geom_line(aes(y=Fail_nopu), colour="brown2")+
        geom_line(aes(y=SucFail_pu), colour="green4")+
        geom_line(aes(y=SucFail_nopu), colour="seagreen2")

##La unica variable que aumenta con el nivel es el Success_pu, es decir, la relación del éxito con el uso de powerups
##PowerUp - Éxito es la unica variables con una tasa de crecimiento mínimamente considerable

##Decir que dadas las circunstancias obviaremos los datos relativos a las 2 últimas variables, donde coinciden poweups, success y fail,
##Y es dificil suponer la relación que pueden tener directamente entre unas y otras (aunque esto se podría conseguir con un algoritmo bien elaborado)

SOL_DIFpu = round(as.integer(POWERUP_TABLE[6,4])/as.integer(POWERUP_TABLE[6,2]),2)

SOL_DIFnopu = round(as.integer(POWERUP_TABLE[6,5])/as.integer(POWERUP_TABLE[6,3]),2)


###Absolutos
c(SOL_DIF=SOL_DIF,SOL_DIFpu=SOL_DIFpu,SOL_DIFnopu=SOL_DIFnopu)

##Medias

POWERUP_TABLE = POWERUP_TABLE[-6,]

summary(POWERUP_TABLE)

##En este esumario se revela algo muy interesante y el la caida de Fails (en medias) con el uso de powerups y sin ello

##En definitiva, Se observa que la diferencia de dificultad del evento (por niveles) del juego cambia importantemente en el uso de pu, hasta en un 86%,
##Si aceptamos las asunciones hechas hasta el momento, ya que la medida sobre exito/fallo con/sin powerups es aproximada, su uso tiene una incidencia importante

##vamos llegando a las conclusiones siguientes:

POWERUP_TABLE2 = DT %>% group_by(level) %>% 
        filter(action=="action.game_start" | action=="action.success" | 
                   action=="action.use_powerup" | action=="action.fail") %>% 
        mutate(Start=ifelse(action=="action.game_start",1,0),
               Success=ifelse(action=="action.success",1,0),
               Fail=ifelse(action=="action.fail",1,0),
               Powerup=ifelse(action=="action.use_powerup",1,0)) %>%
        summarise(Start=sum(Start),Success=sum(Success),Fail=sum(Fail),
                  Powerup=sum(Powerup),Succ_rate=round(sum(Success)/sum(Start)*100,2),
                  PU_rate=round(sum(Powerup)/sum(Start)*100,2)) %>% 
        as.data.table()

kable(POWERUP_TABLE2)

##Apoyándonos en las tablas POWERUP_TABLE 0/2 y el grafico podemos ver el ratio o incremento de uso de PUs en el último nivel, 
##que se corresponde con el incremento de éxito y como elemento fundamental para el éxito del evento completo (subida de más del 300% del primer nivel al último)


#######################################################################################
#####Aquí hago una propuesta de cómo calcular KPI de retencion a días, basandome en el actual Dataset

ET = DT %>% filter(action=="action.game_start",level==1) %>% 
        mutate(DAY=day(client_time))

summary(ET)

ET = DT %>% filter(action=="action.game_start",level==1) %>% 
        mutate(DAY=day(client_time)) %>% group_by(user_id)%>% 
        mutate(D1=ifelse(DAY==5,1,0),D2=ifelse(DAY==6,1,0),
               D3=ifelse(DAY==6,1,0),D4=ifelse(DAY==6,1,0)) %>%
        summarise(D1=ifelse(sum(D1)>=1,1,0),
                  D2=ifelse(sum(D2)>=1,1,0),
                  D3=ifelse(sum(D3)>=1,1,0),
                  D4=ifelse(sum(D4)>=1,1,0)) %>% 
        mutate(RT1=ifelse((D1+D2+D3+D4)>1,1,0),RT2=ifelse((D1+D2+D3+D4)>2,1,0),RT3=ifelse((D1+D2+D3+D4)>3,1,0)) %>%
        select(user_id,RT1,RT2,RT3)

ET           

ET %>% summarise(RTT1=sum(RT1)/n(),RTT2=sum(RT2)/n(),RTT3=sum(RT3)/n())

## Con esto obtenemos un mapa completo por usuario con retenciones a día 1, 2 y 3, que son el máximo de días de los que disponemos en nuestro muestreo
###Se comprueba que la tasa de retencion a día 1 es superior al 50%, también al día 2, y finalmente cae en el día 3 al 25%

ET = DT %>% filter(action=="action.game_start") %>% 
        mutate(DAY=day(client_time)) %>% 
        group_by(user_id)%>% 
        mutate(D1=ifelse(DAY==5,1,0),D2=ifelse(DAY==6,1,0),
               D3=ifelse(DAY==6,1,0),D4=ifelse(DAY==6,1,0)) %>%
        summarise(D1=ifelse(sum(D1)>=1,1,0),
                  D2=ifelse(sum(D2)>=1,1,0),
                  D3=ifelse(sum(D3)>=1,1,0),
                  D4=ifelse(sum(D4)>=1,1,0)) %>% 
        mutate(RT1=ifelse((D1+D2+D3+D4)>1,1,0),RT2=ifelse((D1+D2+D3+D4)>2,1,0),RT3=ifelse((D1+D2+D3+D4)>3,1,0)) %>%
        select(user_id,RT1,RT2,RT3)

ET 
ET %>% summarise(RTT1=sum(RT1)/n(),RTT2=sum(RT2)/n(),RTT3=sum(RT3)/n())


## ¿Qué analisis realizarias si tuvieras acceso a datos adicionales?
###############

##Contabilizaría a tiempo real los Success/fail que estén asociados al uso de powerups durante la partida y los que no, 
##así como la cantidad de powerups usados en ellos. La suma de éstos (como hemos visto en la tabla que he calculado llamada POWERUP_TABLE)
##debería ser igual a la suma total de Success/Fail total, que es la que me habéis proporcionado.
##De esta forma podríamos valorar mucho mejor la influencia de los pu en la dificultad, en la retención de jugadores y  y 

##Me gustaría también poder desgranar los tipos de pu y sus costes en la partida, aunque a rasgos generales no esté mal analizar como si 
##fueran un mismo ente (pese a que se puede jugar mucho en relación al peso económico y la incidencia que tienen)

##Datos adicionales del usuario tipo, como un historial más amplio de su actividad, con la idea de tener trazados sus movimientos

################################
########## Test A/B ############
################################

GT <- read.csv("ab_test.csv")

    GT=as.data.table(GT)

        GT %>% filter(ab_group=="A") %>% summary()

        GT %>% filter(ab_group=="B") %>% summary()

GT %>% group_by(ab_group) %>% 
    summarise(TOT_STARTS = mean(total_game_starts),
              TOT_PU=mean(total_pu_uses),TOT_REN=mean(total_revenue),
              TOT_PROD=mean(total_product))

##Una primera visual ofrece muy pocas variaciones en los indicadores estándares principales, continuamos el TestA/B
#???#De manera que hacemos el test en estilo overlap para visualizar el histograma, para los diferentes "Rates"

START_RATE =  GT %>% 
    group_by(total_game_starts) %>%
    mutate(A=ifelse(ab_group=="A",1,0),B=ifelse(ab_group=="B",1,0)) %>% 
    summarise(A=sum(A),B=sum(B))

        START_RATE %>% 
            gather(key=total_game_starts, value=Value) %>% 
            ggplot(aes(x=Value,fill=total_game_starts)) + 
            geom_histogram()+
            xlim(0,max(START_RATE$total_game_starts))

PU_RATE =  GT %>% 
    group_by(total_pu_uses) %>%
    mutate(A=ifelse(ab_group=="A",1,0),B=ifelse(ab_group=="B",1,0)) %>% 
    summarise(A=sum(A),B=sum(B))

        PU_RATE %>% 
            gather(key=total_pu_uses, value=Value) %>% 
            ggplot(aes(x=Value,fill=total_pu_uses)) + 
            geom_histogram()+
            xlim(0,max(PU_RATE$total_pu_uses))+
            ylim(0,40)

REN_RATE =  GT %>% 
    group_by(total_revenue) %>%
    mutate(A=ifelse(ab_group=="A",1,0),B=ifelse(ab_group=="B",1,0)) %>% 
    summarise(A=sum(A),B=sum(B))
        
        REN_RATE %>% 
            gather(key=total_revenue, value=Value) %>% 
            ggplot(aes(x=Value,fill=total_revenue)) + 
            geom_histogram()+
            xlim(0,max(REN_RATE$total_revenue))+
            ylim(0,200)

PROD_RATE =  GT %>% 
    group_by(total_product) %>%
    mutate(A=ifelse(ab_group=="A",1,0),B=ifelse(ab_group=="B",1,0)) %>% 
    summarise(A=sum(A),B=sum(B))

        PROD_RATE %>% 
            gather(key=total_product, value=Value) %>% 
            ggplot(aes(x=Value,fill=total_product)) + 
            geom_histogram()+
            xlim(0,max(PROD_RATE$total_product)) +
            ylim(0,25)
        
        
GT_1 = GT %>% group_by(ab_group) %>% 
    summarise(USR=n(),
              STR=sum(TS),
              REN=sum(total_revenue),
              PUR=sum(total_product)) %>% 
    mutate(`S/U`=STR/USR,
           `R/U`=REN/USR,
           `P/U`=PUR/USR) %>% 
    adorn_totals(where="row")

sum(GT_1[,2])
for(x in 6:8){
        GT_1[3,x]=((GT_1[2,x]-GT_1[1,x])/GT_1[1,x])*100
}

kable(GT_1)

##Una primera métrica sería establecer indicadores de densidad, o peso específico, para comprobar qué ratio de comienzos de partida, que los llamamos S/U (cantidad de partidas comenzadas por cantidad 
##de usuarios que han participado) y R/U // P/U con cantidad de dinero inyectado y compras realizadas por usuario.
##Después compruebo en estas variables el crecimiento o descenso que ha sufrido en cantidades cuando se ha pasado al grado de dificultad B, y efectivamente han aumentado, aun que no en un porcentaje muy alto (en torno al 3,3%)

GT_2 = GT %>% group_by(ab_group) %>% 
        summarise(USR=n(),
              STR=sum(total_game_starts),
              PWU=sum(total_pu_uses),
              REN=sum(total_revenue),
              PUR=sum(total_product)) %>% 
    mutate(`S/T`=STR/sum(STR),
           `PU/T`=PWU/sum(PWU),
           `R/T`=REN/sum(REN),
           `P/T`=PUR/sum(PUR))

###

GT_3= GT %>% group_by(ab_group) %>% 
    summarise("CORpu-ren"=cor(total_pu_uses,total_revenue),
              "CORpu-prd"=cor(total_pu_uses,total_product),
              "CORpu-str"=cor(total_pu_uses,total_game_starts),
              "CORstr-ren"=cor(total_game_starts,total_revenue),
              "CORstr-prod"=cor(total_game_starts,total_product))

GT_3 = adorn_totals(GT_3,where="row")
for(x in 2:5){
        GT_3[3,x]=((GT_3[2,x]-GT_3[1,x])/GT_3[1,x])*100
}

GT_3

    median(as.double(GT_3[3,2:5]))

    mean(as.double(GT_3[3,2:5]))

GT %>% filter(ab_group=="A") %>% 
    ggplot(aes(x=total_pu_uses,y=total_game_starts)) + 
    geom_point()

##En un mapa de correlaciones observamos que se ha reducido la correlación entre las diferentes variables (Start,PU,REN,PROD) 
##Al pasar al grado de dificultad B que no es beneficiosa, puesto que se diluye un patrón reconocible entre las 
##Esto nos puede situar en una aparente "incogruencia" ya que se presupone, al ver que existe un crecimiento objetivo de los KPI 
##relativos al aumento de compras de productos y dinero gastados así comop uso de powerups, pero sin embargo se ha reducido la relación entre estas variables.
##Se intentará solventar esto clasificando por tipo de jugador

GT = GT %>% rename(TS=total_game_starts,PU=total_pu_uses,RE=total_revenue,PR=total_product,AB=ab_group)

GT_4 = GT %>% group_by(AB) %>% 
    mutate(T1=ifelse(TS<max(TS)/6,1,0),
           T2=ifelse(TS %between% c(max(TS)/6, max(TS)/5),1,0),
           T3=ifelse(TS %between% c(max(TS)/5, max(TS)/4),1,0),
           T4=ifelse(TS %between% c(max(TS)/4, max(TS)/3),1,0),
           T5=ifelse(TS %between% c(max(TS)/3, max(TS)/1.5),1,0),
           T6=ifelse(TS>max(TS)/1.5,1,0)) %>% 
    summarise(T1=sum(T1),
              T2=sum(T2),
              T3=sum(T3),
              T4=sum(T4),
              T5=sum(T5),
              T6=sum(T6))

GT_5 = GT %>% group_by(AB) %>% 
    mutate(T1=ifelse(PU<max(PU)/6,1,0),
           T2=ifelse(PU %between% c(max(PU)/6, max(PU)/5),1,0),
           T3=ifelse(PU %between% c(max(PU)/5, max(PU)/4),1,0),
           T4=ifelse(PU %between% c(max(PU)/4, max(PU)/3),1,0),
           T5=ifelse(PU %between% c(max(PU)/3, max(PU)/1.5),1,0),
           T6=ifelse(PU>max(PU)/1.5,1,0)) %>% 
    summarise(T1=sum(T1),
              T2=sum(T2),
              T3=sum(T3),
              T4=sum(T4),
              T5=sum(T5),
              T6=sum(T6))

GT_6 = GT %>% group_by(AB) %>% 
    mutate(T1=ifelse(PR<mean(PR[PR!=0])*10/6,1,0),
           T2=ifelse(PR %between% c(mean(PR[PR!=0])*10/6, mean(PR[PR!=0])*10/5),1,0),T3=ifelse(PR %between% c(mean(PR[PR!=0])*10/5, mean(PR[PR!=0])*10/4),1,0),T4=ifelse(PR %between% c(mean(PR[PR!=0])*10/4, mean(PR[PR!=0])*10/3),1,0),T5=ifelse(PR %between% c(mean(PR[PR!=0])*10/3, mean(PR[PR!=0])*10/1.5),1,0),T6=ifelse(PR>mean(PR[PR!=0])*10/1.5,1,0)) %>% summarise(T1=sum(T1),T2=sum(T2),T3=sum(T3),T4=sum(T4),T5=sum(T5),T6=sum(T6))

kable(GT_4)
kable(GT_5)
kable(GT_6)

##Y observamos los sumarios por grupo:

kable(summary(filter(GT,AB=="A")))
kable(summary(filter(GT,AB=="B")))

GT %>% group_by(AB) %>% summarise(PU = max(PU),PR = (mean(PR)*10)) %>% rbind(c())


##Pero no se obtiene gran cosa más. Con los datos proporcionados podemos concluir que haber pasado al nivel de dificultad "B" ha tenido mejoras,
##El consumo de los PU y PR ha aumentado como se comentaba antes, y observamos en los sumarios que el valor máximo de consumos en determinados 
##jugadores se ha incrementado al doble en PU, y en más de un 40% en compras, lo que sugiere que se ha vuelto

##Debo decir que no conozco bien las metodologías que se siguen para un test A/B me harían falta datos de días para estudiar las retenciones o los llamados DAUs o MAUs (Daily active Users)


################################
########## USUARIOS DIA ########
################################

# 
# ¿Cúantos usuarios harían falta para poder estimar este KPI con un nivel de confianza al 90% y con un tamaño del intervalo de confianza de 5%?
# 
# 
# La respuesta a esta pregunta es sencilla, aunque hacemos algunas suposiciones, junto con las condiciones de contorno:
# 
#  - Se supone una población universal infinita, dado la capacidad de muestreo de la que se dispone
#  - Se supone una probabilidad de retención a día uno del 50% (40-60%)
#  - Nivel de confianza del 90%
#  - Error máximo asumido o intervalo de confianza del 5%
#  - Se asume que su distribución es atribuible a una Distribucion normal N(0,1)
# 
# 
# Y usando el principio del Teorema central, y simplificando, aplicamos la siguiente fórmula:
# 
#         n = (Z^2·p(1-p))/e^2
# 
#         Donde:
#                 - Z = Zalpha (para intervalo de confianza de 95%  ~1,62 interpolando)
#                 - p = probabilidad de éxito en la retencion a día 1
#                 - e = intervalo de confianza

Nsamples1 = ((1.62^2)*0.5*(0.5))/(0.05^2)
Nsamples1

Nsamples2=sample.size.prop(e=0.05, P=0.5, N=Inf, level=0.9)
Nsamples2

# La muestra total para cumplir con el objetivo de confianza propuesto es de 271 muestras. Como se puede ver,
# calculado manualmente se obtiene una cifra parecida, en torno a las 262 muestras
# 
# Básicamente esto nos explica, basándonos en el Teorema central del límite y llevando en cuenta las asunciones hechas 
# más arriba, que para una distribución de probabilidad asumible como normal, necesitaríamos como mínimo una muestra > 270 (jugadores en nuestro caso)
# de la población completa para que la estimación de la probabilidad de que un sujeto que juegue un día vaya a hacerlo al día siguiente (que en nuestro
##sector como dice el enunciado suele tener un valor comprendido entre 40-60%)

