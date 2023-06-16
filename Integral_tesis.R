#############################################
#Proyecto:Efecto del PZLFN en las ventas internacionales

#Paquetes
#install.packages("treemap")
#install.packages("writexl")
#install.packages("corrplot")
#install.packages("forcats")
#install.packages("xtable")
#install.packages("sjPlot")
#install.packages("tidyverse")
#install.packages('car')
#install.packages('xlsx')
#install.packages('reshape2')
#install.packages('texreg')


#Librerias

library(readxl)
library(tidyverse)
library(ggplot2)
options(scipen=999)
library(treemap)
library(haven)
#library(xlsx)
library(corrplot)
library(forcats)
library(class)
library(reshape2)
library(xtable)
library (car)
library(corrplot)
library(texreg)
library(sjPlot)
library(xtable)


#Setdirectory
setwd("~/2023/Tesis parent project exportaciones municipales/Data codes/R")

#Preparación de bases de datos

##Base de datos de exportaciones
##importacion 
tri_mun_h02_com <- read.csv("~/2023/Tesis parent project exportaciones municipales/Data storage/Raw/tri_mun_h02_com.csv")

##transformación
###Compras  ###base de datos para histórico

comer_tem_com<-tri_mun_h02_com %>%
  mutate(anio=as.character(substring(Quarter.ID,1,4))) %>% 
  mutate(t=as.character(substring(Quarter.ID,5,5))) %>% 
  mutate(tri=paste0("t",t,anio)) %>% 
  mutate(ent=ifelse(State.ID<=9,paste0("0",as.character(State.ID)),as.character(State.ID))) %>% 
  mutate(mun=ifelse(nchar(as.character(Municipality.SUN.ID))==5,as.character(substring(Municipality.SUN.ID,3,5)),as.character(substring(Municipality.SUN.ID,2,4)))) %>% 
  filter(Flow.ID==1) %>% 
  filter(as.numeric(anio)>=2016) %>% 
  filter(as.numeric(anio)<=2021) %>% 
  mutate(compras_dls=Trade.Value) %>% 
  mutate(cve=paste0(ent,mun,tri)) %>% 
  select(cve,compras_dls) %>% 
  group_by(cve) %>% 
  summarise(compras_dls = sum(compras_dls))


###Ventas ###base de datos para histórico

comer_tem_ven<-tri_mun_h02_com %>%
  mutate(anio=as.character(substring(Quarter.ID,1,4))) %>% 
  mutate(t=as.character(substring(Quarter.ID,5,5))) %>% 
  mutate(tri=paste0("t",t,anio)) %>% 
  mutate(ent=ifelse(State.ID<=9,paste0("0",as.character(State.ID)),as.character(State.ID))) %>% 
  mutate(mun=ifelse(nchar(as.character(Municipality.SUN.ID))==5,as.character(substring(Municipality.SUN.ID,3,5)),as.character(substring(Municipality.SUN.ID,2,4)))) %>% 
  filter(Flow.ID==2) %>% 
  filter(as.numeric(anio)>=2016) %>% 
  filter(as.numeric(anio)<=2021) %>%
  mutate(ventas_dls=Trade.Value) %>% 
  mutate(cve=paste0(ent,mun,tri)) %>% 
  select(cve,ventas_dls) %>% 
  group_by(cve) %>% 
  summarise(ventas_dls = sum(ventas_dls))

###Integración de base de datos histórica
comer_hist<-full_join(comer_tem_ven,comer_tem_com,by="cve")
comer_hist<-comer_hist %>% 
  mutate(anio_tri_cve=substr(cve,6,11))

###Integración de bases de datos solo con ventas y compras

comer<-inner_join(comer_tem_ven,comer_tem_com,by="cve")

comer<-comer %>% 
  mutate(anio_tri_cve=substr(cve,6,11))

###limpieza de memoria
rm(comer_tem_com,comer_tem_ven,tri_mun_h02_com)

#Transformación de valores de comercio a dólares

tipo_cambio_base <- read_excel("~/2023/Tesis parent project exportaciones municipales/Data storage/Raw/tipo_cambio.xlsx", ,sheet = "Hoja1", range = "a18:b999",col_types = c("date", "numeric"))
tipo_cambio_base<-tipo_cambio_base[!is.na(tipo_cambio_base$Fecha),]


##trimestralizaciÃ³n del tipo de cambio
tipo_cambio<-tipo_cambio_base %>% 
  mutate(fecha=as.character(Fecha)) %>%
  mutate(tri=substring(as.character(quarters(Fecha)),2,2)) %>% 
  mutate(anio=substring(fecha,1,4)) %>% 
  mutate(anio_tri_cve=gsub(" ","",paste("t",tri,anio))) 

tipo_cambio<-tipo_cambio %>% 
  group_by(anio_tri_cve) %>%
  summarise(tipo_cambio=mean(SF18561))

rm(tipo_cambio_base)


##unión de bases de datos
base_ventas<-inner_join(comer,tipo_cambio,by="anio_tri_cve")

###unión de bases de datos histórica
comer_hist<-inner_join(comer_hist,tipo_cambio,by="anio_tri_cve")

##limpieza de memoria
rm(tipo_cambio)

##Calculo de ventas y compras en pesos
base_ventas<-base_ventas %>% 
  mutate(compras_pesos=compras_dls*tipo_cambio) %>% 
  mutate(ventas_pesos=ventas_dls*tipo_cambio)


##Limipieza de memoria
rm(comer)

##Calculo de ventas y compras en pesos histórica
comer_hist<-comer_hist %>% 
  mutate(compras_pesos=compras_dls*tipo_cambio) %>% 
  mutate(ventas_pesos=ventas_dls*tipo_cambio)


#Importación de base de datos de IMMEX

immex_original<- read_excel("~/2023/Tesis parent project exportaciones municipales/Data storage/Created/IMMEX/IMMEX.xlsx")
immex_hist<-immex_original %>% 
  mutate(mun=substr(cve_mun,3,5)) %>% 
  mutate(ent=cve_ent) %>% 
  #filter(mun!="000") %>% 
  #filter(mun!="999") %>% 
  select(ent,mun,mes,anio,dias,po,rem,ht,ing,ins,ser)

##Trimestralización de datos de IMMEX

immex_hist_tri<-immex_hist %>%
  mutate(t=case_when(as.numeric(mes)<=3 ~"1",as.numeric(mes)==4~"2"	,as.numeric(mes)==5~"2",
                     as.numeric(mes)==6~"2"	,as.numeric(mes)==7~"3"	,as.numeric(mes)==8~"3",
                     as.numeric(mes)==9~"3"	,as.numeric(mes)==10~"4"	,as.numeric(mes)==11~"4",
                     as.numeric(mes)==12~"4")) %>% 
  mutate(cve=paste0(ent,mun,"t",t,anio)) %>% 
  group_by(cve) %>% 
  summarise(dias=mean(dias),po=mean(po),rem=sum(rem),ht=mean(ht),ing=sum(ing),ins=sum(ins),ser=sum(ser))

##Limpieza de memoria
rm(immex_original,immex_hist)


#importación de base de datos de imss
imss_tri <- read_dta("~/2023/Tesis parent project exportaciones municipales/Data storage/Raw/IMSS/imss_tri.dta")

imss<-imss_tri %>% 
  select(cve,ta_sal,masa_sal_ta,masa_sal_ta_mes)
##limpieza de memoria
rm(imss_tri)

#Integración de base datos, solo municipios con ventas internacionales
base_com<-inner_join(base_ventas,immex_hist_tri,by="cve")
base_com<-inner_join(base_com,imss,by="cve")
base_com<-base_com %>% 
  mutate(ent_mun_cve=substring(cve,1,5))


##integración del indice de produccción de Estados Unidos y tipo de cambio real
indice_prod <- read_excel("~/2023/Tesis parent project exportaciones municipales/Data storage/Raw/indice_prod.xls")
indice_prod_usa<-indice_prod %>% 
  select(tri,index_usa)
tc_real<-indice_prod %>% 
  select(tri,tc_real)


#Identificación de municipios fronterizos
Mun_fron <- read_excel("~/2023/Tesis parent project exportaciones municipales/Data storage/Raw/Mun_fron.xlsx")
cat_mun_fron<-Mun_fron %>% 
  mutate(mun_fron=Mun_fron) %>% 
  mutate(ent_mun_cve=paste0(ent,mun)) %>% 
  select(ent_mun_cve,mun_fron)
base_com<-inner_join(base_com,cat_mun_fron,by="ent_mun_cve")


##limpieza de memoria
rm(Mun_fron)

#IdentificaciÃ³n de municipios con informaciÃ³n completa en la serie desde el primer trimestre de 2016 hasta el cuarto trimestre de 2021


mun_com<-base_com %>%
  group_by(ent_mun_cve) %>% 
  summarise(registros=n()) %>% 
  filter(registros==max(registros))


base_inf_completa<-base_com %>% 
  inner_join(mun_com)

#Calculo de la participación de los municipios fronterizos con información completa en el 2018
tot_ventas_2018_mun<-base_com %>%
  mutate(anio=as.numeric(substr(cve,8,11))) %>% 
  filter(mun_fron==1) %>%
  filter(anio==2018) %>% 
  summarise(sum(ventas_pesos))

tot_ventas_2018_mun_sel<-base_inf_completa %>%
  mutate(anio=as.numeric(substr(cve,8,11))) %>% 
  filter(mun_fron==1) %>%
  filter(anio==2018) %>% 
  summarise(sum(ventas_pesos))

Participacion_mun_sel<-(tot_ventas_2018_mun_sel/tot_ventas_2018_mun)*100

tot_ventas_2018<-base_com %>%
  mutate(anio=as.numeric(substr(cve,8,11))) %>% 
  filter(anio==2018) %>% 
  summarise(sum(ventas_pesos))

Participacion_mun_fron<-(tot_ventas_2018_mun/tot_ventas_2018)*100


#write.csv(base_inf_completa,"base_inf_completa.csv")

################################################################################
#Identificación de municipios control
#Importación de datos de los Censos Económicos 2019
## importaciÃ³n de datos

ce<-read_dta("~/2023/Tesis parent project exportaciones municipales/Data storage/Raw/CE/Bases/ce.dta")
ce_mun<-ce %>% 
  select(-a112a,-a113a,-a114a,-a115a,-a116a,-a117a,-a118a,-a119a,-a141a,-a143a,-a144a,-a145a,-a146a,
         -a147a,-a148a,-a149a,-a151a,-a152a,-a153a,-a154a,-a155a,-a156a,-a157a,-a158a,-a159a,-a171a,
         -a173a,-a174a,-a175a,-a176a,-a177a,-a179a,-a181a,-a182a,-a192a,-a193a,-a194a,-a195a,-a197a,
         -a201a,-a202a,-a204a,-a206a,-a208a,-a209a,-a212a,-a213a,-a214a,-a215a,-a216a,-a217a,-a218a,
         -a219a,-a222a,-a223a,-a224a,-a225a,-a226a,-a227a,-a228a,-a229a,-a231a,-a235a,-a236a,-a237a,
         -a238a,-a242a,-a243a,-a244a,-a245a,-a247a,-a248a,-a252a,-a253a,-a256a,-a433a,-a434a,-a436a,
         -a529a,-a534a,-a747a,-a748a,-a752a,-a764a,-v187) %>% 
  filter(municipio!="" & (codigo=="31-33") & is.na(id_estrato))

##selección de datos con información de ventas

ce_mun_ventas<-ce_mun %>% 
  inner_join(base_inf_completa %>% select(ent_mun_cve) %>% unique())
##seleccion de variables numéricas

censo_2018_varnum<-(ce_mun_ventas %>% select(-mun_cve,-ent_mun_cve,-cve,-entidad,-ent_cve,-municipio,-codigo,-id_estrato))

##análisis de correlación entre variables
###selección de variables para análisis de correlación
censo_2018_varnum_corr<-censo_2018_varnum %>% 
  select(ue,a111a,a121a,a131a,a211a,a221a,a700a,a800a,j000a,k000a,k610a,m000a,m700a,o010a,p000a)

cor_censo_2018_varnum<-cor(censo_2018_varnum_corr)
write.csv(cor_censo_2018_varnum,"cor_censo_2018_varnum2.csv")

#calculo de pca
data.pca <- prcomp(censo_2018_varnum, scale = TRUE)
summary(data.pca)
plot(data.pca)


###calculate total variance explained by each principal component
var_explained = data.pca$sdev^2 / sum(data.pca $sdev^2)

#create scree plot
qplot(c(1:15), var_explained[1:15]) + 
  geom_line(color = 15,
            lwd = 2,
            linetype = 1) + 
  xlab("Componentes principales") + 
  ylab("Varianza explicada") +
  ylim(0, 1)+
  theme_minimal()


#con 12 componentes se acumula el 0.90 de la variabilidad

ce_n_componentes<-as.data.frame(data.pca$x) %>%
  select(1:12) %>%
  mutate(ent_mun_cve = ce_mun_ventas$ent_mun_cve)
ce_n_componentes_tip_mun<-ce_n_componentes %>% 
  inner_join(cat_mun_fron)

##limpieza de memoria
rm(ce,ce_mun,mun_com)

test <- summary(data.pca)$importance

write.table(test, "test.txt")

write_csv(ce_n_componentes, "ce_n_componentes.csv")


##limpieza de memoria
rm(ce,ce_mun,mun_com)

################################################################################
#Obtención de la distancia en Componetes Principales con cada uno de los municipios fronterizos y no fronterizos
#con un índice para filtarlos y analizarlos

municipios_fronterizos <- ce_n_componentes_tip_mun$ent_mun_cve[ce_n_componentes_tip_mun$mun_fron==1]
municipios_no_fronterizos <- ce_n_componentes_tip_mun$ent_mun_cve[ce_n_componentes_tip_mun$mun_fron==0]

# Data frame vacio
distancias_municipios <- tibble(ent_mun_cve_from = character(),
                                ent_mun_cve_to = character(),
                                distancia = numeric())

# LLenado iteractivo con el cálculo de la distancia euclidiana
for(mun_interes_fron in municipios_fronterizos){
  
  datos_num_mun_interes_fron <- ce_n_componentes_tip_mun %>% 
    filter(ent_mun_cve == mun_interes_fron) %>% 
    select(-ent_mun_cve, mun_fron) %>% 
    melt(id.vars = NULL) %>% #convierte de un dataframe a un vector con números
    pull(value)#extrae una simple columna
  
  for(mun_interes_no_fron in municipios_no_fronterizos){
    
    datos_num_mun_interes_no_fron <- ce_n_componentes_tip_mun %>% 
      filter(ent_mun_cve == mun_interes_no_fron) %>% 
      select(-ent_mun_cve, mun_fron) %>% 
      melt(id.vars = NULL) %>% 
      pull(value)
    
    distancia <- dist(rbind(datos_num_mun_interes_fron, datos_num_mun_interes_no_fron))#dist para distancia, rbin combina los dos objetos en este caso filas
    nueva_fila <- tibble(ent_mun_cve_from = mun_interes_fron, 
                         ent_mun_cve_to = mun_interes_no_fron, 
                         distancia = distancia)#creando data frame con los municipios fronterizos y su correspondiente distancia con los no fronterizos
    
    distancias_municipios <- distancias_municipios %>% 
      rbind(nueva_fila)#se incorpora una nueva fila
    
  }
  
}

#Incorpora un ranking
distancias_municipios <- distancias_municipios %>% 
  group_by(ent_mun_cve_from) %>% 
  mutate(rango = rank(distancia))#columna nueva que inicia en el 1 ordenados por distancia

#Catalogo 
mun_fron <- read_excel("~/2023/Tesis parent project exportaciones municipales/Data storage/Raw/Mun_fron.xlsx")

cat_mun_des <- mun_fron %>% 
  mutate(ent_mun_cve=paste0(ent,mun)) %>% 
  mutate(desc=paste(ent_nom,mun_nom,sep=", ")) %>% 
  select(ent_mun_cve,desc)

distancia_municipios_des <- distancias_municipios %>%
  left_join(cat_mun_des, by = c("ent_mun_cve_from" = "ent_mun_cve")) %>%
  left_join(cat_mun_des, by = c("ent_mun_cve_to" = "ent_mun_cve"))

##limpieza de memoria
rm(nueva_fila,mun_fron,distancias_municipios,censo_2018_varnum,
   datos_num_mun_interes_fron,datos_num_mun_interes_no_fron,distancia,mun_interes_fron,
   mun_interes_no_fron,municipios_fronterizos,municipios_no_fronterizos)


##selección de municipios control

distancia_municipios_sel<-distancia_municipios_des %>% 
  filter(rango<=15)
#write.csv(distancia_municipios_sel,"distancia_municipios_sin_filtro140523.csv")

###calculo de correlaciones entre municipios
matriz_mun_sel<-base_inf_completa %>%
  select(ent_mun_cve,anio_tri_cve,ventas_pesos) 

matriz_mun_cor<-matriz_mun_sel%>%
  spread(ent_mun_cve,ventas_pesos) %>%
  mutate(anio=substr(anio_tri_cve,3,6)) %>% 
  filter(anio<=2018) %>% 
  select(-anio,-anio_tri_cve)

cor(matriz_mun_cor)
#write.csv(cor(matriz_mun_cor),"corr_control_sin_filtro150423.csv")

#análisis manual

#distancia_municipios_sel<-distancia_municipios_des %>% 
#  filter(ent_mun_cve_from=="14097")

#compra<-base_com%>% 
#  filter(ent_mun_cve=="14097")

#boxplot(compra$ventas_pesos)
#summary(compra$ventas_pesos)

#write.csv(distancia_municipios_sel,"distancia_municipios_sin_filtro140523.csv")

distancia_municipios_sel <- read.csv("~/2023/Tesis parent project exportaciones municipales/Data codes/R/distancia_municipios_sin_filtro140523.csv")


distancia_municipios_sel_con<-distancia_municipios_sel %>% 
    select(-X) %>% 
  filter(sel=="xx") 


##limpieza de memoria
rm(matriz_mun,matriz_mun_cor,matriz_mun_sel)

##Generación de tabulado de municipios tratamiento y contról
tabulado <- distancia_municipios_sel_con %>% 
  mutate(Mun_trat = desc.x) %>% 
  mutate(Mun_cont = desc.y) %>% 
  select(Mun_trat, Mun_cont)

tabla_latex <- xtable(tabulado, caption = "Municipios tratamiento y municipios control correspondientes")
## Imprime la tabla en formato LaTeX
print(tabla_latex, caption.placement = "top")
##limpieza de memoria
rm(tabla_latex,tabulado,panel_descriptivas)

################################################################################
##Integración de base y modelos

mun_control<-distancia_municipios_sel_con %>% 
  mutate(ent_mun_cve=ifelse(nchar(ent_mun_cve_to)==5,ent_mun_cve_to,paste0("0",ent_mun_cve_to))) %>% 
  select(-ent_mun_cve_from,-ent_mun_cve_to,-distancia,-rango,-desc.x,-desc.y,-sel)


mun_estudio<-distancia_municipios_sel_con  %>% 
  mutate(ent_mun_cve=ifelse(nchar(ent_mun_cve_from)==5,ent_mun_cve_from,paste0("0",ent_mun_cve_from))) %>% 
  select(-ent_mun_cve_from,-ent_mun_cve_to,-distancia,-rango,-desc.x,-desc.y,-sel)

cat_panel<-mun_control %>% 
  union_all(mun_estudio)

#creación del panel eliminación de compras y ventas en dls,masa_sal_ta

base_coin_control<-base_inf_completa %>% 
  inner_join(cat_panel)

panel_ventas_modelo<-base_coin_control %>% 
  mutate(anio=as.numeric(substr(cve,8,11))) %>% 
  mutate(programa=ifelse(anio>=2019,1,0)) %>% 
  na.omit() %>% 
  mutate(interaccion_did=programa*mun_fron) %>% 
  mutate(tri=substring(cve,6,11)) %>% 
  inner_join(indice_prod_usa) %>% 
  inner_join(tc_real) %>% 
  select(-cve,-tipo_cambio,-tri,-registros,-anio,-ventas_dls,-compras_dls,-anio_tri_cve,-ent_mun_cve) %>% 
  mutate(compras_pesos=compras_pesos/1000) %>% 
  mutate(ventas_pesos=ventas_pesos/1000) %>% 
  mutate(rem_med=rem/po) %>% 
  mutate(rem_ht=rem/(ht*3)/1000) %>% 
  mutate(rem_ins=rem/ins) %>% 
  mutate(ser_ins=ser/ins)%>% 
  mutate(rem_med_imss=masa_sal_ta_mes/ta_sal/1000)


panel_ventas_modelo_analisis<-base_coin_control %>% 
  mutate(anio=as.numeric(substr(cve,8,11))) %>% 
  mutate(programa=ifelse(anio>=2019,1,0)) %>% 
 mutate(guerra=ifelse(anio>=2017,1,0)) %>% 
  na.omit() %>% 
  mutate(interaccion_did=programa*mun_fron) %>% 
  mutate(tri=substring(cve,6,11)) %>% 
  inner_join(indice_prod_usa) %>% 
  inner_join(tc_real) %>% 
  mutate(compras_pesos=compras_pesos/1000) %>% 
  mutate(ventas_pesos=ventas_pesos/1000) %>% 
  select(-cve,-tipo_cambio,-registros,-ventas_dls,-compras_dls,-anio_tri_cve,-ent_mun_cve) %>% 
 mutate(rem_med=rem/po) %>% 
  mutate(rem_ht=rem/(ht*3)/1000) %>% 
  mutate(rem_ins=rem/ins) %>% 
  mutate(ser_ins=ser/ins) %>% 
  mutate(rem_med_imss=masa_sal_ta_mes/ta_sal/1000)

#exportación de base de datos

#write.csv(panel_ventas_modelo_analisis,"panel_ventas_modelo_analisis.csv")

#datos escalados
panel_ventas_modelo_scale<-panel_ventas_modelo %>% 
  mutate_at(c("compras_pesos","ventas_pesos","dias","po","rem","ht","ing","ins","ser","ta_sal","masa_sal_ta_mes"),~(scale(.) %>% 
                                                                                                              as.vector))
################################################################################
#Estadísticas descriptivas

panel_descriptivas<-base_coin_control %>% 
  mutate(anio=as.numeric(substr(cve,8,11))) %>% 
  mutate(programa=ifelse(anio>=2019,1,0)) %>% 
  na.omit() %>% 
  mutate(interaccion_did=programa*mun_fron) %>% 
  mutate(tri=substring(cve,6,11)) %>% 
  inner_join(indice_prod_usa) %>% 
  inner_join(tc_real) %>% 
  mutate(compras_pesos=compras_pesos/1000) %>% 
  mutate(ventas_pesos=ventas_pesos/1000) %>% 
  select(-cve,-tipo_cambio,-tri,-registros,-anio,-ventas_dls,-compras_dls,-anio_tri_cve,-ent_mun_cve) %>% 
  mutate(rem_med=rem/po) %>% 
  mutate(rem_ht=rem/(ht*3)) %>% 
  mutate(rem_ins=rem/ins) %>% 
  mutate(ser_ins=ser/ins) %>% 
  mutate(rem_med_imss=(rem_med_imss=masa_sal_ta_mes/ta_sal))

##Tabulado descriptivas
tabulado<-panel_descriptivas %>% 
  group_by(mun_fron,programa) %>% 
  summarise(ventas_pesos=mean(ventas_pesos),compras_pesos=mean(compras_pesos),po=mean(po),rem=mean(rem),rem_med=mean(rem_med), ht=mean(ht),rem_ht=mean(rem_ht),ins=mean(ins),ta_sal=mean(ta_sal), rem_med_imss=mean(rem_med_imss),index_usa=mean(index_usa),tc_real=mean(tc_real),rem_ins=mean(rem_ins))

tabla_latex <- xtable(tabulado,caption = "Estadísticas descriptivas",digits = 3, big.mark = ",")

write.csv(tabulado,"descriptivas.csv")

## Imprime la tabla en formato LaTeX
print(tabla_latex, caption.placement = "top")
##limpieza de memoria
rm(tabla_latex,tabulado,panel_descriptivas)

##Pruebas de diferencia de medias

options(scipen=999)

library(tidyverse)

panel_ventas <-base_coin_control %>% 
  mutate(anio=as.numeric(substr(cve,8,11))) %>% 
  mutate(programa=ifelse(anio>=2019,1,0)) %>% 
  na.omit() %>% 
  mutate(interaccion_did=programa*mun_fron) %>% 
  mutate(tri=substring(cve,6,11)) %>% 
  inner_join(indice_prod_usa) %>% 
  inner_join(tc_real) %>% 
  mutate(compras_pesos=compras_pesos/1000) %>% 
  mutate(ventas_pesos=ventas_pesos/1000) %>% 
  select(-cve,-tipo_cambio,-tri,-registros,-ventas_dls,-compras_dls,-anio_tri_cve,-ent_mun_cve) %>% 
  mutate(rem_med=rem/po/1000) %>% 
  mutate(rem_ht=rem/(ht*3)/1000) %>% 
  mutate(rem_ins=rem/ins) %>% 
  mutate(ser_ins=ser/ins) %>% 
  mutate(rem_med_imss=(rem_med_imss=masa_sal_ta_mes/ta_sal/1000))

################################################################################
##Pruebas de diferencia de medias
#######################ventas
# Solamente fronterizo vs no fronterizo ventas internacionales
ventas_fronterizo <- panel_ventas$ventas_pesos[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$ventas_pesos[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo, mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)
# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$ventas_pesos[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$ventas_pesos[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$ventas_pesos[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$ventas_pesos[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

#######################compras
# Solamente fronterizo vs no fronterizo ventas internacionales
ventas_fronterizo <- panel_ventas$compras_pesos[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$compras_pesos[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo, mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$compras_pesos[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$compras_pesos[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$compras_pesos[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$compras_pesos[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

#######################remuneraciones medias
# Solamente fronterizo vs no fronterizo remuneraciones medias
ventas_fronterizo <- panel_ventas$rem_med[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$rem_med[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo,  mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$rem_med[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$rem_med[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$rem_med[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$rem_med[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues, mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

#######################remuneraciones medias imss

# Solamente fronterizo vs no fronterizo remuneraciones
ventas_fronterizo <- panel_ventas$rem_med_imss[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$rem_med_imss[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo, mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$rem_med_imss[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$rem_med_imss[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$rem_med_imss[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$rem_med_imss[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues, mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues, mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)


#######################remuneraciones por hora trabajada

# Solamente fronterizo vs no fronterizo remuneraciones
ventas_fronterizo <- panel_ventas$rem_ht[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$rem_ht[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo,  mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$rem_ht[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$rem_ht[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$rem_ht[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$rem_ht[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues, mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues, mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

#######################rem_ins

# Solamente fronterizo vs no fronterizo remuneraciones
ventas_fronterizo <- panel_ventas$rem_ins[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$rem_ins[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo, mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$rem_ins[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$rem_ins[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$rem_ins[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$rem_ins[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues, mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

#######################ser_ins

# Solamente fronterizo vs no fronterizo remuneraciones
ventas_fronterizo <- panel_ventas$ser_ins[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$ser_ins[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo,  mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$ser_ins[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$ser_ins[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$ser_ins[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$ser_ins[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues, mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)


#######################ser_ins

# Solamente fronterizo vs no fronterizo remuneraciones
ventas_fronterizo <- panel_ventas$ser_ins[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$ser_ins[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo,  mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$ser_ins[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$ser_ins[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$ser_ins[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$ser_ins[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues, mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

#######################personal ocupado IMMEX
# Solamente fronterizo vs no fronterizo ventas internacionales
ventas_fronterizo <- panel_ventas$po[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$po[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo, mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$po[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$po[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$po[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$po[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

#######################remuneraciones IMMEX
# Solamente fronterizo vs no fronterizo ventas internacionales
ventas_fronterizo <- panel_ventas$rem[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$rem[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo, mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$rem[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$rem[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$rem[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$rem[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

#######################horas IMMEX
# Solamente fronterizo vs no fronterizo ventas internacionales
ventas_fronterizo <- panel_ventas$ht[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$ht[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo, mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$ht[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$ht[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$ht[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$ht[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)


#######################po IMSS
# Solamente fronterizo vs no fronterizo ventas internacionales
ventas_fronterizo <- panel_ventas$ta_sal[panel_ventas$mun_fron==1]
ventas_no_fronterizo <- panel_ventas$ta_sal[panel_ventas$mun_fron==0]

t.test(ventas_fronterizo, y = ventas_no_fronterizo, mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Antes y despues del tratamiento, por fronterizo y no fronterizo

ventas_fronterizo_antes <- panel_ventas$ta_sal[panel_ventas$mun_fron==1 & panel_ventas$anio<=2018]
ventas_fronterizo_despues <- panel_ventas$ta_sal[panel_ventas$mun_fron==1 & panel_ventas$anio>=2019]
ventas_no_fronterizo_antes <- panel_ventas$ta_sal[panel_ventas$mun_fron==0 & panel_ventas$anio<=2018]
ventas_no_fronterizo_despues <- panel_ventas$ta_sal[panel_ventas$mun_fron==0 & panel_ventas$anio>=2019]

# fronterizos, paired=true porque son los mismos municipios
t.test(ventas_fronterizo_antes, y = ventas_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

# no fronterizos, paired=true porque son los mismos municipios
t.test(ventas_no_fronterizo_antes, y = ventas_no_fronterizo_despues,  mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)


################################################################################
##Gráfica de tendencias paralelas

indice<-base_coin_control %>%
  select(ventas_pesos,cve,ent_mun_cve,mun_fron) %>%
  mutate(anio_tri=as.numeric(paste0(substring(cve,8,11), substring(cve,7,7))))  %>%
  select(-cve,-ent_mun_cve) %>% 
  group_by(mun_fron,anio_tri) %>% 
  summarise(ventas_pesos=sum(ventas_pesos))

indice_base <- indice %>%
  filter(anio_tri=="20161") %>%
  transmute(mun_fron, base_ventas_2016 = ventas_pesos)


indice <- indice %>%
  inner_join(indice_base) %>% 
  mutate(ventas_indizado = ventas_pesos/base_ventas_2016*100,
         mun_fron=as.factor(mun_fron))

ggplot(indice, aes(x=anio_tri, y=ventas_indizado, group = mun_fron, colour= mun_fron)) +
  geom_line()+
  xlab("Trimestre") + 
  ylab("Indice de Ventas")+
  theme_minimal()+
  scale_color_discrete(name = "Municipio \nFronterizo")


###############################################################################
## Poniendo los datos en formato DID parallel trends with multiple periods

options(scipen=999)

library(tidyverse)

panel_ventas<-base_coin_control %>%
  mutate(anio = str_sub(base_coin_control$cve, 8, 12),
         programa=ifelse(anio<2019,0,1)) %>% 
  select(-anio_tri_cve,-registros,-ventas_dls,-compras_dls,-masa_sal_ta ) %>% 
  unique()

# Crear las variables dummies especiales "D" para el multiple periods
panel_ventas_didmp <- panel_ventas %>% 
  select(cve, anio, mun_fron, programa) %>% 
  mutate(cve_mun = str_sub(panel_ventas$cve, 1, 5),
         dummy_did_nombre = paste0("D_", anio),
         dummy_did_valor = mun_fron) %>% 
  select(-anio, -cve_mun) %>% 
  spread(dummy_did_nombre, dummy_did_valor, fill = 0) %>% 
  select(cve, starts_with("D_")) %>% 
  select(-"D_2018") # manualmente quitamos la del anio anterior al PZLFN

# Remover la variable "original" de DID y pegarle las de DID multiple periods  
panel_ventas_mp <- panel_ventas %>% 
  inner_join(panel_ventas_didmp) %>% 
  select(-cve,-anio,-ent_mun_cve) 

panel_ventas_mp_scale <-as.data.frame(scale(panel_ventas_mp))

# Intentemos el diff in diff sin covariables
did_mp_v0 <- lm(ventas_pesos ~ . , data = panel_ventas_mp_scale %>% select(ventas_pesos, mun_fron, programa, starts_with("D_")))
summary(did_mp_v0)

################################################################################
##Gráfica de correlaciones

corrplot(cor(panel_ventas_modelo %>% select(-mun_fron,-programa,-interaccion_did,-rem_ht,-ser_ins)))

#scatterplotMatrix(panel_ventas_modelo %>% select(-mun_fron,-programa,-interaccion_did,-rem_ht,-ser_ins))

################################################################################
##Limpieza de memoria


##limpieza de memoria
rm(mun_control,mun_estudio,distancia_municipios_des,distancia_municipios_sel,distancia_municipios_sel_con,indice_prod,indice_prod_usa,imss,immex_hist_tri,tc_real,cat_mun_des,cat_mun_fron,cat_panel,ce_n_componentes_tip_mun,base_ventas,censo_2018_varnum,censo_2018_varnum_corr,cor_censo_2018_varnum,test,comer_hist)

################################################################################
##Generación de modelos

################################################################################
##Modelos simple sin transformación 

#primer ejercicio dif in dif

did_v1 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did, data = panel_ventas_modelo)
summary(did_v1)

#presentación de resultados
screenreg(did_v1, include.fstatistic=T,title="Modelos simples",custom.model.names = "Modelo base")

cov(did_v1$residuals,panel_ventas_modelo$interaccion_did)

plot_model(did_v1 ,vline.color = "green",title = "Modelo base",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v2 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med, data = panel_ventas_modelo)
summary(did_v2)

plot_model(did_v2 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMMEX",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v3 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med_imss, data = panel_ventas_modelo)
summary(did_v3)

plot_model(did_v3 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMSS",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()



#texreg(list(did_v1), booktabs = TRUE, dcolumn = TRUE, include.fstatistic=T,ci.test=NA)

lista_modelos <- list(did_v1,did_v2,did_v3)
nombres_modelos <- c("Modelo base", "Modelo remuneraciones medias IMMEX", "Modelo remuneraciones medias IMSS")

screenreg(lista_modelos, include.fstatistic=T,title="Modelos simples",custom.model.names = nombres_modelos)

##cálculo de covarianza para cada modelo
cov(did_v1$residuals,panel_ventas_modelo$interaccion_did)
cov(did_v2$residuals,panel_ventas_modelo$interaccion_did)
cov(did_v3$residuals,panel_ventas_modelo$interaccion_did)


###################
#segundo ejercicio dif in dif incorporación de las compras internacionales

did_v1 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+compras_pesos, data = panel_ventas_modelo)
summary(did_v1)

plot_model(did_v1 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación incluye compras internacionales",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v2 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med+compras_pesos, data = panel_ventas_modelo)
summary(did_v2)

plot_model(did_v2 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMMEX y compras internacionales",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v3 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med_imss+compras_pesos, data = panel_ventas_modelo)
summary(did_v3)

plot_model(did_v3 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMSS y compras internacionales",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()


#presentación de resultados

#texreg(list(did_v1), booktabs = TRUE, dcolumn = TRUE, include.fstatistic=T,ci.test=NA)

lista_modelos <- list(did_v1,did_v2,did_v3)
nombres_modelos <- c("Modelo base con compras", "Modelo remuneraciones medias IMMEX y compras", "Modelo remuneraciones medias IMSS y compras")

screenreg(lista_modelos, include.fstatistic=T,title="Modelos simples",custom.model.names = nombres_modelos)

##cálculo de covarianza para cada modelo
cov(did_v1$residuals,panel_ventas_modelo$interaccion_did)
cov(did_v2$residuals,panel_ventas_modelo$interaccion_did)
cov(did_v3$residuals,panel_ventas_modelo$interaccion_did)

###################
#tercer ejercicio dif in dif incorporación de insumos consumidos immex

did_v1 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+ins, data = panel_ventas_modelo)
summary


plot_model(did_v1 ,vline.color = "green",title = "M. base y consumos de ins. IMMEX",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

#presentación de resultados
screenreg(did_v1, include.fstatistic=T,title="M. base y consumos de ins. IMMEX",custom.model.names = "M. base y consumos de ins. IMMEX")


did_v2 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med+ins, data = panel_ventas_modelo)
summary(did_v2)

plot_model(did_v2 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMMEX e insumos consumidos IMMEX",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v3 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med_imss+ins, data = panel_ventas_modelo)
summary(did_v3)

plot_model(did_v3 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMSS e insumos consumidos IMMEX",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()


#presentación de resultados

#texreg(list(did_v1), booktabs = TRUE, dcolumn = TRUE, include.fstatistic=T,ci.test=NA)

lista_modelos <- list(did_v1,did_v2,did_v3)
nombres_modelos <- c("M. base y consumos de ins. IMMEX", "M. rem. med. y consumos de ins. IMMEX", "M. rem. med. IMSS e isn. IMMEX")

screenreg(lista_modelos, include.fstatistic=T,title="Modelos simples",custom.model.names = nombres_modelos)


##cálculo de covarianza para cada modelo
cov(did_v1$residuals,panel_ventas_modelo$interaccion_did)
cov(did_v2$residuals,panel_ventas_modelo$interaccion_did)
cov(did_v3$residuals,panel_ventas_modelo$interaccion_did)


###################
#cuarto ejercicio dif in dif incorporación de las tipo de cambio real

did_v1 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+tc_real, data = panel_ventas_modelo)
summary(did_v1)

plot_model(did_v1 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación incluye tipo de cambio real",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v2 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med+tc_real, data = panel_ventas_modelo)
summary(did_v2)

plot_model(did_v2 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMMEX y tipo de cambio real",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v3 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med_imss+tc_real, data = panel_ventas_modelo)
summary(did_v3)

plot_model(did_v3 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMSS y tipo de cambio real",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()


#presentación de resultados

#texreg(list(did_v1), booktabs = TRUE, dcolumn = TRUE, include.fstatistic=T,ci.test=NA)

lista_modelos <- list(did_v1,did_v2,did_v3)
nombres_modelos <- c("Modelo base con t.c. real", "Modelo remuneraciones medias IMMEX y t.c. real", "Modelo remuneraciones medias IMSS y t.c. real")

screenreg(lista_modelos, include.fstatistic=T,title="Modelos simples",custom.model.names = nombres_modelos)

##cálculo de covarianza para cada modelo
cov(did_v1$residuals,panel_ventas_modelo$interaccion_did)
cov(did_v2$residuals,panel_ventas_modelo$interaccion_did)
cov(did_v3$residuals,panel_ventas_modelo$interaccion_did)


###################
#quinto ejercicio dif in dif incorporación del índice de producción industrial de EU

did_v1 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+index_usa, data = panel_ventas_modelo)
summary(did_v1)

plot_model(did_v1 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación incluye el índice de producción industrial de EU",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v2 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med+index_usa, data = panel_ventas_modelo)
summary(did_v2)

plot_model(did_v2 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMMEX e índice de producción industrial de EU",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v3 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med_imss+index_usa, data = panel_ventas_modelo)
summary(did_v3)

plot_model(did_v3 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMSS e índice de producción industrial de EU",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()


#presentación de resultados

#texreg(list(did_v1), booktabs = TRUE, dcolumn = TRUE, include.fstatistic=T,ci.test=NA)

lista_modelos <- list(did_v1,did_v2,did_v3)
nombres_modelos <- c("M. base e IP EU", "M. remuneraciones medias IMMEX e IP EU", "M. remuneraciones medias IMSS e IP EU")

screenreg(lista_modelos, include.fstatistic=T,title="Modelos simples",custom.model.names = nombres_modelos)

##cálculo de covarianza para cada modelo
cov(did_v1$residuals,panel_ventas_modelo$interaccion_did)
cov(did_v2$residuals,panel_ventas_modelo$interaccion_did)
cov(did_v3$residuals,panel_ventas_modelo$interaccion_did)


###################
#sexto ejercicio dif in dif incorporación de insumos consumidos immex

did_v1 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+ins, data = panel_ventas_modelo)
summary(did_v1)

plot_model(did_v1 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación incluye insumos consumidos IMMEX",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v2 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med+ins, data = panel_ventas_modelo)
summary(did_v2)

plot_model(did_v2 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMMEX e insumos consumidos IMMEX",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v3 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med_imss+ins, data = panel_ventas_modelo)
summary(did_v3)

plot_model(did_v3 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMSS e insumos consumidos IMMEX",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()


#presentación de resultados

#texreg(list(did_v1), booktabs = TRUE, dcolumn = TRUE, include.fstatistic=T,ci.test=NA)

lista_modelos <- list(did_v1,did_v2,did_v3)
nombres_modelos <- c("M. base e índ. de prod. ind. de EU", "M. remuneraciones medias IMMEX e índ. de prod. ind. de EU", "M. remuneraciones medias IMSS e índ. de prod. ind. de EU")

screenreg(lista_modelos, include.fstatistic=T,title="Modelos simples",custom.model.names = nombres_modelos)

###################
#séptimo ejercicio dif in dif incorporación de servicios

did_v1 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+ser, data = panel_ventas_modelo)
summary(did_v1)

plot_model(did_v1 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación incluye insumos consumidos IMMEX",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v2 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med+ser, data = panel_ventas_modelo)
summary(did_v2)

plot_model(did_v2 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMMEX e insumos consumidos IMMEX",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()

did_v3 <- lm(ventas_pesos ~ mun_fron + programa+ interaccion_did+rem_med_imss+ser, data = panel_ventas_modelo)
summary(did_v3)

plot_model(did_v3 ,vline.color = "green",title = "Intervalos de coeficientes modelo simple sin transformación remuneraciones medias IMSS e insumos consumidos IMMEX",sort.est = TRUE,show.values = TRUE, value.offset = .3)+theme_bw()


#presentación de resultados

#texreg(list(did_v1), booktabs = TRUE, dcolumn = TRUE, include.fstatistic=T,ci.test=NA)

lista_modelos <- list(did_v1,did_v2,did_v3)
nombres_modelos <- c("M. base e índ. de prod. ind. de EU", "M. remuneraciones medias IMMEX e índ. de prod. ind. de EU", "M. remuneraciones medias IMSS e índ. de prod. ind. de EU")

screenreg(lista_modelos, include.fstatistic=T,title="Modelos simples",custom.model.names = nombres_modelos)

