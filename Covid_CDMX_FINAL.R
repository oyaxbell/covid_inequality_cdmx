#Social inequality drives adverse SARS-CoV-2 outcomes amongst vulnerable populations: The COVID-19 pandemic in Mexico City
#Data Analysis: Neftali Eduardo Antonio-Villa (nefoantonio@hotmail.com); Omar Yaxmehen Bello-Chavolla (oyaxbell@yahoo.com.mx)
#Latest version of Analysis 20-Sept-2020
#Any question regarding analysis contact Neftali Eduardo Antonio-Villa or Omar Yaxmehen Bello-Chavolla
#Disclaimer: The current dataset in part of the open-dataset from SINAVE-CDMX (https://datos.cdmx.gob.mx/explore/dataset/base-covid-sinave/export/)
#   https://datos.cdmx.gob.mx/explore/dataset/actas-de-defuncion-en-el-registro-civil-de-la-ciudad-de-mexico/export/
####Library management ####
library(tidyverse); library(dummies);library(lmerTest);library(lme4);library(grid);library(ggplotify)
library(dummies); library(glmnet);library(survival);library(survminer)
library(jtools);gollibrary(pROC);library(epiR);library(reportROC);library(haven)
library(lme4);library(simPH);library(webshot);library(htmlwidgets);library(RColorBrewer)
library(readr); library(tidyverse); library(survival); library(mediation); library(ggpubr); library(rms); library(psych); library(smoothHR)
library(survminer); library(haven); library(rsq); library(ResourceSelection); library(ggsci);library(timereg); library(coxme)
library(pROC); library(rgdal); library(ggpubr); library(ggsci); library(ggmap); library(scales); library(jtools); library(cowplot)
library(ggstance); library(flextable); library(simPH); library(ggthemes); library(lme4); library(lmerTest); library(prismatic)
library(ggplot2); theme_set(theme_bw()); library("sf"); library("rnaturalearth"); library("rnaturalearthdata"); library("maps")
library(fabricatr)
library(ggimage)
library(readxl)
library(zoo)
library(ggspatial)
library(coxme)
library(XML)
library("methods")

####Dataset managment----
setwd("/Users/nefoantonio/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PROYECTOS/COVID19 - TRABAJADORES CDMX")
setwd("/Users/HP-PC/UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/NEFTALI EDUARDO ANTONIO VILLA - COVID19 - TRABAJADORES CDMX")
setwd("/Users/nefoantonio/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PROYECTOS/COVID19 - TRABAJADORES CDMX")
cdmx <- read.csv("Bases de datos/sisver_public.csv")
cdmx[cdmx=="SE IGNORA"]<-"NO"
cdmx[cdmx=="SE IGNORA"]<-FALSE

names(cdmx)<-c("id","origen","sector","cve_entidad_unidad_medica","entidad_unidad_medica","delegacion_unidad_medica","unidad_medica","fecha_de_registro",
                 "sexo","entidad_residencia","cve_entidad_residencia","municipio_residencia","cve_municipio_residencia","localidad_residencia","clave_localidad_residencia",
                 "latloca","longloca","tipo_paciente","evolucion_caso","fecha_defuncion","semana_defuncion","defporinf","defverifi","intubado","diagnostico_clinico_neumonia","fecnaci",
                 "edad","nacionalidad","esta_emabarazada","meses_embarazo","es_indigena","habla_lengua_indigena","ocupacion","servicio_ingreso","fecha_ingreso",
                 "fecha_inicio_sintomas","diagnostico_probable","fiebre","tos","odinofagia","disnea","irritabilidad","diarrea","dolor_toracico","calofrios","cefalea","mialgias","artralgias",
                 "ataque_al_estado_general","rinorrea","polipnea", "vomito","dolor_abdominal","conjuntivitis","cianosis","inicio_subito_sintomas","diabetes","epoc","asma","inmunosupresivo","hipertension",
                 "VIH_SIDA","otra_condicion","enfermedad_cardiaca","obesidad",  "insuficiencia_renal_cronica","tabaquismo","recibio_tratamiento","recibio_tratamiento_antibiotico","recibio_tratamiento_antiviral", "antiviral", "fecha_inicio_tratamiento_antiviral",
                 "contacto_infeccion_viral","contacto_aves","contacto_cerdos","contacto_animales","vacunado","fecha_estimada_vacunacion","toma_muestra","laboratorio","folio_laboratorio",
                 "resultado_definitivo","es_migrante","pais_nacionalidad","pais_origen","fecha_ingreso_pais","paistran1","paistran2","paistran3","paistran4","puerperio", 
                 "dias_puerperio","antipireticos","unidad_cuidados_intensivos","origen_datos","linaje_influenza_tipo_b","viaje_1","viaje_2","viaje_3","viaje_4", "viaje_5","antigencovid","fecha_actualizacion")

cdmx<-cdmx%>%dplyr::filter(cve_entidad_unidad_medica==9)%>%filter(cve_entidad_residencia==9);cdmx$fecha_ingreso_2<-as.character(cdmx$fecha_ingreso)
cdmx$fecha_ingreso_2<-as.Date(cdmx$fecha_ingreso_2); cdmx<-cdmx%>%dplyr::filter(fecha_ingreso_2>="2020-03-01")%>%dplyr::filter(fecha_ingreso_2<="2021-01-31")
cdmx_V1<-cdmx
cdmx<-cdmx_V1

#Variables recodification
cdmx$covid<-NULL;cdmx$covid[cdmx$resultado_definitivo=="SARS-CoV-2"]<-1;cdmx$covid[cdmx$resultado_definitivo!="SARS-CoV-2"]<-0

cdmx[,c(29,31,32,24,25,38:67,83,91,94)][cdmx[,c(29,31,32,24,25,38:67,83,91,94)]=="NO"]<-0;
cdmx[,c(29,30,31,24,25,38:67,83,91,94)][cdmx[,c(29,31,32,24,25,38:67,83,91,94)]=="SI"]<-1
cdmx[,c(29,30,31,24,25,38:67,83,91,94)]<-sapply(cdmx[,c(29,30,31,24,25,38:67,83,91,94)], as.numeric)

cdmx$fecha_defuncion<-as.character(cdmx$fecha_defuncion);cdmx$fecha_defuncion[cdmx$fecha_defuncion==""]<-NA
cdmx$unidad_cuidados_intensivos[is.na(cdmx$unidad_cuidados_intensivos)]<-0;cdmx$intubado[is.na(cdmx$intubado)]<-0
cdmx$FU_time_MORT<-as.Date(cdmx$fecha_defuncion)-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$fecha_inicio_sintomas<-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$FU_time_MORT[is.na(cdmx$FU_time_MORT)]<-as.Date(cdmx$fecha_ingreso)-as.Date(cdmx$fecha_inicio_sintomas);cdmx$FU_time_MORT<-as.numeric(cdmx$FU_time)
cdmx$FU_time_HOSP<-as.Date(cdmx$fecha_ingreso)-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$fecha_inicio_sintomas<-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$FU_time_HOSP[is.na(cdmx$FU_time_HOSP)]<-as.Date(cdmx$fecha_ingreso)-as.Date(cdmx$fecha_inicio_sintomas);cdmx$FU_time_HOSP<-as.numeric(cdmx$FU_time_HOSP)
cdmx$numero_sintomas<-cdmx$fiebre+cdmx$tos+cdmx$disnea+cdmx$irritabilidad+cdmx$odinofagia+cdmx$diarrea+
  cdmx$dolor_toracico+cdmx$calofrios+cdmx$cefalea+cdmx$mialgias+cdmx$artralgias+cdmx$ataque_al_estado_general+
  cdmx$rinorrea+cdmx$polipnea+cdmx$vomito+cdmx$conjuntivitis+cdmx$cianosis+cdmx$inicio_subito_sintomas
cdmx$comorb<-cdmx$diabetes+cdmx$hipertension+cdmx$enfermedad_cardiaca+cdmx$insuficiencia_renal_cronica+
  cdmx$asma+cdmx$VIH_SIDA+cdmx$epoc+cdmx$obesidad+cdmx$tabaquismo+cdmx$otra_condicion


cdmx$comorb<-na.tools::na.replace(cdmx$comorb,0)
cdmx$asintomaticos[cdmx$numero_sintomas>0]<-0;cdmx$asintomaticos[cdmx$numero_sintomas==0]<-1
d1<-dummies::dummy(cdmx$evolucion_caso)
cdmx<-cbind(cdmx, d1)
d2<-dummies::dummy(cdmx$ocupacion)
cdmx<-cbind(cdmx, d2)
NAC_clin<-NULL;NAC_clin[cdmx$diagnostico_clinico_neumonia==1]<-1;NAC_clin<-na.tools::na.replace(NAC_clin,0)
edad65<-NULL;edad65[cdmx$edad>=65]<-1;edad65[cdmx$edad<65]<-0
edad40<-NULL;edad40[cdmx$edad<=40]<-1;edad40[cdmx$edad>40]<-0
DM2_edad40<-NULL;DM2_edad40[edad40==1 & cdmx$diabetes==1]<-1;DM2_edad40<-na.tools::na.replace(DM2_edad40,0)
indi_rec<-NULL;indi_rec[cdmx$es_indigena==1]<-1;indi_rec[cdmx$es_indigena==2]<-0
cdmx$intubado[cdmx$intubado==2]<-0
HOSP<-NULL;HOSP[cdmx$tipo_paciente=="HOSPITALIZADO"]<-1;HOSP<-na.tools::na.replace(HOSP,0)
cdmx$numero_sintomas<-cdmx$fiebre+cdmx$tos+cdmx$disnea+cdmx$irritabilidad+cdmx$odinofagia+cdmx$diarrea+
  cdmx$dolor_toracico+cdmx$calofrios+cdmx$cefalea+cdmx$mialgias+cdmx$artralgias+cdmx$ataque_al_estado_general+
  cdmx$rinorrea+cdmx$polipnea+cdmx$vomito+cdmx$conjuntivitis+cdmx$cianosis+cdmx$inicio_subito_sintomas
cdmx$asintomaticos[cdmx$numero_sintomas>0]<-0;cdmx$asintomaticos[cdmx$numero_sintomas==0]<-1
cdmx$asintomaticos<-na.tools::na.replace(cdmx$asintomaticos,0)

caso_grave<-cdmx$`evolucion_casoCASO GRAVE -`
tiempo_prolong<-NULL;tiempo_prolong[cdmx$FU_time_HOSP>=7]<-1;tiempo_prolong[cdmx$FU_time_HOSP<7]<-0
cdmx<-cbind(cdmx,NAC_clin,edad65,edad40,DM2_edad40,indi_rec,HOSP,tiempo_prolong,caso_grave)
cdmx$trab_salud<-cdmx$ocupacionMEDICOS+cdmx$ocupacionENFERMERAS+cdmx$ocupacionDENTISTAS+cdmx$`ocupacionOTROS TRABAJADORES DE LA SALUD`+
  cdmx$ocupacionLABORATORISTAS;cdmx$trab_salud<-na.tools::na.replace(cdmx$trab_salud,0)

cdmx$trab_salen_casa<-cdmx$ocupacionCHOFERES+cdmx$`ocupacionCOMERCIANTES DE MERCADOS FIJOS O AMBULANTES`+
  cdmx$ocupacionOBREROS+cdmx$ocupacionCAMPESINOS
cdmx$trab_salen_casa<-na.tools::na.replace(cdmx$trab_salen_casa,0)

cdmx$trab_no_salen_casa<-cdmx$ocupacionESTUDIANTES+cdmx$`ocupacionGERENTES O PROPIETARIOS DE EMPRESAS O NEGOCIOS`+cdmx$ocupacionHOGAR+cdmx$`ocupacionJUBILADO / PENSIONADO`+cdmx$ocupacionMAESTROS+cdmx$`ocupacionOTROS PROFESIONISTAS`+cdmx$ocupacionOTROS
cdmx$trab_no_salen_casa<-na.tools::na.replace(cdmx$trab_no_salen_casa,0)
cdmx$trab_mixto<-cdmx$ocupacionEMPLEADOS+cdmx$ocupacionDESEMPLEADOS
cdmx$trab_mixto<-na.tools::na.replace(cdmx$trab_mixto,0)
trabajo_cat<-NULL;trabajo_cat[cdmx$trab_salud==1]<-2;trabajo_cat[cdmx$trab_no_salen_casa==1]<-1;trabajo_cat[cdmx$trab_salen_casa==1]<-4;trabajo_cat[cdmx$trab_mixto==1]<-3

HOSP<-NULL;HOSP[cdmx$tipo_paciente=="HOSPITALIZADO"]<-1;HOSP<-na.tools::na.replace(HOSP,0)
cdmx<-cbind(cdmx,trabajo_cat)
d4<-dummies::dummy(cdmx$trabajo_cat)
colnames(d4)<-c("TRAB_NOSALES","HCW","MIXTOS","TRAB_SALEN")
cdmx<-cbind(cdmx, d4)
cdmx$trabajo_cat_2<-factor(cdmx$trabajo_cat,levels = c(1:4), labels = c("Professions prone to self-isolation",
                                                                        "Health-Care Workers",
                                                                        "Unspecified Professions",
                                                                        "Professions non-prone to self-isolation"))
cdmx$severe_covid19[cdmx$evolucion_casoDEFUNCION+cdmx$intubado+cdmx$unidad_cuidados_intensivos>0]<-1;
cdmx$severe_covid19[is.na(cdmx$severe_covid19)]<-0

cdmx$privado<-NULL;cdmx$privado[cdmx$sector=="PRIVADA"]<-1;
cdmx$privado<-na.tools::na.replace(cdmx$privado,0)

cdmx$mort_covid<-NULL
cdmx$mort_covid[cdmx$evolucion_casoDEFUNCION==1 & cdmx$covid==1]<-1
cdmx$mort_covid<-na.tools::na.replace(cdmx$mort_covid,0)

cdmx$ETI<-NULL;cdmx$ETI[cdmx$diagnostico_probable=="ENFERMEDAD TIPO INFLUENZA (ETI)"]<-1;cdmx$ETI<-na.tools::na.replace(cdmx$ETI,0)
cdmx$IRAG<-NULL;cdmx$IRAG[cdmx$diagnostico_probable=="INFECCION RESPIRATORIA AGUDA GRAVE (IRAG)"]<-1;cdmx$IRAG<-na.tools::na.replace(cdmx$IRAG,0)

#Work categories
cdmx$CAT_TRABAJO<-NULL;
cdmx$CAT_TRABAJO[cdmx$ocupacion=="CAMPESINOS"]<-"CAMPESINOS"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="CHOFERES"]<-"CHOFERES"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="COMERCIANTES DE MERCADOS FIJOS O AMBULANTES"]<-"COMERCIANTES"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="OBREROS"]<-"OBREROS"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="GERENTES O PROPIETARIOS DE EMPRESAS O NEGOCIOS"]<-"GERENTES_PROPIETARIOS"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="MAESTROS"]<-"MAESTROS"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="OTROS PROFESIONISTAS"]<-"OTROS_PROFESIONALES"

cdmx$CAT_TRABAJO[cdmx$ocupacion=="EMPLEADOS"]<-"EMPLEADOS"

cdmx$CAT_TRABAJO[cdmx$ocupacion=="OTROS"]<-"OTROS"

cdmx$CAT_TRABAJO[cdmx$ocupacion=="DENTISTAS"]<-"TRABAJADORES_SALUD"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="ENFERMERAS"]<-"TRABAJADORES_SALUD"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="LABORATORISTAS"]<-"TRABAJADORES_SALUD"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="MEDICOS"]<-"TRABAJADORES_SALUD"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="OTROS TRABAJADORES DE LA SALUD"]<-"TRABAJADORES_SALUD"

cdmx$CAT_TRABAJO[cdmx$ocupacion=="ESTUDIANTES"]<-"ESTUDIANTE"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="HOGAR"]<-"HOGAR"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="JUBILADO / PENSIONADO"]<-"JUBILADO / PENSIONADO"
cdmx$CAT_TRABAJO[cdmx$ocupacion=="DESEMPLEADOS"]<-"DESEMPLEADOS"

#INEGI Work-Categories dataset
trab<-read_excel("Bases de datos/profesion_cdmx.xlsx")
trab$CAT_TRABAJO_INEGI<-NULL
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="CAMPESINOS"]<-"OCUPADO"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="CHOFERES"]<-"OCUPADO"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="COMERCIANTES"]<-"OCUPADO"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="EMPLEADOS"]<-"OCUPADO"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="GERENTES_PROPIETARIOS"]<-"OCUPADO"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="MAESTROS"]<-"OCUPADO"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="OBREROS"]<-"OCUPADO"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="OTROS_PROFESIONALES"]<-"OCUPADO"

trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="HOGAR"]<-"HOGAR"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="OTROS"]<-"OTROS"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="DESEMPLEADOS"]<-"DESEMPLEADOS"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="TRABAJADORES_SALUD"]<-"TRABAJADORES_SALUD"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="ESTUDIANTE"]<-"ESTUDIANTE"
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="JUBILADO / PENSIONADO"]<-"JUBILADO / PENSIONADO"

trab$CAT_TRABAJO_COVID<-NULL
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="CAMPESINOS"]<-"NON_PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="CHOFERES"]<-"NON_PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="COMERCIANTES"]<-"NON_PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="OBREROS"]<-"NON_PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="EMPLEADOS"]<-"NON_PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="DESEMPLEADOS"]<-"NON_PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="OTROS_PROFESIONALES"]<-"NON_PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="OTROS"]<-"NON_PRONE_SELF"

trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="GERENTES_PROPIETARIOS"]<-"PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="MAESTROS"]<-"PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="JUBILADO / PENSIONADO"]<-"PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="ESTUDIANTE"]<-"PRONE_SELF"
trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="HOGAR"]<-"PRONE_SELF"

trab$CAT_TRABAJO_COVID[trab$CAT_TRABAJO=="TRABAJADORES_SALUD"]<-"HWC"
cdmx<-cdmx%>%left_join(trab, by = "CAT_TRABAJO")

#CONAPO dataset
cdmx$id<-paste0(str_pad(cdmx$cve_entidad_residencia, 2,pad = "0"),str_pad(cdmx$cve_municipio_residencia,3, pad="0"))
marg<-read_excel("Bases de datos/margindicadores.xlsx")
marg$Clave2<-as.character(marg$Clave2)
marg$id<-str_pad(marg$Clave2, 5,pad = "0")
marg2<-marg%>%dplyr::select(id,marg,marg_cat,analfabeta_15,no_escuela,ed_basica_inc,no_salud,piso_tierra,
                            no_escusado,no_agua,no_drenaje,no_luz,no_lavadora,no_refri)
cdmx<- cdmx %>%left_join(marg2, by = "id")
cdmx$marg<-as.numeric(cdmx$marg)

#CONAPO variable recodification
marg_rec<-NULL;marg_rec[cdmx$marg_cat=="Muy bajo"]<-1;
marg_rec[cdmx$marg_cat=="Bajo"]<-2;
marg_rec[cdmx$marg_cat=="Medio"]<-3;
marg_rec[cdmx$marg_cat=="Alto"]<-4
marg_rec[cdmx$marg_cat=="Muy alto"]<-4;
marg_rec[cdmx$marg_cat=="ND"]<-3
marg_rec_2<-NULL;marg_rec_2[cdmx$marg_cat=="Muy bajo"]<-0;
marg_rec_2[cdmx$marg_cat=="Bajo"]<-0;marg_rec_2[cdmx$marg_cat=="Medio"]<-1;marg_rec_2[cdmx$marg_cat=="Alto"]<-1
marg_rec_2[cdmx$marg_cat=="Muy alto"]<-1;marg_rec_2[cdmx$marg_cat=="ND"]<-0
cdmx<-cbind(cdmx,marg_rec,marg_rec_2)
d3<-dummies::dummy(cdmx$marg_rec)
colnames(d3)<-c("Very_Low_SLI","Low_SLI","Mid_SLI","High_SLI","NA_SLI")
cdmx<-cbind(cdmx,d3)

#Maps dataset
mx_mun <- st_read(dsn="shapes", layer="areas_geoestadisticas_municipales", stringsAsFactors=FALSE) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
bord <-  st_read(dsn="shapes", layer="areas_geoestadisticas_estatales")  %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% 
  rmapshaper::ms_innerlines()
zm <- st_read(dsn="shapes", layer="ZM_2010", stringsAsFactors=FALSE) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
mx_mun$id<-paste0(str_pad(mx_mun$CVE_ENT, 2,pad = "0"),str_pad(mx_mun$CVE_MUN,3, pad="0"))

#Populational Dataset
load("df_mx.rda")
df_mx$id<-str_pad(df_mx$CLAVE, 5,pad = "0")
pop<-df_mx %>% filter(AÑO %in% c("2020"), CLAVE_ENT==09)%>% 
  group_by(id,AÑO) %>%
  summarise(pop=sum(POB))
#Merge dataset
pop<-pop%>%left_join(marg2, by="id") #Pop1 Population by Municipality
pop$terciles_marg<-NULL;
pop$terciles_marg[pop$marg<=-1.49219]<-1
pop$terciles_marg[pop$marg>-1.49219 & pop$marg<=-1.42677]<-2
pop$terciles_marg[pop$marg>-1.42677]<-3
pop$terciles_marg<-as.factor(pop$terciles_marg)
pop$terciles_marg<-factor(pop$terciles_marg,levels = c(1:3), 
                          labels = c("Low-SLI","Moderate-SLI","High-SLI"))
pop1<-pop%>% group_by(terciles_marg,AÑO) %>%  summarise(pop2=sum(pop)) #Pop2 Population por SLI
pop<-pop%>%left_join(pop1, by=c("terciles_marg"="terciles_marg","AÑO"="AÑO"))%>%dplyr::select(id,pop,pop2)


###Merge all dataset###
covid_casos<- cdmx %>% left_join(pop, by="id") 
covid_casos$pop_3<-sum(pop$pop)

#Demografic data
demo<-read.csv("Bases de datos/dmu_camas.csv")
demo$id<-str_pad(demo$cve_m, 5,pad = "0")
covid_casos<- covid_casos %>% left_join(demo, by="id") 

###Epidemiological rates###
covid_casos_2<- covid_casos %>%
  group_by(id) %>%
  summarise(cases=sum(covid), tested=n(), deaths=sum(evolucion_casoDEFUNCION==1 & covid==1), pop=median(pop), marg=median(marg), asinto=sum(asintomaticos))%>%
  mutate(rate_incidence=cases/pop*100000, rate_mort=deaths/pop*100000, rate_letal=(deaths/cases)*100,rate_pruebas=(tested/pop*100000), rate_asinto=(asinto/pop*100000))%>%
  dplyr::select(id, rate_incidence, rate_mort, rate_letal,pop,marg,rate_pruebas,rate_asinto)%>%
  na.omit() #rate 1 es tasa de incidencia / rate 2 es tasa de mortalidad
mx_mun_2<-mx_mun %>%left_join(covid_casos_2, by="id")
###Filter all positive cases###
cdmx1<- cdmx %>% dplyr::filter(covid==1)
covid_casos$terciles_marg<-NULL;
covid_casos$terciles_marg[covid_casos$marg<=-1.49219]<-1
covid_casos$terciles_marg[covid_casos$marg>-1.49219 & covid_casos$marg<=-1.42677]<-2
covid_casos$terciles_marg[covid_casos$marg>-1.42677]<-3
covid_casos$terciles_marg<-as.factor(covid_casos$terciles_marg)
covid_casos$terciles_marg<-factor(covid_casos$terciles_marg,levels = c(1:3), 
                                  labels = c("Low-SLI","Moderate-SLI","High-SLI"))

covid_casos<-covid_casos%>%filter(id != "09106")%>%filter(id != "09NA")

covid_casos$CAT_TRABAJO_COVID_2<-NULL
covid_casos$CAT_TRABAJO_COVID_2[covid_casos$CAT_TRABAJO_INEGI=="DESEMPLEADOS"]<-7
covid_casos$CAT_TRABAJO_COVID_2[covid_casos$CAT_TRABAJO_INEGI=="JUBILADO / PENSIONADO"]<-6
covid_casos$CAT_TRABAJO_COVID_2[covid_casos$CAT_TRABAJO_INEGI=="HOGAR"]<-5
covid_casos$CAT_TRABAJO_COVID_2[covid_casos$CAT_TRABAJO_INEGI=="OTROS"]<-4
covid_casos$CAT_TRABAJO_COVID_2[covid_casos$CAT_TRABAJO_INEGI=="OCUPADO"]<-3
covid_casos$CAT_TRABAJO_COVID_2[covid_casos$CAT_TRABAJO_INEGI=="ESTUDIANTE"]<-2
covid_casos$CAT_TRABAJO_COVID_2[covid_casos$CAT_TRABAJO_INEGI=="TRABAJADORES_SALUD"]<-1
covid_casos$CAT_TRABAJO_COVID_2<-factor(covid_casos$CAT_TRABAJO_COVID_2, 
                                               labels = c("HCWs", "Students", 
                                                          "Economically-Active", "Other Non-Specified Workers",
                                                          "Home-Related Workers", "Retired Adults", "Unemployed"))

####Descriptive statistics####
nrow(covid_casos)

table(covid_casos$covid)
prop.table(table(covid_casos$covid))*100

table(covid_casos$asintomaticos==1 & covid_casos$covid==1)
prop.table(table(covid_casos$asintomaticos==1 & covid_casos$covid==1))*100

table(covid_casos$HOSP)
prop.table(table(covid_casos$HOSP))*100

table(covid_casos$intubado)
prop.table(table(covid_casos$intubado))*100

table(covid_casos$severe_covid19)
prop.table(table(covid_casos$severe_covid19))*100

table(covid_casos$evolucion_casoDEFUNCION)
prop.table(table(covid_casos$evolucion_casoDEFUNCION))*100

#Sx percentage
table(covid_casos$sx_percentage)
prop.table(table(covid_casos$sx_percentage))*100

#Working categories

table(covid_casos$CAT_TRABAJO_COVID_2)
prop.table(table(covid_casos$CAT_TRABAJO_COVID_2))*100

tapply(covid_casos$edad,covid_casos$CAT_TRABAJO_COVID_2, median)
tapply(covid_casos$comorb,covid_casos$CAT_TRABAJO_COVID_2, mean)
tapply(covid_casos$numero_sintomas,covid_casos$CAT_TRABAJO_COVID_2, mean)

#SLI trends

table(covid_casos$Cuadrante)
prop.table(table(covid_casos$Cuadrante))*100

tapply(covid_casos$edad,covid_casos$Cuadrante, median)
tapply(covid_casos$comorb,covid_casos$Cuadrante, mean)
tapply(covid_casos$numero_sintomas,covid_casos$Cuadrante, mean)


####Individual and structural inequalities and their impact in the COVID-19 pandemic (Figure 1)####
#Age standarized populational SLI analysis
#Upload datasets
Pob_conapo1<- read_xlsx("/Users/nefoantonio/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PROYECTOS/COVID19 - TRABAJADORES CDMX/base_municipios_final_datos_01.xlsx")
Pob_conapo2<- read_xlsx("/Users/nefoantonio/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PROYECTOS/COVID19 - TRABAJADORES CDMX/base_municipios_final_datos_02.xlsx")
Pob_conapo<- rbind(Pob_conapo1, Pob_conapo2)
Pob_conapo$CVE_MUN<-str_pad(Pob_conapo$CLAVE, 5,pad = "0")
demo_2<-demo%>%dplyr::select(id,dmu)
marg3<-marg%>%dplyr::select(id,marg)
names(marg3)[names(marg3) == "id"] <- "CVE_MUN"
marg4<-marg%>%dplyr::select(id,marg)
Pob_conapo<-Pob_conapo%>%left_join(marg3,by="CVE_MUN")
Pob_edad_pais<-Pob_conapo%>%filter(!is.na(marg))
Pob_edad_pais<- Pob_edad_pais %>% filter(`A—O` == 2020) %>% group_by(EDAD_QUIN) %>% tally(POB) %>% rename(POB = n)

#4 SLI cathegories

dmu_cuadrantes<-data.frame(id=9002:9017)
dmu_cuadrantes$id=str_pad(dmu_cuadrantes$id, 5,pad = "0")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(demo_2,by="id")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(marg4,by="id")
dmu_cuadrantes$DISLI<-lm(marg~dmu, data = dmu_cuadrantes)$residuals
dmu_cuadrantes<-dmu_cuadrantes%>%mutate(Cuadrante = factor(ifelse( (dmu<150 & DISLI < -0.103),
                                                                   "Low Density/Low DISLI",
                                                                   ifelse((dmu<150 & DISLI >= -0.103),
                                                                          "Low Density/High DISLI",
                                                                          ifelse((dmu>=150 & DISLI < -0.103),
                                                                                 "High Density/Low DISLI",
                                                                                 ifelse((dmu>=150 & DISLI>= -0.103),
                                                                                        "High Density/High DISLI", "Invalid"))))))

Pob_edad_cuadrante<- Pob_conapo %>% filter(`A—O` == 2020 & CLAVE_ENT == 9) %>% group_by(CVE_MUN, EDAD_QUIN) %>%
  tally(POB) %>% rename(POB = n) %>% 
  left_join(dmu_cuadrantes %>% dplyr::select(c("id","Cuadrante")), by =c("CVE_MUN"="id")) %>%
  group_by(Cuadrante, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))

#Dataset of 4 SLi cathegories 

cases_cuadrante_edad<- covid_casos %>% 
  left_join(dmu_cuadrantes %>% dplyr::select(id,Cuadrante), by = "id" )%>%
  mutate(week=data.table::month(fecha_ingreso),year=data.table::year(fecha_ingreso))%>%
  group_by(year, week, Cuadrante, EDAD_QUIN = cut(as.numeric(edad), 
                                            breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                            right = FALSE, include.lowest = TRUE, 
                                            labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                       "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                       "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                       "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases_def=sum(evolucion_casoDEFUNCION==1 & covid==1, na.rm=T), 
            tested=n(),
            cases=sum(covid, na.rm=T),
            asinto=sum(asintomaticos==1 & covid==1))

cuadrante_ajustada<- cases_cuadrante_edad%>%left_join(Pob_edad_cuadrante, by =c("Cuadrante","EDAD_QUIN"))
cuadrante_ajustada<-cuadrante_ajustada%>%left_join(Pob_edad_pais,by=c("EDAD_QUIN"))
cuadrante_ajustada1<- cuadrante_ajustada %>% mutate(INC_ESPER=(cases/POB.x)*POB.y,
                                                              TEST_ESPER=(tested/POB.x)*POB.y,
                                                              ASINTO_ESPER =(asinto/POB.x)*POB.y,
                                                              DEF_ESPER = (cases_def/POB.x)*POB.y)
cuadrante_ajustada2<- cuadrante_ajustada1 %>% 
  group_by(year, week, Cuadrante) %>% 
  summarise(INC_ESPER= sum(INC_ESPER), TEST_ESPER=sum(TEST_ESPER),
            ASINTO_ESPER=sum(ASINTO_ESPER),DEF_ESPER=sum(DEF_ESPER))%>%
  mutate(INC_AJUST = (INC_ESPER/9018645)*100000,
         TEST_AJUST = (TEST_ESPER/9018645)*100000,
         ASINTO_AJUST = (ASINTO_ESPER/9018645)*100000,
         DEF_AJUST = (DEF_ESPER/9018645)*100000)

cuadrante_ajustada2$week[cuadrante_ajustada2$week==1]<-13
cuadrante_ajustada2$week<-as.factor(cuadrante_ajustada2$week)
cuadrante_ajustada2<-cuadrante_ajustada2%>%
  ungroup()%>%
  group_by(Cuadrante)%>% 
  mutate(INC_ESPE_CUM=cumsum(INC_ESPER),
         TEST_ESPE_CUM=cumsum(TEST_ESPER),
         ASINTO_ESPE_CUM=cumsum(ASINTO_ESPER),
         DEF_ESPE_CUM=cumsum(DEF_ESPER))%>%
  mutate(INC_ACUM_AJUST = (INC_ESPE_CUM/9018645)*100000,
         TEST_ACUM_AJUST = (TEST_ESPE_CUM/9018645)*100000,
         ASINTO_ACUM_AJUST = (ASINTO_ESPE_CUM/9018645)*100000,
         DEF_ACUM_AJUST = (DEF_ESPE_CUM/9018645)*100000)%>%
  na.omit()

cuadrante_ajustada2$week<-factor(cuadrante_ajustada2$week, labels =  c("Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))


Fig1_E<-cuadrante_ajustada2%>% 
  ggplot(aes(x=week, y=INC_ACUM_AJUST, colour=Cuadrante,group=Cuadrante))+
  geom_line(alpha=1, size=1.5)+
  geom_point(alpha=1, size=2.5)+
  theme_hc()+
  ylab("Age-Adjusted Cummulative Incidence Rate
(Rate per 100,000 habitants)")+
  xlab("Month")+
  labs(col="Mean Urban Population Density\n&\nDensity-Independent SLI Categories")+
  scale_colour_viridis_d(option = "viridis")+
  theme(axis.text.x = element_text(angle = 45,size = 10, hjust = 1),
        axis.text.y = element_text(size = 10, hjust = 1))+
  guides(colour=guide_legend(nrow=2,byrow=T))

Fig1_F<-cuadrante_ajustada2%>% 
  ggplot(aes(x=week, y=TEST_ACUM_AJUST, colour=Cuadrante,group=Cuadrante))+
  geom_line(alpha=1, size=1.5)+
  geom_point(alpha=1, size=2.5)+
  theme_hc()+
  ylab("Age-Adjusted Cummulative Testing Rate
(Rate per 100,000 habitants)")+
  xlab("Month")+
  labs(col="Mean Urban Population Density\n&\nDensity-Independent SLI Categories")+
  scale_colour_viridis_d(option = "viridis")+
  theme(axis.text.x = element_text(angle = 45,size = 10, hjust = 1),
        axis.text.y = element_text(size = 10, hjust = 1))+
  guides(colour=guide_legend(nrow=2,byrow=T))

Fig1_G<-cuadrante_ajustada2%>% 
  ggplot(aes(x=week, y=ASINTO_ACUM_AJUST, colour=Cuadrante,group=Cuadrante))+
  geom_line(alpha=1, size=1.5)+
  geom_point(alpha=1, size=2.5)+
  theme_hc()+
  ylab("Age-Adjusted Cummulative Asymptomatic Rate
(Rate per 100,000 habitants)")+
  xlab("Month")+
  labs(col="Mean Urban Population Density\n&\nDensity-Independent SLI Categories")+
  scale_colour_viridis_d(option = "viridis")+
  theme(axis.text.x = element_text(angle = 45,size = 10, hjust = 1),
        axis.text.y = element_text(size = 10, hjust = 1))+
  guides(colour=guide_legend(nrow=2,byrow=T))

Fig1_H<-cuadrante_ajustada2%>% 
  ggplot(aes(x=week, y=DEF_ACUM_AJUST, colour=Cuadrante,group=Cuadrante))+
  geom_line(alpha=1, size=1.5)+
  geom_point(alpha=1, size=2.5)+
  theme_hc()+
  ylab("Age-Adjusted Cummulative Mortality Rate
(Rate per 100,000 habitants)")+
  xlab("Month")+
  labs(col="Mean Urban Population Density\n&\nDensity-Independent SLI Categories")+
  scale_colour_viridis_d(option = "viridis")+
  theme(axis.text.x = element_text(angle = 45,size = 10, hjust = 1),
        axis.text.y = element_text(size = 10, hjust = 1))+
  guides(colour=guide_legend(nrow=2,byrow=T))

#MAPS
Pob_edad_id<- Pob_conapo %>% filter(`A—O` == 2020 & CLAVE_ENT == 9) %>% group_by(CVE_MUN, EDAD_QUIN) %>%
  tally(POB) %>% rename(POB = n)  %>% rename(id = CVE_MUN) 

Defunciones_mun_edad<- covid_casos %>% 
  group_by(id, EDAD_QUIN = cut(as.numeric(edad), 
                               breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                               right = FALSE, include.lowest = TRUE, 
                               labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                          "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                          "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                          "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases_def=sum(evolucion_casoDEFUNCION==1 & covid==1, na.rm=T), 
            tested=n(),
            cases=sum(covid, na.rm=T),
            asinto=sum(asintomaticos==1 & covid==1))

mun_mort_ajustada<- Defunciones_mun_edad%>%left_join(Pob_edad_id, by =c("id","EDAD_QUIN"))
mun_mort_ajustada<-mun_mort_ajustada%>%left_join(Pob_edad_pais,by=c("EDAD_QUIN"))
mun_cent <- st_centroid(mx_mun)

mun_mort_ajustada1<- mun_mort_ajustada %>% mutate(INC_ESPER=(cases/POB.x)*POB.y,
                                                  TEST_ESPER=(tested/POB.x)*POB.y,
                                                  ASINTO_ESPER =(asinto/POB.x)*POB.y,
                                                  DEF_ESPER = (cases_def/POB.x)*POB.y)
mun_mort_ajustada2<- mun_mort_ajustada1 %>% group_by(id) %>% 
  summarise(INC_ESPER= sum(INC_ESPER), TEST_ESPER=sum(TEST_ESPER),
            ASINTO_ESPER=sum(ASINTO_ESPER),DEF_ESPER=sum(DEF_ESPER))%>%
  group_by(id)%>% 
  mutate(INC_AJUST = (INC_ESPER/9018645)*100000,
         TEST_AJUST = (TEST_ESPER/9018645)*100000,
         ASINTO_AJUST = (ASINTO_ESPER/9018645)*100000,
         DEF_AJUST = (DEF_ESPER/9018645)*100000)%>%
  left_join(mun_cent,by="id")


Residuals_incidence_map<- mx_mun %>% filter(CVE_ENT == "09") %>% dplyr::select(id,geometry) %>%
  left_join(dmu_cuadrantes, by = "id" )%>%
  ggplot() + 
  geom_sf(aes(fill = Cuadrante,geometry=geometry)) + 
  geom_sf(data = mun_mort_ajustada2, aes(size = INC_AJUST,geometry=geometry),show.legend = "point", inherit.aes = F, color= "red") +
  scale_size(range = c(0.1,10)) +
  theme_map()+
  scale_fill_viridis_d(option = "viridis") +
  labs(fill = "Mean Urban Population Density & Density-Independent SLI Categories",
       size = "Age-adjusted COVID Incidence Rate\nper 100,000 inhabitants") +
  theme(plot.title = element_text(hjust=0.1),
        legend.title = element_text(hjust = 0.1),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),size=guide_legend(nrow=2,byrow=T))

Residuals_testing_map<- mx_mun %>% filter(CVE_ENT == "09") %>% dplyr::select(id,geometry) %>%
  left_join(dmu_cuadrantes, by = "id" )%>%
  ggplot() + 
  geom_sf(aes(fill = Cuadrante,geometry=geometry)) + 
  geom_sf(data = mun_mort_ajustada2, aes(size = TEST_AJUST,geometry=geometry),show.legend = "point", inherit.aes = F, color= "red") +
  scale_size(range = c(0.1,10)) +
  theme_map()+
  scale_fill_viridis_d(option = "viridis") +
  labs(fill = "Mean Urban Population Density & Density-Independent SLI Categories",
       size = "Age-adjusted COVID Testing Rate\nper 100,000 inhabitants") +
  theme(plot.title = element_text(hjust=0.1),
        legend.title = element_text(hjust = 0.1),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),size=guide_legend(nrow=2,byrow=T))


Residuals_asinto_map<- mx_mun %>% filter(CVE_ENT == "09") %>% dplyr::select(id,geometry) %>%
  left_join(dmu_cuadrantes, by = "id" )%>%
  ggplot() + 
  geom_sf(aes(fill = Cuadrante,geometry=geometry)) + 
  geom_sf(data = mun_mort_ajustada2, aes(size = ASINTO_AJUST,geometry=geometry),show.legend = "point", inherit.aes = F, color= "red") +
  scale_size(range = c(0.1,10)) +
  theme_map()+
  scale_fill_viridis_d(option = "viridis") +
  labs(fill = "Mean Urban Population Density & Density-Independent SLI Categories",
       size = "Age-adjusted COVID Asymptomatic Rate\nper 100,000 inhabitants") +
  theme(plot.title = element_text(hjust=0.1),
        legend.title = element_text(hjust = 0.1),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),size=guide_legend(nrow=2,byrow=T))


Residuals_mort_map<- mx_mun %>% filter(CVE_ENT == "09") %>% dplyr::select(id,geometry) %>%
  left_join(dmu_cuadrantes, by = "id" )%>%
  ggplot() + 
  geom_sf(aes(fill = Cuadrante,geometry=geometry)) + 
  geom_sf(data = mun_mort_ajustada2, aes(size = DEF_AJUST,geometry=geometry),show.legend = "point", inherit.aes = F, color= "red") +
  scale_size(range = c(0.1,10)) +
  theme_map()+
  scale_fill_viridis_d(option = "viridis") +
  labs(fill = "Mean Urban Population Density & Density-Independent SLI Categories",
       size = "Age-adjusted COVID Mortality Rate\nper 100,000 inhabitants") +
  theme(plot.title = element_text(hjust=0.1),
        legend.title = element_text(hjust = 0.1),
        legend.position = "bottom")+
  guides(size=guide_legend(nrow=2,byrow=T),fill = FALSE)

Figure1_Left<-ggarrange(Fig1_E,Fig1_F,Fig1_G, Fig1_H, ncol = 2, nrow = 2, labels = c("A","B","C","D"),common.legend = T)
Figure1_Right<-ggarrange(Residuals_mort_map, ncol = 1, nrow = 1, labels = c("E"),common.legend = T)
Figure1<-ggarrange(Figure1_Left,Figure1_Right, ncol = 2, nrow = 1,common.legend = F)

ggsave(Figure1,
       filename = "Figure_1.1.png", 
       width = 45, 
       height = 25,
       units=c("cm"),
       dpi = 400,
       limitsize = FALSE) 

####Independent role of individual and structural factors on COVID-19 outcomes (Figure 2)####-----
#Cox Regression Models

covid_casos_15<-covid_casos%>% filter(covid==1) %>% filter(edad>=15)
covid_casos_15$CAT_TRABAJO_COVID_3<-NULL
covid_casos_15$CAT_TRABAJO_COVID_3[covid_casos_15$CAT_TRABAJO_INEGI=="DESEMPLEADOS"]<-7
covid_casos_15$CAT_TRABAJO_COVID_3[covid_casos_15$CAT_TRABAJO_INEGI=="JUBILADO / PENSIONADO"]<-6
covid_casos_15$CAT_TRABAJO_COVID_3[covid_casos_15$CAT_TRABAJO_INEGI=="HOGAR"]<-5
covid_casos_15$CAT_TRABAJO_COVID_3[covid_casos_15$CAT_TRABAJO_INEGI=="OTROS"]<-4
covid_casos_15$CAT_TRABAJO_COVID_3[covid_casos_15$CAT_TRABAJO_INEGI=="OCUPADO"]<-3
covid_casos_15$CAT_TRABAJO_COVID_3[covid_casos_15$CAT_TRABAJO_INEGI=="ESTUDIANTE"]<-2
covid_casos_15$CAT_TRABAJO_COVID_3[covid_casos_15$CAT_TRABAJO_INEGI=="TRABAJADORES_SALUD"]<-1
covid_casos_15$CAT_TRABAJO_COVID_3<-factor(covid_casos_15$CAT_TRABAJO_COVID_3, 
                                           labels = c("HCWs", "Students", 
                                                      "Economically-Active", "Other Non-Specified Workers",
                                                      "Home-Related Workers", "Retired Adults", "Unemployed"))

covid_casos_15$DISLI<-lm(marg~dmu, data = covid_casos_15)$residuals
covid_casos_15$Cuadrantes<-NULL;
covid_casos_15$Cuadrantes[covid_casos_15$dmu<150 & covid_casos_15$DISLI<(-0.01482268)]<-1
covid_casos_15$Cuadrantes[covid_casos_15$dmu>=150 & covid_casos_15$DISLI<(-0.01482268)]<-2
covid_casos_15$Cuadrantes[covid_casos_15$dmu<150 & covid_casos_15$DISLI>=(-0.01482268)]<-3
covid_casos_15$Cuadrantes[covid_casos_15$dmu>=150 & covid_casos_15$DISLI>=(-0.01482268)]<-4
covid_casos_15$Cuadrantes<-factor(covid_casos_15$Cuadrantes)


#Hospitalizacion

m1<-coxph(Surv(FU_time_HOSP, HOSP)~scale(edad)+(sexo=="MASCULINO")+(privado==0)+scale(comorb)+scale(numero_sintomas)+factor(Cuadrantes)+factor(CAT_TRABAJO_COVID_3)+frailty(clave_localidad_residencia), data=covid_casos_15)
summary(m1);
exp(confint(m1))
HR<-as.data.frame(cbind(exp(coef(m1)),exp(confint(m1))))
HR$Covariate<-c("Age","Male Sex", "Public Health-Care Setting", 
                "Number of Comorbidities", "Number of Symptoms",
                "High Density/Low DISLI","Low Density/High DISLI","High Density/High DISLI",
                "Students", 
                "Economically-Active", "Other Non-Specified Workers",
                "Home-Related Workers", "Retired Adults", "Unemployed")
colnames(HR)<-c("HR", "ciLow", "ciHigh", "Covariate")
HR$breaks<-seq(1:nrow(HR))
Figure2_A<- ggplot(HR, aes(x = HR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 1, color = "#dc8945") +
  geom_point(size = 3, color = "white") +
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = HR$breaks, labels = HR$Covariate) +
  ylab("") +
  xlab("Risk for Hospitalization in Confirmed Cases with SARS-CoV-2, Hazard ratio (HR, 95%CI)")+scale_x_log10(limits = c(0.1,4))+
  theme(text = element_text(size=10))

#Intubacion

m2<-glm(intubado~scale(edad)+(sexo=="MASCULINO")+(privado==0)+scale(comorb)+scale(numero_sintomas)+factor(Cuadrantes)+factor(CAT_TRABAJO_COVID_3)+frailty(clave_localidad_residencia), family = "binomial", data=covid_casos_15)
summary(m2)
summ(m2,exp = T,confint = T)
HR<-as.data.frame(cbind(exp(coef(m2)),exp(confint(m2))))
HR<-HR[c(-1,-16),]
HR$Covariate<-c("Age","Male Sex", "Public Health-Care Setting", 
                "Number of Comorbidities", "Number of Symptoms",
                "High Density/Low DISLI","Low Density/High DISLI","High Density/High DISLI",
                "Students", 
                "Economically-Active", "Other Non-Specified Workers",
                "Home-Related Workers", "Retired Adults", "Unemployed")
colnames(HR)<-c("HR", "ciLow", "ciHigh", "Covariate")
HR$breaks<-seq(1:nrow(HR))
Figure2_B<- ggplot(HR, aes(x = HR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 1, color = "#dc8945") +
  geom_point(size = 3, color = "white") +
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = HR$breaks, labels = HR$Covariate) +
  ylab("") +
  xlab("IMV in Workers in Confirmed Cases with SARS-CoV-2, Odds Ratio (OR, 95%CI)")+scale_x_log10(limits = c(0.1,4))+
  theme(text = element_text(size=10))


#Covid GRAVE

m3<-coxph(Surv(FU_time_MORT, severe_covid19)~scale(edad)+(sexo=="MASCULINO")+(privado==0)+scale(comorb)+scale(numero_sintomas)+factor(Cuadrantes)+factor(CAT_TRABAJO_COVID_3)+frailty(clave_localidad_residencia), data=covid_casos_15)
summary(m3)
exp(confint(m3))
HR<-as.data.frame(cbind(exp(coef(m3)),exp(confint(m3))))
HR$Covariate<-c("Age","Male Sex", "Public Health-Care Setting", 
                "Number of Comorbidities", "Number of Symptoms",
                "High Density/Low DISLI","Low Density/High DISLI","High Density/High DISLI",
                "Students", 
                "Economically-Active", "Other Non-Specified Workers",
                "Home-Related Workers", "Retired Adults", "Unemployed")
colnames(HR)<-c("HR", "ciLow", "ciHigh", "Covariate")
HR$breaks<-seq(1:nrow(HR))
Figure2_C<- ggplot(HR, aes(x = HR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 1, color = "#dc8945") +
  geom_point(size = 3, color = "white") +
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = HR$breaks, labels = HR$Covariate) +
  ylab("") +
  xlab("Severe Outcome in Confirmed Cases with SARS-CoV-2, Hazard ratio (HR, 95%CI)")+scale_x_log10(limits = c(0.1,4))+
  theme(text = element_text(size=10))


#Mortalidad
covid_casos_15$mort_covid<-na.tools::na.replace(covid_casos_15$mort_covid,0)
m4<-coxph(Surv(FU_time_MORT, mort_covid)~scale(edad)+(sexo=="MASCULINO")+(privado==0)+scale(comorb)+scale(numero_sintomas)+factor(Cuadrantes)+factor(CAT_TRABAJO_COVID_3)+
            frailty(clave_localidad_residencia), data=covid_casos_15)
summary(m4)
exp(confint(m4))
HR<-as.data.frame(cbind(exp(coef(m4)),exp(confint(m4))))
HR$Covariate<-c("Age","Male Sex", "Public Health-Care Setting", 
                "Number of Comorbidities", "Number of Symptoms",
                "High Density/Low DISLI","Low Density/High DISLI","High Density/High DISLI",
                "Students", 
                "Economically-Active", "Other Non-Specified Workers",
                "Home-Related Workers", "Retired Adults", "Unemployed")
colnames(HR)<-c("HR", "ciLow", "ciHigh", "Covariate")
HR$breaks<-seq(1:nrow(HR))
Figure2_D<- ggplot(HR, aes(x = HR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 1, color = "#dc8945") +
  geom_point(size = 3, color = "white") +
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = HR$breaks, labels = HR$Covariate) +
  ylab("") +
  xlab("Risk for Lethality in Confirmed Cases with SARS-CoV-2, Hazard ratio (HR, 95%CI)")+scale_x_log10(limits = c(0.1,4))+
  theme(text = element_text(size=10))

#Armado de la figura 4

Figure_2<-ggarrange(Figure2_A,Figure2_B,Figure2_C,Figure2_D,
                       ncol=2,nrow = 2 ,labels=LETTERS[1:4])

ggsave(Figure_2,
       filename = "Figure_2.png", 
       width = 40, 
       height = 20,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 

####Unequal distribution of excess mortality during the COVID-19 pandemic in Mexico City (Figure 4)#####
####Excess Mortality Datasets###
#Load dataset#
excess<-read.csv("Bases de datos/exceso_mortalidad.csv")
excess<-excess%>%dplyr::filter(alcaldia!="")
excess$fec_defuncion<-as.character(excess$fec_defuncion)
excess$fec_defuncion<-as.Date(excess$fec_defuncion)

#Populational dataset
marg<-read_excel("Bases de datos/margindicadores.xlsx")
dmu<-read.csv("Bases de datos/dmu_camas.csv")
dmu$id<-paste0(str_pad(dmu$CVE_ENT, 2,pad = "0"),str_pad(dmu$CVE_MUN,3, pad="0"))
marg$Clave2<-as.character(marg$Clave2)
marg$id<-str_pad(marg$Clave2, 5,pad = "0")
id2<-levels(as.factor(marg$id))
excess$id<-factor(excess$alcaldia, labels = id2)
excess<-excess%>%mutate(AÑO=data.table::year(fec_defuncion),
                        MES=data.table::month(fec_defuncion))
#SLI dataset
marg2<-marg%>%dplyr::select(id,marg,marg_cat)
marg2$terciles_marg<-NULL
marg2$terciles_marg[marg2$marg<=-1.49219]<-1
marg2$terciles_marg[marg2$marg>-1.49219 & marg2$marg<=-1.42677]<-2
marg2$terciles_marg[marg2$marg>-1.42677]<-3
#Demographic Dataset
load("df_mx.rda")
df_mx$id<-str_pad(df_mx$CLAVE, 5,pad = "0")
pop<-df_mx %>% filter(AÑO %in% c("2017","2018","2019", "2020","2021"), CLAVE_ENT==09)%>% 
  group_by(id,AÑO) %>%
  summarise(pop=sum(POB))
#Merge dataset with excess mortality 
pop<-pop%>%left_join(marg2, by="id") #Pop1 Population by Municipality
pop1<-pop%>% group_by(terciles_marg,AÑO) %>%  summarise(pop2=sum(pop)) #Pop2 Population por SLI
pop<-pop%>%left_join(pop1, by=c("terciles_marg"="terciles_marg","AÑO"="AÑO"))
dmu1<-dmu %>% dplyr::select(id, dmu)
#Final Dataset
excess2<- excess %>%left_join(pop, by = c("id","AÑO"))
excess2<-excess2 %>% left_join(dmu1, by="id")
excess2$pop3_2021<-sum(pop[pop$AÑO=="2021",]$pop)
excess2$pop3_2020<-sum(pop[pop$AÑO=="2020",]$pop)
excess2$pop3_2019<-sum(pop[pop$AÑO=="2019",]$pop)
excess2$pop3_2018<-sum(pop[pop$AÑO=="2018",]$pop)
excess2$pop3_2017<-sum(pop[pop$AÑO=="2017",]$pop)

#Age Adjusted Mortality rate 
#Population datasets
Pob_conapo1<- read_xlsx("/Users/nefoantonio/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PROYECTOS/COVID19 - TRABAJADORES CDMX/base_municipios_final_datos_01.xlsx")
Pob_conapo2<- read_xlsx("/Users/nefoantonio/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PROYECTOS/COVID19 - TRABAJADORES CDMX/base_municipios_final_datos_02.xlsx")
Pob_conapo<- rbind(Pob_conapo1, Pob_conapo2)
Pob_conapo$CVE_MUN<-str_pad(Pob_conapo$CLAVE, 5,pad = "0")
demo_2<-demo%>%dplyr::select(id,dmu)
marg3<-marg%>%dplyr::select(id,marg)
names(marg3)[names(marg3) == "id"] <- "CVE_MUN"
marg4<-marg%>%dplyr::select(id,marg)
excess2$CVE_MUN<-as.character(excess2$id)


#DISLI df
dmu_cuadrantes<-data.frame(id=9002:9017)
dmu_cuadrantes$id=str_pad(dmu_cuadrantes$id, 5,pad = "0")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(demo_2,by="id")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(marg4,by="id")
dmu_cuadrantes$DISLI<-lm(marg~dmu, data = dmu_cuadrantes)$residuals
dmu_cuadrantes<-dmu_cuadrantes%>%mutate(Cuadrante = factor(ifelse( (dmu<150 & DISLI < -0.103),
                                                                   "Low Density/Low DISLI",
                                                                   ifelse((dmu<150 & DISLI >= -0.103),
                                                                          "Low Density/High DISLI",
                                                                          ifelse((dmu>=150 & DISLI < -0.103),
                                                                                 "High Density/Low DISLI",
                                                                                 ifelse((dmu>=150 & DISLI>= -0.103),
                                                                                        "High Density/High DISLI", "Invalid"))))))


#Populational DISLI arrangement
Pob_conapo<-Pob_conapo%>%left_join(marg3,by="CVE_MUN")
Pob_edad_pais<-Pob_conapo%>%filter(!is.na(marg))
Pob_edad_cuadrante<- Pob_conapo %>% filter(`A—O` == 2020 & CLAVE_ENT == 9) %>% group_by(CVE_MUN, EDAD_QUIN) %>%
  tally(POB) %>% rename(POB = n) %>% 
  left_join(dmu_cuadrantes %>% dplyr::select(c("id","Cuadrante")), by =c("CVE_MUN"="id")) %>%
  group_by(Cuadrante, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))
Pob_edad_pais_cuadrante<- Pob_edad_pais %>% filter(`A—O` == 2020) %>% group_by(EDAD_QUIN) %>% tally(POB) %>% rename(POB = n)





###Underregistry deaths###
#COVID death registry merging
COVID_excess_cuadrante_edad<- excess2 %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2020-03-01") %>%
  filter(fec_defuncion<="2021-01-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(year,week, Cuadrante, EDAD_QUIN = cut(as.numeric(edad), 
                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                 right = FALSE, include.lowest = TRUE, 
                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa!="Otra"))%>%
  na.omit()

cuadrante_COVID_mort_ajustada<- COVID_excess_cuadrante_edad%>%left_join(Pob_edad_cuadrante, by =c("Cuadrante","EDAD_QUIN"))
cuadrante_COVID_mort_ajustada<-cuadrante_COVID_mort_ajustada%>%left_join(Pob_edad_pais_cuadrante,by=c("EDAD_QUIN"))

#Age adjusted dataset in death certificates
mun_mort_ajustada1<- cuadrante_COVID_mort_ajustada %>% mutate(DEF_ESPER = (cases/POB.x)*POB.y)
mun_mort_ajustada2<- mun_mort_ajustada1 %>% group_by(Cuadrante, week) %>% 
  summarise(DEF_ESPER=sum(DEF_ESPER,na.rm = T))%>%
  group_by(Cuadrante, week)%>% 
  mutate(DEF_AJUST = (DEF_ESPER/9018645)*100000)

mun_mort_ajustada2$week[mun_mort_ajustada2$week==1]<-13
mun_mort_ajustada2$week<-as.factor(mun_mort_ajustada2$week)
mun_mort_ajustada2$week<-factor(mun_mort_ajustada2$week, labels =  c("Mar-20'", "Apl-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))


#Deaths in COVID-19 CDMX dataset

covid_casos_mort_obs<- covid_casos %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fecha_defuncion>="2020-03-01" & fecha_defuncion<="2021-01-31") %>%
  mutate(week=data.table::month(fecha_defuncion),year=data.table::year(fecha_defuncion))%>%
  group_by(year,week, Cuadrante, EDAD_QUIN = cut(as.numeric(edad), 
                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                 right = FALSE, include.lowest = TRUE, 
                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases_obs=sum(evolucion_casoDEFUNCION))%>%
  na.omit()

cuadrante_COVID_mort_ajustada_obs<- covid_casos_mort_obs%>%left_join(Pob_edad_cuadrante, by =c("Cuadrante","EDAD_QUIN"))
cuadrante_COVID_mort_ajustada_obs<-cuadrante_COVID_mort_ajustada_obs%>%left_join(Pob_edad_pais_cuadrante,by=c("EDAD_QUIN"))

#Aga adjusted dataset in observed cases CDMX
mun_mort_ajustada1_obs<- cuadrante_COVID_mort_ajustada_obs %>% mutate(DEF_ESPER_obs = (cases_obs/POB.x)*POB.y)
mun_mort_ajustada2_obs<- mun_mort_ajustada1_obs %>% group_by(Cuadrante, week) %>% 
  summarise(DEF_ESPER_obs=sum(DEF_ESPER_obs,na.rm = T))%>%
  group_by(Cuadrante, week)%>% 
  mutate(DEF_AJUST_obs = (DEF_ESPER_obs/9018645)*100000)

mun_mort_ajustada2_obs$week[mun_mort_ajustada2_obs$week==1]<-13
mun_mort_ajustada2_obs$week<-as.factor(mun_mort_ajustada2_obs$week)
mun_mort_ajustada2_obs$week<-factor(mun_mort_ajustada2_obs$week, labels =  c("Mar-20'", "Apl-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))
COVID_UNDER_REPORT<-mun_mort_ajustada2%>%left_join(mun_mort_ajustada2_obs, by=c("Cuadrante","week"))%>%mutate(under_report=(DEF_AJUST-DEF_AJUST_obs)) #Final dataset

###Underegistry by###
###Ambularatory/hospitalarian deaths###
#Recoding ambulatory deaths
excess2$LugarMuerte_3<-excess2$LugarMuerte
excess2$lugar_muerte<-na.tools::na.replace(excess2$LugarMuerte_3, "Domicilio")
excess2$lugar_muerte_2<-excess2$lugar_muerte
excess2$lugar_muerte_2<-factor(excess2$lugar_muerte_2, labels = c("Ambulatory", "Hospital"))

covid_casos$lugar_muerte_2<-covid_casos$tipo_paciente
covid_casos$lugar_muerte_2<-factor(covid_casos$lugar_muerte_2, labels = c("Ambulatory", "Hospital"))


#COVID death registry merging

COVID_excess_cuadrante_edad_ambu<- excess2 %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2020-03-01") %>%
  filter(fec_defuncion<="2020-12-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(year,week, Cuadrante, lugar_muerte_2,EDAD_QUIN = cut(as.numeric(edad), 
                                                                breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                                right = FALSE, include.lowest = TRUE, 
                                                                labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                                           "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                                           "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                                           "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa!="Otra"))%>%
  na.omit()

cuadrante_COVID_mort_ajustada_ambu<- COVID_excess_cuadrante_edad_ambu%>%left_join(Pob_edad_cuadrante, by =c("Cuadrante","EDAD_QUIN"))
cuadrante_COVID_mort_ajustada_ambu<-cuadrante_COVID_mort_ajustada_ambu%>%left_join(Pob_edad_pais_cuadrante,by=c("EDAD_QUIN"))

#Age adjusted dataset in death certificates

mun_mort_ajustada1_ambu<- cuadrante_COVID_mort_ajustada_ambu %>% mutate(DEF_ESPER = (cases/POB.x)*POB.y)
mun_mort_ajustada2_ambu<- mun_mort_ajustada1_ambu %>% group_by(Cuadrante, week,lugar_muerte_2) %>% 
  summarise(DEF_ESPER=sum(DEF_ESPER,na.rm = T))%>%
  group_by(Cuadrante, week,lugar_muerte_2)%>% 
  mutate(DEF_AJUST = (DEF_ESPER/9018645)*100000)

mun_mort_ajustada2_ambu$week<-as.factor(mun_mort_ajustada2_ambu$week)
mun_mort_ajustada2_ambu$week<-factor(mun_mort_ajustada2_ambu$week, labels =  c("Mar-20'", "Apl-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'"))

#Deaths in COVID-19 CDMX dataset
covid_casos_mort_obs_amb<- covid_casos %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fecha_defuncion>="2020-03-01" & fecha_defuncion<="2020-12-31") %>%
  mutate(week=data.table::month(fecha_defuncion),year=data.table::year(fecha_defuncion))%>%
  group_by(year,week, Cuadrante,lugar_muerte_2, EDAD_QUIN = cut(as.numeric(edad), 
                                                                breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                                right = FALSE, include.lowest = TRUE, 
                                                                labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                                           "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                                           "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                                           "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases_obs=sum(evolucion_casoDEFUNCION))%>%
  na.omit()

cuadrante_COVID_mort_ajustada_obs_amb<- covid_casos_mort_obs_amb%>%left_join(Pob_edad_cuadrante, by =c("Cuadrante","EDAD_QUIN"))
cuadrante_COVID_mort_ajustada_obs_amb<-cuadrante_COVID_mort_ajustada_obs_amb%>%left_join(Pob_edad_pais_cuadrante,by=c("EDAD_QUIN"))

#Aga adjusted dataset in observed cases CDMX
mun_mort_ajustada1_obs_amb<- cuadrante_COVID_mort_ajustada_obs_amb %>% mutate(DEF_ESPER_obs = (cases_obs/POB.x)*POB.y)
mun_mort_ajustada2_obs_amb<- mun_mort_ajustada1_obs_amb %>% group_by(Cuadrante, week,lugar_muerte_2) %>% 
  summarise(DEF_ESPER_obs=sum(DEF_ESPER_obs,na.rm = T))%>%
  group_by(Cuadrante, week,lugar_muerte_2)%>% 
  mutate(DEF_AJUST_obs = (DEF_ESPER_obs/9018645)*100000)

mun_mort_ajustada2_obs_amb$week<-as.factor(mun_mort_ajustada2_obs_amb$week)
mun_mort_ajustada2_obs_amb$week<-factor(mun_mort_ajustada2_obs_amb$week, labels =  c("Mar-20'", "Apl-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'"))
COVID_UNDER_REPORT_amb<-mun_mort_ajustada2_ambu%>%left_join(mun_mort_ajustada2_obs_amb, by=c("Cuadrante","week","lugar_muerte_2"))%>%mutate(under_report=(DEF_AJUST-DEF_AJUST_obs)) #Final dataset

###Age adjusted NON-COVID deaths by setting
#Hospital or ambulatory setting

excess2$LugarMuerte_3<-excess2$LugarMuerte
excess2$lugar_muerte<-na.tools::na.replace(excess2$LugarMuerte_3, "Non-Specified")
excess2$lugar_muerte_2<-excess2$lugar_muerte
excess2$lugar_muerte_2<-factor(excess2$lugar_muerte_2, labels = c("Ambulatory", "Hospital","Non-Specified"))

nonCOVID_excess_cuadrante_edad_2020_amb<- excess2 %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2020-01-01") %>%
  filter(fec_defuncion<="2021-01-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(year,week, Cuadrante, lugar_muerte_2, EDAD_QUIN = cut(as.numeric(edad), 
                                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                                 right = FALSE, include.lowest = TRUE, 
                                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa=="Otra"))%>%
  na.omit()


cuadrante_nonCOVID_mort_ajustada_2020_amb<- nonCOVID_excess_cuadrante_edad_2020_amb%>%left_join(Pob_edad_cuadrante, by =c("Cuadrante","EDAD_QUIN"))
cuadrante_nonCOVID_mort_ajustada_2020_amb<-cuadrante_nonCOVID_mort_ajustada_2020_amb%>%left_join(Pob_edad_pais_cuadrante,by=c("EDAD_QUIN"))

#Age adjusted dataset in death certificates
non_covid_mun_mort_ajustada1_amb<- cuadrante_nonCOVID_mort_ajustada_2020_amb %>% mutate(DEF_ESPER = (cases/POB.x)*POB.y)
non_covid_mun_mort_ajustada2_amb<- non_covid_mun_mort_ajustada1_amb %>% group_by(Cuadrante, year,week,lugar_muerte_2) %>% 
  summarise(DEF_ESPER=sum(DEF_ESPER,na.rm = T))%>%
  group_by(Cuadrante, year,week,lugar_muerte_2)%>% 
  summarise(DEF_AJUST_2020 = (DEF_ESPER/9018645)*100000)

#Final dataset

non_covid_mun_mort_ajustada2_amb$week<-as.factor(non_covid_mun_mort_ajustada2_amb$week)
non_covid_mun_mort_ajustada2_amb$week[non_covid_mun_mort_ajustada2_amb$year==2021 & non_covid_mun_mort_ajustada2_amb$week==1]<-13
non_covid_mun_mort_ajustada2_amb$week<-na.tools::na.replace(non_covid_mun_mort_ajustada2_amb$week,13)
non_covid_mun_mort_ajustada2_amb$week<-factor(non_covid_mun_mort_ajustada2_amb$week, labels =  c("Jan-20'","Feb-20'","Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'", "Jan-21'"))

####Mortality Excess### 
#Non-COVID deaths 2020
nonCOVID_excess_cuadrante_edad_2020<- excess2 %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2020-01-01") %>%
  filter(fec_defuncion<="2021-01-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(year,week, Cuadrante, EDAD_QUIN = cut(as.numeric(edad), 
                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                 right = FALSE, include.lowest = TRUE, 
                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa=="Otra"))%>%
  na.omit()

cuadrante_nonCOVID_mort_ajustada_2020<- nonCOVID_excess_cuadrante_edad_2020%>%left_join(Pob_edad_cuadrante, by =c("Cuadrante","EDAD_QUIN"))
cuadrante_nonCOVID_mort_ajustada_2020<-cuadrante_nonCOVID_mort_ajustada_2020%>%left_join(Pob_edad_pais_cuadrante,by=c("EDAD_QUIN"))

#Age adjusted dataset in death certificates
non_covid_mun_mort_ajustada1<- cuadrante_nonCOVID_mort_ajustada_2020 %>% mutate(DEF_ESPER = (cases/POB.x)*POB.y)
non_covid_mun_mort_ajustada2<- non_covid_mun_mort_ajustada1 %>% group_by(Cuadrante,year, week) %>% 
  summarise(DEF_ESPER=sum(DEF_ESPER,na.rm = T))%>%
  group_by(Cuadrante, year, week)%>% 
  summarise(DEF_AJUST_2020 = (DEF_ESPER/9018645)*100000)

non_covid_mun_mort_ajustada2$week<-as.factor(non_covid_mun_mort_ajustada2$week)


#Non-COVID deaths 2017-2019
#Populational DISLI arrangement 2017-2019

Pob_edad_pais<-Pob_conapo%>%filter(!is.na(marg))
Pob_edad_cuadrante_anual<- Pob_conapo %>% filter(`A—O` %in% c(2017,2018,2019) & CLAVE_ENT == 9) %>% group_by(`A—O`,CVE_MUN, EDAD_QUIN) %>%
  tally(POB) %>% rename(POB = n) %>% rename(year = `A—O`)%>%
  left_join(dmu_cuadrantes %>% dplyr::select(c("id","Cuadrante")), by =c("CVE_MUN"="id")) %>%
  group_by(year,Cuadrante, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))
Pob_edad_pais_anual<- Pob_edad_pais %>% filter(`A—O` %in% c(2017,2018,2019)) %>% rename(year = `A—O`) %>% group_by(year,EDAD_QUIN) %>% tally(POB) %>% rename(POB = n) 

#Estimating mortality excess
nonCOVID_excess_cuadrante_edad_2017<- excess2 %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2017-01-01") %>%
  filter(fec_defuncion<="2019-12-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(year,week, Cuadrante, EDAD_QUIN = cut(as.numeric(edad), 
                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                 right = FALSE, include.lowest = TRUE, 
                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa=="Otra"))%>%
  na.omit()

cuadrante_nonCOVID_mort_ajustada_2017<- nonCOVID_excess_cuadrante_edad_2017%>%left_join(Pob_edad_cuadrante_anual, by =c("year","Cuadrante","EDAD_QUIN"))
cuadrante_nonCOVID_mort_ajustada_2017<-cuadrante_nonCOVID_mort_ajustada_2017%>%left_join(Pob_edad_pais_anual,by=c("year","EDAD_QUIN"))

#Age adjusted dataset in death certificates
non_covid_mun_mort_ajustada1_2017<- cuadrante_nonCOVID_mort_ajustada_2017 %>% mutate(DEF_ESPER = (cases/POB.x)*POB.y)
non_covid_mun_mort_ajustada2_2017<- non_covid_mun_mort_ajustada1_2017 %>% group_by(year,Cuadrante, week) %>% 
  summarise(DEF_ESPER=sum(DEF_ESPER,na.rm = T))%>%
  group_by(year, Cuadrante, week)
non_covid_mun_mort_ajustada2_2017$pop_total<-NULL
non_covid_mun_mort_ajustada2_2017$pop_total[non_covid_mun_mort_ajustada2_2017$year==2017]<-9049086
non_covid_mun_mort_ajustada2_2017$pop_total[non_covid_mun_mort_ajustada2_2017$year==2018]<-9041395
non_covid_mun_mort_ajustada2_2017$pop_total[non_covid_mun_mort_ajustada2_2017$year==2019]<-9031213
non_covid_mun_mort_ajustada2_2017<-non_covid_mun_mort_ajustada2_2017%>% mutate(DEF_AJUST = (DEF_ESPER/pop_total)*100000)%>%group_by(Cuadrante,week)%>%summarise(DEF_AJUST_2017=mean(DEF_AJUST))
non_covid_mun_mort_ajustada2_2017$week<-as.factor(non_covid_mun_mort_ajustada2_2017$week)

#Final dataset
age_adjusted_excess_mortality<-non_covid_mun_mort_ajustada2%>%left_join(non_covid_mun_mort_ajustada2_2017,by=c("Cuadrante","week"))%>%mutate(excess_mort=(DEF_AJUST_2020-DEF_AJUST_2017))
age_adjusted_excess_mortality$week<-as.factor(age_adjusted_excess_mortality$week)
age_adjusted_excess_mortality$week[age_adjusted_excess_mortality$year==2021 & age_adjusted_excess_mortality$week==1]<-13
age_adjusted_excess_mortality$week<-na.tools::na.replace(age_adjusted_excess_mortality$week,13)
age_adjusted_excess_mortality$week<-factor(age_adjusted_excess_mortality$week, labels =  c("Jan-20'","Feb-20'","Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))

#Figure

quantile(COVID_UNDER_REPORT[COVID_UNDER_REPORT$Cuadrante=="High Density/High DISLI",]$under_report)
quantile(COVID_UNDER_REPORT[COVID_UNDER_REPORT$Cuadrante=="Low Density/High DISLI",]$under_report)
quantile(COVID_UNDER_REPORT[COVID_UNDER_REPORT$Cuadrante=="High Density/Low DISLI",]$under_report)
quantile(COVID_UNDER_REPORT[COVID_UNDER_REPORT$Cuadrante=="Low Density/Low DISLI",]$under_report)

Figure5A<-COVID_UNDER_REPORT %>%
  ggplot(aes(x=week, y=under_report, group=Cuadrante,fill=Cuadrante))+
  geom_bar(stat = "identity",position = "dodge")+
  xlab("Month, year 2020")+
  ylab("Differences in Suspected\nAge Adjusted COVID-19 Mortality,
(Rate per 100,000 inhabitants)")+
  labs(fill="Mean Urban Density\n&\nDensity-Independent SLI Categories")+
  theme_hc()+
  scale_fill_viridis_d()+
  geom_segment(aes(x=0.5,y=1,xend=0.5,yend=30),arrow=arrow(length=unit(0.2,"cm")))+
  geom_segment(aes(x=0.5,y=1,xend=0.5,yend=-10),arrow=arrow(length=unit(0.2,"cm")))+
  geom_text(aes(x = 2,y = 30,label = '\n Suspected COVID-19 deaths \n in GCR records'),show.legend = F,check_overlap = TRUE)+
  geom_text(aes(x = 2,y = -5,label = '\n Suspected COVID-19 deaths \n in SINAVE records'),show.legend = F,check_overlap = TRUE)+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 45,size = 10, hjust = 1),
        axis.text.y = element_text(size = 10, hjust = 1))+
  guides(fill=guide_legend(nrow=2,byrow=T))

Figure5B<-COVID_UNDER_REPORT_amb %>%
  ggplot(aes(x=week, y=under_report, group=Cuadrante,fill=Cuadrante))+
  geom_bar(stat = "identity",position = "dodge")+
  xlab("Month, year 2020")+
  ylab("Differences in Suspected\nAge Adjusted COVID-19 Mortality,
(Rate per 100,000 inhabitants)")+
  labs(fill="Mean Urban Density\n&\nDensity-Independent SLI Categories")+
  theme_hc()+
  facet_grid(~lugar_muerte_2)+
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 45,size = 8, hjust = 1))+
  geom_segment(aes(x=0.5,y=1,xend=0.5,yend=30),arrow=arrow(length=unit(0.2,"cm")))+
  geom_segment(aes(x=0.5,y=1,xend=0.5,yend=-10),arrow=arrow(length=unit(0.2,"cm")))+
  geom_text(aes(x = 3,y = 30,label = '\n Suspected COVID-19 deaths \n in GCR records'),show.legend = F,check_overlap = TRUE)+
  geom_text(aes(x = 3,y = -5,label = '\n Suspected COVID-19 deaths \n in SINAVE records'),show.legend = F,check_overlap = TRUE)+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 45,size = 10, hjust = 1),
        axis.text.y = element_text(size = 10, hjust = 1))+
  guides(fill=guide_legend(nrow=2,byrow=T))

Figure5C<-non_covid_mun_mort_ajustada2_amb %>% 
  ggplot(aes(x=factor(week), y=DEF_AJUST_2020, fill=factor(Cuadrante)))+
  geom_bar(stat = "identity",position = "dodge")+
  theme_hc()+
  ylab("Non-COVID-19 deaths,
(Rate per 100,000 inhabitants)")+
  xlab("Month")+
  guides(col=guide_legend(nrow=2,byrow=TRUE))+
  labs(fill="Mean Urban Density\n&\nDensity-Independent SLI Categories")+
  facet_wrap(~lugar_muerte_2)+
  scale_fill_viridis_d()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 45,size = 10, hjust = 1),
        axis.text.y = element_text(size = 10, hjust = 1))+
  guides(fill=guide_legend(nrow=2,byrow=T))

Figure5D<-age_adjusted_excess_mortality %>% 
  ggplot(aes(x=factor(week), y=excess_mort, fill=factor(Cuadrante)))+
  geom_bar(stat = "identity",position = "dodge")+
  theme_hc()+
  ylab("Age Adjusted Excess Mortality,
(Rate per 100,000 inhabitants)")+
  xlab("Month")+
  guides(col=guide_legend(nrow=2,byrow=TRUE))+
  labs(fill="Mean Urban Density\n&\nDensity-Independent SLI Categories")+
  scale_fill_viridis_d()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 45,size = 10, hjust = 1),
        axis.text.y = element_text(size = 10, hjust = 1))+
  guides(fill=guide_legend(nrow=2,byrow=T))

Figure4<-ggarrange(Figure5A,Figure5B,Figure5C,Figure5D, ncol = 2, nrow = 2, labels = c("A","B","C","D"),common.legend = T)

ggsave(Figure4,
       filename = "Figure_4.png", 
       width = 55, 
       height = 20,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 

#Descriptive Analysis 

table(excess2$AÑO,excess2$causa!="Otra",excess2$LugarMuerte_3,useNA = "always")
prop.table(table(excess2$causa!="Otra",excess2$LugarMuerte_3,useNA = "always"),1)*100

table(covid_casos$evolucion_casoDEFUNCION)
table(covid_casos$evolucion_casoDEFUNCION,covid_casos$tipo_paciente)
prop.table(table(covid_casos$evolucion_casoDEFUNCION,covid_casos$tipo_paciente),1)*100

quantile(mun_mort_ajustada2[mun_mort_ajustada2$Cuadrante=="High Density/High DISLI",]$DEF_AJUST)
quantile(mun_mort_ajustada2[mun_mort_ajustada2$Cuadrante=="High Density/Low DISLI",]$DEF_AJUST)
quantile(mun_mort_ajustada2[mun_mort_ajustada2$Cuadrante=="Low Density/High DISLI",]$DEF_AJUST)
quantile(mun_mort_ajustada2[mun_mort_ajustada2$Cuadrante=="Low Density/Low DISLI",]$DEF_AJUST)


####Underreported COVID-19 deaths are dependent on social lag#####
#Poisson Regression analysis by SLI continous

excess6<- excess2 %>%
  mutate(week=data.table::month(fec_defuncion))%>%
  mutate(year=data.table::year(fec_defuncion))%>%
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,DISLI)),by="id")%>%
  group_by(id,week, year) %>% 
  summarise(covid= sum(!causa=="Otra"),
            no_covid= sum(causa=="Otra"),
            pop_1=median(pop),
            DISLI=median(DISLI),
            covid_rate_acta=sum(!causa=="Otra")/median(pop)*100000,
            no_covid_rate_acta= sum(causa=="Otra")/median(pop)*100000,
            age=median(edad),
            male_sex_cov=sum(sexo=="Hombre" & causa!="Otra"),
            female_sex_cov=sum(sexo=="Mujer" & causa!="Otra"),
            dmu=median(dmu),
            dependent_cov=sum((edad>=65 | edad<15 ) & causa!="Otra", na.rm = T),
            no_dependent_cov=sum((edad<65 & edad>=15) & causa!="Otra" ,na.rm = T),
            male_sex=sum(sexo=="Hombre" & causa=="Otra"),
            female_sex=sum(sexo=="Mujer" & causa=="Otra"),
            dependent=sum((edad>=65 | edad<15 ) & causa=="Otra", na.rm = T),
            no_dependent=sum((edad<65 & edad>=15) & causa=="Otra" ,na.rm = T),
            marg=median(marg)) %>%
  mutate(ind_masc=male_sex/female_sex,
         ind_depend=dependent/no_dependent,
         ind_masc_cov=male_sex_cov/female_sex_cov,
         ind_depend_cov=dependent_cov/no_dependent_cov)%>%
  group_by(week, year) %>% 
  filter(year>=2020)%>%
  filter(week>=3)%>%
  dplyr::select(id,week,year, covid, no_covid, covid_rate_acta, no_covid_rate_acta,pop_1,ind_depend,dmu,ind_masc,ind_masc_cov,ind_depend_cov,marg,DISLI)


excess6$ind_masc_cov[is.infinite(excess6$ind_masc_cov)]<-0
excess6$ind_masc_cov[is.na(excess6$ind_masc_cov)]<-0
excess6$ind_depend_cov[is.infinite(excess6$ind_depend_cov)]<-0
excess6$ind_depend_cov[is.na(excess6$ind_depend_cov)]<-0
pop_sli<-pop%>%filter(AÑO==2020)

#Final Dataset
covid_casos_mort<-NULL

data_tasas_def<-covid_casos%>%
  mutate(week=data.table::month(fecha_defuncion))%>%
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,DISLI)),by="id")%>%
  group_by(id, week)%>% 
  summarise(covid_def_obs=sum(evolucion_casoDEFUNCION==1), 
            pop2=median(pop,na.rm = T))%>%
  mutate(covid_rep_tasa=(covid_def_obs/pop2)*100000)%>%
  filter(week>=3)%>%
  drop_na()%>%
  dplyr::select(id,week, covid_def_obs,covid_rep_tasa, pop2)

excess_2020_3<-excess6%>%dplyr::select(id, week,covid,covid_rate_acta,pop_1,ind_depend,dmu,ind_masc,ind_masc_cov,ind_depend_cov,marg,DISLI)
excess_COVID_2020_3<-excess_2020_3%>%left_join(data_tasas_def, by=c("id"="id","week"="week"))
excess_COVID_2020_3<-excess_COVID_2020_3%>%mutate(dif_mort=((covid_rate_acta-covid_rep_tasa)))
excess_COVID_2020_3<-excess_COVID_2020_3%>%mutate(dif_cases=(covid-covid_def_obs)+919)


#glm for underegistry of marg
m3<-MASS::glm.nb(dif_cases~dmu+offset(log(pop2))+ind_masc_cov+ind_depend_cov, data=excess_COVID_2020_3)
summary(m3)
summ(m3, confint = T, exp = T)

####DISLI adjustment##
dmu_cuadrantes<-data.frame(id=9002:9017)
dmu_cuadrantes$id=str_pad(dmu_cuadrantes$id, 5,pad = "0")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(demo_2,by="id")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(marg4,by="id")
dmu_cuadrantes$DISLI<-lm(marg~dmu, data = dmu_cuadrantes)$residuals
dmu_cuadrantes<-dmu_cuadrantes%>%mutate(Cuadrante = factor(ifelse( (dmu<150 & DISLI < -0.103),
                                                                   "Low Density/Low DISLI",
                                                                   ifelse((dmu<150 & DISLI >= -0.103),
                                                                          "Low Density/High DISLI",
                                                                          ifelse((dmu>=150 & DISLI < -0.103),
                                                                                 "High Density/Low DISLI",
                                                                                 ifelse((dmu>=150 & DISLI>= -0.103),
                                                                                        "High Density/High DISLI", "Invalid"))))))

##Poisson Regression Model for SLI Categories

excess3<- excess2 %>%
  mutate(week=data.table::month(fec_defuncion))%>%
  mutate(year=data.table::year(fec_defuncion))%>%
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2020-03-01") %>%
  filter(fec_defuncion<="2021-01-31") %>%
  group_by(Cuadrante,week, year) %>% 
  summarise(covid= sum(!causa=="Otra"),
            no_covid= sum(causa=="Otra"),
            pop_1=median(pop2),
            covid_rate_acta=sum(!causa=="Otra")/median(pop2)*100000,
            no_covid_rate_acta= sum(causa=="Otra")/median(pop2)*100000,
            age=median(edad),
            male_sex_cov=sum(sexo=="Hombre" & causa!="Otra"),
            female_sex_cov=sum(sexo=="Mujer" & causa!="Otra"),
            dmu=median(dmu),
            dependent_cov=sum((edad>=65 | edad<15 ) & causa!="Otra", na.rm = T),
            no_dependent_cov=sum((edad<65 & edad>=15) & causa!="Otra" ,na.rm = T),
            male_sex=sum(sexo=="Hombre" & causa=="Otra"),
            female_sex=sum(sexo=="Mujer" & causa=="Otra"),
            dependent=sum((edad>=65 | edad<15 ) & causa=="Otra", na.rm = T),
            no_dependent=sum((edad<65 & edad>=15) & causa=="Otra" ,na.rm = T),
            ind_amb_covid=sum(lugar_muerte=="Domicilio" & causa!="Otra" ,na.rm = T),
            ind_hosp_covid=sum(lugar_muerte=="Hospital" & causa!="Otra" ,na.rm = T)) %>%
  mutate(ind_masc=male_sex/female_sex,
         ind_depend=dependent/no_dependent,
         ind_masc_cov=male_sex_cov/female_sex_cov,
         ind_depend_cov=dependent_cov/no_dependent_cov,
         amb_hosp_index=ind_amb_covid/ind_hosp_covid)%>%
  group_by(week, year) %>% 
  filter(year>=2020)%>%
  dplyr::select(Cuadrante,week,year, covid, no_covid, covid_rate_acta, no_covid_rate_acta,pop_1,ind_depend,dmu,ind_masc,ind_masc_cov,ind_depend_cov,amb_hosp_index)

excess3$ind_masc_cov[is.infinite(excess3$ind_masc_cov)]<-0
excess3$ind_depend_cov[is.infinite(excess3$ind_depend_cov)]<-0

#Obserfed Cases DF
pop_sli<-pop%>%filter(AÑO==2020)
#Final Dataset

covid_casos_mort<- covid_casos %>% dplyr::filter(fecha_defuncion<="2021-01-31")
data_tasas_def<-covid_casos_mort%>%
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  mutate(week=data.table::month(fecha_defuncion))%>%
  group_by(Cuadrante, week)%>% 
  summarise(covid_def_obs=sum(evolucion_casoDEFUNCION==1), 
            pop2=median(pop2,na.rm = T))%>%
  mutate(covid_rep_tasa=(covid_def_obs/pop2)*100000)%>%
  drop_na()%>%
  dplyr::select(Cuadrante,week, covid_def_obs,covid_rep_tasa, pop2)

excess_2020_2<-excess3%>%dplyr::select(Cuadrante, week,covid,covid_rate_acta,pop_1,ind_depend,dmu,ind_masc,ind_masc_cov,ind_depend_cov,amb_hosp_index)
excess_COVID_2020_2<-excess_2020_2%>%left_join(data_tasas_def, by=c("Cuadrante"="Cuadrante","week"="week"))
excess_COVID_2020_2<-excess_COVID_2020_2%>%mutate(dif_mort=((covid_rate_acta-covid_rep_tasa)))
excess_COVID_2020_2<-excess_COVID_2020_2%>%mutate(dif_cases=(covid-covid_def_obs)+233)

#Negative binomial poisson for underegistry

m2<-MASS::glm.nb(dif_cases~(Cuadrante=="Low Density/Low DISLI")+offset(log(pop_1))+ind_masc_cov+ind_depend_cov, data=excess_COVID_2020_2)
summary(m2)
summ(m2, confint = T, exp = T)
vif(m2)

#####Excess Mortality Crude Descriptive #####
Pob_edad_pais<-Pob_conapo
Pob_edad_mun<- Pob_conapo %>% filter(`A—O` %in% c(2020) & CLAVE_ENT == 9) %>% group_by(`A—O`,CVE_MUN, EDAD_QUIN) %>%
  tally(POB) %>% rename(POB = n) %>% rename(year = `A—O`)%>% rename(id = CVE_MUN)%>%
  group_by(year,EDAD_QUIN) %>% 
  summarise(POB = sum(POB))
Pob_edad_pais_anual<- Pob_edad_pais %>% filter(`A—O` %in% c(2020)) %>% rename(year = `A—O`) %>% group_by(year,EDAD_QUIN) %>% tally(POB) %>% rename(POB = n) 

nonCOVID_excess_edad_2020<- excess2 %>% 
  filter(fec_defuncion>="2020-01-01") %>%
  filter(fec_defuncion<="2021-01-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(year,EDAD_QUIN = cut(as.numeric(edad), 
                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                 right = FALSE, include.lowest = TRUE, 
                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa=="Otra"))%>%
  na.omit()

nonCOVID_mort_ajustada_2020<- nonCOVID_excess_edad_2020%>%left_join(Pob_edad_mun, by =c("EDAD_QUIN"))
nonCOVID_mort_ajustada_2020<-nonCOVID_mort_ajustada_2020%>%left_join(Pob_edad_pais_anual,by=c("EDAD_QUIN"))

#Age adjusted dataset in death certificates
non_covid_mun_mort_ajustada1_all<- nonCOVID_mort_ajustada_2020 %>% mutate(DEF_ESPER = (cases/POB.x)*POB.y)
non_covid_mun_mort_ajustada2_all<- non_covid_mun_mort_ajustada1_all %>% ungroup %>%
  summarise(DEF_ESPER=sum(DEF_ESPER,na.rm = T))%>%
  summarise(DEF_AJUST_2020 = (DEF_ESPER/9018645)*100000)

#Non-COVID deaths 2017-2019
#Populational DISLI arrangement 2017-2019

Pob_edad_pais<-Pob_conapo
Pob_edad_mun<- Pob_conapo %>% filter(`A—O` %in% c(2017,2018,2019) & CLAVE_ENT == 9) %>% group_by(`A—O`,CVE_MUN, EDAD_QUIN) %>%
  tally(POB) %>% rename(POB = n) %>% rename(year = `A—O`)%>% rename(id = CVE_MUN)%>%
  group_by(year,EDAD_QUIN) %>% 
  summarise(POB = sum(POB))
Pob_edad_pais_anual<- Pob_edad_pais %>% filter(`A—O` %in% c(2017,2018,2019)) %>% rename(year = `A—O`) %>% group_by(year,EDAD_QUIN) %>% tally(POB) %>% rename(POB = n) 

#Estimating mortality excess
nonCOVID_excess_cuadrante_edad_2017<- excess2 %>% 
  filter(fec_defuncion>="2017-01-01") %>%
  filter(fec_defuncion<="2019-12-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(year,EDAD_QUIN = cut(as.numeric(edad), 
                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                 right = FALSE, include.lowest = TRUE, 
                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa=="Otra"))%>%
  na.omit()
cuadrante_nonCOVID_mort_ajustada_2017<- nonCOVID_excess_cuadrante_edad_2017%>%left_join(Pob_edad_mun, by =c("year","EDAD_QUIN"))
cuadrante_nonCOVID_mort_ajustada_2017<-cuadrante_nonCOVID_mort_ajustada_2017%>%left_join(Pob_edad_pais_anual,by=c("year","EDAD_QUIN"))
#Age adjusted dataset in death certificates
non_covid_mun_mort_ajustada1_2017<- cuadrante_nonCOVID_mort_ajustada_2017 %>% mutate(DEF_ESPER = (cases/POB.x)*POB.y)
non_covid_mun_mort_ajustada2_2017<- non_covid_mun_mort_ajustada1_2017 %>% group_by(year) %>% 
  summarise(DEF_ESPER=sum(DEF_ESPER,na.rm = T))%>%
  group_by(year)

non_covid_mun_mort_ajustada2_2017$pop_total<-NULL
non_covid_mun_mort_ajustada2_2017$pop_total[non_covid_mun_mort_ajustada2_2017$year==2017]<-9049086
non_covid_mun_mort_ajustada2_2017$pop_total[non_covid_mun_mort_ajustada2_2017$year==2018]<-9041395
non_covid_mun_mort_ajustada2_2017$pop_total[non_covid_mun_mort_ajustada2_2017$year==2019]<-9031213
non_covid_mun_mort_ajustada2_2017<-non_covid_mun_mort_ajustada2_2017%>% mutate(DEF_AJUST = (DEF_ESPER/pop_total)*100000)%>%ungroup()%>%summarise(DEF_AJUST_2017=mean(DEF_AJUST))

#Final dataset

age_adjusted_excess_mortality<-non_covid_mun_mort_ajustada2_all%>%left_join(non_covid_mun_mort_ajustada2_2017,by=c("Cuadrante","week"))%>%mutate(excess_mort=(DEF_AJUST_2020-DEF_AJUST_2017))
age_adjusted_excess_mortality$week<-as.factor(age_adjusted_excess_mortality$week)
age_adjusted_excess_mortality$week[age_adjusted_excess_mortality$year==2021 & age_adjusted_excess_mortality$week==1]<-13
age_adjusted_excess_mortality$week<-na.tools::na.replace(age_adjusted_excess_mortality$week,13)
age_adjusted_excess_mortality$week<-factor(age_adjusted_excess_mortality$week, labels =  c("Jan-20'","Feb-20'","Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))


######Mortality Excess by SLI categories#####
nonCOVID_excess_cuadrante_edad_2020<- excess2 %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2020-01-01") %>%
  filter(fec_defuncion<="2020-12-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(week, Cuadrante, EDAD_QUIN = cut(as.numeric(edad), 
                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                 right = FALSE, include.lowest = TRUE, 
                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa=="Otra"),
            male_sex=sum(sexo=="Hombre" & causa=="Otra"),
            female_sex=sum(sexo=="Mujer" & causa=="Otra"),
            dependent=sum((edad>=65 | edad<15 ) & causa=="Otra", na.rm = T),
            no_dependent=sum((edad<65 & edad>=15) & causa=="Otra" ,na.rm = T))%>%
  mutate(ind_masc=male_sex/female_sex,
         ind_depend=dependent/no_dependent)%>%
  na.omit()

cuadrante_nonCOVID_mort_ajustada_2020<- nonCOVID_excess_cuadrante_edad_2020%>%left_join(Pob_edad_cuadrante, by =c("Cuadrante","EDAD_QUIN"))
cuadrante_nonCOVID_mort_ajustada_2020<-cuadrante_nonCOVID_mort_ajustada_2020%>%left_join(Pob_edad_pais_cuadrante,by=c("EDAD_QUIN"))

#Age adjusted dataset in death certificates
non_covid_mun_mort_ajustada1<- cuadrante_nonCOVID_mort_ajustada_2020 %>% mutate(DEF_ESPER = (cases/POB.x)*POB.y)

#Estimating mortality excess
nonCOVID_excess_cuadrante_edad_2017<- excess2 %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2017-01-01") %>%
  filter(fec_defuncion<="2019-12-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(year,week, Cuadrante, EDAD_QUIN = cut(as.numeric(edad), 
                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                 right = FALSE, include.lowest = TRUE, 
                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa=="Otra"))%>%
  group_by(week, Cuadrante,EDAD_QUIN)%>%
  summarise(cases_2017=mean(cases))%>%
  na.omit()

#Final dataset
age_adjusted_excess_mortality<-non_covid_mun_mort_ajustada1%>%left_join(nonCOVID_excess_cuadrante_edad_2017,by=c("Cuadrante","week","EDAD_QUIN"))%>%mutate(excess_cases=(cases-cases_2017)+32.667)
age_adjusted_excess_mortality$Cuadrante<-relevel(age_adjusted_excess_mortality$Cuadrante,ref = "Low Density/Low DISLI")
age_adjusted_excess_mortality$ind_masc[is.infinite(age_adjusted_excess_mortality$ind_masc)]<-0
age_adjusted_excess_mortality$ind_depend[is.infinite(age_adjusted_excess_mortality$ind_depend)]<-0

#Estimation of excess mortality

m3<-MASS::glm.nb(excess_cases~Cuadrante+ind_masc+ind_depend+offset(log(POB.y)), data=age_adjusted_excess_mortality)
m3.1<-MASS::glm.nb(excess_cases~(Cuadrante=="High Density/High DISLI")+ind_masc+ind_depend+offset(log(POB.y)), data=age_adjusted_excess_mortality)
m3.2<-MASS::glm.nb(excess_cases~(Cuadrante=="High Density/Low DISLI")+ind_masc+ind_depend+offset(log(POB.y)), data=age_adjusted_excess_mortality)
m3.3<-MASS::glm.nb(excess_cases~(Cuadrante=="Low Density/High DISLI")+ind_masc+ind_depend+offset(log(POB.y)), data=age_adjusted_excess_mortality)
m3.4<-MASS::glm.nb(excess_cases~(Cuadrante=="Low Density/Low DISLI")+ind_masc+ind_depend+offset(log(POB.y)), data=age_adjusted_excess_mortality)
summ(m3, confint = T, exp = T)
summ(m3.1, confint = T, exp = T)
summ(m3.2, confint = T, exp = T)
summ(m3.3, confint = T, exp = T)
summ(m3.4, confint = T, exp = T)

#####Working group cathegories epidemiological rates (Supplementary Figure X)#####
#Working groups cathegories
data_tasas_workers<-covid_casos%>%
  filter(edad>=15)%>%
  mutate(week=data.table::month(fecha_ingreso),year=data.table::year(fecha_ingreso))%>%
  group_by(year,week,CAT_TRABAJO_INEGI)%>% 
  summarise(cases_def=sum(evolucion_casoDEFUNCION==1 & covid==1 ,na.rm=T), 
            asinto=sum(asintomaticos== 1 & covid==1, na.rm=T),
            tested=n(),
            cases=sum(covid==1,na.rm=T), 
            pop=median(pop_trabaja_INEGI_3, na.rm=T),
            edad=median(edad, na.rm = T),
            male=sum(sexo=="MASCULINO", na.rm = T),
            comorbitities=median(comorb, na.rm = T),
            sx_time=mean(FU_time_HOSP, na.rm = T),
            sx_time_7d=sum(FU_time_HOSP>=7, na.rm = T),
            HOSP=sum(HOSP, na.rm = T),
            INTUB=sum(intubado, na.rm = T),
            SEVERE=sum(severe_covid19, na.rm = T),
            sx_time=mean(FU_time_HOSP, na.rm = T),
            sx_time_7d=sum(FU_time_HOSP>=7, na.rm = T))%>%
  mutate(incidence_rate=cases/pop*100000,
         testing_rate=tested/pop*100000,
         asinto_rate=asinto/pop*100000,
         mort_rate=cases_def/pop*100000,
         host_rate=(HOSP/tested)*100,
         intub_rate=(INTUB/tested)*100,
         severe_rate=(SEVERE/tested)*100,
         letality=(cases_def/cases)*100,
         percentage_hosp =( HOSP / sum(HOSP))*100,
         percentage_intub = (INTUB / sum(INTUB))*100,
         percentage_caso_grave = (SEVERE / sum(SEVERE))*100,
         percentage_letality = (cases_def / sum(cases_def))*100,
         percentage_7d_sx =( sx_time_7d / sum(sx_time_7d))*100,)%>%
  ungroup()%>%
  group_by(CAT_TRABAJO_INEGI)%>%
  mutate(cum_incidence_rate=cumsum(incidence_rate),
         cum_testing_rate=cumsum(testing_rate),
         cum_asinto_rate=cumsum(asinto_rate),
         cum_mortality_rate=cumsum(mort_rate))%>%
  drop_na()

data_tasas_workers$week[data_tasas_workers$week==1]<-13
data_tasas_workers$week<-factor(data_tasas_workers$week, labels =  c("Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))

data_tasas_workers$CAT_TRABAJO_COVID_2<-NULL
data_tasas_workers$CAT_TRABAJO_COVID_2[data_tasas_workers$CAT_TRABAJO_INEGI=="DESEMPLEADOS"]<-7
data_tasas_workers$CAT_TRABAJO_COVID_2[data_tasas_workers$CAT_TRABAJO_INEGI=="JUBILADO / PENSIONADO"]<-6
data_tasas_workers$CAT_TRABAJO_COVID_2[data_tasas_workers$CAT_TRABAJO_INEGI=="HOGAR"]<-5
data_tasas_workers$CAT_TRABAJO_COVID_2[data_tasas_workers$CAT_TRABAJO_INEGI=="OTROS"]<-4
data_tasas_workers$CAT_TRABAJO_COVID_2[data_tasas_workers$CAT_TRABAJO_INEGI=="OCUPADO"]<-3
data_tasas_workers$CAT_TRABAJO_COVID_2[data_tasas_workers$CAT_TRABAJO_INEGI=="ESTUDIANTE"]<-2
data_tasas_workers$CAT_TRABAJO_COVID_2[data_tasas_workers$CAT_TRABAJO_INEGI=="TRABAJADORES_SALUD"]<-1
data_tasas_workers$CAT_TRABAJO_COVID_2<-factor(data_tasas_workers$CAT_TRABAJO_COVID_2, 
                                               labels = c("HCWs", "Students", 
                                                          "Economically-Active", "Other Non-Specified Workers",
                                                          "Home-Related Workers", "Retired Adults", "Unemployed"))


#Populational based analysis of workers
Figure1A<-data_tasas_workers %>%
  ggplot(aes(x=factor(week), y=cum_incidence_rate, group=factor(CAT_TRABAJO_COVID_2),colour=factor(CAT_TRABAJO_COVID_2)))+
  geom_line(alpha=1, size=1.5)+
  geom_point(alpha=1, size=2.5)+
  xlab("Month")+
  ylab("Cummulative Incidence Rate
(Rate per 100,000 inhabitants)")+
  scale_color_jama()+
  labs(colour="Working Category")+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 45,size = 10, hjust = 1))

Figure1B<-data_tasas_workers %>%
  ggplot(aes(x=factor(week), y=cum_testing_rate, group=factor(CAT_TRABAJO_COVID_2),colour=factor(CAT_TRABAJO_COVID_2)))+
  geom_line(alpha=1, size=1.5)+
  geom_point(alpha=1, size=2.5)+
  xlab("Month")+
  ylab("Cummulative Testing Rate
(Rate per 100,000 inhabitants)")+
  scale_color_jama()+
  labs(colour="Working Category")+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 45,size = 10, hjust = 1))

Figure1C<-data_tasas_workers %>%
  ggplot(aes(x=factor(week), y=cum_asinto_rate, group=factor(CAT_TRABAJO_COVID_2),colour=factor(CAT_TRABAJO_COVID_2)))+
  geom_line(alpha=1, size=1.5)+
  geom_point(alpha=1, size=2.5)+
  xlab("Month")+
  ylab("Cummulative Asymtomatic Rate
(Rate per 100,000 inhabitants)")+
  scale_color_jama()+
  labs(colour="Working Category")+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 45,size = 10, hjust = 1))

Figure1D<-data_tasas_workers %>%
  ggplot(aes(x=factor(week), y=cum_mortality_rate, group=factor(CAT_TRABAJO_COVID_2),colour=factor(CAT_TRABAJO_COVID_2)))+
  geom_line(alpha=1, size=1.5)+
  geom_point(alpha=1, size=2.5)+
  xlab("Month")+
  ylab("Cummulative Mortality Rate
(Rate per 100,000 inhabitants)")+
  scale_color_jama()+
  labs(colour="Working Category")+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 45,size = 10, hjust = 1))

Figure1_UP<-ggarrange(Figure1A,Figure1B,Figure1C, Figure1D, ncol = 2, nrow = 2, labels = c("A","B","C","D"),common.legend = T)
ggsave(Figure1_UP,
       filename = "Sup_Figure_X.png", 
       width = 40, 
       height = 20,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 



####Profile SARI/ILI rate and positivity rate (Supplementary Figure 3)#####

covid_casos$fecha_ingreso<-as.character(covid_casos$fecha_ingreso)
covid_casos$fecha_ingreso<-as.Date(covid_casos$fecha_ingreso)

positivity_rate<-covid_casos%>% 
  filter(fecha_ingreso>="2020-03-01") %>%
  filter(fecha_ingreso<="2021-01-31") %>%
  mutate(week=data.table::month(fecha_ingreso),
         year=data.table::year(fecha_ingreso))%>%
  group_by(year,week)%>% 
  summarise(covid_pos=sum(covid==1,na.rm=T),
            tested=n())%>%
  mutate(covid_rate=(covid_pos/tested)*100)

positivity_rate$week<-as.factor(positivity_rate$week)
positivity_rate$week[positivity_rate$year==2021 & positivity_rate$week==1]<-13
positivity_rate$week<-na.tools::na.replace(positivity_rate$week,13)
positivity_rate$week<-factor(positivity_rate$week, labels =  c("Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))


Sup_Fig_2A<-ggplot(positivity_rate, aes(x=week, y=covid_rate,group = 1))+
  geom_line(size=1.5,colour="#209dd1")+
  geom_point(size=4, colour="#dc8945")+
  theme_hc()+
  ylab("Positivity Rate in Mexico City
(1-month rolling average)")+
  xlab("Month")+
  scale_y_continuous(limits = c(10, 50), breaks = seq(from = 10, to = 60, by = 10))


#SARI and ILI rates

covid_casos$sx_percentage<-NULL
covid_casos$sx_percentage[covid_casos$ETI==1 & covid_casos$covid==1]<-1
covid_casos$sx_percentage[covid_casos$ETI==1 & covid_casos$covid==0]<-2
covid_casos$sx_percentage[covid_casos$IRAG==1 & covid_casos$covid==1]<-3
covid_casos$sx_percentage[covid_casos$IRAG==1 & covid_casos$covid==0]<-4
covid_casos$sx_percentage<-factor(covid_casos$sx_percentage, labels = c("ILI & Covid", "ILI & Non-Covid", "SARI & Covid","SARI & Non-Covid"))

data_tasas_1<-covid_casos%>% 
  filter(fecha_ingreso>="2020-03-01") %>%
  filter(fecha_ingreso<="2021-01-31") %>%
  mutate(week=data.table::month(fecha_ingreso),
         year=data.table::year(fecha_ingreso))%>%
  group_by(year,week)%>% 
  summarise(case=n())

data_tasas_2<-covid_casos%>%
  filter(fecha_ingreso>="2020-03-01") %>%
  filter(fecha_ingreso<="2021-01-31") %>%
  mutate(week=data.table::month(fecha_ingreso),
         year=data.table::year(fecha_ingreso))%>%
  group_by(year,week,sx_percentage)%>% 
  summarise(case_sx=n())


data_tasas_2<-data_tasas_2%>%left_join(data_tasas_1, by=c("year","week"))
data_tasas_2<-data_tasas_2%>%mutate(prop_cases=(case_sx/case)*100)

data_tasas_2$week[data_tasas_2$year==2021 & data_tasas_2$week==1]<-13
data_tasas_2$week<-na.tools::na.replace(data_tasas_2$week,13)
data_tasas_2$week<-factor(data_tasas_2$week, labels =  c("Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))

Sup_Fig_2B<-data_tasas_2 %>% 
  ggplot()+
  geom_area(aes(x=week, y=prop_cases, group=sx_percentage,fill=sx_percentage), alpha=0.8 , size=1, colour="black")+
  theme_hc()+
  ylab("Tested cases in Mexico City
(1-month rolling average)")+
  xlab("Month")+
  scale_fill_jama()+
  labs(fill="Case")

Supplementary_Figure_3<-ggarrange(Sup_Fig_2A,Sup_Fig_2B, labels = c("A", "B"), nrow = 1, ncol = 2)
ggsave(Supplementary_Figure_3,
       filename = "Supplementary_Figure_3.png", 
       width = 40, 
       height = 20,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 


####Epidemiological rates by month (Supplementary Figure 4)####

data_tasas<-covid_casos%>% 
  mutate(week=data.table::month(fecha_ingreso),
         year=data.table::year(fecha_ingreso))%>%
  group_by(year,week)%>% 
  summarise(cases_def=sum(evolucion_casoDEFUNCION==1 & covid==1,na.rm=T), 
            tested=n(),
            cases=sum(covid,na.rm=T), 
            pop=median(pop_3, na.rm=T),
            marg=median(marg,na.rm=T),
            asinto=sum(asintomaticos== 1 & covid==1, na.rm=T),
            cases_eti_covid=sum(ETI==1 & covid==1, na.rm=T),
            cases_eti_no_covid=sum(ETI==1 & covid==0, na.rm=T),
            cases_irag_covid=sum(IRAG==1 & covid==1, na.rm=T),
            cases_irag_no_covid=sum(IRAG==1 & covid==0, na.rm=T))%>%
  mutate(positivity_rate= cases/tested*100,
         rate_incidence=cases/pop*100000, 
         rate_mort=cases_def/pop*100000, 
         rate_letal=(cases_def/cases)*100, 
         rate_pruebas=tested/pop*100000, 
         rate_asinto=asinto/pop*100000,
         prop_cases_eti_covid=cases_eti_covid/tested*100,
         prop_cases_eti_no_covid=cases_eti_no_covid/tested*100,
         prop_cases_irag_covid=cases_irag_covid/tested*100,
         prop_cases_irag_no_covid=cases_irag_no_covid/tested*100)


data_tasas$week[data_tasas$year==2021 & data_tasas$week==1]<-13
data_tasas$week<-na.tools::na.replace(data_tasas$week,13)
data_tasas$week<-factor(data_tasas$week, labels =  c("Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))

Sup_figure_4<-data_tasas %>% 
  ggplot()+
  geom_bar(aes(x=week, y=rate_pruebas,fill="Testing"),stat="identity")+ 
  geom_bar(aes(x=week, y=rate_incidence,fill="Incidence"),stat="identity")+ 
  geom_bar(aes(x=week, y=rate_asinto,fill="Asymptomatic"),stat="identity")+ 
  geom_bar(aes(x=week, y=rate_mort,fill="Mortality"),stat="identity")+
  theme_hc()+
  ylab("Cases per 100,000 inhabitants 
(Rolling average), log scale")+
  xlab("Month")+
  scale_fill_jama()+
  labs(fill="Epidemiological Rate")+
  scale_y_log10(limits = c(1,5000), breaks = seq(from = 0, to = 5000, by = 500))

ggsave(Sup_figure_4,
       filename = "Supplementary_Figure_4.png", 
       width = 20, 
       height = 20,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 


####Profile SARI/ILI rate and positivity rate by Working Categories and SLI (Supplementary Figure 5)####

positivity_rate_work<-covid_casos%>% 
  filter(fecha_ingreso>="2020-03-01") %>%
  filter(fecha_ingreso<="2021-01-31") %>%
  mutate(year=data.table::year(fecha_ingreso),
         week=data.table::month(fecha_ingreso))%>%
  group_by(year,week,CAT_TRABAJO_COVID_2)%>% 
  summarise(covid_pos=sum(covid==1,na.rm=T),
            tested=n())%>%
  mutate(covid_rate=(covid_pos/tested)*100)

positivity_rate_work$week[positivity_rate_work$year==2021 & positivity_rate_work$week==1]<-13
positivity_rate_work$week<-na.tools::na.replace(positivity_rate_work$week,13)
positivity_rate_work$week<-factor(positivity_rate_work$week, labels =  c("Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))

Sup_Fig_4A<-ggplot(positivity_rate_work, aes(x=week, y=covid_rate, group = CAT_TRABAJO_COVID_2,colour = CAT_TRABAJO_COVID_2))+
  geom_line(size=1.5)+
  geom_point(size=4)+
  theme_hc()+
  ylab("Positivity Rate
(1-month rolling average)")+
  xlab("Month, 2020")+
  scale_color_jama()+
  labs(colour="Working Category")+
  scale_y_continuous(limits = c(10, 60), breaks = seq(from = 10, to = 60, by = 10))

#SARI and ILI rates
covid_casos$sx_percentage<-NULL
covid_casos$sx_percentage[covid_casos$ETI==1 & covid_casos$covid==1]<-1
covid_casos$sx_percentage[covid_casos$ETI==1 & covid_casos$covid==0]<-2
covid_casos$sx_percentage[covid_casos$IRAG==1 & covid_casos$covid==1]<-3
covid_casos$sx_percentage[covid_casos$IRAG==1 & covid_casos$covid==0]<-4
covid_casos$sx_percentage<-factor(covid_casos$sx_percentage, labels = c("ILI & Covid", "ILI & Non-Covid", "SARI & Covid","SARI & Non-Covid"))


data_tasas_1_work<-covid_casos%>% 
  filter(fecha_ingreso>="2020-03-01") %>%
  filter(fecha_ingreso<="2021-01-31") %>%
  mutate(year=data.table::year(fecha_ingreso),
         week=data.table::month(fecha_ingreso))%>%
  group_by(year,week,CAT_TRABAJO_COVID_2)%>% 
  summarise(case=n())

data_tasas_2_work<-covid_casos%>% 
  filter(fecha_ingreso>="2020-03-01") %>%
  filter(fecha_ingreso<="2021-01-31") %>%
  mutate(year=data.table::year(fecha_ingreso),
         week=data.table::month(fecha_ingreso))%>%
  group_by(year,week,CAT_TRABAJO_COVID_2,sx_percentage)%>% 
  summarise(case_sx=n())

data_tasas_2_work<-data_tasas_2_work%>%left_join(data_tasas_1_work, by=c("week", "CAT_TRABAJO_COVID_2"))
data_tasas_2_work<-data_tasas_2_work%>%mutate(prop_cases=(case_sx/case)*100)

data_tasas_2_work$week[data_tasas_2_work$year==2021 & data_tasas_2_work$week==1]<-13
data_tasas_2_work$week<-na.tools::na.replace(data_tasas_2_work$week,13)
data_tasas_2_work$week<-factor(data_tasas_2_work$week, labels =  c("Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))


Sup_Fig_4B<-data_tasas_2_work %>% 
  ggplot()+
  geom_area(aes(x=week, y=prop_cases, group=sx_percentage,fill=sx_percentage), alpha=0.8 , size=1, colour="black")+
  theme_hc()+
  ylab("Tested cases
(1-month rolling average)")+
  xlab("Month, 2020")+
  facet_wrap(~CAT_TRABAJO_COVID_2, ncol = 4)+
  scale_fill_jama()+
  labs(fill="Case")+
  theme(axis.text.x = element_text(angle = 45,size = 8, hjust = 1))

#SLI Categories
#DISLI df

demo_2<-demo%>%dplyr::select(id,dmu)
marg4<-marg%>%dplyr::select(id,marg)
dmu_cuadrantes<-data.frame(id=9002:9017)
dmu_cuadrantes$id=str_pad(dmu_cuadrantes$id, 5,pad = "0")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(demo_2,by="id")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(marg4,by="id")
dmu_cuadrantes$DISLI<-lm(marg~dmu, data = dmu_cuadrantes)$residuals
dmu_cuadrantes<-dmu_cuadrantes%>%mutate(Cuadrante = factor(ifelse( (dmu<150 & DISLI < -0.103),
                                                                   "Low Density/Low DISLI",
                                                                   ifelse((dmu<150 & DISLI >= -0.103),
                                                                          "Low Density/High DISLI",
                                                                          ifelse((dmu>=150 & DISLI < -0.103),
                                                                                 "High Density/Low DISLI",
                                                                                 ifelse((dmu>=150 & DISLI>= -0.103),
                                                                                        "High Density/High DISLI", "Invalid"))))))

positivity_rate_sli<-covid_casos%>% 
  filter(fecha_ingreso>="2020-03-01") %>%
  filter(fecha_ingreso<="2021-01-31") %>%
  mutate(year=data.table::year(fecha_ingreso),
         week=data.table::month(fecha_ingreso))%>%
  left_join(dmu_cuadrantes %>% dplyr::select(c("id","Cuadrante")), by =c("id")) %>%
  group_by(year,week,Cuadrante)%>% 
  summarise(covid_pos=sum(covid==1,na.rm=T),
            tested=n())%>%
  mutate(covid_rate=(covid_pos/tested)*100)

positivity_rate_sli$week[positivity_rate_sli$year==2021 & positivity_rate_sli$week==1]<-13
positivity_rate_sli$week<-na.tools::na.replace(positivity_rate_sli$week,13)
positivity_rate_sli$week<-factor(positivity_rate_sli$week, labels =  c("Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))

Sup_Fig_4C<-ggplot(positivity_rate_sli, aes(x=week, y=covid_rate, group = Cuadrante,colour = Cuadrante))+
  geom_line(size=1.5)+
  geom_point(size=4)+
  theme_hc()+
  ylab("Positivity Rate
(1-month rolling average)")+
  xlab("Month, 2020")+
  scale_color_viridis_d()+
  labs(colour="Social Lag Index Category")+
  scale_y_continuous(limits = c(10, 50), breaks = seq(from = 10, to = 60, by = 10))

#SARI and ILI rates

covid_casos$sx_percentage<-NULL
covid_casos$sx_percentage[covid_casos$ETI==1 & covid_casos$covid==1]<-1
covid_casos$sx_percentage[covid_casos$ETI==1 & covid_casos$covid==0]<-2
covid_casos$sx_percentage[covid_casos$IRAG==1 & covid_casos$covid==1]<-3
covid_casos$sx_percentage[covid_casos$IRAG==1 & covid_casos$covid==0]<-4
covid_casos$sx_percentage<-factor(covid_casos$sx_percentage, labels = c("ILI & Covid", "ILI & Non-Covid", "SARI & Covid","SARI & Non-Covid"))

data_tasas_1_sli<-covid_casos%>% 
  filter(fecha_ingreso>="2020-03-01") %>%
  filter(fecha_ingreso<="2021-01-31") %>% 
  left_join(dmu_cuadrantes %>% dplyr::select(c("id","Cuadrante")), by =c("id")) %>%
  mutate(year=data.table::year(fecha_ingreso),
         week=data.table::month(fecha_ingreso))%>%
  group_by(year,week,Cuadrante)%>% 
  summarise(case=n())

data_tasas_2_sli<-covid_casos%>% 
  filter(fecha_ingreso>="2020-03-01") %>%
  filter(fecha_ingreso<="2021-01-31") %>% 
  left_join(dmu_cuadrantes %>% dplyr::select(c("id","Cuadrante")), by =c("id")) %>%
  mutate(year=data.table::year(fecha_ingreso),
         week=data.table::month(fecha_ingreso))%>%
  group_by(year,week,Cuadrante,sx_percentage)%>% 
  summarise(case_sx=n())

data_tasas_2_sli<-data_tasas_2_sli%>%left_join(data_tasas_1_sli, by=c("year","week", "Cuadrante"))
data_tasas_2_sli<-data_tasas_2_sli%>%mutate(prop_cases=(case_sx/case)*100)
data_tasas_2_sli$week[data_tasas_2_sli$year==2021 & data_tasas_2_sli$week==1]<-13
data_tasas_2_sli$week<-na.tools::na.replace(data_tasas_2_sli$week,13)
data_tasas_2_sli$week<-factor(data_tasas_2_sli$week, labels =  c("Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))

Sup_Fig_4D<-data_tasas_2_sli %>% 
  ggplot()+
  geom_area(aes(x=week, y=prop_cases, group=sx_percentage,fill=sx_percentage), alpha=0.8 , size=1, colour="black")+
  theme_hc()+
  ylab("Tested cases
(1-month rolling average)")+
  xlab("Month, 2020")+
  facet_grid(~Cuadrante)+
  scale_fill_jama()+
  labs(fill="Case")+
  theme(axis.text.x = element_text(angle = 45,size = 8, hjust = 1))

Sup_Fig_5<-ggarrange(Sup_Fig_4A, Sup_Fig_4B,Sup_Fig_4C,Sup_Fig_4D, labels = c("A", "B","C","D"), nrow = 2, ncol = 2)
ggsave(Sup_Fig_5,
       filename = "Supplementary_Figure_5.png", 
       width = 58, 
       height = 25,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 


####Age, comorbidities and symtomatology profile by working group categories (Suplementary Figure 6)####

covid_casos_comorb<- covid_casos %>% filter(cve_entidad_residencia=="9") %>% filter(!is.na(marg)) %>% filter(covid==1) %>% filter(evolucion_casoDEFUNCION==1)  %>% filter(edad>=15) 
graf_1<-covid_casos_comorb%>% dplyr::select(edad,comorb,numero_sintomas,CAT_TRABAJO_INEGI)%>%drop_na()
graf_1$CAT_TRABAJO_COVID_2<-NULL
graf_1$CAT_TRABAJO_COVID_2[graf_1$CAT_TRABAJO_INEGI=="DESEMPLEADOS"]<-7
graf_1$CAT_TRABAJO_COVID_2[graf_1$CAT_TRABAJO_INEGI=="JUBILADO / PENSIONADO"]<-6
graf_1$CAT_TRABAJO_COVID_2[graf_1$CAT_TRABAJO_INEGI=="HOGAR"]<-5
graf_1$CAT_TRABAJO_COVID_2[graf_1$CAT_TRABAJO_INEGI=="OTROS"]<-4
graf_1$CAT_TRABAJO_COVID_2[graf_1$CAT_TRABAJO_INEGI=="OCUPADO"]<-3
graf_1$CAT_TRABAJO_COVID_2[graf_1$CAT_TRABAJO_INEGI=="ESTUDIANTE"]<-2
graf_1$CAT_TRABAJO_COVID_2[graf_1$CAT_TRABAJO_INEGI=="TRABAJADORES_SALUD"]<-1
graf_1$CAT_TRABAJO_COVID_2<-factor(graf_1$CAT_TRABAJO_COVID_2, 
                                        labels = c("HCWs", "Students", 
                                                   "Economically-Active", "Other Non-Specified Workers",
                                                   "Home-Related Workers", "Retired Adults", "Unemployed"))


my_comparisons <- list( c("HCWs", "Students"), c("HCWs", "Economically-Active"), c("HCWs", "Other Non-Specified Workers"), c("HCWs", "Home-Related Workers"),c("HCWs", "Retired Adults"),c("HCWs", "Unemployed"),
                        c("Students","Economically-Active"), c("Students", "Other Non-Specified Workers"),c("Students", "Home-Related Workers"),c("Students", "Retired Adults"),c("Students", "Unemployed"),
                        c("Economically-Active","Other Non-Specified Workers"),c("Economically-Active","Home-Related Workers"),c("Economically-Active","Retired Adults"),c("Economically-Active","Unemployed"),
                        c("Other Non-Specified Workers","Home-Related Workers"),c("Other Non-Specified Workers","Retired Adults"),c("Other Non-Specified Workers","Unemployed"),
                        c("Home-Related Workers","Retired Adults"),c("Home-Related Workers","Unemployed"))

sup_6_a<-ggplot(graf_1, aes(x=edad, fill=CAT_TRABAJO_COVID_2))+
  geom_histogram(aes(y = (..count..)/sum(..count..)),col="black", binwidth = 6,stat="count")+
  xlab("Age (years)")+
  ylab("Relative Frequencies
(Lethal Cases)")+
  theme_hc()+
  scale_y_continuous(labels=scales::percent) +
  labs(fill="Working Categories")+
  theme(legend.position="top")+
  scale_fill_jama()

sup_6_b<-ggplot(graf_1,aes(x=CAT_TRABAJO_COVID_2,y=edad,fill=CAT_TRABAJO_COVID_2))+
  geom_violin(alpha=0.8)+
  geom_boxplot(width=0.3)+
  xlab("SLI-Categories")+
  ylab("Age (years)
(Lethal Cases)")+
  theme_hc()+
  labs(fill="SLI-Categories")+
  theme(legend.position="top", axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_fill_jama()



sup_6_c<-ggplot(graf_1, aes(x=numero_sintomas, fill=CAT_TRABAJO_COVID_2))+
  geom_histogram(aes(y = (..count..)/sum(..count..)),col="black", binwidth = 6,stat="count")+
  xlab("Number of Symptoms")+
  ylab("Relative Frequencies
(Lethal Cases)")+
  theme_hc()+
  scale_y_continuous(labels=scales::percent) +
  labs(fill="SLI-Categories")+
  theme(legend.position="top")+
  scale_fill_jama()

sup_6_d<-ggplot(graf_1,aes(x=CAT_TRABAJO_COVID_2,y=numero_sintomas,fill=CAT_TRABAJO_COVID_2))+
  geom_violin(alpha=0.8)+
  geom_boxplot(width=0.3)+
  xlab("SLI-Categories")+
  ylab("Number of Symptoms
(Lethal Cases)")+
  theme_hc()+
  labs(fill="SLI-Categories")+
  theme(legend.position="top", axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_fill_jama()

sup_6_e<-ggplot(graf_1, aes(x=comorb, fill=CAT_TRABAJO_COVID_2))+
  geom_histogram(aes(y = (..count..)/sum(..count..)),col="black", binwidth = 6,stat="count")+
  xlab("Comorbid Conditions")+
  ylab("Relative Frequencies
(Lethal Cases)")+
  theme_hc()+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks = seq(from = 0, to = 18, by = 1))+
  labs(fill="SLI-Categories")+
  theme(legend.position="top")+
  scale_fill_jama()

sup_6_f<-ggplot(graf_1,aes(x=CAT_TRABAJO_COVID_2,y=comorb,fill=CAT_TRABAJO_COVID_2))+
  geom_violin(alpha=0.8)+
  geom_boxplot(width=0.3)+
  xlab("SLI-Categories")+
  ylab("Comorbid Conditions
(Lethal Cases)")+
  theme_hc()+
  scale_y_continuous(limits = c(-2,15))+
  labs(fill="SLI-Categories")+
  theme(legend.position="top", axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_fill_jama()

#Age

covid_casos_comorb$edad_cat<-NULL;
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=0 & covid_casos_comorb$edad<=9]<-1
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=10 & covid_casos_comorb$edad<=19]<-2
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=20 & covid_casos_comorb$edad<=29]<-3
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=30 & covid_casos_comorb$edad<=39]<-4
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=40 & covid_casos_comorb$edad<=49]<-5
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=50 & covid_casos_comorb$edad<=59]<-6
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=60 & covid_casos_comorb$edad<=69]<-7
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=70 & covid_casos_comorb$edad<=79]<-8
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=80 & covid_casos_comorb$edad<=89]<-9
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=90]<-10

covid_casos_comorb$CAT_TRABAJO_COVID_2<-NULL
covid_casos_comorb$CAT_TRABAJO_COVID_2[covid_casos_comorb$CAT_TRABAJO_INEGI=="DESEMPLEADOS"]<-7
covid_casos_comorb$CAT_TRABAJO_COVID_2[covid_casos_comorb$CAT_TRABAJO_INEGI=="JUBILADO / PENSIONADO"]<-6
covid_casos_comorb$CAT_TRABAJO_COVID_2[covid_casos_comorb$CAT_TRABAJO_INEGI=="HOGAR"]<-5
covid_casos_comorb$CAT_TRABAJO_COVID_2[covid_casos_comorb$CAT_TRABAJO_INEGI=="OTROS"]<-4
covid_casos_comorb$CAT_TRABAJO_COVID_2[covid_casos_comorb$CAT_TRABAJO_INEGI=="OCUPADO"]<-3
covid_casos_comorb$CAT_TRABAJO_COVID_2[covid_casos_comorb$CAT_TRABAJO_INEGI=="ESTUDIANTE"]<-2
covid_casos_comorb$CAT_TRABAJO_COVID_2[covid_casos_comorb$CAT_TRABAJO_INEGI=="TRABAJADORES_SALUD"]<-1
covid_casos_comorb$CAT_TRABAJO_COVID_2<-factor(covid_casos_comorb$CAT_TRABAJO_COVID_2, 
                                   labels = c("HCWs", "Students", 
                                              "Economically-Active", "Other Non-Specified Workers",
                                              "Home-Related Workers", "Retired Adults", "Unemployed"))

table_age<-prop.table(table(covid_casos_comorb$CAT_TRABAJO_COVID_2,covid_casos_comorb$edad_cat),1)*100

rownames(table_age) <- c("HCWs", "Students", 
                         "Economically-Active", "Other Non-Specified Workers",
                         "Home-Related Workers", "Retired Adults", "Unemployed")
colnames(table_age) <- c("11-19 y.o","20-29 y.o","30-39 y.o","40-49 y.o","50-59 y.o","60-69 y.o","70-79 y.o","80-89 y.o",">90 y.o")
table_age_2<- rbind(rep(50,9) , rep(0,9) , table_age)
table_age_3<-as.data.frame(table_age_2)

sup_6_g<-as.ggplot(~fmsb::radarchart(table_age_3,axistype=1, maxmin=T,
                                     pcol=c("#3a4e56","#e1a86a","#31a0d8", "#ab4844","#80af96","#69659c","#80796a"), plwd=4 , plty=1,
                                     cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,40,5), cglwd=2,
                                     vlcex=1,title = "Age Ranges (Lethal Cases), (%)"))

#Comorbidities
x1<-prop.table(table(covid_casos_comorb$diabetes,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x2<-prop.table(table(covid_casos_comorb$hipertension,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x3<-prop.table(table(covid_casos_comorb$obesidad,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x4<-prop.table(table(covid_casos_comorb$enfermedad_cardiaca,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x5<-prop.table(table(covid_casos_comorb$tabaquismo,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x6<-prop.table(table(covid_casos_comorb$insuficiencia_renal_cronica,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x7<-prop.table(table(covid_casos_comorb$epoc,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x8<-prop.table(table(covid_casos_comorb$asma,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x9<-prop.table(table(covid_casos_comorb$inmunosupresivo,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x10<-prop.table(table(covid_casos_comorb$VIH_SIDA,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x11<-prop.table(table(covid_casos_comorb$otra_condicion,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100


p1<-x1[c(2),]
p2<-x2[c(2),]
p3<-x3[c(2),]
p4<-x4[c(2),]
p5<-x5[c(2),]
p6<-x6[c(2),]
p7<-x7[c(2),]
p8<-x8[c(2),]
p9<-x9[c(2),]
p10<-x10[c(2),]
p11<-x11[c(2),]

table_comorb<-matrix(c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11),ncol=11,byrow=F)
rownames(table_comorb) <-  c("HCWs", "Students", 
                             "Economically-Active", "Other Non-Specified Workers",
                             "Home-Related Workers", "Retired Adults", "Unemployed")
colnames(table_comorb) <- c("Diabetes","Arterial Hypertension","Obesity","CVD","Smoking Status","CKD","COPD","Asthma","Immunosuppression","HIV/AIDS","Other conditions")

table_comorb_2<-as.data.frame(table_comorb)
table_comorb_3<- rbind(rep(55,5) , rep(0,5) , table_comorb_2)
table_comorb_3<-as.data.frame(table_comorb_3)

sup_6_h<-as.ggplot(~fmsb::radarchart(table_comorb_3,axistype=1, maxmin=T,
                                     pcol=c("#3a4e56","#e1a86a","#31a0d8", "#ab4844","#80af96","#69659c","#80796a"), plwd=3 , plty=1,
                                     cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,50,10), cglwd=2, 
                                     vlcex=1,title = "Prevalence of Comorbidities (Lethal Cases), (%)"))
#Radar plot de sintomas

x1<-prop.table(table(covid_casos_comorb$fiebre,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x2<-prop.table(table(covid_casos_comorb$tos,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x3<-prop.table(table(covid_casos_comorb$disnea,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x4<-prop.table(table(covid_casos_comorb$irritabilidad,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x5<-prop.table(table(covid_casos_comorb$odinofagia,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x6<-prop.table(table(covid_casos_comorb$diarrea,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x7<-prop.table(table(covid_casos_comorb$dolor_toracico,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x8<-prop.table(table(covid_casos_comorb$calofrios,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x9<-prop.table(table(covid_casos_comorb$cefalea,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x10<-prop.table(table(covid_casos_comorb$mialgias,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x11<-prop.table(table(covid_casos_comorb$artralgias,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x12<-prop.table(table(covid_casos_comorb$ataque_al_estado_general,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x13<-prop.table(table(covid_casos_comorb$rinorrea,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x14<-prop.table(table(covid_casos_comorb$polipnea,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x15<-prop.table(table(covid_casos_comorb$vomito,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x16<-prop.table(table(covid_casos_comorb$conjuntivitis,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x17<-prop.table(table(covid_casos_comorb$cianosis,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100
x18<-prop.table(table(covid_casos_comorb$inicio_subito_sintomas,covid_casos_comorb$CAT_TRABAJO_COVID_2),2)*100

#Grafica Agrupada
p1<-x1[c(2),]
p2<-x2[c(2),]
p3<-x3[c(2),]
p4<-x4[c(2),]
p5<-x5[c(2),]
p6<-x6[c(2),]
p7<-x7[c(2),]
p8<-x8[c(2),]
p9<-x9[c(2),]
p10<-x10[c(2),]
p11<-x11[c(2),]
p12<-x12[c(2),]
p13<-x13[c(2),]
p14<-x14[c(2),]
p15<-x15[c(2),]
p16<-x16[c(2),]
p17<-x17[c(2),]
p18<-x18[c(2),]

table_sintomas<-matrix(c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18),ncol=18,byrow=F)
rownames(table_sintomas) <-  c("HCWs", "Students", 
                             "Economically-Active", "Other Non-Specified Workers",
                             "Home-Related Workers", "Retired Adults", "Unemployed")

colnames(table_sintomas) <- c("Fever","Cought", "Dysnea","Irritability",
                              "Odynophagia","Diarrhea","Chest Pain", "Shivering",
                              "Headache","Myalgias","Arthralgias","Dizziness",
                              "Rhinorrhea","Polypnea","Vomiting","Conjunctivitis","Cyanosis","Sudden Onset")
table_sintomas_2<-as.data.frame(table_sintomas)
table_sintomas_3<- rbind(rep(100,5) , rep(0,5) , table_sintomas_2)
table_sintomas_3<-as.data.frame(table_sintomas_3)

sup_6_i<-as.ggplot(~fmsb::radarchart(fmsb::radarchart(table_sintomas_3,axistype=1, maxmin=T,
                                                      pcol=c("#3a4e56","#e1a86a","#31a0d8", "#ab4844","#80af96","#69659c","#80796a"), plwd=4 , plty=1,
                                                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=2,
                                                      vlcex=1,title = "Prevalence of Symptoms (Lethal Cases), (%)")))

Supplementary_Figure_6<-ggarrange(sup_6_a,sup_6_c,sup_6_e,
                                    sup_6_b,sup_6_d,sup_6_f,
                                    sup_6_g,sup_6_i,sup_6_h,
                                    ncol=3,nrow = 3,labels=LETTERS[1:9], common.legend = T)

ggsave(Supplementary_Figure_6,
       filename = "Supplementary_Figure_6.png", 
       width = 50, 
       height = 40,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


####Age, comorbidities and symtomatology profile by SLI categories (Suplementary Figure 7)#####

demo_2<-demo%>%dplyr::select(id,dmu)
marg4<-marg%>%dplyr::select(id,marg)
dmu_cuadrantes<-data.frame(id=9002:9017)
dmu_cuadrantes$id=str_pad(dmu_cuadrantes$id, 5,pad = "0")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(demo_2,by="id")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(marg4,by="id")
dmu_cuadrantes$DISLI<-lm(marg~dmu, data = dmu_cuadrantes)$residuals
dmu_cuadrantes<-dmu_cuadrantes%>%mutate(Cuadrante = factor(ifelse( (dmu<150 & DISLI < -0.103),
                                                                   "Low Density/Low DISLI",
                                                                   ifelse((dmu<150 & DISLI >= -0.103),
                                                                          "Low Density/High DISLI",
                                                                          ifelse((dmu>=150 & DISLI < -0.103),
                                                                                 "High Density/Low DISLI",
                                                                                 ifelse((dmu>=150 & DISLI>= -0.103),
                                                                                        "High Density/High DISLI", "Invalid"))))))

covid_casos_comorb<- covid_casos %>% filter(cve_entidad_residencia=="9") %>% left_join(dmu_cuadrantes %>% dplyr::select(c("id","Cuadrante")), by =c("id")) %>%
  filter(!is.na(marg)) %>% filter(covid==1) %>% filter(evolucion_casoDEFUNCION==1) 
graf_1<-covid_casos_comorb%>% dplyr::select(edad,comorb,numero_sintomas,Cuadrante)%>%drop_na()
graf_1$Cuadrante<-as.factor(graf_1$Cuadrante)
levels(graf_1$Cuadrante)<-c("High Density/High DISLI","High Density/Low DISLI","Low Density/High DISLI","Low Density/Low DISLI")
my_comparisons <- list( c("High Density/High DISLI", "High Density/Low DISLI"), c("High Density/High DISLI", "Low Density/High DISLI"), c("High Density/High DISLI", "Low Density/Low DISLI"),
                        c("High Density/Low DISLI", "Low Density/High DISLI"),c("High Density/Low DISLI", "Low Density/Low DISLI"),
                        c("Low Density/High DISLI", "Low Density/Low DISLI"))

covid_casos_comorb$edad_cat<-NULL;
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=0 & covid_casos_comorb$edad<=9]<-1
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=10 & covid_casos_comorb$edad<=19]<-2
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=20 & covid_casos_comorb$edad<=29]<-3
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=30 & covid_casos_comorb$edad<=39]<-4
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=40 & covid_casos_comorb$edad<=49]<-5
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=50 & covid_casos_comorb$edad<=59]<-6
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=60 & covid_casos_comorb$edad<=69]<-7
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=70 & covid_casos_comorb$edad<=79]<-8
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=80 & covid_casos_comorb$edad<=89]<-9
covid_casos_comorb$edad_cat[covid_casos_comorb$edad>=90]<-10


sup_5_a<-ggplot(graf_1, aes(x=edad, fill=Cuadrante))+
  geom_histogram(aes(y = (..count..)/sum(..count..)),col="black", binwidth = 6,stat="count")+
  xlab("Age (years)")+
  ylab("Relative Frequencies
(Lethal Cases)")+
  theme_hc()+
  scale_y_continuous(labels=scales::percent) +
  labs(fill="Mean Urban Density & Density-Independent SLI categories")+
  theme(legend.position="top")+
  scale_fill_viridis_d(option = "viridis")

sup_5_b<-ggplot(graf_1,aes(x=Cuadrante,y=edad,fill=Cuadrante))+
  geom_violin(alpha=0.8)+
  geom_boxplot(width=0.3)+
  xlab("SLI-Categories")+
  ylab("Age (years)
(Lethal Cases)")+
  theme_hc()+
  labs(fill="Mean Urban Density & Density-Independent SLI categories")+
  theme(legend.position="top")+
  scale_fill_viridis_d(option = "viridis")+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif")+ 
  stat_compare_means(label.y = 150)

sup_5_c<-ggplot(graf_1, aes(x=numero_sintomas, fill=Cuadrante))+
  geom_histogram(aes(y = (..count..)/sum(..count..)),col="black", binwidth = 6,stat="count")+
  xlab("Number of Symptoms")+
  ylab("Relative Frequencies
(Lethal Cases)")+
  theme_hc()+
  scale_y_continuous(labels=scales::percent) +
  labs(fill="Mean Urban Density & Density-Independent SLI categories")+
  theme(legend.position="top")+
  scale_fill_viridis_d(option = "viridis")

sup_5_d<-ggplot(graf_1,aes(x=Cuadrante,y=numero_sintomas,fill=Cuadrante))+
  geom_violin(alpha=0.8)+
  geom_boxplot(width=0.3)+
  labs(fill="Mean Urban Density & Density-Independent SLI categories")+
  ylab("Number of Symptoms
(Lethal Cases)")+
  theme_hc()+
  labs(fill="SLI-Categories")+
  theme(legend.position="top")+
  scale_fill_viridis_d(option = "viridis")+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif")+ 
  stat_compare_means(label.y = 25)

sup_5_e<-ggplot(graf_1, aes(x=comorb, fill=Cuadrante))+
  geom_histogram(aes(y = (..count..)/sum(..count..)),col="black", binwidth = 6,stat="count")+
  xlab("Comorbid Conditions")+
  ylab("Relative Frequencies
(Lethal Cases)")+
  theme_hc()+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks = seq(from = 0, to = 18, by = 1))+
  labs(fill="Mean Urban Density & Density-Independent SLI categories")+
  theme(legend.position="top")+
  scale_fill_viridis_d(option = "viridis")


sup_5_f<-ggplot(graf_1,aes(x=Cuadrante,y=comorb,fill=Cuadrante))+
  geom_violin(alpha=0.8)+
  geom_boxplot(width=0.3)+
  xlab("SLI-Categories")+
  ylab("Comorbid Conditions
(Lethal Cases)")+
  theme_hc()+
  scale_y_continuous(limits = c(-2,15))+
  labs(fill="Mean Urban Density & Density-Independent SLI categories")+
  theme(legend.position="top")+
  scale_fill_viridis_d(option = "viridis")+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif")+ 
  stat_compare_means(label.y = 15)

#Age
table_age<-prop.table(table(covid_casos_comorb$Cuadrante,covid_casos_comorb$edad_cat),1)*100
colnames(table_age) <- c("<10 y.o","11-19 y.o","20-29 y.o","30-39 y.o","40-49 y.o","50-59 y.o","60-69 y.o","70-79 y.o","80-89 y.o",">90 y.o")
table_age_2<- rbind(rep(30,10) , rep(0,10) , table_age)
table_age_3<-as.data.frame(table_age_2)
sup_5_g<-as.ggplot(~fmsb::radarchart(table_age_3,axistype=1, maxmin=T,
                                     pcol=c("#440854","#016991","#35b779","#fde725"), plwd=4 , plty=1,
                                     cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,25,5), cglwd=2,
                                     vlcex=1,title = "Age Ranges (Lethal Cases), (%)"))

#Comorbidities

x1<-prop.table(table(covid_casos_comorb$diabetes,covid_casos_comorb$Cuadrante),2)*100
x2<-prop.table(table(covid_casos_comorb$hipertension,covid_casos_comorb$Cuadrante),2)*100
x3<-prop.table(table(covid_casos_comorb$obesidad,covid_casos_comorb$Cuadrante),2)*100
x4<-prop.table(table(covid_casos_comorb$enfermedad_cardiaca,covid_casos_comorb$Cuadrante),2)*100
x5<-prop.table(table(covid_casos_comorb$tabaquismo,covid_casos_comorb$Cuadrante),2)*100
x6<-prop.table(table(covid_casos_comorb$insuficiencia_renal_cronica,covid_casos_comorb$Cuadrante),2)*100
x7<-prop.table(table(covid_casos_comorb$epoc,covid_casos_comorb$Cuadrante),2)*100
x8<-prop.table(table(covid_casos_comorb$asma,covid_casos_comorb$Cuadrante),2)*100
x9<-prop.table(table(covid_casos_comorb$inmunosupresivo,covid_casos_comorb$Cuadrante),2)*100
x10<-prop.table(table(covid_casos_comorb$VIH_SIDA,covid_casos_comorb$Cuadrante),2)*100
x11<-prop.table(table(covid_casos_comorb$otra_condicion,covid_casos_comorb$Cuadrante),2)*100


p1<-x1[c(2),]
p2<-x2[c(2),]
p3<-x3[c(2),]
p4<-x4[c(2),]
p5<-x5[c(2),]
p6<-x6[c(2),]
p7<-x7[c(2),]
p8<-x8[c(2),]
p9<-x9[c(2),]
p10<-x10[c(2),]
p11<-x11[c(2),]

table_comorb<-matrix(c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11),ncol=11,byrow=F)
rownames(table_comorb) <- c("High Density/High DISLI","High Density/Low DISLI","Low Density/High DISLI","Low Density/Low DISLI")
colnames(table_comorb) <- c("Diabetes","Arterial Hypertension","Obesity","CVD","Smoking Status","CKD","COPD","Asthma","Immunosuppression","HIV/AIDS","Other conditions")

table_comorb_2<-as.data.frame(table_comorb)
table_comorb_3<- rbind(rep(50,5) , rep(0,5) , table_comorb_2)
table_comorb_3<-as.data.frame(table_comorb_3)
sup_5_h<-as.ggplot(~fmsb::radarchart(table_comorb_3,axistype=1, maxmin=T,
                                     pcol=c("#440854","#016991","#35b779","#fde725") , plwd=3 , plty=1,
                                     cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,50,10), cglwd=2, 
                                     vlcex=1,title = "Prevalence of Comorbidities (Lethal Cases), (%)"))

#Radar plot de sintomas

x1<-prop.table(table(covid_casos_comorb$fiebre,covid_casos_comorb$Cuadrante),2)*100
x2<-prop.table(table(covid_casos_comorb$tos,covid_casos_comorb$Cuadrante),2)*100
x3<-prop.table(table(covid_casos_comorb$disnea,covid_casos_comorb$Cuadrante),2)*100
x4<-prop.table(table(covid_casos_comorb$irritabilidad,covid_casos_comorb$Cuadrante),2)*100
x5<-prop.table(table(covid_casos_comorb$odinofagia,covid_casos_comorb$Cuadrante),2)*100
x6<-prop.table(table(covid_casos_comorb$diarrea,covid_casos_comorb$Cuadrante),2)*100
x7<-prop.table(table(covid_casos_comorb$dolor_toracico,covid_casos_comorb$Cuadrante),2)*100
x8<-prop.table(table(covid_casos_comorb$calofrios,covid_casos_comorb$Cuadrante),2)*100
x9<-prop.table(table(covid_casos_comorb$cefalea,covid_casos_comorb$Cuadrante),2)*100
x10<-prop.table(table(covid_casos_comorb$mialgias,covid_casos_comorb$Cuadrante),2)*100
x11<-prop.table(table(covid_casos_comorb$artralgias,covid_casos_comorb$Cuadrante),2)*100
x12<-prop.table(table(covid_casos_comorb$ataque_al_estado_general,covid_casos_comorb$Cuadrante),2)*100
x13<-prop.table(table(covid_casos_comorb$rinorrea,covid_casos_comorb$Cuadrante),2)*100
x14<-prop.table(table(covid_casos_comorb$polipnea,covid_casos_comorb$Cuadrante),2)*100
x15<-prop.table(table(covid_casos_comorb$vomito,covid_casos_comorb$Cuadrante),2)*100
x16<-prop.table(table(covid_casos_comorb$conjuntivitis,covid_casos_comorb$Cuadrante),2)*100
x17<-prop.table(table(covid_casos_comorb$cianosis,covid_casos_comorb$Cuadrante),2)*100
x18<-prop.table(table(covid_casos_comorb$inicio_subito_sintomas,covid_casos_comorb$Cuadrante),2)*100

#Grafica Agrupada
p1<-x1[c(2),]
p2<-x2[c(2),]
p3<-x3[c(2),]
p4<-x4[c(2),]
p5<-x5[c(2),]
p6<-x6[c(2),]
p7<-x7[c(2),]
p8<-x8[c(2),]
p9<-x9[c(2),]
p10<-x10[c(2),]
p11<-x11[c(2),]
p12<-x12[c(2),]
p13<-x13[c(2),]
p14<-x14[c(2),]
p15<-x15[c(2),]
p16<-x16[c(2),]
p17<-x17[c(2),]
p18<-x18[c(2),]

table_sintomas<-matrix(c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18),ncol=18,byrow=F)
rownames(table_sintomas) <- c("High Density/High DISLI","High Density/Low DISLI","Low Density/High DISLI","Low Density/Low DISLI")
colnames(table_sintomas) <- c("Fever","Cought", "Dysnea","Irritability",
                              "Odynophagia","Diarrhea","Chest Pain", "Shivering",
                              "Headache","Myalgias","Arthralgias","Dizziness",
                              "Rhinorrhea","Polypnea","Vomiting","Conjunctivitis","Cyanosis","Sudden Onset")
table_sintomas_2<-as.data.frame(table_sintomas)
table_sintomas_3<- rbind(rep(100,5) , rep(0,5) , table_sintomas_2)
table_sintomas_3<-as.data.frame(table_sintomas_3)
sup_5_i<-as.ggplot(~fmsb::radarchart(fmsb::radarchart(table_sintomas_3,axistype=1, maxmin=T,
                                                      pcol=c("#440854","#016991","#35b779","#fde725"), plwd=4 , plty=1,
                                                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=2,
                                                      vlcex=1,title = "Prevalence of Symptoms (Lethal Cases), (%)")))

Supplementary_Figure_7<-ggarrange(sup_5_a,sup_5_c,sup_5_e,
                                  sup_5_b,sup_5_d,sup_5_f,
                                  sup_5_g,sup_5_h,sup_5_i,
                                  ncol=3,nrow = 3,labels=LETTERS[1:9], common.legend = T)

ggsave(Supplementary_Figure_7,
       filename = "Supplementary_Figure_7.png", 
       width = 50, 
       height = 40,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

####Influence of ambulatory setting on adverse outcomes  (Supplementary_Figure_8)#####

m6<-coxme::coxme(formula = Surv(FU_time_MORT, evolucion_casoDEFUNCION) ~ scale(edad)+(sexo=="MASCULINO")+scale(comorb)+scale(numero_sintomas)+(Cuadrantes)+(HOSP==0)*CAT_TRABAJO_COVID_3+(1|clave_localidad_residencia), data = covid_casos_15)
summary(m6)
exp(confint(m6))

Supplementary_Figure_8<-ggstatsplot::ggcoefstats(
  x = m6,
  exp=T,
  conf.int = TRUE,
  package = "RColorBrewer",
  vline = F,
  only.significant = T,
  palette = "Dark2",
  ggtheme = ggthemes::theme_hc(),
  title = "Risk for Letality for COVID-19 in an Ambulatory Setting",
  xlab = "Random Effects Cox Proportional Hazards Regression Model; HR (95% CI)",
  ylab = "")+
  scale_x_log10(limits = c(-1, 5))+
  ggplot2::scale_y_discrete(labels = c("Age","Male Sex", "Public Health-Care Setting", 
                                       "Number of Comorbidities", "Number of Symptoms",
                                       "High Density/Low DISLI","Low Density/High DISLI","High Density/High DISLI",
                                       "Students", 
                                       "Economically-Active", "Other Non-Specified Workers",
                                       "Home-Related Workers", "Retired Adults", "Unemployed",
                                       "Students treated in ambulatory setting", 
                                       "Economically-Active treated in ambulatory setting", "Other Non-Specified Workers treated in ambulatory setting",
                                       "Home-Related Workers treated in ambulatory setting", "Retired Adults treated in ambulatory setting", 
                                       "Unemployed treated in ambulatory setting"))+
  geom_vline(aes(xintercept = 1), size = 0.5, linetype = "dashed")

ggsave(Supplementary_Figure_8,
       filename = "Supplementary_Figure_8.png", 
       width = 50, 
       height = 25,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 

#Interaction with domestic workers

m5<-coxph(Surv(FU_time_MORT, evolucion_casoDEFUNCION)~comorb+numero_sintomas+privado+terciles_marg+(dmu>200)+(sexo=="FEMENINO")+(edad65==0)*factor(CAT_TRABAJO_COVID_3=="Home-Related Workers")*(HOSP==0)+frailty(clave_localidad_residencia), data=covid_casos_15)
summary(m5)
exp(confint(m5))








####Mexico City DISLI Maps (Suplementary Figure 11)------
#Map managment
Pob_edad_id<- Pob_conapo %>% filter(`A—O` == 2020 & CLAVE_ENT == 9) %>% group_by(CVE_MUN, EDAD_QUIN) %>%
  tally(POB) %>% rename(POB = n)  %>% rename(id = CVE_MUN) 

Defunciones_mun_edad<- covid_casos %>% 
  group_by(id, EDAD_QUIN = cut(as.numeric(edad), 
                               breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                               right = FALSE, include.lowest = TRUE, 
                               labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                          "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                          "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                          "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases_def=sum(evolucion_casoDEFUNCION==1 & covid==1, na.rm=T), 
            tested=n(),
            cases=sum(covid, na.rm=T),
            asinto=sum(asintomaticos==1 & covid==1))

mun_mort_ajustada<- Defunciones_mun_edad%>%left_join(Pob_edad_id, by =c("id","EDAD_QUIN"))
mun_mort_ajustada<-mun_mort_ajustada%>%left_join(Pob_edad_pais,by=c("EDAD_QUIN"))
mun_cent <- st_centroid(mx_mun)

mun_mort_ajustada1<- mun_mort_ajustada %>% mutate(INC_ESPER=(cases/POB.x)*POB.y,
                                                  TEST_ESPER=(tested/POB.x)*POB.y,
                                                  ASINTO_ESPER =(asinto/POB.x)*POB.y,
                                                  DEF_ESPER = (cases_def/POB.x)*POB.y)
mun_mort_ajustada2<- mun_mort_ajustada1 %>% group_by(id) %>% 
  summarise(INC_ESPER= sum(INC_ESPER), TEST_ESPER=sum(TEST_ESPER),
            ASINTO_ESPER=sum(ASINTO_ESPER),DEF_ESPER=sum(DEF_ESPER))%>%
  group_by(id)%>% 
  mutate(INC_AJUST = (INC_ESPER/9018645)*100000,
         TEST_AJUST = (TEST_ESPER/9018645)*100000,
         ASINTO_AJUST = (ASINTO_ESPER/9018645)*100000,
         DEF_AJUST = (DEF_ESPER/9018645)*100000)%>%
  left_join(mun_cent,by="id")


Residuals_incidence_map<- mx_mun %>% filter(CVE_ENT == "09") %>% dplyr::select(id,geometry) %>%
  left_join(dmu_cuadrantes, by = "id" )%>%
  ggplot() + 
  geom_sf(aes(fill = Cuadrante,geometry=geometry)) + 
  geom_sf(data = mun_mort_ajustada2, aes(size = INC_AJUST,geometry=geometry),show.legend = "point", inherit.aes = F, color= "red") +
  scale_size(range = c(0.1,10)) +
  theme_map()+
  scale_fill_viridis_d(option = "viridis") +
  labs(fill = "Mean Urban Density & Density-Independent SLI categories",
       size = "Age-adjusted COVID Incidence Rate\nper 100,000 inhabitants") +
  theme(plot.title = element_text(hjust=0.1),
        legend.title = element_text(hjust = 0.1),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),size=guide_legend(nrow=2,byrow=T))

Residuals_testing_map<- mx_mun %>% filter(CVE_ENT == "09") %>% dplyr::select(id,geometry) %>%
  left_join(dmu_cuadrantes, by = "id" )%>%
  ggplot() + 
  geom_sf(aes(fill = Cuadrante,geometry=geometry)) + 
  geom_sf(data = mun_mort_ajustada2, aes(size = TEST_AJUST,geometry=geometry),show.legend = "point", inherit.aes = F, color= "red") +
  scale_size(range = c(0.1,10)) +
  theme_map()+
  scale_fill_viridis_d(option = "viridis") +
  labs(fill = "Mean Urban Density & Density-Independent SLI categories",
       size = "Age-adjusted COVID Testing Rate\nper 100,000 inhabitants") +
  theme(plot.title = element_text(hjust=0.1),
        legend.title = element_text(hjust = 0.1),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),size=guide_legend(nrow=2,byrow=T))


Residuals_asinto_map<- mx_mun %>% filter(CVE_ENT == "09") %>% dplyr::select(id,geometry) %>%
  left_join(dmu_cuadrantes, by = "id" )%>%
  ggplot() + 
  geom_sf(aes(fill = Cuadrante,geometry=geometry)) + 
  geom_sf(data = mun_mort_ajustada2, aes(size = ASINTO_AJUST,geometry=geometry),show.legend = "point", inherit.aes = F, color= "red") +
  scale_size(range = c(0.1,10)) +
  theme_map()+
  scale_fill_viridis_d(option = "viridis") +
  labs(fill = "Mean Urban Density & Density-Independent SLI categories",
       size = "Age-adjusted COVID Asymptomatic Rate\nper 100,000 inhabitants") +
  theme(plot.title = element_text(hjust=0.1),
        legend.title = element_text(hjust = 0.1),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),size=guide_legend(nrow=2,byrow=T))


Residuals_mort_map<- mx_mun %>% filter(CVE_ENT == "09") %>% dplyr::select(id,geometry) %>%
  left_join(dmu_cuadrantes, by = "id" )%>%
  ggplot() + 
  geom_sf(aes(fill = Cuadrante,geometry=geometry)) + 
  geom_sf(data = mun_mort_ajustada2, aes(size = DEF_AJUST,geometry=geometry),show.legend = "point", inherit.aes = F, color= "red") +
  scale_size(range = c(0.1,10)) +
  theme_map()+
  scale_fill_viridis_d(option = "viridis") +
  labs(fill = "Mean Urban Density & Density-Independent SLI categories",
       size = "Age-adjusted COVID Mortality Rate\nper 100,000 inhabitants") +
  theme(plot.title = element_text(hjust=0.1),
        legend.title = element_text(hjust = 0.1),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),size=guide_legend(nrow=2,byrow=T))

Supplementary_Figure_11<-ggarrange(Residuals_incidence_map,Residuals_testing_map,Residuals_asinto_map, Residuals_mort_map, ncol = 2, nrow = 2, labels = c("A","B","C","D"),common.legend = F)

ggsave(Supplementary_Figure_11,
       filename = "Supplementary_Figure_11.png", 
       width = 60, 
       height = 30,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 

####Age Adjusted Mortality Rate in Death Certificates (Supplementary Figure 12)#####

Supplementary_Figure_10<-mun_mort_ajustada2 %>% 
  ggplot(aes(x=factor(week), y=DEF_AJUST, fill=factor(Cuadrante)))+
  geom_bar(stat = "identity",position = "dodge")+
  theme_hc()+
  ylab("Age Adjusted COVID-19 Mortality in Death Certificates,
(Rate per 100,000 inhabitants)")+
  xlab("Month")+
  guides(col=guide_legend(nrow=2,byrow=TRUE))+
  labs(fill="Mean Urban Density & Density-Independent SLI categories")+
  scale_fill_viridis_d()

ggsave(Supplementary_Figure_10,
       filename = "Supplementary_Figure_10.png", 
       width = 60, 
       height = 30,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 
#####Impact of SLI on Excess Mortality (Supplementary Figure 13)####

nonCOVID_excess_cuadrante_edad_2020<- excess2 %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante,DISLI)),by="id")%>%
  filter(fec_defuncion>="2020-01-01") %>%
  filter(fec_defuncion<="2021-01-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(year,week, Cuadrante, EDAD_QUIN = cut(as.numeric(edad), 
                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                 right = FALSE, include.lowest = TRUE, 
                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa=="Otra"),
            DISLI=median(DISLI),
            DISLI=median(DISLI),
            ind_amb_no_covid=sum(lugar_muerte=="Domicilio" & causa=="Otra" ,na.rm = T),
            ind_hosp_no_covid=sum(lugar_muerte=="Hospital" & causa=="Otra" ,na.rm = T),
            ind_amb_covid=sum(lugar_muerte=="Domicilio" & causa!="Otra" ,na.rm = T),
            ind_hosp_covid=sum(lugar_muerte=="Hospital" & causa!="Otra" ,na.rm = T))%>%
  mutate(ind_amb_hosp_no_covid=ind_amb_no_covid/ind_hosp_no_covid,
         ind_amb_hosp_covid=ind_amb_covid/ind_hosp_covid)%>%
  na.omit()

cuadrante_nonCOVID_mort_ajustada_2020<- nonCOVID_excess_cuadrante_edad_2020%>%left_join(Pob_edad_cuadrante, by =c("Cuadrante","EDAD_QUIN"))
cuadrante_nonCOVID_mort_ajustada_2020<-cuadrante_nonCOVID_mort_ajustada_2020%>%left_join(Pob_edad_pais_cuadrante,by=c("EDAD_QUIN"))

#Age adjusted dataset in death certificates
non_covid_mun_mort_ajustada1<- cuadrante_nonCOVID_mort_ajustada_2020 %>% mutate(DEF_ESPER = (cases/POB.x)*POB.y)
non_covid_mun_mort_ajustada2<- non_covid_mun_mort_ajustada1 %>% group_by(Cuadrante,year, week) %>% 
  mutate(DEF_ESPER=sum(DEF_ESPER,na.rm = T))%>%
  group_by(Cuadrante, year, week)%>% 
  mutate(DEF_AJUST_2020 = (DEF_ESPER/9018645)*100000)


#Non-COVID deaths 2017-2019
#Populational DISLI arrangement 2017-2019

Pob_edad_pais<-Pob_conapo%>%filter(!is.na(marg))
Pob_edad_cuadrante_anual<- Pob_conapo %>% filter(`A—O` %in% c(2017,2018,2019) & CLAVE_ENT == 9) %>% group_by(`A—O`,CVE_MUN, EDAD_QUIN) %>%
  tally(POB) %>% rename(POB = n) %>% rename(year = `A—O`)%>%
  left_join(dmu_cuadrantes %>% dplyr::select(c("id","Cuadrante")), by =c("CVE_MUN"="id")) %>%
  group_by(year,Cuadrante, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))
Pob_edad_pais_anual<- Pob_edad_pais %>% filter(`A—O` %in% c(2017,2018,2019)) %>% rename(year = `A—O`) %>% group_by(year,EDAD_QUIN) %>% tally(POB) %>% rename(POB = n) 

#Estimating mortality excess
nonCOVID_excess_cuadrante_edad_2017<- excess2 %>% 
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2017-01-01") %>%
  filter(fec_defuncion<="2019-12-31") %>%
  mutate(week=data.table::month(fec_defuncion),year=data.table::year(fec_defuncion))%>%
  group_by(year,week, Cuadrante, EDAD_QUIN = cut(as.numeric(edad), 
                                                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf), 
                                                 right = FALSE, include.lowest = TRUE, 
                                                 labels = c("pobm_00_04", "pobm_05_09", "pobm_10_14","pobm_15_19", 
                                                            "pobm_20_24", "pobm_25_29", "pobm_30_34", "pobm_35_39", 
                                                            "pobm_40_44", "pobm_45_49","pobm_50_54", "pobm_55_59", 
                                                            "pobm_60_64", "pobm_65_mm"))) %>%
  summarise(cases=sum(causa=="Otra"))%>%
  na.omit()

cuadrante_nonCOVID_mort_ajustada_2017<- nonCOVID_excess_cuadrante_edad_2017%>%left_join(Pob_edad_cuadrante_anual, by =c("year","Cuadrante","EDAD_QUIN"))
cuadrante_nonCOVID_mort_ajustada_2017<-cuadrante_nonCOVID_mort_ajustada_2017%>%left_join(Pob_edad_pais_anual,by=c("year","EDAD_QUIN"))

#Age adjusted dataset in death certificates
non_covid_mun_mort_ajustada1_2017<- cuadrante_nonCOVID_mort_ajustada_2017 %>% mutate(DEF_ESPER = (cases/POB.x)*POB.y)
non_covid_mun_mort_ajustada2_2017<- non_covid_mun_mort_ajustada1_2017 %>% group_by(year,Cuadrante, week) %>% 
  summarise(DEF_ESPER=sum(DEF_ESPER,na.rm = T))%>%
  group_by(year, Cuadrante, week)
non_covid_mun_mort_ajustada2_2017$pop_total<-NULL
non_covid_mun_mort_ajustada2_2017$pop_total[non_covid_mun_mort_ajustada2_2017$year==2017]<-9049086
non_covid_mun_mort_ajustada2_2017$pop_total[non_covid_mun_mort_ajustada2_2017$year==2018]<-9041395
non_covid_mun_mort_ajustada2_2017$pop_total[non_covid_mun_mort_ajustada2_2017$year==2019]<-9031213
non_covid_mun_mort_ajustada2_2017<-non_covid_mun_mort_ajustada2_2017%>% mutate(DEF_AJUST = (DEF_ESPER/pop_total)*100000)%>%group_by(Cuadrante,week)%>%summarise(DEF_AJUST_2017=mean(DEF_AJUST))

#Final dataset

age_adjusted_excess_mortality<-non_covid_mun_mort_ajustada2%>%left_join(non_covid_mun_mort_ajustada2_2017,by=c("Cuadrante","week"))%>%mutate(excess_mort=(DEF_AJUST_2020-DEF_AJUST_2017)+67.612)
age_adjusted_excess_mortality$week<-as.factor(age_adjusted_excess_mortality$week)
age_adjusted_excess_mortality$week[age_adjusted_excess_mortality$year==2021 & age_adjusted_excess_mortality$week==1]<-13
age_adjusted_excess_mortality$week<-na.tools::na.replace(age_adjusted_excess_mortality$week,13)
age_adjusted_excess_mortality$week<-factor(age_adjusted_excess_mortality$week, labels =  c("Jan-20'","Feb-20'","Mar-20'", "Apr-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'","Jan-21'"))

#Model to estimate excess mortality

age_adjusted_excess_mortality$ind_amb_hosp_no_covid[is.infinite(age_adjusted_excess_mortality$ind_amb_hosp_no_covid)]<-0
age_adjusted_excess_mortality$ind_amb_hosp_covid[is.infinite(age_adjusted_excess_mortality$ind_amb_hosp_covid)]<-0

m3<-MASS::glm.nb(excess_mort~poly(DISLI,3)+offset(log(POB.x)), data=age_adjusted_excess_mortality)
summary(m3)
anova(m3)
summ(m3, confint = T, exp = T)

m3<-MASS::glm.nb(ind_amb_hosp_no_covid~poly(DISLI,3)+offset(log(POB.x)), data=age_adjusted_excess_mortality)
summary(m3)
summ(m3, confint = T, exp = T)

m3<-MASS::glm.nb(ind_amb_hosp_covid~poly(DISLI,3)+offset(log(POB.x)), data=age_adjusted_excess_mortality)
summary(m3)
summ(m3, confint = T, exp = T)

Sup_Fig_1<-age_adjusted_excess_mortality%>%
  na.omit()%>%
  ggplot(aes(DISLI,ind_amb_hosp_covid))+
  geom_smooth(method =glm, formula = y~poly(x,3), color="red")+
  theme_hc()+
  ylab("Ambulatory-To-Hospitalarian Rate 
(COVID deaths)")+
  xlab("Density Indepedent Social Lag Index")

Sup_Fig_2<-age_adjusted_excess_mortality%>%
  na.omit()%>%
  ggplot(aes(DISLI,ind_amb_hosp_no_covid))+
  geom_smooth(method =glm, formula = y~poly(x,3), color="red")+
  theme_hc()+
  ylab("Ambulatory-To-Hospitalarian Rate 
(Non-COVID deaths)")+
  xlab("Density Indepedent Social Lag Index")

Sup_Fig_3<-age_adjusted_excess_mortality%>%
  na.omit()%>%
  ggplot(aes(DISLI,excess_mort))+
  geom_smooth(method =glm, formula = y~poly(x,3), color="red")+
  theme_hc()+
  ylab("Age Adjusted Excess Mortality Rate Non-COVID-19 deaths
(Rate per 100,000 inhabitants)")+
  xlab("Density Indepedent Social Lag Index")

Sup_Fig_13<-ggarrange(Sup_Fig_1,Sup_Fig_2,Sup_Fig_3, ncol = 3, nrow = 1, labels = c("A","B","C"))

ggsave(Sup_Fig_13,
       filename = "Supplementary_Figure_13.png", 
       width = 60, 
       height = 15,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 

#####NOn-Covid 19 deahts by ambulatory/hospitalarian setting (Supplementary Figure 14)#####

dmu_cuadrantes<-data.frame(id=9002:9017)
dmu_cuadrantes$id=str_pad(dmu_cuadrantes$id, 5,pad = "0")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(demo_2,by="id")
dmu_cuadrantes<-dmu_cuadrantes%>%left_join(marg4,by="id")
dmu_cuadrantes$DISLI<-lm(marg~dmu, data = dmu_cuadrantes)$residuals
dmu_cuadrantes<-dmu_cuadrantes%>%mutate(Cuadrante = factor(ifelse( (dmu<150 & DISLI < -0.103),
                                                                   "Low Density/Low DISLI",
                                                                   ifelse((dmu<150 & DISLI >= -0.103),
                                                                          "Low Density/High DISLI",
                                                                          ifelse((dmu>=150 & DISLI < -0.103),
                                                                                 "High Density/Low DISLI",
                                                                                 ifelse((dmu>=150 & DISLI>= -0.103),
                                                                                        "High Density/High DISLI", "Invalid"))))))


excess6.2<- excess2 %>%
  mutate(week=data.table::month(fecha_defuncion))%>%
  mutate(year=data.table::year(fecha_defuncion))%>%
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2020-03-01") %>%
  filter(fec_defuncion<="2021-01-31") %>%
  group_by(lugar_muerte,week, year) %>% 
  summarise(covid= sum(!causa=="Otra"),
            no_covid= sum(causa=="Otra"),
            pop_1=median(pop3_2020),
            covid_rate_acta=sum(!causa=="Otra")/median(pop_1)*100000,
            no_covid_rate_acta= sum(causa=="Otra")/median(pop_1)*100000,
            dmu=median(dmu),
            male_sex=sum(sexo=="Hombre" & causa=="Otra"),
            female_sex=sum(sexo=="Mujer" & causa=="Otra"),
            dependent=sum((edad>=65 | edad<15 ) & causa=="Otra", na.rm = T),
            no_dependent=sum((edad<65 & edad>=15) & causa=="Otra" ,na.rm = T),
            marg=median(marg),
            ind_amb_no_covid=sum(lugar_muerte=="Domicilio" & causa=="Otra" ,na.rm = T),
            ind_hosp_no_covid=sum(lugar_muerte=="Hospital" & causa=="Otra" ,na.rm = T),
            ind_amb_covid=sum(lugar_muerte=="Domicilio" & causa!="Otra" ,na.rm = T),
            ind_hosp_covid=sum(lugar_muerte=="Hospital" & causa!="Otra" ,na.rm = T)) %>%
  mutate(ind_masc=male_sex/female_sex,
         ind_depend=dependent/no_dependent,
         ind_amb_hosp_no_covid=ind_amb_no_covid/ind_hosp_no_covid,
         ind_amb_hosp_covid=ind_amb_covid/ind_hosp_covid)%>%
  group_by(lugar_muerte,week, year) 


#Rate of ambulatory/hospital ratio

excess2$lugar_muerte_NA<-excess2$LugarMuerte
excess2$lugar_muerte_NA<-factor(excess2$lugar_muerte_NA,labels = c("Ambulatory","Hospital"))

excess6<- excess2 %>%
  left_join(dmu_cuadrantes%>%dplyr::select(c(id,Cuadrante)),by="id")%>%
  filter(fec_defuncion>="2020-03-01") %>%
  filter(fec_defuncion<="2020-12-31") %>%
  mutate(week=data.table::month(fec_defuncion))%>%
  mutate(year=data.table::year(fec_defuncion))%>%
  group_by(Cuadrante,week, year) %>% 
  summarise(covid= sum(!causa=="Otra"),
            no_covid= sum(causa=="Otra"),
            pop_1=median(pop2),
            covid_rate_acta=sum(!causa=="Otra")/median(pop_1)*100000,
            no_covid_rate_acta= sum(causa=="Otra")/median(pop_1)*100000,
            dmu=median(dmu),
            male_sex=sum(sexo=="Hombre" & causa=="Otra"),
            female_sex=sum(sexo=="Mujer" & causa=="Otra"),
            dependent=sum((edad>=65 | edad<15 ) & causa=="Otra", na.rm = T),
            no_dependent=sum((edad<65 & edad>=15) & causa=="Otra" ,na.rm = T),
            marg=median(marg),
            ind_amb_no_covid=sum(LugarMuerte=="Domicilio" & causa=="Otra" ,na.rm = T),
            ind_hosp_no_covid=sum(LugarMuerte=="Hospital" & causa=="Otra" ,na.rm = T),
            ind_amb_covid=sum(LugarMuerte=="Domicilio" & causa!="Otra" ,na.rm = T),
            ind_hosp_covid=sum(LugarMuerte=="Hospital" & causa!="Otra" ,na.rm = T)) %>%
  mutate(ind_masc=male_sex/female_sex,
         ind_depend=dependent/no_dependent,
         ind_amb_hosp_no_covid=ind_amb_no_covid/ind_hosp_no_covid,
         ind_amb_hosp_covid=ind_amb_covid/ind_hosp_covid)%>%
  group_by(Cuadrante,week, year) %>% 
  dplyr::select(Cuadrante,week,year, covid, ind_amb_covid, ind_hosp_covid, no_covid, no_covid_rate_acta,pop_1, dmu, marg, ind_masc,ind_depend,ind_amb_no_covid,ind_hosp_no_covid,ind_amb_hosp_no_covid,ind_amb_hosp_covid)

excess3_2020_4<-excess6 %>% 
  dplyr::select(Cuadrante,week,no_covid,no_covid_rate_acta,ind_masc,ind_depend,dmu,marg,pop_1,ind_amb_no_covid,ind_hosp_no_covid,ind_amb_hosp_no_covid,ind_amb_hosp_covid)%>%
  mutate(no_covid_rate_2020=no_covid_rate_acta,
         no_covid_2020=no_covid)%>%
  dplyr::select(Cuadrante,week,no_covid_2020,no_covid_rate_2020,ind_masc,ind_depend,marg,dmu,marg,pop_1,ind_amb_no_covid,ind_hosp_no_covid,ind_amb_hosp_no_covid,ind_amb_hosp_covid)

excess3_2020_4$week<-factor(excess3_2020_4$week, labels =  c("Mar-20'", "Apl-20'", "May-20'", "Jun-20'", "Jul-20'", "Aug-20'", "Sept-20'", "Oct-20'", "Nov-20'", "Dec-20'"))

#Descriptive Statistics
quantile(excess3_2020_4[excess3_2020_4$Cuadrante=="High Density/High DISLI",]$ind_amb_hosp_covid)
quantile(excess3_2020_4[excess3_2020_4$Cuadrante=="Low Density/High DISLI",]$ind_amb_hosp_covid)
quantile(excess3_2020_4[excess3_2020_4$Cuadrante=="High Density/Low DISLI",]$ind_amb_hosp_covid)
quantile(excess3_2020_4[excess3_2020_4$Cuadrante=="Low Density/High DISLI",]$ind_amb_hosp_covid)

quantile(excess3_2020_4[excess3_2020_4$Cuadrante=="High Density/High DISLI",]$ind_amb_hosp_no_covid)
quantile(excess3_2020_4[excess3_2020_4$Cuadrante=="Low Density/High DISLI",]$ind_amb_hosp_no_covid)
quantile(excess3_2020_4[excess3_2020_4$Cuadrante=="High Density/Low DISLI",]$ind_amb_hosp_no_covid)
quantile(excess3_2020_4[excess3_2020_4$Cuadrante=="Low Density/High DISLI",]$ind_amb_hosp_no_covid)


Sup_Figure_11A<-excess3_2020_4 %>% 
  ggplot(aes(x=factor(week), y=ind_amb_hosp_no_covid, fill=factor(Cuadrante)))+
  geom_bar(stat = "identity",position = "dodge")+
  theme_hc()+
  ylab("Non-COVID-19 deaths
Rate of Ambulatory/Hospitalarian")+
  xlab("Month, year 2020")+
  guides(col=guide_legend(nrow=2,byrow=TRUE))+
  labs(fill="SLI categories")+
  scale_fill_viridis_d()+
  geom_hline(aes(yintercept = 1),linetype="dashed",size=2)

Sup_Figure_11B<-excess3_2020_4 %>% 
  ggplot(aes(x=factor(week), y=ind_amb_hosp_covid, fill=factor(Cuadrante)))+
  geom_bar(stat = "identity",position = "dodge")+
  theme_hc()+
  ylab("COVID-19 deaths
Rate of Ambulatory/Hospitalarian")+
  xlab("Month, year 2020")+
  guides(col=guide_legend(nrow=2,byrow=TRUE))+
  labs(fill="SLI categories")+
  scale_fill_viridis_d()+
  geom_hline(aes(yintercept = 1),linetype="dashed",size=2)

Supplementary_Figure_14<-ggarrange(Sup_Figure_11A,Sup_Figure_11B, labels = c("A","B"), ncol = 2, nrow = 1)
ggsave(Supplementary_Figure_14,
       filename = "Supplementary_Figure_14.png", 
       width = 40, 
       height = 20,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE) 

