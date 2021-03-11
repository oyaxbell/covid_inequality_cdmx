#### Database management ####
library(tidyverse); library(dummies);library(lmerTest);library(lme4); library(polypoly); library(dummies); library(glmnet);
library(survival); library(survminer); library(jtools); library(caret); library(OptimalCutpoints); library(pROC); library(epiR); 
library(reportROC); library(haven); library(lme4); library(dplyr); library(sjPlot); library(dynlm); library(lubridate); library(lmtest); 
library(ARDL); library(dynamac); library(urca); library(ggfortify); library(forecast); library(fabricatr); library(ggstance); 
library(flextable); library(simPH); library(ggthemes); library(lme4); library(lmerTest); library(prismatic); library(sf); 
library(rnaturalearth); library(rnaturalearthdata); library(maps); library(rmapshaper); library(cowplot); library(corrplot); 
library(readxl); library("ggsci"); library(segmented); library(splitstackshape); library(data.table); library(MASS); library(rms);
library(effects)

setwd("~/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/Desigualdades CDMX")
cdmx<-read.csv("sisver_public.csv")
Chsq <- function(x){
  ## input is a row of your data
  ## creating a table from each row
  x <- matrix(x,byrow =TRUE,nrow=1)
  ### this will return the p value
  return(chisq.test(x)$p.value)
}

n<-c("id", "origen", "sector", "cve_entidad_unidad_medica", "entidad_unidad_medica", "delegacion_unidad_medica", "unidad_medica",
  "fecha_de_registro", "sexo", "entidad_residencia", "cve_entidad_residencia", "municipio_residencia", "cve_municipio_residencia",
  "localidad_residencia", "cve_localidad_residencia", "latloca", "longloca", "tipo_paciente", "evolucion_caso", "fecha_defuncion",
  "semana_defuncion", "defporinf", "defverifi", "intubado", "diagnostico_clinico_neumonia", "fecnaci", "edad", "nacionalidad",
  "esta_embarazada", "meses_embarazo", "es_indigena", "habla_lengua_indigena", "ocupacion", "servicio_ingreso", "fecha_ingreso",
  "fecha_inicio_sintomas", "diagnostico_probable", "fiebre", "tos", "odinofagia", "disnea", "irritabilidad", "diarrea", "dolor_toracico",
  "calofrios", "cefalea", "mialgias", "artralgias", "ataque_al_estado_general", "rinorrea", "polipnea", "vomito", "dolor_abdominal",
  "conjuntivitis", "cianosis", "inicio_subito_sintomas", "diabetes", "epoc", "asma", "inmunosupresivo", "hipertension", "VIH_SIDA",
  "otra_condicion", "enfermedad_cardiaca", "obesidad", "insuficiencia_renal_cronica", "tabaquismo", "recibio_tratamiento", 
  "recibio_tratamiento_antibiotico", "recibio_tratamiento_antiviral", "antiviral", "fecha_inicio_tratamiento_antiviral",
  "contacto_infeccion_viral", "contacto_aves", "contacto_cerdos", "contacto_animales", "vacunado", "fecha_estimada_vacunacion",
  "toma_muestra", "laboratorio", "folio_laboratorio", "resultado_definitivo", "es_migrante", "pais_nacionalidad", "pais_origen",
  "fecha_ingreso_pais", "paistran1","paistran2","paistran3","paistran4","puerperio", "dias_puerperio","antipireticos",
  "unidad_cuidados_intensivos","origen_datos","linaje_influenza_tipo_b","viaje_1","viaje_2","viaje_3","viaje_4", "viaje_5",
  "antigencovid","fecha_actualizacion")

colnames(cdmx)<-n

cdmx<-cdmx%>%dplyr::select(id, origen, sector, cve_entidad_unidad_medica, fecha_de_registro, sexo, cve_entidad_residencia, cve_municipio_residencia,
                           tipo_paciente, evolucion_caso, fecha_defuncion, intubado, diagnostico_clinico_neumonia, edad, esta_embarazada, meses_embarazo,
                           es_indigena, habla_lengua_indigena, ocupacion, fecha_ingreso, fecha_inicio_sintomas, diagnostico_probable, fiebre, tos, odinofagia,
                           disnea, irritabilidad, diarrea, dolor_toracico, calofrios, cefalea, mialgias, artralgias, ataque_al_estado_general, rinorrea, 
                           polipnea, vomito, dolor_abdominal, conjuntivitis, cianosis, inicio_subito_sintomas, diabetes, epoc, asma, inmunosupresivo,
                           hipertension, VIH_SIDA, otra_condicion, enfermedad_cardiaca, obesidad, insuficiencia_renal_cronica, tabaquismo, vacunado, 
                           fecha_estimada_vacunacion, resultado_definitivo, puerperio, dias_puerperio, unidad_cuidados_intensivos, antigencovid)

cdmx[cdmx=="SE IGNORA"]<-NA

cdmx$covid<-NULL
cdmx$covid[cdmx$resultado_definitivo=="SARS-CoV-2"]<-1; cdmx$covid[cdmx$resultado_definitivo!="SARS-CoV-2"]<-0
cdmx$diabetes[cdmx$diabetes!="SI"]<-0; cdmx$diabetes[cdmx$diabetes=="SI"]<-1; cdmx$diabetes[is.na(cdmx$diabetes)]<-0
cdmx$hipertension[cdmx$hipertension!="SI"]<-0; cdmx$hipertension[cdmx$hipertension=="SI"]<-1; cdmx$hipertension[is.na(cdmx$hipertension)]<-0
cdmx$enfermedad_cardiaca[cdmx$enfermedad_cardiaca!="SI"]<-0; cdmx$enfermedad_cardiaca[cdmx$enfermedad_cardiaca=="SI"]<-1; cdmx$enfermedad_cardiaca[is.na(cdmx$efermedad_cardiaca)]<-0
cdmx$insuficiencia_renal_cronica[cdmx$insuficiencia_renal_cronica!="SI"]<-0; cdmx$insuficiencia_renal_cronica[cdmx$insuficiencia_renal_cronica=="SI"]<-1; cdmx$insuficiencia_renal_cronica[is.na(cdmx$insuficiencia_renal_cronica)]<-0
cdmx$asma[cdmx$asma!="SI"]<-0; cdmx$asma[cdmx$asma=="SI"]<-1; cdmx$asma[is.na(cdmx$asma)]<-0
cdmx$inmunosupresivo[cdmx$inmunosupresivo!="SI"]<-0; cdmx$inmunosupresivo[cdmx$inmunosupresivo=="SI"]<-1; cdmx$inmunosupresivo[is.na(cdmx$inmunosupresivo)]<-0
cdmx$VIH_SIDA[cdmx$VIH_SIDA!="SI"]<-0; cdmx$VIH_SIDA[cdmx$VIH_SIDA=="SI"]<-1; cdmx$VIH_SIDA[is.na(cdmx$VIH_SIDA)]<-0
cdmx$epoc[cdmx$epoc!="SI"]<-0; cdmx$epoc[cdmx$epoc=="SI"]<-1; cdmx$epoc[is.na(cdmx$epoc)]<-0
cdmx$obesidad[cdmx$obesidad!="SI"]<-0; cdmx$obesidad[cdmx$obesidad=="SI"]<-1; cdmx$obesidad[is.na(cdmx$obesidad)]<-0
cdmx$tabaquismo[cdmx$tabaquismo!="SI"]<-0; cdmx$tabaquismo[cdmx$tabaquismo=="SI"]<-1; cdmx$tabaquismo[is.na(cdmx$tabaquismo)]<-0
cdmx$otra_condicion[cdmx$otra_condicion!="SI"]<-0; cdmx$otra_condicion[cdmx$otra_condicion=="SI"]<-1; cdmx$otra_condicion[is.na(cdmx$otra_condicion)]<-0
cdmx$intubado[cdmx$intubado!="SI"]<-0; cdmx$intubado[cdmx$intubado=="SI"]<-1; cdmx$intubado[is.na(cdmx$intubado)]<-0
cdmx$diagnostico_clinico_neumonia[cdmx$diagnostico_clinico_neumonia!="SI"]<-0; cdmx$diagnostico_clinico_neumonia[cdmx$diagnostico_clinico_neumonia=="SI"]<-1; cdmx$diagnostico_clinico_neumonia[is.na(cdmx$diagnostico_clinico_neumonia)]<-0
cdmx$esta_embarazada[cdmx$esta_embarazada!="SI"]<-0; cdmx$esta_embarazada[cdmx$esta_embarazada=="SI"]<-1; cdmx$esta_embarazada[is.na(cdmx$esta_embarazada)]<-0
cdmx$es_indigena[cdmx$es_indigena!="SI"]<-0; cdmx$es_indigena[cdmx$es_indigena=="SI"]<-1; cdmx$es_indigena[is.na(cdmx$es_indigena)]<-0
cdmx$habla_lengua_indigena[cdmx$habla_lengua_indigena!="SI"]<-0; cdmx$habla_lengua_indigena[cdmx$habla_lengua_indigena=="SI"]<-1; cdmx$habla_lengua_indigena[is.na(cdmx$habla_lengua_indigena)]<-0
cdmx$fiebre[cdmx$fiebre!="SI"]<-0; cdmx$fiebre[cdmx$fiebre=="SI"]<-1; cdmx$fiebre[is.na(cdmx$fiebre)]<-0
cdmx$tos[cdmx$tos!="SI"]<-0; cdmx$tos[cdmx$tos=="SI"]<-1; cdmx$tos[is.na(cdmx$tos)]<-0
cdmx$odinofagia[cdmx$odinofagia!="SI"]<-0; cdmx$odinofagia[cdmx$odinofagia=="SI"]<-1; cdmx$odinofagia[is.na(cdmx$odinofagia)]<-0
cdmx$disnea[cdmx$disnea!="SI"]<-0; cdmx$disnea[cdmx$disnea=="SI"]<-1; cdmx$disnea[is.na(cdmx$disnea)]<-0
cdmx$irritabilidad[cdmx$irritabilidad!="SI"]<-0; cdmx$irritabilidad[cdmx$irritabilidad=="SI"]<-1; cdmx$irritabilidad[is.na(cdmx$irritabilidad)]<-0
cdmx$diarrea[cdmx$diarrea!="SI"]<-0; cdmx$diarrea[cdmx$diarrea=="SI"]<-1; cdmx$diarrea[is.na(cdmx$diarrea)]<-0
cdmx$dolor_toracico[cdmx$dolor_toracico!="SI"]<-0; cdmx$dolor_toracico[cdmx$dolor_toracico=="SI"]<-1; cdmx$dolor_toracico[is.na(cdmx$dolor_toracico)]<-0
cdmx$calofrios[cdmx$calofrios!="SI"]<-0; cdmx$calofrios[cdmx$calofrios=="SI"]<-1; cdmx$calofrios[is.na(cdmx$calofrios)]<-0
cdmx$cefalea[cdmx$cefalea!="SI"]<-0; cdmx$cefalea[cdmx$cefalea=="SI"]<-1; cdmx$cefalea[is.na(cdmx$cefalea)]<-0
cdmx$mialgias[cdmx$mialgias!="SI"]<-0; cdmx$mialgias[cdmx$mialgias=="SI"]<-1; cdmx$mialgias[is.na(cdmx$mialgias)]<-0
cdmx$artralgias[cdmx$artralgias!="SI"]<-0; cdmx$artralgias[cdmx$artralgias=="SI"]<-1; cdmx$artralgias[is.na(cdmx$artralgias)]<-0
cdmx$ataque_al_estado_general[cdmx$ataque_al_estado_general!="SI"]<-0; cdmx$ataque_al_estado_general[cdmx$ataque_al_estado_general=="SI"]<-1; cdmx$ataque_al_estado_general[is.na(cdmx$ataque_al_estado_general)]<-0
cdmx$rinorrea[cdmx$rinorrea!="SI"]<-0; cdmx$rinorrea[cdmx$rinorrea=="SI"]<-1; cdmx$rinorrea[is.na(cdmx$rinorrea)]<-0
cdmx$polipnea[cdmx$polipnea!="SI"]<-0; cdmx$polipnea[cdmx$polipnea=="SI"]<-1; cdmx$polipnea[is.na(cdmx$polipnea)]<-0
cdmx$vomito[cdmx$vomito!="SI"]<-0; cdmx$vomito[cdmx$vomito=="SI"]<-1; cdmx$vomito[is.na(cdmx$vomito)]<-0
cdmx$dolor_abdominal[cdmx$dolor_abdominal!="SI"]<-0; cdmx$dolor_abdominal[cdmx$dolor_abdominal=="SI"]<-1; cdmx$dolor_abdominal[is.na(cdmx$dolor_abdominal)]<-0
cdmx$conjuntivitis[cdmx$conjuntivitis!="SI"]<-0; cdmx$conjuntivitis[cdmx$conjuntivitis=="SI"]<-1; cdmx$conjuntivitis[is.na(cdmx$conjuntivitis)]<-1
cdmx$cianosis[cdmx$cianosis!="SI"]<-0; cdmx$cianosis[cdmx$cianosis=="SI"]<-1; cdmx$cianosis[is.na(cdmx$cianosis)]<-0
cdmx$inicio_subito_sintomas[cdmx$inicio_subito_sintomas!="SI"]<-0; cdmx$inicio_subito_sintomas[cdmx$inicio_subito_sintomas=="SI"]<-1; cdmx$inicio_subito_sintomas[is.na(cdmx$inicio_subito_sintomas)]<-0
cdmx$sexo[cdmx$sexo!="FEMENINO"]<-0; cdmx$sexo[cdmx$sexo=="FEMENINO"]<-1
cdmx$unidad_cuidados_intensivos[cdmx$unidad_cuidados_intensivos!="SI"]<-0; cdmx$unidad_cuidados_intensivos[cdmx$unidad_cuidados_intensivos=="SI"]<-1; cdmx$unidad_cuidados_intensivos[is.na(cdmx$unidad_cuidados_intensivos)]<-0

cdmx[,c(4, 6:8, 12:18, 23:52, 56, 58 )]<-sapply(cdmx[,c(4, 6:8, 12:18, 23:52, 56, 58)], as.numeric)

cdmx$fecha_defuncion[cdmx$fecha_defuncion==""]<-NA
cdmx$fecha_defuncion<-as.Date(cdmx$fecha_defuncion)

cdmx$FU_time_MORT<-cdmx$fecha_defuncion-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$fecha_ingreso<-as.Date(cdmx$fecha_ingreso)
cdmx$fecha_inicio_sintomas<-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$FU_time_HOSP<-as.Date(cdmx$fecha_ingreso)-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$fecha_inicio_sintomas<-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$FU_time_HOSP[is.na(cdmx$FU_time_HOSP)]<-as.Date(cdmx$fecha_ingreso)-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$FU_time_HOSP<-as.numeric(cdmx$FU_time_HOSP)
cdmx$H_7d<-NULL
cdmx$H_7d[cdmx$FU_time_HOSP>=7]<-1
cdmx$H_7d[cdmx$FU_time_HOSP<7]<-0

cdmx$comorb<-(cdmx$diabetes+cdmx$hipertension+cdmx$enfermedad_cardiaca+cdmx$insuficiencia_renal_cronica+cdmx$asma+cdmx$VIH_SIDA+cdmx$epoc+
              cdmx$obesidad+cdmx$tabaquismo+cdmx$otra_condicion+cdmx$inmunosupresivo)

d1<-dummies::dummy(cdmx$evolucion_caso); cdmx<-cbind(cdmx, d1)

cdmx$NAC_clin<-NULL; cdmx$NAC_clin[cdmx$diagnostico_clinico_neumonia==1]<-1; cdmx$NAC_clin<-na.tools::na.replace(cdmx$NAC_clin,0)

cdmx$edad65<-NULL; cdmx$edad65[cdmx$edad>=65]<-1; cdmx$edad65[cdmx$edad<65]<-0

cdmx$edad40<-NULL; cdmx$edad40[cdmx$edad<=40]<-1; cdmx$edad40[cdmx$edad>40]<-0

cdmx$DM2_edad40<-NULL; cdmx$DM2_edad40[cdmx$edad40==1 & cdmx$diabetes==1]<-1; cdmx$DM2_edad40<-na.tools::na.replace(cdmx$DM2_edad40,0)

cdmx$HOSP<-NULL; cdmx$HOSP[cdmx$tipo_paciente=="HOSPITALIZADO"]<-1; cdmx$HOSP<-na.tools::na.replace(cdmx$HOSP,0)

cdmx$numero_sintomas<-(cdmx$fiebre+cdmx$tos+cdmx$disnea+cdmx$irritabilidad+cdmx$odinofagia+cdmx$diarrea+cdmx$dolor_toracico+cdmx$calofrios+cdmx$cefalea+
                       cdmx$mialgias+cdmx$artralgias+cdmx$ataque_al_estado_general+cdmx$rinorrea+cdmx$polipnea+cdmx$vomito+cdmx$conjuntivitis+cdmx$cianosis+
                       cdmx$inicio_subito_sintomas)

cdmx$asintomaticos[cdmx$numero_sintomas>0]<-0; cdmx$asintomaticos[cdmx$numero_sintomas==0&cdmx$covid==1]<-1; cdmx$asintomaticos<-na.tools::na.replace(cdmx$asintomaticos,0)

cdmx$caso_grave<-cdmx$`evolucion_casoCASO GRAVE -`; cdmx$defuncion<-cdmx$evolucion_casoDEFUNCION

cdmx$caso_severo<-NULL; cdmx$caso_severo[(cdmx$intubado==1 | cdmx$unidad_cuidados_intensivos==1 | cdmx$defuncion==1)& cdmx$covid==1]<-1; cdmx$caso_severo[is.na(cdmx$caso_severo)]<-0

cdmx$tiempo_prolong<-NULL; cdmx$tiempo_prolong[cdmx$FU_time_HOSP>=7]<-1; cdmx$tiempo_prolong[cdmx$FU_time_HOSP<7]<-0

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

table(cdmx$CAT_TRABAJO, useNA = "always")

#INEGI Work-Categories dataset
trab<-read_excel("profesion_cdmx.xlsx")

trab$CAT_TRABAJO_INEGI<-NULL
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="CAMPESINOS"]<-3
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="CHOFERES"]<-3
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="COMERCIANTES"]<-3
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="EMPLEADOS"]<-3
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="GERENTES_PROPIETARIOS"]<-3
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="MAESTROS"]<-3
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="OBREROS"]<-3
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="OTROS_PROFESIONALES"]<-3

trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="JUBILADO / PENSIONADO"]<-7
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="ESTUDIANTE"]<-2
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="HOGAR"]<-5
trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="OTROS"]<-4

trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="DESEMPLEADOS"]<-6

trab$CAT_TRABAJO_INEGI[trab$CAT_TRABAJO=="TRABAJADORES_SALUD"]<-1

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

trab$CAT_TRABAJO_INEGI<-as.numeric(trab$CAT_TRABAJO_INEGI)
cdmx<-merge(trab,cdmx, by = "CAT_TRABAJO", all.x=T)

cdmx$fecha_de_registro<-as.Date(cdmx$fecha_de_registro)
cdmx$caso_defuncion<-as.numeric(cdmx$evolucion_casoDEFUNCION)

cdmx<-cdmx%>%filter(fecha_de_registro>="2020-03-02")
cdmx2<-cdmx%>%filter(fecha_de_registro<="2021-01-31")
cdmx1<-cdmx%>%filter(fecha_de_registro<="2020-12-31")

cdmx1$id<-paste0(str_pad(cdmx1$cve_entidad_residencia, 2,pad = "0"),str_pad(cdmx1$cve_municipio_residencia,3, pad="0"))
cdmx2$id<-paste0(str_pad(cdmx2$cve_entidad_residencia, 2,pad = "0"),str_pad(cdmx2$cve_municipio_residencia,3, pad="0"))

cdmxmov<-cdmx1%>%filter(cve_entidad_unidad_medica==9)%>%filter(cve_municipio_residencia<=17)%>%filter(cve_entidad_residencia==9)
cdmxmov3<-cdmx2%>%filter(cve_entidad_unidad_medica==9)%>%filter(cve_municipio_residencia<=17)%>%filter(cve_entidad_residencia==9)

mobility<-read.csv("diferencias-porcentuales-en-el-transito-vehicular-en-la-cdmx.csv")
colnames(mobility)<-c("id","date", "week", "total_cdmx", "azcapotzalco", "benito_juarez", "coyoacan","cuajimalpa", "cuauhtemoc", "gam", "iztacalco", "iztapalapa",
                      "magdalena_contreras", "miguel_hidalgo", "milpa_alta", "tlalpan","tlahuac", "venustiano_carranza", "xochimilco", "alvaro_obregon")

mobility$fecha_de_registro<-as.Date(mobility$date)
mobility<-mobility%>%filter(fecha_de_registro>="2020-03-02")
mobility<-mobility%>%filter(fecha_de_registro<="2020-12-31")
mobility<-mobility%>%arrange(fecha_de_registro)

cdmxmov1<-cdmxmov%>%arrange(fecha_de_registro)

f1<-sort(as.Date(unique(mobility$fecha_de_registro)))
f2<-seq(1:length(f1))
i_fechas<-as.data.frame(cbind(f1, f2))
i_fechas$fecha_de_registro<-as.Date(i_fechas$f1)
i_fechas<-i_fechas%>%dplyr::select(fecha_de_registro, f2)

mobility<-merge(mobility, i_fechas, by="fecha_de_registro", all.x=T)
miss<-anti_join(cdmxmov1, i_fechas, by="fecha_de_registro")
cdmxmov1<-left_join(cdmxmov1, i_fechas, by="fecha_de_registro")%>%fill(f2, .direction = "up")


mov1<-gather(mobility, key="deleg", value="i_mov", azcapotzalco, benito_juarez, coyoacan,cuajimalpa, cuauhtemoc, gam, iztacalco, iztapalapa,
             magdalena_contreras, miguel_hidalgo, milpa_alta, tlalpan,tlahuac, venustiano_carranza, xochimilco, alvaro_obregon)

mov1$cve_municipio_residencia<-NULL
mov1$cve_municipio_residencia[mov1$deleg=="azcapotzalco"]<-2
mov1$cve_municipio_residencia[mov1$deleg=="coyoacan"]<-3
mov1$cve_municipio_residencia[mov1$deleg=="cuajimalpa"]<-4
mov1$cve_municipio_residencia[mov1$deleg=="gam"]<-5
mov1$cve_municipio_residencia[mov1$deleg=="iztacalco"]<-6
mov1$cve_municipio_residencia[mov1$deleg=="iztapalapa"]<-7
mov1$cve_municipio_residencia[mov1$deleg=="magdalena_contreras"]<-8
mov1$cve_municipio_residencia[mov1$deleg=="milpa_alta"]<-9
mov1$cve_municipio_residencia[mov1$deleg=="alvaro_obregon"]<-10
mov1$cve_municipio_residencia[mov1$deleg=="tlahuac"]<-11
mov1$cve_municipio_residencia[mov1$deleg=="tlalpan"]<-12
mov1$cve_municipio_residencia[mov1$deleg=="xochimilco"]<-13
mov1$cve_municipio_residencia[mov1$deleg=="benito_juarez"]<-14
mov1$cve_municipio_residencia[mov1$deleg=="cuauhtemoc"]<-15
mov1$cve_municipio_residencia[mov1$deleg=="miguel_hidalgo"]<-16
mov1$cve_municipio_residencia[mov1$deleg=="venustiano_carranza"]<-17

cdmxmov1<-cdmxmov1%>%left_join(mov1, by=c("cve_municipio_residencia","fecha_de_registro"))

marg<-read.csv("marg.csv")
margcdmx<-marg%>%filter(Clave==9)
margcdmx$marg<-as.numeric(margcdmx$marg)

margcdmx$Nombre.1<-c("azcapotzalco","coyoacan", "cuajimalpa", "gam",  "iztacalco","iztapalapa", "magdalena_contreras", "milpa_alta", "alvaro_obregon",
                     "tlahuac", "tlalpan", "xochimilco","benito_juarez",  "cuauhtemoc", "miguel_hidalgo",  "venustiano_carranza")

margcdmx$cve_municipio_residencia<-NULL
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="azcapotzalco"]<-2
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="coyoacan"]<-3
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="cuajimalpa"]<-4
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="gam"]<-5
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="iztacalco"]<-6
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="iztapalapa"]<-7
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="magdalena_contreras"]<-8
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="milpa_alta"]<-9
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="alvaro_obregon"]<-10
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="tlahuac"]<-11
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="tlalpan"]<-12
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="xochimilco"]<-13
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="benito_juarez"]<-14
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="cuauhtemoc"]<-15
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="miguel_hidalgo"]<-16
margcdmx$cve_municipio_residencia[margcdmx$Nombre.1=="venustiano_carranza"]<-17

cdmxmov1<-cdmxmov1%>%left_join(margcdmx, by=c("cve_municipio_residencia"))
cdmxmov3<-cdmxmov3%>%left_join(margcdmx, by=c("cve_municipio_residencia"))

dmu<-read.csv("dmu_camas.csv")%>%rename(cve_municipio_residencia=CVE_MUN)%>%filter(cve_m>=9002&cve_m<=9017)

cdmxmov2<-cdmxmov1%>%left_join(dmu, by="cve_municipio_residencia")
cdmxmov3<-cdmxmov3%>%left_join(dmu, by="cve_municipio_residencia")

cdmxmov2$DISLI<-lm(marg~dmu, data = cdmxmov2)$residuals
cdmxmov2$cuad<-factor(ifelse((cdmxmov2$dmu<150 & cdmxmov2$DISLI < -0.103),"Low Density/Low DISLI", 
                              ifelse((cdmxmov2$dmu<150 & cdmxmov2$DISLI >= -0.103),"Low Density/High DISLI", 
                                     ifelse((cdmxmov2$dmu>=150 & cdmxmov2$DISLI < -0.103),"High Density/Low DISLI",
                                            ifelse((cdmxmov2$dmu>=150 & cdmxmov2$DISLI>= -0.103), "High Density/High DISLI", "Invalid")))))

cdmxmov3$DISLI<-lm(cdmxmov3$marg~cdmxmov3$dmu, data = cdmxmov3)$residuals
cdmxmov3$cuad<-factor(ifelse((cdmxmov3$dmu<150 & cdmxmov3$DISLI < -0.103),"Low Density/Low DISLI", 
                             ifelse((cdmxmov3$dmu<150 & cdmxmov3$DISLI >= -0.103),"Low Density/High DISLI", 
                                    ifelse((cdmxmov3$dmu>=150 & cdmxmov3$DISLI < -0.103),"High Density/Low DISLI",
                                           ifelse((cdmxmov3$dmu>=150 & cdmxmov3$DISLI>= -0.103), "High Density/High DISLI", "Invalid")))))

mob1<-mobility%>%dplyr::select(azcapotzalco, coyoacan, cuajimalpa, gam, iztacalco,
                        iztapalapa, magdalena_contreras, milpa_alta, alvaro_obregon,
                        tlahuac, tlalpan, xochimilco,benito_juarez, cuauhtemoc,  
                        miguel_hidalgo, venustiano_carranza)
mob1<-apply(mob1, FUN = median, MARGIN = 2, na.rm=T)

group1<-cdmxmov1%>%filter(CAT_TRABAJO_INEGI==1)
group2<-cdmxmov1%>%filter(CAT_TRABAJO_INEGI==2)
group3<-cdmxmov1%>%filter(CAT_TRABAJO_INEGI==3)
group4<-cdmxmov1%>%filter(CAT_TRABAJO_INEGI==4)

#1.ARDL####
#Positivity
positive_rate<-cdmxmov2 %>% mutate(week=data.table::week(fecha_de_registro))%>%
  group_by(week, CAT_TRABAJO_INEGI)%>% summarise(cases_pos=sum(covid, na.rm=T),
                                                 tested=n(), movi=mean(i_mov, na.rm=T), 
                                                 margin=mean(DISLI, na.rm=T),
                                                 pop=mean(pop_trabaj_INEGI))%>%
  mutate(rate=(cases_pos*100000)/nrow(cdmxmov2))%>%filter(week<50)
positive_rate<-positive_rate[!is.nan(positive_rate$movi),]

grangertest(rate~movi, order = 1, data = positive_rate)
grangertest(movi~rate, order = 1, data = positive_rate)

summary(ur.df(positive_rate$movi, type = c("none"), lags = 1))
summary(ur.pp(positive_rate$movi, type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.ers(positive_rate$movi, type = c("DF-GLS"), model = c("constant"), lag.max = 1)) 
summary(ur.kpss(positive_rate$movi, type = c("mu"), use.lag = 1))

pos1<-positive_rate%>%filter(CAT_TRABAJO_INEGI==1)%>%filter(week>=9&week<50)

pos1$movi<-ts(pos1$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos1$rate<-ts(pos1$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos1$marg<-ts(pos1$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(pos1$movi)
ts.plot(pos1$rate)
ts.plot(pos1$marg)

lag1<-auto_ardl(rate ~ movi + margin, data=pos1, max_order=c(5,5,5), selection="BIC")
summary(lag1$best_model)
tend<-coint_eq(lag1$best_model, case=2)
plot(tend)
b_lag1<-lag1$best_model
hist(b_lag1$residuals)
uecm_1<-uecm(b_lag1)
summary(uecm_1)
recm_1<-recm(uecm_1, case=2)
summary(recm_1)

bounds_f_test(b_lag1, case = 2)
bounds_f_test(b_lag1, case = 3)
#No cointegration

Box.test(b_lag1$residuals, lag=10, fitdf=0)

pos2<-positive_rate%>%filter(CAT_TRABAJO_INEGI==2)%>%filter(week>=9&week<50)

pos2$movi<-ts(pos2$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos2$rate<-ts(pos2$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos2$marg<-ts(pos2$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(pos2$movi)
ts.plot(pos2$rate)

lag2<-auto_ardl(rate ~ movi + margin, data=pos2, max_order=c(1,2,1), 
                selection="BIC")
summary(lag2$best_model)
tend2<-coint_eq(lag2$best_model, case=2)
plot(tend2)
b_lag2<-lag2$best_model
hist(b_lag2$residuals)
uecm_2<-uecm(b_lag2)
summary(uecm_2)
recm_2<-recm(uecm_2, case=2)
summary(recm_2)

bounds_f_test(b_lag2, case = 2)
bounds_f_test(b_lag2, case = 3)
#No cointegration

Box.test(b_lag2$residuals, lag=10, fitdf=0)

pos3<-positive_rate%>%filter(CAT_TRABAJO_INEGI==3)%>%filter(week>=9&week<50)

pos3$movi<-ts(pos3$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos3$rate<-ts(pos3$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos3$marg<-ts(pos3$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(pos3$movi)
ts.plot(pos3$rate)

lag3<-auto_ardl(rate ~ movi + margin, data=pos3, max_order=c(5,5,5),
                selection="BIC")
summary(lag3$best_model)
tend3<-coint_eq(lag3$best_model, case=2)
plot(tend3)
b_lag3<-lag3$best_model
hist(b_lag3$residuals)
uecm_3<-uecm(b_lag3)
summary(uecm_3)
recm_3<-recm(uecm_3, case=2)
summary(recm_3)

bounds_f_test(b_lag3, case = 2)
bounds_f_test(b_lag3, case = 3)
#No cointegration

Box.test(b_lag3$residuals, lag=10, fitdf=0)

pos4<-positive_rate%>%filter(CAT_TRABAJO_INEGI==4)%>%filter(week>=9&week<50)

pos4$movi<-ts(pos4$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos4$rate<-ts(pos4$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos4$marg<-ts(pos4$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos4$int<-pos4$movi*pos4$marg

ts.plot(pos4$movi)
ts.plot(pos4$rate)

lag4<-auto_ardl(rate ~ movi + margin, data=pos4, max_order=c(5,5,5), 
                selection="BIC")
summary(lag4$best_model)
tend4<-coint_eq(lag4$best_model, case=2)
plot(tend4)
b_lag4<-lag4$best_model
hist(b_lag4$residuals)
uecm_4<-uecm(b_lag4)
summary(uecm_4)
recm_4<-recm(uecm_4, case=2)
summary(recm_4)

bounds_f_test(b_lag4, case = 2)
bounds_f_test(b_lag4, case = 3)
#No cointegration

Box.test(b_lag4$residuals, lag=10, fitdf=0)

pos5<-positive_rate%>%filter(CAT_TRABAJO_INEGI==5)%>%filter(week>=9&week<50)

pos5$movi<-ts(pos5$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos5$rate<-ts(pos5$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos5$marg<-ts(pos5$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(pos5$movi)
ts.plot(pos5$rate)
ts.plot(pos5$marg)

lag5<-auto_ardl(rate ~ movi + margin, data=pos5, max_order=c(5,5,5), 
                selection="BIC")
summary(lag5$best_model)
tend5<-coint_eq(lag5$best_model, case=2)
plot(tend5)
b_lag5<-lag5$best_model
hist(b_lag5$residuals)
uecm_5<-uecm(b_lag5)
summary(uecm_5)
recm_5<-recm(uecm_5, case=2)
summary(recm_5)

bounds_f_test(b_lag5, case = 2)
bounds_f_test(b_lag5, case = 3)
#No long term cointegration

Box.test(b_lag5$residuals, lag=10, fitdf=0)

pos6<-positive_rate%>%filter(CAT_TRABAJO_INEGI==6)%>%filter(week>=9&week<50)

pos6$movi<-ts(pos6$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos6$rate<-ts(pos6$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos6$marg<-ts(pos6$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(pos6$movi)
ts.plot(pos6$rate)
ts.plot(pos6$marg)

lag6<-auto_ardl(rate ~ movi + margin, data=pos6, max_order=c(5,5,5), 
                selection="BIC")
summary(lag6$best_model)
tend6<-coint_eq(lag6$best_model, case=2)
plot(tend6)
b_lag6<-lag6$best_model
hist(b_lag6$residuals)
uecm_6<-uecm(b_lag6)
summary(uecm_6)
recm_6<-recm(uecm_6, case=2)
summary(recm_6)

bounds_f_test(b_lag6, case = 2)
bounds_f_test(b_lag6, case = 3) #No cointegration

Box.test(b_lag6$residuals, lag=10, fitdf=0)

pos7<-positive_rate%>%filter(CAT_TRABAJO_INEGI==7)%>%filter(week>=9&week<50)

pos7$movi<-ts(pos7$movi, start=c(2020, 14), end=c(2020, 53), frequency=52)
pos7$rate<-ts(pos7$rate, start=c(2020, 14), end=c(2020, 53), frequency=52)
pos7$marg<-ts(pos7$margin, start=c(2020, 14), end=c(2020, 53), frequency=52)

ts.plot(pos7$movi)
ts.plot(pos7$rate)
ts.plot(pos7$marg)

lag7<-auto_ardl(rate ~ movi + margin, data=pos7, max_order=c(5,5,5), 
                selection="BIC")
summary(lag7$best_model)
tend7<-coint_eq(lag7$best_model, case=2)
plot(tend7)
b_lag7<-lag1$best_model
hist(b_lag1$residuals)
uecm_7<-uecm(b_lag7)
summary(uecm_7)
recm_7<-recm(uecm_7, case=2)
summary(recm_7)

bounds_f_test(b_lag7, case = 2)
bounds_f_test(b_lag7, case = 3)
#No cointegration

Box.test(b_lag7$residuals, lag=10, fitdf=0)

plag1<-autoplot(b_lag1$fitted.values)+
  autolayer(b_lag1$fitted.values)+
  autolayer(b_lag2$fitted.values)+
  autolayer(b_lag3$fitted.values)+
  autolayer(b_lag4$fitted.values)+
  autolayer(b_lag5$fitted.values)+
  autolayer(b_lag6$fitted.values)+
  autolayer(b_lag7$fitted.values)+
  ylab("Incidence rate")+
  xlab("Epidemiologic week")+
  labs(color="Working categories")+
  scale_color_jama(labels=c("HCW", "Students","Economically-Active", "Other professions", 
                            "Home-Related", "Unemployed", "Retired"))+
  theme_hc()

g1<-ts(pos1$rate, start=0)
g2<-ts(pos2$rate, start=0)
g3<-ts(pos3$rate, start=0)
g4<-ts(pos4$rate, start=0)
g5<-ts(pos5$rate, start=0)
g6<-ts(pos6$rate, start=0)
g7<-ts(pos7$rate, start=0)

plag1.1<-autoplot(ts.union(g1, tend, g2, tend2, g3, tend3, g4, tend4, g5, tend5, g6, tend6, g7, tend7))+
  aes(linetype=series)+
  ylab("Incidence rate per 100,000")+
  xlab("Epidemiologic week")+
  labs(color="Working categories")+
  scale_linetype_manual(labels = c(g1="HCW", tend="ARDL HCW", g2="Students", tend2="ARDL students",
                                   g3="Economically-Active",tend3="ARDL EA", g4="Other professions", 
                                   tend4="ARDL Other", g5="Home-Related", tend5="ARDL Home-Related",
                                   g6="Unemployed", tend6="ARDL Unemployed", g7="Retired", tend7="ARDL Retired"),
                        values = c(3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1))+
  scale_color_manual(values=c("#374E55FF","374E55FF","#DF8F44FF","#DF8F44FF","#00A1D5FF","#00A1D5FF",
                              "#B24745FF","#B24745FF","#79AF97FF", "#79AF97FF","#6A6599FF",
                              "#6A6599FF", "#80796BFF", "#80796BFF"),
                     labels=c(g1="HCW", tend="ARDL HCW", g2="Students", tend2="ARDL students",
                              g3="Economically-Active",tend3="ARDL EA", g4="Other professions", 
                              tend4="ARDL Other", g5="Home-Related", tend5="ARDL Home-Related",
                              g6="Unemployed", tend6="ARDL Unemployed", g7="Retired", tend7="ARDL Retired"))+ 
  guides(linetype=FALSE)+
  ylim(-50,1000)+
  theme_hc()

#Mortality
mortality_rate<-cdmxmov2 %>% mutate(week=data.table::week(fecha_de_registro))%>%
  group_by(week, CAT_TRABAJO_INEGI)%>% summarise(cases_def=sum(evolucion_casoDEFUNCION, na.rm=T),
                                                 tested=n(), movi=mean(i_mov, na.rm=T), 
                                                 margin=mean(DISLI, na.rm=T),
                                                 pop=mean(pop_trabaj_INEGI, na.rm=T))%>%
  mutate(rate=(cases_def*100000)/nrow(cdmxmov2))%>%filter(week<50)

mortality_rate<-mortality_rate[!is.nan(mortality_rate$movi),]

grangertest(rate~CAT_TRABAJO_INEGI, order = 1, data = mortality_rate)
grangertest(CAT_TRABAJO_INEGI~rate, order = 1, data = mortality_rate)

summary(ur.df(mortality_rate$movi, type = c("none"), lags = 1))
summary(ur.pp(mortality_rate$movi, type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.ers(mortality_rate$movi, type = c("DF-GLS"), model = c("constant"), lag.max = 1)) 
summary(ur.kpss(mortality_rate$movi, type = c("mu"), use.lag = 1))

mort1<-mortality_rate%>%filter(CAT_TRABAJO_INEGI==1)%>%filter(week>=9&week<50)

mort1$movi<-ts(mort1$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort1$rate<-ts(mort1$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort1$marg<-ts(mort1$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort1$movi)
ts.plot(mort1$rate)

mlag1<-auto_ardl(rate ~ movi+margin, data=mort1, max_order=c(5,5,5), selection="BIC")
summary(mlag1$best_model)
mtend<-coint_eq(mlag1$best_model, case=2)
plot(mtend)
m_lag1<-mlag1$best_model
autoplot(m_lag1$residuals)
hist(m_lag1$residuals)
muecm_1<-uecm(m_lag1)
summary(muecm_1)
mrecm_1<-recm(muecm_1, case=2)
summary(mrecm_1)

bounds_f_test(m_lag1, case = 2)
bounds_f_test(m_lag1, case = 3)

Box.test(m_lag1$residuals, lag=10, fitdf=0)

mort2<-mortality_rate%>%filter(CAT_TRABAJO_INEGI==2)%>%filter(week>=9&week<50) 

mort2$movi<-ts(mort2$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort2$rate<-ts(mort2$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort2$marg<-ts(mort2$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort2$movi)
ts.plot(mort2$rate)

mlag2<-auto_ardl(rate ~ movi+marg, data=mort2, max_order=c(5,5,5), selection="BIC")
summary(mlag2$best_model)
mtend2<-coint_eq(mlag2$best_model, case=2)
plot(mtend2)
m_lag2<-mlag2$best_model
autoplot(m_lag2$residuals)
hist(m_lag2$residuals)
muecm_2<-uecm(m_lag2)
summary(muecm_2)
mrecm_2<-recm(muecm_2, case=2)
summary(mrecm_2)

bounds_f_test(m_lag2, case = 2)
bounds_f_test(m_lag2, case = 3)

Box.test(m_lag2$residuals, lag=10, fitdf=0)

mort3<-mortality_rate%>%filter(CAT_TRABAJO_INEGI==3)%>%filter(week>=9&week<50)

mort3$movi<-ts(mort3$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort3$rate<-ts(mort3$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort3$marg<-ts(mort3$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort3$movi)
ts.plot(mort3$rate)

mlag3<-auto_ardl(rate ~ movi+marg, data=mort3, max_order=c(5,5,5), selection="BIC")
summary(mlag3$best_model)
mtend3<-coint_eq(mlag3$best_model, case=2)
plot(mtend3)
m_lag3<-mlag3$best_model
autoplot(m_lag3$residuals)
hist(m_lag3$residuals)
muecm_3<-uecm(m_lag3)
summary(muecm_3)
mrecm_3<-recm(muecm_3, case=2)
summary(mrecm_3)

bounds_f_test(m_lag3, case = 2)
bounds_f_test(m_lag3, case = 3)

Box.test(m_lag3$residuals, lag=10, fitdf=0)

mort4<-mortality_rate%>%filter(CAT_TRABAJO_INEGI==4)%>%filter(week>=9&week<50)

mort4$movi<-ts(mort4$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort4$rate<-ts(mort4$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort4$marg<-ts(mort4$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort4$movi)
ts.plot(mort4$rate)

mlag4<-auto_ardl(rate ~ movi+marg, data=mort4, max_order=c(5,5,5), selection="BIC")

summary(mlag4$best_model)
mtend4<-coint_eq(mlag4$best_model, case=2)
plot(mtend)
m_lag4<-mlag4$best_model
autoplot(m_lag4$residuals)
hist(m_lag4$residuals)
muecm_4<-uecm(m_lag4)
summary(muecm_4)
mrecm_4<-recm(muecm_4, case=2)
summary(recm_8)

bounds_f_test(m_lag4, case = 2)
bounds_f_test(m_lag4, case = 3)
#No cointegration
Box.test(m_lag4$residuals, lag=10, fitdf=0)

mort5<-mortality_rate%>%filter(CAT_TRABAJO_INEGI==5)%>%filter(week>=9&week<50)

mort5$movi<-ts(mort5$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort5$rate<-ts(mort5$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort5$marg<-ts(mort5$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort5$movi)
ts.plot(mort5$rate)

mlag5<-auto_ardl(rate ~ movi+marg, data=mort5, max_order=c(5,5,5), selection="BIC")

summary(mlag5$best_model)
mtend5<-coint_eq(mlag5$best_model, case=2)
plot(mtend5)
m_lag5<-mlag5$best_model
autoplot(m_lag5$residuals)
hist(m_lag5$residuals)
muecm_5<-uecm(m_lag5)
summary(muecm_5)
mrecm_5<-recm(muecm_5, case=2)
summary(mrecm_5)

bounds_f_test(m_lag5, case = 2)
bounds_f_test(m_lag5, case = 3)
#No cointegration

Box.test(m_lag5$residuals, lag=10, fitdf=0)

mort6<-mortality_rate%>%filter(CAT_TRABAJO_INEGI==6)%>%filter(week>=9&week<50)

mort6$movi<-ts(mort6$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort6$rate<-ts(mort6$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort6$marg<-ts(mort6$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort6$movi)
ts.plot(mort6$rate)

mlag6<-auto_ardl(rate ~ movi+marg, data=mort6, max_order=c(5,5,5), selection="BIC")

summary(mlag6$best_model)
mtend6<-coint_eq(mlag6$best_model, case=2)
plot(mtend6)
m_lag6<-mlag6$best_model
autoplot(m_lag6$residuals)
hist(m_lag6$residuals)
muecm_6<-uecm(m_lag6)
summary(muecm_6)
mrecm_6<-recm(muecm_6, case=2)
summary(mrecm_6)

bounds_f_test(m_lag6, case = 2)
bounds_f_test(m_lag6, case = 3)

Box.test(m_lag6$residuals, lag=10, fitdf=0)

mort7<-mortality_rate%>%filter(CAT_TRABAJO_INEGI==6)%>%filter(week>=9&week<50)

mort7$movi<-ts(mort7$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort7$rate<-ts(mort7$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort7$marg<-ts(mort7$margin, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort7$movi)
ts.plot(mort7$rate)

mlag7<-auto_ardl(rate ~ movi+marg, data=mort7, max_order=c(5,5,5), selection="BIC")

summary(mlag7$best_model)
mtend7<-coint_eq(mlag7$best_model, case=2)
plot(mtend7)
m_lag7<-mlag7$best_model
autoplot(m_lag7$residuals)
hist(m_lag7$residuals)
muecm_7<-uecm(m_lag7)
summary(muecm_7)
mrecm_8<-recm(muecm_7, case=2)
summary(mrecm_7)

bounds_f_test(m_lag7, case = 2)
bounds_f_test(m_lag7, case = 3)

Box.test(m_lag7$residuals, lag=10, fitdf=0)

plag2<-autoplot(m_lag1$fitted.values)+
  autolayer(m_lag1$fitted.values)+
  autolayer(m_lag2$fitted.values)+
  autolayer(m_lag3$fitted.values)+
  autolayer(m_lag4$fitted.values)+
  ylab("Mortality rate per 100,000")+
  xlab("Epidemiologic week")+
  labs(color="Working categories")+
  scale_color_jama(labels=c("HCW", "Economically-Active", "Economically-Unactive", "Unemployed"))+
  theme_hc()

g8<-ts(mort1$rate, start=0)
g9<-ts(mort2$rate, start=0)
g10<-ts(mort3$rate, start=0)
g11<-ts(mort4$rate, start=0)
g12<-ts(mort5$rate, start=0)
g13<-ts(mort6$rate, start=0)
g14<-ts(mort7$rate, start=0)

plag2.1<-autoplot(ts.union(g8, mtend, g9, mtend2, g10, mtend3, g11, mtend4, g12, mtend5, g13, 
                           mtend6, g14, mtend7))+
  aes(linetype=series)+
  ylab("Mortality rate per 100,000")+
  xlab("Epidemiologic week")+
  labs(color="Working categories")+
  scale_linetype_manual(labels = c(g1="HCW", tend="ARDL HCW", g2="Students", tend2="ARDL students",
                                   g3="Economically-Active",tend3="ARDL EA", g4="Other professions", 
                                   tend4="ARDL Other", g5="Home-Related", tend5="ARDL Home-Related",
                                   g6="Unemployed", tend6="ARDL Unemployed", g7="Retired", tend7="ARDL Retired"),
                        values = c(3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1))+
  scale_color_manual(values=c("#374E55FF","374E55FF","#DF8F44FF","#DF8F44FF","#00A1D5FF","#00A1D5FF",
                              "#B24745FF","#B24745FF","#79AF97FF", "#79AF97FF","#6A6599FF",
                              "#6A6599FF", "#80796BFF", "#80796BFF"),
                     labels=c(g1="HCW", tend="ARDL HCW", g2="Students", tend2="ARDL students",
                              g3="Economically-Active",tend3="ARDL EA", g4="Other professions", 
                              tend4="ARDL Other", g5="Home-Related", tend5="ARDL Home-Related",
                              g6="Unemployed", tend6="ARDL Unemployed", g7="Retired", tend7="ARDL Retired"))+ 
  guides(linetype=FALSE)+
  theme_hc()

ardl1<-ggarrange(plag1.1, plag2.1, common.legend =T, ncol=2, nrow=1, 
                 labels=c("A", "B"))
print(ardl1)

ggsave(file="ardl1.png", ardl1, width = 40, height = 10, units="cm", dpi = 600,
       limitsize = FALSE)

#2.ARDL con marginación####

#Positivity
positive_rate_marg<-cdmxmov2 %>% mutate(week=data.table::week(fecha_de_registro))%>%
  group_by(week, cuad)%>% summarise(cases_pos=sum(covid, na.rm=T),
                                        tested=n(), movi=mean(i_mov, na.rm=T), 
                                        trab=mean(CAT_TRABAJO_INEGI, na.rm=T))%>%
  mutate(rate=(cases_pos*100000)/nrow(cdmxmov2))%>%filter(week<50)
positive_rate_marg<-positive_rate_marg[!is.nan(positive_rate_marg$movi),]

grangertest(rate~movi, order = 1, data = positive_rate_marg)
grangertest(movi~rate, order = 1, data = positive_rate_marg)

summary(ur.df(positive_rate_marg$movi, type = c("none"), lags = 1))
summary(ur.pp(positive_rate_marg$movi, type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.ers(positive_rate_marg$movi, type = c("DF-GLS"), model = c("constant"), lag.max = 1)) 
summary(ur.kpss(positive_rate_marg$movi, type = c("mu"), use.lag = 1))

pos1marg<-positive_rate_marg%>%filter(cuad=="High Density/High DISLI")%>%filter(week>=9&week<50) 

pos1marg$movi<-ts(pos1marg$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos1marg$rate<-ts(pos1marg$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos1marg$trab<-ts(pos1marg$trab, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(pos1marg$movi)
ts.plot(pos1marg$rate)
ts.plot(pos1marg$trab)

lag1marg<-auto_ardl(rate ~ movi + trab, data=pos1marg, max_order=c(5,5,5), selection="BIC")
summary(lag1marg$best_model)
tend17<-coint_eq(lag1marg$best_model, case=2)
plot(tend17)
marg_lag1<-lag1marg$best_model
hist(marg_lag1$residuals)
uecm_17<-uecm(marg_lag1)
summary(uecm_17)
recm_17<-recm(uecm_17, case=2)
summary(recm_17)

bounds_f_test(marg_lag1, case = 2)
bounds_f_test(marg_lag1, case = 3)
#No cointegration

Box.test(marg_lag1$residuals, lag=10, fitdf=0)

pos2marg<-positive_rate_marg%>%filter(cuad=="High Density/Low DISLI")%>%filter(week>=9&week<50)

pos2marg$movi<-ts(pos2marg$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos2marg$rate<-ts(pos2marg$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos2marg$trab<-ts(pos2marg$trab, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(pos2marg$movi)
ts.plot(pos2marg$rate)
ts.plot(pos2marg$trab)

lag2marg<-auto_ardl(rate ~ movi + trab, data=pos2marg, max_order=c(2,4,3), selection="BIC")
summary(lag2marg$best_model)
tend18<-coint_eq(lag2marg$best_model, case=2)
plot(tend18)
marg_lag2<-lag2marg$best_model
hist(marg_lag2$residuals)
uecm_18<-uecm(marg_lag2)
summary(uecm_18)
recm_18<-recm(uecm_18, case=2)
summary(recm_18)

bounds_f_test(marg_lag2, case = 2)
bounds_f_test(marg_lag2, case = 3)
#No cointegration

Box.test(marg_lag2$residuals, lag=10, fitdf=0)

pos3marg<-positive_rate_marg%>%filter(cuad=="Low Density/High DISLI")%>%filter(week>=9&week<50)

pos3marg$movi<-ts(pos3marg$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos3marg$rate<-ts(pos3marg$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos3marg$trab<-ts(pos3marg$trab, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(pos3marg$movi)
ts.plot(pos3marg$rate)
ts.plot(pos3marg$trab)

lag3marg<-auto_ardl(rate ~ movi + trab, data=pos3marg, max_order=c(1,1,2), selection="BIC")
summary(lag3marg$best_model)
tend19<-coint_eq(lag3marg$best_model, case=2)
plot(tend19)
marg_lag3<-lag3marg$best_model
hist(marg_lag3$residuals)
uecm_19<-uecm(marg_lag3)
summary(uecm_19)
recm_19<-recm(uecm_19, case=2)
summary(recm_19)

bounds_f_test(marg_lag3, case = 2)
bounds_f_test(marg_lag3, case = 3)
#No cointegration

Box.test(marg_lag3$residuals, lag=10, fitdf=0)

pos4marg<-positive_rate_marg%>%filter(cuad=="Low Density/Low DISLI")%>%filter(week>=9&week<50)

pos4marg$movi<-ts(pos4marg$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos4marg$rate<-ts(pos4marg$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
pos4marg$trab<-ts(pos4marg$trab, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(pos4marg$movi)
ts.plot(pos4marg$rate)
ts.plot(pos4marg$trab)

lag4marg<-auto_ardl(rate ~ movi + trab, data=pos4marg, max_order=c(5,5,5), selection="BIC")
summary(lag4marg$best_model)
tend19_1<-coint_eq(lag4marg$best_model, case=2)
plot(tend19_1)
marg_lag4<-lag4marg$best_model
hist(marg_lag4$residuals)
uecm_19_1<-uecm(marg_lag4)
summary(uecm_19_1)
recm_19_1<-recm(uecm_19_1, case=2)
summary(recm_19_1)

bounds_f_test(marg_lag4, case = 2)
bounds_f_test(marg_lag4, case = 3)

Box.test(marg_lag4$residuals, lag=10, fitdf=0)

plag5<-autoplot(marg_lag1$fitted.values)+
  autolayer(marg_lag1$fitted.values)+
  autolayer(marg_lag2$fitted.values)+
  autolayer(marg_lag3$fitted.values)+
  autolayer(marg_lag4$fitted.values)+
  ylab("Incidence rate per 100,000")+
  xlab("Epidemiologic week")+
  labs(color="Mean Urban Population Density\n&\nDensity-Independent SLI Categories", size=10)+
  scale_color_viridis_d(option = "viridis", labels=c("High Density/High DISLI","High Density/Low DISLI", "Low Density/High DISLI", 
                                                     "Low Density/Low DISLI")) +
  theme(axis.title.x = element_text(size = 13), axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 13), axis.text.y = element_text(size = 10))+
  ylim(-50,1000)+
  theme_hc()

g17<-ts(pos1marg$rate, start=0)
g18<-ts(pos2marg$rate, start=0)
g19<-ts(pos3marg$rate, start=0)
g19_1<-ts(pos4marg$rate, start=0)

plag5.1<-autoplot(ts.union(g17, tend17, g18, tend18, g19, tend19, g19_1, tend19_1))+
  aes(linetype=series)+
  ylab("Incidence rate per 100,000")+
  xlab("Epidemiologic week")+
  labs(color="Mean Urban Population Density\n&\nDensity-Independent SLI Categories")+
  scale_linetype_manual(labels = c(g17="High Density/High DISLI", tend17="ARDL High Density/High DISLI", g18="High Density/Low DISLI", 
                                   tend18="ARDL High Density/Low DISLI", g19="Low Density/High DISLI",tend19="ARDL Low Density/High DISLI",
                                   g19_1="Low Density/Low DISLI", tend19_1="ARDL Low Density/Low DISLI"),
                        values = c(3, 1, 3, 1, 3, 1, 3, 1)) +
  scale_color_brewer(labels=c(g17="High Density/High DISLI", tend17="ARDL High Density/High DISLI", g18="High Density/Low DISLI", 
                              tend18="ARDL High Density/Low DISLI", g19="Low Density/High DISLI",tend19="ARDL Low Density/High DISLI",
                              g19_1="Low Density/Low DISLI", tend19_1="ARDL Low Density/Low DISLI"), 
                     palette = "Paired")+
  ylim(-50,1000)+
  guides(linetype=FALSE)+
  theme_hc()

#Mortality
mortality_rate_marg<-cdmxmov2 %>% mutate(week=data.table::week(fecha_de_registro))%>%
  group_by(week, cuad)%>% summarise(cases_def=sum(evolucion_casoDEFUNCION, na.rm=T),
                                           tested=n(), movi=mean(i_mov, na.rm=T), 
                                           trab=mean(CAT_TRABAJO_INEGI, na.rm=T))%>%
  mutate(rate=(cases_def*100000)/nrow(cdmxmov2))

mortality_rate_marg<-mortality_rate_marg[!is.nan(mortality_rate_marg$movi),]

grangertest(rate~movi, order = 1, data = mortality_rate_marg)
grangertest(movi~rate, order = 1, data = mortality_rate_marg)

summary(ur.df(mortality_rate_marg$movi, type = c("none"), lags = 1))
summary(ur.pp(mortality_rate_marg$movi, type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.ers(mortality_rate_marg$movi, type = c("DF-GLS"), model = c("constant"), lag.max = 1)) 
summary(ur.kpss(mortality_rate_marg$movi, type = c("mu"), use.lag = 1))

mort1marg<-mortality_rate_marg%>%filter(cuad=="High Density/High DISLI")%>%filter(week>=9&week<50)

mort1marg$movi<-ts(mort1marg$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort1marg$rate<-ts(mort1marg$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort1marg$trab<-ts(mort1marg$trab, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort1marg$movi)
ts.plot(mort1marg$rate)
ts.plot(mort1marg$trab)

mlag1marg<-auto_ardl(rate ~ movi+trab, data=mort1marg, max_order=c(4,4,4), selection="BIC")#no cointegration
summary(mlag1marg$best_model)
tend20<-coint_eq(mlag1marg$best_model, case=2)
plot(tend20)
marg_lag4<-mlag1marg$best_model
autoplot(marg_lag4$residuals)
hist(marg_lag4$residuals)
uecm_20<-uecm(marg_lag4)
summary(uecm_20)
recm_20<-recm(uecm_20, case=2)
summary(recm_20)

bounds_f_test(marg_lag4, case = 2)
bounds_f_test(marg_lag4, case = 3)
#no cointegration

Box.test(marg_lag4$residuals, lag=10, fitdf=0)

mort2marg<-mortality_rate_marg%>%filter(cuad=="High Density/Low DISLI")%>%filter(week>=9&week<50) #No cointegration

mort2marg$movi<-ts(mort2marg$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort2marg$rate<-ts(mort2marg$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort2marg$trab<-ts(mort2marg$trab, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort2marg$movi)
ts.plot(mort2marg$rate)
ts.plot(mort2marg$trab)

mlag2marg<-auto_ardl(rate ~ movi+trab, data=mort2marg, max_order=c(5,5,5), selection="BIC")
summary(mlag2marg$best_model)
BIC(mlag2marg$best_model)
tend21<-coint_eq(mlag2marg$best_model, case=2)
plot(tend21)
marg_lag5<-mlag2marg$best_model
autoplot(marg_lag5$residuals)
hist(marg_lag5$residuals)
uecm_21<-uecm(marg_lag5)
summary(uecm_21)
recm_21<-recm(uecm_21, case=2)
summary(recm_21)

bounds_f_test(marg_lag5, case = 2)
bounds_f_test(marg_lag5, case = 3)
#No cointegration

Box.test(marg_lag5$residuals, lag=10, fitdf=0)

mort3marg<-mortality_rate_marg%>%filter(cuad=="Low Density/High DISLI")%>%filter(week>=9&week<50)

mort3marg$movi<-ts(mort3marg$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort3marg$rate<-ts(mort3marg$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort3marg$trab<-ts(mort3marg$trab, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort3marg$movi)
ts.plot(mort3marg$rate)
ts.plot(mort3marg$trab)

mlag3marg<-auto_ardl(rate ~ movi + trab, data=mort3marg, max_order=c(5,5,5), selection="BIC") 
summary(mlag3marg$best_model)
BIC(mlag3marg$best_model)
tend22<-coint_eq(mlag3marg$best_model, case=2)
plot(tend22)
marg_lag6<-mlag3marg$best_model
autoplot(marg_lag6$residuals)
hist(marg_lag6$residuals)
uecm_22<-uecm(marg_lag6)
summary(uecm_22)
recm_22<-recm(uecm_22, case=2)
summary(recm_22)

bounds_f_test(marg_lag6, case = 2)
bounds_f_test(marg_lag6, case = 3)

Box.test(marg_lag6$residuals, lag=10, fitdf=0)

mort4marg<-mortality_rate_marg%>%filter(cuad=="Low Density/Low DISLI")%>%filter(week>=9&week<50)

mort4marg$movi<-ts(mort4marg$movi, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort4marg$rate<-ts(mort4marg$rate, start=c(2020, 13), end=c(2020, 53), frequency=52)
mort4marg$trab<-ts(mort4marg$trab, start=c(2020, 13), end=c(2020, 53), frequency=52)

ts.plot(mort4marg$movi)
ts.plot(mort4marg$rate)
ts.plot(mort4marg$trab)

mlag4marg<-auto_ardl(rate ~ movi + trab, data=mort4marg, max_order=c(5,5,5), selection="BIC") #No cointegration
summary(mlag4marg$best_model)
BIC(mlag4marg$best_model)
tend23<-coint_eq(mlag4marg$best_model, case=2)
plot(tend23)
marg_lag7<-mlag4marg$best_model
autoplot(marg_lag7$residuals)
hist(marg_lag7$residuals)
uecm_23<-uecm(marg_lag7)
summary(uecm_23)
recm_23<-recm(uecm_23, case=2)
summary(recm_23)

bounds_f_test(marg_lag7, case = 2)
bounds_f_test(marg_lag7, case = 3)
#No cointegration

Box.test(marg_lag7$residuals, lag=10, fitdf=0)

plag6<-autoplot(marg_lag4$fitted.values)+
  autolayer(marg_lag4$fitted.values)+
  autolayer(marg_lag5$fitted.values)+
  autolayer(marg_lag6$fitted.values)+
  autolayer(marg_lag7$fitted.values)+
  ylab("Mortality rate per 100,000")+
  xlab("Epidemiologic week")+
  labs(color="Mean Urban Population Density\n&\nDensity-Independent SLI Categories", size=10)+
  scale_color_viridis_d(option = "viridis", labels=c("High Density/High DISLI","High Density/Low DISLI", "Low Density/High DISLI", 
                                                     "Low Density/Low DISLI")) +
  theme(axis.title.x = element_text(size = 13), axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 13), axis.text.y = element_text(size = 10))+
  theme_hc()

g20<-ts(mort1marg$rate, start=0)
g21<-ts(mort2marg$rate, start=0)
g22<-ts(mort3marg$rate, start=0)
g23<-ts(mort4marg$rate, start=0)

plag6.1<-autoplot(ts.union(g20, tend20, g21, tend21, g22, tend22, g23, tend23))+
  aes(linetype=series)+
  ylab("Mortality rate per 100,000")+
  xlab("Epidemiologic week")+
  labs(color="Mean Urban Population Density\n&\nDensity-Independent SLI Categories")+
  scale_linetype_manual(labels = c(g20="High Density/High DISLI", tend20="ARDL High Density/High DISLI", g21="High Density/Low DISLI", 
                                   tend21="ARDL High Density/Low DISLI", g22="Low Density/High DISLI",tend22="ARDL Low Density/High DISLI",
                                   g23="Low Density/Low DISLI", tend23="ARDL Low Density/Low DISLI"),
                        values = c(3, 1, 3, 1, 3, 1, 3, 1)) +
  scale_color_brewer(labels=c(g20="High Density/High DISLI", tend20="ARDL High Density/High DISLI", g21="High Density/Low DISLI", 
                              tend21="ARDL High Density/Low DISLI", g22="Low Density/High DISLI",tend22="ARDL Low Density/High DISLI",
                              g23="Low Density/Low DISLI", tend23="ARDL Low Density/Low DISLI"), 
                     palette = "Paired")+
  guides(linetype=FALSE)+
  theme_hc()

ardl2<-ggarrange(plag5.1, plag6.1, common.legend = T, ncol=2, nrow=1, 
                 labels=c("A", "B"))

ardl3<-ggarrange(plag5,plag6, common.legend = T, ncol=2, nrow=1)
print(ardl3)

ardl4<-ggarrange(ardl1, ardl2, common.legend=F, ncol=1, nrow=2)

ggsave(file="ardl4.png", ardl4, width = 40, height = 20, units="cm", dpi = 600,
       limitsize = FALSE)

ggsave(file="ardl2.png", ardl2, width = 40, height = 10, units="cm", dpi = 600,
       limitsize = FALSE)

ggsave(file="ardl3.png", ardl3, width = 40, height = 10, units="cm", dpi = 600,
       limitsize = FALSE)

#3.Interacción modelos mixtos (poisson)####

Pob_conapo1<-read_xlsx("base_municipios_final_datos_01.xlsx")
Pob_conapo2<-read_xlsx("base_municipios_final_datos_02.xlsx")
Pob_conapo<-rbind(Pob_conapo1, Pob_conapo2)%>%filter(CLAVE_ENT==9 & `A—O` == 2020)
Pob_conapo$cve_municipio_residencia<-as.numeric(Pob_conapo$CLAVE)-9000
dmu_2<-dmu%>%dplyr::select(cve_municipio_residencia,dmu)
marg3<-margcdmx%>%dplyr::select(cve_municipio_residencia,marg)

Pob_edad_cuadrante<- Pob_conapo %>% group_by(cve_municipio_residencia, SEXO) %>%
  tally(POB) %>% rename(POB = n)
Pob_edad_cuadrante$sexo<-NULL; Pob_edad_cuadrante$sexo[Pob_edad_cuadrante$SEXO=="Mujeres"]<-0; Pob_edad_cuadrante$sexo[Pob_edad_cuadrante$SEXO!="Mujeres"]<-1

cdmxmov2<-cdmxmov2%>%left_join(Pob_edad_cuadrante, by=c("cve_municipio_residencia", "sexo"))

cdmxmov2$month<-data.table::month(cdmxmov2$fecha_de_registro)
cdmxmov2$periodo<-NA
cdmxmov2$periodo[cdmxmov2$fecha_de_registro<="2020-06-29"|cdmxmov2$fecha_de_registro<="2020-12-19"]<-0
cdmxmov2$periodo[cdmxmov2$fecha_de_registro>"2020-06-29"]<-1
cdmxmov2$dmucat<-NA
cdmxmov2$dmucat[cdmxmov2$dmu>=200]<-1
cdmxmov2$dmucat[cdmxmov2$dmu<200]<-0

spline_mov<-rcs(cdmxmov2$i_mov)

set.seed(123)

n.i.1<-glmer(covid~cuad*rcs(i_mov)+edad+periodo+comorb+(1|week)+
            (1|cve_municipio_residencia)+offset(log(POB)),family="poisson", data=cdmxmov2)

summary(n.i.1)
ranef(n.i.1)
deviance(n.i.1)/(nrow(cdmxmov2)-1)

s1<-summ(n.i.1, exp=T, confint=T)

pn1.1<-plot_model(n.i.1, type="eff", terms = c("i_mov [all]", "cuad"),
                  show.intercept = T, legend.title="DISLI quadrant", 
                  axis.title="Incidence rate")+
  xlab("Mobility index")+
  theme_hc()+
  scale_color_viridis_d(option = "viridis", labels=c("High Density/High DISLI","High Density/Low DISLI", "Low Density/High DISLI", 
                                                     "Low Density/Low DISLI"))+
  theme(
    axis.title.x = element_text(color="black", size=13),
    axis.title.y = element_text(color="black", size=13),
    axis.text.x = element_text(color="black", size=10),
    axis.text.y = element_text(color="black", size=10),
    legend.text = element_text(color="black", size=10),
    legend.title = element_text(color="black", size=10),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"))+
    scale_y_log10()+
  ggtitle(NULL)
print(pn1.1)

n.i.2<-glmer(caso_defuncion~cuad*rcs(i_mov)+edad+comorb+periodo+
             (1|week)+(1|cve_municipio_residencia)+offset(log(POB)), data=cdmxmov2, family="poisson")
summary(n.i.2)
deviance(n.i.2)/(nrow(cdmxmov2)-1)

s2<-summ(n.i.2, exp=T, confint=T)

pn1.2<-plot_model(n.i.2, type="eff", terms = c("i_mov [all]", "cuad"),
                  show.intercept = T, legend.title="DISLI quadrant", 
                  axis.title="Mortality rate")+
  xlab("Mobility index")+
  scale_color_viridis_d(option = "viridis", labels=c("High Density/High DISLI","High Density/Low DISLI", "Low Density/High DISLI", 
                                                     "Low Density/Low DISLI"))+
  theme_hc()+
  theme(
    axis.title.x = element_text(color="black", size=13),
    axis.title.y = element_text(color="black", size=13),
    axis.text.x = element_text(color="black", size=10),
    axis.text.y = element_text(color="black", size=10),
    legend.text = element_text(color="black", size=10),
    legend.title = element_text(color="black", size=10),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"))+
  scale_y_log10()+
  ggtitle(NULL)
print(pn1.2)

n.i.3<-glmer(caso_severo~cuad*rcs(i_mov)+edad+comorb+periodo+
             (1|week)+(1|cve_municipio_residencia)+offset(log(POB)), data=cdmxmov2, family="poisson")
summary(n.i.3)
deviance(n.i.3)/(nrow(cdmxmov2)-1)

s3<-summ(n.i.3, exp=T, confint=T)

pn1.3<-plot_model(n.i.3, type="eff", terms = c("i_mov [all]", "cuad"),
                  show.intercept = T, legend.title="DISLI quadrant", 
                  axis.title="Severe case rate")+
  xlab("Mobility index")+
  scale_color_viridis_d(option = "viridis", labels=c("High Density/High DISLI","High Density/Low DISLI", "Low Density/High DISLI", 
                                                     "Low Density/Low DISLI"))+
  theme_hc()+
  theme(
    axis.title.x = element_text(color="black", size=13),
    axis.title.y = element_text(color="black", size=13),
    axis.text.x = element_text(color="black", size=10),
    axis.text.y = element_text(color="black", size=10),
    legend.text = element_text(color="black", size=10),
    legend.title = element_text(color="black", size=10),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"))+
  scale_y_log10()+
  ggtitle(NULL)
print(pn1.3)

n.i.4<-glmer(HOSP~cuad*rcs(i_mov)+edad+comorb+periodo+
               (1|week)+(1|cve_municipio_residencia)+offset(log(POB)), data=cdmxmov2, family="poisson")
summary(n.i.4)
deviance(n.i.4)/(nrow(cdmxmov2)-1)

s4<-summ(n.i.4, exp=T, confint=T)

pn1.4<-plot_model(n.i.4, type="eff", terms = c("i_mov [all]", "cuad"),
                  show.intercept = T, legend.title="DISLI Quadrant")+
  xlab("Mobility index")+
  scale_color_viridis_d(option = "viridis", labels=c("High Density/High DISLI","High Density/Low DISLI", "Low Density/High DISLI", 
                                                     "Low Density/Low DISLI"))+
  theme_hc()+
  theme(
    axis.title.x = element_text(color="black", size=13),
    axis.title.y = element_text(color="black", size=13),
    axis.text.x = element_text(color="black", size=10),
    axis.text.y = element_text(color="black", size=10),
    legend.text = element_text(color="black", size=10),
    legend.title = element_text(color="black", size=10),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"))+
  scale_y_log10()+
  ggtitle(NULL)
print(pn1.4)

int1<-ggarrange(pn1.1, pn1.2, pn1.3, pn1.4, ncol=2, nrow=2, common.legend = TRUE, 
                labels=c("A","B", "C", "D"))
print(int1)

ggsave(file = "int1.png", int1, width = 40, height = 20, units=c("cm"), dpi = 600,
       limitsize = FALSE)

#4.Tablas####

cat_names<-c("Female sex [%]", "Positivity [%]", "Asymptomatics [%]", "Mortality [%]", 
             "Hospitalization [%]", "Clinical pneumonia [%]", "Severe outcome [%]", 
             "Mechanical ventilation [%]", "7 days since beginning of symptoms [%]", 
             "Diabetes [%]", "Hypertension [%]",  "Asthma [%]", "COPD [%]", 
             "HIV/AIDS [%]", "CVD [%]", "CKD [%]", "Obesity [%]","Smoking stat1us [%]",
             "Pregnant women [%]", "Indigenous [%]")

cat.gen<-NULL
cat.gen<-as.matrix(cdmxmov3 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                              diagnostico_clinico_neumonia,   caso_severo, intubado, 
                                              H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                              enfermedad_cardiaca, insuficiencia_renal_cronica,
                                              obesidad, tabaquismo, esta_embarazada, 
                                              es_indigena) %>% sapply(sum, cat.gen, na.rm=T))
rownames(cat.gen)<-cat_names
colnames(cat.gen)<-"General"

cuad1<-cdmxmov3%>%filter(cuad=="High Density/High DISLI")

cat.1<-NULL
cat.1<-as.matrix(cuad1 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                         diagnostico_clinico_neumonia, caso_severo, intubado, 
                                         H_7d, diabetes, hipertension, asma,  epoc, VIH_SIDA, 
                                         enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                         obesidad,  tabaquismo, esta_embarazada, es_indigena) %>% 
                   sapply(sum, cat.1, na.rm=T))

rownames(cat.1)<-cat_names
colnames(cat.1)<-"High_High"

cuad2<-cdmxmov3%>%filter(cuad=="High Density/Low DISLI")

cat.2<-NULL
cat.2<-as.matrix(cuad2 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                         diagnostico_clinico_neumonia,  caso_severo, intubado, 
                                         H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                         enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                         obesidad, tabaquismo, esta_embarazada, es_indigena) %>% 
                   sapply(sum, cat.2, na.rm=T))
rownames(cat.2)<-cat_names
colnames(cat.2)<-"High_Low"

cuad3<-cdmxmov3%>%filter(cuad=="Low Density/High DISLI")

cat.3<-NULL
cat.3<-as.matrix(cuad3 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                        diagnostico_clinico_neumonia, caso_severo, intubado, 
                                        H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                        enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                        obesidad, tabaquismo, esta_embarazada, es_indigena) %>% 
                   sapply(sum, cat.3, na.rm=T))
rownames(cat.3)<-cat_names
colnames(cat.3)<-"Low_High"

cuad4<-cdmxmov3%>%filter(cuad=="Low Density/Low DISLI")

cat.4<-NULL
cat.4<-as.matrix(cuad4 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                         diagnostico_clinico_neumonia, caso_severo, intubado, 
                                         H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                         enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                         obesidad, tabaquismo, esta_embarazada, es_indigena) %>% 
                   sapply(sum, cat.4, na.rm=T))
rownames(cat.4)<-cat_names
colnames(cat.4)<-"Low_Low"

p.cat<-cbind(cat.1, cat.2, cat.3, cat.4)

Ps<-as.vector(apply(p.cat,1,Chsq))
p.cat1<-data.frame(cbind(cat.gen, p.cat))

p.cat2<-p.cat1%>%mutate(p_1=(High_High/nrow(cdmxmov3%>%filter(cuad=="High Density/High DISLI"))),
                        p_2=(High_Low/nrow(cdmxmov3%>%filter(cuad=="High Density/Low DISLI"))),
                        p_3=(Low_High/nrow(cdmxmov3%>%filter(cuad=="Low Density/High DISLI"))),
                        p_4=(Low_Low/nrow(cdmxmov3%>%filter(cuad=="Low Density/Low DISLI"))),
                        p_general=(General/nrow(cdmxmov3)))
rownames(p.cat2)<-cat_names
p.cat2<-cbind(p.cat2, Ps)

age<-cdmxmov2%>%dplyr::select(edad, cuad)
age1<-age%>%filter(cuad=="High Density/High DISLI")
age2<-age%>%filter(cuad=="High Density/Low DISLI")
age3<-age%>%filter(cuad=="Low Density/High DISLI")
age4<-age%>%filter(cuad=="Low Density/Low DISLI")

p<-kruskal.test(edad~cuad, data=age)$p.value

ages<-c(mean(age$edad), mean(age1$edad), mean(age2$edad), mean(age3$edad), mean(age4$edad), sd(age$edad), 
        sd(age1$edad), sd(age2$edad), sd(age3$edad), sd(age4$edad), p)

p.cat2<-rbind(p.cat2, ages)

p.cat2<-round(p.cat2, digits=4)

write.csv(p.cat2, "cdmx_cat.csv") 

#Tabla trabajadores

cat.gen<-NULL
cat.gen<-as.matrix(cdmxmov3 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP,
                                               diagnostico_clinico_neumonia, caso_severo, intubado, 
                                               H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                               enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                               obesidad, tabaquismo, esta_embarazada, 
                                               es_indigena)%>%sapply(sum, cat.gen, na.rm=T))

rownames(cat.gen)<-cat_names
colnames(cat.gen)<-"General"

group1<-cdmxmov3%>%filter(CAT_TRABAJO_INEGI==1)

cat.health<-NULL
cat.health<-as.matrix(group1 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP,
                                               diagnostico_clinico_neumonia, caso_severo, intubado, 
                                               H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                               enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                               obesidad, tabaquismo, esta_embarazada, 
                                               es_indigena) %>% sapply(sum, cat.health, na.rm=T))

rownames(cat.health)<-cat_names
colnames(cat.health)<-"Health"

group2<-cdmxmov3%>%filter(CAT_TRABAJO_INEGI==2)

cat.students<-NULL
cat.students<-as.matrix(group2 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                             diagnostico_clinico_neumonia, caso_severo, intubado, 
                                             H_7d, diabetes, hipertension, asma,  epoc, VIH_SIDA, 
                                             enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                             obesidad, tabaquismo, esta_embarazada, 
                                             es_indigena) %>% sapply(sum, cat.students, na.rm=T))
rownames(cat.students)<-cat_names
colnames(cat.students)<-"Students"

group3<-cdmxmov3%>%filter(CAT_TRABAJO_INEGI==3)

cat.active<-NULL
cat.active<-as.matrix(group3 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                              diagnostico_clinico_neumonia, caso_severo, intubado, 
                                              H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                              enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                              obesidad, tabaquismo, esta_embarazada, 
                                              es_indigena) %>% sapply(sum, cat.active, na.rm=T))
rownames(cat.active)<-cat_names
colnames(cat.active)<-"Active"

group4<-cdmxmov3%>%filter(CAT_TRABAJO_INEGI==4)

cat.other<-NULL
cat.other<-as.matrix(group4 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                            diagnostico_clinico_neumonia, caso_severo, intubado, 
                                            H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                            enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                            obesidad, tabaquismo, esta_embarazada, 
                                            es_indigena) %>% sapply(sum, cat.other, na.rm=T))
rownames(cat.other)<-cat_names
colnames(cat.other)<-"Other"

group5<-cdmxmov3%>%filter(CAT_TRABAJO_INEGI==5)

cat.home<-NULL
cat.home<-as.matrix(group5 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                              diagnostico_clinico_neumonia, caso_severo, intubado, 
                                              H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                              enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                              obesidad, tabaquismo, esta_embarazada, 
                                              es_indigena) %>% sapply(sum, cat.home, na.rm=T))
rownames(cat.home)<-cat_names
colnames(cat.home)<-"Home"

group6<-cdmxmov3%>%filter(CAT_TRABAJO_INEGI==6)

cat.unemployed<-NULL
cat.unemployed<-as.matrix(group6 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                              diagnostico_clinico_neumonia, caso_severo, intubado, 
                                              H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                              enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                              obesidad, tabaquismo, esta_embarazada, 
                                              es_indigena) %>% sapply(sum, cat.unemployed, na.rm=T))
rownames(cat.unemployed)<-cat_names
colnames(cat.unemployed)<-"Unemployed"

group7<-cdmxmov3%>%filter(CAT_TRABAJO_INEGI==7)

cat.retired<-NULL
cat.retired<-as.matrix(group7 %>% dplyr::select(sexo, covid, asintomaticos, caso_defuncion, HOSP, 
                                              diagnostico_clinico_neumonia, caso_severo, intubado, 
                                              H_7d, diabetes, hipertension, asma, epoc, VIH_SIDA, 
                                              enfermedad_cardiaca, insuficiencia_renal_cronica, 
                                              obesidad, tabaquismo, esta_embarazada, 
                                              es_indigena) %>% sapply(sum, cat.retired, na.rm=T))
rownames(cat.retired)<-cat_names
colnames(cat.retired)<-"Retired"

p.cat<-cbind(cat.health, cat.students, cat.active, cat.other, cat.home, cat.unemployed, cat.retired)

Ps<-as.vector(apply(p.cat,1,Chsq))
p.cat1<-data.frame(cbind(cat.gen, p.cat))

p.cat2<-p.cat1%>%mutate(p_general=(General/nrow(cdmxmov2)),
                        p_health=(Health/nrow(group1)),
                        p_students=(Students/nrow(group2)),
                        p_active=(Active/nrow(group3)),
                        p_other=(Other/nrow(group4)),
                        p_home=(Home/nrow(group5)),
                        p_unemployed=(Unemployed/nrow(group6)),
                        p_retired=(Retired/nrow(group7)))
rownames(p.cat2)<-cat_names
p.cat2<-cbind(p.cat2, Ps)

age<-cdmxmov2%>%dplyr::select(edad, CAT_TRABAJO_INEGI)
age1<-age%>%filter(CAT_TRABAJO_INEGI==1)
age2<-age%>%filter(CAT_TRABAJO_INEGI==2)
age3<-age%>%filter(CAT_TRABAJO_INEGI==3)
age4<-age%>%filter(CAT_TRABAJO_INEGI==4)
age5<-age%>%filter(CAT_TRABAJO_INEGI==5)
age6<-age%>%filter(CAT_TRABAJO_INEGI==6)
age7<-age%>%filter(CAT_TRABAJO_INEGI==7)

p<-kruskal.test(edad~CAT_TRABAJO_INEGI, data=age)$p.value

ages<-c(mean(age$edad), mean(age1$edad), mean(age2$edad), mean(age3$edad), mean(age4$edad), 
        mean(age5$edad), mean(age6$edad), mean(age7$edad), 
        sd(age$edad),  sd(age1$edad), sd(age2$edad), sd(age3$edad), sd(age4$edad),
        sd(age5$edad),  sd(age6$edad), sd(age7$edad), p)

p.cat2<-rbind(p.cat2, ages)

p.cat2<-round(p.cat2, digits=4)

write.csv(p.cat2, "cdmx_cat_t.csv") 

#5.Mapas####
library(ggstance); library(flextable); library(simPH); library(ggthemes); library(lme4); 
library(lmerTest); library(prismatic); library(sf); library(rnaturalearth); 
library(rnaturalearthdata); library(maps); library(rmapshaper)

load("df_mx.rda")
dfe3 <- df_mx %>% 
  filter(A?O==2020) %>% 
  transmute(
    age_levels = sub("-mm", "+", sub("_","-",substr(EDAD_QUIN,6,10))),
    sex = SEXO %>%substr(1,1),
    id = CLAVE,
    value = POB) %>%
  mutate(age=case_when(
    age_levels=="00-04" | age_levels== "05-09" | age_levels=="10-14" ~ "0-15",
    age_levels== "15-19" ~ "15-19",
    age_levels=="20-24" | age_levels== "25-29" ~ "20-29",
    age_levels=="30-34" | age_levels== "35-39" ~ "30-39",
    age_levels=="40-44" | age_levels== "45-49" ~ "40-49",
    age_levels=="50-54" | age_levels== "55-59" | age_levels=="60-64"~ "50-64",
    age_levels=="65+"~ "65+",
  ))

dfe3$id<-str_pad(dfe3$id, 5,pad = "0")
df_mx$id<-str_pad(df_mx$CLAVE, 5,pad = "0")
pop_mun<-df_mx %>% filter(A?O==2020)%>%group_by(id)%>%summarise(pop=sum(POB))

mx_mun <- st_read(dsn="shapes", layer="areas_geoestadisticas_municipales", 
                  stringsAsFactors=FALSE) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
bord <-  st_read(dsn="shapes", layer="areas_geoestadisticas_estatales")  %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% 
  rmapshaper::ms_innerlines()
zm <- st_read(dsn="shapes", layer="ZM_2010", stringsAsFactors=FALSE) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

mx_mun$id<-paste0(str_pad(mx_mun$CVE_ENT, 2,pad = "0"),str_pad(mx_mun$CVE_MUN,3, pad="0"))

#Cargar las bases de poblacion 
pop<-read.csv("conapo.csv")
names(pop)<-c("r", "year", "ENTIDAD", "CVE", "EDAD", "SEXO", "pop")
#Poblacion por lugar de origen
pop<-df_mx %>% filter(A?O==2020)%>% group_by(id) %>%
  summarise(pop=sum(POB))%>%dplyr::select(id, pop)
#Seleccionar la base de CDMX
covid_casos<- cdmxmov2 %>% left_join(pop, by="id") 
#Seleccionar casos por id
covid_casos_2<- covid_casos %>%
  group_by(id) %>%
  summarise(cases=sum(covid), 
            asympt=sum(asintomaticos),
            deaths=sum(evolucion_casoDEFUNCION==1 & covid==1), 
            pop=median(pop), 
            marg=median(marg), 
            mov=mean(i_mov, na.rm=T),
            tested=n())%>%
  mutate(rate_incidence=cases*100000/pop, 
         rate_mort=deaths*100000/pop, 
         rate_letal=(deaths/cases)*100,
         rate_pruebas=(cases*100000/pop), 
         rate_as=(asympt/cases)*100, 
         rate_pos=(cases/tested)*100)%>%
  dplyr::select(id, rate_incidence, rate_mort, rate_letal,pop,marg,mov,rate_pruebas, 
                rate_as, rate_pos) #rate 1 es tasa de incidencia / rate 2 es 
#tasa de mortalidad

#Unir con la base poblacional
mx_mun_2<-mx_mun %>%left_join(covid_casos_2, by="id")
cdmx1<- cdmx %>% dplyr::filter(covid==1)

#Creamos un df con los datos de cada punto por delegacion
names_CDMX<- data.frame(longitude = c(-99.234644,-99.184834,-99.162658,-99.148994,-99.309952,
                                      -99.147956,-99.120309,-99.098115,-99.060128,-99.268249,
                                      -99.200661,-99.024215,-99.011502,-99.215883,-99.089719,
                                      -99.097766), 
                        latitude  = c(19.356390, 19.486807, 19.379518, 19.327041, 19.341047, 
                                      19.437349, 19.491101, 19.395161, 19.348187, 19.269310, 
                                      19.434724, 19.1540, 19.285377, 19.214526, 19.429385, 
                                      19.245850),
                        id        = c("09010", "09002", "09014", "09003", "09004", "09015", 
                                      "09005", "09006", "09007", "09008", "09016", "09009", 
                                      "09011", "09012", "09017", "09013"),
                        name      = c("Alvaro Obregon","Azcapozalco", "Benito Juarez", 
                                      "Coyoacan", "Cuajimalpa", "Cuautemoc", 
                                      "La PODEROSISIMA GAM", "Iztacalco", "Iztapalapa", 
                                      "Magdalena Contreras", "Miguel Hidalgo", "Milpa Alta", 
                                      "Tlahuac", "Tlalpan", "Venustiano Carranza",
                                      "Xochimilco"))

names_CDMX_2<- names_CDMX %>% left_join(mx_mun_2, by="id") 

#Crear las variables de tercilas y cuartilas
names_CDMX_2$marg_cuart<-split_quantile(x = names_CDMX_2$marg, type = 4)
names_CDMX_2$marg_ter<-split_quantile(x = names_CDMX_2$marg, type = 3)

#Agrupar a todos por cuartilas

mx_mun_2_CDMX<-mx_mun_2%>%filter(CVE_ENT=="09")

mx_mun_2_CDMX$rate_incidence_cat<-NULL;
mx_mun_2_CDMX$rate_incidence_cat[mx_mun_2_CDMX$rate_incidence<=400]<-1;
mx_mun_2_CDMX$rate_incidence_cat[mx_mun_2_CDMX$rate_incidence>400 & 
                                   mx_mun_2_CDMX$rate_incidence<=800]<-2;
mx_mun_2_CDMX$rate_incidence_cat[mx_mun_2_CDMX$rate_incidence>800 & 
                                   mx_mun_2_CDMX$rate_incidence<=1200]<-3
mx_mun_2_CDMX$rate_incidence_cat[mx_mun_2_CDMX$rate_incidence>=1200]<-4

mx_mun_2_CDMX$rate_incidence_cat<-as.factor(mx_mun_2_CDMX$rate_incidence_cat)
mx_mun_2_CDMX$rate_incidence_cat<-factor(mx_mun_2_CDMX$rate_incidence_cat, 
                                         levels = c(1,2,3,4), 
                                         labels = c("<400","400-800","800-1200",">1200"))
mx_mun_2_CDMX$marg_cuart<-split_quantile(x = mx_mun_2_CDMX$marg, type = 4)
mx_mun_2_CDMX$marg_ter<-split_quantile(x = mx_mun_2_CDMX$marg, type = 3)
mx_mun_2_CDMX$mortcat<-split_quantile(x=mx_mun_2_CDMX$rate_mort, type=3)
mx_mun_2_CDMX$mortcat<-factor(mx_mun_2_CDMX$mortcat,
                              labels=c("<62", "63-85", ">86"))
mx_mun_2_CDMX$poscat<-split_quantile(x=mx_mun_2_CDMX$rate_pos, type=3)
mx_mun_2_CDMX$poscat<-factor(mx_mun_2_CDMX$poscat,
                             labels=c("<38%","38%-39%", ">40%"))

mob1<-data.frame(mob1)
mob1$CVE_MUN<-seq(from=2, to=17)
mob1$CVE_MUN<-str_pad(mob1$CVE_MUN, 3, pad = "0")

mx_mun_2_CDMX<-arrange(mx_mun_2_CDMX, by=CVE_MUN)

mx_mun_2_CDMX$movcuart<-split_quantile(x=mx_mun_2_CDMX$mov, type=4)
mx_mun_2_CDMX$avemov<-mob1$mob1
mx_mun_2_CDMX$amovcuart<-split_quantile(x=mx_mun_2_CDMX$avemov, type=4)
mx_mun_2_CDMX$amovcuart<-factor(mx_mun_2_CDMX$amovcuart,
                             labels=c("Biggest\ndecrease","Big\ndecrease", 
                                      "Small\ndecrease", "Smallest\ndecrease"))

mx_mun_2_CDMX_cent <- st_centroid(mx_mun_2_CDMX)

#Series de mapas (movilidad)

cdmx_data<-covid_casos%>% 
  filter(cve_entidad_residencia==09)%>%
  mutate(month=data.table::month(fecha_ingreso))%>%
  group_by(month,id)%>% 
  summarise(cases_def=sum(evolucion_casoDEFUNCION==1 & covid==1, na.rm=T), 
            tested=n(),cases=sum(covid), 
            pop=median(pop),
            marg=median(marg),
            mov=mean(i_mov, na.rm=T))%>%
  mutate(rate_incidence=cases/pop*100000, rate_mort=cases_def/pop*100000, 
         rate_letal=(cases_def/cases)*100)%>% filter(month>=3 & month<=12)

mx_mun_3<-mx_mun%>%left_join(cdmx_data, by="id")

mx_mun_marzo<-mx_mun_3%>%filter(month==3)
mx_mun_abril<-mx_mun_3%>%filter(month==4)
mx_mun_mayo<-mx_mun_3%>%filter(month==5)
mx_mun_junio<-mx_mun_3%>%filter(month==6)
mx_mun_julio<-mx_mun_3%>%filter(month==7)
mx_mun_agosto<-mx_mun_3%>%filter(month==8)
mx_mun_septiembre<-mx_mun_3%>%filter(month==9)
mx_mun_octubre<-mx_mun_3%>%filter(month==10)
mx_mun_noviembre<-mx_mun_3%>%filter(month==11)
mx_mun_diciembre<-mx_mun_3%>%filter(month==12)

#Marzo
cdmx_marzo<-ggplot() +
  geom_sf(data = bord, color = "darkgray", size = .7)+
  geom_sf(data=mx_mun_marzo, aes(fill = mov, geometry=geometry), color = "darkgray") +
  coord_sf(xlim = c( -99.446848, -98.877881), ylim = c(19.002259, 19.658663), 
           expand = FALSE) +
  theme_map()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Relative percent mobility diminish")+ 
  scale_fill_gradient(low = "green4", high = "red", limits = c(-85,20))

#Abril
cdmx_abril<-ggplot() +
  geom_sf(data=mx_mun_abril, aes(fill = mov, geometry=geometry), color = "darkgray") +
  geom_sf(data = bord, color = "darkgray", size = .7)+
  coord_sf(xlim = c( -99.446848, -98.877881), ylim = c(19.002259, 19.658663),
           expand = FALSE) +
  theme_map()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Relative percent mobility diminish")+ 
  scale_fill_gradient(low = "green4", high = "red", limits = c(-85,20))
#Mayo
cdmx_mayo<-ggplot() +
  geom_sf(data=mx_mun_mayo, aes(fill = mov, geometry=geometry), color = "darkgray") +
  geom_sf(data = bord, color = "darkgray", size = .7)+
  coord_sf(xlim = c( -99.446848, -98.877881), ylim = c(19.002259, 19.658663), 
           expand = FALSE)+
  theme_map()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Relative percent mobility diminish")+ 
  scale_fill_gradient(low = "green4", high = "red", limits = c(-85,20))
#Junio
cdmx_junio<-ggplot() +
  geom_sf(data=mx_mun_junio, aes(fill = mov, geometry=geometry), color = "darkgray") +
  geom_sf(data = bord, color = "darkgray", size = .7)+
  coord_sf(xlim = c( -99.446848, -98.877881), ylim = c(19.002259, 19.658663), 
           expand = FALSE) +
  theme_map()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Relative percent mobility diminish")+ 
  scale_fill_gradient(low = "green4", high = "red", limits = c(-85,20))
#Julio
cdmx_julio<-ggplot() +
  geom_sf(data=mx_mun_julio, aes(fill = mov, geometry=geometry), color = "darkgray" ) +
  geom_sf(data = bord, color = "darkgray", size = .7)+
  coord_sf(xlim = c( -99.446848, -98.877881), ylim = c(19.002259, 19.658663), 
           expand = FALSE) +
  theme_map()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Relative percent mobility diminish")+ 
  scale_fill_gradient(low = "green4", high = "red", limits = c(-85,20))
#Agosto
cdmx_agosto<-ggplot() +
  geom_sf(data=mx_mun_agosto, aes(fill = mov, geometry=geometry), color = "darkgray") +
  geom_sf(data = bord, color = "darkgray", size = .7)+
  coord_sf(xlim = c( -99.446848, -98.877881), ylim = c(19.002259, 19.658663), 
           expand = FALSE) +
  theme_map()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Relative percent mobility diminish")+ 
  scale_fill_gradient(low = "green4", high = "red", limits = c(-85,20))
#Septiembre
cdmx_septiembre<-ggplot() +
  geom_sf(data=mx_mun_septiembre, aes(fill = mov, geometry=geometry), color = "darkgray" ) +
  geom_sf(data = bord, color = "darkgray", size = .7)+
  coord_sf(xlim = c( -99.446848, -98.877881), ylim = c(19.002259, 19.658663), 
           expand = FALSE) +
  theme_map()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Relative percent mobility diminish")+ 
  scale_fill_gradient(low = "green4", high = "red", limits = c(-85,20))
#Octubre
cdmx_octubre<-ggplot() +
  geom_sf(data=mx_mun_octubre, aes(fill = mov, geometry=geometry), color = "darkgray" ) +
  geom_sf(data = bord, color = "darkgray", size = .7)+
  coord_sf(xlim = c( -99.446848, -98.877881), ylim = c(19.002259, 19.658663), 
           expand = FALSE) +
  theme_map()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Relative percent mobility diminish")+ 
  scale_fill_gradient(low = "green4", high = "red", limits = c(-85,20))
#Noviembre
cdmx_noviembre<-ggplot() +
  geom_sf(data=mx_mun_noviembre, aes(fill = mov, geometry=geometry), color = "darkgray" ) +
  geom_sf(data = bord, color = "darkgray", size = .7)+
  coord_sf(xlim = c( -99.446848, -98.877881), ylim = c(19.002259, 19.658663), 
           expand = FALSE) +
  theme_map()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Relative percent mobility diminish")+ 
  scale_fill_gradient(low = "green4", high = "red", limits = c(-85,20))
#diciembre
cdmx_diciembre<-ggplot() +
  geom_sf(data=mx_mun_diciembre, aes(fill = mov, geometry=geometry), color = "darkgray" ) +
  geom_sf(data = bord, color = "darkgray", size = .7)+
  coord_sf(xlim = c( -99.446848, -98.877881), ylim = c(19.002259, 19.658663), 
           expand = FALSE) +
  theme_map()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Relative percent mobility diminish")+ 
  scale_fill_gradient(low = "green4", high = "red", limits = c(-85,20))


mov_months<-ggarrange(cdmx_marzo, cdmx_abril, cdmx_mayo, cdmx_junio, cdmx_julio,
                      cdmx_agosto, cdmx_septiembre, cdmx_octubre, cdmx_noviembre,
                      cdmx_diciembre, ncol=5, nrow=2, common.legend = TRUE, 
                      legend="bottom",
                      labels=c("March 2020", "April 2020", "May 2020", "June 2020", 
                               "July 2020", "August 2020", "September 2020", "October 2020",
                               "November 2020", "December 2020"))

ggsave(mov_months, filename = "mov_months.png", width = 40, height = 20, units=c("cm"), 
       dpi = 600,  limitsize = FALSE)

