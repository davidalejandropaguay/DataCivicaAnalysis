
install.packages('gtools')
install.packages('Amelia')
library(foreign)
library(readxl)
library(expss)
library(ggplot2)
library(plotly)
library(gtools)
library(Amelia)
library(dplyr)

rm(list=ls())

labels <- read_excel('./defunciones_base_datos_2024_dbf/Descripcion_BD_Defunciones_2024.xlsx', sheet = 'DEFUN24VARIABLES')

defun20 <- read.dbf("./defunciones_base_datos_2020_dbf/DEFUN20.dbf")
defun21 <- read.dbf("./defunciones_base_datos_2021_dbf/DEFUN21.dbf")
defun22 <- read.dbf("./defunciones_base_datos_2022_dbf/DEFUN22.dbf")
defun23 <- read.dbf("./defunciones_base_datos_2023_dbf/DEFUN23.dbf")
defun24 <- read.dbf("./defunciones_base_datos_2024_dbf/DEFUN24.dbf")


# solo homicidios: las bases de 2020 y 2021, no contien la variable TIPO_DEFUN
# Se podría tomar las CAUSA_DEF clasificadas como homicidios (TIPO_DEFUN==2) de los años posteriores y asumirlos como homicidios en las bases 2020 y 2021, pero podría ser arriesgado.
# Para mayor seguridad, se excluye 2020 y 2021. Sin embargo, en caso de que se quisiese hacer eso, sería de la siguiente forma:

# defun <- smartbind(defun20, defun21, defun22, defun23, defun24)
# defun$CAUSA_DEF <- as.character(defun$CAUSA_DEF)
# Defunciones.Homicidios <- data.frame(CAUSA_DEF = as.character(defun$CAUSA_DEF[which(defun$TIPO_DEFUN==2)]),
#                                      TIPO_DEFUN_ESTIMADA=2)
# Defunciones.Homicidios <- unique(Defunciones.Homicidios)
# defun <- merge(x = defun, y = Defunciones.Homicidios, by = 'CAUSA_DEF')
# 
# # visualización de vacíos
# missmap(defun)


# el análsis continúa sólo con los años 22-2024
defun.2224 <- smartbind(defun22, defun23, defun24)

defun.2224 <- apply_labels(defun.2224,
                      ENT_REGIS='Entidad de registro.',
                      MUN_REGIS='Municipio o demarcación territorial de registro.',
                      TLOC_REGIS='Tamaño de localidad de registro.',
                      LOC_REGIS='Localidad de registro.',
                      ENT_RESID='Entidad de residencia habitual del (la) fallecido (a).',
                      MUN_RESID='Municipio o demarcación territorial de residencia habitual del (la) fallecido (a).',
                      TLOC_RESID='Tamaño de localidad de residencia habitual del (la) fallecido (a).',
                      LOC_RESID='Localidad de residencia habitual del (la) fallecido (a).',
                      ENT_OCURR='Entidad de ocurrencia.',
                      MUN_OCURR='Municipio o demarcación territorial de ocurrencia.',
                      TLOC_OCURR='Tamaño de localidad de ocurrencia.',
                      LOC_OCURR='Localidad de ocurrencia.',
                      CAUSA_DEF='Causa de la defunción (lista detallada).',
                      COD_ADICIO='Código adicional CIE.',
                      LISTA_MEX='Causa de la defunción (lista mexicana).',
                      SEXO='Sexo del (la) fallecido (a).',
                      ENT_NAC='Lugar de nacimiento.',
                      AFROMEX='Condición de autoadscripción como persona afromexicana.',
                      CONINDIG='Condición de autoadscripción como persona indígena.',
                      LENGUA='Condición de habla lengua indígena.',
                      CVE_LENGUA='Clave de la lengua indígena.',
                      NACIONALID='Nacionalidad.',
                      NACESP_CVE='Nacionalidad extranjera.',
                      EDAD='Edad de la persona fallecida.',
                      SEM_GEST='Semanas de gestación de las personas fallecidas con menos de 28 días de edad.',
                      GRAMOS='Peso en gramos de las personas fallecidas con menos de 28 días de edad.',
                      DIA_OCURR='Día de ocurrencia.',
                      MES_OCURR='Mes de ocurrencia.',
                      ANIO_OCUR='Año de ocurrencia.',
                      DIA_REGIS='Día de registro.',
                      MES_REGIS='Mes de registro.',
                      ANIO_REGIS='Año de registro.',
                      DIA_NACIM='Día de nacimiento de la persona fallecida.',
                      MES_NACIM='Mes de nacimiento de la persona fallecida.',
                      ANIO_NACIM='Año de nacimiento de la persona fallecida.',
                      COND_ACT='Condición de actividad económica.',
                      OCUPACION='Ocupación de la persona fallecida.',
                      ESCOLARIDA='Nivel de escolaridad de la persona fallecida.',
                      EDO_CIVIL='Situación conyugal de la persona fallecida.',
                      TIPO_DEFUN='Tipo de defunción.',
                      OCURR_TRAB='Ocurrió en el desempeño de su trabajo.',
                      LUGAR_OCUR='Sitio de ocurrencia de la lesión.',
                      PAR_AGRE='Parentesco del presunto agresor.',
                      VIO_FAMI='Condición de violencia familiar.',
                      ASIST_MEDI='Condición de atención médica.',
                      CIRUGIA='Condición de cirugía practicada a la persona fallecida en las últimas 4 sema- nas previas al fallecimiento.',
                      NATVIOLE='La muerte fue accidental o violenta.',
                      NECROPSIA='Condición de necropsia.',
                      USONECROPS='Condición de uso de la necropsia para la certificación.',
                      ENCEFALICA='Condición de muerte encefálica.',
                      DONADOR='Condición de donador(a) de órganos.',
                      SITIO_OCUR='Sitio de ocurrencia de la defunción.',
                      COND_CERT='Persona que certificó la defunción.',
                      DERECHOHAB='Afiliación a los servicios de salud (derechohabiencia) de la persona fallecida.',
                      EMBARAZO='Condición de embarazo.',
                      REL_EMBA='Causas relacionadas con el embarazo.',
                      HORAS='Hora de la defunción.',
                      MINUTOS='Minuto de la defunción.',
                      CAPITULO='Causas detalladas CIE (capítulo).',
                      GRUPO='Causas detalladas CIE (grupo).',
                      LISTA1='Lista de tabulación 1 