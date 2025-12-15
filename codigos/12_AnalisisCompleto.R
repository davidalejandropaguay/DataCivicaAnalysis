
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
                      LISTA1='Lista de tabulación 1 para mortalidad de la CIE.',
                      GR_LISMEX='Lista mexicana de enfermedades (grupo).',
                      AREA_UR='Área urbana-rural de residencia habitual del (la) fallecido (a).',
                      EDAD_AGRU='Edad (agrupada) del (la) fallecido (a).',
                      COMPLICARO='Complicaron el embarazo.',
                      DIA_CERT='Día de certificación.',
                      MES_CERT='Mes de certificación.',
                      ANIO_CERT='Año de certificación.',
                      MATERNAS='Defunciones maternas totales.',
                      ENT_OCULES='Entidad de ocurrencia de la lesión.',
                      MUN_OCULES='Municipio o demarcación territorial de ocurrencia de la lesión.',
                      LOC_OCULES='Localidad de ocurrencia de la lesión.',
                      RAZON_M='Defunciones para calcular la razón de la mortalidad materna.',
                      DIS_RE_OAX='Distritos de registro de Oaxaca.')




# filtros y variables adicionales -----------------------------------------------------------------

# solo homicidios
defun.2224 <- subset(defun.2224, subset = (TIPO_DEFUN == 2))
# Edad estimada
defun.2224$edad_estimada <- defun.2224$ANIO_OCUR - defun.2224$ANIO_NACIM
# Cambio de etiquetas para SEXO
defun.2224$SEXO <- ifelse(defun.2224$SEXO==1,"Hombre", ifelse(defun.2224$SEXO==2, "Mujer", "Indefinido"))
# años de ocurrencia
View(table(defun.2224$ANIO_OCUR))

# Qué ---------------------------------------------------------------------

# What—accidente? (1: sí; 8: NAp)
g <- defun.2224 %>% 
  count(NATVIOLE, SEXO, ANIO_OCUR, sort = T) %>% 
  mutate(NATVIOLE=ifelse(NATVIOLE==1,"Sí", ifelse(NATVIOLE==2, "No", "No Aplica"))) %>% 
  group_by(NATVIOLE) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(NATVIOLE=reorder(NATVIOLE, -total))
  
ggplotly(ggplot(g, aes(x = as.factor(NATVIOLE), y=n, fill=as.factor(SEXO))) +
           geom_bar(stat = 'identity') +
           labs(
             x="La muerte fue accidental o violenta",
             y="Número de casos",
             fill="Sexo"
           )
         # + facet_wrap(~ANIO_OCUR) # se deja como opcional. Para que ayude al análisis habría que contar con información de más años. Además, tomar en cuenta que el año de registro no es igual que el de ocurrencia del evento
)


# What—exactamente (X954—AGRESION CON DISPARO DE OTRAS ARMAS DE FUEGO, Y LAS NO ESPECIFICADAS, EN CALLES Y CARRETERAS)
g <- defun.2224 %>% 
  count(CAUSA_DEF, SEXO, sort = T) %>% 
  group_by(CAUSA_DEF) %>% 
  mutate(total=sum(n)) %>% 
  ungroup() %>% 
  mutate(
    CAUSA_DEF=reorder(CAUSA_DEF, total),
    CAUSA_DEF_P=total/sum(n)
    ) %>% 
  filter(total>100)

ggplotly(ggplot(g, aes(x = CAUSA_DEF, y = n, fill = as.character(SEXO))) +
           geom_bar(stat = 'identity') +
           coord_flip() +
           theme(axis.text.y = element_text(size = 7)) +
           labs(
             x="Causa de la defunción",
             y="Número de casos",
             fill="Sexo")
)                

# Quién (víctima)---------------------------------------------------------------------

g <- defun.2224 %>% 
  count(LENGUA, edad_estimada) %>% 
  filter(edad_estimada>=0 & edad_estimada<150) %>% 
  mutate(LENGUA=case_when(
    LENGUA==1~"Sí",
    LENGUA==2~"No",
    LENGUA==8~"MD3A",
    LENGUA==9~"Se ignora",
  ))

# Edad promedio
g$edad_estimada <- as.numeric(as.character(g$edad_estimada))
g$n <- as.numeric(as.character(g$n))
sum(g$edad_estimada*(g$n/sum(g$n)))
# Mediana
median(defun.2224$edad_estimada) 
# gráfico
ggplotly(ggplot(g, aes(x=edad_estimada, y = n, fill = as.factor(LENGUA))) +
          geom_histogram(stat = 'identity') +
          labs(
            x='Edad de la víctima',
            y='Número de casos',
            fill="¿Hablaba una lengua indígena?"
          )
)

# Quién (victimario)---------------------------------------------------------------------

parentesco <- read.dbf(file = './defunciones_base_datos_2024_dbf/PARENTESCO.dbf', as.is = T)
parentesco$DESCRIP <- iconv(x = parentesco$DESCRIP, from = "CP850", to = "UTF-8")

#99% de los casos no existe especificación del agresor
g <- defun.2224 %>% 
  count(PAR_AGRE, SEXO, ANIO_OCUR)
g <- merge(x = g, y = parentesco, by.x = "PAR_AGRE", by.y = "CVE", all.x = T)
total <- sum(g$n) #99094 total de casos
g <- g %>% 
  group_by(DESCRIP) %>% 
  summarise(n=sum(n), p=100*(sum(n)/total))
importancia <- c("Concubina, compañera", "Concubino, compañero", "Esposa, Cónyuge", "Esposo, Cónyuge", "Ex esposo")
g[which(g$DESCRIP %in% importancia), 'importancia'] <- 1
sum(g$n[which(g$importancia==1)]) #105 casos
sum(g$p[which(g$importancia==1)]) #10% casos

# Diferencia entre sexos
g <- defun.2224 %>% 
  count(PAR_AGRE, SEXO, ANIO_OCUR)
g <- merge(x = g, y = parentesco, by.x = "PAR_AGRE", by.y = "CVE", all.x = T)
total <- sum(g$n) #99094 total de casos
g <- g %>% 
  group_by(DESCRIP, SEXO) %>% 
  summarise(n=sum(n), p=100*(sum(n)/total))
g[which(g$DESCRIP %in% importancia), 'importancia'] <- 1
sum(g$n[which(g$SEXO=="Hombre" & g$importancia == 1)]) #29 casos
sum(g$n[which(g$SEXO=="Mujer" & g$importancia == 1)]) #76 casos
sum(g$p[which(g$SEXO=="Hombre" & g$importancia == 1)]) #3%
sum(g$p[which(g$SEXO=="Mujer" & g$importancia == 1)]) #7%



# base para gráf
g <- defun.2224 %>% 
  count(PAR_AGRE, SEXO, ANIO_OCUR) %>% 
  filter(PAR_AGRE!=99 & PAR_AGRE!=71 & PAR_AGRE!=72)
g <- merge(x = g, y = parentesco, by.x = "PAR_AGRE", by.y = "CVE", all.x = T)

ggplotly(ggplot(g, aes(x = ANIO_OCUR, y = n, fill = as.factor(DESCRIP))) +
           geom_bar(stat = 'identity', position = "fill") +
           labs(
             x='',
             y='Porcentaje del total de casos',
             fill='Agresor'
           ) +
           facet_wrap(~SEXO)
)

