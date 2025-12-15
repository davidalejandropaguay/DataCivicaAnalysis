
install.packages("foreign")
install.packages("expss")
install.packages("ggplot2")
install.packages("plotly")

library(foreign)
library(readxl)
library(expss)
library(ggplot2)
library(plotly)


rm(list=ls())

labels <- read_excel('./defunciones_base_datos_2024_dbf/Descripcion_BD_Defunciones_2024.xlsx', sheet = 'DEFUN24VARIABLES')
defun24 <- read.dbf("./defunciones_base_datos_2024_dbf/DEFUN24.dbf")
defun24 <- apply_labels(defun24,
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


homicidios <- subset(defun24, subset = TIPO_DEFUN==2)


# localidad <- read.dbf("./defunciones_base_datos_2024_dbf/CATEMLDE24.dbf", as.is = T)
# lenguas <- read.dbf("./defunciones_base_datos_2024_dbf/LENGUAS.dbf", as.is = T)
# listamex <- read.dbf("./defunciones_base_datos_2024_dbf/GPOLIMEX.dbf", as.is = T)
# parentesco <- read.dbf('./defunciones_base_datos_2024_dbf/PARENTESCO.dbf')
# table(homicidios$LENGUA)



# Qué ---------------------------------------------------------------------

# What—accidente? (1: sí; 8: NAp)
ggplotly(ggplot(homicidios, aes(x = NATVIOLE, fill=as.factor(SEXO))) +
  geom_bar()
  )


# What—exactamente (X954—AGRESION CON DISPARO DE OTRAS ARMAS DE FUEGO, Y LAS NO ESPECIFICADAS, EN CALLES Y CARRETERAS)
ggplotly(ggplot(homicidios, aes(x = CAUSA_DEF)) +
  geom_bar() +
  coord_flip()
  )

# Quién (víctima)---------------------------------------------------------------------

# Who—sexo (1: H; 2:M) >90% son hombres
ggplotly(ggplot(homicidios, aes(x=CAUSA_DEF, fill = as.factor(SEXO))) +
  geom_bar() +
  coord_flip() +
    theme(axis.text.y = element_text(size = 5)) +
    scale_fill_discrete(name="Sexo",breaks=c("1","2","9"), labels=c("Hombre","Mujer","Indeterminado"))
)


# Who—grupo etáreo  > la mayoría tienen entre 30 y 39 años
ggplotly(ggplot(homicidios, aes(x = CAUSA_DEF, fill = EDAD_AGRU)) +
  geom_bar() +
  coord_flip())

# Who—distribución etáreo  > 
homicidios$edad_estimada <- homicidios$ANIO_OCUR - homicidios$ANIO_NACIM
View(homicidios[,c("edad_estimada", "ANIO_OCUR", "ANIO_NACIM")])
edad_estimada <- subset(homicidios, 
                        subset = (ANIO_OCUR != 9999 & ANIO_NACIM != 9999), 
                        select = c(edad_estimada, SEXO))

ggplotly(ggplot(edad_estimada, aes(x = edad_estimada, fill=as.factor(SEXO))) +
  geom_histogram()
)

# Who—enfocado en algún grupo vulnerable?
ggplotly(
  ggplot
)

# Quién (victimario)---------------------------------------------------------------------

# Who—victimario: No especificado
ggplotly(ggplot(homicidios, aes(x = as.factor(PAR_AGRE), fill = as.factor(SEXO))) +
  geom_bar()
)

# Dónde ---------------------------------------------------------------------
# Where—entidad federativa

mexico_political <- read_excel("./catun_municipio/AGEEML_2025102162256_UTF.xlsx", skip = 3, sheet = "Consulta")
homicidios$codubicacion <- paste0(homicidios$ENT_REGIS, homicidios$MUN_REGIS)
homicidios <- merge(x = homicidios,
                    y = subset(mexico_political, select = c(CVEGEO, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN)),
                    by.x = "codubicacion",
                    by.y = "CVEGEO",
                    all.x = T)

# table(homicidios$ENT_REGIS, homicidios$NOM_ENT)
# table(homicidios$MUN_REGIS, homicidios$NOM_MUN)
ggplotly(
  ggplot(homicidios, aes(x=NOM_ENT, fill=NOM_MUN)) +
    geom_bar() +
    coord_flip()
)

# Where—lugar exacto, circunstancia, poco menos del 50% ocurrió en una carretera

lugar_ocur_nom <- read_excel('./catun_municipio/AGEEML_2025102162256_UTF.xlsx', sheet = 'lugar')
homicidios <- merge(x = homicidios,
                    y = lugar_ocur_nom, 
                    by = "LUGAR_OCUR",
                    all.x = T)

# la mayoria ocurrio en la calle o carretera, en Baja California predomina el 'se ignora' (fosas comunes?)
ggplotly(
  ggplot(homicidios, aes(x = as.factor(NOM_ENT), fill=as.factor(lugar))) +
    geom_bar()
)

# Por qué ---------------------------------------------------------------------
# hay un patrón para las muertes por edad?
ggplotly(
  ggplot(subset(homicidios, subset = (ANIO_OCUR != 9999 & ANIO_NACIM != 9999)),
                aes(x = edad_estimada,
                    fill=as.factor(lugar))) +
    geom_histogram()
)

# hay un patrón para las muertes de mujeres?—Aparente a mayor edad, incrementa la probabilidad de que el homicidio ocurra en la vivienda, pero sólo es un sesgo de selección
ggplotly(
  ggplot(subset(homicidios, subset = (SEXO==2 & (ANIO_OCUR != 9999 & ANIO_NACIM != 9999))), aes(x = edad_estimada, fill = as.factor(lugar))) +
    geom_bar()
)
ggplotly(
  ggplot(subset(homicidios, subset = (SEXO==2 & (ANIO_OCUR != 9999 & ANIO_NACIM != 9999))), aes(x = edad_estimada, fill = as.factor(lugar))) +
    geom_bar(position = "fill")
)
# table(homicidios$edad_estimada, homicidios$SEXO)


