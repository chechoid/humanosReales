# Librerías -----------------

library(dplyr)
library(readr)


# Fuente de datos ------------

encuesta <- read_delim("https://raw.githubusercontent.com/r4hr/r4hr_introduccion_dplyr/main/Datos/encuesta_sueldos.csv",
                       delim = ",")


summary(encuesta)


encuesta %>% 
  group_by(universidad) %>% 
  summarise(sueldo_medio = mean(sueldo_bruto))
