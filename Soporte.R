# Librerías -----------------

# Instalar paquetes (única vez)
install.packages("dplyr")
install.packages("tidyverse")


# Cargar paquetes (cada vez que abrimos el script)

library(dplyr)     # Manipulación y limpieza de datos
library(readr)     # Carga de archivos
library(ggplot2)   # Hacer gráficos


# Fuente de datos ------------

encuesta <- read_delim("https://raw.githubusercontent.com/r4hr/r4hr_introduccion_dplyr/main/Datos/encuesta_sueldos.csv",
                       delim = ",")


# Inicio ----------

# ¿Cómo saber si un paquete está cargado?

glimpse(encuesta)


# El "pipe" (Ctrl + Shift + M) -------

encuesta %>% 
  select(puesto, sueldo_bruto) %>% # Selecciona las columnas
  group_by(puesto) %>%     # Agrupa por la variable puesto
  summarise(sueldo_promedio = mean(sueldo_bruto)) %>% # Crea una variable con el sueldo promedio
  arrange(-sueldo_promedio) # Ordena descendentemente los resultados por la variable que pasemos.


# Ejemplo de código encadena sin pipe
arrange(filter(select(encuesta, puesto, sueldo_bruto, personas_a_cargo), personas_a_cargo > 0), puesto)

# Ejemplo de código encadena con pipe
encuesta %>%
  select(puesto, sueldo_bruto, personas_a_cargo) %>%
  filter(personas_a_cargo>0) %>%
  arrange(puesto)



# Funciones principales de dplyr ------------

# Select ------

# Elegir las columnas de satisfaccion y sueldo_bruto
encuesta %>% 
  select(satisfaccion, sueldo_bruto)

# Variante: Selección por condiciones
encuesta %>%
  select_if(is.numeric) # Selecciona variables numéricas únicamente



# Filter ------

# Filtrar las respuestas provenientes de universidades públicas
encuesta %>%
  # Prestar atención al doble signo igual "=="
  filter(universidad == "Universidad Pública")



# Filtrar casos que no trabajen en el ámbito público
encuesta %>%
  # Prestar atención al signo "No igual a "!="
  filter(universidad != "Función pública")

# Operadores lógicos
# Filtramos los empleados que son de Universidad Privada y de empresas con más de 700 empleados.
encuesta %>%
  filter(universidad == "Universidad Privada" &  empleados > 700)


# Caso de Estudio 1 --------------

ggplot(encuesta, aes(x = sueldo_bruto)) + # Defino fuente de datos y variable a graficar
  geom_histogram() +                  # Tipo de gráfico
  ggtitle("Distribución de sueldos brutos") # Agrego título al gráfico


summary(encuesta$sueldo_bruto)


encuesta_limpia <- encuesta %>% 
  filter(between(sueldo_bruto,   # Variable a filtrar 
                 50000,          # Valor mínimo
                 200000))        # Valor máximo

# Vuelvo a graficar
ggplot(encuesta_limpia, aes(x = sueldo_bruto)) +
  geom_histogram() +
  ggtitle("Distribución de sueldos brutos")


# Filtrar por dos (o más) valores de una misma variable

# Ver resultados del rubro Comerio y Servicios de Salud
encuesta %>%
  filter(rubro == "Comercio"| rubro == "Servicios de salud")


# Una alternativa es usar un vector de selección
encuesta %>%
  filter(rubro %in% c("Comercio", "Servicios de salud")) 




# Group_by y summarise -----------

# Calcular sueldo bruto por puesto
encuesta %>%
  select(puesto, sueldo_bruto) %>%
  # Agrupo por puesto
  group_by(puesto) %>%  #<<
  #Calculo el sueldo promedio
  summarise(sueldo_promedio = mean(sueldo_bruto))





# Mutate ------------

# Crear una columna nueva en base a variables existentes
encuesta %>%
  select(puesto, sueldo_bruto) %>%
  mutate(sueldo_bruto_anual = sueldo_bruto * 13)


# Cambiar las características de una variable
class(encuesta$puesto)

# Cambiamos la variable a tipo factor
encuesta %>% 
  select(puesto, sueldo_bruto) %>% 
  mutate(puesto = factor(puesto, 
                         levels = c("Director", "Gerente", "Jefe", "Responsable",
                                    "HRBP", "Analista", "Administrativo"))) %>% # Ordena la variable
  group_by(puesto) %>% 
  summarise(sueldo_promedio = mean(sueldo_bruto))


# Case when - Crear rangos a partir de una variable numérica 

encuesta <- encuesta %>%  
  mutate(cantidad_empleados = case_when( 
    empleados <= 100 ~ "Hasta 100",      
    empleados <= 500 ~ "Hasta 500",      
    empleados <= 1000 ~ "Hasta 1000",    
    empleados <= 5000 ~ "Hasta 5000",    
    empleados = TRUE ~ "Más de 5000"     
  )) 

encuesta %>% 
  group_by(cantidad_empleados) %>% 
  summarise(sueldo_promedio = mean(sueldo_bruto))

# Caso de Estudio 2 -------------
# Secuencia de análisis

## 1. Agrupar los grupos que quiero comparar (group_by)
## 2. Calcular medidas de resumen estadístico (summarise, mutate)
## 3. Graficar los resultados (ggplot)

# Creo un objeto nuevo
analisis_sueldos <- encuesta %>% 
  # Agrupo por origen_capital y cantidad_empleados
  group_by(origen_capital, cantidad_empleados) %>% 
  # Calculo el sueldo promedio
  summarise(sueldo_promedio = mean(sueldo_bruto))

# Veo los resultados
analisis_sueldos

# Grafico los resultados
ggplot(analisis_sueldos, aes(x = cantidad_empleados,
                             y = sueldo_promedio,
                             fill = origen_capital)) + # Color de las barras
  geom_col(position = "dodge") # Posiciona las barras una al lado de la otra


# Realizo un gráfico con mejor formato

library(scales) # Modificar formato escalas gráfico
library(ggthemes) # Añade más opciones de formato a los gráficos

ggplot(analisis_sueldos, aes(x = cantidad_empleados,
                             y = sueldo_promedio,
                             fill = origen_capital)) + # Color de las barras
  geom_col(position = "dodge") +
  scale_fill_economist() + # Cambia paleta de colores de las barras.
  # Añade separador de miles al eje y
  scale_y_continuous(labels = comma_format(big.mark = ".", 
                                           decimal.mark = ",")) +
  labs(title = "Sueldo Promedio por Origen de Capital y Tamaño de Empresa", #Título
       subtitle = "En Pesos Argentinos", #Subtítulo
       x = "", y ="",                    # Elimino etiquetas de eje
       fill = "Origen de Capital",       # Cambio nombre colores
       caption = "HR Bootcamp - Humanos Reales \n Fuente: Encuesta KIWI de Sueldos de RH") + # Agrega nota al pie
  theme_economist() # Modifica el estilo del gráfico
