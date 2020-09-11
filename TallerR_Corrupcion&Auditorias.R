########################################
# Analizando corrupción con datos de auditorías
# María Montoya Aguirre
# Septiembre 2020


# Vamos a examinar los resultados de las auditorías al gasto federalizado en 2017

# Configuración general ---------------------------------------------------

  rm(list=ls()) # Limpiar los elementos en el ambiente de R

# Cargar paquetes
  # install.packages("tidyverse") 
  library(tidyverse) # Paquete para manipulación de datos 
  options(scipen=999) # Desactivar notación científica

# Determinar directorio
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
    # Misma carpeta que este script

  

# Importar datos ------------------------------------------------------------

# Cargar base de datos
  data <- read_csv("ASFDatos_2017GastoFederalizado_20200902.csv")
  
  names(data) # Nombre de las variables
    # Vamos a renombrar las variables porque es muy problemático tener espacios 
    # y acentos en los nombres de las variables
  
# Generar un libro de variables o codebook
  variable <- c("año", "grupofuncional", "sector", "ente", "entidad", "tipoaudit",
                  "numero", "titulo", "universo", "muestra", "enteaccion", "accion",
                  "claveaccion", "accionsup", "textoaccion", "estado", "auditorias", "acciones")
  descripcion <- names(data)
  codebook <- cbind(variable, descripcion)
  
# Renombrar las variables
  names(data) <- variable


# Explorar la base --------------------------------------------------------
  # Para darse una idea general de cómo se ve la base usar glimpse o head
  glimpse(data)
  head(data)
  
  # ¿Qué valores tienen algunas de nuestras variables?
  unique(data$entidad)
  unique(data$tipoaudit)
  unique(data$accion)
  unique(data$textoaccion)


# Limpiar la base ---------------------------------------------------------
  glimpse(data)
  
  # Convertir montos en character a numeric (universo, muestra)
  data <- data %>% 
    mutate(universo = str_remove_all(universo, ","),
           universo = str_remove_all(universo,"-"),
           universo = as.numeric(universo)) %>% 
    mutate(muestra = str_remove_all(muestra, ","),
           muestra = str_remove_all(muestra,"-"),
           muestra = as.numeric(muestra)) %>% 
    mutate(universo = 1000*universo,
           muestra = 1000*muestra)
    glimpse()
  

# Generar variables de corrupción -----------------------------------------

# Dummy si se emitió un pliego de observaciones 
  data <- data %>% mutate(pliego = if_else(accion == "Pliego de Observaciones",1,0))

# Monto del pliego de observaciones (daño o perjuicio a la Hacienda Pública)
  unique(data$textoaccion) 
  # 1. Extraer la parte que dice "monto de XX pesos" y quitar la coma del monto
    data <- data %>% mutate(monto1 = str_extract(textoaccion, "monto de (.*?) pesos"),
                    monto1 = str_remove_all(monto1, ",")) 
    data$monto1

  # 2. Extraer solo el monto
    data <- data %>%  mutate(monto2 = str_extract(monto1, "\\d+\\.\\d*"),
                     monto2 = as.numeric(monto2)) 

  # Comprobar que salió bien:  
    data %>% select(monto1, monto2) %>% View()
    
  # 3. Renombrar variable y quedarnos solo con el monto definitivo  
    data <-data %>% rename(monto = monto2) %>% 
      select(-monto1)
    
  # 4. Poner monto de 0 para las observaciones que no tienen pliego de observaciones
    data <- data %>% mutate(monto = if_else(pliego !=1, 0, monto)) %>% 
      glimpse()
    
    
# Medidas correctivas 
    data <- data %>%
      mutate(correct = if_else(accion %in% c("Promoción de Responsabilidad Administrativa Sancionatoria", 
                                             "Informe de Presunta Responsabilidad Administrativa",
                                             "Promoción del Ejercicio de la Facultad de Comprobación Fiscal",
                                             "Denuncia de Hechos"),1,0))
  

    
# Análisis ----------------------------------------------------------------
    
    
# Tenemos que decidir qué recursos vamos a analizar.
  # Algunas ideas para analizar en el gasto federalizado:
    # Según el ente fiscalizado:
    unique(data$ente)
      # Universidades estatales, municipios o gobiernos de los estados
    
    # Según el fondo: 
    unique(data$titulo)
      # Participaciones federales a municipios o entidades, fondos de aportaciones específicas 
    
    # Según el sector (!) Tener cuidado con esto porque no son exactos. 
    unique(data$sector)

# Nos vamos a enfocar en algunos fondos de aportaciones (ramo 33) y si hay tiempo,
# veremos participaciones de municipios (ramo 28)
  

#   Fondo de Aportaciones para la Salud -------------------------------------

    fassa <- data %>% 
      filter(titulo =="Recursos del Fondo de Aportaciones para los Servicios de Salud") %>%
      group_by(entidad) %>% 
      summarise(monto = sum(monto, na.rm=TRUE),
                monto = monto/1000000,
                pliego = sum(pliego),
                correct = sum(correct)) %>% 
      mutate(pliego = if_else(pliego>=1, 1,0),
             correct = if_else(correct>=1, 1,0)) %>% 
      glimpse() 
    
    plot(fassa$monto)
    table(fassa$pliego,fassa$correct)
    
    graph1 <-fassa %>% ggplot(aes(monto, reorder(entidad, desc(entidad)))) +
      geom_col() +
      geom_text(aes(label= round(monto, 1), x = monto + 2), position = position_dodge(0.9), hjust=0)+
      theme_minimal() + 
      labs(title="Fondo de Aportaciones para los Servicios de Salud 2017",
           x="Monto del pliego de observaciones (mdp)", 
           y="Entidad federativa")
    

#   Fondo de Aportaciones Múltiples --------------------

    fam <- data %>% 
      filter(titulo =="Recursos del Fondo de Aportaciones Múltiples") %>%
      group_by(entidad) %>% 
      summarise(monto = sum(monto, na.rm=TRUE),
                monto = monto/1000000,
                pliego = sum(pliego)) %>% 
      mutate(pliego = if_else(pliego>=1, 1,0)) %>% 
      glimpse() 
    
    
    graph2 <- fam %>% ggplot(aes(monto, reorder(entidad, desc(entidad)))) +
      geom_col() +
      geom_text(aes(label= round(monto, 1), x = monto + 2), position = position_dodge(0.9), hjust=0)+
      theme_minimal() + 
      labs(title="Fondo de Aportaciones Múltiples 2017",
           x="Monto del pliego de observaciones (mdp)", 
           y="Entidad federativa")
    

    
