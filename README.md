# PRIMERA-EVALUACION-
# 2.3Variable numérica
base_85[sapply(base_85, is.numeric)] <- lapply(base_85[sapply(base_85, is.numeric)], as.factor)

## 3. Análisis descriptivo

### 3.1 Frecuencias

### Variable Inicial: percepcion_1_a
table(base_85$percepcion_1_a)

### Variable dependiente: percepcion_1_a en porcentaje 
table(base_85$percepcion_1_a)
table_1 <- table(base_85$percepcion_1_a)
prop.table(table_1)

### 3.2 Ejemplo tablas de contingencia: percepcion_1_a y sexo

## Visualizacion en RStudio
sjt.xtab(  base_85$percepcion_1_a, # Variable 1
  base_85$sexo, # Variable 2
  var.labels = c("Percepcion", "Sexo"), # Nombres de las variables
  show.exp = TRUE
)

## Visualizacion en word
sjt.xtab(base_85$percepcion_1_a, #filas
         base_85$sexo, #columnas
         file = "1.doc")

### Ejemplo tablas de contingencia: percepcion_1_a y sexo % Columnas
sjt.xtab(base_85$percepcion_1_a, #filas
         base_85$sexo, #columnas
         show.col.prc = T, file = "2.doc")

sjt.xtab(base_85$percepcion_1_a, #filas
         base_85$religion_1, #columnas
         show.col.prc = T, file = "3.doc")

sjt.xtab(base_85$edad, #filas
         base_85$percepcion_1_a, #columnas
         show.col.prc = T, file = "5.doc")

### Ejemplo tablas de contingencia: percepcion_1_a y sexo % Filas y Columnas
sjt.xtab(base_85$percepcion_1_a, #filas
         base_85$sexo, #columnas
         show.col.prc = T, show.row.prc = T, file = "6.doc")

sjt.xtab(base_85$percepcion_1_a, #filas
         base_85$religion_1, #columnas
         show.col.prc = T, show.row.prc = T, file = "7.doc")

sjt.xtab(base_85$edad, #filas
         base_85$percepcion_1_a, #columnas
         show.col.prc = T, show.row.prc = T, file = "9.doc")

#### 3.3 Graficos 

mi_factor <- factor(base_85$percepcion_1_a) 


#### 3.3.1 Gráfico de barras
plot(mi_factor, main = "Diagrama de barras")

plot(mi_factor,base_85$sexo, main = "Diagrama de barras")
plot(mi_factor,base_85$edad, main = "Diagrama de barras")
plot(mi_factor,base_85$religion_1, main = "Diagrama de barras")

#### 3.3.2 Diagrama de dispersión

library(ggplot2)

ggplot(base_85, aes(x = base_85$edad, y = base_85$percepcion_1_a)) +
  geom_point(colour = 4)

ggplot(base_85, aes(x = base_85$percepcion_1_a, y = base_85$religion_1)) +
  geom_point(colour = 4)

ggplot(base_85, aes(x = base_85$sexo, y = base_85$percepcion_1_a)) +
  geom_point(colour = 4)

## 5. Recodificar y crear una nueva variable 

## 5.1 Variable dummy 1 candidato -- BINARIA
base_85$percepcion_1_a.Dummy<-ifelse(base_85$percepcion_1_a =="percepcion_1_a",1,0)
table(base_85$percepcion_1_a.Dummy)


base_85$edad.Dummy<-ifelse(base_85$edad  =="edad",1,0)
table(base_85$edad.Dummy)

base_85$religion_1.Dummy<-ifelse(base_85$edad  =="religion_1",1,0)
table(base_85$religion_1.Dummy)

table <- table(base_85$percepcion_1_a.Dummy)
prop.table(table)
  
table <- table(base_85$religion_1.Dummy)
prop.table(table)

table <- table(base_85$sexo.Dummy)
prop.table(table)

## 5.2 Recodificar variables de preferencias -- MULTIPLE

## Pregunta: percepcion_1_a
table(base_85$percepcion_1_a)
base_85$percepcion_1_a.Dummy = revalue(base_85$percepcion_1_a, c("1"="2", "2"="2","3"="1", "4"="1", "5"="1", "8"="0","9"="0"))
table(base_85$percepcion_1_a.Dummy)

## Recodificar Edad

base_85["eda"] = cut(as.numeric(base_85$edad), c(0, 18, 30, 45, 55, Inf), c("0-17", "18-29", "30-44", "45-54", "55<"), include.lowest=TRUE)

table_8 <- table(base_85$Edad_Re2)

table(base_85$Edad_Re2)

## Recodificar Edad

base_85["age_N-2"] <- as.numeric(as.character(base_85$edad))

table(base_85$`age_N-2`)

base_85["Edad_Re6"] = cut(base_85$`age_N-2`, 
                          c(18, 30, 45, 55, Inf), c("18-29", "30-44", "45-54", "55<"), 
                          include.lowest=TRUE)

table(base_85$edad)


                         
