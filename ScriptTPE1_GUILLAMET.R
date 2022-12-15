library(readxl) #carga de libraria para importar datos desde un excel
library(tidyverse) #carga de libraria para dar formato wide a la tabla de datoss
library(ggplot2) #carga de libraria para realizar gráficos
library(reshape2) #carga de libraria para reorganizar la matriz de correlaciones y crear un mapa de calor


#Carga de los datos
df = read_excel('/Users/ceciliaguillametchargue/Library/Mobile Documents/com~apple~CloudDocs/Documents/Maestría_UBA/Taller de programacion/Entregas finales/Evaluacion 1/DatosTPE1_GUILLAMET.xlsx', skip = 1)

#Selección de las variables a utilizar
df = df[,c(1,3,4,8)]

#Renombro las columnas
colnames(df) = c('Pais', 'Poblacion', 'Grupo_alimento', 'Media')

#Elimino la notación científica y convierto a numérica la variable Media
options(scipen=999)
df['Media'] = as.numeric(unlist(df['Media']))

#Renombro las categorias de las variables
df$Pais[df$Pais == 'Belgium'] = 'Belgica'
df$Pais[df$Pais == 'Czech Republic'] = 'Republica Checa'
df$Pais[df$Pais == 'Denmark'] = 'Dinamarca'
df$Pais[df$Pais == 'Finland'] = 'Finlandia'
df$Pais[df$Pais == 'France'] = 'Francia'
df$Pais[df$Pais == 'Germany'] = 'Alemania'
df$Pais[df$Pais == 'Ireland'] = 'Irlanda'
df$Pais[df$Pais == 'Italy'] = 'Italia'
df$Pais[df$Pais == 'Netherlands'] = 'Holanda'
df$Pais[df$Pais == 'Romania'] = 'Rumania'
df$Pais[df$Pais == 'Spain'] = 'España'
df$Pais[df$Pais == 'Sweden'] = 'Suecia'
df$Pais[df$Pais == 'United Kingdom'] = 'Reino Unido'
df$Pais[df$Pais == 'Hungary'] = 'Hungria'
df$Pais[df$Pais == 'Latvia'] = 'Letonia'

df$Poblacion[df$Poblacion == 'Adolescents'] = 'Adolescentes'
df$Poblacion[df$Poblacion == 'Adults'] = 'Adultos'
df$Poblacion[df$Poblacion == 'Elderly'] = 'Ancianos'
df$Poblacion[df$Poblacion == 'Infants'] = 'Infantes'
df$Poblacion[df$Poblacion == 'Other children'] = 'Otros_niños'
df$Poblacion[df$Poblacion == 'Pregnant women'] = 'Mujeres_embarazadas'
df$Poblacion[df$Poblacion == 'Toddlers'] = 'Niños'
df$Poblacion[df$Poblacion == 'Very elderly'] = 'Muy_ancianos'

df$Grupo_alimento[df$Grupo_alimento == "Grains and grain-based products"] = 'granos_prod_granos'
df$Grupo_alimento[df$Grupo_alimento == "Starchy roots and tubers"] = 'almidon_tuberc_raices'
df$Grupo_alimento[df$Grupo_alimento == "Fruit and fruit products"] = 'frutas_prod_frutas'
df$Grupo_alimento[df$Grupo_alimento == "Fish and other seafood (including amphibians, rept"] = 'pescados_mariscos'
df$Grupo_alimento[df$Grupo_alimento == "Eggs and egg products"] = 'huevos_prod_huevos'
df$Grupo_alimento[df$Grupo_alimento == "Animal and vegetable fats and oils"] = 'grasa_aceite_ani_veg'
df$Grupo_alimento[df$Grupo_alimento == "Non-alcoholic beverages (excepting milk based beve"] = 'beb_no_alcoholicas'
df$Grupo_alimento[df$Grupo_alimento == "Drinking water (water without any additives except"] = 'agua'
df$Grupo_alimento[df$Grupo_alimento == "Food for infants and small children"] = 'alim_infantiles'
df$Grupo_alimento[df$Grupo_alimento == "Composite food (including frozen products)"] = 'alim_compuestos'
df$Grupo_alimento[df$Grupo_alimento == "Vegetables and vegetable products (including fungi"] = 'veg_prod_veg'
df$Grupo_alimento[df$Grupo_alimento == "Legumes, nuts and oilseeds"] = 'legum_frutos_secos'
df$Grupo_alimento[df$Grupo_alimento == "Meat and meat products (including edible offal)"] = 'carnes_prod_carnes'
df$Grupo_alimento[df$Grupo_alimento == "Milk and dairy products"] = 'prod_lacteos'
df$Grupo_alimento[df$Grupo_alimento == "Sugar and confectionary"] = 'azucar_conf'
df$Grupo_alimento[df$Grupo_alimento == "Fruit and vegetable juices"] = 'frutas_veg_jugos'
df$Grupo_alimento[df$Grupo_alimento == "Alcoholic beverages"] = 'beb_alcoholicas'
df$Grupo_alimento[df$Grupo_alimento == "Herbs, spices and condiments"] = 'hierbas_especias'
df$Grupo_alimento[df$Grupo_alimento == "Products for special nutritional use"] = 'prod_nutricionales'
df$Grupo_alimento[df$Grupo_alimento == "Snacks, desserts, and other foods"] = 'snacks_postres_otros'

#Pasamos al formato wide la base de datos
df_wide = pivot_wider(df, names_from = 'Grupo_alimento', values_from = 'Media')

#Verifico la existencia de valores na
colSums(is.na(df_wide))

#Tabla de doble entrada entre pais y grupo poblacional
table(df_wide$Pais, df_wide$Poblacion)
table(df$Pais, df$Poblacion)

#Elimino "Pregnant women" dado que solo está para Letonia
df = df[df$Poblacion != 'Mujeres_embarazadas',]
df_wide = df_wide[df_wide$Poblacion != 'Mujeres_embarazadas',]

#Obtención de medidas de dispersión y tendencia central de las variables
summary = summary(df_wide)
summary

#Exportación de la tabla de summary a formato csv
write.csv2(x = summary, file = "tabla descriptiva.csv")

#Boxplot de las cantidades medias consumidas según cada uno de los alimentos
ggplot(df, aes(x = Grupo_alimento, y = Media)) + #gráfico de las medias para cada grupo de alimento 
  geom_boxplot(fill = 'lightblue', #especificamos un boxplot
               outlier.colour = 'lightblue') + #el color de los outliers
  coord_flip() + #inversión de los ejes para obtener boxplot horizontales y no verticales
  labs(y = 'Cantidad', #nombre del eje
       x = 'Alimento') + #nombre del eje
  theme(axis.text = element_text(size = 17), #tamaño de letra de las categorías de la variable
        axis.title.x = element_text(size = 20), #tamaño de letra del eje
        axis.title.y = element_text(size = 20)) #tamaño de letra del eje

#Estandarización de las variables cuantitativass
df.scaled = as.data.frame(scale(df_wide[-c(1,2)], center = TRUE, scale = TRUE))
df.scaled['Pais'] = df_wide[,1] #agrego variable pais al dataset 
df.scaled['Poblacion'] = df_wide[,2] #agrego variable población al dataset 
df.scaled = df.scaled[, c(21, 22, 1:20)] #reorganizo el orden de las variables

#Convierto el dataset a un formato largo
df.scaled_long = df.scaled %>% pivot_longer(c(3:22), names_to = "Grupo_alimento", values_to = "Media")

#Boxplot de las cantidades medias consumidas según cada uno de los alimentos
ggplot(df.scaled_long, aes(x = Grupo_alimento, y = Media)) + #gráfico de las medias para cada grupo de alimento 
  geom_boxplot(fill = 'lightblue', #especificamos un boxplot
               outlier.colour = 'lightblue') + #el color de los outliers
  coord_flip() + #inversión de los ejes para obtener boxplot horizontales y no verticales
  labs(y = 'Cantidad', #nombre del eje
       x = 'Alimento') + #nombre del eje
  theme(axis.text = element_text(size = 17), #tamaño de letra de las categorías de la variable
        axis.title.x = element_text(size = 20), #tamaño de letra del eje
        axis.title.y = element_text(size = 20)) #tamaño de letra del eje

#Función para la imputación valores atípicos
replace_outliers = function(x, removeNA = TRUE){
  qrts = quantile(x, probs = c(0.25, 0.75), na.rm = removeNA)
  caps = quantile(x, probs = c(0.05, 0.95), na.rm = removeNA)
  iqr = qrts[2]-qrts[1]
  h = 1.5 * iqr
  x[x<qrts[1]-h] = caps[1]
  x[x>qrts[2]+h] = caps[2]
  x
}

#Implementación de la función en el conjunto de variables cuantitativas del dataset
columns = colnames(df.scaled)[-c(1,2)]
df.scaled.cap = NULL
for (col in columns){ #iteración sobre las columnas del dataset
  y = replace_outliers(df.scaled[, col]) #aplicación de la función
  df.scaled.cap = as.data.frame(cbind(df.scaled.cap, y)) #guardamos las columnas nuevas en un nuevo dataset
}

df.scaled.cap['Pais'] = df.scaled[,1] #agrego variable pais al dataset 
df.scaled.cap['Poblacion'] = df.scaled[,2] #agrego variable población al dataset 
df.scaled.cap = df.scaled.cap[, c(21, 22, 1:20)] #reorganizo el orden de las variables
colnames(df.scaled.cap) = colnames(df_wide) #asigno los nombres del dataset anterior

#Convierto el dataset a un formato largo
df.scaled.cap.long = df.scaled.cap %>% pivot_longer(c(3:22), names_to = "Grupo_alimento", values_to = "Media")

#Boxplot de las cantidades medias consumidas según cada uno de los alimentos para el dataset con los valores imputados
ggplot(df.scaled.cap.long, aes(x = Grupo_alimento, y = Media)) + #gra´fico de las medias para cada grupo de alimento
  geom_boxplot(fill = 'lightblue', #especificamos un boxplot
               outlier.colour = 'lightblue') + #color de los valores outliers
  coord_flip() + #inversión de los ejes para obtener boxplot horizontales y no verticales
  labs(y = 'Cantidad', #nombre del eje
       x = 'Alimento') + #nombre del eje
  theme(axis.text = element_text(size = 17), #tamaño de letra de las categorías de la variable
        axis.title.x = element_text(size = 20), #tamaño de letra del eje
        axis.title.y = element_text(size = 20)) #tamaño de letra del eje

#Detección de las observaciones outliers
var_outliers = c('prod_nutricionales', 'alim_infantiles', 'granos_prod_granos', 'frutas_veg_jugos') #selección de las variables con outliers
outliers = as.data.frame(sapply(df_wide[, var_outliers], function(x) quantile(x, probs = seq(0, 1, 1/4)))) #calculo los cuantiles para esas variables
columns = colnames(outliers)
#obtengo para cada variable la posición de las observación que es outlier
for (col in columns){
  iqr = outliers[4, col] - outliers[2, col]
  at = df_wide[, col] < (outliers[2, col] - 1.5*iqr) | df_wide[, col] > (outliers[4, col] + 1.5*iqr)
  print(paste('Las observaciones atípicas en', col, 'son:'))
  print(which(at))
}

#Gráfico de consumos medios según país, grupo alimenticio y grupo poblacional
jpeg('rplot.jpg', width = 1000, height = 1000, quality = 100) #Guardo el gráfico con formato jpg
ggplot(df.scaled.cap.long, aes(x = factor(Poblacion), y = Media, color = Grupo_alimento)) + #gráfico del consumo medio, el pais, el grupo de alimento
  geom_point(aes(group = Pais), size = 3, alpha = 0.6) + #agrupado por pais
  facet_wrap(~Pais, ncol = 4) + #creación de un gráfico para cada pais y que lo agrupe en 4 columnas
  scale_x_discrete(labels = abbreviate, #abrevio las categorías de la población
                   limits = c("Infantes", "Niños", "Otros_niños", "Adolescentes", "Adultos", "Ancianos", "Muy_ancianos")) +
  scale_color_manual(values=c("#D32601", "#C8D202", "#28D202", '#0244D2', '#D202CF', '#891E21', '#2B1E89', '#339DC8', '#76D7C4',
                             '#A260BE', '#FFA200', '#27FF00', '#ACA6F9', '#F5B6F8', '#72305E', '#2C3E50', '#138D75', '#5499C7', 
                             '#F7DC6F', '#909497')) + #seteo los colores
  labs(color = 'Clasificación alimentos', #nombre de la clasificacion por color
       x = 'Población', #nombre del eje
       y = 'Consumo medio') + #nombre del eje
  theme(axis.title.x = element_text(size = 15), #tamaño de letra eje
        axis.title.y = element_text(size = 15), #tamaño de letra eje
        legend.title = element_text(size = 15), #tamaño de letra titulo de clasificacion por color
        legend.text = element_text(size = 13), #tamaño de letra categorias de clasificacion por color
        strip.text = element_text(size = 15)) #tamaño de letra paises

dev.off()

#Matriz de correlaciones
cormat = round(cor(df.scaled.cap[, c(3:22)]),2) #creación matriz de correlaciones
melted_cormat = melt(cormat) #reorganización de la matriz de correlaciones para crear un mapa de calor

#Gráfico de correlaciones entre las variables
ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) + #gráfico de cada variable con el resto de ellas
  geom_tile() + #especificamos un mapa de calor
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + #agrega el valor de la correlación
  scale_fill_gradient(low = 'white', high = 'steelblue') + #seteo del color
  labs(x = 'Clasificación alimentos', #nombre del eje
       y = 'Clasificación alimentos') + #nombre del eje
  guides(x = guide_axis(angle = 90)) + #angulo de los nombres de las variables
  theme(axis.text.x = element_text(size = 15), #tamaño de letra del eje
        axis.text.y = element_text(size = 15)) #tamaño de letra del eje
  



