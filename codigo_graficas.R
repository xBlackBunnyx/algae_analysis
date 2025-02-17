#La versión de R es la 4.3.1
#Lo primero que hay que hacer es activar las librerías
library(dplyr) #Permite ordenar los datos
library(ggplot2) #Hace las graficas
library(readxl) #Permite abrir archivos excel

#Lo siguiente es abrir el archivo de excel con el que vas a trabajar
#y guardarlo en una variable
Tabla_excel = 
  read_excel("C:\\Users\\lorenamd.proyecto\\Desktop\\graficas_R\\TABLA BASE 0.xlsx")
#Si hacemos un view(Tabla_excel), vemos que el encabezado cuenta como fila
#Ponemos la primera fila como el encabezado
names(Tabla_excel) = Tabla_excel[1,]
#Quitamos la primera fila porque ya no hace falta
Tabla_excel = Tabla_excel[-1,]

#Ahora hacemos una grafica de barras para ver los distintos filos
#Primero contamos las veces en las que aparece un filo
conteo_filos = table(Tabla_excel$Filo)
#Asignamos los colores a la gráfica (se pueden cambiar por los que quieras)
colores_filo = c("#38C824", "#ECB439", "#EE3A1F")
#Ahora hacemos la grafica como tal añadiendo las etiquetas y el limite de la escala
barplot(conteo_filos, main = "Frecuencia del filo", xlab = "Filo", ylab = "Frecuencia",
        col = colores_filo, ylim = c(0, max(conteo_filos) + 100))

#Lo siguiente es hacer una gráfica para ver las familias más abundantes de cada filo

#Para eso, hacemos una nueva tabla filtrando los datos para cada filo
Clorofitas = Tabla_excel[Tabla_excel$Filo == "Chlorophyta",]
Ochrofitas = Tabla_excel[Tabla_excel$Filo == "Ochrophyta",]
Rodofitas = Tabla_excel[Tabla_excel$Filo == "Rhodophyta",]
#Igual que antes, contamos las veces que aparece cada Orden 
conteo_fam_cloro = table(Clorofitas$Orden)
conteo_fam_ochro = table(Ochrofitas$Orden)
conteo_fam_rodo = table(Rodofitas$Orden)
#convertimos estos datos en un dataframe (tabla) para que pueda identificarlo
df_cont_fam_cloro = as.data.frame(conteo_fam_cloro)
df_cont_fam_ochro = as.data.frame(conteo_fam_ochro)
df_cont_fam_rodo = as.data.frame(conteo_fam_rodo)
#añadimos el encabezado a esas nuevas tablas
names(df_cont_fam_cloro) = c("Orden", "Frecuencia")
names(df_cont_fam_ochro) = c("Orden", "Frecuencia")
names(df_cont_fam_rodo) = c("Orden", "Frecuencia")
#Calculamos los porcentajes para cada familia
df_cont_fam_cloro$Porcentaje = df_cont_fam_cloro$Frecuencia / sum(df_cont_fam_cloro$Frecuencia) * 100
df_cont_fam_ochro$Porcentaje = df_cont_fam_ochro$Frecuencia / sum(df_cont_fam_ochro$Frecuencia) * 100
df_cont_fam_rodo$Porcentaje = df_cont_fam_rodo$Frecuencia / sum(df_cont_fam_rodo$Frecuencia) * 100


#Creamos el grafico para las clorofitas
p_cloro = ggplot(df_cont_fam_cloro, aes(x = "", y = Frecuencia, fill = Orden)) +
  geom_bar(stat = "identity") + 
  coord_polar("y", start = 0) + 
  theme_void() +
  labs(title = "Frecuencia de los ordenes en el filo Chlorophyta") +
  theme(legend.position = "bottom")
#Añadimos las etiquetas con los porcentajes a la leyenda
p_cloro = p_cloro +
  guides(fill = guide_legend(override.aes = list(label = NULL)))
p_cloro + scale_fill_manual(values = terrain.colors(length(levels(df_cont_fam_cloro$Orden))),
                            labels = paste0(levels(df_cont_fam_cloro$Orden),
                                            " (", round(df_cont_fam_cloro$Porcentaje, 1), "%)"))

#Creamos el grafico para las ocrofitas
p_ochro = ggplot(df_cont_fam_ochro, aes(x = "", y = Frecuencia, fill = Orden)) +
  geom_bar(stat = "identity") + 
  coord_polar("y", start = 0) + 
  theme_void() +
  labs(title = "Frecuencia de los ordenes en el filo Ochrophyta") +
  theme(legend.position = "bottom")
#Añadimos las etiquetas con los porcentajes a la leyenda
rev_ochra = rev(terrain.colors(length(levels(df_cont_fam_ochro$Orden))))
p_ochro = p_ochro +
  guides(fill = guide_legend(override.aes = list(label = NULL)))
p_ochro + scale_fill_manual(values = rev_ochra,
                            labels = paste0(levels(df_cont_fam_ochro$Orden),
                                            " (", round(df_cont_fam_ochro$Porcentaje, 1), "%)"))

#Creamos el grafico para las rodophytas
p_rodo = ggplot(df_cont_fam_rodo, aes(x = "", y = Frecuencia, fill = Orden)) +
  geom_bar(stat = "identity") + 
  coord_polar("y", start = 0) + 
  theme_void() +
  labs(title = "Frecuencia de los ordenes en el filo Rhodophyta") +
  theme(legend.position = "bottom")
#Añadimos las etiquetas con los porcentajes a la leyenda
p_rodo = p_rodo +
  guides(fill = guide_legend(override.aes = list(label = NULL)))
p_rodo + scale_fill_manual(values = heat.colors(length(levels(df_cont_fam_rodo$Orden))),
                            labels = paste0(levels(df_cont_fam_rodo$Orden),
                                            " (", round(df_cont_fam_rodo$Porcentaje, 1), "%)"))
#Quitamos los valores NA
Tabla_sin_NA = Tabla_excel %>% filter(!is.na(Filo) & !is.na(Especie))
#Ahora creamos una nueva tabla para almacenar los 10 géneros más abundantes de cada filo
top_especies = Tabla_sin_NA %>% 
  group_by(Filo, Género) %>% 
  summarise(total_especies = n()) %>% 
  group_by(Filo) %>% 
  top_n(5, total_especies) %>% 
  arrange(Filo, desc(total_especies))
#Como por algun motivo Ochraphyta tiene más de 5, las eliminamos manualmente
top_5 = top_especies[-c(11,12,13,14),]
#Creamos el grafico
graph = ggplot(top_5, aes(x = Género, y = total_especies, fill = Filo)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  labs(title = "5 géneros más representadas de cada filo",
       x = "Géneros", y = "Géneros totales") + 
  scale_fill_manual(values = c("Chlorophyta" = "#38C824", "Ochrophyta" = "#ECB439", "Rhodophyta" = "#EE3A1F")) +
  theme_minimal() + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_continuous(limits = c(0,20))
#Vemos el grafico
print(graph)


