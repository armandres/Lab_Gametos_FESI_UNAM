getwd()
setwd("/Users/andresammx/OneDrive/Itzel")
library(reshape)
library(dplyr)
library(ggplot2)

#Creación del objeto base
l1<-read.csv("lote1.csv", header=TRUE, sep=",", stringsAsFactors=TRUE)
names(l1)
#Cambiar al formato largo
fus<-melt(l1, id.vars=c("tx", "id", "sennosen", "sennosen2"))
levels(fus$variable)<-c("0", "1", "2", "3", "4")
#Crear un objeto que solamente contenga los datos de tolueno
fus_tol<-subset(fus, tx == "Tol", select=c(tx, id, sennosen, variable, value))
fus_tol <- droplevels(fus_tol)
levels(fus_tol$tx)
#Crear un objeto que solaemnte contenga los datos de aire
fus_air<-subset(fus, tx == "Air", select=c(tx, id, sennosen, variable,value))
fus_air<- droplevels(fus_air)
levels(fus_air$tx)

Groups<-c("Toluene non-sensitized", "Toluene sensitized")

#Crear un grafico que contenga boxplot por semana, puntos por semana y lineas por semana. Data are in long format, to perform the change from  wide to long format was used the function XXX from the library reshape
p<-ggplot(data = fus_tol, aes(x = variable, y = value)) +
geom_boxplot(data = fus_tol, aes(x=variable, y=value)) +
geom_point(aes(color = sennosen, alpha=0.5, group=id)) +
geom_line(aes(color = sennosen, alpha=0.5, group=id)) +
coord_cartesian(ylim = c(0, 500)) + 
scale_colour_manual(values = c("red", "blue"),  labels = Groups) + 
labs(x = "Weeks", y = "Locomotor activity (# of crossings/10 min)") + 
theme(
	axis.text.x = element_text(face = "bold", size=14), 
	axis.text.y = element_text(face = "bold", size=14), 
	axis.title.x = element_text(face = "bold", size = 14), 
	axis.title.y = element_text(face = "bold", size = 14), 
	legend.text = element_text(size = 14), 
	panel.background = element_rect(fill = "white", color = "black"),
	panel.grid.major = element_blank(),	
	#panel.grid.minor = element_blank(),
	legend.title = element_blank())
p


_______________________________________
#Estrategia para superponer dos graficos
#Primero debo hacer el subset de los datos
#El objeto "fus" en la variable tx tiene el nivel Air
_______________________________________
#unicamente lineas azules
q<-ggplot(data = fus_air, aes(x = variable, y = value)) + 
geom_point(color = "blue") + 
geom_line(aes(group = id), color = "blue") +
coord_cartesian(ylim = c(0,500)) + 
labs(x = "Week", y = "Locomotor activity (# crossing / 10 min)") + 
theme_bw() + 
theme(legend.title = element_blank())
q

combined_plot <- facet_grid(p, q, ncol = 1)
combined_plot

library(ggplot2)

# Gráfico p (Boxplot con puntos y líneas)
p <- ggplot(data = fus_tol, aes(x = variable, y = value)) +
  geom_boxplot(aes(color = sennosen, group = id)) +
  geom_point(data = fus_tol, aes(x = variable, y = value, color = sennosen)) +
  geom_line(data = fus_tol, aes(x = variable, y = value, color = sennosen)) +
  labs(x = "Variable", y = "Value") +  # Etiquetas de ejes
  theme_bw()  # Tema de fondo blanco

# Gráfico q (Puntos y líneas en azul)
s <- ggplot(data = fus_air, aes(x = variable, y = value)) +
  geom_point(color = "blue") +
  #geom_point(aes(alpha = 0.7), color = "blue") +
  geom_line(aes(group = id), color = "blue") +
  coord_cartesian(ylim = c(0, 500)) +
  labs(x = "Week", y = "Locomotor activity (# crossing / 10 min)") +
  theme_bw()  # Tema de fondo blanco
s

_____________________________________________________________
# Agregar capas del gráfico q a p
combined_plot <- p +
  geom_point(data = fus_air, aes(x = variable, y = value), color = "blue") +
  geom_line(data = fus_air, aes(x = variable, y = value, group = id), color = "blue") +
  #coord_cartesian(ylim = c(0, 500))  # Ajustar límites del eje y
  theme(legend.title = element_text())

# Mostrar el gráfico combinado
print(combined_plot)



combined_plot <- p +
  geom_point(data = fus_air, aes(x = variable, y = value), color = "#76BD6A", alpha=0.6) +
  geom_line(data = fus_air, aes(x = variable, y = value, group = id), color = "#76BD6A", alpha=0.5) +
  #coord_cartesian(ylim = c(0, 500))  # Ajustar límites del eje y
  theme(legend.title = element_text())

# Mostrar el gráfico combinado
print(combined_plot)

ggsave("grafico.tiff", plot = combined_plot, device = "tiff", compression = "lzw")

# Posterior modifications, such as addition of asterisks, hashtags and text modifica0tion was made by using the Inkscape software (https://inkscape.org)







