getwd()
setwd("/Users/andresammx/OneDrive/Itzel")

#Required libraries
library(reshape)
library(dplyr)
library(ggplot2)

#Getting the dataset
l1<-read.csv("lote1.csv", header=TRUE, sep=",", stringsAsFactors=TRUE)
names(l1)

#Changing from wide to long format
fus<-melt(l1, id.vars=c("tx", "id", "sennosen", "sennosen2"))
levels(fus$variable)<-c("0", "1", "2", "3", "4")

#Filtering the dataset to get the toluene data
fus_tol<-subset(fus, tx == "Tol", select=c(tx, id, sennosen, variable, value))
fus_tol <- droplevels(fus_tol)

#Filtering the dataset to get the air data
fus_air<-subset(fus, tx == "Air", select=c(tx, id, sennosen, variable,value))
fus_air<- droplevels(fus_air)

#Create an object with the names of the categories in the toluene data
Groups<-c("Toluene non-sensitized", "Toluene sensitized")

#Create the plot with toluene data
p<-ggplot(data = fus_tol, aes(x = variable, y = value)) +
geom_boxplot(data = fus_tol, aes(x = variable, y = value)) +
geom_point(aes(color = sennosen, alpha=0.5, group = id)) +
geom_line(aes(color = sennosen, alpha=0.5, group = id)) +
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
	#legend.title = element_blank())
print(p)

#Adding the sheat with air data to the first plot
combined_plot <- p +
  geom_point(data = fus_air, aes(x = variable, y = value), color = "#76BD6A", alpha=0.6) +
  geom_line(data = fus_air, aes(x = variable, y = value, group = id), color = "#76BD6A", alpha=0.5) +
  #coord_cartesian(ylim = c(0, 500))  # Ajustar límites del eje y
  theme(legend.title = element_blank())

#Printing the combined plot
print(combined_plot)

#Saving the combined plot
ggsave("grafico.tiff", plot = combined_plot, device = "tiff", compression = "lzw")

# Posterior modifications, such as addition of asterisks, hashtags and text modifica0tion was made by using the Inkscape software (https://inkscape.org)







