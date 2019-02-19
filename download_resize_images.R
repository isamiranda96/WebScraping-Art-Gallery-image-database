library(readr)
install.packages("imager")
library(imager)

a <- read.csv("drawings _arte_contemporaneo.csv")
b <- read.csv("paintings _arte_contemporaneo.csv")
c <- read.csv("mixed+media _arte_contemporaneo.csv")

completa <- rbind(a,b,c)

title <- str_replace(as.character(completa$title),"[?]", "")%>%
  as.vector()

completa <- select(completa, c("artist", "price", "classification","url"))

nueva_bd_completa <- cbind(title,completa)%>%
  unique()
 
write.csv(nueva_bd_completa, "arte_contemporaneo_juntas.csv")


descargar_img <- function(num_img,empezar_n){
  
  imagenes_jpgs <- read.csv("arte_contemporaneo_juntas.csv")
  imagenes_jpgs$url<- as.character(imagenes_jpgs$url)
  imagenes_jpgs$title <- as.character(imagenes_jpgs$title)
  
  n <- empezar_n
  
  while (n <= num_img){
    
    id_title <- as.character(imagenes_jpgs[n,2])
    link <- as.character(imagenes_jpgs[n,6])
    
    download.file(link, destfile = paste(n, id_title,".jpg", sep="" ),
                  mode = 'wb')
    n <- n+1
    
  }
}

descargar_img(5003,4810)

#------------------------------------Resize images----------------------

original_dataset_dir <- "~/original_dataset"
my_file <- file.path(original_dataset_dir)

im <- load.image(my_file)

thmb <- resize(im,round(width(im)/10),round(height(im)/10))

