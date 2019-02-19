library(rvest)
library(tidyr)
install.packages("tibble")
library(tibble)
install.packages("janitor")
library(janitor)

#Se haran distintas funciones, una para cada medio de clasificación

obras_escultura <- function(num_art){
        
        urls <- c("http://www.artnet.com/galleries/fine-art-artworks-for-sale/?q=medium-sculpture-sort-by-price-high-to-low")
        art_db <- read_html(urls)
        
        n <- 0
        p <- 2
        datos <- matrix(0, nrow = num_art, ncol = 6)
        paginas <- seq(11, num_art, by=12)
      
        while(n < num_art){
        
            for(i in paginas){
            a <- 0
                while(n <= i){
                n_artista <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_lblArtistName", sep="")) %>%
                  html_text()
                n_obra <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_spnArtworkName", sep="")) %>%
                  html_text()
                n_obra <- gsub("\r\n                                            ","",n_obra)
                precio_obra <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_spnPrice", sep="")) %>%
                  html_text()
                precio_obra <- gsub("\r\n                                        ","",precio_obra)
                año_trabajo <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_spnWorkYear", sep="")) %>%
                  html_text()
                año_trabajo <- gsub("\r\n                                        ","",año_trabajo)
                galeria <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_lblGalleryName", sep="")) %>%
                  html_text()
                galeria <- gsub("\r\n                                        ","",galeria)
                imagen <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_imgArtImage", sep="")) %>%
                  html_attr('src')
          
                
                datos[n+1,1] <- n_artista
                datos[n+1,2] <- n_obra
                datos[n+1,3] <- precio_obra
                datos[n+1,4] <- año_trabajo
                datos[n+1,5] <- galeria
                datos[n+1,6] <- imagen
                
                n <- n+1
                a <- a+1
                }
              
            art_db <- read_html(paste(urls,"/", p, sep = ""))
            p <- p + 1
            
            }
        }
        
        colnames(datos) <- c("Artista","Nombre_Obra", "Precio", "Año","Galeria","Imagen" )
        datos <- data.frame(datos)
        print(datos)
      }
esculturas1 <- obras_escultura(300)%>%
                      add_column(Medio = "escultura")
esculturas1[esculturas1== ""]<- NA
esculturas1<- esculturas1[complete.cases(esculturas1),]
  
obras_escultura2 <- function(num_art, num_pag_inicio){
          
          urls <- c("http://www.artnet.com/galleries/fine-art-artworks-for-sale/?q=medium-sculpture-sort-by-price-high-to-low")
          art_db <- read_html(paste(urls,"/", num_pag_inicio, sep=""))
          
          n <- 0
          datos <- matrix(0, nrow = num_art, ncol = 6)
          paginas <- seq(11, num_art, by=12)
          
          while(n < num_art){
            
            for(i in paginas){
              a <- 0
              while(n <= i){
                n_artista <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_lblArtistName", sep="")) %>%
                  html_text()
                n_obra <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_spnArtworkName", sep="")) %>%
                  html_text()
                n_obra <- gsub("\r\n                                            ","",n_obra)
                precio_obra <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_spnPrice", sep="")) %>%
                  html_text()
                precio_obra <- gsub("\r\n                                        ","",precio_obra)
                año_trabajo <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_spnWorkYear", sep="")) %>%
                  html_text()
                año_trabajo <- gsub("\r\n                                        ","",año_trabajo)
                galeria <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_lblGalleryName", sep="")) %>%
                  html_text()
                galeria <- gsub("\r\n                                        ","",galeria)
                imagen <- art_db %>%
                  html_node(paste("#rptArtworkDetail__ctl",a,"_imgArtImage", sep="")) %>%
                  html_attr('src')
                
                
                datos[n+1,1] <- n_artista
                datos[n+1,2] <- n_obra
                datos[n+1,3] <- precio_obra
                datos[n+1,4] <- año_trabajo
                datos[n+1,5] <- galeria
                datos[n+1,6] <- imagen
                
                n <- n+1
                a <- a+1
              }
              
              art_db <- read_html(paste(urls,"/", num_pag_inicio + 1, sep = ""))
              
            }
          }
          
          colnames(datos) <- c("Artista","Nombre_Obra", "Precio", "Año","Galeria","Imagen" )
          datos <- data.frame(datos)
          print(datos)
        }
esculturas2 <- obras_escultura2(300,24)%>%
                        add_column(Medio = "escultura")
esculturas2[esculturas2== ""]<- NA
esculturas2 <- esculturas2[complete.cases(esculturas2),]
        
esculturas3 <- obras_escultura2(300,49)%>%
                add_column(Medio = "escultura")
esculturas3[esculturas3== ""]<- NA
esculturas3 <- esculturas3[complete.cases(esculturas3),]

esculturas4 <- obras_escultura2(300,74)%>%
  add_column(Medio = "escultura")
esculturas4[esculturas4== ""]<- NA
esculturas4 <- esculturas4[complete.cases(esculturas4),]

esculturas5 <- obras_escultura2(300,124)%>%
  add_column(Medio = "escultura")
esculturas5[esculturas5== ""]<- NA
esculturas5 <- esculturas5[complete.cases(esculturas5),]

esculturas <- rbind(esculturas1, esculturas2,esculturas3,esculturas4,esculturas5)


#OBRAS DE PINTURA

obras_pintura <- function(num_art){
  
  urls <- c("http://www.artnet.com/galleries/fine-art-artworks-for-sale/?q=medium-paintings-sort-by-price-high-to-low")
  art_db <- read_html(urls)
  
  n <- 0
  p <- 2
  datos <- matrix(0, nrow = num_art, ncol = 6)
  paginas <- seq(11, num_art, by=12)
  
  while(n < num_art){
    
    for(i in paginas){
      a <- 0
      while(n <= i){
        n_artista <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblArtistName", sep="")) %>%
          html_text()
        n_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnArtworkName", sep="")) %>%
          html_text()
        n_obra <- gsub("\r\n                                            ","",n_obra)
        precio_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnPrice", sep="")) %>%
          html_text()
        precio_obra <- gsub("\r\n                                        ","",precio_obra)
        año_trabajo <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnWorkYear", sep="")) %>%
          html_text()
        año_trabajo <- gsub("\r\n                                        ","",año_trabajo)
        galeria <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblGalleryName", sep="")) %>%
          html_text()
        galeria <- gsub("\r\n                                        ","",galeria)
        imagen <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_imgArtImage", sep="")) %>%
          html_attr('src')
        
        datos[n+1,1] <- n_artista
        datos[n+1,2] <- n_obra
        datos[n+1,3] <- precio_obra
        datos[n+1,4] <- año_trabajo
        datos[n+1,5] <- galeria
        datos[n+1,6] <- imagen
        
        n <- n+1
        a <- a+1
      }
      
      art_db <- read_html(paste(urls,"/", p, sep = ""))
      p <- p + 1
      
    }
  }
  
  colnames(datos) <- c("Artista","Nombre_Obra", "Precio", "Año","Galeria","Imagen" )
  datos <- data.frame(datos)
  print(datos)
}
pinturas1 <- obras_pintura(300)%>%
  add_column(Medio = "pintura")
pinturas1[pinturas1== ""]<- NA
pinturas1 <- pinturas1[complete.cases(pinturas1),]


obras_pintura2 <- function(num_art, num_pag_inicio){
  
  urls <- c("http://www.artnet.com/galleries/fine-art-artworks-for-sale/?q=medium-paintings-sort-by-price-high-to-low")
  art_db <- read_html(paste(urls,"/", num_pag_inicio, sep=""))
  
  n <- 0
  datos <- matrix(0, nrow = num_art, ncol = 6)
  paginas <- seq(11, num_art, by=12)
  
  while(n < num_art){
    
    for(i in paginas){
      a <- 0
      while(n <= i){
        n_artista <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblArtistName", sep="")) %>%
          html_text()
        n_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnArtworkName", sep="")) %>%
          html_text()
        n_obra <- gsub("\r\n                                            ","",n_obra)
        precio_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnPrice", sep="")) %>%
          html_text()
        precio_obra <- gsub("\r\n                                        ","",precio_obra)
        año_trabajo <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnWorkYear", sep="")) %>%
          html_text()
        año_trabajo <- gsub("\r\n                                        ","",año_trabajo)
        galeria <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblGalleryName", sep="")) %>%
          html_text()
        galeria <- gsub("\r\n                                        ","",galeria)
        imagen <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_imgArtImage", sep="")) %>%
          html_attr('src')
        
        
        datos[n+1,1] <- n_artista
        datos[n+1,2] <- n_obra
        datos[n+1,3] <- precio_obra
        datos[n+1,4] <- año_trabajo
        datos[n+1,5] <- galeria
        datos[n+1,6] <- imagen
        
        n <- n+1
        a <- a+1
      }
      
      art_db <- read_html(paste(urls,"/", num_pag_inicio + 1, sep = ""))
      
    }
  }
  
  colnames(datos) <- c("Artista","Nombre_Obra", "Precio", "Año","Galeria","Imagen" )
  datos <- data.frame(datos)
  print(datos)
}
pinturas2 <- obras_pintura2(300,24)%>%
  add_column(Medio = "pintura")
pinturas2[pinturas2== ""]<- NA
pinturas2 <- pinturas2[complete.cases(pinturas2),]

pinturas3 <- obras_pintura2(300,74)%>%
  add_column(Medio = "pintura")
pinturas3[pinturas3== ""]<- NA
pinturas3 <- pinturas3[complete.cases(pinturas3),]

pinturas4 <- obras_pintura2(300,99)%>%
  add_column(Medio = "pintura")
pinturas4[pinturas4== ""]<- NA
pinturas4 <- pinturas4[complete.cases(pinturas4),]

pinturas5 <- obras_pintura2(300,124)%>%
  add_column(Medio = "pintura")
pinturas5[pinturas5== ""]<- NA
pinturas5 <- pinturas5[complete.cases(pinturas5),]

pinturas6 <- obras_pintura2(300,149)%>%
  add_column(Medio = "pintura")
pinturas6[pinturas6== ""]<- NA
pinturas6 <- pinturas6[complete.cases(pinturas6),]

pinturas7 <- obras_pintura2(300,174)%>%
  add_column(Medio = "pintura")
pinturas7[pinturas7== ""]<- NA
pinturas7 <- pinturas7[complete.cases(pinturas7),]

pinturas8 <- obras_pintura2(300,199)%>%
  add_column(Medio = "pintura")
pinturas8[pinturas8== ""]<- NA
pinturas8 <- pinturas8[complete.cases(pinturas8),]

pinturas9 <- obras_pintura2(300,224)%>%
  add_column(Medio = "pintura")
pinturas9[pinturas9== ""]<- NA
pinturas9 <- pinturas9[complete.cases(pinturas9),]

pinturas10 <- obras_pintura2(300,249)%>%
  add_column(Medio = "pintura")
pinturas10[pinturas10== ""]<- NA
pinturas10 <- pinturas10[complete.cases(pinturas10),]

pinturas11 <- obras_pintura2(300,274)%>%
  add_column(Medio = "pintura")
pinturas11[pinturas11== ""]<- NA
pinturas11 <- pinturas11[complete.cases(pinturas11),]

pinturas <- rbind(pinturas1, pinturas10,pinturas11,pinturas2,pinturas3,
                  pinturas4,pinturas5,pinturas6,pinturas7,pinturas8,pinturas9)


#OBRAS EN PAPEL


obras_papel <- function(num_art){
  
  urls <- c("http://www.artnet.com/galleries/fine-art-artworks-for-sale/?q=medium-works-on-paper-sort-by-price-high-to-low")
  art_db <- read_html(urls)
  
  n <- 0
  p <- 2
  datos <- matrix(0, nrow = num_art, ncol = 6)
  paginas <- seq(11, num_art, by=12)
  
  while(n < num_art){
    
    for(i in paginas){
      a <- 0
      while(n <= i){
        n_artista <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblArtistName", sep="")) %>%
          html_text()
        n_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnArtworkName", sep="")) %>%
          html_text()
        n_obra <- gsub("\r\n                                            ","",n_obra)
        precio_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnPrice", sep="")) %>%
          html_text()
        precio_obra <- gsub("\r\n                                        ","",precio_obra)
        año_trabajo <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnWorkYear", sep="")) %>%
          html_text()
        año_trabajo <- gsub("\r\n                                        ","",año_trabajo)
        galeria <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblGalleryName", sep="")) %>%
          html_text()
        galeria <- gsub("\r\n                                        ","",galeria)
        imagen <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_imgArtImage", sep="")) %>%
          html_attr('src')
        
        datos[n+1,1] <- n_artista
        datos[n+1,2] <- n_obra
        datos[n+1,3] <- precio_obra
        datos[n+1,4] <- año_trabajo
        datos[n+1,5] <- galeria
        datos[n+1,6] <- imagen
        
        n <- n+1
        a <- a+1
      }
      
      art_db <- read_html(paste(urls,"/", p, sep = ""))
      p <- p + 1
      
    }
  }
  
  colnames(datos) <- c("Artista","Nombre_Obra", "Precio", "Año","Galeria","Imagen" )
  datos <- data.frame(datos)
  print(datos)
}
papel1 <- obras_papel(300)%>%
  add_column(Medio = "Trabajo en papel")
papel1[papel1== ""]<- NA
papel1 <- papel1[complete.cases(papel1),]

obras_papel2 <- function(num_art, num_pag_inicio){
  
  urls <- c("http://www.artnet.com/galleries/fine-art-artworks-for-sale/?q=medium-works-on-paper-sort-by-price-high-to-low")
  art_db <- read_html(paste(urls,"/", num_pag_inicio, sep=""))
  
  n <- 0
  datos <- matrix(0, nrow = num_art, ncol = 6)
  paginas <- seq(11, num_art, by=12)
  
  while(n < num_art){
    
    for(i in paginas){
      a <- 0
      while(n <= i){
        n_artista <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblArtistName", sep="")) %>%
          html_text()
        n_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnArtworkName", sep="")) %>%
          html_text()
        n_obra <- gsub("\r\n                                            ","",n_obra)
        precio_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnPrice", sep="")) %>%
          html_text()
        precio_obra <- gsub("\r\n                                        ","",precio_obra)
        año_trabajo <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnWorkYear", sep="")) %>%
          html_text()
        año_trabajo <- gsub("\r\n                                        ","",año_trabajo)
        galeria <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblGalleryName", sep="")) %>%
          html_text()
        galeria <- gsub("\r\n                                        ","",galeria)
        imagen <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_imgArtImage", sep="")) %>%
          html_attr('src')
        
        
        datos[n+1,1] <- n_artista
        datos[n+1,2] <- n_obra
        datos[n+1,3] <- precio_obra
        datos[n+1,4] <- año_trabajo
        datos[n+1,5] <- galeria
        datos[n+1,6] <- imagen
        
        n <- n+1
        a <- a+1
      }
      
      art_db <- read_html(paste(urls,"/", num_pag_inicio + 1, sep = ""))
      
    }
  }
  
  colnames(datos) <- c("Artista","Nombre_Obra", "Precio", "Año","Galeria","Imagen" )
  datos <- data.frame(datos)
  print(datos)
}
papel2 <- obras_papel2(300,24)%>%
  add_column(Medio = "Trabajo en papel")
papel2[papel2== ""]<- NA
papel2 <- papel2[complete.cases(papel2),]

papel3 <- obras_papel2(300,49)%>%
  add_column(Medio = "Trabajo en papel")
papel3[papel3== ""]<- NA
papel3 <- papel3[complete.cases(papel3),]

papel4 <- obras_papel2(300,74)%>%
  add_column(Medio = "Trabajo en papel")
papel4[papel4== ""]<- NA
papel4 <- papel4[complete.cases(papel4),]

papel5 <- obras_papel2(300,99)%>%
  add_column(Medio = "Trabajo en papel")
papel5[papel5== ""]<- NA
papel5 <- papel5[complete.cases(papel5),]

papel <- rbind(papel1,papel2,papel3,papel4,papel5)

#ESCULTURAS FIGURINES


obras_figurines <- function(num_art){
  
  urls <- c("http://www.artnet.com/galleries/fine-art-artworks-for-sale/?q=medium-figures-figurines-sculpture-carvings-sort-by-price-high-to-low")
  art_db <- read_html(urls)
  
  n <- 0
  p <- 2
  datos <- matrix(0, nrow = num_art, ncol = 6)
  paginas <- seq(11, num_art, by=12)
  
  while(n < num_art){
    
    for(i in paginas){
      a <- 0
      while(n <= i){
        n_artista <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblArtistName", sep="")) %>%
          html_text()
        n_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnArtworkName", sep="")) %>%
          html_text()
        n_obra <- gsub("\r\n                                            ","",n_obra)
        precio_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnPrice", sep="")) %>%
          html_text()
        precio_obra <- gsub("\r\n                                        ","",precio_obra)
        año_trabajo <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnWorkYear", sep="")) %>%
          html_text()
        año_trabajo <- gsub("\r\n                                        ","",año_trabajo)
        galeria <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblGalleryName", sep="")) %>%
          html_text()
        galeria <- gsub("\r\n                                        ","",galeria)
        imagen <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_imgArtImage", sep="")) %>%
          html_attr('src')
        
        datos[n+1,1] <- n_artista
        datos[n+1,2] <- n_obra
        datos[n+1,3] <- precio_obra
        datos[n+1,4] <- año_trabajo
        datos[n+1,5] <- galeria
        datos[n+1,6] <- imagen
        
        n <- n+1
        a <- a+1
      }
      
      art_db <- read_html(paste(urls,"/", p, sep = ""))
      p <- p + 1
      
    }
  }
  
  colnames(datos) <- c("Artista","Nombre_Obra", "Precio", "Año","Galeria","Imagen" )
  datos <- data.frame(datos)
  print(datos)
}
figurines1 <- obras_figurines(300)%>%
  add_column(Medio = "figurines_escultura")
figurines1[figurines1== ""]<- NA
figurines1 <- figurines1[complete.cases(figurines1),]

obras_figurines2 <- function(num_art, num_pag_inicio){
  
  urls <- c("http://www.artnet.com/galleries/fine-art-artworks-for-sale/?q=medium-figures-figurines-sculpture-carvings-sort-by-price-high-to-low")
  art_db <- read_html(paste(urls,"/", num_pag_inicio, sep=""))
  
  n <- 0
  datos <- matrix(0, nrow = num_art, ncol = 6)
  paginas <- seq(11, num_art, by=12)
  
  while(n < num_art){
    
    for(i in paginas){
      a <- 0
      while(n <= i){
        n_artista <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblArtistName", sep="")) %>%
          html_text()
        n_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnArtworkName", sep="")) %>%
          html_text()
        n_obra <- gsub("\r\n                                            ","",n_obra)
        precio_obra <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnPrice", sep="")) %>%
          html_text()
        precio_obra <- gsub("\r\n                                        ","",precio_obra)
        año_trabajo <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_spnWorkYear", sep="")) %>%
          html_text()
        año_trabajo <- gsub("\r\n                                        ","",año_trabajo)
        galeria <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_lblGalleryName", sep="")) %>%
          html_text()
        galeria <- gsub("\r\n                                        ","",galeria)
        imagen <- art_db %>%
          html_node(paste("#rptArtworkDetail__ctl",a,"_imgArtImage", sep="")) %>%
          html_attr('src')
        
        
        datos[n+1,1] <- n_artista
        datos[n+1,2] <- n_obra
        datos[n+1,3] <- precio_obra
        datos[n+1,4] <- año_trabajo
        datos[n+1,5] <- galeria
        datos[n+1,6] <- imagen
        
        n <- n+1
        a <- a+1
      }
      
      art_db <- read_html(paste(urls,"/", num_pag_inicio + 1, sep = ""))
      
    }
  }
  
  colnames(datos) <- c("Artista","Nombre_Obra", "Precio", "Año","Galeria","Imagen" )
  datos <- data.frame(datos)
  print(datos)
}
figurines2 <- obras_figurines2(300,24)%>%
  add_column(Medio = "figurines_escultura")
figurines2[figurines2== ""]<- NA
figurines2 <- figurines2[complete.cases(figurines2),]


figurines <- figurines1


#BASE DE DATOS COMPLETA 1
  
db_completa <- rbind(esculturas,figurines,papel,pinturas)
write.csv(db_completa, 'db_obras_de_arte.csv')

