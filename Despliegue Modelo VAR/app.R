#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library (RJDemetra)
library (tseries)
library (vars)
library(readxl)
library(ggplot2)
library(dplyr)
library(data.table)



#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
BaseSITP <- read.table("BaseSITP.txt",header = T,dec = ",")


Funcion_VAR= function(Empresa){
  base= BaseSITP[BaseSITP$Empresa==Empresa,]
  #ipcsepc <- tramoseats_spec("RSAfull") # Funci?n TRAMO
  ICK= ts(base$ICK,frequency = 12, start = c(2019,7)) # Convertimos en Serie de Tiempo
  #ICK=tramoseats(ICK, ipcsepc ) # Quitamos la estacionalidad
  # g1=plot(ICK$final$series,main="Componentes serie ICK")
  #####################################################################
  #ick <- ICK$final$series # Creamos una nueva variable con los componentes de la serie
  #ick <- data.frame(ick) # La convertimos en una tabla
  #ICK <- ts(ick$sa,frequency = 52, start = c(2019,7)) # Convertir la nueva variable en serie de tiempo, siempre usar $sa
  # g1=plot(ICK, main="Serie ICK desestacionalizada \n Empresa") # Gr?fico de la serie ajustada sin estacionalidad
  g1=ggplot(data= data.frame(Tiempo= time(ICK), ICK=ICK), aes(x=Tiempo, y=ICK)) + geom_line(size=1) +
    theme_bw() + ggtitle("Serie ICK \n Empresa") + theme(plot.title=element_text(hjust=0.5, face="bold",size=15))
  #ipcsepc <- tramoseats_spec("RSAfull") # Funci?n TRAMO
  ICO= ts(base$ICO ,frequency = 12, start = c(2019,7)) # Convertimos en Serie de Tiempo
  #ICO=tramoseats(ICO, ipcsepc ) # Quitamos la estacionalidad
  #g2= plot(ICO$final$series,main="Componentes serie ICO") # Gr?fico de los componentes de la serie
  #####################################################################
  #ICO <- ICO$final$series # Creamos una nueva variable con los componentes de la serie
  #ICO <- data.frame(ICO) # La convertimos en una tabla
  #ICO <- ts(ICO$sa,frequency = 52, start = c(2019,7)) # Convertir la nueva variable en serie de tiempo, siempre usar $sa
  g2= ggplot(data= data.frame(Tiempo= time(ICO), ICO=ICO), aes(x=Tiempo, y=ICO)) + geom_line(size=1) +  
    theme_bw() + ggtitle("Serie ICO desestacionalizada \n Empresa") + theme(plot.title=element_text(hjust=0.5, face="bold",size=15))
  # g3=plot(ICK, main="Serie ICK e ICO \n Empresa", ylim=c(0,100),col="red",lwd=2)
  # lines(ICO, col="blue",lwd=2) 
  # legend("bottomleft",legend = c("ICK","ICO"),col=c("red","blue"),lwd=c(2,2),cex=1.2,box.lty=2, box.lwd=2)
  g3=ggplot(data= data.frame(Tiempo= time(ICK), ICK=ICK, ICO=ICO) %>% reshape2::melt(id.var="Tiempo"), 
            aes(x=Tiempo,y=value,col=variable)) + geom_line(size=1) + theme_bw() + ggtitle("Serie ICK e ICO") +
    theme(plot.title=element_text(hjust=0.5, face="bold",size=15))
  p1=adf.test(ICK) # p valor > 0.05, NO es estacionaria, por lo tanto se debe aplicar una diferencia
  # Serie ICK diferenciado 1 vez:
  dICK = diff(ICK) #Crear una variable con la diferencia
  p2=adf.test(dICK) # p valor = 0.01 < 0.05, Es estacionaria
  p3=adf.test(ICO) # p valor > 0.05, NO es estacionaria, por lo tanto se debe aplicar una diferencia
  # Serie ICO diferenciado 1 vez:
  dICO = diff(ICO) #Crear una variable con la diferencia
  p4=adf.test(dICO) # p valor = 0.01 < 0.05, Es estacionaria
  # par(mfrow=c(1,2)) #1 fila y 2 columnas (2 variables)
  # plot(dICK, main="Serie estacionaria ICK \n Empresa")
  # plot(dICO, main="Serie estacionaria ICO \n Empresa" )
  # par(mfrow=c(1,1)) #1 fila y 2 columnas (2 variables)
  g4=ggplot(data= data.frame(Tiempo= time(dICK), ICK=dICK, ICO=dICO) %>% reshape2::melt(id.var="Tiempo"), 
            aes(x=Tiempo,y=value,col=variable)) + geom_line(size=1) + theme_bw() + ggtitle("Series estacionarias ICK e ICO") +
    theme(plot.title=element_text(hjust=0.5, face="bold",size=15))
  #g4=plot(dICK, main="Series estacionarias ICK e ICO \n Empresa", ylim=c(-40,40),col="red",lwd=1.5)
  #lines(dICO, col="blue",lwd=1.5) 
  #legend("bottomleft",legend = c("ICK","ICO"),col=c("red","blue"),lwd=c(2,2),cex=1.2,box.lty=2, box.lwd=2)
  dseries<-data.table(dICK,dICO)
  dseries<-ts(dseries, frequency = 52, start = c(2019,7))
  p5=grangertest(dICK~dICO,order=26, data=dseries) #cambiar el order hasta encontrar p-valor que sea menor que alpha (0.05)
  #grangertest(dICO~dICK,order=26, data=dseries) #cambiar el order hasta encontrar p-valor que sea menor que alpha (0.05)
  
  
  # Selecci?n del oden del modelo
  # bic=c()
  # aic=c()
  # for (reza in 1:6) {
  #   b=BIC(VAR(dseries, p=reza , type="const"))
  #   c=AIC(VAR(dseries, p=reza , type="const"))
  #   bic=c(bic,b)
  #   aic=c(aic,c)
  # }
  # rezago=max(modeest::mfv(which.min(bic), which.min(aic)))
  # p6= paste0("Para este caso, tanto el criterio de informaci?n de AIC y BIC indican que el mejor modelo es con un rezago de " ,
  #            rezago, " por lo tanto se hablar? de un modelo VAR(",rezago,").")
  
  p6=VARselect(dseries, lag.max = 6, type ="const") #tupe constante quiere decir que no me interesa $beta0$, 6 es el numero de periodos maximo que debo utilizar
  
  modelo<-VAR(dseries, p=3 , type="const")
  p7=summary(modelo)
  
  modelo.fir<-irf(modelo, impulse = "dICK", response=c("dICO"),boot = T)
  
  valores_irf=modelo.fir$irf$dICK
  g5= ggplot(data.frame(Tiempo=1:dim(valores_irf), Valores_irf=valores_irf)) + 
    geom_hline(yintercept = 0, color="red") +
    geom_line(group=1,aes(x=Tiempo,y=valores_irf))+ xlab("Periodo") + 
    theme(plot.title = element_text(hjust=0.5,face="bold"))+
    ylab("Valor IRF") + ggtitle("Grafico. Funcion de respuesta al impulso") + 
    theme_minimal()
  #g5=plot(modelo.fir)
  return(list(g1,g2,g3,p1,p2,p3,p4,g4,p5,p6,p7,g5))
}

#Funcion_VAR("A")


# Define UI
ui <- fluidPage(
  titlePanel("Analisis VAR"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Empresa", "Seleccione una empresa:", choices = unique(BaseSITP$Empresa),selected=NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graficos",
                 plotOutput("g1"),
                 plotOutput("g2"),
                 plotOutput("g3"),
                 plotOutput("g4"),
                 plotOutput("g5")
        ),
        tabPanel("Resultados",
                 verbatimTextOutput("p1"),
                 verbatimTextOutput("p2"),
                 verbatimTextOutput("p3"),
                 verbatimTextOutput("p4"),
                 verbatimTextOutput("p5"),
                 verbatimTextOutput("p6"),
                 verbatimTextOutput("p7")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$g1 <- renderPlot({
    Funcion_VAR(input$Empresa)[[1]]
  })
  output$g2 <- renderPlot({
    Funcion_VAR(input$Empresa)[[2]]
  })
  output$g3 <- renderPlot({
    Funcion_VAR(input$Empresa)[[3]]
  })
  output$g4 <- renderPlot({
    Funcion_VAR(input$Empresa)[[8]]
  })
  output$g5 <- renderPlot({
    Funcion_VAR(input$Empresa)[[12]]
  })
  output$p1 <- renderPrint({
    cat("Prueba ADF ICK \n")
    Funcion_VAR(input$Empresa)[[4]]
  })
  output$p2 <- renderPrint({
    cat("Prueba ADF ICK diferenciado \n")
    Funcion_VAR(input$Empresa)[[5]]
  })
  output$p3 <- renderPrint({
    cat("Prueba ADF ICO \n" )
    Funcion_VAR(input$Empresa)[[6]]
  })
  output$p4 <- renderPrint({
    cat("Prueba ADF ICO diferenciado \n")
    Funcion_VAR(input$Empresa)[[7]]
  })
  output$p5 <- renderPrint({
    cat("Prueba de causalidad de Granger \n")
    Funcion_VAR(input$Empresa)[[9]]
  })
  output$p6 <- renderPrint({
    cat("Seleccion del modelo VAR \n")
    Funcion_VAR(input$Empresa)[[10]]
  })
  output$p7 <- renderPrint({
    cat("Resumen del modelo VAR \n")
    Funcion_VAR(input$Empresa)[[11]]
  })
}

shinyApp(ui = ui, server = server)
