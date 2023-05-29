#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tseries)
library(vars)
library(urca)
library(tsDyn)
library(readxl)
library(dplyr)
library(ggplot2)


#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
BaseSITP <- read.table("BaseSITP.txt",header = T,dec = ",")


#Empresa="A"
Funcion_VEC=function(Empresa){
  base= BaseSITP[BaseSITP$Empresa==Empresa,]
  
  ICK= ts(base$ICK,frequency = 12, start = c(2019,7)) # Convertimos en Serie de Tiempo
  g1=ggplot(data= data.frame(Tiempo= time(ICK), ICK=ICK), aes(x=Tiempo, y=ICK)) + geom_line(size=1) +
    theme_bw() + ggtitle("Serie ICK \n Empresa") + theme(plot.title=element_text(hjust=0.5, face="bold",size=15))
  
  
  ICO= ts(base$ICO ,frequency = 12, start = c(2019,7)) # Convertimos en Serie de Tiempo
  g2= ggplot(data= data.frame(Tiempo= time(ICO), ICO=ICO), aes(x=Tiempo, y=ICO)) + geom_line(size=1) +  
    theme_bw() + ggtitle("Serie ICO desestacionalizada \n Empresa") + theme(plot.title=element_text(hjust=0.5, face="bold",size=15))
  
  g3=ggplot(data= data.frame(Tiempo= time(ICK), ICK=ICK, ICO=ICO) %>% reshape2::melt(id.var="Tiempo"), 
            aes(x=Tiempo,y=value,col=variable)) + geom_line(size=1) + theme_bw() + ggtitle("Serie ICK e ICO") +
    theme(plot.title=element_text(hjust=0.5, face="bold",size=15)) + scale_color_manual(values=c("#E29484","#84E2A6"))
  
  p1=adf.test(ICK) # p valor > 0.05, NO es estacionaria, por lo tanto se debe aplicar una diferencia
  # Serie ICK diferenciado 1 vez:
  dICK= diff(ICK) #Crear una variable con la diferencia
  p2=adf.test(dICK) # p valor = 0.01 < 0.05, Es estacionaria
  
  p3=adf.test(ICO) # p valor > 0.05, NO es estacionaria, por lo tanto se debe aplicar una diferencia
  # Serie ICO diferenciado 1 vez:
  dICO = diff(ICO) #Crear una variable con la diferencia
  p4=adf.test(dICO) # p valor = 0.01 < 0.05, Es estacionaria
  
  g4=ggplot(data= data.frame(Tiempo= time(dICK), ICK=dICK, ICO=dICO) %>% reshape2::melt(id.var="Tiempo"), 
            aes(x=Tiempo,y=value,col=variable)) + geom_line(size=1) + theme_bw() + ggtitle("Series estacionarias ICK e ICO") +
    theme(plot.title=element_text(hjust=0.5, face="bold",size=15))+ scale_color_manual(values=c("#E29484","#84E2A6"))
  
  
  # Prueba de cointegración de Johansen - Juseluis
  
  # Determinar la existencia de una relación de mediano y largo plazo en las series estudiadas.
  dseries<-data.frame(dICK,dICO)
  dseries<-ts(dseries, frequency = 52, start = c(2019,7))
  sjd.vecm<-ca.jo(dseries, ecdet = "const", type = "trace", K= 3, spec = "longrun")
  p5=summary(sjd.vecm)
  
  # Selección del modelo:
  bic=c()
  aic=c()
  ssr=c()
  for (reza in 1:6) {
    a=summary(VECM (dseries, lag = reza, r=1, include = "none", estim = "ML"))$SSR
    b=summary(VECM (dseries, lag = reza, r=1, include = "none", estim = "ML"))$bic
    c=summary(VECM (dseries, lag = reza, r=1, include = "none", estim = "ML"))$aic
    ssr=c(ssr,a)
    bic=c(bic,b)
    aic=c(aic,c)
    
  }
  rezago=max(modeest::mfv(which.min(ssr),which.min(bic), which.min(aic)))
  p6= paste0("Para este caso, tanto el criterio de información de AIC y BIC indican que el mejor modelo es con un rezago de " ,
             rezago, " por lo tanto se hablará de un modelo VEC(",rezago,").")
  
  modelo= VECM (dseries, lag = max(modeest::mfv(which.min(ssr),which.min(bic), which.min(aic))),
                r=1, include = "none", estim = "ML")
  p7=summary(modelo)
  
  var<-vec2var(sjd.vecm, r=1)
  modelo.fir<-irf(var,impulse="dICK", response = "dICO")
  
  valores_irf=modelo.fir$irf$dICK
  g5= ggplot(data.frame(Tiempo=1:dim(valores_irf), Valores_irf=valores_irf)) + geom_hline(yintercept = 0, color="red") +
    geom_line(group=1,aes(x=Tiempo,y=valores_irf))+ xlab("Período") + theme(plot.title = element_text(hjust=0.5,face="bold"))+
    ylab("Valor IRF") + ggtitle("Grafico. Funcion de respuesta al impulso") + theme_minimal() 
  #g5=plot(modelo.fir)
  
  # Validacion, independencia.
  p8=serial.test(var,lags.pt = 12) 
  
  return(list(g1,g2,g3,p1,p2,p3,p4,g4,p5,p6,p7,g5,p8))
}
#Funcion_VEC("B")



# Define UI
ui <- fluidPage(
  titlePanel("Analisis VEC"),
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
                 verbatimTextOutput("p7"),
                 verbatimTextOutput("p8")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$g1 <- renderPlot({
    Funcion_VEC(input$Empresa)[[1]]
  })
  output$g2 <- renderPlot({
    Funcion_VEC(input$Empresa)[[2]]
  })
  output$g3 <- renderPlot({
    Funcion_VEC(input$Empresa)[[3]]
  })
  output$g4 <- renderPlot({
    Funcion_VEC(input$Empresa)[[8]]
  })
  output$g5 <- renderPlot({
    Funcion_VEC(input$Empresa)[[12]]
  })
  output$p1 <- renderPrint({
    cat("Prueba ADF ICK \n")
    Funcion_VEC(input$Empresa)[[4]]
  })
  output$p2 <- renderPrint({
    cat("Prueba ADF ICK diferenciado \n")
    Funcion_VEC(input$Empresa)[[5]]
  })
  output$p3 <- renderPrint({
    cat("Prueba ADF ICO \n" )
    Funcion_VEC(input$Empresa)[[6]]
  })
  output$p4 <- renderPrint({
    cat("Prueba ADF ICO diferenciado \n")
    Funcion_VEC(input$Empresa)[[7]]
  })
  output$p5 <- renderPrint({
    cat("Prueba de cointegracion de Johansen - Juseluis \n")
    Funcion_VEC(input$Empresa)[[9]]
  })
  output$p6 <- renderPrint({
    cat("Seleccion del modelo VEC \n")
    Funcion_VEC(input$Empresa)[[10]]
  })
  output$p7 <- renderPrint({
    cat("Resumen del modelo VEC \n")
    Funcion_VEC(input$Empresa)[[11]]
  })
  output$p8 <- renderPrint({
    cat("Validacion. Independencia \n")
    Funcion_VEC(input$Empresa)[[13]]
  })
}

shinyApp(ui = ui, server = server)
