library(shiny)
library(HistData)
data(GaltonFamilies)
library(dplyr)
library(ggplot2)

# 1st step: pass inches to cm
gf <- GaltonFamilies
gf <- gf %>% mutate(father=father*2.54,
                    mother=mother*2.54,
                    childHeight=childHeight*2.54)

# linear model
model1 <- lm(childHeight ~ father + mother + gender, data=gf)

server <- shinyServer(function(input, output) {
  output$pText <- renderText({
    paste("Father's height is",
          strong(round(input$inFh, 1)),
          "cm, and mother's height is",
          strong(round(input$inMh, 1)),
          "cm, then:")
  })
  output$pred <- renderText({
    df <- data.frame(father=input$inFh,
                     mother=input$inMh,
                     gender=factor(input$inGen, levels=levels(gf$gender)))
    ch <- predict(model1, newdata=df)
    kid <- ifelse(
      input$inGen=="female",
      "Daugther",
      "Son"
    )
    paste0(em(strong(kid)),
           "'s predicted height is going to be around ",
           em(strong(round(ch))),
           " cm"
    )
  })
  output$Plot <- renderPlot({
    kid <- ifelse(
      input$inGen=="female",
      "Daugther",
      "Son"
    )
    df <- data.frame(father=input$inFh,
                     mother=input$inMh,
                     gender=factor(input$inGen, levels=levels(gf$gender)))
    ch <- predict(model1, newdata=df)
    yvals <- c("Father", kid, "Mother")
    df <- data.frame(
      x = factor(yvals, levels = yvals, ordered = TRUE),
      y = c(input$inFh, ch, input$inMh))
    ggplot(df, aes(x=x, y=y, color=c("red", "green", "blue"), fill=c("red", "green", "blue"))) +
      geom_bar(stat="identity", width=0.5) +
      xlab("") +
      ylab("Height (cm)") +
      theme_minimal() +
      theme(legend.position="none")
  })
})

ui <- shinyUI(fluidPage(
  titlePanel("Prediction of height"),
  sidebarLayout(
    sidebarPanel(
      helpText("Prediction of the child's height considering gender and parent's height"),
      helpText("Parameters:"),
      sliderInput(inputId = "inFh",
                  label = "Father's height (cm):",
                  value = 150,
                  min = 150,
                  max = 220,
                  step = 1),
      sliderInput(inputId = "inMh",
                  label = "Mother's height (cm):",
                  value = 140,
                  min = 140,
                  max = 200,
                  step = 1),
      radioButtons(inputId = "inGen",
                   label = "Child's gender: ",
                   choices = c("Female"="female", "Male"="male"),
                   inline = TRUE)
    ),
    
    mainPanel(
      htmlOutput("pText"),
      htmlOutput("pred"),
      plotOutput("Plot", width = "50%")
    )
  )
))

shinyApp(ui, server)
