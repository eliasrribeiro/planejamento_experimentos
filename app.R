library(shiny)
library(shinydashboard)
library(tidyverse)
library(xtable)
library(knitr)
theme_set(theme_light())
options(knitr.kable.NA = '-')


dados <- readr::read_table("Tree_throughfall.txt")[,1:11]
colnames(dados) <- c("date", "pluviometro", "plot", "distance", 
                     "tree_sp", "tree_number", "rainfall", "rainfall_size", 
                     "throughfall", "interception", "lai")

var_utilizadas <- c(
  "plot",
  "distance",
  "tree_sp",
  "rainfall_size"
)

dados <- dados %>% 
  transform(date = as.Date(date, format = "%d/%m/%Y"),
            plot = as.factor(plot),
            rainfall_size = as.factor(rainfall_size),
            tree_sp = as.factor(tree_sp),
            tree_number = as.factor(tree_number),
            distance = as.factor(distance))

ui <- navbarPage(
  "PLANEJAMENTO DE EXPERIMENTOS NO SISTEMA DE CAFÉ SOMBREADO",
  tabPanel(
    "Descritiva",
    icon = icon("bar-chart-o"),
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "eixox",
            label = "Fator do eixo X:",
            choices = var_utilizadas,
            selected = "rainfall_size"
          ),
          selectInput(
            inputId = "color",
            label = "Fator como cor:",
            choices = c(var_utilizadas),
            selected = c("tree_sp")
          ),
          selectInput(
            inputId = "facet",
            label = "Deseja utilizar facet?",
            choices = c("Sim", "Não"),
            selected = c("Não")
          ),
          conditionalPanel(
            condition = "input.facet == 'Sim'",
            selectInput(
              inputId = "var_facet",
              label = "Selecione o fator:",
              choices = var_utilizadas,
              selected = c("distance")
            )
          )
        ),
        mainPanel(plotOutput("plot"))
      )
    )
  ),
  tabPanel(
    "ANOVA",
    icon = icon("th", lib = "glyphicon"),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "n_var",
            label = "Quantidade de fatores utilizados na ANOVA:",
            choices = c("1", "2", "3"),
            selected = c("2")
          ),
          conditionalPanel(
            condition = "input.n_var == '1'",
            selectInput(
              inputId = "var1",
              label = "Selecione o fator:",
              choices = var_utilizadas,
              selected = c("rainfall_size")
            )
          ),
          conditionalPanel(
            condition = "input.n_var == '2'",
            selectInput(
              inputId = "var_1",
              label = "Selecione o primeiro fator:",
              choices = var_utilizadas,
              selected = c("rainfall_size")
            ),
            selectInput(
              inputId = "var_2",
              label = "Selecione o segundo fator:",
              choices = var_utilizadas,
              selected = c("distance")
            )
          ),
          conditionalPanel(
            condition = "input.n_var == '3'",
            selectInput(
              inputId = "var__1",
              label = "Selecione o primeiro fator:",
              choices = var_utilizadas,
              selected = c("rainfall_size")
            ),
            selectInput(
              inputId = "var__2",
              label = "Selecione o segundo fator:",
              choices = var_utilizadas,
              selected = c("distance")
            ),
            selectInput(
              inputId = "var__3",
              label = "Selecione o terceiro fator:",
              choices = var_utilizadas,
              selected = c("tree_sp")
            )
          ),
          conditionalPanel(
            condition = "input.n_var != '1'",
            selectInput(
              inputId = "delineamento",
              label = "Tipo de delineamento:",
              choices = c("Blocos", "Fatorial"),
              selected = c("Fatorial")
            )
          )
        ),
        mainPanel(verbatimTextOutput("anova"),
                  plotOutput("plot2"))
      )
    )
  ),
  tabPanel(
    "Sobre",
    icon = icon("info-sign", 
                lib = "glyphicon"),
    fluidPage(
      includeMarkdown("sobre.md")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$facet == "Sim") {
      dados %>%
        ggplot(aes(
          x = get(input$eixox),
          y = throughfall,
          color = get(input$color),
          fill = get(input$color)
        )) +
        geom_boxplot(alpha = 0.6) +
        labs(x = input$eixox,
             color = input$color,
             fill = input$color) + 
        facet_wrap(~ get(input$var_facet)) +
        scale_fill_viridis_d() +
        scale_color_viridis_d() +
        theme(legend.position = "bottom")
    }
    else{
      dados %>%
        ggplot(aes(
          x = get(input$eixox),
          y = throughfall,
          color = get(input$color),
          fill = get(input$color)
        )) +
        geom_boxplot(alpha = 0.6) +
        labs(x = input$eixox,
             color = input$color,
             fill = input$color) + 
        scale_fill_viridis_d() +
        scale_color_viridis_d() +
        theme(legend.position = "bottom")
    }
  })
  
  output$anova <- renderPrint({
    if(input$n_var == '1'){
      x <- aov(throughfall ~ get(input$var1),
          data = dados) %>%
        anova() 
      
      rownames(x)[1] <- c(input$var1)
    }
    else if(input$n_var == '2' & input$delineamento == 'Blocos'){
      x <- aov(throughfall ~ get(input$var_1) + get(input$var_2),
          data = dados) %>%
        anova()
      
      rownames(x)[1:2] <- c(input$var_1, input$var_2)
    }
    else if(input$n_var == '2' & input$delineamento == 'Fatorial'){
      x <- aov(throughfall ~ get(input$var_1) * get(input$var_2),
          data = dados) %>%
        anova()
      
      rownames(x)[1:3] <- c(input$var_1, input$var_2, 
                            paste0(input$var_1, ":", input$var_2))
    }
    else if(input$n_var == '3' & input$delineamento == 'Blocos'){
      x <- aov(throughfall ~ get(input$var__1) + get(input$var__2) + get(input$var__3),
          data = dados) %>%
        anova()
      
      rownames(x)[1:3] <- c(input$var__1, input$var__2, input$var__3)
    }
    else if(input$n_var == '3' & input$delineamento == 'Fatorial'){
      x <- aov(throughfall ~ get(input$var__1) * get(input$var__2) * get(input$var__3),
          data = dados) %>%
        anova()
      
      rownames(x)[1:7] <- c(input$var__1, input$var__2, input$var__3,
                            paste0(input$var__1, ":", input$var__2),
                            paste0(input$var__1, ":", input$var__3),
                            paste0(input$var__2, ":", input$var__3),
                            paste0(input$var__1, ":", input$var__2, ":", input$var__3))
    }
    x
  })
  output$plot2 <- renderPlot({
    if(input$n_var == '2' & input$delineamento == 'Fatorial'){
      interaction.plot(dados[[input$var_1]], 
                       dados[[input$var_2]], 
                       dados[["throughfall"]], 
                       legend=T, 
                       col=2:4, 
                       lwd=2,
                       xlab = input$var_1, 
                       ylab = "Throughfall",
                       data=dados,
                       trace.label = input$var_2,
                       type="l")
    }
    if(input$n_var == '3' & input$delineamento == 'Fatorial'){
      colr <- RColorBrewer::brewer.pal(11, "PiYG")
      colr <- colorRampPalette(colr, space = "rgb")
      lattice::wireframe(throughfall ~  get(input$var__1) + get(input$var__2) | get(input$var__3), 
                         data = dados, 
                         xlab = list(input$var__1, rot = 30),
                         ylab = list(input$var__2, rot = -30),
                         drape = TRUE, 
                         type = "on",
                         scales = list(arrows = FALSE),
                         col.regions = colr(100))
    }
  })
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('sobre.md', quiet = TRUE)))
  })
}

shinyApp(ui, server)
