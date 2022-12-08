# Load libraries
library(shiny)
library(asthma)
library(vroom)

ui <- fluidPage(


  titlePanel("Data distribution and viusalized"),
  #  fileInput("file1", label = "Please upload file: ", accept = c(".csv", ".tsv", ".txt")),
  ######file####
  fileInput("file", label = "Please upload csv file with columns of different variables you want to see the relationship with Asthma, and with a column specified individual's asthma condition(1 or 0).  Example data will be find in url above: ", accept = c(".csv", ".tsv", ".txt")),
  uiOutput("tab"),



  #####His####
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "varx",
                label = "Variable you want to display as histgram (1 var per time)",
                value = "AGE"
      ),
      textInput(inputId = "vary",
                label = "Name of Y axix  (A character)",
                value="FREQ"
      ),

    ),#sidebarPanel
    mainPanel(plotOutput(outputId = "His_plot"))
  ),#sidebarLayout
  #####Cor####
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "cor_x",
        "Select the variables you want to show correlation",
        choices = c("AGE","GENDER"),
      )
    ),
    mainPanel(plotOutput(outputId = "Cor_plot"))

  ),#sidebarLayout
  ####ln_name####
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "ln_name",
        "Varibale(s)'s distribution exception that should be ln to improve normality :",
        choices = c("AGE","GENDER"),
      )
    ),mainPanel("")
  ),
  #####fc_name####
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "fc_name",
        "Categorical variables should treated as factor in glm, y(Dependent variable) in glm should be defined here because status of disease only with 0 or 1",
        choices = c("AGE","GENDER"),
      )
    ),mainPanel(tableOutput(outputId = "data_lay"))
  ),
  #####glm###

  ####glm_cov####
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "glm_cov",
        "Covariates in glm. Attention: don't be the same as inependent(x) ",
        choices = c("AGE","GENDER"),
      )
    ),mainPanel("")
  ),
  ####glm_y####
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "glm_y",
                label = "Dependent variable in regression, should be defined in fc_name",
                value = "ASTHMA"
      )

    ),#sidebarPanel
    mainPanel("")
  ),
  ####glm_x####
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "glm_x",
                label = "Independent variable in regression",
                value = "AGE"
      )

    ),#sidebarPanel
    mainPanel(plotOutput(outputId = "Glm_plot"))
  ),
  #####
)#fluid


server <- function(input, output,session) {
  url <- a("Example: test.csv", href="https://github.com/zhouti34/asthma/tree/master/inst/extdata/test.csv")
  output$tab <- renderUI({
    tagList("URL link:", url)})
  data_use <- reactive({
    req(input$file)

    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ","),
           tsv = vroom::vroom(input$file$datapath, delim = "\t"),
           txt = vroom::vroom(input$file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv, .tsv or a .txt file")
    )
  })

  #corplot
  observe({


    updateCheckboxGroupInput(
      session,
      "cor_x",
      choices = colnames(x=data_use()),

    )


  })
  observe({
    updateCheckboxGroupInput(
      session,
      "ln_name",
      choices = colnames(x=data_use()),
    )
  })
  observe({
    updateCheckboxGroupInput(
      session,
      "fc_name",
      choices = colnames(x=data_use()),
    )
  })
  observe({
    updateCheckboxGroupInput(
      session,
      "glm_cov",
      choices = colnames(x=data_use()),
    )
  })

  # output$text <- renderText(input$cor_x[[1]])
  output$His_plot    <- renderPlot({

    print(his_plot(input$varx,title = paste0("Histogram of ",input$varx),xlab =input$varx ,ylab = input$vary,data = data_use()))
  })


  output$Cor_plot    <- renderPlot({
    print(cor_plot(input$cor_x, data = data_use()))
  })
  title1 <- reactive(paste0(input$glm_y," to ",input$glm_x))

  data_glm <- reactive((data_pre(ln_name=input$ln_name,fc_name=input$fc_name,data =data_use())))
  output$data_lay <- renderTable(head(x=data_glm(),10))



  output$Glm_plot    <- renderPlot({
    {print(glm_process(y=input$glm_y,x=input$glm_x,cov = input$glm_cov,family = "binomial", data = data_glm())[[2]]+ylab(input$glm_y)+ggtitle(title1()))}

  })
}#server

shinyApp(ui = ui, server = server)
