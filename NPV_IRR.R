#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(FinancialMath)
#library(FinancialMath)

# Define UI for application that computes NPV and IRR
ui <- fluidPage(
  
  # Application title
  titlePanel("NPV and IRR Financial Calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textOutput("Result"),
      radioButtons(inputId = "Type_of_Calculation",
                   label = "NPV or IRR",
                   choices = list("Net Present Value"=1, "Internal Rate of Return"=2),
                   selected=1),
      
      numericInput("nop_s",
                   "Number of Payments: ",
                   min = 0,
                   max = 999999999999999,
                   value = 5),
      numericInput("interest",
                   "Interest Rate (%): ",
                   min = 0,
                   max = 999999999999999,
                   value = 0),
      
      
      #create n number of input items
      
      actionButton("btnCOMPUTE", "Calculate")
      
      
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      uiOutput("ui1")
      
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  
  output$mynpvcalculate<- renderText(input$npvcalculate)
  
  #npv= present value of inflow - present value of outflow
  #present value of ouflow = cashflow0
  #present value of  inflow= ((cashflow1/(1+((interest/100))^1)+((cashflow2/(1+((interest/100))^2)+((cashflow3/(1+((interest/100))^3)+((cashflow4/(1+((interest/100))^4)
  
  
  
  observeEvent(input$btnCOMPUTE,{
    if(input$Type_of_Calculation==1){
      
      pv = as.integer(-input$cashflow0)
      interest = input$interest
      for (i in 1:input$nop_s){
        cashflow_i = input[[paste0("cashflow", i)]]
        
        #pv = pv + (cashflow_i/(1+((interest/100))**i))
        pv = pv + (cashflow_i/((1+(interest/100))**i))
        #  (1+(interest/100))^-1
        
        #output$myinterest<- renderText({input[[paste0("cashflow", i)]]})
      }
      output$Result<- renderText({paste0('The computed value for NPV is : ',round(pv,4))})
      
    }else{
      pv = as.integer(-input$cashflow0)
      interest = input$interest
      vector_cfs = c()
      vector_times = c()
      for (i in 1:input$nop_s){
        cashflow_i = input[[paste0("cashflow", i)]]
        
        vector_cfs <- c(vector_cfs, cashflow_i)
        vector_times <- c(vector_times, i)
      }
      output$Result<- renderText({paste0('The calculated value of IRR is: ',round(IRR(cf0=input$cashflow0,cf=vector_cfs,times=vector_times),6)*100,'%')})
      
    }
  })
  
  
  output$ui1 <- renderUI({
    #req(input$interest)
    
    lapply(0:input$nop_s, function(i) {
      
      
      numericInput(paste0("cashflow", i),
                   paste0("CF", i,":"),
                   min = 0,
                   max = 999999999999999,
                   value = 0)
      
      # Do Calculation here 
      
      
      
    })
  })
  
  
}


shinyApp(ui = ui, server = server)
# Run the application