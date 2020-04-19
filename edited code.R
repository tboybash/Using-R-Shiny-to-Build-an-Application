library(shiny)
library(FinancialMath)

ui <- fluidPage(
  titlePanel("Financial Calculator"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "Annuity_Approach",
                   label = "Click Ordinary, Arithmetric or Geometric",
                   choices = list("Ordinary Annuity"=1, "Arithmetric Progression"=2, "Geometric Progression"=3),
                   selected=1),
      numericInput("txtPeriod","Number of Period", value = 0),
      numericInput("txtRate","Interest Rate", value = 0),
      radioButtons(inputId = "Type_of_InterestRate",
                   label = "Click Effective or Nominal Interest Rate",
                   choices = list("Effective Interest Rate"=1, "Nominal Interest Rate"=2),
                   selected=1),
      numericInput("txtPV","Present Value", value = 0),
      numericInput("txtPMT","Payment Amount", value = 0),
      numericInput("txtFV","Future Value", value = 0),
      numericInput("txtINCRMT","Increment", value = 0),
      numericInput("txtP_Y","P/Y", value = 1),
      numericInput("txtC_Y","C/Y", value = 1),
      radioButtons(inputId = "Type_of_Calculation",
                   label = "Click Annuity Due or Annuity Immediate",
                   choices = list("Annuity Immediate"=1, "Annuity Due"=2),
                   selected=1),
      actionButton("btnCOMPUTE", "Compute")
    ),
    mainPanel(
      
      #uiOutput("ui1")
      textOutput("results_1"),
      textOutput("results"),
      textOutput("results_2"),
      textOutput("results_3")
      
    )
  )
) 

server <- shinyServer(function(input,output){
  
  output$results = renderText({ 
    "Computed Results will be displayed here"})
  
  observeEvent(input$btnCOMPUTE, {
  if (input$Annuity_Approach == 1){ 
    result_3 = "Ordinary Annuity"
    if (input$Type_of_InterestRate == 1){
      result_2 = "Calculation using Effective Interest rate"
       # this is ORDINARY ANNUITY
      if (input$Type_of_Calculation == 1){
        #We Selected Anuity Immediate
        result_1 = "Calculations Using Annuity Immediate"
        
        if (is.na(input$txtPeriod)){
          
          if (is.na(input$txtPV)){
            
            # Put Future Value formular here
            n = log(((input$txtFV*input$txtRate)+input$txtPMT)/input$txtPMT)/log(1+input$txtRate)
            result = paste0("The calculated number of Periods is : ", n)
            
          }else{
            
            tmp =log(input$txtPMT/(input$txtPMT-(input$txtPV*input$txtRate)))/log(1+input$txtRate)
            result = paste0("The calculated number of Periods is : ", tmp)
            
          }
          
        }
        else if (is.na(input$txtRate)){
          
          #result = "This is the result for Rate"
          a=annuity.level(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,pmt=input$txtPMT,i=(input$txtRate/100),ic=1,pf=1,imm=T)
          erate=a['Eff Rate',]
          result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
          
          
        }
        else if (is.na(input$txtPV)){
          
          #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
          #result = "This is the result for Rate"
          
          #PV = pmt((1-(1+i))/i)
          tmp = input$txtPMT*((1-((1+input$txtRate)^(-input$txtPeriod)))/input$txtRate)
          result = paste0("The calculated Present Value is : ", tmp)
          
        }
        else if (is.na(input$txtPMT)){
          if (is.na(input$txtPV)){
            m = input$txtFV/((1-((1+input$txtRate)^(-input$txtPeriod)))/input$txtRate)
            result = paste0("The calculated Payment Amount is : ", m)
          }
          else {
            tmp = input$txtPV/((1-((1+input$txtRate)^(-input$txtPeriod)))/input$txtRate)
            result = paste0("The calculated Payment Amount is : ", tmp)
          }
          
        }
        else if (is.na(input$txtFV)){
          
          tmp = input$txtPMT*((((1+input$txtRate)^(input$txtPeriod))-1)/input$txtRate)
          result = paste0("The calculated Future Value is : ", tmp)
          
        }
        
        
      }else{
        # We Selected Annuity Due 
        result_1 = "Calculations Using Annuity Due"
        
        
        if (is.na(input$txtPeriod)){
          if (is.na(input$txtPV)){
            n = log(((input$txtFV*input$txtRate)+input$txtPMT*(1+input$txtRate))/input$txtPMT*(1+input$txtRate))/log(1+input$txtRate)
            result = paste0("The calculated number of Periods is : ", n)
          }
          else {
            tmp =log((input$txtPMT * (1+input$txtRate))/((input$txtPMT * (1 + input$txtRate)) - (input$txtPV * input$txtRate)))/log(1+input$txtRate)
            result = paste0("The calculated number of Periods is : ", tmp)
          }
          
        }
        else if (is.na(input$txtRate)){
          
          a=annuity.level(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,pmt=input$txtPMT,i=(input$txtRate/100),ic=1,pf=1,imm=F)
          erate=a['Eff Rate',]
          result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
          
        }
        else if (is.na(input$txtPV)){
          
          #result = "This is the result for Present value"
          tmp =(((1+input$txtRate)*input$txtPMT)*((1-((1+input$txtRate)^(-input$txtPeriod)))/input$txtRate))
          result = paste("The calculated Present Value is : ", tmp)
          
        }
        else if (is.na(input$txtPMT)){
          
          if (is.na(input$txtPV)){
            tmp = input$txtFV/(((1+input$txtRate))*((1-((1+input$txtRate)^(-input$txtPeriod)))/input$txtRate))
            result = paste0("The calculated Payment Amount is : ", tmp)
          }
          else {
            tmp = input$txtPV/(((1+input$txtRate))*((1-((1+input$txtRate)^(-input$txtPeriod)))/input$txtRate))
            result = paste0("The calculated Payment Amount is : ", tmp)
          }
        }
        else if (is.na(input$txtFV)){
          
          tmp =(((1+input$txtRate)*input$txtPMT)*((((1+input$txtRate)^(input$txtPeriod))-1)/input$txtRate))
          result = paste("The calculated Future Value is : ", tmp)
          
        }
        
      }
      
    }
    else {
      result_2 = "Calculation using Nominal Interest rate"
      if (input$Type_of_Calculation == 1){
        #We Selected Anuity Immediate
        result_1 = "Calculations Using Annuity Immediate"
        
        if (is.na(input$txtPeriod)){
          
          if (is.na(input$txtPV)){
            
            # Put Future Value formular here
            n = log(((input$txtFV*(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)))+input$txtPMT)/input$txtPMT)/log(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))
            result = paste0("The calculated number of Periods is : ", n)
            
          }else{
            
            tmp =log(input$txtPMT/(input$txtPMT-(input$txtPV*(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1))))/log(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))
            result = paste0("The calculated number of Periods is : ", tmp)
            
            
          }
          
        }
        else if (is.na(input$txtRate)){
          
          #result = "This is the result for Rate"
          a=annuity.level(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,pmt=input$txtPMT,i=(input$txtRate/100),ic= input$C_Y,pf=1,imm=T)
          erate=a['Eff Rate',]
          result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
          
          
        }
        else if (is.na(input$txtPV)){
          
          #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
          #result = "This is the result for Rate"
          
          #PV = pmt((1-(1+i))/i)
          tmp = input$txtPMT*((1-((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))^(-input$txtPeriod)))/(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1))
          result = paste0("The calculated Present Value is : ", tmp)
          
        }
        else if (is.na(input$txtPMT)){
          
          if (is.na(input$txtPV)){
            m = input$txtFV/((1-((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))^(-input$txtPeriod)))/(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1))
            result = paste0("The calculated Payment Amount is : ", m)
          }
          else {
            tmp = input$txtPV/((1-((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))^(-input$txtPeriod)))/(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1))
            result = paste0("The calculated Payment Amount is : ", tmp)
          }
          
        }
        else if (is.na(input$txtFV)){
          
          tmp = input$txtPMT*((((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))^(input$txtPeriod))-1)/(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1))
          result = paste0("The calculated Future Value is : ", tmp)
          
        }
        
        
      }else{
        # We Selected Annuity Due 
        result_1 = "Calculations Using Annuity Due"
        
        
        if (is.na(input$txtPeriod)){
          if (is.na(input$txtPV)){
            n = log(((input$txtFV*(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1))+input$txtPMT*(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)))/input$txtPMT*(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)))/log(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1)
            result = paste0("The calculated number of Periods is : ", n)
          }
          else {
            tmp =log((input$txtPMT * (((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)))/((input$txtPMT * (((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))) - (input$txtPV * (((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1))))/log(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))
            result = paste0("The calculated number of Periods is : ", tmp)
          }
          
        }
        else if (is.na(input$txtRate)){
          
          a=annuity.level(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,pmt=input$txtPMT,i=(input$txtRate/100),ic=input$C_Y,pf=1,imm=F)
          erate=a['Eff Rate',]
          result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
          
        }
        else if (is.na(input$txtPV)){
          
          #result = "This is the result for Present value"
          tmp =((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))*input$txtPMT)*((1-((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))^(-input$txtPeriod)))/(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1))
          result = paste("The calculated Present Value is : ", tmp)
          
        }
        else if (is.na(input$txtPMT)){
          
          if (is.na(input$txtPV)){
            tmp = input$txtFV/(((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)))*((1-((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))^(-input$txtPeriod)))/(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1)))
            result = paste0("The calculated Payment Amount is : ", tmp)
          }
          else {
            tmp = input$txtPV/(((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)))*((1-((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))^(-input$txtPeriod)))/(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1)))
            result = paste0("The calculated Payment Amount is : ", tmp)
          }
          
        }
        else if (is.na(input$txtFV)){
          
          tmp =(((((1+(input$txtRate)/input$txtC_Y))^input$txtC_Y))*input$txtPMT)*((((((1+(input$txtRate/input$txtC_Y))^input$txtC_Y))^(input$txtPeriod))-1)/(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)-1))
          result = paste("The calculated Future Value is : ", tmp)
          
        }
        
      }
    }
    
    
    
    
    
  } 
    
###############################################################
######   END OF FIRST   ##########    
    
   else if (input$Annuity_Approach == 2){   
     result_3 = "Arithmetric Progression Annuity"
     if (input$Type_of_InterestRate == 1){
       result_2 = "Calculation using Effective Interest rate"
       
       if (input$Type_of_Calculation == 1){
         #We Selected Anuity Immediate
         result_1 = "Calculations Using Annuity Immediate"
         
         if (is.na(input$txtPeriod)){
           
             
             # Put Future Value formular here
             n=annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=1,pf=1,imm=TRUE,plot=FALSE)
             #n=annuity.level(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,pmt=input$txtPMT,i=input$txtRate,ic=1,pf=1,imm=TRUE,plot=FALSE)
           
             nn=round(n['Years',])
             
             result = paste0("The calculated number of Periods is : ", nn, 'years')

           
         }
         else if (is.na(input$txtRate)){
           
           #result = "This is the result for Rate"
           a=annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           erate=a['Eff Rate',]
           result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
           
           
         }
         else if (is.na(input$txtPV) && is.na(input$txtFV)){
           
           #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
           #result = "This is the result for Rate"
           
           #PV = pmt((1-(1+i))/i)
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)

           result = paste0("The calculated Present Value is : ", round(tmp['PV',],4), ' and the calculated Future Value is ',round(tmp['FV',],4))
           
         }
         
         else if (is.na(input$txtPV)){
           
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Present Value is : ", round(tmp['PV',],4))
           
         }
         
         else if (is.na(input$txtPMT)){
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Payment Amount is : ", round(tmp['P',],4))
           
         }
         
         else if (is.na(input$txtFV)){
           
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Future Value is : ", round(tmp['FV',],4))
           
         }
         
         else if (is.na(input$txtINCRMNT)){
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Increment is : ", round(tmp['Q',],4))
           
         }
         
         
       }else{
         # We Selected Annuity Due 
         result_1 = "Calculations Using Annuity Due"
         
         
         if (is.na(input$txtPeriod)){
           
           
           # Put Future Value formular here
           n=annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           result = paste0("The calculated number of Periods is : ", round(n['Years',]), 'years')
           
           
         }
         else if (is.na(input$txtRate)){
           
           #result = "This is the result for Rate"
           a=annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           erate=a['Eff Rate',]
           result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
           
           
         }
         
         
         
         else if (is.na(input$txtPV) && is.na(input$txtFV)){
           
           #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
           #result = "This is the result for Rate"
           
           #PV = pmt((1-(1+i))/i)
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           
           result = paste0("The calculated Present Value is : ", round(tmp['PV',],4), ' and the calculated Future Value is ',round(tmp['FV',],4))
           
         }
         
         else if (is.na(input$txtPV)){
           
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           
           result = paste0("The calculated Present Value is : ", round(tmp['PV',],4))
           
         }
         
         
         else if (is.na(input$txtPMT)){
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           
           result = paste0("The calculated Payment Amount is : ", round(tmp['P',],4))
           
         }
         
         else if (is.na(input$txtFV)){
           
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           
           result = paste0("The calculated Future Value is : ", round(tmp['FV',],4))
           
         }
         
         else if (is.na(input$txtINCRMNT)){
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Increment is : ", round(tmp['Q',],4))
           
         }
       }
       
     }
##################################################
     else {
       result_2 = "Calculation using Nominal Interest rate"
       if (input$Type_of_Calculation == 1){
         #We Selected Anuity Immediate
         result_1 = "Calculations Using Annuity Immediate"
         
         if (is.na(input$txtPeriod)){
           
           
           # Put Future Value formular here
           n=annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           result = paste0("The calculated number of Periods is : ", round(n['Years',]), 'years')
           
           
         }
         else if (is.na(input$txtRate)){
           
           #result = "This is the result for Rate"
           a=annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           erate=a['Eff Rate',]
           result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
           
           
         }
         
         else if (is.na(input$txtPV) && is.na(input$txtFV)){
           
           #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
           #result = "This is the result for Rate"
           
           #PV = pmt((1-(1+i))/i)
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Present Value is : ", round(tmp['PV',],4), ' and the calculated Future Value is ',round(tmp['FV',],4))
           
         }
         
         else if (is.na(input$txtPV)){
           
           #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
           #result = "This is the result for Rate"
           
           #PV = pmt((1-(1+i))/i)
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Present Value is : ", round(tmp['PV',],4))
           
         }
         else if (is.na(input$txtPMT)){
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Payment Amount is : ", round(tmp['P',],4))
           
         }
         
         else if (is.na(input$txtFV)){
           
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Future Value is : ", round(tmp['FV',],4))
           
         }
         
         else if (is.na(input$txtINCRMNT)){
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Increment is : ", round(tmp['Q',],4))
           
         }
         
         
       }else{
         # We Selected Annuity Due 
         result_1 = "Calculations Using Annuity Due"
         
         
         if (is.na(input$txtPeriod)){
           
           
           # Put Future Value formular here
           n=annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           result = paste0("The calculated number of Periods is : ", round(n['Years',]), 'years')
           
           
         }
         else if (is.na(input$txtRate)){
           
           #result = "This is the result for Rate"
           a=annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           erate=a['Eff Rate',]
           result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
          
             
         }
         
         else if (is.na(input$txtPV) && is.na(input$txtFV)){
           
           #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
           #result = "This is the result for Rate"
           
           #PV = pmt((1-(1+i))/i)
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           
           result = paste0("The calculated Present Value is : ", round(tmp['PV',],4), ' and the calculated Future Value is ',round(tmp['FV',],4))
           
         }
         
         
         else if (is.na(input$txtPV)){
           
           #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
           #result = "This is the result for Rate"
           
           #PV = pmt((1-(1+i))/i)
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           
           result = paste0("The calculated Present Value is : ", round(tmp['PV',],4))
           
         }
         else if (is.na(input$txtPMT)){
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           
           result = paste0("The calculated Payment Amount is : ", round(tmp['P',],4))
           
         }
         
         else if (is.na(input$txtFV)){
           
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
           
           result = paste0("The calculated Future Value is : ", round(tmp['FV',],4))
           
         }
         
         else if (is.na(input$txtINCRMNT)){
           tmp = annuity.arith(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,q=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
           
           result = paste0("The calculated Payment Amount is : ", round(tmp['Q',],4))
           
         }
       }       
     } 
     
   }
    
###############################################################
######   END OF SECOND   ##########     
    
    else if (input$Annuity_Approach == 3){   
      result_3 = "Geometric Progression Annuity"
      if (input$Type_of_InterestRate == 1){
        result_2 = "Calculation using Effective Interest rate"
        
        if (input$Type_of_Calculation == 1){
          #We Selected Anuity Immediate
          result_1 = "Calculations Using Annuity Immediate"
          
          if (is.na(input$txtPeriod)){
            
            
            # Put Future Value formular here
            n=annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            result = paste0("The calculated number of Periods is : ", round(n['Years',]), 'years')
            
            
          }
          else if (is.na(input$txtRate)){
            
            #result = "This is the result for Rate"
            a=annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            erate=a['Eff Rate',]
            result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
            
            
          }
          
          else if (is.na(input$txtPV) && is.na(input$txtFV)){
            
            #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
            #result = "This is the result for Rate"
            
            #PV = pmt((1-(1+i))/i)
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            
            result = paste0("The calculated Present Value is : ", round(tmp['PV',],4), ' and the calculated Future Value is ',round(tmp['FV',],4))
            
          }
          
          else if (is.na(input$txtPMT)){
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            
            result = paste0("The calculated Payment Amount is : ", round(tmp['P',],4))
            
          }
          
          else if (is.na(input$txtFV)){
              
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            
            result = paste0("The calculated Future Value is : ", round(tmp['FV',],4))

          }
          
          else if (is.na(input$txtINCRMT)){
            
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            
            result = paste0("The calculated Increment is : ", round(tmp['K',],4))
            
          }
          
          
        }else{
          # We Selected Annuity Due 
          result_1 = "Calculations Using Annuity Due"
          
          
          if (is.na(input$txtPeriod)){
            
            
            # Put Future Value formular here
            n=annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            result = paste0("The calculated number of Periods is : ", round(n['Years',]), 'years')
            
            
          }
          else if (is.na(input$txtRate)){
            
            #result = "This is the result for Rate"
            a=annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            erate=a['Eff Rate',]
            result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
          
            
          }
          
          
          else if (is.na(input$txtPV) && is.na(input$txtFV)){
            
            #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
            #result = "This is the result for Rate"
            
            #PV = pmt((1-(1+i))/i)
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            
            result = paste0("The calculated Present Value is : ", round(tmp['PV',],4), ' and the calculated Future Value is ',round(tmp['FV',],4))
            
          }
          
          else if (is.na(input$txtPV)){
            
            #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
            #result = "This is the result for Rate"
            
            #PV = pmt((1-(1+i))/i)
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            
            result = paste0("The calculated Present Value is : ", round(tmp['PV',],4))
            
          }
          else if (is.na(input$txtPMT)){
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            
            result = paste0("The calculated Payment Amount is : ", round(tmp['P',],4))
            
          }
          
          else if (is.na(input$txtFV)){
            
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            
            result = paste0("The calculated Future Value is : ", round(tmp['FV',],4))
            
            
          }
          
          else if (is.na(input$txtINCRMT)){
            
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=input$txtRate,ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            
            result = paste0("The calculated Increment is : ", round(tmp['K',],4))
            
          }
          
        }
        
      }
      ##################################################
      else {
        result_2 = "Calculation using Nominal Interest rate"
        if (input$Type_of_Calculation == 1){
          #We Selected Anuity Immediate
          result_1 = "Calculations Using Annuity Immediate"
          
          if (is.na(input$txtPeriod)){
            
            
            # Put Future Value formular here
            n=annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            result = paste0("The calculated number of Periods is : ", round(n['Years',]), 'years')
            
            
          }
          else if (is.na(input$txtRate)){
            
            #result = "This is the result for Rate"
            a=annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            erate=a['Eff Rate',]
            result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
            
            
          }
          
          else if (is.na(input$txtPV) && is.na(input$txtFV)){
            
            #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
            #result = "This is the result for Rate"
            
            #PV = pmt((1-(1+i))/i)
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            
            result = paste0("The calculated Present Value is : ", round(tmp['PV',],4), ' and the calculated Future Value is ',round(tmp['FV',],4))
            
          }
          
          else if (is.na(input$txtPV)){
            
            #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
            #result = "This is the result for Rate"
            
            #PV = pmt((1-(1+i))/i)
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            
            result = paste0("The calculated Present Value is : ", round(tmp['PV',],4))
            
          }
          else if (is.na(input$txtPMT)){
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            
            result = paste0("The calculated Payment Amount is : ", round(tmp['P',],4))
            
          }
          
          else if (is.na(input$txtFV)){
            
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            
            result = paste0("The calculated Future Value is : ", round(tmp['FV',],4))
            
          }
          
          else if (is.na(input$txtINCRMT)){
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            
            result = paste0("The calculated Increment is : ", round(tmp['K',],4))
            
          }
          
          
        }
        
        else{
          # We Selected Annuity Due 
          result_1 = "Calculations Using Annuity Due"
          
          
          if (is.na(input$txtPeriod)){
            
            
            # Put Future Value formular here
            n=annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            result = paste0("The calculated number of Periods is : ", round(n['Years',]), 'years')
            
            
          }
          else if (is.na(input$txtRate)){
            
            #result = "This is the result for Rate"
            a=annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            erate=a['Eff Rate',]
            result = paste0("The calculated rate is : ", round(erate,4)*100,'%')
            
            
          }
          
          else if (is.na(input$txtPV) && is.na(input$txtFV)){
            
            #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
            #result = "This is the result for Rate"
            
            #PV = pmt((1-(1+i))/i)
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            
            result = paste0("The calculated Present Value is : ", round(tmp['PV',],4), ' and the calculated Future Value is ',round(tmp['FV',],4))
            
          }
          
          else if (is.na(input$txtPV)){
            
            #PV = input$txtPMT((1-(1+input$txtRate))/input$txtRate)
            #result = "This is the result for Rate"
            
            #PV = pmt((1-(1+i))/i)
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            
            result = paste0("The calculated Present Value is : ", round(tmp['PV',],4))
            
          }
          else if (is.na(input$txtPMT)){
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            
            result = paste0("The calculated Payment Amount is : ", round(tmp['P',],4))
            
          }
          
          else if (is.na(input$txtFV)){
            
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=FALSE,plot=FALSE)
            
            result = paste0("The calculated Future Value is : ", round(tmp['FV',],4))
            
          }
          
          else if (is.na(input$txtINCRMT)){
            tmp = annuity.geo(pv=input$txtPV,fv=input$txtFV,n=input$txtPeriod,p=input$txtPMT,k=input$txtINCRMT,i=(((1+(input$txtRate/input$txtC_Y))^input$txtC_Y)),ic=input$txtC_Y,pf=input$txtP_Y,imm=TRUE,plot=FALSE)
            
            result = paste0("The calculated Increment is : ", round(tmp['K',],4))
            
          }
        }       
      } 
      
      
    }
    
###############################################################
######   END OF THIRD   ########## 
    
    # k = input$Type_of_Calculation
    output$results_1 = renderText({ 
      result_1})
    
    output$results = renderText({ 
      result})
    output$results_2 = renderText({ 
      result_2})
    output$results_3 = renderText({ 
      result_3})
    
  
    
    
  })
  
  
  
})

shinyApp(ui, server)