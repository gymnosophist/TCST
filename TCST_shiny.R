### Shiny app for TCST project ### 

library(shiny)
library(tidyverse)
library(hrbrthemes)
library(readr)
library(shinythemes)

# set seed 
set.seed(123)

# load data 
data <- read.csv('updated_master.csv')

data <- 
    data %>% # filter out two-year colleges 
    filter(ICLEVEL == 1)

select_school_list <- 
    data %>% 
    select(INSTNM) %>% 
    unique %>% arrange(INSTNM)

# set critical values -- We can hard code those taken from the original TCST or generate our own \
# warnings set at 25th percentile, alerts at 10th 

## 1. For enrollment, we can recalculate the critical values 

enrollment_warning <- quantile(data$index_enrollment_change, .20)
enrollment_alert <- quantile(data$index_enrollment_change, .1) 

## 2. Retention rate -- these are already scaled 

retention_warning <- quantile(data$RET.PCF, .20)
retention_alert <- quantile(data$RET.PCF, .1)

## 3. Market price 

price_warning <- quantile(data$index_price_change, .20)
price_alert <- quantile(data$index_price_change, .1)

## 4. Endowment:expense ratio 

ratio_warning <- quantile(data$endowment.expense, .2)
ratio_alert <- quantile(data$endowment.expense, .1)

## 5. appropriation change 

appropriation_warning <- quantile(data$index_total_appropriations, .2)
appropriation_alert <- quantile(data$index_total_appropriations, .1)


# TODO: 
# Adjust window sizes to accommodate variations in scale 
# 


### Define UI ### 

ui <-
    fluidPage(
    theme = shinytheme("journal"), 
    
    ######### Title page
    
    titlePanel("Exploring The College Stress Test with IPEDS Data"
    ),
    
    ######## Spacer 
    
    fluidRow(
        column(width = 3,
               style = "background-color:#bd0000;"
        ),
        column(width = 9, 
               style = 'background-color:bd0000;')
    ),
    
    ######## Input and score
    
    fluidRow(
        column(6, 
        h3("Choose a school down here 👇  to see its Stress Test score components and score over here -->"),
        inputPanel(
            selectizeInput(inputId = "school_select", # try selectizeInput
                        label = "Type in the name of a school:", 
                        choices = select_school_list, 
                        options = list(maxOptions = 5)
                        )
            )
    ),
    column(6, 
           textOutput('score')
           )
    ),
    
    ####### Display output
    fluidRow(
        column(width = 6,
               plotOutput("enrollment_plot")
               ),
        column(width = 6,
               plotOutput("retention_plot")
               ),
        ),
    
    ####### Finance 
    ####### Here we need a conditional plot depending on the kind of school selected 
    
    fluidRow(
        column(width = 6,
               plotOutput('market_price')
               ),
        column(width = 6,
               ##### Try render plot with if / else expression
               plotOutput("expense_appropriations"
               )
               )
        ),
    
    #### Spacer 
    
    fluidRow(
        column(width = 3,
               style = "background-color:#bd0000;"
        ),
        column(width = 9, 
               style = 'background-color:bd0000;')
    ),
    
    #### Distribution
    
    fluidRow(
        column(12,
               h4("Under construction")
               )
        )
    )




server <- function(input, output, session){
    
    ### Data
    
    data <- read.csv("updated_master.csv")
    
    
    #### Helpers 
    
    # Critical values
    
    ## 1 - First-year UG enrollment 
    
    enrollment_warning <- quantile(data$index_enrollment_change, .20, na.rm = T)
    enrollment_alert <- quantile(data$index_enrollment_change, .1, na.rm = T) 
    
    ## 2. Retention rate -- these are already scaled 
    
    retention_warning <- quantile(data$RET.PCF, .20, na.rm = T)
    retention_alert <- quantile(data$RET.PCF, .1, na.rm = T)
    
    ## 3. Market price 
    
    price_warning <- quantile(data$index_price_change, .20, na.rm = T)
    price_alert <- quantile(data$index_price_change, .1, na.rm = T)
    
    ## 4. Endowment:expense ratio 
    
    ratio_warning <- quantile(data$endowment.expense, .2, na.rm = T)
    ratio_alert <- quantile(data$endowment.expense, .1, na.rm = T)
    
    ## 5. appropriation change 
    
    appropriation_warning <- quantile(data$index_total_appropriations, .2, na.rm = T)
    appropriation_alert <- quantile(data$index_total_appropriations, .1, na.rm = T)
    
    
    
    ### Setup 
    
    # make variable reactive 
    
    stage_data = reactiveVal(data)
    
     plot_data <- reactive({
        stage_data() %>% 
             filter(INSTNM == input$school_select)
    })
     
     ctrl <- eventReactive(plot_data(), {
         plot_data() %>%
         pull(CONTROL) %>% mean()
         })
     
     
     
     
    
    ####### Render outputs 
     
    ####### Enrollment metrics 
     
    output$enrollment_plot <- # `EFUG1ST`
        renderPlot(
            {
                ggplot(plot_data(), aes(x = year, y = index_enrollment_change)) + 
                    geom_point() + 
                    geom_line(lty = 2, col = 'black', lwd = 1) +
                    geom_smooth(method = 'lm', se = F, lty = 2) + 
                    theme_bw() + 
                    geom_hline(yintercept = enrollment_warning, col = 'orange') + 
                    geom_hline(yintercept = enrollment_alert, col = 'red') + 
                    ggtitle(str_glue("Indexed Change in Enrollment at {input$school_select}")) + 
                    ylab("First-year Undergraduate Enrollment Change (2011 = 100)") + 
                    xlab("Year") + 
                    theme(panel.grid.minor = element_blank()) + 
                    annotate("text", x = 2018, y = enrollment_warning + 3.5, label = "Warning level") + 
                    annotate("text", x = 2018, y = enrollment_alert + 3.5, label = "Alert level")
                }
            )
        
    output$retention_plot <-
        renderPlot({
            ggplot(plot_data(), aes(x = year, y = RET.PCF)) + 
                geom_point() + 
                geom_line(lty = 2, col = 'gray', lwd = 1) + 
                geom_smooth(method = 'lm', se = F, lty = 2) + 
                theme_bw() + 
                theme(panel.grid.minor = element_blank()) + 
                ggtitle(str_glue("Freshman Retention Rate at {input$school_select}")) + 
                ylab("First to Second Year Retention Rate") + 
                xlab("Year") + 
                geom_hline(yintercept = retention_warning, col = 'orange') + 
                geom_hline(yintercept = retention_alert, col = 'red') + 
                annotate("text", x = 2018, y = retention_warning + .5, label = "Warning level") + 
                annotate("text", x = 2018, y = retention_alert + .5, label = "Alert level")
        })
        
    #### Financial Metrics 
    
    output$market_price <- 
        renderPlot({
            ggplot(plot_data(), aes(x = year, y = index_price_change)) + 
                geom_point() + 
                geom_line(lty = 2, col = 'gray', lwd = 1) + 
                geom_smooth(method = 'lm', se = F, lty = 2) + 
                theme_bw() + 
                theme(panel.grid.minor = element_blank()) + 
                scale_y_continuous(labels=scales::dollar_format()) +
                ggtitle(str_glue("Average Net Price at {input$school_select}")) + 
                ylab("First to Second Year Retention Rate (2011 = 100)") + 
                xlab("Year") + 
                geom_hline(yintercept = price_warning, col = 'orange') + 
                geom_hline(yintercept = price_alert, col = 'red') + 
                annotate("text", x = 2018, y = price_warning + .5, label = "Warning level") + 
                annotate("text", x = 2018, y = price_alert + .5, label = "Alert level")
        })

    output$expense_appropriations <- # try an eventReactive 
        
        renderPlot({
                
                if (ctrl() == 2) { # if CONTROL == 2 we have a PRIVATE institution -- endowment graph
                    
                        ggplot(plot_data(), aes(x = year, y = `endowment.expense`)) + 
                            geom_point() + 
                            geom_line(lty = 2, col = 'gray', lwd = 1) + 
                            geom_smooth(method = 'lm', se = F, lty = 2) + 
                            theme_bw() + 
                            theme(panel.grid.minor = element_blank()) + 
                            ggtitle(str_glue("Average Endowment:Expense Ratio at {input$school_select}")) + 
                            ylab("Endowment:Expense Ratio (2011 = 100)") + 
                            xlab("Year") + 
                            geom_hline(yintercept = ratio_warning, col = 'orange') + 
                            geom_hline(yintercept = ratio_alert, col = 'red') + 
                            annotate("text", x = 2018, y = ratio_warning + .5, label = "Warning level") + 
                            annotate("text", x = 2018, y = ratio_alert + .5, label = "Alert level")
                    }
                
                else if (ctrl() == 1){ ## public
                    ggplot(plot_data(), aes(x = year, y = index_total_appropriations)) +
                        geom_point() +
                        geom_line(lty = 2, col = 'gray', lwd = 1) +
                        geom_smooth(method = 'lm', se = F, lty = 2) +
                        theme_bw() +
                        theme(panel.grid.minor = element_blank()) + 
                        ggtitle(str_glue("Total State and Local Appropriations at {input$school_select}")) +
                        ylab("Total Appropriations (2011 = 100)") +
                        xlab("Year") + 
                        geom_hline(yintercept = appropriation_warning, col = 'orange') + 
                        geom_hline(yintercept = appropriation_alert, col = 'red') + 
                        annotate("text", x = 2018, y = appropriation_warning + .5, label = "Warning level") + 
                        annotate("text", x = 2018, y = appropriation_alert + .5, label = "Alert level")
                }
                
            # community colleges filtered out. ELSE condition is for-profit or NaNs 
            

            #### Calculate Score -- four binary metrics 
            
            # 1. Are any of the data points below the alert or warning levels?   
            
            
            
        })
    
    output$score <- renderText({
        "Under construction"
    })
        
        }
    


shinyApp(ui, server)
