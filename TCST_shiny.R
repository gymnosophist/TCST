### Shiny app for TCST project ### 

library(shiny)
library(tidyverse)
library(hrbrthemes)
library(readr)
library(shinythemes)

# set seed 
set.seed(123)

# load data 
data <- read.csv('master_df.csv')

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

enrolment_warning <- quantile(data$index_enrollment_change, .20)
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
# - Change criteria based on class of school
# - fix finance plots 


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
        column(9, 
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
               ##### Conditional plot output 
               plotOutput('expense_ratio'))
    ))



server <- function(input, output, session){
    
    data <- read.csv("master_df.csv")
    
    print(names(data))
    
        
    # make variable reactive 
    
    stage_data = reactiveVal(data)
    
     plot_data <- reactive({
        stage_data() %>% filter(INSTNM == input$school_select)
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
                ggtitle(str_glue("Indexed Change in Enrollment at {input$school_select}")) + 
                ylab("First-year Undergraduate Enrollment Change (2011 = 100)") + 
                xlab("Year")
                }
            )
        
    output$retention_plot <-
        renderPlot({
            ggplot(plot_data(), aes(x = year, y = RET.PCF)) + 
                geom_point() + 
                geom_line(lty = 2, col = 'gray', lwd = 1) + 
                geom_smooth(method = 'lm', se = F, lty = 2) + 
                theme_bw() + 
                ggtitle(str_glue("Freshman Retention Rate at {input$school_select}")) + 
                ylab("First to Second Year Retention Rate") + 
                xlab("Year")
        })
        
    #### Financial Metrics 
    
    output$market_price <- 
        renderPlot({
            ggplot(plot_data(), aes(x = year, y = avg_net_price)) + 
                geom_point() + 
                geom_line(lty = 2, col = 'gray', lwd = 1) + 
                geom_smooth(method = 'lm', se = F, lty = 2) + 
                theme_bw() + 
                scale_y_continuous(labels=scales::dollar_format()) +
                ggtitle(str_glue("Average Net Price at {input$school_select}")) + 
                ylab("First to Second Year Retention Rate (2011 = 100)") + 
                xlab("Year")
        })
        

    output$expense_ratio <-
        renderPlot({
            ggplot(plot_data(), aes(x = year, y = `endowment.expense`)) + 
                geom_point() + 
                geom_line(lty = 2, col = 'gray', lwd = 1) + 
                geom_smooth(method = 'lm', se = F, lty = 2) + 
                theme_bw() + 
                ggtitle(str_glue("Average Endowment:Expense Ratio at {input$school_select}")) + 
                ylab("Endowment:Expense Ratio (2011 = 100)") + 
                xlab("Year") 
        })
            
        
        
        }
    

shinyApp(ui, server)
