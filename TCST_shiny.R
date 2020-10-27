### Shiny app for TCST project ### 


## TODO: 
# Ensure formula generates correct / consistent TCST resulst 
# Fix `index_ratio` variable to scale properly 



library(shiny)
library(tidyverse)
library(readr)
library(shinythemes)
library(purrr)
# set seed 
set.seed(123)


# load and adjust data ---------------------------------------------------------

data <- read.csv('updated_master.csv')

data <- 
    data %>% # filter out two-year colleges 
    filter(ICLEVEL == 1) %>% # add freshman class size as a proxy for "league" / competition... could be other ways of doing this 
    mutate(size = cut_width(.$EFUG1ST, width = 500, boundary = 0))

select_school_list <- 
    data %>% 
    select(INSTNM) %>% 
    unique %>% arrange(INSTNM)


# set critical values -----------------------------------------------------

# warnings set at 25th percentile, alerts at 10th 


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

ratio_warning <- quantile(data$index_ratio, .2, na.rm = T)
ratio_alert <- quantile(data$index_ratio, .1, na.rm = T)

## 5. appropriation change 

appropriation_warning <- quantile(data$index_total_appropriations, .2, na.rm = T)
appropriation_alert <- quantile(data$index_total_appropriations, .1, na.rm = T)






# Define UI ---------------------------------------------------------------

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
    

# UI for Input and score --------------------------------------------------
    
    fluidRow(
        column(6,
        h3("Choose a school down here 👇  to see its Stress Test score components and score over here -->"),
        inputPanel(
            selectizeInput(inputId = "school_select", 
                        label = "Type in the name of a school:", 
                        choices = select_school_list, 
                        options = list(maxOptions = 5)
                        )
            )
    ),
    column(6, 
           h3("Score Components. These range from 0 (good) to 12 (worse)"), 
           tableOutput("score")
           )
    ),
    

# First Display output - line plots ----------------------------------------------------

    fluidRow(
        column(width = 6,
               plotOutput("enrollment_plot")
               ),
        column(width = 6,
               plotOutput("retention_plot")
               )
        ),
    
    ####### Finance 
    
    fluidRow(
        column(width = 6,
               plotOutput('market_price')
               ),
        column(width = 6,
               plotOutput("expense_appropriations")
               )
        ),
    fluidRow(
        column(6, textOutput('text'))
    ), 
    

# display 2 Comparisons and distributions  --------------------------------
    
    fluidRow(
        column(12,
               h2(textOutput("school_name"))
               )
    ),
    fluidRow(
        column(6, # plot distributions
               textOutput("distro")
                   ),
        column(6, # fun facts?
               textOutput("fun_facts")
        )
    )
    )


# Server.R ----------------------------------------------------------------


server <- function(input, output, session){
    
    library(shiny)
    library(tidyverse)
    library(readr)
    library(shinythemes)
    library(purrr)
    library(kableExtra)
    
    ### Data
    
    data <- read.csv("updated_master.csv")
    
    years <- seq(2019, 2025)
    
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
    
    ratio_warning <- quantile(data$index_ratio, .2, na.rm = T)
    ratio_alert <- quantile(data$index_ratio, .1, na.rm = T)
    
    ## 5. appropriation change 
    
    appropriation_warning <- quantile(data$index_total_appropriations, .2, na.rm = T)
    appropriation_alert <- quantile(data$index_total_appropriations, .1, na.rm = T)
    
    
    
    ### Setup 
    

# Compute sample score distribution ---------------------------------------

    # Define functions --------------------------------------------------------
    
        build_model <- function(target){
            #' Creates a linear model as part of score calculation pipeline 
            
            lm(year ~ target, data = data)
            
        }
        
        check_critical_values <- function(col, crit_one, crit_two, data = data){
            #' Calculate TSCT score for an element based on the slope of its trendline
            #' crit_one the WARNING level 
            #' crit_two the ALERT level
            #' col: the name target column. eg `index_price_change`
            
            # get vector of function evals 
            
            target <- col
            
            model <- lm(year ~ target, data = data)
            
            slope <- model$coefficients[[2]]
            intercept <- model$coefficients[[1]]
            
            years <- seq(2019, 2025)
            
            eval <- purrr::map(years, ~slope*(.x) + intercept)
            
            # forecast values and check whether any are below crit values 
            
            score <- 
                ifelse(
                    sum(eval < crit_two) >=1, 2, # if it passes the alert level, score 2  
                    ifelse( # if it passes crit_one but not crit_two, then score 1 
                        sum(eval < crit_one) >=1 & sum(eval < crit_two) ==0, 1, 0 ))
            
        }
        
        calculate_data_score <- function(target, critical_value, data = data){ # breaks 
            #' calculates the TSCT score for whether a data point falls below a critical value 
            #' target: target column 
            #' critical_value: threshold for setting score 
            
            
            score <- ifelse(sum(data$target < critical_value) >= 2, 1, 0)
        }
        
    
    # Store the distribution of scores for comparisson across schools     
    
    # public_scores <- 
    #     data %>%
    #     filter(CONTROL == 1) %>%
    #     group_by(UNITID) %>% # group by school, compute score components
    #     mutate( # calculate data scores adding columns to store results
    #         enrollment_data_score = ifelse(sum(.$index_enrollment_change < enrollment_warning) >=2, 1 , 0),
    #         retention_data_score = ifelse(sum(.$RET.PCF < retention_warning) >=2, 1 , 0),
    #         price_data_score = ifelse(sum(.$index_price_change < price_warning) >=2, 1 , 0),
    #         appropriation_data_score = ifelse(sum(.$index_total_appropriations < appropriation_warning) >=2, 1 , 0),
    #         # add trend scores 
    #     enrollment_trend_score = check_critical_values(col = index_enrollment_change, crit_one = enrollment_warning, enrollment_alert), 
    #     retention_trend_score = check_critical_values(RET.PCF, retention_warning, retention_alert), 
    #     price_trend_score = check_critical_values(index_price_change, price_warning, price_alert), 
    #     appropriation_trend_score = check_critical_values(index_total_appropriations, appropriation_warning, appropriation_alert),
    #     # add size for comparison 
    #     size = cut_width(.$EFUG1ST, width = 500, boundary = 0)
    #     ) %>% 
    #     ungroup()
    # 
    # private_scores <- 
    #     data %>%
    #     filter(CONTROL != 1) %>% # private schools 
    #     group_by(UNITID) %>% # group by school, compute score components
    #     mutate( # calculate data scores adding columns to store results
    #         enrollment_data_score = ifelse(sum(.$index_enrollment_change < enrollment_warning) >=2, 1 , 0),
    #         retention_data_score = ifelse(sum(.$RET.PCF < retention_warning) >=2, 1 , 0),
    #         price_data_score = ifelse(sum(.$index_price_change < price_warning) >=2, 1 , 0),
    #         ratio_data_score = ifelse(sum(.$index_ratio < ratio_alert) >= 2, 1, 0), 
    #         # add trend scores 
    #         enrollment_trend_score = check_critical_values(col = .$index_enrollment_change, crit_one = enrollment_warning, enrollment_alert), 
    #         retention_trend_score = check_critical_values(col = .$RET.PCF, retention_warning, retention_alert), 
    #         price_trend_score = check_critical_values(col = .$index_price_change, price_warning, price_alert), 
    #         ratio_trend_score = check_critical_values(col = .$index_ratio, ratio_warning, ratio_alert),
    #         # add size for comparison 
    #         size = cut_width(.$EFUG1ST, width = 500, boundary = 0)
    #     ) 
    

# reactive variables ------------------------------------------------------
    
    stage_data = reactiveVal(data)
    
     plot_data <- reactive({
        stage_data() %>% 
             filter(INSTNM == input$school_select)
    })
     
     ctrl <- eventReactive(plot_data(), {
         plot_data() %>%
         pull(CONTROL) %>% mean()
         })
     
     eventReactive(plot_data(), {print(plot_data())})
     

# Render outputs  ---------------------------------------------------------


# School name -------------------------------------------------------------

     output$school_name <- 
         toString(event_reactive(input$school_select, {input$school_select}))
     

# TODO Fun facts table ---------------------------------------------------------
##TODO 
     output$fun_facts <- 
         renderText({
                 str_glue("{'selected school'} is the {'quantile'} among {'n'} schools for enrollment scores Under construction")
             })
         

# debug histogram ---------------------------------------------------------
     
    
    
    output$distro <- # histogram (/ violinplot or something cooler?)
        
        # distro_data() %>% ggplot(aes(x = `SCORES`)) + # need scores 
        # geom_histogram(bins = 30) + 
        # geom_hline(plot_data() %>% select(`SCORES`))
        # xlab("Histogram")  
        

# enrollment metrics -- EFUG | RET.PCF -----------------------------------------------------
     
    output$enrollment_plot <- # `EFUG1ST`
        renderPlot(
            {
                ggplot(plot_data(), aes(x = year, y = index_enrollment_change)) + 
                    geom_point() + 
                    geom_line(lty = 3, col = 'black', lwd = 1) +
                    geom_smooth(method = 'lm', se = F, lty = 2) + 
                    theme_bw() + 
                    geom_hline(yintercept = enrollment_warning, col = 'orange') + 
                    geom_hline(yintercept = enrollment_alert, col = 'red') + 
                    ggtitle(str_glue("Indexed Change in Enrollment at {input$school_select}")) + 
                    ylab("First-year Undergraduate Enrollment Change (2012 = 100)") + 
                    xlab("Year") + 
                    theme(panel.grid.minor = element_blank()) + 
                    scale_y_continuous(expand = c(.1, .1)) +
                    annotate("text", x = 2018, y = enrollment_warning + 3.5, label = "Warning level") + 
                    annotate("text", x = 2018, y = enrollment_alert + 3.5, label = "Alert level")
                }
            )
        
    output$retention_plot <-
        renderPlot({
            ggplot(plot_data(), aes(x = year, y = RET.PCF)) + 
                geom_point() + 
                geom_line(lty = 3, col = 'gray', lwd = 1) + 
                geom_smooth(method = 'lm', se = F, lty = 2) + 
                theme_bw() + 
                theme(panel.grid.minor = element_blank()) + 
                scale_y_continuous(expand = c(.1, .1)) +
                ggtitle(str_glue("Freshman Retention Rate at {input$school_select}")) + 
                ylab("First to Second Year Retention Rate") + 
                xlab("Year") + 
                geom_hline(yintercept = retention_warning, col = 'orange') + 
                geom_hline(yintercept = retention_alert, col = 'red') + 
                annotate("text", x = 2018, y = retention_warning + .5, label = "Warning level") + 
                annotate("text", x = 2018, y = retention_alert + .5, label = "Alert level")
        })
        

# financial metrics -- market price, endowment, app -----------------------

    output$market_price <- 
        renderPlot({
            ggplot(plot_data(), aes(x = year, y = index_price_change)) + 
                geom_point() + 
                geom_line(lty = 3, col = 'gray', lwd = 1) + 
                geom_smooth(method = 'lm', se = F, lty = 2) + 
                theme_bw() + 
                theme(panel.grid.minor = element_blank()) + 
                scale_y_continuous(expand = c(.1, .1)) +
                scale_y_continuous(labels=scales::dollar_format()) +
                ggtitle(str_glue("Average Net Price at {input$school_select}")) + 
                ylab("First to Second Year Retention Rate (2012 = 100)") + 
                xlab("Year") + 
                geom_hline(yintercept = price_warning, col = 'orange') + 
                geom_hline(yintercept = price_alert, col = 'red') + 
                annotate("text", x = 2018, y = price_warning + 1.5, label = "Warning level") + 
                annotate("text", x = 2018, y = price_alert + 1.5, label = "Alert level")
        })

    output$expense_appropriations <- # try an eventReactive 
        
        renderPlot({
                
                if (ctrl() == 2) { # if CONTROL == 2 we have a PRIVATE institution -- endowment graph
                    
                        ggplot(plot_data(), aes(x = year, y = index_ratio)) + 
                            geom_point() + 
                            geom_line(lty = 2, col = 'gray', lwd = 1) + 
                            geom_smooth(method = 'lm', se = F, lty = 2) + 
                            theme_bw() +# intentionally broken 
                            theme(panel.grid.minor = element_blank()) + 
                            scale_y_continuous(expand = c(.1, .1)) +
                            ggtitle(str_glue("Average Endowment:Expense Ratio at {input$school_select}")) + 
                            ylab("Endowment:Expense Ratio (2012 = 100)") + 
                            xlab("Year") + 
                            geom_hline(yintercept = ratio_warning, col = 'orange') + 
                            geom_hline(yintercept = ratio_alert, col = 'red') + 
                            annotate("text", x = 2018, y = ratio_warning + 1.5, label = "Warning level") + 
                            annotate("text", x = 2018, y = ratio_alert + 1.5, label = "Alert level")
                    }
                
                else if (ctrl() == 1){ ## public
                    ggplot(plot_data(), aes(x = year, y = index_total_appropriations)) +
                        geom_point() +
                        geom_line(lty = 3, col = 'gray', lwd = 1) +
                        geom_smooth(method = 'lm', se = F, lty = 2) +
                        theme_bw() +
                        scale_y_continuous(expand = c(.1, .1)) +
                        theme(panel.grid.minor = element_blank()) +
                        ggtitle(str_glue("Total State and Local Appropriations at {input$school_select}")) +
                        ylab("Total Appropriations (2012 = 100)") +
                        xlab("Year") + 
                        geom_hline(yintercept = appropriation_warning, col = 'orange') + 
                        geom_hline(yintercept = appropriation_alert, col = 'red') + 
                        annotate("text", x = 2018, y = appropriation_warning + 1.5, label = "Warning level") + 
                        annotate("text", x = 2018, y = appropriation_alert + 1.5, label = "Alert level")
                }
                
            # community colleges filtered out. ELSE condition is for-profit or NaNs 
            
        })
    

# Calculate score 1. Are any of the data points below the alert or warning levels? ------------------------

    # Are any of the data points below the alert or warning levels?

    enrollment_score <- 
        eventReactive(plot_data(), {
        plot_data() %>%
                pull(index_enrollment_change) %>% 
                {ifelse(sum(. < enrollment_warning) >= 2, 1, 0)}
        })
    
    retention_data_score <-
        eventReactive(plot_data(), {
            plot_data() %>%
                pull(RET.PCF) %>%
                {ifelse(sum(. < retention_warning) >= 2, 1, 0)}
        })

    price_data_score <-
        eventReactive(plot_data(), {
            plot_data() %>%
                pull(index_price_change) %>%
                {ifelse(sum(. < price_warning) >= 2, 1, 0)}
        })

    # depending on the class of school...

    appropriations_data_score <-
        eventReactive(plot_data(), {
            plot_data() %>%
                pull(index_total_appropriations) %>%
                {ifelse(sum(. < appropriation_warning) >= 2, 1, 0)}
        })

    ratio_data_score <-
        eventReactive(plot_data(), {
            plot_data() %>%
                pull(index_ratio) %>%
                {ifelse(sum(. < ratio_warning) >= 2, 1, 0)}
        })

    
    

# Calculate score 2. Does the trendline approach the alert or warning thresholds? --------
    
    # Function didn't work for this, which led to code bloat 
    
    enrollment_crits <- eventReactive(plot_data(), {
        
        e.model <- lm(year ~ index_enrollment_change, data = plot_data())
        e.slope <- e.model$coefficients[[2]]
        e.int <- e.model$coefficients[[1]]
        e.vals <- purrr::map(years, ~ e.slope * .x + e.int)
        
        ifelse(
            sum(e.vals < enrollment_alert) >=1, 2, # if it passes the alert level, score 2  
            ifelse( # if it passes crit_one but not crit_two, then score 1 
                sum(e.vals < enrollment_warning) >=1 & sum(e.vals < enrollment_alert) ==0, 1, 0 ))
        
        
        })

    retention_crits <- eventReactive(plot_data(), {
        r.model <- lm(year ~ RET.PCF, data = plot_data())
        r.slope <- r.model$coefficients[[2]]
        r.int <- r.model$coefficients[[1]]
        r.vals <- purrr::map(years, ~ r.slope * .x + r.int)
        
        ifelse(
            sum(r.vals < retention_alert) >=1, 2, # if it passes the alert level, score 2  
            ifelse( # if it passes crit_one but not crit_two, then score 1 
                sum(r.vals < retention_warning) >=1 & sum(r.vals < retention_alert) ==0, 1, 0 ))
        
        })

    price_crits <- eventReactive(plot_data(), {
        p.model <- lm(year ~ index_price_change, data = plot_data())
        p.slope <- p.model$coefficients[[2]]
        p.int <- p.model$coefficients[[1]]
        p.vals <- purrr::map(years, ~ p.slope * .x + p.int)
        
        ifelse(
            sum(p.vals < price_alert) >=1, 2, # if it passes the alert level, score 2  
            ifelse( # if it passes crit_one but not crit_two, then score 1 
                sum(p.vals < price_warning) >=1 & sum(p.vals < price_alert) ==0, 1, 0 ))
        })


    appropriations_crits <- eventReactive(plot_data(), {
        a.model <- lm(year ~ index_total_appropriations, data = plot_data())
        a.slope <- a.model$coefficients[[2]]
        a.int <- a.model$coefficients[[1]]
        a.vals <- purrr::map(years, ~ a.slope * .x + a.int)
        
        ifelse(
            sum(a.vals < appropriation_alert) >=1, 2, # if it passes the alert level, score 2  
            ifelse( # if it passes crit_one but not crit_two, then score 1 
                sum(a.vals < appropriation_warning) >=1 & sum(a.vals < appropriation_alert) ==0, 1, 0 ))
        })


    ratio_crits <- eventReactive(plot_data(), {
        o.model <- lm(year ~ index_ratio, data = plot_data())
        o.int <- o.model$coefficients[[1]]
        o.slope <- o.model$coefficients[[2]]
        o.vals <- purrr::map(years, ~ o.slope * .x + o.int)
        
        ifelse(
            sum(o.vals < retention_alert) >=1, 2, # if it passes the alert level, score 2  
            ifelse( # if it passes crit_one but not crit_two, then score 1 
                sum(o.vals < retention_warning) >=1 & sum(o.vals < retention_alert) ==0, 1, 0 ))
        })
    


# compute scores ----------------------------------------------------------
    
    data_score <- eventReactive(plot_data(), {if (ctrl() == 1){
        price_data_score() + enrollment_score() + retention_data_score() + appropriations_data_score()
        }
        
        else {
            price_data_score() + enrollment_score() + retention_data_score() + ratio_data_score()
            }
        })
    
    trend_score <- eventReactive(plot_data(), {
        if (ctrl() == 1){
            enrollment_crits() + price_crits() + appropriations_crits() + retention_crits()
        }

        else {
            enrollment_crits() + price_crits() + ratio_crits() + retention_crits()
        }
    })
    

# Create output table -----------------------------------------------------
    
    out_table <- eventReactive(plot_data(), {
        if (ctrl() == 1){
        tibble("Metric" = c("Data score", "Trend score"), 
            "Enrollment Metric" = c(enrollment_score() + enrollment_crits()), 
            "Retention Metric" = c(retention_data_score() + retention_crits()),
            "Price Metric" = c(price_data_score() + price_crits()), 
            "Appropriations Metric" = c(appropriations_data_score() + appropriations_crits())
        )}
        
        else {
            tibble("Metric" = c("Data score", "Trend score"), 
                   "Enrollment Metric" = c(enrollment_score() + enrollment_crits()), 
                   "Retention Metric" = c(retention_data_score() + retention_crits()),
                   "Price Metric" = c(price_data_score() + price_crits()), 
                   "Appropriations Metric" = c(appropriations_data_score() + appropriations_crits())
            )}
        })
    
    
    output$score <- renderTable({
        tibble("Metric" = c("Enrollment", "Financial"),
            "Data" = as.integer(data_score()), 
               "Trend" = as.integer(trend_score()), 
               "Total (/12)" = as.integer(data_score() + trend_score()))
    })
    
    output$text <- renderText({
        cat(data_score(), ', ', trend_score(), ', ')
    })
        
        }
    


shinyApp(ui, server)
