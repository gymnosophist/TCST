### Shiny app for TCST project ###


## TODO:
# Fix functions to calculate scores
# Ensure formula generates correct / consistent TCST results
# Fix `index_ratio` variable to scale properly



library(shiny)
library(tidyverse)
library(readr)
library(shinythemes)
library(purrr)

# set seed
set.seed(123)


# load and adjust data ---------------------------------------------------------

data <- # filter out two-year colleges
    read.csv('tcst_data_updated.csv') %>%
    filter(level == 1) %>%
    filter(control == 1 | control == 2) %>%
    # add freshman class size as a proxy for "league" / competition... could be other ways of doing this
    mutate(size = cut_interval(freshman_enrollment, 5))

data <-
    data %>%
    group_by(id) %>% # index price and appropriations
    mutate(index_price = (`avg_net_price` / first(`avg_net_price`) - 1 * 100) +  100) %>%
    mutate(index_price = index_price * 100)  %>%
    mutate(index_appropriations = (total_appropriations / first(total_appropriations) - 1 * 100) + 100) %>%
    mutate(index_appropriations = index_appropriations * 100)

select_school_list <-
    data %>% group_by(id) %>%
    arrange(name) %>%
    pull(name) %>%
    unique

# set critical values -----------------------------------------------------

# warnings set at 25th percentile, alerts at 10th


## 1 - First-year UG enrollment

# TCST groups by control, and calculates the mean change over time.
# We'll have values of 100 overvalued, so we should drop the FIRST
# of each group

critical_values <-
    data %>% group_by(id) %>%
    slice(2:n()) %>% # remove index rows
    ungroup() %>%
    group_by(control) %>%
    summarise(
        enrollment_warning = quantile(index_enrollment_change, .2),
        enrollment_alert = quantile(index_enrollment_change, .1),
        # do retention separately -- we need the index rows for that
        price_warning = quantile(index_price, .2, na.rm = T),
        # several NaNs to remove
        price_alert = quantile(index_price, .1, na.rm = T),
        ratio_warning = quantile(index_ratio, .2, na.rm = T),
        ratio_alert = quantile(index_ratio, .1, na.rm = T),
        appropriation_warning = quantile(index_appropriations, .2, na.rm = T),
        appropriation_alert = quantile(index_appropriations, .1, na.rm = T)
    )

critical_values

## 2. Retention rate -- these are already scaled

retention_critical_values <-
    data %>%
    group_by(control) %>%
    summarise(
        retention_warning = quantile(retention, .20),
        retention_alert = quantile(retention, .1)
    )

critical_values <-
    critical_values %>% cbind(retention_critical_values)

# Define UI ---------------------------------------------------------------

ui <-
    fluidPage(
        theme = shinytheme("journal"),
        
        ######### Title page
        
        titlePanel("Exploring The College Stress Test with IPEDS Data"),
        
        ####### Spacer
        
        fluidRow(
            column(width = 3,
                   style = "background-color:#bd0000;",
                   h1(' ')),
            column(width = 9,
                   style = 'background-color:#D3D3D3;',
                   h1(' '))
        ),
        
        
        # UI for Input and score --------------------------------------------------
        
        ####### Input school_select
        
        fluidRow(column(
            6,
            h3(
                "Choose a school down here 👇  to see its Stress Test score components and score over here -->"
            ),
            inputPanel(
                selectizeInput(
                    inputId = "school_select",
                    label = "Type in the name of a school:",
                    choices = select_school_list,
                    options = list(maxOptions = 5)
                )
            )
        ),
        
        ####### Score output
        
        column(
            6,
            h3(
                "Here's the score breakdown for your school.  These range from 0 (good) to 12 (worse)"
            ),
            tableOutput("score")
        )),
        
        
        # First Display output - line plots ----------------------------------------------------
        
        fluidRow(column(width = 6,
                        plotOutput("enrollment_plot")),
                 column(width = 6,
                        plotOutput("retention_plot"))),
        
        ####### Finance
        
        fluidRow(column(width = 6,
                        plotOutput('market_price')),
                 column(
                     width = 6,
                     plotOutput("expense_appropriations")
                 )),
        
        
        # display 2 Comparisons and distributions  --------------------------------
        
        ####### Spacer
        
        fluidRow(
            column(3,
                   style = "background-color:#bd0000;",
                   h2(" ")),
            column(9,
                   style = "background-color:#D3D3D3; opacity: .75",
                   h2(' '))
        ),
        
        ####### Distribution output
        
        fluidRow(column(6, # plot distributions
                        plotOutput("distro")),
                 column(6, # fun facts?
                        plotOutput("lollipop"))),
        fluidRow(column(12, 
                        style = "background-color:#D3D3D3", 
                        h2("Thanks for reading")))
    )



# Server.R ----------------------------------------------------------------


server <- function(input, output, session) {
    library(shiny)
    library(tidyverse)
    library(readr)
    library(shinythemes)
    library(purrr)
    library(kableExtra)
    library(ggridges)
    
    
    # Load data ---------------------------------------------------------------
    
    
    data <- # filter out two-year colleges
        read.csv('tcst_data_updated.csv') %>%
        filter(level == 1) %>%
        filter(control == 1 | control == 2) %>%
        # add freshman class size as a proxy for "league" / competition... could be other ways of doing this
        mutate(size = cut_interval(freshman_enrollment, 5)) %>% 
        mutate(size_bucket = as.integer(as.factor(size)))
    
    data <-
        data %>%
        group_by(id) %>% # index price and appropriations
        mutate(index_price = (`avg_net_price` / first(`avg_net_price`) - 1 * 100) +  100) %>%
        mutate(index_price = index_price * 100)  %>%
        mutate(index_appropriations = (total_appropriations / first(total_appropriations) - 1 * 100) + 100) %>%
        mutate(index_appropriations = index_appropriations * 100)
    
    select_school_list <-
        data %>% group_by(id) %>%
        arrange(name) %>%
        pull(name) %>%
        unique
    
    public_scores <- # drop_na
        read_csv('total_public_scores_2.csv') %>% drop_na() %>% 
        mutate(
            total = enrollment_data_score + 
                retention_data_score + 
                price_data_score + 
                appropriation_data_score + 
                enrollment_trend + 
                price_trend + 
                retention_trend + 
                appropriations_trend
        )
    
    
    private_scores <- # drop na
        read_csv('total_private_scores_2.csv')  %>% drop_na() %>%
        mutate(
            total = enrollment_data_score + 
                retention_data_score + 
                price_data_score + 
                ratio_data_score + 
                enrollment_trend + 
                price_trend + 
                retention_trend + 
                ratio_trend
        )
    
    
    all_scores_total <-
        rbind(public_scores[, c('id', 'total')], private_scores[, c("id", 'total')])
    
    data <-
        data %>%
        left_join(all_scores_total,
                  by = 'id')
    
    
    
    # Define critical values --------------------------------------------------
    
    # warnings set at 25th percentile, alerts at 10th
    
    ## 1 - First-year UG enrollment
    
    # TCST groups by control, and calculates the mean change over time.
    # We'll have values of 100 overvalued, so we should drop the FIRST
    # of each group
    
    critical_values <-
        data %>% group_by(id) %>%
        slice(2:n()) %>% # remove index rows
        filter(index_ratio > 0 |
                   index_appropriations > 0) %>%
        ungroup() %>%
        group_by(control) %>%
        summarise(
            enrollment_warning = quantile(index_enrollment_change, .2),
            enrollment_alert = quantile(index_enrollment_change, .1),
            # do retention separately -- we need the index rows for that
            price_warning = quantile(index_price, .2, na.rm = T),
            # several NaNs to remove
            price_alert = quantile(index_price, .1, na.rm = T),
            ratio_warning = quantile(index_ratio, .2, na.rm = T),
            ratio_alert = quantile(index_ratio, .1, na.rm = T),
            appropriation_warning = quantile(index_appropriations, .2, na.rm = T),
            appropriation_alert = quantile(index_appropriations, .1, na.rm = T)
        )
    
    
    
    ## 2. Retention rate -- these are already scaled
    
    retention_critical_values <-
        data %>%
        group_by(control) %>%
        summarise(
            retention_warning = quantile(retention, .20),
            retention_alert = quantile(retention, .1)
        )
    
    critical_values <-
        critical_values %>% cbind(retention_critical_values)
    
    
    
    # reactive variables ------------------------------------------------------
    
    stage_data = reactiveVal(data)
    
    reactive_private_scores <- reactiveVal(private_scores)
    
    reactive_public_scores <- reactiveVal(public_scores)
    
    plot_data <- reactive({
        stage_data() %>%
            filter(name == input$school_select)
    })
    
    selected_school_id <- eventReactive(plot_data(), {
        plot_data() %>%
            pull(id) %>% .[[1]]
    })
    
    ctrl <- eventReactive(plot_data(), {
        plot_data() %>%
            pull(control) %>% mean()
    })
    
    score_table <-
        eventReactive(plot_data(), {
            if (ctrl() == 1) {
                public_scores %>% filter(id == selected_school_id())
            }
            
            else {
                private_scores %>% filter(id == selected_school_id())
            }
            
        })
    
    size_select <- 
        eventReactive(plot_data(), {
            plot_data() %>% pull(size_bucket) %>% .[[1]]
        })
    
    
    avg_score_by_size <-
        eventReactive(plot_data(), {
            stage_data() %>%
                filter(size_bucket == size_select(),
                       control == ctrl()) %>%
                left_join(all_scores_total, by = 'id') %>%
                pull(total.x) %>%
                as.numeric() %>%
                mean(., na.rm = T)
        })
        
    
    
    
    # Render outputs  ---------------------------------------------------------
    
    # score table ---------------------------------------------------------
    
    
    output$score <-  
        eventReactive(plot_data(), {
            if (ctrl() == 1) {
                tibble(
                    "Enrollment score" = (
                        score_table()$enrollment_data_score + score_table()$enrollment_trend
                    ),
                    "Retention score" = (
                        score_table()$retention_data_score + score_table()$retention_trend
                    ),
                    "Net price score" = (
                        score_table()$price_data_score + score_table()$price_trend
                    ),
                    "State appropriations score" = (
                        score_table()$appropriation_data_score + score_table()$appropriations_trend
                    ), 
                    "Total" = score_table()$total
                ) %>% knitr::kable(align = rep('c', 5)) %>% column_spec(5, bold = T, border_left = T) %>% kable_styling() 
                
            }
            else {
                tibble(
                    "Enrollment score" = (
                        score_table()$enrollment_data_score + score_table()$enrollment_trend
                    ),
                    "Retention score" = (
                        score_table()$retention_data_score + score_table()$retention_trend
                    ),
                    "Net price score" = (
                        score_table()$price_data_score + score_table()$price_trend
                    ),
                    "Expense ratio score" = (
                        score_table()$ratio_data_score + score_table()$ratio_trend
                    ), 
                    "Total" = (
                        score_table()$total
                    )
                ) %>% knitr::kable(align = rep('c', 5)) %>% column_spec(5, bold = T, border_left = T) %>% kable_styling() 
                
            }
        })
    
    # Distribution ---------------------------------------------------------
    
    ## Distribution done -- would it be better as a scatterplot? 
    
    # munge data
    distro_data <- 
        eventReactive(
            plot_data(), {
                stage_data() %>% 
                    filter(size_bucket == size_select()) %>% 
                    left_join(all_scores_total, by = 'id')
            }
        )
    
    scatter_data <- 
        eventReactive(ctrl(),{
            if (ctrl() == 1){
                stage_data() %>% 
                    filter(size_bucket == size_select(), 
                           control == ctrl()) %>% 
                    left_join(public_scores, by = 'id') %>% 
                    mutate(
                        x = enrollment_data_score + retention_data_score + enrollment_trend + retention_trend, 
                        y = price_trend + price_data_score + appropriations_data_score + appropriations_trend
                    )
            }
            else {
                stage_data() %>% 
                    filter(size_bucket == size_select(), 
                           control == ctrl()) %>% 
                    left_join(private_scores, by = 'id') %>% 
                    mutate(
                        x = enrollment_data_score + retention_data_score + enrollment_trend + retention_trend, 
                        y = price_trend + price_data_score + ratio_data_score + ratio_trend
                    )
            }
            })
        
    
    output$distro <- 
        renderPlot({
            distro_data() %>%
                mutate(fill_col = ifelse(total.x < plot_data()$total[[1]], 1, 0)) %>%
                
                ggplot(x = total.x, ) +
                geom_density(aes(x = total.x, fill = '#69b3a2', alpha = .8)) +
                scale_fill_manual(values = '#69b3a2') + 
                geom_vline(xintercept = plot_data()$total[[1]],
                           lty = 1,
                           alpha = .8) + # school marker
                xlab("Stress Test Score") +
                annotate('text',
                         x = plot_data()$total[[1]] + 1.75,
                         y = .29,
                         label = "Your school's score") +
                geom_segment( # arrow to school's score 
                    x = plot_data()$total[[1]] + 1,
                    xend = plot_data()$total[[1]] + .1,
                    y = .3,
                    yend = .325,
                    arrow = arrow(length = unit(.5, 'cm'))
                ) +
                geom_vline(
                    xintercept = avg_score_by_size(),
                    # average for the segment
                    lty = 2,
                    color = '#d17171',
                    lwd = .5
                ) +
                geom_segment( # arrow to average 
                    x = avg_score_by_size() + 1, 
                    xend = avg_score_by_size() + .1, 
                    y = .255, 
                    yend = .275, 
                    color = '#d17171',
                    lty = 1, 
                    lwd = .5,
                    arrow = arrow(length = unit(.5, 'cm'))
                ) + 
                annotate('text',
                         x = avg_score_by_size() + 1.75,
                         y = .25,
                         label = 'Average score for schools of this size') +
                ylab("Percentage of Schools") +
                ggtitle("Score distribution among similar schools") +
                theme_bw() +
                theme(legend.position = "none", 
                      panel.grid = element_blank())  + 
                scale_x_continuous(breaks = seq(0,12, by = 1))
        })
    
    # Lollipop ----------------------------------------------------------------
    
    ## TODO
    
    # prepare data for lollipop chart 
    
    # 1. Get average scores for each component 
    
    score_averages <- eventReactive(plot_data(), { # ugly. 
        if (ctrl() == 1) {
            stage_data() %>%
                filter(size_bucket == size_select()) %>%
                select(id) %>%
                left_join(public_scores, by = 'id') %>%
                group_by(id) %>%
                summarise(
                    mean_enrollment_score = mean(.$enrollment_data_score + .$enrollment_trend, na.rm = T),
                    mean_retention_score = mean(.$retention_data_score + .$retention_trend, na.rm = T),
                    mean_price_score = mean(.$price_data_score + .$price_trend, na.rm = T),
                    mean_appropriation_score = mean(.$appropriation_data_score + .$appropriations_trend, na.rm = T)
                ) %>%
                slice(1) %>%
                select(2:ncol(.)) %>% t()
        }
        
        else {
            stage_data() %>%
                filter(size_bucket == size_select()) %>%
                select(id) %>%
                left_join(private_scores, by = 'id') %>%
                group_by(id) %>%
                summarise(
                    mean_enrollment_score = mean(.$enrollment_data_score + .$enrollment_trend, na.rm = T),
                    mean_retention_score = mean(.$retention_data_score + .$retention_trend, na.rm = T),
                    mean_price_score = mean(.$price_data_score + .$price_trend, na.rm = T),
                    mean_ratio_score = mean(.$ratio_data_score + .$ratio_trend, na.rm = T),
                ) %>%
                slice(1) %>%
                select(2:ncol(.)) %>% 
                t()
        }
    })
    
    summed_score_table <- # this works 
        eventReactive(score_averages(), {
            if (ctrl() == 1) {
                score_table() %>% transmute(
                    enrollment = enrollment_data_score + enrollment_trend,
                    retention = retention_data_score + retention_trend,
                    price = price_data_score + price_trend,
                    apppropriations = appropriation_data_score + appropriations_trend
                )
            }
            else {
                score_table() %>%
                    transmute(
                        enrollment = enrollment_data_score + enrollment_trend,
                        retention = retention_data_score + retention_trend,
                        price = price_data_score + price_trend,
                        ratio = ratio_data_score + ratio_trend
                    )
            }
            
        })
    
    output$spy <- 
        NULL
    
    # 2. create data.frame 
    
    lollipop_data <-
        eventReactive(score_averages(), {
            if (ctrl() == 1) {
                tibble(
                    "category" = c("Enrollment", 'Retention', 'Price', 'Appropriations'),
                    'score_means' = score_averages(),
                    'school_scores' = summed_score_table() %>% t()
                )
            }
            else{
                tibble(
                    'category' = c(
                        "Enrollment",
                        'Retention',
                        'Price',
                        'Endowment:Expense Ratio'
                    ),
                    'score_means' = score_averages(),
                    'school_scores' = summed_score_table() %>% t()
                    )
            }
        })
                
    output$lollipop <-
        renderPlot({
            # a la https://www.data-to-viz.com/graph/lollipop.html
            ggplot(data = lollipop_data()) +
                geom_segment(
                    aes(
                        x = category,
                        xend = category,
                        y = school_scores,
                        yend = score_means
                    )
                ) +
                geom_point(data = lollipop_data(), aes(y = score_means, x = category, color = 'Average school of this size'), color = '#d17171', alpha = .8, size = 3) +
                geom_point(data = lollipop_data(), aes(y = school_scores, x = category, color = 'Your school'), color = '#69b3a2', alpha = .8, size = 3) + 
                theme_bw() +
                coord_flip() + 
                labs(title = 'Under construction', 
                     subtitle = "How does your school compare to others on each metric?")
        })
    
    # enrollment metrics -----------------------------------------------------
    
    
    output$enrollment_plot <- # `EFUG1ST`
        renderPlot({
            ggplot(plot_data(), aes(x = year, y = index_enrollment_change)) +
                geom_point() +
                geom_line(lty = 3,
                          col = 'black',
                          lwd = 1) +
                geom_smooth(method = 'lm',
                            se = F,
                            lty = 2
                ) +
                theme_bw() +
                geom_hline(yintercept = critical_values[ctrl(), 'enrollment_warning'], col = 'orange') +
                geom_hline(yintercept = critical_values[ctrl(), 'enrollment_alert'], col = 'red') +
                ggtitle(str_glue("Indexed Change in Enrollment at {input$school_select}")) +
                ylab("First-year Undergraduate Enrollment Change (2011 = 100)") +
                xlab("Year") +
                theme(panel.grid.minor = element_blank()) +
                scale_y_continuous(expand = c(.1, .1)) +
                annotate("text",
                         x = 2017,
                         y = critical_values[ctrl(), 'enrollment_warning'] + 3.5,
                         label = "Warning level") +
                annotate("text",
                         x = 2017,
                         y = critical_values[ctrl(), 'enrollment_alert'] + 3.5,
                         label = "Alert level")
        })
    
    output$retention_plot <-
        renderPlot({
            ggplot(plot_data(), aes(x = year, y = retention)) +
                geom_point() +
                geom_line(lty = 3,
                          col = 'black',
                          lwd = 1) +
                geom_smooth(method = 'lm',
                            se = F,
                            lty = 2) +
                theme_bw() +
                theme(panel.grid.minor = element_blank()) +
                scale_y_continuous(expand = c(.1, .1)) +
                ggtitle(str_glue("Freshman Retention Rate at {input$school_select}")) +
                ylab("First to Second Year Retention Rate") +
                xlab("Year") +
                geom_hline(yintercept = critical_values[ctrl(), 'retention_warning'], col = 'orange') +
                geom_hline(yintercept = critical_values[ctrl(), 'retention_alert'], col = 'red') +
                annotate("text",
                         x = 2017,
                         y = critical_values[ctrl(), 'retention_warning'] + .75,
                         label = "Warning level") +
                annotate("text",
                         x = 2017,
                         y = critical_values[ctrl(), 'retention_alert'] + .75,
                         label = "Alert level")
        })
    
    
    # financial metrics -- market price, endowment, app -----------------------
    
    output$market_price <-
        renderPlot({
            ggplot(plot_data(), aes(x = year, y = index_price)) +
                geom_point() +
                geom_line(lty = 3,
                          col = 'black',
                          lwd = 1) +
                geom_smooth(method = 'lm',
                            se = F,
                            lty = 2) +
                theme_bw() +
                theme(panel.grid.minor = element_blank()) +
                scale_y_continuous(expand = c(.1, .1)) +
                scale_y_continuous(labels = scales::dollar_format()) +
                ggtitle(str_glue("Average Net Price at {input$school_select}")) +
                ylab("Average Net Price (2011 = 100)") +
                xlab("Year") +
                geom_hline(yintercept = critical_values[ctrl(), 'price_warning'], col = 'orange') +
                geom_hline(yintercept = critical_values[ctrl(), 'price_alert'], col = 'red') +
                annotate("text",
                         x = 2017,
                         y = critical_values[ctrl(), 'price_warning'] + 1,
                         label = "Warning level") +
                annotate("text",
                         x = 2017,
                         y = critical_values[ctrl(), 'price_alert'] + 1,
                         label = "Alert level")
        })
    
    output$expense_appropriations <-
        
        renderPlot({
            if (ctrl() == 2) {
                # if control == 2 we have a PRIVATE institution -- endowment graph
                
                ggplot(plot_data(), aes(x = year, y = index_ratio)) +
                    geom_point() +
                    geom_line(lty = 2,
                              col = 'black',
                              lwd = 1) +
                    geom_smooth(method = 'lm',
                                se = F,
                                lty = 2) +
                    theme_bw() + # intentionally broken
                    theme(panel.grid.minor = element_blank()) +
                    scale_y_continuous(expand = c(.1, .1)) +
                    ggtitle(str_glue(
                        "Average Endowment:Expense Ratio at {input$school_select}"
                    )) +
                    ylab("Endowment:Expense Ratio (2011 = 100)") +
                    xlab("Year") +
                    geom_hline(yintercept = critical_values[2, 'ratio_warning'], col = 'orange') +
                    geom_hline(yintercept = critical_values[2, 'ratio_alert'], col = 'red') +
                    annotate("text",
                             x = 2017,
                             y = critical_values[2, 'ratio_warning'] + 1.5,
                             label = "Warning level") +
                    annotate("text",
                             x = 2017,
                             y = critical_values[2, 'ratio_alert'] + 1.5,
                             label = "Alert level")
            }
            
            else if (ctrl() == 1) {
                ## public
                ggplot(plot_data(),
                       aes(x = year, y = index_appropriations)) +
                    geom_point() +
                    geom_line(lty = 3,
                              col = 'black',
                              lwd = 1) +
                    geom_smooth(
                        method = 'lm',
                        se = F,
                        lty = 2,
                        col = '#D3D3D3'
                    ) +
                    theme_bw() +
                    scale_y_continuous(expand = c(.1, .1)) +
                    theme(panel.grid.minor = element_blank()) +
                    ggtitle(
                        str_glue(
                            "Total State and Local Appropriations at {input$school_select}"
                        )
                    ) +
                    ylab("Total Appropriations (2011 = 100)") +
                    xlab("Year") +
                    geom_hline(yintercept = critical_values[1, 'appropriation_warning'], col = 'orange') +
                    geom_hline(yintercept = critical_values[1, 'appropriation_alert'], col = 'red') +
                    annotate("text",
                             x = 2017,
                             y = critical_values[1, 'appropriation_warning'] + 1.5,
                             label = "Warning level") +
                    annotate("text",
                             x = 2017,
                             y = critical_values[1, 'appropriation_alert'] + 1.5,
                             label = "Alert level")
            }
            
            # community colleges filtered out. ELSE condition is for-profit or NaNs
            
        })
    
    
    
}



shinyApp(ui, server)
