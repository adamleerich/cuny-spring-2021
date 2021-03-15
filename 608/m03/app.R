#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# rm(list = ls())


causes <- read_rds('./data/causes.Rds')
years <- read_rds('./data/years.Rds')
states <- read_rds('./data/states.Rds')
counts_state <- read_rds('./data/counts_state.Rds')
counts_us <- read_rds('./data/counts_nationwide.Rds')


causes_drop <- 
    c(sort(setdiff(causes$cause_abbr, 'Other')), 'Other')






q1_plot <- function(y, k, ord = 'Alphabetically') {
    cs <- counts_state %>% 
        filter(year == y, cause_abbr == k) %>% 
        select(state_abbr, rate)
    
    L <- order(cs$rate, decreasing = TRUE)
    
    if (ord == 'By Rate') {
        cs$state <- factor(cs$state_abbr, levels = cs$state_abbr[L])
    } else {
        cs$state <- cs$state_abbr
    }
    
    
    nw <- counts_us %>% 
        filter(year == y, cause_abbr == k) %>% 
        pull(rate)
    
    ggplot(data = cs) +
        aes(x = state, y = rate) +
        geom_col(alpha = 0.5, color = 'grey') + 
        geom_hline(aes(yintercept = nw), color = 'blue', size = 1.5) +
        labs(
            title = 'CDC Cause-of-Death Rates by State', 
            subtitle = paste0('Year: ', y, ', Cause Group: ', k),
            caption = 'Line shows nationwide rate') +
        xlab('State') + 
        ylab('Deaths per 100,000 People')
}

q2_plot <- function(s, k) {
    
    cs2 <- counts_state %>% 
        filter(state_long == s, cause_abbr == k) %>% 
        select(year, rate) %>% 
        mutate(Area = s)
    
    cn2 <- counts_us %>% 
        ungroup() %>% 
        filter(cause_abbr == k) %>% 
        select(year, rate) %>% 
        mutate(Area = 'Nationwide')
    
    ggplot(data = rbind(cs2, cn2)) + 
        aes(x = year, y = rate, color = Area, linetype = Area) + 
        geom_line(size = 1.5) +
        labs(
            title = 'CDC Cause-of-Death Rates by Year', 
            subtitle = paste0(s, ' vs. Nationwide for Cause Group: ', k)) +
        xlab('Year') + 
        ylab('Deaths per 100,000 People')
    
}

cause_lookup <- function(k) {
    row <- causes[causes$cause_abbr == k, ]
    paste0(
        'Cause of death codes in this group are from the <em>',
        row$cause_long, '</em> chapter of ICD 10.  ',
        'This includes codes ', row$cause_code, '.  ',
        'Please find more information at ', 
        '<a href="https://www.cdc.gov/nchs/data/dvs/im9_2002.pdf.pdf">',
        'https://www.cdc.gov/nchs/data/dvs/im9_2002.pdf.pdf</a>')
}




tp1 <- tabPanel(
    "Notes",
    includeMarkdown("notes.md")
)




tp2_inputs <- sidebarPanel(
    selectInput('q1_year', 'Year', years, selected = 2010),
    radioButtons('q1_cause', 'Cause of Death Category', causes_drop),
    radioButtons('q1_order', 'Sort', c('By Rate', 'Alphabetically'))
)




tp2 <- tabPanel(
    "Question #1",
    sidebarLayout(
        tp2_inputs,
        mainPanel(
            plotOutput("q1_plot"),
            htmlOutput("q1_notes"))
    )
)



tp3_inputs <- sidebarPanel(
    selectInput('q2_state', 'State', states$state_long),
    radioButtons('q2_cause', 'Cause of Death Category', causes_drop)
)




tp3 <- tabPanel(
    "Question #2",
    sidebarLayout(
        tp3_inputs,
        mainPanel(
            plotOutput("q2_plot"),
            htmlOutput("q2_notes"))
    )
)


ui <- navbarPage("Data 608 HW #3", tp1, tp2, tp3)





# Define server logic required to draw a histogram
# server <- function(input, output) {}
server <- function(input, output) {
    
    output$q1_plot <- renderPlot({
        q1_plot(input$q1_year, input$q1_cause, input$q1_order)
    })
    
    output$q1_notes <- renderText({
        cause_lookup(input$q1_cause)
    })
    
    output$q2_plot <- renderPlot({
        q2_plot(input$q2_state, input$q2_cause)
    })
    
    output$q2_notes <- renderText({
        cause_lookup(input$q2_cause)
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
