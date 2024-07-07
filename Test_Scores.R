



rm(list=ls())

setwd('C:\\Users\\miker\\Documents\\R\\Shiny\\Scores')


library(shiny)
library(bslib)
library(DT)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)

#### Download data files ######################################################

# fiel path to download files
file_path <- 'C:\\Users\\miker\\Documents\\R\\Shiny\\Scores'

# url of test scores
scores_url <- 'https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2223/'

# English Language Arts scores
ela_file <- 'pagr_schools_ela_all_2223.xlsx'
options(timeout=60)
download.file(paste(scores_url,ela_file,sep=''),ela_file,mode='wb')
ela_scores_raw <- read_excel(paste(file_path,ela_file,sep='\\'))

# Math scores
math_file <- 'pagr_schools_math_all_2223.xlsx'
download.file(paste(scores_url,math_file,sep=''),math_file,mode='wb')
math_scores_raw <- read_excel(paste(file_path,math_file,sep='\\'))


#### Clean up data ############################################################

ela_scores <- ela_scores_raw %>%
  filter(`Student Group`=='Total Population (All Students)') %>%
  select(c(`District`,`School`,`Subject`,`Percent Proficient (Level 3 or 4)`))
math_scores <- math_scores_raw %>%
  filter(`Student Group`=='Total Population (All Students)') %>%
  select(c(`District`,`School`,`Subject`,`Percent Proficient (Level 3 or 4)`))

all_scores_long <- ela_scores %>%
  rbind(math_scores)
colnames(all_scores_long) <- c('District','School','Subject','Percent Proficient')

all_scores <- all_scores_long %>%
  pivot_wider(names_from = Subject,values_from = `Percent Proficient`)

all_scores <- all_scores %>%
  mutate(`English Language Arts`=round(as.numeric(`English Language Arts`),digits=0),
         Mathematics=round(as.numeric(Mathematics),digits=0)) %>%
  filter(grepl("Elementary",School)) %>%
  mutate(School=str_replace(School,'Elementary School',''),
         District=str_replace(District," SD.+",''))


#### build shiny dashboard ####################################################

ui <- fluidPage(
  tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}"))),
  # dropdown for school district
  selectInput('district',
              label='District', 
              choices = unique(all_scores$District),
              multiple=TRUE,
              selected='Beaverton'),
  tags$div("Data Source:",tags$a(href="https://www.oregon.gov/ode/educator-resources/assessment/Pages/Assessment-Group-Reports.aspx"," OR Dept of Ed")),
  # four panels- table, ELA plot, math plot, and ELA & math plot
  navbarPage(title = NULL,
             tabPanel("Table",
                      dataTableOutput('table')),
             tabPanel("Chart-ELA",
                      plotlyOutput('ela_plot')),
             tabPanel("Chart-Math",
                      plotlyOutput('math_plot')),
             tabPanel("Chart-Both",
                      plotlyOutput('both_plot'))
  )
)

server <- function(input, output, session) {
  
  selected_scores <- reactive({
    all_scores %>%
      filter(District %in% input$district) %>%
      select(District,School,`English Language Arts`, `Mathematics`)
  })
  # table of scores going on first panel
  output$table <- DT::renderDT({
    selected_scores()
  },
  caption = "Percent Proficient, School Year 2022-23 (All Grades)",
  options=list(pageLength=-1,
               lengthMenu = list(c(10,25,-1),
                                 c('10','25','All'))),
  rownames=FALSE,
  server=FALSE) 
  
  # ELA plot
  output$ela_plot <- renderPlotly({
    
    ela_order <- all_scores %>%
      filter(District %in% input$district) %>%
      arrange(`English Language Arts`) %>%
      select(School) %>%
      array()
    # get mean, low, and high score for plot annotation
    mean_ela <- mean(all_scores$`English Language Arts`[!is.na(all_scores$`English Language Arts`) & all_scores$District %in% input$district])
    low_ela <- all_scores[all_scores$District %in% input$district,]$School[which.min(all_scores$`English Language Arts`[all_scores$District %in% input$district])]
    high_ela <- all_scores[all_scores$District %in% input$district,]$School[which.max(all_scores$`English Language Arts`[all_scores$District %in% input$district])]
    # bar plot of ela scores
    all_scores %>%
      filter(District %in% input$district) %>%
      plot_ly(x=~`English Language Arts`,
              y= ~School,
              color=~District,
              type='bar',
              orientation='h') %>%
      layout(title='Percent Proficient, School year 2022-23 (All Grades)',
              height=700,
             #annotations= list(x=70,y=5,text='Raw data sourced from OR Dept of Ed',showarrow=F),
             xaxis=list(title='Percent Proficient'),
             yaxis=list(title='',tickmode='linear',categoryorder='array',categoryarray=array(unlist(ela_order)))) %>%
      add_segments(x=mean_ela,xend=mean_ela,y=high_ela,yend=low_ela,color=I('black'),showlegend=FALSE)
  })
  
  
  # maths cores plot
  output$math_plot <- renderPlotly({
    
    math_order <- all_scores %>%
      filter(District %in% input$district) %>%
      arrange(Mathematics) %>%
      select(School) %>%
      array()
    # get mean, low, and high math scores for plot annotatin
    mean_math <- mean(all_scores$Mathematics[!is.na(all_scores$Mathematics) & all_scores$District %in% input$district])
    low_math <- all_scores[all_scores$District %in% input$district,]$School[which.min(all_scores$Mathematics[all_scores$District %in% input$district])]
    high_math <- all_scores[all_scores$District %in% input$district,]$School[which.max(all_scores$Mathematics[all_scores$District %in% input$district])]
    # bar plot of math scores
    all_scores %>%
      filter(District %in% input$district) %>%
      plot_ly(x=~Mathematics,
              y= ~School,
              color=~District,
              type='bar',
              orientation='h') %>%
      layout(title='Percent Proficient, School year 2022-23 (All Grades)',
              height=700,
             #annotations= list(x=70,y=5,text='Raw data sourced from OR Dept of Ed',showarrow=F),
             xaxis=list(title='Percent Proficient'),
             yaxis=list(title='',tickmode='linear',categoryorder='array',categoryarray=array(unlist(math_order)))) %>%
      add_segments(x=mean_math,xend=mean_math,y=high_math,yend=low_math,color=I('black'),showlegend=FALSE)
  })
  # scatter plot of ELA and math with labels
  output$both_plot <- renderPlotly({
    
    all_scores %>%
      filter(District %in% input$district) %>%
      plot_ly(x=~Mathematics,
              y= ~`English Language Arts`,
              text= ~School,
              color=~District) %>%
      #add_markers() %>%
      add_text() %>%
      layout(title='ELA and Math Percent Proficient, School year 2022-23 (All Grades)',
             #annotations= list(x=70,y=30,text='Raw data sourced from OR Dept of Ed',showarrow=F),
              height=700)
  })
  
  
}

shinyApp(ui, server)





