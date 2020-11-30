#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

df = read.csv('vgsales.csv',stringsAsFactors = F)
colnames(df)[which(colnames(df)=='Name')] = 'Game'

shinyUI(fluidPage(
    
    titlePanel("Video Game Sales"
               ),
    h5("Jose Alberto Ligorria Taracena"),
    h5("Juan Pablo Carranza Hurtado"),
    
    tabsetPanel(
        tabPanel("Tabular view",
            sidebarLayout(
                sidebarPanel(
                    
                    selectInput('top_t',
                                'Filter by:',
                                choices = c('Platform','Year','Genre',"Publisher","Game")),
                    sliderInput("n_top_1",
                                "Number of tops:",
                                min = 1,
                                max = 10,
                                value = 5),
                    selectInput('top_field',
                                'Top by:',
                                choices = c('NA_Sales','EU_Sales','JP_Sales',"Other_Sales","Global_Sales","Amount"),
                                selected = 'NA_Sales'),
                    textAreaInput("url",
                              "Link",
                              value = ""),
                    tabsetPanel(id='params_1',
                                type = 'hidden',
                                tabPanel('Platform',
                                         selectInput('year_1',
                                                     'Year',
                                                     choices = c("All",df %>% select(Year)  %>% distinct() %>% arrange(Year)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         ),
                                         selectInput('genre_1',
                                                     'Genre',
                                                     choices = c("All",df %>% select(Genre)  %>% distinct() %>% arrange(Genre)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         )
                                ),
                                tabPanel('Year',
                                         selectInput('genre_2',
                                                     'Genre',
                                                     choices = c("All",df %>% select(Genre)  %>% distinct() %>% arrange(Genre)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         ),
                                         selectInput('platform_2',
                                                     'Platform',
                                                     choices = c("All",df %>% select(Platform)  %>% distinct() %>% arrange(Platform)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         )
                                ),
                                tabPanel('Genre',
                                         selectInput('year_3',
                                                     'Year',
                                                     choices = c("All",df %>% select(Year)  %>% distinct() %>% arrange(Year)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         ),
                                         selectInput('platform_3',
                                                     'Platform',
                                                     choices = c('All',df %>% select(Platform)  %>% distinct() %>% arrange(Platform)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         )
                                ),
                                tabPanel('Publisher',
                                         selectInput('year_4',
                                                     'Year',
                                                     choices = c("All",df %>% select(Year)  %>% distinct() %>% arrange(Year)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         ),
                                         selectInput('platform_4',
                                                     'Platform',
                                                     choices = c("All",df %>% select(Platform)  %>% distinct() %>% arrange(Platform)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         ),
                                         selectInput('genre_4',
                                                     'Genre',
                                                     choices = c("All",df %>% select(Genre)  %>% distinct() %>% arrange(Genre)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         )
                                ),
                                tabPanel('Game',
                                         selectInput('year_5',
                                                     'Year',
                                                     choices = c("All",df %>% select(Year)  %>% distinct() %>% arrange(Year)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         ),
                                         selectInput('platform_5',
                                                     'Platform',
                                                     choices = c("All",df %>% select(Platform)  %>% distinct() %>% arrange(Platform)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         ),
                                         selectInput('genre_5',
                                                     'Genre',
                                                     choices = c("All",df %>% select(Genre)  %>% distinct() %>% arrange(Genre)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         )
                                )
                    )
                ),
                mainPanel(
                    dataTableOutput("topTab_1"),
                    plotOutput("graph_1")
                )
                
                
            )

        ),
        tabPanel("Time series",
            sidebarLayout(
                sidebarPanel(
                    selectInput('top_t_tab2',
                                'Filter by:',
                                choices = c('Platform','Genre',"Publisher","Game")),
                    selectInput('top_field_tab2',
                                'Top by:',
                                choices = c('NA_Sales','EU_Sales','JP_Sales',"Other_Sales","Global_Sales","Amount"),
                                selected = 'NA_Sales'),
                    tabsetPanel(id='params_2',
                                type = 'hidden',
                                tabPanel('Platform',
                                         selectInput('genre_1_tab2',
                                                     'Genre',
                                                     choices = c("All",df %>% select(Genre)  %>% distinct() %>% arrange(Genre)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         )
                                ),
                                tabPanel('Genre',
                                         selectInput('platform_1_tab2',
                                                     'Platform',
                                                     choices = c('All',df %>% select(Platform)  %>% distinct() %>% arrange(Platform)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         )
                                ),
                                tabPanel('Publisher',
                                         selectInput('platform_2_tab2',
                                                     'Platform',
                                                     choices = c("All",df %>% select(Platform)  %>% distinct() %>% arrange(Platform)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         ),
                                         selectInput('genre_2_tab2',
                                                     'Genre',
                                                     choices = c("All",df %>% select(Genre)  %>% distinct() %>% arrange(Genre)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         )
                                ),
                                tabPanel('Game',
                                         selectInput('platform_3_tab2',
                                                     'Platform',
                                                     choices = c("All",df %>% select(Platform)  %>% distinct() %>% arrange(Platform)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         ),
                                         selectInput('genre_3_tab2',
                                                     'Genre',
                                                     choices = c("All",df %>% select(Genre)  %>% distinct() %>% arrange(Genre)),
                                                     multiple = TRUE,
                                                     selected = "All"
                                         )
                                )
                    )
                ),
                mainPanel(
                    h4("Video game sales through time"),
                    plotOutput("graph_2",
                               click = 'clk',
                               brush = 'mbrush'),
                    dataTableOutput("DT_tabla1")
                    
                )
            )
        )
    ))
)
