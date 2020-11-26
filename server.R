#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DT)
library(dplyr)

df = read.csv('vgsales.csv',stringsAsFactors = F)
colnames(df)[which(colnames(df)=='Name')] = 'Game'

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    observeEvent(input$top_t,{
        updateTabsetPanel(session,
                          inputId = 'params_1',
                          selected = input$top_t)
    })
    
    
    tab_sample = reactive({
        #print("Entramos tab_sample")
        filter_year_1 = input$year_1
        #print('year 1')
        filter_year_3 = input$year_3
        #print('year 2')
        filter_year_4 = input$year_4
        #print('year 3')
        filter_year_5 = input$year_5
        #print('year 4')
        if(!is.null(input$year_1)) {
        if(input$year_1 == "All"){filter_year_1 = c(df %>% select(Year)  %>% distinct() %>% arrange(Year))[[1]]}}
        #print('year 5')
        if(!is.null(input$year_3)) {
        if(input$year_3 == "All"){filter_year_3 = c(df %>% select(Year)  %>% distinct() %>% arrange(Year))[[1]]}}
        #print('year 6')
        if(!is.null(input$year_4)) {
        if(input$year_4 == "All"){filter_year_4 = c(df %>% select(Year)  %>% distinct() %>% arrange(Year))[[1]]}}
        #print('year 7')
        if(!is.null(input$year_5)) {
        if(input$year_5 == "All"){filter_year_5 = c(df %>% select(Year)  %>% distinct() %>% arrange(Year))[[1]]}}
        #print('Pasamos year')
        filter_genre_1 = input$genre_1
        #print('Genre 1')
        filter_genre_2 = input$genre_2
        #print('Genre 2')
        filter_genre_4 = input$genre_4
        #print('Genre 3')
        filter_genre_5 = input$genre_5
        #print('Genre 4')
        if(!is.null(input$genre_1)) {
        if(input$genre_1 == "All"){filter_genre_1 = c(df %>% select(Genre)  %>% distinct() %>% arrange(Genre))[[1]]}}
        #print('Genre 5')
        if(!is.null(input$genre_2)) {
        if(input$genre_2 == "All"){filter_genre_2 = c(df %>% select(Genre)  %>% distinct() %>% arrange(Genre))[[1]]}}
        #print('Genre 6')
        if(!is.null(input$genre_4)) {
        if(input$genre_4 == "All"){filter_genre_4 = c(df %>% select(Genre)  %>% distinct() %>% arrange(Genre))[[1]]}}
        #print('Genre 7')
        if(!is.null(input$genre_5)) {
        if(input$genre_5 == "All"){filter_genre_5 = c(df %>% select(Genre)  %>% distinct() %>% arrange(Genre))[[1]]}}
        #print(filter_genre_1)
        #print('Pasamos Genre)
        filter_plat_2 = input$platform_2
        filter_plat_3 = input$platform_3
        filter_plat_4 = input$platform_4
        filter_plat_5 = input$platform_5
        if(!is.null(input$platform_2)) {
            if(input$platform_2 == "All"){filter_plat_2 = c(df %>% select(Platform)  %>% distinct() %>% arrange(Platform))[[1]]}}
        if(!is.null(input$platform_3)) {
            if(input$platform_3 == "All"){filter_plat_3 = c(df %>% select(Platform)  %>% distinct() %>% arrange(Platform))[[1]]}}
        if(!is.null(input$platform_4)) {
            if(input$platform_4 == "All"){filter_plat_4 = c(df %>% select(Platform)  %>% distinct() %>% arrange(Platform))[[1]]}}
        if(!is.null(input$platform_5)) {
            if(input$platform_5 == "All"){filter_plat_5 = c(df %>% select(Platform)  %>% distinct() %>% arrange(Platform))[[1]]}}
             
        switch(input$top_t,
               'Platform' = df %>% filter(Year %in% filter_year_1 & Genre %in% filter_genre_1) %>% group_by(Platform) %>% summarise(NA_Sales = sum(NA_Sales),EU_Sales=sum(EU_Sales),JP_Sales=sum(JP_Sales),Other_Sales=sum(Other_Sales),Global_Sales=sum(Global_Sales),Amount = n()),
               'Year' = df  %>% filter(Genre %in% filter_genre_2 & Platform %in% filter_plat_2) %>% group_by(Year) %>% summarise(NA_Sales = sum(NA_Sales),EU_Sales=sum(EU_Sales),JP_Sales=sum(JP_Sales),Other_Sales=sum(Other_Sales),Global_Sales=sum(Global_Sales),Amount = n()),
               'Genre' = df %>% filter(Year %in% filter_year_3 & Platform %in% filter_plat_3) %>% group_by(Genre) %>% summarise(NA_Sales = sum(NA_Sales),EU_Sales=sum(EU_Sales),JP_Sales=sum(JP_Sales),Other_Sales=sum(Other_Sales),Global_Sales=sum(Global_Sales),Amount = n()),
               'Publisher' = df %>% filter(Year %in% filter_year_4 & Genre %in% filter_genre_4 & Platform %in% filter_plat_4) %>% group_by(Publisher) %>% summarise(NA_Sales = sum(NA_Sales),EU_Sales=sum(EU_Sales),JP_Sales=sum(JP_Sales),Other_Sales=sum(Other_Sales),Global_Sales=sum(Global_Sales),Amount = n()),
               'Game' = df %>% filter(Year %in% filter_year_5 & Genre %in% filter_genre_5 & Platform %in% filter_plat_5) %>% group_by(Game) %>% summarise(NA_Sales = sum(NA_Sales),EU_Sales=sum(EU_Sales),JP_Sales=sum(JP_Sales),Other_Sales=sum(Other_Sales),Global_Sales=sum(Global_Sales),Amount = n())
        )
        #print('finaliza switch')
    })

    output$topTab_1 <- renderDataTable({
        #print('Llegamos 1')
        piv_year_1 <<-input$year_1
        n_top_1 = input$n_top_1
        #print('Llegamos 2')
        new_df = tab_sample()
        #print('Llegamos 3')
        #print(input$top_field)
        aaa1 <- input$top_field
        aaa2 <- input$top_t
        kk_tp1<-enquo(aaa1)
        kk_tp2<-enquo(aaa2)
        la_new_df <-new_df %>% select(!!kk_tp2,!!kk_tp1)%>% arrange(!!kk_tp1) %>% top_n(n_top_1)
        #print('Llegamos 4')
        validate(need(nrow(la_new_df)>0, 'Please choose other configurations'))
        la_new_df
    })

})


#Test2 <- function(df,kk) {
#    kk<-enquo(kk)
#    xx1 <- group_by_at(mtcars,vars(mpg,cyl,!!kk)) %>% summarise(FreqOG = length(cyl))
#    xx1 <- data.frame(xx1)}

#yy1 <- Test2(mtcars,aaa1)


#kk_tp<-enquo(aaa1)
#df %>% arrange(!!kk_tp) %>% head()
