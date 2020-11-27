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
    
    observe({
        query = parseQueryString(session$clientData$url_search)
        #print(typeof(query))
        if(length(query)>0) query = tapply(unlist(query, use.names = FALSE), rep(names(query), lengths(query)), FUN = c)
        #print(query)
        top_t = query[["top_t"]]
        n_top_1 = query[["n_top_1"]]
        top_field = query[["top_field"]]
        year = query[["year"]]
        genre = query[["genre"]]
        platform = query[["platform"]]
        
        
        if(!is.null(top_t)){
            top_t = as.character(top_t)
            updateSelectInput(session,
                              inputId = "top_t",
                              selected  = top_t)
        }
        if(!is.null(n_top_1)){
            n_top_1 = as.integer(n_top_1)
            updateSliderInput(session,
                              inputId = "n_top_1",
                              value = n_top_1)
        }
        if(!is.null(top_field)){
            top_field = as.character(top_field)
            updateSelectInput(session,
                              inputId = "top_field",
                              selected = top_field)
        }
        #print(top_t)
        if(!is.null(top_t)) {
            if(top_t=="Platform") {
                #print(year)
                if(!is.null(year)){
                    year = as.character(year)
                    updateSelectInput(session,
                                      inputId = "year_1",
                                      selected = year)
                }
                #print(genre)
                if(!is.null(genre)){
                    genre = as.character(genre)
                    updateSelectInput(session,
                                      inputId = "genre_1",
                                      selected = genre)
                }
            }
            if(top_t=="Year") {
                #print(genre)
                if(!is.null(genre)){
                    genre = as.character(genre)
                    updateSelectInput(session,
                                      inputId = "genre_2",
                                      selected = genre)
                }
                if(!is.null(platform)){
                    platform = as.character(platform)
                    updateSelectInput(session,
                                      inputId = "platform_2",
                                      selected = platform)
                }
            }
            if(top_t=="Genre") {
                if(!is.null(year)){
                    year = as.character(year)
                    updateSelectInput(session,
                                      inputId = "year_3",
                                      selected = year)
                }
                if(!is.null(platform)){
                    platform = as.character(platform)
                    updateSelectInput(session,
                                      inputId = "platform_3",
                                      selected = platform)
                }
            }
            if(top_t=="Publisher") {
                if(!is.null(year)){
                    year = as.character(year)
                    updateSelectInput(session,
                                      inputId = "year_4",
                                      selected = year)
                }
                if(!is.null(genre)){
                    genre = as.character(genre)
                    updateSelectInput(session,
                                      inputId = "genre_4",
                                      selected = genre)
                }
                if(!is.null(platform)){
                    platform = as.character(platform)
                    updateSelectInput(session,
                                      inputId = "platform_4",
                                      selected = platform)
                }
            }
            if(top_t=="Game") {
                if(!is.null(year)){
                    year = as.character(year)
                    updateSelectInput(session,
                                      inputId = "year_5",
                                      selected = year)
                }
                if(!is.null(genre)){
                    genre = as.character(genre)
                    updateSelectInput(session,
                                      inputId = "genre_5",
                                      selected = genre)
                }
                if(!is.null(platform)){
                    platform = as.character(platform)
                    updateSelectInput(session,
                                      inputId = "platform_5",
                                      selected = platform)
                }
            }
        }
        
    })
    
    observe({
        top_t = input$top_t
        n_top_1 = input$n_top_1
        top_field = input$top_field
        year_str = 'year=All&'
        genre_str = 'genre=All&'
        platform_str = 'platform=All'
        #print(top_t)
        if(top_t == 'Platform'){
            #print("Entramos Platform")
            #print(input$year_1)
            if(!is.null(input$year_1)) {
                year_str = ''
                n_years = length(input$year_1)
                for(year_i in 1:n_years) {
                    year_str = paste0(year_str,"year=",input$year_1[year_i],"&")
                }
            }
            #print(input$genre_1)
            if(!is.null(input$genre_1)) {
                genre_str = ''
                n_genre = length(input$genre_1)
                for(genre_i in 1:n_genre) {
                    genre_str = paste0(genre_str,"genre=",input$genre_1[genre_i],"&")
                }
            }
        }
        if(top_t == 'Year'){
            if(!is.null(input$platform_2)) {
                platform_str = ''
                n_platform = length(input$platform_2)
                for(platform_i in 1:n_platform) {
                    if(platform_i<n_platform) {
                        platform_str = paste0(platform_str,"platform=",input$platform_2[platform_i],"&")
                    }
                    if(platform_i==n_platform){
                        platform_str = paste0(platform_str,"platform=",input$platform_2[n_platform])
                    }
                }
            }
            if(!is.null(input$genre_2)) {
                genre_str = ''
                n_genre = length(input$genre_2)
                for(genre_i in 1:n_genre) {
                    genre_str = paste0(genre_str,"genre=",input$genre_2[genre_i],"&")
                }
            }
        }
        if(top_t == 'Genre'){
            if(!is.null(input$platform_3)) {
                platform_str = ''
                n_platform = length(input$platform_3)
                for(platform_i in 1:n_platform) {
                    if(platform_i<n_platform) {
                        platform_str = paste0(platform_str,"platform=",input$platform_3[platform_i],"&")
                    }
                    if(platform_i==n_platform){
                        platform_str = paste0(platform_str,"platform=",input$platform_3[n_platform])
                    }
                }
            }
            if(!is.null(input$year_3)) {
                year_str = ''
                n_year = length(input$year_3)
                for(year_i in 1:n_year) {
                    year_str = paste0(year_str,"year=",input$year_3[year_i],"&")
                }
            }
        }
        if(top_t == 'Publisher'){
            if(!is.null(input$platform_4)) {
                platform_str = ''
                n_platform = length(input$platform_4)
                for(platform_i in 1:(n_platform-1)) {
                    if(platform_i<n_platform) {
                        platform_str = paste0(platform_str,"platform=",input$platform_4[platform_i],"&")
                    }
                    if(platform_i==n_platform) {
                        platform_str = paste0(platform_str,"platform=",input$platform_4[n_platform])
                    }
                }
            }
            if(!is.null(input$year_4)) {
                year_str = ''
                n_year = length(input$year_4)
                for(year_i in 1:n_year) {
                    year_str = paste0(year_str,"year=",input$year_4[year_i],"&")
                }
            }
            if(!is.null(input$genre_4)) {
                genre_str = ''
                n_genre = length(input$genre_4)
                for(genre_i in 1:n_genre) {
                    genre_str = paste0(genre_str,"genre=",input$genre_4[genre_i],"&")
                }
            }
        }
        if(top_t == 'Game'){
            if(!is.null(input$platform_5)) {
                platform_str = ''
                n_platform = length(input$platform_5)
                for(platform_i in 1:(n_platform-1)) {
                    if(platform_i<n_platform) {
                        platform_str = paste0(platform_str,"platform=",input$platform_5[platform_i],"&")
                    }
                    if(platform_i==n_platform) {
                        platform_str = paste0(platform_str,"platform=",input$platform_5[n_platform])
                    }
                }
            }
            if(!is.null(input$year_5)) {
                year_str = ''
                n_year = length(input$year_5)
                for(year_i in 1:n_year) {
                    year_str = paste0(year_str,"year=",input$year_5[year_i],"&")
                }
            }
            if(!is.null(input$genre_5)) {
                genre_str = ''
                n_genre = length(input$genre_5)
                for(genre_i in 1:n_genre) {
                    genre_str = paste0(genre_str,"genre=",input$genre_5[genre_i],"&")
                }
            }
        }
        link_url = paste0("http://",session$clientData$url_hostname,":",
                          session$clientData$url_port,
                          session$clientData$url_pathname,
                          "?top_t=",top_t,"&",
                          "n_top_1=",n_top_1, "&",
                          "top_field=",top_field, "&",
                          year_str,
                          genre_str,
                          platform_str)
        updateTextInput(session,
                        "url",
                        value = link_url)
        
    })

})




