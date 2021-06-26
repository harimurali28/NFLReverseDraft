setwd("~/Documents/Documents/R Files/NFL stuffs/Draft Analysis/Reverse Draft Script")

### Loading required packages

library(RCurl)
library(xml2)
library(XML)
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(DT)
library(plotly)
library(teamcolors)
#library(rsconnect) Only needed to deploy to shiny


### Bringing in team colors for graphs
teamcolorsnfl <- teamcolors %>% filter(league == "nfl")

teamabrv <- c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN",
              "DET","GB","HOU","IND","JAX","KC","LAC","LAR","MIA","MIN","NE","NO","NYG","NYJ",
              "LV","PHI","PIT","SF","SEA","TB","TEN","WFT")


teamcolorsnfl<- cbind(teamcolorsnfl,teamabrv)
teamcolorsnfl <- teamcolorsnfl %>% select(teamabrv,primary,secondary,logo)

## Draft Scraper from ProFootball Reference 

draftscrape = function(yr) {
  url = paste0("https://www.pro-football-reference.com/years/",yr,"/draft.htm")
  draft_data = getURL(url)
  draft_data = readHTMLTable(draft_data,stringsAsFactors = F)[[1]]
}

### This scraper allows you to scrape multiple years of drafts
scrape_mult_draft = function(startyr,endyr){
  master=data.frame()
  for (i in startyr:endyr) {
    draft = draftscrape(i)
    draft$yr = i
    master %<>% 
      bind_rows(draft)
  }
  master <- master[,c(1:28,30)]
  numeric_columns <- 
    master %>%
    select(-c(Tm, Player, Pos, `College/Univ`)) %>% 
    names 
  master[numeric_columns] <-
    sapply(master[numeric_columns],as.numeric)
  master <- master %>% filter(Tm != "Tm")
  master$DrAV <- ifelse(is.na(master$DrAV),0,master$DrAV)
  master$CarAV <- ifelse(is.na(master$CarAV),0,master$CarAV)
  return(master)
}

### Test pull of 2015 and 2016 drafts
#draft2015 <- scrape_mult_draft(2015,2016)


# Toppicks <- draft2015 %>% dplyr::filter(yr =="2016") %>% 
#   select(Pick,Tm,Player,Pos,CarAV) %>% mutate(rank = rank(desc(CarAV)))
# 
# PHIpicks <- draft2015 %>% dplyr::filter(yr =="2016" & Tm == "PHI") %>% 
#   dplyr::select(Pick,Tm,Player,Pos,CarAV) 

# Creating a Not in Function
`%!in%` = Negate(`%in%`)


### This function helps find the player that should be picked in a perfect draft
Playerfinder <- function(PickNo,PlayerNo,playlag,Toppicks){
  Playerperson <- Toppicks %>% filter(Pick >= PickNo & Player %!in% playlag) %>% arrange(rank) %>% select(Player,CarAV,Pos,Pick)
  Playerperson <- Playerperson %>% slice(PlayerNo)
  playeradd %<>%
    bind_rows(Playerperson)
  return(Playerperson)
}


# PHIreverse <- PHIpicks %>% arrange(desc(Pick))
# teampicks <- PHIreverse %>% select(Pick)

### Creating Empty dataframes for Draft Value Function
playeradd <- data.frame()
master <- data.frame()

## This function pulls draft results and then builds out a data frame with every perfect pick for every draft and team
DraftValueFunction <- function(startyear,endyear){
  final <- data.frame()
  draftbig <- scrape_mult_draft(startyear,endyear)
  draftyr <- draftbig %>% select(yr) %>% unique()
  for(j in draftyr$yr){
  draft <- draftbig %>% filter(yr == j)  %>% dplyr::select(Pick,Tm,Player,Pos,CarAV) 
  Toppicksloop <- draft %>% select(Pick,Tm,Player,Pos,CarAV) %>% mutate(rank = rank(desc(CarAV)))
  teams <- draft %>% select(Tm) %>% unique()
  for(f in teams$Tm){
  teamreverse <- draft %>% filter(Tm == f) %>% arrange(desc(Pick))
  playeradd <- data.frame()
  master <- data.frame()
for(i in teamreverse$Pick){
  pickedplayers <- playeradd$Player
  name <- Playerfinder(PickNo = i,PlayerNo = 1,playlag = pickedplayers,Toppicks = Toppicksloop)
  justnames <- name %>% select(Player)
  playeradd %<>%
    bind_rows(justnames)
  master %<>%
    bind_rows(name)}
teamreverse <- teamreverse %>% mutate(DraftReplacement = master$Player,
                      RepCarAV = master$CarAV,
                      RepPos = master$Pos,
                      RepPick = master$Pick,
                      Year = j)
final %<>%
  bind_rows(teamreverse)}}
return(final)}                                    


# DraftReverse2015 <- DraftValueFunction(year = 2015)
# 
# DraftReverse2015 %>% group_by(DraftReplacement) %>% summarize(count = n()) %>% arrange(desc(count))
# teamvalue2015 <- DraftReverse2015 %>% group_by(Tm) %>% summarize(DraftAV = sum(CarAV),
#                                                                  RepAv = sum(RepCarAV),
#                                                                  Value = round(sum(CarAV)/sum(RepCarAV),3)) %>% arrange(desc(Value))


# draftreverse1617 <- DraftValueFunction(2016,2017)
# 
# teamvalue1617<-draftreverse1617 %>% group_by(Tm) %>% summarize(DraftAV = sum(CarAV),
#                            RepAv = sum(RepCarAV),
#                            Value = round(sum(CarAV)/sum(RepCarAV),3)) %>% arrange(desc(Value))


### Pulling drafts from 2005 - 2018 can pull more if wanted
draftreverse0518 <- DraftValueFunction(2005,2018)

### Creating df for year by year team values with similar team names for joins 
teamvalue0518<-draftreverse0518 %>% mutate(Tm = case_when(Tm == "STL" ~ "LAR",
                                                                         Tm == "TAM" ~ "TB",
                                                                         Tm == "WAS" ~ "WFT",
                                                                         Tm == "SFO" ~ "SF",
                                                                         Tm == "KAN" ~ "KC",
                                                                         Tm == "SDG" ~ "LAC",
                                                                         Tm == "GNB" ~ "GB",
                                                                         Tm == "NWE" ~ "NE",
                                                                         Tm == "NOR" ~ "NO",
                                                                         Tm == "OAK" ~ "LV",
                                                                         Tm == "LVR" ~ "LV",
                                                                         Tm == "DEN" ~ "DEN",
                                                                         TRUE ~ as.character(Tm))) %>% 
  group_by(Tm,Year) %>% summarize(DraftAV = sum(CarAV),
                                                               RepAv = sum(RepCarAV),
                                                               Value = round(sum(CarAV)/sum(RepCarAV),3),
                             
                             TotalPicks = n()) %>% arrange(desc(Value))




draftreverse0518 <- draftreverse0518 %>%  mutate(Tm = case_when(Tm == "STL" ~ "LAR",
                                                                Tm == "TAM" ~ "TB",
                                                                Tm == "WAS" ~ "WFT",
                                                                Tm == "SFO" ~ "SF",
                                                                Tm == "KAN" ~ "KC",
                                                                Tm == "SDG" ~ "LAC",
                                                                Tm == "GNB" ~ "GB",
                                                                Tm == "NWE" ~ "NE",
                                                                Tm == "NOR" ~ "NO",
                                                                Tm == "OAK" ~ "LV",
                                                                Tm == "LVR" ~ "LV",
                                                                Tm == "DEN" ~ "DEN",
                                                                TRUE ~ as.character(Tm)))


#teamvalue0518 %>% dplyr::filter(Tm == "PHI") %>% ggplot(aes(x = Year,y = Value)) + geom_line() + geom_point()
## Creates the plotly line graoh
valueplot <- function(Team){
trace0 <- teamvalue0518 %>% 
  filter(Tm == Team) %>% left_join(teamcolorsnfl, by = c("Tm" = "teamabrv")) %>%  select(Year,Value,primary,secondary,logo,TotalPicks) %>%  mutate(Value = round(Value *100),1)
trace1 <- teamvalue0518 %>% group_by(Year) %>% summarize(Value2 = round(mean(Value)*100,1),HighValue = max(Value)) %>% select(Year,Value2,HighValue)

plotdata <- trace0 %>% left_join(trace1,by = "Year") %>% arrange(Year)

p1 <- plot_ly(plotdata,x= ~Year, y = ~Value, name = 'Team Value',
              type = 'scatter',mode = "lines+markers",line = list(color = ~primary),marker = list(color = ~secondary),
              text = ~TotalPicks,
              hovertemplate = paste("%{xaxis.title.text}: %{x}<br>",
                                    "%{yaxis.title.text}: %{y}<br>",
                                    "Total Picks: %{text}")) %>%
  add_trace(y = ~Value2,name ='League Avg',mode = "lines",line = list(color = ~secondary),
            text = ~HighValue,
            hovertemplate = paste("%{xaxis.title.text}: %{x}<br>",
                                 "League Average: %{y}<br>",
                                 "Highest League Value: %{text:.1%}"))
p1 <-  p1 %>% layout(title = paste0(Team," Reverse Draft Value by Year"),yaxis = list(ticksuffix = "%",range = c(0, 75)))
p1
}


### Shiny Setup 
library(shiny)

ui <- fluidPage(
  selectInput("teams","Team Select",choices = sort(unique(draftreverse0518$Tm))),
  selectInput("year","Year Select",choices = unique(draftreverse0518$Year)),
  DTOutput("picks"),
  plotlyOutput("valuegraph")
)

server <- function(input, output, session) {
  teams <- reactive({
    filter(draftreverse0518, Tm == input$teams)
  })
  output$picks <- renderDT({
    DT::datatable(teams() %>% 
      filter(Year == input$year) %>% 
      arrange(Pick),options = list(class = 'cell-border stripe',
                                   pageLength = 25,
                                   columnDefs = list(list(className = 'dt-center',targets =0:10))))
  })
  
  output$valuegraph <- renderPlotly({valueplot(Team = input$teams)
    })
}


shinyApp(ui, server)





