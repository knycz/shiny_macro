#available on https://nomicten.shinyapps.io/WIZN/
library("shiny")
library("tidyr")
library("dplyr")
library("XML")
library("ggplot2")
library("grid")
library("plotly")
library("devtools")
library("WDI")
library("usethis")


### List of countries ###
WDI::WDI(extra = T) %>% filter(region != "NA") %>% select(iso2c, country) -> countries #NA instead of Aggregates
countries <- as.data.frame(cbind(unique(countries$iso2c), unique(countries$country))) 
regions <- list("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean",
                "Middle East & North Africa", "North America", "South Asia", "Sub-Saharan Africa")

### DATA ### 

gdp_growth_an <- WDI(indicator ="NY.GDP.MKTP.KD.ZG", extra = T) 
gdp_growth_an_world <- subset(gdp_growth_an, country == "World")
gdp_growth_an_world %>% select(NY.GDP.MKTP.KD.ZG, year) -> gdp_growth_an_world
gdp_growth_an <- subset(gdp_growth_an, region != "NA")
gdp_growth_pc <- WDI(indicator = "5.51.01.10.gdp", extra = T)
gdp_growth_pc <- subset(gdp_growth_pc, region != "NA")
gdp_level_2011 <- WDI(indicator = "NY.GDP.MKTP.KD", extra = T)
gdp_level_2010_world <- subset(gdp_level_2011, country == "World")
gdp_level_2011 <- subset(gdp_level_2011, region != "NA")
gdp_level_2011 %>% rename(gdp_lvl2010const = NY.GDP.MKTP.KD) %>% arrange(country, year) -> gdp_level_2011
gdp_level_2011 %>% group_by(country) %>% mutate(change=(gdp_lvl2010const-lag(gdp_lvl2010const,1))/lag(gdp_lvl2010const,1)*100) %>% as.data.frame -> gdp_level_2011
gdp_level_2010_world %>% rename(gdp_lvl2010const = NY.GDP.MKTP.KD) %>% arrange(country, year) -> gdp_level_2010_world
gdp_level_2010_world %>% group_by(country) %>% mutate(change=(gdp_lvl2010const-lag(gdp_lvl2010const,1))/lag(gdp_lvl2010const,1)*100) %>% as.data.frame -> gdp_level_2010_world

#Define UI for app that draws a histogram ----
ui <- navbarPage("Macro in a nutshell",
                 tabPanel("Global growth",
                          
                          fluidPage(
                            titlePanel("World Development Indicators"),
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput(inputId = "bins",
                                            label = h3("Group countries into subgroups"),
                                            min = 1,
                                            max = 215,
                                            value = 30),
                                sliderInput(inputId = "selectedyear", label = h3("Select year"), 
                                            min = min(gdp_growth_an$year)+1,
                                            max = max(gdp_growth_an$year)-1,
                                            value = max(gdp_growth_an$year)-1, step = 1),
                                sliderInput("growthrange", label = h3("Range of GDP growth"), 
                                            min = round(min(gdp_growth_an$NY.GDP.MKTP.KD.ZG, na.rm = T),-1),
                                            max = round(max(gdp_growth_an$NY.GDP.MKTP.KD.ZG, na.rm = T),-1), 
                                            value = c(-20, 20))
                              ),
                              
                              mainPanel(
                                plotOutput(outputId = "distPlot")
                              )
                            )
                          )
                 ),
                 tabPanel("Country profile",
                          
                          fluidPage(
                            titlePanel("Demographics"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("country", label = h3("Select country"), 
                                            choices = countries[,2], 
                                            selected = "Poland"),
                                
                                sliderInput(inputId = "year", label = h3("Select year"), 
                                            min = 1960,
                                            max = 2018,
                                            value = 2018, step = 1, animate = T,
                                            animationOptions(interval = 150, 
                                                             loop = FALSE, 
                                                             playButton = "play",
                                                             pauseButton = "pause"))
                                
                              ),
                              
                              mainPanel(
                                plotlyOutput(outputId = "population_structure"),
                                plotlyOutput(outputId = "population_total"),
                                plotlyOutput(outputId = "TFR")
                              )
                            )
                          )
                 ),
                 tabPanel("International comparison", 
                          
                          fluidPage(
                            titlePanel("Comparison"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("indicator100", label = h3("Select indicator"), 
                                            choices = list( 
                                              # "Per capita GDP growth" =	"5.51.01.10.gdp"	,
                                              #                    "GDP (current $)" =	"6.0.GDP_current"	,
                                              #                    "GDP growth (annual %)"	=	"6.0.GDP_growth"	,
                                              #                    "GDP (constant 2005 $)" 	=	"6.0.GDP_usd"	,
                                              #                    "GDP per capita, PPP (constant 2011 international $) "                    	=	"6.0.GDPpc_constant"	,
                                              #                    "Trade in services (% of GDP)"                                            	=	"BG.GSR.NFSV.GD.ZS"	,
                                              #                    "Gross private capital flows (% of GDP, PPP)"                             	=	"BG.KAC.FNEI.GD.PP.ZS"	,
                                              #                    "Gross private capital flows (% of GDP)"                                  	=	"BG.KAC.FNEI.GD.ZS"	,
                                              #                    "Gross foreign direct investment (% of GDP, PPP)"                         	=	"BG.KLT.DINV.GD.PP.ZS"	,
                                              #                    "Gross foreign direct investment (% of GDP)"                              	=	"BG.KLT.DINV.GD.ZS"	,
                                              #                    "Wage bill as a percentage of GDP"                                        	=	"BI.WAG.TOTL.GD.ZS"	,
                                              #                    "Merchandise imports (BOP): percentage of GDP (%)"                        	=	"BM.GSR.MRCH.ZS"	,
                                              #                    "Foreign direct investment, net outflows (% of GDP)"                      	=	"BM.KLT.DINV.GD.ZS"	,
                                              #                    "Foreign direct investment, net outflows (% of GDP)"                      	=	"BM.KLT.DINV.WD.GD.ZS"	,
                                              #                    "Current account balance (% of GDP)"                                      	=	"BN.CAB.XOKA.GD.ZS"	,
                                              #                    "Current account balance (% of GDP)"                                      	=	"BN.CAB.XOKA.GDP.ZS"	,
                                              #                    "Curr. acc. bal. before official transf. (% of GDP)"                      	=	"BN.CAB.XOTR.ZS"	,
                                              #                    "Current account balance excluding net official capital grants (% of GDP)"	=	"BN.CUR.GDPM.ZS"	,
                                              #                    "Net income (% of GDP)"                                                   	=	"BN.GSR.FCTY.CD.ZS"	,
                                              #                    "Foreign direct investment (% of GDP)" 	=	"BN.KLT.DINV.CD.ZS"	,
                                                                "GDP per capita, PPP (constant 2011 international $)" = "NY.GDP.PCAP.PP.KD",
                                                                "Urban population (% of total)" = "SP.URB.TOTL.IN.ZS"), 
                                            selected =  "NY.GDP.PCAP.PP.KD"
                                ),
                                sliderInput(inputId = "year1", label = h3("Select year"), 
                                            min = 1990,
                                            max = 2018,
                                            value = 2018, step = 1, animate = F
                                ),
                                sliderInput("slider2", label = h3("Position in the ranking"), min = 1, 
                                            max = 215, value = c(1, 10)
                                )
                                # ,
                                # checkboxGroupInput("region", label = h3("Choose area"), 
                                #                    choices = regions)
                              ),
                              
                              mainPanel(
                                plotlyOutput(outputId = "international_comparison")
                              )
                            )
                          )
                          
                 )
                 
)


server <- function(input, output) {
  ### reactive expressions ###
  country_code <- reactive({
    countries %>% filter(V2 == input$country) -> tmp
    as.character(tmp[,1])
  })
  
  population_data <- reactive({
    piramida_ma <- c("SP.POP.0004.MA","SP.POP.0509.MA","SP.POP.1014.MA","SP.POP.1519.MA",
                     "SP.POP.2024.MA","SP.POP.2529.MA","SP.POP.3034.MA","SP.POP.3539.MA",
                     "SP.POP.4044.MA","SP.POP.4549.MA","SP.POP.5054.MA","SP.POP.5559.MA",
                     "SP.POP.6064.MA","SP.POP.6569.MA","SP.POP.7074.MA","SP.POP.7579.MA",
                     "SP.POP.80UP.MA")
    
    datatmp_ma <- WDI(indicator = piramida_ma,
                      country = country_code(), extra = T)
    
    datatmp_ma$id <- rownames(datatmp_ma)
    datatmp_ma %>% select(id, country, year, SP.POP.0004.MA:SP.POP.80UP.MA) -> datatmp_ma
    datatmp_ma_narrow <- gather(datatmp_ma, age, population, SP.POP.0004.MA:SP.POP.80UP.MA)
    datatmp_ma_narrow %>% mutate(gender = "Male") -> datatmp_ma_narrow
    datatmp_ma_narrow$age <- paste(substr(datatmp_ma_narrow$age,8,9), "-",
                                   substr(datatmp_ma_narrow$age,10,11))
    datatmp_ma_narrow$population <- -datatmp_ma_narrow$population
    
    piramida_fe <- c("SP.POP.0004.FE","SP.POP.0509.FE","SP.POP.1014.FE","SP.POP.1519.FE",
                     "SP.POP.2024.FE","SP.POP.2529.FE","SP.POP.3034.FE","SP.POP.3539.FE",
                     "SP.POP.4044.FE","SP.POP.4549.FE","SP.POP.5054.FE","SP.POP.5559.FE",
                     "SP.POP.6064.FE","SP.POP.6569.FE","SP.POP.7074.FE","SP.POP.7579.FE",
                     "SP.POP.80UP.FE")
    
    datatmp_fe <- WDI(indicator = piramida_fe,
                      country = country_code(), extra = T)
    datatmp_fe$id <- rownames(datatmp_fe)
    datatmp_fe %>% select(id, country, year, SP.POP.0004.FE:SP.POP.80UP.FE) -> datatmp_fe
    datatmp_fe_narrow <- gather(datatmp_fe, age, population, SP.POP.0004.FE:SP.POP.80UP.FE)
    datatmp_fe_narrow %>% mutate(gender = "Female") -> datatmp_fe_narrow
    datatmp_fe_narrow$age <- paste(substr(datatmp_fe_narrow$age,8,9), "-",
                                   substr(datatmp_fe_narrow$age,10,11))
    
    datatmp <- rbind(datatmp_ma_narrow, datatmp_fe_narrow)
    return(datatmp)
  })
  
  TFR_data <- reactive({
    WDI(indicator = "SP.DYN.TFRT.IN", country = country_code(), extra = T) %>% rename(TFR = SP.DYN.TFRT.IN) -> datatmp
    return(datatmp)
  })
  ###
  range <- reactive({
    return(seq(input$slider2[1], input$slider2[2]))
  })
  
  data_indicator <- reactive({
    WDI(indicator = as.character(input$indicator100), extra = T) -> temp
    return(temp)
  })
  
  year_indicator <- reactive({
    data_indicator() %>% filter(year == input$year1) -> temp
    return(temp)
  })
  
  region_indicator <- reactive({
    if(is.null(input$region)){
      year_indicator()  %>% filter(region != "NA")  %>% arrange(-.[,3]) -> temp
    } else {
      year_indicator()  %>% filter(region != "NA") %>% filter(region == input$region) %>% arrange(-.[,3]) -> temp
    }
    return(temp)
  })
  
  #############################################
  output$distPlot <- renderPlot({
    
    x <- gdp_growth_an %>% filter(year == input$selectedyear) %>% select(NY.GDP.MKTP.KD.ZG)
    x$positive <- (x$NY.GDP.MKTP.KD.ZG >= 0)
    bins <- seq(min(x$NY.GDP.MKTP.KD.ZG, na.rm = T), max(x$NY.GDP.MKTP.KD.ZG, na.rm = T), length.out = input$bins + 1)
    
    p1 <- ggplot(x, aes(x=NY.GDP.MKTP.KD.ZG, fill = positive)) + 
      geom_histogram(bins = input$bins + 1, colour = "white") +
      geom_vline(xintercept = gdp_growth_an_world %>% filter(year == input$selectedyear) %>% select(NY.GDP.MKTP.KD.ZG) %>% as.numeric -> temp, linetype = "dashed") +
      geom_text(aes(x=temp, label=paste("\nworld avg: ", round(temp,1), "%"), y = 0, hjust=0), colour="black", angle=90) +
      theme_minimal() + 
      scale_x_continuous(name = "GDP growth", limits = c(input$growthrange[1], input$growthrange[2]))+
      ggtitle("Distribution of GDP growth - number of countries") + 
      theme(plot.title = element_text(hjust = 0.5)) 
    
    x2 <- gdp_level_2011 %>% filter(year == input$selectedyear) %>% select(change, gdp_lvl2010const)
    x2$positive <- (x2$change >= 0)
    
    p2 <- ggplot(x2, aes(x=change, weight=gdp_lvl2010const/10^12, fill = positive)) + 
      geom_histogram(bins= input$bins + 1, colour = "white") +  theme_minimal() +
      geom_vline(xintercept = gdp_growth_an_world %>% filter(year == input$selectedyear) %>% select(NY.GDP.MKTP.KD.ZG) %>% as.numeric, linetype = "dashed")+
      geom_text(aes(x=temp, label=paste("\nworld avg: ", round(temp,1), "%"), y = 0, hjust=0), colour="black", angle=90) +
      scale_x_continuous(name = "GDP growth", limits = c(input$growthrange[1], input$growthrange[2]))+
      scale_y_continuous(name = "GDP in trillion US dollars, 2010 prices")+
      ggtitle("Distribution of GDP growth - size of GDP") +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    grid.newpage()
    grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
    
    
  })
  
  ##### 
  
  output$population_total <- renderPlotly({
    population_data() %>% mutate(population = abs(population)) %>% group_by(year) %>% summarise(population_total = sum(population)) -> temp
    ggplot(temp, aes(x=year, y=population_total)) +
      geom_line() + theme_classic() + geom_vline(xintercept = input$year, linetype = "dashed") +
      # geom_text(aes(x = input$year+0.5, 
      #               label = paste0(temp %>% filter(year == input$year) %>% select(population_total) %>% as.numeric %>% round(-5)/10^6, "m"), 
      #               y = temp %>% filter(year == input$year) %>% select(population_total) %>% as.numeric()*1.08, hjust=0), colour="black", angle=90) +
      scale_y_continuous(limits = c(0, 1.1*max(temp$population_total, na.rm = T)), name = "Population in total",  
                         labels = f <- function(x){
                           return(paste0(abs(x)/10^6, "m"))
                         }) +
      ggtitle(paste("Population of", input$country, "in total"))  +
      theme(plot.title = element_text(hjust = 0.5)) -> p3
    p3 <- ggplotly(p3)
    p3
  })
  
  output$population_structure <- renderPlotly({
    validate(
      need(!(population_data() %>% filter(year == input$year) %>% select(population) %>% anyNA), 'Data not complete')
    )
    pyramid <- ggplot(population_data() %>% filter(year == input$year) -> tmp, aes(x = age, y = population, fill = gender)) + 
      geom_bar(data = tmp1 <- subset(tmp, gender == "Female") , stat = "identity") +
      geom_bar(data = tmp2 <- subset(tmp, gender == "Male") , stat = "identity") + 
      scale_y_continuous(limits = 1.05*c(-max(max(-population_data()$population, na.rm=T), max(population_data()$population, na.rm = T)),
                                         max(max(-population_data()$population, na.rm=T), max(population_data()$population, na.rm = T))),
                         labels = f <- function(x){
                           return(paste0(abs(x)/10^6, "m"))
                         },
                         name = "number of people")+
      theme_classic() + ggtitle(paste("The age structure of the population:", input$country))  +
      theme(plot.title = element_text(hjust = 0.5))  + coord_flip()
    pyramid <- ggplotly(pyramid)
    pyramid
  })
  
  output$TFR <- renderPlotly({
    ggplot(TFR_data() -> temp, aes(x=year, y=TFR)) +
      geom_line() + theme_classic() + geom_vline(xintercept = input$year, linetype = "dashed") +
      geom_hline(yintercept = 2.1, linetype = "dashed", colour = "red") + 
      # geom_text(aes(x = input$year+0.5, 
      #               label = temp %>% filter(year == input$year) %>% select(TFR) %>% as.numeric, 
      #               y = temp %>% filter(year == input$year) %>% select(TFR) %>% as.numeric()*1.5, hjust=0), colour="black", angle=90) +
      scale_y_continuous(limits = c(0, 1.1*max(temp$TFR, na.rm = T))) +
      ggtitle(paste("Total fertility rate of", input$country)) +
      theme(plot.title = element_text(hjust = 0.5)) -> tmp
    tmp <- ggplotly(tmp)
    tmp
  })
  
  output$international_comparison <- renderPlotly({
    temp <- region_indicator()[range(),]
    countries_tmp <- temp[,1:2]
    rownames(countries_tmp) <- countries_tmp[,1]
    ggplot(temp , aes(x = reorder(iso2c, -temp[,3]), y = temp[,3],#)#)+
      text = paste('country: ', countries_tmp[as.character(temp$iso2c),2],
                    '<br>value:', round(temp[,3])))) +
      geom_bar(stat = "identity", aes(fill = "darkorange3")) + theme_classic() +
      scale_y_continuous(name = tmp <- (WDIsearch(tmp <- colnames(temp)[3], field = "indicator") -> temp2) %>% is.vector %>% ifelse(., return(t(temp2)), return(temp2)) %>% as.data.frame  %>% filter(indicator == tmp) %>% select(name) %>% .[1,] %>% as.character) +
      scale_x_discrete(name = "country code") +
      ggtitle("Comparison of countries") + 
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, size = 8, vjust =.5), axis.text.y = element_text(angle = 0, size = 8, vjust =.5), legend.position = "none") +
      guides(fill=FALSE) -> tmp
    tmp <- ggplotly(tmp, tooltip = c("text"))
    tmp
  })
  
}

shinyApp(ui = ui, server = server)



