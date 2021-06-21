# 01-kmeans-app

load( "maindata.RData")
library(dplyr )
library(ggplot2)
library(cowplot)

library(shiny)
library(shinyWidgets)
library(foreach)
library(iterators)

library(plyr)
library(scales)
library(psych)
library(data.table)
library(gridExtra )




ui <- fluidPage(

   mainPanel(
     navbarPage("LifeSim",
                
                 tabPanel("Individual Outcomes", 
                          # headerPanel('Estimated lifetime outcomes for individuals similar to you'),
                          
                          sidebarPanel(
                            selectInput('varc', 'What was your biological sex at birth?', choices=c("Female"=0, "Male"=1), selected = 1 
                            ) ,
                               # selectInput('varc2', 'How would you rank your cognitive skills at age 7 relative to peers?', choices=c("In the lowest quintile"=1, "In the 2nd lowest quintile"=2,   "Around the average"=3, "In the 2nd highest quintile"=4, "In the top quintile"=5), selected = 5 
                          # ),
                          selectInput('varc3', 'Did you graduate with a university degree?', choices=c("Yes"=1, "No"=0), selected =1
                          ),
                              checkboxGroupInput('vard', 'Did any of the following apply to you during early childhood?' , 
                                                           choices= c("Born in poverty"="depr_b","Parental depression"="mhealth_p2","No parental university degree"="noedu_p",
                                                                      "High conduct problems at age 5"="highcp5"  ),    inline = F ) ,
                          checkboxGroupInput('vard1', 'Did any of the following apply to you during teenage years?' , 
                                                             choices= c( "Conduct disorder at age 18"="cd18",
                                                                        "Regular smoking at age 14"="smokes14", "Regular smoking at age 19"="unh_beh19"  ),   inline = F ),    inline = T )  
                          
                        ,
                     
                          
                          mainPanel(
                            plotOutput('plot0'),
                           p(  em("Note: The grey bars show the estimated lifetime outcomes for individuals with the selected characteristics. The red lines mark the population average values (adjusted by sex)."),align="left" )  )) ,  
                 tabPanel("Improving Life Chances", 
                       
                          sidebarPanel( 
                            checkboxGroupInput('varb', 'Select the  childhood circumstances that you would like to improve:' ,  choices= c("Born in poverty"="depr_b","Parental depression"="mhealth_p2","No parental university degree"="noedu_p" ),
                                                          selected = names(maindata)[[1]] )  ), mainPanel( 
                          tabsetPanel( tabPanel( "Average impact", plotOutput('plot1'), 
                                                 p(  em("Note:  Total population lifetime gains from a better childhood (per 100,000 individuals) in England."),align="left" )  ),
                          # sidebarPanel( ),
                                    tabPanel("Distributional impact",plotOutput('plot2'),
                                             p(  em("Note: Distribution of lifetime gains from a better childhood (among 100,000 individuals) accross the lifetime income decile groups."),align="left" ) )   )  )),
                tabPanel("About", 
                         sidebarPanel( p("Legal Disclaimer. The authors make no representations or warranties of any kind with respect to the information, graphics and outputs available on this site."  ) ,
                                       
                                       p("Funding. This is independent research supported by the National Institute for Health Research (NIHR), the Wellcome Trust, and the UK Prevention Research Partnership (UKPRP). All the errors and opinions in the application are entirely those of development team and do not reflect those of NIHR, Wellcome Trust, UKPRP or the University of York." ), width =5 ),
                         mainPanel(  p("This is an interactive tool based on simulated data for by the lifecourse microsimulation model", 
                                     tags$a(href="https://doi.org/10.1101/2021.02.12.21251642", "LifeSim,")," which simulates 
                                     the lives for each child from birth to death in a British birth cohort of children born in year 2000. 
                                     Full documentation can be found ", tags$a(href="https://doi.org/10.1101/2021.02.12.21251642", "here."), sep="")  ,
                         
                         p("The 'Individual Outcomes' tab allows you to enter child charactertistics, as well as various circumstances during childhood and young adulthood, and see how predicted lifetime public costs, health and wellbeing vary, according to the simulation."  ) ,
                         p("The 'Improving Life Chances' tab allows to choose a set of childhood circumstances to improve. It then calculates the long term benefits accross the population (on average and for population subgroups)."  ) ,
                         
                         p("The estimates produced by this tool are simulations based on various sources of data. The predictions of the interactive tool are purely descriptive associations, and not based on causal analysis."  ), width =7  )
                       )
     
                )
    
      # fluidRow(column(3, verbatimTextOutput("text_choice"))),
     # tableOutput("data")
     
   )


)

server <- function(input, output) {
  # svarc <- reactive({
  #   maindata[,  c(  input$varc   )  ]
  # })
  

   

  
    
   output$plot0 <- renderPlot({
# data0<-maindata[apply(maindata[c( input$vard )] == 1, 1, all),][apply(!is.na(maindata[apply(maindata[c( input$vard )] == 1, 1, all),]),1,  all),]
#       
#  data0<-data0[apply(data0[c( input$vard1 )] == 1, 1, all),][apply(!is.na(data0[apply(data0[c( input$vard1 )] == 1, 1, all),]),1,  all),]

     # if(length(maindata[apply(maindata[c( input$vard )] == 1, 1, all),][apply(!is.na(maindata[apply(maindata[c( input$vard )] == 1, 1, all),]),1,  all),])>1){data0<-maindata[apply(maindata[c( input$vard )] == 1, 1, all),][apply(!is.na(maindata[apply(maindata[c( input$vard )] == 1, 1, all),]),1,  all),]
     # }else if(length(maindata[apply(maindata[c( input$vard )[1:(length(input$vard)-1)]] == 1, 1, all),][apply(!is.na(maindata[apply(maindata[c( input$vard  )[1:(length(input$vard)-1)]] == 1, 1, all),]),1,  all),])>1){data0<-maindata[apply(maindata[c( input$vard )[1:(length(input$vard)-1)]] == 1, 1, all),][apply(!is.na(maindata[apply(maindata[c( input$vard)[1:(length(input$vard)-1)]] == 1, 1, all),]),1,  all),]
     # }else{ data0<-maindata}
     # 
     # if(length(data0[apply(data0[c( input$vard1 )] == 1, 1, all),][apply(!is.na(data0[apply(data0[c( input$vard1 )] == 1, 1, all),]),1,  all),])>1){data0<-data0[apply(data0[c( input$vard1 )] == 1, 1, all),][apply(!is.na(data0[apply(data0[c( input$vard1 )] == 1, 1, all),]),1,  all),]
     # }else if(length(data0[apply(data0[c( input$vard1)[1:(length(input$vard1)-1)]] == 1, 1, all),][apply(!is.na(data0[apply(data0[c( input$vard1)[1:(length(input$vard1)-1)]] == 1, 1, all),]),1,  all),])>1){data0<-data0[apply(data0[c( input$vard1 )[1:(length(input$vard1)-1)]] == 1, 1, all),][apply(!is.na(data0[apply(data0[c( input$vard1 )[1:(length(input$vard1)-1)]] == 1, 1, all),]),1,  all),]
     # }else if(length(data0[apply(data0[c( input$vard1)[1:(length(input$vard1)-2)]] == 1, 1, all),][apply(!is.na(data0[apply(data0[c( input$vard1)[1:(length(input$vard1)-2)]] == 1, 1, all),]),1,  all),])>1){data0<-data0[apply(data0[c( input$vard1 )[1:(length(input$vard1)-2)]] == 1, 1, all),][apply(!is.na(data0[apply(data0[c( input$vard1 )[1:(length(input$vard1)-2)]] == 1, 1, all),]),1,  all),]
     # }
     
 
     
     # if(length(data0[apply(data0[c(  "sex" )] == input$varc, 1, all),]$id)>=1){
     #   data0<-data0[apply(data0[c(  "sex" )] == input$varc, 1, all),]} 
 
 benchmark<-maindata[apply(maindata[c(  "sex" )] == input$varc, 1, all),]
 benchmark<- bind_rows(colMeans(benchmark,na.rm=T) )
 
 data0<-maindata[apply(maindata[c(  "sex" )] == input$varc & !is.na(maindata[c(  "sex" )]), 1, all),]
 # data0<-data0[apply( (data0[c(  "qntile_cog7" )] == input$varc2 &  !is.na(data0[c(  "qntile_cog7" )]) )  , 1, all),]
 data0<-data0[apply( (data0[c(  "edu" )] ==input$varc3 &  !is.na(data0[c(  "edu" )]) ), 1, all),]
 data0<-data0[apply( ( data0[c( input$vard )] == 1& !is.na(data0[c( input$vard )]) ), 1, all),]
 
if(length(data0$id)>0){ data0<-data0[apply( ( data0[c( input$vard1 )] == 1& !is.na(data0[c( input$vard1 )]) ), 1, all),] }
 
 
     # if(length(data0[apply(data0[c(  "qntile_cog7" )] == input$varc2, 1, all),]$id)>=1){  
     #   data0<-data0[apply(data0[c(  "qntile_cog7" )] == input$varc2, 1, all),]}
     # 
     # if(length(data0[apply(data0[c(  "edu" )] ==input$varc3, 1, all),]$id)>=1){ 
     #   data0<-data0[apply(data0[c(  "edu" )] ==input$varc3, 1, all),] }
     
     
     data0<- bind_rows(colMeans(data0,na.rm=T) )
     
     outcomes1<-c(   "life_length", "health",  "QALY" )
     
     outcomes3<-c(     "earnings",  "consumption"  )
     outcomes5<-c(     "servicecosts",  "benefits", "taxes"  )
     
     
     
     
     labels1<-c( "Life expectancy", "Healthy life expectancy" , "Good life expectancy"  )
     labels3<-c( "Gross earnings","Consumption" )
     labels5<-c( "Public services","Cash benefits","Tax receipts")
     benchmark1<-select(benchmark, all_of(outcomes1))%>%reshape2::melt( variable.name = "Outcome")
     benchmark3<-select(benchmark, all_of(outcomes3))%>%reshape2::melt( variable.name = "Outcome")
     benchmark5<-select(benchmark, all_of(outcomes5))%>%reshape2::melt( variable.name = "Outcome")
     
     data01<-select(data0, all_of(outcomes1))%>%reshape2::melt( variable.name = "Outcome")
     data03<-select(data0, all_of(outcomes3))%>%reshape2::melt( variable.name = "Outcome")
     data05<-select(data0, all_of(outcomes5))%>%reshape2::melt( variable.name = "Outcome")
     
     positions <- rev(c(all_of(outcomes1)  ) )  
     g1<-ggplot(data01, aes( x=factor(Outcome, levels=c( outcomes1 )     ), y=value  ), colour="black")+
       scale_x_discrete(limits = positions, labels=rev(labels1) )+
       scale_y_continuous(labels = scales::comma, limits=c(0, 90))+
       geom_bar(stat='identity', position = "dodge", alpha=0.8)+
       labs(x = "", y = "Years" , 
             title = "Individual Lifetime Outcomes")+
       theme_minimal( base_size=15 )+
       scale_color_discrete(breaks=c( )) + 
       coord_flip()+ 
       theme(legend.position="none" ,
             plot.title = element_text(hjust = 0.5)) +
       geom_errorbar(data=benchmark1,  aes( y = value, ymin=value, ymax=value,  col = "Ref1"), col = "red", linetype = 1,size=1)
     if(is.na(data0$id) ){g1<-g1+ geom_text( x=2, y=40,label="Insufficient data points to make this prediction.", size=4, fontface="italic" )}
     
     positions <- rev(c(all_of(outcomes3)  ) )
     g3<-ggplot(data03, aes( x=factor(Outcome, levels=c( outcomes3 )     ), y=value ), colour="black")+
       scale_x_discrete(limits = positions, labels=rev(labels3) )+
       scale_y_continuous(labels = scales::comma, limits=c(0, 45000) )+
       geom_bar(stat='identity', position = "dodge", alpha=0.8)+
       labs(x = "", y = paste(enc2utf8("\u00A3"), ", average annual value", sep="") ) +
       theme_minimal( base_size=15)+
       scale_color_discrete(breaks=c( )) + 
       coord_flip()+ 
       theme(legend.position="none" ) +
       geom_errorbar(data=benchmark3,  aes( y = value, ymin=value, ymax=value,  col = "Ref1"), col = "red", linetype = 1,size=1)
     if(is.na(data0$id) ){g3<-g3+ geom_text( x=2, y=20000,label="Insufficient data points to make this prediction.", size=4, fontface="italic" )}
     
     positions <- rev(c(all_of(outcomes5)  ) )
     g5<-ggplot(data05, aes( x=factor(Outcome, levels=c( outcomes5 )     ), y=value  ), colour="black")+
       scale_x_discrete(limits = positions, labels=rev(labels5) )+
       scale_y_continuous(labels = scales::comma , limits=c(0, 700000))+
       geom_bar(stat='identity', position = "dodge", alpha=0.8)+
       labs(x = "", y = paste(enc2utf8("\u00A3"), ", lifetime total", sep="") )+
       theme_minimal( base_size=15 )+
       scale_color_discrete(breaks=c( )) + 
       coord_flip()+ 
       theme(legend.position="none",
             plot.caption = element_text(hjust=0,  face="italic")) +
       geom_errorbar(data=benchmark5,  aes( y = value, ymin=value, ymax=value,  col = "Ref1"), col = "red", linetype = 1,size=1) 
     if(is.na(data0$id) ){g5<-g5+ geom_text( x=2, y=300000,label="Insufficient data points to make this prediction.", size=4, fontface="italic" )}
     
      plot_grid( g1, g3, g5, ncol=1, align="v", rel_heights=c(6, 4, 5  ) )
   }) 
   
  output$plot1 <- renderPlot({

data1<-maindata[apply(maindata[c(  input$varb )] == 0, 1, all),]
data2<-maindata[apply(maindata[c(  input$varb )] == 1, 1, any),]

    no_adv<-dplyr::summarize(data1, count=n())$count[1]
    no_disadv<-dplyr::summarize(data2, count=n())$count[1]

    data1<- bind_rows(colMeans(data1,na.rm=T) )
    data2<- bind_rows(colMeans(data2,na.rm=T) )
    data<-data1-data2
    outcomes1<-c(   "life_length", "health",  "QALY" )

    outcomes3<-c(     "earnings",  "consumption"  )
    outcomes5<-c(     "servicecosts",  "benefits", "taxes"  )
    

 
    
    labels1<-c( "Life years", "Healthy life years" , "Good life years"  )
    labels3<-c( "Gross earnings","Consumption" )
    labels5<-c( "Public service savings","Cash savings","Tax receipts")
    
    data1<-select(data, all_of(outcomes1))%>%reshape2::melt( variable.name = "Outcome")
    data3<-select(data, all_of(outcomes3))%>%reshape2::melt( variable.name = "Outcome")
    data5<-select(data, all_of(outcomes5))%>%reshape2::melt( variable.name = "Outcome")
    
    positions <- rev(c(all_of(outcomes1)  ) )  
    g1<-ggplot(data1, aes( x=factor(Outcome, levels=c( outcomes1 )     ), y=value*no_disadv  ), colour="black")+
      scale_x_discrete(limits = positions, labels=rev(labels1) )+
      scale_y_continuous(labels = scales::comma, limits=c(0, 350000))+
      geom_bar(stat='identity', position = "dodge", alpha=0.8)+
      labs(x = "", y = "Years" ,
           title = "Total Population Gains for England")+
      theme_minimal( base_size=15 )+
      scale_color_discrete(breaks=c( )) + 
      coord_flip()+ 
      theme(legend.position="none",
            plot.title = element_text(hjust = 0.5)) 
    

    positions <- rev(c(all_of(outcomes3)  ) )
    g3<-ggplot(data3, aes( x=factor(Outcome, levels=c( outcomes3 )     ), y=value*no_disadv  ), colour="black")+
      scale_x_discrete(limits = positions, labels=rev(labels3) )+
      scale_y_continuous(labels = c("0", "100M", "2000M", "300M",   "400M",   "500M") ,  limits=c(0,500000000 ), breaks=c(0, 100000000, 200000000, 300000000, 400000000, 500000000))+
      # scale_y_continuous(labels = scales::comma, limits=c(0, 500000000) )+
      geom_bar(stat='identity', position = "dodge", alpha=0.8)+
      labs(x = "", y = paste(enc2utf8("\u00A3"), ", average annual value", sep="") ) +
      theme_minimal( base_size=15 )+
      scale_color_discrete(breaks=c( )) + 
      coord_flip()+ 
      theme(legend.position="none" ) 
    
    positions <- rev(c(all_of(outcomes5)  ) )
    g5<-ggplot(data5, aes( x=factor(Outcome, levels=c( outcomes5 )     ), y=ifelse(Outcome!='taxes', -1*value*no_disadv, value*no_disadv)  ), colour="black")+
      scale_x_discrete(limits = positions, labels=rev(labels5) )+
      scale_y_continuous(labels = c("0", "500M", "1000M", "1500M") ,  limits=c(0,1500000000 ), breaks=c(0, 500000000, 1000000000, 1500000000))+
      geom_bar(stat='identity', position = "dodge", alpha=0.8)+
      labs(x = "", y = paste(enc2utf8("\u00A3"), ", lifetime total", sep="") )+
      theme_minimal( base_size=15 )+
      scale_color_discrete(breaks=c( )) + 
      coord_flip()+ 
      theme(legend.position="none",
            plot.caption = element_text(hjust=0,  face="italic")) 
    plot_grid( g1, g3, g5, ncol=1, align="v", rel_heights=c(6 ,4, 5  ) )
      })
  
  output$plot2 <- renderPlot({

    datagr_levels<-group_by(maindata,  decile_cons)%>%
      dplyr::summarize(QALY=mean(QALY),  consumption=mean(consumption),  health=mean(health),  life_length=mean(life_length) )%>%
      mutate(  group="Baseline level")

     datagr1<-maindata[apply(maindata[ c( input$varb ) ] == 0, 1, all),]%>%group_by( decile_cons)%>%
      dplyr::summarize( QALY=mean(QALY, na.rm = T),  consumption=mean(consumption, na.rm = T),  health=mean(health, na.rm = T), 
                        life_length=mean(life_length, na.rm = T) )
    
    datagr2<-maindata[apply(maindata[ c( input$varb ) ] == 1, 1, any),]%>%group_by( decile_cons)%>%
      dplyr::summarize( QALY=mean(QALY, na.rm = T),  consumption=mean(consumption, na.rm = T),  health=mean(health, na.rm = T), 
                        life_length=mean(life_length, na.rm = T) )

    if(length(datagr2$decile_cons)<10){
      x<-length(datagr2$decile_cons)[1 ]
      
      y<- data.frame(decile_cons=c(rep(0, 10-x)),
                     QALY=c(rep(0, 10-x)),
                     consumption=c(rep(0, 10-x)), 
                     health=c(rep(0, 10-x)), 
                     life_length=c(rep(0, 10-x)))
      
      datagr2<-  bind_rows(datagr2, y) }
    
    datagr<-data.frame(datagr1[,c( "decile_cons"  )],  (datagr1[,c( "QALY",  "consumption" , "health", "life_length")] ) -(datagr2[,c( "QALY",  "consumption" , "health", "life_length")]) )

    
    datagrf<- mutate( datagr, group="Gains")

        
    
     datagrff<-bind_rows(datagr_levels, datagrf)
 
    ggplot(datagrff[order(datagrff$group, decreasing = F),], aes(decile_cons, QALY,  fill=factor(group, levels=c( "Gains", "Baseline level"))), colour=group
    ) +
      geom_col( alpha=1, position="stack" , colour='black'  )+
      theme_minimal( base_size=15 )+
      labs(x = "Income decile groups", y = "Lifetime wellbeing, good years" ,
           title = "Distribution of Gains" )+
      theme(plot.caption = element_text(hjust=0,  face="italic"),
            legend.position = "top",plot.margin = unit(c(1, 1 , 2, 4), "lines"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text( hjust = 0.5) )+
      scale_fill_grey(start = 0, end = 1, name = "")+
      scale_y_continuous(labels = scales::comma, limits=c(0, 80))+
      coord_cartesian(clip = 'off')

     

  })

  }

shinyApp(ui = ui, server = server)
