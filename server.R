if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,shinyWidgets,shinydashboard,DT,ggplot2,
               ggthemes,leaflet,plotly,data.table,sf,mapview,mapedit,RColorBrewer,
               rintrojs,utf8,stringr,bit64,plyr,dplyr,sp,crosstalk, htmltools, leaflet.extras)


hardfunct=function(x){
  if (x>=400) {
    return(400)
  } else if (x<=25) {
    return(25)
  } else {
    return(x)}
}

listofparams=c("CADMIUM","CHROMIUM",
               "COPPER","LEAD","MERCURY","NICKEL","SILVER","ZINC")

params <- paste(listofparams, collapse = "|")

#listofsites=as.vector(unique(metals$Basin))

diss=c("Total","Dissolved")


###########
 
options(shiny.maxRequestSize=30*1024^2) 

shinyServer(function(input, output, session) { 
 
  
  parmchoice=reactive({
    myVectorOfStrings <- input$paramschoice
    DD=paste(myVectorOfStrings, collapse = "|")  
  })

  

  tabname <- renderText({
    input$paramschoice
  })
  
  reacMET <- reactive({
    
    if(is.null(input$paramschoice)==T){return()}
    myVectorOfStrings8 <- input$paramschoice
    matchExpression8 <- paste(myVectorOfStrings8, collapse = "|")
    matchExpression8
  })
  
############### LOAD THE EXCEL SHEET ############################################
  
  
  DF =reactive({
    
   req(input$file1)
    
   df <- read.csv(input$file1$datapath)
    
   df$Date=as.Date(df$Date.Time,format = "%m/%d/%Y")
   df$Type=ifelse(df$Level.1.Code=="ESTURY","Salt","Fresh")
   
   
   df=df %>% mutate(Value=ifelse(Parm.Meas.Unit== "ng/l" & str_detect(Parm.Name, "MERCURY"),Value/1000,Value))

   })
  
  
NoSamples=reactive ({
    
a=DF()
    
a %>% group_by(ID,Latitude,Longitude) %>% 
summarise(No.Sample.Days=n_distinct(Date))
})
  



output$ui_basins<-renderUI({
  
  cc=DF()
  cc=as.character(unique(cc$Basin))  
  
  pickerInput("basin","Select Basin",
              choices = cc,selected = cc,multiple = TRUE,options = list(`actions-box` = TRUE))
})
 
output$ui_sites<-renderUI({
  
b=DF()
bb=as.character(unique(b$ID))  

selectInput("sites","Select Sites",
              choices = bb,multiple = TRUE)
})

output$ui_streams<-renderUI({
  
  c=DF()
  cc=toupper(unique(c$Stream.Name))
  
  selectInput("streams","Select Stream",
              choices = cc,multiple = TRUE)
})

Hardness= reactive({
  
  Hard_df=DF() 
  Hard_df %>% filter(Storet.Parm.Code == "DHARD", Value) %>% mutate(Adj_Hard=sapply(Value,hardfunct)) %>% select(ID,Date,Value,Adj_Hard)
  
  })
  
  
Standards= reactive({ 
 
adjhard=Hardness()  
metals_df=DF()
numsamp=NoSamples()


Z=left_join(adjhard,metals_df,by=c("Date","ID"))
ZZ=left_join(numsamp,Z,by=c("ID","Latitude","Longitude"))


})

###This is the full data set############# 
StandardZ= reactive({ 
  
  rdz=Standards()
  
 standardz=rdz %>% mutate(
    Acute = case_when(
      str_detect(Parm.Name, "CADMIUM") & Type=="Fresh" ~ exp(1.128*log(Adj_Hard)-3.828),
      str_detect(Parm.Name, "CADMIUM") & Type=="Salt"  ~ 40,
      str_detect(Parm.Name, "COPPER") & Type=="Fresh"  ~ exp(0.9422*log(Adj_Hard)-1.70)*0.96,
      str_detect(Parm.Name, "COPPER") & Type=="Salt"   ~ 9.3, 
      str_detect(Parm.Name, "LEAD") & Type=="Fresh"    ~ exp((1.273*log(Adj_Hard)-1.084))*(1.46203-(log(Adj_Hard)*0.145712)),
      str_detect(Parm.Name, "LEAD") & Type=="Salt"     ~ 230,
      str_detect(Parm.Name, "MERCURY") & Type=="Fresh" ~ 1.4,
      str_detect(Parm.Name, "MERCURY") & Type=="Salt"  ~ 1.8,
      str_detect(Parm.Name, "NICKEL") & Type=="Fresh"  ~ exp(0.846*log(Adj_Hard)+1.312)*0.998,
      str_detect(Parm.Name, "NICKEL") & Type=="Salt"   ~ 74,
      str_detect(Parm.Name, "SILVER") & Type=="Fresh"  ~ exp(1.72*log(Adj_Hard)-6.52)*0.85,
      str_detect(Parm.Name, "SILVER") & Type=="Salt"   ~ 1.9,
      str_detect(Parm.Name, "ZINC") & Type=="Fresh"    ~ exp(0.8473*log(Adj_Hard)+0.884)*0.978,
      str_detect(Parm.Name, "ZINC") & Type=="Salt"     ~ 90
    ),
    Chronic= case_when(
      str_detect(Parm.Name, "CADMIUM") & Type=="Fresh" ~ exp(0.7852*log(Adj_Hard)-3.490),
      str_detect(Parm.Name, "CADMIUM") & Type=="Salt"  ~ 8.8,
      str_detect(Parm.Name, "COPPER") & Type=="Fresh"  ~ exp(0.8545*log(Adj_Hard)-1.702)*0.96,
      str_detect(Parm.Name, "COPPER") & Type=="Salt"   ~ 6,
      str_detect(Parm.Name, "LEAD") & Type=="Fresh"    ~ exp((1.273*log(Adj_Hard)-3.259))*(1.46203-(log(Adj_Hard)*0.145712)),
      str_detect(Parm.Name, "LEAD") & Type=="Salt"     ~ 8.8,
      str_detect(Parm.Name, "MERCURY") & Type=="Fresh" ~ 0.77,
      str_detect(Parm.Name, "MERCURY") & Type=="Salt"  ~ 0.94,
      str_detect(Parm.Name, "NICKEL") & Type=="Fresh"  ~ exp(0.846*log(Adj_Hard)-0.884)*0.997,
      str_detect(Parm.Name, "NICKEL") & Type=="Salt"   ~ 82,
      str_detect(Parm.Name, "ZINC") & Type=="Fresh"    ~ exp(0.8473*log(Adj_Hard)+0.884)*0.986,
      str_detect(Parm.Name, "ZINC") & Type=="Salt"     ~ 81
    ),Acute=round(Acute,3),Chronic=round(Chronic,3),Exceedance=case_when(
      Acute <= Value.y | Chronic <= Value.y            ~"Yes",
      is.na(Chronic) &  Acute <= Value.y               ~"Yes"
    ) 
      
  )

  
})

  

subset_standardz=reactive ({
  
  StandardZ() %>% select(ID,Date,Acute,Chronic,Value.y,Parm.Name,Type,Adj_Hard,Longitude,Latitude,Basin,Huc8,No.Sample.Days,Exceedance,Survey.Prog.Code.ID,Stream.Name) %>% 
    filter(!str_detect(Parm.Name, "HARDNESS") & str_detect(Parm.Name, parmchoice()) & Date >= input$dates[1] & Date <= input$dates[2] & Basin %in% input$basin & No.Sample.Days >= input$samples[1] & No.Sample.Days>= input$samples[2] ) 

})


output$standards<- renderDataTable({  
  
  datatable(
    subset_standardz(),
    extensions = 'Buttons', options = list(
      columnDefs = list(list(className = 'dt-center', targets = 8)),
      pageLength = 150,
      lengthMenu = c(5, 10, 25, 50)
    )
    
  ) 
  
 
  
})

exceed_map=reactive ({
  
  Rt=StandardZ() %>% filter( Exceedance=="Yes") %>% 
    group_by(ID,Latitude,Longitude) %>% arrange(Date) %>%
    summarise(dates  = paste(Date, collapse ="|"),Metal = paste(Parm.Name, collapse ="|"), No.times = length(Date),No.Params= length(unique(Parm.Name)))  %>%
    arrange(desc(dates), No.times) 
  })
###Crosstalk######

output$plot1 = renderPlotly({
  
  N= subset_standardz() 

  p <- ggplot(N,aes(x=Date,y=Value.y,fill=as.factor(Parm.Name),key=N$ID,text =paste("ID:",ID)
                    ,shape=as.factor(Exceedance)))+
    geom_point(size=3)+
    
      scale_shape_manual(guide=FALSE, values=c(17, 1))+
    scale_fill_manual(labels = listofparams, values=c("red","blue","green","yellow","orange","purple","magenta","black"))+
   
       theme_classic()+
    
      labs(x = "Date", y = "ug/L")+
      scale_x_date(date_breaks = "1 year", date_labels ="%Y")+
      theme(axis.text.x=element_text(angle=60, hjust=1))
       ggplotly(p) %>% layout(dragmode="select")
  
  })



  
 SD=SharedData$new(subset_standardz)
              


 
 output$scatter2 <- DT::renderDataTable({
   
   datatable(SD)
 }, server = FALSE)
 
 
 output$plotlyscatter2= renderPlotly ({
   
   p <- plot_ly(SD, x = ~Date, y = ~Value.y, color = ~Parm.Name) %>% 
   add_markers(alpha = 0.5) %>%
     layout(dragmode = "select") %>%
     highlight("plotly_selected",dynamic=TRUE)

   
    }) 
 

output$plot2 = renderPlotly({
  
  #if(is.null(input$col)==T){return()}
  
 N2=StandardZ() %>% filter( Exceedance=="Yes") %>% 
    group_by(ID,Latitude,Longitude) %>% arrange(Date) %>%
    summarise(dates  = paste(Date, collapse ="|"),Metal = paste(Parm.Name, collapse ="|"), No.times = length(Date),No.Params= length(unique(Parm.Name)))  %>%
    arrange(desc(dates), No.times) 

#N3 =N2 %>%select(ID,No.times, No.Params) %>% filter(Exceedance=="Yes" & No.times >= 0 & No.Params >= 1) 
 
 p=plot_ly(N2, x = ~ID, y = ~No.times,name="Number of Exceedances", type = "bar")%>% 
    add_trace(y = ~No.Params, name = 'Number of Parameters') %>%
    layout(
     title = 'Chronic and or Acute Exceedances',
     xaxis = list(
       type = 'category',
       title = 'Site ID'
     ))
  
  
})


###################

output$numsamples<- renderDataTable({  
  
  NoSamples() #%>% filter(No.Sample.Days >= input$samples[1] & No.Sample.Days>= input$samples[2]) 
  
})


output$exceed<- renderDataTable({  
  
  exx=StandardZ() %>% filter( Exceedance=="Yes") %>% 
    group_by(ID) %>% arrange(Date) %>%
    summarise(dates  = paste(Date, collapse ="|"),Metal = paste(Parm.Name, collapse ="|"), No.times = length(Date),No.Params= length(unique(Parm.Name)))  %>%
    arrange(desc(dates), No.times)

  DT::datatable(
    exx,
    extensions = 'Buttons', options = list(
      columnDefs = list(list(className = 'dt-center', targets = 5)),
      pageLength = 50,
      scrollX='400px',
      lengthMenu = c(5, 10, 25, 50),
      dom = 'Bfrtip',
      buttons = 
        list('copy', 'print', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        ))
      
    )
    
  ) 
  
  
  
})
  
outVar2 = reactive({
  mydata2 = exceed_map()
  Max=max(mydata2$No.times)
})

observe({
  updateNumericInput(session, "exceedances",
                    max = outVar2()
  )})


outVar3 = reactive({
  mydata3 = exceed_map()
  Min=min(mydata3$No.times)
})

observe({
  updateNumericInput(session, "exceedances",
                     min = outVar3()
  )})





output$hardness<- renderDataTable({  
  
  Hardness()
  
})








output$downloadbutton <- renderUI({
  if (is.null(input$file1)){
    print("click 'Browse'to upload a file")
  } else {
    downloadButton("downloadData", "Download")
  }
})


output$downloadData <- downloadHandler(
  
 filename = function() {
    paste("Metals_App_Data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(subset_standardz(), file)
  }
) 
  
######

   
master.subset.data <- reactive({
  
  subset(subset_standardz(), Basin %in% input$basin)

  })



  

output$mymap <- renderLeaflet({
  
leaflet() %>% 
addProviderTiles("OpenStreetMap.Mapnik", group = "Mapnik") 
})


df3=reactive({
  
f=st_as_sf(x = exceed_map(), 
               coords = c("Longitude", "Latitude"),
               crs = "+proj=longlat +datum=WGS84")
})

df2=reactive({
  
  g=st_as_sf(x = subset_standardz(), 
             coords = c("Longitude", "Latitude"),
             crs = "+proj=longlat +datum=WGS84")
})


 
observe({
  
  df <- subset_standardz()
  df2=df2()
  df3=df3() %>% filter(No.times >= input$exceedances)
  
  ## check for empty dataframe
  if(nrow(df2) == 0){
    leafletProxy("mymap", data = df2) %>% 
      clearMarkers()
  }else{
    leafletProxy("mymap", data = df2) %>% 
      setView(lng = -77.78747222, lat = 37.39122222, zoom = 8) %>% 
      clearMarkers() %>%     
      addProviderTiles("Esri.WorldImagery", group="WorldImagery") %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="OpenStreetMap") %>%
      
      addCircleMarkers(data= df2,radius=8 , group = "Filtered Sites",color="black", label= ~ID, fillColor="red", 
                       stroke = TRUE, fillOpacity = 0.8,
                       popup = popupTable(df,row.numbers = TRUE),labelOptions = labelOptions(noHide = FALSE, offset=c(0,-2), textOnly = FALSE)) %>%
      addCircleMarkers(data= df3,radius=8 , group = "Filtered Exceedances",color="black", label= ~ID, fillColor="blue", 
                       stroke = TRUE, fillOpacity = 0.8,
                       popup = popupTable(df3,row.numbers = TRUE),labelOptions = labelOptions(noHide = FALSE, offset=c(0,-2), textOnly = FALSE)) %>%
    addLayersControl(overlayGroups = c("Filtered Sites","Filtered Exceedances") , baseGroups = c("WorldImagery","OpenStreetMap"), options = layersControlOptions(collapsed = FALSE))
    
       }
 
   
   
})













  
  
  
output$csv_import_ready <- reactive({
  return(!is.null(input$file1))
})

outputOptions(output, "csv_import_ready", suspendWhenHidden = FALSE)


  
  

  
  
   
####End of App    
    
})      



