library(shinydashboard)
library(keras)
library(dplyr)
library(plotly)

######  UI PART #################################

ui <- dashboardPage(
  dashboardHeader(title = "A simple Video analyzer", titleWidth = 600),
  dashboardSidebar(width=300,
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      numericInput("fps", "Frames per second ", 1, 0.01, 1,0.01),
      fileInput('file1', 'Choose an image (max 500 MB)'),
      menuItem("Video images", tabName = "videoanalysis", icon = icon("th")),
      menuItem("Info on extracted classes", tabName = "extracted", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "introduction",
        h3("Introduction"),
        list(
          h4("ffmpeg is used to extract images from the video, then using the keras package a VGG16 pre 
trained network is used to tag the extracted images for each image I return the top 3 tags from vgg16"),
          p(" "),
          h4("Cheers, Longhow")
        )
      ),
      tabItem(
        tabName = "videoanalysis",
        h4("images taken from video"),
        fluidRow(
          dataTableOutput('images')
        )
      ),
      tabItem(
        tabName = "extracted",
        h4("Video info and Overview of tags extracted"),
        fluidRow(
          textOutput('videoinfo'),
          plotlyOutput('tagoverview')
        )
      )
    )
  )
)

#######  SERVER PART ########################################################

options(shiny.maxRequestSize=500*1024^2)

convertVideoToImages <- function(file, framesPerSecond = 1) {
  
  ffCommand <- paste0(
    "ffmpeg -i \"", 
    file, 
    "\"  -s 600x400" ,
    " -t 1200 -r ",
    framesPerSecond,
    " \"www\\out_%04d.jpg\"")
  system(ffCommand)
}


vgg16 = application_vgg16(weights = 'imagenet')

server <- function(input, output, session) {
  
  ######## reactive function #################
  
  extractedImages <- reactive({
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(
      message = 'Analyzing Video in progress', 
      detail = 'This may take a few minutes'
    )
    
    inFile = input$file1
    
    if (!is.null(inFile))
    {
      unlink("www/*")
      convertVideoToImages(inFile$datapath, input$fps)
      
      fk = list.files("www")
      out = data.frame()
      
      for(i in fk)
      {
        img = image_load(paste0("www\\",i), target_size = c(224,224)    )
        x = image_to_array(img)
        
        dim(x) <- c(1, dim(x))
        x = imagenet_preprocess_input(x)
        
        # extract features
        preds = vgg16 %>% predict(x)
        iter_i = imagenet_decode_predictions(preds, top = 3)[[1]]
        iter_i$image = i
        out = rbind(out, iter_i )
      }
      out$images = paste0(
        "<img src='",
        out$image,
        "' height='180' width='200'>"
      )
      return(out)
    }
    else
    {
     return(0)
    }
  })
  
  ####### TABLE with extracted images #############################
  output$images = renderDataTable({
    
    tmp = extractedImages()
    tmp %>% select(-class_name, -image)
  },  escape=FALSE)

  ######## print video information ###################
  output$videoinfo = renderPrint({
    inFile = input$file1
    ffCommand <- paste0(
      "ffmpeg -i \"", 
      inFile$datapath
    )
    a = system(ffCommand, intern=TRUE)
    print(a)
  })
  
  ####### plotly graph of extracted tags ##############
  output$tagoverview = renderPlotly({
    extractedImages() %>% 
      group_by(class_description) %>% 
      summarise(n=mean(score)) %>%
      mutate(
        class_description = forcats::fct_reorder(class_description, n, .desc=TRUE)
      ) %>%
    plot_ly(
      x = ~class_description, 
      y = ~n, 
      type="bar"
    )
  })
}

shinyApp(ui, server)