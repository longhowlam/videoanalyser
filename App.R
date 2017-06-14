library(shinydashboard)
library(keras)
library(dplyr)


######  UI PART #################################

ui <- dashboardPage(
  dashboardHeader(title = "Video analyzer", titleWidth = 600),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      numericInput("fps", "Frames per second ", 1, 0.01, 1,0.01),
      fileInput('file1', 'Choose an image (max 300 MB)'),
      menuItem("Video images", tabName = "videoanalysis", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              h4("Introduction"),
              list(
                p("ffmpeg is used to extract images from the videoa, then a VGG 16 pre trained network is used to tag the images
 for each image I return the top 3 tags from vgg16"),
                p(" "),
                p("Cheers, Longhow")
                )
              
              ),
      tabItem(tabName = "videoanalysis",
              h4("images taken from video"),
              fluidRow(
                dataTableOutput('images')
              
              )
      )
      
  )
    )
)


#######  SERVER PART ########################################################

options(shiny.maxRequestSize=300*1024^2)

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
 
  extractedImages <- reactive({
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(
      message = 'Analyzing Video in progress', 
      detail = 'This may take a few seconds...'
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
      return(out)
    }
    else
    {
     return(0)
    }
  })
  
  
  
  output$images = renderDataTable({
    
    tmp = extractedImages()
    tmp$images = paste0(
      "<img src='",
      tmp$image,
      "' height='180' width='200'>"
    )
    tmp %>% select(-class_name, -image)
  },  escape=FALSE)
  
  
  output$plaatje <- renderImage({
    
    inFile = input$file1
    print(inFile)
    if (!is.null(inFile))
    {
      
      width  <- session$clientData$output_plaatje_width
      height <- session$clientData$output_plaatje_height
      list(
        src = inFile$datapath,
        width=width,
        height=height
      )
    }
    else
    {
      list(src="www/IM07,jpg")
    }
  },
  deleteFile = FALSE
  )
  
  
}

shinyApp(ui, server)