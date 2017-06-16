library(shinydashboard)
library(keras)
library(dplyr)
library(plotly)
library(miniUI)
library(DT)
#################################  UI PART ###########################################################

ui <- miniPage(
  gadgetTitleBar(left = NULL, right = NULL,"Video Analyzer"),
    miniTabstripPanel(
      
      miniTabPanel(
        "introduction", icon = icon("area-chart"),
        miniContentPanel(
          htmlOutput("intro")
        )
      ),
      miniTabPanel(
        "Parameters", icon = icon("sliders"),
        miniContentPanel(
          numericInput("fps", "Frames per second ", 0.25, 0.01, 1,0.01),
          fileInput('file1', 'Upload a video (max 500 MB)')
        )
      ),
      miniTabPanel("images", icon = icon("file-image-o"),
                   miniContentPanel(
                     padding = 0,
                     dataTableOutput('images')
                   )
      ),
      miniTabPanel("tags", icon = icon("file-image-o"),
                   miniContentPanel(
                     padding = 0,
                     plotlyOutput('tagoverview')
                   )
      )
    )
)
    
    

################################  SERVER PART ########################################################

options(shiny.maxRequestSize=500*1024^2)

convertVideoToImages <- function(file, framesPerSecond = 1) {
  ## helper function to call ffmpeg from within R
  ffCommand <- paste0(
    "ffmpeg -i \"", 
    file, 
    "\"  -s 600x400" ,
    " -t 1200 -r ",
    framesPerSecond,
    " \"www/out_%04d.jpg\"")
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
        img = image_load(paste0("www/",i), target_size = c(224,224))
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
        "' height='180' width='220'>"
      )
      out = out %>% rename(tag = class_description)
      return(out)
    }
    else
    {
      return(NULL)
    }
  })
  
  
  
  output$intro <- renderUI({
    list(
      h4("Upload a video (< 500 MB), then ffmpeg is used to extract images from the video, specify the number of frames per second. 
A value of 0.125 means one frame every 8 seconds. Then using the keras package a VGG16 pre trained network is
             used to tag the extracted images. For each image the top 3 tags are returned"),
      p(" "),
      h4("Cheers, Longhow")
    )
  })
  
  ######## TABLE with extracted images #############################
  output$images = renderDataTable({
    
    tmp = extractedImages()
    if(!is.null(tmp)){
      datatable(rownames = FALSE,
        tmp %>% select(-class_name, -image),
        options = list(
          autoWidth = FALSE,
          columnDefs = list(list(width = '80px', targets = c(0,1)))
        ),
        escape = FALSE
      ) %>% 
      formatPercentage('score', 1)
    }
  })
  
  ######## print video information #################################
  output$videoinfo = renderPrint({
    inFile = input$file1
    ffCommand <- paste0(
      "ffmpeg -i \"", 
      inFile$datapath
    )
    a = system(ffCommand, intern=TRUE)
    print(a)
  })
  
  ######## plotly graph of extracted tags #########################
  output$tagoverview = renderPlotly({
    tmp = extractedImages()
    if(!is.null(tmp)){
      tmp %>% 
        group_by(tag) %>% 
        summarise(n=mean(score)) %>%
        mutate(
          tag = forcats::fct_reorder(tag, n, .desc=TRUE)
        ) %>%
        plot_ly(
          x = ~tag, 
          y = ~n, 
          type="bar"
        )
    }
  })
}

shinyApp(ui, server)