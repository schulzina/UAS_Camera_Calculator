# Author: Ina Schulz
# github: schulzina
# date: 17.04.2023
# University of Würzburg, M.Sc. Applied Earth Observation and Geonalaysis of the Living Environment
# course: MB2
# lecturer: Dr. Martin Wegmann
# built: R 4.2.3

# load libraries
pacman::p_load(
  "shiny",
  "dplyr",
  "ggplot2",
  "shinyjs",
  "XML",
  "sf",
  "elevatr",
  "XML",
  "raster",
  "magrittr",
  "leaflet"
)

# Load functions from Functions.R script
source("Functions.R")

# set your working directory this is automatically the folder in which the code is also saved.
setwd(choose.dir())

# Data ----
# load CSV containing all available camera specs
specs <- read.csv("camera_specs.csv",
                  sep = ";",
                  dec = ",",
                  header = T)

# UI ----
ui <- fluidPage(
  # shiny theme is not being displayed
  #shinythemes::shinytheme("flatly"),
  useShinyjs(),
  titlePanel("UAS Camera Calculator"),
  sidebarLayout(
    sidebarPanel(
      fluid = T,
      width = 5,
      fluidRow(
        column(
          6,
          strong(h4("Flight Parameters")),
          hr(),
          # Input Camera Choices
          selectizeInput("Camera", "Camera", choices = colnames(specs[2:length(specs)])),
          
          hr(),
          # Input: Sensor Width
          fluidRow(column(6,
                          numericInput("SW", "Sensor Width [mm]:",
                                       value = 2000, min = 0, max = 1000, step = 1),
          ),
          column(6,
                 numericInput(
                   "SH",
                   "Sensor Height [mm]:",
                   value = 2000,
                   min = 0,
                   max = 1000,
                   step = 1
                 ),
          ), ),
          
          fluidRow(column(
            6,
            numericInput(
              "IW","Image Width [px.]:",
              value = 2000,min = 0, max = 100000,
              step = 1
            ),
          ),
          column(
            6,
            numericInput(
              "IH","Image Height [px.]:",
              value = 2000,min = 0,max = 100000,
              step = 1
            ),
          ), ),
          
          numericInput(
            "FL",
            "Focal length [mm]:",
            value = 130,
            min = 0,
            max = 100000,
            step = 1
          ),
          hr(),
          
          # Input: Flight Height (FlightHm)
          sliderInput(
            "FlightHm",
            "Flight Height [m]:",
            min = 1,
            max = 120,
            value = c(80, 100),
            step = 1
          ),
          
          hr(),
          
          fluidRow(# Inputs of Photogrammetric Overlap
            # Input: Front overlap (FrontOvL)
            column(
              6,
              sliderInput(
                "FrontOvL",
                "Front Overlap [%]:",
                min = 50,
                max = 100,
                value = 75,
                step = 5
              ),
            ),
            
            # Input: Side overlap
            column(
              6,
              sliderInput(
                "SideOvL",
                "Side overlap [%]:",
                min = 50,
                max = 100,
                value = 75,
                step = 5
              ),
            ), ),
          # Input: Tolerance of sensor
          sliderInput(
            "SensorP",
            "Sensor precision [%]:",
            min = 0,
            max = 100,
            value = 90,
            step = 5
          ),
        ),
        
        # Inputs of AOI or Flight Data
        column(
          6,
          strong(h4("AOI and DTM")),
          #hr(),
          strong("Coordinate system = EPSG 4326 (WGS84)"),
          fileInput(
            "AOI",
            "Select geopackage-file of AOI",
            multiple = F,
            accept = ".gpkg"
          ),
          textInput("dtm_filename",
                    "Enter filename of DTM :",
                    value = "",
                    placeholder = "Your Filename"),
          checkboxInput("DTM_downl",
                        "Would you like to download the DTM? It will be saved in your working directory/DTM/name.",
                        value = F),
          fileInput(
            "DTM",
            "Select Digital Terrain Model :",
            multiple = F,
            accept = ".tif"
          ),
          # clipping DTM to AOI, however this is not working on the server at the moment
          # checkboxInput("clip",
          #               "Clip DTM to AOI", 
          #               value=F),
          hr(),
          fileInput(
            "GPX",
            "Select gpx file of planned flight :",
            multiple = F,
            accept = ".gpx"
          ),
          
          fluidRow(
            h5("Starting point [dec. deg.]"),
            column(6,
                   numericInput("lat", value = -25.33, "Lat.:", step = 0.001, min = -90, max = 90)),
            column(6,
                   numericInput("lng",  value = 31.40, "Long.:", step = 0.001, min = -180, max = 180))
          ),
          hr(),
          checkboxInput("export", "Export GSD raster", value = F),
          textInput("raster_filename", "Enter Filname of Export", placeholder = "Your Filename"),
          textOutput("export_gsd")
        ),
      ),
    ),
    
    mainPanel(
      width =7,
      tabsetPanel( type= "tabs",
                   # Tab of Intro Text
                   tabPanel("Introduction",
                            htmlOutput("introtext")     
                   ),
                   # Tab of the basic parameters
                   tabPanel(
                     "Basic Parameters",
                     fluidRow(
                       br(),
                       strong(h4("Basic Parameters")),
                       hr(),
                       column(7, textOutput("HeightInfo")),
                       column(7, textOutput("GrSaDi")),
                       column(7, textOutput("DWidth")),
                       column(7, textOutput("DHeight")),
                       column(7, textOutput("AreaM")),
                     ),
                     # Overlap Plot Output
                     strong("Overlap of Images Across Rows of Flight"),
                     fluidRow(column(12, 
                                     column(6,
                                     plotOutput("ovlplot")),
                                     column(6, h4("GSD based on Flight Height Input"),
                                            plotOutput("GSDHeightPlot")),
                                     br(),
                                     # in this leaflet, the overlap of images can be plotted - not working
                                     ),
                              column(12, 
                                     h5("This plot shows the size of one image compared to the AOI/DTM"),
                                     leafletOutput("overlap_leaflet")),
                     
                     )
                   ),
                  
                   # This panel is used to Show how the GSD changes
                   # depending on the variation in Flight height
                   tabPanel("GSD Statistics",
                            fluidPage(br(),
                                      h4("GSD Based on Terrain"),
                                      h5("The minimum value of the flight height sliders is being used for the calculations"),
                                      
                                      fluidRow(
                                        h6("Plot showing changes in GSD based on flight parameters and DTM"),
                                        column(12,
                                               
                                               column(6, h4("GSD based on Terrain in AOI"),
                                                      plotOutput("GSD_dtm_plot")),
                                               ),
                                        fluidRow(
                                        column(4,
                                               plotOutput("GSD_dtm_hist")),
                                        column(4,
                                               plotOutput("FEP_hist")),
                                        column(4,
                                               plotOutput("elev_hist")),
                                        )
                                      ), ), ),
                   # This Panel shows two maps of the DTM and the GSD based on inputs
                   tabPanel("DTM and GSD Map",
                            fluidPage(br(),
                                      
                                      fluidRow(
                                        h5("Note: Please load downloaded DTM through interface for it to be shown in the overview plot"),
                                        h5("The minimum value of the flight height sliders is being used for the calculations"),
                                        column(
                                          6,
                                          h4("AOI, DTM and GPX track"),
                                          leafletOutput("DTM",)
                                        ),
                                        column(
                                          6,
                                          h4("GSD depending on the DTM and the Flight Height"),
                                          leafletOutput("GSD_dtm"),
                                          
                                          h5("Values below 10m are shown as NA in red. Flights at this height are not recommended.")
                                        )
                                        
                                      )))
      ))
    
  )
)


# Server ----
server <- function(input, output, session) {
  output$introtext <- renderUI({
    HTML("
    <h1>Welcome to the UAS Calculator!</h1>
    <p>Author: Ina Schulz <a href='https://github.com/schulzina'> github </a> </p>
    <p>This is the UAS Calculator. 
    In this Shiny app, you can use UAS Cameras already in the Database or input another one by choosing 'Other' in the camera selector. The height of the planned flight can be chosen, as well as the required overlap for photogrammetric analysis and the precision used on the sensor. <br></br>
    In the second column of the sidebar, a Digital Terrain Model (DTM) and an Area of Interest (AOI) can be selected. Please note that the latter should be provided as a geopackage file. Any shapefile can easily be transformed into geopackage (.gpkg) using QGIS. 
    The DTM can either be uploaded you or you can choose to download it form the AWS servers using mapzen. For the latter option, please select a name for your file. </p>
  <p>
    <b>Note: ALL data needs to be in EPSG:4326 (WGS84)</b>
    </p>
<p> Above this text, you noticed a couple other tabs, the function of each is explained below: 
<br> </br>
    In the tab <em>'Basic Parameters' </em>, you can see the Ground Sampling Distance (GSD), the Image Height, Width and the area covered by one image based on the inputs you give in the sidebar. 
    There are three plots: one of the overlap between the images based on your inputs from the sidepanel, and another one showing the 
    GSD dependent on both inputs in the flight height slider and a third one, showing the size of one image compared to the entire DTM of the AOI.
<br> </br>
    In the tab <em>'GSD Plot' </em> you can see the changes of the GSD across the chosen DTM. Below, you can see an overview of the occurence of the GSD, the flight height and the elevation.
    The plots can be <em> exported </em> by clicking right and choosing 'save as'.
    This information should help you make informed decisions about where to start the mission in your area of interest.
<br></br>
    In the tab <em> 'DTM and GSD Map'</em> the (down)loaded DTM as well as the calculated GSD raster is displayed. 
  The option of inputing a gpx file and displaying it is also given and displayed.

<br></br>
    Enjoy planning your flights with this shiny applicaion.
<br></br>
    For questions and notes, please contact me at ina.schulz@stud-mail.uni-wuerzburg.de
    
<br></br>
    Date updated: 17.04.2023

</p>

<br> </br>
    
    ")
    
  })
  
  
  
  # create reactive Values
  # define camera specs value
  camera_specs <- reactiveVal(specs, label = "Camera")
  # define DTM reactive value
  dtm <- reactive({
    if (!is.null(input$DTM)) {
     # if(input$clip == F){
        # load dtm
        req(input$DTM)
        raster(input$DTM$datapath)
        # option to clip DTM to AOI, not working in this current version
     # } else {
     #    # clip provided dtm to aoi, if input to do so is given
     #    req(input$DTM, input$AOI)
     #    raster::mask(x = crop(raster(input$DTM$datapath), as(extent(st_read(input$AOI$datpath)), "SpatialPolygons")), 
     #                 as(extent(bbox(st_read(input$AOI$datpath))), "SpatialPolygons"), filename = paste0("DTM/", input$dtm_filename, ".tif"))
     #  }
    } else {
      # download DTM and save it
      if (!is.null(input$DTM_downl) &
          !is.null(input$dtm_filename) & !is.null(input$AOI)) {
        req(input$DTM_downl, input$dtm_filename, input$AOI)
        DTM_download_crop(input$AOI$datapath, input$dtm_filename)
        raster(paste0("DTM/", input$dtm_filename, ".tif"))
      }
    }
  })
  
  
  # define DF in which the GSD over Raster is Calculated
  GSD_df <- reactive({
    dtm_df <- as.data.frame(dtm(), xy = T) 
    names(dtm_df) <- c("lng", "lat", "elev")
    FES <- input$FlightHm[1]
    lat <-  as.numeric(input$lat)
    lng <-  as.numeric(input$lng) 
    p <- SpatialPoints(coords = cbind(lng, lat),
                       proj4string = CRS("EPSG:4326"))
    ES <-  extract(dtm(), p)
    # use lapply on df to calculate FEP
    dtm_df$FEP <- lapply(dtm_df$elev, FEP_calc, FES = FES, ES = ES) %>% as.numeric()
    GSD_df <- dtm_df %>% filter(dtm_df$FEP > 10)
    # calculate GSD
    GSD_df$GSD <- lapply(GSD_df$FEP,
                         GSD,
                         SW = input$SW,
                         FL = input$FL,
                         IW = input$IW) %>% as.numeric()
    GSD_df
  })
  
  # raster of GSD
  GSD_rast <- reactive({
    sp1 <- as.data.frame(GSD_df(), xy=T)
    coordinates(sp1) <- ~ lng + lat
    gridded(sp1) <- TRUE
    GSD_dtm <- raster(sp1, "GSD")
    crs(GSD_dtm) <- CRS('+init=EPSG:4326')
    GSD_dtm
  })

  
# update inputs based on changes by the user  
    
  # update input based on Camera Specified in DF
  observeEvent(input$Camera, {
    # put values of the DF stored in camera_specs() in the Select input values
    # now, the data written in the csv file can be used as input
    updateNumericInput(session, inputId = "SW", value = camera_specs()[camera_specs()$Param == "SensorWidth", input$Camera])
    updateNumericInput(session, inputId = "SH", value = camera_specs()[camera_specs()$Param == "SensorHeight", input$Camera])
    updateNumericInput(session, inputId = "FL", value = camera_specs()[camera_specs()$Param == "FocalLength", input$Camera])
    updateNumericInput(session, inputId = "IW", value = camera_specs()[camera_specs()$Param == "ImageWidth", input$Camera])
    updateNumericInput(session, inputId = "IH", value = camera_specs()[camera_specs()$Param == "ImageHeight", input$Camera])
    
    # Make the right inputs editable based on the cameras selected
    if (!(input$Camera == "Other")) {
      if (!(input$Camera == "NikonZ7II")) {
        # If selection is neither Other or Nikon, disable All ( all values are given)
        shinyjs::disable(id = "SW")
        shinyjs::disable(id = "SH")
        shinyjs::disable(id = "IW")
        shinyjs::disable(id = "IH")
        shinyjs::disable(id = "FL")
      } else {
        # else = it is Nikon, only enable Focal Length
        shinyjs::disable(id = "SW")
        shinyjs::disable(id = "SH")
        shinyjs::disable(id = "IW")
        shinyjs::disable(id = "IH")
        shinyjs::enable(id = "FL")
      }
      
      # If Other is selected, everything is editable
    } else{
      shinyjs::enable(id = "SW")
      shinyjs::enable(id = "SH")
      shinyjs::enable(id = "IW")
      shinyjs::enable(id = "IH")
      shinyjs::enable(id = "FL")
    }
  })
  
  # Set input of Starting Point to Center of AOI / DTM
  observeEvent(input$DTM, {
    center <- center(dtm())
    updateNumericInput(session, inputId = "lng", value = center[1])
    updateNumericInput(session, inputId = "lat", value = center[2])
  })
  observeEvent(input$AOI, {
    center <- center(terra::vect(input$AOI$datapath))
    updateNumericInput(session, inputId = "lng", value = center[1])
    updateNumericInput(session, inputId = "lat", value = center[2])
  })
  
  # commented out because dragging a marker does not work right now
  # observe({
  #   latlng <- input$map_marker_dragged
  #   updateNumericInput(session, inputId = "lat", value = latlng$lat)
  #   updateNumericInput(session, inputId = "lng", value = latlng$lng)
  # })
  
  # Calculate Basic Parameters
  # Calculate Ground Sampling Distance
  GrSaDi <-  reactive(GSD(
    SW = input$SW,
    H = input$FlightHm[1],
    FL = input$FL,
    IW = input$IW
  ))
  
  # Calculate Image Width
  DWidth <-  reactive(DW(
    SW = input$SW,
    H = input$FlightHm[1],
    FL = input$FL,
    IW = input$IW
  ))
  
  # Calculate Image Height
  DHeight <-  reactive(DH(
    SW = input$SW,
    H = input$FlightHm[1],
    FL = input$FL,
    IW = input$IW,
    IH = input$IH
  ))
  
  # Calculate Area Covered
  AreaM2 <- reactive(CoverageM(
    SW = input$SW,
    H = input$FlightHm[1],
    FL = input$FL,
    IW = input$IW,
    IH = input$IH
  ))
  
  # Outputs for Basic Tab ----
  # Create Outputs
  output$HeightInfo <- renderText(c("Calculated based on a flight height of ", input$FlightHm[1], " m."))
  output$GrSaDi <- renderText(c("Ground Sampling Distance :", GrSaDi(), "cm/px"))
  output$DWidth <- renderText(c("Width of each Image :", DWidth(), "meters"))
  output$DHeight <- renderText(c("Height of each Image :", DHeight(), "meters"))
  output$AreaM <- renderText(c(
    "Area covered by each Image :",AreaM2(),"m² or",M2toHa(AreaM2()),"hectars"))
  


 # leaflet plot of the AOI / DTM in comparison to the size of one image.
  # plot overlapping rectangle next to it.
  # possibly: calculate how many images need to be taken to cover the aoi?
  output$overlap_leaflet <- renderLeaflet({
    validate(need((!is.null(input$AOI)|!is.null(dtm())), "Please enter an AOI or DTM"))
    # calcualte the coverage of one image
    x <- calc_coverage_img(input$lat, input$lng, DWidth(), DHeight())
    dtm_local <- dtm()
    df <- as.data.frame(dtm_local, xy = T)
    pal <- colorNumeric(palette ="RdYlBu", domain = values(dtm_local),
                        na.color = "gray", reverse = T)
    # show plot of one image
    leaflet(df) %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
      addTiles(group = "OSM (default") %>% 
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      addScaleBar(position = "bottomleft") %>% 
      addRasterImage(dtm_local, opacity = 0.7, colors = pal) %>% 
      addRectangles(
        lng1 = x$lonw,
        lng2 = x$lone,
        lat1 = x$latn,
        lat2 = x$lats,
        stroke=T
      ) 

  })
  
  
  # ggplot of overlap between two images
  output$ovlplot <- renderPlot({
    validate(need(!is.null(input$FL) , "Please set the focal length of the camera."))
    ggplot(data.frame(x = c(200
                            , 200 + (
                              FoV_W(input$SH, input$FlightHm[1], input$FL, input$SensorP) - (
                                input$SideOvL * FoV_W(input$SH, input$FlightHm[1], input$FL, input$SensorP) /
                                  100
                              )
                            )),
                      y = c(200
                            , 200 + (
                              FoV_H(input$SW, input$FlightHm[1], input$FL, input$SensorP) - (
                                input$FrontOvL * FoV_H(input$SW, input$FlightHm[1], input$FL, input$SensorP) /
                                  100
                              )
                            )))) +
      geom_point(aes(x, y),
                 color = c("darkred", "darkgreen")) +
      geom_rect(
        aes(xmin = x - (FoV_W(input$SH, input$FlightHm[1], input$FL, input$SensorP) / 2),
            xmax =  x + (FoV_W(input$SH, input$FlightHm[1], input$FL, input$SensorP) / 2),
            ymin = y - (FoV_H(input$SW, input$FlightHm[1], input$FL, input$SensorP) / 2),
            ymax = y + (FoV_H(input$SW, input$FlightHm[1], input$FL, input$SensorP) / 2)),
        fill = c("red", "green"),
        alpha = .2) + coord_fixed()
  }, res = 96)
  
  # create a reactive value that includes the min and max Flight Height values and the corresponding GSD
  HGSD <- reactive({
    H <- seq.int(from = input$FlightHm[1],
                 to = input$FlightHm[2],
                 by = 1)
    x <- H
    for (i in 1:length(H)) {
      x[i] <- GSD(input$SW, H[i], input$FL, input$IW)
    }
    data.frame(FlightHm = H, GSD = x)
  })
  
  # Plot the GSD based on input of  flight height sliders
  output$GSDHeightPlot <- renderPlot({
    validate(need(!is.null(input$FL), "Please select all parameters of the camera."))
    ggplot(data = HGSD(), aes(x = FlightHm, y = GSD)) +
      geom_line(linewidth = 2,
                color = "#009988",
                lineend = "round") +
      labs(
        title = paste0("Ground Sampling Distance of ",input$Camera),
        subtitle = "GSD based on given flight height changes.",
        x = "Flight Height [m]",
        y = "Ground Sampling Distance [cm/px]"
      ) +
      geom_vline(
        aes(xintercept = 120),
        show.legend = F
      ) +
      geom_text(mapping = aes(x = 120, y = 0,
                              label = "legal limit of flight height",
                              hjust= -0.1, 
                              vjust= -0.5,
                              angle = 90
      ))+
      theme_minimal()
  },
  width = 500,
  height = 300,
  res = 96)
  
  
  # GSD Statistics Tab ----
  # create GSD Plot with Flight Elevation and Based on DTM
  output$GSD_dtm_plot <- renderPlot({
    validate(need((!is.null(input$DTM) | !is.null(input$AOI)) , "Please select  a DTM or an AOI"))
    df <- as.data.frame(GSD_df(), xy=T)
    H <- seq.int(from = input$FlightHm[1],
                 to = input$FlightHm[2],
                 by = 1)
    x <- H
    for (i in 1:length(H)) {
      x[i] <- GSD(input$SW, H[i], input$FL, input$IW)
    }
    HGSD <- data.frame(FEP = H, GSD_input = x)
    df_combined <- left_join(df, HGSD, by = "FEP")
    ggplot(df_combined, aes(FEP)) +
      geom_line(aes(y = GSD_input,
                    color = "GSD Input"),
                linewidth = 5,
                lineend = "round") +
      geom_line(aes(y = GSD,
                    color = "GSD"),
                linewidth = 1,
                lineend = "round") +
      labs(
        title = paste0("Ground Sampling Distance of ",input$Camera),
        subtitle = "GSD based on flight height changes due to terrain",
        x = "Flight Height [m]",
        y = "Ground Sampling Distance [cm/px]"
      ) +
      geom_vline(
        aes(xintercept = 120),
        show.legend = F
      ) +
      geom_text(mapping = aes(x = 120, y = 0,
                              label = "legal limit of flight height",
                              hjust= -0.1, 
                              vjust= -0.5,
                              angle = 90
      ))+
      theme_minimal()},
    width = 500,
    height = 300,
    res=96)
  
  # plot Histogram of GSD of AOI
  output$GSD_dtm_hist <- renderPlot({
    validate(need((!is.null(input$DTM) | !is.null(input$AOI)) , "Please select  a DTM or an AOI"))
    df <- as.data.frame(GSD_df(), xy=T)
    ggplot(df) +
      geom_histogram(aes(x = GSD),
                     binwidth = 0.1,
                     color ="black", 
                     fill ="black") +
      labs(
        title = paste0("GSD of ",input$Camera),
        x = "GSD [cm/px]",
        y = "Frequency"
      ) +
      theme_minimal()},
    width = 270,
    height = 200,
    res=96)
  
  # plot flight height
  output$FEP_hist <- renderPlot({
    validate(need((!is.null(input$DTM) | !is.null(input$AOI)) , "Please select  a DTM or an AOI"))
    df <- as.data.frame(GSD_df(), xy=T)
    ggplot(df) +
      geom_histogram(aes(x = FEP),
                     binwidth = 1,
                     color ="black", 
                     fill ="black") +
      labs(
        title = paste0("Flight Height ",input$Camera),
        x = "Flight Height [m]",
        y = "Frequency"
      ) +
      theme_minimal()},
    width = 270,
    height = 200,
    res=96)
  
  # plot elevation given in dtm
  output$elev_hist <- renderPlot({
    validate(need((!is.null(input$DTM) | !is.null(input$AOI)) , "Please select  a DTM or an AOI"))
    df <- as.data.frame(GSD_df(), xy=T)
    ggplot(df) +
      geom_histogram(aes(x = elev),
                     binwidth = 1,
                     color ="black", 
                     fill ="black") +
      labs(
        title = paste0("Elevation ",input$Camera),
        x = "Elevation [m]",
        y = "Frequency"
      ) +
      theme_minimal()},
    width = 270,
    height = 200,
    res=96)
  
  # GSD DTM Map ----
  # GSD in DTM
  # # create a plot made of the Downloaded DTM and the GPX track changed into a spatial data frame
  output$DTM <- renderLeaflet({
    validate(need((!is.null(input$DTM) | !is.null(input$AOI)) , "Please select a DTM or an AOI."))
    if (!is.null(input$DTM_downl) |
        !is.null(input$DTM) |
        !is.null(input$AOI) |
        !is.null(input$GPX)) {
      if (!is.null(input$GPX) &
          (!is.null(input$DTM_downl) |
           !is.null(input$DTM)) &
          is.null(input$AOI)) {
        dtm_local <- dtm()
        df <- as.data.frame(dtm_local, xy = T)
        pal <- colorNumeric(palette = "RdYlBu", domain = values(dtm_local),
                            na.color = "gray",reverse = T)
        x <- gpx_to_SPDF(input$GPX$datapath)
        leaflet(df) %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
          addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
          addTiles(group = "OSM (default") %>% 
          addLayersControl(
            baseGroups = c("OSM (default)", "Satellite"),
            options = layersControlOptions(collapsed = FALSE)) %>%
          addRasterImage(dtm_local, opacity = 0.7,colors = pal) %>%
          addCircleMarkers(
            lng = x$lon,
            lat = x$lat,
            radius = 1,
            opacity = 0.7,
            fillColor = "blue"
          ) %>%
          addScaleBar(position = "bottomleft")%>%
          addLegend("bottomright",
                    pal = pal,
                    values = values(dtm_local),
                    title =("Elev. [m]"))
        # input is only gpx
      } else if (!is.null(input$GPX) &
                 (is.null(input$DTM_downl) |
                  is.null(input$DTM)) & is.null(input$AOI)) {
        x <- gpx_to_SPDF(input$GPX$datapath)
        if (is.data.frame(x)) {
          leaflet(x) %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
            addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
            addTiles(group = "OSM (default") %>% 
            addLayersControl(
              baseGroups = c("OSM (default)", "Satellite"),
              options = layersControlOptions(collapsed = FALSE)) %>%
            addCircleMarkers(
              lng = x$lon,
              lat = x$lat,
              radius = 1,
              opacity = 0.7,
              fillColor = "blue"
            ) %>%
            addScaleBar(position = "bottomleft")
        } else{
          leaflet() %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
            addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
            addTiles(group = "OSM (default") %>% 
            addLayersControl(
              baseGroups = c("OSM (default)", "Satellite"),
              options = layersControlOptions(collapsed = FALSE)) %>%
            addScaleBar(position = "bottomleft")
        }
        #input is only DTM
      } else if ((!is.null(input$DTM_downl) |
                  !is.null(input$DTM)) &
                 is.null(input$GPX) & is.null(input$AOI)) {
        dtm_local <- dtm()
        df <- as.data.frame(dtm_local, xy = T)
        pal <- colorNumeric(palette ="RdYlBu", 
                            domain = values(dtm_local),
                            na.color = "gray",
                            reverse = T)
        leaflet(df) %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
          addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
          addTiles(group = "OSM (default") %>% 
          addLayersControl(
            baseGroups = c("OSM (default)", "Satellite"),
            options = layersControlOptions(collapsed = FALSE)) %>%
          addRasterImage(dtm_local, opacity = 0.7, colors = pal) %>%
          addScaleBar(position = "bottomleft") %>%
          addLegend("bottomright",
                    pal = pal,
                    values = values(dtm_local),
                    title =("Elev. [m]"))
        #only input is AOI
      } else if ((is.null(input$DTM_downl) |
                  is.null(input$DTM)) &
                 is.null(input$GPX) & !is.null(input$AOI)) {
        aoi <- st_read(input$AOI$datapath)
        leaflet(aoi) %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
          addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
          addTiles(group = "OSM (default") %>% 
          addLayersControl(
            baseGroups = c("OSM (default)", "Satellite"),
            options = layersControlOptions(collapsed = FALSE)) %>%
          addPolygons(data = aoi,
                      opacity = 0.8,
                      fillColor = "grey") %>%
          addScaleBar(position = "bottomleft")
        # DTM and AOI
      } else if (is.null(input$GPX) &
                 (!is.null(input$DTM_downl) |
                  !is.null(input$DTM)) & !is.null(input$AOI)) {
        aoi <- st_read(input$AOI$datapath)
        dtm_local <- dtm()
        df <- as.data.frame(dtm_local, xy = T)
        pal <- colorNumeric(palette = "RdYlBu", domain = values(dtm_local),
                            na.color = "gray", reverse = T)
        leaflet(aoi) %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
          addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
          addTiles(group = "OSM (default") %>% 
          addLayersControl(
            baseGroups = c("OSM (default)", "Satellite"),
            options = layersControlOptions(collapsed = FALSE)) %>%
          addRasterImage(dtm_local, opacity = 0.7, colors = pal) %>%
          addPolygons(data = aoi,
                      opacity = 0.8,
                      fillColor = "grey") %>%
          addScaleBar(position = "bottomleft")%>%
          addLegend("bottomright",
                    pal = pal,
                    values = values(dtm_local),
                    title =("Elev. [m]"))
        # DTM, GPX and AOI
      } else if (!is.null(input$GPX) &
                 (!is.null(input$DTM_downl) |
                  !is.null(input$DTM)) & !is.null(input$AOI)) {
        aoi <- st_read(input$AOI$datapath)
        dtm_local <- dtm()
        df <- as.data.frame(dtm(), xy = T)
        pal <- colorNumeric(palette = "RdYlBu", domain = values(dtm_local),
                            na.color = "gray", reverse = T)
        x <- gpx_to_SPDF(input$GPX$datapath)
        if (is.data.frame(x)) {
          leaflet(df) %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
            addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
            addTiles(group = "OSM (default") %>% 
            addLayersControl(
              baseGroups = c("OSM (default)", "Satellite"),
              options = layersControlOptions(collapsed = FALSE)) %>%
            addRasterImage(dtm_local, opacity = 0.7, colors = pal) %>%
            addPolygons(
              data = aoi,
              opacity = 0.8,
              fillColor = "grey"
            ) %>%
            addCircleMarkers(
              lng = x$lon,
              lat = x$lat,
              radius = 1,
              opacity = 0.7,
              fillColor = "blue"
            ) %>%
            addScaleBar(position = "bottomleft")  %>%
            addLegend("bottomright",
                      pal = pal,
                      values = values(dtm_local),
                      title =("Elev. [m]"))
        } else{
          leaflet(df) %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
            addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
            addTiles(group = "OSM (default") %>% 
            addLayersControl(
              baseGroups = c("OSM (default)", "Satellite"),
              options = layersControlOptions(collapsed = FALSE)) %>%
            addRasterImage(dtm_local, opacity = 0.7, colors = pal) %>%
            addPolygons(
              data = aoi,
              opacity = 0.8,
              fillColor = "grey"
            ) %>% 
            addScaleBar(position = "bottomleft")  %>%
            addLegend("bottomright",
                      pal = pal,
                      values = values(dtm_local),
                      title =("Elev. [m]"))
        }
      }
    } else {
      leaflet() %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
        addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
        addTiles(group = "OSM (default") %>% 
        addLayersControl(
          baseGroups = c("OSM (default)", "Satellite"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        addScaleBar(position = "bottomleft")
    }
  })
  
  
  # calculate GSD for raster from DTM and Flight Height
  # GSD Plot over DTM
  output$GSD_dtm <- renderLeaflet({
    validate(need(input$FL != "", "Please input all parameters of the camera."),
             need(!is.null(dtm()), "Please select a DTM."))
    if (!is.null(GSD_df())) {
      GSD_df <- as.data.frame(GSD_df(), xy=T)
      GSD <- GSD_rast()
      # create plot
      pal <- colorNumeric(
        palette = "YlGnBu",
        domain = values(GSD),
        na.color = "red"
      )
      # make leaflet plot
      leaflet(GSD_df[, 1:2]) %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
        addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
        addTiles(group = "OSM (default") %>% 
        addLayersControl(
          baseGroups = c("OSM (default)", "Satellite"),
          options = layersControlOptions(collapsed = FALSE))%>%
        addRasterImage(GSD, opacity = 0.7, colors = pal) %>%
        addCircleMarkers(
          p,
          lng = as.numeric(input$lng),
          lat = as.numeric(input$lat),
          radius = 2,
          color = "red"
        ) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = values(GSD),
                  title = "GSD [cm/px]") %>%
        addScaleBar(position = "bottomleft")
    } else {
      leaflet() %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
        addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
        addTiles(group = "OSM (default") %>% 
        addLayersControl(
          baseGroups = c("OSM (default)", "Satellite"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        addScaleBar(position = "bottomleft")
    }
    
  })
  
  # export output of GSD raster
  observeEvent(input$export, {
    output$export_gsd <- renderText({
      validate(need(!is.null(dtm()), "Please input a DTM."))
      if(input$export == TRUE){ 
        GSD <- GSD_rast()
        dir.create("output/")
        terra::writeRaster(GSD, filename = paste0("output/", input$raster_filename, ".tif"), filetype = "GTiff", overwrite = T)
        label <- paste0("The raster of the GSD of the AOI has been saved to :'", 
                        getwd(), "output/", input$raster_filename, ".tif'." )
        label
      } else {
        if(input$export == FALSE){label <- "The raster has not been saved."
        label}}
    })
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)




# Notes:
#' 
#' overall:
#' add calculation of how many images are needed depending on chosen overlap
#'    calc area of aoi
#'    
#' add shinytheme in a working manner
#' choose directory with shinydirbutton and shinydirchoose
#' button for exporting and if you want do download instead of checkbox
#'        this is reactive and calls a function every time it is called
#' 
#' leaflet:
#' Drag and Drop Starting Point
#'    theres 6 marker
#'    none of them change the input for the calculation
#' slider for transparency in leaflet plot
#' connect all leaflet plots into one plot
#' add leaflet layers to target groups in order to turn them on and off again (see trial below)
#' put NA values in DTM raster to red/crashing
#' 
#' 
#' 
#' ggplot
#' label values in DTM and Camera GSD plot, so they are easily seen
#' 
#' 
#' ## this plot is trying to add all leaflet plots together and making the layers clickable  
# output$DTM_map_groups <- renderLeaflet({
#   aoi <- if(!is.null(input$AOI)){st_read(input$AOI$datapath)}else{NULL}
#   dtm_local <- if(!is.null(dtm())){dtm()}else{NULL}
#   df <- if(!is.null(dtm())){as.data.frame(dtm(), xy = T)}else{NULL}
#   pal <- colorNumeric(palette = rev("RdYlBu"), domain = values(dtm_local),na.color = "gray")
#   x <- if(!is.null(input$GPX)){gpx_to_SPDF(input$GPX$datapath)}else{NULL}
#   
#   
#   leaflet(df) %>% addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
#     addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite") %>% 
#     addTiles(group = "OSM (default") %>% 
#     addRasterImage(dtm_local, opacity = 0.7, colors = pal, group = "DTM")
#     addPolygons(data = aoi,opacity = 0.8,fillColor = "grey", group = "AOI") %>%
#     addCircleMarkers(lng = x$lon,lat = x$lat,radius = 1,opacity = 0.7,fillColor = "lightblue", group = "GPX") %>%
#     addScaleBar(position = "bottomleft")  %>%
#     addLegend("bottomright",
#               pal = pal,
#               values = values(dtm_local),
#               title =("Elev. [m]")) %>% 
#     addLayersControl(
#       baseGroups = c("OSM (default)", "Satellite"),
#       overlayGroups = c("AOI", "DTM", "GPX"),
#       options = layersControlOptions(collapsed = FALSE))
# })

