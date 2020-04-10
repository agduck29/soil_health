library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinythemes)
library(corrplot)
library(DT)
library(ggfortify)
library(ggplot2)
library(hashids)
library(leaflet)
library(leafpop)
library(tidyverse)
library(rsconnect)

rsconnect::setAccountInfo(name='agduck29', token='FD9F192A4EC49AB82BA160877756E44B', secret='w7IwYf0IvQFoP2VvfrK04qQ6bzqHcORE5/oijHx5')

#setwd when testing, remove when using "Run App"
#setwd("C:/Student5/Shiny/soil_health")

#source for lightbox styel image gallery
source("src/lightbox.R")

#All data from samplings
Soil_Health <- read.csv("R_Soil_Health.csv")
#Glossary for data metrics
sh_gloss <- read.csv("R_sh_gloss.csv")
colnames(sh_gloss)[1] <- "Variable"
#change values from factors to charactars
sh_gloss[] <- lapply(sh_gloss, as.character)

#manipulate data for shortened display
colnames(Soil_Health)[1] <- "Sample.Date"
Soil_Health <- Soil_Health %>% 
  dplyr::rename(X.ID1 = ID1, #REMOVE 2
                Sample.Loc = ID2,
                N.Haney = N.lbs.per.acre,
                P.Haney = P2O5.lbs.per.acre,
                K.Haney = K2O.lbs.per.acre,
                X.Nut.Val = Nutrient.value.per.acre.., #REMOVE 7
                NO3.70 = NO3.N.Only.lbs.per.acre.70.,
                N.Add = Additional.N.lbs.per.acre,
                x.N.Save = X..nitrogen.saved..per.acre, #REMOVE 10
                CO2.Day = X1.day.CO2.C,
                C.Org = Organic.C,
                N.Org = Organic.N,
                C.N = Organic.C.N,
                S.H.C. = Soil.Health.Calculation,
                X.Cov.Crop = Cover.crop.mix, #REMOVE 16
                N.Tot = Total.Nitrogen.lbs.acre,
                N.Inorg = Inorganic.N,
                X.N.Org2 = Organic.N.1, #REMOVE DUPLICATE 19
                P.Tot = Total.Phosphate..lbs.acre,
                P.Inorg = Inorganic.P,
                P.Org = Organic.P,
                Al.H = Al.ppm,
                Fe.H = Fe.ppm,
                Ca.H = Ca.ppm,
                N.Org.Rel = Organic.N.release,
                N.Org.Res = Organic.N.reserve,
                P.Org.Rel = Organic.P.release,
                P.Org.Res = Organic.P.reserve,
                P.Sat.Per = X..P.sat,
                #Ca.AlFe
                X.SHC2 = SHC, #REMOVE DUPLICATE 32
                X.N.Sav2 = Nitrogen.Saved.., #REMOVE DUPLICATE 33
                #pH
                X.buffer.pH = buffer.pH, #REMOVE 35
                X.Lime = Lime, #REMOVE 36
                #NO3
                #Fe
                #Mn
                #Zn
                #Cu
                #P
                #K
                #Ca
                #Mg
                #S
                #Na
                #EC
                Bio.Tot = Total.Biomass,
                Div.Ind = Diversity.Index,
                Bac.Tot.Per = Bacteria..,
                Bac.Tot.Bio = Total.Bacteria.Biomass,
                Act.Per = Actinomycetes..,
                Act.Bio = Actinomycetes.Biomass,
                Grm.N.Per = Gram......,
                Grm.N.Bio = Gram.....Biomass,
                Rhi.Per = Rhizobia..,
                Rhi.Bio = Rhizobia.Biomass,
                Fun.Tot.Per = Total.Fungi..,
                Fun.Tot.Bio = Total.Fungi.Biomass,
                Arb.Myc.Per = Arbusular.Mycorrhizal..,
                Arb.Myc.Bio = Arbuscular.Mycorrhizal.Biomass,
                Sap.Per = Saprophytic..,
                Sap.Bio = Saprophytes.Biomass,
                Pro.Per = Protozoan..,
                Pro.Bio = Protozoa.Biomass,
                Grm.P.Bio = Gram.....Biomass.1,
                Grm.P.Per = Gram.......1,
                Undif.Per = Undifferentiated..,
                Undif.Bio = Undifferentiated.Biomass,
                Fun.Bac = Fungi.Bacteria,
                Pred.Prey = Predator.Prey,
                Grm.P.Grm.N = Gram....Gram...,
                #Sat.Unsat
                #Mono.Poly
                Pre.16 = Pre.16.1w7c.cy17.0,
                Pre.18 = Pre.18.1w7c.cy19.0
  )

#remove duplicate and unwanted columns
Soil_Health <- select(Soil_Health, -2, -7, -10, -16, -19, -32, -33, -35, -36)

#change column order
Soil_Health <- Soil_Health[ , c(
  "Sample.Date",
  "Sample.Loc",
  "CO2.Day",
  "C.Org",
  "C.N",
  "N.Haney",
  "NO3.70",
  "N.Add",
  "N.Tot",
  "N.Inorg",
  "N.Org",
  "N.Org.Rel",
  "N.Org.Res",
  "P.Haney",
  "P.Tot",
  "P.Inorg",
  "P.Org",
  "P.Org.Rel",
  "P.Org.Res",
  "P.Sat.Per",
  "K.Haney",
  "Al.H",
  "Ca.H",
  "Fe.H",
  "Ca.AlFe",
  "S.H.C.",
  "pH",
  "NO3",
  "P",
  "K",
  "Ca",
  "Mg",
  "S",
  "Fe",
  "Mn",
  "Zn",
  "Cu",
  "Na",
  "EC",
  "Div.Ind",
  "Bio.Tot",
  "Bac.Tot.Bio",
  "Grm.P.Bio",
  "Act.Bio",
  "Grm.N.Bio",
  "Rhi.Bio",
  "Fun.Tot.Bio",
  "Arb.Myc.Bio",
  "Sap.Bio",  
  "Pro.Bio",
  "Undif.Bio",
  "Bac.Tot.Per",
  "Grm.P.Per",
  "Act.Per",
  "Grm.N.Per",
  "Rhi.Per",
  "Fun.Tot.Per",
  "Arb.Myc.Per",
  "Sap.Per",
  "Pro.Per",
  "Undif.Per",
  "Fun.Bac",
  "Pred.Prey",
  "Grm.P.Grm.N",
  "Sat.Unsat",
  "Mono.Poly",
  "Pre.16",
  "Pre.18"  
)
]

#convert lbs/ac to ppm (lbs/ac divided by 2 = ppm)
Soil_Health$N.Haney <- Soil_Health$N.Haney / 2
Soil_Health$P.Haney <- Soil_Health$P.Haney / 2
Soil_Health$K.Haney <- Soil_Health$K.Haney / 2
Soil_Health$NO3.70 <- Soil_Health$NO3.70 / 2
Soil_Health$N.Add <- Soil_Health$N.Add / 2
Soil_Health$N.Tot <- Soil_Health$N.Tot / 2
Soil_Health$N.Inorg <- Soil_Health$N.Inorg / 2
Soil_Health$P.Tot <- Soil_Health$P.Tot / 2
Soil_Health$P.Inorg <- Soil_Health$P.Inorg / 2
Soil_Health$P.Org <- Soil_Health$P.Org / 2
Soil_Health$N.Org.Rel <- Soil_Health$N.Org.Rel / 2
Soil_Health$N.Org.Res <- Soil_Health$N.Org.Res / 2
Soil_Health$P.Org.Rel <- Soil_Health$P.Org.Rel / 2
Soil_Health$P.Org.Res <- Soil_Health$P.Org.Res / 2

#change data format from factor
Soil_Health$NO3 <- as.numeric(Soil_Health$NO3)

#factors in Fun.Bac, Pred.Prey, Mono.Poly, Pre.16, Pre.18
#replace Div/0 errors to min (0) or max of variable
Soil_Health$Fun.Bac <- as.character(Soil_Health$Fun.Bac)
Soil_Health$Fun.Bac[Soil_Health$Fun.Bac == "ALL BACTERIA"] <- "0"
Soil_Health$Fun.Bac <- as.numeric(Soil_Health$Fun.Bac)

Soil_Health$Pred.Prey <- as.character(Soil_Health$Pred.Prey)
Soil_Health$Pred.Prey[Soil_Health$Pred.Prey == "ALL PREY"] <- "0"
Soil_Health$Pred.Prey <- as.numeric(Soil_Health$Pred.Prey)

Soil_Health$Mono.Poly <- as.character(Soil_Health$Mono.Poly)
Soil_Health$Mono.Poly[Soil_Health$Mono.Poly == "ALL MONO"] <- "230"
Soil_Health$Mono.Poly <- as.numeric(Soil_Health$Mono.Poly)

Soil_Health$Pre.16 <- as.character(Soil_Health$Pre.16)
Soil_Health$Pre.16[Soil_Health$Pre.16 == "NONE FOUND"] <- "0"
Soil_Health$Pre.16[Soil_Health$Pre.16 == "ALL PRE 16:1"] <- "20"
Soil_Health$Pre.16 <- as.numeric(Soil_Health$Pre.16)

Soil_Health$Pre.18 <- as.character(Soil_Health$Pre.18)
Soil_Health$Pre.18[Soil_Health$Pre.18 == "ALL CYCLO 19:0"] <- "0"
Soil_Health$Pre.18[Soil_Health$Pre.18 == "ALL PRE 18:1"] <- "65"
Soil_Health$Pre.18 <- as.numeric(Soil_Health$Pre.18)

#only numeric data
sh_numeric <- select(Soil_Health, -1, -2)
sh_choice <- names(sh_numeric)

#build image gallery with Lightbox style
images <<- data.frame(src = list.files('www/img')) %>%
  tidyr::separate(col = "src", c("Sample.Date", "Sample.Loc", "Img.Num"), sep = "_|\\.", remove = FALSE) %>%
  rowwise() %>%
  mutate(key = hashids::encode(as.integer(Img.Num), hashid_settings(salt = "this is my salt")))

#user interface (ui)
ui <- dashboardPage(
  dashboardHeader(title = "Soil Health Study"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("seedling")),
      menuItem("Boxplots", tabName = "boxplots", icon = icon("poll")),
      menuItem("Correlation", tabName = "corrplot", icon = icon("cubes")),
      menuItem("Images", tabName = "gallery", icon = icon("images")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("PCA", tabName = "pca", icon = icon("asterisk")),
      menuItem("Raw Data", tabName = "raw", icon = icon("table")),
      menuItem("User Discussion", tabName = "user", icon = icon("university"),
               menuSubItem("Hart", tabName = "hart")
               )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              fluidRow(
                column(6,
                       includeHTML("intro.html")
                )
              )
      ),
      tabItem(tabName = "boxplots",
              fluidPage(
                pageWithSidebar(
                  headerPanel("Boxplots"),
                  sidebarPanel(
                    checkboxGroupInput("checkGroup",
                                       h5(strong("Sampling Date")),
                                       choices = list("2018 Winter" = "1Win18",
                                                      "2018 Spring" = "2Spr18",
                                                      "2018 Summer" = "3Sum18",
                                                      "2018 Fall" = "4Fal18",
                                                      "2019 Winter" = "5Win19",
                                                      "2019 Spring" = "6Spr19",
                                                      "2019 Summer" = "7Sum19",
                                                      "2019 Fall" = "8Fal19",
                                                      "2020 Winter" = "9Win20"),
                                       selected = c("1Win18",
                                                    "2Spr18",
                                                    "3Sum18",
                                                    "4Fal18",
                                                    "5Win19",
                                                    "6Spr19",
                                                    "7Sum19",
                                                    "8Fal19",
                                                    "9Win20")
                    ),
                    selectInput("ycol", "Variable", colnames(sh_numeric)),
                    strong(textOutput("unitOutput")),
                    strong(textOutput("labOutput")),
                    br(),
                    strong("Definition:"),
                    textOutput("defineOutput"),
                    width = 3),
                  mainPanel(
                    plotOutput("boxPlot", width = "100%", height = "800px")
                  )
                )
              )
      ),
      tabItem(tabName = "corrplot",
              fluidRow(
                column(width = 2,
                       bsCollapse(open = "Options",
                                  bsCollapsePanel("Haney",
                                                  checkboxGroupInput("corrCheck1",
                                                                     "Do not uncheck date & location",
                                                                     choices = names(Soil_Health[1:26]),
                                                                     selected = c("Sample.Date",
                                                                                  "Sample.Loc"))
                                  ),
                                  bsCollapsePanel("SFA",
                                                  checkboxGroupInput("corrCheck2",
                                                                     "Variable",
                                                                     choices = sh_choice[25:37])
                                  ),
                                  bsCollapsePanel("Ward",
                                                  checkboxGroupInput("corrCheck3",
                                                                     "Variable",
                                                                     choices = sh_choice[38:66],
                                                                     selected = sh_choice[38:43])
                                  ),
                                  bsCollapsePanel("Sampling Date & Location",
                                                  checkboxGroupInput("corrDate",
                                                                     "Date",
                                                                     choices = list("2018 Winter" = "1Win18",
                                                                                    "2018 Spring" = "2Spr18",
                                                                                    "2018 Summer" = "3Sum18",
                                                                                    "2018 Fall" = "4Fal18",
                                                                                    "2019 Winter" = "5Win19",
                                                                                    "2019 Spring" = "6Spr19",
                                                                                    "2019 Summer" = "7Sum19",
                                                                                    "2019 Fall" = "8Fal19",
                                                                                    "2020 Winter" = "9Win20"),
                                                                     selected = c("1Win18",
                                                                                  "2Spr18",
                                                                                  "3Sum18",
                                                                                  "4Fal18",
                                                                                  "5Win19",
                                                                                  "6Spr19",
                                                                                  "7Sum19",
                                                                                  "8Fal19",
                                                                                  "9Win20")
                                                  ),
                                                  checkboxGroupInput("corrLoc",
                                                                     "Location",
                                                                     choices = unique(sort(Soil_Health$Sample.Loc)),
                                                                     selected = unique(Soil_Health$Sample.Loc))
                                  ),
                                  bsCollapsePanel("Options",
                                           radioButtons("corrOrder",
                                                        "Order",
                                                        choices = list("Alphabetical" = "alphabet",
                                                                       "First PC Order" = "FPC",
                                                                       "Hierarchial" = "hclust"),
                                                        selected = "hclust"),
                                           radioButtons("corrMeth",
                                                        "Method",
                                                        choices = list("Circle" = "circle",
                                                                       "Color" = "color",
                                                                       "Number" = "number",
                                                                       "Square" = "square"),
                                                        selected = "color"),
                                           numericInput("corrSize",
                                                        "Text Size",
                                                        value = 2),
                                           sliderInput("corrColor",
                                                       "Text Color",
                                                       min = 0, max = 10, value = 2)
                                  )
                       )
                ),
                column(width = 10,
                       plotOutput("corrPlot", width = "100%", height = "800px")
                )
              )
      ),
      tabItem(tabName = "gallery",
              fluidRow(uiOutput("lb")
              )
      ),
      tabItem(tabName = "map",
              headerPanel("Sampling Locations"),
              mainPanel(
                leafletOutput("mapOutput", width = "100%", height = "800px")
              )
      ),
      tabItem(tabName = "pca",
              fluidRow(
                column(width = 2,
                       bsCollapse(open = "Options",
                                  bsCollapsePanel("Haney",
                                                  checkboxGroupInput("pcaCheck1",
                                                                     "Do not uncheck date & location",
                                                                     choices = names(Soil_Health[1:26]),
                                                                     selected = c("Sample.Date",
                                                                                  "Sample.Loc"))
                                  ),
                                  bsCollapsePanel("SFA",
                                                  checkboxGroupInput("pcaCheck2",
                                                                     "Variable",
                                                                     choices = sh_choice[25:37])
                                  ),
                                  bsCollapsePanel("Ward",
                                                  checkboxGroupInput("pcaCheck3",
                                                                     "Variable",
                                                                     choices = sh_choice[38:66],
                                                                     selected = sh_choice[38:43])
                                  ),
                                  bsCollapsePanel("Sampling Date & Location",
                                                  checkboxGroupInput("pcaDate",
                                                                     "Date",
                                                                     choices = list("2018 Winter" = "1Win18",
                                                                                    "2018 Spring" = "2Spr18",
                                                                                    "2018 Summer" = "3Sum18",
                                                                                    "2018 Fall" = "4Fal18",
                                                                                    "2019 Winter" = "5Win19",
                                                                                    "2019 Spring" = "6Spr19",
                                                                                    "2019 Summer" = "7Sum19",
                                                                                    "2019 Fall" = "8Fal19",
                                                                                    "2020 Winter" = "9Win20"),
                                                                     selected = c("1Win18",
                                                                                  "2Spr18",
                                                                                  "3Sum18",
                                                                                  "4Fal18",
                                                                                  "5Win19",
                                                                                  "6Spr19",
                                                                                  "7Sum19",
                                                                                  "8Fal19",
                                                                                  "9Win20")
                                                  ),
                                                  checkboxGroupInput("pcaLoc",
                                                                     "Location",
                                                                     choices = unique(sort(Soil_Health$Sample.Loc)),
                                                                     selected = unique(Soil_Health$Sample.Loc)
                                                  )
                                  ),
                                  bsCollapsePanel("Options",
                                                  radioButtons("groupby", h5(strong("Group by")),
                                                               choices = list("Location" = "Sample.Loc",
                                                                              "Date" = "Sample.Date")
                                                  ),
                                                  h5(strong("PCA Options")),
                                                  checkboxInput("frame","Frames", value = TRUE),  
                                                  checkboxInput("eigen","Eigenvectors", value = FALSE),
                                                  checkboxInput("load.label","Labels", value = FALSE),
                                                  numericInput("pcalabsize", "Label Size", value = 3),
                                                  numericInput("pcaNum", "Point Size", value = 3)
                                  )
                       )
                ),
                column(width = 10,
                       plotOutput("pcaPlot", width = "100%", height = "800px")
                )
              )
      ),
      tabItem(tabName = "raw",
              fluidRow(
                column(width = 2,
                       bsCollapse(open = "Sampling Date & Location",
                                  bsCollapsePanel("Haney",
                                                  checkboxGroupInput("rawCheck1",
                                                                     "Variables",
                                                                     choices = names(Soil_Health[1:26]),
                                                                     selected = c("Sample.Date",
                                                                                  "Sample.Loc"))
                                  ),
                                  bsCollapsePanel("SFA",
                                                  checkboxGroupInput("rawCheck2",
                                                                     "Variable",
                                                                     choices = sh_choice[25:37],
                                                                     selected = sh_choice[25:37])
                                  ),
                                  bsCollapsePanel("Ward",
                                                  checkboxGroupInput("rawCheck3",
                                                                     "Variable",
                                                                     choices = sh_choice[38:66])
                                  ),
                                  bsCollapsePanel("Sampling Date & Location",
                                                  checkboxGroupInput("rawDate",
                                                                     "Date",
                                                                     choices = list("2018 Winter" = "1Win18",
                                                                                    "2018 Spring" = "2Spr18",
                                                                                    "2018 Summer" = "3Sum18",
                                                                                    "2018 Fall" = "4Fal18",
                                                                                    "2019 Winter" = "5Win19",
                                                                                    "2019 Spring" = "6Spr19",
                                                                                    "2019 Summer" = "7Sum19",
                                                                                    "2019 Fall" = "8Fal19",
                                                                                    "2020 Winter" = "9Win20"),
                                                                     selected = c("1Win18",
                                                                                  "2Spr18",
                                                                                  "3Sum18",
                                                                                  "4Fal18",
                                                                                  "5Win19",
                                                                                  "6Spr19",
                                                                                  "7Sum19",
                                                                                  "8Fal19",
                                                                                  "9Win20")
                                                  ),
                                                  checkboxGroupInput("rawLoc",
                                                                     "Location",
                                                                     choices = unique(sort(Soil_Health$Sample.Loc)),
                                                                     selected = unique(Soil_Health$Sample.Loc)
                                                  )
                                  )
                       )
                ),
                column(width = 10,
                       dataTableOutput("rawtable")
                )
              )
      ),
      tabItem(tabName = "hart",
              fluidRow(
                column(7,
                       includeHTML("userHart.html")
                )
              )
      )
    )
  )
)

#server
server <- function(input, output) {
  #boxplots
  filterData <- reactive({
    Soil_Health %>% dplyr::filter(Sample.Date %in% c(input$checkGroup))
  })
  selectedData <- reactive({
    filterData() %>% select(Sample.Loc, input$ycol)
  })
  output$boxPlot <- renderPlot({
    ggplot(selectedData(), aes(x = Sample.Loc, y = selectedData()[, 2], fill = Sample.Loc)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("Sampling Location") + 
      ylab(input$ycol) +
      labs(fill = "Legend") +
      theme(axis.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  #image gallery
  output$lb <- renderUI({
    lightbox_gallery(images, "lb-gallery", display = TRUE)
  })
  #correlation matrix
  corrDate <- reactive({
    Soil_Health %>%
      dplyr::filter(Sample.Date %in% c(input$corrDate))
  })
  corrLoc <- reactive({
    corrDate() %>% 
      dplyr::filter(Sample.Loc %in% c(input$corrLoc))
  })
  corrSelect <- reactive({
    corrLoc() %>% 
      select(c(input$corrCheck1, input$corrCheck2, input$corrCheck3))
  })
  corrOmit <- reactive({
    select(corrSelect(), -1, -2) %>% 
      na.omit() %>% 
      cor()
  })
  output$corrPlot <- renderPlot({
    corrplot(corrOmit(), 
             method = input$corrMeth,
             type = "upper",
             order = input$corrOrder,
             number.cex = input$corrSize, 
             addCoef.col = input$corrColor)
  })
  #DT raw data tables
  rawFilter <- reactive({
    Soil_Health %>% 
      dplyr::filter(Sample.Date %in% c(input$rawDate)) %>% 
      dplyr::filter(Sample.Loc %in% c(input$rawLoc))
  })
  rawSelect <- reactive({
    rawFilter() %>% 
      select(c(input$rawCheck1, input$rawCheck2, input$rawCheck3))
  })
  output$rawtable <- renderDataTable({
    datatable(rawSelect(), rownames = FALSE, options = list(lengthMenu = FALSE, pageLength = 20)) %>% 
      formatRound(c(3:68), 2)
  })
  #definition under boxplot variables
  filterUnit <- reactive({
    sh_gloss %>% dplyr::filter(sh_gloss$Variable == input$ycol)
  })
  selectUnit <- reactive({
    filterUnit() %>% select(Units)
  })
  output$unitOutput <- renderText({
    paste("Units: ", selectUnit())
  })
  filterLab <- reactive({
    sh_gloss %>% dplyr::filter(sh_gloss$Variable == input$ycol)
  })
  selectLab <- reactive({
    filterLab() %>% select(Laboratory)
  })
  output$labOutput <- renderText({
    paste("Laboratory: ", selectLab())
  })
  filterDef <- reactive({
    sh_gloss %>% dplyr::filter(sh_gloss$Variable == input$ycol)
  })
  selectDef <- reactive({
    filterDef() %>% select(Glossary)
  })
  output$defineOutput <- renderText({
    paste(selectDef())
  })
  #pca
  pcaDate <- reactive({
    Soil_Health %>% 
      dplyr::filter(Sample.Date %in% c(input$pcaDate))
  })
  pcaLoc <- reactive({
    pcaDate() %>% 
      dplyr::filter(Sample.Loc %in% c(input$pcaLoc))
  })
  pcaSelect <- reactive({
    pcaLoc() %>%
      select(c(input$pcaCheck1, input$pcaCheck2, input$pcaCheck3)) %>% 
      na.omit()
  })
  output$pcaPlot <- renderPlot({
    autoplot(prcomp(select(pcaSelect(), -1,-2)),
             data = pcaSelect(),
             colour = input$groupby,
             size = input$pcaNum,
             frame = input$frame,
             loadings = input$eigen,
             loadings.label = input$load.label,
             loadings.label.size = input$pcalabsize
    )
  })
  #leaflet map
  output$mapOutput <- renderLeaflet({
    leaflet() %>% 
      setView(lat = 31.504978, lng = -94.762271, zoom = 12) %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "ESRI NatGeo", options = providerTileOptions(noWrap = TRUE)) %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(lat = 31.4938, lng = -94.7799, 
                 popup = popupImage(img = "img/4Fal18_Bottom_1.jpg", src = "local", width = "300px", height = "100%"),
                 label = "Bottom",  clusterOptions = markerClusterOptions()) %>% 
      addMarkers(lat = 31.5147, lng = -94.7168, 
                 popup = popupImage(img = "img/4Fal18_Burned_2.jpg", src = "local", width = "300px", height = "100%"),
                 label = "Burned", clusterOptions = markerClusterOptions()) %>% 
      addMarkers(lat = 31.5062, lng = -94.7624, 
                 popup = popupImage(img = "img/4Fal18_Crop_3.jpg", src = "local", width = "300px", height = "100%"),
                 label = "Crop", clusterOptions = markerClusterOptions()) %>%
      addMarkers(lat = 31.5328, lng = -94.7311, 
                 popup = popupImage(img = "img/4Fal18_Loblolly_4.jpg", src = "local", width = "300px", height = "100%"),
                 label = "Loblolly", clusterOptions = markerClusterOptions()) %>% 
      addMarkers(lat = 31.5135, lng = -94.7168, 
                 popup = popupImage(img = "img/4Fal18_Longleaf_5.jpg", src = "local", width = "300px", height = "100%"),
                 label = "Longleaf", clusterOptions = markerClusterOptions()) %>% 
      addMarkers(lat = 31.5032, lng = -94.7645, 
                 popup = popupImage(img = "img/4Fal18_Overgrown_6.jpg", src = "local", width = "300px", height = "100%"),
                 label = "Overgrown", clusterOptions = markerClusterOptions()) %>% 
      addMarkers(lat = 31.5194, lng = -94.7624, 
                 popup = popupImage(img = "img/4Fal18_Pasture_7.jpg", src = "local", width = "300px", height = "100%"),
                 label = "Pasture", clusterOptions = markerClusterOptions()) %>% 
      addMarkers(lat = 31.5141, lng = -94.7164, 
                 popup = popupImage(img = "img/4Fal18_Switch_8.jpg", src = "local", width = "300px", height = "100%"),
                 label = "Switch") %>% 
      addLayersControl(
        baseGroups = c("ESRI NatGeo", "Esri World Imagery"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

#run App
shinyApp(ui=ui, server=server)