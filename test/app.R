library(sf)
library(DBI)
library(glue)
library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(shinyjs)
library(ggplot2)
library(stringr)
library(lubridate)
library(duckspatial)

source(here::here('functions','utils.R'))


# Globals -----------------------------------------------------------------
mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") |>
  st_transform(mycrs)

limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

taus = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

regions = c("United States of America", "Europe")

years = as.character(2000:2023)

pv_opt = c("p <= 0.05 (dec)",
           "0.05 < p <= 0.1 (dec)",
           "0.1  < p <= 0.33 (dec)",
           "p > 0.33",
           "0.1  < p <= 0.33 (inc)",
           "0.05 < p <= 0.1 (inc)",
           "p <= 0.05 (inc)"
)

p_colours = c(
  rgb(0, 0, 0.6),
  rgb(0.1176, 0.3922, 1),
  rgb(0.4706, 0.7373, 1),
  rgb(0.6431, 0.7569, 0.4431),
  rgb(1, 0.7294, 0.4),
  rgb(1, 0.3922, 0),
  rgb(0.6471, 0, 0.1294)
) |>
  setNames(pv_opt)



# Globals from DB ---------------------------------------------------------

con = connect_to_db()

types = tbl(con, "clusterTimeSeries") |>
  select(type) |>
  distinct() |>
  pull() |>
  sort() |>
  factor()

clusterDat = tbl(con, "clusterTimeSeries") |>
  collect() |>
  group_by(region, cluster, type, tau) |>
  mutate(size = n(),
         cluster = ifelse(size <= 3, NA, cluster)) |>
  ungroup() |>
  nest_by(tau, type) |>
  mutate(data = data |> # this nightmare reindexes the cluster number to use the lowest avalible values
           nest_by(cluster) |>
           ungroup() |>
           mutate(cluster = ifelse(!is.na(cluster), row_number(), NA)) |>
           unnest(data) |>
           list()) |>
  unnest(data) |>
  left_join(
    tbl(con, "combinedMeta") |>
      select(station_id, latitude, longitude) |>
      distinct() |>
      collect(),
    "station_id"
  ) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(mycrs)

dbDisconnect(con, shutdown = T)


# UI ----------------------------------------------------------------------

ui <- navbarPage(
  id = "navbar",
  title = "TOAR Data Viewer",
  tabPanel(
    title = "Cluster Overview",
    value = "clusterOverview",
    div(id ="Sidebar-1",
        sidebarPanel(
          width = 2,
          checkboxGroupInput(
            inputId = "overviewType",
            label = "Type",
            choices = types,
            selected = "daily_all",
          ),
          selectInput(
            inputId = "overviewRegion",
            label = "Region",
            choices = regions,
            selected = "Europe"
          ),
          checkboxGroupInput(
            inputId = "overviewTau",
            label = "Tau",
            choices = taus,
            selected = 0.5,
          ),
          actionButton("overviewPlotUpdate", "Update Plot")
        )
    ),
    mainPanel(

      plotlyOutput("plot_overview",
                   height = "800px"),
    )
  ),
  tabPanel(
    title = "Cluster Detail",
    div(id = "Sidebar-2",
        sidebarPanel(
          "Category Select",
          selectInput(
            inputId = "detailType",
            label = "Type",
            choices = types,
            selected = "daily_all",
          ),
          selectInput(
            inputId = "detailRegion",
            label = "Region",
            choices = regions,
            selected = "Europe"
          ),
          selectInput(
            inputId = "detailTau",
            label = "Tau",
            choices = taus,
            selected = 0.5,
          ),
          actionButton("detailPlotUpdate", "Update Plot"),
          uiOutput("detailSelectCluster"),
          selectInput(
            inputId = "detailYear",
            label = "Years",
            choices = years,
            selected = c("2004", "2010", "2016", "2022"),
            multiple = T
          ),

          width = 2
        )
    ),
    mainPanel(
      fluidRow(
        column(
          6,
          plotlyOutput("plot_detail")
        ),
        column(
          6,
          plotOutput("detailPanelB")
        )
      ),
      fluidRow(
        column(
          6,
          plotOutput("detailPanelC")
        ),
        column(
          6,
          "Panel D"
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {

  detailClusterChoices = NULL

  plotDat = eventReactive(
    input$overviewPlotUpdate,
    {
      clusterDat |>
        filter(
          type %in% input$overviewType,
          tau %in% input$overviewTau,
          region == input$overviewRegion
        )
    },
    ignoreNULL = FALSE)

  output$plot_overview = renderPlotly({
    plot_overview = ggplot()+
      geom_sf(data = world)+
      geom_sf(data = plotDat(),
              aes(colour = factor(cluster),
                  text = station_id)) +
      facet_grid(tau~type)+
      # guides(colour = "none")+
      theme_minimal()+
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),
            legend.position = "bottom",
            legend.byrow = T)

    if(isolate(input$overviewRegion) == "Europe"){
      plot_overview = plot_overview+
        scale_y_continuous(limits = st_coordinates(limEU)[,2])+
        scale_x_continuous(limits = st_coordinates(limEU)[,1])

    }else{
      plot_overview = plot_overview+
        scale_y_continuous(limits = st_coordinates(limUS)[,2])+
        scale_x_continuous(limits = st_coordinates(limUS)[,1])
    }

    plotly::ggplotly(plot_overview, tooltip = "text")

  })

  detailPlotDat = eventReactive(
    input$detailPlotUpdate,
    {

      clusterDat |> filter(
        type %in% input$detailType,
        tau %in% input$detailTau,
        region == input$detailRegion
      )
    },
    ignoreNULL = FALSE)

  output$plot_detail = renderPlotly({
    plot_overview = ggplot()+
      geom_sf(data = world)+
      geom_sf(data = detailPlotDat(),
              aes(colour = factor(cluster)))+
      facet_grid(tau~type)+
      # guides(colour = "none")+
      theme_minimal()+
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),
            legend.position = "bottom",
            legend.byrow = T)

    if(isolate(input$detailRegion) == "Europe"){
      plot_overview = plot_overview+
        scale_y_continuous(limits = st_coordinates(limEU)[,2])+
        scale_x_continuous(limits = st_coordinates(limEU)[,1])

    }else{
      plot_overview = plot_overview+
        scale_y_continuous(limits = st_coordinates(limUS)[,2])+
        scale_x_continuous(limits = st_coordinates(limUS)[,1])
    }

    plotly::ggplotly(plot_overview)

  })

  detailClusterChoices = reactive({
    detailPlotDat()$cluster |>
      unique()
  })

  output$detailSelectCluster = renderUI({
    selectInput(
      inputId = "detailClusters",
      label = "Choose Cluster",
      choices = detailClusterChoices(),
      multiple = TRUE
    )
  })

  detailArrowDatAll = reactive({

    if(str_detect(input$detailType, "mda8")){
      tableSuffix = switch (input$detailType,
                            mda8 = "mda8_anom_all",
                            mda8_warm = "mda8_anom_warm",
                            mda8_cold = "mda8_anom_cold"
      )
    }else{
      tableSuffix = input$detailType
    }

    con = connect_to_db()
    on.exit(dbDisconnect(con, shutdown = T))
    ddbs_read_vector(con, paste0("arrow_data_freeTau_", tableSuffix),
                     clauses = "WHERE name == 'o3'")

  })

  clusters_stations = reactive({
    detailPlotDat() |>
      filter(cluster == input$detailClusters)
  }) |>
    bindEvent(input$detailClusters)

  detailArrowDat = reactive({

    detailArrowDatAll() |>
      mutate(region = ifelse(country == "United States of America", country, "Europe")) |>
      filter(region == input$detailRegion,
             tau == input$detailTau,
             year %in% input$detailYear,
             station_id %in% clusters_stations()$station_id)
  })

  output$detailPanelB = renderPlot({

    if(str_detect(input$detailType, "metric")){
      groupVars = c("station_id", "name", "tau", "metric")
      facetFormula = metric~factor(year)
    }else{
      groupVars = c("station_id", "name", "tau")
      facetFormula = tau~factor(year)
    }

    detailPanelB = ggplot() +
      geom_sf(data = world, fill = "white")+
      geom_sf(data = detailArrowDat(),
              mapping = aes(colour = pvStr),
              linewidth = 0.25,
              arrow = arrow(angle = 30,
                            ends = "last",
                            type = "open",
                            length = unit(0.1, "cm"))) +
      scale_colour_manual(values = p_colours, name = "")+
      facet_wrap(facetFormula)+
      theme_minimal()+
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),
            legend.position = "bottom",
            legend.byrow = T)

    if(isolate(input$detailRegion) == "Europe"){
      detailPanelB = detailPanelB+
        scale_y_continuous(limits = st_coordinates(limEU)[,2])+
        scale_x_continuous(limits = st_coordinates(limEU)[,1])

    }else{
      detailPanelB = detailPanelB+
        scale_y_continuous(limits = st_coordinates(limUS)[,2])+
        scale_x_continuous(limits = st_coordinates(limUS)[,1])
    }

    detailPanelB

  })


  detailPiecewiseDat = reactive({

    con = connect_to_db()
    on.exit(dbDisconnect(con, shutdown = T))

    if(str_detect(input$detailType, "mda8")){
      tableSuffix = switch (input$detailType,
                            mda8 = "mda8_anom_all",
                            mda8_warm = "mda8_anom_warm",
                            mda8_cold = "mda8_anom_cold"
      )
    }else{
      tableSuffix = input$detailType
    }

    tbl(con, paste0("piecewise_data_freeTau_", tableSuffix)) |>
      left_join(
        combinedMetaRegion(con) |>
          select(station_id, region) |>
          distinct(),
        by = "station_id") |>
      filter(day(date) == 1,
             month(date) == min(month(date), na.rm = T)) |>
      collect() |>
      filter(region == input$detailRegion,
             tau == input$detailTau,
             station_id %in% clusters_stations()$station_id,
             name == "o3"
      )
  })

  output$detailPanelC = renderPlot({
    detailPiecewiseDat() |>
      ggplot()+
      geom_line(aes(date, piecewise, colour = station_id, group = station_id))+
      guides(colour = "none")
  })



}

# Run the application
shinyApp(ui = ui, server = server)
