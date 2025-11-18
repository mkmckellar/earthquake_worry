#
# shiny app exploring fear of earthquakes by US region
# visualize worry response with column chart
# simple table showing proportion of regional populations 
# that have experienced an earthquake


# shiny app exploring fear of earthquakes by US region

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinybusy)

#library(fivethirtyeight)
# I don't think I need to open the entire 538 library
san_andreas <- fivethirtyeight::san_andreas

san_andreas_traits <- san_andreas |> 
  select(age, hhold_income, region, female) |> 
  rename("household income" = hhold_income,
         "sex" = female)

# missing data by columns
#sapply(san_andreas, function(x) sum(is.na(x)))

# attach(san_andreas)

# US census bureau definition of regions
# https://en.wikipedia.org/wiki/List_of_regions_of_the_United_States


# Define UI for application that draws a histogram
ui <- navbarPage(theme = "yeti",
                 title = NULL,
                 
                 # first panel: general earthquake worry
                 tabPanel(("Worry"),
                           # Application title
                          titlePanel("Earthquake Worry by U.S. Region"),

                          # Sidebar with a slider input for number of bins 
                          # this function doesn't rely on calling a data object
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "selected.worry",
                                          label = "Select type of earthquake worry for plot",
                                          selected = "General",
                                          choices = c("General", "The Big One"),
                                          multiple = FALSE),
                              p("'The Big One' refers to a massive, catastrophic earthquake."),
                              hr(),
                              # select regions for table
                              selectInput(inputId = "selected.regions",
                                          label = "Select regions for table",
                                          selected = unique(san_andreas$region)[1:2],
                                          choices = unique(san_andreas$region),
                                          multiple = TRUE),
                              p(tags$a(href = "https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf",
                                       "Statistical grouping of states as defined by the US Census Bureau.")),
                              p("Data sourced from fivethirtyeight R package.")
                              ),
                            # Show a plot of the generated distribution
                            mainPanel(
                              add_busy_spinner(spin = "half-circle",
                                               position = "top-right"),
                              plotOutput(outputId = "responsePlot1", width = "100%"),
                              hr(),
                              DT::dataTableOutput("responseTable2")
                              )
                            )
                          ),
                 
                 # second panel: preparedness
                 tabPanel(("Preparedness"),
                          # application title
                          titlePanel("Earthquake Preparedness in Response to Past Experience"),
                          sidebarLayout(
                            sidebarPanel(
                              varSelectInput(inputId = "selected.trait",
                                    label = "Select trait",
                                    data = san_andreas_traits,
                                    selected = "age"),
                              hr(),
                              p(tags$a(href = "https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf",
                              "Statistical grouping of states as defined by the US Census Bureau.")),
                              p("Data sourced from fivethirtyeight R package.")
                              ),
                            # Show a plot of the generated distribution
                            mainPanel(
                              add_busy_spinner(spin = "half-circle",
                                               position = "top-right"),
                              plotOutput(outputId = "responsePlot2", width = "100%"),
                              hr(),
                              DT::dataTableOutput("responseTable3")
                              )
                            )
                          )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # make a column plot of the proportion of general worry responses by region
  # use selected.regions.data2
  output$responsePlot1 <- renderPlot({
    
    worry <- sym(req(input$selected.worry))
    
    df2 <- san_andreas |> 
      rename("General" = worry_general) |> 
      rename("The Big One" = worry_bigone) |> 
      select(region, experience, !!worry) |> 
      # remove NA values 
      dplyr::filter(region != is.na(region)) |> 
      dplyr::filter(!!worry != is.na(!!worry))
    
    ggplot(df2,
           aes(x = !!worry)) +
      geom_bar(aes(fill = experience)) +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "top") +
      facet_wrap(vars(region), ncol = 3) +
      xlab("Worry Intensity") +
      ylab("Worry Response Count") +
      guides(fill = guide_legend(title = "Have you ever experienced an earthquake?",
                                 title.position = "top")) +
      theme(legend.position = "top",
            axis.text.x = element_text(face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = 12),
            axis.title.x = element_text(size = 12, margin = margin(t = 15)),
            axis.title.y = element_text(size = 12, margin = margin(r = 15)),
            strip.text.x = element_text(size = 10),
            legend.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 14, face = "bold")) 
  })
  
  # use selected.regions.data2
  
  # Need to fix sex response from logical to character, else male response won't visualize
  output$responsePlot2 <- renderPlot({
    
    trait <- sym(req(input$selected.trait))
    
    # make another column named sex based on the logical female column
    # use sex column for traits
    df3 <- san_andreas |> 
      dplyr::rename("household income" = hhold_income) |> 
      dplyr::mutate(sex = case_when(female == TRUE ~ "female",
                                    female == FALSE ~ "male")
                    ) |> 
      select(!!trait, experience, prepared) |> 
      dplyr::mutate(experience2 = case_when(experience == "No" ~ "No",
                                            experience == "Yes, one or more minor ones" ~ "Yes (minor)",
                                            experience == "Yes, one or more major ones" ~ "Yes (major)"),
                    experience2 = factor(experience2, levels = c("No", "Yes (minor)", "Yes (major)"))
      ) |> 
      dplyr::group_by(!!trait, experience2) |> 
      dplyr::count(prepared) |> 
      dplyr::mutate(proportion = round(n/sum(n),2)) |> 
      dplyr::filter(experience2 != is.na(experience2)) |>
      dplyr::filter(!!trait != is.na(!!trait))
    
    ggplot(df3, 
           aes(x = experience2,
               y = proportion)) +
      geom_col(aes(fill = prepared),
               alpha = 0.85) +
      xlab("Past Experience") + 
      ylab("Earthquake Preparedness") +
      coord_flip() +
      facet_wrap(vars(!!trait)) +
      # adding bw theme to align with other plot
      theme_bw() +
      theme(legend.position = "top",
            axis.text.x = element_text(face = "bold", size = 10),
            axis.text.y = element_text(face = "bold", size = 12),
            axis.title.x = element_text(size = 12, margin = margin(t = 15)),
            axis.title.y = element_text(size = 12, margin = margin(r = 15)),
            strip.text.x = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 14, face = "bold")) +
      guides(fill = guide_legend(title = "Have you taken any precautions for an earthquake?",
                                 title.position = "top")) +
      # adding stronger color contrast
      scale_fill_viridis_d()
  })
  
  
  # make a dataset that is reactive to regions selected
  selected.regions.data <- reactive(
    san_andreas |> 
      dplyr::filter(region != is.na(region)) |> 
      dplyr::filter(region %in% input$selected.regions))
  
  
  # output table showing proportion of region that has
  # experienced an earthquake
  # need to fix s
  output$responseTable2 <- DT::renderDataTable(DT::datatable(selected.regions.data() |> 
                                                              select(region, experience, prepared) |> 
                                                              dplyr::group_by(region, experience) |> 
                                                              count(prepared) |> 
                                                               # removing proportion of region b/c not intuitive in table
                                                              #dplyr::mutate("proportion of region by experience" = round(n/sum(n),2)) |> 
                                                              dplyr::mutate(prepared = as.character(prepared),
                                                                            prepared = case_when(prepared == "FALSE" ~ "No",
                                                                                                 prepared == "TRUE" ~ "Yes")
                                                              ),
                                                            caption = "Past experience with earthquakes increases likelihood of taking safety precautions.",
                                                            options = list(searching = TRUE,
                                                                           lengthChange = FALSE,
                                                                           bPaginate = FALSE)
                                                            )
                                               )

  
  
  output$responseTable3 <- DT::renderDataTable({
    
    trait <- sym(req(input$selected.trait))
    
    # make another column named sex based on the logical female column
    # use sex column for traits
    df3 <- san_andreas |> 
      dplyr::rename("household income" = hhold_income) |> 
      dplyr::mutate(sex = case_when(female == TRUE ~ "female",
                                    female == FALSE ~ "male"),
                    sex = as.factor(sex),
                    prepared = as.character(prepared),
                    prepared = case_when(prepared == "FALSE" ~ "No",
                                         prepared == "TRUE" ~ "Yes"),
                    prepared = as.factor(prepared)
      ) |> 
      select(!!trait, experience, prepared) |> 
      dplyr::mutate(experience = case_when(experience == "No" ~ "No",
                                            experience == "Yes, one or more minor ones" ~ "Yes (minor)",
                                            experience == "Yes, one or more major ones" ~ "Yes (major)"),
                    experience = factor(experience, levels = c("No", "Yes (minor)", "Yes (major)"))
      ) |> 
      dplyr::group_by(!!trait, experience) |> 
      dplyr::count(prepared) |> 
      dplyr::filter(experience != is.na(experience)) |>
      dplyr::filter(!!trait != is.na(!!trait))
    
    
    DT::datatable(df3,
                  filter = "top",
                  caption = "Past experience with earthquakes increases likelihood of taking safety precautions.",
                  options = list(searching = FALSE,
                                 lengthChange = FALSE,
                                 bPaginate = FALSE)
                  )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

