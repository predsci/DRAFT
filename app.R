library(plotly)
library(stringr)
library(shinyWidgets)
library(shinycustomloader)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(ggspatial)
library(raster)
library(colorspace)
library(scales)
library(reshape2)

source("cov_hybrid_calib.R", local = TRUE)
source('helpers.R', local = TRUE)

# loading the US just to get the names correctly for the age groups 

US_dist = readRDS("data/US_age_dist.rds")

US_mil_dist = read.csv("data/active-duty_dist.csv")

country_epi_data = readRDS("data/country_epi_data.new.rds")

map_limits = read.csv(file="data/map_limits.csv", stringsAsFactors=F)

## "country_name" "state_name"   "counry_pop"   "country_age"  "country_day1" "country_rt"   "country_R0"   "country_R1" 
## Set default values 

country_pop = country_epi_data$country_pop ## Need to fix the typo
country_name = country_epi_data$country_name
country_state_name  = country_epi_data$state_name
country_age = country_epi_data$country_age
country_day1 = country_epi_data$country_day1
country_rt = country_epi_data$country_rt
country_R0 = country_epi_data$country_R0
country_R1 = country_epi_data$country_R1
country_day2_start = country_epi_data$country_day2_start
country_death_dates = country_epi_data$country_all_death_dates
country_death       = country_epi_data$country_all_death
country_cases       = country_epi_data$country_all_cases

# change a couple of country names
country_name[country_name=="US"] = "United States"
country_name[country_name=="Korea, South"] = "South Korea"

 country_default = 'Germany'
#country_default = 'Russia'

ind_default = which(country_name == country_default)

pop_default = country_pop[[ind_default]]
age_default = country_age[[ind_default]]
R0_default = country_R0[[ind_default]]
R1_default = country_R1[[ind_default]]
day1_default = country_day1[[ind_default]]
rt_default = country_rt[[ind_default]]
day2_start_default = country_day2_start[[ind_default]]
country_death_dates_default = country_death_dates[[ind_default]]
country_death_default = country_death[[ind_default]]
country_cases_default = country_cases[[ind_default]]

## Keep these 
mil_dist <- US_mil_dist$total[2:6]

civ_names <- c(US_dist$age, 'All')
mil_names <- c(as.character(US_mil_dist$age[2:6]), 'All')

today = Sys.Date()

dt = 1
ndays = 360
xvals = seq(0, ndays, 1)
vecTcalc=seq(0, ndays,dt)

# map file location and filename parsing
if (Sys.info()['nodename']=="Q") {
  map_path = "/srv/shiny-server/covid19/worldpop_maps"
} else {
  map_path = "~/Dropbox/LEPR04/data/worldpop_maps"
}
map_filenames = list.files(path=map_path)
split_filenames = strsplit(x=map_filenames, split="_")
map_info = data.frame(iso3=rep("", length(map_filenames)), precision="", path="", stringsAsFactors=F)
for (ii in 1:nrow(map_info)) {
  map_info$iso3[ii] = toupper(split_filenames[[ii]][1])
  map_info$precision[ii] = split_filenames[[ii]][4]
  map_info$path[ii] = file.path(map_path, map_filenames[ii])
}
# match country names from country_epi_data
match_index = match(x=map_info$iso3, table=unlist(country_epi_data$country_iso3))
map_info$country = NA
na_index = is.na(match_index)
# remove maps that we don't have data for
map_info = map_info[!na_index, ]
map_info$country = unlist(country_name)[match_index[!na_index]]

# intervention min/max dates. may want to make these more dynamic in the future
min_date = as.Date("2020-03-01")
max_date = as.Date("2021-03-31")
# import intervention data
IC_dists = readRDS(file="data/IC-tools.rds")
date_length0 = as.Date(integer(), origin=as.Date("1900-01-01"))

nReals = 50

nrt = length(country_name)

list_vecRtrel <- generate_rt(list_rt = country_rt, nrt = nrt, xvals = xvals, vecTcalc = vecTcalc)

country.name <- as.character(country_name)

belowOnset <- 2

xaxis = list(title = "", domain = c(0.5,1))

ylab1 <- list(title = "Daily Incidence, Fixed R0", color = "blue")

ylab2 <- list(title = "Daily ICU, Fixed R0", color = "blue")

ylab3 <- list(title = "Cumulative Deaths, Fixed R0", color = "blue")



ui <- fluidPage(
  # App title ----
  titlePanel(title=h1("COVID-19 Prototype Tool", align ="center"), windowTitle = "psiCOVID19"),
  tags$hr(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
	fluid = TRUE,
    # Sidebar panel for inputs ----
    sidebarPanel(
    fluidRow(
        tags$head(
      tags$style(type="text/css", ".inline label{ display: table-cell; text-align: left; vertical-align: middle;
                 .inline .form-group{display: table-row;}")
      ),
	helpText(HTML("Modify the default parameters and ",
                             "press the Run Button"))    
    ),
      wellPanel(selectInput(inputId = 'country', label = 'Select Country', c(Choose='', country.name), selected = country_default, multiple = FALSE),
      br(), # Input: population size
      radioButtons(inputId='popType',
      label = 'Population Age Distribution Type',
      choices = c("Civilian" = 1, "Military" = 2),
      inline = TRUE,
      selected = 1),
      br(),      
      uiOutput(outputId="pop"),
      br(),
      uiOutput(outputId='date1'),
      br(),
      uiOutput(outputId="I0")),
      br(),
      #uiOutput(outputId='modulation'),
      wellPanel(
        radioButtons(inputId = "modulation", label = "R(t) Modulation Option:", choices = c('Country Fit' = 1, 'Trigger' = 2, 'Interventions-by-Date'=3), inline = TRUE, selected = 1),
      br(),
      ### MBN commented this - no longer needed we offer our fit to the deaths 
      # conditionalPanel(condition="input.modulation==1",
                                    # uiOutput(outputId="profile"),
                                    # uiOutput(outputId="day2start")
                                   
                   # ),  
      # R0 slider appears for input$modulation is 2 or 3
      conditionalPanel(condition="input.modulation==2 || input.modulation==3", 
        uiOutput(outputId="R0")
      ),
      
      conditionalPanel(condition="input.modulation==2",
        # uiOutput(outputId="R0"),
        #uiOutput(outputId="Rp"),
        radioButtons(inputId='trigType',
    	    label = "Trigger for Reduction in R(t):", 
    	    choices = c("Day Number " = 2, "Per Capita ICU " = 1),
    	    inline = TRUE,
    	    selected = 2), 
      #helpText("Select a Modulation Trigger above and update default trigger value below"),     
        conditionalPanel(condition='input.trigType==1', uiOutput(outputId="trigicu")),
        conditionalPanel(condition='input.trigType==2', uiOutput(outputId="trigday")),
        uiOutput(outputId="Rp")
      ),
      # Intervention conditional panel
      conditionalPanel(condition="input.modulation == 3",
        # Basic reproduction number
        # uiOutput(outputId="R0"),
        # School closure
        checkboxInput(inputId="school_binary", label="School Closure", value=TRUE),
        conditionalPanel(condition="input.school_binary == true",
          dateRangeInput(inputId="school_range", label="Start and End Dates", start=as.Date("2020-03-18"), end=as.Date("2020-08-20"), min=min_date, max=max_date)
        ),
        # Self Isolation
        checkboxInput(inputId="self_isolation_check", label="Self Isolation", value=TRUE),
        conditionalPanel(condition="input.self_isolation_check == true",
          dateRangeInput(inputId="self_isolation_range", label="Start and End Dates", start=as.Date("2020-03-23"), end=as.Date("2020-08-01"), min=min_date, max=max_date)
        ),
        # Social Distancing
        checkboxInput(inputId="social_distancing_check", label="Social Distancing", value=TRUE),
        conditionalPanel(condition="input.social_distancing_check == true",
          dateRangeInput(inputId="social_distancing_range", label="Start and End Dates", start=as.Date("2020-03-23"), end=as.Date("2020-07-15"), min=min_date, max=max_date)
        ),
        # Public Events
        checkboxInput(inputId="public_events_check", label="Public Events", value=TRUE),
        conditionalPanel(condition="input.public_events_check == true",
          dateRangeInput(inputId="public_events_range", label="Start and End Dates", start=as.Date("2020-03-23"), end=as.Date("2020-07-01"), min=min_date, max=max_date)
        ),
        # Lockdown
        checkboxInput(inputId="lockdown_check", label="Lockdown/Shelter-in-Place", value=TRUE),
        conditionalPanel(condition="input.lockdown_check == true",
          dateRangeInput(inputId="lockdown_range", label="Start and End Dates", start=as.Date("2020-03-23"), end=as.Date("2020-05-15"), min=min_date, max=max_date)
        )
      )
     ),
     br(),     

    br(),
	  # TTT ---
	  #h3(textOutput("country_text")),
	  #br(),
    # submitButton("Submit", icon("refresh"))
	  actionButton(inputId="run_sim", label="Run")
	  # TTT +++
    ),	    
     # Main panel for displaying outputs ----
    mainPanel(
	 tabsetPanel(
	 type= 'tabs', id="main_tabs",
         tabPanel(
          title="Visualizations", value="viz",
          tags$hr(),
      # Output: Histogram ----
      plotlyOutput("plot1", height = "20%", width = "80%") %>% withSpinner(color ="#0dc5c1"),
      #br(),
      plotlyOutput("plot2", height = "20%", width = "80%") %>% withSpinner(color ="#0dc5c1"),
      #br(),
      plotlyOutput("plot3", height = "20%", width = "80%") %>% withSpinner(color ="#0dc5c1")
	  ),
	  # R-modulation plotting tab
	  tabPanel(
	   title="R(t)", value="Rt",
	   br(),
	   tags$p("For best experience, return to the 'Visualizations' tab before clicking the 'Run' button."),
	   br(),
	   fluidRow(align="center", 
	            tableOutput('interv_table')
	            ),
	   br(),
	   br(),
	   # plotOutput('rt_profs'),
	   plotlyOutput('rt_profs'),
	   textOutput('noPlotText')
	  ),
	 # Map tab --------------
	  tabPanel(
	    title="Map", value="Maps",
	    br(),
	    uiOutput(outputId="map_date"),
	    selectInput(inputId="map_metric", label="Metric to map", choices=c(`Daily New Infected`=1, `Daily New ICU`=2, `Cumulative Deaths`=3), selected=1),
	    tags$p("New changes in left panel need to be 'Run' before map will update properly."),
	    actionButton(inputId="make_map", label="Make Map"),
	    textOutput(outputId="NoMapText"),
	    br(),
	    br(),
	    #TT withSpinner(plotOutput("map_plot")),
	    withSpinner(uiOutput(outputId="map_plot")),
	    br(),
	    br(),
	    # selectInput(inputId="map_file_format", label="Download format", choices=c("GeoTIFF", "Shapefile - .zip", "Shapefile - .tgz"), selected="Shapefile - .tgz"),
	    selectInput(inputId="map_file_format", label="Download format", choices=c("GeoTIFF", "Shapefile - .zip"), selected="Shapefile - .zip"),
	    downloadButton(outputId="download_map_button", "Download")
	  ),
	  # Data table and download tab 
	  tabPanel("Data Table",
	  tags$hr(),
	  titlePanel("Preparing Data For Download"),
    # Main panel for displaying outputs ----
    mainPanel(
    br(),
    downloadButton("downloadData", "Download"),
    br(),
    br(),
      dataTableOutput("table", width = "95%", height = "auto") %>% withSpinner(color ="#0dc5c1")
    )
  # )
),
	  tabPanel("About",
		# br(),
		# helpText("This page is a work in progress and currently provides only limited important details."),
		h2("Model"),
		tags$p("The app uses a compartmental SE[I]R model stratified by age and disease severity. The four severity compartments of [I] are asymptomatic, mild, flu-like,  and severe. Mild and flu-like infections have COVID-19 symptoms, but do not require hospitalization.  As the data progresses, these two severities will have different probabilities of being tested and becoming confirmed cases. Only the severe compartment leads to a need for an ICU bed."  ),
		# For a basic explanation on compartmental models please see ", a(href="https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology", "for example")
	    # br(),
	    h3("Age Parameters"),
		tags$p("Model parameters are specific to age.  The following table shows the severity probabilities for each age group rounded to 2 decimal places."),
		# insert severity probabilities here
		tableOutput('vec_df'),
        tags$p("Each compartment of the model is stratified by age.  Civilian distributions have nine age groups (0-9, 10-19,..,80+).  The military distribution is broken up into six age groups (0-9 removed and above 60 is combined into 60+).  The military age groupings maintain decadal spacing for easy comparison with civilian populations."),
		# br(),
		h3("Basic Reprodution Number Modulation"),
        tags$p("The Basic Reproduction Number R0 may be time-dependent R(t) via three options:"),
        tags$p("1. Use the results of our fit to each country.  We use a smoothly varying function for R(t) and fit the initial value of this function, the day it begins to decrease and the final value.  We use the reported cumulative death (bottom plot in main 'Visualization' tab) for the fits.  This is done for each country in our list and the values we obtain are used as defaults for options 2 and 3 below"),
        tags$p("2. Change is triggered on a given day or when the need for ICUs exceeds a user determined threshold. Our default values for the initial and final value of R(t) is derived from our fits to the reported cumulative deaths."),
		    tags$p("3. User enters an initial R0 value for the population and date ranges for each intervention type.  A distribution describing proportional drop in R as a result of each intervention is derived from Imperial College Coronavirus Report #13. R(t) profiles are generated from date range and a draw from each parameter distribution. Currently, only the mean R(t) profile is used in simulations."),
		h3("Interval Parameters"),
    tags$p("The stochastic nature of these simulations result in the distribution of generation times in the figure below.  The mean generation time-or serial interval-is 9.6 days.  The majority of transmission occurs 5-10 days after infection."),
		# Insert generation time interval here
		div(img(src="GenTime.png", height=500), style="text-align: center;"),
		tags$p("Individuals leaving the severe state enter ICU.  Mean waiting time in the ICU is set to 7 days."),
		h3("Country Defaults"),
		tags$p("When the user selects a country, the app fills in estimates for population, date of first cases, and R0.  Approximate number of first cases always defaults to 100."),
		tags$p("Country default Basic Reproduction Number (R0):  For each country we fit the reported cumulative number of deaths using a two-value model for R(t). The initial and final values of these fits are used as default values for R0 and the reduced transmission R(t)."),
		tags$p("Approximate Date of First Cases: this date is estimated as follows.  For any given country, we find the date of first reported death and assume that on this date there were already 1,000 cumulative cases in this country.  Using our estimate for basic reproduction number, we run our model and find how many days it takes to reach a total of 1,000 cases. Using this number of days and the reported date of first death we determine our estimate for the date of first case(s). This process is repeated for each country."),
        br(),
		    br(),
		h3("Technology"),
		tags$p("All the data and model analysis are done with R codes and displayed using R, shinyApp and plotly."),
		br(),
		br()
	  ),
	  tabPanel("Contact",
                        column(width=7,
                        br(),
                        br(),
                        tags$p(tags$em("For questions about this web site please contact:")),
                        br(),
                        tags$p("Dr. Pete Riley: pete@predsci.com"),
                        tags$p("Dr. M. Ben-Nun: mbennun@predsci.com"),
                        tags$p("Dr. J. Turtle: jturtle@predsci.com"),
                        br(),
                        br(),
                        includeHTML("html/address.html")))
	  )
    )
  )
)

server <- function(input, output, session) {
  
  # --- dynamic tab switching -----
  # change to Visualizations tab when button is pressed
  # observeEvent(input$run_sim, {
  #   updateTabsetPanel(session, inputId="main_tabs", selected="viz")
  # })
  # shift to R-modulation tab when Intervention Radio button selected
  observeEvent(input$modulation, {
    if (input$modulation==3) {
      updateTabsetPanel(session, inputId="main_tabs", selected="Rt")
    }
  })
  # ---------------------------------
  
	   # set the population based on the country chosen 
	   output$pop <- renderUI({
	   	mycountry = as.character(input$country)
	   	myindex <- which(country_name == mycountry)
	   	mypop = country_pop[[myindex]]
	   	mypop = round(mypop)
	   	textInput(inputId = "pop", label = HTML("Population Size <br/> (1,000 - 350,000,000)"), value = as.character(mypop))
	   })

 	Ntotal <- reactive({
 	  input$run_sim
 	  if ("pop" %in% names(isolate(input))) {
 	    input_pop = as.numeric(input$pop)
 	  } else {
 	    input_pop = pop_default
 	  } 	  
 	  input_pop
 	  })

	output$I0 <- renderUI({
		numericInput(inputId = "I0", label = HTML("Approx. No of First Cases: <br/>(10 - 1000)"), min = 10, max = 1000, value = 100, step = 10)
	})

 	I0total <- reactive({
 	  input$run_sim
 	  if ("I0" %in% names(isolate(input))) {
 	    myI0 = as.numeric(input$I0)
 	  } else {
 	    myI0 = 100.
 	  }
 	  myI0
 	  })

	output$date1 <- renderUI({
		#input$run_sim
		mycountry = as.character(input$country)
		myindex <- which(country_name == mycountry)
		mydate1 = as.Date(country_day1[[myindex]], format = "%Y-$m-%d")
		dateInput(inputId = 'date1', label = 'Approx. Date of First Cases:', value = mydate1, format = 'mm/dd/yyyy')
	})
	
	day1 <- reactive({
		input$run_sim
		if ("date1" %in% names(isolate(input))) {
			input_date1 = as.Date(input$date1)
		} else {
			input_date1 = day1_default
		}
		input_date1
	})
	
	modulation <- reactive({
		input$run_sim
	  if ("modulation" %in% names(isolate(input))) {
 	    modulation = as.numeric(input$modulation)
 	  } else {
 	    modulation = 1
 	  }
 	  modulation
	})
	
	
	output$profile <- renderUI({
		#input$modulation
		mycountry = as.character(input$country)
		selectInput(inputId = 'profile', label = 'Geographic R(t) Profile', choices = c(Choose='', country.name), selected = mycountry, multiple = FALSE)
	})

	output$day2start <- renderUI({
		#input$modulation
		mycountry = as.character(input$country)
		myindex <- which(country_name == mycountry)
		myday2start <- country_day2_start[[myindex]]
		numericInput(inputId = 'day2start', label = 'Profile Start Day', min = 14, max = 49, value = myday2start)
	})

 	country_day2start_default <- reactive({
 		#input$run_sim
 		mycountry = as.character(input$country)
 		index = which(country_name == mycountry)
 		input_day2_start = country_day2_start[[index]]
 		input_day2_start
 	})		
 	
	day2start <- reactive({
		input$run_sim
		if ("day2start" %in% names(isolate(input))) {
			day2start = as.numeric(input$day2start)
		} else {
			day2start = country_day2start_default()
		}
		day2start
	})
	
	output$R0 <- renderUI({
		input$modulation
		mycountry = as.character(input$country)
		index = which(country_name == mycountry)
		input_R0 = country_R0[[index]] 
		
		sliderInput(inputId = "R0", label = "Initial Reproduction Number (R0)", min = 1.5, max = 5.5, step = 0.1, value = input_R0)
	})
 	
 	
 	country_R0_default <- reactive({
 		input$run_sim
		mycountry = as.character(input$country)
		index = which(country_name == mycountry)
		input_R0 = country_R0[[index]]  
		input_R0		
 	})	
 	
 	R0 <- reactive({
 		input$run_sim
		#mycountry = as.character(input$country)
		#index = which(country_name == mycountry)
		#input_R0 = country_R0[[index]]  		
		if ("R0" %in% names(isolate(input))) {
			if ( input$modulation == 1) {
				input_R0 = country_R0_default()
			} else {
				input_R0 = as.numeric(input$R0)
			}			
			 # Here will need to take it from the table
		} else {
			input_R0 = R0_default # Here take it for default country
		}
		input_R0 		
 	})

 	output$Rp <- renderUI({
		input$modulation
		mycountry = as.character(input$country)
		index = which(country_name == mycountry)
		input_Rp = country_R1[[index]] 
		sliderInput(inputId = "Rp", label = "Reduced Reproduction Number R(t)", min = 0.2, max = 2., step = 0.1, value = input_Rp)
	})
 	
  	country_Rp_default <- reactive({
 		input$run_sim
		mycountry = as.character(input$country)
		index = which(country_name == mycountry)
		input_R1 = country_R1[[index]]  
		input_R1		
 	})	
 		
  	Rp <- reactive({
 		input$run_sim
		mycountry = as.character(input$country)
		index = which(country_name == mycountry)
		input_Rp = country_R1[[index]]  		
		if ("Rp" %in% names(isolate(input))) {
			if ( input$modulation == 1) {
				input_Rp = country_Rp_default()
			} else {
				input_Rp = as.numeric(input$Rp)
			}			
		} else {
			input_Rp = input_Rp  # Here take it for default country
		}
		input_Rp 		
 	})

	output$trigicu <- renderUI({
		input$modulation
		numericInput(inputId='trigicu', label ="ICU beds per 100,000 ", min = 1., max=30, step=1, value=10.)
	})

	trigicu <- reactive({
		input$run_sim
		if ("trigicu" %in% names(isolate(input))) {
			input_icu = as.numeric(input$trigicu) 
		} else {
			input_icu = 10 # Here take it for default country
		}
		input_icu 		
	})
	
	output$trigday <- renderUI({
		input$modulation
		mycountry = as.character(input$country)
		index = which(country_name == mycountry)
		input_trigday = country_day2_start[[index]]		
		numericInput(inputId='trigday', label ="Day Number of Reduced Transmission R(t)", min = 14, max=60, step=1, value=input_trigday)
	})	

	trigday <- reactive({
		input$run_sim
		if ("trigday" %in% names(isolate(input))) {
			trig_day = as.numeric(input$trigday) 
		} else {
			trig_day = day2_start_default
		}
		trig_day 		
	})

         	
	profile.id <- reactive({
		input$run_sim
		if ("profile" %in% names(isolate(input))) {
			input_profile = as.character(input$profile)
		} else {
			input_profile = as.character(input$country)
		}
		input_profile
	})
	  	  
			
	nAges <- reactive({
	  input$run_sim
	  popType = isolate(input$popType)
		if (as.numeric(popType) == 1) nAges = 9 #length(civ_dist)
		if (as.numeric(popType) == 2) nAges = 5 #length(mil_dist)
		nAges	
		})


	vecPS <- reactive({
	  input$run_sim
	  popType = isolate(input$popType)
		if (as.numeric(popType) == 1) vecPS = c(0.0000161, 0.0000695, 0.000309, 0.000844, 0.00161, 0.00595, 0.0193, 0.0428, 0.078)
		if (as.numeric(popType) == 2) vecPS = c(0.0000695, 0.000309, 0.000844, 0.00161, 0.00595)
		vecPS
	}) 
	
	
	vecPM <- reactive({
		input$run_sim
		popType = isolate(input$popType)
		if (as.numeric(popType) == 1) {
			vecPM = c(0, 0.00041, 0.014, 0.0343, 0.0425, 0.0816, 0.118, 0.166, 0.184)
		} else {
			vecPM = c(0.00041, 0.014, 0.0343, 0.0425, 0.0816)
		}
		vecPM
	}) 


	vecPA <- reactive({
		vecPA = rep(0.4, nAges())
	})

	output$vec_df <- renderTable({
	  vecPM = vecPM()
	  vecPA = vecPA()
	  vecPS = vecPS()
	  vecPF = 1- vecPM - vecPA - vecPS
	  out_df = data.frame(Age=c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"), `Prob Asmpt`=vecPA, `Prob Mild`=vecPM, `Prob Flu`=vecPF, `Prob Severe`=vecPS, stringsAsFactors=F, check.names=F)
	  return(out_df)
	})
			
	vecN <- reactive({
		input$run_sim
		popType = isolate(input$popType)

		if (as.numeric(popType) == 1) {
			mycountry = isolate(input$country)
			index = which(country_name == mycountry)
			civ_dist = country_age[[index]]
			round(isolate(Ntotal()) * civ_dist)
		} else {
			round(isolate(Ntotal()) * mil_dist)
		}
	})

	vecI0 <- reactive({
		input$run_sim
		popType = isolate(input$popType)
		if (as.numeric(popType) == 1) {
			mycountry = isolate(input$country)
			index = which(country_name == mycountry)
			civ_dist = country_age[[index]]
			round(isolate(I0total()) * civ_dist)
		} else {
			round(isolate(I0total()) * mil_dist)
		}
	})	

	trig_pres <- reactive({
		input$run_sim
		in_trigicu = isolate(input$trigicu)
		trig_pres = ceiling(as.numeric(in_trigicu) * isolate(Ntotal())/10000)
		trig_pres
	})

	trig_day <- reactive({
		input$run_sim
		in_trigday = isolate(input$trigday)
		trig_day <- as.numeric(in_trigday)
		trig_day
	})
	
	trig_type <- reactive({
	  input$run_sim
	  in_trigType = isolate(input$trigType)
	  as.numeric(in_trigType)
	  })
	
	icu_cap <- reactive({
		0.8 * 10000 / isolate(Ntotal())
	})

	trig_modulation <- reactive({
		input$modulation
		as.numeric(input$modulation)
	})
	
	cum_conf_death_df <- reactive({
		input$run_sim
		mycountry = isolate(input$country)
		index = which(country_name == mycountry)
		df = data.frame(dates = country_death_dates[[index]], cum_death = as.numeric(country_death[[index]]))
		df <- subset(df, dates >= isolate(day1()))		
		df
	})

	cases_df <- reactive({
		input$run_sim
		mycountry = isolate(input$country)
		index = which(country_name == mycountry)
		# replace negative daily confirmed cases with NA - must be an error in data
		cases = c(0, diff(as.numeric(country_cases[[index]])))
		cases[cases<0] = NA
		df = data.frame(dates = country_death_dates[[index]], cases = cases)
		df <- subset(df, dates >= isolate(day1()))		
		df
	})	
		
	# debug
	output$country_text <- renderText({
		#country.id()
		as.character(R0())
	})
	
		
	# --- R(t) from interventions -------
	intervs_df = reactive({
	  out_df = data.frame(interv_name=character(), start_date=date_length0, end_date=date_length0, stringsAsFactors=FALSE)
	  if (input$modulation==3) {
	    if (input$school_binary) {
	      date_range = input$school_range
	      out_df = rbind(out_df, data.frame(interv_name="school", start_date=date_range[1], end_date=date_range[2], stringsAsFactors=FALSE))
	    }
	    if (input$social_distancing_check) {
	      date_range = input$social_distancing_range
	      out_df = rbind(out_df, data.frame(interv_name="social_dist", start_date=date_range[1], end_date=date_range[2], stringsAsFactors=FALSE))
	    }
	    if (input$public_events_check) {
	      date_range = input$public_events_range
	      out_df = rbind(out_df, data.frame(interv_name="public_events", start_date=date_range[1], end_date=date_range[2], stringsAsFactors=FALSE))
	    }
	    if (input$self_isolation_check) {
	      date_range = input$self_isolation_range
	      out_df = rbind(out_df, data.frame(interv_name="self_isolate", start_date=date_range[1], end_date=date_range[2], stringsAsFactors=FALSE))
	    }
	    if (input$lockdown_check) {
	      date_range = input$lockdown_range
	      out_df = rbind(out_df, data.frame(interv_name="lockdown", start_date=date_range[1], end_date=date_range[2], stringsAsFactors=FALSE))
	    }
	  } # else if (input$interv_control_radio==1) {
	  #   date_range = input$all_interv_range
	  #   out_df = data.frame(interv_name = c("school", "social_dist", "public_events", "self_isolate", "lockdown"), start_date = date_range[1], end_date = date_range[2], stringsAsFactors = FALSE)
	  # }
	  return(out_df)
	})
	
	output$interv_table = renderTable({
	  out_df = intervs_df()
	  if (nrow(out_df)>0) {
	    out_df$start_date = as.character(out_df$start_date)
	    out_df$end_date = as.character(out_df$end_date)
	    names(out_df) = c("Intervention", "Start Date", "End Date")
	    return(out_df)
	  } else {
	    return(NULL)
	  }
	  
	})
	
	rt_profs <- reactive({
	  start_date = input$date1
	  end_date = start_date + ndays
	  # return nReals Rt profiles
	  out_list = build_sample_Rt_profiles(start_date=start_date, end_date=end_date, radio_control=input$modulation, intervs_df=intervs_df(), n_profiles=nReals, IC_dists=IC_dists)
	  out_list$Rt = input$R0*out_list$Rt
	  return(out_list)
	})
	
	rt_mean <- reactive({
	  start_date = input$date1
	  end_date = start_date + ndays
	  # return the mean Rt profile
	  out_list = build_mean_Rt(start_date=start_date, end_date=end_date, radio_control=input$modulation, intervs_df=intervs_df(), IC_dists=IC_dists)
	  out_list$Rt = input$R0*out_list$Rt
	  return(out_list)
	})
	
	# create alternate text for when there is no plot
	output$noPlotText = renderText({
	  if (input$modulation=="2" && input$trigType=="1") {
	    return("R(t) cannot be pre-determined for the ICU trigger.")
	  } else {
	    return(NULL)
	  }
	})
	
	# output$rt_profs = renderPlot({
	output$rt_profs = renderPlotly({
	  # legend location
	  legend_pos = c(0.98, 0.02)
	  legend_just = c("right", "bottom")
	  
	  if (input$modulation==3) {
	    # out_list = build_sample_Rt_profiles(start_date=as.Date("2020-02-01"), end_date=as.Date("2020-08-31"), radio_control=input$modulation, intervs_df=intervs_df(), n_profiles=nReals, IC_dists=IC_dists)
	    # data_df = as.data.frame(input$R0*out_list$Rt)
	    Rt_list = rt_profs()
	    data_df = as.data.frame(Rt_list$Rt)
	    data_df$date = Rt_list$date
	    data_df_long = melt(data_df, id.vars="date")
	    line_groups = unique(data_df_long$variable)
	    ngroups = length(line_groups)
	    legend_vec = rep(FALSE, ngroups)
	    legend_vec[ngroups] = TRUE
	    names(legend_vec) = line_groups
	    # add mean Rt
	    Rt_mean = rt_mean()
	    Rt_mean = as.data.frame(Rt_mean)
	    
	    p = ggplot() +
	      geom_line(data=data_df_long, mapping=aes(x=date, y=value, group=variable, color="R(t) profiles"), alpha=0.2, size=2, show.legend=legend_vec) +
	      geom_line(data=Rt_mean, mapping=aes(x=date, y=Rt, color="R(t) mean"), size=2, show.legend=TRUE) +
	      ylim(0, max(data_df_long$value)) +
	      labs(y="Reproduction Number", x="Time", 
	           title="Modulated Basic Reproduction Number") +
	      scale_color_manual(values=c("steelblue3", "gray50")) +
	      theme(plot.title=element_text(size=20, family="serif", face="bold", hjust=0.5),
	            legend.position=legend_pos,
	            legend.justification=legend_just,
	            legend.title=element_blank(),
	            text = element_text(size=16),
	            legend.text=element_text(size=16),
	            axis.text.x=element_text(size=14),
	            axis.text.y=element_text(size=14)#,
	      )
	    return(ggplotly(p))
	  } else if (input$modulation==2) {
	    trig_type = as.numeric(input$trigType)
	    if (trig_type==2) {
	      R0 = input$R0
	      Rp = input$Rp
	      trig_day = input$trigday
	      if (is.na(trig_day)) {
	        return(NULL)
	      }
	      start_date = input$date1
	      date_vec = seq(from=start_date, by=1, length.out=ndays+1)
	      del = R0 - Rp
	      R_vec = rep(R0, ndays+1)
	      R_vec[vecTcalc>trig_day] = R_vec[vecTcalc>trig_day] - del*(tanh((vecTcalc[vecTcalc>trig_day]-trig_day)/5))
	      R0_vec = rep(R0, ndays+1)
	      data_df = data.frame(date=date_vec, Rt=R_vec, R0 = R0_vec)
	      
	      p = ggplot() +
	        geom_line(data=data_df, mapping=aes(x=date, y=R0, colour="Fixed R(t)"),  size=2, linetype = 'dashed', show.legend=TRUE) +
	        geom_line(data=data_df, mapping=aes(x=date, y=Rt, colour="Triggered R(t)"),  size=2, show.legend=TRUE) +
	        ylim(0, max(data_df$Rt)) +
	        labs(y="Reproduction Number", x="Time", 
	             title="Modulated Basic Reproduction Number") +
	        scale_color_manual(values=c("steelblue3", "steelblue2")) +
	        theme(plot.title=element_text(size=20, family="serif", face="bold", hjust=0.5),
	              legend.position=legend_pos,
	              legend.justification=legend_just,
	              legend.title=element_blank(),
	              text = element_text(size=16),
	              legend.text=element_text(size=16),
	              axis.text.x=element_text(size=14),
	              axis.text.y=element_text(size=14)#,
	        )
	      return(ggplotly(p))
	    } else {
	      return(NULL)
	    }
	  } else if (input$modulation==1) {
	    mycountry = as.character(isolate(input$country))
	    id = which(country_name == mycountry)
	    vecRtrel = country_rt[[id]]
	    nold = length(vecTcalc)
	    myn = length(vecRtrel)
	    if (myn < nold ) {
	      nadd = nold-myn
	      Rtrel_last = vecRtrel[myn]
	      vecRtrel <- c(vecRtrel, rep(Rtrel_last, nadd ))
	    }
	    vecRtrel <- vecRtrel[1:nold]
	    R0_vec <- rep(vecRtrel[1], nold)
	    start_date = input$date1
	    date_vec = seq(from=start_date, by=1, length.out=ndays+1)
	    data_df = data.frame(date=date_vec, Rt=vecRtrel, R0 = R0_vec)
	    
	    p = ggplot() +
	      geom_line(data=data_df, mapping=aes(x=date, y=R0, colour="Fixed R(t)"),  size=2, linetype = 'dashed', show.legend=TRUE) +
	      geom_line(data=data_df, mapping=aes(x=date, y=Rt, colour="Fit R(t)"),  size=2, show.legend=TRUE) +
	      ylim(0, max(data_df$Rt)) +
	      labs(y="Reproduction Number", x="Time", 
	           title="Modulated Basic Reproduction Number") +
	      scale_color_manual(values=c("steelblue3", "steelblue2")) +
	      theme(plot.title=element_text(size=20, family="serif", face="bold", hjust=0.5),
	            legend.position=legend_pos,
	            legend.justification=legend_just,
	            legend.title=element_blank(),
	            text = element_text(size=16),
	            legend.text=element_text(size=16),
	            axis.text.x=element_text(size=14),
	            axis.text.y=element_text(size=14)#,
	      )
	    return(ggplotly(p))
	  } else {
	    return(NULL)
	  }
	  
	})
	# -----------------------------------
	
	# ---- Map tab backend --------------
	# generate date input for map tab
	output$map_date <- renderUI({
	  # determine last date of data
	  death_data = cum_conf_death_df()
	  data_date = death_data$dates[nrow(death_data)]
	  min_death_date = death_data$dates[min(which(death_data$cum_death>0))]
	  dateInput(inputId = 'map_date', label = 'Date of results to map', value=data_date+28, format = 'mm/dd/yyyy', min=min_death_date, max=max_date)
	})
	
	#TT output$map_plot <- renderPlot({
	output$map_plot <- renderUI({
	  # execute when button pressed
	  input$make_map
	  
	  if (!("map_date" %in% names(isolate(input)))) {
	    return(NULL)
	  } 
	  
	  map_date = isolate(input$map_date)
	  death_data = isolate(cum_conf_death_df())
	  data_date = death_data$dates[nrow(death_data)]
	  
	  # Eventually allow user to select which metric to overlay
	  overlay_metric = isolate(input$map_metric)
	  model_df = df2()
	  if (overlay_metric=="1") {
	    metric_value = model_df$rtn[model_df$x==map_date]
	  } else if (overlay_metric=="2") {
	    metric_value = model_df$icu[model_df$x==map_date]
	  } else if (overlay_metric=="3") {
	    metric_value = model_df$ded[model_df$x==map_date]
	  }
	  
	  if (metric_value==0) {
	    return(NULL)
	  }
	  
	  # create list of available maps and match with country menu
	  country_name = isolate(input$country)
	  if (grepl(pattern=",", x=country_name, fixed=TRUE)) {
	    return(NULL)
	  } else {
	    map_index = map_info$country==country_name
	    map_filename = map_info$path[map_index]
	    map_iso3  = map_info$iso3[map_index]
	    map_units = map_info$precision[map_index]
	    grid_edge = gsub(pattern="km", replacement="", x=map_units)
	    grid_edge = gsub(pattern="m", replacement="", x=grid_edge)
	    grid_unit = gsub(pattern=grid_edge, replacement="", x=map_units)
	    grid_edge = as.integer(grid_edge)
	    grid_area = grid_edge^2
	    if (grid_unit=="m" & grid_area >= 1e5) {
	      grid_unit = "km"
	      grid_area = round(grid_area/1e6, digits=1)
	    }
	    area_units = paste0(grid_area, grid_unit)
	  }
	  
	  # load map
	  pop_map = raster(map_filename)
	  plot_map = metric_value*pop_map

	  # scaling for only one time/frame
	  max_val = cellStats(plot_map, "max")
    # consistent scaling for all times in simulation
	  # max_val = max_val * max(model_df$rtn, na.rm=T)
	  	  
	  # make very small/0 values of plot_map a little larger for color scale plotting
	  pixel_quantile = 0.05
	  thresh_val = quantile(plot_map, pixel_quantile)
	  while (thresh_val==0) {
	    pixel_quantile = pixel_quantile + .05
	    thresh_val = quantile(plot_map, pixel_quantile)
	  }
	  plot_map[plot_map < thresh_val & !is.na(plot_map)] = thresh_val
	  # scaling for only one time/frame
	  min_val = thresh_val
	  # consistent scaling for all times in simulation
	  # min_val = thresh_val*min(model_df$rtn, na.rm=T)
	  min_val = min_val*.99
	  
	  color_limits = c(min_val, max_val)

	  max_base_digits = floor(log10(max_val))
	  max_base = 10^max_base_digits
	  max_label = max_base*floor(max_val/max_base)
	  min_base_digits = floor(log10(min_val))
	  order_change = max_base_digits - min_base_digits + 1
	  if (order_change > 5) {
	    reduce_log = seq(from=0, to=-order_change+1, by=-2)
	  } else {
	    reduce_log = seq(from=0, to=-order_change+1, by=-1)
	  }
	  all_breaks = 10^(log10(max_label) + reduce_log)
	  for (ii in 1:length(all_breaks)) {
	    log10_break = log10(all_breaks[ii])
	    digits = -floor(log10_break)
	    all_breaks[ii] = round(x=all_breaks[ii], digits=digits)
	  }
	  all_labels = as.character(all_breaks)
	  
	  # generate color scale and legend title
	  if (overlay_metric=="1") {
	    legend_expression = bquote(atop(New~Infected,per~.(area_units)^2))
	    palette_name = "Viridis"
	    color_rev = FALSE
	    # ggscale = scale_fill_continuous_sequential(palette="Heat", rev=FALSE, limits=c(0, max_val), name=legend_expression, na.value=NA)
	    # ggscale = scale_fill_viridis_c(limits=color_limits, name=legend_expression, na.value=NA)
	  } else if (overlay_metric=="2") {
	    legend_expression = bquote(atop(New~ICU, per~.(area_units)^2))
	    palette_name = "Plasma"
	    color_rev = FALSE
	  } else if (overlay_metric=="3") {
	    legend_expression = bquote(atop(Mortality, per~.(area_units)^2))
	    palette_name = "Heat"
	    color_rev = FALSE
	  }
	  
	  # ggscale = scale_fill_viridis_c(limits=c(min_val, max_val), name=legend_expression, na.value=NA, trans="log10", breaks=all_breaks, labels=all_labels)
	  ggscale = scale_colour_continuous_sequential(palette=palette_name, aesthetics="fill", rev=color_rev, na.value=NA, limits=c(min_val, max_val), name=legend_expression, trans="log10", breaks=all_breaks, labels=all_labels)
	  
	  # check if map limits have been recorded in map_limits.csv
	  limit_plot = FALSE
	  if (map_iso3 %in% map_limits$iso3) {
	    limit_plot = TRUE
	    limit_index = map_iso3==map_limits$iso3
	    xlimits = c(map_limits$x_min[limit_index], map_limits$x_max[limit_index])
	    ylimits = c(map_limits$y_min[limit_index], map_limits$y_max[limit_index])
	  }
	  
	  p = ggplot() +
	    layer_spatial(plot_map) +
	    ggscale
	  if (limit_plot) {
	    p = p +
	      scale_x_continuous(limits=xlimits, expand=c(0,0)) +
	      scale_y_continuous(limits=ylimits, expand=c(0,0))
	  } else {
	    p = p +
	      scale_x_continuous(expand=c(0,0)) +
	      scale_y_continuous(expand=c(0,0))
	  }
	  
	  #TT return(p)
	  # dynamically adjust map height
	  output$ggmap <- renderPlot(p)
	  if (exists("ylimits")) {
	    aspect_ratio = diff(ylimits)/diff(xlimits)
	  } else {
	    aspect_ratio = nrow(plot_map)/ncol(plot_map)
	  }
	  
	  if (aspect_ratio>0.4) {
	    npix = sqrt(500^2*aspect_ratio/0.4)
	    height = paste0(round(npix), "px")
	  } else {
	    height = '500px'
	  }
	  plotOutput('ggmap', height=height)
	})
	
	
	output$NoMapText <- renderText({
	  input$make_map
	  country_name = input$country
	  if (!all(c("map_metric", "map_date") %in% names(isolate(input)))){
	    return(NULL)
	  }
	  map_date = isolate(input$map_date)
	  overlay_metric = isolate(input$map_metric)
	  model_df = df2()
	  if (grepl(pattern=",", x=country_name, fixed=TRUE)) {
	    return("Maps for administrative regions not yet supported.")
	  } 
	  
	  if (overlay_metric=="1") {
	    metric_value = model_df$rtn[model_df$x==map_date]
	  } else if (overlay_metric=="2") {
	    metric_value = model_df$icu[model_df$x==map_date]
	  } else if (overlay_metric=="3") {
	    metric_value = model_df$ded[model_df$x==map_date]
	  }
	  
	  if (metric_value==0) {
	    return("On the selected date, the model 'Metric to map' is 0. No map produced. Please select a different date/metric combination.")
	  } else {
	    return(NULL)
	  }
	})
	
	
	output$download_map_button <- downloadHandler(
	  filename = function() {
	    map_date = input$map_date
	    file_format = input$map_file_format
	    
	    # match with country menu
	    country_name = input$country
	    map_index = map_info$country==country_name
	    map_units = map_info$precision[map_index]
	    
	    if (file_format=="GeoTIFF") {
	      overlay_metric = input$map_metric
	      if (overlay_metric=="1") {
	        metric_label = "new-infected"
	      } else if (overlay_metric=="2") {
	        metric_label = "new-icu"
	      } else if (overlay_metric=="3") {
	        metric_label = "cumul-death"
	      }
	      
	      out_filename = paste0(input$country, "_", map_units, "_", metric_label, "_", input$map_date, ".tif")
	    } else if (file_format=="Shapefile - .zip") {
	      out_filename = paste0(input$country, "_", map_units, "_", input$map_date, ".zip")
	    } else if (file_format=="Shapefile - .tgz") {
	      out_filename = paste0(input$country, "_", map_units, "_", input$map_date, ".tgz")
	    }
	    
	    return(out_filename)
	  },
	  content = function(con) {
	    withProgress(message="Generating Download", value=0, detail="preparing map", {
	      map_date = input$map_date
	      overlay_metric = input$map_metric
	      file_format = input$map_file_format
	      model_df = df2()
	      
	      # create list of available maps and match with country menu
	      country_name = input$country
	      map_index = map_info$country==country_name
	      map_filename = map_info$path[map_index]
	      map_units = map_info$precision[map_index]
	      
	      # load map
	      pop_map = raster(map_filename)
	      
	      if (file_format=="GeoTIFF") {
	        if (overlay_metric=="1") {
	          metric_value = model_df$rtn[model_df$x==map_date]
	        } else if (overlay_metric=="2") {
	          metric_value = model_df$icu[model_df$x==map_date]
	        } else if (overlay_metric=="3") {
	          metric_value = model_df$ded[model_df$x==map_date]
	        }
	        
	        plot_map = metric_value*pop_map
	        incProgress(2/10, detail="writing to file")
	        # save file
	        writeRaster(x=plot_map, filename=con, format="GTiff")
	        incProgress(6/10, detail="downloading")
	      } else if (file_format %in% c("Shapefile - .zip", "Shapefile - .tgz")) {
	        incProgress(2/10, detail="converting raster to polygons")
	        sp_map = rasterToPolygons(x=pop_map, fun=NULL, n=4, na.rm=TRUE)
	        # add daily infected, daily icu, and cumulative deaths
	        names(sp_map@data) = "pop_dist"
	        sp_map@data$daily_inf = sp_map@data$pop_dist * model_df$rtn[model_df$x==map_date]
	        sp_map@data$daily_icu = sp_map@data$pop_dist * model_df$icu[model_df$x==map_date]
	        sp_map@data$cml_death = sp_map@data$pop_dist * model_df$ded[model_df$x==map_date]
	        
	        filename_stub = paste0(input$country, "_", map_units, "_", input$map_date)
	        
	        incProgress(5/10, detail="writing shapefiles")
	        # write files to temp dir
	        temp_dir = tempdir()
	        raster::shapefile(x=sp_map, filename=file.path(temp_dir, filename_stub), overwrite=TRUE)
	        # tar files to out connection
	        files_list = file.path(temp_dir, paste0(filename_stub, c(".dbf", ".prj", ".shp", ".shx")))
	        if (file_format=="Shapefile - .zip") {
	          incProgress(8/10, detail="zipping shapefiles")
	          zip(con, files=files_list, flags="-j")
	        } else if (file_format=="Shapefile - .tgz") {
	          browser()
	          file_con = gzfile(con, open="wb")
	          tar(file_con, files=files_list, compression="gzip", extra_flags="-j")
	          close(file_con)
	        }
	        # clean-up shapefiles
	        file.remove(files_list)
	        incProgress(9/10, detail="downloading")
	      }
	    }) # end progress bar
	  } # end download content function
	)
	
	# ----------------------------------
	
	
	y1 <- reactive({
	  # No intervention profile
	  cov_hybrid(vecN = vecN(), vecI0 = vecI0(), R0 = isolate(R0()), Rp = isolate(Rp()), Rover = 0.95, vecPS = vecPS(), vecPM = vecPM(), vecPA = vecPA(), trig_pres = 99999999, trig_day = 99999999, trig_type = 0, icu_cap = icu_cap(), sevBchange = FALSE, trickle = 0, vecTcalc = vecTcalc, 
			nReals = nReals)
	})
	
	y2 <- reactive({ # This is the option of running with country fit - will need to add here when to apply country profile

	  #profile_id = isolate(profile.id())
	  mycountry = as.character(isolate(input$country))
		if (isolate(modulation()) == 1) {
			 #id = which(country_name == profile_id)
			
			id = which(country_name == mycountry)
			 vecRtrel = country_rt[[id]]
			 nold = length(vecTcalc)
			 myn = length(vecRtrel)
			 if (myn < nold ) {
			 	nadd = nold-myn
			 	 Rtrel_last = vecRtrel[myn]
			 	vecRtrel <- c(vecRtrel, rep(Rtrel_last, nadd ))
			 }

			 vecRtrel <- vecRtrel[1:nold]
			 
			 y2 <- cov_hybrid(vecN = vecN(), vecI0 = vecI0(), R0 = isolate(R0()), Rp = isolate(Rp()), Rover = 0.95, vecPS = vecPS(), vecPM = vecPM(), vecPA = vecPA(), trig_pres = 999999, trig_day = isolate(day2start()), trig_type = 2, sevBchange = FALSE, trickle = 0, vecTcalc = vecTcalc, 
			nReals = nReals, vecRtrel = vecRtrel)
		} else if (isolate(modulation()) == 2 ) {
		y2 <- cov_hybrid(vecN = vecN(), vecI0 = vecI0(), R0 = isolate(R0()), Rp = isolate(Rp()), Rover = 0.95, vecPS = vecPS(), vecPM = vecPM(), vecPA = vecPA(), trig_pres = isolate(trig_pres()), trig_day = isolate(trig_day()), trig_type = isolate(trig_type()), icu_cap = icu_cap(), scLim=c(isolate(trig_day()),99999), sevBchange = FALSE, trickle = 0, vecTcalc = vecTcalc, 
			nReals = nReals)			
		} else {
		y2 <- cov_hybrid(vecN = vecN(), vecI0 = vecI0(), R0 = isolate(R0()), Rp = isolate(Rp()), Rover = 0.95, vecPS = vecPS(), vecPM = vecPM(), vecPA = vecPA(), trig_pres = 999999, trig_day = 99999, trig_type = 3, icu_cap = icu_cap(),sevBchange = FALSE, trickle = 0, vecTcalc = vecTcalc, 
			nReals = nReals, vecRtrel = isolate(rt_mean()$Rt))			
		}

	y2
	})
	
	out1 <- reactive({
		process_cov_hybrid_output(nAges = nAges(), day1 = isolate(day1()), y = y1())
	})

	out2 <- reactive({
		process_cov_hybrid_output(nAges = nAges(), day1 = isolate(day1()), y = y2())
	})

	all.out1 <- reactive({
		process_cov_hybrid_output_total(nAges = nAges(), day1 = isolate(day1()), y = y1())
	})	
	
	all.out2 <- reactive({
		process_cov_hybrid_output_total(nAges = nAges(), day1 = isolate(day1()), y = y2())
	})	
		
		
	
	nmax <- reactive({
		nmax = ndays
		nmax1 = trim.data.in(longvec = rowSums(out1()$mrtn), change = belowOnset)	
		nmax2 = trim.data.in(longvec = rowSums(out2()$mrtn), change = belowOnset)	
		nmax = max(nmax1, nmax2,length(cases_df()$cases))
		nmax
	})
	
	df1 <- reactive({
		data.frame(x=all.out1()$ddates[1:nmax()], rtn = all.out1()$mrtn[1:nmax()], icu = all.out1()$micu[1:nmax()], ded = all.out1()$mded[1:nmax()], sdrtn = all.out1()$sdrtn[1:nmax()], sdicu = all.out1()$sdicu[1:nmax()], sdded = all.out1()$sdded[1:nmax()], Rt = y1()$rt_daily[1:nmax()], mcinf = all.out1()$mcinf[1:nmax()], day = 1:nmax())
	})

	df2 <- reactive({
		data.frame(x=all.out2()$ddates[1:nmax()], rtn = all.out2()$mrtn[1:nmax()], icu = all.out2()$micu[1:nmax()], ded = all.out2()$mded[1:nmax()], sdrtn = all.out2()$sdrtn[1:nmax()], sdicu = all.out2()$sdicu[1:nmax()], sdded = all.out2()$sdded[1:nmax()], Rt = y2()$rt_daily[1:nmax()], mcinf = all.out2()$mcinf[1:nmax()], day = 1:nmax())
	})
	
    ylabR <- reactive({
    	list(title = "", side = "right", overlaying = "y", color = "grey", zeroline = FALSE, showgrid = FALSE, showline = TRUE, showticklabels = FALSE, showticks = FALSE,  range = c(0,1.2 * isolate(R0())), visible = FALSE)
    })  
    

	
	ageNames <- reactive({
	  input$run_sim
	  popType = isolate(input$popType)
	if (as.numeric(popType) == 1) {
			ageNames <- civ_names
		} else {
			ageNames <- mil_names	
		}		
	})
	
	datasetInput <- reactive({ 
		make_table(df1 = df1(), out1 = out1(), all.out1 = all.out1(), df2 = df2(), out2 = out2(), all.out2 = all.out2(), nmax = nmax(), ageNames = ageNames(), obs_death = cum_conf_death_df()) 
		})
		
	output$table <- renderDataTable(datasetInput())
	
	## output$table <- renderDataTable(cum_conf_death_df())
	
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('results', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
 
 
	output$plot1 <- renderPlotly({

 	  mycountry = as.character(isolate(input$country))
	  myindex <- which(country_name == mycountry)
	  mypop = country_pop[[myindex]]
	  print_data = FALSE
	  if (isolate(Ntotal()) == mypop) print_data = TRUE
	  
	  hover_text_avg1 = paste0("Date: ",df1()$x, '<br>Day: ', df1()$day, "<br> Median Cases: ", df1()$rtn, "<br> S.D. Cases: ", df1()$sdrtn, "<br>Cum. Sum.", df1()$mcinf, "<br>R0: ", df1()$Rt) 
	  hover_text_avg2 = paste0("Date: ",df2()$x, '<br>Day: ', df2()$day, "<br> Median Cases: ", df2()$rtn, "<br> S.D. Cases: ", df2()$sdrtn, "<br>Cum. Sum.", df2()$mcinf, "<br>R(t): ",df2()$Rt)  
	  ym1 = max(df1()$rtn)
	  xm1 = which.max(df1()$rtn)	
	  ym2 = max(df2()$rtn)
	  xm2 = which.max(df2()$rtn)
	  xa = c(rep(df1()$x[round(0.92*nmax())], 3), df1()$x[xm1], df1()$x[xm2]-3, df1()$x[round(0.92*nmax())]) 
	  ya = c(0.9*max(df1()$rtn), 0.8*max(df1()$rtn), 0.7*max(df1()$rtn), ym1, ym2, 0.6*max(df1()$rtn))
	  
	  if (print_data) {
	  ta = c("Fixed R0", 'Modulated R(t)', 'JHU Data', as.character(ym1), as.character(ym2))

	  } else {
	  ta = c("Fixed R0", 'Modulated R(t)', '', as.character(ym1), as.character(ym2))
	  }
	  fc = c('blue', "rgba(255,120,0,0.9)", "green", "blue", "rgba(255,120,0,0.9)")	  		  	
	  a1 <- list( x= xa[1], y = ya[1], text = ta[1], font=list(size=14, color=fc[1]), showarrow = F)
	  a2 <- list( x= xa[2], y = ya[2], text = ta[2], font=list(size=14, color=fc[2]), showarrow = F)
	  a3 <- list( x= xa[3], y = ya[3], text = ta[3], font=list(size=14, color=fc[3]), showarrow = F)
	  a4 <- list( x= xa[4], y = ya[4], text = ta[4], font=list(size=14, color=fc[4]), showarrow = F, xanchor = 'right', yanchor = 'bottom')
	  a5 <- list( x= xa[5], y = ya[5], text = ta[5], font=list(size=14, color=fc[5]), showarrow = F, xanchor = 'left', yanchor = 'bottom', yref="y2")
	  #a = list(a1, a2, a3, a4, a5)
	  hover_text_r1    = paste0("Date: ",df2()$x, '<br> R0: ', df1()$Rt)
	  hover_text_r2    = paste0("Date: ",df2()$x, '<br> R(t): ', df2()$Rt)
	  
	  hover_text_obs_cases = paste0('Date: ', cases_df()$dates, "<br> Reported Daily Cases: ", cases_df()$cases)
	  
	  x   = df1()$x
	  y1u = rowSums(out1()$upprtn[1:nmax(), ])
	  y1l = rowSums(out1()$lowrtn[1:nmax(), ])
	  
	  xaxis <- list(anchor="free", position=0.0)
	  
	  yaxis1 <- list(title = "Daily Incidence, Fixed R0", color = "blue", side="left", anchor="free", position=0.0, rangemode="tozero")
	  
	  y2u = rowSums(out2()$upprtn[1:nmax(), ])
	  y2l = rowSums(out2()$lowrtn[1:nmax(), ])
	  # scale data axis to be comparable to the model for the same time range
	  data_range = range(cases_df()$dates)
	  data_max = max(cases_df()$cases)
	  data_cumul = sum(cases_df()$cases)
	  model_max = max(y2u)
	  axis2_limits = c(0, model_max*1.05)
	  model_range_index = df2()$x >= data_range[1] & df2()$x <= data_range[2]
	  model_range_max = max(df2()$rtn[model_range_index])
	  model_range_cumul = sum(df2()$rtn[model_range_index])
	  # first scale to cumulative
	  data_upper_equiv = model_range_max*data_cumul/model_range_cumul
	  data_upper_limit = data_upper_equiv*model_max/model_range_max
	  # if that 'chops' any of the data, adjust up to data max
	  data_upper_limit = max(data_upper_limit, data_max)
	  axis4_limits = c(0, data_upper_limit*1.05)
	  
	  yaxis2 <- list(title = "Daily Incidence, Modulated R(t)", side = "right",  overlaying = "y", color = "rgba(255,140,0,1)", zeroline = FALSE, showgrid = FALSE, showline = FALSE, anchor="free", position=1.0, range=axis2_limits)
	  
	  yaxis4 <- list(title=list(text="", standoff=65), side = "right", overlaying = "y", color = "green", zeroline = FALSE, showgrid = FALSE, showline = FALSE, range=axis4_limits) #, visible = FALSE)

	  # Do a simple linear regression to finf the scaling between the model and the data
	  if (print_data) {
	  	xdata = cases_df()$cases
	  	ymodel = df2()$rtn[model_range_index]
	  	coeff = coef(lm(ymodel ~ xdata))[[2]]
	  	coeff = round(coeff, digits = 2)

	  	a6 = list(x = xa[6], y = ya[6], text = paste0("Scaling ~ ", coeff), font = list(size = 14, color = fc[3]), showarrow = F)
	  } else {
	  	a6 = list(x = xa[6], y = ya[6], text = "", font = list(size = 14, color = fc[3]), showarrow = F)
	  }
  	  
  	  # Add to the list that we are displaying
  	  #
  	  a = list(a1, a2, a3, a4, a5, a6)
  	  
	  pl1 <- plot_ly()
	  pl1 <- add_trace(pl1, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
	                   showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y' )
	  pl1 <- add_trace(pl1, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
	                   showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y', fill = 'tonexty', fillcolor =  "#C9EFF9")                  
	  for (ii in 1:nAges()) {
	    y1u = out1()$upprtn[1:nmax(), ii]
	    y1l = out1()$lowrtn[1:nmax(), ii] 	
	    pl1 <- add_trace(pl1, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
	                     showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y' )
	    pl1 <- add_trace(pl1, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
	                     showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y', fill = 'tonexty', fillcolor =  "#C9EFF9") 	 		
	  }
	  
	  
	  pl1 <- add_trace(pl1, y = y2l, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.4)"),
	                   showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2' )
	  pl1 <- add_trace(pl1, y = y2u, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.4)"),
	                   showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2', fill = 'tonexty', fillcolor =  "rgba(255,140,0,0.4)")
	  
	  for (ii in 1:nAges()) {
	    y1u = out2()$upprtn[1:nmax(), ii]
	    y1l = out2()$lowrtn[1:nmax(), ii] 	
	    pl1 <- add_trace(pl1, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.3)"),
	                     showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2' )
	    pl1 <- add_trace(pl1, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.3)"),
	                     showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2', fill = 'tonexty', fillcolor =  "rgba(255,140,0,0.3)") 	 		
	  }   
	  
	  pl1 <-add_trace(pl1, x = df1()$x, y = df1()$rtn, type="scatter", mode="lines", line = list(color = as.factor("blue"), width = 2), name = 'median', showlegend = FALSE, legendgroup = 'cases', inherit = TRUE, yaxis = 'y', hoverinfo = 'text', text = hover_text_avg1,  cliponaxis = FALSE)  	 
	  
	  pl1 <-add_trace(pl1, x = df2()$x, y = df2()$rtn, type="scatter", mode="lines",
	                  line = list(color = as.factor("rgba(255,120,0,0.8)"), width = 2), name = 'median',
	                  showlegend = FALSE, legendgroup = 'cases', inherit = TRUE, yaxis = 'y2',
	                  hoverinfo = 'text', text = hover_text_avg2,  cliponaxis = FALSE) 
	  
	  
	  if (print_data) {
	  pl1 <- add_trace(pl1, x = cases_df()$dates, y = cases_df()$cases, type = "scatter", mode = "lines",
	                   line = list(color = as.factor("rgba(0,128,0,0.5)"), width = 2), name = 'conf_daily',
	                   showlegend = FALSE, legendgroup = 'daily_cases', inherit = TRUE, yaxis = 'y4',
	                   hoverinfo = 'text', text = hover_text_obs_cases,  cliponaxis = FALSE) %>%                   
	    layout(hovermode="x",  xaxis=xaxis, yaxis=yaxis1, yaxis2=yaxis2, yaxis4=yaxis4, title = '', annotations = a, showlegend = FALSE, legend = list(x=0.9, y=0.9), margin = list(pad=55, t=50, r=110, b=0, l=50))	  	
	  } else {
	    pl1 <- layout(pl1, hovermode="x",  xaxis=xaxis, yaxis=yaxis1, yaxis2=yaxis2, title = '', annotations = a, showlegend = FALSE, legend = list(x=0.9, y=0.9), margin = list(t = 50, r = 50, b = 0, l = 50))
	  	
	  }
	    	  
	  pl1
	})

 output$plot2 <- renderPlotly({
 
 	hover_text_avg1 = paste0("Date: ",df1()$x, '<br>Day: ', df1()$day, "<br> Median ICU: ", df1()$icu, "<br> S.D. ICU: ", df1()$sdicu, "<br>R0: ", df1()$Rt) 
	hover_text_avg2 = paste0("Date: ",df2()$x, '<br>Day: ', df2()$day, "<br> Median ICU: ", df2()$icu, "<br> S.D. ICU: ", df2()$sdicu, "<br>R(t): ", df2()$Rt)  
	hover_text_r1    = paste0("Date: ",df2()$x, '<br> R0: ', df1()$Rt) 
	hover_text_r2    = paste0("Date: ",df2()$x, '<br> R(t): ', df2()$Rt) 
	ym1 = max(df1()$icu)
	xm1 = which.max(df1()$icu)	
	ym2 =  max(df2()$icu)
	xm2 = which.max(df2()$icu)	
	xa = c(rep(df1()$x[round(0.92*nmax())], 3), df1()$x[xm1], df1()$x[xm2]-3)
	ya = c(0.9*max(rowSums(out1()$uppicu[1:nmax(),])), 0.8*max(rowSums(out1()$uppicu[1:nmax(),])), 0.7*max(df1()$icu), ym1, ym2)
	ta = c("Fixed R0", 'Modulated R(t)', "",  as.character(ym1), as.character(ym2))
	fc = c('blue', "rgba(255,120,0,0.9)", "green", "blue", "rgba(255,120,0,0.9)")
	
	a1 <- list( x= xa[1], y = ya[1], text = ta[1], font=list(size=14, color=fc[1]), showarrow = F)
	a2 <- list( x= xa[2], y = ya[2], text = ta[2], font=list(size=14, color=fc[2]), showarrow = F)
	a3 <- list( x= xa[3], y = ya[3], text = ta[3], font=list(size=14, color=fc[3]), showarrow = F)
	a4 <- list( x= xa[4], y = ya[4], text = ta[4], font=list(size=14, color=fc[4]), showarrow = F, xanchor = 'right', yanchor = 'bottom')
	a5 <- list( x= xa[5], y = ya[5], text = ta[5], font=list(size=14, color=fc[5]), showarrow = F, xanchor = 'left', yanchor = 'bottom', yref="y2")
	a = list(a1, a2, a3, a4, a5) 
	
	yaxis2 <- list(title = "Daily ICU, Modulated R(t)", side = "right", overlaying = "y", color = "rgba(255,140,0,1)", zeroline = FALSE, showgrid = FALSE, showline = FALSE)
 	pl2 <- plot_ly() 
 	
 	x = df1()$x
 	y1u = rowSums(out1()$uppicu[1:nmax(),])
 	y1l = rowSums(out1()$lowicu[1:nmax(),])
 
     pl2 <- plot_ly()
	pl2 <- add_trace(pl2, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y' )
    pl2 <- add_trace(pl2, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y', fill = 'tonexty', fillcolor =  "#C9EFF9")     
                                   
  	for (ii in 1:nAges()) {
 	y1u = out1()$uppicu[1:nmax(), ii]
 	y1l = out1()$lowicu[1:nmax(), ii] 	
	pl2 <- add_trace(pl2, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y' )
    pl2 <- add_trace(pl2, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y', fill = 'tonexty', fillcolor =  "#C9EFF9") 	 		
  	}
		
 	y1u = rowSums(out2()$uppicu[1:nmax(),])
 	y1l = rowSums(out2()$lowicu[1:nmax(),])
	pl2 <- add_trace(pl2, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.4)"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2' )
    pl2 <- add_trace(pl2, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.4)"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2', fill = 'tonexty', fillcolor =  "rgba(255,140,0,0.4)")
                      
  	for (ii in 1:nAges()) {
 	y1u = out2()$uppicu[1:nmax(), ii]
 	y1l = out2()$lowicu[1:nmax(), ii] 	
	pl2 <- add_trace(pl2, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.3)"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2' )
    pl2 <- add_trace(pl2, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.3)"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2', fill = 'tonexty', fillcolor =  "rgba(255,140,0,0.3)") 	 		
  	}   
  	
    pl2 <-add_trace(pl2, x = df1()$x, y = df1()$icu, type="scatter", mode="lines",
                   line = list(color = as.factor('blue'), width = 2), name = 'median',
                   showlegend = FALSE, legendgroup = 'cases', inherit = TRUE, yaxis = 'y',
                   hoverinfo = 'text', text = hover_text_avg1,  cliponaxis = FALSE) 
                   
    pl2 <-add_trace(pl2, x = df2()$x, y = df2()$icu, type="scatter", mode="lines",
                   line = list(color = as.factor('rgba(255,120,0,0.8)'), width = 2), name = 'median',
                   showlegend = FALSE, legendgroup = 'cases', inherit = TRUE, yaxis = 'y2',
                   hoverinfo = 'text', text = hover_text_avg2,  cliponaxis = FALSE) %>%
 
		 layout(hovermode="x",  yaxis = ylab2, yaxis2 = yaxis2,  title = '' , annotations = a, showlegend = FALSE, legend = list(x=0.9, y=0.9), margin = list(t = 50, r = 50, b = 0, l = 50))                              
    pl2
 })


 output$plot3 <- renderPlotly({
 
 	  mycountry = as.character(isolate(input$country))
	  myindex <- which(country_name == mycountry)
	  mypop = country_pop[[myindex]]
	  print_data = FALSE
	  if (isolate(Ntotal()) == mypop) print_data = TRUE

 	
	hover_text_avg1 = paste0("Date: ",df1()$x, '<br>Day: ', df1()$day, "<br> Median Cum. Death: ", df1()$ded, "<br> S.D. Cum. Death: ", df1()$sdded, "<br>R0: ", df1()$Rt) 
	hover_text_avg2 = paste0("Date: ",df2()$x, '<br>Day: ', df2()$day, "<br> Median Cum. Death: ", df2()$ded, "<br> S.D. Cum. Death: ", df2()$sdded, "<br>R(t): ", df2()$Rt)  
	hover_text_r1    = paste0("Date: ",df2()$x, '<br> R0: ', df1()$Rt) 
	hover_text_r2    = paste0("Date: ",df2()$x, '<br> R(t): ', df2()$Rt) 
	xa = rep(df1()$x[round(0.04*nmax())], 3)
	ya = c(0.92*max(rowSums(out1()$uppded[1:nmax(),])), 0.8*max(rowSums(out1()$uppded[1:nmax(),])), 0.7*max(rowSums(out1()$uppded[1:nmax(),])))
	if (print_data){
		ta = c(paste0("Fixed R0: ", df1()$ded[nmax()]), paste0('Modulated R(t): ', df2()$ded[nmax()]), "JHU Data")
	} else {
		ta = c(paste0("Fixed R0: ", df1()$ded[nmax()]), paste0('Modulated R(t): ', df2()$ded[nmax()]), "")
	}
	
	fc = c('blue', "rgba(255,120,0,0.9)", "green")

	a1 <- list( x= xa[1], y = ya[1], text = ta[1], font=list(size=14, color=fc[1]), showarrow = F, xanchor = 'left')
	a2 <- list( x= xa[2], y = ya[2], text = ta[2], font=list(size=14, color=fc[2]), showarrow = F, xanchor = 'left')
	a3 <- list( x= xa[3], y = ya[3], text = ta[3], font=list(size=14, color=fc[3]), showarrow = F, xanchor = 'left')
	
	a = list(a1, a2, a3)
		
	hover_text_obs_death = paste0('Date: ', cum_conf_death_df()$dates, "<br> Reported Cum. Death: ", cum_conf_death_df()$cum_death)
	

	
	yaxis2 <- list(title = "Cumulative Deaths, Modulated R(t)", side = "right", overlaying = "y", color = "rgba(255,140,0,1)", zeroline = FALSE, showgrid = FALSE, showline = FALSE)
 	pl3 <- plot_ly() 	
 		
 	x = df1()$x
 	y1u = rowSums(out1()$uppded[1:nmax(),])
 	y1l = rowSums(out1()$lowded[1:nmax(),])
 
    pl3 <- plot_ly()
	pl3 <- add_trace(pl3, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y' )
    pl3 <- add_trace(pl3, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y', fill = 'tonexty', fillcolor =  "#C9EFF9")     
                                   
  	for (ii in 1:nAges()) {
 	y1u = out1()$uppded[1:nmax(), ii]
 	y1l = out1()$lowded[1:nmax(), ii] 	
	pl3 <- add_trace(pl3, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y' )
    pl3 <- add_trace(pl3, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "#C9EFF9"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y', fill = 'tonexty', fillcolor =  "#C9EFF9") 	 		
  	}
		
 	y1u = rowSums(out2()$uppded[1:nmax(),])
 	y1l = rowSums(out2()$lowded[1:nmax(),])
	pl3 <- add_trace(pl3, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.4)"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2' )
    pl3 <- add_trace(pl3, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.4)"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2', fill = 'tonexty', fillcolor =  "rgba(255,140,0,0.4)")
                      
  	for (ii in 1:nAges()) {
 	y1u = out2()$uppded[1:nmax(), ii]
 	y1l = out2()$lowded[1:nmax(), ii] 	
	pl3 <- add_trace(pl3, y = y1l, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.3)"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2' )
    pl3 <- add_trace(pl3, y = y1u, x = x, type ='scatter', mode = 'lines', line = list(color = "rgba(255,140,0,0.3)"),
                      showlegend = FALSE, inherit = TRUE, hoverinfo = "none",  cliponaxis = FALSE, type = "scatter", yaxis = 'y2', fill = 'tonexty', fillcolor =  "rgba(255,140,0,0.3)")	 		
  	}   
    pl3 <-add_trace(pl3, x = df1()$x, y = df1()$ded, type="scatter", mode="lines",
                   line = list(color = as.factor('blue'), width = 2), name = 'median',
                   showlegend = FALSE, legendgroup = 'cases', inherit = TRUE, yaxis = 'y',
                   hoverinfo = 'text', text = hover_text_avg1,  cliponaxis = FALSE) 	 
 	 
    pl3 <-add_trace(pl3, x = df2()$x, y = df2()$ded, type="scatter", mode="lines",
                   line = list(color = as.factor('rgba(255,120,0,0.8)'), width = 2), name = 'median',
                   showlegend = FALSE, legendgroup = 'cases', inherit = TRUE, yaxis = 'y2',
                   hoverinfo = 'text', text = hover_text_avg2,  cliponaxis = FALSE) 
                   
   ## Add Observed confirmed Death 
	  
	  if (print_data) {
   pl3 <- add_trace(pl3, x = cum_conf_death_df()$dates, y = cum_conf_death_df()$cum_death, type = "scatter", mode = "lines",
   					line = list(color = as.factor("rgba(0,128,0,0.5)"), width = 3), name = 'cum_death',
                   showlegend = FALSE, legendgroup = 'cum_death', inherit = TRUE, yaxis = 'y2',
                   hoverinfo = 'text', text = hover_text_obs_death,  cliponaxis = FALSE) 
                   
 	}
	pl3 <- layout(pl3, hovermode = "x", yaxis = ylab3, yaxis2 = yaxis2, title = "", annotations = a, showlegend = FALSE, legend = list(x = 0.9, y = 0.9), margin = list(t = 50, r = 50, 
		b = 0, l = 50))
               
                   
    pl3
 })
 	
 
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
}

shinyApp(ui, server)
