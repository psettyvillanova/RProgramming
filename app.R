

# packages required

library(shiny)
library(tidyverse)
library(dplyr)


# define url where CDC death count data is located, set NA values to 0
read.csv("https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD") -> cause
cause[is.na(cause)] <- 0
read.csv("https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD") -> hist_cause
hist_cause[is.na(hist_cause)] <- 0

# create list of states for selection
cause %>%
  group_by(Jurisdiction.of.Occurrence) %>%
  summarise(count = n_distinct(Jurisdiction.of.Occurrence)) -> state_list

# seperate 2019/2020 data into seperate tables
filter(cause, MMWR.Year == 2019) -> cause_19
filter(cause, MMWR.Year == 2020) -> cause_20



## Merge both Cause and Hist_cause table to show in the same plot
hist_cause %>%
  mutate(flag_cov19ucod="N",
         flag_cov19mcod="N",
         COVID.19..U071..Underlying.Cause.of.Death.=0,
         COVID.19..U071..Multiple.Cause.of.Death.=0) -> hist_cause

cause %>%
  rename(All..Cause=All.Cause) -> cause

hist_cause%>% mutate(flag_allcause=as.logical(flag_allcause)) -> hist_cause

hist_cause%>% mutate(flag_natcause=as.logical(flag_natcause)) -> hist_cause

cause%>% mutate(flag_allcause=as.logical(flag_allcause)) -> cause

cause%>% mutate(flag_natcause=as.logical(flag_natcause)) -> cause

cause %>%
  rename(Influenza.and.pneumonia..J10.J18.=Influenza.and.pneumonia..J09.J18.) -> cause

union(cause,hist_cause) -> causetotal



# Generate weekly means from 2014-2020 data and change column names to make them cleaner
causetotal %>%
  group_by(Jurisdiction.of.Occurrence,MMWR.Week) %>%
  summarize_at(vars(All..Cause:Cerebrovascular.diseases..I60.I69.), ~ mean(.x, na.rm = TRUE)) %>%
  rename(All = All..Cause, Natural = Natural.Cause, Septicemia = Septicemia..A40.A41., Malignant_Neoplasms = Malignant.neoplasms..C00.C97., 
         Diabetes = Diabetes.mellitus..E10.E14., Alzheimers = Alzheimer.disease..G30., Influenza_Pneumonia = Influenza.and.pneumonia..J10.J18., 
         Lower_Respiratory = Chronic.lower.respiratory.diseases..J40.J47., Other_Respiratory = Other.diseases.of.respiratory.system..J00.J06.J30.J39.J67.J70.J98.,
         Nephritis = Nephritis..nephrotic.syndrome.and.nephrosis..N00.N07.N17.N19.N25.N27., 
         Unknown = Symptoms..signs.and.abnormal.clinical.and.laboratory.findings..not.elsewhere.classified..R00.R99.,
         Heart_Disease = Diseases.of.heart..I00.I09.I11.I13.I20.I51., Cerebrovascular = Cerebrovascular.diseases..I60.I69.) -> dis_mean_hist


# Define UI for application that produces a ggimage plot for two selected metrics
# This will produce a drop down for state selection and a checkbox field for disease selection
ui <- pageWithSidebar(
  
  # Application title
  headerPanel("Cause of Death Comparison to COVID-19"),
  
  # Selectors for state and disease
  sidebarPanel(
    selectInput('state','State', state_list$Jurisdiction.of.Occurrence),
    checkboxGroupInput("variable", "Diseases to show:",
                       colnames(dis_mean_hist[3:15]))
               
  ),
  mainPanel(
    plotOutput('plot1'),
   verbatimTextOutput("helper")
   )
 
)



# Define server logic required to produce a line graph


server <- function(input, output) {
  
  # packages required to run in the server
  library(shiny)
  library(tidyverse)
  library(dplyr)


# create COV_20_state reactive to be joined with dis_mean_hist used in renderPlot  
   COV_20_state <- reactive({
     cause_20 %>%
       mutate(COVID_19 = COVID.19..U071..Multiple.Cause.of.Death.+COVID.19..U071..Underlying.Cause.of.Death.) %>%
       filter(Jurisdiction.of.Occurrence == input$state) %>%
       select(MMWR.Week, COVID_19)
       # %>% add_row(MMWR.Week = 26:53, COVID_19 = 0:0) #Adds rows to make 2020 same length as other years
     
 }) 
 
# Create plot data object which contains the selected diseases and COVID-19
  # x <- input$variable
   
   
     plot_data <- reactive({
       
       x <- input$variable
      
       if (is.null(x)) {
         dis_mean_hist %>%
           filter(Jurisdiction.of.Occurrence == input$state) %>%
           left_join(COV_20_state(), by = c("MMWR.Week" = "MMWR.Week")) %>%
           gather(All:COVID_19, key = "Disease", value = "Cases") %>%
           filter(Disease == "COVID_19") 
         
       }else{
       dis_mean_hist %>%
         filter(Jurisdiction.of.Occurrence == input$state) %>%
         left_join(COV_20_state(), by = c("MMWR.Week" = "MMWR.Week")) %>%
         gather(All:COVID_19, key = "Disease", value = "Cases") %>%
         filter(Disease == input$variable[1] | Disease == input$variable[2] | Disease == input$variable[3] | Disease == "COVID_19" ) 
       }
     })
   
   






   output$plot1 <- renderPlot({
     
# construct of ggplot
     
   plot_data() %>%
       ggplot(aes(x = MMWR.Week, y = Cases, color = Disease)) +
       geom_line(size=1) +
       xlab("Week of the Year") +
       ylab("Total Weekly Deaths") +
       scale_x_continuous(breaks = seq(1,52,2))
      
       
       
       
    
  })
 #  output$helper <- renderPrint({  str(plot_data())   })
}
# Run the application 
shinyApp(ui = ui, server = server)
