#############################   GLOBAL  ########################################

################################################################################
########                   LIBRARIES                                    ########
################################################################################
library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(futile.logger)
library(rlang)


################################################################################
########                   DATA                                     ############
################################################################################


#to do:
#' add posssibility to select more years in barplot
#' add lines connecting points to 0.5 line in barplot
#' in both piecharts sort them in clockwise order with other as last category

asylum = readRDS("asylum.rds")

#fix geo names
asylum$geo[which(asylum$geo == "Germany (until 1990 former territory of the FRG)")] = "Germany"
asylum$geo[which(asylum$geo == "European Union - 27 countries (from 2020)")] = "EU - 27"
asylum$geo[which(asylum$geo == "European Union - 28 countries (2013-2020)")] = "EU - 28"


#fix citizen names
asylum$citizen[which(asylum$citizen == "Democratic Republic of the Congo")] = "Congo"
asylum$citizen[which(asylum$citizen == "China including Hong Kong")] = "China"
asylum$citizen[which(asylum$citizen == "Germany (until 1990 former territory of the FRG)")] = "Germany"
asylum$citizen[which(asylum$citizen == "European Union - 27 countries (from 2020)")] = "EU - 27"
asylum$citizen[which(asylum$citizen == "European Union - 28 countries (2013-2020)")] = "EU - 28"
asylum$citizen[which(asylum$citizen == "Federated States of Micronesia")] = "Micronesia"
asylum$citizen[which(asylum$citizen == "Kosovo (under United Nations Security Council Resolution 1244/99)")] = "Kosovo"


################################################################################
##                        UI FUNCTIONS                                        ##
################################################################################




# Piechart UI-------------------------------------------------------------------
pieUI = function(title, ID, text, threshref, timeID, thresholdID, 
                 geoID, sexID, ageID, notcit = T, dataname, htmlout){
  tabPanel(title,
           fluidRow(
             column(10,
                    style = "background-color:#E6EEFF;",
                    plotlyOutput(outputId = ID),
                    htmlOutput(htmlout, style="text-align: justify;font-weight: bold;"),
                    text,
                    p("The user can select the time window, the country, the number of applications threshold for
                                      a provenience country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"), 
                    p(paste("Data comes from eurostat database", dataname), style="text-align: justify;")
                    ),
             column(2,
                    style = "background-color:#E6EEFF;",
                    sliderInput(inputId = timeID,
                                label = "Time window you want to consider",
                                min = 2008,
                                max = 2020,
                                value = c(2008,2020),
                                sep = ""),
                    sliderInput(inputId = thresholdID,
                                label = paste("Number of", threshref, "threshold"),
                                min = min(asylum$Applications, na.rm = T),
                                max = max(asylum$Applications, na.rm = T),
                                value= c(range(asylum$Applications, na.rm = T))),
                    selectInput(inputId = geoID,
                                label = "Select country",
                                choices = if(notcit == T){unique(asylum$geo)[order(unique(asylum$geo))]}
                                          else{unique(asylum$citizen)[order(unique(asylum$citizen))]},
                                selected = if(notcit == T){"Belgium"}else{"Syria"}),
                    selectInput(inputId = sexID,
                                label = "Sex",
                                choices = unique(asylum$sex),
                                selected = "T"),
                    selectInput(inputId = ageID,
                                label = "Age",
                                choices = unique(asylum$age),
                                selected = "TOTAL")
                    )
           )
  )
}


################################################################################
##                      DATA  FUNCTIONS                                       ##
################################################################################

# data creation-----------------------------------------------------------------
crData = function(geovar, sexvar, agevar, time1, time2){
  asylum %>% 
    filter(geo == geovar & sex == sexvar & age == agevar) %>% 
    filter(time >= time1,
           time <= time2) %>% 
    filter(!citizen %in% c("Total", "EU - 27",                        
                           "EU - 28", "Extra-EU27 (from 2020)",
                           "Extra-EU28 (2013-2020)")) %>% 
    group_by(geo, citizen) %>% 
    summarise(Applications = sum(Applications, na.rm = T),
              Genconv_first = sum(Genconv_first, na.rm = T),
              Humstat_first = sum(Humstat_first, na.rm = T),
              Rejected_first = sum(Rejected_first, na.rm = T),
              Sub_prot_first = sum(Sub_prot_first, na.rm = T),
              Temp_prot_first = sum(Temp_prot_first, na.rm = T),
              Total_first = sum(Total_first, na.rm = T),
              Total_pos_first = sum(Total_pos_first, na.rm = T),
              Genconv_def = sum(Genconv_def, na.rm = T),
              Humstat_def = sum(Humstat_def, na.rm = T),
              Rejected_def = sum(Rejected_def, na.rm = T),
              Sub_prot_def = sum(Sub_prot_def, na.rm = T),
              Temp_prot_def = sum(Temp_prot_def, na.rm = T),
              Total_def = sum(Total_def, na.rm = T),
              Total_pos_def = sum(Total_pos_def, na.rm = T))%>% 
    mutate(RR_f = Total_pos_first/Total_first,
           RR_d = Total_pos_def/Total_def)
}





# update dataset----------------------------------------------------------------
upData = function(data, trvar, threshold1, threshold2){
  data[which(trvar %in% seq(threshold1, threshold2)),]
}

# update geo piechart dataset---------------------------------------------------
upDatageo = function(data, trvar, threshold1, threshold2){
  flog.info("Updating data geo piechart...")
  data$dum = ifelse(trvar >= threshold1 & trvar <= threshold2,
                    1,
                    0)
  data %>% 
    mutate(citizen = ifelse(dum == 1, citizen, "Other")) %>% 
    group_by(citizen) %>% 
    summarise(Applications = sum(Applications, na.rm = T),
              Genconv_first = sum(Genconv_first, na.rm = T),
              Humstat_first = sum(Humstat_first, na.rm = T),
              Rejected_first = sum(Rejected_first, na.rm = T),
              Sub_prot_first = sum(Sub_prot_first, na.rm = T),
              Temp_prot_first = sum(Temp_prot_first, na.rm = T),
              Total_first = sum(Total_first, na.rm = T),
              Total_pos_first = sum(Total_pos_first, na.rm = T),
              Genconv_def = sum(Genconv_def, na.rm = T),
              Humstat_def = sum(Humstat_def, na.rm = T),
              Rejected_def = sum(Rejected_def, na.rm = T),
              Sub_prot_def = sum(Sub_prot_def, na.rm = T),
              Temp_prot_def = sum(Temp_prot_def, na.rm = T),
              Total_def = sum(Total_def, na.rm = T),
              Total_pos_def = sum(Total_pos_def, na.rm = T))
}


# data creation citizen--------------------------------------------------------- 

crDatacit = function(geovar, sexvar, agevar, time1, time2){
  asylum %>% 
    filter(citizen == geovar & sex == sexvar & age == agevar) %>% 
    filter(time >= time1,
           time <= time2) %>% 
    filter(!geo %in% c("Total", "EU - 27",                        
                       "EU - 28", "Extra-EU27 (from 2020)")) %>%  
    group_by(citizen, geo) %>% 
    summarise(Applications = sum(Applications, na.rm = T),
              Genconv_first = sum(Genconv_first, na.rm = T),
              Humstat_first = sum(Humstat_first, na.rm = T),
              Rejected_first = sum(Rejected_first, na.rm = T),
              Sub_prot_first = sum(Sub_prot_first, na.rm = T),
              Temp_prot_first = sum(Temp_prot_first, na.rm = T),
              Total_first = sum(Total_first, na.rm = T),
              Total_pos_first = sum(Total_pos_first, na.rm = T),
              Genconv_def = sum(Genconv_def, na.rm = T),
              Humstat_def = sum(Humstat_def, na.rm = T),
              Rejected_def = sum(Rejected_def, na.rm = T),
              Sub_prot_def = sum(Sub_prot_def, na.rm = T),
              Temp_prot_def = sum(Temp_prot_def, na.rm = T),
              Total_def = sum(Total_def, na.rm = T),
              Total_pos_def = sum(Total_pos_def, na.rm = T))
}

# data update citizen-----------------------------------------------------------
upDatacit = function(data, trvar, threshold1, threshold2){
  flog.info("Updating data citizenship pie...")
  data$dum = ifelse(trvar >= threshold1 & trvar <= threshold2,
                    1,
                    0)
  data %>% 
    mutate(geo = ifelse(dum == 1, geo, "Other")) %>% 
    group_by(citizen, geo) %>% 
    summarise(Applications = sum(Applications, na.rm = T),
              Genconv_first = sum(Genconv_first, na.rm = T),
              Humstat_first = sum(Humstat_first, na.rm = T),
              Rejected_first = sum(Rejected_first, na.rm = T),
              Sub_prot_first = sum(Sub_prot_first, na.rm = T),
              Temp_prot_first = sum(Temp_prot_first, na.rm = T),
              Total_first = sum(Total_first, na.rm = T),
              Total_pos_first = sum(Total_pos_first, na.rm = T),
              Genconv_def = sum(Genconv_def, na.rm = T),
              Humstat_def = sum(Humstat_def, na.rm = T),
              Rejected_def = sum(Rejected_def, na.rm = T),
              Sub_prot_def = sum(Sub_prot_def, na.rm = T),
              Temp_prot_def = sum(Temp_prot_def, na.rm = T),
              Total_def = sum(Total_def, na.rm = T),
              Total_pos_def = sum(Total_pos_def, na.rm = T))
}



################################################################################
##                              CHART FUNCTIONS                               ##
################################################################################

# BARPLOT ----------------------------------------------------------------------
barDef = function(data, decisions, pos_decisions, name, sortvar, denom,
                  genconv, humstat, subprot, RR, geo, time1, time2){
    req(data)
    plot_ly() %>%
      add_trace(data = data,
                name = "Number of decisions",
                text = "Number of decisions",
                y = as.formula(paste0('~', decisions)),
                x = ~ citizen,
                type = "bar",
                marker = list(color = "darkorange",
                              line = list(color = "black",
                                          width = 0.5)),
                hovertemplate = paste("Number of decisions on %{x}: <br> %{y}")) %>% 
      add_trace(data = data,
                name = paste("Total asylums granted at", name),
                y = as.formula(paste0('~', pos_decisions)),
                x = ~ citizen,
                type = "bar",
                color = I("green"),
                hovertemplate = paste("Total number of asylums granted to %{x}: %{y}<br>",
                                      "Of which for: <br>", 
                                      " - Geneve convention: ", genconv, "(", round(genconv/denom, digits = 4)*100, "%)<br>",
                                      " - Humanitarian status: ", humstat, "(", round(humstat/denom, digits = 4)*100, "%)<br>",
                                      " - Subsidiary protection: ", subprot, "(", round(subprot/denom, digits = 4)*100, "%)<br>")
      ) %>%
      add_trace(data = data,
                name = "Recognition rate at first istance",
                y = as.formula(paste0('~', RR)),
                x = ~ citizen,
                type = "scatter",
                mode = "markers",
                color = I("darkred"),
                yaxis = "y2",
                size = 5) %>% 
      add_trace(data = data,
                name = "",
                text = "",
                type = "bar",
                x = ~ citizen,
                y = as.formula(paste0('~', RR, "-0.5")),
                base = 0.5,
                opacity = 0.2,
                width = 0.1,
                yaxis = "y2",
                showlegend = F,
                color = I("darkred")) %>% 
      layout(xaxis = list(categoryorder = "array",
                          categoryarray = data$citizen[order(sortvar, decreasing = T)])) %>% 
      layout(plot_bgcolor='white', 
             barmode = "overlay",
             title = paste("Recognition rates at", name, "of", geo, "in", time1, "-", time2),
             legend = list(title = list(text = "<b> Legend </b>"),
                           x = 1.1,
                           y = 1),
             xaxis = list( 
               title = list(title = "<b> Asylum seekers citizenship </b>"),
               zerolinecolor = 'black', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               title = "",
               zerolinecolor = 'black', 
               zerolinewidth = 2, 
               gridcolor = 'grey'),
             yaxis2 = list(
               zerolinecolor = "#B22222",
               gridcolor = "#B22222",
               tickvals = list(0, 0.5, 1),
               gridwidth = 2,
               zerolinewidth = 2,
               overlaying = "y",
               side = "right",
               title = "<b> Recognition rate </b>",
               tickfont = list(color = "#B22222", size = 14),
               tickangle = 270
             ),
             margin = list(
               l = 0,
               r = 0,
               b = 150,
               t = 30,
               pad = 20)
      ) 

}



# GEO PIECHART------------------------------------------------------------------

pieDefgeo = function(data, variable){
    req(data)
    flog.info("Preparing geo piechart...")
    a = data %>% 
      filter(citizen != "Other") %>%
      arrange_(.dots = variable) %>% 
      ungroup() %>% 
      select(citizen)
    flog.info("'a' is created")
    a = as.vector(a$citizen)
    srt = c("Other", rev(a))
    flog.info("'srt' is created")
    plot_ly() %>% 
      add_trace(data = data %>% slice(match(srt, citizen)),
                labels = ~citizen,
                values = ~get(variable),
                textposition = 'inside',
                textinfo = 'label+percent',
                type = "pie",
                showlegend = F,
                sort = F) %>% 
      layout(
             xaxis = list(showgrid = F,
                          zeroline = F),
             yaxis = list(showgrid = F,
                          zeroline = F))
}

?arrange_

# CITIZEN PIECHART--------------------------------------------------------------
pieDefcit = function(data, variable){
    req(data)
    a = data %>% 
      filter(geo != "Other") %>%
      arrange_(.dots = variable, sort = F) %>% 
      ungroup() %>% 
      select(geo)
    a = as.vector(a$geo)
    srt = c("Other", rev(a))
    plot_ly() %>% 
      add_trace(data = data %>% slice(match(srt, geo)),
                labels = ~geo,
                values = ~get(variable),
                textposition = 'inside',
                textinfo = 'label+percent',
                type = "pie",
                showlegend = F,
                sort = F) %>% 
      layout(
             xaxis = list(showgrid = F,
                          zeroline = F),
             yaxis = list(showgrid = F,
                          zeroline = F))
}


################################################################################
# writing text------------------------------------------------------------------

syrtext1 = "This app allows the user to inspect migration flows 
of asulym seekers. It is possible to inspect the phenomenon from three
points of view given by the graphs. in the first graph the user can focus on recognition rate (RR)
to determine the propensity of a country to grant asylum status to immigrants
from a determinate nationality. In the second graph the user can focus on 
applications and asylum approvals in a determinate time window for a country, 
this allows to inspect which nationalities seeks asylum in one country and how
much this country accepts them. The third graph is useful to focus on which country
has more applications from a specific provenience country and which country granted
most asylums. To show how an analysis can be conducted, we will focus on a 
specific historical fact: the Syrian migration crisis.
The Syrian migration crisis is one of the most relevant 
migration phenomenon of the last decade. It started with the beginnning
of the Syrian civil war in March 2011 and it's still going on. At the beginning
of the humanitarian crisis, most of the immigrants were welcomed mostly
by Turkey and Europe but because of the amount of refugees involved, the 
situation is becoming more sensitive also from a diplomatic point of view.
An very recent example is the decision of Turkey, in light of the non-arrival
of founds requested to Europe to welcome Syrians, to open the north fronteer, 
creating very critical situations. We will not discuss in detail the political 
implications of the crisis, but we will focus on the migration flows directed
to continental Europe."


syrtext2 = "If the user selects as country EU-28 (except for 2020 because of Brexit,
in this case EU-27) the migration crisis is easy noticeable. Since 2012 the number of 
refugees from Syria starts rasing fast until it reaches the maximum in 2016, where it's
around 400k. If then the user selects Germany and looks at 2015 and 2016 it is noticeable 
one very interesting fact: the German chancellor Angela Merkel in these years decided 
to go against her own party and to welcome a very huge number of Syrian refugees (this
is also noticeable in the following two plots). Another interesting fact is that while
countries like Syria, Iran and Afghanistan recieve a relative high recognition rate, 
balcanian countries as Albania or Serbia have a recognition rate that is very close 
to zero."

syrtext3 = "Selecting EU-28 (or EU-27) the evidences above are confirmed: Syria is
one of the state which do most of the applications, but talking about approvals it's
far the one which recieves most of them (in 2011-2019 period, even tough Afghanistan and Syria
do almost the same amount of applications, Syria gets the higher number of permits, namely
the 25.9% of total approvals). This percentage becomes even higher when we focus on Germany,
in the same period Syrian approvals cover the 55.7% of the approvals."

syrtext4 = "Looking at Syria and given what was observed above the graph results are
no surprise. Germany is the country which welcomed the highest percentage of Syrians
in the period 2011-2020 (60.9% of them). Something that is very tragic to notice is that
in this period there were almost one milion three hundred thousand applications from Syria
to Europe and around one milion and two hundred thousand asylum approvals (first istance plus 
definitive decision)."