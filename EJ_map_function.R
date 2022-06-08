library(tidyverse)
library(ggplot2)
library(tidycensus)
library(sf)
library(sp)
library(leaflet)



map_EJ <- function(myyear = 2019, mystate = "OR") {
  race_vars <- c(White = "B03002_003", Black = "B03002_004", Native = "B03002_005", 
               Asian = "B03002_006", HIPI = "B03002_007", Hispanic = "B03002_012")

# Request a summary variable from the ACS
  or_race <- get_acs(geography = "tract", 
                   state = mystate,
                   variables = race_vars, 
                   year = myyear,
                   geometry = TRUE,
                   summary_var = c(tr_pop = "B03002_001"))

# for now, focus on %non-white as an indicator of diversity
# manipulate or_race dataframe to create the appropriate dataframe for diversity
  or_dem <- or_race %>% filter(variable == "White")
  colnames(or_dem)[4] <- "pop_white"
  colnames(or_dem)[5] <- "moe_white"
  colnames(or_dem)[6] <- "pop_tot"
  colnames(or_dem)[7] <- "moe_tot"
  or_dem <- or_dem %>% mutate(pct_div = (100 - (pop_white*100/pop_tot))) 
  or_dem$pct_div <- round(or_dem$pct_div, 1)

  div80pct <- quantile(or_dem$pct_div, probs=.8, na.rm = TRUE)

# retrieve poverty variables, and merge with or_dem dataframe
  or_pov <- get_acs(geography = "tract", 
                  state = mystate,
                  variables = c(over_2x_pov = "C17002_008"), 
                  year = myyear,
                  summary_var = "C17002_001")
  or_pov$pct_u2xpov <- 100 - or_pov$estimate*100/or_pov$summary_est
  or_pov$pct_u2xpov <- round(or_pov$pct_u2xpov, 1)
  or_dem <- merge(or_dem, or_pov[, c("GEOID", "pct_u2xpov")])

  pov80pct <- quantile(or_dem$pct_u2xpov, probs=.8, na.rm = TRUE)

# retrieve education variables
# create "less than high school" education category by simming up
# male and female grades completed numbers, and calculate pct.
  or_edu <- get_acs(geography = "tract", 
                  state = mystate,
                  table = c("B15002"),
                  year = myyear,
                  output = "wide")
  or_edu$lshs <- or_edu$B15002_003E + or_edu$B15002_004E + or_edu$B15002_005E + or_edu$B15002_006E +
    or_edu$B15002_007E + or_edu$B15002_008E + or_edu$B15002_009E + or_edu$B15002_010E +
    or_edu$B15002_020E + or_edu$B15002_021E + or_edu$B15002_022E + or_edu$B15002_023E +
    or_edu$B15002_024E + or_edu$B15002_025E + or_edu$B15002_026E + or_edu$B15002_027E
  or_edu$pct_lshs <- or_edu$lshs*100/or_edu$B15002_001E
  or_edu$pct_lshs <- round(or_edu$pct_lshs, 1)
  or_dem <- merge(or_dem, or_edu[, c("GEOID", "pct_lshs")])

  edu80pct <- quantile(or_dem$pct_lshs, probs=.8, na.rm = TRUE)

  or_dem_cat <- or_dem

  or_dem_cat <- mutate(or_dem_cat,
                     colorcat = case_when(pct_div >= div80pct & pct_lshs <= edu80pct & pct_u2xpov <= pov80pct  ~ 1, 
                                          pct_div <= div80pct & pct_lshs >= edu80pct & pct_u2xpov <= pov80pct  ~ 2,
                                          pct_div <= div80pct & pct_lshs <= edu80pct & pct_u2xpov >= pov80pct  ~ 3,
                                          pct_div >= div80pct & pct_lshs >= edu80pct & pct_u2xpov <= pov80pct  ~ 4,
                                          pct_div >= div80pct & pct_lshs <= edu80pct & pct_u2xpov >= pov80pct  ~ 5,
                                          pct_div <= div80pct & pct_lshs >= edu80pct & pct_u2xpov >= pov80pct  ~ 6,
                                          pct_div >= div80pct & pct_lshs >= edu80pct & pct_u2xpov >= pov80pct  ~ 7,
                                          TRUE ~ 8
                     ))                

  or_dem_cat$colorcat <- as.factor(or_dem_cat$colorcat)

  factpal <- colorFactor(c("Yellow", "Blue", "Red", "Green", "Orange", "Purple", "Black", "#FFFFFF00"), or_dem_cat$colorcat)


  labelscat <- c("%POC > 80th %ile", "%LSHS > 80th %ile", "%Poverty > 80th %ile", "80th %ile POC & LSHS", "80th %ile POC & Poverty", "80th %ile LSHS & Poverty", "80th %ile POC & LSHS & Poverty", "")
  labels1 <- sprintf(  "<strong>Total Pop: %g</strong><br/>%g Percent POC<br/>%g Percent LSHS<br/>%g Percent Poverty",or_dem_cat$pop_tot,
                     or_dem_cat$pct_div, or_dem_cat$pct_lshs, or_dem_cat$pct_u2xpov) %>% lapply(htmltools::HTML)

  leafcat <-leaflet(or_dem_cat) %>%  setView(lng = -121, lat = 44.6, zoom = 7) %>% addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")%>%
    addPolygons(group = "Population Vulnerability",
              fillColor = ~factpal(colorcat),
              weight = 1,
              opacity = .3,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.3,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "cyan",
                dashArray = "",
                fillOpacity = .3,
                bringToFront = TRUE), label = labels1,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addLegend(group = "Population Vulnerability", pal = factpal, values = or_dem_cat$colorcat, opacity = 0.7, title = NULL,
            position = "bottomleft", labFormat = function(type, cuts, p) {  
              paste0(labelscat)}) %>% 
  addLayersControl(baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                   overlayGroups = c("Population Vulnerability"),
                   options = layersControlOptions(collapsed = TRUE))
  return(leafcat)
} ##################### END map_EJ ###########

