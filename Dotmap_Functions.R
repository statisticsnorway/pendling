library(PxWebApiData)
library(dplyr)
library(ggplot2)
library(klassR)
library(sp)

#' Filter kommune
#' This function filters available kommune on the mainland from messy data files
#'@param data A dataset to evaluate
#'@param komm_var The municipality variable as a text string
#'@return A data frame containing only municipalities with location points on the mainland
Make_kommune <- function(data, komm_var, komm_punkt){
  # data_new <- data[data[, komm_var] %in% Komm_names$Komm, ]  #sjekk for kommune navn
  data_new <- data[data[, komm_var] %in% komm_punkt$NR, ]  # sjekk for kommune XY punkt
  return(data_new)
}



Load_geo_data <- function(year, package = TRUE){
  komm_punkt <- paste0("komm_punkt", year)
  komm_shape <- paste0("komm_shape", year)
  if (package) {
    data(komm_punkt)
    data(komm_shape)
  } else {
    load(paste0("data/", komm_punkt, ".RData"))
    load(paste0("data/", komm_shape, ".RData"))
  }
  return(list(get(komm_punkt), get(komm_shape)))
}


#' Load statistical data
#' This function loads data using SSBs API for population, employment and commuting tables
#'@param year A numeric year
#'@return No return but datasets are loaded into working environment
Load_stat_data <- function(year, komm_punkt){
  
  # Antall i befolkning og sysselsatte 15-74
  #dt <- ApiData(url = "https://data.ssb.no/api/v0/no/table/11618/"
  #              , Region = TRUE, Kjonn = "0", Tid = year, Alder = "15-74",
  #              ContentsCode =TRUE
  #)
  #dt <- Make_kommune(dt$dataset, komm_var = "Region", komm_punkt = komm_punkt)

  dt <- readRDS(paste0("data/data_kommune", year,".rds"))
  
  befolk <- dt[dt$ContentsCode == "BefolkningBosted", ]
  syss <- dt[dt$ContentsCode == "SysselBosted", ]
  arbb <- dt[dt$ContentsCode == "SysselArbsted", ]
  
  #antall som pendler in og ut
  #dt <- ApiData(url = "https://data.ssb.no/api/v0/no/table/03321/"
  #              , Tid = year, ArbstedKomm = TRUE, Bokommuen=TRUE)
  #pend <- Make_kommune(dt$dataset, "Bokommuen", komm_punkt = komm_punkt) #ta ut data vi ikke har punkter for
  #pend <- Make_kommune(pend, "ArbstedKomm", komm_punkt = komm_punkt) # ta ut linje vi ikke har arbsted punkt for
  pend <- readRDS(paste0("data/data_pendling", year,".rds"))
  
  # Save matched kommune number order
  mat_pop <- match(as.factor(befolk$Region), komm_punkt$NR) 
  
  return(list(befolk, syss, arbb, pend, mat_pop))
  
}


Beregn_sirkel <- function(adjA, befolk, syss, arbb, komm_punkt){
  # Beregne populasjonsirkler
  pop1 <- sqrt(befolk$value/max(befolk$value)) * adjA
  #names(pop1) <- befolk$Region
  mat_pop <- match(as.factor(befolk$Region), komm_punkt$NR)
  
  # Beregne sysselsatte sirkler - bosted
  pop2 <- sqrt(syss$value/max(befolk$value))*adjA
  
  # Beregne sysselsatte sirkler - arbsted
  pop11 <- sqrt(arbb$value/max(befolk$value))*adjA
  
  
  return(list(pop1, mat_pop, pop2, pop11, Region=befolk$Region))
}


Beregn_sirkel_arbsted <- function(adjA, arbb, komm_punkt){
  # Beregne populasjonsirkler
  # pop1 <<- sqrt(befolk$value/max(befolk$value)) * adjA
   mat_pop <- match(as.factor(arbb$Region), komm_punkt$NR) ##Brukes i app ?
  
  # Beregne sysselsatte sirkler - bosted
  # pop_bo <- sqrt(syss$value/max(arbb$value))*adjA
  
  # Beregne sysselsatte sirkler - arbsted - pop11
  pop_arb <- sqrt(arbb$value/max(arbb$value))*adjA
  return(list(pop_arb, mat_pop))
  #return(list(pop_arb, pop_bo))
}



#Update kommune nummer og navn
UpdateKomm <- function(fil, year, type = "shapefil"){
  dato <- paste0(year,"-12-31")
  changeTabell <- GetKlass(131, correspond = T, date = c("2012-01-01", dato))
  newnum <- fil$KOMM
  newname <- as.character(fil$NAVN)
  for (i in seq(3)){
    m <- match(newnum, as.numeric(changeTabell$sourceCode))
    newnum <- ifelse(is.na(m), newnum, changeTabell$targetCode[m])
    newname <- ifelse(is.na(m), newname, changeTabell$targetName[m])
  }
  fil$KOMM <- as.numeric(newnum)
  fil$NAVN <- newname
  Encoding(fil$NAVN) <- "UTF-8"
  return(fil)
}



#' Make barplot
#' This function filters data, finds top (n) commuting municipalities and creates barplot
#'@param zip A character municipality number (4 digits)
#'@param n Number of municipalties to show
#'@return A barplot is returned
Make_barplot <- function(zip, n, komm_shape, pend, antkom, scaleLine = FALSE){
  dat <- pend[pend$Bokommuen == zip & pend$value > 0, ]
  n <- min(n, nrow(dat)) # check there is enough data for n
  topKom <- dat[order(-dat$value),][1:(n+1),]
  topKom$NAVN <- komm_shape$NAVN[match(topKom$ArbstedKomm, komm_shape$NR)]
  # par(mar = c(4,7,1,1))
  selected_navn <- topKom$NAVN[topKom$ArbstedKomm == zip]
    
  # max number of bars in barplot
  n_bars = antkom + 1
  n_invisible = n_bars - nrow(topKom)
  
  if(n_invisible>0){
    null_data = matrix(c(seq(n_invisible),rep(0,n_invisible)),ncol=2, nrow=n_invisible)
    
    test2 = as_tibble(rbind(as.matrix(topKom[, c("NAVN", "value")]), 
                            null_data))
    
  } else{
    n_invisible=0
    test2 = as_tibble(topKom[seq(n_bars), c("NAVN", "value")])
  }
  
  test2$NAVN = factor(test2$NAVN, levels = rev(test2$NAVN))
  test2$value = as.numeric(as.character(test2$value))
  
  # Cut long names shorter
  test2$NAVN2 <- as.character(test2$NAVN)
  test2$NAVN2[str_length(test2$NAVN)>16] <- paste0(substr(test2$NAVN2[str_length(test2$NAVN) > 16], 1, 14), "...")
  test2$NAVN2 = factor(test2$NAVN2, levels = rev(test2$NAVN2))
  
  # Need to map windows font - doesn't work on server 
  # windowsFonts(Open_Sans = windowsFont("Open Sans")) - not working on server
  par(xpd=TRUE)
  ggplot(test2, aes(x=NAVN, y=value, 
                    #fill=NAVN == topKom$NAVN[1])
                    fill=NAVN == selected_navn)
         ) +
    geom_bar(stat="identity", alpha=1) +
    geom_text(hjust=-.1, aes(label=format(value, big.mark = " ")), size=3, 
              alpha=c(rep(1, sum(test2$value>0)), rep(0, sum(test2$value==0)))) +
    ylab("") +
    xlab("") +
    ylim(0, max(test2$value)*1.3) +
    coord_flip() +
    scale_fill_manual(values=c("#F8A67D", "#C4351C")) +
    theme_light() +
    scale_x_discrete(breaks=test2$NAVN , labels=c(as.character(test2$NAVN2[seq(nrow(topKom))]), rep(" ",n_invisible))) +
    theme(legend.position = "none", 
          text = element_text(size=10), #, family = "Open Sans"), 
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()
    )
}



Make_barplot_arb <- function(zip, n, komm_shape, pend, antkom, scaleLine = FALSE){
  dat <- pend[pend$ArbstedKomm == zip & pend$value > 0, ]
  n <- min(n, nrow(dat))
  topKom <- dat[order(-dat$value),][1:(n+1),]
  topKom$NAVN <- komm_shape$NAVN[match(topKom$Bokommuen, komm_shape$NR)]
  selected_navn <- topKom$NAVN[topKom$Bokommuen == zip]
  
  # max number of bars in barplot
  n_bars = antkom + 1
  n_invisible = n_bars - nrow(topKom)
  
  if(n_invisible>0){
    null_data = matrix(c(seq(n_invisible),rep(0,n_invisible)),ncol=2, nrow=n_invisible)
    test2 = as_tibble(rbind(as.matrix(topKom[, c("NAVN", "value")]), 
                            null_data))
    
  } else{
    n_invisible=0
    test2 = as_tibble(topKom[seq(n_bars), c("NAVN", "value")])
  }
  
  # Cut long names shorter
  test2$NAVN2 <- as.character(test2$NAVN)
  test2$NAVN2[str_length(test2$NAVN) > 16] <- paste0(substr(test2$NAVN2[str_length(test2$NAVN) > 16], 1, 14), "...")
  test2$NAVN2 = factor(test2$NAVN2, levels = rev(test2$NAVN2))
  
  test2$NAVN = factor(test2$NAVN, levels = rev(test2$NAVN))
  test2$value = as.numeric(as.character(test2$value))
  
  ggplot(test2, aes(x = NAVN, y=value, 
                    fill = NAVN == selected_navn)
  ) +
    geom_bar(stat="identity", alpha=1)+
    geom_text(hjust=-.1, aes(label=format(value, big.mark = " ")), size=4, 
              alpha=c(rep(1, sum(test2$value>0)), rep(0, sum(test2$value==0)))) +
    ylab("") +
    xlab("") +
    ylim(0, max(test2$value)*1.3) +
    coord_flip() +
    scale_fill_manual(values=c("#83C1E9", "#006CB6")) +
    theme_light() +
    scale_x_discrete(breaks=test2$NAVN, labels=c(as.character(test2$NAVN2[seq(nrow(topKom))]), rep(" ",n_invisible))) +
    theme(legend.position = "none", 
          text = element_text(size=12), #family = "Open Sans",
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()
    )
}

#' Filter data
#' This function filters data for chosen municipaltiy and finds top (n) commuting municipalities
#'@param zip A character variable for chosen municipality (4 digits)
#'@param n The number of of municipalities to show
#'@param adjA Adjustment factor
#'@return Returns a list of 6 elements...

Filter_data <- function(zip, n, adjA, scaleLine =FALSE, komm_punkt, pend, pop11, befolk, arbb){
  zip <- formatC(zip, width = 4, flag = "0")
  # Filter data til valgt kommune
  dat <- pend[pend$Bokommuen == zip & pend$value > 0, ]

  # Finn hvilken rad det gjelder valgt kommune som også er bosted
  mat_komm <- match(zip, dat$ArbstedKomm)
  
  # Finn hvilken rad i kommune punkt data til valgt kommune
  mat_komm_punkt <- match(zip, komm_punkt$NR)
  
  # Finne antall (n) topp pendlings kommune
  topKom <- dat$ArbstedKomm[order(-dat$value)][1:(n+1)]
  
  # Legg til valgt kommune (hvis ikke med)
  if (!zip %in% topKom){
    topKom <- c(zip, topKom)[1:n+1]
  }
  
  # sett valgt kommune øverst
  if (match(zip, topKom) != 1){
    m <- match(zip, topKom)
    topKom <- topKom[c(m, seq(1, n+1)[-m])]
  }
  
  # match top kommuner i dataset og kommune punkter 
  mat_arb <- match(topKom, dat$ArbstedKomm)
  mat_arb_punkt <- match(topKom, komm_punkt$NR)
  
  # Finn koordinater til poly linjer
  arbX <- komm_punkt$lat[mat_arb_punkt[2:(n+1)]]
  arbY <- komm_punkt$lng[mat_arb_punkt[2:(n+1)]]
  boX <- komm_punkt$lat[mat_komm_punkt] #match of kommune
  boY <- komm_punkt$lng[mat_komm_punkt] #match of kommune
  lng <- rep(boY, n*2); lat <- rep(boX, n*2)
  lng[seq(from = 2, to = n*2, 2)] <- arbY
  lat[seq(from = 2, to = n*2, 2)] <- arbX
  group <- rep(c("A", "B"), n)
  mylines <- data.frame(group = group, lat = lat, lng = lng, type = "line")
  
  # sett bredde på linje (dette er fast akkurat nå)
  ww <- pmax(dat$value[mat_arb]/sum(dat$value[mat_arb]), 0.01) #bredde av linje
  if (scaleLine == FALSE) {ww <- rep(0.02, n)}
  
  # sirkel størrelsen til sysselsatte som bor på valgt kommune
  pop4 <- sqrt(dat$value[mat_arb]/max(befolk$value)) * adjA
  
  mat_tot_punkt <- match(topKom[2:(n+1)], komm_punkt$NR)
  pop5 <- 0
  
  match_arb <- match(topKom[2:(n+1)], arbb$Region)
  pop12 <- pop11[match_arb]
  
  return_dat <- list(komm_punkt[mat_tot_punkt,], komm_punkt[mat_arb_punkt,], mylines,
                     pop5, pop4, ww, pop12)
  return(return_dat)
}



Filter_data_arb <- function(zip, n, adjA, scaleLine = FALSE, komm_punkt, pend, pop_arb){
  zip <- formatC(zip, width = 4, flag = "0")
  # Filter data til valgt kommune
  dat <- pend[pend$ArbstedKomm == zip & pend$value > 0, ]
  
  # Finn hvilken rad det gjelder valgt kommune som også er bosted
  mat_komm <- match(zip, dat$Bokommuen)
  
  # Finn hvilken rad i kommune punkt data til valgt kommune
  mat_komm_punkt <- match(zip, komm_punkt$NR)
  
  # Finne antall (n) topp pendlings kommune
  topKom <- dat$Bokommuen[order(-dat$value)][1:(n+1)]
  
  # Legg til valgt kommune (hvis ikke med)
  if (!zip %in% topKom){
    topKom <- c(zip, topKom)[1:n+1]
  }
  
  # sett valgt kommune øverst
  if (match(zip, topKom) != 1){
    m <- match(zip, topKom)
    topKom <- topKom[c(m, seq(1, n+1)[-m])]
  }
  
  # match top kommuner i dataset og kommune punkter 
  mat_arb <- match(topKom, dat$Bokommuen)
  mat_arb_punkt <- match(topKom, komm_punkt$NR)
  
  # Finn koordinater til poly linjer
  arbX <- komm_punkt$lat[mat_arb_punkt[2:(n+1)]]
  arbY <- komm_punkt$lng[mat_arb_punkt[2:(n+1)]]
  boX <- komm_punkt$lat[mat_komm_punkt] #match of kommune
  boY <- komm_punkt$lng[mat_komm_punkt] #match of kommune
  lng <- rep(boY, n*2); lat <- rep(boX, n*2)
  lng[seq(from = 2, to = n*2, 2)] <- arbY
  lat[seq(from = 2, to = n*2, 2)] <- arbX
  group <- rep(c("A", "B"), n)
  mylines <- data.frame(group = group, lat = lat, lng = lng, type = "line")
  
  # sett bredde på linje (dette er fast akkurat nå)
  ww <- pmax(dat$value[mat_arb]/sum(dat$value[mat_arb]), 0.01) #bredde av linje
  if (scaleLine == FALSE) {ww <- rep(0.02, n)}
  
  # sirkel størrelsen til sysselsatte som jobbe på valgt kommune
  pop4 <- sqrt(dat$value[mat_arb]/max(pop_arb$value)) * adjA
  
  mat_tot_punkt <- match(topKom[2:(n+1)], komm_punkt$NR)
  pop5 <- 0
  
  pop12 <- 0
  
  return_dat <- list(komm_punkt[mat_tot_punkt,], komm_punkt[mat_arb_punkt,], mylines,
                     pop5, pop4, ww, pop12)
  return(return_dat)
}


#' Add popup kommune name and details to application
Add_popup <- function(komm_shape_fil, befolk, syss, pend){
  m <- match(komm_shape_fil$NR, befolk$Region) 
  komm_shape_fil$befolk <- befolk$value[m]
  m <- match(komm_shape_fil$NR, syss$Region)
  komm_shape_fil$sysb <- syss$value[m]
  temp <- pend[pend$ArbstedKomm == pend$Bokommuen, ]
  m <- match(komm_shape_fil$NR, temp$Bokommuen)
  komm_shape_fil$sysa <- temp$value[m]
  komm_shape_fil$perc <- round(komm_shape_fil$sysa/komm_shape_fil$sysb * 100, 1)
  return(komm_shape_fil)
}