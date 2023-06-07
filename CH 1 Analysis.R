## CH 1 ANALYSIS  ##
##  JUNE 2021     ##
##     ECO        ##
####################



# Clear global environment
#rm(list=ls())


# Allmighty Mac in computer lab
if(Sys.info()[1] == 'Darwin' & Sys.info()[7] == 'eco20il'){
  path <- '/Volumes/big_game-1/Erin_OConnell'
}

# Breaker Box #
#------------------#
mapRecords = TRUE
mapAll = TRUE
makeKDEs = TRUE
#------------------#


#setwd("/Volumes/big_game/Erin_OConnell")



###-------------------------------------###
### Import and activate needed packages ###
###-------------------------------------###

ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


packages <- c('adehabitatHR','blastula','beeper', 'dplyr','httr','jsonlite','lubridate','rgdal','RODBC','sp',
              'magrittr', "stringr", "ggplot2", "tidyr")
ipak(packages)
### Installs packages
### More packages may be added to the previous vector as needed ###



### DATA Wrangling YAY~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~jk####

# If using Mac in Computer Lab
    if(Sys.info()[1] == 'Darwin' & Sys.info()[7] == 'eco20il'){
  
    # Open FileMaker Pro
    system2('open', "/Volumes/big_game/Big_Game.fmp12")
  
    # Establish Database Channel Connectiom
    ch <- odbcConnect(dsn = 'big_game', uid = 'Admin')
  }



### Initial Query ####
    dat_cap  <- sqlQuery(ch, "SELECT * FROM Capture WHERE animal_id LIKE 'ANAM-%%%%-%%%' AND collar_sn > 0") # Option to create CSV file here
    #dat.mort <- sqlQuery(ch, "SELECT * FROM Mortality WHERE animal_id LIKE 'ANAM-%%%%-%%%'")

### Filter to which individuals we want ------------------------------


      bad_ids<- c('ANAM-2020-57R', # Died shortly after release
                  'ANAM-2020-72R', # Died shortly after release
                  'ANAM-2019-850', # One of the bucks with collar issues
                  'ANAM-2019-854', # One of the bucks with collar issues
                  'ANAM-2019-855', # One of the bucks with collar issues, died same day
                  'ANAM-2019-858', # One of the bucks with collar issues
                  'ANAM-2019-864', # One of the bucks with collar issues, died same day
                  'ANAM-2019-857', # Need to revisit what happened with this collar
                  'ANAM-2019-859', # Need to revisit what happened with this collar
                  'ANAM-2019-862', # Need to revisit what happened with this collar
                  'ANAM-2019-863'), # Need to revisit what happened with this collar
                  #'ANAM-2019-867' # Need to revisit what happened with this collar
                  ) # One of the bucks with collar issues


                  # Check Big Game Drive Filemaker Pro for more Info on bad_ids



  # Filters for dat_cap individuals
    dat_cap <- dat_cap[which(dat_cap$collar_sn > 0),]      # No uncollared ones
    dat_cap <- dat_cap[which(dat_cap$collar_sn < 100000),] # No LifeCycles
    dat_cap <- dat_cap[dat_cap$capture_event == '2020_Pronghorn'|  # Just Rocker b Speed Goats
                     dat_cap$capture_event == '2019_Rockerb',]
    dat_cap <- dat_cap[which(dat_cap$animal_id %in% bad_ids == FALSE),] # No problem children
    # End of filtering for dat_cap #
         ### 33 INDIVIDUALS HERE CHECK THIS
    
    
  # Create Empty Tibble Table 
    dat_gps_all <- tibble(
      animal_id = character(),
      timestamp = numeric(),
      lat = numeric(),
      long = numeric(),
    )
    
  
    
    
  # Query and Filters 
    for(i in 1: length(unique(dat_cap$animal_id))){ 
        # i <- dat_cap$animal_id[2]
        # i=3 diagnostics
  
      # Create gps dataframe for i individual  
      dat_gps <- sqlQuery(channel = ch, query = paste0("SELECT * FROM GPS WHERE animal_id = '", dat_cap$animal_id[i],"'"))
     
    
    
      # Correct data types, filter DOPs, remove fixes with no location data
      dat_gps$timestamp <- as_datetime(dat_gps$timestamp)
      dat_gps <- subset(dat_gps, dat_gps$pdop > 0 & dat_gps$pdop < 10)
      dat_gps <- subset(dat_gps, is.na(dat_gps$lat) == FALSE)
      dat_gps <- subset(dat_gps, is.na(dat_gps$lon) == FALSE)
      
      # Filter points prior to capture and after mortality event
      dat_gps <- dat_gps[c(as.Date(dat_gps$timestamp) > as.Date(dat_cap$capture_date[i]) + 7),] # days to omit post capture 
      dat_gps <- dat_gps[c(as.Date(dat_gps$timestamp) < as.Date(dat_cap$end_date[i])),]
       
      
      # Select columns we want to include and bind
      dat_gps<- dat_gps[, c(1,2,3,4)]
      dat_gps_all<- rbind(dat_gps_all, dat_gps)
  }
    
      # Removes locations prior to 2020 Captures (+ 7 days of buffer)
      dat_gps_all <- dat_gps_all[which(dat_gps_all$timestamp>"2020-02-05 00:00:00"), ] 
      # capturedate<- "2020-02-05 00:00:01"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
      
      ### Close connection
      odbcClose(ch);rm(ch) # Close the connection

      # Kill FMP so everyone else can use it.. or don't -(Chaotic Evil)
      system2("taskkill", args = '/im "FileMaker Pro 18 Advanced.exe"')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
      ### Get IDs
      id_list <- unique(dat_gps_all$animal_id)#[-1] 
      
     


      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ### END OF DB CONNECTION AND FILTERING CRAP... YAY ! ### 
      


  # Making a pretty table...  
      
      #Add date columns
      dat_gps_all$week<-week(dat_gps_all$timestamp)
      dat_gps_all$year<-year(dat_gps_all$timestamp)
      dat_gps_all$wy <- paste(dat_gps_all$week, "-", dat_gps_all$year)
      
      # Reminder: without week since release correction (later step)
      
      #dat_gpsall1<- dat_gps_all # make a copy for edits
      
   
     
      
################################################################################      


      
      # Dynamic Function to Calculate Weeks Since Release $ ####
      
      capturedate<- "2020-02-05 00:00:00"
      
      dat_gps_all$week<-(round(difftime(dat_gps_all$timestamp, capturedate, units = "weeks" ), digits = 0)) # complete weeks since rlease
      
      
################################################################################  
      
      
      
      
      

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
              ### DEFINE FUNCTIONS ####
      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    ### Defining Functions to help us out ####
      
      
      makeKDE <- function(dat_gps, id, homerange, idCol, grid = 1000, epsg = 26914, project = TRUE, epsgOut = NULL){
        
        if(missing(grid)){
          warning("argument grid not provided. Default set to 500")
        }
        
        input <- subset(dat_gps, dat_gps[,idCol] == id)
        xc<-as.vector(input$lon)
        yc<-as.vector(input$lat)
        xy<-as.matrix(cbind(xc,yc))
        xy.sp <- SpatialPoints(xy,proj4string = CRS("+init=epsg:4326"))
        xy.sp<- spTransform(xy.sp, CRSobj = CRS("+init=epsg:26914"))
        kud <- kernelUD(xy.sp, kern = "bivnorm", grid = grid)
        homerange <- kernel.area (kud, percent = 75, unout = "km2")
        #writeOGR(obj = homerange, dsn = path, layer =  paste0(id, "_75KDE"), 
        #driver = "ESRI Shapefile", overwrite_layer = TRUE)
        return(homerange)
      }
      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
 
         ### INTENSIVE DATA CALCULATIONS ####      
      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        
         #Loop For ALL WEEKLY KDEs
         uds <- list()
         ids <- c()
         for (ii in unique(id_list)) {
           dat <- dat_gps_all[dat_gps_all$animal_id == ii,]
           
           for (j in unique(dat$week)) {
             
             if(nrow(dat[dat$week == j & dat$week == j,]) >= 15){
               uds<- append(uds,makeKDE(dat[dat$week == j,], id= ii, homerange = 75, idCol='animal_id'))
               names(uds)[length(uds)] <- paste(ii,j,sep='-')
               
             }   else{
               uds <- append(uds, NA)
               names(uds)[length(uds)] <- paste(ii,j,sep='-')
             }
             ids <-c(ids,ii)
           }
         }
         beepr::beep(8) 
         
        
         
         
 ### EMAIL YOURSELF DUMMY #### 
      install.packages(blastula)
      email <- compose_email(
        body = md(c(
          "I AM COOL"
          
        ))
      )       
      email %>%
        smtp_send(
          from = "kaleloconnell@gmail.com",
          to = "kaleloconnell@gmail.com",
          subject = "YOUR CODE IS FINISHED RUNNING :)",
          credentials = creds_key(id = "gmail")
        )
      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      
  ### Making the Dataframe ####
          UDvector <- c(rep(NA, times = 1997)) #Weekly KDE  #previously 1242 records! #As of 22 SEPT21   # New records: tbd as of 2022.... 
          # New records as of JAN 20 2022 =1997 so I changed it from 1284 (old) as of JAN 20 2022
          counter <- 1
      
          for(k in (uds)){
          UDvector[counter]<- uds[[counter]][1]
          counter <- counter + 1
          print(counter)
          }
      
          UDvector
          IDtest <- names(uds)
      
      
    ### MAKE the df with Weekly KDE #### 
          UDdataframe <- data.frame(ID  = IDtest,
                                KDE = UDvector)
      
      
      
    ### To Calculate Mean of all individuals (AVG KDE of ALL PRONGHORN BY WEEK) ####
    #mean(UDdataframe$KDE, na.rm = T) This was ~14km^2 
      
      
      
    ### CREATE $ WITH IND AN_ID 'PRONGHORN' ####
          UDdataframe$PRONGHORN <- rep(NA, times = length(UDdataframe$ID)) 
          View(UDdataframe)
      
          idstring <- as.data.frame(str_split_fixed(UDdataframe$ID, "-", 4))
          idstring[,-4]
          UDdataframe$PRONGHORN <- paste(idstring$V1,idstring$V2,idstring$V3, sep = "-")
      
          
    # CREATE $ with Separated values so you can index through them later
      #UDdataframe<- merge(UDdataframe, idstring)
          UDdataframe <-UDdataframe %>% separate(PRONGHORN, c("A", "B", "C") ) 
          #%>% select('A')
      
      
      
    # To Remake the PRONGHORN COLUMN RUN THIS AGAIN
      UDdataframe$PRONGHORN <- rep(NA, times = length(UDdataframe$ID)) 
      View(UDdataframe)
      
      idstring <- as.data.frame(str_split_fixed(UDdataframe$ID, "-", 4))
      idstring[,-4]
      UDdataframe$PRONGHORN <- paste(idstring$V1,idstring$V2,idstring$V3, sep = "-")
      
      
      UDdataframe = UDdataframe %>% select(ID, KDE, PRONGHORN, A, B, C)
      # Reorder for aesthetics
      
      
      
      
      # To Create Column to delineate between T & R (TRUE/FALSE)
      UDdataframe$Behavior<- (UDdataframe$B == "2020")
      
      
      
      # Create Column that I renamed 'Behavior' with the names T or R pasted in the df
      UDdataframe$Translocated <- ifelse(test=UDdataframe$Behavior==TRUE, yes="Translocated", no = "Resident")
      
      
      # Aesthetics
      names(UDdataframe)[7] <- "Translocated?" 
      names(UDdataframe)[8] <- "Behavior"
      UDdataframe = UDdataframe %>% select(ID, KDE, PRONGHORN, A, B, C, Behavior, `Translocated?`)
      # Reorder for aesthetics
      View(UDdataframe)
      
      
      
      
      #### Make Weeks Since Release $ ####
      
      # Create Week Column from idstring
      UDdataframe$WeekSinceRelease<- paste(idstring$V4)
      
      UDdataframe$WeekSinceRelease<- as.numeric(UDdataframe$Week)
      
      
      #UDdataframe$WeekSinceRelease<-UDdataframe$Week-5 DONT DO THIS ANYMORE WITH NEW FUNCTION USING DIFTIME
      
##############################################################################################
  #NOTES ON THIS SECTION: 
      # Both Capture events were on the 5th Week of 2019 and 2020
      # So Week 1 Since Release is technically Week 5 
      # This line reclassifies the weeks 
      # I decided to take out Week 1 
      # Do not have to Filter out end date because all these collars are still active. 
###############################################################################################     
      #This means we only care about Week 1 and so on
      #So we get rid of the zeros
      
      UDdataframe<- UDdataframe[UDdataframe$WeekSinceRelease >= 0, ]
      # removes negative weeks
      UDdataframe <- UDdataframe[UDdataframe$WeekSinceRelease != 0, ]
      # removes the weeks since release with a value of zero
      
      
      ### Removing NAs ####
      ALLNAs<-any(is.na(UDdataframe$KDE))
      
      
      # Diagnostics To see which ID and Week has an NA 
      #Problem_Pronghorn_NAs<- unique(UDdataframe$ID[which(is.na(UDdataframe$KDE== TRUE))])
      
      
      
      UDdataframe <-na.omit(UDdataframe)
      # Removes ALL NAs
      
      
      #### If you wanted to Pull individuals avg of the avg weekly KDEs by making it a vector #### 
      
      # Here, I use a TAPPLY statement, but this could be done another way...
      # avgIndKDE <- tapply(UDdataframe$KDE, INDEX = UDdataframe$C, FUN=mean, na.rm = TRUE)
      
      
      
      # Add column to use in Model 
      UDdataframe$BehaviorCode<-ifelse(test=UDdataframe$`Translocated?` ==TRUE, 
                                    yes=1 , no = 0)
      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Save the newly created df as a CSV
      
      
      write.csv(UDdataframe, file = "/Volumes/big_game/Erin_OConnell/BadGoatsJan22.csv") 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
      #### Models #### 
      # We will be estimating using a Gamma distribution 
      
      library(lme4)
      BadGoats<-UDdataframe
      #BadGoats<-BadGoatsJan22 # to update
      
      # define parameters
      rs<-  BadGoats$KDE
      x<-   BadGoats$BehaviorCode
      q<-   BadGoats$C
      t<-   BadGoats$WeekSinceRelease
      
      #fixing issues 
      #BadGoats$Behave<- ifelse(test= BadGoats$`Translocated?`== TRUE, yes= "1", no= "0")
      
      
      
      dat<-cbind(rs,x,t,q) 
      dat<- as.data.frame(dat)
      
      #Testing different Rates Transform Time Prior to Model
      BadGoats$t_1<- 1.006^(-BadGoats$WeekSinceRelease) # 1.006
      BadGoats$t_2<- 1.01^(-BadGoats$WeekSinceRelease) # 1.01
      BadGoats$t_3<- 1.03^(-BadGoats$WeekSinceRelease) # 1.03
      BadGoats$t_4<- 1.2^(-BadGoats$WeekSinceRelease) #1.2
      BadGoats$t_5<- 1.4^(-BadGoats$WeekSinceRelease) #1.4
      BadGoats$t_6<- 1.5^(-BadGoats$WeekSinceRelease) #1.5
      BadGoats$t_7<- 1.8^(-BadGoats$WeekSinceRelease) #1.8
      BadGoats$t_8<- 1.9^(-BadGoats$WeekSinceRelease) #1.9
      BadGoats$t_9<- 2.0^(-BadGoats$WeekSinceRelease) #2.0
      
      
      
      
      
      
      #Models   
      mod.t1 <- glmer(rs~ x + x:t_1 + (1 | q), # Individuals have random intercepts
                      data = BadGoats, family= Gamma(link= "log"))
       
      mod.t2<- glmer(rs~ x + x:t_2 + (1 | q), 
                     data = BadGoats, family= Gamma(link= "log"))
      
      mod.t3<- glmer(rs~ x + x:t_3 + (1 | q), 
                     data = BadGoats, family= Gamma(link= "log"))
      
      mod.t4<- glmer(rs~ x + x:t_4 + (1 | q), 
                     data = BadGoats, family= Gamma(link= "log"))
      
      mod.t5<- glmer(rs~ x + x:t_5 + (1 | q), 
                     data = BadGoats, family= Gamma(link= "log"))
      
      mod.t6<- glmer(rs~ x + x:t_6 + (1 | q), 
                     data = BadGoats, family= Gamma(link= "log"))
      
      mod.t7<- glmer(rs~ x + x:t_7 + (1 | q), 
                     data = BadGoats, family= Gamma(link= "log"))
      
      mod.t8<- glmer(rs~ x + x:t_8 + (1 | q), 
                     data = BadGoats, family= Gamma(link= "log"))
      
      mod.t9<- glmer(rs~ x + x:t_9 + (1 | q), 
                     data = BadGoats, family= Gamma(link= "log"))
      
      
      
      
      AICs <- c(AIC(mod.t1),AIC(mod.t2),AIC(mod.t3), AIC(mod.t4),AIC(mod.t5),AIC(mod.t6),AIC(mod.t7), AIC(mod.t8), AIC(mod.t9) )
      names(AICs) <- c('mod.t1','mod.t2','mod.t3','mod.t4', 'mod.t5','mod.t6','mod.t7','mod.t8','mod.t9')
      
      sort(AICs)    
      # Model t.3 
      
      
      summary(mod.t3)
      library(sjPlot)
      tab_model(mod.t3)
      
      
      sjPlot::plot_model(mod.t3,
                         axis.labels = c('Initial Dif.','Final Dif.','Resident*'),
                         show.values = T, show.intercept = T, axis.title = 'Estimates (km^2)',
                         title = 'Translocated vs. Resident Range Size')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
     
      
       ### EFFECTS AND CI'S #### 
      
      # Use the effects package --> effect function. 
      # term = the fixed effect you want to get data on 
      #  mod = name of your model
      
      effects.trans <- effects::effect(term= "x:t_3", mod= mod.t3)
      summary(effects.trans) #output of what the values are
      
     
      
      x.trans <- as.data.frame(effects.trans)# Save the effects values as a df:
      
      x.conf <- confint(mod.t3, method = "boot", nsim = 600 ) 
      #x.conf <- confint(mod.t3)  won't work, so we needed to BOOTSTRAP !
  
      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~           
  ### NOTES AND ISSUES ####
  # needed to bootstrap calc for confidence intervals
  # in order for this to work
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      
      
      ## grad the inverse link function
      # ilink <- family(mod.t3)$linkinv
      # ## add fit and se.fit on the **link** scale
      # ndata <- bind_cols(ndata, setNames(as_tibble(predict(mod.t3, ndata, se.fit = TRUE)[1:2]),
      #                                    c('fit_link','se_link')))
      # ## create the interval and backtransform
      # ndata <- mutate(ndata,
      #                 fit_resp  = ilink(fit_link),
      #                 right_upr = ilink(fit_link + (2 * se_link)),
      #                 right_lwr = ilink(fit_link - (2 * se_link)))
      # show ndata      
      
      
      
  # We had to make df numeric
      dat <- data.frame(rs = as.numeric(dat$rs),
                        x = as.numeric(dat$x),
                        t = as.numeric(dat$t),
                        q= dat$q)      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
      
      
      ### Plot ####  
      
      trans.plot1 <- ggplot() + 
        ggtitle('Resident and Translocated Pronghorn Range Size') +
        ggeasy::easy_center_title() +
        
        scale_x_continuous(expand = c(0, 0), limits = c(0, 50)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0, 150)) +
        
        scale_fill_discrete(name="Group",
                                 
                                 labels=c("Translocated", "Resident")) +
        
        #2 Translocated
        geom_point(data=subset(dat, x==1), aes(x = t, y = rs), color = 'red', size = 0.1) +
        
        #3 Residents
        geom_point(data=subset(dat, x==0), aes(x = t, y = rs), color="blue", size = 0.1) +
        
        
        #4a Trans
        geom_line(aes(x= 0:50, y= exp(mod.t3@beta[3]*1.03^-c(0:50) + 
                                        mod.t3@beta[2] + mod.t3@beta[1])), color="red", size = 1) +
        #4b Residents
        geom_line(aes(x= 0:50, y= exp(mod.t3@beta[1])), color="blue", size = 1) +
        
        
        
        
        # Definitions to plot CIs of the model t3
        # Turn ON First time running (see below)
        
        # upper <- exp(x.conf[3,2]) #exponential of bootstrapping CIs
        # lower <- exp(x.conf[3,1])
        # plot((exp(mod.t3@beta[3]*1.03^-c(0:60)))) #curve red line
      # plot(exp(mod.t3@beta[2]))
      # plot(exp(mod.t3@beta[1])) #constant blue line
      # plot(x = 0:60, y= exp(mod.t3@beta[3]*1.03^-c(0:60) +
      #             mod.t3@beta[2] + mod.t3@beta[1]))
      # 
      # plot(upper + (exp(mod.t3@beta[3]*1.03^-c(0:60))))
      
      
      
      
      # 5A: Plot Ribbon for Translocated CIs
      geom_ribbon(aes(x=0:50,
                      ymin= (exp(mod.t3@beta[3]*1.03^-c(0:50) + 
                                   mod.t3@beta[2] + mod.t3@beta[1]))- x.conf[3,1],
                      ymax= (exp(mod.t3@beta[3]*1.03^-c(0:50) + 
                                   mod.t3@beta[2] + mod.t3@beta[1])) + x.conf[3,2],
                      alpha= 0.3, fill="red")) +
        
        
        
        #5b Ribbon for Residents CIs
        geom_ribbon(aes(x=0:50, 
                        ymin= (exp(mod.t3@beta[1])) - x.conf[3,1],
                        ymax= (exp(mod.t3@beta[1])) + x.conf[3,2]),
                    alpha= 0.3, fill="blueviolet") +
        
        
        
        
        #6 Add Labels
        
        labs(x="Weeks Since Release", y="Range Size (km^2)") 
        
        
      
      
      
      
      
        trans.plot1 
      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~         
        #### Model Fit #### 
      
      install.packages("r2glmm")
      r2beta(mod.t3)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        
      
      

      
      