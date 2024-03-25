#Model scripts are stored elsewhere
#Note that I kept the model diagnostics code in here, despite it not being polished
#If this is helpful, I'm glad! If not, feel free to skip it (but know that it will save folders with diagnostic plots if you don't remove it but still run the code).

library(runjags)
library(MCMCvis)
library(bayesplot)
library(coda) #allows 'as.mcmc.list' function

#read in data if not already in your working directory
#DATA <- read.csv("./data.cleaned/DATA.complete.csv")
#DATA$effort <- scale(DATA$effort)
#obsXsurvey <- read.csv("./data.cleaned/DATA.functional.obs.by.funct.by.survey.csv")
#obsXsurvey$effort <- scale(obsXsurvey$effort)


##### Modeling Species diversity (Shannon) and functional richness
nparams <- 7
nCity <- length(unique(DATA$city))
vars.vect <- c("DC","MN","PA","area","EI","clumpy","temp","pop.dens","year","effort")

dir.create(paste(Sys.Date(),"modelOutput", sep="_")) #Create parent folder for model output to be stored

y.vars <- c("sppShannonDiv", "funRich")

for(z in 1:length(y.vars)){ #begin loop for Species diversity (Shannon) and functional richness
  
  y.col <- which(colnames(DATA) %in% y.vars[z]) #index the column of interest
  
  #DATA LIST
  data_jags <- list("y" = as.vector(log(DATA[,y.col])), #response variable is log-transformed to achieve log-normal distribution
                    "n" = nrow(DATA), #length of dataset
                    "prcnt_park_area" = as.vector(DATA$prcnt_park_area),
                    "park_clumpy" = as.vector(DATA$park_clumpy),#specify variable
                    "mean_park_EIratio" = as.vector(DATA$park_EIratio),
                    "regional.C" = as.vector(DATA$regional.C),
                    "Pop.Dens" = as.vector(DATA$Pop.Dens),
                    "total.effort" = as.vector(DATA$effort),
                    "Year" = as.vector(DATA$year),
                    "City" = as.vector(as.numeric(as.factor(DATA$city))),
                    "npar" = nparams,
                    "ncity" = nCity) #specify n params
  
  params_jags = c("bCity","b") #this is from a tutorial. No clue what this is
  
  #Assign initial values
  # function that generates starting values for parameters using a different RNG for each chain
  inits_jags <- function(chain){
    gen_list <- function(chain = chain){
      list(b = rnorm(nparams, 0, 1),
           bCity = rnorm(nCity, 0, 1),
           sigma = rgamma(1, 1, 1),
           mu.city = rnorm(1, 0, 1),
           sigma.city = rgamma(1, 1, 1),
           .RNG.name = switch(chain,
                              "1" = "base::Wichmann-Hill",
                              "2" = "base::Marsaglia-Multicarry",
                              "3" = "base::Super-Duper",
                              "4" = "base::Wichmann-Hill",
           ),
           .RNG.seed = sample(1:1e+06, 1)
      )
    }
    return(switch(chain,           
                  "1" = gen_list(chain),
                  "2" = gen_list(chain),
                  "3" = gen_list(chain),
                  "4" = gen_list(chain)
                  
    )
    )
  }
  
  
  
  n.chains <- 4
  n.adapt <- 10000 
  burn.in <- 250000
  n.samples <- 500000
  thin.steps <- 5
  
  
  model <- as.mcmc.list(run.jags(model = "./scripts/MODEL_normal.R", #specify the model string
                                 data = data_jags, #specify the data list
                                 monitor = params_jags,  #params to save
                                 inits = inits_jags, #specify initial values? I think for MCMC chain?
                                 n.chains = n.chains, #specify number of chains
                                 adapt = n.adapt, #similar to burn-in, but runs first.
                                 burnin = burn.in, #burn in
                                 sample = ceiling(n.samples/n.chains), #n samples per chain
                                 thin = thin.steps, #reduce autocorrelation
                                 summarise = FALSE, 
                                 plots=FALSE,
                                 method = "parallel"))
  
  
  ############################
  #Running and saving diagnostics/results
  
  #Create a few named char vectors to help keep things straight
  response.name <- y.vars[z] #create a name for y.vars[z] (easier to replicate this way)
  
  #Create a new folder for materials to be kept in
  dir.create(paste("./",Sys.Date(),"_modelOutput/",response.name, sep="")) 
  
  #Create summary of MCMC chains
  MCMC_summarydf <- MCMCsummary(model,
                                params = params_jags,
                                round=5)
  
  row.names(MCMC_summarydf) <- vars.vect
  
  #Save summary of MCMC chains as a CSV
  file.name <- paste("./",Sys.Date(),"_modelOutput/",response.name,"/MCMC.summary.csv", sep="") #Create a file name within the appropriate subfolder
  write.csv(x = MCMC_summarydf, #Save file to correct subfolder
            file = file.name)
  
  #Create and save MCMC traceplots
  file.name <- paste("./",Sys.Date(),"_modelOutput/",response.name, sep="") #Create a file name within the appropriate subfolder
  MCMCtrace(model, #specify model
            params = params_jags, #crate trace plots for all beta paramaters
            Rhat=TRUE, #include r-hat
            n.eff=TRUE, #include effective sample size
            pdf=TRUE, #print plots to a PDF document
            wd = file.name, #use specified file path/name
            open_pdf = FALSE) #do NOT open each PDF
  
  
  MCMC_summarydf$color <- ifelse(MCMC_summarydf$`2.5%` <= -1 & MCMC_summarydf$`97.5%` <= -1, "very negative",
                                 ifelse(MCMC_summarydf$`2.5%` <= -0.25 & MCMC_summarydf$`97.5%` <= -0.25, "negative",
                                        ifelse(MCMC_summarydf$`2.5%` <= -0 & MCMC_summarydf$`97.5%` <= -0, "slightly negative",
                                               ifelse(MCMC_summarydf$`2.5%` >= 1 & MCMC_summarydf$`97.5%` >= 1, "very positive",
                                                      ifelse(MCMC_summarydf$`2.5%` >= 0.25 & MCMC_summarydf$`97.5%` >= 0.25, "positive",
                                                             ifelse(MCMC_summarydf$`2.5%` >= 0 & MCMC_summarydf$`97.5%` >= 0, "slightly positive", "non-significant"))))))
  
  
  #Establish color pallete
  great <- "#77DD76"
    
  good <- "#BDE7BD"
    
  fine <- "#E7F1E8"
    
  grey <- "#D3D3D3"
    
  meh <- "#FFD5D4"
    
  bad <- "#FFB6B3"
    
  horrid <- "#FF6962"
    
  
  values_plot <- ggplot(data = MCMC_summarydf, #specify data to initiate plot
                        aes(x=1, y=rownames(MCMC_summarydf)))+ #specify layout to initiate plot
    geom_tile(inherit.aes = FALSE, #plot tiles without regard to overall plot
              data = MCMC_summarydf, #specify tile data
              aes(x=1, y=rownames(MCMC_summarydf), fill=n.eff), #specify tile aesthetics
              col="black", #specify tiles have a black border
              fill="transparent")+ #specify tiles have no fill color
    geom_text(aes(x=1, y=rownames(MCMC_summarydf), label=n.eff))+ #print effective sample size in each tile
    geom_tile(inherit.aes = FALSE, #differentiate this plot from previous plots
              data = MCMC_summarydf, #specify data
              aes(x=2, y=rownames(MCMC_summarydf), fill=Rhat), #specity tile aesthetics
              col= "black", #specify tiles have a black border
              fill = ifelse(MCMC_summarydf$Rhat<0.8, horrid, #set conditional color values for r-hat
                            ifelse(MCMC_summarydf$Rhat>=0.8 & MCMC_summarydf$Rhat<0.9, bad,
                                   ifelse(MCMC_summarydf$Rhat>=0.9 & MCMC_summarydf$Rhat<0.95, good,
                                          ifelse(MCMC_summarydf$Rhat>= 0.95 & MCMC_summarydf$Rhat<= 1.05, great,
                                                 ifelse(MCMC_summarydf$Rhat>1.05 & MCMC_summarydf$Rhat<1.1, good,
                                                        ifelse(MCMC_summarydf$Rhat>= 1.1 & MCMC_summarydf$Rhat<1.2, bad, horrid)))))))+
    geom_text(aes(x=2, y=rownames(MCMC_summarydf), label=Rhat))+ #print r-hat size in each tile
    geom_tile(inherit.aes = FALSE, #differentiate this plot from previous plots
              data = MCMC_summarydf, #specify data
              aes(x=3, y=rownames(MCMC_summarydf), fill=color), #specity tile aesthetics
              col= "black", #specify tiles have a black border
              fill = ifelse(MCMC_summarydf$color %in% "very negative", horrid, #Set colors to indicate if 95% CI includes zero or not
                            ifelse(MCMC_summarydf$color %in% "negative", bad,
                                   ifelse(MCMC_summarydf$color %in% "slightly negative", meh,
                                          ifelse(MCMC_summarydf$color %in% "slightly positive", fine,
                                                 ifelse(MCMC_summarydf$color %in% "positive", good,
                                                        ifelse(MCMC_summarydf$color %in% "very positive", great, grey)))))))+
    geom_text(aes(x=3, y=rownames(MCMC_summarydf), label=round(mean, 2)))+ #print r-hat size in each tile
    coord_equal()+ #keeps tiles as perfect squares
    xlab("")+ #removes x axis label
    ylab("")+ #removes y axis label
    scale_x_continuous(labels=c("n.eff",expression(hat("r")),"mean"), breaks = c(1,2,3))+ #replaces x axis ticks with meaningful text
    ggtitle(y.vars[z])+
    theme(panel.grid = element_blank(), #removes grid lines
          panel.background = element_blank()) #removes background color
  
  
  #Building a posterior predictive check from MCMC chain and original dataset
  
  ndraws <- 100 #specify number of posterior draws
  mcmc.mat <- as.matrix(model) #combine model's MCMC into a single matrix
  
  samples <- sample(n.samples-burn.in, #Sample numbers between 0 (implied) and the number of non-burn-in chains
                    ndraws) #The number of samples is specified by ndraws
  
  samples <- mcmc.mat[samples,] #Create a matrix from the MCMC chain that has only the rows indicated by samples (n random draws from the chain)
  
  plot.df <- list() #create empty list to put plot into
  
  for(p in 1:nrow(samples)) { #begin function to run model on samples
    
    pull.values <- samples[p,]
    #Note that the model must be changed here if it has been changed in the actual model script, "poisson_model"
    
    temp.data <- DATA #creating new data frame to avoid overwriting original data in each simulation
    
    #Set the intercept based on the city in each row (from MCMC chain sample)
    temp.data$intercept <- ifelse(temp.data$city %in% "DC", pull.values[1],
                                  ifelse(temp.data$city %in% "MN", pull.values[2], pull.values[3]))
    
    #Run the model using the sampled parameter values from MCMC chain on each datapoint
    temp.data$y <- temp.data$intercept +
      (pull.values[4]*temp.data$prcnt_park_area) + 
      (pull.values[5]*temp.data$park_EIratio) +
      (pull.values[6]*temp.data$park_clumpy) +
      (pull.values[7]*temp.data$regional.C) +
      (pull.values[8]*temp.data$Pop.Dens) +
      (pull.values[9]*temp.data$year) +
      (pull.values[10]*temp.data$effort)
    
    output <- as.data.frame(temp.data$y) #isolate output
    output$run <- p
    
    plot.df[[p]] <- output #add output to the existing plot.df as its own column
    
  } #end function to run model on samples
  
  plot.df <- do.call("rbind", plot.df)
  names(plot.df) <- c("V1", "run")
  plot.df$V1 <- exp(plot.df$V1) #back-transform to original scale
  response.df <- as.data.frame(exp(data_jags$y)) #back-transform to original scale
  names(response.df) <- "y"
  
  
  pp_check_plot <- ggplot(data=plot.df, aes(V1, fill=as.factor(run)))+ #establish plot
    geom_density(color=fine, #make lines a muted green
                 lwd=.5, #specify line width
                 show.legend = FALSE)+ #remove legend
    scale_fill_manual(values = rep("transparent",ndraws))+ #Make fill color transparent so we only have lines
    geom_density(inherit.aes = FALSE, #add density line of actual data (not model results)
                 data = response.df, #specify data source
                 aes(y),
                 color=great, #make line a bight green
                 lwd=1, #specify line width
                 show.legend = FALSE)+ #remove legend
    xlim(0,max(response.df$y)+.5*max(response.df$y))+ #ensure x axis doesn't cut data off
    theme(panel.grid = element_blank(), #removes grid lines
          panel.background = element_blank(), #removes background color
          axis.line.x.bottom = element_line("black"), #colors x axis black
          axis.line.y.left = element_line("black"))+ #colors y axis black
    xlab(response.name)
  
  #Save MCMC chains
  mcmc.mat <- as.data.frame(mcmc.mat) #convert matrix to dataframe
  file.name <- paste("./",Sys.Date(),"_modelOutput/",response.name,"/MCMC.chains.csv", sep="") #Create a file name within the appropriate subfolder
  write.csv(mcmc.mat, 
            file = file.name)
  
  ########### Residuals 
  resid.df <- split(plot.df, plot.df$run) #split data frame into list of data frames by run
  
  format.resid.df <- function(x){ #begin sub-function
    
    df <- data.frame(var = x[,1]) #create new dataframe consisting only of the predicted values (which are always in the first column)
    
    names(df) <- paste("run_",x$run[1]) #rename to avoid issues of columns all being named the same thing
    
    df #leaving here so R knows what to take
    
  } #end sub-function
  
  resid.df <- lapply(resid.df, format.resid.df) #applying above function to resid.df
  resid.df <- do.call("cbind", resid.df) #combine residuals back into a single data frame where each column is a run, and each row is a survey prediction
  
  resid.df$Mean <- rowMeans(resid.df[,]) #calculate mean value for each predicted datapoint from MCMC chain
  resid.df$Residual <- exp(data_jags$y) - resid.df$Mean #calculate residuals
  resid.df$y <- exp(data_jags$y) #add true values to data frame for plotting purposes
  resid.df$city <- data_jags$City #add city value to residuals
  resid.df$year <- data_jags$Year #add year value to residuals
  
  #residuals by actual values
  resid.by.val <- ggplot(data=resid.df, aes(x=y, y=Residual, color=as.factor(city)))+ #establish plot
    geom_point()+ #add points to plot
    geom_hline(yintercept = 0)+ #add black line at zero
    xlab("Predicted Value")+
    theme(panel.grid = element_blank(), #removes grid lines
          panel.background = element_blank(), #removes background color
          axis.line.x.bottom = element_line("black"), #colors x axis black
          axis.line.y.left = element_line("black")) #colors y axis black
  
  #residuals by year
  resid.by.year <- ggplot(data=resid.df, aes(x=year, y=Residual, color=as.factor(city)))+ #establish plot
    geom_point()+ #add points to plot
    geom_hline(yintercept = 0)+ #add black line at zero
    xlab("Year")+
    theme(panel.grid = element_blank(), #removes grid lines
          panel.background = element_blank(), #removes background color
          axis.line.x.bottom = element_line("black"), #colors x axis black
          axis.line.y.left = element_line("black")) #colors y axis black
  
  
  #Group residual plots annd pp plot on top of one another
  resid.plots <- plot_grid(resid.by.val,resid.by.year,pp_check_plot,
                           nrow = 3)
  
  #Combine all plots into one
  all.plots <- ggdraw()+
    draw_plot(plot = resid.plots, 
              x = .4,
              y = 0,
              width=.6,
              height=1)+
    draw_plot(plot = values_plot,
              x=0,
              y=0,
              width=.4,
              height=1)
  
  file.name <- paste("./",Sys.Date(),"_modelOutput/",response.name,"/Diagnostics.png", sep="") #Create a file name within the appropriate subfolder
  
  #Save plot 
  ggsave(plot=all.plots,
         filename=file.name,
         width = 10,
         height = 8,
         units = "in",
         dpi=150)
  
} #end loop for Species diversity (Shannon) and functional richness


##### Modeling functional diversity

#DATA LIST
data_jags <- list("y" = as.vector(DATA[,"funDiverg"]), #response variable is log-transformed to achieve log-normal distribution
                  "n" = nrow(DATA), #length of dataset
                  "prcnt_park_area" = as.vector(DATA$prcnt_park_area),
                  "park_clumpy" = as.vector(DATA$park_clumpy),#specify variable
                  "mean_park_EIratio" = as.vector(DATA$park_EIratio),
                  "regional.C" = as.vector(DATA$regional.C),
                  "Pop.Dens" = as.vector(DATA$Pop.Dens),
                  "total.effort" = as.vector(DATA$effort),
                  "Year" = as.vector(DATA$year),
                  "City" = as.vector(as.numeric(as.factor(DATA$city))),
                  "npar" = nparams,
                  "ncity" = nCity) #specify n params

#params_jags = c("bCity","b") #this is from a tutorial. No clue what this is

#Assign initial values
# function that generates starting values for parameters using a different RNG for each chain
inits_jags <- function(chain){
  gen_list <- function(chain = chain){
    list(b = rnorm(nparams, 0, 1),
         bCity = rnorm(nCity, 0, 1),
         sigma = rgamma(1, 1, 1),
         mu.city = rnorm(1, 0, 1),
         sigma.city = rgamma(1, 1, 1),
         phi = runif(1, 0.001,2),
         .RNG.name = switch(chain,
                            "1" = "base::Wichmann-Hill",
                            "2" = "base::Marsaglia-Multicarry",
                            "3" = "base::Super-Duper",
                            "4" = "base::Wichmann-Hill",
         ),
         .RNG.seed = sample(1:1e+06, 1)
    )
  }
  return(switch(chain,           
                "1" = gen_list(chain),
                "2" = gen_list(chain),
                "3" = gen_list(chain),
                "4" = gen_list(chain)
                
  )
  )
}

model <- as.mcmc.list(run.jags(model = "./scripts/MODEL_beta.R", #specify the model string
                               data = data_jags, #specify the data list
                               monitor = params_jags,  #params to save
                               inits = inits_jags, #specify initial values? I think for MCMC chain?
                               n.chains = n.chains, #specify number of chains
                               adapt = n.adapt, #similar to burn-in, but runs first.
                               burnin = burn.in, #burn in
                               sample = ceiling(n.samples/n.chains), #n samples per chain
                               thin = thin.steps, #reduce autocorrelation
                               summarise = FALSE, 
                               plots=FALSE,
                               method = "parallel"))


############################
#Running and saving diagnostics/results

#Create a new folder for materials to be kept in
dir.create(paste("./",Sys.Date(),"_modelOutput/funDiverg", sep="")) 

#Create summary of MCMC chains
MCMC_summarydf <- MCMCsummary(model,
                              params = params_jags,
                              round=5)

rownames(MCMC_summarydf) <- vars.vect

#Save summary of MCMC chains as a CSV
file.name <- paste("./",Sys.Date(),"_modelOutput/funDiverg/MCMC.summary.csv", sep="") #Create a file name within the appropriate subfolder
write.csv(x = MCMC_summarydf, #Save file to correct subfolder
          file = file.name)

#Create and save MCMC traceplots
file.name <- paste("./",Sys.Date(),"_modelOutput/funDiverg", sep="") #Create a file name within the appropriate subfolder
MCMCtrace(model, #specify model
          params = params_jags[2], #crate trace plots for all beta paramaters
          Rhat=TRUE, #include r-hat
          n.eff=TRUE, #include effective sample size
          pdf=TRUE, #print plots to a PDF document
          wd = file.name, #use specified file path/name
          open_pdf = FALSE) #do NOT open each PDF


MCMC_summarydf$color <- ifelse(MCMC_summarydf$`2.5%` <= -1 & MCMC_summarydf$`97.5%` <= -1, "very negative",
                               ifelse(MCMC_summarydf$`2.5%` <= -0.25 & MCMC_summarydf$`97.5%` <= -0.25, "negative",
                                      ifelse(MCMC_summarydf$`2.5%` <= -0 & MCMC_summarydf$`97.5%` <= -0, "slightly negative",
                                             ifelse(MCMC_summarydf$`2.5%` >= 0 & MCMC_summarydf$`97.5%` >= 0, "slightly positive",
                                                    ifelse(MCMC_summarydf$`2.5%` >= 0.25 & MCMC_summarydf$`97.5%` >= 0.25, "positive",
                                                           ifelse(MCMC_summarydf$`2.5%` >= 1 & MCMC_summarydf$`97.5%` >= 1, "very positive", "non-significant"))))))

#rownames(MCMC_summarydf) <- c("% park", "EI ratio", "clumpy", "temp", "pop dens", "year")

#Plot model outcomes as colored boxes with text
values_plot <- ggplot(data = MCMC_summarydf, #specify data to initiate plot
                      aes(x=1, y=rownames(MCMC_summarydf)))+ #specify layout to initiate plot
  geom_tile(inherit.aes = FALSE, #plot tiles without regard to overall plot
            data = MCMC_summarydf, #specify tile data
            aes(x=1, y=rownames(MCMC_summarydf), fill=n.eff), #specify tile aesthetics
            col="black", #specify tiles have a black border
            fill="transparent")+ #specify tiles have no fill color
  geom_text(aes(x=1, y=rownames(MCMC_summarydf), label=n.eff))+ #print effective sample size in each tile
  geom_tile(inherit.aes = FALSE, #differentiate this plot from previous plots
            data = MCMC_summarydf, #specify data
            aes(x=2, y=rownames(MCMC_summarydf), fill=Rhat), #specity tile aesthetics
            col= "black", #specify tiles have a black border
            fill = ifelse(MCMC_summarydf$Rhat<0.8, horrid, #set conditional color values for r-hat
                          ifelse(MCMC_summarydf$Rhat>=0.8 & MCMC_summarydf$Rhat<0.9, bad,
                                 ifelse(MCMC_summarydf$Rhat>=0.9 & MCMC_summarydf$Rhat<0.95, good,
                                        ifelse(MCMC_summarydf$Rhat>= 0.95 & MCMC_summarydf$Rhat<= 1.05, great,
                                               ifelse(MCMC_summarydf$Rhat>1.05 & MCMC_summarydf$Rhat<1.1, good,
                                                      ifelse(MCMC_summarydf$Rhat>= 1.1 & MCMC_summarydf$Rhat<1.2, bad, horrid)))))))+
  geom_text(aes(x=2, y=rownames(MCMC_summarydf), label=Rhat))+ #print r-hat size in each tile
  geom_tile(inherit.aes = FALSE, #differentiate this plot from previous plots
            data = MCMC_summarydf, #specify data
            aes(x=3, y=rownames(MCMC_summarydf), fill=color), #specity tile aesthetics
            col= "black", #specify tiles have a black border
            fill = ifelse(MCMC_summarydf$color %in% "very negative", horrid, #Set colors to indicate if 95% CI includes zero or not
                          ifelse(MCMC_summarydf$color %in% "negative", bad,
                                 ifelse(MCMC_summarydf$color %in% "slightly negative", meh,
                                        ifelse(MCMC_summarydf$color %in% "slightly positive", fine,
                                               ifelse(MCMC_summarydf$color %in% "positive", good,
                                                      ifelse(MCMC_summarydf$color %in% "very positive", great, grey)))))))+
  geom_text(aes(x=3, y=rownames(MCMC_summarydf), label=round(mean, 2)))+ #print r-hat size in each tile
  coord_equal()+ #keeps tiles as perfect squares
  xlab("")+ #removes x axis label
  ylab("")+
  ggtitle("funDiverg")+
  theme(panel.grid = element_blank(), #removes grid lines
        panel.background = element_blank()) #removes background color


#Building a posterior predictive check from MCMC chain and original dataset

ndraws <- 100 #specify number of posterior draws
mcmc.mat <- as.matrix(model) #combine model's MCMC into a single matrix

samples <- sample(n.samples-burn.in, #Sample numbers between 0 (implied) and the number of non-burn-in chains
                  ndraws) #The number of samples is specified by ndraws

samples <- mcmc.mat[samples,] #Create a matrix from the MCMC chain that has only the rows indicated by samples (n random draws from the chain)

plot.df <- list() #create empty list to put plot into

for(p in 1:nrow(samples)) { #begin function to run model on samples
  
  pull.values <- samples[p,]
  #Note that the model must be changed here if it has been changed in the actual model script, "poisson_model"
  
  temp.data <- DATA #creating new data frame to avoid overwriting original data in each simulation
  
  #Set the intercept based on the city in each row (from MCMC chain sample)
  temp.data$intercept <- ifelse(temp.data$city %in% "DC", pull.values[1],
                                ifelse(temp.data$city %in% "MN", pull.values[2], pull.values[3]))
  
  #Run the model using the sampled parameter values from MCMC chain on each datapoint
  temp.data$y <- temp.data$intercept +
    (pull.values[4]*temp.data$prcnt_park_area) + 
    (pull.values[5]*temp.data$park_EIratio) +
    (pull.values[6]*temp.data$park_clumpy) +
    (pull.values[7]*temp.data$regional.C) +
    (pull.values[8]*temp.data$Pop.Dens) +
    (pull.values[9]*temp.data$year)+
    (pull.values[10]*temp.data$effort)
  
  temp.data$y <- boot::inv.logit(temp.data$y) #back-transform predicted values for sake of beta regression (requires boot library)
  
  output <- as.data.frame(temp.data$y) #isolate output
  output$run <- p
  
  plot.df[[p]] <- output #add output to the existing plot.df as its own column
  
} #end function to run model on samples

plot.df <- do.call("rbind", plot.df)
names(plot.df) <- c("V1", "run")
response.df <- as.data.frame(data_jags$y)
names(response.df) <- "y"
#response.df$y <- response.df$y

pp_check_plot <- ggplot(data=plot.df, aes(V1, fill=as.factor(run)))+ #establish plot
  geom_density(color=fine, #make lines a muted green
               lwd=.5, #specify line width
               show.legend = FALSE)+ #remove legend
  scale_fill_manual(values = rep("transparent",ndraws))+ #Make fill color transparent so we only have lines
  geom_density(inherit.aes = FALSE, #add density line of actual data (not model results)
               data = response.df, #specify data source
               aes(y),
               color=great, #make line a bight green
               lwd=1, #specify line width
               show.legend = FALSE)+ #remove legend
  xlim(0,max(response.df$y)+.5*max(response.df$y))+ #ensure x axis doesn't cut data off
  theme(panel.grid = element_blank(), #removes grid lines
        panel.background = element_blank(), #removes background color
        axis.line.x.bottom = element_line("black"), #colors x axis black
        axis.line.y.left = element_line("black"))+ #colors y axis black
  xlab("funDiverg")

#Save MCMC chains
mcmc.mat <- as.data.frame(mcmc.mat) #convert matrix to dataframe
file.name <- paste("./",Sys.Date(),"_modelOutput/funDiverg/MCMC.chains.csv", sep="") #Create a file name within the appropriate subfolder
write.csv(mcmc.mat, 
          file = file.name)

########### Residuals 
resid.df <- split(plot.df, plot.df$run) #split data frame into list of data frames by run

format.resid.df <- function(x){ #begin sub-function
  
  df <- data.frame(var = x[,1]) #create new dataframe consisting only of the predicted values (which are always in the first column)
  
  names(df) <- paste("run_",x$run[1]) #rename to avoid issues of columns all being named the same thing
  
  df #leaving here so R knows what to take
  
} #end sub-function

resid.df <- lapply(resid.df, format.resid.df) #applying above function to resid.df
resid.df <- do.call("cbind", resid.df) #combine residuals back into a single data frame where each column is a run, and each row is a survey prediction

resid.df$Mean <- rowMeans(resid.df[,]) #calculate mean value for each predicted datapoint from MCMC chain
resid.df$Residual <- data_jags$y - resid.df$Mean #calculate residuals
resid.df$y <- data_jags$y #add true values to data frame for plotting purposes
resid.df$city <- data_jags$City #add city value to residuals
resid.df$year <- data_jags$Year #add year value to residuals

#residuals by actual values
resid.by.val <- ggplot(data=resid.df, aes(x=y, y=Residual, color=as.factor(city)))+ #establish plot
  geom_point()+ #add points to plot
  geom_hline(yintercept = 0)+ #add black line at zero
  xlab("Predicted Value")+
  theme(panel.grid = element_blank(), #removes grid lines
        panel.background = element_blank(), #removes background color
        axis.line.x.bottom = element_line("black"), #colors x axis black
        axis.line.y.left = element_line("black")) #colors y axis black

#residuals by year
resid.by.year <- ggplot(data=resid.df, aes(x=year, y=Residual, color=as.factor(city)))+ #establish plot
  geom_point()+ #add points to plot
  geom_hline(yintercept = 0)+ #add black line at zero
  xlab("Year")+
  theme(panel.grid = element_blank(), #removes grid lines
        panel.background = element_blank(), #removes background color
        axis.line.x.bottom = element_line("black"), #colors x axis black
        axis.line.y.left = element_line("black")) #colors y axis black


#Group residual plots annd pp plot on top of one another
resid.plots <- plot_grid(resid.by.val,resid.by.year,pp_check_plot,
                         nrow = 3)

#Combine all plots into one
all.plots <- ggdraw()+
  draw_plot(plot = resid.plots, 
            x = .4,
            y = 0,
            width=.6,
            height=1)+
  draw_plot(plot = values_plot,
            x=0,
            y=0,
            width=.4,
            height=1)

file.name <- paste("./",Sys.Date(),"_modelOutput/funDiverg/Diagnostics.png", sep="") #Create a file name within the appropriate subfolder

#Save plot 
ggsave(plot=all.plots,
       filename=file.name,
       width = 10,
       height = 8,
       units = "in",
       dpi=150)

#end process for functional diversity


##### Modeling Species richness

#DATA LIST
data_jags <- list("y" = as.vector(DATA[,"sppRichness"]), #response variable is log-transformed to achieve log-normal distribution
                  "n" = nrow(DATA), #length of dataset
                  "prcnt_park_area" = as.vector(DATA$prcnt_park_area),
                  "park_clumpy" = as.vector(DATA$park_clumpy),#specify variable
                  "mean_park_EIratio" = as.vector(DATA$park_EIratio),
                  "regional.C" = as.vector(DATA$regional.C),
                  "Pop.Dens" = as.vector(DATA$Pop.Dens),
                  "total.effort" = as.vector(DATA$effort),
                  "Year" = as.vector(DATA$year),
                  "City" = as.vector(as.numeric(as.factor(DATA$city))),
                  "npar" = nparams,
                  "ncity" = nCity) #specify n params

#params_jags = c("bCity","b","sigma", "mu.city", "sigma.city") #this is from a tutorial. No clue what this is

#Assign initial values
# function that generates starting values for parameters using a different RNG for each chain
inits_jags <- function(chain){
  gen_list <- function(chain = chain){
    list(b = rnorm(nparams, 0, 1),
         bCity = rnorm(nCity, 0, 1),
         sigma = rgamma(1, 1, 1),
         mu.city = rnorm(1, 0, 1),
         sigma.city = rgamma(1, 1, 1),
         .RNG.name = switch(chain,
                            "1" = "base::Wichmann-Hill",
                            "2" = "base::Marsaglia-Multicarry",
                            "3" = "base::Super-Duper",
                            "4" = "base::Wichmann-Hill",
         ),
         .RNG.seed = sample(1:1e+06, 1)
    )
  }
  return(switch(chain,           
                "1" = gen_list(chain),
                "2" = gen_list(chain),
                "3" = gen_list(chain),
                "4" = gen_list(chain)
                
  )
  )
}

model <- as.mcmc.list(run.jags(model = "./scripts/MODEL_rich.poisson.R", #specify the model string
                               data = data_jags, #specify the data list
                               monitor = params_jags,  #params to save
                               inits = inits_jags, #specify initial values? I think for MCMC chain?
                               n.chains = n.chains, #specify number of chains
                               adapt = n.adapt, #similar to burn-in, but runs first.
                               burnin = burn.in, #burn in
                               sample = ceiling(n.samples/n.chains), #n samples per chain
                               thin = thin.steps, #reduce autocorrelation
                               summarise = FALSE, 
                               plots=FALSE,
                               method = "parallel"))

############################
#Running and saving diagnostics/results

#Create a new folder for materials to be kept in
dir.create(paste("./",Sys.Date(),"_modelOutput/sppRichness", sep="")) 


#Create summary of MCMC chains
MCMC_summarydf <- MCMCsummary(model,
                              params = params_jags,
                              round=5)
rownames(MCMC_summarydf) <- vars.vect

#Save summary of MCMC chains as a CSV
file.name <- paste("./",Sys.Date(),"_modelOutput/sppRichness/MCMC.summary.csv",sep="") #Create a file name within the appropriate subfolder
write.csv(x = MCMC_summarydf, #Save file to correct subfolder
          file = file.name)

#Create and save MCMC traceplots
file.name <- paste("./",Sys.Date(),"_modelOutput/sppRichness", sep="") #Create a file name within the appropriate subfolder
MCMCtrace(model, #specify model
          params = c(params_jags), #crate trace plots for all beta paramaters
          Rhat=TRUE, #include r-hat
          n.eff=TRUE, #include effective sample size
          pdf=TRUE, #print plots to a PDF document
          wd = file.name, #use specified file path/name
          open_pdf = FALSE) #do NOT open each PDF

#assign a color based on whether or not the credible interval contains 0
MCMC_summarydf$color <- ifelse(MCMC_summarydf$`2.5%` <= -1 & MCMC_summarydf$`97.5%` <= -1, "very negative",
                               ifelse(MCMC_summarydf$`2.5%` <= -0.25 & MCMC_summarydf$`97.5%` <= -0.25, "negative",
                                      ifelse(MCMC_summarydf$`2.5%` <= -0 & MCMC_summarydf$`97.5%` <= -0, "slightly negative",
                                             ifelse(MCMC_summarydf$`2.5%` >= 0 & MCMC_summarydf$`97.5%` >= 0, "slightly positive",
                                                    ifelse(MCMC_summarydf$`2.5%` >= 0.25 & MCMC_summarydf$`97.5%` >= 0.25, "positive",
                                                           ifelse(MCMC_summarydf$`2.5%` >= 1 & MCMC_summarydf$`97.5%` >= 1, "very positive", "non-significant"))))))

sp.output <- data.frame(species = y.vars[z],
                        b1 = ifelse(MCMC_summarydf[1,"color"] %in% "non-significant", NA, MCMC_summarydf[1,"mean"]),
                        b2 = ifelse(MCMC_summarydf[2,"color"] %in% "non-significant", NA, MCMC_summarydf[2,"mean"]),
                        b3 = ifelse(MCMC_summarydf[3,"color"] %in% "non-significant", NA, MCMC_summarydf[3,"mean"]),
                        b4 = ifelse(MCMC_summarydf[4,"color"] %in% "non-significant", NA, MCMC_summarydf[4,"mean"]),
                        b5 = ifelse(MCMC_summarydf[5,"color"] %in% "non-significant", NA, MCMC_summarydf[5,"mean"]),
                        b6 = ifelse(MCMC_summarydf[6,"color"] %in% "non-significant", NA, MCMC_summarydf[6,"mean"]))


values_df <- MCMC_summarydf #rename just for this plot
values_df <- values_df[-which(rownames(values_df) %in% c("bCity[1]", "bCity[2]", "bCity[3]", "effort", "mu.city", "sigma.city")), ] #remove rows that we don't care about (effort shouldn't be there in the first place, but removing just in case it is)
#rownames(values_df) <- c("% park", "EI ratio", "clumpy", "temp", "pop dens", "year")

#Need to change color gradients to be more intutitive! 
values_plot <- ggplot(data = values_df, #specify data to initiate plot
                      aes(x=1, y=rownames(values_df)))+ #specify layout to initiate plot
  geom_tile(inherit.aes = FALSE, #plot tiles without regard to overall plot
            data = values_df, #specify tile data
            aes(x=1, y=rownames(values_df), fill=n.eff), #specify tile aesthetics
            col="black", #specify tiles have a black border
            fill="transparent")+ #specify tiles have no fill color
  geom_text(aes(x=1, y=rownames(values_df), label=n.eff))+ #print effective sample size in each tile
  geom_tile(inherit.aes = FALSE, #differentiate this plot from previous plots
            data = values_df, #specify data
            aes(x=2, y=rownames(values_df), fill=Rhat), #specity tile aesthetics
            col= "black", #specify tiles have a black border
            fill = ifelse(values_df$Rhat<0.8, horrid, #set conditional color values for r-hat
                          ifelse(values_df$Rhat>=0.8 & values_df$Rhat<0.9, bad,
                                 ifelse(values_df$Rhat>=0.9 & values_df$Rhat<0.95, good,
                                        ifelse(values_df$Rhat>= 0.95 & values_df$Rhat<= 1.05, great,
                                               ifelse(values_df$Rhat>1.05 & values_df$Rhat<1.1, good,
                                                      ifelse(values_df$Rhat>= 1.1 & values_df$Rhat<1.2, bad, horrid)))))))+
  geom_text(aes(x=2, y=rownames(values_df), label=Rhat))+ #print r-hat size in each tile
  geom_tile(inherit.aes = FALSE, #differentiate this plot from previous plots
            data = values_df, #specify data
            aes(x=3, y=rownames(values_df), fill=color), #specity tile aesthetics
            col= "black", #specify tiles have a black border
            fill = ifelse(values_df$color %in% "very negative", horrid, #Set colors to indicate if 95% CI includes zero or not
                          ifelse(values_df$color %in% "negative", bad,
                                 ifelse(values_df$color %in% "slightly negative", meh,
                                        ifelse(values_df$color %in% "slightly positive", fine,
                                               ifelse(values_df$color %in% "positive", good,
                                                      ifelse(values_df$color %in% "very positive", great, grey)))))))+
  geom_text(aes(x=3, y=rownames(values_df), label=round(mean, 2)))+ #print r-hat size in each tile
  coord_equal()+ #keeps tiles as perfect squares
  xlab("")+ #removes x axis label
  ylab("")+ #removes y axis label
  scale_x_continuous(labels=c("n.eff",expression(hat("r")),"mean"), breaks = c(1,2,3))+ #replaces x axis ticks with meaningful text
  ggtitle("sppRichness")+
  theme(panel.grid = element_blank(), #removes grid lines
        panel.background = element_blank()) #removes background color


#Building a posterior predictive check from MCMC chain and original dataset

ndraws <- 100 #specify number of posterior draws
mcmc.mat <- as.matrix(model) #combine model's MCMC into a single matrix

samples <- sample(n.samples-burn.in, #Sample numbers between 0 (implied) and the number of non-burn-in chains
                  ndraws) #The number of samples is specified by ndraws

samples <- mcmc.mat[samples,] #Create a matrix from the MCMC chain that has only the rows indicated by samples (n random draws from the chain)

plot.df <- list() #create empty list to put plot into

for(p in 1:nrow(samples)) { #begin function to run model on samples
  
  pull.values <- samples[p,]
  
  temp.data <- DATA #creating new data frame to avoid overwriting original data in each simulation
  
  #Set the intercept based on the city in each row (from MCMC chain sample)
  temp.data$intercept <- ifelse(temp.data$city %in% "DC", pull.values[1],
                                ifelse(temp.data$city %in% "MN", pull.values[2], pull.values[3]))
  
  #Run the model using the sampled parameter values from MCMC chain on each datapoint
  temp.data$y <- temp.data$intercept +
    (pull.values[4]*temp.data$prcnt_park_area) + 
    (pull.values[5]*temp.data$park_EIratio) +
    (pull.values[6]*temp.data$park_clumpy) +
    (pull.values[7]*temp.data$regional.C) +
    (pull.values[8]*temp.data$Pop.Dens) +
    (pull.values[9]*temp.data$year)+ #no trait data here since average
    (pull.values[10]*temp.data$effort)
  
  #Back-transform predicted values
  temp.data$y <- exp(temp.data$y) 
  
  output <- as.data.frame(temp.data$y)
  output$run <- p
  
  plot.df[[p]] <- output #add output to the existing plot.df as its own column
  
} #end function to run model on samples

plot.df <- do.call("rbind", plot.df)
names(plot.df) <- c("V1", "run")
response.df <- as.data.frame(data_jags$y)
names(response.df) <- "y"
response.df$y <- response.df$y

pp_check_plot <- ggplot(data=plot.df, aes(V1, fill=as.factor(run)))+ #establish plot
  geom_density(color=fine, #make lines a muted green
               lwd=.5, #specify line width
               show.legend = FALSE)+ #remove legend
  scale_fill_manual(values = rep("transparent",ndraws))+ #Make fill color transparent so we only have lines
  geom_density(inherit.aes = FALSE, #add density line of actual data (not model results)
               data = response.df, #specify data source
               aes(y),
               color=great, #make line a bight green
               lwd=1, #specify line width
               show.legend = FALSE)+ #remove legend
  xlim(0,max(response.df$y)+.5*max(response.df$y))+ #ensure x axis doesn't cut data off
  theme(panel.grid = element_blank(), #removes grid lines
        panel.background = element_blank(), #removes background color
        axis.line.x.bottom = element_line("black"), #colors x axis black
        axis.line.y.left = element_line("black"))+ #colors y axis black
  xlab("sppRichness")

#Save MCMC chains
mcmc.mat <- as.data.frame(mcmc.mat) #convert matrix to dataframe
file.name <- paste("./",Sys.Date(),"_modelOutput/sppRichness/MCMC.chains.csv", sep="") #Create a file name within the appropriate subfolder
write.csv(mcmc.mat, 
          file = file.name)

########### Residuals 
resid.df <- split(plot.df, plot.df$run) #split data frame into list of data frames by run

format.resid.df <- function(x){ #begin sub-function
  
  df <- data.frame(var = x[,1]) #create new dataframe consisting only of the predicted values (which are always in the first column)
  
  names(df) <- paste("run_",x$run[1]) #rename to avoid issues of columns all being named the same thing
  
  df #leaving here so R knows what to take
  
} #end sub-function

resid.df <- lapply(resid.df, format.resid.df) #applying above function to resid.df
resid.df <- do.call("cbind", resid.df) #combine residuals back into a single data frame where each column is a run, and each row is a survey prediction

#Calculate residual from predicted value
resid.df$Expected <- rowMeans(resid.df)
resid.df$Residual <- DATA$sppRichness - resid.df$Expected
resid.df$PearsonResid <- resid.df$Residual / sqrt(resid.df$Expected)
resid.df$city <- DATA$city #adding this for plotting purposes
resid.df$year <- DATA$year #adding this for plotting purposes

#residuals by actual values
resid.by.val <- ggplot(data=resid.df, aes(x=Expected, y=PearsonResid, color=as.factor(city)))+ #establish plot
  geom_point()+ #add points to plot
  geom_hline(yintercept = 0)+ #add black line at zero
  xlab("Predicted Value")+
  theme(panel.grid = element_blank(), #removes grid lines
        panel.background = element_blank(), #removes background color
        axis.line.x.bottom = element_line("black"), #colors x axis black
        axis.line.y.left = element_line("black")) #colors y axis black

#residuals by year
resid.by.year <- ggplot(data=resid.df, aes(x=year, y=PearsonResid, color=as.factor(city)))+ #establish plot
  geom_point()+ #add points to plot
  geom_hline(yintercept = 0)+ #add black line at zero
  xlab("Year")+
  theme(panel.grid = element_blank(), #removes grid lines
        panel.background = element_blank(), #removes background color
        axis.line.x.bottom = element_line("black"), #colors x axis black
        axis.line.y.left = element_line("black")) #colors y axis black


#Group residual plots and pp plot on top of one another
resid.plots <- plot_grid(resid.by.val,resid.by.year,pp_check_plot,
                         nrow = 3)

#Combine all plots into one
all.plots <- ggdraw()+
  draw_plot(plot = resid.plots, 
            x = .4,
            y = 0,
            width=.6,
            height=1)+
  draw_plot(plot = values_plot,
            x=0,
            y=0,
            width=.4,
            height=1)

file.name <- paste("./",Sys.Date(),"_modelOutput/sppRichness/Diagnostics.png", sep="") #Create a file name within the appropriate subfolder

#Save plot 
ggsave(plot=all.plots,
       filename=file.name,
       width = 10,
       height = 8,
       units = "in",
       dpi=150)


##### Modeling abundance of trait groups
#DAN! Something in here isn't working. Pick back up here.
if(!(dir.exists(paste(Sys.Date(),"_modelOutput/by_group", sep="_")))){dir.create(paste("./",Sys.Date(),"_modelOutput/by_group", sep=""))}

y.vars <- c("range_class",
            "clutch_class",
            "forage_class",
            "mass_class",
            "diet_class")

params_jags = c(params_jags, "bLarge", "bSmall") #this is from a tutorial. No clue what this is
vars.vect <- c(vars.vect, "trait.large", "trait.small")

for(z in 1:length(y.vars)){ #begin functional group-specific loop
  
  dir.create(paste("./",Sys.Date(),"_modelOutput/by_group/", y.vars[z],sep="")) #create species-specific sub-folder
  
  funct.DATA <- obsXsurvey[obsXsurvey$funct.trait %in% y.vars[z],] #Isolate data set to relevant values
  
  funct.DATA$small <- ifelse(funct.DATA$funct.group %in% "2small", 1, 0)
  funct.DATA$large <- ifelse(funct.DATA$funct.group %in% "3large", 1, 0)
  
  #ntraits <- length(unique(funct.DATA$funct.group)) #calculate the number trait groups in the data set
  
  #DATA LIST
  data_jags <- list("y" = as.vector(funct.DATA$n.obs), #response variable
                    "n" = nrow(funct.DATA), #length of dataset
                    "prcnt_park_area" = funct.DATA$prcnt_park_area,
                    "park_clumpy" = funct.DATA$park_clumpy,#specify variable
                    "mean_park_EIratio" = funct.DATA$park_EIratio,
                    "regional.C" = funct.DATA$regional.C,
                    "Pop.Dens" = funct.DATA$Pop.Dens,
                    "total.effort" = as.vector(funct.DATA$effort),
                    #"trait" = as.vector(as.numeric(as.factor(funct.DATA$funct.group))),
                    "small" = funct.DATA$small,
                    "large" = funct.DATA$large,
                    "Year" = as.vector(funct.DATA$year),
                    "City" = as.vector(as.numeric(as.factor(funct.DATA$city))),
                    "npar" = nparams, #specify n params
                    #"ntrait" = ntraits, #specify number of trait groups
                    "ncity" = nCity)  #specify number of cities
  
  
  #Assign initial values
  # function that generates starting values for parameters using a different RNG for each chain
  inits_jags <- function(chain){
    gen_list <- function(chain = chain){
      list(b = rnorm(nparams, 0, 10),
           bCity = rnorm(nCity, 0, 10),
           mu.city = rnorm(1, 0, 10),
           tau.city = rgamma(1, 3, 2),
           .RNG.name = switch(chain,
                              "1" = "base::Wichmann-Hill",
                              "2" = "base::Marsaglia-Multicarry",
                              "3" = "base::Super-Duper",
                              "4" = "base::Wichmann-Hill",
           ),
           .RNG.seed = sample(1:1e+06, 1)
      )
    }
    return(switch(chain,           
                  "1" = gen_list(chain),
                  "2" = gen_list(chain),
                  "3" = gen_list(chain),
                  "4" = gen_list(chain)
                  
    )
    )
  }
  
  
  
  model <- as.mcmc.list(run.jags(model = "./scripts/MODEL_trait.poisson.R", #specify the model string
                                 data = data_jags, #specify the data list
                                 monitor = params_jags,  #params to save
                                 inits = inits_jags, #specify initial values? I think for MCMC chain?
                                 n.chains = n.chains, #specify number of chains
                                 adapt = n.adapt, #similar to burn-in, but runs first.
                                 burnin = burn.in, #burn in
                                 sample = ceiling(n.samples/n.chains), #n samples per chain
                                 thin = thin.steps, #reduce autocorrelation
                                 summarise = FALSE, 
                                 module = "glm",               
                                 method = "parallel"))
  
  
  ############################
  #Running and saving diagnostics/results
  
  #Create summary of MCMC chains
  MCMC_summarydf <- MCMCsummary(model,
                                params = params_jags,
                                round=5)
  
  #Save summary of MCMC chains as a CSV
  file.name <- paste("./",Sys.Date(),"_modelOutput/by_group/",y.vars[z],"/MCMC.summary.csv",sep="") #Create a file name within the appropriate subfolder
  write.csv(x = MCMC_summarydf, #Save file to correct subfolder
            file = file.name)
  
  #Create and save MCMC traceplots
  file.name <- paste("./",Sys.Date(),"_modelOutput/by_group/",y.vars[z], sep="") #Create a file name within the appropriate subfolder
  MCMCtrace(model, #specify model
            params = c(params_jags), #crate trace plots for all beta paramaters
            Rhat=TRUE, #include r-hat
            n.eff=TRUE, #include effective sample size
            pdf=TRUE, #print plots to a PDF document
            wd = file.name, #use specified file path/name
            open_pdf = FALSE) #do NOT open each PDF
  
  #assign a color based on whether or not the credible interval contains 0
  MCMC_summarydf$color <- ifelse(MCMC_summarydf$`2.5%` <= -1 & MCMC_summarydf$`97.5%` <= -1, "very negative",
                                 ifelse(MCMC_summarydf$`2.5%` <= -0.25 & MCMC_summarydf$`97.5%` <= -0.25, "negative",
                                        ifelse(MCMC_summarydf$`2.5%` <= -0 & MCMC_summarydf$`97.5%` <= -0, "slightly negative",
                                               ifelse(MCMC_summarydf$`2.5%` >= 0 & MCMC_summarydf$`97.5%` >= 0, "slightly positive",
                                                      ifelse(MCMC_summarydf$`2.5%` >= 0.25 & MCMC_summarydf$`97.5%` >= 0.25, "positive",
                                                             ifelse(MCMC_summarydf$`2.5%` >= 1 & MCMC_summarydf$`97.5%` >= 1, "very positive", "non-significant"))))))
  
  sp.output <- data.frame(species = y.vars[z],
                          b1 = ifelse(MCMC_summarydf[1,"color"] %in% "non-significant", NA, MCMC_summarydf[1,"mean"]),
                          b2 = ifelse(MCMC_summarydf[2,"color"] %in% "non-significant", NA, MCMC_summarydf[2,"mean"]),
                          b3 = ifelse(MCMC_summarydf[3,"color"] %in% "non-significant", NA, MCMC_summarydf[3,"mean"]),
                          b4 = ifelse(MCMC_summarydf[4,"color"] %in% "non-significant", NA, MCMC_summarydf[4,"mean"]),
                          b5 = ifelse(MCMC_summarydf[5,"color"] %in% "non-significant", NA, MCMC_summarydf[5,"mean"]),
                          b6 = ifelse(MCMC_summarydf[6,"color"] %in% "non-significant", NA, MCMC_summarydf[6,"mean"]),
                          b7 = ifelse(MCMC_summarydf[7,"color"] %in% "non-significant", NA, MCMC_summarydf[7,"mean"]))
  
  #spp.output <- rbind(spp.output, sp.output) #add model parameters from this model to overall model results
  
  #Establish color pallete
  great <- "#77DD76"
    
  good <- "#BDE7BD"
    
  fine <- "#E7F1E8"
    
  grey <- "#D3D3D3"
    
  meh <- "#FFD5D4"
    
  bad <- "#FFB6B3"
    
  horrid <- "#FF6962"
    
  values_df <- MCMC_summarydf #rename just for this plot
  #values_df <- values_df[-which(rownames(values_df) %in% c("bCity[1]", "bCity[2]", "bCity[3]")), ]
  rownames(values_df) <- vars.vect #rename the rows to keep things straight
  
  #Need to change color gradients to be more intutitive! 
  values_plot <- ggplot(data = values_df, #specify data to initiate plot
                        aes(x=1, y=rownames(values_df)))+ #specify layout to initiate plot
    geom_tile(inherit.aes = FALSE, #plot tiles without regard to overall plot
              data = values_df, #specify tile data
              aes(x=1, y=rownames(values_df), fill=n.eff), #specify tile aesthetics
              col="black", #specify tiles have a black border
              fill="transparent")+ #specify tiles have no fill color
    geom_text(aes(x=1, y=rownames(values_df), label=n.eff))+ #print effective sample size in each tile
    geom_tile(inherit.aes = FALSE, #differentiate this plot from previous plots
              data = values_df, #specify data
              aes(x=2, y=rownames(values_df), fill=Rhat), #specity tile aesthetics
              col= "black", #specify tiles have a black border
              fill = ifelse(values_df$Rhat<0.8, horrid, #set conditional color values for r-hat
                            ifelse(values_df$Rhat>=0.8 & values_df$Rhat<0.9, bad,
                                   ifelse(values_df$Rhat>=0.9 & values_df$Rhat<0.95, good,
                                          ifelse(values_df$Rhat>= 0.95 & values_df$Rhat<= 1.05, great,
                                                 ifelse(values_df$Rhat>1.05 & values_df$Rhat<1.1, good,
                                                        ifelse(values_df$Rhat>= 1.1 & values_df$Rhat<1.2, bad, horrid)))))))+
    geom_text(aes(x=2, y=rownames(values_df), label=Rhat))+ #print r-hat size in each tile
    geom_tile(inherit.aes = FALSE, #differentiate this plot from previous plots
              data = values_df, #specify data
              aes(x=3, y=rownames(values_df), fill=color), #specity tile aesthetics
              col= "black", #specify tiles have a black border
              fill = ifelse(values_df$color %in% "very negative", horrid, #Set colors to indicate if 95% CI includes zero or not
                            ifelse(values_df$color %in% "negative", bad,
                                   ifelse(values_df$color %in% "slightly negative", meh,
                                          ifelse(values_df$color %in% "slightly positive", fine,
                                                 ifelse(values_df$color %in% "positive", good,
                                                        ifelse(values_df$color %in% "very positive", great, grey)))))))+
    geom_text(aes(x=3, y=rownames(values_df), label=round(mean, 2)))+ #print r-hat size in each tile
    coord_equal()+ #keeps tiles as perfect squares
    xlab("")+ #removes x axis label
    ylab("")+ #removes y axis label
    scale_x_continuous(labels=c("n.eff",expression(hat("r")),"mean"), breaks = c(1,2,3))+ #replaces x axis ticks with meaningful text
    ggtitle(y.vars[z])+
    theme(panel.grid = element_blank(), #removes grid lines
          panel.background = element_blank()) #removes background color
  
  
  #Building a posterior predictive check from MCMC chain and original dataset
  
  ndraws <- 100 #specify number of posterior draws
  mcmc.mat <- as.matrix(model) #combine model's MCMC into a single matrix
  
  samples <- sample(n.samples-burn.in, #Sample numbers between 0 (implied) and the number of non-burn-in chains
                    ndraws) #The number of samples is specified by ndraws
  
  samples <- mcmc.mat[samples,] #Create a matrix from the MCMC chain that has only the rows indicated by samples (n random draws from the chain)
  
  plot.df <- list() #create empty list to put plot into
  
  for(p in 1:nrow(samples)) { #begin function to run model on samples
    
    pull.values <- samples[p,]
    #Note that the model must be changed here if it has been changed in the actual model script, "poisson_model"
    
    temp.data <- DATA #creating new data frame to avoid overwriting original data in each simulation
    
    #Set the intercept based on the city in each row (from MCMC chain sample)
    temp.data$intercept <- ifelse(temp.data$city %in% "DC", pull.values[1],
                                  ifelse(temp.data$city %in% "MN", pull.values[2], pull.values[3]))
    
    #Run the model using the sampled parameter values from MCMC chain on each datapoint
    temp.data$y_avg <- temp.data$intercept +
      (pull.values[4]*temp.data$prcnt_park_area) + 
      (pull.values[5]*temp.data$park_EIratio) +
      (pull.values[6]*temp.data$park_clumpy) +
      (pull.values[7]*temp.data$regional.C) +
      (pull.values[8]*temp.data$Pop.Dens) +
      (pull.values[9]*temp.data$year)+ 
      (pull.values[10]*temp.data$effort) #no trait data here since average
    
    
    temp.data$y_high <- temp.data$intercept +
      (pull.values[4]*temp.data$prcnt_park_area) + 
      (pull.values[5]*temp.data$park_EIratio) +
      (pull.values[6]*temp.data$park_clumpy) +
      (pull.values[7]*temp.data$regional.C) +
      (pull.values[8]*temp.data$Pop.Dens) +
      (pull.values[9]*temp.data$year) +
      (pull.values[12])+ #addition for large trait group
      (pull.values[10]*temp.data$effort) 
    
    if(!(y.vars[z] %in% "mass_class")){ #don't do this for the mass trait since there are no "small" birds
      temp.data$y_low <- temp.data$intercept +
        (pull.values[4]*temp.data$prcnt_park_area) + 
        (pull.values[5]*temp.data$park_EIratio) +
        (pull.values[6]*temp.data$park_clumpy) +
        (pull.values[7]*temp.data$regional.C) +
        (pull.values[8]*temp.data$Pop.Dens) +
        (pull.values[9]*temp.data$year) +
        (pull.values[13])+ #addition for small trait group
        (pull.values[10]*temp.data$effort)
    } #end loop
    
    #Back-transform predicted values
    temp.data$y_avg <- exp(temp.data$y_avg) 
    temp.data$y_high <- exp(temp.data$y_high)
    if(!(y.vars[z] %in% "mass_class")){temp.data$y_low <- exp(temp.data$y_low)}
    
    avg.vals <- as.data.frame(temp.data$y_avg)
    names(avg.vals) <- "val"
    
    large.vals <- as.data.frame(temp.data$y_high)
    names(large.vals) <- "val"
    
    if(!(y.vars[z] %in% "mass_class")){
      small.vals <- as.data.frame(temp.data$y_low)
      names(small.vals) <- "val"}
    
    if(!(y.vars[z] %in% "mass_class")){
      output <- rbind(avg.vals,large.vals,small.vals)} #isolate output
    if((y.vars[z] %in% "mass_class")){
      output <- rbind(avg.vals,large.vals)} #isolate output
    
    
    output$run <- p
    
    plot.df[[p]] <- output #add output to the existing plot.df as its own column
    
  } #end function to run model on samples
  
  plot.df <- do.call("rbind", plot.df)
  names(plot.df) <- c("V1", "run")
  response.df <- as.data.frame(data_jags$y)
  names(response.df) <- "y"
  #response.df$y <- response.df$y
  
  pp_check_plot <- ggplot(data=plot.df, aes(V1, fill=as.factor(run)))+ #establish plot
    geom_density(color=fine, #make lines a muted green
                 lwd=.5, #specify line width
                 show.legend = FALSE)+ #remove legend
    scale_fill_manual(values = rep("transparent",ndraws))+ #Make fill color transparent so we only have lines
    geom_density(inherit.aes = FALSE, #add density line of actual data (not model results)
                 data = response.df, #specify data source
                 aes(y),
                 color=great, #make line a bight green
                 lwd=1, #specify line width
                 show.legend = FALSE)+ #remove legend
    xlim(0,max(response.df$y)+.5*max(response.df$y))+ #ensure x axis doesn't cut data off
    theme(panel.grid = element_blank(), #removes grid lines
          panel.background = element_blank(), #removes background color
          axis.line.x.bottom = element_line("black"), #colors x axis black
          axis.line.y.left = element_line("black"))+ #colors y axis black
    xlab(y.vars[z])
  
  #Save MCMC chains
  mcmc.mat <- as.data.frame(mcmc.mat) #convert matrix to dataframe
  file.name <- paste("./",Sys.Date(),"_modelOutput/by_group/",y.vars[z],"/MCMC.chains.csv", sep="") #Create a file name within the appropriate subfolder
  write.csv(mcmc.mat, 
            file = file.name)
  
  ########### Residuals 
  #resid.df <- split(plot.df, plot.df$run) #split data frame into list of data frames by run
  
  resid.df <- funct.DATA
  
  #format.resid.df <- function(x){ #begin sub-function
  #  
  #  df <- data.frame(var = x[,1]) #create new dataframe consisting only of the predicted values (which are always in the first column)
  #  
  #  names(df) <- paste("run_",x$run[1]) #rename to avoid issues of columns all being named the same thing
  #  
  #  df #leaving here so R knows what to take
  #  
  #} #end sub-function
  #
  #resid.df <- lapply(resid.df, format.resid.df) #applying above function to resid.df
  #resid.df <- do.call("cbind", resid.df) #combine residuals back into a single data frame where each column is a run, and each row is a survey prediction
  
  #Assign intercept based on city
  resid.df$intercept <- ifelse(resid.df$city %in% "DC", MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "bCity[1]"),"mean"],
                               ifelse(resid.df$city %in% "MN", MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "bCity[2]"),"mean"],
                                      MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "bCity[3]"),"mean"]))
  
  #Predict observations
  resid.df$Residual <- resid.df$intercept+
    resid.df$prcnt_park_area*MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "b[1]"),"mean"]+
    resid.df$park_EIratio*MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "b[2]"),"mean"]+
    resid.df$park_clumpy*MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "b[3]"),"mean"]+
    resid.df$regional.C*MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "b[4]"),"mean"]+
    resid.df$Pop.Dens*MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "b[5]"),"mean"]+
    resid.df$year*MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "b[6]"),"mean"]+
    resid.df$year*MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "b[7]"),"mean"]+
    resid.df$small*MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "bSmall"),"mean"]+
    resid.df$large*MCMC_summarydf[which(rownames(MCMC_summarydf) %in% "bLarge"),"mean"]
  
  
  #Back-transform observations
  resid.df$Residual <- exp(resid.df$Residual)
  
  #Calculate residual from predicted value
  resid.df$Residual <- resid.df$n.obs - resid.df$Residual
  resid.df$PearsonResid <- resid.df$Residual / sqrt(resid.df$n.obs)
  
  #resid.df$Mean <- rowMeans(resid.df[,]) #calculate mean value for each predicted datapoint from MCMC chain
  #resid.df$Residual <- data_jags$y - resid.df$Mean #calculate residuals
  #resid.df$y <- data_jags$y #add true values to data frame for plotting purposes
  #resid.df$city <- data_jags$City #add city value to residuals
  #resid.df$year <- data_jags$Year #add year value to residuals
  
  #residuals by actual values
  resid.by.val <- ggplot(data=resid.df, aes(x=n.obs, y=PearsonResid, color=as.factor(city)))+ #establish plot
    geom_point()+ #add points to plot
    geom_hline(yintercept = 0)+ #add black line at zero
    xlab("Predicted Value")+
    theme(panel.grid = element_blank(), #removes grid lines
          panel.background = element_blank(), #removes background color
          axis.line.x.bottom = element_line("black"), #colors x axis black
          axis.line.y.left = element_line("black")) #colors y axis black
  
  #residuals by year
  resid.by.year <- ggplot(data=resid.df, aes(x=year, y=PearsonResid, color=as.factor(city)))+ #establish plot
    geom_point()+ #add points to plot
    geom_hline(yintercept = 0)+ #add black line at zero
    xlab("Year")+
    theme(panel.grid = element_blank(), #removes grid lines
          panel.background = element_blank(), #removes background color
          axis.line.x.bottom = element_line("black"), #colors x axis black
          axis.line.y.left = element_line("black")) #colors y axis black
  
  
  #Group residual plots annd pp plot on top of one another
  resid.plots <- plot_grid(resid.by.val,resid.by.year,pp_check_plot,
                           nrow = 3)
  
  #Combine all plots into one
  all.plots <- ggdraw()+
    draw_plot(plot = resid.plots, 
              x = .4,
              y = 0,
              width=.6,
              height=1)+
    draw_plot(plot = values_plot,
              x=0,
              y=0,
              width=.4,
              height=1)
  
  file.name <- paste("./",Sys.Date(),"_modelOutput/by_group/",y.vars[z],"/Diagnostics.png", sep="") #Create a file name within the appropriate subfolder
  
  #Save plot 
  ggsave(plot=all.plots,
         filename=file.name,
         width = 10,
         height = 8,
         units = "in",
         dpi=150)
  
} #end species-specific loop
