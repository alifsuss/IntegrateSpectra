if (!require("pacman")) install.packages("pacman")
setwd("~/R/latih/IntegrateSpectra")

pacman::p_load(datasets, pacman, tidyverse, rio, dplyr, ggplot2, magrittr)
#magrittr = for pipes
#rio = for import and export data
#ggplot2 = for plotting
#dplyr / tidyverse for modifying table

#Importing the spectral data
df <- import("test_Spectra.txt") %>% 
  as_tibble() #%>%

#subtract the data with the means from row 1200 to 1300 <- baseline
for(i in 2:(ncol(df)-1)) {
  df[,i] = df[,i] - colMeans(df[1200:1300,i]) # subtract column 2 and higher with its means
}

#By creating this part for later to use it as the deltaX component
delx <- df %>%
  select(X)

#Calculating the delta X for all columns for the integration
for(i in 2:(nrow(df))){
  delx[i,] = delx[(i+2),]-delx[i+1,]
} #this will results in the delx[1,1] equal to the first wavelength and the last two rows as N/A

#Following code fills the first row as second and the last two rows as the third row from the last
delx[1,] = delx[2,]  
delx[(nrow(delx)-1),] = delx[(nrow(delx)-2),]
delx[(nrow(delx)),] = delx[(nrow(delx)-1),]  
delx = abs(delx) #make the dataframe in absolute value: removing the negative sign

#importing the delayed Time. time_duration files contains X=delayed time and the Y = duration
xaxis <- import("time_duration.txt") %>%
  as_tibble() #%>%

#computing the actual delayed time (the middle point of duration + delayed time) and store it in new column
xaxis %<>%
  mutate(tau = xaxis$V1 + xaxis$V2/2) %>%
  select(V1, V2, tau) # %>%

#Identifying the the integration window and the sensor deltaX block 
intwindow = xaxis[1:(ncol(df)-1),2]   #this is the window to divide the integtration based on the duration of aquisition

#INTEGRATION STARTS HERE:
for (i in (nrow(df))){
  B <- df[,2:(ncol(df)-1)] * t(delx)         #dX block is 0.24 (this obtained from intstrument) times all data from coloumn 2 to the end.
                                            # t(delx) transpose the delx so it can be used to multiply
}
#Alternatively, if you know exacly the deltaX value, then just use B below
#B <- 0.24 * df[,2:(ncol(df)-1)]      #This use less precise delta X resolutions
C = colSums(B) / intwindow            #divide the sum of the B with the intwindow
as_tibble(C)

decay <- merge(xaxis, C, by = 0)      #merge the xasis with C by 0 meaning by row
as_tibble(decay) 

#Selecting important columns
decay %<>%
  mutate(Intensity = V2.y) %>%        #set new column from V2.y
  select(tau, Intensity) # %>%        #delete all column except these two
#print()

#declare the ggplot component for easier plotting
pict <- decay %>% 
  ggplot(aes(x = tau, y = Intensity)) +
  geom_point()

#plot graph with log scale axes
pict + 
  ggtitle("Log-Log plot") +
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10") +
  xlab("time (ns)") +
  ylab("Integrated Intensity (a. u.)") +
  theme_light() +
#remove the above # and below, if you want to save the figure
  ggsave(filename = paste0("Integrated_Lin_lin_", format(Sys.time(), "%Y-%m-%d_%H-%M", tz = "Europe/London"),".jpg"), width = 6.3 , height = 8.49, dpi = 300, units = "in")


#plot graph with log lin axes
pict + 
  ggtitle("Log-Lin plot") +
  scale_y_continuous(trans = "log10") +
  theme_light() +
  xlab("time (ns)") +
  ylab("Integrated Intensity (a. u.)") +
#remove the above # and below, if you want to save the figure
  ggsave(filename = paste0("Integrated_Log_lin_", format(Sys.time(), "%Y-%m-%d_%H-%M", tz = "Europe/London"),".jpg"), width = 6.3 , height = 8.49, dpi = 300, units = "in")



#save the data with its corresponding time, Chenge the Integrated_XXX_ for every new file
export(decay, paste0("Integrated_XXX_", format(Sys.time(), "%Y-%m-%d_%H-%M", tz = "Europe/London"),".csv"))         

#remember to delete all data stored before proceed to next data!
rm(list = ls())                      

