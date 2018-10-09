

#' --- 
#' title: "R-scripts, statistical analyses, results and plots."
#' subtitle: "Results from auxin inhibition study."
#' author: "Bishwa K. Giri"
#' date: "Oct 2nd, 2018"
#' output:
#'    html_document:
#'      toc: true
#' --- 


#' ## Step 01: Import the required libary if required
#' **Note : you can use your own data if need be.**

#library(ggplot2)
#library(tidyverse)


#' ## Step 02: Let's create some data
# make 4 treatment groups with 6 replicates
treatmentGrp = sample(rep(c("Aux_Spray", "Aux_Drop", "DMSO", "Water"), each = 6))

# we now need 4x6 = 24 samples
sample_id = sample(rep(c(1:24)))

# let's create a random diameter size for the month of January, February and March
diameterJan = sample(c(seq(from = 3, to = 7, by = 0.4)), size = 24, replace = TRUE)
diameterFeb = sample(c(seq(from = 4, to = 10, by = 0.3)), size = 24, replace = TRUE)
diameterMar = sample(c(seq(from = 3.5, to = 7, by = 0.7)), size = 24, replace = TRUE)

## Let's put all the above data into a dataframe
my_df <- data.frame(sampleID = sample_id, Treatment = treatmentGrp,
                    diameterInJan = diameterJan, diameterInFeb = diameterFeb,
                    diameterInMar = diameterMar)

# Now, we compute "diameter changes" betweeen each month
# and add it as new columns
my_df$diamChangeJanToFeb = my_df$diameterInFeb - my_df$diameterInJan
my_df$diamChangeFebToMar = my_df$diameterInMar - my_df$diameterInFeb



#' ## Step 03: Subset and Reshape the data 

# Before we reshape we need to subset the data that is required by the plot 
# Here, we need only "Treatment" and "Diameter" from each month)
my_dfSubset = subset(
  my_df, select = c("diameterInJan", "diameterInFeb", "diameterInMar",
                    "diamChangeJanToFeb", "diamChangeFebToMar", "Treatment"))
head(my_dfSubset)

# reshape the subsetted data from wide to long and thin format
my_dfReshaped = 
  my_dfSubset %>%  gather(Month, Diameter, -Treatment) 
head(my_dfReshaped)



#' ## Step 04: Calculate other required values from the data 

# Calculate "mean" and standard error of the mean (i.e SE) 
# by "Treatment" for each time period i.e "Month")
my_dfSubMean = aggregate(my_dfReshaped$Diameter,
                         by=list(Treatment = my_dfReshaped$Treatment, 
                                 Month = my_dfReshaped$Month), FUN = mean , na.rm=TRUE)  

head(my_dfSubMean)

# Change the column name "x" to "SubMean"
colnames(my_dfSubMean)[colnames(my_dfSubMean)=="x"] <- "SubMean"
my_dfSubMean

# Calculate the "standard error" of the mean (for each "Treatment" for each "Month") 
my_dfSubSE = aggregate(my_dfReshaped$Diameter,
                       by=list(Treatment = my_dfReshaped$Treatment, 
                               Month = my_dfReshaped$Month), FUN = sd , na.rm=TRUE)  

head(my_dfSubSE)

# Change column name "x" to "SubMeanSE"
colnames(my_dfSubSE)[colnames(my_dfSubSE)=="x"] <- "SubSE"
head(my_dfSubSE)

# Now, bind the "my_dfSubMean" with "my_dfSE" 
my_dfSubMean = Reduce(function(x, y) 
  merge(x, y, by=c("Treatment", "Month"), 
        all = TRUE), list(my_dfSubMean, my_dfSubSE))


#' ## Step 05: Split the data into smaller groups, so plotting can be made easier

## Split the "SubMean" and "SubSE" data into two sets
# one set containing the "diameter" values for each month
# another set containing the "diameter changes" values betweeen consecutive months
# ** Note : This becomes helpful when we want to apply "geom_line" and other methods to ..
# .. the "Diameter" and "Diameter changes" separately

# select values (from "Month" column) that contains only "diameter" for that month.
# we use grep to capture the values that have "Change" string in it, then invert the selection
my_dfDiameterSubMean = my_dfSubMean[grep('Change', my_dfSubMean$Month, invert = TRUE), ]

# select values (from "Month" column) that represent "diameter changes" between two 
# .. consecutive months
my_dfChangeSubMean = my_dfSubMean[grep('Change', my_dfSubMean$Month), ]


#' ## Step 06 : Now, start plotting

# We use input the main reshaped data for all the observations 

# pipe in the reshaped data 
my_dfReshaped  %>%
  
  # make an empty plot to start with
  ggplot() + 
  
  # create a minimal background for the plot
  # this should be put early in plot building, if not it will strip the 
  # .. custom theme added to the overall plot
  theme_minimal() + 
  
  # add main title and then x,y axis title
  ggtitle("Effect of auxin inhibitor (NPA) on diameter and diameter changes.") +
  xlab("Month") +
  ylab("Diameter Changes (mm) \t|\t Diameter (mm)") + 
  
  # add a layer of point/dot plot from all the observation (i.e "Diameter" and "Diameter Changes")
  # the data from different treatement as colored distinctly 
  geom_point(aes(x = factor(Month), y = Diameter, colour = Treatment), 
             na.rm = TRUE, size = 2,
             position = position_dodge(width = 0.2)) +
  
  # add custom color to the "Treatment" levels 
  scale_colour_manual( 
    values = c("Aux_Drop" = "Purple", "Aux_Spray" = "Red", 
               "DMSO" = "Orange", "Water" = "Green")) + 
  
  # add finer ticks to the y-axis values
  scale_y_continuous(breaks = pretty(my_dfReshaped$Diameter, n = 20)) +
  
  # Improve the quality of the plot title and x,y title
  theme(plot.title = element_text(face="bold", hjust=0, size = 18), 
        axis.title.x = element_text(face="bold", size = 12), 
        axis.title.y = element_text(color="red", face="bold", size = 12)) +
  
  # rearrange the x-axis (keep months in order), 
  # and rename the factors to make it neat
  # lower the position of the factor that indicate diameter changes by adding line break (i.e '\n'). 
  scale_x_discrete(limits=c(
    "diameterInJan", "diamChangeJanToFeb", "diameterInFeb", 
    "diamChangeFebToMar", "diameterInMar"), 
    labels = c("Jan", "\nJanToFeb", "Feb", "\nFebToMar", "Mar")) + 
  
  # update the font quality of the renamed x-axis ticks and y-axis ticks value 
  theme(axis.text.x = element_text(face = c(
    "plain", "bold", "plain", "bold", "plain"), size = 10), 
    axis.text.y = element_text(size = 10)) +
  # we can add, vjust, hjust, size to control the text quality in x-axis.
  
  # Now, add another layer of dot plots from  calculated mean (we use another dataframe)
  # also make the size of the "mean" value larger so it can be seen as a mean value
  geom_point(data = my_dfSubMean, size = 5, aes(
    x = factor(Month), y = SubMean, colour = Treatment), 
    na.rm = TRUE, position = position_dodge(width = 0.2)) +
  
  # add an error bar for "Mean" value for each "Treatement" for each time point
  geom_errorbar(data = my_dfSubMean, aes(
    x = Month, ymin=SubMean-SubSE, ymax=SubMean+SubSE, colour = Treatment),
    position = position_dodge(width = 0.2), width=.1, size = 0.5) + 
  
  # add horizontal abline to show positive vs. negative trend in diameter changes .. 
  # .. between consecutive months
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  
  ## Add another layer of line plot using "geom_line()"
  # Connect the "subMeans" of the "Diameter" values across time points
  # for this we need to connect the means of "Diameter" and means of "Diameter Changes" separately
  
  # connect the mean of "Diameter" for month of Jan, Feb, Mar
  geom_line(data = my_dfDiameterSubMean, aes(
    x = Month, y = SubMean, group = Treatment, colour = Treatment), 
    position = position_dodge(width = 0.2)) +
  
  # separately connect the mean of the "Diameter Changes" between Jan-Feb and Feb-Mar
  geom_line(data = my_dfChangeSubMean, aes(
    x = Month, y = SubMean, group = Treatment, colour = Treatment), 
    position = position_dodge(width = 0.2))




