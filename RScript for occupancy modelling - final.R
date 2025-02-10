#### 1. Getting started ####


#This is an R Script for carrying out occupancy analyses of data gathered from... 
#...the Masters Dissertation of Paul Pop (author of this script) in 2018. 
#In 2018, the analysis was run in the PRESENCE software version 12.10.
#Over 350 different models were run in the co-occurrence and single-species models...
#...at that time. However, the analyses is redone here (in 2024) due to overfitting...
#...models and issues surrounding large SE and 95% CI errors during the 2018 analyses.
#This script will be very useful for those attempting to do single-season single- or 
#...two-species occupancy modelling.
#The pre-print of the paper is available here: https://doi.org/10.1101/2024.04.17.589990

#The occupancy analyses is carried out using the package RPresence v. 2.13.60.
#Other packages used (besides the base packages and the ones automatically loaded...
#...via namespace): ggplot2_3.4.4, ggcorrplot_0.1.4.1, ggbeeswarm_0.7.2, dplyr_1.1.4,...
#...tidyr_1.2.0, data.table_1.14.2, gridExtra_2.3, janitor_2.2.0, stringr_1.4.0,...
#...tibble_3.1.6, officer_0.6.5, flextable_0.9.5, and knitr_1.37(optional).

#The codes used in section 2, 3, and 6 of this script has been adapted largely...
#...from the RStudio RPresence tutorials called OccupancyTuts...
#...(https://doi.org/10.1111/2041-210X.14285), as well as documentation for the...
#...package RPresence.

#This script doesn't assume any prior knowledge of R by the user.
#Install RStudio in your system to run this script.

#If you don't have the package installed, uncomment (remove the hashtags) and... 
#...run the following line of code:
#install.packages('RPresence', repo = 'https://eesc.usgs.gov/mbr/mbrCRAN')

#load the package
library(RPresence)

#Set working directory. This is important for selecting input files and saving...
#...outputs.In RStudio, you can simply click on the tab 'Session' on top and select... 
#'Set Working Directory' and then 'Choose Directory' OR simply press Ctrl+Shift+H
#Code for the doing the same:
#setwd(filepath)
#Filepath will look something like: "/media/paul/Dissertation/Paper" in Linux or...
#..."D:\\paul\\Dissertation\\occupancy\\Paper" in Windows.

setwd("/media/Dissertation/Paper")
  
#After setting the working directory, confirm that the folder you meant is...
#actually the one set as a directory.
getwd()
#See all the files in the working directory: 
list.files()


#### 2. Preparing inputs ####


#Load the input file for occupancy analyses from the working directory:

d <- read.csv("Data_for_analyses.csv", stringsAsFactors = FALSE, header = TRUE, 
               blank.lines.skip = TRUE)

#To learn more about the 'read.csv' function (for any custom modification): 
?read.table

#After the creation of 'd', see the structure of the components of this file:
str(d)

#Note that categorical variables such as 'Burn' is stored as integers (0 and 1)...
#...in my dataset instead of factors (Y or N). Both will work when running a... 
#...model. So, any of these two forms of coding of these covariates is acceptable.

#Now, create a visual representation of these covariates with histograms:

#Call ggplot2 for graphing:
library(ggplot2)
#For understanding the functions available in ggplot2:
?ggplot2

#Create the plots for site-level covariates:
#Area:
h1 <- ggplot(d, aes(x = Area)) +
  geom_bar(stat = 'count') +
  labs(x = "Area (ha)", y = "Count") +
  stat_bin(breaks = seq(from = 0, to = 5.25, by = 0.01)) +
  scale_x_continuous(breaks = seq(from = 0, to = 5.25, by = 0.5))

#Canopy height:
h2 <- ggplot(d, aes(x = Canopy_height)) +
  geom_bar(stat = 'count') +
  labs(x = "Canopy height (m)", y = "Count") +
  stat_bin(breaks = seq(from = 4, to = 31, by = 0.1)) +
  scale_x_continuous(breaks = seq(from = 4, to = 31, by = 1))

#Understorey height:
h3 <- ggplot(d, aes(x = Understorey_height)) +
  geom_bar(stat = 'count') +
  labs(x = "Understorey height (m)", y = "Count") +
  stat_bin(breaks = seq(from = 1, to = 2.5, by = 0.05)) +
  scale_y_continuous(breaks = seq(from = 0, to = 8, by = 1))

#Visibility:
h4 <- ggplot(d, aes(x = Visibility)) +
  geom_bar(stat = 'count') +
  labs(x = "Visibility (%)", y = "Count") +
  stat_bin(breaks = seq(from = 0, to = 100, by = 1)) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 1))

#Aspect
h5 <- ggplot(d, aes(x = Aspect)) +
  geom_bar(stat = 'count') +
  labs(x = "Aspect (degrees)", y = "Count") +
  stat_bin(breaks = seq(from = 40, to = 294, by = 1)) +
  scale_x_continuous(breaks = seq(from = 40, to = 294, by = 20)) +
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 1))

#Elevation
h6 <- ggplot(d, aes(x = Elevation)) +
  geom_bar(stat = 'count') +
  labs(x = "Elevation (m. a. s. l.)", y = "Count") +
  stat_bin(breaks = seq(from = 1450, to = 2140, by = 5)) +
  scale_x_continuous(breaks = seq(from = 1450, to = 2140,
                                                          by = 50)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 1))

#Slope
h7 <- ggplot(d, aes(x = Slope)) +
  geom_bar(stat = 'count') +
  labs(x = "Slope (degrees)", y = "Count") +
  stat_bin(breaks = seq(from = 29, to = 78, by = 0.5)) +
  scale_x_continuous(breaks = seq(from = 29, to = 78, by = 4)) +
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 1))

#Plot all the continuous site-specific covariates in a grid with gridExtra:
#install.library (gridExtra) #Install if not present
library(gridExtra)

plot(grid.arrange(h1, h2, h3, h4, ncol = 1))

plot(grid.arrange(h5, h6, h7, ncol = 1))

#You can keep removing objects from the environment by using the 'remove' function...
#...to clear unused memory and keep the system from lagging when using a system...
#...with low memory:
remove(h1, h2, h3, h4, h5, h6, h7)

#Since 9 of the site-specific covariates are binary (present/absent), plot them...
#...using stacked barplot to save time and space:

#Create dataframe for the barplot (the column numbers correspond to the binary...
#...covariates):
df <- d[c(22, 26:33)]
head (df, 10)

#This data needs to be converted into a specific format for graphing:
#install (if necessary) the following the packages and load them:
#install.packages("tidyr") #likely already there in RStudio by default
library(tidyr)
#install.packages("dplyr") #likely already there in RStudio by default
library(dplyr) 
#install.packages("janitor")
library(janitor)

#Convert dataframe into a compressed summarized form, by pivoting:
dfm <- df %>%
  pivot_longer(cols = everything(), values_to = "binary", names_to = "type") %>%
  tabyl(binary, type)

dfm

#Convert this wide format dataframe into long format:
dfmm <- data.frame(dfm[1], stack(dfm[2:ncol(dfm)]))
dfmm

#Recode the variable names for neater labeling (removing underscores and changing...
#...binary to "yes/no"):
dfmm$covariates <-recode(dfmm$ind, Burn = "Burn", Dirt_road = "Dirt road",
        Dry_lotic = "Dry lotic", Lantana_camara = "Lantana camara", Lentic =
        "Lentic", Metalled_road = "Metalled road", No_road = "No road",
        Rubus_niveus = "Rubus niveus", Wet_lotic = "Wet lotic")

dfmm$Presence <-recode(dfmm$binary, "0" = "No", "1" = "Yes")

#When labelling stacked plots, one method of changing stacking order - by specifying...
#...breaks in a scale, won’t work properly because the order of the cumulative sum...
#...won’t be the same as the stacking order. So, its best done by modifying the...
#...order of levels in the factor before taking the cumulative sum:
dfmm <- dfmm %>%
  arrange(covariates, rev(Presence))

#To put the labels in the middle of each bar, there must be an adjustment to the...
#...cumulative sum. So, calculate the y position, placing it in the middle:
dfmm <- dfmm %>%
  group_by(covariates) %>%
  mutate(label_y = cumsum(values) - 0.5 * values)

dfmm

#Create the stacked barplot:
ggplot(dfmm, aes(fill=Presence, y=values, x=covariates)) +
  geom_col() +
  geom_text(aes(y = label_y, label = values), colour = "white")+
  labs(x = "Covariates", y = "Count") +
  scale_y_continuous(breaks = seq(from = 0, to = 27, by = 1))+
  theme(legend.title = element_text(size = rel(1.2)))+
  theme(legend.text = element_text(face = "italic",family = "Arial",
                                   size = rel(1.2)))+
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

remove(df,dfm,dfmm)

#As stated by Gary White in the MARK helpﬁles: "When the mean value of individual...
#...covariates is very large or small, or the range of the covariate is over...
#...several orders of magnitude, the numerical optimization algorithm may fail...
#...to ﬁnd the correct parameter estimates." This is the case with my continous...
#...covariates. So, it has to be scaled. Use tne the Z score standardisation... 
#...technique for this purpose. The formula = (xi – x)/sd where: xi = The ith...
#...value in the dataset, x = The sample mean, and sd = The sample standard... 
#deviation. Here, the data will be scaled with x = 0 and sd = 1. 

#make the next step reproducible:
set.seed(1)

#scale the continuous variables to have x = 0 and sd = 1 using the mutate and...
#... and pipe (%>%) functions of the dplyr package:
dz <- d %>% mutate_at(c('Area', 'Canopy_height',	'Understorey_height',	
                          'Visibility',	'Aspect',	'Elevation',	'Slope'), 
                          ~(scale(.) %>% as.vector))

#Check the new database with z-standardised values:
head(dz, n = 5)

#Check if any of the continuous variables are highly correlated to each other...
#...by using Pearson's correlation. Compute the corelation matrix:
cor_matrix <- cor(
        dz[, c("Area", "Canopy_height", "Understorey_height",	
        "Visibility",	"Aspect",	"Elevation",	"Slope")]
                 )

#As a rule of thumb, any correlation over 0.5 can be considered significant. So...
#...there is a significant positive correlation between visibility and understorey...
#...height, and significant negative correlation between elevation and understorey...
#...height.

#Display the correlation matrix:
cor_matrix

#Plot the matrix with the package ggcorrplot:
library(ggcorrplot)
ggcorrplot(cor_matrix, 
            type = "lower",
            insig = "blank",
            lab = TRUE,
            digits = 3)

remove(cor_matrix)

#Check the correlation for non-standardised data:
cor_matrix2 <- cor(
        d[, c("Area", "Canopy_height", "Understorey_height",	
        "Visibility",	"Aspect",	"Elevation",	"Slope")]
                  )

#Display the correlation matrix:
cor_matrix2

#As you can see, the values are the exact same for z-standardised and non-standardised.

#Plot the matrix:
ggcorrplot(cor_matrix2, method = "circle")

remove(cor_matrix2)

#Create detection-history matrix variable for Montecincla fairbakii, Sholicola...
#...albiventris, and then two "compressed" format dataframes for 2 species models,...
#...omitting the 1st column (site names):
dethistMf=dz[,2:5]  #histories in columns 2-5
View(dethistMf)

dethistSa=dz[,6:9]  #histories in columns 6-9
View(dethistSa)

#MS format - Species A = M. fairbankii, Species B = S. albiventris:
dethist2sp=dz[,10:13]  #histories in columns 10-13
View(dethist2sp)

#SM format - Species A = S. albiventris, Species B = M. fairbankii:
dethistTsp=dz[,14:17]  #histories in columns 14-17
View(dethistTsp)

#Name the rows of dethist matrix variable using site names:
sitenames=dz[,1]  #sitenames in the 1st column
nsites=nrow(dethistMf)

sitenames=dz[,1] 
nsites=nrow(dethistSa)

sitenames=dz[,1]
nsites=nrow(dethist2sp)

sitenames=dz[,1]
nsites=nrow(dethistTsp)

#Create z-standardised site covariate table:
sitecov=as.data.frame(dz[18:33])
head(sitecov)

#Create non-standardised site covariate table (this is to test if there are...
#...significant differences in results due to z-standardisation)

sitecov2=as.data.frame(d[18:33])
head(sitecov2)

#Get the survey covariates:

#Before Noon (Surveys carried out before 1200 hrs)
BefNoon <- dz[,34:37]
                           
#After Noon (Surveys carried out after 1200 hrs)
AftNoon <- dz[,38:41]

#Clear Sky (The sky was clear/calm-cloudy during the survey)
ClrSky <- dz[,42:45]

#Overcast Sky (The sky was overcast during the survey)
Ovrcst <- dz[,46:49]

#Sunny (It was sunny during the survey)
Sunny <- dz[,50:53]

#Windy (It was windy during the survey)
Windy <- dz[,54:57]

#Warm (The felt temperature was warm during the survey)
Warm <- dz[,58:61]

#Neutral (The felt temperature was neutral during the survey)
Neutral <- dz[,62:65]

#Cool (The felt temperature was cool during the survey)
Cool <- dz[,66:69]

#Create data-frame from the 9  survey covariates. Nine 2-dimensional tables need...
#to be converted into a single 2-dimensional table. So, collapse each of the...
#covariate tables into single column vectors, and then create a data-frame from...
#these 9 columns:
surveycov=data.frame(BefNoon=unlist(BefNoon),
                     AftNoon=unlist(AftNoon),
                     ClrSky=unlist(ClrSky),
                     Ovrcst=unlist(Ovrcst),
                     Sunny=unlist(Sunny),
                     Windy=unlist(Windy),
                     Warm=unlist(Warm),
                     Neutral=unlist(Neutral),
                     Cool=unlist(Cool))

#Remove the rownames
row.names(surveycov) <- NULL
                     
#Look at the structure of the covariates:
str(surveycov)
head(surveycov)
View(surveycov)

#I will go on a tangent here and create the stacked barplot for these survey...
#...covariates, all of which are binary, using the steps used previously:
#Convert the surveycov dataframe into a compressed summarized form, by pivoting:
dfm <- surveycov %>%
  pivot_longer(cols = everything(), values_to = "binary", names_to = "type") %>%
  tabyl(binary, type)

dfm

#Convert this wide format dataframe into long format:
dfmm <- data.frame(dfm[1], stack(dfm[2:ncol(dfm)]))
dfmm

#Recode some of the variable names for neater labeling (expanding contractions...
#...and changing binary to "yes/no"):
dfmm$covariates <-recode(dfmm$ind, AftNoon = "After noon", BefNoon = "Before noon",
                         ClrSky = "Clear sky", Ovrcst = "Overcast")

dfmm$Presence <-recode(dfmm$binary, "0" = "No", "1" = "Yes")

#Modifying the order of levels in the factor before taking the cumulative sum:
dfmm <- dfmm %>%
  arrange(covariates, rev(Presence))

#Calculate the y position, placing the label in the middle:
dfmm <- dfmm %>%
  group_by(covariates) %>%
  mutate(label_y = cumsum(values) - 0.5 * values)

dfmm

#Create the stacked barplot:
ggplot(dfmm, aes(fill=Presence, y=values, x=covariates)) +
  geom_col() +
  geom_text(aes(y = label_y, label = values), colour = "white")+
  labs(x = "Covariates", y = "Count", fill = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 108, by = 10))+
  theme(legend.title = element_text(size = rel(1.2)))+
  theme(legend.text = element_text(face = "italic",family = "Arial",
                                   size = rel(1.2)))+
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

remove(dfm,dfmm)

#Create the input pao (Presence Absence Object) for Montecincla occupany analyses:
Monte=createPao(data=dethistMf,
               unitcov=sitecov,
               survcov=surveycov,
               title="Montecincla fairbankii",unitnames=sitenames)

#Create the input "Monte2" for checking any differences in results when using... 
#...non-standardised dataset as input:
Monte2=createPao(data=dethistMf,
                unitcov=sitecov2,
                survcov=surveycov,
                title  ="Montecincla fairbankii 2",unitnames=sitenames)

#Create the input "pao" for Sholicola occupany analyses:
Sholi=createPao(data=dethistSa,
                unitcov=sitecov,
                survcov=surveycov,
                title="Sholicola albiventris",unitnames=sitenames)

#Create the input "pao" for the two species co-occurence occupany analyses (MS):
Twosp=createPao(data=dethist2sp,
                unitcov=sitecov,
                survcov=surveycov,
                title="Two Species MS", unitnames=sitenames)

#Create the input "pao" for the two species co-occurence occupany analyses (MS):
Tsp=createPao(data=dethistTsp,
              unitcov=sitecov,
              survcov=surveycov,
              title="Two Species SM", unitnames=sitenames)

#To verify whether the format is pao, one can use any of these codes:
class(Monte)
class(Monte2)
class(Sholi)
class(Twosp)
class(Tsp)
#OR
is.pao(Monte)
is.pao(Monte2)
is.pao(Sholi)
is.pao(Twosp)
is.pao(Tsp)

#See the summary of the pao:
summary(Monte)
summary(Monte2)
summary(Sholi)
summary(Twosp)
summary(Tsp)
#So, the naive occupancy for M. fairbankii is approximately 0.85, for S. albiventris...
#...is 0.44, and for the co-occurrence of species is 0.85.
#Note that the survey covariate 'SURVEY' is automatically added, and refers to...
#.. the factor numbered 1 to 4 representing each repeated survey.
#See the structure of the pao:
str(Monte)
str(Monte2)
str(Sholi)
str(Twosp)
str(Tsp)



#### 3. Running models ####



##### 3.1 Montecincla fairbankii Single Species Single Season Occupancy Models ####

# 3.1.1 Run simplest model - the dol model = psi(.)p(.) - where the occupancy and...
#...detection is kept constant:
psi_dot_p_dot = occMod(model=list(psi~1,p~1), data=Monte, type="so")
#Type is "so" or "static occupancy" since it's a single season occupancy model.

#Extract model coefficients, with 95% confidence interval:
coef(object = psi_dot_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_dot, param = 'p', prob = 0.95)
#Note that RPresence converts psi (ψ) and p (both bound between 0 and 1) to... 
#...unconstrained beta coefficients which have no boundsa, to find the optimal...
#...solution. Therefore,the coeﬀicients are on the logit scale.

#Psi and p needs to be back-transformed to find the real probabilities (next step).
#Print the real estimates of occupancy and detection for this model:
print(unique(psi_dot_p_dot$real$psi))
print(unique(psi_dot_p_dot$real$p))
#Alternately, the 'fitted' function of RPresence can be used for this:
fitted(object = psi_dot_p_dot, param = "psi")
# Only the estimate for one site needed as the values are same for each site...
#...in this model:
print_one_site_estimates(mod = psi_dot_p_dot, site = 1)

#Look at the summary of the model:
summary(psi_dot_p_dot)

#Now, the rest of the models which were found to be significant during the...
#analyses in 2028 in the PRESENCE software will be run:

# 3.1.2 psi(Elevation), p(.)
psi_E_p_dot = occMod(model=list(psi~Elevation,p~1), data=Monte, type="so")

summary(psi_E_p_dot)

#Always check the beta coefficients to see if it contains 0, in which case, the...
#...model becomes statistically insignificant. It's a rule of thumb approach, so...
#...it may not hold biological significanc 100% of the time. But for model selection...
#...in occupancy this approach is used. A beta coefficient of 0 is equivalent to...
#...a probability of 0.5

#Extract model coefficients, with 95% confidence interval:
coef(object = psi_E_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_E_p_dot, param = 'p', prob = 0.95)

#Note the high standard error here for the Elevation model and the fact that...
#...confidence interval of beta coefficient contains 0. We will 
#...But, let's verify if this is the same with the non-standardised dataset:

psi_E2_p_dot = occMod(model=list(psi~Elevation,p~1), data=Monte2, type="so")
summary(psi_E2_p_dot)
coef(object = psi_E2_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_E2_p_dot, param = 'p', prob = 0.95)

#We see that the non-standardised data throws an SE order of magnitudes bigger...
#...than the estimate. So, its functionally useless to use non-standardised values...
#...of covariates with large values for occupancy analyses. We can check the same... 
#...for a model which contains continuous data of small values and range too.

# 3.1.3 psi(Lantana camara), p(.)
psi_Lc_p_dot = occMod(model=list(psi~Lantana_camara,p~1), data=Monte, type="so")
summary(psi_Lc_p_dot)
coef(object = psi_Lc_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Lc_p_dot, param = 'p', prob = 0.95)

#The SEs and confidence intervals were not estimated. So, this model is suspect.
#We'll discard the model. But we'll test it again by coding it as a factor to check...
#...if there is any difference between running as an integer ('int') or factor...
#...('fct') in RPresence

psi_Lc2_p_dot = occMod(model=list(psi~as.factor(Lantana_camara),p~1), data=Monte,
                       type="so")
summary(psi_Lc2_p_dot)
coef(object = psi_Lc2_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Lc2_p_dot, param = 'p', prob = 0.95)

#We see that there is no difference in the results. So, RPresence is smart enough...
#...to automatically consider 0s and 1s as factors (or equivalent) during the analyses. 

# 3.1.4 psi(Area), p(.)
psi_Ar_p_dot = occMod(model=list(psi~Area,p~1), data=Monte, type="so")
summary(psi_Ar_p_dot)
coef(object = psi_Ar_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Ar_p_dot, param = 'p', prob = 0.95)

#Let's run it using non-standardised date:
psi_Ar2_p_dot = occMod(model=list(psi~Area,p~1), data=Monte2, type="so")
summary(psi_Ar2_p_dot)
coef(object = psi_Ar2_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Ar2_p_dot, param = 'p', prob = 0.95)

#We see here that there that estimates are lower when using non-standardised...
#...values for area, but the associated SE and CIs are proportional in both models.
#So, there is likely no difference between using standardised or non-standardised...
#data.

# 3.1.5 psi(Rubus niveus), p(.)
psi_Rn_p_dot = occMod(model=list(psi~Rubus_niveus,p~1), data=Monte, type="so")
summary(psi_Rn_p_dot)
coef(object = psi_Rn_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Rn_p_dot, param = 'p', prob = 0.95)

# 3.1.6 psi(Elevation + Area), p(.)
psi_EPlusAr_p_dot = occMod(model=list(psi~Elevation+Area,p~1), data=Monte, type="so")
summary(psi_EPlusAr_p_dot)
coef(object = psi_EPlusAr_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_EPlusAr_p_dot, param = 'p', prob = 0.95)

# 3.1.7 psi(Area + Lantana camara), p(.)
psi_ArPlusLc_p_dot = occMod(model=list(psi~Area+Lantana_camara,p~1), data=Monte,
                            type="so")
summary(psi_ArPlusLc_p_dot)
coef(object = psi_ArPlusLc_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_ArPlusLc_p_dot, param = 'p', prob = 0.95)

# 3.1.8 psi(Elevation + Lantana camara), p(.)
psi_EPlusLc_p_dot = occMod(model=list(psi~Elevation+Lantana_camara,p~1), 
                           data=Monte, type="so")
summary(psi_EPlusLc_p_dot)
coef(object = psi_EPlusLc_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_EPlusLc_p_dot, param = 'p', prob = 0.95)

# 3.1.9 psi(Rubus niveus + Lantana camara), p(.)
psi_RnPlusLc_p_dot = occMod(model=list(psi~Rubus_niveus+Lantana_camara,p~1), 
                            data=Monte, type="so")
summary(psi_RnPlusLc_p_dot)
coef(object = psi_RnPlusLc_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_RnPlusLc_p_dot, param = 'p', prob = 0.95)

# 3.1.10 psi(Rubus niveus + Lantana camara), p(exp.^2)
#For this model, the automatically created categorical covariate "SURVEY" would...
#...need to be converted to a continuous covariate so that it can be used with...
#...the "I" function to model detection as function of an exponential of the...
#...survey number. So, convert it to numeric:
Monte$survcov$survey = as.numeric(Monte$survcov$SURVEY)

#You can see the structure of this covariate here:
list(Monte$survcov$survey)

psi_RnPlusLc_p_exp2 = occMod(model=list(psi~Rubus_niveus+Lantana_camara, 
                                        p~I(2^survey)), 
                             data=Monte, type="so")

#To verify that the exponential model covariate was created correctly, check:
head(psi_RnPlusLc_p_exp2[["data"]][["survcov"]][["p.I(2^survey)"]], 108)
#It should read 2, 4, 8 and 16 (times 27 for each number).

summary(psi_RnPlusLc_p_exp2)
coef(object = psi_RnPlusLc_p_exp2, param = 'psi', prob = 0.95)
coef(object = psi_RnPlusLc_p_exp2, param = 'p', prob = 0.95)

# 3.1.11 psi(Elevation), p(Warm)
psi_E_p_Wa = occMod(model=list(psi~Elevation, p~Warm),data=Monte, type="so")
summary(psi_E_p_Wa)
coef(object = psi_E_p_Wa, param = 'psi', prob = 0.95)
coef(object = psi_E_p_Wa, param = 'p', prob = 0.95)

# 3.1.12 psi(Area, Elevation, No road, Wet lotic, and R.niveus), p(Warm)
psi_PlusAENrWlRn_p_Wa = occMod(model=list
                               (psi~Area+Elevation+No_road+Wet_lotic+Rubus_niveus, 
                                 p~Warm), 
                    data=Monte, type="so")
summary(psi_PlusAENrWlRn_p_Wa)
coef(object = psi_PlusAENrWlRn_p_Wa, param = 'psi', prob = 0.95)
coef(object = psi_PlusAENrWlRn_p_Wa, param = 'p', prob = 0.95)

# 3.1.13 psi(Area, Elevation, Dirt road, Wet lotic, and R.niveus), p(Warm)
psi_PlusAEDrWlRn_p_Wa = occMod(model=list
                               (psi~Area+Elevation+Dirt_road+Wet_lotic+Rubus_niveus,
                                 p~Warm), 
                               data=Monte, type="so")
summary(psi_PlusAEDrWlRn_p_Wa)
coef(object = psi_PlusAEDrWlRn_p_Wa, param = 'psi', prob = 0.95)
coef(object = psi_PlusAEDrWlRn_p_Wa, param = 'p', prob = 0.95)

# 3.1.14 psi(Area, Elevation, No road, Wet lotic, and R.niveus), p(exp.^2, Warm)
psi_PlusAENrWlRn_p_exp2PlusWa = occMod(model=list
                            (psi~Area+Elevation+No_road+Wet_lotic+Rubus_niveus, 
                                          p~I(2^survey)+Warm), 
                               data=Monte, type="so")
summary(psi_PlusAENrWlRn_p_exp2PlusWa)
coef(object = psi_PlusAENrWlRn_p_exp2PlusWa, param = 'psi', prob = 0.95)
coef(object = psi_PlusAENrWlRn_p_exp2PlusWa, param = 'p', prob = 0.95)

# 3.1.15 psi(Area, Elevation, Dirt road, Wet lotic, and R.niveus), p(exp.^2, Warm)
psi_PlusAEDrWlRn_p_exp2PlusWa = occMod(model=list
                            (psi~Area+Elevation+Dirt_road+Wet_lotic+Rubus_niveus, 
                                        p~I(2^survey)+Warm), 
                                        data=Monte, type="so")
summary(psi_PlusAEDrWlRn_p_exp2PlusWa)
coef(object = psi_PlusAEDrWlRn_p_exp2PlusWa, param = 'psi', prob = 0.95)
coef(object = psi_PlusAEDrWlRn_p_exp2PlusWa, param = 'p', prob = 0.95)

# 3.1.16 psi(Area, Elevation, No road, Wet lotic, and R.niveus), p(exp.^3, Warm)
psi_PlusAENrWlRn_p_exp3PlusWa = occMod(model=list
                             (psi~Area+Elevation+No_road+Wet_lotic+Rubus_niveus, 
                                        p~I(3^survey)+Warm), 
                                        data=Monte, type="so")
summary(psi_PlusAENrWlRn_p_exp3PlusWa)
coef(object = psi_PlusAENrWlRn_p_exp3PlusWa, param = 'psi', prob = 0.95)
coef(object = psi_PlusAENrWlRn_p_exp3PlusWa, param = 'p', prob = 0.95)

# 3.1.17 psi(Area, Elevation, No road, Wet lotic, and R.niveus), p(exp.^4, Warm)
psi_PlusAENrWlRn_p_exp4PlusWa = occMod(model=list
                            (psi~Area+Elevation+No_road+Wet_lotic+Rubus_niveus, 
                                         p~I(4^survey)+Warm), 
                                       data=Monte, type="so")
summary(psi_PlusAENrWlRn_p_exp4PlusWa)
coef(object = psi_PlusAENrWlRn_p_exp4PlusWa, param = 'psi', prob = 0.95)
coef(object = psi_PlusAENrWlRn_p_exp4PlusWa, param = 'p', prob = 0.95)

# 3.1.18 psi(Area, Elevation, No road, Wet lotic, and R.niveus), p(exp.^10, Warm)
psi_PlusAENrWlRn_p_exp10PlusWa = occMod(model=list
                            (psi~Area+Elevation+No_road+Wet_lotic+Rubus_niveus, 
                                         p~I(10^survey)+Warm), 
                                       data=Monte, type="so")
summary(psi_PlusAENrWlRn_p_exp10PlusWa)
coef(object = psi_PlusAENrWlRn_p_exp10PlusWa, param = 'psi', prob = 0.95)
coef(object = psi_PlusAENrWlRn_p_exp10PlusWa, param = 'p', prob = 0.95)

# 3.1.19 psi(Area, Elevation, No road, Wet lotic, R.niveus, and L. camara), p(exp.^2)
psi_PlusAENrWlRnLc_p_exp2 = occMod(model=list
                (psi~Area+Elevation+No_road+Wet_lotic+Rubus_niveus+Lantana_camara, 
                                   p~I(2^survey)), 
                                   data=Monte, type="so")
summary(psi_PlusAENrWlRnLc_p_exp2)
coef(object = psi_PlusAENrWlRnLc_p_exp2, param = 'psi', prob = 0.95)
coef(object = psi_PlusAENrWlRnLc_p_exp2, param = 'p', prob = 0.95)

#Collect the results of each model into a single list variable, "modelsMf1":
models_Mf1 = list(psi_dot_p_dot, psi_E_p_dot, psi_Lc_p_dot, psi_Ar_p_dot, psi_Rn_p_dot, 
                 psi_EPlusAr_p_dot, psi_ArPlusLc_p_dot, psi_EPlusLc_p_dot,
                 psi_RnPlusLc_p_dot, psi_RnPlusLc_p_exp2, psi_E_p_Wa, psi_PlusAENrWlRn_p_Wa, 
                 psi_PlusAEDrWlRn_p_Wa, psi_PlusAENrWlRn_p_exp2PlusWa,
                 psi_PlusAEDrWlRn_p_exp2PlusWa, psi_PlusAENrWlRn_p_exp3PlusWa, 
                 psi_PlusAENrWlRn_p_exp4PlusWa, psi_PlusAENrWlRn_p_exp10PlusWa, 
                 psi_PlusAENrWlRnLc_p_exp2)

#Create AIC table of the model results:
resultsMf1<-createAicTable(models_Mf1, use.aicc = TRUE)

#Print the results summary table:
cat('Montecincla fairbankii\nSingle Species Models')
print(resultsMf1$table)

#These are all the  models found to be top models in in the 2018 model runs.
#However, there has been considerable re-ordering in the rankings. This may be...
#...due to difference in values when z-standardising, and improved modelling...
#...capability of PRESENCE version used at the time compared to the RPresence used now.
#Since there are only 27 sites, the rule of thumb recommendation of 1 covariate...
#...per model for every 30 or 15 sites can be observed to avoid overfitting. 
#This means that the number of site-specific covariates should be 2 at most, 
#... on the conservative end of the spectrum to avoid model overfitting. However,...
#...since there are 4 temporal replicates, the number of survey-specific can be...
#...3 or 4 (since there are 4*27 observations). Observing these constraints,...
#...more models will be run to see if there are models with less AICc.

# 3.1.20 psi(Aspect), p(.)
psi_As_p_dot = occMod(model=list(psi~Aspect,p~1), data=Monte, type="so")
summary(psi_As_p_dot)
coef(object = psi_As_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_As_p_dot, param = 'p', prob = 0.95)

# 3.1.21 psi(Slope), p(.)
psi_Sl_p_dot = occMod(model=list(psi~Slope,p~1), data=Monte, type="so")
summary(psi_Sl_p_dot)
coef(object = psi_Sl_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Sl_p_dot, param = 'p', prob = 0.95)

# 3.1.22 psi(Canopy height), p(.)
psi_Ch_p_dot = occMod(model=list(psi~Canopy_height,p~1), data=Monte, type="so")
summary(psi_Ch_p_dot)
coef(object = psi_Ch_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Ch_p_dot, param = 'p', prob = 0.95)

# 3.1.23 psi(Understorey height), p(.)
psi_Uh_p_dot = occMod(model=list(psi~Understorey_height,p~1), data=Monte, type="so")
summary(psi_Uh_p_dot)
coef(object = psi_Uh_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Uh_p_dot, param = 'p', prob = 0.95)

# 3.1.24 psi(Visibility), p(.)
psi_V_p_dot = occMod(model=list(psi~Visibility,p~1), data=Monte, type="so")
summary(psi_V_p_dot)
coef(object = psi_V_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_V_p_dot, param = 'p', prob = 0.95)

# 3.1.25 psi(Burn), p(.)
psi_B_p_dot = occMod(model=list(psi~Burn,p~1), data=Monte, type="so")
summary(psi_B_p_dot)
coef(object = psi_B_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_B_p_dot, param = 'p', prob = 0.95)

# 3.1.26 psi(Metalled road), p(.)
psi_Mr_p_dot = occMod(model=list(psi~Metalled_road,p~1), data=Monte, type="so")
summary(psi_Mr_p_dot)
coef(object = psi_Mr_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Mr_p_dot, param = 'p', prob = 0.95)

# 3.1.27 psi(Dirt road), p(.)
psi_Dr_p_dot = occMod(model=list(psi~Dirt_road,p~1), data=Monte, type="so")
summary(psi_Dr_p_dot)
coef(object = psi_Dr_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Dr_p_dot, param = 'p', prob = 0.95)

# 3.1.28 psi(No road), p(.)
psi_Nr_p_dot = occMod(model=list(psi~No_road,p~1), data=Monte, type="so")
summary(psi_Nr_p_dot)
coef(object = psi_Nr_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_dot, param = 'p', prob = 0.95)

# 3.1.29 psi(Lentic), p(.)
psi_L_p_dot = occMod(model=list(psi~Lentic,p~1), data=Monte, type="so")
summary(psi_L_p_dot)
coef(object = psi_L_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_L_p_dot, param = 'p', prob = 0.95)

# 3.1.30 psi(Wet lotic), p(.)
psi_Wl_p_dot = occMod(model=list(psi~Wet_lotic,p~1), data=Monte, type="so")
summary(psi_Wl_p_dot)
coef(object = psi_Wl_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Wl_p_dot, param = 'p', prob = 0.95)

# 3.1.31 psi(Dry lotic), p(.)
psi_Dl_p_dot = occMod(model=list(psi~Dry_lotic,p~1), data=Monte, type="so")
summary(psi_Dl_p_dot)
coef(object = psi_Dl_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_Dl_p_dot, param = 'p', prob = 0.95)

#Collect the results of each model into a single list variable, "modelsMf2":

models_Mf2 = list(psi_dot_p_dot, psi_E_p_dot, psi_Lc_p_dot, psi_Ar_p_dot, psi_Rn_p_dot, 
                 psi_EPlusAr_p_dot, psi_ArPlusLc_p_dot, psi_EPlusLc_p_dot,
                 psi_RnPlusLc_p_dot, psi_RnPlusLc_p_exp2, psi_E_p_Wa, psi_PlusAENrWlRn_p_Wa, 
                 psi_PlusAEDrWlRn_p_Wa, psi_PlusAENrWlRn_p_exp2PlusWa,
                 psi_PlusAEDrWlRn_p_exp2PlusWa, psi_PlusAENrWlRn_p_exp3PlusWa, 
                 psi_PlusAENrWlRn_p_exp4PlusWa, psi_PlusAENrWlRn_p_exp10PlusWa, 
                 psi_PlusAENrWlRnLc_p_exp2, psi_As_p_dot, psi_Sl_p_dot, psi_Ch_p_dot,
                 psi_Uh_p_dot, psi_V_p_dot, psi_B_p_dot, psi_Mr_p_dot,
                 psi_Dr_p_dot, psi_Nr_p_dot, psi_L_p_dot, psi_Wl_p_dot,
                 psi_Dl_p_dot)

#Create AIC table of the model results:
resultsMf2<-createAicTable(models_Mf2, use.aicc = TRUE)

#Print the results summary table:
cat('Montecincla fairbankii\nSingle Species Models')
print(resultsMf2$table)

#From the covariates showing up in the top significant models, build a simple...
#...interactive model to test:
# 3.1.32 psi(Elevation * Area), p(.)
psi_ECrossAr_p_dot = occMod(model=list(psi~Elevation*Area,p~1), data=Monte, type="so")
summary(psi_ECrossAr_p_dot)
coef(object = psi_ECrossAr_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_ECrossAr_p_dot, param = 'p', prob = 0.95)

#We see here than the number of parameters (k) for psi is 4 in the case of even...
#...a simple interactive model. The k will increase if there are more than two...
#...categories in a categorical variable used in an interactive model. So, it's...
#...not suitable given the sample size. 

#Check an simple additive model:
# 3.1.33 psi(Dirt road + No road), p(.)
psi_DrPlusNr_p_dot = occMod(model=list(psi~Dirt_road+No_road,p~1), data=Monte, type="so")
summary(psi_DrPlusNr_p_dot)
coef(object = psi_DrPlusNr_p_dot, param = 'psi', prob = 0.95)
coef(object = psi_DrPlusNr_p_dot, param = 'p', prob = 0.95)

#We see that there are three parameters for psi, which we need to avoid, given...
#...smaller sample size. So, we will not include additive models either.

#Check a simple model containing exponents:
# 3.1.34 psi(Area), p(exp.^2)
psi_Ar_p_exp2 = occMod(model=list(psi~Area,p~I(2^survey)),data=Monte, type="so")
summary(psi_Ar_p_exp2)
coef(object = psi_Ar_p_exp2, param = 'psi', prob = 0.95)
coef(object = psi_Ar_p_exp2, param = 'p', prob = 0.95)

#We see that there are two parameters for psi and two for p, which are acceptable.
#So, we will include models containing exponents.

#Check a simple polynomial model:
# 3.1.35 psi(Area), p(survey+survey^2)
psi_Ar_p_SurPoly = occMod(model=list(psi~Area,p~survey+I(survey^2)), 
                                       data=Monte, type="so")
summary(psi_Ar_p_SurPoly)
coef(object = psi_Ar_p_SurPoly, param = 'psi', prob = 0.95)
coef(object = psi_Ar_p_SurPoly, param = 'p', prob = 0.95)
#Three parameters for p is acceptable for the given sample size.

# 3.1.36 psi(Area), p(survey+survey^2+Warm)
psi_Ar_p_SurPolyPlusWa = occMod(model=list(psi~Area,p~survey+I(survey^2)+Warm), 
                         data=Monte, type="so")
summary(psi_Ar_p_SurPolyPlusWa)
coef(object = psi_Ar_p_SurPolyPlusWa, param = 'psi', prob = 0.95)
coef(object = psi_Ar_p_SurPolyPlusWa, param = 'p', prob = 0.95)
#Four parameters for p is also reluctantly acceptable for the given sample size.

# 3.1.37 psi(.), p(Before noon + Cool)
psi_dot_p_BnPlusC = occMod(model=list(psi~1,p~BefNoon+Cool),data=Monte, type="so")
summary(psi_dot_p_BnPlusC)
coef(object = psi_dot_p_BnPlusC, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_BnPlusC, param = 'p', prob = 0.95)

# 3.1.38 psi(.), p(Before noon + Cool + Windy)
psi_dot_p_PlusBnCWi = occMod(model=list(psi~1,p~BefNoon+Cool+Windy), 
                           data=Monte, type="so")
summary(psi_dot_p_PlusBnCWi)
coef(object = psi_dot_p_PlusBnCWi, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_PlusBnCWi, param = 'p', prob = 0.95)

# 3.1.39 psi(.), p(Cool + Windy + Overcast)
psi_dot_p_PlusCWiO = occMod(model=list(psi~1,p~Cool+Windy+Ovrcst), 
                             data=Monte, type="so")
summary(psi_dot_p_PlusCWiO)
coef(object = psi_dot_p_PlusCWiO, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_PlusCWiO, param = 'p', prob = 0.95)

# 3.1.40 psi(Elevation), p(Before noon + Cool)
psi_E_p_BnPlusC = occMod(model=list(psi~Elevation,p~BefNoon+Cool), 
                           data=Monte, type="so")
summary(psi_E_p_BnPlusC)
coef(object = psi_E_p_BnPlusC, param = 'psi', prob = 0.95)
coef(object = psi_E_p_BnPlusC, param = 'p', prob = 0.95)

# 3.1.41 psi(Elevation), p(Before noon + Cool + Windy)
psi_E_p_PlusBnCWi = occMod(model=list(psi~Elevation,p~BefNoon+Cool+Windy), 
                             data=Monte, type="so")
summary(psi_E_p_PlusBnCWi)
coef(object = psi_E_p_PlusBnCWi, param = 'psi', prob = 0.95)
coef(object = psi_E_p_PlusBnCWi, param = 'p', prob = 0.95)

# 3.1.42 psi(Elevation), p(Cool + Windy + Overcast)
psi_E_p_PlusCWiO = occMod(model=list(psi~Elevation,p~Cool+Windy+Ovrcst), 
                            data=Monte, type="so")
summary(psi_E_p_PlusCWiO)
coef(object = psi_E_p_PlusCWiO, param = 'psi', prob = 0.95)
coef(object = psi_E_p_PlusCWiO, param = 'p', prob = 0.95)

# 3.1.43 psi(.), p(Before Noon)
psi_dot_p_Bn = occMod(model=list(psi~1,p~BefNoon),data=Monte, type="so")
summary(psi_dot_p_Bn)
coef(object = psi_dot_p_Bn, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_Bn, param = 'p', prob = 0.95)

# 3.1.44 psi(.), p(After noon)
psi_dot_p_An = occMod(model=list(psi~1,p~AftNoon),data=Monte, type="so")
summary(psi_dot_p_An)
coef(object = psi_dot_p_An, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_An, param = 'p', prob = 0.95)

# 3.1.45 psi(.), p(Clear sky)
psi_dot_p_Cs = occMod(model=list(psi~1,p~ClrSky),data=Monte, type="so")
summary(psi_dot_p_Cs)
coef(object = psi_dot_p_Cs, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_Cs, param = 'p', prob = 0.95)

# 3.1.46 psi(.), p(Overcast)
psi_dot_p_O = occMod(model=list(psi~1,p~Ovrcst),data=Monte, type="so")
summary(psi_dot_p_O)
coef(object = psi_dot_p_O, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_O, param = 'p', prob = 0.95)

# 3.1.47 psi(.), p(Sunny)
psi_dot_p_Sun = occMod(model=list(psi~1,p~Sunny),data=Monte, type="so")
summary(psi_dot_p_Sun)
coef(object = psi_dot_p_Sun, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_Sun, param = 'p', prob = 0.95)

# 3.1.48 psi(.), p(Windy)
psi_dot_p_Wi = occMod(model=list(psi~1,p~Windy),data=Monte, type="so")
summary(psi_dot_p_Wi)
coef(object = psi_dot_p_Wi, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_Wi, param = 'p', prob = 0.95)

# 3.1.49 psi(.), p(Warm)
psi_dot_p_Wa = occMod(model=list(psi~1,p~Warm),data=Monte, type="so")
summary(psi_dot_p_Wa)
coef(object = psi_dot_p_Wa, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_Wa, param = 'p', prob = 0.95)

# 3.1.50 psi(.), p(Neutral)
psi_dot_p_N = occMod(model=list(psi~1,p~Neutral),data=Monte, type="so")
summary(psi_dot_p_N)
coef(object = psi_dot_p_N, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_N, param = 'p', prob = 0.95)

# 3.1.51 psi(.), p(Cool)
psi_dot_p_C = occMod(model=list(psi~1,p~Cool),data=Monte, type="so")
summary(psi_dot_p_C)
coef(object = psi_dot_p_C, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_C, param = 'p', prob = 0.95)

# 3.1.52 psi(.), p(Survey)
psi_dot_p_Sur = occMod(model=list(psi~1,p~survey),data=Monte, type="so")
summary(psi_dot_p_Sur)
coef(object = psi_dot_p_Sur, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_Sur, param = 'p', prob = 0.95)

#Collect the results of each model into a single list variable, "models_Mf3":

models_Mf3 = list(psi_dot_p_dot, psi_E_p_dot, psi_Lc_p_dot, psi_Ar_p_dot, 
                  psi_Rn_p_dot, psi_E_p_Wa, psi_As_p_dot, psi_Sl_p_dot,
                  psi_Ch_p_dot, psi_Uh_p_dot, psi_V_p_dot, psi_B_p_dot,
                  psi_Mr_p_dot, psi_Dr_p_dot, psi_Nr_p_dot, psi_L_p_dot,
                  psi_Wl_p_dot, psi_Dl_p_dot, psi_Ar_p_exp2, psi_Ar_p_SurPoly,
                  psi_Ar_p_SurPolyPlusWa, psi_dot_p_BnPlusC, psi_dot_p_PlusBnCWi,
                  psi_dot_p_PlusCWiO, psi_E_p_BnPlusC, psi_E_p_PlusBnCWi,
                  psi_E_p_PlusCWiO, psi_dot_p_Bn, psi_dot_p_An, psi_dot_p_Cs,
                  psi_dot_p_O, psi_dot_p_Sun, psi_dot_p_Wi, psi_dot_p_Wa,
                  psi_dot_p_N, psi_dot_p_C, psi_dot_p_Sur)

#Create AIC table of the model results:
resultsMf3<-createAicTable(models_Mf3, use.aicc = TRUE)

#Print the results summary table:
cat('Montecincla fairbankii\nSingle Species Models')
print(resultsMf3$table)

#Here the model "psi(Area)p(survey+I(survey^2)+Warm)" has DAICc greater than 7.
#So, it can be discarded. You can also choose not to discard it and keep it to...
#...check if it falls under DQAICc of 7 (spoiler alert: it doesn't). Among others,...
#...more models with covariates appearing in the top models can be tested:

# 3.1.53 psi(No_road), p(Windy)
psi_Nr_p_Wi = occMod(model=list(psi~No_road,p~Windy),data=Monte, type="so")
summary(psi_Nr_p_Wi)
coef(object = psi_Nr_p_Wi, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_Wi, param = 'p', prob = 0.95)

# 3.1.54 psi(Dirt_road), p(Windy)
psi_Dr_p_Wi = occMod(model=list(psi~Dirt_road,p~Windy),data=Monte, type="so")
summary(psi_Dr_p_Wi)
coef(object = psi_Dr_p_Wi, param = 'psi', prob = 0.95)
coef(object = psi_Dr_p_Wi, param = 'p', prob = 0.95)

# 3.1.55 psi(No_road), p(Windy+Survey)
psi_Nr_p_WiPlusSur = occMod(model=list(psi~No_road,p~Windy+survey),
                            data=Monte, type="so")
summary(psi_Nr_p_WiPlusSur)
coef(object = psi_Nr_p_WiPlusSur, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_WiPlusSur, param = 'p', prob = 0.95)

# 3.1.56 psi(Dirt_road), p(Survey)
psi_Dr_p_Sur = occMod(model=list(psi~Dirt_road,p~survey),
                            data=Monte, type="so")
summary(psi_Dr_p_Sur)
coef(object = psi_Dr_p_Sur, param = 'psi', prob = 0.95)
coef(object = psi_Dr_p_Sur, param = 'p', prob = 0.95)

# 3.1.57 psi(Slope), p(Windy+Survey)
psi_Sl_p_WiPlusSur = occMod(model=list(psi~Slope,p~Windy+survey),
                            data=Monte, type="so")
summary(psi_Sl_p_WiPlusSur)
coef(object = psi_Sl_p_WiPlusSur, param = 'psi', prob = 0.95)
coef(object = psi_Sl_p_WiPlusSur, param = 'p', prob = 0.95)

# 3.1.58 psi(Elevation), p(Windy+Survey)
psi_E_p_WiPlusSur = occMod(model=list(psi~Elevation,p~Windy+survey),
                           data=Monte, type="so")
summary(psi_E_p_WiPlusSur)
coef(object = psi_E_p_WiPlusSur, param = 'psi', prob = 0.95)
coef(object = psi_E_p_WiPlusSur, param = 'p', prob = 0.95)

# 3.1.59 psi(Visibility), p(Windy+Survey)
psi_V_p_WiPlusSur = occMod(model=list(psi~Visibility,p~Windy+survey),
                           data=Monte, type="so")
summary(psi_V_p_WiPlusSur)
coef(object = psi_V_p_WiPlusSur, param = 'psi', prob = 0.95)
coef(object = psi_V_p_WiPlusSur, param = 'p', prob = 0.95)

# 3.1.60 psi(Dry lotic), p(Windy+Survey)
psi_Dl_p_WiPlusSur = occMod(model=list(psi~Dry_lotic,p~Windy+survey),
                           data=Monte, type="so")
summary(psi_Dl_p_WiPlusSur)
coef(object = psi_Dl_p_WiPlusSur, param = 'psi', prob = 0.95)
coef(object = psi_Dl_p_WiPlusSur, param = 'p', prob = 0.95)

# 3.1.61 psi(Area), p(Windy+Survey)
psi_Ar_p_WiPlusSur = occMod(model=list(psi~Area,p~Windy+survey),
                            data=Monte, type="so")
summary(psi_Ar_p_WiPlusSur)
coef(object = psi_Ar_p_WiPlusSur, param = 'psi', prob = 0.95)
coef(object = psi_Ar_p_WiPlusSur, param = 'p', prob = 0.95)

# 3.1.62 psi(Understorey height), p(Windy+Survey)
psi_Uh_p_WiPlusSur = occMod(model=list(psi~Understorey_height,p~Windy+survey),
                           data=Monte, type="so")
summary(psi_Uh_p_WiPlusSur)
coef(object = psi_Uh_p_WiPlusSur, param = 'psi', prob = 0.95)
coef(object = psi_Uh_p_WiPlusSur, param = 'p', prob = 0.95)

# 3.1.63 psi(Rubus niveus), p(Windy+Survey)
psi_Rn_p_WiPlusSur = occMod(model=list(psi~Rubus_niveus,p~Windy+survey),
                            data=Monte, type="so")
summary(psi_Rn_p_WiPlusSur)
coef(object = psi_Rn_p_WiPlusSur, param = 'psi', prob = 0.95)
coef(object = psi_Rn_p_WiPlusSur, param = 'p', prob = 0.95)

# 3.1.64 psi(Aspect), p(Windy+Survey)
psi_As_p_WiPlusSur = occMod(model=list(psi~Aspect,p~Windy+survey),
                            data=Monte, type="so")
summary(psi_As_p_WiPlusSur)
coef(object = psi_As_p_WiPlusSur, param = 'psi', prob = 0.95)
coef(object = psi_As_p_WiPlusSur, param = 'p', prob = 0.95)

# 3.1.65 psi(No road), p(Before noon)
psi_Nr_p_Bn = occMod(model=list(psi~No_road,p~BefNoon),data=Monte, type="so")
summary(psi_Nr_p_Bn)
coef(object = psi_Nr_p_Bn, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_Bn, param = 'p', prob = 0.95)

# 3.1.66 psi(No road), p(Windy + Before noon)
psi_Nr_p_WiPlusBn = occMod(model=list(psi~No_road,p~Windy+BefNoon),data=Monte,
                           type="so")
summary(psi_Nr_p_WiPlusBn)
coef(object = psi_Nr_p_WiPlusBn, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_WiPlusBn, param = 'p', prob = 0.95)

# 3.1.67 psi(No road), p(Windy + Before noon)
psi_Nr_p_WiPlusBn = occMod(model=list(psi~No_road,p~Windy+BefNoon),data=Monte,
                           type="so")
summary(psi_Nr_p_WiPlusBn)
coef(object = psi_Nr_p_WiPlusBn, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_WiPlusBn, param = 'p', prob = 0.95)

# 3.1.68 psi(No road), p(Clear sky + Before noon)
psi_Nr_p_CsPlusBn = occMod(model=list(psi~No_road,p~ClrSky+BefNoon),data=Monte,
                           type="so")
summary(psi_Nr_p_CsPlusBn)
coef(object = psi_Nr_p_CsPlusBn, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_CsPlusBn, param = 'p', prob = 0.95)

# 3.1.69 psi(Slope), p(Clear sky + Before noon)
psi_Sl_p_CsPlusBn = occMod(model=list(psi~Slope,p~ClrSky+BefNoon),data=Monte,
                           type="so")
summary(psi_Sl_p_CsPlusBn)
coef(object = psi_Sl_p_CsPlusBn, param = 'psi', prob = 0.95)
coef(object = psi_Sl_p_CsPlusBn, param = 'p', prob = 0.95)

# 3.1.70 psi(Elevation), p(Clear sky + Before noon)
psi_E_p_CsPlusBn = occMod(model=list(psi~Elevation,p~ClrSky+BefNoon),data=Monte,
                          type="so")
summary(psi_E_p_CsPlusBn)
coef(object = psi_E_p_CsPlusBn, param = 'psi', prob = 0.95)
coef(object = psi_E_p_CsPlusBn, param = 'p', prob = 0.95)

# 3.1.71 psi(Dirt road), p(Clear sky + Before noon)
psi_Dr_p_CsPlusBn = occMod(model=list(psi~Dirt_road,p~ClrSky+BefNoon),data=Monte,
                           type="so")
summary(psi_Dr_p_CsPlusBn)
coef(object = psi_Dr_p_CsPlusBn, param = 'psi', prob = 0.95)
coef(object = psi_Dr_p_CsPlusBn, param = 'p', prob = 0.95)

# 3.1.72 psi(Dry lotic), p(Clear sky + Before noon)
psi_Dl_p_CsPlusBn = occMod(model=list(psi~Dry_lotic,p~ClrSky+BefNoon),data=Monte,
                           type="so")
summary(psi_Dl_p_CsPlusBn)
coef(object = psi_Dl_p_CsPlusBn, param = 'psi', prob = 0.95)
coef(object = psi_Dl_p_CsPlusBn, param = 'p', prob = 0.95)

# 3.1.73 psi(Visibility), p(Clear sky + Before noon)
psi_V_p_CsPlusBn = occMod(model=list(psi~Visibility,p~ClrSky+BefNoon),data=Monte,
                          type="so")
summary(psi_V_p_CsPlusBn)
coef(object = psi_V_p_CsPlusBn, param = 'psi', prob = 0.95)
coef(object = psi_V_p_CsPlusBn, param = 'p', prob = 0.95)

# 3.1.74 psi(Area), p(Clear sky + Before noon)
psi_Ar_p_CsPlusBn = occMod(model=list(psi~Area,p~ClrSky+BefNoon),data=Monte,
                           type="so")
summary(psi_Ar_p_CsPlusBn)
coef(object = psi_Ar_p_CsPlusBn, param = 'psi', prob = 0.95)
coef(object = psi_Ar_p_CsPlusBn, param = 'p', prob = 0.95)

# 3.1.75 psi(Area), p(Windy + survey + Before noon)
psi_Ar_p_PlusWiSurBn = occMod(model=list(psi~Area,p~Windy+survey+BefNoon),
                              data=Monte, type="so")
summary(psi_Ar_p_PlusWiSurBn)
coef(object = psi_Ar_p_PlusWiSurBn, param = 'psi', prob = 0.95)
coef(object = psi_Ar_p_PlusWiSurBn, param = 'p', prob = 0.95)

# 3.1.76 psi(No road), p(Windy + survey + Before noon)
psi_Nr_p_PlusWiSurBn = occMod(model=list(psi~No_road,p~Windy+survey+BefNoon),
                              data=Monte, type="so")
summary(psi_Nr_p_PlusWiSurBn)
coef(object = psi_Nr_p_PlusWiSurBn, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_PlusWiSurBn, param = 'p', prob = 0.95)

# 3.1.77 psi(Elevation), p(Windy + survey + Before noon)
psi_E_p_PlusWiSurBn = occMod(model=list(psi~Elevation,p~Windy+survey+BefNoon),
                              data=Monte, type="so")
summary(psi_E_p_PlusWiSurBn)
coef(object = psi_E_p_PlusWiSurBn, param = 'psi', prob = 0.95)
coef(object = psi_E_p_PlusWiSurBn, param = 'p', prob = 0.95)

# 3.1.78 psi(Rubus niveus), p(Windy + survey + Before noon)
psi_Rn_p_PlusWiSurBn = occMod(model=list(psi~Rubus_niveus,p~Windy+survey+BefNoon),
                             data=Monte, type="so")
summary(psi_Rn_p_PlusWiSurBn)
coef(object = psi_Rn_p_PlusWiSurBn, param = 'psi', prob = 0.95)
coef(object = psi_Rn_p_PlusWiSurBn, param = 'p', prob = 0.95)

# 3.1.79 psi(Slope), p(Windy + survey + Before noon)
psi_Sl_p_PlusWiSurBn = occMod(model=list(psi~Slope,p~Windy+survey+BefNoon),
                              data=Monte, type="so")
summary(psi_Sl_p_PlusWiSurBn)
coef(object = psi_Sl_p_PlusWiSurBn, param = 'psi', prob = 0.95)
coef(object = psi_Sl_p_PlusWiSurBn, param = 'p', prob = 0.95)

# 3.1.80 psi(Dry lotic), p(Windy + survey + Before noon)
psi_Dl_p_PlusWiSurBn = occMod(model=list(psi~Dry_lotic,p~Windy+survey+BefNoon),
                              data=Monte, type="so")
summary(psi_Dl_p_PlusWiSurBn)
coef(object = psi_Dl_p_PlusWiSurBn, param = 'psi', prob = 0.95)
coef(object = psi_Dl_p_PlusWiSurBn, param = 'p', prob = 0.95)

# 3.1.81 psi(Visibility), p(Windy + survey + Before noon)
psi_V_p_PlusWiSurBn = occMod(model=list(psi~Visibility,p~Windy+survey+BefNoon),
                              data=Monte, type="so")
summary(psi_V_p_PlusWiSurBn)
coef(object = psi_V_p_PlusWiSurBn, param = 'psi', prob = 0.95)
coef(object = psi_V_p_PlusWiSurBn, param = 'p', prob = 0.95)

# 3.1.82 psi(Aspect), p(Windy + survey + Before noon)
psi_As_p_PlusWiSurBn = occMod(model=list(psi~Aspect,p~Windy+survey+BefNoon),
                             data=Monte, type="so")
summary(psi_As_p_PlusWiSurBn)
coef(object = psi_As_p_PlusWiSurBn, param = 'psi', prob = 0.95)
coef(object = psi_As_p_PlusWiSurBn, param = 'p', prob = 0.95)

# 3.1.83 psi(Area), p(Windy + survey + After noon)
psi_Ar_p_PlusWiSurAn = occMod(model=list(psi~Area,p~Windy+survey+AftNoon),
                              data=Monte, type="so")
summary(psi_Ar_p_PlusWiSurAn)
coef(object = psi_Ar_p_PlusWiSurAn, param = 'psi', prob = 0.95)
coef(object = psi_Ar_p_PlusWiSurAn, param = 'p', prob = 0.95)

# 3.1.84 psi(No road), p(Windy + survey + After noon)
psi_Nr_p_PlusWiSurAn = occMod(model=list(psi~No_road,p~Windy+survey+AftNoon),
                              data=Monte, type="so")
summary(psi_Nr_p_PlusWiSurAn)
coef(object = psi_Nr_p_PlusWiSurAn, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_PlusWiSurAn, param = 'p', prob = 0.95)

# 3.1.85 psi(Elevation), p(Windy + survey + After noon)
psi_E_p_PlusWiSurAn = occMod(model=list(psi~Elevation,p~Windy+survey+AftNoon),
                             data=Monte, type="so")
summary(psi_E_p_PlusWiSurAn)
coef(object = psi_E_p_PlusWiSurAn, param = 'psi', prob = 0.95)
coef(object = psi_E_p_PlusWiSurAn, param = 'p', prob = 0.95)

# 3.1.86 psi(Rubus niveus), p(Windy + survey + After noon)
psi_Rn_p_PlusWiSurAn = occMod(model=list(psi~Rubus_niveus,p~Windy+survey+AftNoon),
                              data=Monte, type="so")
summary(psi_Rn_p_PlusWiSurAn)
coef(object = psi_Rn_p_PlusWiSurAn, param = 'psi', prob = 0.95)
coef(object = psi_Rn_p_PlusWiSurAn, param = 'p', prob = 0.95)

# 3.1.87 psi(Slope), p(Windy + survey + After noon)
psi_Sl_p_PlusWiSurAn = occMod(model=list(psi~Slope,p~Windy+survey+AftNoon),
                              data=Monte, type="so")
summary(psi_Sl_p_PlusWiSurAn)
coef(object = psi_Sl_p_PlusWiSurAn, param = 'psi', prob = 0.95)
coef(object = psi_Sl_p_PlusWiSurAn, param = 'p', prob = 0.95)

# 3.1.88 psi(Dry lotic), p(Windy + survey + After noon)
psi_Dl_p_PlusWiSurAn = occMod(model=list(psi~Dry_lotic,p~Windy+survey+AftNoon),
                              data=Monte, type="so")
summary(psi_Dl_p_PlusWiSurAn)
coef(object = psi_Dl_p_PlusWiSurAn, param = 'psi', prob = 0.95)
coef(object = psi_Dl_p_PlusWiSurAn, param = 'p', prob = 0.95)

# 3.1.89 psi(Visibility), p(Windy + survey + After noon)
psi_V_p_PlusWiSurAn = occMod(model=list(psi~Visibility,p~Windy+survey+AftNoon),
                             data=Monte, type="so")
summary(psi_V_p_PlusWiSurAn)
coef(object = psi_V_p_PlusWiSurAn, param = 'psi', prob = 0.95)
coef(object = psi_V_p_PlusWiSurAn, param = 'p', prob = 0.95)

# 3.1.90 psi(Aspect), p(Windy + survey + After noon)
psi_As_p_PlusWiSurAn = occMod(model=list(psi~Aspect,p~Windy+survey+AftNoon),
                              data=Monte, type="so")
summary(psi_As_p_PlusWiSurAn)
coef(object = psi_As_p_PlusWiSurAn, param = 'psi', prob = 0.95)
coef(object = psi_As_p_PlusWiSurAn, param = 'p', prob = 0.95)

# 3.1.91 psi(No road), p(Visibility)
psi_Nr_p_V = occMod(model=list(psi~No_road,p~Visibility),data=Monte, type="so")
summary(psi_Nr_p_V)
coef(object = psi_Nr_p_V, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_V, param = 'p', prob = 0.95)

# 3.1.92 psi(No road), p(Visibility + Windy)
psi_Nr_p_VPlusWi = occMod(model=list(psi~No_road,p~Visibility+Windy),data=Monte,
                          type="so")
summary(psi_Nr_p_VPlusWi)
coef(object = psi_Nr_p_VPlusWi, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_VPlusWi, param = 'p', prob = 0.95)

# 3.1.93 psi(No road), p(Visibility + Windy + survey)
psi_Nr_p_PlusVWiSur = occMod(model=list(psi~No_road,p~Visibility+Windy+survey),
                             data=Monte, type="so")
summary(psi_Nr_p_PlusVWiSur)
coef(object = psi_Nr_p_PlusVWiSur, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_PlusVWiSur, param = 'p', prob = 0.95)

# 3.1.94 psi(No road), p(Visibility + Windy + Afternoon)
psi_Nr_p_PlusVWiAn = occMod(model=list(psi~No_road,p~Visibility+Windy+AftNoon),
                             data=Monte, type="so")
summary(psi_Nr_p_PlusVWiAn)
coef(object = psi_Nr_p_PlusVWiAn, param = 'psi', prob = 0.95)
coef(object = psi_Nr_p_PlusVWiAn, param = 'p', prob = 0.95)

# 3.1.95 psi(Slope), p(Visibility + Windy + survey)
psi_Sl_p_PlusVWiSur = occMod(model=list(psi~Slope,p~Visibility+Windy+survey),
                             data=Monte, type="so")
summary(psi_Sl_p_PlusVWiSur)
coef(object = psi_Sl_p_PlusVWiSur, param = 'psi', prob = 0.95)
coef(object = psi_Sl_p_PlusVWiSur, param = 'p', prob = 0.95)

# 3.1.96 psi(.), p(Visibility + Windy + survey)
psi_dot_p_PlusVWiSur = occMod(model=list(psi~1,p~Visibility+Windy+survey),
                             data=Monte, type="so")
summary(psi_dot_p_PlusVWiSur)
coef(object = psi_dot_p_PlusVWiSur, param = 'psi', prob = 0.95)
coef(object = psi_dot_p_PlusVWiSur, param = 'p', prob = 0.95)

#Notice that almost all models had high SEs + CIs of betas containing 0. This is...
#...unavoidable due to the smaller sample size. This is also true for Sholicola...
#...albiventris models, as well as the the 2 species co-occurrence models. But,...
#...by itself is not a cause of great concern, since what it means is that...
#...when the unconstrained betas are back-transformed, the probabilities are...
#...are sometimes going to be below 0.5 (which is not great), and the CI intervals...
#...will be larger than ideal.

#Collect the results of each model into a single list variable, "models_Mf4":

models_Mf4 = list(psi_dot_p_dot, psi_E_p_dot, psi_Ar_p_dot, 
                  psi_Rn_p_dot, psi_E_p_Wa, psi_As_p_dot, psi_Sl_p_dot,
                  psi_Ch_p_dot, psi_Uh_p_dot, psi_V_p_dot, psi_B_p_dot,
                  psi_Mr_p_dot, psi_Dr_p_dot, psi_Nr_p_dot, psi_L_p_dot,
                  psi_Wl_p_dot, psi_Dl_p_dot, psi_Ar_p_exp2, psi_Ar_p_SurPoly,
                  psi_dot_p_BnPlusC, psi_dot_p_PlusBnCWi,psi_dot_p_PlusCWiO,
                  psi_E_p_BnPlusC, psi_E_p_PlusBnCWi, psi_E_p_PlusCWiO, 
                  psi_dot_p_Bn, psi_dot_p_An, psi_dot_p_Cs, psi_dot_p_O,
                  psi_dot_p_Sun, psi_dot_p_Wi, psi_dot_p_Wa, psi_dot_p_N,
                  psi_dot_p_C, psi_dot_p_Sur, psi_Nr_p_Wi, psi_Dr_p_Wi,
                  psi_Nr_p_WiPlusSur, psi_Dr_p_Sur, psi_Sl_p_WiPlusSur,
                  psi_E_p_WiPlusSur, psi_V_p_WiPlusSur, psi_Dl_p_WiPlusSur,
                  psi_Ar_p_WiPlusSur, psi_Uh_p_WiPlusSur, psi_Rn_p_WiPlusSur,
                  psi_As_p_WiPlusSur, psi_Nr_p_Bn, psi_Nr_p_WiPlusBn,
                  psi_Nr_p_CsPlusBn, psi_Sl_p_CsPlusBn, psi_E_p_CsPlusBn,
                  psi_Dr_p_CsPlusBn, psi_Dl_p_CsPlusBn, psi_V_p_CsPlusBn,
                  psi_Ar_p_CsPlusBn, psi_Ar_p_PlusWiSurBn, psi_Nr_p_PlusWiSurBn,
                  psi_E_p_PlusWiSurBn, psi_Rn_p_PlusWiSurBn, psi_Sl_p_PlusWiSurBn,
                  psi_Dl_p_PlusWiSurBn, psi_V_p_PlusWiSurBn, psi_As_p_PlusWiSurBn,
                  psi_Ar_p_PlusWiSurAn, psi_Nr_p_PlusWiSurAn,psi_E_p_PlusWiSurAn,
                  psi_Rn_p_PlusWiSurAn, psi_Sl_p_PlusWiSurAn, psi_Dl_p_PlusWiSurAn,
                  psi_V_p_PlusWiSurAn, psi_As_p_PlusWiSurAn, psi_Nr_p_V,
                  psi_Nr_p_VPlusWi, psi_Nr_p_PlusVWiSur, psi_Nr_p_PlusVWiAn,
                  psi_Sl_p_PlusVWiSur, psi_dot_p_PlusVWiSur)

#Create AIC table of the model results:
resultsMf4<-createAicTable(models_Mf4, use.aicc = TRUE)

#Print the results summary table:
cat('Montecincla fairbankii\nSingle Species Models')
print(resultsMf4$table)

#Write the results to a csv file:
write.csv(resultsMf4$table, "/media/Dissertation/Paper/Mf4.csv")

#Do a Goodness of Fit (GoF) test to see if QAICc needs to be used instead of AICc
#We will use a model with one of the highest number of parameters in the final...
#...model set. Set the number of bootstraps to a large number (50,000) and add...
#...seed to make the simulations reproducible:

MfBoot1 <- occMod(
  model = list(psi~Elevation, p~Cool+Windy+Ovrcst),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

#Print the results:
print(MfBoot1$gof)

#The probability is 13% (0.13). This gives the impression that the observed...
#...data fits the model (anything less that 5% can be said not to fit (5% being a...
#...common convention)). Note that, setting the no. of bootstraps to 10,000 will...
#...also give a similar probability (12.98%)[These values will slightly change even...
#...after setting seed for some unknown reason]. However, note the c-hat value...
#...which greater than one. This means that its best to adjust the AICc, and use....
#...QAICc instead.

MfBoot1_10 <- occMod(
  model = list(psi~Elevation, p~Cool+Windy+Ovrcst),
  data = Monte,
  modfitboot = 10000,
  seed = 23)

print(MfBoot1_10$gof)

#Since the c-hat (quasilikelihood variance inflation factor) is greater that one,...
#...the standard errors and confidence intervals of the real parameter estimates...
#...of all the models in the final model set need to be adjusted. This is because...
#... a large c-hat means variances in models are under-estimated and need to be...
#...adjusted upward, aﬀecting the complex models with larger variances more.
#This will ideally enable worse-fitting models in allowing the confidence intervals...
#...of estimates to cover the true values of the estimates.  

#Since there are multiple models with no. of parameters = 6 (the maximum), it will...
#...be prudent to go the extra distance to find the model with the model with the...
#...least c-hat value among them, and then using that c-hat value for creation...
#...of the QAICc table. So, GOF test needs to be done for all such models.

MfBoot2 <- occMod(
  model = list(psi~No_road, p~Windy+survey+BefNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot3 <- occMod(
  model = list(psi~No_road, p~Windy+survey+AftNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot4 <- occMod(
  model = list(psi~Slope, p~Windy+survey+BefNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot5 <- occMod(
  model = list(psi~Slope, p~Windy+survey+AftNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot6 <- occMod(
  model = list(psi~Elevation, p~Windy+survey+BefNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot7 <- occMod(
  model = list(psi~Elevation, p~Windy+survey+AftNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot8 <- occMod(
  model = list(psi~No_road, p~Visibility+Windy+survey),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot9 <- occMod(
  model = list(psi~Dry_lotic, p~Windy+survey+BefNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot10 <- occMod(
  model = list(psi~Dry_lotic, p~Windy+survey+AftNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot11 <- occMod(
  model = list(psi~Visibility, p~Windy+survey+BefNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot12 <- occMod(
  model = list(psi~Visibility, p~Windy+survey+AftNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot13 <- occMod(
  model = list(psi~Area, p~Windy+survey+BefNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot14 <- occMod(
  model = list(psi~Area, p~Windy+survey+AftNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot15 <- occMod(
  model = list(psi~No_road, p~Visibility+Windy+AftNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot16 <- occMod(
  model = list(psi~Rubus_niveus, p~Windy+survey+BefNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot17 <- occMod(
  model = list(psi~Rubus_niveus, p~Windy+survey+AftNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot18 <- occMod(
  model = list(psi~Aspect, p~Windy+survey+BefNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot19 <- occMod(
  model = list(psi~Aspect, p~Windy+survey+AftNoon),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot20 <- occMod(
  model = list(psi~Slope, p~Visibility+Windy+survey),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

MfBoot21 <- occMod(
  model = list(psi~Elevation, p~BefNoon+Cool+Windy),
  data = Monte,
  modfitboot = 50000,
  seed = 23)

#Create a list of c-hat values from all these models created for GOF tests:
chat_values <- cbind(MfBoot1$gof$chat, MfBoot2$gof$chat, MfBoot3$gof$chat,
             MfBoot4$gof$chat, MfBoot5$gof$chat, MfBoot6$gof$chat,
             MfBoot7$gof$chat, MfBoot8$gof$chat, MfBoot9$gof$chat,
             MfBoot10$gof$chat, MfBoot11$gof$chat, MfBoot12$gof$chat,
             MfBoot13$gof$chat, MfBoot14$gof$chat, MfBoot15$gof$chat,
             MfBoot16$gof$chat, MfBoot17$gof$chat, MfBoot18$gof$chat,
             MfBoot19$gof$chat, MfBoot20$gof$chat, MfBoot21$gof$chat)

#View the list:
chat_values 

#OUTPUTS (for the record):
#chat_values 
#[,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]   [,9]  [,10]  [,11] 
#1.4133 1.4928 1.4928 1.4793 1.4793 1.4712 1.4712 1.4434 1.5111 1.5111 1.4957 
# [,12]  [,13]  [,14]  [,15] [,16]  [,17]  [,18]  [,19]  [,20]  [,21]
#1.4957 1.4854 1.4854 1.4784 1.4915 1.4915 1.5039 1.5039 1.4265 1.4332 #

#Extract the minimum c-hat value from the list:
min(chat_values)

#The minimum value of c-hat is 1.4133 and from the list, its clear that its from...
#...the model with the highest delta AICc (but not necessarily in other model sets).
#So, the c-hat of the model psi_E_p_PlusCWiO can be used for the creation of the...
#QAICc table.

#But before that, we will have to check in how many plots can Dry lotic and...
#...and Lantana camara are found, This is because the lack of numerical convergence...
#...could be caused by them them existing in almost all/all of the plots occupied by...
#...Montecincla fairbankii.

#Use the detection history table of M. fairbankii to create a new dataframe.
#Define a new variable SSO (Site-specific occupancy) using mutate() and case_when():
SSOdf <- dethistMf %>%
  mutate(SSO = case_when(M1+M2+M3+M4>=1 ~ 1,
                         M1+M2+M3+M4<1 ~ 0))
#View new dataframe:
SSOdf

#Check how many occurrences of site-occupancy of M. fairbankii overlap with Lantana...
#...camara, using the 'sitecov' dataframe (can be skipped since L. camara doesn't...
#...appear in the final model set):
summary(SSOdf$SSO == sitecov$Lantana_camara)

#From this, we can understand that there is no clear overlap between the presence/...
#...detection of M. fairbankii and L. camara

#Similarly for Dry lotic:
summary(SSOdf$SSO == sitecov$Dry_lotic)

#Here too, there is no clear overlap between the presence/detection of M. fairbankii...
#...and Dry lotic conditions. So, both covariates can be safely used for occupancy...
#...analysis.

#Create qAICc table to adjust the variance due to >1 c-hat:
qaicMf <- createAicTable(
  mod.list = list(psi_dot_p_dot, psi_E_p_dot, psi_Ar_p_dot, 
         psi_Rn_p_dot, psi_E_p_Wa, psi_As_p_dot, psi_Sl_p_dot,
         psi_Ch_p_dot, psi_Uh_p_dot, psi_V_p_dot, psi_B_p_dot,
         psi_Mr_p_dot, psi_Dr_p_dot, psi_Nr_p_dot, psi_L_p_dot,
         psi_Wl_p_dot, psi_Dl_p_dot, psi_Ar_p_exp2, psi_Ar_p_SurPoly,
         psi_dot_p_BnPlusC, psi_dot_p_PlusBnCWi,psi_dot_p_PlusCWiO,
         psi_E_p_BnPlusC, psi_E_p_PlusBnCWi, psi_E_p_PlusCWiO, 
         psi_dot_p_Bn, psi_dot_p_An, psi_dot_p_Cs, psi_dot_p_O,
         psi_dot_p_Sun, psi_dot_p_Wi, psi_dot_p_Wa, psi_dot_p_N,
         psi_dot_p_C, psi_dot_p_Sur, psi_Nr_p_Wi, psi_Dr_p_Wi,
         psi_Nr_p_WiPlusSur, psi_Dr_p_Sur, psi_Sl_p_WiPlusSur,
         psi_E_p_WiPlusSur, psi_V_p_WiPlusSur, psi_Dl_p_WiPlusSur,
         psi_Ar_p_WiPlusSur, psi_Uh_p_WiPlusSur, psi_Rn_p_WiPlusSur,
         psi_As_p_WiPlusSur, psi_Nr_p_Bn, psi_Nr_p_WiPlusBn,
         psi_Nr_p_CsPlusBn, psi_Sl_p_CsPlusBn, psi_E_p_CsPlusBn,
         psi_Dr_p_CsPlusBn, psi_Dl_p_CsPlusBn, psi_V_p_CsPlusBn,
         psi_Ar_p_CsPlusBn, psi_Ar_p_PlusWiSurBn, psi_Nr_p_PlusWiSurBn,
         psi_E_p_PlusWiSurBn, psi_Rn_p_PlusWiSurBn, psi_Sl_p_PlusWiSurBn,
         psi_Dl_p_PlusWiSurBn, psi_V_p_PlusWiSurBn, psi_As_p_PlusWiSurBn,
         psi_Ar_p_PlusWiSurAn, psi_Nr_p_PlusWiSurAn,psi_E_p_PlusWiSurAn,
         psi_Rn_p_PlusWiSurAn, psi_Sl_p_PlusWiSurAn, psi_Dl_p_PlusWiSurAn,
         psi_V_p_PlusWiSurAn, psi_As_p_PlusWiSurAn, psi_Nr_p_V,
         psi_Nr_p_VPlusWi, psi_Nr_p_PlusVWiSur, psi_Nr_p_PlusVWiAn,
         psi_Sl_p_PlusVWiSur, psi_dot_p_PlusVWiSur),
  use.aicc = TRUE,
  chat = MfBoot1$gof$chat
)
# print the table
library(knitr)
kable(qaicMf$table, "simple")

#Write the results to a csv file:
write.csv(qaicMf$table, 
          "/media/Dissertation/Paper/_QAICc.csv")

#Check the number of models
str(qaicMf$table)   #n = 78

#Install flextable (if not installed) for creating a publication-ready tables:
#install.packages("flextable")
library(flextable)

#Set defaults (font and theme):
set_flextable_defaults(
  font.family = "Times New Roman",
  theme_fun = theme_vanilla)

#Convert to flextable:
qaicMf_tbl <- flextable(qaicMf$table)
#To save as image:
save_as_image(qaicMf_tbl,
              "/media/Dissertation/Paper/qaicMf_tbl.png")

#To save as word document:
library(officer)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = qaicMf_tbl) %>%   #Insert the flextable object there
  print(target = "qaicMf_tbl.docx")    

#Model average the psi estimates:
psi_maMf <- modAvg(qaicMf, param = 'psi')

#Show the results and summary:
kable(psi_maMf, "pipe")
summary(psi_maMf)

#look at the structure of psi_maMf:
str(psi_maMf)
#Since the column containing the site name is not a vector in this dataframe,...
#...it needs to be added. Convert the first column of the dataframe containing the...
#...site names ("Data for analyses") into a dataframe:
dfr <- data.frame(dz[,1])
str(dfr)
#Combine these this column with psi_maMf:
Mf_psi <- cbind(siteIDs = dfr$dz...1., psi_maMf)
head(Mf_psi)
str(Mf_psi)

#Save as word document:
rMf_psi <- setNames(Mf_psi,cbind("Site IDs", "Estimate","Standard Error",
                                     "Lower 95% Confidence Interval",
                                     "Upper 95% Confidence Interval"))
rMf_psi <- flextable(rMf_psi)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rMf_psi) %>%  #Insert the flextable object there
  print(target = "rMf_psi.docx")    

#Mean of psi for M. fairbankii:
mean(psi_maMf$est) # 0.861815

#Range of psi for M. fairbankii:
min(psi_maMf$est) # 0.6659567
max(psi_maMf$est) # 0.9229368

#Range of SE of psi for M. fairbankii:
min(psi_maMf$se) # 0.074722
max(psi_maMf$se) # 0.2888994

#Range of 95% CI of psi for M. fairbankii:
min(psi_maMf$lower_0.95) # 0.1358938
max(psi_maMf$upper_0.95) # 0.9894606

#The estimate of psi in the best model for M. fairbankii:
fitted(object = psi_Nr_p_dot, param = "psi")

#Model average the p estimates:
p_maMf <- modAvg(qaicMf, param = 'p')

#Show the results and summary:
kable(p_maMf, "pipe")
summary(p_maMf)

#Prepare this data output into a format which can be used for creating a table....
#...Look at the structure of p_maMf:
str(p_maMf)
#Since the column containing the site name is not a vector in this dataframe,...
#...it needs to be added.
#Create a dataframe from 'dfr' where the column repeats four times (27*4=108 rows):
dframe <- data.frame(dfr,i=rep(1:4,ea=NROW(dfr)))
str(dframe)
#Combine these two columns with p_maMf:
Mf_p <- cbind(dframe, p_maMf)
colnames(Mf_p)[1] <- "siteIDs"
colnames(Mf_p)[2] <- "Survey_number"
head(Mf_p)
str(Mf_p)

#Save as word document:
rMf_p <- setNames(Mf_p,cbind("Site IDs", "Survey no.", "Estimate","Standard Error",
                                 "Lower 95% Confidence Interval",
                                 "Upper 95% Confidence Interval"))
rMf_p <- flextable(rMf_p)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rMf_p) %>%  #Insert the flextable object there
  print(target = "rMf_p.docx")   

#Mean of p for M. fairbankii:
mean(p_maMf$est) # 0.6553877

#Range of p for M. fairbankii:
min(p_maMf$est) # 0.5171142
max(p_maMf$est) # 0.7341469

#Range of SE of p for M. fairbankii:
min(p_maMf$se) # 0.0737302
max(p_maMf$se) # 0.1774118

#Range of 95% CI of p for M. fairbankii:
min(p_maMf$lower_0.95) # 0.2101485
max(p_maMf$upper_0.95) # 0.8845152

#The estimate of p in the best model for M. fairbankii:
fitted(object = psi_Nr_p_dot, param = "p") #0.653484



##### 3.2 Sholicola albiventris Single Species Single Season Occupancy Models ####



#As visible in the model lists with AICc, the model names are automatically created...
#...based on the values/covariates for psi and p, and therefore the laborious task...
#...naming each model is not necessary. Mark models as "s1", "s2", "s3"... henceforth.

#Running all the basic models for Sholicola albiventris:

# 3.2.1  Dot model = psi(.)p(.)
s1 = occMod(model=list(psi~1,p~1), data=Sholi, type="so")
summary(s1)
coef(object = s1, param = 'psi', prob = 0.95)
coef(object = s1, param = 'p', prob = 0.95)

#Print the real estimates of occupancy and detection for this model:
print(unique(s1$real$psi))
print(unique(s1$real$p))

# 3.2.2 psi(Area), p(.)
s2 = occMod(model=list(psi~Area,p~1), data=Sholi, type="so")
summary(s2)
coef(object = s2, param = 'psi', prob = 0.95)
coef(object = s2, param = 'p', prob = 0.95)

# 3.2.3 psi(Canopy height), p(.)
s3 = occMod(model=list(psi~Canopy_height,p~1), data=Sholi, type="so")
summary(s3)
coef(object = s3, param = 'psi', prob = 0.95)
coef(object = s3, param = 'p', prob = 0.95)

# 3.2.4 psi(Understorey height), p(.)
s4 = occMod(model=list(psi~Understorey_height,p~1), data=Sholi, type="so")
summary(s4)
coef(object = s4, param = 'psi', prob = 0.95)
coef(object = s4, param = 'p', prob = 0.95)

# 3.2.5 psi(Visibility), p(.)
s5 = occMod(model=list(psi~Visibility,p~1), data=Sholi, type="so")
summary(s5)
coef(object = s5, param = 'psi', prob = 0.95)
coef(object = s5, param = 'p', prob = 0.95)

# 3.2.6 psi(Burn), p(.)
s6 = occMod(model=list(psi~Burn,p~1), data=Sholi, type="so")
summary(s6)
coef(object = s6, param = 'psi', prob = 0.95)
coef(object = s6, param = 'p', prob = 0.95)

# 3.2.7 psi(Aspect), p(.)
s7 = occMod(model=list(psi~Aspect,p~1), data=Sholi, type="so")
summary(s7)
coef(object = s7, param = 'psi', prob = 0.95)
coef(object = s7, param = 'p', prob = 0.95)

# 3.2.8 psi(Elevation), p(.)
s8 = occMod(model=list(psi~Elevation,p~1), data=Sholi, type="so")
summary(s8)
coef(object = s8, param = 'psi', prob = 0.95)
coef(object = s8, param = 'p', prob = 0.95)

# 3.2.9 psi(Slope), p(.)
s9 = occMod(model=list(psi~Slope,p~1), data=Sholi, type="so")
summary(s9)
coef(object = s9, param = 'psi', prob = 0.95)
coef(object = s9, param = 'p', prob = 0.95)

# 3.2.10 psi(Metalled road), p(.)
s10 = occMod(model=list(psi~Metalled_road,p~1), data=Sholi, type="so")
summary(s10)
coef(object = s10, param = 'psi', prob = 0.95)
coef(object = s10, param = 'p', prob = 0.95)

# 3.2.11 psi(Dirt road), p(.)
s11 = occMod(model=list(psi~Dirt_road,p~1), data=Sholi, type="so")
summary(s11)
coef(object = s11, param = 'psi', prob = 0.95)
coef(object = s11, param = 'p', prob = 0.95)

# 3.2.12 psi(No road), p(.)
s12 = occMod(model=list(psi~No_road,p~1), data=Sholi, type="so")
summary(s12)
coef(object = s12, param = 'psi', prob = 0.95)
coef(object = s12, param = 'p', prob = 0.95)
#Since there is large betas, SE, and CIs for the covariate, check the values...
#...of the real parameter.
print_one_site_estimates(s12)
View(s12)

# 3.2.13 psi(Lentic), p(.)
s13 = occMod(model=list(psi~Lentic,p~1), data=Sholi, type="so")
summary(s13)
coef(object = s13, param = 'psi', prob = 0.95)
coef(object = s13, param = 'p', prob = 0.95)

# 3.2.14 psi(Wet lotic), p(.)
s14 = occMod(model=list(psi~Wet_lotic,p~1), data=Sholi, type="so")
summary(s14)
coef(object = s14, param = 'psi', prob = 0.95)
coef(object = s14, param = 'p', prob = 0.95)

# 3.2.15 psi(Dry lotic), p(.)
s15 = occMod(model=list(psi~Dry_lotic,p~1), data=Sholi, type="so")
summary(s15)
coef(object = s15, param = 'psi', prob = 0.95)
coef(object = s15, param = 'p', prob = 0.95)

# 3.2.16 psi(Rubus niveus), p(.)
s16 = occMod(model=list(psi~Rubus_niveus,p~1), data=Sholi, type="so")
summary(s16)
coef(object = s16, param = 'psi', prob = 0.95)
coef(object = s16, param = 'p', prob = 0.95)

# 3.2.17 psi(Lantana camara), p(.)
s17 = occMod(model=list(psi~Lantana_camara,p~1), data=Sholi, type="so")
summary(s17)
coef(object = s17, param = 'psi', prob = 0.95)
coef(object = s17, param = 'p', prob = 0.95)

# 3.2.18 psi(.), p(Before noon)
s18 = occMod(model=list(psi~1,p~BefNoon), data=Sholi, type="so")
summary(s18)
coef(object = s18, param = 'psi', prob = 0.95)
coef(object = s18, param = 'p', prob = 0.95)

# 3.2.19 psi(.), p(After noon)
s19 = occMod(model=list(psi~1,p~AftNoon), data=Sholi, type="so")
summary(s19)
coef(object = s19, param = 'psi', prob = 0.95)
coef(object = s19, param = 'p', prob = 0.95)

# 3.2.20 psi(.), p(Clear sky)
s20 = occMod(model=list(psi~1,p~ClrSky), data=Sholi, type="so")
summary(s20)
coef(object = s20, param = 'psi', prob = 0.95)
coef(object = s20, param = 'p', prob = 0.95)

# 3.2.21 psi(.), p(Overcast)
s21 = occMod(model=list(psi~1,p~Ovrcst), data=Sholi, type="so")
summary(s21)
coef(object = s21, param = 'psi', prob = 0.95)
coef(object = s21, param = 'p', prob = 0.95)

# 3.2.22 psi(.), p(Sunny)
s22 = occMod(model=list(psi~1,p~Sunny), data=Sholi, type="so")
summary(s22)
coef(object = s22, param = 'psi', prob = 0.95)
coef(object = s22, param = 'p', prob = 0.95)

# 3.2.23 psi(.), p(Windy)
s23 = occMod(model=list(psi~1,p~Windy), data=Sholi, type="so")
summary(s23)
coef(object = s23, param = 'psi', prob = 0.95)
coef(object = s23, param = 'p', prob = 0.95)

# 3.2.24 psi(.), p(Warm)
s24 = occMod(model=list(psi~1,p~Warm), data=Sholi, type="so")
summary(s24)
coef(object = s24, param = 'psi', prob = 0.95)
coef(object = s24, param = 'p', prob = 0.95)

# 3.2.25 psi(.), p(Neutral)
s25 = occMod(model=list(psi~1,p~Neutral), data=Sholi, type="so")
summary(s25)
coef(object = s25, param = 'psi', prob = 0.95)
coef(object = s25, param = 'p', prob = 0.95)

# 3.2.26 psi(.), p(Cool)
s26 = occMod(model=list(psi~1,p~Cool), data=Sholi, type="so")
summary(s26)
coef(object = s26, param = 'psi', prob = 0.95)
coef(object = s26, param = 'p', prob = 0.95)

# 3.2.27 psi(.), p(survey)

#First Convert SURVEY to numeric:
Sholi$survcov$survey = as.numeric(Sholi$survcov$SURVEY)
list(Sholi$survcov$survey)

s27 = occMod(model=list(psi~1,p~survey), data=Sholi, type="so")
summary(s27)
coef(object = s27, param = 'psi', prob = 0.95)
coef(object = s27, param = 'p', prob = 0.95)

#Collect the results of each model into a single list variable, "models_Sa1":

models_Sa1 = list(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14,
                  s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27)

#Create AICc table of the model results:
resultsSa1<-createAicTable(models_Sa1, use.aicc = TRUE)

#Print the results summary table:
cat('Sholicola albiventris\nSingle Species Models')
print(resultsSa1$table)

#Run few more models (as only a few covariates appear in the models with the lowest...
#...AICc and DAICc within 7):

# 3.2.28 psi(Elevation), p(Neutral)
s28 = occMod(model=list(psi~Elevation,p~Neutral), data=Sholi, type="so")
summary(s28)
coef(object = s28, param = 'psi', prob = 0.95)
coef(object = s28, param = 'p', prob = 0.95)

# 3.2.29 psi(Lantana camara), p(Neutral)
s29 = occMod(model=list(psi~Lantana_camara,p~Neutral), data=Sholi, type="so")
summary(s29)
coef(object = s29, param = 'psi', prob = 0.95)
coef(object = s29, param = 'p', prob = 0.95)

# 3.2.30 psi(Understorey height), p(Neutral)
s30 = occMod(model=list(psi~Understorey_height,p~Neutral), data=Sholi, type="so")
summary(s30)
coef(object = s30, param = 'psi', prob = 0.95)
coef(object = s30, param = 'p', prob = 0.95)

# 3.2.31 psi(Elevation), p(survey)
s31 = occMod(model=list(psi~Elevation,p~survey), data=Sholi, type="so")
summary(s31)
coef(object = s31, param = 'psi', prob = 0.95)
coef(object = s31, param = 'p', prob = 0.95)

# 3.2.32 psi(Lantana camara), p(survey)
s32 = occMod(model=list(psi~Lantana_camara,p~survey), data=Sholi, type="so")
summary(s32)
coef(object = s32, param = 'psi', prob = 0.95)
coef(object = s32, param = 'p', prob = 0.95)

# 3.2.33 psi(Understorey height), p(survey)
s33 = occMod(model=list(psi~Understorey_height,p~survey), data=Sholi, type="so")
summary(s33)
coef(object = s33, param = 'psi', prob = 0.95)
coef(object = s33, param = 'p', prob = 0.95)

# 3.2.34 psi(Wet lotic), p(survey)
s34 = occMod(model=list(psi~Wet_lotic,p~survey), data=Sholi, type="so")
summary(s34)
coef(object = s34, param = 'psi', prob = 0.95)
coef(object = s34, param = 'p', prob = 0.95)

# 3.2.35 psi(No road), p(survey)
s35 = occMod(model=list(psi~No_road,p~survey), data=Sholi, type="so")
summary(s35)
coef(object = s35, param = 'psi', prob = 0.95)
coef(object = s35, param = 'p', prob = 0.95)

# 3.2.36 psi(Area), p(survey)
s36 = occMod(model=list(psi~Area,p~survey), data=Sholi, type="so")
summary(s36)
coef(object = s36, param = 'psi', prob = 0.95)
coef(object = s36, param = 'p', prob = 0.95)

# 3.2.37 psi(Burn), p(survey)
s37 = occMod(model=list(psi~Burn,p~survey), data=Sholi, type="so")
summary(s37)
coef(object = s37, param = 'psi', prob = 0.95)
coef(object = s37, param = 'p', prob = 0.95)

# 3.2.38 psi(Elevation), p(Warm)
s38 = occMod(model=list(psi~Elevation,p~Warm), data=Sholi, type="so")
summary(s38)
coef(object = s38, param = 'psi', prob = 0.95)
coef(object = s38, param = 'p', prob = 0.95)

# 3.2.39 psi(Lantana camara), p(Warm)
s39 = occMod(model=list(psi~Lantana_camara,p~Warm), data=Sholi, type="so")
summary(s39)
coef(object = s39, param = 'psi', prob = 0.95)
coef(object = s39, param = 'p', prob = 0.95)

# 3.2.40 psi(Understorey height), p(Warm)
s40 = occMod(model=list(psi~Understorey_height,p~Warm), data=Sholi, type="so")
summary(s40)
coef(object = s40, param = 'psi', prob = 0.95)
coef(object = s40, param = 'p', prob = 0.95)

# 3.2.41 psi(Wet lotic), p(Warm)
s41 = occMod(model=list(psi~Wet_lotic,p~Warm), data=Sholi, type="so")
summary(s41)
coef(object = s41, param = 'psi', prob = 0.95)
coef(object = s41, param = 'p', prob = 0.95)

# 3.2.42 psi(Elevation), p(Neutral + survey)
s42 = occMod(model=list(psi~Elevation,p~Neutral+survey), data=Sholi, type="so")
summary(s42)
coef(object = s42, param = 'psi', prob = 0.95)
coef(object = s42, param = 'p', prob = 0.95)

# 3.2.43 psi(Lantana camara), p(Neutral + survey)
s43 = occMod(model=list(psi~Lantana_camara,p~Neutral+survey), data=Sholi, 
             type="so")
summary(s43)
coef(object = s43, param = 'psi', prob = 0.95)
coef(object = s43, param = 'p', prob = 0.95)

# 3.2.44 psi(Understorey height), p(Neutral + survey)
s44 = occMod(model=list(psi~Understorey_height,p~Neutral+survey), data=Sholi,
             type="so")
summary(s44)
coef(object = s44, param = 'psi', prob = 0.95)
coef(object = s44, param = 'p', prob = 0.95)

# 3.2.45 psi(Elevation), p(Neutral + survey + Warm)
s45 = occMod(model=list(psi~Elevation,p~Neutral+survey+Warm), data=Sholi, type="so")
summary(s45)
coef(object = s45, param = 'psi', prob = 0.95)
coef(object = s45, param = 'p', prob = 0.95)

# 3.2.46 psi(Lantana camara), p(Neutral + survey + Warm)
s46 = occMod(model=list(psi~Lantana_camara,p~Neutral+survey+Warm), data=Sholi,
             type="so")
summary(s46)
coef(object = s46, param = 'psi', prob = 0.95)
coef(object = s46, param = 'p', prob = 0.95)

# 3.2.47 psi(Understorey height), p(Neutral + survey + Warm)
s47 = occMod(model=list(psi~Understorey_height,p~Neutral+survey+Warm), data=Sholi,
             type="so")
summary(s47)
coef(object = s47, param = 'psi', prob = 0.95)
coef(object = s47, param = 'p', prob = 0.95)

# 3.2.48 psi(Elevation), p(Neutral + survey + Cool)
s48 = occMod(model=list(psi~Elevation,p~Neutral+survey+Cool), data=Sholi, type="so")
summary(s48)
coef(object = s48, param = 'psi', prob = 0.95)
coef(object = s48, param = 'p', prob = 0.95)

# 3.2.49 psi(Lantana camara), p(Neutral + survey + Cool)
s49 = occMod(model=list(psi~Lantana_camara,p~Neutral+survey+Cool), data=Sholi,
             type="so")
summary(s49)
coef(object = s49, param = 'psi', prob = 0.95)
coef(object = s49, param = 'p', prob = 0.95)

# 3.2.50 psi(Understorey height), p(Neutral + survey + Cool)
s50 = occMod(model=list(psi~Understorey_height,p~Neutral+survey+Cool),
             data=Sholi, type="so")
summary(s50)
coef(object = s50, param = 'psi', prob = 0.95)
coef(object = s50, param = 'p', prob = 0.95)

# 3.2.51 psi(No road), p(Neutral + survey + Cool)
s51 = occMod(model=list(psi~No_road,p~Neutral+survey+Cool), data=Sholi, type="so")
summary(s51)
coef(object = s51, param = 'psi', prob = 0.95)
coef(object = s51, param = 'p', prob = 0.95)

# 3.2.52 psi(No road), p(Neutral + survey)
s52 = occMod(model=list(psi~No_road,p~Neutral+survey), data=Sholi, type="so")
summary(s52)
coef(object = s52, param = 'psi', prob = 0.95)
coef(object = s52, param = 'p', prob = 0.95)

# 3.2.53 psi(No road), p(Cool + survey)
s53 = occMod(model=list(psi~No_road,p~Cool+survey), data=Sholi, type="so")
summary(s53)
coef(object = s53, param = 'psi', prob = 0.95)
coef(object = s53, param = 'p', prob = 0.95)

#Collect the results of each model into a single list variable, "models_Sa2":
models_Sa2 = list(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14,
                  s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27,
                  s28, s29, s30, s31, s32, s33, s34, s35, s36, s37, s38, s39, s40,
                  s41, s42, s43, s44, s45, s46, s47, s48, s49, s50, s51, s52, s53)

#Create AIC table of the model results:
resultsSa2<-createAicTable(models_Sa2, use.aicc = TRUE)

#Print the results summary table:
cat('Sholicola albiventris\nSingle Species Occupancy Models')
print(resultsSa2$table)

#Check whether SURVEY (as factors), instead of survey (as numeric) makes any...
#...difference to the top models:

# 3.2.54 psi(Elevation), p(Neutral + SURVEY)
s54 = occMod(model=list(psi~Elevation,p~Neutral+SURVEY), data=Sholi, type="so")
summary(s54)
coef(object = s54, param = 'psi', prob = 0.95)
coef(object = s54, param = 'p', prob = 0.95)

# 3.2.55 psi(Elevation), p(SURVEY)
s55 = occMod(model=list(psi~Elevation,p~SURVEY), data=Sholi, type="so")
summary(s55)
coef(object = s55, param = 'psi', prob = 0.95)
coef(object = s55, param = 'p', prob = 0.95)

# 3.2.56 psi(Lantana camara), p(Neutral + SURVEY)
s56 = occMod(model=list(psi~Lantana_camara,p~Neutral+SURVEY), data=Sholi, type="so")
summary(s56)
coef(object = s56, param = 'psi', prob = 0.95)
coef(object = s56, param = 'p', prob = 0.95)

# 3.2.57 psi(Elevation), p(Neutral + SURVEY + Cool)
s57 = occMod(model=list(psi~Elevation,p~Neutral+SURVEY+Cool), data=Sholi, type="so")
summary(s57)
coef(object = s57, param = 'psi', prob = 0.95)
coef(object = s57, param = 'p', prob = 0.95)

# 3.2.58 psi(Elevation), p(Neutral + SURVEY + Warm)
s58 = occMod(model=list(psi~Elevation,p~Neutral+SURVEY+Warm), data=Sholi, type="so")
summary(s58)
coef(object = s58, param = 'psi', prob = 0.95)
coef(object = s58, param = 'p', prob = 0.95)

# 3.2.59 psi(Lantana camara), p(Neutral + SURVEY + Cool)
s59 = occMod(model=list(psi~Lantana_camara,p~Neutral+SURVEY+Cool), data=Sholi,
             type="so")
summary(s59)
coef(object = s59, param = 'psi', prob = 0.95)
coef(object = s59, param = 'p', prob = 0.95)

#Since we see that models s28, s42, s29, s31, s43, s8, s38, s45, s48, s17,...
#...s49, and s30 are the only ones within DAICc of 7, and s32, s39 & s46...
#...shows high convergence problems,create AICc table of the 7 significant models...
#...and write them to a csv file: 

models_Sa3 = list(s8, s17, s28, s29, s30, s31, s38, s42, s43, s45, s48, s49, s54,
                  s55, s56, s57, s58, s59)
results_Sa3<-createAicTable(models_Sa3, use.aicc = TRUE)

# print the table
kable(results_Sa3$table, "pipe")

#Only one model with 'SURVEY' (as factor) fall into the DAICc of 7 (psi(Elevation)....
#...p(Neutral+SURVEY). And since this model is already represented in the model set...
#...by its numerical equivalent (Elevation)p(Neutral+survey) at second place,...
#...ignore all the models with 'SURVEY'. This is also true when using QAICc (verified).

#Do Goodness of Fit (GoF) tests to see if QAICc needs to be used instead of AICc
#We will use a model with one of the highest number of parameters and delta AICc,...
#...in the final model set. Set the number of bootstraps to 50,000 and set seed:

#GOF tests of models with 6 parameters:

SaBoot1 <- occMod(
  model = list(psi~Lantana_camara, p~Neutral+survey+Cool),
  data = Sholi,
  modfitboot = 50000,
  seed = 23)

#Print the results:
print(SaBoot1$gof)

#The probability is 31.35% (0.3135). So, the observed data looks like it fits...
#...the model. However, note the c-hat value which greater than one. This means...
#...that its best to adjust the AICc, and use QAICc instead. Note that, setting...
#...the no. of bootstraps to 10,000 will also give a similar probability (31.31%). 

SaBoot1_10 <- occMod(
  model = list(psi~Lantana_camara, p~Neutral+survey+Cool),
  data = Sholi,
  modfitboot = 10000,
  seed = 23)

print(SaBoot1_10$gof)

#GOF tests of other 6 parameter models:

SaBoot2 <- occMod(
  model = list(psi~Elevation, p~Neutral+survey+Cool),
  data = Sholi,
  modfitboot = 50000,
  seed = 23)

SaBoot3 <- occMod(
  model = list(psi~Elevation, p~Neutral+survey+Warm),
  data = Sholi,
  modfitboot = 50000,
  seed = 23)

#Create a list of c-hat values:
Sa_chat <- cbind(SaBoot1$gof$chat, SaBoot2$gof$chat, SaBoot3$gof$chat)

#View the list:
Sa_chat 

#Extract the minimum c-hat value from the list:
min(Sa_chat)

#Since the lowest c-hat is greater that one (1.0281 in SaBoot2), QAICc can be used...
#...(but can also be ignored since its close to 1). In this case, we will err on...
#...the side of caution and use QAICc.

#But before that, we will have to check in how many plots S. albiventris and...
#...L. camara are found together, This is because the lack of numerical convergence...
#...containing L.camara could be caused by them them co-existing in almost all/all...
#...of the plots. Same is true for the covariate No road, but in terms of large...
#...beta estimates, SE, and 95% CIs.

#Use the detection history table of S. albiventris to create a new dataframe.
#Define a new variable SaSSO (Site-specific occupancy) using mutate() and case_when():
SaSSOdf <- dethistSa %>%
  mutate(SaSSO = case_when(S1+S2+S3+S4>=1 ~ 1,
                         S1+S2+S3+S4<1 ~ 0))
#View new dataframe:
SaSSOdf

#Check how many occurences of site-occupancy of S. albiventris overlaps with Lantana...
#...camara, using the 'sitecov' dataframe:
summary(SaSSOdf$SaSSO == sitecov$Lantana_camara)

#Check the same for 'No road' covariate:
summary(SaSSOdf$SaSSO == sitecov$No_road)

#From this, we can understand that there is no prominent overlap between the presence/...
#...detection of S. albiventris and L. camara or No road. So, the covariate can be...
#...safely used for occupancy analysis.

#Create qAICc table to adjust the variance due to >1 c-hat:
qaicSa <- createAicTable(
  mod.list = list(s8, s17, s28, s29, s30, s31, s38, s42, s43, s45, s48, s49),
  use.aicc = TRUE,
  chat = SaBoot2$gof$chat
)

# print the table
kable(qaicSa$table, "simple")

#Write the results to a csv file:
write.csv(qaicSa$table, "/media/Dissertation/Paper/Sa_QAICc.csv")

#Save as word document:
qaicSa_tbl <- flextable(qaicSa$table)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = qaicSa_tbl) %>%   #Insert the flextable object there
  print(target = "qaicSa_tbl.docx")    

#Model average the psi estimates:
psi_maSa <- modAvg(qaicSa, param = 'psi')

#Show the results and summary:
kable(psi_maSa, "pipe")
summary(psi_maSa)

#Add the site IDs to psi_maMf:
Sa_psi <- cbind(siteIDs = dfr$dz...1., psi_maSa)
head(Sa_psi)
str(Sa_psi)

#Save as word document:
rSa_psi <- setNames(Sa_psi,cbind("Site IDs", "Estimate","Standard Error",
                                 "Lower 95% Confidence Interval",
                                 "Upper 95% Confidence Interval"))
rSa_psi <- flextable(rSa_psi)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rSa_psi) %>%  #Insert the flextable object there
  print(target = "rSa_psi.docx")    

#Mean of psi for S. albiventris:
mean(psi_maSa$est) # 0.4543001

#Range of psi for S. albiventris:
min(psi_maSa$est) # 0.1358074
max(psi_maSa$est) # 0.9971837

#Range of SE of psi for S. albiventris:
min(psi_maSa$se) # 0.02940619
max(psi_maSa$se) # 0.2430908

#Range of 95% CI of psi for S. albiventris:
min(psi_maSa$lower_0.95) # 4.319817e-07
max(psi_maSa$upper_0.95) # 1

#Model average the p estimates:
p_maSa <- modAvg(qaicSa, param = 'p')

#Show the results and summary:
kable(p_maSa, "pipe")
summary(p_maSa)
str(p_maSa)

#Add the site IDs and survey numbers to p_maSa
Sa_p <- cbind(dframe, p_maSa)
colnames(Sa_p)[1] <- "siteIDs"
colnames(Sa_p)[2] <- "Survey_number"
head(Sa_p)
str(Sa_p)

#Save as word document:
rSa_p <- setNames(Sa_p,cbind("Site IDs", "Survey no.", "Estimate","Standard Error",
                             "Lower 95% Confidence Interval",
                             "Upper 95% Confidence Interval"))
rSa_p <- flextable(rSa_p)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rSa_p) %>%  #Insert the flextable object there
  print(target = "rSa_p.docx")  

#Mean of p for S. albiventris:
mean(p_maSa$est) # 0.5730775

#Range of p for S. albiventris:
min(p_maSa$est) # 0.4102089
max(p_maSa$est) # 0.7939474

#Range of SE of p for S. albiventris:
min(p_maSa$se) # 0.1137778
max(p_maSa$se) # 0.1713888

#Range of 95% CI of p for S. albiventris:
min(p_maSa$lower_0.95) # 0.1827463
max(p_maSa$upper_0.95) # 0.9472857



#####3.3 Two Species Single Season Occupancy models####


######3.3.1. MS format: Species A = M. fairbankii, Species B = S. albiventris ######

#In this scenario of two species models, the models use M. fairbankii as Species...
#...A (coded as 1), and S. albiventris as Species B (coded as 2). 
#Here, psiA = the probability of species A being present, regardless of the...
#...occupancy status of species B; psiB = the probability of species B being...
#...present, regardless of occupancy status of species A; psiAB = the probability...
#...of both species being present; psiBA = probability of occupancy for species B,...
#...given species A is present; psiBa = probability of occupancy for species B,...
#...given species A is absent; pA = the probability of detecting species A, given...
#...only A is present; pB = the probability of detecting species B given only B is...
#...present; rA = the probability of detecting species A, given A and B are...
#...present; rB = the probability of detecting species B, given A and B are...
#...present; rAB = the probability of detecting both A and B, given A and B are...
#...present; rBA = the conditional probability of detecting B, given that A was...
#...detected; rBa = the conditional probability of detecting B, given that A was...
#...not detected

# 3.3.1.1 Run the dot model with constant occupancy and detection:
c1 <- occMod(model = list(psi~1,p~1), data=Twosp, type="so.2sp.1")
summary(c1)
coef(object = c1, param = 'psi', prob = 0.95)
coef(object = c1, param = 'p', prob = 0.95)

#Look at the structure of the 'real' estimates from the output:
str(c1$real, max.level = 2, give.attr = F)

#Look at the structure of the unconditional probabilities like psiB which has been...
#...converted from conditional probabilities like psiBA and psiBa: 
str(c1$derived, max.level = 2, give.attr = F)

#Look at the unique probabilities for psi estimates:
lapply(c1$real, FUN = unique)

#Look at the unique probabilities for detection estimates (for the first site):
print_one_site_estimates(c1)

# 3.3.1.2 Run a model with species-specific detection and occupancy:
c2<- occMod(model = list(psi~SP,p~SP), data=Twosp, type="so.2sp.1")  
summary(c2)
coef(object = c2, param = 'psi', prob = 0.95)
coef(object = c2, param = 'p', prob = 0.95)
str(c2$derived, max.level = 2, give.attr = F)

# 3.3.1.3 Run a model with species-specific detection and occupancy as well as...
#...interaction effect on occupancy only:
c3<- occMod(model = list(psi~SP+INT,p~SP), data=Twosp, type="so.2sp.1")  
summary(c3)
coef(object = c3, param = 'psi', prob = 0.95)
coef(object = c3, param = 'p', prob = 0.95)

#Notice the high beta coefficient of psiBa and SE and 95% CI being order of magnitudes...
#...high, the outcome of which is that the real estimate of psiBA and its standard...
#...error being estimated as zero, along with a confidence interval of 0-1. Mark...
#...all such models with "Sus".

# 3.3.1.4 Run a model with species-specific detection and occupancy as well as...
#...interaction of occupancy on detection (detection of a species is different...
#...in presence of the other species as opposed to when its not):
c4<- occMod(model = list(psi~SP,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c4)
coef(object = c4, param = 'psi', prob = 0.95)
coef(object = c4, param = 'p', prob = 0.95)

# 3.3.1.5 Run a model with species-specific detection and occupancy as well as...
#...interaction of detection on detection (detection of a species is different...
#...when the other species is also detected as opposed to when its not):
c5<- occMod(model = list(psi~SP,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c5)
coef(object = c5, param = 'psi', prob = 0.95)
coef(object = c5, param = 'p', prob = 0.95)
print_one_site_estimates(c5)

# 3.3.1.6 Run a model with species-specific detection and occupancy; interaction...
#...effect on occupancy; and interaction of occupancy on detection:
c6<- occMod(model = list(psi~SP+INT,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c6)
coef(object = c6, param = 'psi', prob = 0.95)
coef(object = c6, param = 'p', prob = 0.95)
str(c6$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.7 Run a model with species-specific detection and occupancy; interaction...
#...effect on occupancy; interaction of occupancy on detection and occupancy:
c7<- occMod(model = list(psi~SP+INT,p~SP+INT_o+INT_d), data=Twosp, type="so.2sp.1")  
summary(c7)
coef(object = c7, param = 'psi', prob = 0.95)
coef(object = c7, param = 'p', prob = 0.95)
str(c7$derived, max.level = 2, give.attr = F)

#Sus

#Now, run models with site-specific covariates:

# 3.3.1.8 psi (SP + INT + Area), p (SP) 
c8<- occMod(model = list(psi~SP+INT+Area,p~SP), data=Twosp, type="so.2sp.1")  
summary(c8)
coef(object = c8, param = 'psi', prob = 0.95)
coef(object = c8, param = 'p', prob = 0.95)
str(c8$derived, max.level = 2, give.attr = F)
#When we get suspicious values for beta of psiBa, the real parameters can be viewed...
#...for every site (which should confirm est of, se of 0, and 95% CI of 0-1):
c8[["real"]][["psiBa"]]

#Sus

# 3.3.1.9 psi (SP + INT + Area), p (SP+INT_o) 
c9<- occMod(model = list(psi~SP+INT+Area,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c9)
coef(object = c9, param = 'psi', prob = 0.95)
coef(object = c9, param = 'p', prob = 0.95)
str(c9$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.10 psi (SP + INT + Area), p (SP+INT_d) 
c10<- occMod(model = list(psi~SP+INT+Area,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c10)
coef(object = c10, param = 'psi', prob = 0.95)
coef(object = c10, param = 'p', prob = 0.95)
str(c10$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.11 psi (SP + INT + Area), p (ID) (here ID = SP+INT_o+INT_d)
c11<- occMod(model = list(psi~SP+INT+Area,p~ID), data=Twosp, type="so.2sp.1")  
summary(c11)
coef(object = c11, param = 'psi', prob = 0.95)
coef(object = c11, param = 'p', prob = 0.95)
str(c11$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.12 psi (SP + INT + Canopy height), p (SP) 
c12<- occMod(model = list(psi~SP+INT+Canopy_height,p~SP), data=Twosp, type="so.2sp.1")  
summary(c12)
coef(object = c12, param = 'psi', prob = 0.95)
coef(object = c12, param = 'p', prob = 0.95)
str(c12$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.13 psi (SP + INT + Canopy height), p (SP+INT_o) 
c13<- occMod(model = list(psi~SP+INT+Canopy_height,p~SP+INT_o), data=Twosp,
             type="so.2sp.1")  
summary(c13)
coef(object = c13, param = 'psi', prob = 0.95)
coef(object = c13, param = 'p', prob = 0.95)
str(c13$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.14 psi (SP + INT + Canopy height), p (SP+INT_d) 
c14<- occMod(model = list(psi~SP+INT+Canopy_height,p~SP+INT_d), data=Twosp,
             type="so.2sp.1")  
summary(c14)
coef(object = c14, param = 'psi', prob = 0.95)
coef(object = c14, param = 'p', prob = 0.95)
str(c14$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.15 psi (SP + INT + Canopy height), p (ID)
c15<- occMod(model = list(psi~SP+INT+Canopy_height,p~ID), data=Twosp, type="so.2sp.1")  
summary(c15)
coef(object = c15, param = 'psi', prob = 0.95)
coef(object = c15, param = 'p', prob = 0.95)
str(c15$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.16 psi (SP + INT + Understorey height), p (SP) 
c16<- occMod(model = list(psi~SP+INT+Understorey_height,p~SP), data=Twosp,
             type="so.2sp.1")  
summary(c16)
coef(object = c16, param = 'psi', prob = 0.95)
coef(object = c16, param = 'p', prob = 0.95)
str(c16$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.17 psi (SP + INT + Understorey height), p (SP+INT_o) 
c17<- occMod(model = list(psi~SP+INT+Understorey_height,p~SP+INT_o), data=Twosp,
             type="so.2sp.1")  
summary(c17)
coef(object = c17, param = 'psi', prob = 0.95)
coef(object = c17, param = 'p', prob = 0.95)
str(c17$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.18 psi (SP + INT + Understorey height), p (SP+INT_d) 
c18<- occMod(model = list(psi~SP+INT+Understorey_height,p~SP+INT_d), data=Twosp,
             type="so.2sp.1")  
summary(c18)
coef(object = c18, param = 'psi', prob = 0.95)
coef(object = c18, param = 'p', prob = 0.95)
str(c18$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.19 psi (SP + INT + Understorey height), p (ID)
c19<- occMod(model = list(psi~SP+INT+Understorey_height,p~ID), data=Twosp,
             type="so.2sp.1")  
summary(c19)
coef(object = c19, param = 'psi', prob = 0.95)
coef(object = c19, param = 'p', prob = 0.95)
str(c19$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.20 psi (SP + INT + Visibility), p (SP) 
c20<- occMod(model = list(psi~SP+INT+Visibility,p~SP), data=Twosp, type="so.2sp.1")  
summary(c20)
coef(object = c20, param = 'psi', prob = 0.95)
coef(object = c20, param = 'p', prob = 0.95)
str(c20$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.21 psi (SP + INT + Visibility), p (SP+INT_o) 
c21<- occMod(model = list(psi~SP+INT+Visibility,p~SP+INT_o), data=Twosp,
             type="so.2sp.1")  
summary(c21)
coef(object = c21, param = 'psi', prob = 0.95)
coef(object = c21, param = 'p', prob = 0.95)
str(c21$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.22 psi (SP + INT + Visibility), p (SP+INT_d) 
c22<- occMod(model = list(psi~SP+INT+Visibility,p~SP+INT_d), data=Twosp,
             type="so.2sp.1")  
summary(c22)
coef(object = c22, param = 'psi', prob = 0.95)
coef(object = c22, param = 'p', prob = 0.95)
str(c22$derived, max.level = 2, give.attr = F)

#Sus

# 3.3.1.23 psi (SP + INT + Visibility), p (ID)
c23<- occMod(model = list(psi~SP+INT+Visibility,p~ID), data=Twosp, type="so.2sp.1")  
summary(c23)
coef(object = c23, param = 'psi', prob = 0.95)
coef(object = c23, param = 'p', prob = 0.95)
str(c23$derived, max.level = 2, give.attr = F)

#Sus

#As is clear from the many models run so far, all models with the interactive...
#...effect for psi produces estimates of psiBa as 0, which is not valid. Hence,...
#...we can't use such models. So, only models with psi(SP+site-specific covariates)...
#...can be used.

# 3.3.1.24 psi (SP + Area), p (SP) 
c24<- occMod(model = list(psi~SP+Area,p~SP), data=Twosp, type="so.2sp.1")  
summary(c24)
coef(object = c24, param = 'psi', prob = 0.95)
coef(object = c24, param = 'p', prob = 0.95)
print_one_site_estimates(c24)

# 3.3.1.25 psi (SP + Area), p (SP+INT_o) 
c25<- occMod(model = list(psi~SP+Area,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c25)
coef(object = c25, param = 'psi', prob = 0.95)
coef(object = c25, param = 'p', prob = 0.95)
print_one_site_estimates(c25)

# 3.3.1.26 psi (SP + Area), p (SP+INT_d) 
c26<- occMod(model = list(psi~SP+Area,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c26)
coef(object = c26, param = 'psi', prob = 0.95)
coef(object = c26, param = 'p', prob = 0.95)
print_one_site_estimates(c26)

# 3.3.1.27 psi (SP + Area), p (ID)
c27<- occMod(model = list(psi~SP+Area,p~ID), data=Twosp, type="so.2sp.1")  
summary(c27)
coef(object = c27, param = 'psi', prob = 0.95)
coef(object = c27, param = 'p', prob = 0.95)
print_one_site_estimates(c27)

#Sus (note values of p and r)

# 3.3.1.28 psi (SP + Canopy height), p (SP) 
c28<- occMod(model = list(psi~SP+Canopy_height,p~SP), data=Twosp, type="so.2sp.1")  
summary(c28)
coef(object = c28, param = 'psi', prob = 0.95)
coef(object = c28, param = 'p', prob = 0.95)
print_one_site_estimates(c28)

# 3.3.1.29 psi (SP + Canopy height), p (SP+INT_o) 
c29<- occMod(model = list(psi~SP+Canopy_height,p~SP+INT_o), data=Twosp,
             type="so.2sp.1")  
summary(c29)
coef(object = c29, param = 'psi', prob = 0.95)
coef(object = c29, param = 'p', prob = 0.95)
print_one_site_estimates(c29)

# 3.3.1.30 psi (SP + Canopy height), p (SP+INT_d) 
c30<- occMod(model = list(psi~SP+Canopy_height,p~SP+INT_d), data=Twosp,
             type="so.2sp.1")  
summary(c30)
coef(object = c30, param = 'psi', prob = 0.95)
coef(object = c30, param = 'p', prob = 0.95)
print_one_site_estimates(c30)

# 3.3.1.31 psi (SP + Canopy height), p (ID)
c31<- occMod(model = list(psi~SP+Canopy_height,p~ID), data=Twosp, type="so.2sp.1")  
summary(c31)
coef(object = c31, param = 'psi', prob = 0.95)
coef(object = c31, param = 'p', prob = 0.95)
print_one_site_estimates(c31)

#Sus (note beta estimate of pB)

# 3.3.1.32 psi (SP + Understorey height), p (SP) 
c32<- occMod(model = list(psi~SP+Understorey_height,p~SP), data=Twosp,
             type="so.2sp.1")  
summary(c32)
coef(object = c32, param = 'psi', prob = 0.95)
coef(object = c32, param = 'p', prob = 0.95)
print_one_site_estimates(c32)

# 3.3.1.33 psi (SP + Understorey height), p (SP+INT_o) 
c33<- occMod(model = list(psi~SP+Understorey_height,p~SP+INT_o), data=Twosp,
             type="so.2sp.1")  
summary(c33)
coef(object = c33, param = 'psi', prob = 0.95)
coef(object = c33, param = 'p', prob = 0.95)
print_one_site_estimates(c33)

# 3.3.1.34 psi (SP + Understorey height), p (SP+INT_d) 
c34<- occMod(model = list(psi~SP+Understorey_height,p~SP+INT_d), data=Twosp,
             type="so.2sp.1")  
summary(c34)
coef(object = c34, param = 'psi', prob = 0.95)
coef(object = c34, param = 'p', prob = 0.95)
print_one_site_estimates(c34)

# 3.3.1.35 psi (SP + Understorey height), p (ID)
c35<- occMod(model = list(psi~SP+Understorey_height,p~ID), data=Twosp,
             type="so.2sp.1")  
summary(c35)
coef(object = c35, param = 'psi', prob = 0.95)
coef(object = c35, param = 'p', prob = 0.95)
print_one_site_estimates(c35)

#Sus

#Check the AICc values for the models:
models_tsp1 = list(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,
                  c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,c32,c33,c34,
                  c35)
results_tsp1<-createAicTable(models_tsp1, use.aicc=TRUE)

# show the table
kable(results_tsp1$table, "pipe")

#since the ones with ID (SP+INT_o+INT_d) almost consistently fall outside of the...
#...DAICc of 7 (mostly due to the no. of parameters), and also since they can't...
#...correctly calculate pB, they can removed. Similarly all models with INT as...
#...a factor of psi can also be removed since it always miscalculates psiBa.
#Also note the convergence warnings ("warn.conv") for some of these models. They...
#...barely make it.

#To avoid creating useless models, do not create models containing INT as a factor...
#...of psi, and ID as factor of p.

# 3.3.1.36 psi (SP + Visibility), p (SP) 
c36<- occMod(model = list(psi~SP+Visibility,p~SP), data=Twosp, type="so.2sp.1")  
summary(c36)
coef(object = c36, param = 'psi', prob = 0.95)
coef(object = c36, param = 'p', prob = 0.95)
print_one_site_estimates(c36)

# 3.3.1.37 psi (SP + Visibility), p (SP+INT_o) 
c37<- occMod(model = list(psi~SP+Visibility,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c37)
coef(object = c37, param = 'psi', prob = 0.95)
coef(object = c37, param = 'p', prob = 0.95)
print_one_site_estimates(c37)

# 3.3.1.38 psi (SP + Visibility), p (SP+INT_d) 
c38<- occMod(model = list(psi~SP+Visibility,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c38)
coef(object = c38, param = 'psi', prob = 0.95)
coef(object = c38, param = 'p', prob = 0.95)
print_one_site_estimates(c38)

# 3.3.1.39 psi (SP + Burn), p (SP) 
c39<- occMod(model = list(psi~SP+Burn,p~SP), data=Twosp, type="so.2sp.1")  
summary(c39)
coef(object = c39, param = 'psi', prob = 0.95)
coef(object = c39, param = 'p', prob = 0.95)
print_one_site_estimates(c39)

# 3.3.1.40 psi (SP + Burn), p (SP+INT_o) 
c40<- occMod(model = list(psi~SP+Burn,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c40)
coef(object = c40, param = 'psi', prob = 0.95)
coef(object = c40, param = 'p', prob = 0.95)
print_one_site_estimates(c40)

# 3.3.1.41 psi (SP + Burn), p (SP+INT_d) 
c41<- occMod(model = list(psi~SP+Burn,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c41)
coef(object = c41, param = 'psi', prob = 0.95)
coef(object = c41, param = 'p', prob = 0.95)
print_one_site_estimates(c41)

# 3.3.1.42 psi (SP + Aspect), p (SP) 
c42<- occMod(model = list(psi~SP+Aspect,p~SP), data=Twosp, type="so.2sp.1")  
summary(c42)
coef(object = c42, param = 'psi', prob = 0.95)
coef(object = c42, param = 'p', prob = 0.95)
print_one_site_estimates(c42)

# 3.3.1.43 psi (SP + Aspect), p (SP+INT_o) 
c43<- occMod(model = list(psi~SP+Aspect,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c43)
coef(object = c43, param = 'psi', prob = 0.95)
coef(object = c43, param = 'p', prob = 0.95)
print_one_site_estimates(c43)

# 3.3.1.44 psi (SP + Aspect), p (SP+INT_d) 
c44<- occMod(model = list(psi~SP+Aspect,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c44)
coef(object = c44, param = 'psi', prob = 0.95)
coef(object = c44, param = 'p', prob = 0.95)
print_one_site_estimates(c44)

# 3.3.1.45 psi (SP + Elevation), p (SP) 
c45<- occMod(model = list(psi~SP+Elevation,p~SP), data=Twosp, type="so.2sp.1")  
summary(c45)
coef(object = c45, param = 'psi', prob = 0.95)
coef(object = c45, param = 'p', prob = 0.95)

# 3.3.1.46 psi (SP + Elevation), p (SP+INT_o) 
c46<- occMod(model = list(psi~SP+Elevation,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c46)
coef(object = c46, param = 'psi', prob = 0.95)
coef(object = c46, param = 'p', prob = 0.95)

# 3.3.1.47 psi (SP + Elevation), p (SP+INT_d) 
c47<- occMod(model = list(psi~SP+Elevation,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c47)
coef(object = c47, param = 'psi', prob = 0.95)
coef(object = c47, param = 'p', prob = 0.95)

# 3.3.1.48 psi (SP + Slope), p (SP) 
c48<- occMod(model = list(psi~SP+Slope,p~SP), data=Twosp, type="so.2sp.1")  
summary(c48)
coef(object = c48, param = 'psi', prob = 0.95)
coef(object = c48, param = 'p', prob = 0.95)

# 3.3.1.49 psi (SP + Slope), p (SP+INT_o) 
c49<- occMod(model = list(psi~SP+Slope,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c49)
coef(object = c49, param = 'psi', prob = 0.95)
coef(object = c49, param = 'p', prob = 0.95)

# 3.3.1.50 psi (SP + Slope), p (SP+INT_d) 
c50<- occMod(model = list(psi~SP+Slope,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c50)
coef(object = c50, param = 'psi', prob = 0.95)
coef(object = c50, param = 'p', prob = 0.95)

# 3.3.1.51 psi (SP + Metalled road), p (SP) 
c51<- occMod(model = list(psi~SP+Metalled_road,p~SP), data=Twosp, type="so.2sp.1")  
summary(c51)
coef(object = c51, param = 'psi', prob = 0.95)
coef(object = c51, param = 'p', prob = 0.95)

# 3.3.1.52 psi (SP + Metalled road), p (SP+INT_o) 
c52<- occMod(model = list(psi~SP+Metalled_road,p~SP+INT_o), data=Twosp,
             type="so.2sp.1")  
summary(c52)
coef(object = c52, param = 'psi', prob = 0.95)
coef(object = c52, param = 'p', prob = 0.95)

# 3.3.1.53 psi (SP + Metalled road), p (SP+INT_d) 
c53<- occMod(model = list(psi~SP+Metalled_road,p~SP+INT_d), data=Twosp,
             type="so.2sp.1")  
summary(c53)
coef(object = c53, param = 'psi', prob = 0.95)
coef(object = c53, param = 'p', prob = 0.95)

# 3.3.1.54 psi (SP + Dirt road), p (SP) 
c54<- occMod(model = list(psi~SP+Dirt_road,p~SP), data=Twosp, type="so.2sp.1")  
summary(c54)
coef(object = c54, param = 'psi', prob = 0.95)
coef(object = c54, param = 'p', prob = 0.95)

# 3.3.1.55 psi (SP + Dirt road), p (SP+INT_o) 
c55<- occMod(model = list(psi~SP+Dirt_road,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c55)
coef(object = c55, param = 'psi', prob = 0.95)
coef(object = c55, param = 'p', prob = 0.95)

# 3.3.1.56 psi (SP + Dirt road), p (SP+INT_d) 
c56<- occMod(model = list(psi~SP+Dirt_road,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c56)
coef(object = c56, param = 'psi', prob = 0.95)
coef(object = c56, param = 'p', prob = 0.95)

# 3.3.1.57 psi (SP + No road), p (SP) 
c57<- occMod(model = list(psi~SP+No_road,p~SP), data=Twosp, type="so.2sp.1")  
summary(c57)
coef(object = c57, param = 'psi', prob = 0.95)
coef(object = c57, param = 'p', prob = 0.95)

# 3.3.1.58 psi (SP + No road), p (SP+INT_o) 
c58<- occMod(model = list(psi~SP+No_road,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c58)
coef(object = c58, param = 'psi', prob = 0.95)
coef(object = c58, param = 'p', prob = 0.95)

# 3.3.1.59 psi (SP + No road), p (SP+INT_d) 
c59<- occMod(model = list(psi~SP+No_road,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c59)
coef(object = c59, param = 'psi', prob = 0.95)
coef(object = c59, param = 'p', prob = 0.95)

# 3.3.1.60 psi (SP + Lentic), p (SP) 
c60<- occMod(model = list(psi~SP+Lentic,p~SP), data=Twosp, type="so.2sp.1")  
summary(c60)
coef(object = c60, param = 'psi', prob = 0.95)
coef(object = c60, param = 'p', prob = 0.95)

# 3.3.1.61 psi (SP + Lentic), p (SP+INT_o) 
c61<- occMod(model = list(psi~SP+Lentic,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c61)
coef(object = c61, param = 'psi', prob = 0.95)
coef(object = c61, param = 'p', prob = 0.95)

# 3.3.1.62 psi (SP + Lentic), p (SP+INT_d) 
c62<- occMod(model = list(psi~SP+Lentic,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c62)
coef(object = c62, param = 'psi', prob = 0.95)
coef(object = c62, param = 'p', prob = 0.95)

# 3.3.1.63 psi (SP + Wet lotic), p (SP) 
c63<- occMod(model = list(psi~SP+Wet_lotic,p~SP), data=Twosp, type="so.2sp.1")  
summary(c63)
coef(object = c63, param = 'psi', prob = 0.95)
coef(object = c63, param = 'p', prob = 0.95)

# 3.3.1.64 psi (SP + Wet lotic), p (SP+INT_o) 
c64<- occMod(model = list(psi~SP+Wet_lotic,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c64)
coef(object = c64, param = 'psi', prob = 0.95)
coef(object = c64, param = 'p', prob = 0.95)

# 3.3.1.65 psi (SP + Wet lotic), p (SP+INT_d) 
c65<- occMod(model = list(psi~SP+Wet_lotic,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c65)
coef(object = c65, param = 'psi', prob = 0.95)
coef(object = c65, param = 'p', prob = 0.95)

# 3.3.1.66 psi (SP + Dry lotic), p (SP) 
c66<- occMod(model = list(psi~SP+Dry_lotic,p~SP), data=Twosp, type="so.2sp.1")  
summary(c66)
coef(object = c66, param = 'psi', prob = 0.95)
coef(object = c66, param = 'p', prob = 0.95)

# 3.3.1.67 psi (SP + Dry lotic), p (SP+INT_o) 
c67<- occMod(model = list(psi~SP+Dry_lotic,p~SP+INT_o), data=Twosp, type="so.2sp.1")  
summary(c67)
coef(object = c67, param = 'psi', prob = 0.95)
coef(object = c67, param = 'p', prob = 0.95)

# 3.3.1.68 psi (SP + Dry lotic), p (SP+INT_d) 
c68<- occMod(model = list(psi~SP+Dry_lotic,p~SP+INT_d), data=Twosp, type="so.2sp.1")  
summary(c68)
coef(object = c68, param = 'psi', prob = 0.95)
coef(object = c68, param = 'p', prob = 0.95)

# 3.3.1.69 psi (SP + Rubus niveus), p (SP) 
c69<- occMod(model = list(psi~SP+Rubus_niveus,p~SP), data=Twosp, type="so.2sp.1")  
summary(c69)
coef(object = c69, param = 'psi', prob = 0.95)
coef(object = c69, param = 'p', prob = 0.95)

# 3.3.1.70 psi (SP + Rubus niveus), p (SP+INT_o) 
c70<- occMod(model = list(psi~SP+Rubus_niveus,p~SP+INT_o), data=Twosp,
             type="so.2sp.1")  
summary(c70)
coef(object = c70, param = 'psi', prob = 0.95)
coef(object = c70, param = 'p', prob = 0.95)

# 3.3.1.71 psi (SP + Rubus niveus), p (SP+INT_d) 
c71<- occMod(model = list(psi~SP+Rubus_niveus,p~SP+INT_d), data=Twosp,
             type="so.2sp.1")  
summary(c71)
coef(object = c71, param = 'psi', prob = 0.95)
coef(object = c71, param = 'p', prob = 0.95)

# 3.3.1.72 psi (SP + Lantana camara), p (SP) 
c72<- occMod(model = list(psi~SP+Lantana_camara,p~SP), data=Twosp, type="so.2sp.1")  
summary(c72)
coef(object = c72, param = 'psi', prob = 0.95)
coef(object = c72, param = 'p', prob = 0.95)

#Sus. Note the very low convergence. Not acceptable.

# 3.3.1.73 psi (SP + Lantana camara), p (SP+INT_o) 
c73<- occMod(model = list(psi~SP+Lantana_camara,p~SP+INT_o), data=Twosp,
             type="so.2sp.1")  
summary(c73)
coef(object = c73, param = 'psi', prob = 0.95)
coef(object = c73, param = 'p', prob = 0.95)

#Sus. Note the very low convergence. Not acceptable.

# 3.3.1.74 psi (SP + Lantana camara), p (SP+INT_d) 
c74<- occMod(model = list(psi~SP+Lantana_camara,p~SP+INT_d), data=Twosp,
             type="so.2sp.1")  
summary(c74)
coef(object = c74, param = 'psi', prob = 0.95)
coef(object = c74, param = 'p', prob = 0.95)

#Sus. Note the low convergence. Barely acceptable.

#Now, create models using survey-specific covariates:

# 3.3.1.75 psi (SP), p (SP + Before noon) 
c75<- occMod(model = list(psi~SP,p~SP+BefNoon), data=Twosp, type="so.2sp.1")  
summary(c75)
coef(object = c75, param = 'psi', prob = 0.95)
coef(object = c75, param = 'p', prob = 0.95)

# 3.3.1.76 psi (SP), p (SP+INT_o+Before noon) 
c76<- occMod(model = list(psi~SP,p~SP+INT_o+BefNoon), data=Twosp, type="so.2sp.1")  
summary(c76)
coef(object = c76, param = 'psi', prob = 0.95)
coef(object = c76, param = 'p', prob = 0.95)

# 3.3.1.77 psi (SP), p (SP+INT_d+Before noon) 
c77<- occMod(model = list(psi~SP,p~SP+INT_d+BefNoon), data=Twosp, type="so.2sp.1")  
summary(c77)
coef(object = c77, param = 'psi', prob = 0.95)
coef(object = c77, param = 'p', prob = 0.95)

#Check what happens to the AICc when site and survey-specific covariates are used...
#...together. Create some such models with Area as site-specific covariate (since...
#...it's the covariate in the top models run in the model set "models_tsp1":

# 3.3.1.78 psi (SP + Area), p (SP + Before noon) 
c78<- occMod(model = list(psi~SP+Area,p~SP+BefNoon), data=Twosp, type="so.2sp.1")  
summary(c78)
coef(object = c78, param = 'psi', prob = 0.95)
coef(object = c78, param = 'p', prob = 0.95)

# 3.3.1.79 psi (SP + Area), p (SP+INT_o+Before noon) 
c79<- occMod(model = list(psi~SP+Area,p~SP+INT_o+BefNoon), data=Twosp,
             type="so.2sp.1")  
summary(c79)
coef(object = c79, param = 'psi', prob = 0.95)
coef(object = c79, param = 'p', prob = 0.95)

# 3.3.1.80 psi (SP + Area), p (SP+INT_d+Before noon) 
c80<- occMod(model = list(psi~SP+Area,p~SP+INT_d+BefNoon), data=Twosp,
             type="so.2sp.1")  
summary(c80)
coef(object = c80, param = 'psi', prob = 0.95)
coef(object = c80, param = 'p', prob = 0.95)

#Create AICc table using the models so far (except the excluded ones till # 3.3.1.36)...
#...and other ones with poor convergence of values:
models_tsp2 = list(c1,c2,c4,c24,c25,c26,c28,c29,c30,c32,c33,c34,c36,c37,c38,c39,
                  c40,c41,c42,c43,c44,c45,c46,c47,c48,c49,c50,c51,c52,c53,c54,
                  c55,c56,c57,c58,c59,c60,c61,c62,c63,c64,c65,c66,c67,c68,c69,
                  c70,c71,c74,c75,c76,c77,c78,c79,c80)
results_tsp2<-createAicTable(models_tsp2, use.aicc=TRUE)

#show the table:
kable(results_tsp2$table, "pipe")

#Since most models above DAICc 7 are simple models containing Elevation, it...
#...would be useless to run very complex models with both site- and survey-specific...
#...covariates. Create simple models:

# 3.3.1.81 psi (SP), p (SP + After noon) 
c81<- occMod(model = list(psi~SP,p~SP+AftNoon), data=Twosp, type="so.2sp.1")  
summary(c81)
coef(object = c81, param = 'psi', prob = 0.95)
coef(object = c81, param = 'p', prob = 0.95)

# 3.3.1.82 psi (SP), p (SP+INT_o+After noon) 
c82<- occMod(model = list(psi~SP,p~SP+INT_o+AftNoon), data=Twosp, type="so.2sp.1")  
summary(c82)
coef(object = c82, param = 'psi', prob = 0.95)
coef(object = c82, param = 'p', prob = 0.95)

# 3.3.1.83 psi (SP), p (SP+INT_d+After noon) 
c83<- occMod(model = list(psi~SP,p~SP+INT_d+AftNoon), data=Twosp, type="so.2sp.1")  
summary(c83)
coef(object = c83, param = 'psi', prob = 0.95)
coef(object = c83, param = 'p', prob = 0.95)

# 3.3.1.84 psi (SP), p (SP + Clear sky) 
c84<- occMod(model = list(psi~SP,p~SP+ClrSky), data=Twosp, type="so.2sp.1")  
summary(c84)
coef(object = c84, param = 'psi', prob = 0.95)
coef(object = c84, param = 'p', prob = 0.95)

# 3.3.1.85 psi (SP), p (SP+INT_o+Clear sky) 
c85<- occMod(model = list(psi~SP,p~SP+INT_o+ClrSky), data=Twosp, type="so.2sp.1")  
summary(c85)
coef(object = c85, param = 'psi', prob = 0.95)
coef(object = c85, param = 'p', prob = 0.95)

# 3.3.1.86 psi (SP), p (SP+INT_d+Clear sky) 
c86<- occMod(model = list(psi~SP,p~SP+INT_d+ClrSky), data=Twosp, type="so.2sp.1")  
summary(c86)
coef(object = c86, param = 'psi', prob = 0.95)
coef(object = c86, param = 'p', prob = 0.95)

# 3.3.1.87 psi (SP), p (SP + Overcast) 
c87<- occMod(model = list(psi~SP,p~SP+Ovrcst), data=Twosp, type="so.2sp.1")  
summary(c87)
coef(object = c87, param = 'psi', prob = 0.95)
coef(object = c87, param = 'p', prob = 0.95)

# 3.3.1.88 psi (SP), p (SP+INT_o+Overcast) 
c88<- occMod(model = list(psi~SP,p~SP+INT_o+Ovrcst), data=Twosp, type="so.2sp.1")  
summary(c88)
coef(object = c88, param = 'psi', prob = 0.95)
coef(object = c88, param = 'p', prob = 0.95)

# 3.3.1.89 psi (SP), p (SP+INT_d+Overcast) 
c89<- occMod(model = list(psi~SP,p~SP+INT_d+Ovrcst), data=Twosp, type="so.2sp.1")  
summary(c89)
coef(object = c89, param = 'psi', prob = 0.95)
coef(object = c89, param = 'p', prob = 0.95)

# 3.3.1.90 psi (SP), p (SP + Sunny) 
c90<- occMod(model = list(psi~SP,p~SP+Sunny), data=Twosp, type="so.2sp.1")  
summary(c90)
coef(object = c90, param = 'psi', prob = 0.95)
coef(object = c90, param = 'p', prob = 0.95)

# 3.3.1.91 psi (SP), p (SP+INT_o+Sunny) 
c91<- occMod(model = list(psi~SP,p~SP+INT_o+Sunny), data=Twosp, type="so.2sp.1")  
summary(c91)
coef(object = c91, param = 'psi', prob = 0.95)
coef(object = c91, param = 'p', prob = 0.95)

# 3.3.1.92 psi (SP), p (SP+INT_d+Sunny) 
c92<- occMod(model = list(psi~SP,p~SP+INT_d+Sunny), data=Twosp, type="so.2sp.1")  
summary(c92)
coef(object = c92, param = 'psi', prob = 0.95)
coef(object = c92, param = 'p', prob = 0.95)

# 3.3.1.93 psi (SP), p (SP + Windy) 
c93<- occMod(model = list(psi~SP,p~SP+Windy), data=Twosp, type="so.2sp.1")  
summary(c93)
coef(object = c93, param = 'psi', prob = 0.95)
coef(object = c93, param = 'p', prob = 0.95)

# 3.3.1.94 psi (SP), p (SP+INT_o+Windy) 
c94<- occMod(model = list(psi~SP,p~SP+INT_o+Windy), data=Twosp, type="so.2sp.1")  
summary(c94)
coef(object = c94, param = 'psi', prob = 0.95)
coef(object = c94, param = 'p', prob = 0.95)

# 3.3.1.95 psi (SP), p (SP+INT_d+Windy) 
c95<- occMod(model = list(psi~SP,p~SP+INT_d+Windy), data=Twosp, type="so.2sp.1")  
summary(c95)
coef(object = c95, param = 'psi', prob = 0.95)
coef(object = c95, param = 'p', prob = 0.95)

# 3.3.1.96 psi (SP), p (SP + Warm) 
c96<- occMod(model = list(psi~SP,p~SP+Warm), data=Twosp, type="so.2sp.1")  
summary(c96)
coef(object = c96, param = 'psi', prob = 0.95)
coef(object = c96, param = 'p', prob = 0.95)

# 3.3.1.97 psi (SP), p (SP+INT_o+Warm) 
c97<- occMod(model = list(psi~SP,p~SP+INT_o+Warm), data=Twosp, type="so.2sp.1")  
summary(c97)
coef(object = c97, param = 'psi', prob = 0.95)
coef(object = c97, param = 'p', prob = 0.95)

# 3.3.1.98 psi (SP), p (SP+INT_d+Warm) 
c98<- occMod(model = list(psi~SP,p~SP+INT_d+Warm), data=Twosp, type="so.2sp.1")  
summary(c98)
coef(object = c98, param = 'psi', prob = 0.95)
coef(object = c98, param = 'p', prob = 0.95)

# 3.3.1.99 psi (SP), p (SP + Neutral) 
c99<- occMod(model = list(psi~SP,p~SP+Neutral), data=Twosp, type="so.2sp.1")  
summary(c99)
coef(object = c99, param = 'psi', prob = 0.95)
coef(object = c99, param = 'p', prob = 0.95)

# 3.3.1.100 psi (SP), p (SP+INT_o+Neutral) 
c100<- occMod(model = list(psi~SP,p~SP+INT_o+Neutral), data=Twosp, type="so.2sp.1")  
summary(c100)
coef(object = c100, param = 'psi', prob = 0.95)
coef(object = c100, param = 'p', prob = 0.95)

# 3.3.1.101 psi (SP), p (SP+INT_d+Neutral) 
c101<- occMod(model = list(psi~SP,p~SP+INT_d+Neutral), data=Twosp, type="so.2sp.1")  
summary(c101)
coef(object = c101, param = 'psi', prob = 0.95)
coef(object = c101, param = 'p', prob = 0.95)

# 3.3.1.102 psi (SP), p (SP + Cool) 
c102<- occMod(model = list(psi~SP,p~SP+Cool), data=Twosp, type="so.2sp.1")  
summary(c102)
coef(object = c102, param = 'psi', prob = 0.95)
coef(object = c102, param = 'p', prob = 0.95)

# 3.3.1.103 psi (SP), p (SP+INT_o+Cool) 
c103<- occMod(model = list(psi~SP,p~SP+INT_o+Cool), data=Twosp, type="so.2sp.1")  
summary(c103)
coef(object = c103, param = 'psi', prob = 0.95)
coef(object = c103, param = 'p', prob = 0.95)

# 3.3.1.104 psi (SP), p (SP+INT_d+Cool) 
c104<- occMod(model = list(psi~SP,p~SP+INT_d+Cool), data=Twosp, type="so.2sp.1")  
summary(c104)
coef(object = c104, param = 'psi', prob = 0.95)
coef(object = c104, param = 'p', prob = 0.95)

# 3.3.1.105 psi (SP), p (SP + Visibility) 
c105<- occMod(model = list(psi~SP,p~SP+Visibility), data=Twosp, type="so.2sp.1")  
summary(c105)
coef(object = c105, param = 'psi', prob = 0.95)
coef(object = c105, param = 'p', prob = 0.95)

# 3.3.1.106 psi (SP), p (SP+INT_o+Visibility) 
c106<- occMod(model = list(psi~SP,p~SP+INT_o+Visibility), data=Twosp, type="so.2sp.1")  
summary(c106)
coef(object = c106, param = 'psi', prob = 0.95)
coef(object = c106, param = 'p', prob = 0.95)

# 3.3.1.107 psi (SP), p (SP+INT_d+Visibility) 
c107<- occMod(model = list(psi~SP,p~SP+INT_d+Visibility), data=Twosp, type="so.2sp.1")  
summary(c107)
coef(object = c107, param = 'psi', prob = 0.95)
coef(object = c107, param = 'p', prob = 0.95)

#Create AICc model set:
models_tsp3 = list(c1,c2,c4,c24,c25,c26,c28,c29,c30,c32,c33,c34,c36,c37,c38,c39,
                   c40,c41,c42,c43,c44,c45,c46,c47,c48,c49,c50,c51,c52,c53,c54,
                   c55,c56,c57,c58,c59,c60,c61,c62,c63,c64,c65,c66,c67,c68,c69,
                   c70,c71,c74,c75,c76,c77,c78,c79,c80,c81,c82,c83,c84,c85,c86,
                   c87,c88,c89,c90,c91,c92,c93,c94,c95,c96,c97,c98,c99,c100,c101,
                   c102,c103,c104,c105,c106,c107)
results_tsp3<-createAicTable(models_tsp3, use.aicc=TRUE)

#show the table:
kable(results_tsp3$table, "pipe")

#Only the models: psi(SP+Elevation)p(SP); psi(SP+Elevation)p(SP+INT_d);...
#...psi(SP+Elevation)p(SP+INT_o);psi(SP+Lantana_camara)p(SP+INT_d); and... 
#...psi(SP+No_road)p(SP) fall within DAICc of 7. 

#We have 27 possibilities of occupancy for species A and B. In co-occurence models...
#...encounter history is compressed into 0,1,2, & 3 with the data for both species...
#...occupying the same lines in the detection-history matrix. The 4 possibilities...
#...in the encounter/detection history instead of the 2 in single species models...
#...makes it possible to estimate extra parameters. So, we can have more number of...
#...covariates than what is recommended in single species models. Let's limit it...
#...4 for psi and 8 for p. Try some models that, with top survey- and site-...
#...specific covariates in the top models of the last model set ("models_tsp3"):

# 3.3.1.108 psi (SP + Elevation), p (SP+INT_d+BefNoon) 
c108<- occMod(model = list(psi~SP+Elevation,p~SP+INT_d+BefNoon), data=Twosp,
              type="so.2sp.1")  
summary(c108)
coef(object = c108, param = 'psi', prob = 0.95)
coef(object = c108, param = 'p', prob = 0.95)

# 3.3.1.109 psi (SP + Elevation + No road), p (SP+INT_d+BefNoon) 
c109<- occMod(model = list(psi~SP+Elevation+No_road,p~SP+INT_d+BefNoon), data=Twosp,
              type="so.2sp.1")  
summary(c109)
coef(object = c109, param = 'psi', prob = 0.95)
coef(object = c109, param = 'p', prob = 0.95)

# 3.3.1.110 psi (SP + Area + No road), p (SP+INT_d+BefNoon) 
c110<- occMod(model = list(psi~SP+Area+No_road,p~SP+INT_d+BefNoon), data=Twosp,
              type="so.2sp.1")  
summary(c110)
coef(object = c110, param = 'psi', prob = 0.95)
coef(object = c110, param = 'p', prob = 0.95)

# 3.3.1.111 psi (SP + Elevation + Area), p (SP+INT_d+BefNoon) 
c111<- occMod(model = list(psi~SP+Elevation+Area,p~SP+INT_d+BefNoon),
              data=Twosp, type="so.2sp.1")  
summary(c111)
coef(object = c111, param = 'psi', prob = 0.95)
coef(object = c111, param = 'p', prob = 0.95)

# 3.3.1.112 psi (SP + Elevation + Wet lotic), p (SP+INT_d+BefNoon) 
c112<- occMod(model = list(psi~SP+Elevation+Wet_lotic,p~SP+INT_d+BefNoon),
              data=Twosp, type="so.2sp.1")  
summary(c112)
coef(object = c112, param = 'psi', prob = 0.95)
coef(object = c112, param = 'p', prob = 0.95)

# 3.3.1.113 psi (SP + Elevation + No road), p (SP) 
c113<- occMod(model = list(psi~SP+Elevation+No_road,p~SP), data=Twosp,
              type="so.2sp.1")  
summary(c113)
coef(object = c113, param = 'psi', prob = 0.95)
coef(object = c113, param = 'p', prob = 0.95)

# 3.3.1.114 psi (SP + Area + No road), p (SP) 
c114<- occMod(model = list(psi~SP+Area+No_road,p~SP), data=Twosp, type="so.2sp.1")  
summary(c114)
coef(object = c114, param = 'psi', prob = 0.95)
coef(object = c114, param = 'p', prob = 0.95)

# 3.3.1.115 psi (SP + Elevation + Area), p (SP) 
c115<- occMod(model = list(psi~SP+Elevation+Area,p~SP), data=Twosp,
              type="so.2sp.1")  
summary(c115)
coef(object = c115, param = 'psi', prob = 0.95)
coef(object = c115, param = 'p', prob = 0.95)

# 3.3.1.116 psi (SP + Elevation + Wet lotic), p (SP) 
c116<- occMod(model = list(psi~SP+Elevation+Wet_lotic,p~SP),data=Twosp,
              type="so.2sp.1")  
summary(c116)
coef(object = c116, param = 'psi', prob = 0.95)
coef(object = c116, param = 'p', prob = 0.95)

# 3.3.1.117 psi (SP + Elevation + Lantana camara), p (SP+INT_d) 
c117<- occMod(model = list(psi~SP+Elevation+Lantana_camara,p~SP+INT_d),data=Twosp,
              type="so.2sp.1")  
summary(c117)
coef(object = c117, param = 'psi', prob = 0.95)
coef(object = c117, param = 'p', prob = 0.95)

# 3.3.1.118 psi (SP + Elevation + Lantana camara), p (SP) 
c118<- occMod(model = list(psi~SP+Elevation+Lantana_camara,p~SP),data=Twosp,
              type="so.2sp.1")  
summary(c118)
coef(object = c118, param = 'psi', prob = 0.95)
coef(object = c118, param = 'p', prob = 0.95)

#Models involving the covariates 'SURVEY' was missed. So, that too will be carried...
#...out. #First Convert SURVEY to numeric:
Twosp$survcov$survey = as.numeric(Twosp$survcov$SURVEY)
list(Twosp$survcov$survey)

# 3.3.1.119 psi (SP), p (SP+survey)
c119<- occMod(model = list(psi~SP,p~SP+survey),data=Twosp,type="so.2sp.1")  
summary(c119)
coef(object = c119, param = 'psi', prob = 0.95)
coef(object = c119, param = 'p', prob = 0.95)

# 3.3.1.120 psi (SP + Elevation), p (SP+ survey)
c120<- occMod(model = list(psi~SP+Elevation,p~SP+survey),data=Twosp,type="so.2sp.1")  
summary(c120)
coef(object = c120, param = 'psi', prob = 0.95)
coef(object = c120, param = 'p', prob = 0.95)

# 3.3.1.121 psi (SP + Elevation), p (SP+INT_d+survey)
c121<- occMod(model = list(psi~SP+Elevation,p~SP+INT_d+survey),data=Twosp,
              type="so.2sp.1")  
summary(c121)
coef(object = c121, param = 'psi', prob = 0.95)
coef(object = c121, param = 'p', prob = 0.95)

# 3.3.1.122 psi (SP + Elevation), p (SP+INT_o+survey)
c122<- occMod(model = list(psi~SP+Elevation,p~SP+INT_o+survey),data=Twosp,
              type="so.2sp.1")  
summary(c122)
coef(object = c122, param = 'psi', prob = 0.95)
coef(object = c122, param = 'p', prob = 0.95)

# 3.3.1.123 psi (SP + Lantana camara), p (SP+INT_d+survey)
c123<- occMod(model = list(psi~SP+Lantana_camara,p~SP+INT_d+survey),data=Twosp,
              type="so.2sp.1")  
summary(c123)
coef(object = c123, param = 'psi', prob = 0.95)
coef(object = c123, param = 'p', prob = 0.95)

# 3.3.1.124 psi (SP + No road), p (SP+survey)
c124<- occMod(model = list(psi~SP+No_road,p~SP+survey),data=Twosp,type="so.2sp.1")  
summary(c124)
coef(object = c124, param = 'psi', prob = 0.95)
coef(object = c124, param = 'p', prob = 0.95)

#Create AICc model set (after removing most models found to more than DAICc 7 in...
#...the previous runs + c118 due to high convergence problem) along with the...
#...survey models:
models_tsp4 = list(c45,c46,c47,c57,c74,c108,c109,c111,c113,c115,c116,c117,c119,
                   c120,c121,c122,c123,c124)
results_tsp4<-createAicTable(models_tsp4, use.aicc=TRUE)

#show the table:
kable(results_tsp4$table, "pipe")

#Since a GOF test is not available for the 2 species model at the moment, we can...
#...use the c-hat value of the most common species among the two to create a...
#...QAICc table (just to be on the safer side of statistical validity). Test with...
#...all in previous list and then remove ones beyond DQAICc 7 (step not shown...
#...here):

#Create qAICc table to adjust the variance due to >1 c-hat:
qaicTsp <- createAicTable(
  mod.list = list(c45,c46,c47,c57,c74,c108,c109,c111,c113,c115,c116,c117,c119,
                  c120,c121,c122,c123,c124),
  use.aicc = TRUE,
  chat = MfBoot1$gof$chat
)

# print the table
kable(qaicTsp$table, "pipe")

#c119 is above DQAICc 7. After removal:
qaicTsp <- createAicTable(
  mod.list = list(c45,c46,c47,c57,c74,c108,c109,c111,c113,c115,c116,c117,c120,
                  c121,c122,c123,c124),
  use.aicc = TRUE,
  chat = MfBoot1$gof$chat
)

# print the table
kable(qaicTsp$table, "pipe")

#Write the results to a csv file:
write.csv(qaicTsp$table,
          "/media/Dissertation/Paper/Tsp_QAICc.csv")

#Co-occurence models without any interactive effects on occupancy or detection...
#...probabilities corroborates results from the single species models i.e. Elevation,...
#...No road, Area, survey, and Wet lotic are covariates that are likely affecting...
#...occupancy of both species, or at least one of the species.

#Since the bigger goal of co-occurrence models is to see any interactive effects,
#...reduce the models to only those with interactive effects on detection prob.: 
qaicTspf <- createAicTable(
  mod.list = list(c46,c47,c74,c108,c109,c111,c117,c121,c122,c123),
  use.aicc = TRUE,
  chat = MfBoot1$gof$chat
)

# print the table
kable(qaicTspf$table, "pipe")

#Write the results to a csv file:
write.csv(qaicTspf$table,
          "/media/Dissertation/Paper/Tspf_QAICc.csv")

#Save as word document:
qaicTspf_tbl <- flextable(qaicTspf$table)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = qaicTspf_tbl) %>%  #Insert the flextable object there
  print(target = "qaicTspf_tbl.docx")  

#Since we have an idea about psiA (pr(M. faibankii) being present, regardless of...
#...psi status of species S. albiventris, and vice versa (psiB) from the single...
#...species models, we don't need to concern ourselves with them. Same with pA...
#...and pB. The following parameters needs attention: psiBA, psiBa, rA, rBA & rBa...
#...(from those available from the default parameterisation. Calculation of other...
#...values and their SEs and CIs is complex).

#Model average the psi estimates:
psiBA_maTsp <- modAvg(qaicTspf, param = 'psiBA')
psiBa_maTsp <- modAvg(qaicTspf, param = 'psiBa')

#Show the results and summary:
kable(psiBA_maTsp, "pipe")
summary(psiBA_maTsp)
kable(psiBa_maTsp, "pipe")
summary(psiBa_maTsp)

#Note: As James Hines explains: "A subtle trait of the 2-species model is that...
#...when psiA is high, psiBa becomes more  difficult to estimate because there... 
#...will be few sites which are not occupied by species A. Similarly, if occupancy...
#...for species is very low, psiBA becomes more difficult to estimate as there...
#...are few sites occupied by species A. To summarize, if psiA is high, then you...
#...need more sites in order to get reasonable estimates for psiBa."

#Let's check psiA:
psiA_maTsp <- modAvg(qaicTspf, param = 'psiA')
kable(psiA_maTsp, "pipe")

str(qaicTspf)

#Model average the p and r estimates:
rA_maTsp <- modAvg(qaicTspf, param = 'rA')
rBA_maTsp <- modAvg(qaicTspf, param = 'rBA')
rBa_maTsp <- modAvg(qaicTspf, param = 'rBa')

#Show the results and summary:
kable(rA_maTsp, "pipe")
summary(rA_maTsp)
kable(rBA_maTsp, "pipe")
summary(rBA_maTsp)
kable(rBa_maTsp, "pipe")
summary(rBa_maTsp)

#Check similarity between the psiBA and psiBa (they should match 100% since there...
#...are not interactive effects on psi in any of the models in the final model set):
summary(psiBA_maTsp == psiBa_maTsp)
#It does match 100%.

#However, rBA and rBa shouldn't match 100% since there are interactive effects on...
#...detection probabilities:
summary(rBA_maTsp == rBa_maTsp)
#Match is 0%

#Mean of rA:
mean(rA_maTsp$est, na.rm = TRUE) # 0.6721908

#Range of rA:
min(rA_maTsp$est, na.rm = TRUE) # 0.5784086
max(rA_maTsp$est, na.rm = TRUE) # 0.7257911

#Range of SE of rA:
min(rA_maTsp$se, na.rm = TRUE) # 0.06471453
max(rA_maTsp$se, na.rm = TRUE) # 0.1222575

#Range of 95% CI of rA:
min(rA_maTsp$lower_0.95, na.rm = TRUE) # 0.339303
max(rA_maTsp$upper_0.95, na.rm = TRUE) # 0.86092

#Mean of rBA:
mean(rBA_maTsp$est, na.rm = TRUE) # 0.6368594

#Range of rBA:
min(rBA_maTsp$est, na.rm = TRUE) # 0.5237279
max(rBA_maTsp$est, na.rm = TRUE) # 0.6991368

#Range of SE of rBA:
min(rBA_maTsp$se, na.rm = TRUE) # 0.08823564
max(rBA_maTsp$se, na.rm = TRUE) # 0.1630434

#Range of 95% CI of rBA:
min(rBA_maTsp$lower_0.95, na.rm = TRUE) # 0.2339502
max(rBA_maTsp$upper_0.95, na.rm = TRUE) # 0.8558282

#Mean of rBa:
mean(rBa_maTsp$est, na.rm = TRUE) # 0.485799

#Range of rBa:
min(rBa_maTsp$est, na.rm = TRUE) # 0.2843787
max(rBa_maTsp$est, na.rm = TRUE) # 0.5778163

#Range of SE of rBa:
min(rBa_maTsp$se, na.rm = TRUE) # 0.130426
max(rBa_maTsp$se, na.rm = TRUE) # 0.719273

#Range of 95% CI of rBa:
min(rBa_maTsp$lower_0.95, na.rm = TRUE) # 0.0003957452
max(rBa_maTsp$upper_0.95, na.rm = TRUE) # 0.9974992

str(rA_maTsp)
str(rBA_maTsp)
str(rBa_maTsp)
#Add the site ID and survey number columns to these:
Tsp_rA <- cbind(dframe, rA_maTsp)
colnames(Tsp_rA)[1] <- "siteIDs"
colnames(Tsp_rA)[2] <- "Survey_number"
Tsp_rBA <- cbind(dframe, rBA_maTsp)
colnames(Tsp_rBA)[1] <- "siteIDs"
colnames(Tsp_rBA)[2] <- "Survey_number"
Tsp_rBa <- cbind(dframe, rBa_maTsp)
colnames(Tsp_rBa)[1] <- "siteIDs"
colnames(Tsp_rBa)[2] <- "Survey_number"
head(Tsp_rA)
head(Tsp_rBA)
head(Tsp_rBa)
str(Tsp_rA)
str(Tsp_rBA)
str(Tsp_rBa)

#Save as word document:
rTsp_rA <- setNames(Tsp_rA,cbind("Site IDs", "Survey no.", "Estimate",
                             "Standard Error", "Lower 95% Confidence Interval",
                             "Upper 95% Confidence Interval"))
rTsp_rBA <- setNames(Tsp_rBA,cbind("Site IDs", "Survey no.", "Estimate",
                             "Standard Error","Lower 95% Confidence Interval",
                             "Upper 95% Confidence Interval"))
rTsp_rBa <- setNames(Tsp_rBa,cbind("Site IDs", "Survey no.", "Estimate",
                             "Standard Error", "Lower 95% Confidence Interval",
                             "Upper 95% Confidence Interval"))
rTsp_rA <- flextable(rTsp_rA)
rTsp_rBA <- flextable(rTsp_rBA)
rTsp_rBa <- flextable(rTsp_rBa)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rTsp_rA) %>%  #Insert the flextable object there
  print(target = "rTsp_rA.docx")  
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rTsp_rBA) %>%  #Insert the flextable object there
  print(target = "rTsp_rBA.docx")
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rTsp_rBa) %>%  #Insert the flextable object there
  print(target = "rTsp_rBa.docx")  


###### 3.3.2 SM format: Species A = S. albiventris, Species B = M. fairbankii ######


#In this scenario of two species models, the models use S. albiventris as Species...
#...A (coded as 1), and M. fairbankii as Species B (coded as 2). 

# 3.3.2.1 Run the dot model with constant occupancy and detection:
nc1 <- occMod(model = list(psi~1,p~1), data=Tsp, type="so.2sp.1")
summary(nc1)
coef(object = nc1, param = 'psi', prob = 0.95)
coef(object = nc1, param = 'p', prob = 0.95)

#Look at the structure of the 'real' estimates from the output:
str(nc1$real, max.level = 2, give.attr = F)

#Look at the structure of the unconditional probabilities like psiB which has been...
#...converted from conditional probabilities like psiBA and psiBa: 
str(nc1$derived, max.level = 2, give.attr = F)

#Look at the unique probabilities for psi estimates:
lapply(nc1$real, FUN = unique)

#Look at the unique probabilities for detection estimates (for the first site):
print_one_site_estimates(nc1)

# 3.3.2.2 Run a model with species-specific detection and occupancy:
nc2<- occMod(model = list(psi~SP,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc2)
coef(object = nc2, param = 'psi', prob = 0.95)
coef(object = nc2, param = 'p', prob = 0.95)
str(nc2$derived, max.level = 2, give.attr = F)

# 3.3.2.3 Run a model with species-specific detection and occupancy as well as...
#...interaction effect on occupancy only:
nc3<- occMod(model = list(psi~SP+INT,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc3)
coef(object = nc3, param = 'psi', prob = 0.95)
coef(object = nc3, param = 'p', prob = 0.95)

# 3.3.2.4 Run a model with species-specific detection and occupancy as well as...
#...interaction of occupancy on detection (detection of a species is different...
#...in presence of the other species as opposed to when its not):
nc4<- occMod(model = list(psi~SP,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc4)
coef(object = nc4, param = 'psi', prob = 0.95)
coef(object = nc4, param = 'p', prob = 0.95)

# 3.3.2.5 Run a model with species-specific detection and occupancy as well as...
#...interaction of detection on detection (detection of a species is different...
#...when the other species is also detected as opposed to when its not):
nc5<- occMod(model = list(psi~SP,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc5)
coef(object = nc5, param = 'psi', prob = 0.95)
coef(object = nc5, param = 'p', prob = 0.95)
print_one_site_estimates(nc5)

# 3.3.2.6 Run a model with species-specific detection and occupancy; interaction...
#...effect on occupancy; and interaction of occupancy on detection:
nc6<- occMod(model = list(psi~SP+INT,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc6)
coef(object = nc6, param = 'psi', prob = 0.95)
coef(object = nc6, param = 'p', prob = 0.95)
str(nc6$derived, max.level = 2, give.attr = F)

# 3.3.2.7 Run a model with species-specific detection and occupancy; interaction...
#...effect on occupancy; interaction of occupancy on detection and occupancy:
nc7<- occMod(model = list(psi~SP+INT,p~SP+INT_o+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc7)
coef(object = nc7, param = 'psi', prob = 0.95)
coef(object = nc7, param = 'p', prob = 0.95)
str(nc7$derived, max.level = 2, give.attr = F)

#Now, run models with site-specific covariates:

# 3.3.2.8 psi (SP + INT + Area), p (SP) 
nc8<- occMod(model = list(psi~SP+INT+Area,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc8)
coef(object = nc8, param = 'psi', prob = 0.95)
coef(object = nc8, param = 'p', prob = 0.95)
str(nc8$derived, max.level = 2, give.attr = F)

# 3.3.2.9 psi (SP + INT + Area), p (SP+INT_o) 
nc9<- occMod(model = list(psi~SP+INT+Area,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc9)
coef(object = nc9, param = 'psi', prob = 0.95)
coef(object = nc9, param = 'p', prob = 0.95)
str(nc9$derived, max.level = 2, give.attr = F)

# 3.3.2.10 psi (SP + INT + Area), p (SP+INT_d) 
nc10<- occMod(model = list(psi~SP+INT+Area,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc10)
coef(object = nc10, param = 'psi', prob = 0.95)
coef(object = nc10, param = 'p', prob = 0.95)
str(nc10$derived, max.level = 2, give.attr = F)

# 3.3.2.11 psi (SP + INT + Area), p (ID) (here ID = SP+INT_o+INT_d)
nc11<- occMod(model = list(psi~SP+INT+Area,p~ID), data=Tsp, type="so.2sp.1")  
summary(nc11)
coef(object = nc11, param = 'psi', prob = 0.95)
coef(object = nc11, param = 'p', prob = 0.95)
str(nc11$derived, max.level = 2, give.attr = F)

#Notice the high SE, and 95%CI of detection probabilities (several times higher...
#...than the beta estimate. Mark all such models with "Sus".

# 3.3.2.12 psi (SP + INT + Canopy height), p (SP) 
nc12<- occMod(model = list(psi~SP+INT+Canopy_height,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc12)
coef(object = nc12, param = 'psi', prob = 0.95)
coef(object = nc12, param = 'p', prob = 0.95)
str(nc12$derived, max.level = 2, give.attr = F)

# 3.3.2.13 psi (SP + INT + Canopy height), p (SP+INT_o) 
nc13<- occMod(model = list(psi~SP+INT+Canopy_height,p~SP+INT_o), data=Tsp,
              type="so.2sp.1")  
summary(nc13)
coef(object = nc13, param = 'psi', prob = 0.95)
coef(object = nc13, param = 'p', prob = 0.95)
str(nc13$derived, max.level = 2, give.attr = F)

# 3.3.2.14 psi (SP + INT + Canopy height), p (SP+INT_d) 
nc14<- occMod(model = list(psi~SP+INT+Canopy_height,p~SP+INT_d), data=Tsp,
              type="so.2sp.1")  
summary(nc14)
coef(object = nc14, param = 'psi', prob = 0.95)
coef(object = nc14, param = 'p', prob = 0.95)
str(nc14$derived, max.level = 2, give.attr = F)

# 3.3.2.15 psi (SP + INT + Canopy height), p (ID)
nc15<- occMod(model = list(psi~SP+INT+Canopy_height,p~ID), data=Tsp, type="so.2sp.1")  
summary(nc15)
coef(object = nc15, param = 'psi', prob = 0.95)
coef(object = nc15, param = 'p', prob = 0.95)
str(nc15$derived, max.level = 2, give.attr = F)

#Sus. SE several times higher than beta estimate.

# 3.3.2.16 psi (SP + INT + Understorey height), p (SP) 
nc16<- occMod(model = list(psi~SP+INT+Understorey_height,p~SP), data=Tsp,
              type="so.2sp.1")  
summary(nc16)
coef(object = nc16, param = 'psi', prob = 0.95)
coef(object = nc16, param = 'p', prob = 0.95)
str(nc16$derived, max.level = 2, give.attr = F)

# 3.3.2.17 psi (SP + INT + Understorey height), p (SP+INT_o) 
nc17<- occMod(model = list(psi~SP+INT+Understorey_height,p~SP+INT_o), data=Tsp,
              type="so.2sp.1")  
summary(nc17)
coef(object = nc17, param = 'psi', prob = 0.95)
coef(object = nc17, param = 'p', prob = 0.95)
str(nc17$derived, max.level = 2, give.attr = F)

# 3.3.2.18 psi (SP + INT + Understorey height), p (SP+INT_d) 
nc18<- occMod(model = list(psi~SP+INT+Understorey_height,p~SP+INT_d), data=Tsp,
              type="so.2sp.1")  
summary(nc18)
coef(object = nc18, param = 'psi', prob = 0.95)
coef(object = nc18, param = 'p', prob = 0.95)
str(nc18$derived, max.level = 2, give.attr = F)

# 3.3.2.19 psi (SP + INT + Understorey height), p (ID)
nc19<- occMod(model = list(psi~SP+INT+Understorey_height,p~ID), data=Tsp,
              type="so.2sp.1")  
summary(nc19)
coef(object = nc19, param = 'psi', prob = 0.95)
coef(object = nc19, param = 'p', prob = 0.95)
str(nc19$derived, max.level = 2, give.attr = F)

#Sus. SE several times higher than beta estimate.

# 3.3.2.20 psi (SP + INT + Visibility), p (SP) 
nc20<- occMod(model = list(psi~SP+INT+Visibility,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc20)
coef(object = nc20, param = 'psi', prob = 0.95)
coef(object = nc20, param = 'p', prob = 0.95)
str(nc20$derived, max.level = 2, give.attr = F)

# 3.3.2.21 psi (SP + INT + Visibility), p (SP+INT_o) 
nc21<- occMod(model = list(psi~SP+INT+Visibility,p~SP+INT_o), data=Tsp,
              type="so.2sp.1")  
summary(nc21)
coef(object = nc21, param = 'psi', prob = 0.95)
coef(object = nc21, param = 'p', prob = 0.95)
str(nc21$derived, max.level = 2, give.attr = F)

# 3.3.2.22 psi (SP + INT + Visibility), p (SP+INT_d) 
nc22<- occMod(model = list(psi~SP+INT+Visibility,p~SP+INT_d), data=Tsp,
              type="so.2sp.1")  
summary(nc22)
coef(object = nc22, param = 'psi', prob = 0.95)
coef(object = nc22, param = 'p', prob = 0.95)
str(nc22$derived, max.level = 2, give.attr = F)

# 3.3.2.23 psi (SP + INT + Visibility), p (ID)
nc23<- occMod(model = list(psi~SP+INT+Visibility,p~ID), data=Tsp, type="so.2sp.1")  
summary(nc23)
coef(object = nc23, param = 'psi', prob = 0.95)
coef(object = nc23, param = 'p', prob = 0.95)
str(nc23$derived, max.level = 2, give.attr = F)

#Sus. SE several times higher than beta estimate.

# 3.3.2.24 psi (SP + Area), p (SP) 
nc24<- occMod(model = list(psi~SP+Area,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc24)
coef(object = nc24, param = 'psi', prob = 0.95)
coef(object = nc24, param = 'p', prob = 0.95)
print_one_site_estimates(nc24)

# 3.3.2.25 psi (SP + Area), p (SP+INT_o) 
nc25<- occMod(model = list(psi~SP+Area,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc25)
coef(object = nc25, param = 'psi', prob = 0.95)
coef(object = nc25, param = 'p', prob = 0.95)
print_one_site_estimates(nc25)

# 3.3.2.26 psi (SP + Area), p (SP+INT_d) 
nc26<- occMod(model = list(psi~SP+Area,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc26)
coef(object = nc26, param = 'psi', prob = 0.95)
coef(object = nc26, param = 'p', prob = 0.95)
print_one_site_estimates(nc26)

# 3.3.2.27 psi (SP + Area), p (ID)
nc27<- occMod(model = list(psi~SP+Area,p~ID), data=Tsp, type="so.2sp.1")  
summary(nc27)
coef(object = nc27, param = 'psi', prob = 0.95)
coef(object = nc27, param = 'p', prob = 0.95)
print_one_site_estimates(nc27)

# 3.3.2.28 psi (SP + Canopy height), p (SP) 
nc28<- occMod(model = list(psi~SP+Canopy_height,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc28)
coef(object = nc28, param = 'psi', prob = 0.95)
coef(object = nc28, param = 'p', prob = 0.95)
print_one_site_estimates(nc28)

# 3.3.2.29 psi (SP + Canopy height), p (SP+INT_o) 
nc29<- occMod(model = list(psi~SP+Canopy_height,p~SP+INT_o), data=Tsp,
              type="so.2sp.1")  
summary(nc29)
coef(object = nc29, param = 'psi', prob = 0.95)
coef(object = nc29, param = 'p', prob = 0.95)
print_one_site_estimates(nc29)

# 3.3.2.30 psi (SP + Canopy height), p (SP+INT_d) 
nc30<- occMod(model = list(psi~SP+Canopy_height,p~SP+INT_d), data=Tsp,
              type="so.2sp.1")  
summary(nc30)
coef(object = nc30, param = 'psi', prob = 0.95)
coef(object = nc30, param = 'p', prob = 0.95)
print_one_site_estimates(nc30)

# 3.3.2.31 psi (SP + Canopy height), p (ID)
nc31<- occMod(model = list(psi~SP+Canopy_height,p~ID), data=Tsp, type="so.2sp.1")  
summary(nc31)
coef(object = nc31, param = 'psi', prob = 0.95)
coef(object = nc31, param = 'p', prob = 0.95)
print_one_site_estimates(nc31)

#Sus (note beta estimate of pB)

# 3.3.2.32 psi (SP + Understorey height), p (SP) 
nc32<- occMod(model = list(psi~SP+Understorey_height,p~SP), data=Tsp,
              type="so.2sp.1")  
summary(nc32)
coef(object = nc32, param = 'psi', prob = 0.95)
coef(object = nc32, param = 'p', prob = 0.95)
print_one_site_estimates(nc32)

# 3.3.2.33 psi (SP + Understorey height), p (SP+INT_o) 
nc33<- occMod(model = list(psi~SP+Understorey_height,p~SP+INT_o), data=Tsp,
              type="so.2sp.1")  
summary(nc33)
coef(object = nc33, param = 'psi', prob = 0.95)
coef(object = nc33, param = 'p', prob = 0.95)
print_one_site_estimates(nc33)

# 3.3.2.34 psi (SP + Understorey height), p (SP+INT_d) 
nc34<- occMod(model = list(psi~SP+Understorey_height,p~SP+INT_d), data=Tsp,
              type="so.2sp.1")  
summary(nc34)
coef(object = nc34, param = 'psi', prob = 0.95)
coef(object = nc34, param = 'p', prob = 0.95)
print_one_site_estimates(nc34)

# 3.3.2.35 psi (SP + Understorey height), p (ID)
nc35<- occMod(model = list(psi~SP+Understorey_height,p~ID), data=Tsp,
              type="so.2sp.1")  
summary(nc35)
coef(object = nc35, param = 'psi', prob = 0.95)
coef(object = nc35, param = 'p', prob = 0.95)
print_one_site_estimates(nc35)

#Sus

#Check the AICc values for the models:
models_tsp1n = list(nc1,nc2,nc3,nc4,nc5,nc6,nc7,nc8,nc9,nc10,nc11,nc12,nc13,nc14,nc15,nc16,nc17,nc18,
                    nc19,nc20,nc21,nc22,nc23,nc24,nc25,nc26,nc27,nc28,nc29,nc30,nc31,nc32,nc33,nc34,
                    nc35)
results_tsp1n <-createAicTable(models_tsp1n, use.aicc=TRUE)

# show the table
kable(results_tsp1n$table, "pipe")

#since the ones with ID (SP+INT_o+INT_d) almost consistently fall outside of the...
#...DAICc of 7 (mostly due to the no. of parameters), and also since they can't...
#...correctly calculate pB, they can removed. Similarly all models with INT as...
#...a factor of psi can also be removed since it always miscalculates psiBa.
#Also note the convergence warnings ("warn.conv") for some of these models. They...
#...barely make it.

#To avoid creating useless models, do not create models containing INT as a factor...
#...of psi, and ID as factor of p.

# 3.3.2.36 psi (SP + Visibility), p (SP) 
nc36<- occMod(model = list(psi~SP+Visibility,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc36)
coef(object = nc36, param = 'psi', prob = 0.95)
coef(object = nc36, param = 'p', prob = 0.95)
print_one_site_estimates(nc36)

# 3.3.2.37 psi (SP + Visibility), p (SP+INT_o) 
nc37<- occMod(model = list(psi~SP+Visibility,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc37)
coef(object = nc37, param = 'psi', prob = 0.95)
coef(object = nc37, param = 'p', prob = 0.95)
print_one_site_estimates(nc37)

# 3.3.2.38 psi (SP + Visibility), p (SP+INT_d) 
nc38<- occMod(model = list(psi~SP+Visibility,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc38)
coef(object = nc38, param = 'psi', prob = 0.95)
coef(object = nc38, param = 'p', prob = 0.95)
print_one_site_estimates(nc38)

# 3.3.2.39 psi (SP + Burn), p (SP) 
nc39<- occMod(model = list(psi~SP+Burn,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc39)
coef(object = nc39, param = 'psi', prob = 0.95)
coef(object = nc39, param = 'p', prob = 0.95)
print_one_site_estimates(nc39)

# 3.3.2.40 psi (SP + Burn), p (SP+INT_o) 
nc40<- occMod(model = list(psi~SP+Burn,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc40)
coef(object = nc40, param = 'psi', prob = 0.95)
coef(object = nc40, param = 'p', prob = 0.95)
print_one_site_estimates(nc40)

# 3.3.2.41 psi (SP + Burn), p (SP+INT_d) 
nc41<- occMod(model = list(psi~SP+Burn,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc41)
coef(object = nc41, param = 'psi', prob = 0.95)
coef(object = nc41, param = 'p', prob = 0.95)
print_one_site_estimates(nc41)

# 3.3.2.42 psi (SP + Aspect), p (SP) 
nc42<- occMod(model = list(psi~SP+Aspect,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc42)
coef(object = nc42, param = 'psi', prob = 0.95)
coef(object = nc42, param = 'p', prob = 0.95)
print_one_site_estimates(nc42)

# 3.3.2.43 psi (SP + Aspect), p (SP+INT_o) 
nc43<- occMod(model = list(psi~SP+Aspect,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc43)
coef(object = nc43, param = 'psi', prob = 0.95)
coef(object = nc43, param = 'p', prob = 0.95)
print_one_site_estimates(nc43)

# 3.3.2.44 psi (SP + Aspect), p (SP+INT_d) 
nc44<- occMod(model = list(psi~SP+Aspect,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc44)
coef(object = nc44, param = 'psi', prob = 0.95)
coef(object = nc44, param = 'p', prob = 0.95)
print_one_site_estimates(nc44)

# 3.3.2.45 psi (SP + Elevation), p (SP) 
nc45<- occMod(model = list(psi~SP+Elevation,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc45)
coef(object = nc45, param = 'psi', prob = 0.95)
coef(object = nc45, param = 'p', prob = 0.95)

# 3.3.2.46 psi (SP + Elevation), p (SP+INT_o) 
nc46<- occMod(model = list(psi~SP+Elevation,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc46)
coef(object = nc46, param = 'psi', prob = 0.95)
coef(object = nc46, param = 'p', prob = 0.95)

# 3.3.2.47 psi (SP + Elevation), p (SP+INT_d) 
nc47<- occMod(model = list(psi~SP+Elevation,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc47)
coef(object = nc47, param = 'psi', prob = 0.95)
coef(object = nc47, param = 'p', prob = 0.95)

# 3.3.2.48 psi (SP + Slope), p (SP) 
nc48<- occMod(model = list(psi~SP+Slope,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc48)
coef(object = nc48, param = 'psi', prob = 0.95)
coef(object = nc48, param = 'p', prob = 0.95)

# 3.3.2.49 psi (SP + Slope), p (SP+INT_o) 
nc49<- occMod(model = list(psi~SP+Slope,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc49)
coef(object = nc49, param = 'psi', prob = 0.95)
coef(object = nc49, param = 'p', prob = 0.95)

# 3.3.2.50 psi (SP + Slope), p (SP+INT_d) 
nc50<- occMod(model = list(psi~SP+Slope,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc50)
coef(object = nc50, param = 'psi', prob = 0.95)
coef(object = nc50, param = 'p', prob = 0.95)

# 3.3.2.51 psi (SP + Metalled road), p (SP) 
nc51<- occMod(model = list(psi~SP+Metalled_road,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc51)
coef(object = nc51, param = 'psi', prob = 0.95)
coef(object = nc51, param = 'p', prob = 0.95)

# 3.3.2.52 psi (SP + Metalled road), p (SP+INT_o) 
nc52<- occMod(model = list(psi~SP+Metalled_road,p~SP+INT_o), data=Tsp,
              type="so.2sp.1")  
summary(nc52)
coef(object = nc52, param = 'psi', prob = 0.95)
coef(object = nc52, param = 'p', prob = 0.95)

# 3.3.2.53 psi (SP + Metalled road), p (SP+INT_d) 
nc53<- occMod(model = list(psi~SP+Metalled_road,p~SP+INT_d), data=Tsp,
              type="so.2sp.1")  
summary(nc53)
coef(object = nc53, param = 'psi', prob = 0.95)
coef(object = nc53, param = 'p', prob = 0.95)

# 3.3.2.54 psi (SP + Dirt road), p (SP) 
nc54<- occMod(model = list(psi~SP+Dirt_road,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc54)
coef(object = nc54, param = 'psi', prob = 0.95)
coef(object = nc54, param = 'p', prob = 0.95)

# 3.3.2.55 psi (SP + Dirt road), p (SP+INT_o) 
nc55<- occMod(model = list(psi~SP+Dirt_road,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc55)
coef(object = nc55, param = 'psi', prob = 0.95)
coef(object = nc55, param = 'p', prob = 0.95)

# 3.3.2.56 psi (SP + Dirt road), p (SP+INT_d) 
nc56<- occMod(model = list(psi~SP+Dirt_road,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc56)
coef(object = nc56, param = 'psi', prob = 0.95)
coef(object = nc56, param = 'p', prob = 0.95)

# 3.3.2.57 psi (SP + No road), p (SP) 
nc57<- occMod(model = list(psi~SP+No_road,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc57)
coef(object = nc57, param = 'psi', prob = 0.95)
coef(object = nc57, param = 'p', prob = 0.95)

# 3.3.2.58 psi (SP + No road), p (SP+INT_o) 
nc58<- occMod(model = list(psi~SP+No_road,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc58)
coef(object = nc58, param = 'psi', prob = 0.95)
coef(object = nc58, param = 'p', prob = 0.95)

# 3.3.2.59 psi (SP + No road), p (SP+INT_d) 
nc59<- occMod(model = list(psi~SP+No_road,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc59)
coef(object = nc59, param = 'psi', prob = 0.95)
coef(object = nc59, param = 'p', prob = 0.95)

# 3.3.2.60 psi (SP + Lentic), p (SP) 
nc60<- occMod(model = list(psi~SP+Lentic,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc60)
coef(object = nc60, param = 'psi', prob = 0.95)
coef(object = nc60, param = 'p', prob = 0.95)

# 3.3.2.61 psi (SP + Lentic), p (SP+INT_o) 
nc61<- occMod(model = list(psi~SP+Lentic,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc61)
coef(object = nc61, param = 'psi', prob = 0.95)
coef(object = nc61, param = 'p', prob = 0.95)

# 3.3.2.62 psi (SP + Lentic), p (SP+INT_d) 
nc62<- occMod(model = list(psi~SP+Lentic,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc62)
coef(object = nc62, param = 'psi', prob = 0.95)
coef(object = nc62, param = 'p', prob = 0.95)

# 3.3.2.63 psi (SP + Wet lotic), p (SP) 
nc63<- occMod(model = list(psi~SP+Wet_lotic,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc63)
coef(object = nc63, param = 'psi', prob = 0.95)
coef(object = nc63, param = 'p', prob = 0.95)

# 3.3.2.64 psi (SP + Wet lotic), p (SP+INT_o) 
nc64<- occMod(model = list(psi~SP+Wet_lotic,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc64)
coef(object = nc64, param = 'psi', prob = 0.95)
coef(object = nc64, param = 'p', prob = 0.95)

# 3.3.2.65 psi (SP + Wet lotic), p (SP+INT_d) 
nc65<- occMod(model = list(psi~SP+Wet_lotic,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc65)
coef(object = nc65, param = 'psi', prob = 0.95)
coef(object = nc65, param = 'p', prob = 0.95)

# 3.3.2.66 psi (SP + Dry lotic), p (SP) 
nc66<- occMod(model = list(psi~SP+Dry_lotic,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc66)
coef(object = nc66, param = 'psi', prob = 0.95)
coef(object = nc66, param = 'p', prob = 0.95)

# 3.3.2.67 psi (SP + Dry lotic), p (SP+INT_o) 
nc67<- occMod(model = list(psi~SP+Dry_lotic,p~SP+INT_o), data=Tsp, type="so.2sp.1")  
summary(nc67)
coef(object = nc67, param = 'psi', prob = 0.95)
coef(object = nc67, param = 'p', prob = 0.95)

# 3.3.2.68 psi (SP + Dry lotic), p (SP+INT_d) 
nc68<- occMod(model = list(psi~SP+Dry_lotic,p~SP+INT_d), data=Tsp, type="so.2sp.1")  
summary(nc68)
coef(object = nc68, param = 'psi', prob = 0.95)
coef(object = nc68, param = 'p', prob = 0.95)

# 3.3.2.69 psi (SP + Rubus niveus), p (SP) 
nc69<- occMod(model = list(psi~SP+Rubus_niveus,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc69)
coef(object = nc69, param = 'psi', prob = 0.95)
coef(object = nc69, param = 'p', prob = 0.95)

# 3.3.2.70 psi (SP + Rubus niveus), p (SP+INT_o) 
nc70<- occMod(model = list(psi~SP+Rubus_niveus,p~SP+INT_o), data=Tsp,
              type="so.2sp.1")  
summary(nc70)
coef(object = nc70, param = 'psi', prob = 0.95)
coef(object = nc70, param = 'p', prob = 0.95)

# 3.3.2.71 psi (SP + Rubus niveus), p (SP+INT_d) 
nc71<- occMod(model = list(psi~SP+Rubus_niveus,p~SP+INT_d), data=Tsp,
              type="so.2sp.1")  
summary(nc71)
coef(object = nc71, param = 'psi', prob = 0.95)
coef(object = nc71, param = 'p', prob = 0.95)

# 3.3.2.72 psi (SP + Lantana camara), p (SP) 
nc72<- occMod(model = list(psi~SP+Lantana_camara,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc72)
coef(object = nc72, param = 'psi', prob = 0.95)
coef(object = nc72, param = 'p', prob = 0.95)

#Sus. Note the very low convergence. Not acceptable.

# 3.3.2.73 psi (SP + Lantana camara), p (SP+INT_o) 
nc73<- occMod(model = list(psi~SP+Lantana_camara,p~SP+INT_o), data=Tsp,
              type="so.2sp.1")  
summary(nc73)
coef(object = nc73, param = 'psi', prob = 0.95)
coef(object = nc73, param = 'p', prob = 0.95)

#Sus. Note the very low convergence.

# 3.3.2.74 psi (SP + Lantana camara), p (SP+INT_d) 
nc74<- occMod(model = list(psi~SP+Lantana_camara,p~SP+INT_d), data=Tsp,
              type="so.2sp.1")  
summary(nc74)
coef(object = nc74, param = 'psi', prob = 0.95)
coef(object = nc74, param = 'p', prob = 0.95)

#Sus. Note the low convergence. Not acceptable.

#Now, create models using survey-specific covariates:

# 3.3.2.75 psi (SP), p (SP + Before noon) 
nc75<- occMod(model = list(psi~SP,p~SP+BefNoon), data=Tsp, type="so.2sp.1")  
summary(nc75)
coef(object = nc75, param = 'psi', prob = 0.95)
coef(object = nc75, param = 'p', prob = 0.95)

# 3.3.2.76 psi (SP), p (SP+INT_o+Before noon) 
nc76<- occMod(model = list(psi~SP,p~SP+INT_o+BefNoon), data=Tsp, type="so.2sp.1")  
summary(nc76)
coef(object = nc76, param = 'psi', prob = 0.95)
coef(object = nc76, param = 'p', prob = 0.95)

# 3.3.2.77 psi (SP), p (SP+INT_d+Before noon) 
nc77<- occMod(model = list(psi~SP,p~SP+INT_d+BefNoon), data=Tsp, type="so.2sp.1")  
summary(nc77)
coef(object = nc77, param = 'psi', prob = 0.95)
coef(object = nc77, param = 'p', prob = 0.95)

#Check what happens to the AICc when site and survey-specific covariates are used...
#...together. Create some such models with Area as site-specific covariate (since...
#...it's the covariate in the top models run in the model set "models_tsp1":

# 3.3.2.78 psi (SP + Area), p (SP + Before noon) 
nc78<- occMod(model = list(psi~SP+Area,p~SP+BefNoon), data=Tsp, type="so.2sp.1")  
summary(nc78)
coef(object = nc78, param = 'psi', prob = 0.95)
coef(object = nc78, param = 'p', prob = 0.95)

# 3.3.2.79 psi (SP + Area), p (SP+INT_o+Before noon) 
nc79<- occMod(model = list(psi~SP+Area,p~SP+INT_o+BefNoon), data=Tsp,
              type="so.2sp.1")  
summary(nc79)
coef(object = nc79, param = 'psi', prob = 0.95)
coef(object = nc79, param = 'p', prob = 0.95)

# 3.3.2.80 psi (SP + Area), p (SP+INT_d+Before noon) 
nc80<- occMod(model = list(psi~SP+Area,p~SP+INT_d+BefNoon), data=Tsp,
              type="so.2sp.1")  
summary(nc80)
coef(object = nc80, param = 'psi', prob = 0.95)
coef(object = nc80, param = 'p', prob = 0.95)

#Create AICc table using the models so far (except the excluded ones: 72 & 74...
#...and other ones with poor convergence of values:
models_tsp2n =
  list(nc1,nc2,nc3,nc4,nc5,nc6,nc7,nc8,nc9,nc10,nc11,nc12,nc13,nc14,nc15,nc16,nc17,
       nc18,nc19,nc20,nc21,nc22,nc23,nc24,nc25,nc26,nc27,nc28,nc29,nc30,nc31,nc32,
       nc33,nc34,nc35,nc36,nc37,nc38,nc39,nc40,nc41,nc42,nc43,nc44,nc45,nc46,nc47,
       nc48,nc49,nc50,nc51,nc52,nc53,nc54,nc55,nc56,nc57,nc58,nc59,nc60,nc61,nc62,
       nc63,nc64,nc65,nc66,nc67,nc68,nc69,nc70,nc71,nc73,nc75,nc76,nc77,nc78,nc79,
       nc80)
results_tsp2n<-createAicTable(models_tsp2n, use.aicc=TRUE)

#show the table:
kable(results_tsp2n$table, "pipe")

#Since most models above DAICc 7 are simple models containing Elevation, it...
#...would be useless to run very complex models with both site- and survey-specific...
#...covariates. Create simple models:

# 3.3.2.81 psi (SP), p (SP + After noon) 
nc81<- occMod(model = list(psi~SP,p~SP+AftNoon), data=Tsp, type="so.2sp.1")  
summary(nc81)
coef(object = nc81, param = 'psi', prob = 0.95)
coef(object = nc81, param = 'p', prob = 0.95)

# 3.3.2.82 psi (SP), p (SP+INT_o+After noon) 
nc82<- occMod(model = list(psi~SP,p~SP+INT_o+AftNoon), data=Tsp, type="so.2sp.1")  
summary(nc82)
coef(object = nc82, param = 'psi', prob = 0.95)
coef(object = nc82, param = 'p', prob = 0.95)

# 3.3.2.83 psi (SP), p (SP+INT_d+After noon) 
nc83<- occMod(model = list(psi~SP,p~SP+INT_d+AftNoon), data=Tsp, type="so.2sp.1")  
summary(nc83)
coef(object = nc83, param = 'psi', prob = 0.95)
coef(object = nc83, param = 'p', prob = 0.95)

# 3.3.2.84 psi (SP), p (SP + Clear sky) 
nc84<- occMod(model = list(psi~SP,p~SP+ClrSky), data=Tsp, type="so.2sp.1")  
summary(nc84)
coef(object = nc84, param = 'psi', prob = 0.95)
coef(object = nc84, param = 'p', prob = 0.95)

# 3.3.2.85 psi (SP), p (SP+INT_o+Clear sky) 
nc85<- occMod(model = list(psi~SP,p~SP+INT_o+ClrSky), data=Tsp, type="so.2sp.1")  
summary(nc85)
coef(object = nc85, param = 'psi', prob = 0.95)
coef(object = nc85, param = 'p', prob = 0.95)

# 3.3.2.86 psi (SP), p (SP+INT_d+Clear sky) 
nc86<- occMod(model = list(psi~SP,p~SP+INT_d+ClrSky), data=Tsp, type="so.2sp.1")  
summary(nc86)
coef(object = nc86, param = 'psi', prob = 0.95)
coef(object = nc86, param = 'p', prob = 0.95)

# 3.3.2.87 psi (SP), p (SP + Overcast) 
nc87<- occMod(model = list(psi~SP,p~SP+Ovrcst), data=Tsp, type="so.2sp.1")  
summary(nc87)
coef(object = nc87, param = 'psi', prob = 0.95)
coef(object = nc87, param = 'p', prob = 0.95)

# 3.3.2.88 psi (SP), p (SP+INT_o+Overcast) 
nc88<- occMod(model = list(psi~SP,p~SP+INT_o+Ovrcst), data=Tsp, type="so.2sp.1")  
summary(nc88)
coef(object = nc88, param = 'psi', prob = 0.95)
coef(object = nc88, param = 'p', prob = 0.95)

# 3.3.2.89 psi (SP), p (SP+INT_d+Overcast) 
nc89<- occMod(model = list(psi~SP,p~SP+INT_d+Ovrcst), data=Tsp, type="so.2sp.1")  
summary(nc89)
coef(object = nc89, param = 'psi', prob = 0.95)
coef(object = nc89, param = 'p', prob = 0.95)

# 3.3.2.90 psi (SP), p (SP + Sunny) 
nc90<- occMod(model = list(psi~SP,p~SP+Sunny), data=Tsp, type="so.2sp.1")  
summary(nc90)
coef(object = nc90, param = 'psi', prob = 0.95)
coef(object = nc90, param = 'p', prob = 0.95)

# 3.3.2.91 psi (SP), p (SP+INT_o+Sunny) 
nc91<- occMod(model = list(psi~SP,p~SP+INT_o+Sunny), data=Tsp, type="so.2sp.1")  
summary(nc91)
coef(object = nc91, param = 'psi', prob = 0.95)
coef(object = nc91, param = 'p', prob = 0.95)

# 3.3.2.92 psi (SP), p (SP+INT_d+Sunny) 
nc92<- occMod(model = list(psi~SP,p~SP+INT_d+Sunny), data=Tsp, type="so.2sp.1")  
summary(nc92)
coef(object = nc92, param = 'psi', prob = 0.95)
coef(object = nc92, param = 'p', prob = 0.95)

# 3.3.2.93 psi (SP), p (SP + Windy) 
nc93<- occMod(model = list(psi~SP,p~SP+Windy), data=Tsp, type="so.2sp.1")  
summary(nc93)
coef(object = nc93, param = 'psi', prob = 0.95)
coef(object = nc93, param = 'p', prob = 0.95)

# 3.3.2.94 psi (SP), p (SP+INT_o+Windy) 
nc94<- occMod(model = list(psi~SP,p~SP+INT_o+Windy), data=Tsp, type="so.2sp.1")  
summary(nc94)
coef(object = nc94, param = 'psi', prob = 0.95)
coef(object = nc94, param = 'p', prob = 0.95)

# 3.3.2.95 psi (SP), p (SP+INT_d+Windy) 
nc95<- occMod(model = list(psi~SP,p~SP+INT_d+Windy), data=Tsp, type="so.2sp.1")  
summary(nc95)
coef(object = nc95, param = 'psi', prob = 0.95)
coef(object = nc95, param = 'p', prob = 0.95)

# 3.3.2.96 psi (SP), p (SP + Warm) 
nc96<- occMod(model = list(psi~SP,p~SP+Warm), data=Tsp, type="so.2sp.1")  
summary(nc96)
coef(object = nc96, param = 'psi', prob = 0.95)
coef(object = nc96, param = 'p', prob = 0.95)

# 3.3.2.97 psi (SP), p (SP+INT_o+Warm) 
nc97<- occMod(model = list(psi~SP,p~SP+INT_o+Warm), data=Tsp, type="so.2sp.1")  
summary(nc97)
coef(object = nc97, param = 'psi', prob = 0.95)
coef(object = nc97, param = 'p', prob = 0.95)

# 3.3.2.98 psi (SP), p (SP+INT_d+Warm) 
nc98<- occMod(model = list(psi~SP,p~SP+INT_d+Warm), data=Tsp, type="so.2sp.1")  
summary(nc98)
coef(object = nc98, param = 'psi', prob = 0.95)
coef(object = nc98, param = 'p', prob = 0.95)

# 3.3.2.99 psi (SP), p (SP + Neutral) 
nc99<- occMod(model = list(psi~SP,p~SP+Neutral), data=Tsp, type="so.2sp.1")  
summary(nc99)
coef(object = nc99, param = 'psi', prob = 0.95)
coef(object = nc99, param = 'p', prob = 0.95)

# 3.3.2.100 psi (SP), p (SP+INT_o+Neutral) 
nc100<- occMod(model = list(psi~SP,p~SP+INT_o+Neutral), data=Tsp, type="so.2sp.1")  
summary(nc100)
coef(object = nc100, param = 'psi', prob = 0.95)
coef(object = nc100, param = 'p', prob = 0.95)

# 3.3.2.101 psi (SP), p (SP+INT_d+Neutral) 
nc101<- occMod(model = list(psi~SP,p~SP+INT_d+Neutral), data=Tsp, type="so.2sp.1")  
summary(nc101)
coef(object = nc101, param = 'psi', prob = 0.95)
coef(object = nc101, param = 'p', prob = 0.95)

# 3.3.2.102 psi (SP), p (SP + Cool) 
nc102<- occMod(model = list(psi~SP,p~SP+Cool), data=Tsp, type="so.2sp.1")  
summary(nc102)
coef(object = nc102, param = 'psi', prob = 0.95)
coef(object = nc102, param = 'p', prob = 0.95)

# 3.3.2.103 psi (SP), p (SP+INT_o+Cool) 
nc103<- occMod(model = list(psi~SP,p~SP+INT_o+Cool), data=Tsp, type="so.2sp.1")  
summary(nc103)
coef(object = nc103, param = 'psi', prob = 0.95)
coef(object = nc103, param = 'p', prob = 0.95)

# 3.3.2.104 psi (SP), p (SP+INT_d+Cool) 
nc104<- occMod(model = list(psi~SP,p~SP+INT_d+Cool), data=Tsp, type="so.2sp.1")  
summary(nc104)
coef(object = nc104, param = 'psi', prob = 0.95)
coef(object = nc104, param = 'p', prob = 0.95)

# 3.3.2.105 psi (SP), p (SP + Visibility) 
nc105<- occMod(model = list(psi~SP,p~SP+Visibility), data=Tsp, type="so.2sp.1")  
summary(nc105)
coef(object = nc105, param = 'psi', prob = 0.95)
coef(object = nc105, param = 'p', prob = 0.95)

# 3.3.2.106 psi (SP), p (SP+INT_o+Visibility) 
nc106<- occMod(model = list(psi~SP,p~SP+INT_o+Visibility), data=Tsp, type="so.2sp.1")  
summary(nc106)
coef(object = nc106, param = 'psi', prob = 0.95)
coef(object = nc106, param = 'p', prob = 0.95)

# 3.3.2.107 psi (SP), p (SP+INT_d+Visibility) 
nc107<- occMod(model = list(psi~SP,p~SP+INT_d+Visibility), data=Tsp, type="so.2sp.1")  
summary(nc107)
coef(object = nc107, param = 'psi', prob = 0.95)
coef(object = nc107, param = 'p', prob = 0.95)

#Create AICc model set:
models_tsp3n =
list(nc1,nc2,nc4,nc22,nc23,nc24,nc25,nc26,nc28,nc29,nc30,nc32,nc33,nc34,nc36,nc37,
     nc38,nc39,nc40,nc41,nc42,nc43,nc44,nc45,nc46,nc47,nc48,nc49,nc50,nc51,nc52,
     nc53,nc54,nc55,nc56,nc57,nc58,nc59,nc60,nc61,nc62,nc63,nc64,nc65,nc66,nc67,
     nc68,nc69,nc70,nc71,nc73,nc75,nc76,nc77,nc78,nc79,nc80,nc81,nc82,nc83,nc84,
     nc85,nc86,nc87,nc88,nc89,nc90,nc91,nc92,nc93,nc94,nc95,nc96,nc97,nc98,nc99,
     nc100,nc101,nc102,nc103,nc104,nc105,nc106,nc107)
results_tsp3n<-createAicTable(models_tsp3n, use.aicc=TRUE)

#show the table:
kable(results_tsp3n$table, "pipe")

#Only the models: psi(SP+Elevation)p(SP); psi(SP+Elevation)p(SP+INT_o);...
#...psi(SP+Elevation)p(SP+INT_d);psi(SP+Lantana_camara)p(SP+INT_o); and... 
#...psi(SP+No_road)p(SP) fall within DAICc of 7. 

#We have 27 possibilities of occupancy for species A and B. In co-occurence models...
#...encounter history is compressed into 0,1,2, & 3 with the data for both species...
#...occupying the same lines in the detection-history matrix. The 4 possibilities...
#...in the encounter/detection history instead of the 2 in single species models...
#...makes it possible to estimate extra parameters. So, we can have more number of...
#...covariates than what is recommended in single species models. Let's limit it...
#...4 for psi and 8 for p. Try some models that, with top survey- and site-...
#...specific covariates in the top models of the last model set ("models_tsp3"):

# 3.3.2.108 psi (SP + Elevation), p (SP+INT_d+BefNoon) 
nc108<- occMod(model = list(psi~SP+Elevation,p~SP+INT_d+BefNoon), data=Tsp,
               type="so.2sp.1")  
summary(nc108)
coef(object = nc108, param = 'psi', prob = 0.95)
coef(object = nc108, param = 'p', prob = 0.95)

# 3.3.2.109 psi (SP + Elevation + No road), p (SP+INT_d+BefNoon) 
nc109<- occMod(model = list(psi~SP+Elevation+No_road,p~SP+INT_d+BefNoon), data=Tsp,
               type="so.2sp.1")  
summary(nc109)
coef(object = nc109, param = 'psi', prob = 0.95)
coef(object = nc109, param = 'p', prob = 0.95)

# 3.3.2.110 psi (SP + Area + No road), p (SP+INT_d+BefNoon) 
nc110<- occMod(model = list(psi~SP+Area+No_road,p~SP+INT_d+BefNoon), data=Tsp,
               type="so.2sp.1")  
summary(nc110)
coef(object = nc110, param = 'psi', prob = 0.95)
coef(object = nc110, param = 'p', prob = 0.95)

# 3.3.2.111 psi (SP + Elevation + Area), p (SP+INT_d+BefNoon) 
nc111<- occMod(model = list(psi~SP+Elevation+Area,p~SP+INT_d+BefNoon),
               data=Tsp, type="so.2sp.1")  
summary(nc111)
coef(object = nc111, param = 'psi', prob = 0.95)
coef(object = nc111, param = 'p', prob = 0.95)

# 3.3.2.112 psi (SP + Elevation + Wet lotic), p (SP+INT_d+BefNoon) 
nc112<- occMod(model = list(psi~SP+Elevation+Wet_lotic,p~SP+INT_d+BefNoon),
               data=Tsp, type="so.2sp.1")  
summary(nc112)
coef(object = nc112, param = 'psi', prob = 0.95)
coef(object = nc112, param = 'p', prob = 0.95)

# 3.3.2.113 psi (SP + Elevation + No road), p (SP) 
nc113<- occMod(model = list(psi~SP+Elevation+No_road,p~SP), data=Tsp,
               type="so.2sp.1")  
summary(nc113)
coef(object = nc113, param = 'psi', prob = 0.95)
coef(object = nc113, param = 'p', prob = 0.95)

# 3.3.2.114 psi (SP + Area + No road), p (SP) 
nc114<- occMod(model = list(psi~SP+Area+No_road,p~SP), data=Tsp, type="so.2sp.1")  
summary(nc114)
coef(object = nc114, param = 'psi', prob = 0.95)
coef(object = nc114, param = 'p', prob = 0.95)

# 3.3.2.115 psi (SP + Elevation + Area), p (SP) 
nc115<- occMod(model = list(psi~SP+Elevation+Area,p~SP), data=Tsp,
               type="so.2sp.1")  
summary(nc115)
coef(object = nc115, param = 'psi', prob = 0.95)
coef(object = nc115, param = 'p', prob = 0.95)

# 3.3.2.116 psi (SP + Elevation + Wet lotic), p (SP) 
nc116<- occMod(model = list(psi~SP+Elevation+Wet_lotic,p~SP),data=Tsp,
               type="so.2sp.1")  
summary(nc116)
coef(object = nc116, param = 'psi', prob = 0.95)
coef(object = nc116, param = 'p', prob = 0.95)

# 3.3.2.117 psi (SP + Elevation + Lantana camara), p (SP+INT_d) 
nc117<- occMod(model = list(psi~SP+Elevation+Lantana_camara,p~SP+INT_d),data=Tsp,
               type="so.2sp.1")  
summary(nc117)
coef(object = nc117, param = 'psi', prob = 0.95)
coef(object = nc117, param = 'p', prob = 0.95)

# 3.3.2.118 psi (SP + Elevation + Lantana camara), p (SP) 
nc118<- occMod(model = list(psi~SP+Elevation+Lantana_camara,p~SP),data=Tsp,
               type="so.2sp.1")  
summary(nc118)
coef(object = nc118, param = 'psi', prob = 0.95)
coef(object = nc118, param = 'p', prob = 0.95)

#Models involving the covariates 'SURVEY' was missed. So, that too will be carried...
#...out. #First Convert SURVEY to numeric:
Tsp$survcov$survey = as.numeric(Tsp$survcov$SURVEY)
list(Tsp$survcov$survey)

# 3.3.2.119 psi (SP), p (SP+survey)
nc119<- occMod(model = list(psi~SP,p~SP+survey),data=Tsp,type="so.2sp.1")  
summary(nc119)
coef(object = nc119, param = 'psi', prob = 0.95)
coef(object = nc119, param = 'p', prob = 0.95)

# 3.3.2.120 psi (SP + Elevation), p (SP+ survey)
nc120<- occMod(model = list(psi~SP+Elevation,p~SP+survey),data=Tsp,type="so.2sp.1")  
summary(nc120)
coef(object = nc120, param = 'psi', prob = 0.95)
coef(object = nc120, param = 'p', prob = 0.95)

# 3.3.2.121 psi (SP + Elevation), p (SP+INT_d+survey)
nc121<- occMod(model = list(psi~SP+Elevation,p~SP+INT_d+survey),data=Tsp,
               type="so.2sp.1")  
summary(nc121)
coef(object = nc121, param = 'psi', prob = 0.95)
coef(object = nc121, param = 'p', prob = 0.95)

# 3.3.2.122 psi (SP + Elevation), p (SP+INT_o+survey)
nc122<- occMod(model = list(psi~SP+Elevation,p~SP+INT_o+survey),data=Tsp,
               type="so.2sp.1")  
summary(nc122)
coef(object = nc122, param = 'psi', prob = 0.95)
coef(object = nc122, param = 'p', prob = 0.95)

# 3.3.2.123 psi (SP + Lantana camara), p (SP+INT_d+survey)
nc123<- occMod(model = list(psi~SP+Lantana_camara,p~SP+INT_d+survey),data=Tsp,
               type="so.2sp.1")  
summary(nc123)
coef(object = nc123, param = 'psi', prob = 0.95)
coef(object = nc123, param = 'p', prob = 0.95)

# 3.3.2.124 psi (SP + No road), p (SP+survey)
nc124<- occMod(model = list(psi~SP+No_road,p~SP+survey),data=Tsp,type="so.2sp.1")  
summary(nc124)
coef(object = nc124, param = 'psi', prob = 0.95)
coef(object = nc124, param = 'p', prob = 0.95)

#Some more with INT_o:

# 3.3.2.125 psi (SP + Elevation), p (SP+INT_o+BefNoon) 
nc125<- occMod(model = list(psi~SP+Elevation,p~SP+INT_o+BefNoon), data=Tsp,
               type="so.2sp.1")  
summary(nc125)
coef(object = nc125, param = 'psi', prob = 0.95)
coef(object = nc125, param = 'p', prob = 0.95)

# 3.3.2.126 psi (SP + Elevation + No road), p (SP+INT_o+BefNoon) 
nc126<- occMod(model = list(psi~SP+Elevation+No_road,p~SP+INT_o+BefNoon), data=Tsp,
               type="so.2sp.1")  
summary(nc126)
coef(object = nc126, param = 'psi', prob = 0.95)
coef(object = nc126, param = 'p', prob = 0.95)

# 3.3.2.127 psi (SP + Area + No road), p (SP+INT_o+BefNoon) 
nc127<- occMod(model = list(psi~SP+Area+No_road,p~SP+INT_o+BefNoon), data=Tsp,
               type="so.2sp.1")  
summary(nc127)
coef(object = nc127, param = 'psi', prob = 0.95)
coef(object = nc127, param = 'p', prob = 0.95)

# 3.3.2.128 psi (SP + Elevation + Area), p (SP+INT_o+BefNoon) 
nc128<- occMod(model = list(psi~SP+Elevation+Area,p~SP+INT_o+BefNoon),
               data=Tsp, type="so.2sp.1")  
summary(nc128)
coef(object = nc128, param = 'psi', prob = 0.95)
coef(object = nc128, param = 'p', prob = 0.95)

# 3.3.2.129 psi (SP + Elevation + Wet lotic), p (SP+INT_o+BefNoon) 
nc129<- occMod(model = list(psi~SP+Elevation+Wet_lotic,p~SP+INT_o+BefNoon),
               data=Tsp, type="so.2sp.1")  
summary(nc129)
coef(object = nc129, param = 'psi', prob = 0.95)
coef(object = nc129, param = 'p', prob = 0.95)

# 3.3.2.130 psi (SP + Elevation + Lantana camara), p (SP+INT_o) 
nc130<- occMod(model = list(psi~SP+Elevation+Lantana_camara,p~SP+INT_o),data=Tsp,
               type="so.2sp.1")  
summary(nc130)
coef(object = nc130, param = 'psi', prob = 0.95)
coef(object = nc130, param = 'p', prob = 0.95)

# 3.3.2.131 psi (SP + Lantana camara), p (SP+INT_o+survey)
nc131<- occMod(model = list(psi~SP+Lantana_camara,p~SP+INT_o+survey),data=Tsp,
               type="so.2sp.1")  
summary(nc131)
coef(object = nc131, param = 'psi', prob = 0.95)
coef(object = nc131, param = 'p', prob = 0.95)

#Since a GOF test is not available for the 2 species model at the moment, we can...
#...use the c-hat value of the most common species among the two to create a...
#...QAICc table (just to be on the safer side of statistical validity). Test with...
#...all models in case some model is missed within the range of 0-7, since DQAIVCc...
#...tends to be lower than DAICc and then remove ones beyond DQAICc 7.

qaicTspn <- createAicTable(
  mod.list = 
list(nc1,nc2,nc3,nc4,nc5,nc6,nc7,nc8,nc9,nc10,nc11,nc12,nc13,nc14,nc15,nc16,nc17,
nc18,nc19,nc20,nc21,nc22,nc23,nc24,nc25,nc26,nc27,nc28,nc29,nc30,nc31,nc32,nc33,
nc34,nc36,nc37,nc38,nc39,nc40,nc41,nc42,nc43,nc44,nc45,nc46,nc47,nc48,nc49,nc50,
nc51,nc52,nc53,nc54,nc55,nc56,nc57,nc58,nc59,nc60,nc61,nc62,nc63,nc64,nc65,nc66,
nc67,nc68,nc69,nc70,nc71,nc73,nc75,nc76,nc77,nc78,nc79,nc80,nc81,nc82,nc83,nc84,
nc85,nc86,nc87,nc88,nc89,nc90,nc91,nc92,nc93,nc94,nc95,nc96,nc97,nc98,nc99,nc100,
nc101,nc102,nc103,nc104,nc105,nc106,nc107,nc108,nc109,nc110,nc111,nc112,nc113,
nc114,nc115,nc116,nc117,nc118,nc119,nc120,nc121,nc122,nc123,nc124,nc125,nc126,
nc127,nc128,nc129,nc130,nc131),
  use.aicc = TRUE,
  chat = MfBoot1$gof$chat
)

# print the table
kable(qaicTsp$table, "pipe")

#Create qAICc table to adjust the variance due to >1 c-hat:
qaicTspn <- createAicTable(
  mod.list = list(nc45,nc46,nc47,nc57,nc73,nc109,nc111,nc113,nc115,nc116,
                  nc117,nc118,nc120,nc121,nc122,nc124,nc126,nc128,nc130,nc131),
  use.aicc = TRUE,
  chat = MfBoot1$gof$chat
)

# print the table
kable(qaicTspn$table, "pipe")

#Write the results to a csv file:
write.csv(qaicTspn$table,
          "/media/Dissertation/Paper/Tsp_QAICc_alt.csv")

#Co-occurence models without any interactive effects on occupancy or detection...
#...probabilities corroborates results from the single species models i.e. Elevation,...
#...No road, Area, survey, and Wet lotic are covariates that are likely affecting...
#...occupancy of both species, or at least one of the species.

#Since the bigger goal of co-occurrence models is to see any interactive effects,
#...reduce the models to only those with interactive effects on detection prob.: 
qaicTspfn <- createAicTable(
  mod.list = list(nc46,nc47,nc73,nc109,nc111,nc117,nc121,nc122,nc126,nc128,nc130,
                  nc131),
  use.aicc = TRUE,
  chat = MfBoot1$gof$chat
)

# print the table
kable(qaicTspfn$table, "pipe")

#Write the results to a csv file:
write.csv(qaicTspfn$table,
          "/media/Dissertation/Paper/Tspf_QAICc_alt.csv")

#Save as word document:
qaicTspfn_tbl <- flextable(qaicTspfn$table)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = qaicTspfn_tbl) %>%  #Insert the flextable object there
  print(target = "qaicTspfn_tbl.docx")  

#Since we have an idea about psiA (pr(S. albiventris) being present, regardless of...
#...psi status of species M. fairbankii, and vice versa (psiB) from the single...
#...species models, we don't need to concern ourselves with them. Same with pA...
#...and pB. The following parameters needs attention: psiBA, psiBa, rA, rBA & rBa...
#...(from those available from the default parameterisation. Calculation of other...
#...values and their SEs and CIs is complex).

#Model average the psi estimates:
psiBA_maTspn <- modAvg(qaicTspfn, param = 'psiBA')
psiBa_maTspn <- modAvg(qaicTspfn, param = 'psiBa')

#Show the results and summary:
kable(psiBA_maTspn, "pipe")
summary(psiBA_maTspn)
kable(psiBa_maTspn, "pipe")
summary(psiBa_maTspn)

#Note: As James Hines explains: "A subtle trait of the 2-species model is that...
#...when psiA is high, psiBa becomes more  difficult to estimate because there... 
#...will be few sites which are not occupied by species A. Similarly, if occupancy...
#...for species is very low, psiBA becomes more difficult to estimate as there...
#...are few sites occupied by species A. To summarize, if psiA is high, then you...
#...need more sites in order to get reasonable estimates for psiBa."

#Let's check psiA:
psiA_maTspn <- modAvg(qaicTspfn, param = 'psiA')
kable(psiA_maTspn, "pipe")
#psiA is low in most sites

#Model average the p and r estimates:
rA_maTspn <- modAvg(qaicTspfn, param = 'rA')
rBA_maTspn <- modAvg(qaicTspfn, param = 'rBA')
rBa_maTspn <- modAvg(qaicTspfn, param = 'rBa')

#Show the results and summary:
kable(rA_maTspn, "pipe")
summary(rA_maTspn)
kable(rBA_maTspn, "pipe")
summary(rBA_maTspn)
kable(rBa_maTspn, "pipe")
summary(rBa_maTspn)

#Check similarity between the psiBA and psiBa (they should match 100% since there...
#...are not interactive effects on psi in any of the models in the final model set):
summary(psiBA_maTspn == psiBa_maTspn)
#It does match 100%.

#However, rBA and rBa shouldn't match 100% since there are interactive effects on...
#...detection probabilities:
summary(rBA_maTspn == rBa_maTspn)
#Match is 0%

#Mean of rA:
mean(rA_maTspn$est, na.rm = TRUE) # 0.6158483

#Range of rA:
min(rA_maTspn$est, na.rm = TRUE) # 0.5217848
max(rA_maTspn$est, na.rm = TRUE) # 0.6637335

#Range of SE of rA:
min(rA_maTspn$se, na.rm = TRUE) # 0.07859856
max(rA_maTspn$se, na.rm = TRUE) # 0.1095407

#Range of 95% CI of rA:
min(rA_maTspn$lower_0.95, na.rm = TRUE) # 0.3160967
max(rA_maTspn$upper_0.95, na.rm = TRUE) # 0.8282263

#Mean of rBA:
mean(rBA_maTspn$est, na.rm = TRUE) # 0.6941389

#Range of rBA:
min(rBA_maTspn$est, na.rm = TRUE) # 0.6087046
max(rBA_maTspn$est, na.rm = TRUE) # 0.7459263

#Range of SE of rBA:
min(rBA_maTspn$se, na.rm = TRUE) # 0.06833524
max(rBA_maTspn$se, na.rm = TRUE) # 0.1184031

#Range of 95% CI of rBA:
min(rBA_maTspn$lower_0.95, na.rm = TRUE) # 0.3699466
max(rBA_maTspn$upper_0.95, na.rm = TRUE) # 0.8761168

#Mean of rBa:
mean(rBa_maTspn$est, na.rm = TRUE) # 0.6170217

#Range of rBa:
min(rBa_maTspn$est, na.rm = TRUE) # 0.3658438
max(rBa_maTspn$est, na.rm = TRUE) # 0.7157891

#Range of SE of rBa:
min(rBa_maTspn$se, na.rm = TRUE) # 0.1048587
max(rBa_maTspn$se, na.rm = TRUE) # 0.7455679

#Range of 95% CI of rBa:
min(rBa_maTspn$lower_0.95, na.rm = TRUE) # 0.001082947
max(rBa_maTspn$upper_0.95, na.rm = TRUE) # 0.9967531

str(rA_maTspn)
str(rBA_maTspn)
str(rBa_maTspn)
#Add the site ID and survey number columns to these:
Tspn_rA <- cbind(dframe, rA_maTspn)
colnames(Tspn_rA)[1] <- "siteIDs"
colnames(Tspn_rA)[2] <- "Survey_number"
Tspn_rBA <- cbind(dframe, rBA_maTspn)
colnames(Tspn_rBA)[1] <- "siteIDs"
colnames(Tspn_rBA)[2] <- "Survey_number"
Tspn_rBa <- cbind(dframe, rBa_maTspn)
colnames(Tspn_rBa)[1] <- "siteIDs"
colnames(Tspn_rBa)[2] <- "Survey_number"
head(Tspn_rA)
head(Tspn_rBA)
head(Tspn_rBa)
str(Tspn_rA)
str(Tspn_rBA)
str(Tspn_rBa)

#Save as word document:
rTspn_rA <- setNames(Tspn_rA,cbind("Site IDs", "Survey no.", "Estimate",
                                 "Standard Error", "Lower 95% Confidence Interval",
                                 "Upper 95% Confidence Interval"))
rTspn_rBA <- setNames(Tspn_rBA,cbind("Site IDs", "Survey no.", "Estimate",
                                   "Standard Error","Lower 95% Confidence Interval",
                                   "Upper 95% Confidence Interval"))
rTspn_rBa <- setNames(Tspn_rBa,cbind("Site IDs", "Survey no.", "Estimate",
                                   "Standard Error", "Lower 95% Confidence Interval",
                                   "Upper 95% Confidence Interval"))
rTspn_rA <- flextable(rTspn_rA)
rTspn_rBA <- flextable(rTspn_rBA)
rTspn_rBa <- flextable(rTspn_rBa)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rTspn_rA) %>%  #Insert the flextable object there
  print(target = "rTspn_rA.docx")  
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rTspn_rBA) %>%  #Insert the flextable object there
  print(target = "rTspn_rBA.docx")
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rTspn_rBa) %>%  #Insert the flextable object there
  print(target = "rTspn_rBa.docx")  

#Model average the p estimates:
pA_maTspn <- modAvg(qaicTspfn, param = 'pA')
pB_maTspn <- modAvg(qaicTspfn, param = 'pB')

#Model average the p estimates (of MS model):
pA_maTsp <- modAvg(qaicTspf, param = 'pA')
pB_maTsp <- modAvg(qaicTspf, param = 'pB')



#### 4. Effects of covariates####



##### 4.1 M. fairbankii #####


#To understand the effects of covariates, the betas of the models in the final...
#...model set can be compiled into a dataframe and, count & weights of covariates...
#...can be calculated.

#Collect the manually given names of the models in the final model set:
Mf_given_names <- data.frame(rbind(
         "psi_dot_p_dot", "psi_E_p_dot", "psi_Ar_p_dot", 
         "psi_Rn_p_dot", "psi_E_p_Wa", "psi_As_p_dot", "psi_Sl_p_dot",
         "psi_Ch_p_dot", "psi_Uh_p_dot", "psi_V_p_dot", "psi_B_p_dot",
         "psi_Mr_p_dot", "psi_Dr_p_dot", "psi_Nr_p_dot", "psi_L_p_dot",
         "psi_Wl_p_dot", "psi_Dl_p_dot", "psi_Ar_p_exp2", "psi_Ar_p_SurPoly",
         "psi_dot_p_BnPlusC", "psi_dot_p_PlusBnCWi", "psi_dot_p_PlusCWiO",
         "psi_E_p_BnPlusC", "psi_E_p_PlusBnCWi", "psi_E_p_PlusCWiO", 
         "psi_dot_p_Bn", "psi_dot_p_An", "psi_dot_p_Cs", "psi_dot_p_O",
         "psi_dot_p_Sun", "psi_dot_p_Wi", "psi_dot_p_Wa", "psi_dot_p_N",
         "psi_dot_p_C", "psi_dot_p_Sur", "psi_Nr_p_Wi", "psi_Dr_p_Wi",
         "psi_Nr_p_WiPlusSur", "psi_Dr_p_Sur", "psi_Sl_p_WiPlusSur",
         "psi_E_p_WiPlusSur", "psi_V_p_WiPlusSur", "psi_Dl_p_WiPlusSur",
         "psi_Ar_p_WiPlusSur", "psi_Uh_p_WiPlusSur", "psi_Rn_p_WiPlusSur",
         "psi_As_p_WiPlusSur", "psi_Nr_p_Bn", "psi_Nr_p_WiPlusBn",
         "psi_Nr_p_CsPlusBn", "psi_Sl_p_CsPlusBn", "psi_E_p_CsPlusBn",
         "psi_Dr_p_CsPlusBn", "psi_Dl_p_CsPlusBn", "psi_V_p_CsPlusBn",
         "psi_Ar_p_CsPlusBn", "psi_Ar_p_PlusWiSurBn", "psi_Nr_p_PlusWiSurBn",
         "psi_E_p_PlusWiSurBn", "psi_Rn_p_PlusWiSurBn", "psi_Sl_p_PlusWiSurBn",
         "psi_Dl_p_PlusWiSurBn", "psi_V_p_PlusWiSurBn", "psi_As_p_PlusWiSurBn",
         "psi_Ar_p_PlusWiSurAn", "psi_Nr_p_PlusWiSurAn","psi_E_p_PlusWiSurAn",
         "psi_Rn_p_PlusWiSurAn", "psi_Sl_p_PlusWiSurAn", "psi_Dl_p_PlusWiSurAn",
         "psi_V_p_PlusWiSurAn", "psi_As_p_PlusWiSurAn", "psi_Nr_p_V",
         "psi_Nr_p_VPlusWi", "psi_Nr_p_PlusVWiSur", "psi_Nr_p_PlusVWiAn",
         "psi_Sl_p_PlusVWiSur", "psi_dot_p_PlusVWiSur"))
str(Mf_given_names)
#Give names to column
Mf_given_names <- setNames(Mf_given_names,cbind("Given_names"))

#Till here, the steps are common for both betas of psi and p

###### 4.1.1 On psi #####

#Using for loop as a way to reduce typing and copy-pasting:
Mfbpsi_list <- list()               #Create empty list
i <-1                        #Create index for the purpose of saving the output.
# list()
for(Given_names in Mf_given_names){
  output <- print(paste0("cbind(coef(object = ", Given_names,
                         ", param = 'psi', prob = 0.95)),"))
  Mfbpsi_list[[i]] <- output 
}
#Convert it to a dataframe and print the output without the row names (numbers), so...
#...that the numbers doesn't have to be individually removed by hand:
Mfbpsi_list <- as.data.frame(Mfbpsi_list)
print(Mfbpsi_list, row.names = FALSE)

#Copy paste the results from console. Then create the output 'Betas_of_psi_Mf' by...
#...binding rows (using dplyr; rbind will not work when joining name with values...
#...derived from an argument). The lines get indented like a staircase. So, use the...
#...'Shift+Tab' (continuous pressing, not intermittent clicking) to bring all...
#...of them within the margin. You can skip this since this has already been done:

Betas_of_psi_Mf <- rbind(
  cbind(coef(object = psi_dot_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_E_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Ar_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Rn_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_E_p_Wa, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_As_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Sl_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Ch_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Uh_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_V_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_B_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Mr_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Dr_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_L_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Wl_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Dl_p_dot, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Ar_p_exp2, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Ar_p_SurPoly, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_BnPlusC, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_PlusBnCWi, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_PlusCWiO, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_E_p_BnPlusC, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_E_p_PlusBnCWi, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_E_p_PlusCWiO, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_Bn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_An, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_Cs, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_O, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_Sun, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_Wi, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_Wa, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_N, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_C, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_Sur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_Wi, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Dr_p_Wi, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_WiPlusSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Dr_p_Sur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Sl_p_WiPlusSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_E_p_WiPlusSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_V_p_WiPlusSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Dl_p_WiPlusSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Ar_p_WiPlusSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Uh_p_WiPlusSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Rn_p_WiPlusSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_As_p_WiPlusSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_Bn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_WiPlusBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_CsPlusBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Sl_p_CsPlusBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_E_p_CsPlusBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Dr_p_CsPlusBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Dl_p_CsPlusBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_V_p_CsPlusBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Ar_p_CsPlusBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Ar_p_PlusWiSurBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_PlusWiSurBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_E_p_PlusWiSurBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Rn_p_PlusWiSurBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Sl_p_PlusWiSurBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Dl_p_PlusWiSurBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_V_p_PlusWiSurBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_As_p_PlusWiSurBn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Ar_p_PlusWiSurAn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_PlusWiSurAn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_E_p_PlusWiSurAn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Rn_p_PlusWiSurAn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Sl_p_PlusWiSurAn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Dl_p_PlusWiSurAn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_V_p_PlusWiSurAn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_As_p_PlusWiSurAn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_V, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_VPlusWi, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_PlusVWiSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Nr_p_PlusVWiAn, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_Sl_p_PlusVWiSur, param = 'psi', prob = 0.95)),
  cbind(coef(object = psi_dot_p_PlusVWiSur, param = 'psi', prob = 0.95)))

str(Betas_of_psi_Mf)
#Check which site-specific covariates appear in the betas of psi of the models:
View(Betas_of_psi_Mf)
#15 covariates appear.Collect only the rows which contain covariate betas:
library(data.table)
Mfbpsi1 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Elevation",]
Mfbpsi2 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Area",]
Mfbpsi3 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Rubus_niveus",]
Mfbpsi4 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Aspect",]
Mfbpsi5 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Slope",]
Mfbpsi6 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Canopy_height",]
Mfbpsi7 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Understorey_height",]
Mfbpsi8 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Visibility",]
Mfbpsi9 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Burn",]
Mfbpsi10 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Metalled_road",]
Mfbpsi11 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Dirt_road",]
Mfbpsi12 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "No_road",]
Mfbpsi13 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Lentic",]
Mfbpsi14 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Wet_lotic",]
Mfbpsi15 <- Betas_of_psi_Mf[rownames(Betas_of_psi_Mf) %like% "Dry_lotic",]

Mfbpsi <- rbind(Mfbpsi1,Mfbpsi2,Mfbpsi3,Mfbpsi4,Mfbpsi5,Mfbpsi6,Mfbpsi7,Mfbpsi8,
                Mfbpsi9,Mfbpsi10,Mfbpsi11,Mfbpsi12,Mfbpsi13,Mfbpsi14,Mfbpsi15)

#Remove unnecessary dataframes to remove clutter:
remove(Mfbpsi1,Mfbpsi2,Mfbpsi3,Mfbpsi4,Mfbpsi5,Mfbpsi6,Mfbpsi7,Mfbpsi8,
       Mfbpsi9,Mfbpsi10,Mfbpsi11,Mfbpsi12,Mfbpsi13,Mfbpsi14,Mfbpsi15)

#Observe the standard errors and confidence intervals of betas of various covariates:
Mfbpsi

#Filter out betas which have 95% CI which pass through zero:
Mfbpsif <- rbind(neg <- subset(Mfbpsi, lowerCI<0 & upperCI<0),
                 pos <- subset(Mfbpsi, lowerCI>0 & upperCI>0))
View(Mfbpsif)

#Both of these dataframes gives a lot of information about the covariates.
#CI range is reasonably high for all the covariates in all the models. Assess them...
#...case by case:
# 1. 'Elevation' - in all models, SE is less than estimate, but high. Moderate...
#...certainty of influence on psi. Effect likely positive as corresponding betas...
#...positive.
# 2. 'Area' - in all models, SE is slightly less than estimate or nearly the same.
#...Low to Moderate certainty of influence on psi. Effect likely positive as...
#...corresponding betas positive.
# 3. 'Rubus niveus - in all models: SE > estimate. Very low certainty of influence...
#...on psi.
# 4. 'Aspect' - in all models, SE > estimate. Very low certainty of influence on...
#...psi.
# 5. 'Slope' - in all models: SE is slightly less than estimate, but high (nearly...
#...2/3rd of est. Moderate certainty of influence on psi. Effect likely positive...
#...as corresponding betas positive.
# 6. 'Canopy height' - only in one model, and SE > est. Very low certainty of...
#...influence on psi.
# 7.'Understorey height' - Only in two models, and SE < estimate, but nearly the...
#...the same. Low certainty of influence on p.
# 8. 'Visibility' - in all models, SE < estimate, but high. Moderate certainty of...
#...influence on psi. Effect likely positive as corresponding betas positive.
# 9. 'Burn' - only in one model, and SE > est. Very low certainty of influence...
#...on psi.
# 10. 'Metalled road' - only in one model, and SE > est. Very low certainty of...
#...influence on psi.
# 11. 'Dirt road' - in all models, SE < estimate, but high. Moderate certainty of...
#...influence on psi. Effect likely positive as corresponding betas positive.
# 12. 'No road' - beta estimates don't pass through 0 in any models where they...
#...appear. This indicates that 'No road' likely has a strong effect on psi.
#...Effect likely negative as corresponding betas negative.
# 13. 'Lentic' - only in one model, and SE > est. Very low certainty of influence...
#...on psi.
# 14. 'Wet lotic' - only in one model, and SE > est. Very low certainty of influence...
#...on psi.
# 15. 'Dry lotic' - in all models, SE >>>> est. Very very low certainty of influence...
#...on psi.


#Count the number of time each covariate appears in models:
library(stringr)
getCount <- function(data,keyword)
{
  wcount <- str_count(qaicMf$table$Model, keyword)
  return(data.frame(data,wcount))
}
Mf_E_psi <- getCount(qaicMf$table$Model,'Elevation')
Mf_Ar_psi <- getCount(qaicMf$table$Model,'Area')
Mf_Rn_psi <- getCount(qaicMf$table$Model,'Rubus_niveus')
Mf_As_psi <- getCount(qaicMf$table$Model,'Aspect')
Mf_Sl_psi <- getCount(qaicMf$table$Model,'Slope')
Mf_Ch_psi <- getCount(qaicMf$table$Model,'Canopy_height')
Mf_Uh_psi <- getCount(qaicMf$table$Model,'Understorey_height')
Mf_V_psi <- getCount(qaicMf$table$Model,'Visibility')
Mf_B_psi <- getCount(qaicMf$table$Model,'Burn')
Mf_Mr_psi <- getCount(qaicMf$table$Model,'Metalled_road')
Mf_Dr_psi <- getCount(qaicMf$table$Model,'Dirt_road')
Mf_Nr_psi <- getCount(qaicMf$table$Model,'No_road')
Mf_L_psi <- getCount(qaicMf$table$Model,'Lentic')
Mf_Wl_psi <- getCount(qaicMf$table$Model,'Wet_lotic')
Mf_Dl_psi <- getCount(qaicMf$table$Model,'Dry_lotic')

#Count for each covariate. Order the covariates in the next three lists to match...
#...in the final dataframe:
Covar_Mf_psi <- rbind('Area', 'Aspect', 'Burn', 'Canopy_height', 'Dry_lotic',
                      'Dirt_road', 'Elevation', 'Lentic', 'Metalled_road', 
                      'No_road', 'Rubus_niveus', 'Slope', 'Understorey_height',
                      'Visibility', 'Wet_lotic')

Sum_Mf_psi <-rbind (
  (sum(Mf_Ar_psi$wcount)),(sum(Mf_As_psi$wcount)),(sum(Mf_B_psi$wcount)),
  (sum(Mf_Ch_psi$wcount)),(sum(Mf_Dl_psi$wcount)),(sum(Mf_Dr_psi$wcount)),
  (sum(Mf_E_psi$wcount)),(sum(Mf_L_psi$wcount)),(sum(Mf_Mr_psi$wcount)),
  (sum(Mf_Nr_psi$wcount)),(sum(Mf_Rn_psi$wcount)),(sum(Mf_Sl_psi$wcount)),
  (sum(Mf_Uh_psi$wcount)),(sum(Mf_V_psi$wcount)),(sum(Mf_Wl_psi$wcount))                                                
)

Crossprod_Mf_psi <-rbind (
                          (crossprod(qaicMf$table$wgt,Mf_Ar_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_As_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_B_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_Ch_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_Dl_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_Dr_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_E_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_L_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_Mr_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_Nr_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_Rn_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_Sl_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_Uh_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_V_psi$wcount)),
                          (crossprod(qaicMf$table$wgt,Mf_Wl_psi$wcount))                                                
                        )

Mfpsi_cov <-as.data.frame(cbind(Covar_Mf_psi, Sum_Mf_psi, Crossprod_Mf_psi))
#Add column names:
Mfpsi_cov <- setNames(Mfpsi_cov,cbind("Covariates","Count",
                                      "Sum_weight"))
Mfpsi_cov

#Write results to a csv:
write.csv(Mfpsi_cov,
          "/media/Dissertation/Paper/Mfpsi_cov.csv")

#Save as word document:
Mfpsi_cov_tbl <- flextable(Mfpsi_cov)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = Mfpsi_cov_tbl) %>%  #Insert the flextable object there
  print(target = "Mfpsi_cov_tbl.docx")  


###### 4.1.2 On p #####


#Using for loop as a way to reduce typing and copy-pasting:
Mfbp_list <- list()               #Create empty list
i <-1                        #Create index for the purpose of saving the output.
# list()
for(Given_names in Mf_given_names){
  output <- print(paste0("cbind(coef(object = ", Given_names,
                         ", param = 'p', prob = 0.95)),"))
  Mfbp_list[[i]] <- output 
}
#Convert it to a dataframe and print the output without the row names (numbers):
Mfbp_list <- as.data.frame(Mfbp_list)
print(Mfbp_list, row.names = FALSE)

#Copy paste the results from console. Then create the output 'Betas_of_p_Mf' by...
#...binding rows. You can skip this since this has already been done:

Betas_of_p_Mf <- rbind(
      cbind(coef(object = psi_dot_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_E_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Ar_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Rn_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_E_p_Wa, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_As_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Sl_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Ch_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Uh_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_V_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_B_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Mr_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Dr_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_L_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Wl_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Dl_p_dot, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Ar_p_exp2, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Ar_p_SurPoly, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_BnPlusC, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_PlusBnCWi, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_PlusCWiO, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_E_p_BnPlusC, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_E_p_PlusBnCWi, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_E_p_PlusCWiO, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_Bn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_An, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_Cs, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_O, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_Sun, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_Wi, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_Wa, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_N, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_C, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_Sur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_Wi, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Dr_p_Wi, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_WiPlusSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Dr_p_Sur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Sl_p_WiPlusSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_E_p_WiPlusSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_V_p_WiPlusSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Dl_p_WiPlusSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Ar_p_WiPlusSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Uh_p_WiPlusSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Rn_p_WiPlusSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_As_p_WiPlusSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_Bn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_WiPlusBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_CsPlusBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Sl_p_CsPlusBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_E_p_CsPlusBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Dr_p_CsPlusBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Dl_p_CsPlusBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_V_p_CsPlusBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Ar_p_CsPlusBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Ar_p_PlusWiSurBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_PlusWiSurBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_E_p_PlusWiSurBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Rn_p_PlusWiSurBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Sl_p_PlusWiSurBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Dl_p_PlusWiSurBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_V_p_PlusWiSurBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_As_p_PlusWiSurBn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Ar_p_PlusWiSurAn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_PlusWiSurAn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_E_p_PlusWiSurAn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Rn_p_PlusWiSurAn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Sl_p_PlusWiSurAn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Dl_p_PlusWiSurAn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_V_p_PlusWiSurAn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_As_p_PlusWiSurAn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_V, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_VPlusWi, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_PlusVWiSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Nr_p_PlusVWiAn, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_Sl_p_PlusVWiSur, param = 'p', prob = 0.95)),
      cbind(coef(object = psi_dot_p_PlusVWiSur, param = 'p', prob = 0.95)))
  
str(Betas_of_p_Mf)
#Check which site-specific covariates appear in the betas of psi of the models:
View(Betas_of_p_Mf)

#11 covariates appear. Collect only the rows which contain covariate betas:
Mfbp1 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "Warm",]
Mfbp2 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "survey",]
Mfbp3 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "BefNoon",]
Mfbp4 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "Cool",]
Mfbp5 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "Windy",]
Mfbp6 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "Ovrcst",]
Mfbp7 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "AftNoon",]
Mfbp8 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "ClrSky",]
Mfbp9 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "Sunny",]
Mfbp10 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "Neutral",]
Mfbp11 <- Betas_of_p_Mf[rownames(Betas_of_p_Mf) %like% "Visibility",]

Mfbp <- rbind(Mfbp1,Mfbp2,Mfbp3,Mfbp4,Mfbp5,Mfbp6,Mfbp7,Mfbp8,Mfbp9,Mfbp10,
              Mfbp11)

#Remove unnecessary dataframes to remove clutter:
remove(Mfbp1,Mfbp2,Mfbp3,Mfbp4,Mfbp5,Mfbp6,Mfbp7,Mfbp8,Mfbp9,Mfbp10,Mfbp11)

#Observe the standard errors and confidence intervals of betas of various covariates:
Mfbp
str(Mfbp)

#Filter out betas which have 95% CI which pass through zero:
Mfbpf <- rbind(neg <- subset(Mfbp, lowerCI<0 & upperCI<0),
              pos <- subset(Mfbp, lowerCI>0 & upperCI>0))
View(Mfbpf)

#Both of these dataframes gives a lot of information about the covariates.
#CI range is reasonably high for all the covariates in all the models. Assess them...
#...case by case:
# 1. 'Warm' - in all models: SE is higer than estimate (~4 times). Very low...
#...certainty of influence on p.
# 2.'survey' - in all models: SE is less than estimate, but high. Moderate certainty...
#...of influence on p. Effect likely positive as corresponding betas positive.
# 3. 'Windy' - beta estimates of "Windy" rarely pass through 0 in any models where...
#...they alone act as a covariate of det. prob., but it does in models where...
#...'survey' is also a covariate. This indicates that 'Windy' likely has a strong...
#...effect on p. Effect likely negative as corresponding betas negative.
# 4. 'Visibility' - in around half of the models (with the covariate survey),...
#...SE < estimate, but high, in others (alone) SE > estimate (as large 90...
#...times in one). Very low certainty of influence on p.
# 5. 'Before noon' - in all models: SE is less than estimate, but high (nearly half...
#...or more of est. Moderate certainty of influence on p. Effect likely positive...
#...as corresponding betas positive.
# 6. 'Cool' - in all models: SE is higer than estimate (slightly higher than est)...
#...Low certainty of influence on p.
# 7.'Overcast' - out of three models, two have SE < estimate, but high. Other one...
#...has SE around half times higher than estimate. Low to moderate certainty of...
#...influence on p. Effect likely positive as corresponding betas positive.
# 8. 'After noon' - in all models: SE is less than estimate, but high. Moderate...
#...certainty of influence on p. Effect likely negative as corresponding betas...
#...negative.
# 10. 'Clear sky' - in all models except one: SE < est, but high. In one model,...
#...SE > est. Moderate certainty of influence on p. Effect likely negative as...
#...corresponding betas negative.
# 11. 'Neutral' - Only one model where SE >> est. So, very low certainty of influence...
#...on p.

#Count the number of time each covariate appears in models (using the getCount...
#...defined in the earlier section):
Mf_Wa_p <- getCount(qaicMf$table$Model,'Warm')
Mf_Sur_p <- getCount(qaicMf$table$Model,'survey')
Mf_Bn_p <- getCount(qaicMf$table$Model,'BefNoon')
Mf_C_p <- getCount(qaicMf$table$Model,'Cool')
Mf_Wi_p <- getCount(qaicMf$table$Model,'Windy')
Mf_O_p <- getCount(qaicMf$table$Model,'Ovrcst')
Mf_An_p <- getCount(qaicMf$table$Model,'AftNoon')
Mf_Cs_p <- getCount(qaicMf$table$Model,'ClrSky')
Mf_Sun_p <- getCount(qaicMf$table$Model,'Sunny')
Mf_N_p <- getCount(qaicMf$table$Model,'Neutral')
Mf_V_p <- getCount(qaicMf$table$Model,'Visibility')

#Count for each covariate. Order the covariates in the next three lists to match...
#...in the final dataframe:
Covar_Mf_p <- rbind('Warm', 'survey', 'BefNoon', 'Cool', 'Windy','Ovrcst',
                     'AftNoon', 'ClrSky', 'Sunny', 'Neutral', 'Visibility')

Sum_Mf_p <-rbind (
  (sum(Mf_Wa_p$wcount)),(sum(Mf_Sur_p$wcount)),(sum(Mf_Bn_p$wcount)),
  (sum(Mf_C_p$wcount)),(sum(Mf_Wi_p$wcount)),(sum(Mf_O_p$wcount)),
  (sum(Mf_An_p$wcount)),(sum(Mf_Cs_p$wcount)),(sum(Mf_Sun_p$wcount)),
  (sum(Mf_N_p$wcount)),(sum(Mf_V_p$wcount))                                                
                  )

Crossprod_Mf_p <-rbind (
                        (crossprod(qaicMf$table$wgt,Mf_Wa_p$wcount)),
                        (crossprod(qaicMf$table$wgt,Mf_Sur_p$wcount)),
                        (crossprod(qaicMf$table$wgt,Mf_Bn_p$wcount)),
                        (crossprod(qaicMf$table$wgt,Mf_C_p$wcount)),
                        (crossprod(qaicMf$table$wgt,Mf_Wi_p$wcount)),
                        (crossprod(qaicMf$table$wgt,Mf_O_p$wcount)),
                        (crossprod(qaicMf$table$wgt,Mf_An_p$wcount)),
                        (crossprod(qaicMf$table$wgt,Mf_Cs_p$wcount)),
                        (crossprod(qaicMf$table$wgt,Mf_Sun_p$wcount)),
                        (crossprod(qaicMf$table$wgt,Mf_N_p$wcount)),
                        (crossprod(qaicMf$table$wgt,Mf_V_p$wcount))
                        )

Mfp_cov <-as.data.frame(cbind(Covar_Mf_p, Sum_Mf_p, Crossprod_Mf_p))
#Add column names:
Mfp_cov <- setNames(Mfp_cov,cbind("Covariates","Count", "Sum_weight"))
Mfp_cov

#Note that there will be additional count and weight where models are polynomial
#...(eg: model 3.1.35 where p is a function of survey + survey+I(survey^2)). This...
#...is because the 'getCount' function counts for the specified covariate name within...
#...the model list and gives the total number (even if repeating). Since there is...
#...only one such case in this entire script and since the covariate 'survey' is...
#...already known to be a very significant covariate from the count and weight...
#...it can be ignored (there is no need for modifying the counting function).

#Write results to a csv:
write.csv(Mfp_cov,
          "/media/Dissertation/Paper/Mfp_cov.csv")

#Save as word document:
Mfp_cov_tbl <- flextable(Mfp_cov)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = Mfp_cov_tbl) %>%  #Insert the flextable object there
  print(target = "Mfp_cov_tbl.docx")  


##### 4.2 S. albiventris #####


#Collect the manually given names of the models in the final model set:
Sa_given_names <-  data.frame(rbind("s8", "s17", "s28", "s29", "s30", "s31",
                    "s38", "s42", "s43", "s45", "s48", "s49"))
str(Sa_given_names)
#Give names to column
Sa_given_names <- setNames(Sa_given_names,cbind("Given_names"))

#Till here, the steps are common for both betas of psi and p

###### 4.2.1 On psi #####

#Using for loop as a way to reduce typing and copy-pasting:
Sabpsi_list <- list()               #Create empty list
i <-1                        #Create index for the purpose of saving the output.
# list()
for(Given_names in Sa_given_names){
  output <- print(paste0("coef(object = ", Given_names,
                         ", param = 'psi', prob = 0.95),"))
  Sabpsi_list[[i]] <- output 
}
#Convert it to a dataframe and print the output without the row names (numbers):
Sabpsi_list <- as.data.frame(Sabpsi_list)
print(Sabpsi_list, row.names = FALSE)

#Copy paste the results from console. Then create the output 'Betas_of_psi_Sa' by...
#...binding rows:

Betas_of_psi_Sa <-  rbind(coef(object = s8, param = 'psi', prob = 0.95),
                          coef(object = s17, param = 'psi', prob = 0.95),
                          coef(object = s28, param = 'psi', prob = 0.95),
                          coef(object = s29, param = 'psi', prob = 0.95),
                          coef(object = s30, param = 'psi', prob = 0.95),
                          coef(object = s31, param = 'psi', prob = 0.95),
                          coef(object = s38, param = 'psi', prob = 0.95),
                          coef(object = s42, param = 'psi', prob = 0.95),
                          coef(object = s43, param = 'psi', prob = 0.95),
                          coef(object = s45, param = 'psi', prob = 0.95),
                          coef(object = s48, param = 'psi', prob = 0.95),
                          coef(object = s49, param = 'psi', prob = 0.95))

str(Betas_of_psi_Sa)

#Check which site-specific covariates appear in psi of the models:
Betas_of_psi_Sa
#Three covariates appear.

#Collect only those rows containing betas of psi of covariates:
Sabpsi1 <- Betas_of_psi_Sa[rownames(Betas_of_psi_Sa) %like% "Elevation",]
Sabpsi2 <- Betas_of_psi_Sa[rownames(Betas_of_psi_Sa) %like% "Lantana_camara",]
Sabpsi3 <- Betas_of_psi_Sa[rownames(Betas_of_psi_Sa) %like% "Understorey_height",]
Sabpsi <- rbind(Sabpsi1,Sabpsi2,Sabpsi3)

#Remove unnecessary dataframes to remove clutter:
remove(Sabpsi1,Sabpsi2,Sabpsi3)

#Observe the standard errors and confidence intervals of betas of various covariates:
Sabpsi

#Filter out betas which have 95% CI which pass through zero:
Sabpsif <- rbind(neg <- subset(Sabpsi, lowerCI<0 & upperCI<0),
                 pos <- subset(Sabpsi, lowerCI>0 & upperCI>0))
Sabpsif

#CI range is reasonably high for all the covariates in all the models, but none...
#...of 95% CIs of the betas contain zero. Assess them case by case:
# 1. 'Elevation' - in all models, SE is less than estimate, and less than half.
#...High certainty of influence on psi. Effect likely positive as corresponding...
#...betas positive.
# 2. 'Lantana camara' - in all models, SE << estimate. Very high certainty of...
#...influence on psi. Effect likely negative as corresponding betas negative.
# 3. 'Understorey height' - only in one model, but SE < estimate, and less than...
#...half. Moderate to high certainty of influence on psi. Effect likely negative...
#...as corresponding betas negative.

#Count the number of time each covariate appears in models:
getCount <- function(data,keyword)
{
  wcount <- str_count(qaicSa$table$Model, keyword)
  return(data.frame(data,wcount))
}
Sa_E_psi <- getCount(qaicSa$table$Model,'Elevation')
Sa_Lc_psi <- getCount(qaicSa$table$Model,'Lantana_camara')
Sa_Uh_psi <- getCount(qaicSa$table$Model,'Understorey_height')

#Count for each covariate. Order the covariates in the next three lists to match...
#...in the final dataframe:
Covar_Sa_psi <- rbind('Elevation', 'Lantana_camara', 'Understorey_height')

Sum_Sa_psi <-rbind (
      (sum(Sa_E_psi$wcount)),(sum(Sa_Lc_psi$wcount)),(sum(Sa_Uh_psi$wcount)))

Crossprod_Sa_psi <-rbind ((crossprod(qaicSa$table$wgt,Sa_E_psi$wcount)),
                          (crossprod(qaicSa$table$wgt,Sa_Lc_psi$wcount)),
                          (crossprod(qaicSa$table$wgt,Sa_Uh_psi$wcount)))                                             

Sapsi_cov <-as.data.frame(cbind(Covar_Sa_psi, Sum_Sa_psi, Crossprod_Sa_psi))
#Add column names:
Sapsi_cov <- setNames(Sapsi_cov,cbind("Covariates","Count","Sum_weight"))
Sapsi_cov

#Write results to a csv:
write.csv(Sapsi_cov,
          "/media/Dissertation/Paper/Sapsi_cov.csv")

#Save as word document:
Sapsi_cov_tbl <- flextable(Sapsi_cov)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = Sapsi_cov_tbl) %>%  #Insert the flextable object there
  print(target = "Sapsi_cov_tbl.docx")  

###### 4.2.2 On p #####

#Using for loop as a way to reduce typing and copy-pasting:
Sabp_list <- list()               #Create empty list
i <-1                        #Create index for the purpose of saving the output.
# list()
for(Given_names in Sa_given_names){
  output <- print(paste0("coef(object = ", Given_names,
                         ", param = 'p', prob = 0.95),"))
  Sabp_list[[i]] <- output 
}
#Convert it to a dataframe and print the output without the row names (numbers):
Sabp_list <- as.data.frame(Sabp_list)
print(Sabp_list, row.names = FALSE)

#Copy paste the results from console. Then create the output 'Betas_of_p_Sa' by...
#...binding rows. You can skip this since this has already been done:

Betas_of_p_Sa <- rbind(coef(object = s8, param = 'p', prob = 0.95),
                       coef(object = s17, param = 'p', prob = 0.95),
                       coef(object = s28, param = 'p', prob = 0.95),
                       coef(object = s29, param = 'p', prob = 0.95),
                       coef(object = s30, param = 'p', prob = 0.95),
                       coef(object = s31, param = 'p', prob = 0.95),
                       coef(object = s38, param = 'p', prob = 0.95),
                       coef(object = s42, param = 'p', prob = 0.95),
                       coef(object = s43, param = 'p', prob = 0.95),
                       coef(object = s45, param = 'p', prob = 0.95),
                       coef(object = s48, param = 'p', prob = 0.95),
                       coef(object = s49, param = 'p', prob = 0.95))
str(Betas_of_p_Sa)
#Check which site-specific covariates appear in the betas of psi of the models:
Betas_of_p_Sa
#Four covariates appear in the data. Collect only those rows containing betas of...
#...p of covariates:
Sabp1 <- Betas_of_p_Sa[rownames(Betas_of_p_Sa) %like% "Neutral",]
Sabp2 <- Betas_of_p_Sa[rownames(Betas_of_p_Sa) %like% "survey",]
Sabp3 <- Betas_of_p_Sa[rownames(Betas_of_p_Sa) %like% "Warm",]
Sabp4 <- Betas_of_p_Sa[rownames(Betas_of_p_Sa) %like% "Cool",]
Sabp <- rbind(Sabp1,Sabp2,Sabp3,Sabp4)

#Remove unnecessary dataframes to remove clutter:
remove(Sabp1,Sabp2,Sabp3,Sabp4)

#Observe the standard errors and confidence intervals of betas of various covariates:
Sabp
str(Sabp)

#Filter out betas which have 95% CI which pass through zero:
Sabpf <- rbind(neg <- subset(Sabp, lowerCI<0 & upperCI<0),
               pos <- subset(Sabp, lowerCI>0 & upperCI>0))
Sabpf

#CI range is slightly high for all the covariates in all the models. Assess them...
#...case by case:
# 1. 'Neutral' - in all models alone, SE < estimate, and less than half, but...
#...with 'survey', the SE slightly increases. In many models, the covariate doesn't...
#...have 0 in the 95% CIs of the betas. High certainty of influence on p. Effect...
#...likely positive as corresponding betas positive.
# 2. 'survey'- in all models, SE < estimate,  but high. Moderate certainty of...
#...influence on p. Effect likely positive as corresponding betas positive.
# 3. 'Warm' - only in two models, in one SE < estimate, but high, and in another...
#...SE > est. Low certainty of influence on p. 
# 4. 'Cool' - only in two models, in both SE > estimate. Very low certainty of...
#...influence on p. 

#Count the number of time each covariate appears in models (using the getCount...
#...defined in the earlier section):
Sa_N_p <- getCount(qaicSa$table$Model,'Neutral')
Sa_Sur_p <- getCount(qaicSa$table$Model,'survey')
Sa_Wa_p <- getCount(qaicSa$table$Model,'Warm')
Sa_C_p <- getCount(qaicSa$table$Model,'Cool')

#Count for each covariate. Order the covariates in the next three lists to match...
#...in the final dataframe:
Covar_Sa_p <- rbind('Neutral', 'survey', 'Warm', 'Cool')

Sum_Sa_p <-rbind ((sum(Sa_N_p$wcount)),(sum(Sa_Sur_p$wcount)),
                  (sum(Sa_Wa_p$wcount)),(sum(Sa_C_p$wcount)))

Crossprod_Sa_p <-rbind ((crossprod(qaicSa$table$wgt,Sa_N_p$wcount)),
                        (crossprod(qaicSa$table$wgt,Sa_Sur_p$wcount)),
                        (crossprod(qaicSa$table$wgt,Sa_Wa_p$wcount)),
                        (crossprod(qaicSa$table$wgt,Sa_C_p$wcount)))

Sap_cov <-as.data.frame(cbind(Covar_Sa_p, Sum_Sa_p, Crossprod_Sa_p))
#Add column names:
Sap_cov <- setNames(Sap_cov,cbind("Covariates","Count", "Sum_weight"))
Sap_cov

#Write results to a csv:
write.csv(Sap_cov,
          "/media/Dissertation/Paper/Sap_cov.csv")

#Save as word document:
Sap_cov_tbl <- flextable(Sap_cov)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = Sap_cov_tbl) %>%  #Insert the flextable object there
  print(target = "Sap_cov_tbl.docx")  


##### 4.3 Co-occurrence models #####


###### 4.3.1. MS format: Species A = M. fairbankii, Species B = S. albiventris ######

#Collect the manually given names of the models in the final model set:
Tsp_given_names <-  data.frame(rbind("c46", "c47", "c74", "c108", "c109",
                                     "c111", "c117", "c121", "c122", "c123"))

str(Tsp_given_names)
#Give names to column
Tsp_given_names <- setNames(Tsp_given_names,cbind("Given_names"))

#Till here, the steps are common for both betas of psi and p

###### 4.3.1.1 On psi #####

#Using for loop as a way to reduce typing and copy-pasting:
Tspbpsi_list <- list()               #Create empty list
i <-1                        #Create index for the purpose of saving the output.
# list()
for(Given_names in Tsp_given_names){
  output <- print(paste0("coef(object = ", Given_names,
                         ", param = 'psi', prob = 0.95),"))
  Tspbpsi_list[[i]] <- output 
}
#Convert it to a dataframe and print the output without the row names (numbers):
Tspbpsi_list <- as.data.frame(Tspbpsi_list)
print(Tspbpsi_list, row.names = FALSE)

#Copy paste the results from console. Then create the output 'Betas_of_psi_Tsp' by...
#...binding rows:

Betas_of_psi_Tsp <- rbind(coef(object = c46, param = 'psi', prob = 0.95),
                          coef(object = c47, param = 'psi', prob = 0.95),
                          coef(object = c74, param = 'psi', prob = 0.95),
                          coef(object = c108, param = 'psi', prob = 0.95),
                          coef(object = c109, param = 'psi', prob = 0.95),
                          coef(object = c111, param = 'psi', prob = 0.95),
                          coef(object = c117, param = 'psi', prob = 0.95),
                          coef(object = c121, param = 'psi', prob = 0.95),
                          coef(object = c122, param = 'psi', prob = 0.95),
                          coef(object = c123, param = 'psi', prob = 0.95))
str(Betas_of_psi_Tsp)
#Check which site-specific covariates appear in psi of the models:
Betas_of_psi_Tsp
#Four covariates appear.

#Collect only those rows containing betas of psi of covariates:
cbpsi1 <- Betas_of_psi_Tsp[rownames(Betas_of_psi_Tsp) %like% "Elevation",]
cbpsi2 <- Betas_of_psi_Tsp[rownames(Betas_of_psi_Tsp) %like% "No_road",]
cbpsi3 <- Betas_of_psi_Tsp[rownames(Betas_of_psi_Tsp) %like% "Lantana_camara",]
cbpsi4 <- Betas_of_psi_Tsp[rownames(Betas_of_psi_Tsp) %like% "Area",]

cbpsi <- rbind(cbpsi1,cbpsi2,cbpsi3,cbpsi4)
cbpsi
#Remove unnecessary dataframes to remove clutter:
remove(cbpsi1,cbpsi2,cbpsi3,cbpsi4)

#Filter out betas which have 95% CI which pass through zero:
cbpsif <- rbind(neg <- subset(cbpsi, lowerCI<0 & upperCI<0),
                pos <- subset(cbpsi, lowerCI>0 & upperCI>0))
cbpsif

#CI range is reasonably high for all the covariates in all the models, but almost...
#...none of 95% CIs of the betas contain zero. Assess them case by case:
# 1. 'Elevation' - in all models, SE is less than estimate, and less than half.
#...High certainty of influence on psi. Effect likely positive as corresponding...
#...betas positive.
# 2. 'No road' - in one models, but SE is less than estimate, and less than half.
#...Moderate to high certainty of influence on psi. Effect likely negative as...
#...corresponding betas negative.
# 3. 'Lantana camara' - in all models, SE << estimate. Very high certainty of...
#...influence on psi. Effect likely negative as corresponding betas negative.
# 4. 'Area' - only in one model, but SE < estimate, and less than half. Moderate...
#...to high certainty of influence on psi. Effect likely positive as corresponding...
#...betas positive.

#Count the number of time each covariate appears in models:
getCount <- function(data,keyword)
{
  wcount <- str_count(qaicTspf$table$Model, keyword)
  return(data.frame(data,wcount))
}
Tsp_E_psi <- getCount(qaicTspf$table$Model,'Elevation')
Tsp_Nr_psi <- getCount(qaicTspf$table$Model,'No_road')
Tsp_Lc_psi <- getCount(qaicTspf$table$Model,'Lantana_camara')
Tsp_Ar_psi <- getCount(qaicTspf$table$Model,'Area')

#Count for each covariate. Order the covariates in the next three lists to match...
#...in the final dataframe:
Covar_Tsp_psi <- rbind('Elevation', 'No_road', 'Lantana_camara', 'Area')

Sum_Tsp_psi <-rbind (
  (sum(Tsp_E_psi$wcount)),(sum(Tsp_Nr_psi$wcount)),(sum(Tsp_Lc_psi$wcount)),
  (sum(Tsp_Ar_psi$wcount)))

Crossprod_Tsp_psi <-rbind ((crossprod(qaicTspf$table$wgt,Tsp_E_psi$wcount)),
                           (crossprod(qaicTspf$table$wgt,Tsp_Nr_psi$wcount)),
                           (crossprod(qaicTspf$table$wgt,Tsp_Lc_psi$wcount)),
                           (crossprod(qaicTspf$table$wgt,Tsp_Ar_psi$wcount)))                                             

Tsppsi_cov <-as.data.frame(cbind(Covar_Tsp_psi, Sum_Tsp_psi, Crossprod_Tsp_psi))
#Add column names:
Tsppsi_cov <- setNames(Tsppsi_cov,cbind("Covariates","Count","Sum_weight"))
Tsppsi_cov

#Write results to a csv:
write.csv(Tsppsi_cov,
          "/media/Dissertation/Paper/Tsppsi_cov.csv")

#Save as word document:
Tsppsi_cov_tbl <- flextable(Tsppsi_cov)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = Tsppsi_cov_tbl) %>%  #Insert the flextable object there
  print(target = "Tsppsi_cov_tbl.docx")  

###### 4.3.2.2 On p #####

#Using for loop as a way to reduce typing and copy-pasting:
Tspbp_list <- list()               #Create empty list
i <-1                        #Create index for the purpose of saving the output.
# list()
for(Given_names in Tsp_given_names){
  output <- print(paste0("coef(object = ", Given_names,
                         ", param = 'p', prob = 0.95),"))
  Tspbp_list[[i]] <- output 
}
#Convert it to a dataframe and print the output without the row names (numbers):
Tspbp_list <- as.data.frame(Tspbp_list)
print(Tspbp_list, row.names = FALSE)

#Copy paste the results from console. Then create the output 'Betas_of_p_Tsp' by...
#...binding rows. You can skip this since this has already been done:
Betas_of_p_Tsp <- rbind(coef(object = c46, param = 'p', prob = 0.95),
                          coef(object = c47, param = 'p', prob = 0.95),
                          coef(object = c74, param = 'p', prob = 0.95),
                          coef(object = c108, param = 'p', prob = 0.95),
                          coef(object = c109, param = 'p', prob = 0.95),
                          coef(object = c111, param = 'p', prob = 0.95),
                          coef(object = c117, param = 'p', prob = 0.95),
                          coef(object = c121, param = 'p', prob = 0.95),
                          coef(object = c122, param = 'p', prob = 0.95),
                          coef(object = c123, param = 'p', prob = 0.95))

str(Betas_of_p_Tsp)
#Check which site-specific covariates appear in the betas of psi of the models:
Betas_of_p_Tsp
#Only one covariate appear ("Before noon"). Collect those rows containing...
#...betas of p of covariates.
cbp <- Betas_of_p_Tsp[rownames(Betas_of_p_Tsp) %like% "BefNoon",]

#Observe the standard errors and confidence intervals of betas of various covariates:
cbp
str(cbp)

#Filter out betas which have 95% CI which pass through zero:
Tspbp <- rbind(neg <- subset(cbp, lowerCI<0 & upperCI<0),
               pos <- subset(cbp, lowerCI>0 & upperCI>0))
Tspbp

#Only one covariate, and the CI of all beta estimates pass through 0:
# 1. 'Before noon' SE  slightly greater than est. So, low certainty of influence...
#...of det. prob..

#Count the number of time each covariate appears in models:
Tsp_Bn_p <- getCount(qaicTspf$table$Model,'BefNoon')

#Find the count and weight of the covariate, and set names to columns:
Tspp_cov <- dplyr::bind_cols('BefNoon', (sum(Tsp_Bn_p$wcount)),
                            crossprod(qaicTspf$table$wgt,Tsp_Bn_p$wcount))
Tspp_cov <- setNames(Tspp_cov,cbind("Covariates","Count", "Sum_weight"))
Tspp_cov

#Write results to a csv:
write.csv(Tspp_cov,
          "/media/Dissertation/Paper/Tspp_cov.csv")

#Save as word document:
Tspp_cov_tbl <- flextable(Tspp_cov)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = Tspp_cov_tbl) %>%  #Insert the flextable object there
  print(target = "Tspp_cov_tbl.docx")  

###### 4.3.2. SM format: Species A = S. albiventris, Species B = M. fairbankii ######

#Collect the manually given names of the models in the final model set:
Tspn_given_names <-  data.frame(rbind("nc46","nc47","nc73","nc109","nc111","nc117",
                        "nc121","nc122","nc126","nc128","nc130","nc131"))

str(Tspn_given_names)
#Give names to column
Tspn_given_names <- setNames(Tspn_given_names,cbind("Given_names"))

#Till here, the steps are common for both betas of psi and p

###### 4.3.2.1 On psi #####

#Using for loop as a way to reduce typing and copy-pasting:
Tspnbpsi_list <- list()               #Create empty list
i <-1                        #Create index for the purpose of saving the output.
# list()
for(Given_names in Tspn_given_names){
  output <- print(paste0("coef(object = ", Given_names,
                         ", param = 'psi', prob = 0.95),"))
  Tspnbpsi_list[[i]] <- output 
}
#Convert it to a dataframe and print the output without the row names (numbers):
Tspnbpsi_list <- as.data.frame(Tspnbpsi_list)
print(Tspnbpsi_list, row.names = FALSE)

#Copy paste the results from console. Then create the output 'Betas_of_psi_Tsp' by...
#...binding rows:

Betas_of_psi_Tspn <- rbind(coef(object = nc46, param = 'psi', prob = 0.95),
                          coef(object = nc47, param = 'psi', prob = 0.95),
                          coef(object = nc73, param = 'psi', prob = 0.95),
                          coef(object = nc109, param = 'psi', prob = 0.95),
                          coef(object = nc111, param = 'psi', prob = 0.95),
                          coef(object = nc117, param = 'psi', prob = 0.95),
                          coef(object = nc121, param = 'psi', prob = 0.95),
                          coef(object = nc122, param = 'psi', prob = 0.95),
                          coef(object = nc126, param = 'psi', prob = 0.95),
                          coef(object = nc128, param = 'psi', prob = 0.95),
                          coef(object = nc130, param = 'psi', prob = 0.95),
                          coef(object = nc131, param = 'psi', prob = 0.95))
str(Betas_of_psi_Tspn)
#Check which site-specific covariates appear in psi of the models:
Betas_of_psi_Tspn
#Four covariates appear.

#Collect only those rows containing betas of psi of covariates:
cnbpsi1 <- Betas_of_psi_Tspn[rownames(Betas_of_psi_Tspn) %like% "Elevation",]
cnbpsi2 <- Betas_of_psi_Tspn[rownames(Betas_of_psi_Tspn) %like% "No_road",]
cnbpsi3 <- Betas_of_psi_Tspn[rownames(Betas_of_psi_Tspn) %like% "Lantana_camara",]
cnbpsi4 <- Betas_of_psi_Tspn[rownames(Betas_of_psi_Tspn) %like% "Area",]

cnbpsi <- rbind(cnbpsi1,cnbpsi2,cnbpsi3,cnbpsi4)
cnbpsi #You can see that cbpsi (for MS format) has lesser no. of rows.
#Remove unnecessary dataframes to remove clutter:
remove(cnbpsi1,cnbpsi2,cnbpsi3,cnbpsi4)

#Filter out betas which have 95% CI which pass through zero:
cnbpsif <- rbind(neg <- subset(cnbpsi, lowerCI<0 & upperCI<0),
                 pos <- subset(cnbpsi, lowerCI>0 & upperCI>0))
cnbpsif

#CI range reasonably high for all the covariates in all the models, but almost...
#...none of 95% CIs of the betas contain zero. Assess them case by case:
# 1. 'Elevation' - in all models, SE is less than estimate, and less than half.
#...High certainty of influence on psi. Effect likely positive as corresponding...
#...betas positive.
# 2. 'No road' - in two models, but SE is less than estimate, and less than half.
#...Moderate to high certainty of influence on psi. Effect likely negative as...
#...corresponding betas negative.
# 3. 'Lantana camara' - in all models, SE << estimate. Very high certainty of...
#...influence on psi. Effect likely negative as corresponding betas negative.
# 4. 'Area' - only in two models, but SE < estimate, and less than half. Moderate...
#...to high certainty of influence on psi. Effect likely positive as corresponding...
#...betas positive.

#Note that the interpretation is functionally the same for psi in both MS and SM...
#...format.

#Count the number of time each covariate appears in models:
getCount <- function(data,keyword)
{
  wcount <- str_count(qaicTspfn$table$Model, keyword)
  return(data.frame(data,wcount))
}
Tspn_E_psi <- getCount(qaicTspfn$table$Model,'Elevation')
Tspn_Nr_psi <- getCount(qaicTspfn$table$Model,'No_road')
Tspn_Lc_psi <- getCount(qaicTspfn$table$Model,'Lantana_camara')
Tspn_Ar_psi <- getCount(qaicTspfn$table$Model,'Area')

#Count for each covariate. Order the covariates in the next three lists to match...
#...in the final dataframe:

Sum_Tspn_psi <-rbind (
  (sum(Tspn_E_psi$wcount)),(sum(Tspn_Nr_psi$wcount)),(sum(Tspn_Lc_psi$wcount)),
  (sum(Tspn_Ar_psi$wcount)))

Crossprod_Tspn_psi <-rbind ((crossprod(qaicTspfn$table$wgt,Tspn_E_psi$wcount)),
                           (crossprod(qaicTspfn$table$wgt,Tspn_Nr_psi$wcount)),
                           (crossprod(qaicTspfn$table$wgt,Tspn_Lc_psi$wcount)),
                           (crossprod(qaicTspfn$table$wgt,Tspn_Ar_psi$wcount)))                                             

Tspnpsi_cov <-as.data.frame(cbind(Covar_Tsp_psi, Sum_Tspn_psi, Crossprod_Tspn_psi))
Covar_Tsp_psi = rbind('Elevation', 'No_road', 'Lantana_camara', 'Area')
#Add column names:
Tspnpsi_cov <- setNames(Tspnpsi_cov,cbind("Covariates","Count","Sum_weight"))
Tspnpsi_cov

#Write results to a csv:
write.csv(Tspnpsi_cov,
          "/media/Dissertation/Paper/Tspnpsi_cov.csv")

#Save as word document:
Tspnpsi_cov_tbl <- flextable(Tspnpsi_cov)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = Tspnpsi_cov_tbl) %>%  #Insert the flextable object there
  print(target = "Tspnpsi_cov_tbl.docx")  

###### 4.3.2.2 On p #####

#Using for loop as a way to reduce typing and copy-pasting:
Tspnbp_list <- list()               #Create empty list
i <-1                        #Create index for the purpose of saving the output.
# list()
for(Given_names in Tspn_given_names){
  output <- print(paste0("coef(object = ", Given_names,
                         ", param = 'p', prob = 0.95),"))
  Tspnbp_list[[i]] <- output 
}
#Convert it to a dataframe and print the output without the row names (numbers):
Tspnbp_list <- as.data.frame(Tspnbp_list)
print(Tspnbp_list, row.names = FALSE)

#Copy paste the results from console. Then create the output 'Betas_of_p_Tsp' by...
#...binding rows. You can skip this since this has already been done:
Betas_of_p_Tspn <- rbind(coef(object = nc46, param = 'p', prob = 0.95),
                         coef(object = nc47, param = 'p', prob = 0.95),
                         coef(object = nc73, param = 'p', prob = 0.95),
                         coef(object = nc109, param = 'p', prob = 0.95),
                         coef(object = nc111, param = 'p', prob = 0.95),
                         coef(object = nc117, param = 'p', prob = 0.95),
                         coef(object = nc121, param = 'p', prob = 0.95),
                         coef(object = nc122, param = 'p', prob = 0.95),
                         coef(object = nc126, param = 'p', prob = 0.95),
                         coef(object = nc128, param = 'p', prob = 0.95),
                         coef(object = nc130, param = 'p', prob = 0.95),
                         coef(object = nc131, param = 'p', prob = 0.95))

str(Betas_of_p_Tspn)
#Check which site-specific covariates appear in the betas of psi of the models:
Betas_of_p_Tspn
#Only one covariate appear ("Before noon"). Collect those rows containing...
#...betas of p of covariates.
cnbp <- Betas_of_p_Tspn[rownames(Betas_of_p_Tspn) %like% "BefNoon",]

#Observe the standard errors and confidence intervals of betas of various covariates:
cnbp
str(cnbp)

#Filter out betas which have 95% CI which pass through zero:
Tspnbp <- rbind(neg <- subset(cnbp, lowerCI<0 & upperCI<0),
                pos <- subset(cnbp, lowerCI>0 & upperCI>0))
Tspnbp

#Only one covariate, and the CI of all beta estimates pass through 0:
# 1. 'Before noon' SE  slightly greater than est. So, low certainty of influence...
#...of det. prob. This is the same for MS format.

#Count the number of time each covariate appears in models:
Tspn_Bn_p <- getCount(qaicTspfn$table$Model,'BefNoon')

#Find the count and weight of the covariate, and set names to columns:
Tspnp_cov <- dplyr::bind_cols('BefNoon', (sum(Tspn_Bn_p$wcount)),
                             crossprod(qaicTspfn$table$wgt,Tspn_Bn_p$wcount))
Tspnp_cov <- setNames(Tspnp_cov,cbind("Covariates","Count", "Sum_weight"))
Tspnp_cov

#Write results to a csv:
write.csv(Tspnp_cov,
          "/media/Dissertation/Paper/Tspnp_cov.csv")

#Save as word document:
Tspnp_cov_tbl <- flextable(Tspnp_cov)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = Tspnp_cov_tbl) %>%  #Insert the flextable object there
  print(target = "Tspnp_cov_tbl.docx")  

#Compare the counts and sum-weights of psi in MS and SM formats. There is very...
#...very little difference between them both.



#### 5. Visualising results ####



##### 5.1 Occupancy ####


#Use Mf_psi and Sa_psi for plotting. As there is no interactive effects on psi...
#...in the two-species models, they will not be plotted.

#For plotting both species in the same graph, create a column for species ID:
Rep1 <- rep("Mf", each=27)
Rep2 <- rep("Sa", each=27)
Rep <- data.frame(cbind(Rep1, Rep2))
MfSa <- data.frame(Rep=unlist(Rep))

#Then merge both the dataframe together:
merged_MfSa <- rbind(Mf_psi, Sa_psi)
merged_MfSa 

#...and then add the column from the first step:
merged_MfSa <- cbind(merged_MfSa,MfSa$Rep)
merged_MfSa 

#Rename the added column:
merged_MfSa <- merged_MfSa %>% rename_at('MfSa$Rep', ~'Species')
head(merged_MfSa)

remove(Rep1,Rep2,Rep,MfSa)

#Plot psi values for M. fairbankii:
min(Mf_psi$lower_0.95)
max(Mf_psi$upper_0.95)
min(Sa_psi$lower_0.95)
max(Sa_psi$upper_0.95)

psi_and_CI <- ggplot(merged_MfSa, aes(x=as.factor(siteIDs), y = est,
                            colour = Species, group = Species)) +
  ggtitle(label = expression(paste("Probability of occupancy"))) +
  scale_colour_manual(values=c("#ed872d", "#035096")) +
  geom_pointrange(aes(ymin = lower_0.95, ymax = upper_0.95),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lower_0.95, ymax = upper_0.95), width = 0.7,
                size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(x = "Site IDs",
       y = "Estimated psi and 95% confidence intervals") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

psi_and_CI


##### 5.2 Detection probabilities ####


#Plot graphs to view the detection probabilities across the four surveys:

#M. fairbankii:
#check lower and upper limits in the estimates to suppy the limits for breaks...
#...in graph:
min(Mf_p$est)
max(Mf_p$est)
#Plot graph:
Mf_surveys_p <- ggplot(Mf_p, aes(x=Survey_number, y=est,)) +
  ggtitle(label = expression(paste("(a) Probability of detecting ",
                                   italic("M. fairbankii")))) +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="red", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="black",
              fill=as.factor(Mf_p$siteIDs)) +
  scale_y_continuous(breaks = seq(from = 0.51, to = 0.74, by = 0.01)) +
  labs(x= NULL, y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))
#View graph:
Mf_surveys_p 

#S. albiventris:
min(Sa_p$est)
max(Sa_p$est)

Sa_surveys_p <- ggplot(Sa_p, aes(x=Survey_number, y=est,)) +
  ggtitle(label = expression(paste("(b) Probability of detecting ",
                                   italic("S. albiventris")))) +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="red", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="black",
              fill=as.factor(Sa_p$siteIDs)) +
  scale_y_continuous(breaks = seq(from = 0.4, to = 0.8, by = 0.02)) +
  labs(x= NULL, y = NULL) +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Sa_surveys_p 

#Two species MS format - rA (unconditional prob. of detecting M. fairbankii...
#...given both species are present):
min(Tsp_rA$est, na.rm = TRUE)
max(Tsp_rA$est, na.rm = TRUE)

Tsp_surveys_rA <- ggplot(Tsp_rA, aes(x=Survey_number, y=est,)) +
  ggtitle(label = expression(paste("(c) Prob. of detecting ",
                                   italic("M. fairbankii"),
                                   " given presence of both species"))) +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="red", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="black",
              fill=as.factor(Tsp_rA$siteIDs)) +
  scale_y_continuous(breaks = seq(from = 0.59, to = 0.71, by = 0.01)) +
  labs(x= "Survey number", y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Tsp_surveys_rA

#Two species SM format - rA (unconditional prob. of detecting S. albiventris...
#...given both species are present):
min(Tspn_rA$est, na.rm = TRUE)
max(Tspn_rA$est, na.rm = TRUE)

Tspn_surveys_rA <- ggplot(Tspn_rA, aes(x=Survey_number, y=est,)) +
  ggtitle(label = expression(paste("(d) Prob. of detecting ",
                                   italic("S. albiventris"),
                                   " given presence of both species"))) +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="red", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="black",
              fill=as.factor(Tspn_rA$siteIDs)) +
  scale_y_continuous(breaks = seq(from = 0.51, to = 0.68, by = 0.01)) +
  labs(x= "Survey number", y = NULL) +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Tspn_surveys_rA 

#Survey-wise detection probabilities first plate:
grid.arrange(Mf_surveys_p, Sa_surveys_p,
                            Tsp_surveys_rA, Tspn_surveys_rA,
                            nrow = 2, ncol = 2)

#Two species MS format - rBA (prob. of detecting S. albiventris given M. fairbankii...
#...was detected):
min(Tsp_rBA$est, na.rm = TRUE)
max(Tsp_rBA$est, na.rm = TRUE)

Tsp_surveys_rBA <- ggplot(Tsp_rBA, aes(x=Survey_number, y=est,)) +
  ggtitle(label = expression(paste("(a) Prob. of detecting ",
  italic("S. albiventris")," given detection of ", italic("M. fairbankii")))) +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="red", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="black",
              fill=as.factor(Tsp_rBA$siteIDs)) +
  scale_y_continuous(breaks = seq(from = 0.54, to = 0.65, by = 0.01)) +
  labs(x= NULL, y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Tsp_surveys_rBA 

#Two species SM format - rBA (prob. of detecting M. fairbankii given S. albiventris...
#...was detected):
min(Tspn_rBA$est, na.rm = TRUE)
max(Tspn_rBA$est, na.rm = TRUE)

Tspn_surveys_rBA <- ggplot(Tspn_rBA, aes(x=Survey_number, y=est,)) +
  ggtitle(label = expression(paste("(b) Prob. of detecting ",
    italic("M. fairbankii")," given detection of ", italic("S. albiventris")))) + 
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="red", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="black",
              fill=as.factor(Tspn_rBA$siteIDs)) +
  scale_y_continuous(breaks = seq(from = 0.60, to = 0.76, by = 0.01)) +
  labs(x= NULL, y = NULL) +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Tspn_surveys_rBA 

#Two species MS format - rBa (prob. of detecting S. albiventris given M. fairbankii...
#...was not detected):
min(Tsp_rBa$est, na.rm = TRUE)
max(Tsp_rBa$est, na.rm = TRUE)

Tsp_surveys_rBa <- ggplot(Tsp_rBa, aes(x=Survey_number, y=est,)) +
  ggtitle(label = expression(paste("(c) Prob. of detecting ",
                         italic("S. albiventris")," given non-detection of ",
                          italic("M. fairbankii")))) +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="red", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="black",
              fill=as.factor(Tsp_rBa$siteIDs)) +
  scale_y_continuous(breaks = seq(from = 0.4, to = 0.63, by = 0.02)) +
  labs(x= "Survey number", y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Tsp_surveys_rBa 

#Two species SM format - rBa (prob. of detecting M. fairbankii given S. albiventris...
#...was not detected):
min(Tspn_rBa$est, na.rm = TRUE)
max(Tspn_rBa$est, na.rm = TRUE)

Tspn_surveys_rBa <- ggplot(Tspn_rBa, aes(x=Survey_number, y=est,)) +
  ggtitle(label = expression(paste("(d) Prob. of detecting ",
                         italic("M. fairbankii")," given non-detection of ",
                         italic("S. albiventris")))) +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="red", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="black",
              fill=as.factor(Tspn_rBa$siteIDs)) +
  scale_y_continuous(breaks = seq(from = 0.31, to = 0.74, by = 0.02)) +
  labs(x= "Survey number", y= NULL) +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Tspn_surveys_rBa 

#Survey-wise detection probabilities second plate:
grid.arrange(Tsp_surveys_rBA, Tspn_surveys_rBA,
                             Tsp_surveys_rBa, Tspn_surveys_rBa,
                             nrow = 2, ncol = 2)

#Plot graphs to view the detection probabilities and confidence intervals across...
#...the 27 sites

#M. fairbankii
min(Mf_p$lower_0.95)
max(Mf_p$upper_0.95)

sites_Mfp_and_CI <- ggplot(Mf_p, aes(x=as.factor(siteIDs), y = est, colour = 
                  as.factor(Survey_number), group = as.factor(Survey_number))) +
  ggtitle(label = expression(paste("(a) Probability of detecting ",
                                   italic("M. fairbankii")))) +
  geom_pointrange(aes(ymin = lower_0.95, ymax = upper_0.95),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lower_0.95, ymax = upper_0.95), width = 0.7,
            size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0.2, to = 0.9, by = 0.05)) +
  labs(x = NULL,
       y = "Estimated p and 95% confidence intervals",
       colour = "Survey no.") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

#The viridis colour scale is added so that it is easily viewable by people with...
#...common forms of colour blindness. They are also perceptually uniform in both...
#.. colour and black-and-white. So, can be physically printed (only if needed, I...
#...advise against it to reduce paper use) in B&W instead of colour to reduce...
#...colour ink usage.

sites_Mfp_and_CI

#S. albiventris:
min(Sa_p$lower_0.95)
max(Sa_p$upper_0.95)

sites_Sap_and_CI  <- ggplot(Sa_p, aes(x=as.factor(siteIDs), y = est,
         colour = as.factor(Survey_number), group = as.factor(Survey_number))) +
  ggtitle(label = expression(paste("(b) Probability of detecting ",
                                   italic("S. albiventris")))) +
  geom_pointrange(aes(ymin = lower_0.95, ymax = upper_0.95),
                size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lower_0.95, ymax = upper_0.95), width = 0.7,
            size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0.15, to = 0.95, by = 0.05)) +
  labs(x = "Site IDs",
       y = "Estimated p and 95% confidence intervals",
       colour = "Survey no.") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)
 
sites_Sap_and_CI 

#Site-wise detection prob. first plate:
grid.arrange(sites_Mfp_and_CI, sites_Sap_and_CI, 
                            nrow = 2, ncol = 1)

#Two species MS format - rA (unconditional prob. of detecting M. fairbankii...
#...given both species are present):
min(Tsp_rA$lower_0.95, na.rm = TRUE)
max(Tsp_rA$upper_0.95 ,na.rm = TRUE)

sites_TsprA_and_CI <- ggplot(Tsp_rA, aes(x=as.factor(siteIDs), y = est,
       colour = as.factor(Survey_number), group = as.factor(Survey_number))) +
  ggtitle(label = expression(paste("(a) Probability of detecting ",
                                   italic("M. fairbankii"),
                                   " given presence of both species"))) +
  geom_pointrange(aes(ymin = lower_0.95, ymax = upper_0.95),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lower_0.95, ymax = upper_0.95), width = 0.7,
                size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0.36, to = 0.84, by = 0.05)) +
  labs(x = NULL,
       y = "Estimated rA and 95% confidence intervals",
       colour = "Survey no.") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

sites_TsprA_and_CI 

#Two species SM format - rA (unconditional prob. of detecting S. albiventris...
#...given both species are present):
min(Tspn_rA$lower_0.95, na.rm = TRUE)
max(Tspn_rA$upper_0.95 ,na.rm = TRUE)

sites_TspnrA_and_CI <- ggplot(Tspn_rA, aes(x=as.factor(siteIDs), y = est,
         colour = as.factor(Survey_number), group = as.factor(Survey_number))) +
  ggtitle(label = expression(paste("(b) Probability of detecting ",
                                   italic("S. albiventris"),
                                   " given presence of both species"))) +
  geom_pointrange(aes(ymin = lower_0.95, ymax = upper_0.95),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lower_0.95, ymax = upper_0.95), width = 0.7,
                size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0.3, to = 0.84, by = 0.05)) +
  labs(x = "Site IDs",
       y = "Estimated rA and 95% confidence intervals",
       colour = "Survey no.") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

sites_TspnrA_and_CI 

#Site-wise detection prob. second plate:
grid.arrange(sites_TsprA_and_CI, sites_TspnrA_and_CI, 
                            nrow = 2, ncol = 1)

#Two species MS format - rBA (prob. of detecting S. albiventris given M. fairbankii...
#...was detected):
min(Tsp_rBA$lower_0.95, na.rm = TRUE)
max(Tsp_rBA$upper_0.95 ,na.rm = TRUE)

sites_TsprBA_and_CI <- ggplot(Tsp_rBA, aes(x=as.factor(siteIDs), y = est,
       colour = as.factor(Survey_number), group = as.factor(Survey_number))) +
  ggtitle(label = expression(paste("(a) Probability of detecting ",
   italic("S. albiventris")," given detection of ", italic("M. fairbankii")))) +
  geom_pointrange(aes(ymin = lower_0.95, ymax = upper_0.95),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lower_0.95, ymax = upper_0.95), width = 0.7,
                size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0.31, to = 0.82, by = 0.05)) +
  labs(x = NULL,
       y = "Estimated rBA and 95% confidence intervals",
       colour = "Survey no.") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

sites_TsprBA_and_CI

#Two species SM format - rBA (prob. of detecting M. fairbankii given S. albiventris...
#...was detected):
min(Tspn_rBA$lower_0.95, na.rm = TRUE)
max(Tspn_rBA$upper_0.95 ,na.rm = TRUE)

sites_TspnrBA_and_CI <- ggplot(Tspn_rBA, aes(x=as.factor(siteIDs), y = est,
         colour = as.factor(Survey_number), group = as.factor(Survey_number))) +
  ggtitle(label = expression(paste("(b) Probability of detecting ",
   italic("M. fairbankii")," given detection of ", italic("S. albiventris")))) +
  geom_pointrange(aes(ymin = lower_0.95, ymax = upper_0.95),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lower_0.95, ymax = upper_0.95), width = 0.7,
                size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0.36, to = 0.89, by = 0.05)) +
  labs(x = "Site IDs",
       y = "Estimated rBA and 95% confidence intervals",
       colour = "Survey no.") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

sites_TspnrBA_and_CI

#Site-wise detection prob. third plate:
grid.arrange(sites_TsprBA_and_CI, sites_TspnrBA_and_CI, 
                            nrow = 2, ncol = 1)

#Two species MS format - rBa (prob. of detecting S. albiventris given M. fairbankii...
#...was not detected):
min(Tsp_rBa$lower_0.95, na.rm = TRUE)
max(Tsp_rBa$upper_0.95 ,na.rm = TRUE)

sites_TsprBa_and_CI <- ggplot(Tsp_rBa, aes(x=as.factor(siteIDs), y = est,
       colour = as.factor(Survey_number), group = as.factor(Survey_number))) +
  ggtitle(label = expression(paste("(a) Probability of detecting ",
                            italic("S. albiventris")," given non-detection of ",
                            italic("M. fairbankii")))) +
  geom_pointrange(aes(ymin = lower_0.95, ymax = upper_0.95),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lower_0.95, ymax = upper_0.95), width = 0.7,
            size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(x = NULL,
       y = "Estimated rBa and 95% confidence intervals",
       colour = "Survey no.") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

sites_TsprBa_and_CI

#Two species SM format - rBa (prob. of detecting M. fairbankii given S. albiventris...
#...was not detected):
min(Tspn_rBa$lower_0.95, na.rm = TRUE)
max(Tspn_rBa$upper_0.95 ,na.rm = TRUE)

sites_TspnrBa_and_CI <- ggplot(Tspn_rBa, aes(x=as.factor(siteIDs), y = est,
         colour = as.factor(Survey_number), group = as.factor(Survey_number))) +
  ggtitle(label = expression(paste("(b) Probability of detecting ",
                             italic("M. fairbankii")," given non-detection of ",
                             italic("S. albiventris")))) +
  geom_pointrange(aes(ymin = lower_0.95, ymax = upper_0.95),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lower_0.95, ymax = upper_0.95), width = 0.7,
            size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(x = "Site IDs",
       y = "Estimated rBa and 95% confidence intervals",
       colour = "Survey no.") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

sites_TspnrBa_and_CI

#Site-wise detection prob. fourth plate:
grid.arrange(sites_TsprBa_and_CI, sites_TspnrBa_and_CI, 
                            nrow = 2, ncol = 1)


##### 5.3 Effects of covariates #####



#Before looking at the effects of covariates on individual species, look at whether...
#...covariate 'survey' (survey number) is correlated to the survey dates, so that...
#...any observer bias can be understood.

s <- read.csv("Survey_details.csv", stringsAsFactors = FALSE, header = TRUE, 
              blank.lines.skip = TRUE)
head(s)

#Here, 'Date_no.' is the number of days of survey starting from the first (1).

#A Spearman's correlation test can be done to check correlation (Spearman's since...
#...the data is most likely non-normal due to small sample size):
surv_noVSDate_no <- cor.test(s$surv_no,s$Date_no, method = "spearm")
surv_noVSDate_no 
#Kendall's tau can be used since there are ranking ties:
#Interpretation: +/−1 = perfect correlation, +/− 0.99 to +/− 0.40 = strong,...
#...+/−0.39 to +/−0.35 = moderate, and +/−0.34 to +/−0.1 = weak, and 0 = zero...
#...(based on Pop, P. et al (2022) [Preprint]. An investigation into the...
#...spatiotemporal patterns of the Nymphalid butterfly Vagrans egista sinha...
#...(Kollar,[1844]). bioRxiv, 2022-01. https://doi.org/10.1101/2022.01.02.474748.
surv_noVSDate_no <- cor.test(s$surv_no,s$Date_no, method = "kendall")
surv_noVSDate_no 
#tau = 0.222642

#The correlation is weak.

survVsDate <- ggplot(s, aes(x=as.factor(surv_no), y=Date_no,
                            colour=surv_no, fill=surv_no)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha=1000) +  
  geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="light green") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="green", width=1, linewidth=0.5) +
  theme_classic() +
  labs(y= "Date no.", x="Survey no.") +
  scale_y_continuous(breaks = seq(from = 0, to = 44, by = 5)) +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

survVsDate

#The Kendall's tau be added to the plot to provide more information:
survVsDate <- survVsDate + annotate("label",x = 3.5, y = 2,
            size = rel(4.2),label = "Kendall's tau (p = 0.00236) =\n 0.222642")
survVsDate


###### 5.3.1 M. fairbankii ######


#View the effects of covariates on psi and for M. fairbankii (calculated in...
#...section 4):
Mfpsi_cov
Mfp_cov

#Note that the effects table is not indicative of exact strength of a covariate...
#...in explaining a model, but the relative counts and sum wights can be used as...
#...a rough measure of the relative strength when compared to other significant...
#...covariates in the table. Note that the counts and sumweights should only be...
#...considered in the light of the interpretation of betas for psi and p for each...
#...individual covariate (already provided in detail section 4 of this script).

#The interpretation from section 4 has been summarised into table 5.3.1.psi and...
#...5.3.1.p below.
#Table 5.3.1.psi: summary of confidence of effect of a covariate on psi (along with...
#...direction of interaction if effect confidence is not low/very low): 
# 1. 'Elevation' - Moderate - positive
# 2. 'Area' - Low to Moderate - positive
# 3. 'Rubus niveus - Very low
# 4. 'Aspect' -  Very low
# 5. 'Slope' - Moderate - positive
# 6. 'Canopy height' - Very low
# 7.'Understorey height' - Low
# 8. 'Visibility' - Moderate - positive
# 9. 'Burn' - Very low
# 10. 'Metalled road' - Very low
# 11. 'Dirt road' - Moderate - positive
# 12. 'No road' - Strong - negative.
# 13. 'Lentic' - Very low
# 14. 'Wet lotic' - Very low
# 15. 'Dry lotic' - Very very low

#Table 5.3.1.p: summary of confidence of effect of a covariate on p (along with...
#...direction of interaction if effect confidence is not low/very low): 
# 1. 'Warm' - Very low
# 2.'survey'- Moderate - positive
# 3. 'Windy' - Strong - negative
# 4. 'Visibility' - Very low
# 5. 'Before noon' - Moderate - positive
# 6. 'Cool' - Low 
# 7. 'Overcast' - Low to moderate - positive
# 8. 'After noon' - Moderate - negative 
# 10. 'Clear sky' - Moderate - negative.
# 11. 'Neutral' - Very low 

#Plot all significant covariates in the table 5.3.1.psi against psi:

#Plot a graph to view the occupancy probabilities vs 'No road' covariate:
#Calculate min-max for inputting as breaks in the graph:
min(Mf_psi$est)
max(Mf_psi$est)

#Plot:
Mf_No_roadVSpsi <- ggplot(Mf_psi, aes(x=as.factor(sitecov2$No_road), y=est,)) +
  ggtitle(label = "(a) psi vs No road") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#ed872d", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(sitecov2$No_road)) +
  scale_y_continuous(breaks = seq(from = 0.66, to = 0.93, by = 0.02)) +
  labs(x= NULL, y= "Occupancy Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("Road", "No Road")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_No_roadVSpsi 

No_roadVSpsi <- cor.test(Mf_psi$est,sitecov2$No_road, method = "spearm")
No_roadVSpsi
#Kendall's tau can be used since there are ranking ties.
No_roadVSpsi <- cor.test(Mf_psi$est,sitecov2$No_road, method = "kendall")
No_roadVSpsi
#tau = -0.4529108 

#The result can be added to the plot to provide more information:
Mf_No_roadVSpsi <- Mf_No_roadVSpsi + annotate("label",x = 1.5, y = 0.76,
            size = rel(4.2),label = "Kendall's tau (p = 0.005479) =\n - 0.4529")
Mf_No_roadVSpsi

#From table 5.3.1.psi, the plot, and the correlation, its clear that 'No road' has...
#...a strong negative influence on psi. The mean of psi for 'No road' is not only...
#...higher 'Road', but doesn't overlap (this could be due to only a smaller number...
#...of sites with no road. But, as you will see, the effect is significant. Add to...
#...final plot.

#Plot a graph to view the occupancy probabilities vs 'Dirt road' covariate:

Mf_Dirt_roadVSpsi <- ggplot(Mf_psi, aes(x=as.factor(sitecov2$Dirt_road),
                                        y=est,)) +
  ggtitle(label = "(b) psi vs Dirt road") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#ed872d", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(sitecov2$Dirt_road)) +
  scale_y_continuous(breaks = seq(from = 0.66, to = 0.93, by = 0.02)) +
  labs(x= NULL, y= NULL) +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("No Dirt Road", "Dirt Road")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_Dirt_roadVSpsi

#A Spearman's correlation test can be done to check correlation (Spearman's since...
#...the data is most likely non-normal due to small sample size):
Dirt_roadVSpsi <- cor.test(Mf_psi$est,sitecov2$Dirt_road, method = "spearm")
Dirt_roadVSpsi
#Kendall's tau can be used since there are ranking ties:
Dirt_roadVSpsi <- cor.test(Mf_psi$est,sitecov2$Dirt_road, method = "kendall")
Dirt_roadVSpsi
#tau = 0.6468135 

Mf_Dirt_roadVSpsi <- Mf_Dirt_roadVSpsi + annotate("label",x = 1.5, y = 0.75,
          size = rel(4.2),label = "Kendall's tau (p = 7.292e-05) =\n 0.6468")
Mf_Dirt_roadVSpsi

#From table 5.3.1.psi, the plot, and the correlation, its clear that 'Dirt road'...
#...has a moderate to strong positive influence on psi. Add to final plot. The...
#...only three spots where psi severely dips is for sites where 'No road' coincides,...
#...demonstrating the strong influence of 'No road'.

#Plot a graph to view the occupancy probabilities vs 'Elevation' covariate:
min(sitecov2$Elevation)
max(sitecov2$Elevation)

Mf_ElevationVSpsi <- ggplot(Mf_psi, aes(x=sitecov2$Elevation, y=est,)) +
  ggtitle(label = "(c) psi vs Elevation") +
  stat_summary(fun=mean, geom="line", colour="#ed872d") +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(sitecov2$Elevation)) +
  scale_y_continuous(breaks = seq(from = 0.66, to = 0.93, by = 0.02)) +
  scale_x_continuous(breaks = seq(from = 1450, to = 2150, by = 50)) +
  labs(x= "Elevation (in m a.s.l.)", y= NULL) +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_ElevationVSpsi

#Spearman's rank correlation (rho):
ElevationVSpsi <- cor.test(Mf_psi$est,sitecov2$Elevation, method = "spearm")
ElevationVSpsi
#Since there are no rank ties, Spearman's value can be used. Interepretation for...
#...Spearman's rank corr. = (+/−1 = perfect correlation, +/−0.9 to +/−0.7 = strong,...
#+/−0.6 to +/−0.4 = moderate, +/−0.3 to +/−0.1 = weak, and 0 = zero)
#rho = 0.6672772 

Mf_ElevationVSpsi <- Mf_ElevationVSpsi + annotate("label",x = 1850, y = 0.74,
          size = rel(4.2),label = "Spearman's rho (p = 0.000206) =\n 0.6623")
Mf_ElevationVSpsi

#From table 5.3.1.psi, the plot, and the correlation, its clear that 'Elevation'...
#...has a moderate to strong positive influence on psi. Add to final plot. The...
#...only three spots where psi severely dips is for sites where 'No road' coincides,...
#...demonstrating the strong influence of 'No road'.

#Plot a graph to view the occupancy probabilities vs 'Slope' covariate:
min(sitecov2$Slope)
max(sitecov2$Slope)

Mf_SlopeVSpsi <- ggplot(Mf_psi, aes(x=sitecov2$Slope, y=est,)) +
  ggtitle(label = "(d) psi vs Slope") +
  stat_summary(fun=mean, geom="line", colour="#ed872d") +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(sitecov2$Slope)) +
  scale_y_continuous(breaks = seq(from = 0.66, to = 0.93, by = 0.02)) +
  scale_x_continuous(breaks = seq(from = 30, to = 90, by = 5)) +
  labs(x= "Slope (in degrees)", y= "Occupancy Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_SlopeVSpsi

#Spearman's rho:
SlopeVSpsi <- cor.test(Mf_psi$est,sitecov2$Slope, method = "spearm")
SlopeVSpsi
#rho = -0.7716728 

Mf_SlopeVSpsi <- Mf_SlopeVSpsi + annotate("label",x = 40, y = 0.70,
          size = rel(4.2),label = "Spearman's rho (p = 5.25e-06) =\n - 0.7717")
Mf_SlopeVSpsi

#From table 5.3.1.psi, the plot, and the correlation, we find that 'Slope' has....
#...moderate to strong negative influence on psi. Add to final plot. The only...
#...three spots where psi severely dips is for sites where 'No road' coincides,...
#...demonstrating the strong influence of 'No road'.

#Plot a graph to view the occupancy probabilities vs 'Visibility' covariate:
min(sitecov2$Visibility)
max(sitecov2$Visibility)

Mf_VisibilityVSpsi <- ggplot(Mf_psi, aes(x=sitecov2$Visibility, y=est,)) +
  ggtitle(label = "(e) psi vs Visibility") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="green", width=2, linewidth=0.5) +
  stat_summary(fun=mean, geom="line", colour="#ed872d") +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(sitecov2$Visibility)) +
  scale_y_continuous(breaks = seq(from = 0.66, to = 0.93, by = 0.02)) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  labs(x= "Visibility (in %)", y= NULL) +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_VisibilityVSpsi

#Spearman's rho:
VisibilityVSpsi <- cor.test(Mf_psi$est,sitecov2$Visibility, method = "spearm")
VisibilityVSpsi
#Kendall's tau can be used since there are ranking ties:
VisibilityVSpsi <- cor.test(Mf_psi$est,sitecov2$Visibility, method = "kendall")
VisibilityVSpsi
#p-value>0.05. Estimate could be due to chance.
#tau = 0.008571464 
Mf_VisibilityVSpsi <- Mf_VisibilityVSpsi + annotate("label",x = 70, y = 0.70,
            size = rel(4.2),label = "Kendall's tau (p = 0.9501) =\n 0.0086")
Mf_VisibilityVSpsi

#From table 5.3.1.psi, we see a moderate influence on psi, but the plot, and the...
#...correlation doesn't reveal anything much. So, the influence is likely low to...
#...moderate positive influence on psi. But add to final plot. The only three...
#...spots where psi severely dips is for sites where 'No road' coincides,...
#...demonstrating the strong influence of 'No road'.

#Plot a graph to view the occupancy probabilities vs 'Area' covariate:
min(sitecov2$Area)
max(sitecov2$Area)

Mf_AreaVSpsi <- ggplot(Mf_psi, aes(x=sitecov2$Area, y=est,)) +
  ggtitle(label = "(c) psi vs Area") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="green", width=0.2, linewidth=0.5) +
  stat_summary(fun=mean, geom="line", colour="#ed872d") +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(sitecov2$Area)) +
  scale_y_continuous(breaks = seq(from = 0.66, to = 0.93, by = 0.02)) +
  scale_x_continuous(breaks = seq(from = 0, to = 5.2, by = 0.2)) +
  labs(x= "Area (in hectares)", y= "Occupancy Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_AreaVSpsi

#Spearman's rho:
AreaVSpsi <- cor.test(Mf_psi$est,sitecov2$Area, method = "spearm")
AreaVSpsi
#Kendall's tau can be used since there are ranking ties:
AreaVSpsi <- cor.test(Mf_psi$est,sitecov2$Area, method = "kendall")
AreaVSpsi
#p-value>0.05. Estimate could be due to chance.
#tau = 0.2607493
Mf_AreaVSpsi <- Mf_AreaVSpsi + annotate("label",x = 4.2, y = 0.70,
              size = rel(4.2),label = "Kendall's tau (p = 0.05757) =\n 0.2607")
Mf_AreaVSpsi

#From table 5.3.1.psi, we see a low to moderate influence on psi, but the plot,...
#...and the correlation doesn't reveal any clear pattern. The influence is likely...
#...low. Don't add to final plot. The only three spots where psi severely dips...
#...is for sites where 'No road' coincides, demonstrating the strong influence of...
#...'No road'.

#The covariates in table 5.3.1.psi whose strength of influence is likely low, very...
#...low, or very very low don't need to be plotted.

#Final plot:
grid.arrange(Mf_No_roadVSpsi, Mf_Dirt_roadVSpsi, Mf_ElevationVSpsi,
             Mf_SlopeVSpsi, Mf_VisibilityVSpsi, nrow = 2, 
             layout_matrix = rbind(c(1,2,3,3), c(4,4,5,5)))

#Plot all covariates in the table against p (except 'survey', since it has already...
#...been plotted. Here all covariates are plotted out of curiosity, but only those...
#...with moderate or strong influence on p in table 5.3.1.p needs to be plotted.

#Since 'Visibility' is the only continuous site-specific variable in this list,...
#...it needs to be added as a column to Mf_p (since the variable is only 27 rows...
#...in length, but the Mf_p is 108 rows in length)
dfr <- data.frame(d$Visibility)
dframe <- data.frame(dfr,i=rep(1:4,ea=NROW(dfr)))
str(dframe)
#Combine the column with Mf_p:
Mf_p <- cbind(Mf_p,  Visibility = dframe$d.Visibility)
head(Mf_p)
str(Mf_p)

#Plot a graph to view the detection probabilities vs 'Visibility' covariate:
Mf_VisibilityVSp <- ggplot(Mf_p, aes(x=Mf_p$Visibility, y=est,)) +
  ggtitle(label = "p vs Visibility") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#ed872d", width=2, linewidth=0.5) +
  stat_summary(fun=mean, geom="line", colour="green") +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(Mf_p$Visibility)) +
  scale_y_continuous(breaks = seq(from = 0.51, to = 0.74, by = 0.02)) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  labs(x= "Visibility (in %)", y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))
Mf_VisibilityVSp

#Since the graph shows not much a pattern, a Spearman's correlation test can be...
#...done (since the data is most likely non-normal due to small sample size):
VisibilityVSp<- cor.test(Mf_p$est,Mf_p$Visibility, method = "spearm")
VisibilityVSp
#Kendall's tau can be used since there are ranking ties (this is true for all..
#...the covariates for p (I have checked). So, tau can be used instead of rho):
VisibilityVSp<- cor.test(Mf_p$est,Mf_p$Visibility, method = "kendall")
VisibilityVSp
#tau = 0.1380479

#The result can be added to the plot to provide more information:
Mf_VisibilityVSp <- Mf_VisibilityVSp + annotate("label",x = 90, y = 0.53,
         size = rel(4.2),label = "Kendall's tau (p = 0.0372) =\n 0.1380")
Mf_VisibilityVSp

#From table 5.3.1.p, the plot, and the correlation, its clear that Visibility has...
#...no or weak influence p. It doesn't need to be added to the final plot.

#Plot a graph to view the detection probabilities vs 'Windy' covariate:
Mf_WindyVSp <- ggplot(Mf_p, aes(x=as.factor(surveycov$Windy), y=Mf_p$est,)) +
  ggtitle(label = "(a) p vs Windy") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#ed872d", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(surveycov$Windy)) +
  scale_y_continuous(breaks = seq(from = 0.51, to = 0.74, by = 0.02)) +
  labs(x= NULL, y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("Not Windy", "Windy")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_WindyVSp 

#Kendall's rank correlation:
WindyVSp<- cor.test(Mf_p$est,surveycov$Windy, method = "kendall")
WindyVSp
#tau = -0.5432906

#The result can be added to the plot to provide more information:
Mf_WindyVSp <- Mf_WindyVSp + annotate("label",x = 2, y = 0.72, size=rel(4.2),
                          label = "Kendall's tau (p = 7.381e-12) =\n -0.5433")
Mf_WindyVSp 

#From table 5.3.1.p, plot, and correlation, its clear that Windy does negatively...
#...influence p. In other terms, not windy is a condition in which M. fairbankii...
#...is best detected. Add to final plot.

#Note: the x labels are in the order 0 and 1, therefore it should the corresponding...
#...'Not Windy and 'Windy' in that order.

#Plot a graph to view the detection probabilities vs 'Before noon' and 'After noon'...
#...covariate. There is no need to plot them seperately since one is the opposite...
#...of the other:
Mf_BefNoonVSp <- ggplot(Mf_p, aes(x=as.factor(surveycov$BefNoon), y=Mf_p$est,)) +
  ggtitle(label = "(b) p vs Before and After noon") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#ed872d", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(surveycov$BefNoon)) +
  scale_y_continuous(breaks = seq(from = 0.51, to = 0.74, by = 0.02)) +
  labs(x= NULL, y= NULL) +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("After noon", "Before noon")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_BefNoonVSp 

#Kendall's corr.:
BefNoonVSp<- cor.test(Mf_p$est,surveycov$BefNoon, method = "kendall")
BefNoonVSp
#tau = 0.3913255
AftNoonVSp<- cor.test(Mf_p$est,surveycov$AftNoon, method = "kendall")
AftNoonVSp
#tau = -0.3913255

Mf_BefNoonVSp <- Mf_BefNoonVSp + annotate("label",x = 2, y = 0.53, size=rel(4.0),
label = "Kendall's tau (p = 8.055e-07)\n Before noon = 0.3913\nAfter noon = -0.3913")
Mf_BefNoonVSp 

#From the plot, its clear that the mean p is higher for 'Before noon' compared...
#...to 'After noon' (but both are high). From Table 5.3.1.p, plot, and corr., its...
#...clear the influence of both are moderate (Before noon's influence on p being...
#...positive and Afternoon's negative. Add to final plot.

#Plot a graph to view the detection probabilities vs 'Clear sky' and 'Overcast'...
#...covariates. There is no need to plot them separately since one is the binary...
#...opposite of the other:

Mf_ClrSkyVSp <- ggplot(Mf_p, aes(x=as.factor(surveycov$ClrSky), y=Mf_p$est,)) +
  ggtitle(label = "(c) p vs Clear sky and Overcast") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#ed872d", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(surveycov$ClrSky)) +
  scale_y_continuous(breaks = seq(from = 0.51, to = 0.74, by = 0.02)) +
  labs(x= NULL, y= NULL) +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("Overcast", "Clear sky")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_ClrSkyVSp 

#Kendall's tau:
ClrSkyVSp<- cor.test(Mf_p$est,surveycov$ClrSky, method = "kendall")
ClrSkyVSp
#p-value >0.05 and could be due to chance alone.
#tau = 0.02071505
OvrcstVSp<- cor.test(Mf_p$est,surveycov$Ovrcst, method = "kendall")
OvrcstVSp
#tau = 0.02071505
#p-value >0.05 and could be due to chance alone. Ignore.

#From the plot, it's clear that the means of p is almost same for both 'Clear sky'...
#...'Overcast', and p is moderately high. Table 5.3.1.p indicates Low to moderate...
#...positive influence by Overcast and moderate negative influence by Clearsky...
#...but correlations don't reveal much. These still could be factors affecting...
#...p. Add to final plot.

Mf_ClrSkyVSp <- Mf_ClrSkyVSp + annotate("label",x = 1.5, y = 0.52, size=rel(3.9),
label = "Kendall's tau (p = 0.7379)\n Clear sky = 0.0207\nOvercast = -0.0207")
Mf_ClrSkyVSp 

#Plot a graph to view the detection probabilities vs 'Cool':
Mf_CoolVSp <- ggplot(Mf_p, aes(x=as.factor(surveycov$Cool), y=Mf_p$est,)) +
  ggtitle(label = "p vs Cool") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#ed872d", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(surveycov$Cool)) +
  scale_y_continuous(breaks = seq(from = 0.51, to = 0.74, by = 0.02)) +
  labs(x= NULL, y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("Not Cool", "Cool")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_CoolVSp 
#Note: Here the opposite is not 'Warm' since 'Neutral' is another option for...
#...weather.

#Kendall's tau:
CoolVSp<- cor.test(Mf_p$est,surveycov$Cool, method = "kendall")
CoolVSp
#p-value >0.05 and could be due to chance alone.
#tau = 0.0344734

#From the plot, in can be inferred that there are more datapoints for 'Not Cool'...
#...weather conditions, which supports the low influence on p found in Table 5.3.1.p.
#...Correlations don't reveal much. Don't add to final plot.

Mf_CoolVSp <- Mf_CoolVSp + annotate("label",x = 2, y = 0.53, size=rel(4.2),
                              label = "Kendall's tau (p = 0.6638) =\n 0.0345 ")
Mf_CoolVSp

#Plot a graph to view the detection probabilities vs 'Warm':
Mf_WarmVSp <- ggplot(Mf_p, aes(x=as.factor(surveycov$Warm), y=Mf_p$est,)) +
  ggtitle(label = "p vs Warm") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#ed872d", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(surveycov$Warm)) +
  scale_y_continuous(breaks = seq(from = 0.51, to = 0.74, by = 0.02)) +
  labs(x= NULL, y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("Not Warm", "Warm")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_WarmVSp 

#Kendall's tau:
WarmVSp<- cor.test(Mf_p$est,surveycov$Warm, method = "kendall")
WarmVSp
#p-value >0.05 and could be due to chance alone.
#tau = 0.02826669

#Plot doesn't give much information about the covariate, but table 5.3.1.p indicate...
#...very weak influence on p. Correlations don't reveal much. Don't add to final...
#...plot.

Mf_WarmVSp <- Mf_WarmVSp + annotate("label",x = 2, y = 0.53, size=rel(4.2),
                                label = "Kendall's tau (p = 0.7215) =\n 0.0283")
Mf_WarmVSp 

#Plot a graph to view the detection probabilities vs 'Neutral':
Mf_NeutralVSp <- ggplot(Mf_p, aes(x=as.factor(surveycov$Neutral), y=Mf_p$est,)) +
  ggtitle(label = "p vs Neutral") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#ed872d", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(surveycov$Neutral)) +
  scale_y_continuous(breaks = seq(from = 0.51, to = 0.74, by = 0.02)) +
  labs(x= NULL, y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("Not Neutral", "Neutral")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_NeutralVSp 

#Kendall's tau:
NeutralVSp<- cor.test(Mf_p$est,surveycov$Neutral, method = "kendall")
NeutralVSp
#p-value >0.05 and could be due to chance alone.
#tau = -0.03363006

#Plot doesn't give much information about the covariate, but table 5.3.1.p indicate...
#...very weak influence on p. Correlations don't reveal much. Don't add to final...
#...plot.

Mf_NeutralVSp <- Mf_NeutralVSp + annotate("label",x = 1, y = 0.53, size=rel(4.2),
                              label = "Kendall's tau (p = 0.6715) =\n -0.0336")
Mf_NeutralVSp 

#Plot a graph to view the detection probabilities vs 'Sunny':
Mf_SunnyVSp <- ggplot(Mf_p, aes(x=as.factor(surveycov$Sunny), y=Mf_p$est,)) +
  ggtitle(label = "p vs Sunny") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#ed872d", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(surveycov$Sunny)) +
  scale_y_continuous(breaks = seq(from = 0.51, to = 0.74, by = 0.02)) +
  labs(x= NULL, y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("Not Sunny", "Sunny")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Mf_SunnyVSp 

#Kendall's tau:
SunnyVSp<- cor.test(Mf_p$est,surveycov$Sunny, method = "kendall")
SunnyVSp
#p-value >0.05 and could be due to chance alone.
#tau = -0.07379093

#Plot doesn't give much information about the covariate, but table 5.3.1.p indicate...
#...very weak influence on p. Correlations don't reveal much. Don't add to final...
#...plot.

Mf_SunnyVSp <- Mf_SunnyVSp + annotate("label",x = 1, y = 0.52, size=rel(4.2),
                              label = "Kendall's tau (p = 0.3522) =\n -0.0737")
Mf_SunnyVSp 

#Final plot: Mf_WindyVSp, Mf_BefNoonVSp, and Mf_ClrSkyVSp
grid.arrange(Mf_WindyVSp, Mf_BefNoonVSp, Mf_ClrSkyVSp, nrow = 1, ncol = 3)

#Plots for the rest (curiosity plots):
#Plot 1
grid.arrange(Mf_VisibilityVSp, Mf_SunnyVSp, Mf_WarmVSp, nrow = 2, 
             layout_matrix = rbind(c(1,1), c(2,3)))

#Plot 2
grid.arrange(Mf_CoolVSp, Mf_NeutralVSp, nrow = 1, ncol = 2)


###### 5.3.2 S. albiventris ######


#View the effects of covariates on psi and for S. albiventris (calculated in...
#...section 4):
Sapsi_cov
Sap_cov

#The interpretation from section 4 has been summarised into table 5.3.2.psi and...
#...5.3.2.p below.
#Table 5.3.2.psi - summary of confidence of effect of a covariate on psi:
# 1. 'Elevation' - Strong - positive
# 2. 'Lantana camara' - Very strong - negative
# 3. 'Understorey height' - Moderate to Strong - negative.

#Table 5.3.2.p - summary of confidence of effect of a covariate on p: 
# 1. 'Neutral' - Strong - positive
# 2. 'survey'- Moderate - positive 
# 3. 'Warm' - Low 
# 4. 'Cool' - Very low

#Plot all covariates in the table 5.3.2.psi against psi:

#Plot a graph to view the occupancy probabilities vs 'Elevation' covariate:
#Calculate min-max for inputting as breaks in the graph:
min(Sa_psi$est)
max(Sa_psi$est)

Sa_ElevationVSpsi <- ggplot(Sa_psi, aes(x=sitecov2$Elevation, y=est,)) +
  ggtitle(label = "(a) psi vs Elevation") +
  stat_summary(fun=mean, geom="line", colour="#035096") +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(sitecov2$Elevation)) +
  scale_y_continuous(breaks = seq(from = 0.1, to = 1, by = 0.05)) +
  scale_x_continuous(breaks = seq(from = 1450, to = 2150, by = 50)) +
  labs(x= "Elevation (in m a.s.l.)", y= "Occupancy Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Sa_ElevationVSpsi

#Spearman's rho:
ElevationVSpsi <- cor.test(Sa_psi$est,sitecov2$Elevation, method = "spearm")
ElevationVSpsi
#rho = 0.998779 

Sa_ElevationVSpsi <- Sa_ElevationVSpsi + annotate("label",x = 1900, y = 0.74,
            size = rel(4.2),label = "Spearman's rho (p = < 2.2e-16) =\n 0.9988")
Sa_ElevationVSpsi

#From table 5.3.2.psi, the plot, and the correlation, its clear that Elevation has...
#...a strong to very strong influence psi. Add to the final plot.

#Plot a graph to view the occupancy probabilities vs 'Lantana camara' covariate:
Sa_Lantana_camaraVSpsi <- ggplot(Sa_psi, aes(x=as.factor(sitecov2$Lantana_camara),
                                        y=est,)) +
  ggtitle(label = expression(paste("(b) psi vs ",
                                   italic("Lantana camara")))) +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#035096", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(sitecov2$Lantana_camara)) +
  scale_y_continuous(breaks = seq(from = 0.1, to = 1, by = 0.05)) +
  labs(x= NULL, y= "Occupancy Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("No Lantana camara", "Lantana camara")) +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Sa_Lantana_camaraVSpsi

#A Spearman's rho:
Lantana_camaraVSpsi <- cor.test(Sa_psi$est,sitecov2$Lantana_camara,
                                method = "spearm")
Lantana_camaraVSpsi
#Kendall's tau can be used since there are ranking ties:
Lantana_camaraVSpsi <- cor.test(Sa_psi$est,sitecov2$Lantana_camara,
                                method = "kendall")
Lantana_camaraVSpsi
#tau = -0.5991447

Sa_Lantana_camaraVSpsi <- Sa_Lantana_camaraVSpsi +
                            annotate("label",x = 1, y = 0.45, size = rel(4.2),
                          label = "Kendall's tau (p = 0.0002386) =\n - 0.5991")
Sa_Lantana_camaraVSpsi

#From table 5.3.2.psi, the plot, and the correlation, its clear that Lantana...
#...camara has a very strong negative influence on psi. Add to the final plot.

#Plot a graph to view the occupancy probabilities vs 'Understorey height':
min(sitecov2$Understorey_height)
max(sitecov2$Understorey_height)

Sa_Understorey_heightVSpsi <- ggplot(Sa_psi, aes(x=sitecov2$Understorey_height,
                                                 y=est,)) +
  ggtitle(label = "(c) psi vs Understorey height") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="green", width=0.2, linewidth=0.5) +
  stat_summary(fun=mean, geom="line", colour="#035096") +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(sitecov2$Understorey_height)) +
  scale_y_continuous(breaks = seq(from = 0.1, to = 1, by = 0.05)) +
  scale_x_continuous(breaks = seq(from = 1, to = 2.5, by = 0.5)) +
  labs(x= "Understorey height (in m)", y= "Occupancy Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Sa_Understorey_heightVSpsi

#Spearman's rho:
Understorey_heightVSpsi <- cor.test(Sa_psi$est,sitecov2$Understorey_height,
                                    method = "spearm")
Understorey_heightVSpsi
#Kendall's tau can be used since there are ranking ties:
Understorey_heightVSpsi <- cor.test(Sa_psi$est,sitecov2$Understorey_height,
                                    method = "kendall")
Understorey_heightVSpsi
#p-value>0.05. Estimate could be from random chance.
#tau = -0.3847344

Sa_Understorey_heightVSpsi <- Sa_Understorey_heightVSpsi +
                            annotate("label",x = 2, y = 0.8,size = rel(4.2),
                            label = "Kendall's tau (p = 0.01034) =\n - 0.3847")
Sa_Understorey_heightVSpsi

#The effect of this covariate is strong, but it most strong at values of 1 m. 
#Understory heights below 1 m doesn't show high occcupancy prob. 5.3.2.psi...
#...indicates moderate to strong influence on psi, and corr. show moderate...
#...influence. So, the influence is likely moderate to strong. Add to plot.

#Plot one significant covariate (Neutral) in the table 5.3.2.p against p,...
#...(survey has already been plotted in section 5.2):

#Plot a graph to view the detection probabilities vs 'Neutral' covariate:
min(Sa_p$est)
max(Sa_p$est)

Sa_NeutralVSp <- ggplot(Sa_p, aes(x=as.factor(surveycov$Neutral), y=Sa_p$est,)) +
  ggtitle(label = "(d) p vs Neutral") +
  stat_summary(fun = mean, show.legend=TRUE, geom="crossbar",
               color="#035096", width=0.6, linewidth=0.5) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=12, color="black",
              fill=as.factor(surveycov$Neutral)) +
  scale_y_continuous(breaks = seq(from = 0.4, to = 0.8, by = 0.02)) +
  labs(x= NULL, y= "Detection Probability") +
  theme(axis.text = element_text(colour = "black", size = rel(1.2))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  scale_x_discrete(labels= c("Not Neutral", "Neutral")) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00))

Sa_NeutralVSp 

#Spearman's rho:
NeutralVSp <- cor.test(Sa_p$est,surveycov$Neutral,method = "spearm")
NeutralVSp
#Kendall's tau can be used since there are ranking ties:
NeutralVSp <- cor.test(Sa_p$est,surveycov$Neutral,method = "kendall")
NeutralVSp
#tau = 0.7039651

Sa_NeutralVSp <- Sa_NeutralVSp + annotate("label",x = 2, y = 0.44,
          size = rel(4.2),label = "Kendall's tau (p = < 2.2e-16) =\n 0.7039651")
Sa_NeutralVSp

#From table 5.3.2.p, the plot, and the correlation, its clear that 'Neutral' has...
#...a very strong influence p. Add to the final plot.

#Final plot: 
grid.arrange(Sa_ElevationVSpsi, Sa_Lantana_camaraVSpsi, Sa_Understorey_heightVSpsi,
             Sa_NeutralVSp, nrow = 2,
             layout_matrix = rbind(c(1,2), c(3,4)))

#Since no covariate is found to significantly influence the p in co-occurrence...
#...models, and since there is no support for interactive effect on psi in co-...
#...occurrence models, there is no need for the graphing of effects of covariates...
#...for co-occurrence models.



#### 6 Optimal survey design #####



#Now that we have the psi and p for the species, we can calculate the optimum...
#...study design i.e. how many plots and how many surveys is necessary for both...
#...species for a specified level of precision in the psi and p estimates.
#...Given that only two surveys can be realistically done in one survey (due to...
#...travel and climbing involves) - one before noon and one after noon, and...
#...assuming a season lasts 3 months, where psi and p remains constant, a maximum...
#...of around 3*30*2 = 180 surveys can be done, and realistically its likely lower...
#...due to issues such as blockage of a route in the mountains, sickness or other...
#...emergencies. So, let's says 168 surveys can be done in one season. And let's...
#...keep the number of repeated surveys for each site up to 4 (K = 4). So, maximum...
#...no. of sites possible = 172/4 = 43 (S = 43)

#Occupancy models can be run under varying conditions of S and K. Let S range...
#...from 27 to 42 in increments of 1, and let K range from 2 to 4. Simulate data...
#...for each combination of S and K.

###### 6.1 M. fairbankii #######

#Set up the table using the expand.grid() function
Mf_table <- expand.grid(
  S = seq(from = 27, to = 43, by = 2),
  K = 2:4,
  psi = NA,
  se_psi = NA,  #Standard error of psi
  lci_psi = NA, #Lower 95% confidence interval of psi 
  uci_psi = NA, #Upper 95% confidence interval of psi 
  p = NA,      
  se_p = NA,    #Standard error of p
  lci_p = NA,   #Lower 95% confidence interval of p
  uci_p = NA    #Upper 95% confidence interval of p
                      )
#Head the table
head(Mf_table)

#We know that mean of model-averaged psi for M. fairbankii = 0.861815, and mean...
#...p = 0.6553877. And other values are:

#Mean of SE of psi for M. fairbankii:
mean(psi_maMf$se) # 0.1220287

#Mean of range of 95% CI of psi for M. fairbankii:
mean(psi_maMf$lower_0.95) # Mean of LCI = 0.4810529
mean(psi_maMf$upper_0.95) # Mean of UCI = 0.9798454

#Differences of CI and psi:
0.9798454-0.861815 #Upper = + 0.1180304
0.4810529-0.861815 #Lower = - 0.3807621

#Mean of SE of p for M. fairbankii:
mean(p_maMf$se) # 0.100659

#Mean of range of 95% CI of p for M. fairbankii:
mean(p_maMf$lower_0.95) # 0.4451698
mean(p_maMf$upper_0.95) # 0.8199538

#Differences of CI and p:
0.8199538-0.6553877 #Upper = + 0.1645661
0.4451698-0.6553877 #Lower = - 0.2102179

#Here, the diff. between mean modavg. psi and mean lower CI is pretty large, and...
#...can simulate data to figure out how many sites and how many surveys is optimal...
#...for that.

#Fill up the data (using genpresEV simulation) for psi and SE in Mf_table using the...
#...mean psi and p values, by use of for loops:

set.seed(45)
#Loop through rows in Mf_table dataframe:
for (i in 1:nrow(Mf_table)) {
  #Simulate the data for each row i:
  sim_data <- RPresence::genpresEV(
    N = Mf_table[i, "S"],
    K = Mf_table[i, "K"],
    psi = 0.861815,
    p = 0.6553877)
  #Create a pao object using the simulated data:
  pao <- createPao(
    data = sim_data$hst,
    frq = sim_data$frq
  )
  #Analyze the data using the top model for M. fairbankii:
  dot <- occMod(
    model = list(psi ~ 1, p ~ 1),
    data = pao
  )
  #Add psi to the Mf_table for row i, column "psi"
  Mf_table[i, 'psi'] <- unique(dot$real$psi$est)
  #Add SE to the Mf_table for row i, column "se_psi"
  Mf_table[i, 'se_psi'] <- unique(dot$real$psi$se)
  #Add LCI to the Mf_table for row i, column "lci_psi"
  Mf_table[i, 'lci_psi'] <- unique(dot$real$psi$lower_0.95)
  #Add UCI to the Mf_table for row i, column "uci_psi"
  Mf_table[i, 'uci_psi'] <- unique(dot$real$psi$upper_0.95)
  #Add p to the Mf_table for row i, column "p"
  Mf_table[i, 'p'] <- unique(dot$real$p$est)
  #Add SE to the Mf_table for row i, column "se_p"
  Mf_table[i, 'se_p'] <- unique(dot$real$p$se)
  #Add LCI to the Mf_table for row i, column "lci_p"
  Mf_table[i, 'lci_p'] <- unique(dot$real$p$lower_0.95)
  #Add UCI to the Mf_table for row i, column "uci_p"
  Mf_table[i, 'uci_p'] <- unique(dot$real$p$upper_0.95)
}

Mf_table

#The estimate for se of psi for Mf (S = 27, K = 4) is lower than the mean found...
#...during current study. It can normalised.
#Diff. of se from current study and simulation:
0.1220287-0.07016489 # = 0.05186381

#We can divide every value in the se_psi column with the value for S = 27, K = 4,
#...(0.07016489) to maintain the ratios in the outputs of the simulation and then...
#...multiply 0.122028 with all of them to get corrected/ values.
Mf_table$se_psi <- (Mf_table$se_psi/Mf_table [19, 4])*0.1220287
head(Mf_table)

#Similar adjustments across the dataframe needs to be done for comparison:
Mf_table$lci_psi <- (Mf_table$lci_psi/Mf_table [19, 5])*0.4810529
Mf_table$uci_psi <- ifelse((Mf_table$uci_psi/Mf_table [19, 6])*0.9798454 > 1, 1,
                                  (Mf_table$uci_psi/Mf_table [19, 6])*0.9798454)
#If else condition used since prob. cannot be greater than 1.
Mf_table$se_p    <- (Mf_table$se_p/Mf_table [19, 8])*0.100659
Mf_table$lci_p   <- (Mf_table$lci_p/Mf_table [19, 9])*0.4451698
Mf_table$uci_p   <- (Mf_table$uci_p/Mf_table [19, 10])*0.8199538

Mf_table

#Save as word document:
rMf_table <- setNames(Mf_table,cbind("No. of Sites", "No. of surveys", 
                            "Estimate (psi)", "Standard Error (psi)",
                            "Lower 95% Confidence Interval (psi)",
                            "Upper 95% Confidence Interval (psi)",
                            "Estimate (p)", "Standard Error (p)",
                            "Lower 95% Confidence Interval (p)",
                            "Upper 95% Confidence Interval (p)"))
rMf_table <- flextable(rMf_table)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rMf_table) %>%  #Insert the flextable object there
  print(target = "rMf_table.docx")    

#Create plots:

se_psi_Mf <- ggplot(Mf_table, aes(x=as.factor(K), y = se_psi,
         colour = as.factor(S), group = as.factor(S))) +
  ggtitle(label = "(a) SE of psi vs K (Sitewise)") +
  geom_point(size = 3.5, shape ="diamond", show.legend = FALSE) +
  scale_y_continuous(breaks = seq(from = 0, to = 0.21, by = 0.005)) +
  labs(x = "No. of surveys",
       y = "Standard error",
       colour = "No. of sites") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

se_psi_Mf

se_p_Mf <- ggplot(Mf_table, aes(x=as.factor(K), y = se_p,
                                  colour = as.factor(S), group = as.factor(S))) +
  ggtitle(label = "(b) SE of p vs K (Sitewise)") +
  geom_point(size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0, to = 0.21, by = 0.005)) +
  labs(x = "No. of surveys",
       y = NULL,
       colour = "No. of sites") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

se_p_Mf

ci_psi_Mf <- ggplot(Mf_table, aes(x=as.factor(K), y = psi,
                                  colour = as.factor(S), group = as.factor(S))) +
  ggtitle(label = "(c) CI of psi vs K (Sitewise)") +
  geom_pointrange(aes(ymin = lci_psi, ymax = uci_psi),
                 size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lci_psi, ymax = uci_psi), width = 0.7,
               size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(x = NULL,
       y = "95% confidence intervals",
       colour = "No. of sites") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

ci_psi_Mf

ci_p_Mf <- ggplot(Mf_table, aes(x=as.factor(K), y = p,
                                  colour = as.factor(S), group = as.factor(S))) +
  ggtitle(label = "(d) CI of p vs K (Sitewise)") +
  geom_pointrange(aes(ymin = lci_p, ymax = uci_p),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lci_p, ymax = uci_p), width = 0.7,
                size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(x = "No. of surveys",
       y = "95% confidence intervals",
       colour = "No. of sites") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

ci_p_Mf

grid.arrange(se_psi_Mf, se_p_Mf, ci_psi_Mf, ci_p_Mf, nrow = 2, ncol = 5,
             layout_matrix = rbind(c(1,2,3,3,3), c(1,2,4,4,4)))


###### 6.2 S. albiventris #######

#Set up the table using the expand.grid() function
Sa_table <- expand.grid(
  S = seq(from = 27, to = 43, by = 2),
  K = 2:4,
  psi = NA,
  se_psi = NA,  #Standard error of psi
  lci_psi = NA, #Lower 95% confidence interval of psi 
  uci_psi = NA, #Upper 95% confidence interval of psi 
  p = NA,      
  se_p = NA,    #Standard error of p
  lci_p = NA,   #Lower 95% confidence interval of p
  uci_p = NA    #Upper 95% confidence interval of p
                      )
#Head the table
head(Sa_table)

#We know that mean of model-averaged psi for S. albiventris = 0.4543001, and mean...
#...p = 0.5730775. And other values are:

#Mean of SE of psi for S. albiventris:
mean(psi_maSa$se) # 0.12807

#Range of 95% CI of psi for S. albiventris:
mean(psi_maSa$lower_0.95) # Mean of LCI = 0.1351068
mean(psi_maSa$upper_0.95) # Mean of UCI = 0.712619

#Differences of CI and psi:
0.712619-0.4543001 #Upper = + 0.2583189
0.1351068-0.4543001 #Lower = - 0.3191933

#Mean of SE of p for S. albiventris:
mean(p_maSa$se) # 0.1348734

#Range of 95% CI of p for S. albiventris:
mean(p_maSa$lower_0.95) # 0.3066634
mean(p_maSa$upper_0.95) # 0.7947845

#Differences of CI and p:
0.7947845-0.5730775 #Upper = + 0.221707
0.3066634-0.5730775 #Lower = - 0.2664141

#Here, the diff. between mean modavg. psi and mean lower CI as well as upper CI...
#...is slightly large, and we can simulate data to figure out how many sites and...
#...how many surveys is optimal for that.

#Fill up the data (using genpresEV simulation) for psi and SE in Sa_table using the...
#...mean psi and p values, by use of for loops:

#Loop through rows in Mf_table dataframe:
for (i in 1:nrow(Sa_table)) {
  #Simulate the data for each row i:
  sim_data <- RPresence::genpresEV(
    N = Sa_table[i, "S"],
    K = Sa_table[i, "K"],
    psi = 0.4543001,
    p = 0.5730775)
  #Create a pao object using the simulated data:
  pao <- createPao(
    data = sim_data$hst,
    frq = sim_data$frq
  )
  #Analyze the data using the top model for M. fairbankii:
  dot <- occMod(
    model = list(psi ~ 1, p ~ 1),
    data = pao
  )
  #Add psi to the Sa_table for row i, column "psi"
  Sa_table[i, 'psi'] <- unique(dot$real$psi$est)
  #Add SE to the Sa_table for row i, column "se_psi"
  Sa_table[i, 'se_psi'] <- unique(dot$real$psi$se)
  #Add LCI to the Sa_table for row i, column "lci_psi"
  Sa_table[i, 'lci_psi'] <- unique(dot$real$psi$lower_0.95)
  #Add UCI to the Sa_table for row i, column "uci_psi"
  Sa_table[i, 'uci_psi'] <- unique(dot$real$psi$upper_0.95)
  #Add p to the Sa_table for row i, column "p"
  Sa_table[i, 'p'] <- unique(dot$real$p$est)
  #Add SE to the Sa_table for row i, column "se_p"
  Sa_table[i, 'se_p'] <- unique(dot$real$p$se)
  #Add LCI to the Sa_table for row i, column "lci_p"
  Sa_table[i, 'lci_p'] <- unique(dot$real$p$lower_0.95)
  #Add UCI to the Sa_table for row i, column "uci_p"
  Sa_table[i, 'uci_p'] <- unique(dot$real$p$upper_0.95)
}

Sa_table

#The estimate for se of psi for Mf (S = 27, K = 4) is lower than the mean found...
#...during current study. It can normalised.
#Diff. of se from current study and simulation:
0.12807-Sa_table [19, 4] # = 0.02861699

#We can divide every value in the se_psi column with the value for S = 27, K = 4,
#...(0.07016489) to maintain the ratios in the outputs of the simulation and then...
#...multiply 0.122028 with all of them to get corrected/ values.
Sa_table$se_psi <- (Sa_table$se_psi/Sa_table [19, 4])*0.12807
head(Sa_table)

#Similar adjustments across the dataframe needs to be done for comparison:
Sa_table$lci_psi <- (Sa_table$lci_psi/Sa_table [19, 5])*0.1351068
Sa_table$uci_psi <- (Sa_table$uci_psi/Sa_table [19, 6])*0.712619

#If else condition used since p cannot be greate than 1.
Sa_table$se_p    <- (Sa_table$se_p/Sa_table [19, 8])*0.1348734
Sa_table$lci_p   <- (Sa_table$lci_p/Sa_table [19, 9])*0.3066634
Sa_table$uci_p   <- (Sa_table$uci_p/Sa_table [19, 10])*0.7947845

Sa_table
head(Sa_table)

#Save as word document:
rSa_table <- setNames(Sa_table,cbind("No. of Sites", "No. of surveys", 
                                     "Estimate (psi)", "Standard Error (psi)",
                                     "Lower 95% Confidence Interval (psi)",
                                     "Upper 95% Confidence Interval (psi)",
                                     "Estimate (p)", "Standard Error (p)",
                                     "Lower 95% Confidence Interval (p)",
                                     "Upper 95% Confidence Interval (p)"))
rSa_table <- flextable(rSa_table)
read_docx() %>%                                #Create a null .docx object
  body_add_flextable(value = rSa_table) %>%  #Insert the flextable object there
  print(target = "rSa_table.docx")    

#Create plots:

se_psi_Sa <- ggplot(Sa_table, aes(x=as.factor(K), y = se_psi,
                                  colour = as.factor(S), group = as.factor(S))) +
  ggtitle(label = "(a) SE of psi vs K (Sitewise)") +
  geom_point(size = 3.5, shape ="diamond", show.legend = FALSE) +
  scale_y_continuous(breaks = seq(from = 0, to = 0.21, by = 0.005)) +
  labs(x = "No. of surveys",
       y = "Standard error",
       colour = "No. of sites") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

se_psi_Sa

se_p_Sa <- ggplot(Sa_table, aes(x=as.factor(K), y = se_p,
                                colour = as.factor(S), group = as.factor(S))) +
  ggtitle(label = "(b) SE of p vs K (Sitewise)") +
  geom_point(size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0, to = 0.3, by = 0.01)) +
  labs(x = "No. of surveys",
       y = NULL,
       colour = "No. of sites") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

se_p_Sa

ci_psi_Sa <- ggplot(Sa_table, aes(x=as.factor(K), y = psi,
                                  colour = as.factor(S), group = as.factor(S))) +
  ggtitle(label = "(c) CI of psi vs K (Sitewise)") +
  geom_pointrange(aes(ymin = lci_psi, ymax = uci_psi),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lci_psi, ymax = uci_psi), width = 0.7,
                size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(x = NULL,
       y = "95% confidence intervals",
       colour = "No. of sites") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

ci_psi_Sa

ci_p_Sa <- ggplot(Sa_table, aes(x=as.factor(K), y = p,
                                colour = as.factor(S), group = as.factor(S))) +
  ggtitle(label = "(d) CI of p vs K (Sitewise)") +
  geom_pointrange(aes(ymin = lci_p, ymax = uci_p),
                  size = 0.35, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lci_p, ymax = uci_p), width = 0.7,
                size = 0.35, position = position_dodge(0.7), show.legend = FALSE) +
  geom_point(position = position_dodge(0.7), size = 3.5, shape ="diamond") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(x = "No. of surveys",
       y = "95% confidence intervals",
       colour = "No. of sites") +
  theme(axis.text = element_text(colour = "black", size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
  scale_colour_viridis_d(option = "turbo", direction = -1, begin = 0, end = 0.8)

ci_p_Sa

grid.arrange(se_psi_Sa, se_p_Sa, ci_psi_Sa, ci_p_Sa, nrow = 2, ncol = 5,
             layout_matrix = rbind(c(1,2,3,3,3), c(1,2,4,4,4)))
