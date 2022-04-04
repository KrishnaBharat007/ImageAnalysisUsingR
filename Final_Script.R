#### Initial Files and Packages Setup ----

# Change the 'path' variable to the path where required files are present along with 'Final_Script.R'
# Required files: 
#   1.GRIexcel.rds 
#   2.Folder with name 'Telecommunications' with all 847 pdf files of Telecommunications sector
#   3.Dataset.csv which has the analyzed data points from cover images.
library(ggplot2)
library(pdftools)


#### Extract Data from RDS ----

# Setup the path 

path <- paste0(getwd(), "/") # Change this according to your file system

setwd(path)

# Read and Filter Rds data for extracting Telecommunications Data.
filename <- paste0(path,'GRIexcel.rds')
Data_raw <- readRDS(filename)
Data_Telecom <- Data_raw[Data_raw$Sector == "Telecommunications",]
#Comments: Fetches 1540 rows out of 47093 rows, with 41 columns

# Cleanse and add new field 'pdf_names' to display file names of pdfs.
pdf_name <- paste(Data_Telecom$Name,'_',Data_Telecom$`Publication Year`,'.pdf',sep = '')
pdf_name <- gsub(' ','',pdf_name)
pdf_name <- gsub('&','_', pdf_name)
Data_Telecom$pdf_name <- pdf_name


#### Extract Cover Pages ----

# Extracts first pages from all 847 files, skips the files which are corrupted and with non English characters in file name.
dir.create(paste0(path,'Telecom_First_Pages'))
for (file in Data_Telecom$pdf_name) {
  src_file = paste0(path,'Telecommunications/',file)
  tgt_file = paste0(path,'Telecom_First_Pages/',file)
  try(pdf_subset(src_file,pages = 1, output = tgt_file))
}

# List out Available, Proper and Skipped Files
available_files <- list.files(paste0(path,'Telecommunications/')) #847 Files
proper_files <- list.files(paste0(path,'Telecom_First_Pages')) #627 Files
skipped_files <- available_files[!(available_files %in% proper_files)] #220 Files

# Split first page extracted files based on Continent For Analysis
src <- paste0(path,'Telecom_First_Pages/')
cont_dir <- paste0(path,'Continent_Wise_Pdfs/')
dir.create(cont_dir)
setwd(cont_dir)
reg_list <- unique(Data_Telecom$Region)
cont_list <- gsub(' ','_',reg_list)
# Create 6 Directories for Continents.
for (cont in cont_list) { dir.create(cont) }
setwd(src)
# Copy 627 Files to respective Continent folders.
for (ind  in 1:6) {
  tgt <- paste0(cont_dir,cont_list[ind])
  temp_df <- Data_Telecom[Data_Telecom$Region == reg_list[ind],]
  file.copy(temp_df$pdf_name, tgt )
}
setwd(path)


#### Working Dataset Creation ----

# Dataset.csv file is created manually, by analyzing 234 pdf files picked from Africa, Asia and Europe
dataset <- data.frame(read.csv2('Dataset.csv',sep = ','))

# Dataframe dataset_cleanse is created to fix the data integrity issues of the fields.
dataset_cleanse <- dataset
dataset_cleanse$pdf_name <- gsub('Â','', dataset_cleanse$pdf_name)
dataset_cleanse$background <- tolower(dataset_cleanse$background)
dataset_cleanse$background <- ifelse(dataset_cleanse$background == 'w', 'white', dataset_cleanse$background)
dataset_cleanse$nature <- ifelse(dataset_cleanse$nature %in% c('y','Yes'), TRUE, FALSE)
dataset_cleanse$language <- ifelse(dataset_cleanse$language %in% c('english','English','Yes'),'English', 'Others')
dataset_cleanse$photo <- ifelse(dataset_cleanse$photo %in% c('y','Yes'), TRUE, FALSE)
dataset_cleanse$busy <- tolower(dataset_cleanse$busy)
dataset_cleanse$busy <- ifelse(startsWith(dataset_cleanse$busy,'s'),'Somewhat Busy',ifelse(startsWith(dataset_cleanse$busy,'n'),'Not Busy','Very Busy'))
dataset_cleanse$logo <- ifelse(dataset_cleanse$logo %in% c('y','Yes'), TRUE, FALSE)
dataset_cleanse$graphic <- ifelse(dataset_cleanse$graphic %in% c('y','Yes'), TRUE, FALSE)

# Data Cleansing Validation
table(dataset_cleanse$background)
table(dataset$background)
table(dataset_cleanse$nature)
table(dataset$nature)
table(dataset_cleanse$language)
table(dataset$language)
table(dataset_cleanse$photo)
table(dataset$photo)
table(dataset_cleanse$busy)
table(dataset$busy)
table(dataset_cleanse$logo)
table(dataset$logo)
table(dataset_cleanse$graphic)
table(dataset$graphic)

# Check if all our data is in dataset
inside <- dataset_cleanse$pdf_name %in% filenames
sum(inside)/length(inside) # should be 1

# Merge the data
Dataset_complete <- merge(dataset_cleanse, Data_Telecom)
length(Dataset_complete$pdf_name)

# We somehow have 235 entries but we only had 234 before
sort(table(Dataset_complete$pdf_name))
# Comments: Euskaltel_2014.pdf is the culprit with 2 entries in Data_Telecom

# Remove one row from final dataset and check again
Dataset_complete <- Dataset_complete[Dataset_complete$Title != 'Informe Anual 2013',]
length(Dataset_complete$pdf_name) #234 Entries

# Create CSV file of Final Dataset Completed after merging.
write.table(Dataset_complete, file=file('Dataset_complete.csv'), quote=FALSE, sep=',',row.names = F)


#### Plotting Setup ----

# Show counts of files used from each continent
ggplot(data = Dataset_complete, aes(Continent, fill = Continent)) +
  geom_bar(width = 0.5)

# Clean up dataset so humans are split in 3 categories 0, 1, >1
mod_data <- Dataset_complete
mod_data$humans <- ifelse(Dataset_complete$humans < 2, Dataset_complete$humans , "more than 1")

# Golden ratio according to Arend Hintze
width <- 7.5
height = 7


#### Humans plot ----
ggplot(data = mod_data, aes(`Publication Year`, group=humans, fill = humans)) +
  geom_bar(stat = "count") +
  facet_wrap(~Continent,ncol = 1) +
  scale_x_discrete(breaks=seq(2008, 2018, 2)) +
  ylab("Number of Reports") +
  scale_fill_discrete(name = "", 
                      labels = c("No Humans", "1 Human", ">1 Humans")) +
  theme(text = element_text(size = 20))

# Save as pdf
ggsave('plot_humans.pdf', width = width, height = height)  

# Logo plot ----
ggplot(data = mod_data, aes(`Publication Year`, group=logo, fill = logo)) +
  geom_bar(stat = "count") +
  scale_x_discrete(breaks=seq(2008, 2018, 2)) +
  facet_wrap(~Continent,ncol = 1) +
  ylab("Number of Reports") +
  theme(text = element_text(size = 20)) +
  scale_fill_discrete(name = "", 
                      labels = c("No Logo", "Has a logo"))

# Save as pdf
ggsave('plot_logo.pdf', width = width, height = height) 

# Busyness plot ----
ggplot(data = mod_data, aes(`Publication Year`, group=busy, fill = busy)) +
  geom_bar(stat = "count") +
  facet_wrap(~Continent,ncol = 1) +
  scale_x_discrete(breaks=seq(2008, 2018, 2)) +
  ylab("Number of Reports") +
  theme(text = element_text(size = 20)) +
  scale_fill_discrete(name = "")

# Save as pdf
ggsave('plot_busyness.pdf', width = width, height = height)
