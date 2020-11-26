
### Siblings data - Read Me ###

# File 1: Demographics gathering (Demographics gathering.R)

# This is the main file containing all demographics data, incorporating some data from the demographics questionnaires at 6 and 18 months,
# mothers' PVT scores, and CDI data (productive vocabulary) from 6-18 months.

# This is the first script to generate when arranging data for the siblings analysis. It is also used in the Work Status analysis.

# This script also generates the 'Homelifedata.csv' file through combination with the CDI data:

# Homelifedata <- read_csv(Homelifedata.full, "H:/Data/Siblings/Homelifedata.full.csv")


# File 2: Data gathering (Data gathering_useme.R)

# This file extracts the relevant data from the basiclevels feather spreadsheet (Seedlings/Compiled_Data/basiclevels/all_basiclevel_home_data_featherUSEME)
# in the Seedlings folder

# Three main variables are extracted and joined with the Demographics spreadsheet: object presence, utterance type, and speaker type. It creates output
# csv files which are saved in H:\Data\Siblings\Data\CSVs for analysis.

# This script also summarizes in.cdi data. This relates to the 'ease of acquisition' variable in the Siblings analysis. Each word in the basic_level
# column is compared against a file called 'CDI_wordlist' (Y:/Seedlings/Working_Files/Catherine/CDI_wordlist.csv). This file contains all of the CDI words,
# with a list of all basic_level words (as produced by all speakers except the infant) that match the CDI words. For example, the basic level 'vroom+vroom'
# is a match for the CDI word 'vroom'. Similarly, the phrase 'moo+baa+la+la+la' is coded as matching 'baabaa' and 'moo' on the CDI.
# This way, we can determine whether any word in the basic_level column matches a word on the CDI.

# Note: It was decided that basic levels such as 'moo baa la la la' should be included as matching CDI words since they are composed of words that are
# acquired early in production. 


# This script should be run after running the Demograpics gathering script


## Other documents ##

# Sibling ages.xslx is a copy of the Demo_AP.xslx spreadsheet in the Seedlings/Subject_Info_and_Paperwork folder. I extracted all relevant information about
# the infants in the Siblings analysis to calculate mean age of siblings.

# Sibling_ages.csv is a new version of the .xslx spreadsheet above, with SibGroup information added for analysis. Siblings are coded as being 'Younger'
# or 'Older': younger siblings are <3 years older than the infant, older siblings are older than the infant by 3 years or more.

# CDI.csv is taken from the file Y:/Seedlings/Subject_Info_and_Paperwork/demo_deid_cleaned.csv. This version is edited in the Demographics gathering.R document
# and saved here for other uses




