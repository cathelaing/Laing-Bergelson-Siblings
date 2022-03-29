# Laing-Bergelson-Siblings - Read Me ###

## 1. Demographics (Demographics.R)

This is the main file containing all demographics data, incorporating relevant variables from the demographics questionnaires at 6 and 18 months,
mothers' PVT scores, and CDI data (productive vocabulary) from 6-18 months, as well as additional variables taken from the monthy video and audio home-recordings 
at age 6-17 months.

This is the first script to be generated in the siblings analysis markdown file; it generates the *SiblingsData* dataframe used in the main analysis. 

*SiblingsData* includes the following variables:

1. *subj*: infant ID, determined by random number generator          
2. *month*: age in months, 10-18            
3. *sex*: F/M              
4. *MOTedulevel*: numeric variable representing mother's education from 1 (High School) to 6 (Doctorate)           
5. *age_mom*: mother's age in years      
6. *PVTscore*: index of mother's vocabulary size, higher numbers indicate better vocabulary score
7. *SibsYN*: binomial variable to determine whether or not child has any siublings (Y) or not (N) 
8. *Siblings6*: number of siblings when the child was 6 months old as a continuous variable from 0-5
9. *Siblings18*: number of siblings when the child was 18 months old as a continuous variable from 0-5
10. *SibGroup6*: number of siblings when the child was 6 months old as a factorial variable, none (0 siblings), one (1 sibling), 2+ (2 or more siblings)
11. *SibGroup18*: number of siblings when the child was 18 months old as a factorial variable, none (0 siblings), one (1 sibling), 2+ (2 or more siblings)
12. *Total.words*: Total words in the child's productive vocabulary as reported on the CDI questionnaire 
13. *Log.Totalwords*: log-transformed measure of *Total.words* 
14. *tokens_video*: number of word tokens produced by the target child in the hour-long video data 
15. *types_video*: number of word types produced by the target child in the hour-long video data 
16. *tokens_audio*: number of word tokens produced by the target child in the 3-hour audio data
17. *types_audio*: number of word types produced by the target child in the 3-hour audio data
18. *Log.tokens_video*: log-transformed measure of *tokens_video* 
19. *Log.types_video*: log-transformed measure of *types_video*  
20. *Log.tokens_audio*: log-transformed measure of *tokens_audio* 
21. *Log.types_audio*: log-transformed measure of *types_audio* 

Note that one child had no siblings at 6 months but had one sibling at age 18 months.

# 2. Data gathering (Data gathering.R)

This file extracts the relevant data from the basiclevels dataset (basic_levels_randsubj.csv). This is the dataset incorporating all variables extracted from the SEEDLingS corpus (Bergelson, 2016). All video and audio recordings were annotated for each object word (i.e. concrete noun) produced either by or towards the target child, included in the *basic_level* column. For the analysis of caregiver input, this was filtered to include a) only video data, and b) no productions from the target child. Three main variables were then extracted and joined with the Demographics spreadsheet: object presence, utterance type, and speaker type. These are filtered to include only the data relevant to the caregiver input aspect of the analysis. The following dataframes are then generated:

* *utterance.type*: raw number and proportion of all basic_level words that were produced accross 7 utterance type variables (d = declarative, i = imperative, n = non-utterance (i.e. single-word utterance), q = question, r = reading, s = singing, u = unclear). All utterance types were used for the analyses except in the analysis of reading, when the data was filtered to include only object words produced while reading.
* *object.presence*: raw number and proportion of all basic_level words that were produced while being touched, held or pointed at by the speaker or baby, or were clearly being visually focused on.
* *speaker.type*:  raw number and proportion of all utterances produced by the mother (MOT), father (FAT) and sibling/s (SIBLING). An additional variable is created called *Family.input* which aggregates all words produced by the three speaker types

A final dataframe is generated called *in.cdi*: this represents raw number and proportion of basic_level words that match a word on the CDI. Each word in the basic_level column is compared against a list of CDI words, *CDI_wordlist.csv*. This file contains all of the CDI words,
with a list of all basic_level words (as produced by all speakers except the infant) that match the CDI words. In cases where the basic_level is an exact match to the CDI word (e.g. the mother says *arm* which matches the CDI word *arm* exactly, or *legs* which is a close match to CDI form *leg*) the basic_level word is coded as a match to the CDI (i.e. is TRUE). In some cases the basic_level word is a variant of a CDI word; all the words that were not coded as exact matches (i.e. FALSE) were extracted and coded manually. When a compound word contained a noun that was present on the CDI wordlist, this was coded as TRUE, e.g. *fish+bowl* was coded as a match to *fish*, and book titles including words such as *baby* were coded as a match to *baby*.

## Data files saved in folder:

*demo_deid_cleaned.csv* - demographics data for all SEEDLingS participants, copied from Work study folder on 9th July 2019

*ParentPVT* - PVT scores and ages for all SEEDLingS mothers, copied from Work study folder on 9th July 2019

*cdi* - CDI data of all participants, copied from Work study folder on 9th July 2019 and then joined with randomly-generated anonymous subject numbers 

*in_cdi_Wordlist.csv* - This file contains all of the CDI words, with a list of all basic_level words (as produced by all speakers except the infant) that match the CDI words. This was generated using the following code (where 'data' is the all_basic_levels spreadsheet):

```{r}
in_cdi_queries <- data %>%
    distinct(basic_level, object) %>%
    left_join(wordlist, by=c("basic_level"="Word")) %>% # match words to CDI forms using the CDI_wordlist.csv spreadsheet
    mutate(in_cdi = ifelse(!is.na(Form),T, F)) %>%
    arrange(desc(in_cdi))

write_csv(in_cdi_queries, "Data/in_cdi_queries.csv")
```

This generates a spreadsheet with all object words in the data, all corresponding basic_levels, and whether the word matches a CDI word (TRUE or FALSE). If there is a match between object word and CDI word, the CDI word is listed in the Form column.

CL and EB then cross-checked this list in 2017 (updated in 2022!) to make sure that all forms in the list were correctly coded, and to remove any that were not appropriate for the analysis. Since the SEEDLingS corpus looks at object words only, any non-object words (such as onomatopoeia, adjectives, prepositions, etc) were removed from the list. All book titles, character names and people (e.g. 'daddy', 'grandpa') were also removed. The spreadsheet was then saved as in_CDI_wordlist.csv.

## Other documents ##

*Sibling ages.xslx* includes all relevant information about sibling ages to calculate mean age of siblings.

*Sibling_ages.csv* is a new version of the .xslx spreadsheet above, with SibGroup information added for analysis. Siblings are coded as being 'Younger' or 'Older': younger siblings are <3 years older than the infant, older siblings are older than the infant by 3 years or more.

*CDI_wordlist2* is a database of all words on the CDI, 'matched' against basic levels. Each word on the CDI was compared against the basic levels to determine which variants of the CDI form were produced in the Seedlings data. For example, I did a search for the word 'dog' in the basic_levels spreadsheet to see which variants of 'dog' were produced in the data. This included e.g. 'doggie', 'dogs', 'doggie+dog'. These words were entered into the CDI spreadsheet. The CDI_wordlist2 dataset is therefore a list of all CDI words and all of their variants in the Seedlings data. This can be used to determine whether or not a word in the data is a match to a CDI word.




