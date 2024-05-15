#Parse Questionnaire_New
#~Saves Questionnaire+ValueSet resources as json in R4json subfolder
#~Parses Questionnaire Json to flat csv + saves in R4csv subfolder
#~Updates Repo files
#~Logs and saves script instance under Questionnaire name

#Author: j.hendrikx@santeon.nl
 
#~remove all objects from the current workspace to start with a clean slate
rm(list = ls())
 
#Log start run####
rundate<-(paste("#Start of Analysis - System time ", Sys.time()))

#System config####
#install.packages("tidyverse")
#installed.packages("jsonlite")

#Load packages
library(tidyverse)
library(jsonlite)
library(stringr)

#Create generic functions
# Function to remove leading and trailing whitespace from all strings in a dataframe
trim_df <- function(df) {
  # Apply trimws to all columns of the dataframe
  df[] <- lapply(df, function(x) if(is.character(x)) trimws(x) else x)
  return(df)
}

#set paths for different working stations
#Home
wdHomeTeams ="C:\\Users\\joshe\\Santeon\\PB IenI - General\\Development\\SIM\\SIM-on-FHIR\\"
wdHomeRepo ="C:\\Users\\joshe\\Repos\\sim-on-fhir\\BulkEport\\"

#Santeon laptop
wdLaptopTeams ="C:\\Users\\j.hendrikx\\Santeon\\PB IenI - General\\Development\\SIM\\"
wdLaptopRepo ="C:\\Users\\j.hendrikx\\Repos\\HipsETL\\"

#Santeon remote desktop
wdSRD ="S:\\VBHC\\VBHC-Centraal\\SIM\\"
wdSRDRepo="C:\\Users\\j.hendrikx\\Documents\\HipsETL\\"
 
# Input Parameters####
#~Set working directory paths#####
wd<-wdHomeRepo
setwd(wd)

#Load ConceptMaps
library(readr)
ZibToFHIRtypeMap <- read_delim("ZibToFHIRtypeMap.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

#~Set transaction url####
# FHIR transaction URL
#copy link from https://decor.nictiz.nl/ad/#/san-gen-/project/project-index/transactions ' Column FHIR >R4'
#urltrans<-"http://decor.nictiz.nl/fhir/4.0/sansa-/StructureDefinition/2.16.840.1.113883.2.4.3.11.60.909.4.41--20240320000000?_format=json"

#DECOR transaction url (non-FHIR)
#copy link from https://decor.nictiz.nl/ad/#/san-gen-/project/project-index/transactions ' Column Decor JSON'
urltrans2<-"http://decor.nictiz.nl/decor/services/RetrieveTransaction?id=2.16.840.1.113883.2.4.3.11.60.909.4.41&effectiveDate=2024-03-20T00%3A00%3A00&language=nl-NL&ui=nl-NL&format=json"

#DECOR transaction AD-API call####
trans<-read_json(paste0(urltrans2))

#~DECOR parse transaction for generating FHIR Bulk API Query####

#Get type filters (models/resources) in transaction###

types<- tryCatch({
  trans|>
    tibble()%>%
    unnest_longer(trans)%>%
  unnest_longer(trans)%>%
  select(trans)%>%
  filter(row_number()==length(trans))%>%
  unnest_longer(trans)%>%
  unnest_wider(trans, names_sep = ".")%>% #model/type level
  select(trans.shortName)
           }, error = function(e) {
             message("Type (model/resource) filters not present", e$message)
             return(NULL)
           })

#join available types with ZIBtoFHIR conceptmap if present
if (!is.null(types) ) { #check if null or empty df due to filtering displays/groups
  types <- types %>%
    left_join(ZibToFHIRtypeMap, by = 'trans.shortName',suffix=c("",".drop"),copy=TRUE,relationship = 'one-to-one')%>%
    distinct(trans.type, .keep_all=TRUE)%>% #filter out duplicate resource calls e.g. observations
    select(-contains(".drop"))
} else {
  message("error in mapping ZIB models to FHIR resources")
}

querytype<-paste0(types[["trans.type"]], collapse = ",")

query<-paste0("/Patient/$export?_type=",querytype,"&")

print(query)

#Save updated local files & remove object in R
writeLines(query,"exportquery.txt")

#Get typeFilters####
tryCatch({
  typeFilter<- 
    trans|>
    tibble()%>%
    unnest_longer(trans)%>%
    unnest_longer(trans)%>%
    select(trans)%>%
    filter(row_number()==length(trans))%>%
    unnest_longer(trans)%>%
    unnest_wider(trans, names_sep = ".")%>% #model/type level
    select(-trans.id:-trans.iddisplay,-trans.implementation:-trans.terminologyAssociation)%>%
    unnest_longer(trans.concept)%>% #element level
    unnest_wider(trans.concept)%>%
    select(-iddisplay,-implementation,-name,-desc,-valueDomain,-relationship,-operationalization,-terminologyAssociation,-identifierAssociation)%>%
    unnest_wider(context,names_sep = ".")%>% #wider to extract context and keep records without context
    unnest_wider(context.1)%>% #wider to extract context and keep records without context
    select(-language)%>%
    rename(filterValueset='#text')%>%
    unnest_wider(inherit,names_sep = ".")%>% #add ZIB reference
    unnest_wider(inherit.1,names_sep = ".")%>%
    select(-inherit.1.ref:-inherit.1.iEffectiveDate)%>% #remove additional reference metadata
    rename(zibElement=inherit.1.refdisplay)%>%
    select(-valueSet,-concept)%>%
        mutate(filterValueset2 = str_extract_all(filterValueset, '(?<=href=\\"vs::).*(?=\\" class=)'))%>% 
            mutate(filterValueset3 = across(filterValueset2, ~ gsub("[-T:]", "", filterValueset2)))%>%
        mutate(filterValueset = gsub("/", "", filterValueset))
    
    print(typeFilter$filterValueset)
    
    mutate(extracted_string = if_else(!is.na(filterValueset), regmatches(filterValueset, regexpr("(?<=vs::)[^\\/]+", filterValueset, perl=TRUE)), NA),
      extracted_datetime = if_else(!is.na(filterValueset), regmatches(filterValueset, regexpr("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", filterValueset)), NA))
  
    
    mutate(extracted_string =if_else(!is.na(filterValueset),(regmatches(filterValueset, regexpr("(?<=vs::)[^\\/]+", filterValueset, perl=TRUE))),NA),
           extracted_datetime = if_else(!is.na(filterValueset),(regmatches(filterValueset, regexpr("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", filterValueset))),NA))
           
    
}, error = function(e) {
  message("2 level nested valuesets not present", e$message)
  return(NULL)
})

#Get elements filter####
tryCatch({
  elements<- 
    trans|>
    tibble()%>%
    unnest_longer(trans)%>%
    unnest_wider(trans)%>%
    unnest_longer(concept)%>%
    select(concept)%>%
    unnest_wider(concept)%>% #optional 1st level unnesting if one level deeper is needed
    unnest_longer(concept)%>%
    select(concept,inherit)%>%
    unnest_wider(concept,names_sep = '.')%>%
    select(concept.shortName,inherit)%>%
    unnest_longer(inherit, keep_empty = TRUE)%>%
    unnest_wider(inherit),names_sep = '.')%>%
  
}, error = function(e) {
  message("no root elements selected", e$message)
  return(NULL)
})





#                           GET [base]/Condition?code:in=http%3A%2F%2Fsnomed.info%2Fsctfhir_vs%3Disa%2F235862008
# would match any conditions that contain any code from 'http://snomed.info/sct?fhir_vs=isa/235862008', e.g.:
#   
#   SNOMED 235862008 - Hepatitis due to infection (this code)
# SNOMED 773113008 - Acute infectious hepatitis (is-a 235862008)
# SNOMED 95897009 - Amebic hepatitis (is-a 235862008)
# etc.

# %3 =
# %3A :
# %3F ?
# %2F /

print(typeFilter$`trans.concept.context.#text`)

extracted_string <- regmatches(typeFilter$trans.concept.context, regexpr("(?<=vs::)[^\\\"]+", typeFilter$trans.concept.context))

print(extracted_string)

# Extract the desired substring without "vs::"
extracted_string <- regmatches(typeFilter$`trans.concept.context.#text`, regexpr("(?<=vs::)[^\\/]+", typeFilter$`trans.concept.context.#text`, perl=TRUE))
extracted_datetime <- regmatches(typeFilter$`trans.concept.context.#text`, regexpr("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", typeFilter$`trans.concept.context.#text`))

# Print the extracted substring
print(extracted_string)


http://decor.nictiz.nl/fhir/4.0/sansa-/ValueSet/2.16.840.1.113883.2.4.3.11.60.909.11.37--20240320083153

testurl<-paste0("http://decor.nictiz.nl/fhir/4.0/sansa-/ValueSet/", extracted_string, "--", (gsub("[-T:]", "", extracted_datetime)))

testurl

# Provided string
provided_string <- "&lt;p&gt;&lt;a href=\"vs::2.16.840.1.113883.2.4.3.11.60.909.11.37/2024-03-20T08:31:53\" class=\"__artdecor__a__inline__artifact __artdecor_vs\" onclick=\"event.stopPropagation(); event.preventDefault(); return\"&gt;IBDdiagnoses&lt;/a&gt;&lt;/p&gt;"

# Extract the date and time
extracted_datetime <- regmatches(provided_string, regexpr("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", provided_string))

# Format the date and time as "YYYYMMDDHHMMSS"
formatted_datetime <- gsub("[-T:]", "", extracted_datetime)

# Print the formatted date and time
print(formatted_datetime)



Qtrans<-NULL

#~~bind QtransTop, 1L, 2L####
#join available terminology information to higher level items if present
if (!is.null(QtransTop)) { #check if null
  Qtrans <- Qtrans %>%
    bind_rows(QtransTop)%>%
    rowid_to_column(var="index")%>% #union does not work for updated versions of same rows
    group_by(Questionnaire.item.linkId)%>%
    slice(n())%>% #to select last added row within the group// if you want to keep first original existing row then use distinct(code)
    #distinct(code, .keep_all=TRUE)%>%
    ungroup(Questionnaire.item.linkId)%>%
    arrange(index)%>%
    select(-index)%>%
    arrange(Questionnaire.item.linkId) #sort in original Q-item order
} else {
  message("Skipping QtransTop row sliced row binding.")
}

#join available terminology information to higher level items if present
if (!is.null(Qtrans1L)) { #check if null
  Qtrans <- Qtrans %>%
    bind_rows(Qtrans1L)%>%
    rowid_to_column(var="index")%>% #union does not work for updated versions of same rows
    group_by(Questionnaire.item.linkId)%>%
    slice(n())%>% #to select last added row within the group// if you want to keep first original existing row then use distinct(code)
    #distinct(code, .keep_all=TRUE)%>%
    ungroup(Questionnaire.item.linkId)%>%
    arrange(index)%>%
    select(-index)%>%
    arrange(Questionnaire.item.linkId) #sort in original Q-item order
} else {
  message("Skipping Qtrans1L row sliced row binding.")
}

#join available terminology information to higher level items if present
if (!is.null(Qtrans2L)) { #check if null
  Qtrans <- Qtrans %>%
    bind_rows(Qtrans2L)%>%
    rowid_to_column(var="index")%>% #union does not work for updated versions of same rows
    group_by(Questionnaire.item.linkId)%>%
    slice(n())%>% #to select last added row within the group// if you want to keep first original existing row then use distinct(code)
    #distinct(code, .keep_all=TRUE)%>%
    ungroup(Questionnaire.item.linkId)%>%
    arrange(index)%>%
    select(-index)%>%
    arrange(Questionnaire.item.linkId) #sort in original Q-item order
} else {
  message("Skipping Qtrans2L row sliced row binding.")
}

#join available VS references if present
if (!is.null(Qtrans) ) { #check if null or empty df due to filtering displays/groups
  Qflat <- Qflat %>%
    left_join(Qtrans, by = 'Questionnaire.item.linkId',suffix=c("",".drop"),copy=TRUE,relationship = 'one-to-one')%>%
    select(-contains(".drop"))
} else {
  message("Skipping Qtrans join: AnswerLists not present.")
}

#FHIR Transaction AD-API call####
trans<-read_json(urltrans) #if FHIR export url not working go to backup url using decor

#~Parse transaction for answerValueset Reference(s)####

#Extract answerValueset Reference(s) from questionnaire transaction

Qtrans<-tryCatch({
  trans|>
    tibble()%>%
    filter(row_number()==length(trans)-1)%>% # filter snapshot row <last is differential>
    unnest_longer(trans)%>%
    unnest_longer(trans)%>%
    unnest_wider(trans)%>% #all items in Q
    select(id,binding)%>% #select valueset binding column
    unnest_wider(binding)%>%
    filter(!is.na(valueSet))%>% #select rows with valueSet references + info
    mutate( Questionnaire.item.linkId=str_sub(id,end = -17),#remove date version from id for linkId
            Questionnaire.item.answervalueset.url=valueSet#sub("https://decor.nictiz.nl/fhir/","http://decor.nictiz.nl/fhir/4.0/san-gen-/",valueSet)
    )%>% #link to Santeon instead of generic for more metadata <lastUpdated>
    rename(Questionnaire.item.answervalueset.name=description)%>%
    select(Questionnaire.item.linkId,
           Questionnaire.item.answervalueset.name,
           Questionnaire.item.answervalueset.url)#reorder and final selection before join to Q
}, error = function(e) {
  message("Bindings not present", e$message)
  return(NULL)
})

#join available VS references if present
if (!is.null(Qtrans) ) { #check if null or empty df due to filtering displays/groups
  Qflat <- Qflat %>%
    left_join(Qtrans, by = 'Questionnaire.item.linkId',suffix=c("",".drop"),copy=TRUE,relationship = 'one-to-one')%>%
    select(-contains(".drop"))
} else {
  message("Skipping Qtrans join: AnswerLists not present.")
}





#~Set codesystem url####
#normally this does not change, but url can be copied from: https://decor.nictiz.nl/ad/#/san-gen-/project/project-index/codesystems 'Column FHIR > r4' 
cs<-"http://decor.nictiz.nl/fhir/4.0/san-gen-/CodeSystem/2.16.840.1.113883.2.4.3.11.60.124.5.1--20230824132917?_format=json"

#~~Test Questionnaires + transactions (with test Valuesets)####
#test1
#urlQ<-"http://decor.nictiz.nl/decor/services/RetrieveQuestionnaire?id=2.16.840.1.113883.2.4.3.11.60.909.4.32&effectiveDate=2024-01-16T13%3A35%3A30&language=nl-NL&ui=nl-NL&format=json" #metaonly + no VS
#urltrans<-"http://decor.nictiz.nl/fhir/4.0/sansa-/StructureDefinition/2.16.840.1.113883.2.4.3.11.60.909.4.32--20240116133530"

#test2
# urlQ<-"http://decor.nictiz.nl/decor/services/RetrieveQuestionnaire?id=2.16.840.1.113883.2.4.3.11.60.909.4.33&effectiveDate=2024-01-16T13%3A36%3A49&language=nl-NL&ui=nl-NL&format=json" #no nested items
# urltrans<-"http://decor.nictiz.nl/fhir/4.0/sansa-/StructureDefinition/2.16.840.1.113883.2.4.3.11.60.909.4.33--20240116133649"

#test3
# urlQ<-"http://decor.nictiz.nl/decor/services/RetrieveQuestionnaire?id=2.16.840.1.113883.2.4.3.11.60.909.4.34&effectiveDate=2024-01-16T13%3A38%3A08&language=nl-NL&ui=nl-NL&format=json" # 1level netested items
# urltrans<-"http://decor.nictiz.nl/fhir/4.0/sansa-/StructureDefinition/2.16.840.1.113883.2.4.3.11.60.909.4.34--20240116133808"

#test4
# urlQ<-"http://decor.nictiz.nl/fhir/4.0/sansa-/Questionnaire/2.16.840.1.113883.2.4.3.11.60.909.26.3--20240221134334?_format=json" #2level nested items
# urltrans<-"http://decor.nictiz.nl/fhir/4.0/sansa-/StructureDefinition/2.16.840.1.113883.2.4.3.11.60.909.4.35--20240116133934"
# urltrans2<-"http://decor.nictiz.nl/decor/services/RetrieveTransaction?id=2.16.840.1.113883.2.4.3.11.60.909.4.35&effectiveDate=2024-01-16T13%3A39%3A34&language=nl-NL&ui=nl-NL&format=json" 

#CodeSystem OUTPUT ####
#~AD-API call####
santeoncs<-read_json(cs)

#Save raw santeoncs.json
# Specify the file path where you want to save the script with chosen working directory
file_pathjson <- paste0(wd,"codesystems/R4json/",santeoncs$name,".json")

# Check if the directory exists
dir_name <- dirname(file_pathjson)
if (!dir.exists(dir_name)) {
  # Create the directory if it does not exist
  dir.create(dir_name, recursive = TRUE)
}
write_json(santeoncs,file_pathjson)

rm(file_pathjson)

#~Parse CS.json to R4csv####
cscsv<-  santeoncs|>
  tibble()%>%
  filter(row_number()==length(santeoncs))%>%
  unnest_longer(santeoncs)%>%
  unnest_wider(santeoncs)%>%
  unnest_longer(property)%>%
  unnest_longer(property)%>%
  filter(property_id=='valueCode')%>% #filter on status fields here
  select(-property_id)%>%
  unnest_longer(property)%>%
  rename(CodeSystem.concept.code=code,
         CodeSystem.concept.display=display,
         CodeSystem.concept.definition=definition,
         CodeSystem.concept.property.status=property)%>%
  mutate(CodeSystem.id=pluck(santeoncs,"id"),
         CodeSystem.url=pluck(santeoncs,"url"),
         CodeSystem.identifier=pluck(santeoncs,'identifier',1,'value'),
         CodeSystem.version=pluck(santeoncs,"version"),
         CodeSystem.name=pluck(santeoncs,"name"),
         CodeSystem.status=pluck(santeoncs,"status"),
         CodeSystem.content=pluck(santeoncs,"content"),
         CodeSystem.count=pluck(santeoncs,"count"))%>%
  filter(CodeSystem.concept.property.status!="cancelled")%>%
  select(CodeSystem.id,
         CodeSystem.url,
         CodeSystem.identifier,
         CodeSystem.version,
         CodeSystem.name,
         CodeSystem.status,
         CodeSystem.content,
         CodeSystem.count,
         CodeSystem.concept.code,
         CodeSystem.concept.display,
         CodeSystem.concept.definition,
         CodeSystem.concept.property.status)

cscsv[] <- lapply(cscsv, function(x) gsub("[;,\r\n]", "", x)) #remove removes semicolons, commas, carriage returns, and newline characters from strings

# Apply the function to the dataframe
cscsv <- trim_df(cscsv)

#Save flattend json to csv
# Specify the file path where you want to save the script with chosen working directory
file_pathR4csv <- paste0(wd,"codesystems/R4csv/",santeoncs$name,".csv")

# Check if the directory exists
dir_name <- dirname(file_pathR4csv)
if (!dir.exists(dir_name)) {
  # Create the directory if it does not exist
  dir.create(dir_name, recursive = TRUE)
}

write.csv2(cscsv,file_pathR4csv, row.names=FALSE,quote = FALSE,na='')

rm(file_pathR4csv)

#Questionnaire AD-API call####
Q<-read_json(paste0(urlQ))

#~Parse Questionnaire####
#Parsing of items into max 2 levels of nesting (because not all Qs contain nested or subnested items)

#~Parse Q metadata####

Qmeta<-tryCatch({
  Q|>
  tibble(names=names(Q))%>%
  filter(names=='date')%>%
  rename(Questionnaire.date2=Q)%>%
  mutate(Questionnaire.id=pluck(Q,"id"),
         Questionnaire.url=pluck(Q,"url"),
         Questionnaire.identifier=pluck(Q,'identifier',1,'value'),
         Questionnaire.version='v1.0', #luck(Q,"version"),
         Questionnaire.name=pluck(Q,"name"),
         Questionnaire.status=pluck(Q,"status"),
         Questionnaire.date=pluck(Q,"date"),
         Questionnaire.code.Coding.system=pluck(Q,"code",1,"system"),
         Questionnaire.code.Coding.code=pluck(Q,"code",1,"code"),
         Questionnaire.code.Coding.display=pluck(Q,"code",1,"display"),
         Questionnaire.item.linkId=NA,#add empty col so that all questionnaire csv output has same cols
         Questionnaire.item.text=NA,
         Questionnaire.item.code.Coding.system=NA,
         Questionnaire.item.code.Coding.code=NA,
         Questionnaire.item.code.Coding.display=NA,
         Questionnaire.item.type.code=NA,
         Questionnaire.item.required=NA,
         Questionnaire.item.repeats=NA,
         Questionnaire.item.answervalueset.name=NA,
         Questionnaire.item.answervalueset.url=NA)%>%
    select(Questionnaire.id,
           Questionnaire.url,
           Questionnaire.identifier,
           Questionnaire.version,
           Questionnaire.name,
           Questionnaire.status,
           Questionnaire.date,
           Questionnaire.code.Coding.system,
           Questionnaire.code.Coding.code,
           Questionnaire.code.Coding.display,
           Questionnaire.item.linkId,
           Questionnaire.item.text,
           Questionnaire.item.code.Coding.system,
           Questionnaire.item.code.Coding.code,
           Questionnaire.item.code.Coding.display,
           Questionnaire.item.type.code,
           Questionnaire.item.required,
           Questionnaire.item.repeats)
  }, error = function(e) {
    message("Error in Qmeta block", e$message)
    return(NULL)
  })

#Create base flattend output file and add additional metadata when available
Qflat<-Qmeta

#~Parse Top level (unnested) items + without associated terminology ####
#nu niet op groep gefilterd want dan krijg je een df met 0 rijen bij alleen Qmeta en dat geeft issues; de group wordt in qflat nog verwijderd
  
Qitem<- tryCatch({
  Q|>
  tibble()%>%
  filter(row_number()==length(Q))%>%
  unnest_longer(Q)%>%
  unnest_wider(Q)%>%
  # filter(type!='group')%>% #filter top level items here; gives df with 0rows when meta only, fixed with sliced binding
    mutate(Questionnaire.id=pluck(Q,"id"),
           Questionnaire.url=pluck(Q,"url"),
           Questionnaire.identifier=pluck(Q,'identifier',1,'value'),
           Questionnaire.version=pluck(Q,"version"),
           Questionnaire.name=pluck(Q,"name"),
           Questionnaire.status=pluck(Q,"status"),
           Questionnaire.date=pluck(Q,"date"),
           Questionnaire.code.Coding.system=pluck(Q,"code",1,"system"),
           Questionnaire.code.Coding.code=pluck(Q,"code",1,"code"),
           Questionnaire.code.Coding.display=pluck(Q,"code",1,"display"),
           Questionnaire.item.code.Coding.system=NA,#add empty col so that all questionnaire csv output has same cols
           Questionnaire.item.code.Coding.code=NA,
           Questionnaire.item.code.Coding.display=NA,
           Questionnaire.item.answervalueset.name=NA,
           Questionnaire.item.answervalueset.url=NA)%>%
    rename(Questionnaire.item.linkId=linkId,
           Questionnaire.item.text=text,
           Questionnaire.item.type.code=type,
           Questionnaire.item.required=required,
           Questionnaire.item.repeats=repeats) %>%
    select(Questionnaire.id,
           Questionnaire.url,
           Questionnaire.identifier,
           Questionnaire.version,
           Questionnaire.name,
           Questionnaire.status,
           Questionnaire.date,
           Questionnaire.code.Coding.system,
           Questionnaire.code.Coding.code,
           Questionnaire.code.Coding.display,
           Questionnaire.item.linkId,
           Questionnaire.item.text,
           Questionnaire.item.code.Coding.system,
           Questionnaire.item.code.Coding.code,
           Questionnaire.item.code.Coding.display,
           Questionnaire.item.type.code,
           Questionnaire.item.required,
           Questionnaire.item.repeats)
  }, error = function(e) {
    message("Error in Qitem block", e$message)
    return(NULL)
  })

#~~bind item cols with Qmeta####
#join available terminology information to top level items if present
  if (!is.null(Qitem)) {
    Qflat <- Qflat %>%
      dplyr::union(Qitem)%>%
      rowid_to_column(var="index")%>% #union does not work for updated versions next code is to clean up
      group_by(Questionnaire.item.linkId)%>%
      slice(n())%>% #to select last added row // if you want to keep first original existing row then use distinct(code)
      #distinct(code, .keep_all=TRUE)%>%
      ungroup(Questionnaire.item.linkId)%>%
      arrange(index)%>%
      select(-index)%>%
      arrange(Questionnaire.item.linkId) #sort in original Q-item order
    } else {
      message("Skipping Qitem col binding.")
    }

#~Parse Top level (unnested) items + with associated terminology #####

QitemTerm<-tryCatch({
  Q|>
  tibble()%>%
  filter(row_number()==length(Q))%>%
  unnest_longer(Q)%>%
  unnest_wider(Q)%>%
  # filter(type!='group')%>% #filter top level items here // can result in unwanted 0-row dfs
  unnest_longer(code)%>%
  unnest_wider(code)%>% 
    mutate(Questionnaire.id=pluck(Q,"id"),
           Questionnaire.url=pluck(Q,"url"),
           Questionnaire.identifier=pluck(Q,'identifier',1,'value'),
           Questionnaire.version=pluck(Q,"version"),
           Questionnaire.name=pluck(Q,"name"),
           Questionnaire.status=pluck(Q,"status"),
           Questionnaire.code.Coding.system=pluck(Q,"code",1,"system"),
           Questionnaire.code.Coding.code=pluck(Q,"code",1,"code"),
           Questionnaire.code.Coding.display=pluck(Q,"code",1,"display"))%>%
    rename(Questionnaire.item.linkId=linkId,
           Questionnaire.item.text=text,
           Questionnaire.item.code.Coding.system=system,
           Questionnaire.item.code.Coding.code=code,
           Questionnaire.item.code.Coding.display=display,
           Questionnaire.item.type.code=type,
           Questionnaire.item.required=required,
           Questionnaire.item.repeats=repeats) %>%
    select(Questionnaire.id,
           Questionnaire.url,
           Questionnaire.identifier,
           Questionnaire.version,
           Questionnaire.name,
           Questionnaire.status,
           Questionnaire.date,
           Questionnaire.code.Coding.system,
           Questionnaire.code.Coding.code,
           Questionnaire.code.Coding.display,
           Questionnaire.item.linkId,
           Questionnaire.item.text,
           Questionnaire.item.code.Coding.system,
           Questionnaire.item.code.Coding.code,
           Questionnaire.item.code.Coding.display,
           Questionnaire.item.type.code,
           Questionnaire.item.required,
           Questionnaire.item.repeats)
  }, error = function(e) {
    message("Error in QitemTerm block", e$message)
    return(NULL)
  })

#~~join available terminology information to top level items if present####
  if (!is.null(QitemTerm)) {
    Qflat <- Qflat %>%
      dplyr::union(QitemTerm)%>%
      rowid_to_column(var="index")%>% #union does not work for updated versions next code is to clean up
      group_by(Questionnaire.item.linkId)%>%
      slice(n())%>% #to select last added row // if you want to keep first original existing row then use distinct(code)
      #distinct(code, .keep_all=TRUE)%>%
      ungroup(Questionnaire.item.linkId)%>%
      arrange(index)%>%
      select(-index)%>%
      arrange(Questionnaire.item.linkId) #sort in original Q-item order
    } else {
      message("Skipping QitemTerm union.")
    }

#~Parse 1st level (nested) items without associated terminology ####
   
Q1Litem<- tryCatch({
    Q|>
    tibble()%>%
    filter(row_number()==length(Q))%>%
    unnest_longer(Q)%>%
    unnest_wider(Q)%>%
    filter(type=='group')%>% #filter 1st level items here
    filter(text!='MetaData')%>% #filter out Q_MetaData group
    unnest_longer(item)%>%
    select(item)%>%
    unnest_wider(item)%>% 
    #filter(type!='display')%>% #filter out items without terminology // can result in unwanted 0-row dfs
    #filter(type!='group')%>% #filter out items without terminology // can result in unwanted 0-row dfs
    rename(Questionnaire.item.linkId=linkId,
           Questionnaire.item.text=text,
           Questionnaire.item.type.code=type,
           Questionnaire.item.required=required,
           Questionnaire.item.repeats=repeats) %>%
    mutate(Questionnaire.id=pluck(Q,"id"),
           Questionnaire.url=pluck(Q,"url"),
           Questionnaire.identifier=pluck(Q,'identifier',1,'value'),
           Questionnaire.version=pluck(Q,"version"),
           Questionnaire.name=pluck(Q,"name"),
           Questionnaire.status=pluck(Q,"status"),
           Questionnaire.date=pluck(Q,"date"),
           Questionnaire.code.Coding.system=pluck(Q,"code",1,"system"),
           Questionnaire.code.Coding.code=pluck(Q,"code",1,"code"),
           Questionnaire.code.Coding.display=pluck(Q,"code",1,"display"),
           Questionnaire.item.code.Coding.system=NA,#add empty col so that all questionnaire csv output has same cols
           Questionnaire.item.code.Coding.code=NA,
           Questionnaire.item.code.Coding.display=NA,
           Questionnaire.item.answervalueset.name=NA,
           Questionnaire.item.answervalueset.url=NA) %>%  
    select(Questionnaire.id,
           Questionnaire.url,
           Questionnaire.identifier,
           Questionnaire.version,
           Questionnaire.name,
           Questionnaire.status,
           Questionnaire.date,
           Questionnaire.code.Coding.system,
           Questionnaire.code.Coding.code,
           Questionnaire.code.Coding.display,
           Questionnaire.item.linkId,
           Questionnaire.item.text,
           Questionnaire.item.code.Coding.system,
           Questionnaire.item.code.Coding.code,
           Questionnaire.item.code.Coding.display,
           Questionnaire.item.type.code,
           Questionnaire.item.required,
           Questionnaire.item.repeats)
  }, error = function(e) {
    message("Error in Q1Litem block", e$message)
    return(NULL)
  })  

#~~bind 1Litem rows with Qflat####
#join available  information to higher level items if present
  if (!is.null(Q1Litem)) {
    Qflat <- Qflat %>%
      bind_rows(Q1Litem)%>%
      rowid_to_column(var="index")%>% #union does not work for updated versions of same rows
      group_by(Questionnaire.item.linkId)%>%
      slice(n())%>% #to select last added row within the group// if you want to keep first original existing row then use distinct(code)
      #distinct(code, .keep_all=TRUE)%>%
      ungroup(Questionnaire.item.linkId)%>%
      arrange(index)%>%
      select(-index)%>%
      arrange(Questionnaire.item.linkId) #sort in original Q-item order
    } else {
      message("Skipping Q1Litem row sliced row binding.")
    }

#~Parse 1st level (nested) items with associated terminology####
 
Q1Litemterm<-  tryCatch({
  Q|>
  tibble()%>%
  filter(row_number()==length(Q))%>%
  unnest_longer(Q)%>%
  unnest_wider(Q)%>%
  filter(type=='group')%>% #filter 1st level items here
  filter(text!='MetaData')%>% #filter out Q_MetaData group
  unnest_longer(item)%>%
  select(item)%>%
  unnest_wider(item)%>% 
  # filter(type!='display')%>% #filter out items without terminology // can result in unwanted 0-row dfs
  # filter(type!='group')%>% #filter out items without terminology // can result in unwanted 0-row dfs
  unnest_longer(code)%>%#filters out items without terminology
  unnest_wider(code)%>% 
  rename(Questionnaire.item.linkId=linkId,
         Questionnaire.item.text=text,
         Questionnaire.item.code.Coding.system=system,
         Questionnaire.item.code.Coding.code=code,
         Questionnaire.item.code.Coding.display=display,
         Questionnaire.item.type.code=type,
         Questionnaire.item.required=required,
         Questionnaire.item.repeats=repeats) %>%
    mutate(Questionnaire.id=pluck(Q,"id"),
           Questionnaire.url=pluck(Q,"url"),
           Questionnaire.identifier=pluck(Q,'identifier',1,'value'),
           Questionnaire.version=pluck(Q,"version"),
           Questionnaire.name=pluck(Q,"name"),
           Questionnaire.status=pluck(Q,"status"),
           Questionnaire.date=pluck(Q,"date"),
           Questionnaire.code.Coding.system=pluck(Q,"code",1,"system"),
           Questionnaire.code.Coding.code=pluck(Q,"code",1,"code"),
           Questionnaire.code.Coding.display=pluck(Q,"code",1,"display"))%>%  
    select(Questionnaire.id,
           Questionnaire.url,
           Questionnaire.identifier,
           Questionnaire.version,
           Questionnaire.name,
           Questionnaire.status,
           Questionnaire.date,
           Questionnaire.code.Coding.system,
           Questionnaire.code.Coding.code,
           Questionnaire.code.Coding.display,
           Questionnaire.item.linkId,
           Questionnaire.item.text,
           Questionnaire.item.code.Coding.system,
           Questionnaire.item.code.Coding.code,
           Questionnaire.item.code.Coding.display,
           Questionnaire.item.type.code,
           Questionnaire.item.required,
           Questionnaire.item.repeats)
  }, error = function(e) {
    message("Error in Q1Litemterm block", e$message)
    return(NULL)
  })  

#~~bind item rows with Qflat####
#join available terminology information to higher level items if present
  if (!is.null(Q1Litemterm)) {
    Qflat <- Qflat %>%
      bind_rows(Q1Litemterm)%>%
      rowid_to_column(var="index")%>% #union does not work for updated versions of same rows
      group_by(Questionnaire.item.linkId)%>%
      slice(n())%>% #to select last added row within the group// if you want to keep first original existing row then use distinct(code)
      #distinct(code, .keep_all=TRUE)%>%
      ungroup(Questionnaire.item.linkId)%>%
      arrange(index)%>%
      select(-index)%>%
      arrange(Questionnaire.item.linkId) #sort in original Q-item order
    } else {
      message("Skipping Q1Litemterm row sliced row binding.")
    }
  
#~Parse 2nd level (subnested) items without associated terminology ####

Q2Litem<-tryCatch({
  Q|>
    tibble()%>%
    filter(row_number()==length(Q))%>%
    unnest_longer(Q)%>%
    unnest_wider(Q)%>%
    filter(type=='group')%>% #filter 1st level items here
    filter(text!='MetaData')%>% #filter out Q_MetaData group
    unnest_longer(item)%>%
    select(item)%>%
    unnest_wider(item)%>%
    filter(type=='group')%>% #filter 1st level items here
    unnest_longer(item)%>%
    select(item)%>%
    unnest_wider(item)%>%
   # filter(type!='display')%>% #filter out items without terminology // can result in unwanted 0-row dfs
   # filter(type!='group')%>% #filter out items without terminology // can result in unwanted 0-row dfs
  rename(Questionnaire.item.linkId=linkId,
         Questionnaire.item.text=text,
         Questionnaire.item.type.code=type,
         Questionnaire.item.required=required,
         Questionnaire.item.repeats=repeats) %>%
    mutate(Questionnaire.id=pluck(Q,"id"),
           Questionnaire.url=pluck(Q,"url"),
           Questionnaire.identifier=pluck(Q,'identifier',1,'value'),
           Questionnaire.version=pluck(Q,"version"),
           Questionnaire.name=pluck(Q,"name"),
           Questionnaire.status=pluck(Q,"status"),
           Questionnaire.date=pluck(Q,"date"),
           Questionnaire.code.Coding.system=pluck(Q,"code",1,"system"),
           Questionnaire.code.Coding.code=pluck(Q,"code",1,"code"),
           Questionnaire.code.Coding.display=pluck(Q,"code",1,"display"),
           Questionnaire.item.code.Coding.system=NA,#add empty col so that all questionnaire csv output has same cols
           Questionnaire.item.code.Coding.code=NA,
           Questionnaire.item.code.Coding.display=NA,
           Questionnaire.item.answervalueset.name=NA,
           Questionnaire.item.answervalueset.url=NA)%>%
    select(Questionnaire.id,
           Questionnaire.url,
           Questionnaire.identifier,
           Questionnaire.version,
           Questionnaire.name,
           Questionnaire.status,
           Questionnaire.date,
           Questionnaire.code.Coding.system,
           Questionnaire.code.Coding.code,
           Questionnaire.code.Coding.display,
           Questionnaire.item.linkId,
           Questionnaire.item.text,
           Questionnaire.item.code.Coding.system,
           Questionnaire.item.code.Coding.code,
           Questionnaire.item.code.Coding.display,
           Questionnaire.item.type.code,
           Questionnaire.item.required,
           Questionnaire.item.repeats)
  }, error = function(e) {
    message("Error in Q2Litem block", e$message)
    return(NULL)
  })

#~~bind 2Litem rows with Qflat####
#join available information to higher level items if present
if (!is.null(Q2Litem)) { #check if null 
  Qflat <- Qflat %>%
    bind_rows(Q2Litem)%>%
    rowid_to_column(var="index")%>% #union does not work for updated versions of same rows
    group_by(Questionnaire.item.linkId)%>%
    slice(n())%>% #to select last added row within the group// if you want to keep first original existing row then use distinct(code)
    #distinct(code, .keep_all=TRUE)%>%
    ungroup(Questionnaire.item.linkId)%>%
    arrange(index)%>%
    select(-index)%>%
    arrange(Questionnaire.item.linkId) #sort in original Q-item order
} else {
  message("Skipping Q2Litem row sliced row binding.")
}

#~Parse 2nd level (subnested) items with associated terminology ####

Q2Litemterm<-tryCatch({
  Q|>
    tibble()%>%
    filter(row_number()==length(Q))%>%
    unnest_longer(Q)%>%
    unnest_wider(Q)%>%
    filter(type=='group')%>% #filter 1st level items here
    filter(text!='MetaData')%>% #filter out Q_MetaData group
    unnest_longer(item)%>%
    select(item)%>%
    unnest_wider(item)%>%
    filter(type=='group')%>% #filter 1st level items here
    unnest_longer(item)%>%
    select(item)%>%
    unnest_wider(item)%>%
    #filter(type!='display')%>% #filter out items without terminology // can result in unwanted 0-row dfs
    #filter(type!='group')%>% #filter out items without terminology // can result in unwanted 0-row dfs
    unnest_longer(code)%>% #filters out items without terminology
    unnest_wider(code)%>% 
    rename(Questionnaire.item.linkId=linkId,
           Questionnaire.item.text=text,
           Questionnaire.item.code.Coding.system=system,
           Questionnaire.item.code.Coding.code=code,
           Questionnaire.item.code.Coding.display=display,
           Questionnaire.item.type.code=type,
           Questionnaire.item.required=required,
           Questionnaire.item.repeats=repeats) %>%
    mutate(Questionnaire.id=pluck(Q,"id"),
           Questionnaire.url=pluck(Q,"url"),
           Questionnaire.identifier=pluck(Q,'identifier',1,'value'),
           Questionnaire.version=pluck(Q,"version"),
           Questionnaire.name=pluck(Q,"name"),
           Questionnaire.status=pluck(Q,"status"),
           Questionnaire.date=pluck(Q,"date"),
           Questionnaire.code.Coding.system=pluck(Q,"code",1,"system"),
           Questionnaire.code.Coding.code=pluck(Q,"code",1,"code"),
           Questionnaire.code.Coding.display=pluck(Q,"code",1,"display"))%>%
    select(Questionnaire.id,
           Questionnaire.url,
           Questionnaire.identifier,
           Questionnaire.version,
           Questionnaire.name,
           Questionnaire.status,
           Questionnaire.date,
           Questionnaire.code.Coding.system,
           Questionnaire.code.Coding.code,
           Questionnaire.code.Coding.display,
           Questionnaire.item.linkId,
           Questionnaire.item.text,
           Questionnaire.item.code.Coding.system,
           Questionnaire.item.code.Coding.code,
           Questionnaire.item.code.Coding.display,
           Questionnaire.item.type.code,
           Questionnaire.item.required,
           Questionnaire.item.repeats)
}, error = function(e) {
  message("Error in Q2Litemterm block", e$message)
})

#~~bind Q2Litemtetm rows with Qflat####
#join available terminology information to higher level items if present
if (!is.null(Q2Litemterm)) { #check if null
  Qflat <- Qflat %>%
    bind_rows(Q2Litemterm)%>%
    rowid_to_column(var="index")%>% #union does not work for updated versions of same rows
    group_by(Questionnaire.item.linkId)%>%
    slice(n())%>% #to select last added row within the group// if you want to keep first original existing row then use distinct(code)
    #distinct(code, .keep_all=TRUE)%>%
    ungroup(Questionnaire.item.linkId)%>%
    arrange(index)%>%
    select(-index)%>%
    arrange(Questionnaire.item.linkId) #sort in original Q-item order
} else {
  message("Skipping Q2Litemterm row sliced row binding.")
}  

#~~Remove unwanted rows####
  Qflat<- Qflat%>%
    filter(!is.na(Questionnaire.item.type.code))%>% #filter out rows with empty item.type
      filter(Questionnaire.item.type.code!='display'| is.na(Questionnaire.item.type.code))%>% #filter out rows of display type
        filter(Questionnaire.item.type.code!='group'| is.na(Questionnaire.item.type.code)) #filter out rows of group type



#Questionnaire OUTPUT ####
#Save raw Q.json
# Specify the file path where you want to save the script with chosen working directory
file_pathjson <- paste0(wd,"questionnaires/R4json/",Q$name,".json")

# Check if the directory exists
  dir_name <- dirname(file_pathjson)
  if (!dir.exists(dir_name)) {
    # Create the directory if it does not exist
    dir.create(dir_name, recursive = TRUE)
  }
  write_json(Q,file_pathjson)

#Save flattend json to csv
# Specify the file path where you want to save the script with chosen working directory
  file_pathR4csv <- paste0(wd,"questionnaires/R4csv/",Q$name,".csv")

  # Check if the directory exists
    dir_name <- dirname(file_pathR4csv)
    if (!dir.exists(dir_name)) {
      # Create the directory if it does not exist
      dir.create(dir_name, recursive = TRUE)
    }

    Qflat[] <- lapply(Qflat, function(x) gsub("[;,\r\n]", "", x)) #remove removes semicolons, commas, carriage returns, and newline characters from strings

    # Apply the function to the dataframe
    Qflat <- trim_df(Qflat)
    
  write.csv2(Qflat,file_pathR4csv, row.names=FALSE,quote = FALSE,na='')

#Save and update local versions of Santeon_VragenlijstCodelijst & Santeon_VragenCodelijst & informatiemodel Santeon valueSets relation 
  #Import Santeon_VragenCodelijst
    library(readr)
    valueset_Santeon_VragenLijstCodelijst <- read_delim("valuesets/valueset_Santeon_VragenLijstCodelijst.csv",
                                                        delim = ";", escape_double = FALSE, col_types = cols(version = col_character()),
                                                        trim_ws = TRUE)
    
    QmetaTransf<-Qmeta|>
      mutate(valueset_name="Santeon_VragenLijstCodelijst",
             Questionnaire.version=as.character(Questionnaire.version))%>%
      select(valueset_name,
             Questionnaire.code.Coding.system,
             Questionnaire.code.Coding.code,
             Questionnaire.code.Coding.display,
             Questionnaire.name,
             Questionnaire.version)%>%
      rename(system=Questionnaire.code.Coding.system,
             code=Questionnaire.code.Coding.code,
             display=Questionnaire.code.Coding.display,
             definition=Questionnaire.name,
             version=Questionnaire.version)
    
    #add new Qmeta to list
    if (!is.null(QmetaTransf)) { #check if null
    valueset_Santeon_VragenLijstCodelijst<-valueset_Santeon_VragenLijstCodelijst%>%
      dplyr::union(QmetaTransf)%>% #union does not work for updated versions next code is to clean up
      #rowid_to_column(var="index")%>% #use index to keep original file order and add updated rows to bottom
      group_by(code)%>%
      slice(n())%>% #to select last added row // if you want to keep first original existing row then use distinct(code)
      #distinct(code, .keep_all=TRUE)%>%
      ungroup(code)%>%
      #arrange(index)%>%
      #select(-index)
      arrange(tolower(display)) #sort file by questionnaire display (case-insensitive)
    } else {
      message("Skipping QmetaTransf row sliced row binding.")
    }
    
    #Save updated local files & remove object in R
    write.csv2(valueset_Santeon_VragenLijstCodelijst,"valuesets/valueset_Santeon_VragenLijstCodelijst.csv", row.names=FALSE,quote =FALSE,na='')
    rm(valueset_Santeon_VragenLijstCodelijst)

#import valueset_Santeon_VragenCodelijst.csv
  library(readr)
  valueset_Santeon_VragenCodelijst <- read_delim("valuesets/valueset_Santeon_VragenCodelijst.csv",
                                                 delim = ";", show_col_types= FALSE,escape_double = FALSE, col_types = cols(version = col_character()),
                                                 trim_ws = TRUE)
  
  if ("Questionnaire.item.code.Coding.code" %in% colnames(Qflat)) {    #to check if there are items with answervaluesets
    
      #rename Qflat variables to align with Santeon_VragenCodelijst
      QflatTransf<-Qflat|>
        filter(!is.na(Questionnaire.item.code.Coding.code))%>% #filter out any rows without terminology codes
        mutate(valueset_name="Santeon_VragenCodelijst",
               Questionnaire.version=as.character(Questionnaire.version))%>%
        select(valueset_name,
               Questionnaire.item.code.Coding.system,
               Questionnaire.item.code.Coding.code,
               Questionnaire.item.code.Coding.display,
               Questionnaire.item.text,
               Questionnaire.version)%>%
        rename(system=Questionnaire.item.code.Coding.system,
               code=Questionnaire.item.code.Coding.code,
               display=Questionnaire.item.code.Coding.display,
               definition=Questionnaire.item.text,
               version=Questionnaire.version) #FHIR items do not have a version, so substituted Questionnaire.version here
      
      #add Qflat-items to list
      #Bind cols Ans+Ordinal values
      if (!is.null(QflatTransf)) { #check if null
       valueset_Santeon_VragenCodelijst<-valueset_Santeon_VragenCodelijst%>%
        dplyr::union(QflatTransf)%>% #union does not work for updated versions next code is to clean up
        rowid_to_column(var="index")%>% #use index to keep original file order and add updated rows to bottom
        group_by(code)%>%
        slice(n())%>% #to select last added row // if you want to keep first existing row then use distinct(code)
        #distinct(code, .keep_all=TRUE)%>%
        ungroup(code)%>%
        arrange(index)%>%
        select(-index)
      } else {
        message("Skipping QflatTransf row sliced row binding.")
      }
      
      #Save back to local & remove object in R
      write.csv2(valueset_Santeon_VragenCodelijst,"valuesets/valueset_Santeon_VragenCodelijst.csv", row.names=FALSE,quote =FALSE,na='')
      #rm(valueset_Santeon_VragenCodelijst)
  
  } else {
    message("Skipping update Santeon_VragenCodelijst: no items present in Questionnaire.")
  } 

  #Import informatiemodel Santeon valueSets relation
  library(readr)
  relation <- read_delim("SIM/informatiemodel Santeon valueSets relation.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  if ("Questionnaire.item.answervalueset.url" %in% colnames(Qflat)) {    #to check if there are items with terminology codes
    
    #rename Qflat variables to align with informatiemodel Santeon valueSets relation
    #add valueset references
    QflatTransfRelation<-Qflat|>
      filter(!is.na(Questionnaire.item.answervalueset.url))%>% #filter out any rows without answervalueset
      mutate(model="VragenlijstAntwoord",
             field="AntwoordCode",
             parent_valueset_name="Santeon_VragenCodelijst")%>%
      rename(valueset_name=Questionnaire.item.answervalueset.name,
             parent_system=Questionnaire.item.code.Coding.system,
             parent_code=Questionnaire.item.code.Coding.code) %>% #FHIR items do not have a version, so substituted Questionnaire.version here
     select(model,
            field,
            valueset_name,
            parent_valueset_name,
            parent_system,
            parent_code)
    
    #add  QflatTransfRelation-items to list
        if (!is.null(QflatTransfRelation)) { #check if null
      relation<-relation%>%
        dplyr::union(QflatTransfRelation)%>% #union does not work for updated versions next code is to clean up
        #rowid_to_column(var="index")%>% #use index to keep original file order and add updated rows to bottom
        group_by(model,field,valueset_name,parent_code)%>%
        slice(n())%>% #to select last added row // if you want to keep first existing row then use distinct(code)
        #distinct(code, .keep_all=TRUE)%>%
        ungroup(model,field,valueset_name,parent_code)%>%
        #arrange(index)%>%
        #select(-index)%>%
        arrange(tolower(paste(model,field,valueset_name,parent_code)))
    } else {
      message("Skipping relation row sliced row binding.")
    }
    
    #Save back to local & remove object in R
    write.csv2(relation,"SIM/informatiemodel Santeon valueSets relation.csv", row.names=FALSE,quote =FALSE,na='')
    #rm(relation)
    
  } else {
    message("Skipping update informatiemodel Santeon valueSets relation: no answervaluesets present")
  } 

#Loop any valuesets bound in Questionnaire + save as valueSet resource####
# Collection of VS metadata is split from collection of code-metadata within a VS, because not all codes could be present in VS due to license restrictions

# Loop over all distinct records in Qtrans (if present) to extract VSmetadata
if (!is.null(Qtrans)) {
  Qtransdist<-Qtrans%>%
    distinct(Questionnaire.item.answervalueset.url, .keep_all = TRUE)

    for (i in 1:nrow(Qtransdist)) {
                  # Make an HTTP GET request to the URL
                  url <- paste0(Qtransdist$Questionnaire.item.answervalueset.url[i],"?_format=json")
                  
                  # Read JSON from the URL
                  R4AL <- read_json(url)
                  
                  #Save raw VS.json
                  filenameVSjson<-paste0(wd,"valuesets/R4json/", R4AL$name,".json")
                  
                  # Check if the directory exists
                  dir_name <- dirname(filenameVSjson)
                  if (!dir.exists(dir_name)) {
                    # Create the directory if it does not exist
                    dir.create(dir_name, recursive = TRUE)
                    }
                  
                  write_json(R4AL,filenameVSjson)
                  
                  #4-step unnesting because not all codes may have designations, ordinal values (extenstion) or description values (extension)
                  
                  #select codes and displays
                  codedisplay<-R4AL|>
                    tibble()%>%
                    filter(row_number()==length(R4AL))%>%
                    unnest_longer(1)%>%
                    select(R4AL)%>%
                    unnest_longer(R4AL)%>%
                    unnest_wider(R4AL)%>%
                    unnest_longer(concept)%>%
                    unnest_wider(concept)%>%
                    select(system,code,display)%>%
                    mutate(id=pluck(R4AL,'id'),
                           url=pluck(R4AL,'url'),
                           identifier=pluck(R4AL,'identifier',1,'value'),
                           version=pluck(R4AL,'version'),
                           name=pluck(R4AL,"name"),
                           status=pluck(R4AL,'status'),
                           date=pluck(R4AL,'meta','lastUpdated'),
                           designationValue=NA, #add empty columns in case VS does not have these, will be dropped using suffix later on if they do
                           ordinalValue=NA, #add empty columns in case VS does not have these, will be dropped using suffix later on if they do
                           descriptionValue=NA)%>% #add empty columns in case VS does not have these, will be dropped using suffix later on if they do
                    select(id, 
                           url, 
                           identifier, 
                           version, 
                           name, 
                           status,
                           system, 
                           code, 
                           display,
                           designationValue,
                           ordinalValue)
                  
                  #assign codedisplay to final in case following join fails when ordinals are not present
                  final<-codedisplay  
                  
                  #select designation(s) if present
                  
                  designation <- tryCatch({
                    R4AL %>%
                      tibble() %>%
                      filter(row_number() == length(R4AL)) %>%
                      unnest_longer(1) %>%
                      select(1) %>%
                      unnest_longer(1) %>%
                      unnest_wider(1) %>%
                      select(concept) %>%
                      unnest_longer(concept) %>%
                      unnest_wider(concept) %>%
                      select(code, designation) %>%
                      unnest_longer(designation) %>%
                      unnest_wider(designation) %>%
                      rename(designationValue = value) %>%
                      select(code, designationValue)
                  }, error = function(e) {
                    message("Error in designation block: ", e$message)
                    return(NULL)
                  })
                  
                  # Add designation(s) if present
                  if (!is.null(designation)) {
                    final <- final %>%
                      left_join(designation, by = 'code',suffix=c(".drop",""))
                  } else {
                    message("Skipping designation block: designation not present.")
                  }
                  
                  #select ordinal score if present
                  
                  ordinal<- tryCatch({
                    R4AL%>%
                      tibble()%>%
                      filter(row_number()==length(R4AL))%>%
                      unnest_longer(1)%>%
                      select(1)%>%
                      unnest_longer(1)%>%
                      unnest_wider(1)%>%
                      select(concept)%>%
                      unnest_longer(concept)%>%
                      unnest_wider(concept)%>%
                      select(code,extension)%>%
                      unnest_longer(extension)%>%
                      unnest_wider(extension)%>%
                      filter(!is.na(valueDecimal))%>%
                      rename(ordinalValue=valueDecimal)%>%
                      select(code,ordinalValue)
                  }, error = function(e) {
                    message("Error in ordinal block: ", e$message)
                    return(NULL)
                  })
                  
                  # Add ordinal if present
                  if (!is.null(ordinal)) {
                    final <- final %>%
                      left_join(ordinal, by = 'code', suffix=c(".drop",""))
                  } else {
                    message("Skipping ordinal block: ordinal not present")
                  }
                  
                  #select description if present
                  
                  description<- tryCatch({
                    R4AL%>%
                      tibble()%>%
                      filter(row_number()==length(R4AL))%>%
                      unnest_longer(1)%>%
                      select(1)%>%
                      unnest_longer(1)%>%
                      unnest_wider(1)%>%
                      select(concept)%>%
                      unnest_longer(concept)%>%
                      unnest_wider(concept)%>%
                      select(code,extension)%>%
                      unnest_longer(extension)%>%
                      unnest_wider(extension)%>%
                      filter(valueString!='')%>%
                      rename(descriptionValue=valueString)%>%
                      select(code,descriptionValue)
                  }, error = function(e) {
                    message("Error in ordinal block: ", e$message)
                    return(NULL)
                  })
                  
                  # Add description if present
                  if (!is.null(description)) {
                    final <- final %>%
                      left_join(description, by = 'code', suffix=c(".drop",""))
                  } else {
                    message("Skipping ordinal block: description not present")
                  }
                  
                  #drop unused columns if designation and/or ordinals and/or descriptions are present
                  final<-final%>%
                    rename(ValueSet.id=id,
                           ValueSet.url=url, 
                           ValueSet.identifier.value=identifier, 
                           ValueSet.version=version, 
                           ValueSet.name=name,
                           ValueSet.status=status,
                           ValueSet.compose.include.system=system, 
                           ValueSet.compose.include.concept.code=code, 
                           ValueSet.compose.include.concept.display=display,
                           ValueSet.compose.include.concept.designation.designationValue=designationValue,
                           ValueSet.compose.include.concept.extension.ordinalValue=ordinalValue,
                           ValueSet.compose.include.concept.extension.descriptionValue=descriptionValue)%>%
                    select(ValueSet.id,
                           ValueSet.url, 
                           ValueSet.identifier.value, 
                           ValueSet.version, 
                           ValueSet.name,
                           ValueSet.status,
                           ValueSet.compose.include.system, 
                           ValueSet.compose.include.concept.code, 
                           ValueSet.compose.include.concept.display,
                           ValueSet.compose.include.concept.designation.designationValue,
                           ValueSet.compose.include.concept.extension.ordinalValue,
                           ValueSet.compose.include.concept.extension.descriptionValue)
                  
                  #~Valueset Output####
                  filename<-paste0("valuesets/R4csv/",final$ValueSet.name[1],".csv")
                  
                  # Check if the directory exists
                  dir_name <- dirname(filename)
                  if (!dir.exists(dir_name)) {
                    # Create the directory if it does not exist
                    dir.create(dir_name, recursive = TRUE)
                  }
                  #Save sets to csv with R4 names
                  write.csv2(final,filename, row.names=FALSE,quote = FALSE,na='')
                  
                  #format to current (old) valueset_.csv style and naming convention
                  
                  finalold<-final%>%
                    rename(valueset_name=ValueSet.name,
                           system=ValueSet.compose.include.system,
                           code=ValueSet.compose.include.concept.code,
                           display=ValueSet.compose.include.concept.display,
                           definition= ValueSet.compose.include.concept.extension.descriptionValue,
                           version=ValueSet.version,
                           ordinalValue=ValueSet.compose.include.concept.extension.ordinalValue)%>%
                    select(valueset_name,
                           system,
                           code,
                           display,
                           definition,
                           version,
                           ordinalValue)
                  
                  filenameold<-paste0("valuesets/valueset_",finalold$valueset_name[1],".csv")
                  
                  # Check if the directory exists
                  dir_name <- dirname(filenameold)
                  if (!dir.exists(dir_name)) {
                    # Create the directory if it does not exist
                    dir.create(dir_name, recursive = TRUE)
                  }
                  #Save sets to csv
                  write.csv2(finalold,filenameold, row.names=FALSE,quote = FALSE,na='')
                  
                  #Save and update local version of informatiemodel Santeon valueSets metadata 
                  #Import informatiemodel Santeon valueSets metadata
                  library(readr)
                  meta <- read_delim("SIM/informatiemodel Santeon valueSets metadata.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
                  
                  #rename meta variables to align with informatiemodel Santeon valueSets metadata
                  finalTransf<-final%>%
                    mutate(verification_method='local',
                           notes=NA)%>%
                    rename(valueset_name=ValueSet.name,
                           url=ValueSet.url,
                           system_default=ValueSet.compose.include.system)%>%
                    select(valueset_name,
                           url,
                           verification_method,
                           system_default,
                           notes)
                  
                  #add VS-meta
                  if (!is.null(finalTransf)) { #check if null
                    meta<-meta%>%
                      dplyr::union(finalTransf)%>% #union does not work for updated versions next code is to clean up
                      rowid_to_column(var="index")%>% #use index to keep original file order and add updated rows to bottom
                      group_by(valueset_name)%>%
                      slice(n())%>% #to select last added row // if you want to keep first existing row then use distinct(code)
                      #distinct(code, .keep_all=TRUE)%>%
                      ungroup(valueset_name)%>%
                      arrange(index)%>%
                      select(-index)%>%
                      arrange(tolower(valueset_name)) #arrange is case-sensitive
                  } else {
                    message("Skipping QflatTransf row sliced row binding.")
                  }
                  
                  #Save back to local & remove object in R
                  write.csv2(meta,"SIM/informatiemodel Santeon valueSets metadata.csv", row.names=FALSE,quote =FALSE,na='')
                  #rm(meta)
                   }
    } else {
      message("Skipping Qtrans extraction loop: AnswerLists not present.")
    }     
      
#~Save this Script instance in Output folder####
# Make sure to Save this instance of QuestionnaireParse_New before running, otherwise old/previous version is saved in output folder.

# Write the entire script to the file
script_content<-readLines(paste0(wd,"questionnaires/scripts/QuestionnaireParse_New.R"))

# Specify the file path where you want to save the script with chosen working directory
file_path <- paste0(wd,"questionnaires/scripts/Q_parse_",Q$name, ".R")

# Check if the directory exists
  dir_name <- dirname(file_path)
  if (!dir.exists(dir_name)) {
    # Create the directory if it does not exist
    dir.create(dir_name, recursive = TRUE)
  }

# Write the script content to the file
writeLines(c(paste0("#",Q$name),rundate,script_content), file_path)

# Print a message indicating where the script is saved
cat("R script saved to:", file_path, "\n")
