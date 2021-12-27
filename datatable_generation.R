#############################################################
##
## Datables generation functions 
##
############################################################


#install.packages("tibble")
#update.packages("ellipsis")
library(tibble)
library(dplyr)
library(DT)
pacman::p_load(DT, data.table)

## Returns population data
get_pop_data <- function(){
  
  data=tibble()
  
  #read first xls file (2015)
  data_j=readxl::read_xls(paste0("datas/base-ic-evol-struct-pop-2015.xls"),sheet="IRIS", skip=5)
  
  non_year_vars = names(data_j)[0:12]
  year_vars = names(data_j)[13:length(data_j)]
  
  #drop the characters ,nb 2 and 3 representing the year
  year_vars = paste0(substring(year_vars,0,1),substring(year_vars,4))
  
  names(data_j)=c(non_year_vars,year_vars)
  
  #add the variable "annee"
  data_j$annee=2015 
  data=bind_rows(data,data_j)
  
  
  #read other xlsx files (2016,2017)
  
  for (annee in 2016:2017){
    data_i = readxl::read_xlsx(paste0("datas/base-ic-evol-struct-pop-",
                                      annee,".xlsx"), sheet="IRIS", skip=5)
    non_year_vars = names(data_i)[0:12]
    #drop the characters ,nb 2 and 3 representing the year
    year_vars = names(data_i)[13:length(data_i)] 
    year_vars = paste0(substring(year_vars,0,1),substring(year_vars,4))
    
    names(data_i)=c(non_year_vars,year_vars)
    
    data_i$annee=annee
    data=bind_rows(data,data_i)
  }
  return(data)
}


## Returns pop (either by dep or reg) data merged with salaries   
get_pop_wage_data <- function(precision="REG", pop_data=NULL){
  if(!(precision %in% c("REG", "DEP"))){
    stop("Precision has to be either 'REG' or 'DEP'")
  }
  if (is.null(pop_data)){
    pop_data=get_pop_data()
  }
  #[WARNING] data is skewed; population data from 2015-17 was merged with 2019 salaries
  data_wage <- readxl::read_xlsx(paste0("datas/base-cc-bases-tous-salaries-2019.xlsx"),sheet=precision, skip=5)
  vars = colnames(pop_data)
  
  if (precision == "REG"){
    vars <- vars[c(seq.int(-12,-3),-1)] #remove everything not groupable by region (=keep only pop vals + reg )
    new_data <- pop_data[vars] %>%
      group_by(REG,annee) %>%
      summarise_all(sum)
    data_wage <- data_wage %>% rename(REG = CODGEO)
  }
  else{
    vars <- vars[c(seq.int(-12,-4),-1)] #remove everything not groupable by region (=keep only pop vals, dep and reg )
    new_data <- pop_data[vars] %>%
      group_by(REG,DEP,annee) %>%
      summarise_all(sum)
    data_wage <- data_wage %>% rename(DEP = CODGEO)
  }
  
  data_pop_wage <- merge(new_data,data_wage,by=precision)
  return(data_pop_wage)
}