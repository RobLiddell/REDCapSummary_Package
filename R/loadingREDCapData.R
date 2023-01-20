redCAPdata <- values <- tp <- expandedVarnm <- varnm <- shortcats <- longcats <- vis_id <- values <- NULL

#Function for creating variable type list for redcap data
createColList <- function(dataType,variableValues){
  colType <- switch (dataType,
          `$` = vroom::col_character(),
          `01` = 'i',
          `##` = 'n',
          `#` = 'i',
          `dt` = vroom::col_character(),
          `b` = vroom::col_factor(levels = variableValues),
          `b0` = vroom::col_factor(levels = variableValues))
  return(colType)
}

#Formatting CheckBoxVariables
checkBoxFactorer <- function(data,variableName,variableValues,variableLabels){

  variableValues <- variableValues %>%
    append(0)
  variableLabels <- variableLabels %>%
    append(0)

  #assign('test',test,envir=.GlobalEnv) may want to impliment this to be more specific

  returnData<-data[variableName] %>%dplyr::mutate({{variableName}}:=.data[[variableName]]*as.integer(stringr::str_extract(variableName,'(?<=_)[0-9]+$')),
                        {{variableName}}:=factor(.data[[variableName]],
                                                 levels=variableValues,
                                                 labels=variableLabels))
  return(returnData)

}

#Formatting b variables
radioShortCatsFactorer <- function(data,variableName,variableValues,variableLabels){

  returnData<-data[variableName] %>%
    dplyr::mutate({{variableName}}:=factor(.data[[variableName]],
                                           levels=variableValues,
                                           labels=variableLabels) %>% forcats::fct_rev())

  return(returnData)

}

#Formatting b0 Variables
radioBasicFactorer <- function(data,variableName,variableValues,variableLabels){

  returnData<-data[variableName] %>%
    dplyr::mutate({{variableName}}:=factor(.data[[variableName]],
                                           levels=variableValues,
                                           labels=variableValues) %>% forcats::fct_rev())

  return(returnData)


}

#' Load a modified version of a REDCap Data dictionary
#'
#' @param path a path to a modified version of the redcap data dictionary as a csv.
#' @param delim the deliminator for the file type
#'
#' @return a formatted data dictionary
#' @export
#'
#' @examples 'none'
readDataDictionary <- function(path , delim=','){
  dataDictionary <- vroom::vroom(path, delim=delim) %>%
    dplyr::mutate(values=stringr::str_remove_all(values,'\"') %>% stringr::str_split(','),
                  shortcats=stringr::str_remove_all(shortcats,'\"') %>% stringr::str_split(','),
                  longcats=stringr::str_remove_all(longcats,'\"') %>% stringr::str_split(',')) %>%
    dplyr::mutate(tp=dplyr::if_else(tp=='1','01',tp),
                  expandedVarnm=dplyr::if_else(tp=='01',purrr::map2(varnm,values,function(.x,.y) (paste0(.x,'___',.y))),
                                        purrr::map(varnm,function(.x) (.x))))

  return(dataDictionary)
}

#' Expand the data dictionary to account for checkbox entries
#'
#' @param dataDictionary the data dictionary which describes the data set
#'
#' @return a tibble similar to data dictionary but with formatted checkbox entries
#' @export
#'
#' @examples 'none'
expandDataDictionary <- function(dataDictionary){
  longerDataDictionary <- dataDictionary %>%
    dplyr::mutate(varnm=expandedVarnm) %>%
    dplyr::select(-expandedVarnm) %>%
    tidyr::unnest_longer(varnm)

  return(longerDataDictionary)
}

dataTypesList <- function(longerDataDictionary){
  columnTypes=longerDataDictionary %>%
    dplyr::select(values,tp) %$%
    purrr::map2(tp,values,~createColList(.x,.y)) %>%
    rlang::set_names(longerDataDictionary$varnm) %>%
    append(list(redcap_repeat_instrument=vroom::col_character(),redcap_repeat_instance='i'),after=0)

  return(columnTypes)
}

#' readREDCapData
#'
#' @param path a path to the redcap data file
#' @param longerDataDictionary a data dictionary preferably one which has been
#' made longer to include the subvariables for checkboxes
#' @param delim the deliminator used such as ',' for .csv files
#'
#' @return a formatted REDCAP data base as a tibble
#' @export
#'
#' @examples 'none'
readREDCapData <- function(path,longerDataDictionary, delim=','){

  namedListOfVariableTypes <- dataTypesList(longerDataDictionary)

  variableNames <- namedListOfVariableTypes %>%
    names()

  #Read Data
  redCAPdata <- vroom::vroom(path,delim=delim,col_types=namedListOfVariableTypes,col_select=tidyselect::all_of(variableNames)) %>%
    dplyr::mutate(rowID=dplyr::row_number(),.before=1) %>%
    dplyr::mutate(vis_id=stringr::str_remove(vis_id,'^[0]*'))

  #Format Months Properly
  redCAPdata <- longerDataDictionary %>%
    dplyr::filter(tp=='dt') %$%
    dplyr::mutate(redCAPdata,dplyr::across(.col=tidyselect::all_of(varnm),lubridate::ymd))


  for(currDataType in c('01','b','b0')){
    dataOfCurrentType <- longerDataDict %>%
      dplyr::filter(tp==currDataType) %>%
      dplyr::select(varnm,values,shortcats)

    dataTypeVariables <- dataOfCurrentType %>%
      dplyr::pull(varnm)

    factorFunction <- switch(currDataType,
                             `01`=checkBoxFactorer,
                             `b`=radioShortCatsFactorer,
                             `b0`=radioBasicFactorer)

    redCAPdata[dataTypeVariables] <- dataOfCurrentType %$%
      purrr::pmap(list(varnm,values,shortcats),~factorFunction(redCAPdata[dataTypeVariables],..1,..2,..3)) %>%
      dplyr::bind_cols()
  }

  return(redCAPdata)
}


