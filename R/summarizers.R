

#String/Number Conversion to Date String
stringNumToDateChar <- function(string){
  dateString <-  string |>
    as.integer() |>
    as.Date(origin="1970-01-01") |>
    as.character()

  return(dateString)
}

#Format Range labels to be a bit nicer for plots
formatRangeLevels <- function(factor,dataType){

  if(dataType=='dt'){
    levels(factor) <- levels(factor) %>%
      stringr::str_replace('(?<=\\().*?(?=,)',stringNumToDateChar) %>%
      stringr::str_replace('(?<=,).*?(?=\\])',stringNumToDateChar) %>%
      stringr::str_replace('[0-9]{5}',stringNumToDateChar)
  }


  levels(factor) <- levels(factor) %>%
    stringr::str_remove_all('[\\(\\]]') %>%
    stringr::str_replace(',','\n')

  return(factor)

}



#' getDaTa
#' Return appropriate data sections from a REDCap style database
#'
#' @param REDCapdata A REDcap style tibble
#' @param variableName The variable(s) to extract from the dataframe
#' @param instrumentLabel The instrument to extract the data from
#'
#' @return A tibble of the selected data
#' @export
#'
#' @examples '1+1'
getData <- function(REDCapdata,variableName,instrumentLabel){

  REDCapdata <- REDCapdata %>%
    dplyr::mutate(redcap_repeat_instrument=dplyr::if_else(is.na(redcap_repeat_instrument),'',
                                            redcap_repeat_instrument))

  returnDat <- REDCapdata %>%
    dplyr::filter(redcap_repeat_instrument==instrumentLabel) %>%
    dplyr::select(rowID,vis_id,tidyselect::all_of(variableName))

  return(returnDat)
}


cutColumn <- function(graphicDataVec,dataType){

  if(dataType%in%c('$','01','b','b0')){
    return(graphicDataVec)
  }

  nGroups <- graphicDataVec %>%
    unique() %>%
    length()

  if(nGroups<=10){

    graphicDataVec <- graphicDataVec %>%
      factor() %>%
      forcats::fct_rev()
    return(graphicDataVec)
  }

  if(dataType=='dt'){
    graphicDataVec <- graphicDataVec %>%
      as.numeric()
  }

  graphicDataVec <- graphicDataVec %>%
    cut(10) %>%
    forcats::fct_rev()

  graphicDataVec <- formatRangeLevels(graphicDataVec,dataType)

  return(graphicDataVec)

}


#' cutData
#' cut appropriate columns of the tibble (ignoring the first two which should be rownumber and id)
#'
#' @param graphicData data for each summary graphic
#' @param dataTypeList the data type within each graphicData tibble
#' @param graphicStyle the style of graphic to be produced (typically default)
#' @param graphic the exact type of graphic (one of table or plot)
#'
#' @return returns a tibble with cut data as formatted factors
#' @export
#'
#' @examples 2+2
cutData <- function(graphicData,dataTypeList,graphicStyle,graphic){

  if(graphicStyle!='default'){
    return(graphicData)
  }else if(graphic == 'table'){
    return(graphicData)
  }

  graphicData[3:ncol(graphicData)] <- tibble::tibble(dataType=dataTypeList,dataColumns=colnames(graphicData)[3:ncol(graphicData)])%$%
    purrr::map2(dataColumns,dataType,~cutColumn(graphicData[[.x]],.y))

  return(graphicData)
}

#Changing data Labels from varnm to myLabel####

#' Change Label of data columns
#'
#' @param graphicData data for each summary graphic
#' @param dataType The type of each variable
#' @param myLabel Labels to replace variable name with
#'
#' @return the same tibble but with changed labels
#' @export
#'
#' @examples 2+2
changeLabels <- function(graphicData,dataType,myLabel){
  if(dataType[[1]]=='01'){
    return(graphicData)
  }

  renamerList <- colnames(graphicData)[-c(1,2)] %>%
   rlang::set_names(myLabel)

  graphicData <- graphicData %>%
    dplyr::rename(!!renamerList )

  return(graphicData)
}
