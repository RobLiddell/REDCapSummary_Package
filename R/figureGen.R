#Figure Generating Functions####

#' generate figures for plotting
#'
#' @param graphicData Data to generate figure from (table or plot or custom)
#' @param graphicStyle A string specifying what type of subgraphic to generate
#' @param graphic A string specifing table, plot or custom.
#' @param dataType A sting specifying the data types found in graphicData
#' @param subsection a string specifying the subsection these graphics will be found in
#'
#' @return A figure of the type specified
#' @export
#'
#' @examples 'TBD'
figureGen <- function(graphicData,graphicStyle,graphic,dataType,subsection){

  graphicData <- graphicData %>%
    dplyr::rename(`Study ID`=vis_id) %>%
    dplyr::relocate(`Study ID`,.after=1)

  figure <- switch(graphic,
                   plot=plotGen(graphicData,graphicStyle,dataType),
                   table=tableGen(graphicData,graphicStyle,dataType,subsection),
                   custom=NULL)
  return(figure)
}

#Table Generating Functions####

tableGen <- function(graphicData,graphicStyle,tp,subsection){

  flxTbl <- switch(graphicStyle,
                   default=tableGenDefault(graphicData),
                   combined=tableGenCombined(graphicData),
                   difference=tableGenDifference(graphicData),
                   `interaction`=tableGenInteraction(graphicData))

  nTableRows <- flxTbl %>% dim() %>% .$heights %>% length()-1
  nTableCols <- flxTbl %>% dim() %>% .$widths %>% length()

  flxTbl <- flxTbl %>%
    flextable::bg(i=seq(1,nTableRows,2),bg='grey95')

  if(graphicStyle!='default'){
    flxTbl <- flxTbl %>%
      flextable::add_header_row(values=subsection,colwidths = nTableCols)
  }

  return(flxTbl)
}

tableGenDefault <- function(graphicData){

  columnNames <- colnames(graphicData)[-c(1,2)]
  visitID <- colnames(graphicData)[2]

  tableData <- graphicData %>%
    dplyr::mutate({{visitID}}:=as.numeric(.data[[visitID]]),
           dplyr::across(tidyselect::all_of(columnNames),as.character)) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(columnNames),~stringr::str_replace_all(.x,'(?<=[0-9])\n(?=[0-9])','->'))) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(columnNames),~stringr::str_replace_all(.x,'\n',' '))) %>%
    dplyr::filter(!is.na(.data[[columnNames]])) %>%
    dplyr::group_by(dplyr::across(-c(1,2))) %>%
    dplyr::summarise(firstID=dplyr::first(.data[[visitID]]) %>% as.numeric(),
              {{visitID}}:=stringr::str_flatten(.data[[visitID]],', ')) %>%
    dplyr::relocate(tidyselect::all_of(visitID),.before=1) %>%
    dplyr::arrange(firstID) %>%
    dplyr::select(-firstID)

  flxTbl <- tableData %>%
    flextable::flextable() %>%
    flextable::width(c(1,2),c(1.5,5.5))

  return(flxTbl)

}

tableGenCombined <- function(graphicData){
  columnNames <- colnames(graphicData)[-c(1,2)]
  visitID <- colnames(graphicData)[2]

  nColumns=ncol(graphicData)-1

  flxTbl <- graphicData %>%
    dplyr::select(-rowID) %>%
    flextable::flextable() %>%
    flextable::width(c(1:nColumns),width=c(0.75,rep(6.25/(nColumns-1),nColumns-1)))

  return(flxTbl)
}

tableGenDifference <- function(graphicData){

  columnNames <- colnames(graphicData)[-c(1,2)]
  visitID <- colnames(graphicData)[2]

  flxTbl <- graphicData %>%
    dplyr::filter((.data[[columnNames[1]]]!=.data[[columnNames[2]]])|is.na(.data[[columnNames[1]]])|is.na(.data[[columnNames[2]]])) %>%
    dplyr::select(-rowID) %>%
    dplyr::mutate(dplyr::across(.fns=as.character)) %>%
    flextable::flextable() %>%
    flextable::width(c(1:3),width=c(0.75,6.25/2,6.25/2))

  return(flxTbl)
}

tableGenInteraction <- function(graphicData){
  columnNames <- colnames(graphicData)[-c(1,2)]
  visitID <- colnames(graphicData)[2]
  tableData <- graphicData %>%
    dplyr::select(-rowID) %>%
    dplyr::group_by(dplyr::across(-1)) %>%
    dplyr::summarise(firstID=dplyr::first(.data[[visitID]]) %>% as.numeric(),
              {{visitID}}:=stringr::str_flatten(.data[[visitID]],', '),
              .groups = 'drop') %>%
    dplyr::select(-firstID) %>%
    dplyr::relocate(tidyselect::all_of(visitID),.before=1)

  nColumns=ncol(tableData)

  flxTbl <- tableData %>%
    flextable::flextable() %>%
    flextable::width(c(1:nColumns),c(2,rep(5/(nColumns-1),nColumns-1)))

  return(flxTbl)

}

#Plot Generating Function####

plotGen <- function(graphicData,graphicStyle,tp){

  p <- switch(graphicStyle,
              default=plotGenDefault(graphicData,tp))

  p <- p +
    ggplot2::scale_fill_grey(guide='none')+
    ggplot2::coord_flip()

  return(p)
}

plotGenDefault <- function(graphicData,tp){

  columnNames <- colnames(graphicData)[-c(1,2)]
  visitID <- colnames(graphicData)[2]

  pivotData <- graphicData %>%
    tidyr::pivot_longer(cols=-c(1,2))

  tp <- tp[[1]]

  if(tp=='$'){
    pivotData <- pivotData %>%
      dplyr::mutate(value=factor(value))
  }

  if(tp%in%c('01','b','$')){
    levels(pivotData$value) <- levels(pivotData$value) %>%
      stringr::str_wrap(11)
  }

  plotData <- pivotData %>%
    dplyr::group_by(name) %>%
    dplyr::count(value) %>%
    dplyr::ungroup()

  graphLabels <- plotData %>%
    dplyr::filter(n<=5) %>%
    dplyr::mutate(grouped=interaction(name,value)) %>%
    dplyr::pull(grouped) %>%
    {dplyr::filter(pivotData,interaction(name,value)%in%.)} %>%
    dplyr::group_by(name,value) %>%
    dplyr::summarise({{visitID}}:=stringr::str_flatten(.data[[visitID]], ', '),.groups='drop')


  p <- plotData %>%
    ggplot2::ggplot(ggplot2::aes(x=value,fill=value,y=n)) +
    ggplot2::geom_col()+
    ggplot2::geom_text(graphLabels,mapping=ggplot2::aes(y=5,x=value,label=.data[[visitID]]),
              hjust = 0)+
    ggplot2::facet_grid(rows=ggplot2::vars(name),scales = 'free_y')+
    ggplot2::labs(y='Count',x=NULL)

  if(tp=='01'){
    p <- p + ggplot2::scale_x_discrete(drop=TRUE)
  }else{
    p <- p + ggplot2::scale_x_discrete(drop=FALSE)
  }

  return(p)
}
