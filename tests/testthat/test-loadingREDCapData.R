#devtools::load_all()

data <- list(RED=c('1','2','2','1'),
             Green=c(4,5,2,1),
             blue=c(2L,41L,61L,712L)) %>%
  tibble::as_tibble()

dataDict <- list(varnm=c('RED','Green','blue'),
                 values=list(c(1,2),c(1,2,3,4,5),c()),
                 shortcats=list(c('N','Y'),c('One','Two','Three','Four','Five'),c())) %>%
  tibble::as_tibble()


test_that("Factor Making Works", {
  expect_equal(radioShortCatsFactorer(data,
                                      'RED',c('1','2','3','4','5'),
                                      c('One','Two','Three','Four','Five')),
               data %>% dplyr::select(RED) %>%
                 dplyr::mutate(RED=factor(RED,c('1','2','3','4','5'),
                                          c('One','Two','Three','Four','Five')) %>%
                                 forcats::fct_rev()))
})


test_that("Factor Making Works 2", {
  expect_equal(dataDict %>%
                 dplyr::slice(1:2)%$%
                 purrr::pmap(list(varnm,values,shortcats),~radioShortCatsFactorer(data,..1,..2,..3)) %>%
                 dplyr::bind_cols(),
               data %>%
                 dplyr::select(RED,Green) %>%
                 dplyr::mutate(RED=factor(RED,c(1,2),c('N','Y')) %>% forcats::fct_rev(),
                               Green=factor(Green,c(1,2,3,4,5),c('One','Two','Three','Four','Five')) %>% forcats::fct_rev())
  )
})



test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})




# dataDict <- readDataDictionary("C:/Users/user/Documents/Analysis for Others/ME/ConcordanceStudy/Data/meta/04 CONCORD REDCap for data management2021-11-26.csv")
# longerDataDict <- expandDataDictionary(dataDict)
#
# readREDCapData('C:/Users/user/Documents/Analysis for Others/ME/ConcordanceStudy/Data/raw/01 ConcordanceStudy_DATA_2022-12-30_1413.csv',
#                longerDataDict)



