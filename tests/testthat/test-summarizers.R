devtools::load_all()


graphicDataTest <- tibble::tibble(rowNum=1:12,
                                  vis_id=c(1001:1012) %>% as.character(),
                                  RED=factor(c(0,1,1,1,0,NA,0,1,1,1,0,NA),
                                             levels=c(0,1),labels=c('No','Yes')),
                                  BLUE=c(1:12),
                                  GREEN=c(1:12) %>% as.Date(origin="1971-01-01"))

rangeFactor <- graphicDataTest %>%
  dplyr::mutate(BLUE=cut(BLUE,10))


test_that("Number converts to string", {
  expect_equal(stringNumToDateChar('0'),"1970-01-01")
  expect_equal(stringNumToDateChar('365'),"1971-01-01")
  expect_equal(stringNumToDateChar(0),"1970-01-01")
})

test_that("Ranges are made to look nicer", {
  expect_equal(formatRangeLevels(factor(c('(1,2]','(2,3]','(3,4]')),'#'),
               factor(c("1\n2","2\n3","3\n4")))

  expect_equal(formatRangeLevels(factor(c('(0,1]','(1,2]')),'dt'),
               factor(c("1970-01-01\n1970-01-02","1970-01-02\n1970-01-03")))
})


test_that("cutColumn works", {

  #FACTORS
  expect_equal(cutColumn(graphicDataTest['RED'],'b0'),
               graphicDataTest['RED'])

  #NUMERIC
  expect_equal(cutColumn(graphicDataTest[c(1:10),'BLUE'],'#'),
               graphicDataTest[c(1:10),'BLUE'] %>%
                 dplyr::mutate(BLUE=factor(BLUE) %>% forcats::fct_rev()))

  expect_equal(cutColumn(graphicDataTest[c(1:11),'BLUE'],'#'),
               graphicDataTest[c(1:11),'BLUE'] %>%
                 dplyr::mutate(BLUE=cut(BLUE,10) %>% forcats::fct_rev() %>% formatRangeLevels('#')))

  #DATES
  expect_equal(cutColumn(graphicDataTest[c(1:10),'GREEN'],'dt'),
               graphicDataTest[c(1:10),'GREEN'] %>%
                 dplyr::mutate(GREEN=factor(GREEN) %>% forcats::fct_rev()))

  expect_equal(cutColumn(graphicDataTest['GREEN'],'dt'),
               graphicDataTest['GREEN'] %>%
                 dplyr::mutate(GREEN=as.numeric(GREEN) %>% cut(10) %>%
                                 forcats::fct_rev()%>% formatRangeLevels('dt')))

})

test_that("cutData works",{
  expect_equal(cutData(graphicDataTest,c('b0','#','dt'),'default','plot'),
               graphicDataTest %>%
                 dplyr::mutate(RED=cutColumn(RED,'b0'),
                               BLUE=cutColumn(BLUE,'#'),
                               GREEN=cutColumn(GREEN,'dt')))

  expect_equal(cutData(graphicDataTest,c('b0','#','dt'),'default','table'),
               graphicDataTest)

})



