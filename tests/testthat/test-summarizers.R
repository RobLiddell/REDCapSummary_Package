# devtools::load_all()


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


test_that("cutColumn works with Factors", {

  #FACTORS
  expect_equal(cutColumn(graphicDataTest$RED,'b0'),
               graphicDataTest$RED)

})


test_that("cutColumn works with Numbers", {

   #NUMERIC
  expect_equal(cutColumn(graphicDataTest$BLUE[c(1:10)],'#'),
               graphicDataTest$BLUE[c(1:10)] %>%
                 factor() %>% forcats::fct_rev())


  expect_equal(cutColumn(graphicDataTest$BLUE[c(1:11)],'#'),
               graphicDataTest$BLUE[c(1:11)] %>%
                 cut(10) %>% forcats::fct_rev() %>% formatRangeLevels('#'))

})

test_that("cutColumn works with Dates", {

  #DATES
  expect_equal(cutColumn(graphicDataTest$GREEN[c(1:10)],'dt'),
               graphicDataTest$GREEN[c(1:10)] %>%
                 factor() %>% forcats::fct_rev())

  expect_equal(cutColumn(graphicDataTest$GREEN,'dt'),
               graphicDataTest$GREEN %>%
                 as.numeric() %>% cut(10) %>%
                                 forcats::fct_rev()%>% formatRangeLevels('dt'))

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




test_that("data renaming works",{

  expect_equal(changeLabels(graphicDataTest[,c(1:3)],'b0','red'),
               graphicDataTest[,c(1:3)] %>%
                 dplyr::rename(red=RED))
})
