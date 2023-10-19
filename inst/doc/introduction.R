## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  echo = TRUE,
  message = FALSE,
  error = TRUE,
  eval = TRUE,
  out.width = "100%",
  fig.width = 7,
  fig.height = 5,
  dev = "png",
  dpi = 300
)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("getspanel")

## ----setup--------------------------------------------------------------------
library(getspanel)
library(fixest)

## -----------------------------------------------------------------------------
data("EUCO2residential")
head(EUCO2residential)

# let's subset this a little bit to speed this up
EUCO2residential <- EUCO2residential[EUCO2residential$year > 2000 & 
                                       EUCO2residential$country %in% c("Germany", "Austria",
                                                                       "Belgium", "Italy", 
                                                                       "Sweden", "Denmark"),]

# let's create a log emissions per capita variable
EUCO2residential$lagg.directem_pc <- log(EUCO2residential$agg.directem/EUCO2residential$pop)

# and let's also turn off printing the intermediate output from isatpanel
options(print.searchoutput = FALSE)

## -----------------------------------------------------------------------------
is_lm <- isatpanel(data = EUCO2residential,
                   formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
                   index = c("country","year"),
                   
                   effect = "twoways",
                   
                   fesis = TRUE)

## -----------------------------------------------------------------------------
is_gets <- isatpanel(y = EUCO2residential$lagg.directem_pc,
                     mxreg = EUCO2residential$lgdp,
                     time = EUCO2residential$year,
                     id = EUCO2residential$country,

                     effect = "twoways",

                     fesis = TRUE)

## -----------------------------------------------------------------------------
plot(is_lm)

## -----------------------------------------------------------------------------
plot_grid(is_lm)

## -----------------------------------------------------------------------------
plot_counterfactual(is_lm)

## -----------------------------------------------------------------------------
iis_example <- isatpanel(data = EUCO2residential,
                         formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
                         index = c("country","year"),

                         effect = "twoways",

                         iis = TRUE,
                         fesis = TRUE)

## -----------------------------------------------------------------------------
plot(iis_example)

## -----------------------------------------------------------------------------
jsis_example <- isatpanel(data = EUCO2residential,
                          formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
                          index = c("country","year"),

                          effect = "individual",

                          jsis = TRUE)

## -----------------------------------------------------------------------------
csis_example <- isatpanel(data = EUCO2residential,
                          formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
                          index = c("country","year"),

                          effect = "twoways",
                          t.pval = 0.05,

                          csis = TRUE)

## -----------------------------------------------------------------------------
csis_example2 <- isatpanel(data = EUCO2residential,
                           formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
                           index = c("country","year"),

                           effect = "twoways",

                           csis = TRUE,
                           csis_var = "lgdp")

## -----------------------------------------------------------------------------
fesis_example <- isatpanel(data = EUCO2residential,
                           formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
                           index = c("country","year"),

                           effect = "twoways",

                           fesis = TRUE)


## -----------------------------------------------------------------------------
plot(fesis_example)

## -----------------------------------------------------------------------------
fesis_example2 <- isatpanel(data = EUCO2residential,
                           formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
                           index = c("country","year"),

                           effect = "twoways",

                           fesis = TRUE,
                           fesis_id = c("Austria","Denmark"))

## -----------------------------------------------------------------------------
plot(fesis_example2)

## -----------------------------------------------------------------------------
robust_isatpanel(fesis_example, HAC = TRUE, robust = TRUE, cluster = "group")

## -----------------------------------------------------------------------------
cfesis_example <- isatpanel(data = EUCO2residential,
                            formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
                            index = c("country","year"),

                            effect = "twoways",

                            cfesis = TRUE,
                            cfesis_id = c("Belgium","Germany"),
                            cfesis_var = "lgdp",
                            t.pval = 0.001)

## -----------------------------------------------------------------------------
plot(cfesis_example)

## -----------------------------------------------------------------------------
fesis_ar1_example <- isatpanel(data = EUCO2residential,
                               formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
                               index = c("country","year"),

                               effect = "twoways",

                               fesis = TRUE,

                               ar = 1)

## ----eval = FALSE-------------------------------------------------------------
#  fixest_example <- isatpanel(data = EUCO2residential,
#                              formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
#                              index = c("country","year"),
#  
#                              effect = "twoways",
#  
#                              fesis = TRUE,
#  
#                              engine = "fixest",
#                              cluster = "none")

## ---- eval = FALSE------------------------------------------------------------
#  head(fixest_example$isatpanel.result$mean.results)

## ---- eval = FALSE------------------------------------------------------------
#  head(is_lm$isatpanel.result$mean.results)

## ---- eval = FALSE------------------------------------------------------------
#  fixest_example_cluster <- isatpanel(data = EUCO2residential,
#                                      formula = lagg.directem_pc ~ lgdp + I(lgdp^2) + pop,
#                                      index = c("country","year"),
#  
#                                      effect = "twoways",
#  
#                                      fesis = TRUE,
#  
#                                      engine = "fixest",
#                                      cluster = "individual")

## ---- eval = FALSE------------------------------------------------------------
#  plot(fixest_example_cluster)

