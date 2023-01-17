## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  echo = TRUE,
  message = FALSE,
  error=TRUE,
  eval=TRUE,
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(getspanel)

## -----------------------------------------------------------------------------
data("EUCO2residential")
head(EUCO2residential)

# let's subset this a little bit to speed this up
EUCO2residential <- EUCO2residential[EUCO2residential$year > 2000 & 
                                       EUCO2residential$country %in% c("Germany", "Austria",
                                                                       "Belgium", "Italy", 
                                                                       "Sweden", "Denmark"),]

# and let's also turn off printing the intermediate output from isatpanel
options(print.searchoutput = FALSE)

## -----------------------------------------------------------------------------
is_lm <- isatpanel(data = EUCO2residential,
                   formula = lagg.directem_pc ~ lgdp, 
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
                         formula = lagg.directem_pc ~ lgdp,
                         index = c("country","year"),

                         effect = "twoways",

                         iis = TRUE)

## -----------------------------------------------------------------------------
plot(iis_example)

## -----------------------------------------------------------------------------
jsis_example <- isatpanel(data = EUCO2residential,
                          formula = lagg.directem_pc ~ lgdp,
                          index = c("country","year"),

                          effect = "twoways",

                          jsis = TRUE)

## -----------------------------------------------------------------------------
plot(jsis_example)

## -----------------------------------------------------------------------------
csis_example <- isatpanel(data = EUCO2residential,
                          formula = lagg.directem_pc ~ lgdp + I(lgdp^2),
                          index = c("country","year"),

                          effect = "twoways",

                          csis = TRUE)

## -----------------------------------------------------------------------------
plot(csis_example)

## -----------------------------------------------------------------------------
csis_example2 <- isatpanel(data = EUCO2residential,
                           formula = lagg.directem_pc ~ lgdp + I(lgdp^2),
                           index = c("country","year"),

                           effect = "twoways",

                           csis = TRUE,
                           csis_var = "lgdp")

## -----------------------------------------------------------------------------
fesis_example <- isatpanel(data = EUCO2residential,
                           formula = lagg.directem_pc ~ lgdp + I(lgdp^2),
                           index = c("country","year"),

                           effect = "twoways",

                           fesis = TRUE)


## -----------------------------------------------------------------------------
plot(fesis_example)

## -----------------------------------------------------------------------------
fesis_example2 <- isatpanel(data = EUCO2residential,
                           formula = lagg.directem_pc ~ lgdp + I(lgdp^2),
                           index = c("country","year"),

                           effect = "twoways",

                           fesis = TRUE,
                           fesis_id = c("2","3"))

## -----------------------------------------------------------------------------
plot(fesis_example2)

## -----------------------------------------------------------------------------
cfesis_example <- isatpanel(data = EUCO2residential,
                            formula = lagg.directem_pc ~ lgdp + I(lgdp^2),
                            index = c("country","year"),

                            effect = "twoways",

                            cfesis = TRUE,
                            cfesis_id = c("2","3"),
                            cfesis_var = "lgdp")

## -----------------------------------------------------------------------------
plot(cfesis_example)

## -----------------------------------------------------------------------------
fesis_ar1_example <- isatpanel(data = EUCO2residential,
                               formula = lagg.directem_pc ~ lgdp + I(lgdp^2),
                               index = c("country","year"),

                               effect = "twoways",

                               fesis = TRUE,

                               ar = 1)

## -----------------------------------------------------------------------------
robust.isatpanel(fesis_ar1_example, HAC = TRUE, robust = TRUE, cluster = "group")

## -----------------------------------------------------------------------------
fixest_example <- isatpanel(data = EUCO2residential,
                            formula = lagg.directem_pc ~ lgdp,
                            index = c("country","year"),

                            effect = "twoways",

                            fesis = TRUE,

                            engine = "fixest",
                            cluster = "none")

## -----------------------------------------------------------------------------
head(fixest_example$isatpanel.result$mean.results)

## -----------------------------------------------------------------------------
head(is_lm$isatpanel.result$mean.results)

## -----------------------------------------------------------------------------
fixest_example_cluster <- isatpanel(data = EUCO2residential,
                                    formula = lagg.directem_pc ~ lgdp + I(lgdp^2),
                                    index = c("country","year"),

                                    effect = "twoways",

                                    fesis = TRUE,

                                    engine = "fixest",
                                    cluster = "individual")

## -----------------------------------------------------------------------------
plot(fixest_example_cluster)

