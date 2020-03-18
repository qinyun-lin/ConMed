install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
install.packages("roxygen2")
library("roxygen2")

library(ggplot2)

devtools::check()
devtools::install()

library(ConMed)

rxmu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "indirect1", conflevel=0.95,specifyunob = 0)
rxmu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "a1", conflevel=0.95,specifyunob = 0)
rxmu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "b1", conflevel=0.95,specifyunob = 0)
rxmu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "c", conflevel=0.95,specifyunob = 0)
rxmu_plot_a1b1(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123, conflevel=0.95,specifyunob = 0)

rmomu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
           labelest = "indirect1", conflevel=0.95,specifyunob = 0)
rmomu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
           labelest = "a1", conflevel=0.95,specifyunob = 0)
rmomu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
           labelest = "b1", conflevel=0.95,specifyunob = 0)
rmomu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
           labelest = "c", conflevel=0.95,specifyunob = 0)
rmomu_plot_a1b1(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,conflevel=0.95,specifyunob = 0)


rymu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "indirect1", conflevel=0.95,specifyunob = 0)
rymu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "a1", conflevel=0.95,specifyunob = 0)
rymu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "b1", conflevel=0.95,specifyunob = 0)
rymu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "c", conflevel=0.95,specifyunob = 0)
rymu_plot_a1b1(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123, conflevel=0.95,specifyunob = 0)


rxmu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "indirect1", conflevel=0.95,specifyunob = 1,rmomu=0.4,rymu=0.3)

rxmu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "a1", conflevel=0.95,specifyunob = 1,rmomu=0.4,rymu=0.3)

rmomu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
           labelest = "indirect1", conflevel=0.95,specifyunob = 1, rxmu=0.2,rymu = 0.2)
rymu_plot(rxmo=0.1807,rxy=0.1602,rymo=0.446,nobs=123,
          labelest = "indirect1", conflevel=0.95,specifyunob = 1, rxmu=0.1, rmomu = 0.1)


rxmo=0.1807
rxy=0.1602
rymo=0.446
nobs=123
rmomu=0.2
rymu=0.2
result <- rxmu_data(rxmo, rxy, rymo, rmomu, rymu, nobs)

