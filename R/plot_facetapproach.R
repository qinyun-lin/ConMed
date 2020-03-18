rxmo=0.1807
rxy=0.1602
rymo=0.446
nobs=123
labelest = "indirect1"
conflevel=0.95
specifyunob = 0
S1 <- rxmu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.1,rymu=0.1, nobs=nobs, conflevel=conflevel)
S2 <- rxmu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.3,rymu=0.3, nobs=nobs, conflevel=conflevel)
S3 <- rxmu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.5,rymu=0.5, nobs=nobs, conflevel=conflevel)
S4 <- rxmu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.1,rymu=0.5, nobs=nobs, conflevel=conflevel)
S5 <- rxmu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.5,rymu=0.1, nobs=nobs, conflevel=conflevel)
S1$cat <- "S1"
S2$cat <- "S2"
S3$cat <- "S3"
S4$cat <- "S4"
S5$cat <- "S5"
result <- rbind(S1, S2, S3, S4, S5)
result <- result[result$label == labelest,]
newlabel <- c("S1" = expression("Low"~ rho[M[O]*M[U]] ~ "Low" ~ rho[Y*M[U]]),
              "S2" = expression("Medium"~ rho[M[O]*M[U]] ~ "Medium" ~ rho[Y*M[U]]),
              "S3" = expression("High"~ rho[M[O]*M[U]] ~ "High" ~ rho[Y*M[U]]),
              "S4" = expression("Low"~ rho[M[O]*M[U]] ~ "High" ~ rho[Y*M[U]]),
              "S5" = expression("High"~ rho[M[O]*M[U]] ~ "Low" ~ rho[Y*M[U]]))
result$cat <- factor(result$cat, labels = newlabel)

ggplot(result, aes(x=rxmu, y=est)) +
  geom_ribbon(data=result, ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
  geom_line() +
  geom_point() +
  labs(x = expression(rho[X*M[U]])) +
  labs(y = expression(a[1]*b[1]))+
  ggplot2::geom_hline(yintercept=result$est.omit,linetype="dashed") +
  facet_wrap(.~ cat, labeller = label_parsed, nrow = 5, scale = "free") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x=element_blank(),
        legend.text = element_text(colour="black", size = 12),
        legend.title = element_text(colour="black", size = 12))

