library(tidyverse)
library(scales)

# Source: Working Guide to Reservoir Engineering by William Lyons
# The procedure outlined by Dykstra and Parsons was to: 
# (1) divide permeabilities (usually from core analysis)so that all samples are of equal thickness (often 1 ft)
# (2) arrange the permeabilities in descending order from highest to lowest 
# (3) calculate for each sample the percent of samples that have a higher permeability (see example in Table 4.5), 
# (4) plot the data from Step 3 on log-probability paper (see Figure 4.11) 
# (5) draw the best straight line through data (with less emphasis on points at the extremities, if necessary)
# (6) determine the permeability at 84.1% probability (k84.1) and the mean permeability at 50% probability (k50) 
# (7) compute the permeability variation, V=(K50-K84.1)/k50

df_perm_data <- data.frame(permeability=c(860, 640, 380, 340,280,210,160,135,130,110,78, 65,63,54,40,27,21,20,15),
                 fraction=c(0.05,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.90,.95))

x_breaks <- c(0.001, 0.01, 0.05, .10, 0.15, 0.2, 0.25,.30,0.35, 0.4,0.45, .50,0.55, 0.6, .70,0.8, .90, 0.95,0.98, 0.99, 0.999)

y_breaks <- 10^(-10:10)
y_minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

y_min <- 1
y_max <- 1000


eq <- lm(log10(permeability)~fraction, data=df)
kv50 <- 10^ predict(eq, newdata = data.frame(fraction=0.5))
kv84 <- 10^ predict(eq, newdata = data.frame(fraction=0.841))

DP <- (kv50-kv84)/kv50


plt <- ggplot(df_perm_data, aes(fraction,permeability)) +
geom_point(size=3) +
stat_smooth(method="lm", se=F, size=2) +
# geom_segment(aes(x = 0.5, xend = 0.5, y = 1, yend = kv50), color = "red", size=2, linetype="dashed") +
# geom_segment(aes(x = .01, xend =0.5 , y =kv50, yend =kv50), color = "red", size=2, linetype="dashed") +
# geom_segment(aes(x = 0.841, xend = 0.841, y = 1, yend = kv84), color = "red", size=2, linetype="dashed") +
# geom_segment(aes(x = .01, xend =0.841 , y =kv84, yend =kv84), color = "red", size=2, linetype="dashed") +
scale_x_continuous(trans = probability_trans("norm"),
                   breaks = x_breaks,
                   labels = prettyNum(x_breaks*100), 
                   limits = c(0.01,.99),
                   expand = c(0.01, NA)) +
scale_y_continuous(trans='log10',
                   breaks = y_breaks, 
                   minor_breaks = y_minor_breaks,
                   limits = c(y_min, y_max), 
                   expand = c(y_min, NA))+ 
theme_bw() +
theme(axis.title = element_text(size = 18), 
      axis.text.y =element_text(size = 16), 
      axis.text.x =element_text(size = 12) , 
      plot.title = element_text(size = 24)) +
labs(title = "Permeability Variation", 
     x="%Total Samples Having Higher Permeability", 
     y="Permeability, md")+
annotate(geom="text", x=0.1, y=30, label=paste("DP Coeff. = ", round(DP,2)),
           color="red", size = 6)

plt + annotation_logticks(sides = "l", outside = F) + coord_cartesian(clip = "off")
