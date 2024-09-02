#####Librarys####
library(htmlwidgets)
library(tibble)
library(tidyr)
library(purrr)  #Functional programming, para simplificar/reemplazar loops
library(lhs)
library(hydroGOF) # Estadisticas de funciones
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(stringr)
library(plotly)
library(modeest)
library(openxlsx)
library(data.table)
library(SWATplusR)
library(stats)
library(gplots)

#### Functions ####
rsq_r2 <-  function(x, y , na.rm = TRUE) cor(x, y, use="complete.obs")^2  # Importante lo de use, ya que el NA.RM da fallo!!!

rmse <-  function(sim, obs , na.rm = TRUE) sqrt( mean( (sim - obs)^2, na.rm = TRUE) )

#Function to generate the sccaterplot parameter range VS hydrologic variable#
plot_dotty <- function(par, crit, crit_label = 'crit', n_col = 3) {
  dotty_tbl <- par %>% 
    mutate(crit = crit) %>% 
    pivot_longer(., cols = -crit, names_to = 'parameter')
  
  ggplot(data = dotty_tbl) +
    geom_point(aes(x = value, y = crit)) + geom_smooth(aes(x = value, y = crit))+
    facet_wrap(. ~ parameter, ncol = 3, scales = "free_x") +
    labs(x = 'Change of parameter value', y = crit_label) +
    theme_bw()}



#### Soft Calibration process####    
#Upload save data
var_ann<-readRDS ("C:/Users/Portatil_ACMA/Desktop/desk/var_ann.rds")
stream_ann<-  readRDS ("C:/Users/Portatil_ACMA/Desktop/desk/stream_ann.rds")
aqu_ann<- readRDS ("C:/Users/Portatil_ACMA/Desktop/desk/aqu_ann.rds")
pars<- read_csv("C:/Users/Portatil_ACMA/Desktop/desk/Param.csv") %>% .[-1] 

####Hydrological indices Sccaterplots####
#Sccaterplot for each sub-catchment of Runoff coefficient and Groundwater contribution 
plot_dotty1<-plot_dotty(pars[,1:10], unlist(stream_ann$`1 Peralejos`/var_ann$`1 Peralejos`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Peralejos/Q_P_Carb.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,11:20], unlist(stream_ann$`1 Peralejos`/var_ann$`1 Peralejos`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Peralejos/Q_P_DEA.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,21:30],  unlist(stream_ann$`1 Peralejos`/var_ann$`1 Peralejos`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Peralejos/Q_P_DEB.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)

plot_dotty2<-plot_dotty(pars[,1:10], unlist(aqu_ann$`1 Peralejos`/stream_ann$`1 Peralejos`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Peralejos/Fc_Q_Carb.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,11:20],  unlist(aqu_ann$`1 Peralejos`/stream_ann$`1 Peralejos`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Peralejos/Fc_Q_DEA.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,21:30],  unlist(aqu_ann$`1 Peralejos`/stream_ann$`1 Peralejos`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Peralejos/Fc_Q_DEB.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)



plot_dotty1<-plot_dotty(pars[,1:10], unlist(stream_ann$`172 Huete`/var_ann$`172 Huete`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Huete/Q_P_Carb.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,11:20], unlist(stream_ann$`172 Huete`/var_ann$`172 Huete`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Huete/Q_P_DEA.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,21:30],  unlist(stream_ann$`172 Huete`/var_ann$`172 Huete`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Huete/Q_P_DEB.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)

plot_dotty2<-plot_dotty(pars[,1:10], unlist(aqu_ann$`172 Huete`/stream_ann$`172 Huete`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Huete/Fc_Q_Carb.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,11:20],  unlist(aqu_ann$`172 Huete`/stream_ann$`172 Huete`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Huete/Fc_Q_DEA.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,21:30],  unlist(aqu_ann$`172 Huete`/stream_ann$`172 Huete`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Huete/Fc_Q_DEB.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)



plot_dotty1<-plot_dotty(pars[,1:10], unlist(stream_ann$`173 La Peraleja`/var_ann$`173 La Peraleja`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/La Peraleja/Q_P_Carb.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,11:20], unlist(stream_ann$`173 La Peraleja`/var_ann$`173 La Peraleja`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/La Peraleja/Q_P_DEA.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,21:30],  unlist(stream_ann$`173 La Peraleja`/var_ann$`173 La Peraleja`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/La Peraleja/Q_P_DEB.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)

plot_dotty2<-plot_dotty(pars[,1:10], unlist(aqu_ann$`173 La Peraleja`/stream_ann$`173 La Peraleja`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/La Peraleja/Fc_Q_Carb.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,11:20],  unlist(aqu_ann$`173 La Peraleja`/stream_ann$`173 La Peraleja`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/La Peraleja/Fc_Q_DEA.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,21:30],  unlist(aqu_ann$`173 La Peraleja`/stream_ann$`173 La Peraleja`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/La Peraleja/Fc_Q_DEB.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)



plot_dotty1<-plot_dotty(pars[,1:10], unlist(stream_ann$`186 Priego-Trabaque`/var_ann$`186 Priego-Trabaque`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Trabaque/Q_P_Carb.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,11:20], unlist(stream_ann$`186 Priego-Trabaque`/var_ann$`186 Priego-Trabaque`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Trabaque/Q_P_DEA.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,21:30],  unlist(stream_ann$`186 Priego-Trabaque`/var_ann$`186 Priego-Trabaque`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Trabaque/Q_P_DEB.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)

plot_dotty2<-plot_dotty(pars[,1:10], unlist(aqu_ann$`186 Priego-Trabaque`/stream_ann$`186 Priego-Trabaque`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Trabaque/Fc_Q_Carb.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,11:20],  unlist(aqu_ann$`186 Priego-Trabaque`/stream_ann$`186 Priego-Trabaque`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Trabaque/Fc_Q_DEA.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,21:30],  unlist(aqu_ann$`186 Priego-Trabaque`/stream_ann$`186 Priego-Trabaque`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Trabaque/Fc_Q_DEB.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)



plot_dotty1<-plot_dotty(pars[,1:10], unlist(stream_ann$`268 Taravillas`/var_ann$`268 Taravillas`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Taravillas/Q_P_Carb.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,11:20], unlist(stream_ann$`268 Taravillas`/var_ann$`268 Taravillas`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Taravillas/Q_P_DEA.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,21:30],  unlist(stream_ann$`268 Taravillas`/var_ann$`268 Taravillas`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Taravillas/Q_P_DEB.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)

plot_dotty2<-plot_dotty(pars[,1:10], unlist(aqu_ann$`268 Taravillas`/stream_ann$`268 Taravillas`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Taravillas/Fc_Q_Carb.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,11:20],  unlist(aqu_ann$`268 Taravillas`/stream_ann$`268 Taravillas`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Taravillas/Fc_Q_DEA.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,21:30],  unlist(aqu_ann$`268 Taravillas`/stream_ann$`268 Taravillas`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Taravillas/Fc_Q_DEB.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)




plot_dotty1<-plot_dotty(pars[,1:10], unlist(stream_ann$`30 Ventosa`/var_ann$`30 Ventosa`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Ventosa/Q_P_Carb.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,11:20], unlist(stream_ann$`30 Ventosa`/var_ann$`30 Ventosa`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Ventosa/Q_P_DEA.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,21:30],  unlist(stream_ann$`30 Ventosa`/var_ann$`30 Ventosa`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Ventosa/Q_P_DEB.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)

plot_dotty2<-plot_dotty(pars[,1:10], unlist(aqu_ann$`30 Ventosa`/stream_ann$`30 Ventosa`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Ventosa/Fc_Q_Carb.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,11:20],  unlist(aqu_ann$`30 Ventosa`/stream_ann$`30 Ventosa`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Ventosa/Fc_Q_DEA.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,21:30],  unlist(aqu_ann$`30 Ventosa`/stream_ann$`30 Ventosa`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Ventosa/Fc_Q_DEB.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)




plot_dotty1<-plot_dotty(pars[,1:10], unlist(stream_ann$`41 Alcantud`/var_ann$`41 Alcantud`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Alcantud/Q_P_Carb.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,11:20], unlist(stream_ann$`41 Alcantud`/var_ann$`41 Alcantud`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Alcantud/Q_P_DEA.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,21:30],  unlist(stream_ann$`41 Alcantud`/var_ann$`41 Alcantud`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Alcantud/Q_P_DEB.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)

plot_dotty2<-plot_dotty(pars[,1:10], unlist(aqu_ann$`41 Alcantud`/stream_ann$`41 Alcantud`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Alcantud/Fc_Q_Carb.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,11:20],  unlist(aqu_ann$`41 Alcantud`/stream_ann$`41 Alcantud`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Alcantud/Fc_Q_DEA.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,21:30],  unlist(aqu_ann$`41 Alcantud`/stream_ann$`41 Alcantud`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Alcantud/Fc_Q_DEB.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)



plot_dotty1<-plot_dotty(pars[,1:10], unlist(stream_ann$`45 Priego-Escabas`/var_ann$`45 Priego-Escabas`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Escabas/Q_P_Carb.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,11:20], unlist(stream_ann$`45 Priego-Escabas`/var_ann$`45 Priego-Escabas`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Escabas/Q_P_DEA.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty1<-plot_dotty(pars[,21:30],  unlist(stream_ann$`45 Priego-Escabas`/var_ann$`45 Priego-Escabas`$pcp), "Q/p")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Escabas/Q_P_DEB.png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)

plot_dotty2<-plot_dotty(pars[,1:10], unlist(aqu_ann$`45 Priego-Escabas`/stream_ann$`45 Priego-Escabas`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Escabas/Fc_Q_Carb.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,11:20],  unlist(aqu_ann$`45 Priego-Escabas`/stream_ann$`45 Priego-Escabas`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Escabas/Fc_Q_DEA.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(pars[,21:30],  unlist(aqu_ann$`45 Priego-Escabas`/stream_ann$`45 Priego-Escabas`), "F_C/Q")
#ggsave("C:/Jose_ACMA/Model1/Resultados/Resultados/Priego Escabas/Fc_Q_DEB.png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)



#### Hard Calibration process####    
#Upload results
sim_cha_list <- read_rds("C:/Users/Portatil_ACMA/Desktop/desk/stream_day.rds")
sim_gw_list <- read_rds("C:/Users/Portatil_ACMA/Desktop/desk/aqu_day.rds")
obs_flo<- read_rds("C:/Jose_ACMA/Model1/Simul_Datos/CaudalesObserv/HC/Obs_flo11_14.rds")

#### Performance metrics ####
# Estimate performance metrics for each sub-catchment 
names<-names(sim_cha_list)
tibs <- list()
stat_list <- list()
pllot <- list()
nse <- c()
r2_ch <- c()
pbia <- c()
rmse <- c()
for(i in 1:length(sim_cha_list)){
  
  sim <- sim_cha_list[[i]]
  obs <- obs_flo[[i]][,c("obs_flow","date")]
  numcol<-ncol(sim)-1
  
  tibb<- sim%>% left_join(., y = obs, by = "date") %>% .[,c(1, numcol+2, 2:(numcol+1))] 
  tibs[[i]] <-tibb
  
  nses <- c()
  pbias <- c()
  r2_chs <- c()
  rmses <- c()
  
  for(n in 1:(numcol)){
    nse <- NSE(tibs[[i]][,2+n], tibs[[i]][,2])
    r2_ch <-rsq_r2(tibs[[i]][,2+n], tibs[[i]][,2])
    pbia <- pbias(tibs[[i]][,2+n], tibs[[i]][,2])
    rmse <- rmse(tibs[[i]][,2+n], tibs[[i]][,2])
    nses <- c(nses, nse)
    r2_chs <- c(r2_chs,r2_ch)
    pbias <- c(pbias, pbia)
    rmses <- c(rmses,rmse)
  }
  
  
  stats <- tibble(run = seq(1,numcol, 1),nses, r2_chs, pbias, rmses)
  stat_list[[i]] <- stats
  stat_violin <-  stats %>% .[2:5] %>% pivot_longer(., cols = starts_with(c("r", "n", "p"))) 
  

  pllot[[i]] <- ggplot(stat_violin, aes(y = value, x = name, fill = name))+geom_violin()+ facet_wrap(facets = vars(name), scales = "free")+
    theme(legend.position = "no")+ ggtitle((names[i]))
  
  ##ggsave(pllot, filename =  paste("metrics",i,subbasins_str[i], sep = "_", ".jpg"),
  #path = "C:/Jose_ACMA/Model1/FinalPlots/Metrics/Round3/", device = "jpg")
}          


#### Rankings ####
#Bests configurations per sub-catchments and simulations streamflow plots 
gw_flow_ann<- read_rds ("C:/Users/Portatil_ACMA/Desktop/desk/aqu_ann.rds")
str_flow_ann<- read_rds ("C:/Users/Portatil_ACMA/Desktop/desk/stream_ann.rds")
names<-names(sim_cha_list)
names<-c("Peralejos","Huete","La Peraleja","Priego-Trabaque","Taravillas","Ventosa","Alcantud","Priego-Escabas","Trillo")
metrics_list1<-list()
plot<-list()
ranking10_metricas<-list()
ranking10_gw<-list()
best_runs_vec<-list()
for(i in 1:length(sim_cha_list)){
  sim <- sim_cha_list[[i]]
  obs <- obs_flo[[i]]
  
  #gw <- gw_flow_ann[[i]]
  #str <- str_flow_ann[[i]]
  
  best_runs <-  tibble(run = stat_list[[i]]$run,     
                       nse = stat_list[[i]]$nses,        
                       r2 = stat_list[[i]]$r2_chs,
                       rmses = stat_list[[i]]$rmses,
                       pbias = stat_list[[i]]$pbias,
                       rank_nse = rank(stat_list[[i]]$nses), 
                       rank_r2 = rank(stat_list[[i]]$r2_chs),  
                       rank_rmses = rank(-abs(stat_list[[i]]$rmses)),
                       rank_pbias = rank(-abs(stat_list[[i]]$pbias)),
                       rank_run = rank_pbias+rank_r2 +rank_rmses+rank_nse) %>%  
    dplyr::arrange(., desc(rank_run)) %>% .[c(1:20),] 
  
  ranking10_metricas[[i]]<-best_runs
  best_runs_num<-best_runs$run
  
  #gw_cont <- gw / str %>% as.data.frame(.) 
  #best_gwcont <- gw_cont[best_runs_num] %>% as.tibble(.) %>% t(.)

  ranking10_gw[[i]] <- best_runs %>% cbind(best_gwcont) 
  best_runs_vec[[i]] <-best_runs_num %>% gsub("[^0-9]", "", .) %>% as.numeric(.)
  
  unique_names  <- colnames(sim[-1]) %>% c("Observed flow")
  colors <- ifelse(unique_names == "Observed flow", "black", scales::hue_pal()(length(unique_names))) %>% c("black")
  
  plot_all<- sim %>% select(1, best_runs_vec[[i]]+1)  %>% 
    pivot_longer(., cols=-date) %>% 
    left_join(., obs_flo[[i]], by = "date") 
  
  plot_hidr<-ggplot(plot_all, aes(x = date , y = value   , color = name )) +
    geom_line(data =obs_flo[[i]] ,aes(x = date, y = obs_flow, color = "Observed flow"))+ 
    scale_color_manual(name = "Series",
                       values = setNames(colors, unique_names),
                       breaks = unique_names)+
    geom_line()+
    ggtitle(names[i])
  
  plot[[i]]<-ggplotly(plot_hidr)
  
  ##ggsave(plot_hidr, filename =  paste("Hidrograph",i,names[i], sep = "_", ".jpg"),
  #path = "C:/Jose_ACMA/  ", device = "jpg"  )
}
plot[[9]]
 
#Table with best simulations of 9 sub-catchments
best_sims<-as.data.frame(best_runs_vec) %>% setnames(names)

#Create a table of frequencies for each value in the entire data frame
value_counts <- best_sims %>%
  unlist() %>% 
  table() %>%
  as.data.frame()

# Rename the columns to "Value" and "Frequency"
colnames(value_counts) <- c("Value", "Frequency")

#Find in which columns each value appears
value_columns <- lapply(value_counts$Value, function(x) {
  cols <- names(best_sims)[apply(best_sims, 2, function(col) x %in% col)]
  paste(cols, collapse = ", ")
})

# Add a new column with the list of columns where each value appears
value_counts$Columns <- value_columns 

#Sort the values by their frequency in descending order
value_rep <- value_counts %>%  
  dplyr::arrange(., desc(Frequency)) 

####Performance metrics Sccaterplots####
#Sccaterplots of permformance metrics vs parameter range (Hard Calibraion parameters)#   
pars<-read_csv("C:/Users/Portatil_ACMA/Desktop/desk/Param.csv") %>% .[-1]

#Carbonatada
pars_CRB <- pars[,c(1:15)]
pbias_plot_dotty<- stat_list[[1]]$pbias
rmse_plot_dotty <- stat_list[[1]]$rmses
r2_plot_dotty   <- stat_list[[1]]$r2_chs
nse_plot_dotty  <- stat_list[[1]]$nses

plot_dotty1<-plot_dotty(par = pars_CRB, crit = pbias_plot_dotty, "pbias")
#ggsave("C:/Jose_ACMA//pbias[8].png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(par = pars_CRB, crit = rmse_plot_dotty,  "rmse")
#ggsave("C:/Jose_ACMA//rmse[8].png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty3<-plot_dotty(par = pars_CRB, crit = r2_plot_dotty, "r2")#+ ylim(c(0.05,0.35))
#ggsave("C:/Jose_ACMA//r2[8].png",plot = plot_dotty3,device = "png",width = 15 ,height = 9)
plot_dotty4<-plot_dotty(par = pars_CRB, crit = nse_plot_dotty,   "nse") #+ ylim(c(-40,0))
#ggsave("C:/Jose_ACMA//nse[8].png",plot = plot_dotty4,device = "png",width = 15 ,height = 9)

#DetriticasAlta
pars_DTA <- pars[,c(16:30)]
pbias_plot_dotty <-  stat_list[[3]]$pbias
rmse_plot_dotty  <-  stat_list[[3]]$rmses
r2_plot_dotty    <-  stat_list[[3]]$r2_chs
nse_plot_dotty   <-  stat_list[[3]]$nses

plot_dotty1<-plot_dotty(par = pars_DTA, crit = pbias_plot_dotty, "pbias")
#ggsave("C:/Jose_ACMA//pbias[3].png",plot = plot_dotty1,device = "png",width = 15 ,height = 9)
plot_dotty2<-plot_dotty(par = pars_DTA, crit = rmse_plot_dotty,  "rmse")
#ggsave("C:/Jose_ACMA//rmse[3].png",plot = plot_dotty2,device = "png",width = 15 ,height = 9)
plot_dotty3<-plot_dotty(par = pars_DTA, crit = r2_plot_dotty,    "r2")#+ ylim(c(0.05,0.35))
#ggsave("C:/Jose_ACMA//r2[3].png",plot = plot_dotty3,device = "png",width = 15 ,height = 9)
plot_dotty4<- plot_dotty(par = pars_DTA, crit = nse_plot_dotty,   "nse")
#ggsave("C:/Jose_ACMA//nse[3].png",plot = plot_dotty4,device = "png",width = 15 ,height = 9)

#Mix
pars_mix <- pars[,c(31:45)]
pbias_plot_dotty <- stat_list[[5]]$pbias
rmse_plot_dotty <-  stat_list[[5]]$rmses
r2_plot_dotty <-    stat_list[[5]]$r2_chs
nse_plot_dotty <-   stat_list[[5]]$nses

plot_dotty3<-plot_dotty(par = pars_mix, crit = pbias_plot_dotty, "pbias")
#ggsave("C:/Jose_ACMA/pbias[5].png",plot = plot_dotty3,device = "png",width = 15 ,height = 9)
plot_dotty3<-plot_dotty(par = pars_mix, crit = rmse_plot_dotty,  "rmse")
#ggsave("C:/Jose_ACMA/rmse[5].png",plot = plot_dotty3,device = "png",width = 15 ,height = 9)
plot_dotty3<-plot_dotty(par = pars_mix, crit = r2_plot_dotty,    "r2")#+ ylim(c(0,0.25))
#ggsave("C:/Jose_ACMA/r2[5].png",plot = plot_dotty3,device = "png",width = 15 ,height = 9)
plot_dotty3<-plot_dotty(par = pars_mix, crit = nse_plot_dotty,   "nse") + ylim(c(-5,0.5))
#ggsave("C:/Jose_ACMA/nse[5].png",plot = plot_dotty3,device = "png",width = 15 ,height = 9)


#ParCuenca
pars_mix <- pars[,c(46:47)]
pbias_plot_dotty <- stat_list[[1]]$pbias
rmse_plot_dotty <-  stat_list[[1]]$rmses
r2_plot_dotty <-    stat_list[[1]]$r2_chs
nse_plot_dotty <-   stat_list[[1]]$nses
plot_dotty3<-plot_dotty(par = pars_mix, crit = pbias_plot_dotty, "pbias")
#ggsave("C:/Jose_ACMA/pbias[1].png",plot = plot_dotty3,device = "png",width = 15 ,height = 9)
plot_dotty3<-plot_dotty(par = pars_mix, crit = rmse_plot_dotty,  "rmse")
#ggsave("C:/Jose_ACMA/rmse[1].png",plot = plot_dotty3,device = "png",width = 15 ,height = 9)
plot_dotty3<-plot_dotty(par = pars_mix, crit = r2_plot_dotty,    "r2")
#ggsave("C:/Jose_ACMA/r2[1].png",plot = plot_dotty3,device = "png",width = 15 ,height = 9)
plot_dotty3<-plot_dotty(par = pars_mix, crit = nse_plot_dotty,   "nse")
#ggsave("C:/Jose_ACMA/nse[1].png",plot = plot_dotty3,device = "png",width = 15 ,height = 9)



####Selection best perfomance simulations to validate####
#Table with best simulations of 9 sub-catchments
best_sims<-as.data.frame(best_runs_vec) %>% setnames(names)

#Create a table of frequencies for each value in the entire data frame
value_counts <- best_sims %>%
  unlist() %>% 
  table() %>%
  as.data.frame()

# Rename the columns to "Value" and "Frequency"
colnames(value_counts) <- c("Value", "Frequency")

#Find in which columns each value appears
value_columns <- lapply(value_counts$Value, function(x) {
  cols <- names(best_sims)[apply(best_sims, 2, function(col) x %in% col)]
  paste(cols, collapse = ", ")
})

# Add a new column with the list of columns where each value appears
value_counts$Columns <- value_columns 

#Sort the values by their frequency in descending order
value_rep <- value_counts %>%  
  dplyr::arrange(., desc(Frequency)) 
####Plot best simulation. Direct and gw flow in streamflow####
sim_gw_list <- read_rds("C:/Users/Portatil_ACMA/Desktop/desk/aqu_day.rds")

area_region<-c(412485223, 360468848, 255388734, 387975279, 206168246, 943975003, 557866983, 343024862, 3276668547) 
sim_cha_list <- read_rds("C:/Users/Portatil_ACMA/Desktop/desk/stream_day.rds")
for(i in 1:length(sim_cha_list)){
  sim_cha_list[[i]]<-sim_cha_list[[i]]%>% mutate(across(-one_of("date"), ~ . * (86400*1000/ area_region[i])))}


obs_flo<- read_rds("C:/Jose_ACMA/Model1/Simul_Datos/CaudalesObserv/HC/Obs_mm_19_9sub.rds")
for(i in 1:8){
  obs_flo [[i]]<- obs_flo[[i]] %>%  filter(date %in% (ymd("2011-01-01"):ymd("2014-12-31")))}


for(i in 1:8){ 
plot_sim<- sim_cha_list[[i]] %>% select(1, best_runs_vec[[i]][1]+1) %>% setnames(c("date","streamf"))

plot_gw<- sim_gw_list[[i]] %>% select(1, best_runs_vec[[i]][1]+1)  %>% 
  left_join(., plot_sim, by = "date") %>% 
  left_join(., obs_flo[[i]], by = "date") %>% setnames(c("date", "gwcha","stream","cod","obs_flow"))



plot_hidr<-ggplot(plot_gw) +
  geom_area(aes(x = date, y = stream, fill = "Direct flow"), position = "identity") +
  geom_area(aes(x = date, y = gwcha, fill = "GW flow"), position = "identity") +
  geom_line(aes(x = date, y = obs_flow, linetype = "Observed flow", color = "Observed flow")) +
  geom_line(aes(x = date, y = stream , linetype = "Simulated flow", color = "Simulated flow"))+
  labs(fill = NULL, color = NULL, linetype = NULL) +
  theme_minimal() +
  scale_linetype_manual(values = c("Observed flow" = "dashed", "Simulated flow" = "solid")) +
  scale_color_manual(values = c("Observed flow" = "black" ,"Simulated flow" = "blue")) +
  labs(title = NULL, x = "Date", y = expression(mm)) +
  theme(
    legend.position = c(0.25, 1),
    legend.justification = c(1.1, 1.1),
    legend.box.just = "left",
    legend.text = element_text(size = 30),
    legend.key.size = unit(2, 'lines'),
    axis.text = element_text(size = 20, colour = "black"),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1),
    axis.title = element_text(size = 20, colour = "black"),
    panel.background = element_rect(fill = NULL , color = "black", size = 0.5),
    legend.key = element_blank(),
    #plot.title = element_text(hjust = 0.01, vjust = -7, size = 30, face = "bold")+
    plot.margin = margin(t = 30, r = 5, b = 5, l = 5)
  ) +
  
  scale_x_date(breaks = seq(as.Date("2011-01-01"), as.Date("2020-01-01"), by = "years"),
               date_labels = "%Y") +
  coord_cartesian(xlim = c(min(plot_gw$date)+120, max(plot_gw$date))-62)


ggsave(filename = paste(i,"Fcha2", sep = "_", ".jpg"), path =  "C:/Users/Portatil_ACMA/Desktop/" ,plot =plot_hidr, 
       device = "jpg", width = 16, height = 10,dpi = 300)
       #bg = "transparent"
}

