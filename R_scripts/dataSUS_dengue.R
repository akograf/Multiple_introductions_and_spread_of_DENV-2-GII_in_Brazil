#remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)

#A variavel sorotipo só aparece em 2007, mas outras variaveis não estão presentes antes de 2014. 
#Por isso precisei fazer uma versão simples
dengueBR_simples <- fetch_datasus(year_start = 2018, year_end = 2022, information_system = "SINAN-DENGUE",
                    vars = c("ID_AGRAVO","DT_NOTIFIC","SG_UF_NOT","ID_MUNICIP","ID_REGIONA","DT_SIN_PRI",
                            "SG_UF", "ID_MN_RESI","ID_RG_RESI", "ID_PAIS", "DT_INVEST", "SOROTIPO"))

#Vars completude
table(is.na(dengueBR_simples$SOROTIPO))
table(is.na(dengueBR_simples$DT_NOTIFIC))
table(is.na(dengueBR_simples$DT_SIN_PRI))
sorotipados = dengueBR_simples[c(!is.na(dengueBR_simples$SOROTIPO)),]
sorotipos_table = as.data.frame(table(sorotipados$SOROTIPO))
sorotipados_porUF = as.data.frame(table(sorotipados$SG_UF_NOT))

#Transformando código de estado em UF
sorotipados$SG_UF_NOT = ifelse(sorotipados$SG_UF_NOT == 12, 'AC',
                                     ifelse(sorotipados$SG_UF_NOT == 27, 'AL',
                                            ifelse(sorotipados$SG_UF_NOT == 16, 'AP',
                                                   ifelse(sorotipados$SG_UF_NOT == 13, 'AM',
                                                          ifelse(sorotipados$SG_UF_NOT == 29, 'BA',
                                                                 ifelse(sorotipados$SG_UF_NOT == 23, 'CE',
                                                                        ifelse(sorotipados$SG_UF_NOT == 53, 'DF',
                                                                               ifelse(sorotipados$SG_UF_NOT == 32, 'ES',
                                                                                      ifelse(sorotipados$SG_UF_NOT == 52, 'GO',
                                                                                             ifelse(sorotipados$SG_UF_NOT == 21,'MA',
                                                                                                    ifelse(sorotipados$SG_UF_NOT == 51, 'MT',
                                                                                                           ifelse(sorotipados$SG_UF_NOT == 50, 'MS',
                                                                                                                  ifelse(sorotipados$SG_UF_NOT == 31, 'MG',
                                                                                                                         ifelse(sorotipados$SG_UF_NOT == 'NA', 'NA',
                                                                                                                                ifelse(sorotipados$SG_UF_NOT == 15, 'PA',
                                                                                                                                       ifelse(sorotipados$SG_UF_NOT == 25, 'PB',
                                                                                                                                              ifelse(sorotipados$SG_UF_NOT == 41, 'PR',
                                                                                                                                                     ifelse(sorotipados$SG_UF_NOT == 26, 'PE',
                                                                                                                                                            ifelse(sorotipados$SG_UF_NOT == 22, 'PI',
                                                                                                                                                                   ifelse(sorotipados$SG_UF_NOT == 33, 'RJ',
                                                                                                                                                                          ifelse(sorotipados$SG_UF_NOT == 24, 'RN',
                                                                                                                                                                                 ifelse(sorotipados$SG_UF_NOT == 43, 'RS',
                                                                                                                                                                                        ifelse(sorotipados$SG_UF_NOT == 11, 'RO',
                                                                                                                                                                                               ifelse(sorotipados$SG_UF_NOT == 14, 'RR',
                                                                                                                                                                                                      ifelse(sorotipados$SG_UF_NOT == 42, 'SC',
                                                                                                                                                                                                             ifelse(sorotipados$SG_UF_NOT == 35, 'SP',
                                                                                                                                                                                                                    ifelse(sorotipados$SG_UF_NOT == 28, 'SE',
                                                                                                                                                                                                                           ifelse(sorotipados$SG_UF_NOT == 17, 'TO', 999))))))))))))))))))))))))))))


sorotipados_porUF = as.data.frame(table(sorotipados$SG_UF_NOT))

#Frequências por estado - contagem por ano
library(lubridate)
sorotipados$ANO_NOTIFIC = floor_date(sorotipados$DT_NOTIFIC, unit = "year")
sorotipoFreq_ano_uf=xtabs(data = sorotipados, ~ANO_NOTIFIC+SOROTIPO+SG_UF_NOT) 
sorotipoFreq_ano_uf_cont = as.data.frame(sorotipoFreq_ano_uf) #Trasforma a tabela em df
sorotipoFreq_ano_uf_cont$ANO_NOTIFIC = as.Date(sorotipoFreq_ano_uf_cont$ANO_NOTIFIC) #Ajeita o formato das datas

sorotipoFreq_ano_uf_prop=as.data.frame(prop.table(sorotipoFreq_ano_uf, margin = c(1,3))) #Cria um df com as proporções
sorotipoFreq_ano_uf_prop$Perc = sorotipoFreq_ano_uf_prop$Freq *100 #cria coluna com porcentagem
sorotipoFreq_ano_uf_prop$ANO_NOTIFIC = as.Date(sorotipoFreq_ano_uf_prop$ANO_NOTIFIC) #Ajeita o formato das datas

#Frequências por estado - contagem por mês
library(lubridate)
sorotipados$MES_NOTIFIC = floor_date(sorotipados$DT_NOTIFIC, unit = "month")
sorotipoFreq_mes_uf=xtabs(data = sorotipados, ~MES_NOTIFIC+SOROTIPO+SG_UF_NOT) 
sorotipoFreq_mes_uf_cont = as.data.frame(sorotipoFreq_mes_uf) #Trasforma a tabela em df
sorotipoFreq_mes_uf_cont$MES_NOTIFIC = as.Date(sorotipoFreq_mes_uf_cont$MES_NOTIFIC) #Ajeita o formato das datas

sorotipoFreq_mes_uf_prop=as.data.frame(prop.table(sorotipoFreq_mes_uf, margin = c(1,3))) #Cria um df com as proporções
sorotipoFreq_mes_uf_prop$Perc = sorotipoFreq_mes_uf_prop$Freq *100 #cria coluna com porcentagem
sorotipoFreq_mes_uf_prop$MES_NOTIFIC = as.Date(sorotipoFreq_mes_uf_prop$MES_NOTIFIC) #Ajeita o formato das datas

FiveYears_byMonth = subset(sorotipoFreq_mes_uf_prop, MES_NOTIFIC >= "2018-01-01")


#Organizando a ordem dos estadoas
sorotipoFreq_ano_uf_prop$SG_UF_NOT <- factor(sorotipoFreq_ano_uf_prop$SG_UF_NOT, 
                                             levels = c("AC", "AM", "RR","AP", "PA","RO","TO",
                                                        "MA","CE", "PI", "RN", "PB", "PE", "AL", "SE", "BA",
                                                        "ES","MG", "RJ", "SP", 
                                                        "MT", "MS", "GO", "DF",
                                                        "PR", "SC", "RS"))


##Gráfico de linhas suavisadas com % por ano por UF
FiveYears = subset(sorotipoFreq_ano_uf_prop, ANO_NOTIFIC >= "2018-01-01" & SG_UF_NOT != "AP") #Removi Amapá que quase não tinha dados para esse período
Only2022 = subset(sorotipoFreq_ano_uf_prop, ANO_NOTIFIC >= "2022-01-01")

uf3 = ggplot(data = FiveYears, aes(x = ANO_NOTIFIC, y = Perc, color = SOROTIPO))+
  stat_smooth(geom = 'line', method = 'loess', span = 0.7, alpha = 0.8, linewidth = 0.8) #Para trabalhar com linhas suavisadas
  #stat_smooth(geom = 'line', alpha = 0.8, linewidth = 0.8) #Para trabalhar com linhas suavisadas
uf3 = uf3 + scale_color_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3")) 
uf3 = uf3 + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + ylab("DENV serotypes relative frequency (%)") 
#uf3 = uf3 + ylim(c(0, 105))
uf3 = uf3 + theme_minimal()+ theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=45), legend.position = "bottom") + labs(color = "Serotype") 
uf3 = uf3 + facet_wrap(~SG_UF_NOT)
uf3

#Gráfico de barras com contagem por mes -> calcular a % por UF fica muito ruim

d = ggplot(data = sorotipoFreq_mes_uf_cont, aes(x=MES_SIN_PRI, y=Freq, fill = SOROTIPO))+
  geom_bar(position="stack", stat="identity", alpha = 0.5)+
  scale_fill_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3")) +
  ylab("DENV cases") + labs(fill = "DENV serotype")
d = d + scale_x_date(date_breaks = "3 month", date_labels = "%b-%y") 
d = d + theme_minimal()+ theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=45))
d = d + facet_wrap(~SG_UF_NOT, scales = "free")
d


###Trabalhando com número de casos nos últimos 5 anos
#Transformando código de estado em UF
dengueBR_simples$SG_UF_NOT = ifelse(dengueBR_simples$SG_UF_NOT == 12, 'AC',
                               ifelse(dengueBR_simples$SG_UF_NOT == 27, 'AL',
                                      ifelse(dengueBR_simples$SG_UF_NOT == 16, 'AP',
                                             ifelse(dengueBR_simples$SG_UF_NOT == 13, 'AM',
                                                    ifelse(dengueBR_simples$SG_UF_NOT == 29, 'BA',
                                                           ifelse(dengueBR_simples$SG_UF_NOT == 23, 'CE',
                                                                  ifelse(dengueBR_simples$SG_UF_NOT == 53, 'DF',
                                                                         ifelse(dengueBR_simples$SG_UF_NOT == 32, 'ES',
                                                                                ifelse(dengueBR_simples$SG_UF_NOT == 52, 'GO',
                                                                                       ifelse(dengueBR_simples$SG_UF_NOT == 21,'MA',
                                                                                              ifelse(dengueBR_simples$SG_UF_NOT == 51, 'MT',
                                                                                                     ifelse(dengueBR_simples$SG_UF_NOT == 50, 'MS',
                                                                                                            ifelse(dengueBR_simples$SG_UF_NOT == 31, 'MG',
                                                                                                                   ifelse(dengueBR_simples$SG_UF_NOT == 'NA', 'NA',
                                                                                                                          ifelse(dengueBR_simples$SG_UF_NOT == 15, 'PA',
                                                                                                                                 ifelse(dengueBR_simples$SG_UF_NOT == 25, 'PB',
                                                                                                                                        ifelse(dengueBR_simples$SG_UF_NOT == 41, 'PR',
                                                                                                                                               ifelse(dengueBR_simples$SG_UF_NOT == 26, 'PE',
                                                                                                                                                      ifelse(dengueBR_simples$SG_UF_NOT == 22, 'PI',
                                                                                                                                                             ifelse(dengueBR_simples$SG_UF_NOT == 33, 'RJ',
                                                                                                                                                                    ifelse(dengueBR_simples$SG_UF_NOT == 24, 'RN',
                                                                                                                                                                           ifelse(dengueBR_simples$SG_UF_NOT == 43, 'RS',
                                                                                                                                                                                  ifelse(dengueBR_simples$SG_UF_NOT == 11, 'RO',
                                                                                                                                                                                         ifelse(dengueBR_simples$SG_UF_NOT == 14, 'RR',
                                                                                                                                                                                                ifelse(dengueBR_simples$SG_UF_NOT == 42, 'SC',
                                                                                                                                                                                                       ifelse(dengueBR_simples$SG_UF_NOT == 35, 'SP',
                                                                                                                                                                                                              ifelse(dengueBR_simples$SG_UF_NOT == 28, 'SE',
                                                                                                                                                                                                                     ifelse(dengueBR_simples$SG_UF_NOT == 17, 'TO', 999))))))))))))))))))))))))))))


table(dengueBR_simples$SG_UF_NOT)

#Checando datas
dengueBR_simples$DT_SIN_PRI = as.Date(dengueBR_simples$DT_SIN_PRI)
summary(dengueBR_simples$DT_SIN_PRI)
head(dengueBR_simples[order(dengueBR_simples$DT_SIN_PRI, decreasing=F), ], 50)

dengueBR_simples$DT_NOTIFIC = as.Date(dengueBR_simples$DT_NOTIFIC)
summary(dengueBR_simples$DT_NOTIFIC)
head(dengueBR_simples[order(dengueBR_simples$DT_NOTIFIC, decreasing=F), ], 50)

#Removendo datas de notificação antes de 2018
dengueBR_simples = subset(dengueBR_simples, DT_NOTIFIC >= "2018-01-01")
summary(dengueBR_simples$DT_NOTIFIC)

#Frequências de casos por mes por estado
library(lubridate)
dengueBR_simples$MES_NOTIFIC = floor_date(dengueBR_simples$DT_NOTIFIC, unit = "month")
casos_mes=xtabs(data = dengueBR_simples, ~MES_NOTIFIC+SG_UF_NOT) 
casos_mes_cont = as.data.frame(casos_mes) #Trasforma a tabela em df
casos_mes_cont$MES_NOTIFIC = as.Date(casos_mes_cont$MES_NOTIFIC) #Ajeita o formato das datas

##Calculando casos por 100k hab em cada estado
#Lista de UF
UFs = UFpops[,1]
#DF vazio para conter os resultados
casos_mes_cont2 = data.frame(matrix(ncol=4, nrow = 0))
colnames(casos_mes_cont2) <- c("MES_NOTIFIC","SG_UF_NOT","Freq", "casos100k")

for(i in 1:27){
  i_casos_mes_cont = subset(casos_mes_cont, SG_UF_NOT==UFs[i])
  i_casos_mes_cont$casos100k = (i_casos_mes_cont$Freq/(UFpops[i,2]))*100000
  casos_mes_cont2 = rbind(casos_mes_cont2, i_casos_mes_cont)
  }
rm(i_casos_mes_cont)

#Organizando a ordem dos estadoas
casos_mes_cont2$SG_UF_NOT <- factor(casos_mes_cont2$SG_UF_NOT, 
                                             levels = c("AC", "AM", "RR","AP", "PA","RO","TO",
                                                        "MA","CE", "PI", "RN", "PB", "PE", "AL", "SE", "BA",
                                                        "ES","MG", "RJ", "SP", 
                                                        "MT", "MS", "GO", "DF",
                                                        "PR", "SC", "RS"))

#Plot casos por mês 
uf4 = ggplot(data = casos_mes_cont2, aes(x=MES_NOTIFIC, y=casos100k))+
  geom_bar(position="stack", stat="identity", alpha = 0.5)+
  ylab("Dengue cases/100k hab.")
uf4 = uf4 + scale_x_date(date_breaks = "1 year", date_labels = "%Y") 
uf4 = uf4 + theme_minimal()+ theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=45))
uf4 = uf4 + facet_wrap(~SG_UF_NOT, scales = "free")
uf4

#Adicionando 2023 para a linha das proporções cobrir todo o ano de 2022
prop2023 = subset(FiveYears,ANO_NOTIFIC == "2022-01-01")
prop2023$ANO_NOTIFIC = "2023-01-01"
FiveYears_ed = rbind(FiveYears, prop2023)

#Juntando casos e % de sorotipos
uf34 = ggplot()
uf34 = uf34 + geom_smooth(data = FiveYears_ed, aes(x = ANO_NOTIFIC, y = Perc, color = SOROTIPO), span = 0.7, se = F, size = 0.7)
uf34 = uf34 + geom_bar(data = casos_mes_cont2, aes(x=MES_NOTIFIC, y=casos100k/7.5), position="stack", stat="identity", alpha = 0.5)
uf34 = uf34 + scale_y_continuous(sec.axis = sec_axis(~.*7.5, name = "Confirmed Dengue cases / 100k hab."))
uf34 = uf34 + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + ylab("DENV serotypes relative frequency (%)") 
uf34 = uf34 + scale_color_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3")) 
uf34 = uf34 + theme_minimal()+ theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=45), legend.position = "bottom") + labs(color = "Serotype") 
uf34 = uf34 + facet_wrap(~SG_UF_NOT) #scales = "free_y")
uf34

#Re-scaling each country plot, saving in a list and plotting with plot_grid 
b_lst = list()
UFs_reorder = c("AC", "AM", "RR","AP", "PA","RO","TO",
                "MA","CE", "PI", "RN", "PB", "PE", "AL", "SE", "BA",
                "ES","MG", "RJ", "SP", 
                "MT", "MS", "GO", "DF",
                "PR", "SC", "RS")

for(i in 1:27){
  #subset by UF
  i_UF_casos = casos_mes_cont2[c(casos_mes_cont2$SG_UF_NOT==UFs_reorder[i]),]
  i_UF_serotipe = FiveYears_ed[c(FiveYears_ed$SG_UF_NOT==UFs_reorder[i]),]
  #factor to re-escalate the second y-axis is the maximum number of cases in an epiweek
  scaleFactor <- max(i_UF_casos$casos100k)/100 #Add some space in the y-axis to not cut the circles
  
  i_plot = ggplot()
  i_plot = ggplot()
  i_plot = i_plot + geom_smooth(data = i_UF_serotipe, aes(x = ANO_NOTIFIC, y = Perc, color = SOROTIPO), span = 0.7, se = F, size = 0.7)
  i_plot = i_plot + geom_bar(data = i_UF_casos, aes(x=MES_NOTIFIC, y=casos100k/scaleFactor), position="stack", stat="identity", alpha = 0.5)
  i_plot = i_plot + scale_y_continuous(sec.axis = sec_axis(~.*scaleFactor, name = "Confirmed Dengue cases / 100k hab."))
  i_plot = i_plot + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + ylab("DENV serotypes relative frequency (%)") 
  i_plot = i_plot + scale_color_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3")) + ggtitle(UFs_reorder[i], )
  i_plot = i_plot + theme_minimal()+ theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=45),
                                           axis.title.y = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)) 
  
  b_lst[[i]] <-ggplotGrob(i_plot) #Save to the list
}
i_plot

#Plot all together
library(cowplot)
p1 <- plot_grid(plotlist=b_lst, ncol = 4)
p1

#Selected states
b_lst_selected = b_lst[c(1,2,3,6,9,11,12,13,15,19,20,23,25,26,27)]
p2 <- plot_grid(plotlist=b_lst_selected, ncol = 5)
p2

##Numbers march 2021 a march 2023
studyPeriod = subset(dengueBR_simples,DT_NOTIFIC >= "2021-03-01" & DT_NOTIFIC <= "2023-03-31")
studyPeriod_sampledStates = studyPeriod[studyPeriod$SG_UF_NOT %in% c("AM", "PE", "SP","PR", "SC", "RS"),]
summary(studyPeriod_sampledStates$DT_NOTIFIC)
as.data.frame(table(studyPeriod_sampledStates$SG_UF_NOT))

#Cross-table sorotipospor estados
library(janitor)
Sorotipos = studyPeriod_sampledStates %>%
  tabyl(SG_UF_NOT, SOROTIPO, show_na = F) %>%
  adorn_totals(c("row","col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 1) %>%
  adorn_ns() %>%
  adorn_title()
Sorotipos
