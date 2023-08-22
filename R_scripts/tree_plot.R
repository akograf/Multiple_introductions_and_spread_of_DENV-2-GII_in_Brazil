#BiocManager::install("ggtree")

library(ggtree)
library(ggplot2)
library(ape)
library(treeio)
library(grid)
library(lubridate)
#TIME INFORMATION/SETTINGS - MRSD = most recent sampling date
mrsd = decimal_date(ymd("2023-03-12"))

###Load tree###
#MCC = read.nexus("./Gamma.GLM.epoch.MCC.tree")
MCCtree = read.beast(file = "./D2GII.AmClade.AsyBSSVS.ucld.MCC.tree")
#Removing a tip with strange behavior
#GammaMCC_ed=drop.tip(GammaMCC, tip = "Venezuela_Bol4715_2021-01-01")

Locations.col<-c("Brazil-AM"="#006d2c", "Brazil-AC"="#66c2a4", "Brazil-GO"="#d95f0e", "Brazil-PE"="#fe9929",
                 "Brazil-SP"="#ae017e", "Brazil-PR"="#08306b", "Brazil-SC"="#c6dbef", "Brazil-RS"="#6baed6",
                 "Peru"="#cb181d", "Bangladesh"="#fb6a4a")
                 


t = ggtree(MCCtree,size=1, alpha = 0.8, aes(color=location),
           mrsd="2023-03-12",as.Date = F,
           ladderize = T, right = F)
t = t + geom_tippoint(aes(fill=location), size=2, alpha=1) #Colore os tippoints
#t = t + geom_tiplab(size = 1)
t = t + geom_nodepoint(aes(subset=(posterior>=0.9), x=branch),size=0.5, color="black", alpha=1) #Node points posterior
#t = t + geom_nodelab(aes(x=branch, label=round(as.numeric(posterior),2)), vjust=-.5, size=1) #plota posterior no branch
#t = t + geom_nodelab(aes(x=branch, label=node)) #plota o número dos nodos
t = t + geom_range('height_0.95_HPD', color='gray', size=2, alpha=.5,  center = "height") #Adiciona barra de incerteza aos nodos, mas só funciona com as.Date=F
t = t + scale_color_manual(values = Locations.col)
#t = t + scale_x_date(date_breaks = "6 month", date_labels = "%b-%y") #Regula escala de tempo
t = t + scale_x_continuous(breaks = c(2016:2023))
t = t + geom_vline(xintercept = c(2016:2023), linetype="dotted", color = "gray", linewidth=0.5) #Add vertical lines
t = t + coord_cartesian(clip = "off") + theme_tree2(axis.text.x = element_text(angle=0))#plot.margin = margin(5,10,0,0))
t

#Extraindo dados da árvore para alguns nodos de interesse                                           
library(tidytree)
MCCtree.df = as_tibble(MCCtree) # transforma a árvore em um df

#Plota número dos nodos na árvore
tn = ggtree(MCCtree,size=0.5, alpha = 0.8, aes(color=location),
           mrsd="2023-03-12",as.Date = F,
           ladderize = T, right = F)
tn = tn + geom_nodelab(aes(x=branch, label=node))
tn = tn + geom_tiplab(size = 2)
tn

intros = c(128, 116, 97)

node128.probset = as.data.frame(MCCtree.df$location.set.prob[[128]])
colnames(node128.probset) <- c("ancestral.state.prob")
node128.probset$location = c("Brazil-SP","Brazil-AM","Brazil-GO","Brazil-PE","Brazil-PR","Brazil-AC","Brazil-RS","Peru","Brazil-SC")

node116.probset = as.data.frame(MCCtree.df$location.set.prob[[116]])
colnames(node116.probset) <- c("ancestral.state.prob")
node116.probset$location = c("Brazil-SP","Brazil-AM","Bangladesh","Brazil-GO","Brazil-PE","Brazil-PR","Brazil-AC","Peru","Brazil-RS","Brazil-SC")

node97.probset = as.data.frame(MCCtree.df$location.set.prob[[97]])
colnames(node97.probset) <- c("ancestral.state.prob")
node97.probset$location = c("Brazil-AM", "Brazil-AC", "Peru" )

node127.probset = as.data.frame(MCCtree.df$location.set.prob[[127]])
colnames(node80.probset) <- c("ancestral.state.prob")
node80.probset$location = c("Brazil-SP","Brazil-AM","Brazil-PR","Peru", "Brazil-RS","Brazil-SC")

node96.probset = as.data.frame(MCCtree.df$location.set.prob[[96]])
colnames(node85.probset) <- c("ancestral.state.prob")
node85.probset$location = c("Brazil-SP","Brazil-AM","Brazil-PR","Peru", "Brazil-RS","Brazil-SC")

node117.probset = as.data.frame(MCCtree.df$location.set.prob[[117]])
colnames(node117.probset) <- c("ancestral.state.prob")
node117.probset$location =  c("Brazil-SP","Brazil-AM","Bangladesh","Brazil-GO","Brazil-PE","Brazil-AC","Brazil-PR","Peru","Brazil-RS","Brazil-SC")

node123.probset = as.data.frame(MCCtree.df$location.set.prob[[123]])
colnames(node117.probset) <- c("ancestral.state.prob")
node117.probset$location =  c("Brazil-SP","Brazil-AM","Bangladesh","Brazil-GO","Brazil-PE","Brazil-AC","Brazil-PR","Peru","Brazil-RS","Brazil-SC")


#plot location.set.prob para os nodos de interesse e salvando como imagem 
a = ggplot(node128.probset, aes(x="", y=ancestral.state.prob, fill = location)) +
  geom_bar(stat="identity", width=1, show.legend = F) + scale_fill_manual(values = Locations.col) +
  coord_polar("y", start=0) +
  theme_void()   # remove background, grid, numeric labels
a
ggsave("./node128.probSet.png")

b = ggplot(node116.probset, aes(x="", y=ancestral.state.prob, fill = location)) +
  geom_bar(stat="identity", width=1, show.legend = F) + scale_fill_manual(values = Locations.col) +
  coord_polar("y", start=0) +
  theme_void()   # remove background, grid, numeric labels
b
ggsave("./node116.probSet.png")

c = ggplot(node117.probset, aes(x="", y=ancestral.state.prob, fill = location)) +
  geom_bar(stat="identity", width=1, show.legend = F) + scale_fill_manual(values = Locations.col) +
  coord_polar("y", start=0) +
  theme_void()   # remove background, grid, numeric labels
c
ggsave("./node117.probSet.png")

d = ggplot(node85.probset, aes(x="", y=ancestral.state.prob, fill = location)) +
  geom_bar(stat="identity", width=1, show.legend = F) + scale_fill_manual(values = Locations.col) +
  coord_polar("y", start=0) +
  theme_void()   # remove background, grid, numeric labels
d
ggsave("./node85.probSet.png")

e = ggplot(node59.probset, aes(x="", y=ancestral.state.prob, fill = location)) +
  geom_bar(stat="identity", width=1, show.legend = F) + scale_fill_manual(values = Locations.col) +
  coord_polar("y", start=0) +
  theme_void()   # remove background, grid, numeric labels
e
ggsave("./node59.probSet.png")

f = ggplot(node86.probset, aes(x="", y=ancestral.state.prob, fill = location)) +
  geom_bar(stat="identity", width=1, show.legend = F) + scale_fill_manual(values = Locations.col) +
  coord_polar("y", start=0) +
  theme_void()   # remove background, grid, numeric labels
f
ggsave("./node86.probSet.png")

#Criando um df com as coordenadas para iserir as imagens na árvore
node_plots = data.frame(node = as.integer(c("59","66", "79","80","85", "86")), 
                  pies = c("/Users/tgraf/OneDrive/Meus_projetos/Dengue/DENV-PR/DENV2_analise/Beast/node59.probSet.png",
                           "/Users/tgraf/OneDrive/Meus_projetos/Dengue/DENV-PR/DENV2_analise/Beast/node66.probSet.png",
                           "/Users/tgraf/OneDrive/Meus_projetos/Dengue/DENV-PR/DENV2_analise/Beast/node79.probSet.png",
                           "/Users/tgraf/OneDrive/Meus_projetos/Dengue/DENV-PR/DENV2_analise/Beast/node80.probSet.png",
                           "/Users/tgraf/OneDrive/Meus_projetos/Dengue/DENV-PR/DENV2_analise/Beast/node85.probSet.png",
                           "/Users/tgraf/OneDrive/Meus_projetos/Dengue/DENV-PR/DENV2_analise/Beast/node86.probSet.png"))

#Plotando árvore + anotações nos nodos
t %<+% node_plots + geom_nodelab(aes(image=pies), geom="image", alpha = 0.5) #alpha não parece mudar nada aqui


