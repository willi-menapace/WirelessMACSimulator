#
#
# Author: Willi Menapace 199890 <willi.menapace@gmail.com>
#         
# Contributions by: Michele Segata <msegata@disi.unitn.it>
#
# Project: Simulating and Modeling Wireless MAC Protocols
#
#

required.packages = c("plyr", "doMC", "ggplot2", "data.table")

#Automatically installs missing packages
list.of.packages = required.packages
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) 

lapply(required.packages, require, character.only=T)

library(plyr)
library(doMC)
library(ggplot2)
registerDoMC(cores=detectCores())

get.bianchi.values = function(stations.count = 6, slot.size = 0.00001, average.packet.size = 766, tx.rate = 1000000, window.values = 1:1024) {
  
  saturation.values = list();
  
  for(window.size in window.values) {
    
    tau = 2 / (window.size + 1)
    p.transmission = 1 - (1 - tau) ^ stations.count
    p.solo = stations.count * tau * (1 - tau) ^ (stations.count - 1) * p.transmission
    
    average.info.per.slot = stations.count * tau * ((1 - tau) ^ (stations.count - 1)) * average.packet.size
    average.slot.length = (1 - p.transmission) * slot.size + p.transmission * p.solo * average.packet.size / tx.rate + p.transmission * (1 - p.solo) * average.packet.size / tx.rate
    
    saturation.bandwidth = average.info.per.slot / average.slot.length
    
    saturation.bandwidth.mbitps = saturation.bandwidth * 8 / 1024 / 1024
    
    saturation.values[[length(saturation.values) + 1]] = saturation.bandwidth.mbitps
  }
  
  results = data.frame(window = window.values, tr =  rapply(saturation.values, c))
  results$type = "model"
  
  return(results)
  
}

window.size = 128
n.nodes = 6
bitrate.mbps = 8

throughput = read.csv(paste('throughput_', n.nodes, '_', window.size, '.csv', sep=''))

throughput = ddply(throughput, c('lambda', 'ol'), function(x) {
  return(data.frame(tr=mean(x$tr)))
}, .parallel=T)

throughput$tr = throughput$tr * (n.nodes) / (n.nodes - 1)
throughput$tr = throughput$tr / bitrate.mbps
throughput$ol = throughput$ol / bitrate.mbps

throughput$type = "measured"

saturation.throughput = get.bianchi.values(stations.count = n.nodes, window.values = window.size)$tr[[1]] / bitrate.mbps

model.throughput = data.frame(lambda = list(), ol = list(), tr = list())

for(current.ol in throughput$ol) {
  model.throughput = rbind(model.throughput, data.frame(lambda = -1, ol = current.ol, tr = min(saturation.throughput, current.ol)))
}
model.throughput$type = "model"

throughput = rbind(throughput, model.throughput)

div = 3
p = ggplot(throughput, aes(x=ol, y=tr)) +
  geom_line(aes(linetype=type)) +
  geom_point() +
  scale_linetype_manual(values=c("dotted", "solid")) +
  xlab('offered load (Times C)') +
  ylab('channel throughput (Times C)') +
  labs(color="receiver node") +
  ylim(c(0, 1))
ggsave(paste('linear_model_thr_', n.nodes, "_", window.size, '.pdf', sep=''), width=16/div, height=9/div)
print(p)


