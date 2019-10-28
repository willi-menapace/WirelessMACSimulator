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

bitrate.mbps = 8

#Command line argument, if present, indicates the results folder
args = commandArgs(trailingOnly = T)
if (length(args) != 0) {
    res.folder = args[1]
} else {
    res.folder = './'
}

#Possible packet states
PKT_RECEIVING = 0
PKT_RECEIVED = 1
PKT_CORRUPTED = 2
PKT_GENERATED = 3
PKT_QUEUE_DROPPED = 4

#Determine whether a string contains a parsable number"
is.number = function(string) {
    if (length(grep("^[[:digit:]]*$", string)) == 1)
        return (T)
    else
        return (F)
}

#Gets the list of files with a certain prefix and suffix in a folder
get.data.files = function(folder, suffix=".csv") {
    if (strsplit(suffix, '')[[1]][1] == '.')
        suffix = paste('\\', suffix, sep='')
    return(list.files(folder, pattern=paste('.*', suffix, sep='')))
}

#Splits the name of an output file by _ and extracts the values of simulation parameters
get.params = function(filename, fields) {
    p = strsplit(gsub(".csv", "", basename(filename)), "_")[[1]]
    #to add a column, we need to have something in the dataframe, so we add a
    #fake column which we remove at the end
    d = data.frame(todelete=1)
    for (f in 1:length(fields)) {
        v = p[f]
        if (is.number(v))
            d[[fields[[f]]]] = as.numeric(v)
        else
            d[[fields[[f]]]] = v
    }
    d$todelete = NULL
    return (d)
}

#Computes the queue drop rate: dropped packets / generated packets
compute.drop.rate = function(d, group=F) {
    fields = c('window')

    drop.rate = ddply(d, fields, function(x) {
        all.packets = subset(x, event == PKT_GENERATED)
        lost.packets = subset(x, event == PKT_QUEUE_DROPPED)
        return(data.frame(dr=nrow(lost.packets)/nrow(all.packets)))
    }, .parallel=T)
    return(drop.rate)
}

#Computes collision rate: corrupter / (received + corrupted)
compute.collision.rate = function(d, group=F) {
    fields = c('window')

    collision.rate = ddply(d, fields, function(x) {
        all.packets = subset(x, event == PKT_RECEIVED | event == PKT_CORRUPTED)
        lost.packets = subset(all.packets, event == PKT_CORRUPTED)
        return(data.frame(cr=nrow(lost.packets)/nrow(all.packets)))
    }, .parallel=T)
    return(collision.rate)
}

#Compute throughput: total bits received / simulation time
compute.throughput = function(d, data.rate, sim.time, group=F) {
    fields = c('window')

    throughput = ddply(d, fields, function(x) {
        received.packets = subset(x, event == PKT_RECEIVED)
        return(data.frame(tr=sum(received.packets$size*8)/sim.time/(1024**2)))
    }, .parallel=T)

    return(throughput)
}

#Unused function for a bad model
get.my.values = function(average.packet.size = 766, tx.rate = 1000000, window.value = 4, lambda.values = 10:45000) {
  
  throughput.values = list();
  datarate.values = list();
  
  for(current.lambda in lambda.values) {
    datarate = average.packet.size / ((1 / current.lambda) + (average.packet.size / tx.rate))
  
    mu = average.packet.size / tx.rate
    
    other.tx.nodes.probabilities = c(1 / (1 + current.lambda / mu), (current.lambda / mu) * 1 / (1 + current.lambda / mu))
    
    throughput = datarate * (other.tx.nodes.probabilities[[1]] * 1 + other.tx.nodes.probabilities[[2]] * (window.value - 1) / 2 / window.value)
    
    throughput.values[[length(throughput.values) + 1]] = throughput
    datarate.values[[length(datarate.values) + 1]] = datarate
  }
  
  results = data.frame(lambda = lambda.values, tr =  rapply(throughput.values, c), dr =  rapply(datarate.values, c))
  results$type = "model"
  
  return(results)
}

#Calculates predicted saturation throughput for the given parameters
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

aggregated.file = paste(res.folder, 'alld.Rdata', sep='/')

alld = data.frame()
data.files = get.data.files(res.folder, '.csv')
for (f in data.files) {
    full.path = paste(res.folder, f, sep='/')
    print(full.path)
    pars = get.params(full.path, c('prefix', 'lambda', 'seed', 'window'))
    d = read.csv(full.path)
    d = cbind(d, pars)
    alld = rbind(d, alld)
}


# get simulation time and number of nodes from the simulation data
sim.time = max(alld$time)
n.nodes = length(unique(alld$src))

# compute the statistics
cr = compute.collision.rate(alld)
dr = compute.drop.rate(alld)
tr = compute.throughput(alld, bitrate.mbps * 1e6, sim.time)

#Measure throughput as seen by the channel and not by a single station which does not count as throughput what is transmitting itself
tr$tr = tr$tr / n.nodes * (n.nodes + 1) / n.nodes

cr$type = 'measured'
dr$type = 'measured'
tr$type = 'measured'

tr = rbind(tr, get.bianchi.values(stations.count = n.nodes))

tr$tr = tr$tr / bitrate.mbps

# and plot the results
div = 3
p = ggplot(tr, aes(x=window, y=tr)) +
     geom_line(aes(linetype=type)) +
     geom_point() +
     scale_x_continuous(trans='log2') +
     scale_linetype_manual(values=c("dotted", "solid")) +
     xlab('window size (slots #)') +
     ylab('channel throughput (Times C)') +
     labs(color="receiver node") +
     ylim(c(0, 1))
ggsave(paste(res.folder, '/thr_', n.nodes, '.pdf', sep=''), width=16/div, height=9/div)
print(p)

best.windows = NULL

for(current.n in 1:16) {
  bianchi.values = get.bianchi.values(stations.count = current.n)
  bianchi.values = cbind(bianchi.values, data.frame(nodes = current.n))
  
  max.throughput = max(bianchi.values$tr)
  bianchi.values = bianchi.values[bianchi.values$tr == max.throughput,]
  bianchi.values = bianchi.values[1,]
  
  if(is.null(best.windows)) {
    best.windows = bianchi.values
  } else {
    best.windows = rbind(best.windows, bianchi.values)
  }
  
}

#Performs linear regression to find a model for the ideal window size
linearMod = lm(window ~ nodes, data=best.windows)
print(linearMod)

div = 3
p = ggplot(best.windows, aes(x=nodes, y=window)) +
  geom_line() +
  geom_point() +
  xlab('number of nodes') +
  ylab('ideal window size (slots #)') +
ggsave(paste(res.folder, '/ideal_window_sizes.pdf', sep=''), width=16/div, height=9/div)
print(p)


