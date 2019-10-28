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
library(data.table)
registerDoMC(cores=detectCores())

bitrate.mbps = 8
windows.sizes = c(32)
nodes.count = 10

#List of the groups to plot
groups.list = list(list(1, 5, 10), list(2, 6), list(7), list(4, 9), list(3, 8))

for(current.window in windows.sizes) {
  
  #Reads data to plot
  base.throughput.filename = paste("throughput_", nodes.count, "_", current.window, ".csv", sep = '')
  base.collision.filename = paste("collision_rate_", nodes.count, "_", current.window, ".csv", sep = '')
  base.drop.filename = paste("drop_rate_", nodes.count, "_", current.window, ".csv", sep = '')
  
  realistic.throughput.filename = paste("realistic_throughput_", nodes.count, "_", current.window, ".csv", sep = '')
  realistic.collision.filename = paste("realistic_collision_rate_", nodes.count, "_", current.window, ".csv", sep = '')
  realistic.drop.filename = paste("realistic_drop_rate_", nodes.count, "_", current.window, ".csv", sep = '')
  
  base.throughput = fread(base.throughput.filename, header = T, sep = ',')
  base.collision = fread(base.collision.filename, header = T, sep = ',')
  base.drop = fread(base.drop.filename, header = T, sep = ',')
  
  realistic.throughput = fread(realistic.throughput.filename, header = T, sep = ',')
  realistic.collision = fread(realistic.collision.filename, header = T, sep = ',')
  realistic.drop = fread(realistic.drop.filename, header = T, sep = ',')
  
  #Distinguishes base from realistic propagation
  base.throughput$type = "base"
  base.collision$type = "base"
  base.drop$type = "base"
  
  realistic.throughput$type = "realistic"
  realistic.collision$type = "realistic"
  realistic.drop$type = "realistic"
  realistic.throughput$tr = realistic.throughput$tr / 2
  
  #Creates unique data frame
  throughput = rbind(realistic.throughput, base.throughput)
  collision = rbind(realistic.collision, base.collision)
  drop = rbind(realistic.drop, base.drop)
  
  #Normalizes throughput
  throughput$tr = throughput$tr / bitrate.mbps
  throughput$ol = throughput$ol / bitrate.mbps
  collision$ol = collision$ol / bitrate.mbps
  drop$ol = drop$ol / bitrate.mbps
  
  #Computes means for all the simulation runs
  throughput = ddply(throughput, c("dst", "type", "lambda"), function(x) {
    return(data.frame(tr=mean(x$tr), ol=mean(x$ol)))
  }, .parallel=T)
  collision = ddply(collision, c("dst", "type", "lambda"), function(x) {
    return(data.frame(cr=mean(x$cr), ol=mean(x$ol)))
  }, .parallel=T)
  drop = ddply(drop, c("src", "type", "lambda"), function(x) {
    return(data.frame(dr=mean(x$dr), ol=mean(x$ol)))
  }, .parallel=T)
  
  #Creates separate plots for each group of nodes
  for(nodes.group in groups.list) {
    
    #Selects values only for the current group of nodes
    cut.throughput = throughput[throughput$dst %in% nodes.group, ]
    cut.collision = collision[collision$dst %in% nodes.group, ]
    cut.drop = drop[drop$src %in% nodes.group, ]
    
    div = 3
    p = ggplot(cut.throughput, aes(x=ol, y=tr, colour=factor(dst), shape=type, group=interaction(factor(dst), type))) +
      geom_line() +
      geom_point() +
      xlab('total offered load (Times C)') +
      ylab('throughput at receiver (Times C)') +
      labs(color="receiver node") +
      ylim(c(0, 1))
    ggsave(paste('thr_', paste(c(nodes.group), sep="_", collapse='_') , "_ncount_", nodes.count, '_window_', current.window, '.pdf', sep=''), width=16/div, height=9/div)
    print(p)
    
    pcr = ggplot(cut.collision, aes(x=ol, y=cr, color=factor(dst), shape=type, group=interaction(factor(dst), type))) +
      geom_line() +
      geom_point() +
      xlab('total offered load (Times C)') +
      ylab('packet collision rate at receiver') +
      labs(color="receiver node") +
      ylim(c(0, 1))
    ggsave(paste('pcr_', paste(c(nodes.group), sep="_", collapse='_') , "_ncount_", nodes.count, '_window_', current.window, '.pdf', sep=''), width=16/div, height=9/div)
    print(pcr)
    
    pdr = ggplot(cut.drop, aes(x=ol, y=dr, color=factor(src), shape=type, group=interaction(factor(src), type))) +
      geom_line() +
      geom_point() +
      xlab('total offered load (Times C)') +
      ylab('packet drop rate at sender') +
      labs(color="sender node") +
      ylim(c(0, 1))
    ggsave(paste('pdr_', paste(c(nodes.group), sep="_", collapse='_') , "_ncount_", nodes.count, '_window_', current.window, '.pdf', sep=''), width=16/div, height=9/div)
    print(pdr)
    
  }
  
}

