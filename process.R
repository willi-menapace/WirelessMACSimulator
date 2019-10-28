#
#
# Original Author: Michele Segata <msegata@disi.unitn.it>
#         
# Modifications by: Willi Menapace 199890 <willi.menapace@gmail.com>
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

#Command line argument, if present, indicates the results folder
args = commandArgs(trailingOnly = T)
if (length(args) != 0) {
    res.folder = args[1]
} else {
    res.folder = './'
}

bitrate.mbps = 8
windows.sizes = c(2, 4, 8, 16, 32, 64, 128)

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
    return(list.files(folder, pattern=paste('output.*', suffix, sep='')))
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

#Computes the offered load
compute.offered.load = function(d, data.rate, sim.time) {
    # keep generation events only
    d <- subset(d, event == PKT_GENERATED)
    offered.load = ddply(d, c("src", "lambda"), function(x) {
        return(data.frame(ol=(sum(x$size * 8) / sim.time) / (1024**2)))
    }, .parallel=T)
    return(offered.load)
}

#Computes the queue drop rate: dropped packets / generated packets
compute.drop.rate = function(d, group=F) {
    fields <- c('lambda')
    if (!group)
        fields = c('src', fields)
    drop.rate = ddply(d, fields, function(x) {
        all.packets = subset(x, event == PKT_GENERATED)
        lost.packets = subset(x, event == PKT_QUEUE_DROPPED)
        return(data.frame(dr=nrow(lost.packets)/nrow(all.packets)))
    }, .parallel=T)
    return(drop.rate)
}

#Computes collision rate: corrupter / (received + corrupted)
compute.collision.rate = function(d, group=F) {
    fields = c('lambda')
    if (!group)
        fields = c('dst', fields)
    collision.rate = ddply(d, fields, function(x) {
        all.packets = subset(x, event == PKT_RECEIVED | event == PKT_CORRUPTED)
        lost.packets = subset(all.packets, event == PKT_CORRUPTED)
        return(data.frame(cr=nrow(lost.packets)/nrow(all.packets)))
    }, .parallel=T)
    return(collision.rate)
}

#Compute throughput: total bits received / simulation time
compute.throughput = function(d, data.rate, sim.time, group=F) {
    fields = c('lambda')
    if (!group)
        fields = c('dst', fields)
    throughput = ddply(d, fields, function(x) {
        received.packets <- subset(x, event == PKT_RECEIVED)
        return(data.frame(tr=sum(received.packets$size*8)/sim.time/(1024**2)))
    }, .parallel=T)
    return(throughput)
}

#Total offered load in bits per second
offered.load = function(lambda, n.nodes, packet.size=(1460+32)/2) {
    lambda*n.nodes*packet.size*8/1024/1024
}

for(current.window in windows.sizes) {

    #Load all csv files into a single one
    alld = data.frame()
    
    #Dataframes for cumulative results
    cr = data.frame()
    dr = data.frame()
    tr = data.frame()
    
    #Find all csv in current folder with the current window size
    data.files = get.data.files(res.folder, paste('_', current.window, '.csv', sep=''))
    for (f in data.files) {
        full.path = paste(res.folder, f, sep='/')
        print(full.path)
        pars = get.params(full.path, c('prefix', 'lambda', 'seed'))
        
        #Reads the file with fread for increased performance
        d = fread(full.path, header = T, sep = ',')
        
        d = cbind(d, pars)
        
        #Get simulation time and number of nodes from the simulation data
        sim.time = max(d$time)
        n.nodes = length(unique(d$src))
        
        temp.cr = compute.collision.rate(d)
        temp.dr = compute.drop.rate(d)
        temp.tr = compute.throughput(d, bitrate.mbps * 1e6, sim.time)
        
        cr = rbind(cr, temp.cr)
        dr = rbind(dr, temp.dr)
        tr = rbind(tr, temp.tr)
            
    }
    
    
    #Compute offered load starting from labmda
    cr$ol = offered.load(cr$lambda, n.nodes=n.nodes)
    dr$ol = offered.load(dr$lambda, n.nodes=n.nodes)
    tr$ol = offered.load(tr$lambda, n.nodes=n.nodes)
    
    #Saves intermediate results for further elaboration if needed
    write.csv(cr, paste('collision_rate_', n.nodes, "_", current.window, '.csv', sep=''))
    write.csv(dr, paste('drop_rate_', n.nodes, "_", current.window, '.csv', sep=''))
    write.csv(tr, paste('throughput_', n.nodes, "_", current.window, '.csv', sep=''))
    
    #Normalizes throughput
    cr$ol = cr$ol / bitrate.mbps
    dr$ol = dr$ol / bitrate.mbps
    tr$ol = tr$ol / bitrate.mbps
    tr$tr = tr$tr / bitrate.mbps
    
    #Plot the results
    div = 3
    p = ggplot(tr, aes(x=ol, y=tr, color=factor(dst))) +
         geom_line() +
         geom_point() +
         xlab('total offered load (Times C)') +
         ylab('throughput at receiver (Times C)') +
         labs(color="receiver node") +
         ylim(c(0, 1))
    ggsave(paste(res.folder, '/thr_', n.nodes, '_window_', current.window, '.pdf', sep=''), width=16/div, height=9/div)
    print(p)
    
    pcr = ggplot(cr, aes(x=ol, y=cr, color=factor(dst))) +
           geom_line() +
           geom_point() +
           xlab('total offered load (Times C)') +
           ylab('packet collision rate at receiver') +
           labs(color="receiver node") +
           ylim(c(0, 1))
    ggsave(paste(res.folder, '/pcr_', n.nodes, '_window_', current.window, '.pdf', sep=''), width=16/div, height=9/div)
    print(pcr)
    
    pdr = ggplot(dr, aes(x=ol, y=dr, color=factor(src))) +
           geom_line() +
           geom_point() +
           xlab('total offered load (Times C)') +
           ylab('packet drop rate at sender') +
           labs(color="sender node") +
           ylim(c(0, 1))
    ggsave(paste(res.folder, '/pdr_', n.nodes, '_window_', current.window, '.pdf', sep=''), width=16/div, height=9/div)
    print(pdr)
}

