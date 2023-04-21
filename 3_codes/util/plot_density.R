
# ------------------------------------------------------------------------------
# About this code
# Plot densities of hyper parameters
# ------------------------------------------------------------------------------

plot.density <- 
  function(MCMC.name, type){
  
  # load the output data
  load(paste("../5_tmp/", MCMC.name, ".rda", sep = ""))  
  
  # load the hyper parameter estimates from the MCMC result 
  df.params <- 
    out.list[[type]]
    
  # clean up the global environment to avoid conflicts
  if (exists("g.all")) {
    rm(g.all)
  }
  
  # obtain the variable name
  varnames <- 
    colnames(df.params)
  
  # number of variables
  p <- length(varnames)
  
  # assign chain names
  idx <- ceiling(1:nrow(df.params) / (nrow(df.params) / 4))
  df.params <- 
    df.params %>% 
    mutate(chain.name = paste("chain", idx, sep = " "))
  
  # repeat for all the variables
  for (i in 1:p) {
    
    # check the name of current variable
    current.var <- varnames[i]
  
    # plot the density graph
    df.param.i <- 
      df.params %>% 
      select(all_of(current.var), chain.name)
    
    colnames(df.param.i) <- c("value", "chain.name")
    
    g.b_i <-
      df.param.i %>% 
      ggplot() + 
      geom_density(aes(x = value, color = chain.name)) + 
      labs(x = current.var) + 
      theme(legend.position = "none")
      
    # combine with the graph output file
    if (exists("g.all")) {
      g.all <- g.all + g.b_i
    }else{
      g.all <- g.b_i
    }
  }
  
  # arrange the layout
  g.all <- g.all + plot_layout(nrow = 3)
  
  # set the target
  if (type == 2) {
    target <- "_theta"
  } else {
    target <- "_hyper"
  }
  
  # save under the 6_output folder
  ggsave(plot = g.all, 
         width = 15, height = 9, 
         filename = paste("../6_outputs/", MCMC.name, target, ".pdf", sep = ""))
  
  # return and show the plot
  return(g.all)
}
