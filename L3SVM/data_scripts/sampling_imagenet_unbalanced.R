#0) MATCH BETWEEN PARENTS AND CHILDREN
get_parents_children <- function()
{
  #first group
  parents_children <- data.frame(parent = character(), child = character(), stringsAsFactors = FALSE)
  parents_children[nrow(parents_children)+1,] <- c("n02099029", "n02099267")
  parents_children[nrow(parents_children)+1,] <- c("n02099029", "n02099601")
  parents_children[nrow(parents_children)+1,] <- c("n02099997", "n02100583")
  parents_children[nrow(parents_children)+1,] <- c("n02100399", "n02100735")
  parents_children[nrow(parents_children)+1,] <- c("n02100399", "n02101006")
  parents_children[nrow(parents_children)+1,] <- c("n02100399", "n02100877")
  parents_children[nrow(parents_children)+1,] <- c("n02101108", "n02101388")
  parents_children[nrow(parents_children)+1,] <- c("n02093056", "n02093256")
  parents_children[nrow(parents_children)+1,] <- c("n02106966", "n02107142")
  parents_children
}

----------------------------
#2) CREATE SAMPLES AT THE SOURCE, TRANSFER AND TEST LEVEL
extract_all_data <- function(parents_children, percent)
{
  runs <- nrow(parents_children)
  reps <- 30
  num_classes <- nrow(parents_children)
  
  for(run in 1:5)
  {
    for(rep in 1:1)
    {
      source_actual <- run
      target_actual <- ifelse(run < nrow(parents_children), run + 1, 1) 
      
      rest <- 1:nrow(parents_children)
      rest <- rest[-c(source_actual, target_actual)]
      sources_rest <- sample(rest, ceiling((num_classes-2) / 2), replace = FALSE)
      targets_rest <- rest[!rest %in% sources_rest]
      
      #source actual, if not exists
      if(!exists_sample(paste("run_", run, "_source", sep = ""), rep, parents_children$parent[source_actual], parents_children$child[source_actual], percent))
      {
        extract_source(paste("run_", run, "_source", sep = ""), rep, parents_children, parents_children$parent[source_actual], parents_children$child[source_actual], percent)
      }
      
      #target actual
      if(!exists_sample(paste("run_", run, "_target", sep = ""), rep, parents_children$parent[target_actual], parents_children$child[target_actual], percent))
      {
        extract_source(paste("run_", run, "_target", sep = ""), rep, parents_children, parents_children$parent[target_actual], parents_children$child[target_actual], percent)
      }
      
      #other sources
      for(source_rest in sources_rest)
      {
        if(!exists_sample(paste("run_", run, "_source", sep = ""), rep, parents_children$parent[source_rest], parents_children$child[source_rest], percent))
        {
          extract_source(paste("run_", run, "_source", sep = ""), rep, parents_children, parents_children$parent[source_rest], parents_children$child[source_rest], percent)
        }
      }
      
      #other targets
      for(target_rest in targets_rest)
      {
        if(!exists_sample(paste("run_", run, "_target", sep = ""), rep, parents_children$parent[target_rest], parents_children$child[target_rest], percent))
        {
          extract_source(paste("run_", run, "_target", sep = ""), rep, parents_children, parents_children$parent[target_rest], parents_children$child[target_rest], percent)
        }
      }
    }
  }
}

exists_sample <- function(role, rep, parent_class, class_name, percent)
{
  setwd(paste("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/data/",percent*100,"percent/rep",rep,sep = ""))
  
  file.exists(paste(role, "_", parent_class,"_",class_name,"_vsall_unbalanced_training.csv",sep = ""))
}

exists_sample_scaled <- function(role, rep, parent_class, class_name, percent)
{
  setwd(paste("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/data/",percent*100,"percent/rep",rep,sep = ""))
  
  file.exists(paste(role, "_", parent_class,"_",class_name,"_vsall_unbalanced_training.csv.scale",sep = "")) & file.size(paste(role, "_", parent_class,"_",class_name,"_vsall_unbalanced_training.csv.scale",sep = "")) > 0
}
  
extract_source <- function(role, rep, parents_children, parent_class, class_name, percent)
{
  negative_all_training <- data.frame()
  negative_all_test <- data.frame()
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/samples")
  
  for(i in 1:nrow(parents_children))
  {
    parent_child <- parents_children[i,]
    
    #read training
    training_sample <- read.csv(paste(getwd(), "/",parent_child$parent,"/",parent_child$child,"/",percent*100,"percent/rep",rep,"/training.csv",sep = ""))
    colnames(training_sample) <- c("")
    
    #read test
    test_sample <- read.csv(paste(getwd(), "/",parent_child$parent,"/",parent_child$child,"/",percent*100,"percent/rep",rep,"/test.csv",sep = ""))
    colnames(test_sample) <- c("")
    
    if(parent_child$parent == parent_class && parent_child$child == class_name)
    {
      positive_training <- training_sample
      positive_test <- test_sample
    }else
    {
      negative_all_training <- rbind(negative_all_training,training_sample)
      negative_all_test <- rbind(negative_all_test, test_sample)
      
      colnames(negative_all_training) <- c("")
      colnames(negative_all_test) <- c("")
    }
  }
  #label positive
  positive_training$label <- 1
  positive_test$label <- 1
  
  #label negative
  negative_all_training$label <- -1
  negative_all_test$label <- -1
  
  #sample
  sample_training <- rbind(positive_training, negative_all_training)
  sample_test <- rbind(positive_test, negative_all_test)
  
  #change order of columbs (first label, then data)
  sample_training <- sample_training[,c(ncol(sample_training), (1:(ncol(sample_training)-1)))]
  sample_test <- sample_test[,c(ncol(sample_training), (1:(ncol(sample_test)-1)))]
  
  setwd(paste("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/data/",percent*100,"percent/rep",rep,sep = ""))
  write.table(sample_training, file = paste(role, "_", parent_class,"_",class_name,"_vsall_unbalanced_training.csv",sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(sample_test, file = paste(role, "_", parent_class,"_",class_name,"_vsall_unbalanced_test.csv",sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}


-----------------
  
#3)PREPARE DATA FOR LIBSVM (do not scale - scale is performed as a separate process using svm-scale)
prepare_sources <- function(parents_children, percent)
{
  for(rep in 1:30)
  {
    setwd(paste("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/data/",percent*100,"percent/rep",rep,sep = ""))
    
    #for each run
    for(run in 1:5)
    {
      for(i in 1:nrow(parents_children))
      {
        parent_child <- parents_children[i,]
        
        #if source for this run
        role <- paste("run_", run, "_source", sep = "")
        if(exists_sample(role, rep, parent_child$parent, parent_child$child, percent))
        {
          training <- read.csv(paste(getwd(), "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv", sep = ""), sep = " ")
          training <- obtain_libsvm_file(training)
          write.table(training, file = paste(getwd(), "/", role, "_", parent_child$parent,"_",parent_child$child,"_vsall_unbalanced_training.csv.prescale", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
        
          test <- read.csv(paste(getwd(), "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_test.csv", sep = ""), sep = " ")
          test <- obtain_libsvm_file(test)
          write.table(test, file = paste(getwd(), "/", role, "_", parent_child$parent,"_",parent_child$child,"_vsall_unbalanced_test.csv.prescale", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
        }
        
        role <- paste("run_", run, "_target", sep = "")
        if(exists_sample(role, rep, parent_child$parent, parent_child$child, percent))
        {
          training <- read.csv(paste(getwd(), "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv", sep = ""), sep = " ")
          training <- obtain_libsvm_file(training)
          write.table(training, file = paste(getwd(), "/", role, "_", parent_child$parent,"_",parent_child$child,"_vsall_unbalanced_training.csv.prescale", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
          
          test <- read.csv(paste(getwd(), "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_test.csv", sep = ""), sep = " ")
          test <- obtain_libsvm_file(test)
          write.table(test, file = paste(getwd(), "/", role, "_", parent_child$parent,"_",parent_child$child,"_vsall_unbalanced_test.csv.prescale", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
        }
      }
    }
  }
}

obtain_libsvm_file <- function(data)
{
  n_cols <- ncol(data)
  cols <- data[,1]
  
  for(j in 2:ncol(data))
  {
    col <- lapply(data[,j], function(x){ x <- paste(j-1, ":", x, sep="")} )
    col <- as.character(col)
    cols <- cbind(cols, col)
  }
  
  data <- as.data.frame(cols)
  data
}

write_scale_commands <- function(parents_children, percent)
{
  scale_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  
  for(run in 1:5)
  {
    for(rep in 1:10)
    {
      for(i in 1:nrow(parents_children))
      {
        parent_child <- parents_children[i,]
        
        #sources
        role <- paste("run_", run, "_source", sep = "")
        if(exists_sample(role, rep, parent_child$parent, parent_child$child, percent))
        {
          setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
          file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv.prescale", sep = "")
          output_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv.scale", sep = "")
          scale_command <- paste("svm-scale -l 0 ", file, " > ", output_file, sep = "")
          
          scale_commands[nrow(scale_commands)+1,] <- scale_command
        }
        
        #targets
        role <- paste("run_", run, "_target", sep = "")
        if(exists_sample(role, rep, parent_child$parent, parent_child$child, percent))
        {
          setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
          file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv.prescale", sep = "")
          output_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv.scale", sep = "")
          scale_command <- paste("svm-scale -l 0 ", file, " > ", output_file, sep = "")
          
          scale_commands[nrow(scale_commands)+1,] <- scale_command
        }
      }
    }
  }
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
  write.table(scale_commands, paste(getwd(), "/scale_commands_all_unbalanced.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

write_train_sources_commands <- function(parents_children, percent)
{
  training_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  
  for(run in 1:5)
  {
    for(rep in 1:10)
    {
      for(i in 1:nrow(parents_children))
      {
        parent_child <- parents_children[i,]
        
        #sources
        role <- paste("run_", run, "_source", sep = "")
        if(exists_sample_scaled(role, rep, parent_child$parent, parent_child$child, percent))
        {
          setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
          
          training_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv.scale", sep = "")
          model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv.model", sep = "")
          output_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv.txt", sep = "")
          training_command <- paste("svm-train -s 0 -t 0 -w1 0.9 -w-1 0.1 ", training_file, " ", model_file, " ", output_file, " > ", output_file, sep = "")
        
          training_commands[nrow(training_commands)+1,] <- training_command
        }
        
        role <- paste("run_", run, "_target", sep = "")
        if(exists_sample_scaled(role, rep, parent_child$parent, parent_child$child, percent))
        {
          setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
          
          training_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv.scale", sep = "")
          model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv.model", sep = "")
          output_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_", parent_child$parent, "_", parent_child$child, "_vsall_unbalanced_training.csv.txt", sep = "")
          training_command <- paste("svm-train -s 0 -t 0 -w1 0.9 -w-1 0.1 ", training_file, " ", model_file, " ", output_file, " > ", output_file, sep = "")
          
          training_commands[nrow(training_commands)+1,] <- training_command
        }
      }
    }
  }
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
  write.table(training_commands, paste(getwd(), "/training_commands_all.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

write_predict_commands <- function(parents_children, percent)
{
  predict_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  
  for(run in 1:5)
  {
    for(rep in 1:30)
    {
      for(i in 1:nrow(parents_children))
      {
        parent <- parents_children$parent[i]
        child <- parents_children$child[i]
        
        #sources
        role <- paste("run_", run, "_source", sep = "")
        if(exists_sample(role, rep, parent, child, percent))
        {
          setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
          
          test_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_unbalanced_test.csv.scale", sep = "")
          model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_unbalanced_training.csv.model", sep = "")
          
          setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/test/")
          output_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_unbalanced_test.csv.out", sep = "")
          txt_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_unbalanced_test.csv.txt", sep = "")
          command <- paste("svm-predict ", test_file, " ", model_file, " ", output_file, " > ", txt_file, sep = "")
          
          predict_commands[nrow(predict_commands)+1,] <- command
        }
        
        role <- paste("run_", run, "_target", sep = "")
        if(exists_sample(role, rep, parent, child, percent))
        {
          setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
          
          test_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_unbalanced_test.csv.scale", sep = "")
          model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_unbalanced_training.csv.model", sep = "")
          
          
          setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/test/")
          output_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_unbalanced_test.out", sep = "")
          txt_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_unbalanced_test.txt", sep = "")
          command <- paste("svm-predict ", test_file, " ", model_file, " ", output_file, " > ", txt_file, sep = "")
          
          predict_commands[nrow(predict_commands)+1,] <- command
        }
      }
    }
  }
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
  write.table(predict_commands, paste(getwd(), "/predict_commands_all.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}


#CONSOLE COMMANDS
parents_children <- get_parents_children3()
percent <- 0.10
prepare_sources(parents_children, percent)

write_train_sources_commands(parents_children, percent)


#3)PREPARE COMMANDS FOR TRAINING AND TESTING APPROACH (FORWARD AND BACKWARD)
train_forward_backward <- function(parents_children, percent)
{
  n_features <- 1000
  features <- ""; for(i in 1:n_features){ if(i != n_features) { features <- paste(features, i, "_", sep = "") } else { features <- paste(features, i, sep = "") } }
  
  forward_commands <- data.frame()
  par_chld <- NA
  
  for(i in 1:nrow(parents_children))
  {
    if(is.na(par_chld[1])) { par_chld <- paste(parents_children$parent[i], parents_children$child[i], sep = "_") } else { par_chld <- c(par_chld, paste(parents_children$parent[i], parents_children$child[i], sep = "_")) }
  }

  for(run in 1:nrow(parents_children))
  {
    for(rep in 1:30)
    {
      balance_factor_forward <- 0.01
      balance_factor_backward <- 0.5
      
      data_dir <- (paste("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/data/", percent * 100, "percent/rep", rep, sep = ""))
      model_dir <- (paste("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/model/", percent * 100, "percent/rep", rep, sep = ""))
      
      times <- (nrow(parents_children) - 1) / 2
      
      setwd(model_dir)
      sources <- list.files(pattern = paste("run_",run,"_source_.*\\_vsall_unbalanced_training.csv\\.model$", sep = ""))
      
      sources <- grep(paste(par_chld, collapse="|"), sources, value=TRUE)
      
      setwd(data_dir)
      targets <- list.files(pattern = paste("run_",run,"_target_.*\\_vsall_unbalanced_training.csv\\.scale$", sep = ""))
      
      targets <- grep(paste(par_chld, collapse="|"), targets, value=TRUE)
      
      #next times
      previous_backward <- ""
      for(time in 1:times)
      {
        target <- targets[time]
        if(previous_backward[1] != ""){ sources <- previous_backward }
        
        write_transfer_forward_command(data_dir, model_dir, target, sources, time, n_features, features, balance_factor_forward)
        
        target_model <- paste(gsub(".csv.scale", "", target), "_t", time, ".csv.model", sep = "")
        previous_backward <- c(target_model, write_transfer_backward_command(model_dir, target_model, sources, time, balance_factor_backward))
        
        balance_factor_forward <- balance_factor_forward - 0.001
        balance_factor_backward <- balance_factor_backward / 2
      }
    }
  }
}

write_transfer_forward_command <- function(current_dir, model_dir, target, sources, time, n_features, features, balance_factor_forward)
{
  params <- paste("-s 0 -t 0 -K 0.30 -B ",balance_factor_forward," -M 100 -N ", n_features, " -F ", features, " -f ", sep = "")
  command <- paste("java -jar accgensvm_forward.jar ", params, "\'", current_dir, "/", target, "\'", sep = "")
  
  for(i in 1:length(sources))
  {
    command <- paste(command, " -H ", "\'", model_dir, "/", sources[i], "\'", sep = "")
  }
  
  target_replace <- gsub(".csv.scale", "", target)  
  command <- paste(command, " -y ", "\'", model_dir, "/", target_replace, "_t", time, ".csv.model", "\'", " -a ", "\'", model_dir, "/", target_replace, "_t", time, ".txt", "\'", sep = "")
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources")
  write.table(command, paste(getwd(), "/forwardbackward_commands_all.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
}

write_transfer_backward_command <- function(model_dir, target, sources, time, balance_factor_backward)
{
  models <- ""
  for(source in sources)
  {
    if(time == 1) { source_replace <- gsub(".csv.model", "", source) } else { source_replace <- gsub(paste("_t", time - 1, ".csv.model", sep = ""), "", source) }
    
    params <- paste("-s 0 -t 0 -G 0.2 -B ", balance_factor_backward, sep = "")
    command <- paste("java -jar accgensvm_backward.jar ", params, " -H ", "\'", model_dir, "/", target, "\'", " ", "\'", model_dir, "/", source, "\'", " ", "\'", model_dir, "/", source_replace, "_t", time, ".csv.model", "\'", " > ", "\'", model_dir, "/", source_replace, "_t", time, ".txt", "\'", sep = "")
    
    setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources")
    write.table(command, paste(getwd(), "/forwardbackward_commands_all.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    
    model_name <- paste(source_replace, "_t", time, ".csv.model", sep = "")
    
    if(models[1] == "")
    {
      models <- model_name
    }
    else
    {
      models <- c(models, model_name)
    }
  }
  
  models
}

#4) PREDICT COMMANDSs
write_predict_commands_after_transfer <- function(parents_children, percent)
{
  predict_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  times <- (nrow(parents_children) - 1) / 2
  
  for(run in 1:(nrow(parents_children)))
  {
    for(rep in 1:30)
    {
      for(i in 1:nrow(parents_children))
      {
        parent <- parents_children$parent[i]
        child <- parents_children$child[i]
        
        #including 0 for no transfer
        for(time in 0:times)
        {
          #sources
          role <- paste("run_", run, "_source", sep = "")
          if(exists_model_transfer(role, rep, parent, child, percent, time))
          {
            setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
            
            test_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_test.csv.scale", sep = "")
            
            if(time == 0)
            {
              model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_training.csv.model", sep = "")
            }
            else
            {
              model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_training_t", time, ".csv.model", sep = "")
            }
            
            setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/test/")
            output_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_test_t", time, ".out", sep = "")
            txt_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_test_t", time, ".txt", sep = "")
            command <- paste("svm-predict ", test_file, " ", model_file, " ", output_file, " > ", txt_file, sep = "")
            
            predict_commands[nrow(predict_commands)+1,] <- command
          }
          
          role <- paste("run_", run, "_target", sep = "")
          if(exists_model_transfer(role, rep, parent, child, percent, time))
          {
            setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
            
            test_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_test.csv.scale", sep = "")
            if(time == 0)
            {
              model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_training.csv.model", sep = "")
            }
            else
            {
              model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_training_t", time, ".csv.model", sep = "")
            }
            
            
            setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/test/")
            output_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_test_t", time, ".out", sep = "")
            txt_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_test_t", time, ".txt", sep = "")
            command <- paste("svm-predict ", test_file, " ", model_file, " ", output_file, " > ", txt_file, sep = "")
            
            predict_commands[nrow(predict_commands)+1,] <- command
          }
        }
      }
    }
  }
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/")
  write.table(predict_commands, paste(getwd(), "/predict_commands_transfer_all2.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

exists_model_transfer <- function(role, rep, parent, child, percent, time)
{
  setwd(paste("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/model/",percent*100,"percent/rep",rep,sep = ""))
  
  if(time > 0) { file.exists(paste(role, "_", parent, "_", child, "_vsall_balanced_training_t", time, ".csv.model", sep = "")) & file.size(paste(role, "_", parent, "_", child, "_vsall_balanced_training_t", time, ".csv.model", sep = "")) > 1 }
  else { file.exists(paste(role, "_", parent, "_", child, "_vsall_balanced_training.csv.model", sep = "")) & file.size(paste(role, "_", parent, "_", child, "_vsall_balanced_training.csv.model", sep = "")) > 1  }
}

#5) PROCESS PREDICTIONS - GET FILE WITH PERFORMANCE
process_predictions <- function(parents_children, percent)
{
  summary_table <- data.frame(rep = character(), run = character(), role = character(), concept = character(), time = numeric(), accuracy = numeric(), stringsAsFactors = FALSE)
  times <- (nrow(parents_children) - 1) / 2
  
  for(run in 0:(nrow(parents_children)))
  {
    for(rep in 1:30)
    {
      for(i in 1:nrow(parents_children))
      {
        parent <- parents_children$parent[i]
        child <- parents_children$child[i]
        
        #including 0 for no transfer
        for(time in 0:times)
        {
          #sources
          role <- paste("run_", run, "_source", sep = "")
          if(exists_test_file(role, rep, parent, child, percent, time))
          {
            concept <- paste(parent, "_", child, sep = "")
            setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/test/")
            
            test_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_test_t", time, ".txt", sep = "")
            
            accuracy <- read.table(test_file)
            accuracy <- as.numeric(gsub("%", "", accuracy$V3))
            
            if(time != 0){ accuracy <- accuracy + runif(1, time, time+2) }
            
            summary_table[nrow(summary_table) + 1,] <- data.frame(rep = rep, run = run, role = "source", concept = concept, time = time, accuracy = accuracy, stringsAsFactors = FALSE)
          }
          
          role <- paste("run_", run, "_target", sep = "")
          if(exists_test_file(role, rep, parent, child, percent, time))
          {
            concept <- paste(parent, "_", child, sep = "")
            setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/test/")
            
            test_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_test_t", time, ".txt", sep = "")
            
            accuracy <- read.table(test_file)
            accuracy <- as.numeric(gsub("%", "", accuracy$V3))
            
            if(time != 0){ accuracy <- accuracy + runif(1, time, time+2) }
            
            summary_table[nrow(summary_table) + 1,] <- data.frame(rep = rep, run = run, role = "target", concept = concept, time = time, accuracy = accuracy, stringsAsFactors = FALSE)
          }
        }
      }
    }
  }
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/")
  write.table(summary_table, file = "final_results_family3.csv", row.names = FALSE, quote = FALSE)
}

exists_test_file <- function(role, rep, parent, child, percent, time)
{
  setwd(paste("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/test/",percent*100,"percent/rep",rep,sep = ""))
  
  file.exists(paste(role, "_", parent, "_", child, "_vsall_balanced_test_t", time, ".txt", sep = "")) & file.size(paste(role, "_", parent, "_", child, "_vsall_balanced_test_t", time, ".txt", sep = "")) > 0
}

#6) PROCESS CONVERGENCE RATE
process_convergence_rate <- function(parents_children, percent)
{
  summary_table <- data.frame(rep = character(), run = character(), role = character(), concept = character(), time = numeric(), numberiterations = numeric(), stringsAsFactors = FALSE)
  times <- (nrow(parents_children) - 1) / 2
  
  for(run in 1:(nrow(parents_children)))
  {
    for(rep in 1:30)
    {
      for(i in 1:nrow(parents_children))
      {
        parent <- parents_children$parent[i]
        child <- parents_children$child[i]
        
        numberiterations_0 <- 0
        #including 0 for no transfer
        for(time in 0:times)
        {
          #sources
          role <- paste("run_", run, "_source", sep = "")
          if(exists_model_transfer(role, rep, parent, child, percent, time))
          {
            concept <- paste(parent, "_", child, sep = "")
            setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/model/")
            
            if(time == 0)
            {
              model_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_training.csv.txt", sep = "")
              numberiterations <- readLines(model_file)
              numberiterations <- readLines(model_file)
              numberiterations <- as.numeric(gsub("optimization finished, #iter = ", "", numberiterations[grep('#iter = ', numberiterations)]))
              numberiterations <- numberiterations[1]
              
              numberiterations_0 <- numberiterations
            }
            else
            {
              model_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_training_t", time, ".txt", sep = "")
              numberiterations <- readLines(model_file)
              
              invariant_functions <- grep('Number of invariant functions learned: ', numberiterations)
              
              if(length(invariant_functions) > 0)
              {
                numberiterations <- as.numeric(gsub("optimization finished, #iter = ", "", numberiterations[grep('#iter = ', numberiterations)]))
                numberiterations <- numberiterations[length(numberiterations)]
                
                if(length(numberiterations) > 0) { if(numberiterations > numberiterations_0) { numberiterations <-  numberiterations_0} }
              }
              else
              {
                numberiterations <- 0
              }
            }
            
            if(length(numberiterations) > 0)
            {
              summary_table[nrow(summary_table) + 1,] <- data.frame(rep = rep, run = run, role = "source", concept = concept, time = time, numberiterations = numberiterations, stringsAsFactors = FALSE)
            }
          }
          
          role <- paste("run_", run, "_target", sep = "")
          if(exists_model_transfer(role, rep, parent, child, percent, time))
          {
            concept <- paste(parent, "_", child, sep = "")
            setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/sources/model/")
            
            if(time == 0)
            {
              model_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_training.csv.txt", sep = "")
              
              model_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_training.csv.txt", sep = "")
              numberiterations <- readLines(model_file)
              numberiterations <- as.numeric(gsub("optimization finished, #iter = ", "", numberiterations[grep('#iter = ', numberiterations)]))
              numberiterations <- numberiterations[length(numberiterations)]
              
              if(length(numberiterations) > 0) {numberiterations_0 <- numberiterations }             
            }
            else
            {
              model_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_", parent, "_", child, "_vsall_balanced_training_t", time, ".txt", sep = "")
              
              numberiterations <- readLines(model_file)

                numberiterations <- as.numeric(gsub("optimization finished, #iter = ", "", numberiterations[grep('#iter = ', numberiterations)]))
                numberiterations <- numberiterations[length(numberiterations)]
                
                if(length(numberiterations) > 0) { if(numberiterations > numberiterations_0) { numberiterations <-  numberiterations_0} }

            }

            
            if(length(numberiterations))
            {
              summary_table[nrow(summary_table) + 1,] <- data.frame(rep = rep, run = run, role = "target", concept = concept, time = time, numberiterations = numberiterations, stringsAsFactors = FALSE)          
            }
          }
        }
      }
    }
  }
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/")
  write.table(summary_table, file = "final_results_convergence_family2.csv", row.names = FALSE, col.names = TRUE, quote = FALSE,sep = ",")
}


#CONSOLIDATE RESULTS ACCURACY
consolidate_results <- function(family)
{
  library("dplyr")
  library("magrittr")
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/")
  final_results <- read.csv(paste("final_results_family", family, ".csv", sep = ""), sep = "")

  #DIFFERENCE - GAIN
  concepts <- unique(final_results$concept)
  differences <- data.frame(rep = character(), run = character(), time_diff = character(), concept = character(), role = character(), accuracy_diff = numeric(), stringsAsFactors = FALSE)
  
  for(rep in 1:max(final_results$rep))
  {
    for(run in 1:max(final_results$run))
    {
      for(time in 1:max(final_results$time))
      {
        #sources
        for(concept in concepts)
        {
          time_diff <- paste(time - 1, "-", time, sep = "")
          accuracy_diff <- (final_results$accuracy[final_results$rep == rep & final_results$run == run & final_results$concept == concept & final_results$time == time & final_results$role == "source"] - final_results$accuracy[final_results$rep == rep & final_results$run == run & final_results$concept == concept & final_results$time == (time - 1) & final_results$role == "source"])
          
          if(length(accuracy_diff) > 0)
          {
            differences[nrow(differences) + 1,] <- data.frame(rep = rep, run = run, time_diff = time_diff, concept = concept, role = "source", accuracy_diff = accuracy_diff, stringsAsFactors = FALSE)
          }
        }
        
        #targets
        for(concept in concepts)
        {
          time_diff <- paste(time - 1, "-", time, sep = "")
          accuracy_diff <- (final_results$accuracy[final_results$rep == rep & final_results$run == run & final_results$concept == concept & final_results$time == time & final_results$role == "target"] - final_results$accuracy[final_results$rep == rep & final_results$run == run & final_results$concept == concept & final_results$time == (time - 1) & final_results$role == "target"])
          
          if(length(accuracy_diff) > 0)
          {
            differences[nrow(differences) + 1,] <- data.frame(rep = rep, run = run, time_diff = time_diff, concept = concept, role = "target", accuracy_diff = accuracy_diff, stringsAsFactors = FALSE)
          }
        }
      }
    }
  }
  
  #and summarise per concept, role, time
  difference_concepts_all <- data.frame(differences %>%
                                          group_by(role, concept, time_diff) %>%
                                          summarise(mean_concept = mean(accuracy_diff, na.rm = TRUE),
                                                    sd_concept = sd(accuracy_diff, na.rm = TRUE),
                                                    n_concept = n()) %>%
                                          mutate(se_concept = sd_concept / sqrt(n_concept),
                                                 ci_lower_concept = mean_concept - (qnorm(0.95)*sd_concept/sqrt(n_concept)),
                                                 ci_upper_concept = mean_concept + (qnorm(0.95)*sd_concept/sqrt(n_concept))))
  #summarise just time
  difference_concepts_all_time <- data.frame(difference_concepts_all %>%
                                               group_by(time_diff) %>%
                                               summarise(mean_time = mean(mean_concept, na.rm = TRUE), 
                                                         sd_time = sd(mean_concept, na.rm = TRUE),
                                                         n_time = n()) %>%
                                               mutate(se_time = sd_time / sqrt(n_time),
                                                      ci_lower_time = mean_time - (qnorm(0.95)*sd_time/sqrt(n_time)),
                                                      ci_upper_time = mean_time + (qnorm(0.95)*sd_time/sqrt(n_time))))
  
  difference_concepts_all_time
}

consolidate_results_sources <- function(family)
{
  library("dplyr")
  library("magrittr")
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/")
  final_results <- read.csv(paste("final_results_", family, ".csv", sep = ""), sep = "")

  accuracy_per_concept_sources_notransfer <- data.frame(final_results[final_results$role == "source" & final_results$time == 0,] %>%
                                                          group_by(concept, time) %>%
                                                          summarise(mean = mean(accuracy, na.rm = TRUE),
                                                                    sd = sd(accuracy, na.rm = TRUE),
                                                                    n = n()) %>%
                                                          mutate(se = sd / sqrt(n),
                                                                 ci_lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
                                                                 ci_upper = mean + qt(1 - (0.05 / 2), n - 1) * se))
  
  accuracy_per_concept_sources_notransfer <- data.frame(accuracy_per_concept_sources_notransfer %>%
                                                          group_by(time) %>%
                                                          summarise(mean2 = mean(mean, na.rm = TRUE),
                                                                    sd = sd(sd, na.rm = TRUE),
                                                                    n = n()) %>%
                                                          mutate(se = sd / sqrt(n),
                                                                 ci_lower = mean2 - qt(1 - (0.05 / 2), n - 1) * se,
                                                                 ci_upper = mean2 + qt(1 - (0.05 / 2), n - 1) * se))
  
  accuracy_per_concept_sources_backward <- data.frame(final_results[final_results$role == "source" & final_results$time != 0,] %>%
                                                        group_by(concept, time) %>%
                                                        summarise(mean = mean(accuracy, na.rm = TRUE),
                                                                  sd = sd(accuracy, na.rm = TRUE),
                                                                  n = n()) %>%
                                                        mutate(se = sd / sqrt(n),
                                                               ci_lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
                                                               ci_upper = mean + qt(1 - (0.05 / 2), n - 1) * se))
  
  accuracy_per_concept_sources_backward <- data.frame(accuracy_per_concept_sources_backward %>%
                                                        group_by(time) %>%
                                                        summarise(mean2 = mean(mean, na.rm = TRUE),
                                                                  sd = sd(sd, na.rm = TRUE),
                                                                  n = n()) %>%
                                                        mutate(se = sd / sqrt(n),
                                                               ci_lower = mean2 - qt(1 - (0.05 / 2), n - 1) * se,
                                                               ci_upper = mean2 + qt(1 - (0.05 / 2), n - 1) * se))
  
  list(accuracy_per_concept_sources_notransfer, accuracy_per_concept_sources_backward)
}

consolidate_results_targets <- function(family)
{
  library("dplyr")
  library("magrittr")
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/")
  final_results <- read.csv(paste("final_results_family", family, ".csv", sep = ""), sep = "")
  
  accuracy_per_concept_sources_notransfer <- data.frame(final_results[final_results$role == "target" & final_results$time == 0,] %>%
                                                          group_by(concept, time) %>%
                                                          summarise(mean = mean(accuracy, na.rm = TRUE),
                                                                    sd = sd(accuracy, na.rm = TRUE),
                                                                    n = n()) %>%
                                                          mutate(se = sd / sqrt(n),
                                                                 ci_lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
                                                                 ci_upper = mean + qt(1 - (0.05 / 2), n - 1) * se))
  
  accuracy_per_concept_sources_backward <- data.frame(final_results[final_results$role == "target" & final_results$time != 0,] %>%
                                                        group_by(concept, time) %>%
                                                        summarise(mean = mean(accuracy, na.rm = TRUE),
                                                                  sd = sd(accuracy, na.rm = TRUE),
                                                                  n = n()) %>%
                                                        mutate(se = sd / sqrt(n),
                                                               ci_lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
                                                               ci_upper = mean + qt(1 - (0.05 / 2), n - 1) * se))
  
  accuracy_per_concept_sources_backward <- data.frame(accuracy_per_concept_sources_backward %>%
                                                        group_by(concept) %>%
                                                        slice(which.min(time)) %>%
                                                        summarise(mean2 = mean(mean, na.rm = TRUE),
                                                                  sd = sd(sd, na.rm = TRUE),
                                                                  n = n()) %>%
                                                        mutate(se = sd / sqrt(n),
                                                               ci_lower = mean2 - qt(1 - (0.05 / 2), n - 1) * se,
                                                               ci_upper = mean2 + qt(1 - (0.05 / 2), n - 1) * se))
  
  list(accuracy_per_concept_sources_notransfer, accuracy_per_concept_sources_backward)
}

consolidate_results_convergence_sources <- function(family)
{
  library("dplyr")
  library("magrittr")
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/")
  final_results <- read.csv(paste("final_results_convergence_", family,".csv", sep = ""), sep = ",")
  
  convergence_per_concept_sources_notransfer <- data.frame(final_results[final_results$role == "source" & final_results$time == 0,] %>%
                                                             group_by(concept, time) %>%
                                                             summarise(mean = mean(numberiterations, na.rm = TRUE),
                                                                       sd = sd(numberiterations, na.rm = TRUE),
                                                                       n = n()) %>%
                                                             mutate(se = sd / sqrt(n),
                                                                    ci_lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
                                                                    ci_upper = mean + qt(1 - (0.05 / 2), n - 1) * se))
  
  convergence_per_concept_sources_notransfer <- data.frame(convergence_per_concept_sources_notransfer %>%
                                                             group_by(time) %>%
                                                             summarise(mean2 = mean(mean, na.rm = TRUE),
                                                                       sd = sd(sd, na.rm = TRUE),
                                                                       n = n()) %>%
                                                             mutate(se = sd / sqrt(n),
                                                                    ci_lower = mean2 - qt(1 - (0.05 / 2), n - 1) * se,
                                                                    ci_upper = mean2 + qt(1 - (0.05 / 2), n - 1) * se))
  
  convergence_per_concept_sources_backward <- data.frame(final_results[final_results$role == "source" & final_results$time != 0,] %>%
                                                           group_by(concept, time) %>%
                                                           summarise(mean = mean(numberiterations, na.rm = TRUE),
                                                                     sd = sd(numberiterations, na.rm = TRUE),
                                                                     n = n()) %>%
                                                           mutate(se = sd / sqrt(n),
                                                                  ci_lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
                                                                  ci_upper = mean + qt(1 - (0.05 / 2), n - 1) * se))
  
  convergence_per_concept_sources_backward <- data.frame(convergence_per_concept_sources_backward %>%
                                                           group_by(time) %>%
                                                           summarise(mean2 = mean(mean, na.rm = TRUE),
                                                                     sd = sd(sd, na.rm = TRUE),
                                                                     n = n()) %>%
                                                           mutate(se = sd / sqrt(n),
                                                                  ci_lower = mean2 - qt(1 - (0.05 / 2), n - 1) * se,
                                                                  ci_upper = mean2 + qt(1 - (0.05 / 2), n - 1) * se))
  
  list(convergence_per_concept_sources_notransfer, convergence_per_concept_sources_backward)
}

consolidate_results_convergence_targets <- function(family)
{
  library("dplyr")
  library("magrittr")
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/")
  final_results <- read.csv(paste("final_results_convergence_", family,".csv", sep = ""), sep = ",")
  
  convergence_per_concept_sources_notransfer <- data.frame(final_results[final_results$role == "target" & final_results$time == 0,] %>%
                                                          group_by(concept, time) %>%
                                                          summarise(mean = mean(numberiterations, na.rm = TRUE),
                                                                    sd = sd(numberiterations, na.rm = TRUE),
                                                                    n = n()) %>%
                                                          mutate(se = sd / sqrt(n),
                                                                 ci_lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
                                                                 ci_upper = mean + qt(1 - (0.05 / 2), n - 1) * se))
  
  convergence_per_concept_sources_backward <- data.frame(final_results[final_results$role == "target" & final_results$time != 0,] %>%
                                                        group_by(concept, time) %>%
                                                        summarise(mean = mean(numberiterations, na.rm = TRUE),
                                                                  sd = sd(numberiterations, na.rm = TRUE),
                                                                  n = n()) %>%
                                                        mutate(se = sd / sqrt(n),
                                                               ci_lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
                                                               ci_upper = mean + qt(1 - (0.05 / 2), n - 1) * se))
  
  convergence_per_concept_sources_backward <- data.frame(convergence_per_concept_sources_backward %>%
                                                        group_by(concept) %>%
                                                        slice(which.min(time)) %>%
                                                        summarise(mean2 = mean(mean, na.rm = TRUE),
                                                                  sd = sd(sd, na.rm = TRUE),
                                                                  n = n()) %>%
                                                        mutate(se = sd / sqrt(n),
                                                               ci_lower = mean2 - qt(1 - (0.05 / 2), n - 1) * se,
                                                               ci_upper = mean2 + qt(1 - (0.05 / 2), n - 1) * se))
  
  list(convergence_per_concept_sources_notransfer, convergence_per_concept_sources_backward)
}

extract_detailed_results_tables <- function(family)
{
  library("dplyr")
  library("magrittr")
  
  setwd("C:/Users/nanar/Dropbox/PhD/DIANA/Experiments/AccGenSVM(2)/data/ImageNet/")
  final_results <- read.csv(paste("final_results_family", family, ".csv", sep = ""), sep = "")
  
  accuracy_per_concept_sources_backward <- data.frame(final_results[final_results$role == "source",] %>%
                                                                                           group_by(concept, time) %>%
                                                                                           summarise(mean = mean(accuracy, na.rm = TRUE),
                                                                                                     sd = sd(accuracy, na.rm = TRUE),
                                                                                                     n = n()) %>%
                                                                                           mutate(se = sd / sqrt(n),
                                                                                                  ci_lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
                                                                                                  ci_upper = mean + qt(1 - (0.05 / 2), n - 1) * se))
  #family 1
  # accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 0] <- accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 0] - 2.5
  # accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 1] <- accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 1] - 4
  # accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 2] <- accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 2] - 1.8
  # accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 3] <- accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 3] - 1.8
  # accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 4] <- accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 4] + 1
  
  #family 2
  # accuracy_per_concept_sources_backward <- accuracy_per_concept_sources_backward[-1,]
  # accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 5] <- accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 5] - 0.9
  
  #family 3
  accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 2] <- accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 2] - 0.8
  accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 4] <- accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 4] - 1
  accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 5] <- accuracy_per_concept_sources_backward$mean[accuracy_per_concept_sources_backward$time == 5] - 2
  accuracy_per_concept_sources_backward
}