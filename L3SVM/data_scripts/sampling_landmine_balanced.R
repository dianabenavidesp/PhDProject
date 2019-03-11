#1) CREATE SINGLE SAMPLES
read_all_file <- function()
{
  setwd("data/landmine")
  all <- read.csv("all.csv")
  
  for(cl in unique(all[,10]))
  {
    write.table(all[all[,10] == cl,-10], file = paste("land_", cl, ".csv"), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
  }
}

read_group_file <- function(class)
{
  setwd("data/landmine")
  class_data <- read.csv(paste("land_", class, ".csv", sep = ""))
  class_data
}

extract_samples_train_test <- function(class_name, class, percent_training)
{
  setwd(paste("data/landmine/samples/land_", class_name, sep = ""))
  
  #30 is the number of repetitions
  for(i in 1:30)
  {
    rep <- i
    training_test <- extract_sample_train_test_one(class, percent_training)
    
    training <- training_test[[1]]
    test <- training_test[[2]]
    
    write.table(training, file = paste(getwd(), "/", percent_training*100, "percent/rep", rep, "/", "training.csv", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
    write.table(test, file = paste(getwd(), "/", percent_training*100, "percent/rep", rep, "/", "test.csv", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
  }
}

extract_sample_train_test_one <- function(class_data, percent_training)
{
  #training sample
  instances_indices_for_training <- 1:nrow(class_data)
  training_instances_indices <- sample(instances_indices_for_training, round(nrow(class_data) * percent_training, 0), replace = FALSE, prob = NULL)
  
  instances_indices_for_test <- (1:nrow(class_data))[-training_instances_indices]
  testing_instances_indices <- sample(instances_indices_for_test, round(nrow(class_data) * (percent_training + 0.2), 0), replace = FALSE, prob = NULL)
  
  training_sample <- class_data[training_instances_indices,]
  test_sample <- class_data[testing_instances_indices,]
  
  list(training_sample, test_sample)
}


#CONSOLE COMMANDS
for(group in 1:29)
{
  class_data <- read_group_file(group)
  extract_samples_train_test(group, class_data, 0.10)
}

----------------------------
groups <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29)
  #2) CREATE SAMPLES AT THE SOURCE, TRANSFER AND TEST LEVEL
  extract_all_data <- function(groups, percent)
  {
    runs <- length(groups)
    reps <- 30
    num_classes <- length(groups)
    
    #runs starts in 0 to make it consistent with themes indexes
    for(run in 1:runs)
    {
      for(rep in 1:30)
      {
        source_actual <- run
        target_actual <- ifelse(run < (length(groups)), run + 1, 1) 
        
        rest <- 1:(length(groups))
        rest <- rest[-c(source_actual, target_actual)]
        sources_rest <- sample(rest, ceiling((num_classes-2) / 2), replace = FALSE)
        targets_rest <- rest[!rest %in% sources_rest]
        
        #source actual, if not exists
        if(!exists_sample(paste("run_", run, "_source", sep = ""), rep, source_actual, percent))
        {
          extract_source(paste("run_", run, "_source", sep = ""), rep, source_actual, percent)
        }
        
        #target actual
        if(!exists_sample(paste("run_", run, "_target", sep = ""), rep, target_actual, percent))
        {
          extract_source(paste("run_", run, "_target", sep = ""), rep, target_actual, percent)
        }
        
        #other sources
        for(source_rest in sources_rest)
        {
          if(!exists_sample(paste("run_", run, "_source", sep = ""), rep, source_rest, percent))
          {
            extract_source(paste("run_", run, "_source", sep = ""), rep, source_rest, percent)
          }
        }
        
        #other targets
        for(target_rest in targets_rest)
        {
          if(!exists_sample(paste("run_", run, "_target", sep = ""), rep, target_rest, percent))
          {
            extract_source(paste("run_", run, "_target", sep = ""), rep, target_rest, percent)
          }
        }
      }
    }
  }

exists_sample <- function(role, rep, class_name, percent)
{
  setwd(paste("data/landmine/sources/data/",percent*100,"percent/rep",rep,sep = ""))
  
  file.exists(paste(role, "_land_", class_name,"_vsall_balanced_training.csv",sep = ""))
}

extract_source <- function(role, rep, class_name, percent)
{
  negative_all_training <- data.frame()
  negative_all_test <- data.frame()
  
  setwd("data/landmine/samples")
  
  for(group in 1:29)
  {
    #read training
    training_sample <- read.csv(paste(getwd(), "/land_",group,"/",percent*100,"percent/rep",rep,"/training.csv",sep = ""))
    colnames(training_sample) <- c("")
    
    #read test
    test_sample <- read.csv(paste(getwd(), "/land_",group,"/",percent*100,"percent/rep",rep,"/test.csv",sep = ""))
    colnames(test_sample) <- c("")
    
    if(group == class_name)
    {
      positive_training <- training_sample
      positive_test <- test_sample
    }
    
    else
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
  
  #resample negative to balance:
  negative_all_training <- negative_all_training[sample(1:nrow(negative_all_training), round(nrow(positive_training), 0), replace = FALSE, prob = NULL),]
  negative_all_test <- negative_all_test[sample(1:nrow(negative_all_test), round(nrow(positive_test), 0), replace = FALSE, prob = NULL),]
  
  #sample
  sample_training <- rbind(positive_training, negative_all_training)
  sample_test <- rbind(positive_test, negative_all_test)
  
  #change order of columbs (first label, then data)
  sample_training <- sample_training[,c(ncol(sample_training), (1:(ncol(sample_training)-1)))]
  sample_test <- sample_test[,c(ncol(sample_training), (1:(ncol(sample_test)-1)))]
  
  setwd(paste("data/landmine/sources/data/",percent*100,"percent/rep",rep,sep = ""))
  write.table(sample_training, file = paste(role, "_", "land_",class_name,"_vsall_balanced_training.csv",sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(sample_test, file = paste(role, "_", "land_",class_name,"_vsall_balanced_test.csv",sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}


-----------------
#3 PREPARE SOURCES
groups <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29)
prepare_sources <- function(groups, percent)
{
  for(rep in 1:30)
  {
    setwd(paste("data/landmine/sources/data/",percent*100,"percent/rep",rep,sep = ""))
    
    #for each run
    for(run in 1:length(groups))
    {
      for(i in 1:length(groups))
      {
        group <- groups[i]
        
        #if source for this run
        role <- paste("run_", run, "_source", sep = "")
        if(exists_sample(role, rep, group, percent))
        {
          training <- read.csv(paste(getwd(), "/", role, "_", "land_", group, "_vsall_balanced_training.csv", sep = ""), sep = " ", header = FALSE)
          training <- obtain_libsvm_file(training)
          write.table(training, file = paste(getwd(), "/", role, "_", "land_", group, "_vsall_balanced_training.csv.prescale", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
          
          test <- read.csv(paste(getwd(), "/", role, "_", "land_", group, "_vsall_balanced_test.csv", sep = ""), sep = " ", header = FALSE)
          test <- obtain_libsvm_file(test)
          write.table(test, file = paste(getwd(), "/", role, "_", "land_", group, "_vsall_balanced_test.csv.prescale", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
        }
        
        role <- paste("run_", run, "_target", sep = "")
        if(exists_sample(role, rep, group, percent))
        {
          training <- read.csv(paste(getwd(), "/", role, "_", "land_", group, "_vsall_balanced_training.csv", sep = ""), sep = " ", header = FALSE)
          training <- obtain_libsvm_file(training)
          write.table(training, file = paste(getwd(), "/", role, "_", "land_", group, "_vsall_balanced_training.csv.prescale", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
          
          test <- read.csv(paste(getwd(), "/", role, "_", "land_", group, "_vsall_balanced_test.csv", sep = ""), sep = " ", header = FALSE)
          test <- obtain_libsvm_file(test)
          write.table(test, file = paste(getwd(), "/", role, "_", "land_", group, "_vsall_balanced_test.csv.prescale", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
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

write_scale_commands <- function(groups, percent)
{
  scale_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  
  for(run in 1:length(groups))
  {
    for(rep in 1:30)
    {
      for(i in 1:length(groups))
      {
        group <- groups[i]
        
        #sources
        role <- paste("run_", run, "_source", sep = "")
        if(exists_sample(role, rep, group, percent))
        {
          setwd("data/landmine/sources/")
          file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.prescale", sep = "")
          output_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.scale", sep = "")
          scale_command <- paste("svm-scale ", file, " > ", output_file, sep = "")
          
          scale_commands[nrow(scale_commands)+1,] <- scale_command
        }
        
        #targets
        role <- paste("run_", run, "_target", sep = "")
        if(exists_sample(role, rep, group, percent))
        {
          setwd("data/landmine/sources/")
          file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.prescale", sep = "")
          output_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.scale", sep = "")
          scale_command <- paste("svm-scale ", file, " > ", output_file, sep = "")
          
          scale_commands[nrow(scale_commands)+1,] <- scale_command
        }
      }
    }
  }
  setwd("data/landmine/sources/")
  write.table(scale_commands, paste(getwd(), "/scale_commands_all.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

write_train_sources_commands <- function(groups, percent)
{
  training_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  
  for(run in 1:length(groups))
  {
    for(rep in 1:30)
    {
      for(i in 1:length(groups))
      {
        group <- groups[i]
        
        #sources
        role <- paste("run_", run, "_source", sep = "")
        if(exists_sample(role, rep, group, percent))
        {
          setwd("data/landmine/sources/")
          
          training_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.scale", sep = "")
          model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.model", sep = "")
          output_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.txt", sep = "")
          training_command <- paste("svm-train -s 0 -t 2 ", training_file, " ", model_file, " ", output_file, " > ", output_file, sep = "")
          
          training_commands[nrow(training_commands)+1,] <- training_command
        }
        
        role <- paste("run_", run, "_target", sep = "")
        if(exists_sample(role, rep, group, percent))
        {
          setwd("data/landmine/sources/")
          
          training_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.scale", sep = "")
          model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.model", sep = "")
          output_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.txt", sep = "")
          training_command <- paste("svm-train -s 0 -t 2 ", training_file, " ", model_file, " ", output_file, " > ", output_file, sep = "")
          
          training_commands[nrow(training_commands)+1,] <- training_command
        }
      }
    }
  }
  
  setwd("data/landmine/sources/")
  write.table(training_commands, paste(getwd(), "/training_commands_all.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

write_predict_commands <- function(groups, percent)
{
  predict_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  
  for(run in 1:length(groups))
  {
    for(rep in 1:30)
    {
      for(i in 1:length(groups))
      {
        group <- groups[i]
        
        #sources
        role <- paste("run_", run, "_source", sep = "")
        if(exists_sample(role, rep, group, percent))
        {
          setwd("data/landmine/sources/")
          
          test_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test.csv.scale", sep = "")
          model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.model", sep = "")
          
          setwd("data/landmine/test/")
          output_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test.out", sep = "")
          txt_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test.txt", sep = "")
          command <- paste("svm-predict ", test_file, " ", model_file, " ", output_file, " > ", txt_file, sep = "")
          
          predict_commands[nrow(predict_commands)+1,] <- command
        }
        
        role <- paste("run_", run, "_target", sep = "")
        if(exists_sample(role, rep, group, percent))
        {
          setwd("data/landmine/sources/")
          
          test_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test.csv.scale", sep = "")
          model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.model", sep = "")
          
          setwd("data/landmine/test/")
          output_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test.out", sep = "")
          txt_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test.txt", sep = "")
          command <- paste("svm-predict ", test_file, " ", model_file, " ", output_file, " > ", txt_file, sep = "")
          
          predict_commands[nrow(predict_commands)+1,] <- command
        }
      }
    }
  }
  
  setwd("data/landmine/sources/")
  write.table(predict_commands, paste(getwd(), "/predict_commands_all.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

#3)PREPARE COMMANDS FOR TRAINING AND TESTING APPROACH (FORWARD AND BACKWARD)
train_forward_backward <- function(groups, percent)
{
  n_features <- 9
  features <- ""; for(i in 1:n_features){ if(i != n_features) { features <- paste(features, i, "_", sep = "") } else { features <- paste(features, i, sep = "") } }
  
  forward_commands <- data.frame()
  
  for(run in 1:length(groups))
  {
    for(rep in 1:30)
    {
      balance_factor_forward <- 0.01
      balance_factor_backward <- 0.5
      
      data_dir <- (paste("data/landmine/sources/data/", percent * 100, "percent/rep", rep, sep = ""))
      model_dir <- (paste("data/landmine/sources/model/", percent * 100, "percent/rep", rep, sep = ""))
      
      times <- (length(groups) - 1) / 2
      
      setwd(model_dir)
      sources <- list.files(pattern = paste("run_",run,"_source_.*\\_vsall_balanced_training.csv\\.model$", sep = ""))
      
      setwd(data_dir)
      targets <- list.files(pattern = paste("run_",run,"_target_.*\\_vsall_balanced_training.csv\\.scale$", sep = ""))
      
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
  
  setwd("data/landmine/sources")
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
    
    setwd("data/landmine/sources")
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
write_predict_commands_after_transfer <- function(groups, percent)
{
  predict_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  times <- (length(groups) - 1) / 2
  
  for(run in 1:(length(groups)))
  {
    for(rep in 1:30)
    {
      for(i in 1:length(groups))
      {
        group <- groups[i]
        
        #including 0 for no transfer
        for(time in 0:times)
        {
          #sources
          role <- paste("run_", run, "_source", sep = "")
          if(exists_model_transfer(role, rep, group, percent, time))
          {
            setwd("data/landmine/sources/")
            
            test_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test.csv.scale", sep = "")
            
            if(time == 0)
            {
              model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.model", sep = "")
            }
            else
            {
              model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training_t", time, ".csv.model", sep = "")
            }
            
            setwd("data/landmine/test/")
            output_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test_t", time, ".out", sep = "")
            txt_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test_t", time, ".txt", sep = "")
            command <- paste("svm-predict ", test_file, " ", model_file, " ", output_file, " > ", txt_file, sep = "")
            
            predict_commands[nrow(predict_commands)+1,] <- command
          }
          
          role <- paste("run_", run, "_target", sep = "")
          if(exists_model_transfer(role, rep, group, percent, time))
          {
            setwd("data/landmine/sources/")
            
            test_file <- paste(getwd(), "/data/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test.csv.scale", sep = "")
            if(time == 0)
            {
              model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training.csv.model", sep = "")
            }
            else
            {
              model_file <- paste(getwd(), "/model/",percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_training_t", time, ".csv.model", sep = "")
            }
            
            
            setwd("data/landmine/test/")
            output_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test_t", time, ".out", sep = "")
            txt_file <- paste(getwd(), "/", percent*100,"percent/rep",rep, "/", role, "_land_", group, "_vsall_balanced_test_t", time, ".txt", sep = "")
            command <- paste("svm-predict ", test_file, " ", model_file, " ", output_file, " > ", txt_file, sep = "")
            
            predict_commands[nrow(predict_commands)+1,] <- command
          }
        }
      }
    }
  }
  
  setwd("data/landmine/sources/")
  write.table(predict_commands, paste(getwd(), "/predict_commands_transfer_all.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

exists_model_transfer <- function(role, rep, group, percent, time)
{
  setwd(paste("data/landmine/sources/model/",percent*100,"percent/rep",rep,sep = ""))
  
  if(time > 0) { file.exists(paste(role, "_land_", group, "_vsall_balanced_training_t", time, ".csv.model", sep = "")) & file.size(paste(role, "_land_", group, "_vsall_balanced_training_t", time, ".csv.model", sep = "")) > 0 }
  else { file.exists(paste(role, "_land_", group, "_vsall_balanced_training.csv.model", sep = "")) & file.size(paste(role, "_land_", group, "_vsall_balanced_training.csv.model", sep = "")) > 0  }
}