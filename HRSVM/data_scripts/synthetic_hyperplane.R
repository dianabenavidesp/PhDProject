#convert file to libsvm-readable format
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

#read hyperplanes generated with python library at: https://scikit-learn.org/stable/auto_examples/svm/plot_separating_hyperplane.html
read_hyperplanes_python <- function()
{
  setwd("/synthetic_hyperplane/data")
  
  for(i in 1:1000)
  {
    hyp <- read.csv(paste("hyperplane", i, ".csv", sep = ""))
    hyp <- obtain_libsvm_file(hyp)
    
    write.table(hyp, file = paste("hyperplane", i, ".csv", sep = ""), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = "\t")
  }
}

#extract source, target and test levels
extract_samples_train_validation_test <- function(num_concepts, rep)
{
  percent_training <- 0.1
  setwd("/synthetic_hyperplane/data")
  
  #30 is the number of repetitions
  for(class in 1:num_concepts)
  {
    class_data <- read.table(paste("hyperplane", class, ".csv", sep = ""))
      training_test <- extract_sample_train_test_one(class_data, percent_training)
      
      #level of noise
      noise <- 0.45
      training <- training_test[[1]]
      training <- add_noise(training, noise)
      test <- training_test[[2]]
      
      write.table(training, file = paste(getwd(), "/rep", rep, "/hyperplane", class, "_training.csv", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
      write.table(test, file = paste(getwd(), "/rep", rep, "/hyperplane", class, "_test.csv", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
      gc()
  }
}

#helper function
extract_sample_train_test_one <- function(class_data, percent_training)
{
  #training sample
  instances_indices_for_training <- 1:nrow(class_data)
  training_instances_indices <- sample(instances_indices_for_training, round(nrow(class_data) * percent_training, 0), replace = FALSE, prob = NULL)
  
  instances_indices_for_test <- (1:nrow(class_data))[-training_instances_indices]
  testing_instances_indices <- sample(instances_indices_for_test, round(nrow(class_data) * (percent_training + 0.2), 0), replace = FALSE, prob = NULL)
  
  instances_indices_for_validation <- (1:nrow(class_data))[-c(training_instances_indices, testing_instances_indices)]
  validation_instances_indices <- sample(instances_indices_for_validation, round(nrow(class_data) * (percent_training + 0.2), 0), replace = FALSE, prob = NULL)
  
  training_sample <- class_data[training_instances_indices,]
  test_sample <- class_data[testing_instances_indices,]
  validation_sample <- class_data[validation_instances_indices,]
  
  list(training_sample, test_sample, validation_sample)
}

#add noise to make problems harder
add_noise <- function(training, noise)
{
  indexes_noise_positive <- sample(1:nrow(training), nrow(training) * noise, replace = FALSE)
  training[indexes_noise_positive, 1] <- training[indexes_noise_positive, 1]  * -1
  training
}


#helper function
exists_sample <- function(rep, concept, percent)
{
  setwd(paste("/synthetic_hyperplane/data/rep",rep,sep = ""))
  
  file.exists(paste("hyperplane", concept,"_training.csv",sep = ""))
}

#training commands
write_train_sources_commands <- function(number_concepts, percent, rep)
{
  training_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  
      for(i in 1:number_concepts)
      {
        concept <- i
        
        #sources
        if(exists_sample(rep, concept, percent))
        {
          data_dir <- ("/synthetic_hyperplane/data/")
          sources_dir <- ("/synthetic_hyperplane/sources/")
          
          training_file <- paste(data_dir, "rep",rep, "/", "hyperplane", concept, "_training.csv", sep = "")
          model_file <- paste(sources_dir, "model/rep",rep, "/", "hyperplane", concept, "_training.csv.model", sep = "")
          output_file <- paste(sources_dir, "model/rep",rep, "/", "hyperplane", concept, "_training.csv.txt", sep = "")
          training_command <- paste("java -jar libsvm_train.jar -s 0 -t 0 \"", training_file, "\" \"", model_file, "\" \"", output_file, "\" > \"", output_file, "\"", sep = "")
          
          training_commands[nrow(training_commands)+1,] <- training_command
        }
  }
  
  setwd("/synthetic_hyperplane/sources/")
  write.table(training_commands, paste(getwd(), "/training_commands_all.cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

#write commands for training with forward and then backward transfer (refinement)
#these commands will be helpful for calling the .jar file
train_forward_backward <- function(number_concepts, percent, rep)
{
  setwd("/synthetic_hyperplane/sources")
  
  n_features <- 10
  features <- ""; for(i in 1:n_features){ if(i != n_features) { features <- paste(features, i, "_", sep = "") } else { features <- paste(features, i, sep = "") } }
  
  forward_commands <- data.frame()

      balance_factor_forward <- 1
      balance_factor_backward <- 1
      
      data_dir <- (paste("/synthetic_hyperplane/data/rep", rep, sep = ""))
      model_dir <- (paste("/synthetic_hyperplane/sources/model/rep", rep, sep = ""))
      test_dir <- (paste("/synthetic_hyperplane/test/rep", rep, sep = ""))
      
      times <- floor(number_concepts / 2)
      
      setwd(model_dir)
      hyp <- list.files(pattern = paste("hyperplane.*\\.csv.model$", sep = ""))
      
      setwd(data_dir)
      data <- list.files(pattern = paste("hyperplane.*\\_training.csv$", sep = ""))
      
      #next times
      source_indexes <- sample(1:length(hyp), length(hyp) * 0.5, replace = FALSE)
      source <- hyp[as.numeric(gsub("_training.*", "", gsub("hyperplane", "", hyp))) == source_indexes[1]]
      sources <- hyp[as.numeric(gsub("_training.*", "", gsub("hyperplane", "", hyp))) %in% source_indexes[2:length(source_indexes)]]
        
      sources <- c(source, sources)
        
      target <- data[!((gsub(".csv$", "", data) %in% c(gsub(".csv.model$", "", sources), gsub("_t\\d.csv.model$", "", sources))))][1]
      targets <- data[!data %in% target & !((gsub(".csv$", "", data) %in% c(gsub(".csv.model$", "", sources), gsub("_t\\d.csv.model$", "", sources))))]
        
      previous_backward <- ""
      for(time in 1:250)
      {
        if(previous_backward[1] != ""){ sources <- previous_backward; target <- targets[time - 1] }
        
        predict_test_command(sources, model_dir, data_dir, test_dir, number_concepts, time - 1, rep) 
        write_transfer_forward_command(data_dir, model_dir, target, sources, time, n_features, features, balance_factor_forward, rep)
          
        target_model <- paste(gsub(".csv", "", target), "_t", time, ".csv.model", sep = "")
        previous_backward <- c(target_model, write_transfer_backward_command(model_dir, data_dir, target_model, sources, time, balance_factor_backward, rep))
        
        #copy rest of sources files with the same model, for making predictions easier
        write_copy_command(model_dir, sources, target, number_concepts, time, rep)
        #and remove previous model files, to release space
        write_delete_command(model_dir, test_dir, target, number_concepts, time, rep)
      }
      predict_test_command(sources, model_dir, data_dir, test_dir, number_concepts, time, rep) 
}

#commands for copying files when no transfer forward has occurred (created copies of existing models not subject to refinement)
write_copy_command <- function(model_dir, sources, target, number_concepts, time, rep)
{
  setwd("/synthetic_hyperplane/sources")
  
  for(i in 1:number_concepts)
  {
    if(time != 1 & (!paste("hyperplane", i, "_training_t", time - 1, ".csv.model", sep = "") %in% sources) & paste("hyperplane", i, sep = "") != strsplit(target, "_")[[1]][1])
    {
        command <- paste("copy ", paste(model_dir, "/hyperplane", i, "_training_t", time - 1, ".csv.model", sep = ""), paste(model_dir, "/hyperplane", i, "_training_t", time, ".csv.model", sep = ""), "")
        command <- gsub("/", "\\\\", command)
        write.table(command, paste(getwd(), "/forwardbackward_commands_all_hyperplane_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
    else if(time == 1 & (!paste("hyperplane", i, "_training.csv.model", sep = "") %in% sources) & paste("hyperplane", i, sep = "") != strsplit(target, "_")[[1]][1])
    {
        command <- paste("copy ", paste(model_dir, "/hyperplane", i, "_training.csv.model", sep = "" ), paste(model_dir, "/hyperplane", i, "_training_t", time, ".csv.model", sep = "" ), "")
        command <- gsub("/", "\\\\", command)
        write.table(command, paste(getwd(), "/forwardbackward_commands_all_hyperplane_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
  }
}

#for deleting unnecessary files, if required
write_delete_command <- function(model_dir, test_dir, target, number_concepts, time, rep)
{
  setwd("/synthetic_hyperplane/sources")
  
  for(i in 1:number_concepts)
  {
    if(time != 1)
    {
      #delete model
        command <- paste("del ", paste(model_dir, "/hyperplane", i, "_training_t", time - 1, ".csv.model", sep = "" ))
        command <- gsub("/", "\\\\", command)
        write.table(command, paste(getwd(), "/forwardbackward_commands_all_hyperplane_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
      #delete test file
        command <- paste("del ", paste(test_dir, "/hyperplane", i, "_test_t", time - 1, ".out", sep = "" ))
        command <- gsub("/", "\\\\", command)
        write.table(command, paste(getwd(), "/forwardbackward_commands_all_hyperplane_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
    else
    {
        #delete model
        command <- paste("del ", paste(model_dir, "/hyperplane", i, "_training.csv.model", sep = "" ))
        command <- gsub("/", "\\\\", command)
        write.table(command, paste(getwd(), "/forwardbackward_commands_all_hyperplane_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
        #delete test file
        command <- paste("del ", paste(test_dir, "/hyperplane", i, "_test_t0.out", sep = "" ))
        command <- gsub("/", "\\\\", command)
        write.table(command, paste(getwd(), "/forwardbackward_commands_all_hyperplane_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
  }
}

#command for transferring forward
#this will use the .jar file at https://github.com/nanarosebp/PhDProject/tree/master/AccGenSVM
write_transfer_forward_command <- function(current_dir, model_dir, target, sources, time, n_features, features, balance_factor_forward, rep)
{
  params <- paste("-s 0 -t 0 -K 0.30 -M 500 -N ", n_features, " -F ", features, " -D ", "\"", current_dir, "\"", " -S ", "\"", model_dir, "\"", " -f ", sep = "")
  #r_path <- "-Djava.library.path=/gpfs1m/apps/easybuild/RHEL6.3/sandybridge/software/R/3.5.0-gimkl-2017a/lib64/R/library/rJava/jri"
  r_path <- ""
  command <- paste("java ", r_path, " -jar accgensvm.jar ", params, "\"", target, "\"", sep = "")

  target_replace <- gsub(".csv", "", target)  
  command <- paste(command, " -y ", "\"", target_replace, "_t", time, ".csv.model", "\"", " -a ", "\"", target_replace, "_t", time, ".txt", "\"", "", sep = "")
  
  for(i in 1:length(sources))
  {
    command <- paste(command, " -H ", "\"", sources[i], "\"", sep = "")
  }
  
  setwd("/synthetic_hyperplane/sources")
  write.table(command, paste(getwd(), "/forwardbackward_commands_all_hyperplane_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
}

#writes single transfer backward command
#this will use the .jar file at https://github.com/nanarosebp/PhDProject/tree/master/HRSVM
write_transfer_backward_command <- function(model_dir, test_dir, target, sources, time, balance_factor_backward, rep)
{
  models <- ""
  for(source in sources)
  {
    if(time == 1) { source_replace <- gsub(".csv.model", "", source) } else { source_replace <- gsub(paste("_t", time - 1, ".csv.model", sep = ""), "", source) }
    
    test <- gsub("training", "test", source_replace)
    
    params <- paste("-s 1 -t 0 -G 0.001 -B ", balance_factor_backward, sep = "")
    command <- paste("java -jar hrsvm.jar ", params, " -H ", "\"", model_dir, "/", target, "\"", " -S ", "\"", model_dir, "/", source, "\"", " -M ", "\"", model_dir, "/", source_replace, "_t", time, ".csv.model", "\"", " > ", "\"", model_dir, "/", source_replace, "_t", time, ".txt", "\"", sep = "")
    
    setwd("/synthetic_hyperplane/sources")
    write.table(command, paste(getwd(), "/forwardbackward_commands_all_hyperplane_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    
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

#write a predict command to obtain performance right after each timestep of the sequence, so that later this can be evaluated
predict_test_command <- function(sources, model_dir, data_dir, test_dir, number_concepts, time, rep)
{
  predict_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  
    for(i in 1:number_concepts)
    {
      concept <- i
      
      #sources
      if(exists_sample(rep, concept, percent))
      {
        test_file <- paste(data_dir, "/hyperplane", concept, "_test.csv", sep = "")
        if(time == 0)
        {
          model_file <- paste(model_dir, "/hyperplane", concept, "_training.csv.model", sep = "")
        }
        else
        {
          model_file <- paste(model_dir, "/hyperplane", concept, "_training_t", time, ".csv.model", sep = "")
        }

        output_file <- paste(test_dir, "/hyperplane", concept, "_test_t", time, ".out", sep = "")
        txt_file <- paste(test_dir, "/hyperplane", concept, "_test_t", time, ".txt", sep = "")
        command <- paste("java -jar libsvm_predict.jar ", "\"", test_file, "\" \"", model_file, "\" \"", output_file, "\" > \"", txt_file, "\"", sep = "")
        
        predict_commands[nrow(predict_commands)+1,] <- command
      }
    }
  
  setwd("/synthetic_hyperplane/sources")
  write.table(predict_commands, paste(getwd(), "/forwardbackward_commands_all_hyperplane_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
}