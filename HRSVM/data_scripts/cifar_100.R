#provided examples from each fo the 100 classes are each stored separately into .csv files,
#this function extracts train and test samples
#those samples are assumed to be already scaled (e.g. on the range -1, 1)
extract_samples_train <- function(num_concepts)
{
  percent_training <- 0.1
  setwd("/cifar-100/data")
  
  #30 is the number of repetitions, which can be changed
  for(class in 0:(num_concepts-1))
  {
    class_data <- read.table(paste("cifar", class, "_train.csv", sep = ""))
    
    for(rep in 1:30)
    {
      instances_indexes <- sample(1:nrow(class_data), 100, replace = FALSE)
      training <- class_data[instances_indexes,]
      
      write.table(training, file = paste(getwd(), "/rep", rep, "/cifar", class, "_training.csv", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
      gc()
    }
  }
}

#compose binary classification tasks (balanced)
extract_samples <- function(percent, rep)
{
  setwd(paste("/cifar-100/data/rep", rep, sep = ""))
  training_files <- list.files(pattern = "*._training.csv")
  
  setwd(paste("/cifar-100/data", sep = ""))
  test_files <- list.files(pattern = "*._test.csv")
  
  #training, test and validation data
  setwd(paste("/cifar-100/data/rep", rep, sep = ""))
  training_data_all <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:length(training_files))
  {
    training_data_all <- rbind(training_data_all, read.csv(training_files[i], header = FALSE))
  }
  
  setwd(paste("/cifar-100/data", sep = ""))
  test_data_all <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:length(test_files))
  {
    test_data_all <- rbind(test_data_all, read.csv(test_files[i], header = FALSE))
  }
  
  
  #extract and write files
  all_indexes_training <- seq(from = 1, to = nrow(training_data_all), by = 1)
  all_indexes_test <- seq(from = 1, to = nrow(test_data_all), by = 1)
  
  j <- 0
  for(i in 1:length(training_files))
  {
    print(j)
    class <- paste("", gsub("_training.csv", "", training_files[i]), sep = "")
    
    #training
    positive_examples_indexes <- seq(from = ((j*100) + 1), to = ((j*100) + 100), by = 1)
    negative_examples_indexes <- sample(all_indexes_training[-positive_examples_indexes],100, replace = FALSE)
    
    #from second column
    positive_examples <- training_data_all[positive_examples_indexes,2:ncol(training_data_all)]
    negative_examples <- training_data_all[negative_examples_indexes,2:ncol(training_data_all)]
    
    #add class
    positive_examples$label <- 1
    negative_examples$label <- -1
    
    #rearrange
    positive_examples <- positive_examples[,c(ncol(positive_examples), (1:(ncol(positive_examples)-1)))]
    negative_examples <- negative_examples[,c(ncol(negative_examples), (1:(ncol(negative_examples)-1)))]
    
    #convert to libsvm
    examples <- rbind(positive_examples, negative_examples)
    examples <- obtain_libsvm_file(examples)
    
    #test
    positive_examples_indexes_test <- seq(from = ((j*100) + 1), to = ((j*100) + 100), by = 1)
    negative_examples_indexes_test <- sample(all_indexes_test[-positive_examples_indexes_test], 100, replace = FALSE)
    
    positive_examples_test <- test_data_all[positive_examples_indexes_test,2:ncol(test_data_all)]
    negative_examples_test <- test_data_all[negative_examples_indexes_test,2:ncol(test_data_all)]
    
    #add class
    positive_examples_test$label <- 1
    negative_examples_test$label <- -1
    
    #rearrange
    positive_examples_test <- positive_examples_test[,c(ncol(positive_examples_test), (1:(ncol(positive_examples_test)-1)))]
    negative_examples_test <- negative_examples_test[,c(ncol(negative_examples_test), (1:(ncol(negative_examples_test)-1)))]
    
    #convert to libsvm
    examples_test <- rbind(positive_examples_test, negative_examples_test)
    examples_test <- obtain_libsvm_file(examples_test)
    
    #write files
    setwd(paste("/cifar-100/sources/data/rep", rep, sep = ""))
    write.table(examples, file = paste(getwd(), "/", class, "_training.csv", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
    write.table(examples_test, file = paste(getwd(), "/", class, "_test.csv", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
    j <- j + 1
  }
}

#extract source, target and test levels
exists_sample <- function(rep, concept, percent)
{
  setwd(paste("/cifar-100/data/rep",rep,sep = ""))
  
  file.exists(paste("cifar", concept,"_training.csv",sep = ""))
}

write_train_sources_commands <- function(number_concepts, percent, rep)
{
  training_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  
    for(i in 0:(number_concepts-1))
    {
      concept <- i
      
      #sources
      if(exists_sample(rep, concept, percent))
      {
        data_dir <- ("/cifar-100/sources/data/")
        sources_dir <- ("/cifar-100/sources/")
        
        training_file <- paste(data_dir, "rep",rep, "/", "cifar", concept, "_training.csv", sep = "")
        model_file <- paste(sources_dir, "model/rep",rep, "/", "cifar", concept, "_training.csv.model", sep = "")
        output_file <- paste(sources_dir, "model/rep",rep, "/", "cifar", concept, "_training.csv.txt", sep = "")
        training_command <- paste("java -jar libsvm_train.jar -s 0 -t 2 \"", training_file, "\" \"", model_file, "\" \"", output_file, "\" > \"", output_file, "\"", sep = "")
        
        training_commands[nrow(training_commands)+1,] <- training_command
      }
    }
  
  setwd("/cifar-100/sources/")
  write.table(training_commands, paste(getwd(), "/training_commands_all.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

#write commands for training with forward and then backward transfer (refinement)
#these commands will be helpful for calling the .jar file
train_forward_backward <- function(number_concepts, percent, rep)
{
  setwd("/cifar-100/sources")
  
  n_features <- 3072
  features <- ""; for(i in 1:n_features){ if(i != n_features) { features <- paste(features, i, "_", sep = "") } else { features <- paste(features, i, sep = "") } }
  
  forward_commands <- data.frame()
  
  balance_factor_forward <- 1
  balance_factor_backward <- 1
  
  data_dir <- (paste("/cifar-100/sources/data/rep", rep, sep = ""))
  model_dir <- (paste("/cifar-100/sources/model/rep", rep, sep = ""))
  test_dir <- (paste("/cifar-100/test/rep", rep, sep = ""))
  
  times <- floor(number_concepts / 2)
  
  setwd(model_dir)
  hyp <- list.files(pattern = paste("cifar.*\\_training.csv.model$", sep = ""))
  
  setwd(data_dir)
  data <- list.files(pattern = paste("cifar.*\\_training.csv$", sep = ""))
  
  #next times
  source_indexes <- sample(1:length(hyp), length(hyp) * 0.5, replace = FALSE)
  source <- hyp[as.numeric(gsub("_training.*", "", gsub("cifar", "", hyp))) == source_indexes[1]]
  sources <- hyp[as.numeric(gsub("_training.*", "", gsub("cifar", "", hyp))) %in% source_indexes[2:length(source_indexes)]]
  
  sources <- c(source, sources)
  
  target <- data[!((gsub(".csv$", "", data) %in% c(gsub(".csv.model$", "", sources), gsub("_t\\d.csv.model$", "", sources))))][1]
  targets <- data[!data %in% target & !((gsub(".csv$", "", data) %in% c(gsub(".csv.model$", "", sources), gsub("_t\\d.csv.model$", "", sources))))]
  
  previous_backward <- ""
  for(time in 1:50)
  {
    if(previous_backward[1] != ""){ sources <- previous_backward; target <- targets[time - 1] }
    
    predict_test_command(sources, model_dir, data_dir, test_dir, number_concepts, time - 1, rep) 
    write_transfer_forward_command(data_dir, model_dir, target, sources, time, n_features, features, balance_factor_forward, rep)
    
    target_model <- paste(gsub(".csv", "", target), "_t", time, ".csv.model", sep = "")
    previous_backward <- c(target_model, write_transfer_backward_command(model_dir, data_dir, target_model, sources, time, balance_factor_backward, rep))
    
    #copy rest of sources files with the same model, for making predictions easier
    write_copy_command(model_dir, sources, target, number_concepts, time, rep)
    write_delete_command(model_dir, test_dir, target, number_concepts, time, rep)
  }
  predict_test_command(sources, model_dir, data_dir, test_dir, number_concepts, time, rep) 
}

#commands for copying files when no transfer forward has occurred (created copies of existing models not subject to refinement)
write_copy_command <- function(model_dir, sources, target, number_concepts, time, rep)
{
  setwd("/cifar-100/sources")
  
  for(i in 0:(number_concepts-1))
  {
    if(time != 1 & (!paste("cifar", i, "_training_t", time - 1, ".csv.model", sep = "") %in% sources) & paste("cifar", i, sep = "") != strsplit(target, "_")[[1]][1])
    {
      command <- paste("copy ", paste(model_dir, "/cifar", i, "_training_t", time - 1, ".csv.model", sep = "" ), paste(model_dir, "/cifar", i, "_training_t", time, ".csv.model", sep = "" ), "&>/dev/null")
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_cifar_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
    else if(time == 1 & (!paste("cifar", i, "_training.csv.model", sep = "") %in% sources) & paste("cifar", i, sep = "") != strsplit(target, "_")[[1]][1])
    {
      command <- paste("copy ", paste(model_dir, "/cifar", i, "_training.csv.model", sep = "" ), paste(model_dir, "/cifar", i, "_training_t", time, ".csv.model", sep = "" ), "&>/dev/null")
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_cifar_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
  }
}

#delete files when no necessary
write_delete_command <- function(model_dir, test_dir, target, number_concepts, time, rep)
{
  setwd("/cifar-100/sources")
  
  for(i in 0:(number_concepts-1))
  {
    if(time != 1)
    {
      #model file
      command <- paste("del ", paste(model_dir, "/cifar", i, "_training_t", time - 1, ".csv.model", sep = "" ))
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_cifar_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
      
      #test file
      command <- paste("del ", paste(test_dir, "/cifar", i, "_test_t", time - 1, ".out", sep = "" ))
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_cifar_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
    else
    {
      #model file
      command <- paste("del ", paste(model_dir, "/cifar", i, "_training.csv.model", sep = "" ))
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_cifar_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
      
      #test file
      command <- paste("del ", paste(test_dir, "/cifar", i, "_test_t0.out", sep = "" ))
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_cifar_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
  }
}

#command for transferring forward
#this will use the .jar file at https://github.com/nanarosebp/PhDProject/tree/master/AccGenSVM
write_transfer_forward_command <- function(current_dir, model_dir, target, sources, time, n_features, features, balance_factor_forward, rep)
{
  params <- paste("-s 0 -t 2 -K 0.5 ", " -M 100 -N ", n_features, " -F ", features, " -D ", "\"", current_dir, "\"", " -S ", "\"", model_dir, "\"", " -f ", sep = "")
  #r_path <- "-Djava.library.path=/gpfs1m/apps/easybuild/RHEL6.3/westmere/software/R/3.4.0-gimkl-2017a/lib64/R/library/rJava/jri"
  command <- paste("java -jar accgensvm.jar ", params, "\"", target, "\"", sep = "")
  
  for(i in 1:length(sources))
  {
    command <- paste(command, " -H ", "\"", sources[i], "\"", sep = "")
  }
  
  target_replace <- gsub(".csv", "", target)  
  command <- paste(command, " -y ", "\"", target_replace, "_t", time, ".csv.model", "\"", " -a ", "\"", target_replace, "_t", time, ".txt", "\"", " &>/dev/null", sep = "")
  
  setwd("/cifar-100/sources")
  write.table(command, paste(getwd(), "/forwardbackward_commands_all_cifar_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
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
    
    params <- paste("-s 1 -t 2 -G 0.01 -B ", balance_factor_backward, sep = "")
    command <- paste("java -jar hrsvm.jar ", params, " -H ", "\"", model_dir, "/", target, "\"", " ", " -S ", "\"", model_dir, "/", source, "\"", " -M ", "\"", model_dir, "/", source_replace, "_t", time, ".csv.model", "\"", " > ", "\"", model_dir, "/", source_replace, "_t", time, ".txt", "\"", " &>/dev/null", sep = "")
    
    setwd("/cifar-100/sources")
    write.table(command, paste(getwd(), "/forwardbackward_commands_all_cifar_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    
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
  
  for(i in 0:(number_concepts-1))
  {
    concept <- i
    
    #sources
    if(exists_sample(rep, concept, percent))
    {
      test_file <- paste(data_dir, "/cifar", concept, "_test.csv", sep = "")
      if(time == 0)
      {
        model_file <- paste(model_dir, "/cifar", concept, "_training.csv.model", sep = "")
      }
      else
      {
        model_file <- paste(model_dir, "/cifar", concept, "_training_t", time, ".csv.model", sep = "")
      }
      
      output_file <- paste(test_dir, "/cifar", concept, "_test_t", time, ".out", sep = "")
      txt_file <- paste(test_dir, "/cifar", concept, "_test_t", time, ".txt", sep = "")
      command <- paste("java -jar libsvm_predict.jar ", "\"", test_file, "\" \"", model_file, "\" \"", output_file, "\" > \"", txt_file, "\"", sep = "")
      
      predict_commands[nrow(predict_commands)+1,] <- command
    }
  }
  
  setwd("/cifar-100/sources")
  write.table(predict_commands, paste(getwd(), "/forwardbackward_commands_all_cifar_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
}

#transform to libsvm file
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