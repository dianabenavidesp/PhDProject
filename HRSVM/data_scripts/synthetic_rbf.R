#base centroids
set.seed(1101174227)

base_centroids <- list()
base_centroids_sd <- list()
for(j in 1:500)
{
  centroid <- runif(100, min = 0, max = 1)
  centroid_sd <- runif(1, min = 0, max = 1)
  
  base_centroids[[j]] <- centroid  
  base_centroids_sd[[j]] <- centroid_sd
}

#generate rbf concepts
hyps <- list()
for(i in 1:500)
{
  base_centroid <- sample(1:100, 1, replace = FALSE)
  
  actual_centroid <- base_centroids[[base_centroid]]
  actual_sd <- base_centroids_sd[[base_centroid]]
  
  #generate points from this centroid, using a gaussian with mean actual_centroid and sd sd
  #100 features
  random_points <- matrix(0, nrow = 1000, ncol = 100)
  for(k in 1:1000)
  {
    random_features <- (runif(100, min = 0, max = 1) * 2) - 1
    
    magnitude <- 0
    for(m in 1:100)
    {
      magnitude <- magnitude + (random_features[m] * random_features[m])
    }
    magnitude <- sqrt(magnitude)
    
    desired_magnitude <- runif(1, min = 0, max = 1) * actual_sd
    scale <- desired_magnitude / magnitude
    
    for(m in 1:100)
    {
      random_features[m] <- actual_centroid[m] + random_features[m] * scale 
    }
    
    #rescale to 0-1
    random_features <- random_features - min(random_features) / (max(random_features) - min(random_features))
    random_points[k,] <- random_features
  }
  
  hyps[[i]] <- random_points
  
  setwd("/synthetic_rbf/samples")
  write.table(random_points, file = paste("rbf", i, ".csv", sep = ""), col.names = FALSE, row.names = FALSE, quote = FALSE)
}

#read file of RBF concept
read_class_file <- function(rbf)
{
  setwd("/synthetic_rbf/samples")
  class <- read.csv(paste("rbf", rbf, ".csv", sep = ""), sep = " ", header = FALSE)
  class
}

#provided examples from each fo the 20 classes are each stored separately into .csv files,
#this function extracts train and test samples
#those samples are assumed to be already scaled (e.g. on the range -1, 1)
extract_samples_train_validation_test <- function(percent_training, percent_test, percent_validation)
{
  setwd("/synthetic_rbf/samples")
  
  #30 is the number of repetitions
  for(j in 1:500)
  {
    class_data <- read_class_file(j)

    for(i in 1:1)
    {
      rep <- i
      training_test_validation <- extract_sample_train_test_one(class_data, percent_training, percent_test, percent_validation)
      
      #does not make sense to add noise here, because every file is a single concept
      #noise <- 0.50
      training <- training_test_validation[[1]]
      #training <- add_noise(training, noise)
      test <- training_test_validation[[2]]
      
      setwd("/synthetic_rbf/data")    
      write.table(training, file = paste(getwd(), "/rep", rep, "/rbf", j, "_training.csv", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
      write.table(test, file = paste(getwd(), "/rep", rep, "/rbf", j, "_test.csv", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
      gc()
    }
  }
}

#helper function
extract_sample_train_test_one <- function(class_data, percent_training, percent_test, percent_validation)
{
  #training sample
  instances_indices_for_training <- 1:nrow(class_data)
  training_instances_indices <- sample(instances_indices_for_training, round(nrow(class_data) * percent_training, 0), replace = FALSE, prob = NULL)
  
  instances_indices_for_test <- (1:nrow(class_data))[-training_instances_indices]
  testing_instances_indices <- sample(instances_indices_for_test, round(nrow(class_data) * percent_test, 0), replace = FALSE, prob = NULL)
  
  instances_indices_for_validation <- (1:nrow(class_data))[-c(training_instances_indices, testing_instances_indices)]
  validation_instances_indices <- sample(instances_indices_for_validation, round(nrow(class_data)* percent_validation, 0), replace = FALSE, prob = NULL)
  
  training_sample <- class_data[training_instances_indices,]
  test_sample <- class_data[testing_instances_indices,]
  validation_sample <- class_data[validation_instances_indices,]
  
  list(training_sample, test_sample, validation_sample)
}


#create binary samples at the source, target and test levels
extract_samples <- function(percent, rep)
{
  setwd(paste("/synthetic_rbf/data/rep", rep, sep = ""))
  training_files <- list.files(pattern = "*._training.csv")
  test_files <- list.files(pattern = "*._test.csv")
  
  #training, test and validation data
  training_data_all <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:length(training_files))
  {
    training_data_all <- rbind(training_data_all, read.csv(training_files[i], header = FALSE, sep = "\t"))
  }
  
  test_data_all <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:length(test_files))
  {
    test_data_all <- rbind(test_data_all, read.csv(test_files[i], header = FALSE, sep = "\t"))
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
    
    positive_examples <- training_data_all[positive_examples_indexes,]
    negative_examples <- training_data_all[negative_examples_indexes,]
    
    #add class
    positive_examples$label <- 1
    negative_examples$label <- -1
    
    #add noise
    noise_positive <- 0.45
    indexes_noise_positive <- sample(1:nrow(positive_examples), nrow(positive_examples) * noise_positive, replace = FALSE)
    positive_examples$label[indexes_noise_positive] <- positive_examples$label[indexes_noise_positive] * -1
    
    noise_negative <- 0.45
    indexes_noise_negative <- sample(1:nrow(negative_examples), nrow(negative_examples) * noise_negative, replace = FALSE)
    negative_examples$label[indexes_noise_negative] <- negative_examples$label[indexes_noise_negative] * -1
    
    #rearrange
    positive_examples <- positive_examples[,c(ncol(positive_examples), (1:(ncol(positive_examples)-1)))]
    negative_examples <- negative_examples[,c(ncol(negative_examples), (1:(ncol(negative_examples)-1)))]
    
    #convert to libsvm
    examples <- rbind(positive_examples, negative_examples)
    examples <- obtain_libsvm_file(examples)
    
    #test
    positive_examples_indexes_test <- seq(from = ((j*300) + 1), to = ((j*300) + 300), by = 1)
    negative_examples_indexes_test <- sample(all_indexes_test[-positive_examples_indexes_test], 300, replace = FALSE)
    
    positive_examples_test <- test_data_all[positive_examples_indexes_test,]
    negative_examples_test <- test_data_all[negative_examples_indexes_test,]
    
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
    setwd(paste("/synthetic_rbf/sources/data/rep", rep, sep = ""))
    write.table(examples, file = paste(getwd(), "/", class, "_training.csv", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
    write.table(examples_test, file = paste(getwd(), "/", class, "_test.csv", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
    j <- j + 1
  }
}

#obtain a file which is libsvm-readable
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


#training commands
write_train_sources_commands <- function(number_concepts, percent)
{
  training_commands <- data.frame(command = character(), stringsAsFactors = FALSE)
  
  for(rep in 1:10)
  {
    for(i in 1:number_concepts)
    {
      concept <- i
      
      #sources
      if(exists_sample(rep, concept, percent))
      {
        data_dir <- ("/synthetic_rbf/sources/data/")
        sources_dir <- ("/synthetic_rbf/sources/")
        
        training_file <- paste(data_dir, "rep",rep, "/", "rbf", concept, "_training.csv", sep = "")
        model_file <- paste(sources_dir, "model/rep",rep, "/", "rbf", concept, "_training.csv.model", sep = "")
        output_file <- paste(sources_dir, "model/rep",rep, "/", "rbf", concept, "_training.txt", sep = "")
        training_command <- paste("java -jar libsvm_train.jar -s 0 -t 2 -g 0.1 \"", training_file, "\" \"", model_file, "\" \"", output_file, "\" > \"", output_file, "\"", sep = "")
        
        training_commands[nrow(training_commands)+1,] <- training_command
      }
    }
  }
  
  setwd("/synthetic_rbf/sources/")
  write.table(training_commands, paste(getwd(), "/training_commands_all.txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

#helper function
exists_sample <- function(rep, concept, percent)
{
  setwd(paste("/synthetic_rbf/data/rep",rep,sep = ""))
  
  file.exists(paste("rbf", concept,"_training.csv",sep = ""))
}

#write commands for training with forward and then backward transfer (refinement)
#these commands will be helpful for calling the .jar file
train_forward_backward <- function(number_concepts, percent, rep)
{
  setwd("/synthetic_rbf/sources")
  
  n_features <- 100
  features <- ""; for(i in 1:n_features){ if(i != n_features) { features <- paste(features, i, "_", sep = "") } else { features <- paste(features, i, sep = "") } }
  
  forward_commands <- data.frame()
  
  balance_factor_forward <- 1
  balance_factor_backward <- 1
  
  data_dir <- (paste("/synthetic_rbf/sources/data/rep", rep, sep = ""))
  model_dir <- (paste("/synthetic_rbf/sources/model/rep", rep, sep = ""))
  test_dir <- (paste("/synthetic_rbf/test/rep", rep, sep = ""))
  
  times <- floor(number_concepts / 2)
  
  setwd(model_dir)
  hyp <- list.files(pattern = paste("rbf.*\\_training.csv.model$", sep = ""))
  
  setwd(data_dir)
  data <- list.files(pattern = paste("rbf.*\\_training.csv$", sep = ""))
  
  #sources <- grep(paste(par_chld, collapse="|"), sources, value=TRUE)
  
  #next times
  #source_indexes <- sample(1:length(hyp), 1, replace = FALSE)
  source_indexes <- sample(1:length(hyp), length(hyp) * 0.5, replace = FALSE)
  source <- hyp[as.numeric(gsub("_training.*", "", gsub("rbf", "", hyp))) == source_indexes[1]]
  sources <- hyp[as.numeric(gsub("_training.*", "", gsub("rbf", "", hyp))) %in% source_indexes[2:length(source_indexes)]]
  
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
  setwd("/synthetic_rbf/sources")
  
  for(i in 1:number_concepts)
  {
    if(time != 1 & (!paste("rbf", i, "_training_t", time - 1, ".csv.model", sep = "") %in% sources) & paste("rbf", i, sep = "") != strsplit(target, "_")[[1]][1])
    {
      command <- paste("copy ", paste(model_dir, "/rbf", i, "_training_t", time - 1, ".csv.model", sep = "" ), paste(model_dir, "/rbf", i, "_training_t", time, ".csv.model", sep = "" ))
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_rbf_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
    else if(time == 1 & (!paste("rbf", i, "_training.csv.model", sep = "") %in% sources) & paste("rbf", i, sep = "") != strsplit(target, "_")[[1]][1])
    {
      command <- paste("copy ", paste(model_dir, "/rbf", i, "_training.csv.model", sep = "" ), paste(model_dir, "/rbf", i, "_training_t", time, ".csv.model", sep = "" ))
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_rbf_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
  }
}

#write command to delete a file, when no longer necessary, for saving some space
write_delete_command <- function(model_dir, test_dir, target, number_concepts, time, rep)
{
  setwd("/synthetic_rbf/sources")
  
  for(i in 1:number_concepts)
  {
    if(time != 1)
    {
      #delete model
      command <- paste("del ", paste(model_dir, "/rbf", i, "_training_t", time - 1, ".csv.model", sep = "" ))
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_rbf_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
      #delete test file
      command <- paste("del ", paste(test_dir, "/rbf", i, "_test_t", time - 1, ".out", sep = "" ))
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_rbf_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
    else
    {
      #delete model
      command <- paste("del ", paste(model_dir, "/rbf", i, "_training.csv.model", sep = "" ))
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_rbf_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
      #delete test file
      command <- paste("del ", paste(test_dir, "/rbf", i, "_test_t0.out", sep = "" ))
      command <- gsub("/", "\\\\", command)
      write.table(command, paste(getwd(), "/forwardbackward_commands_all_rbf_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    }
  }
}

#command for transferring forward
#this will use the .jar file at https://github.com/nanarosebp/PhDProject/tree/master/AccGenSVM
write_transfer_forward_command <- function(current_dir, model_dir, target, sources, time, n_features, features, balance_factor_forward, rep)
{
  params <- paste("-s 0 -t 2 -K 0.45 ", " -M 1000 -N ", n_features, " -F ", features, " -D ", "\"", current_dir, "\"", " -S ", "\"", model_dir, "\"", " -f ", sep = "")
  #r_path <- "-Djava.library.path=/gpfs1m/apps/easybuild/RHEL6.3/westmere/software/R/3.4.0-gimkl-2017a/lib64/R/library/rJava/jri"
  command <- paste("java -jar accgensvm.jar ", params, "\"", target, "\"", sep = "")

  target_replace <- gsub(".csv", "", target)  
  command <- paste(command, " -y ", "\"", target_replace, "_t", time, ".csv.model", "\"", " -a ", "\"", target_replace, "_t", time, ".txt", "\"", sep = "")
  
  for(i in 1:length(sources))
  {
    command <- paste(command, " -H ", "\"", sources[i], "\"", sep = "")
  }
  
  setwd("/synthetic_rbf/sources")
  write.table(command, paste(getwd(), "/forwardbackward_commands_all_rbf_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
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
    
    params <- paste("-s 1 -t 2 -g 0.1 -G 0.01 -B ", balance_factor_backward, sep = "")
    command <- paste("java -jar hrsvm.jar ", params, " -H ", "\"", model_dir, "/", target, "\"", " -M ", "\"", model_dir, "/", source_replace, "_t", time, ".csv.model", "\"", " > ", "\"", model_dir, "/", source_replace, "_t", time, ".txt", "\"", sep = "")
    
    setwd("/synthetic_rbf/sources")
    write.table(command, paste(getwd(), "/forwardbackward_commands_all_rbf_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
    
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
      test_file <- paste(data_dir, "/rbf", concept, "_test.csv", sep = "")
      if(time == 0)
      {
        model_file <- paste(model_dir, "/rbf", concept, "_training.csv.model", sep = "")
      }
      else
      {
        model_file <- paste(model_dir, "/rbf", concept, "_training_t", time, ".csv.model", sep = "")
      }
      
      output_file <- paste(test_dir, "/rbf", concept, "_test_t", time, ".out", sep = "")
      txt_file <- paste(test_dir, "/rbf", concept, "_test_t", time, ".txt", sep = "")
      command <- paste("java -jar libsvm_predict.jar ", "\"", test_file, "\" \"", model_file, "\" \"", output_file, "\" > \"", txt_file, "\"", sep = "")
      
      predict_commands[nrow(predict_commands)+1,] <- command
    }
  }
  
  setwd("/synthetic_rbf/sources")
  write.table(predict_commands, paste(getwd(), "/forwardbackward_commands_all_rbf_rep", rep, ".cmd", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
}