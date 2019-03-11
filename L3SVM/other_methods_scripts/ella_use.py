# -*- coding: utf-8 -*-
"""
Created on Sat Sep  9 19:37:18 2017

@author: nanar
"""

import os
import pandas
from sklearn.decomposition import PCA
import numpy

def predict_task_source(dataset, rep, ella, run1_files_sources_test, n_features, n_components_pca, k):
    os.chdir('temp_data/'+ dataset + '/rep' + str(rep))
    
    file_name_test = run1_files_sources_test[k]
    file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
    
    X_test = file_test.iloc[:,1:n_features+1]   

    if n_features > 100:
        pca = PCA(n_components=n_components_pca)
        pca.fit(X_test)
        X_test = pandas.DataFrame(pca.transform(X_test))
    
    #add bias term
    X_test = pandas.concat([pandas.DataFrame(numpy.ones(X_test.shape[0])), X_test], axis = 1)
    
    predictions = ella.predict(X_test, k)
    predictions = [1 if x == True else 0 for x in predictions[0]]
    
    return predictions

def calculate_accuracy_source(ella, run1_files_sources_test, k, predictions):
    file_name_test = run1_files_sources_test[k]
    file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
    y_test = file_test.iloc[:,0]
    
    y_test = y_test.replace({-1:0})
    
    correct = 0
    total = len(predictions)
    for i in range(0,len(predictions)):
        if(predictions[i] == y_test[i]):
            correct+=1  
    accuracy = correct / total
    
    return accuracy

def predict_task_target(dataset, rep, ella, run1_files_targets_test, n_features, n_components_pca, k, target_task_id):
    os.chdir('temp_data/'+ dataset + '/rep' + str(rep))
    
    file_name_test = run1_files_targets_test[k]
    file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
    
    X_test = file_test.iloc[:,1:n_features + 1]
    
    if n_features > 100:
        pca = PCA(n_components=n_components_pca)
        pca.fit(X_test)
        X_test = pandas.DataFrame(pca.transform(X_test))
        
    #add bias term
    X_test = pandas.concat([pandas.DataFrame(numpy.ones(X_test.shape[0])), X_test], axis = 1)
    
    predictions = ella.predict(X_test, target_task_id)
    predictions = [1 if x == True else 0 for x in predictions[0]]
    
    return predictions

def calculate_accuracy_target(ella, run1_files_targets_test, k, predictions):
    file_name_test = run1_files_targets_test[k]
    file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
    y_test = file_test.iloc[:,0]
    
    y_test = y_test.replace({-1:0})
    
    correct = 0
    total = len(predictions)
    for i in range(0,len(predictions)):
        if(predictions[i] == y_test[i]):
            correct+=1
            
    accuracy = correct / total
    
    return accuracy
    
def write_accuracy(dataset, rep, run, t, role, task_id, accuracy):
    os.chdir('temp_data/' + dataset)
    results = pandas.DataFrame(data = {'rep': [rep], 'run': [run], 't': [t], 'role': [role], 'task_id': [task_id], 'accuracy': [accuracy]})
    results.to_csv('ella_results.csv', index = False, header = None, mode = 'a', columns = ['rep', 'run', 'role', 'task_id', 't', 'accuracy'])
    

def process_run(dataset, n_features, n_components_pca, rep, run, percentage_latent_components, sparsity_level):
    #read data
    os.chdir('temp_data/'+dataset + '/rep' + str(rep))
    
    run1_files_sources_training = [filename for filename in os.listdir('.') if filename.startswith("run_" + str(run) + "_source_") and filename.endswith('training.csv')]
    run1_files_sources_test = [filename for filename in os.listdir('.') if filename.startswith("run_" + str(run) + "_source_") and filename.endswith('test.csv')]
    run1_files_targets_training = [filename for filename in os.listdir('.') if filename.startswith("run_" + str(run) + "_target_") and filename.endswith('training.csv')]
    run1_files_targets_test = [filename for filename in os.listdir('.') if filename.startswith("run_" + str(run) + "_target_") and filename.endswith('test.csv')]
    
    #create the ELLA object
    if n_features > 100:
        ella = ELLA(n_components_pca+1, int(round(n_components_pca * percentage_latent_components, 0)), LogisticRegression, mu = sparsity_level)
    else:
        ella = ELLA(n_features+1, int(round(n_features * percentage_latent_components, 0)), LogisticRegression, mu = sparsity_level)
    
    #sources first
    for j in range(0,len(run1_files_sources_training)):
        
        os.chdir('temp_data/'+ dataset + '/rep' + str(rep))
        
        file_name_training = run1_files_sources_training[j]
        file_name_test = run1_files_sources_test[j]
        
        if(os.path.isfile(file_name_training)):
            file_training = pandas.read_csv(file_name_training, delimiter= ' ', header = None)
            file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
            
            X_train = file_training.iloc[:,1:n_features+1]
            y_train = file_training.iloc[:,0]
                      
            y_train = y_train.replace({-1:0})
            
            X_test = file_test.iloc[:,1:n_features+1]
            y_test = file_test.iloc[:,0]
            
            y_test = y_test.replace({-1:0})
            
            if n_features > 100:
                pca = PCA(n_components=n_components_pca)
                pca.fit(X_train)
                X_train = pandas.DataFrame(pca.transform(X_train))
                pca = PCA(n_components=n_components_pca)
                pca.fit(X_test)
                X_test = pandas.DataFrame(pca.transform(X_test))

            #add bias term as in example https://github.com/paulruvolo/ELLA/blob/master/ELLA.ipynb
            X_train = pandas.concat([pandas.DataFrame(numpy.ones(X_train.shape[0])), X_train], axis = 1)
            X_test = pandas.concat([pandas.DataFrame(numpy.ones(X_test.shape[0])), X_test], axis = 1)
            
            ella.fit(X_train, y_train, j)
            predictions = ella.predict(X_test, j)
            predictions = [1 if x == True else 0 for x in predictions[0]]
            
            #accuracy
            accuracy = calculate_accuracy_source(ella, run1_files_sources_test, j, predictions)
            
            write_accuracy(dataset, rep, run, 0, 'source', j, accuracy)
            
            for k in range(0, j):
                accuracy_previous = calculate_accuracy_source(ella, run1_files_sources_test, k, predict_task_source(dataset, rep, ella, run1_files_sources_test, n_features, n_components_pca, k))
                write_accuracy(dataset, rep, run, 0, 'source', k, accuracy_previous)
                
    #then targets
    for j in range(0,len(run1_files_targets_training)):
        
        os.chdir('temp_data/'+ dataset + '/rep' + str(rep))
        
        task_id = j + len(run1_files_sources_training)
        
        file_name_training = run1_files_targets_training[j]
        file_name_test = run1_files_targets_test[j]
        
        if(os.path.isfile(file_name_training)):
            file_training = pandas.read_csv(file_name_training, delimiter= ' ', header = None)
            file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
            
            X_train = file_training.iloc[:,1:n_features+1]
            y_train = file_training.iloc[:,0]

            X_test = file_test.iloc[:,1:n_features+1]
            y_test = file_test.iloc[:,0]
            
            y_test = y_test.replace({-1:0})

            y_train = y_train.replace({-1:0})
            
            if n_features > 100:
                pca = PCA(n_components=n_components_pca)
                pca.fit(X_train)
                X_train = pandas.DataFrame(pca.transform(X_train))
                pca = PCA(n_components=n_components_pca)
                pca.fit(X_test)
                X_test = pandas.DataFrame(pca.transform(X_test))
                
            #add bias term as in example https://github.com/paulruvolo/ELLA/blob/master/ELLA.ipynb
            X_train = pandas.concat([pandas.DataFrame(numpy.ones(X_train.shape[0])), X_train], axis = 1)
            X_test = pandas.concat([pandas.DataFrame(numpy.ones(X_test.shape[0])), X_test], axis = 1) 
            
            ella.fit(X_train, y_train, task_id)
            predictions = ella.predict(X_test, task_id)
            predictions = [1 if x == True else 0 for x in predictions]
            
            #accuracy
            accuracy = calculate_accuracy_target(ella, run1_files_targets_test, j, predictions)
            
            write_accuracy(dataset, rep, run, j, 'target', j, accuracy)
    
            #predict sources
            for k in range(0, len(run1_files_sources_training)):
                accuracy_previous = calculate_accuracy_source(ella, run1_files_sources_test, k, predict_task_source(dataset, rep, ella, run1_files_sources_test, n_features, n_components_pca, k))
                write_accuracy(dataset, rep, run, j, 'source', k, accuracy_previous)  
            
            #predict targets
            if j > 0:
                for k in range(0, j):
                    accuracy_previous = calculate_accuracy_target(ella, run1_files_targets_test, k, predict_task_target(dataset, rep, ella, run1_files_targets_test, n_features, n_components_pca, k, k + len(run1_files_sources_training)))
                    write_accuracy(dataset, rep, run, j, 'target', k + + len(run1_files_sources_training), accuracy_previous)            
