# -*- coding: utf-8 -*-
"""
Created on Sat Sep  9 19:37:18 2017

@author: Administrator
"""

import os
import pandas
from sklearn.decomposition import PCA
import numpy
import sys  
sys.path.append("C:/Users/Administrator/Desktop/other_methods/ella") 
from ella import ELLA
from sklearn.datasets import load_svmlight_file
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import Ridge
import random

def predict_task_target(dataset, rep, ella, test_files, n_features, n_components_pca, pca, k):
    os.chdir('C:/Users/Administrator/Desktop/' + dataset + '/sources/data/rep' + str(rep))
    
    file_test = load_svmlight_file(test_files[k], n_features = n_features)
    
    X_test = file_test[0].toarray()
    
    if n_features > 200:
        #pca = PCA(n_components=n_components_pca)
        #pca.fit(X_test)
        X_test = pandas.DataFrame(pca.transform(X_test))
        
    #add bias term
    X_test = pandas.concat([pandas.DataFrame(numpy.ones(X_test.shape[0])), pandas.DataFrame(X_test)], axis = 1)
    
    predictions = ella.predict(X_test, k)
    predictions = [1 if x == True else 0 for x in predictions[0]]
    
    return predictions

def calculate_accuracy_target(test_files, k, predictions, n_features):
    file_test = load_svmlight_file(test_files[k], n_features = n_features)
    
    y_test = list(file_test[1])
    y_test = [0 if x == -1 else 1 for x in y_test]
    
    correct = 0
    total = len(predictions)
    for i in range(0,len(predictions)):
        if(predictions[i] == y_test[i]):
            correct+=1
            
    accuracy = correct / total
    
    return accuracy
    
def write_accuracy(dataset, rep, t, task_id, accuracy):
    os.chdir('C:/Users/Administrator/Desktop/' + dataset + '/ella')
    results = pandas.DataFrame(data = {'rep': [rep], 't': [t], 'task_id': [task_id], 'accuracy': [accuracy]})
    results.to_csv('ella_results.csv', index = False, header = None, mode = 'a', columns = ['rep', 't', 'task_id', 'accuracy'])
    

def process_run(dataset, rep, n_features, n_components_pca, percentage_latent_components, sparsity_level):
    #read data
    os.chdir('C:/Users/Administrator/Desktop/'+dataset + '/sources/data/rep' + str(rep))
    
    #list of files
    files = os.listdir()
    training_files = [x for x in files if 'training.csv' in x]
    test_files = [x for x in files if 'test.csv' in x]
    
    #random order of tasks
    order = random.sample(list(range(len(training_files))), len(training_files))
    
    #create the ELLA object
    if n_features > 200:
        ella = ELLA(n_components_pca+1, int(round(n_components_pca * percentage_latent_components, 0)), LogisticRegression, mu = sparsity_level)
    else:
        ella = ELLA(n_features+1, int(round(n_features * percentage_latent_components, 0)), LogisticRegression, mu = sparsity_level)

    for j in range(0, len(order)):
        task_id = j
                
        #set dir again
        os.chdir('C:/Users/Administrator/Desktop/'+dataset + '/sources/data/rep' + str(rep))
        file_training = load_svmlight_file(training_files[order[j]], n_features = n_features)
        file_test = load_svmlight_file(test_files[order[j]], n_features = n_features)
            
        X_train = file_training[0].toarray()
        y_train = file_training[1]

        X_test = file_test[0].toarray()
        y_test = list(file_test[1])
            
        y_test = [0 if x == -1 else 1 for x in y_test]
        y_train = [0 if x == -1 else 1 for x in y_train]
        
        pca = 0
        if n_features > 200:
            pca = PCA(n_components=n_components_pca)
            pca.fit(X_train)
            
            X_train = pandas.DataFrame(pca.transform(X_train))
            
            #pca = PCA(n_components=n_components_pca)
            #pca.fit(X_test)
            X_test = pandas.DataFrame(pca.transform(X_test))
                
        #add bias term as in example https://github.com/paulruvolo/ELLA/blob/master/ELLA.ipynb
        X_train = pandas.concat([pandas.DataFrame(numpy.ones(X_train.shape[0])), pandas.DataFrame(X_train)], axis = 1)
        X_test = pandas.concat([pandas.DataFrame(numpy.ones(X_test.shape[0])), pandas.DataFrame(X_test)], axis = 1) 
            
        ella.fit(X_train, y_train, task_id)
        predictions = ella.predict(X_test, task_id)
        predictions = [1 if x == True else 0 for x in predictions[0]]
            
        #accuracy
        accuracy = calculate_accuracy_target(test_files, order[j], predictions, n_features)
            
        write_accuracy(dataset, rep, j, order[j], accuracy)
                
        #predict tasks learned so far
        if j > 0:
            for k in range(j-1, -1, -1):
                accuracy_previous = calculate_accuracy_target(test_files, order[k], predict_task_target(dataset, rep, ella, test_files, n_features, n_components_pca, pca, k), n_features)
                write_accuracy(dataset, rep, j, order[k], accuracy_previous)            
                

#max number of components PCA accepted by ELLA: 200
#percentage latent components should be small, according to paper (they try max. 0.25 of the number of features)
#sparsity does not have an impact, seemlengly 
#for rep in range(1, 31):                
    #process_run(dataset = '20newsgroups', rep = rep, n_features = 1000, n_components_pca = 200, percentage_latent_components = 0.10, sparsity_level = 1)

#for rep in range(2, 3):                
    #process_run(dataset = 'synthetic_hyperplane', rep = rep, n_features = 100, n_components_pca = 200, percentage_latent_components = 0.10, sparsity_level = 1)

#for rep in range(1, 6):                
    #process_run(dataset = 'synthetic_rbf', rep = rep, n_features = 100, n_components_pca = 200, percentage_latent_components = 0.25, sparsity_level = 1)

for rep in range(1, 2):                
    process_run(dataset = 'imagenet', rep = rep, n_features = 1000, n_components_pca = 200, percentage_latent_components = 0.25, sparsity_level = 1)
