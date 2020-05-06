# -*- coding: utf-8 -*-
"""
Created on Tue Sep 12 20:52:25 2017

@author: Administrator
"""

from sklearn import svm
from sklearn import preprocessing
import pandas
import os
from sklearn.externals.joblib import Memory
from sklearn.datasets import load_svmlight_file
from random import *
import numpy as np
import random


def check_similarity(predictions, y_train):    
    number_positives = 0
    correct_positives = 0
    
    for i in range(0,len(predictions)):
        #only positive instances from training data
        if(y_train[i]):
            number_positives += 1
            
            if(predictions[i] == y_train[i]):
                correct_positives += 1
    
    similarity = correct_positives/ number_positives
    
    return similarity

def retrain(dataset, rep, training_files, source_id, target_id, n_features, models, t):
    
    os.chdir('C:/Users/Administrator/Desktop/' + dataset + '/data/rep' + str(rep))
    
    file_source = load_svmlight_file(training_files[source_id])
    X_train_source = pandas.DataFrame(file_source[0].todense())
    
    file_target = load_svmlight_file(training_files[target_id])
    X_train_target = pandas.DataFrame(file_target[0].todense())
    
    #positives and negatives of source
    positives_source = list()
    negatives_source = list()
    
    for i in range(0, len(file_source[1])):
        if file_source[1][i] == 1:
            positives_source.append(i)
        else:
            negatives_source.append(i)
            
    #only positives of target
    positives_target = list()

    for i in range(0, len(file_target[1])):
        if file_target[1][i] == 1:
            positives_target.append(i)
            
    #extract positives and convert to negatives
    X_train_target = X_train_target.iloc[positives_target]
                     
    #append to negatives on X_train
    file_training_x = X_train_source.append(X_train_target, ignore_index = True)
        
    file_training_y = file_source[1]
    new_y = np.zeros(X_train_target.shape[0])
    new_y[new_y == 0] = -1
    file_training_y = np.append(file_training_y, new_y)
        
    #separate training and class
    X_train = file_training_x
    y_train = file_training_y

    os.chdir('C:/Users/Administrator/Desktop/'+dataset+'/cl')
    with open('cl_log_convergence.txt', 'a') as file_convergence:
        file_convergence.write('rep = ' + str(rep) + ', t = ' + str(t) + ', hyp = ' + str(source_id) + '\n')
        
    fit_train = svm.SVC(verbose = 2, kernel = 'linear', class_weight = 'balanced')
    fit_train.fit(X_train, y_train)
        
    models.loc[(models['id'] == source_id), 'fit'] = fit_train
                   
    return models

def predict_task_source(dataset, rep, fit, test_files, n_features, k):
    os.chdir('C:/Users/Administrator/Desktop/' + dataset + '/data/rep' + str(rep))
    
    file_name_test = test_files[k]
    file_test = load_svmlight_file(file_name_test)
    
    X_test = file_test[0]
    X_test = (pandas.DataFrame(X_test.todense()))
    
    predictions = fit.predict(X_test)
    
    return pandas.DataFrame(predictions)[0]

def calculate_accuracy_source(test_files, k, predictions):
    file_name_test = test_files[k]
    file_test = load_svmlight_file(file_name_test)
    y_test = file_test[1]
    
    correct = 0
    total = len(predictions)
    for i in range(0,len(predictions)):
        if(predictions[i] == y_test[i]):
            correct+=1
        #for unbalanced problems
        #if(predictions[i] == y_test[i] and y_test[i] > 0):
            #correct+=1
    
    accuracy = correct / total
    
    return accuracy

def write_accuracy(dataset, rep, t, task_id, accuracy):
    os.chdir('C:/Users/Administrator/Desktop/' + dataset + '/cl')
    results = pandas.DataFrame(data = {'rep': rep, 't': [t], 'task_id': [task_id], 'accuracy': [accuracy]})
    results.to_csv('cl_results.csv', index = False, header = False, mode = 'a', columns = ['rep', 't', 'task_id', 'accuracy'])

def predict_all(dataset, rep, t, test_files, models, n_features):  
    for i in range(0, len(models)):
        accuracy = calculate_accuracy_source(test_files, models['id'][i], predict_task_source(dataset, rep, models['fit'][i], test_files, n_features, models['id'][i]))
        write_accuracy(dataset, rep, t, i, accuracy)

#sources first
def train_sources(dataset, rep, n_features, similarity_threshold):
    
    #list of models
    models = pandas.DataFrame(columns=('id', 'fit'))
    convergence_results = pandas.DataFrame(columns = ('id', 'number_iterations'))
    
    #read data
    os.chdir('C:/Users/Administrator/Desktop/'+dataset + '/data/rep' + str(rep))
    
    #list of files
    files = os.listdir()
    training_files = [x for x in files if 'training.csv' in x]
    test_files = [x for x in files if 'test.csv' in x]
    
    order = random.sample(list(range(len(training_files))), len(training_files))
    
    for j in range(0,len(order)):
        
        os.chdir('C:/Users/Administrator/Desktop/'+dataset + '/data/rep' + str(rep))
        file_training = load_svmlight_file(training_files[order[j]])
            
        X_train = file_training[0].toarray()
        y_train = file_training[1]
        
        os.chdir('C:/Users/Administrator/Desktop/'+dataset+'/cl')
        with open('cl_log_convergence.txt', 'a') as file_convergence:
            file_convergence.write('rep = ' + str(rep) + ', t = ' + str(j) + ', hyp = ' + str(order[j]) + '\n')
            
        fit_train = svm.SVC(verbose=2, kernel = 'linear', class_weight = 'balanced')
        fit_train.fit(X_train, y_train)
            
        if len(models) == 0:
            models = pandas.DataFrame(data = {'id': [order[j]], 'fit': [fit_train]})
        else:
            models = models.append(pandas.DataFrame(data = {'id': [order[j]], 'fit': [fit_train]}), ignore_index = True)
        
            #update existing models
            for f in range(0,len(models) - 1):
                predictions = models['fit'][f].predict(X_train)
                similarity = check_similarity(predictions, y_train)
            
                if similarity >= similarity_threshold:
                    models = retrain(dataset, rep, training_files, models['id'][f], order[j], n_features, models, j)
                
        predict_all(dataset, rep, j, test_files, models, n_features)
        
        
#read data
#dataset = '20newsgroups'
#n_features = 1000
#similarity_threshold = 0.2
#train
#for rep in range(1, 31):
    #train_sources(dataset = dataset, rep = rep, n_features = n_features, similarity_threshold = similarity_threshold)

dataset = 'synthetic_hyperplane'
n_features = 100
similarity_threshold = 0.2
#train
for rep in range(3, 6):
    train_sources(dataset = dataset, rep = rep, n_features = n_features, similarity_threshold = similarity_threshold)
