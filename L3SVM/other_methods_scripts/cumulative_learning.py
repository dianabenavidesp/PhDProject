# -*- coding: utf-8 -*-
"""
Created on Tue Sep 12 20:52:25 2017

@author: nanar
"""

from sklearn import svm
from sklearn import preprocessing
import pandas
import os

def check_similarity(predictions, y_train):    
    number_positives = 0
    correct_positives = 0
    
    for i in range(0,len(predictions)):
        if(y_train[i]):
            number_positives += 1
            
            if(predictions[i] == y_train[i]):
                correct_positives += 1
    
    similarity = correct_positives/ number_positives
    
    return similarity

def retrain(dataset, rep, role, source_id, target_id, n_features, models):
    
    os.chdir('temp_data/'+ dataset + '/rep' + str(rep))
    
    file_name_training_target = run1_files_targets_training[target_id]
    file_target = pandas.read_csv(file_name_training_target, delimiter= ' ', header = None)
    
    file_target.columns = file_target.columns.astype(str)
    #extract positives and convert to negatives
    X_train_target = file_target[file_target['0'] == 1]
    X_train_target['0'] = -1
    
    if role == 'source':
        file_name_training = run1_files_sources_training[source_id]
        file_training = pandas.read_csv(file_name_training, delimiter= ' ', header = None)
                     
        #append to negatives on X_train
        file_training.columns = file_training.columns.astype(str)
        file_training = file_training.append(X_train_target, ignore_index = True)
        
        #separate training and class
        X_train = file_training.iloc[:,1:n_features+1]
        y_train = file_training.iloc[:,0]
        
        print('source' + ',' + file_name_training + '_t' + str(target_id))
        fit_train = svm.SVC(verbose = 2, kernel = 'linear', class_weight = 'balanced')
        fit_train.fit(X_train, y_train)
        
        models.loc[(models['role'] == 'source') & (models['id'] == source_id), 'fit'] = fit_train
                   
    return models

def predict_task_source(dataset, rep, fit, run1_files_sources_test, n_features, k):
    os.chdir('temp_data/'+ dataset + '/rep' + str(rep))
    
    file_name_test = run1_files_sources_test[k]
    file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
    
    X_test = file_test.iloc[:,1:n_features + 1]
    X_test = preprocessing.scale(X_test)
    
    predictions = fit.predict(X_test)
    
    return pandas.DataFrame(predictions)[0]

def predict_task_target(dataset, rep, fit, run1_files_targets_test, n_features, k):
    os.chdir('temp_data/'+ dataset + '/rep' + str(rep))
    
    file_name_test = run1_files_targets_test[k]
    file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
    
    X_test = file_test.iloc[:,1:n_features + 1]
    X_test = preprocessing.scale(X_test)
    
    predictions = fit.predict(X_test)
    
    return pandas.DataFrame(predictions)[0]

def calculate_accuracy_source(run1_files_sources_test, k, predictions):
    file_name_test = run1_files_sources_test[k]
    file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
    y_test = file_test.iloc[:,0]
    
    correct = 0
    total = len(predictions)
    for i in range(0,len(predictions)):
        if(predictions[i] == y_test[i]):
            correct+=1
    
    accuracy = correct / total
    
    return accuracy

def calculate_accuracy_target(run1_files_targets_test, k, predictions):
    file_name_test = run1_files_targets_test[k]
    file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
    y_test = file_test.iloc[:,0]
    
    correct = 0
    total = len(predictions)
    for i in range(0,len(predictions)):
        if(predictions[i] == y_test[i]):
            correct+=1
    
    accuracy = correct / total
    
    return accuracy
    
def write_accuracy(dataset, rep, t, role, task_id, accuracy):
    os.chdir('temp_data/' + dataset)
    results = pandas.DataFrame(data = {'rep': rep, 'run': 1, 't': [t], 'role': [role], 'task_id': [task_id], 'accuracy': [accuracy]})
    results.to_csv('cl_results.csv', index = False, header = False, mode = 'a', columns = ['rep', 'run', 'role', 'task_id', 't', 'accuracy'])

def predict_all(dataset, rep, t, run1_files_sources_test, run1_files_targets_test, models, n_features):
    
    for i in range(0, len(models)):
        if models['role'][i] == 'source':
            accuracy = calculate_accuracy_source(run1_files_sources_test, models['id'][i], predict_task_source(dataset, rep, models['fit'][i], run1_files_sources_test, n_features, models['id'][i]))
            write_accuracy(dataset, rep, t, 'source', i, accuracy)
        elif models['role'][i] == 'target':
            accuracy = calculate_accuracy_target(run1_files_targets_test, models['id'][i], predict_task_target(dataset, rep, models['fit'][i], run1_files_targets_test, n_features, models['id'][i]))
            write_accuracy(dataset, rep, t, 'target', i, accuracy)

#sources first
def train_sources(dataset, models, n_features, similarity_threshold, rep, run):
    for j in range(0,len(run1_files_sources_training)):
        
        os.chdir('temp_data/'+ dataset + '/rep' + str(rep))
        
        file_name_training = run1_files_sources_training[j]
        file_name_test = run1_files_sources_test[j]
        
        if(os.path.isfile(file_name_training)):
            file_training = pandas.read_csv(file_name_training, delimiter= ' ', header = None)
            file_test = pandas.read_csv(file_name_test, delimiter= ' ', header = None)
            
            X_train = file_training.iloc[:,1:n_features+1]
            X_train = preprocessing.scale(X_train)
            y_train = file_training.iloc[:,0]
            
            X_test = file_test.iloc[:,1:n_features+1]
            X_test = preprocessing.scale(X_test)
            
            print('source' + ',' + file_name_training + '_t' + str(j))
            fit_train = svm.SVC(verbose=2, kernel = 'linear', class_weight = 'balanced')
            fit_train.fit(X_train, y_train)
            
            if len(models) > 0:
                models = models.append(pandas.DataFrame(data = {'role': ['source'], 'id': [j], 'fit': [fit_train]}), ignore_index = True)
            else:
                models = pandas.DataFrame(data = {'role': ['source'], 'id': [j], 'fit': [fit_train]})

    for j in range(0,len(run1_files_targets_training)):
        
        os.chdir('temp_data/'+ dataset + '/rep' + str(rep))
        
        file_name_training = run1_files_targets_training[j]
        
        file_training = pandas.read_csv(file_name_training, delimiter= ' ', header = None)
            
        X_train = file_training.iloc[:,1:n_features+1]
        X_train = preprocessing.scale(X_train)
        y_train = file_training.iloc[:,0]
        
        #update sources
        for f in range(0,len(models)):
            predictions = models['fit'][f].predict(X_train)
            similarity = check_similarity(predictions, y_train)
            
            if similarity >= similarity_threshold and models['role'][f] == 'source':
                models = retrain(dataset, rep, 'source', f, j, n_features, models)
            elif similarity >= similarity_threshold and models['role'][f] == 'target':
                models = retrain(dataset, rep, 'target', f, j, n_features, models)
        
        #train current model and add to list of models
        print('source' + ',' + file_name_training + '_t' + str(j))
        fit_train = svm.SVC(verbose = 2, kernel = 'linear', class_weight = 'balanced')
        fit_train.fit(X_train, y_train)
    
        models = models.append(pandas.DataFrame(data = {'role': ['target'], 'id': [j], 'fit': [fit_train]}), ignore_index = True)
        
        predict_all(dataset, rep, j+1, run1_files_sources_test, run1_files_targets_test, models, n_features)
