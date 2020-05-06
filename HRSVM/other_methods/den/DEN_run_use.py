# -*- coding: utf-8 -*-
"""
Created on Fri Jun  1 13:52:52 2018

@author: nanar
"""

import os
import tensorflow as tf
import numpy as np
#import DEN
from tensorflow.examples.tutorials.mnist import input_data
#import other
import pandas
from sklearn.preprocessing import OneHotEncoder
from sklearn.datasets import load_svmlight_file
import sys

#read data
number_tasks = 100
dims = [100, 250, 200, number_tasks]

for rep in range(1, 2):
    os.chdir("C:/Users/Administrator/Desktop/other_methods/den")
    import DEN
    
    os.chdir('C:/Users/Administrator/Desktop/synthetic_rbf/sources/data/rep'+str(rep))
    
    np.random.seed(1004)
    flags = tf.app.flags
    #DBP changed max_iter to a smaller value
    flags.DEFINE_integer("max_iter", number_tasks * 2, "Epoch to train")
    flags.DEFINE_float("lr", 0.001, "Learing rate(init) for train")
    flags.DEFINE_integer("batch_size", number_tasks * 2, "The size of batch for 1 iteration")
    flags.DEFINE_string("checkpoint_dir", "checkpoints", "Directory path to save the checkpoints")
    #flags.DEFINE_integer("dims", [784, 312, 128, 10], "Dimensions about layers including output")
    flags.DEFINE_integer("n_classes", number_tasks, 'The number of classes at each task')
    flags.DEFINE_float("l1_lambda", 0.00001, "Sparsity for L1")
    flags.DEFINE_float("l2_lambda", 0.0001, "L2 lambda")
    flags.DEFINE_float("gl_lambda", 0.001, "Group Lasso lambda")
    #DBP large regularization lambda so that the new network looks very similar to the old one (otherwise accuracies decrease a lot!)
    flags.DEFINE_float("regular_lambda", 0.5, "regularization lambda")
    #DBP changed to a larger value (increase 50 units)
    flags.DEFINE_integer("ex_k", 10, "The number of units increased in the expansion processing")
    flags.DEFINE_float('loss_thr', 0.1, "Threshold of dynamic expansion")
    #DBP Depending on this threshold, existing neurons will be split and duplicated, so as to prevent semantic drift
    #DBP changed to a smaller value so that semantic drift is dis-incouraged
    flags.DEFINE_float('spl_thr', 0.5, "Threshold of split and duplication")
    FLAGS = flags.FLAGS
    
    files = os.listdir()
    training_files = [x for x in files if 'training.csv' in x]
    test_files = [x for x in files if 'test.csv' in x]
    
    number_features = 100
    
    trainX = pandas.DataFrame()
    trainY = pandas.DataFrame()
    
    testX = pandas.DataFrame()
    testY = pandas.DataFrame()
    
    for i in range(0, number_tasks):  
        training = load_svmlight_file(training_files[i])
        train_examples = training[0].toarray()
        train_labels = training[1]
        
        train_labels = train_labels[train_labels == 1]
        train_examples = train_examples[0:len(train_labels),:]
        train_labels = pandas.DataFrame([i] * train_examples.shape[0])
    
        test = load_svmlight_file(test_files[i])
        test_examples = test[0].toarray()
        test_labels = test[1]
        
        test_labels = test_labels[test_labels == 1]
        test_examples = test_examples[0:len(test_labels),:]
        test_labels = pandas.DataFrame([i] * test_examples.shape[0])
        
        if trainX.shape[0] > 0:
            trainX = pandas.concat([trainX, pandas.DataFrame(train_examples)])
            trainY = pandas.concat([trainY, pandas.DataFrame(train_labels)])
            
            testX = pandas.concat([testX, pandas.DataFrame(test_examples)])
            testY = pandas.concat([testY, pandas.DataFrame(test_labels)])
        else:
            trainX = pandas.DataFrame(train_examples)
            trainY = pandas.DataFrame(train_labels)
            
            testX = pandas.DataFrame(test_examples)
            testY = pandas.DataFrame(test_labels)
    
    #onehot encode labels
    enc = OneHotEncoder()
    trainY = (enc.fit_transform(trainY)).todense()
    testY = (enc.fit_transform(testY)).todense()
    
    task_permutation = []
    for task in range(number_tasks):
        task_permutation.append(np.random.permutation(number_features))
    
    trainXs, valXs, testXs = [], [], []
    for task in range(number_tasks):
        trainXs.append(np.array(trainX)[:, task_permutation[task]])
        #valXs.append(valX[:, task_permutation[task]])
        testXs.append(np.array(testX)[:, task_permutation[task]])
    
    #DBP pass dims since it is not accepted as part of config as a list
    model = DEN.DEN(FLAGS, dims)
    params = dict()
    avg_perf = []
    
    #os.chdir('C:/Users/nanar/Desktop/temp_data/imagenet/'+family+'/den')
    #with open('den_log.txt', 'a') as file_convergence:
        #file_convergence.write('------------REPETITION------------\n')
        #file_convergence.write('rep = ' + rep + '\n')
    
    for t in range(FLAGS.n_classes):
        f = open('C:/Users/Administrator/Desktop/synthetic_rbf/DEN/den_'+str(number_tasks)+'_tasks_defaultparams.txt', 'a')
        sys.stdout = f

        data = (trainXs[t], trainY, trainXs[t], trainY, testXs[t], testY)
        model.sess = tf.Session()
        print("\n\n\tTASK %d TRAINING\n"%(t+1))
    
        model.T = model.T+1
        model.task_indices.append(t+1)
        model.load_params(params, time = 1)
        perf, sparsity, expansion = model.add_task(t+1, data)
    
        params = model.get_params()
        model.destroy_graph()
        model.sess.close()
    
        model.sess= tf.Session()
        print('\n OVERALL EVALUATION')
        model.load_params(params)
        temp_perfs = []
        for j in range(t+1):
            temp_perf = model.predict_perform(j+1, testXs[j], testY)
            temp_perfs.append(temp_perf)
        avg_perf.append( sum(temp_perfs) / float(t+1) )
        print("   [*] avg_perf: %.4f"%avg_perf[t])
        model.destroy_graph()
        model.sess.close()
        
        f.close()