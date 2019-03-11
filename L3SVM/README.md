L3SVM
================
Diana Benavides-Prado, Yun Sing Koh, Patricia Riddle

Overview
========

L3SVM is a method for hypothesis refinement using knowledge transfer. This software accompanies the paper described on "Selective Hypothesis Transfer for Lifelong Learning", to be published as part of the proceedings of IJCNN 2019.

Our implementation is built on top of LibSVM (Chang and Lin, 2011) and AccGenSVM (Benavides-Prado et al., 2017).

Software usage
==============

In order to use AccGenSVM, consider the parameters: 

| key | name                   | description                                                                          |
|:----|:-----------------------|:-------------------------------------------------------------------------------------|
| -M  | maximum\_sources       | maximum number of source hypotheses that will be transferred from                    |
| -N  | number\_features       | number of features                    												  |
| -F  | number\_features       | features selected by FNN (if not selected, list all features from 1 to d)            |
| -K  | kl\_threshold          | KL-divergence threshold from 0 to 1 (inclusive)                                      |
| -B  | balance\_factor        | contribution of the source hypotheses (set a number between 0 and 1 inclusive)       |
| -f  | training\_examples     | name of the file containing the target training examples                             |
| -H  | source\_hypothesis     | name of the (.model) file containing a source hypothesis                             |
|                              | each source should be listed using "-H source_hypothesis"                            |
| -y  | output\_model          | output (.model) file                                                                 |
| -a  | output\_log            | output log file (optional)                                                           |


Alternatively, see the AccGenSVM software (available at: https://github.com/nanarosebp/PhDProject/tree/master/AccGenSVM). 

To use L3SVM, consider the parameters required for a regular C-SVM (including the name of the file with the training examples and the output model file and output log file), and append:

| key | name                   | description                                                                          |
|:----|:-----------------------|:-------------------------------------------------------------------------------------|
| -H  | target\_model          | name of the (.model) file containing the target SVM learned recently                 |
| -G  | gamma\_value           | gamma in Eq. 6 of the paper                    									  |
| -B  | balance\_factor        | factor stated as omega\_s in Eq. 6 of the paper            						  |

Executable and source code
==========================

Download the .jar executable file for L3SVM (l3svm.jar) and AccGenSVM (accgensvm.jar). 

First, run AccGenSVM with your selected parameters. Two files, names transfer_log.txt and transfer_log_final.txt will be generated as a result of transferring forward selectively, in the same directory where you run AccGenSVM. You will need transfer_log_final.txt for using L3SVM. 

Then run L3SVM.jar with your selected parameters. 

If you want to obtain/use/modify the source code, download the l3svm directory.

The directory data_scripts contains the scripts to extract the samples used in the experiments.

The directory other_methods_scripts contains the scripts to use the counterpart methods ELLA and CL.

References
==========

\[Chang and Lin, 2011\] Chih-Chung Chang and Chih-Jen Lin. LIB-SVM: a library for support vector machines. ACM TIST, 2(3):27, 2011.

\[Benavides-Prado, Koh, Riddle\] AccGenSVM: Selectively Transferring from Source Hypotheses. In IJCAI Proceedings, pp 1440-1446. Available at: https://www.ijcai.org/
proceedings/2017/0199.pdf

