AccGenSVM
================
Diana Benavides-Prado, Yun Sing Koh, Patricia Riddle
1 May 2017

Overview
========

AccGenSVM is a method for selectively transferring information from source hypotheses trained with an SVM. This software accompanies the paper described on "AccGenSVM: Selectively Transferring from Source Hypotheses", to be published as part of the proceedings of IJCAI2017.

Our implementation is build on top of LibSVM (Chang and Lin, 2011), and uses available implementations for KL-divergence (Hausser and Strimmer, 2014) and FNN (Beygelzimer et al., 2013).

Software usage
==============

In order to use AccGenSVM, you should set the following (case-sensitive) parameters:

| key | name                   | description                                                                          |
|:----|:-----------------------|:-------------------------------------------------------------------------------------|
| -s  | svm\_type              | set type of SVM (default 0, see possible values on LibSVM documentation)             |
| -t  | kernel\_type           | set type of kernel function (default 2, see possible values on LibSVM documentation) |
| -d  | degree                 | set degree in kernel function (default 3)                                            |
| -g  | gamma                  | set gamma in kernel function (default 1/num\_features)                               |
| -r  | coef0                  | set coef0 in kernel function (default 0)                                             |
| -c  | cost                   | set the parameter C of C-SVC, epsilon-SVR, and nu-SVR (default 1)                    |
| -n  | nu                     | set the parameter nu of nu-SVC, one-class SVM, and nu-SVR (default 0.5)              |
| -p  | epsilon                | set the epsilon in loss function of epsilon-SVR (default 0.1)                        |
| -m  | cachesize              | set cache memory size in MB (default 100)                                            |
| -e  | epsilon                | set tolerance of termination criterion (default 0.001)                               |
| -h  | shrinking              | whether to use the shrinking heuristics, 0 or 1 (default 1)                          |
| -b  | probability\_estimates | whether to train a SVC or SVR model for probability estimates, 0 or 1 (default 0)    |
| -wi | weight                 | set the parameter C of class i to weight\*C, for C-SVC (default 1)                   |
| -v  | n                      | n-fold cross validation mode                                                         |
| -q  | quiet mode             | (no outputs)                                                                         |

which are already used in LibSVM, plus the following ones for selective transfer:

| key | description                                                            |
|:----|:-----------------------------------------------------------------------|
| -M  | maximum number of source hypotheses                                    |
| -N  | number of features for sources and target                              |
| -F  | features selected for FNN (separated by underscore)                    |
| -K  | KL-divergence threshold                                                |
| -f  | training data file                                                     |
| -H  | source hypothesis file name (for more than one, use -H multiple times) |
| -y  | output (model) file name                                               |
| -a  | output (log) file name                                                 |

Software usage
==============

Download the .jar executable file and run AccGenSVM with the appropriated parameters. If you want to obtain/use/modify the source code, download the AccGenSVM directory. If you want to obtain/use/modify the source code for data extraction, download the ExtractionScripts directory.

References
==========

\[Chang and Lin, 2011\] Chih-Chung Chang and Chih-Jen Lin. LIB-SVM: a library for support vector machines. ACM TIST, 2(3):27, 2011.

\[Hausser and Strimmer, 2014\] Jean Hausser and Korbinian Strimmer. entropy: Estimation of Entropy, Mutual Information and Related Quantities, 2014. R package version 1.2.1.

\[Beygelzimer et al., 2013\] A Beygelzimer, S Kakadet, J Langford, S Arya, D Mount, and S Li. FNN: fast nearest neighbor search algorithms and applications. R package version 1.1, 2013.
