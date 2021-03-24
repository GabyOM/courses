
#**Peer-graded Assignment Prediction Assignment Writeup**
#**Background**

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: [link](http://groupware.les.inf.puc-rio.br/har)

(see the section on the Weight Lifting Exercise Dataset). 

#**Data**

The **training** data for this project are available here: 
    
    [training data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)

The **test** data are available here:
    
    [testing data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)

The data for this project come from this source: [Groupware](http://groupware.les.inf.puc-rio.br/har). The data used in this project has been generously provided by the authors, Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. _"Qualitative Activity Recognition of Weight Lifting Exercises."_" Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.

#**Project Goal**
One thing that people regularly do is quantify how  much of a particular activity they do, but they rarely quantify how well they do it. In this project, the main goal is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.

The goal of the project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.