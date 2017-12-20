
# coding: utf-8

# In[1]:


'''Library'''
from surprise import SVD, SVDpp
from surprise import Dataset
from surprise import evaluate, print_perf
import pandas as pd
from surprise import Reader 
from surprise import accuracy


# In[2]:


#train_data_path = "D:\\741Project\Data\\train_rating.txt"
#test_data_path  =  "D:\\741Project\Data\\test_rating.txt"
train_data_path = "C:\\Users\\raque\\Documents\\SFU\\Data Mining\\Project\\train_rating.txt"
test_data_path  =  "C:\\Users\\raque\\Documents\\SFU\\Data Mining\\Project\\test_rating.txt"


# In[3]:


'''Load training set'''
def load_data(file_path):
    train_file = pd.read_csv(file_path)
    return train_file

train_data = load_data(train_data_path)
train_data = train_data.drop(['train_id','date'],axis=1)
train_data.columns = ['user','item','rating']
train_data_test = train_data.copy()


# In[4]:


'''Split 10% of training to evaluation'''
train_data_test = train_data_test.sample(int(train_data.shape[0]*0.1))
idd = train_data_test["user"].map(str) +'-'+ train_data_test["item"].map(str)
train_data_train = train_data.copy()
train_data_train['id'] = train_data_train["user"].map(str) +'-'+ train_data_train["item"].map(str)
train_data_train = train_data_train[~train_data_train['id'].isin(idd)]
train_data_train = train_data_train.drop('id',axis=1)


# In[5]:


'''training model'''
reader = Reader(rating_scale=(1, 5))
td = Dataset.load_from_df(train_data_train, reader)
algo = SVD()
#algo = SVDpp()
trainset = td.build_full_trainset()
algo.train(trainset)


# In[6]:


'''evaluation on 10% of training set'''
td2 = Dataset.load_from_df(train_data_test,reader)
td2 = td2.build_full_trainset()
testset = td2.build_testset()
predictions = algo.test(testset)
accuracy.rmse(predictions, verbose=True) 


# In[7]:


'''Load testing set'''
test_data = load_data(test_data_path)
test = test_data.drop(['test_id','date'],axis=1)
test.columns = ['user','item']
print(test_data.head())


# In[8]:


'''Predictions testing set'''
rating = []
test_id = []
for i in range(test.shape[0]):
    rating.append(algo.predict(test_data.user_id[i],test_data.business_id[i])[3])
    test_id.append(test_data.test_id[i])


# In[9]:


'''Short output descrition'''
data = {'rating':rating,'test_id':test_id}
output = pd.DataFrame(data)
output.to_csv("output_final_119.csv",sep=',',index=False)
output['rating'].describe()

