
# coding: utf-8

# In[9]:


import pandas as pd
import numpy as np

def load_data(file_path):
    train_file = pd.read_csv(file_path)
    train_file = train_file.sort_values(by='test_id')
    return train_file

output0 = load_data("C:\\Users\\raque\\Documents\\SFU\\Data Mining\\Project\\output_final_0.csv")
output1 = load_data("C:\\Users\\raque\\Documents\\SFU\\Data Mining\\Project\\output_final_1.csv")
out = np.column_stack((output0['rating'],output1['rating']))

n = 20 #insert here max indice output
print(output0['rating'].describe())

for i in np.arange(2,n+1):
    name = "C:\\Users\\raque\\Documents\\SFU\\Data Mining\\Project\\output_final_"+str(i)+".csv"
    output2 = load_data(name)
    out = np.column_stack((out,output2['rating']))


# In[16]:


out2 = np.sum(out, axis=1)
output0['rating'] = out2
output0['rating'].describe()
#should be different
print(output0['rating'].describe())
finaloutput = output0['rating']/(n+1)
finaloutput.describe()


# In[17]:


name = 'output_sum'+str(n+1)+'.csv'
finaloutput.to_csv(name,sep=',',index=False)

