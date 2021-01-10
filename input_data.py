import csv
import numpy as np
import pandas as pd
import random

df=pd.read_csv("D:/홍윤호교수님 연구/ALSFRS_R.csv")
df=pd.DataFrame(df) #feature_delta 0~2114, SubjectID=348~999990
for i in range(len(df):
    if df['feature_delta']!=0
        df['time']=df['feature_delta']//100
    else
        df['time']=0

# ID=sorted(set(df['SubjectID'].tolist()))
# print(len(ID))
# total=np.array(df,dtype='int')
# size1=len(ID)
# data=total[np.random.choice(size1,int(0.7*size1),replace=False),0:15]
# print(data.shape)


# id1=np.random.choice(size1,int(0.7*size1),replace=False)
# print(data[1])
# for id in ID:
#     id1=np.random.choice(size1,int(0.7*size1))
#     print(total[total[:,0]==id][3:15])
# print(total[0,3:15])
# for i in range(len(id1)):
#     print(total[np.where(total[:,0]== id1[i]),3:15])
# for id in id1:
#     print(total[:,3:15][total[:, 0] == id])
#     print(total[total[:,0]==id][3:15])
# subtotal=[]
# for id in ID:
#     subtotal.append(total[:,(0,3,4,5,6,7,8,9,10,11,12,13,14)][total[:,0]==id])
# subtotal1=np.array(subtotal)
# subtotal1=subtotal1.reshape(-1,12)
# print(subtotal1[0][1][0]) #subtotal 284행까지 있고 subtotal1[0][0][1]~[n]으로 하면 각 ID당 내원일별 ALSFRS score가 나옴 첫환자인 348번환자의 첫 내원일 ALSFRS는 [0][0][0]

# print(len(total))
# print(total[,3:14][np.where('SubjectID'==348)])
# print(totaltotal["SubjectID"==348,])
# print(total[:,[3,4,5,6,7,8,9,10,11,12,13,14]][total[:,1]==348])
# print(np.where(348==total[,0])[0][0])
# print(total[1,2])
# print(np.sum(total[1,3:15]))
# a=[random.randrange(1,7)]
# print(a)
# print(np.array([1/6, 1/6, 1/6, 1/6, 1/6, 1/6]*12))
# def input_data(x):
# df=pd.read_csv("D:/홍윤호교수님 연구/ALSFRS_R.csv")
# df=pd.DataFrame(df)
# ID=sorted(set(df['SubjectID'].tolist()))
# # print(ID)
# total=np.array(df,dtype=np.float32)
# print(df)
# print(df.columns)
# total=[]
# for row in f_a:
#     total.append(row)
# f.close()
# total_n=np.array(total)
# print(total_n)
# print(total[2][0])

# for i in len(subtotal):
#     if

# list=input_data("D:/홍윤호교수님 연구/ALSFRS_R.csv")
    # for line in total:
    #     subtotal.append(line)
    # subtotal.pop(0)
    # print(subtotal[1]) #ID별로 1줄씩 subtotal행렬에 담김.
    # subtotal_array=np.array(subtotal)
