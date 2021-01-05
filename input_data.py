import csv
import numpy as np

# def input_data(x):
f=open('D:/홍윤호교수님 연구/ALSFRS_R.csv','r',encoding='utf-8')
total=csv.reader(f)
subtotal=[]
for row in total:
    subtotal.append(row)
f.close()
print(subtotal[2][0])

for i in len(subtotal):
    if 

# list=input_data("D:/홍윤호교수님 연구/ALSFRS_R.csv")
    # for line in total:
    #     subtotal.append(line)
    # subtotal.pop(0)
    # print(subtotal[1]) #ID별로 1줄씩 subtotal행렬에 담김.
    # subtotal_array=np.array(subtotal)
