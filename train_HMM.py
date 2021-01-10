# coding: utf-8
import sys
import pandas as pd
sys.path.append('..')
sys.path.append('../ch07')
import numpy as np
import matplotlib.pyplot as plt
from optimizer import Adam
from trainer import Trainer
from util import eval_seq2seq
from attention_seq2seq import AttentionSeq2seq
from seq2seq import Seq2seq
from peeky_seq2seq import PeekySeq2seq

# 데이터 읽기
df=pd.read_csv("D:/홍윤호교수님 연구/ALSFRS_R.csv")
df=pd.DataFrame(df)
ID=sorted(set(df['SubjectID'].tolist()))
print(len(ID))
total=np.array(df,dtype='int')
size1=len(ID)
x_train, x_val, x_test = total[np.random.choice(size1,int(0.7*size1),replace=False),3:15], total[np.random.choice(size1,int(0.2*size1),replace=False),3:15], total[np.random.choice(size1,int(0.1*size1),replace=False),3:15] #subtotal에서 random하게 데이터를 뽑아서 training 70%, validation 20%, test 10%로 넣기

class RNN:
    def __init__(self,Wx,Wh,b):
        self.params=[Wx,Wh,b]
        self.grads=[np.zeros_like(Wx),np.zeros_like(Wh),np.zeros_like(b)]
        self.cache=None
    def forward(selfself,x,h_prev):
        Wx,Wh,b=self.params
        t=np.matmul(h_prev,Wh)+np.matmul(x,Wx)+b
        h_next=np.tanh(t)
        self.cache=(x,h_prev,h_next)
        return h_next
    def backward(selfself,dh_next):
        Wx,Wh,b=self.params
        x,h_prev,h_next=self.cache
        dt=dh_next*(1-h_next**2)
        db=np.sum(dt,axis=0)
        dWh=np.matmul(h_prev.T,dt)
        dh_prev=np.matmul(dt,Wh.T)
        dWx=np.matmul(x.T,dt)
        dx=np.matmul(dt,Wx.T)
        self.grads[0][...]=dWx
        self.grads[1][...]=dWh
        self.grads[2][...]=db
        return dx,dh_prev

# 입력 데이터 반전
x_train, x_val, x_test = x_train[:, ::-1], x_val[:, ::-1], x_test[:, ::-1]

# 하이퍼파라미터 설정
vocab_size = len(ID)
wordvec_size = 12
hidden_size = 6
batch_size = 100
max_epoch = 10
max_grad = 5.0

model = AttentionSeq2seq(vocab_size, wordvec_size, hidden_size)
# model = Seq2seq(vocab_size, wordvec_size, hidden_size)
# model = PeekySeq2seq(vocab_size, wordvec_size, hidden_size)

optimizer = Adam()
trainer = Trainer(model, optimizer)

acc_list = []
for epoch in range(max_epoch):  # training데이터를 넣어서 가장 확률값이 높은 값을 고르게 하기.
    trainer.fit(x_train, t_train, max_epoch=1,
                batch_size=batch_size, max_grad=max_grad)

    correct_num = 0
    for i in range(len(x_test)):
        question, correct = x_test[[i]], t_test[[i]]
        verbose = i < 10
        correct_num += eval_seq2seq(model, question, correct,
                                    id_to_char, verbose, is_reverse=True)

    acc = float(correct_num) / len(x_test)
    acc_list.append(acc)
    print('정확도 %.3f%%' % (acc * 100))


model.save_params()

# 그래프 그리기
x = np.arange(len(acc_list))
plt.plot(x, acc_list, marker='o')
plt.xlabel('에폭')
plt.ylabel('정확도')
plt.ylim(-0.05, 1.05)
plt.show()
