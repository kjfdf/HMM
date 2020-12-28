import numpy as np
import matplotlib.pyplot as plt

def random_normalized(d1,d2):
    x=np.random.random((d1,d2))
    return x/x.sum(axis=1,keepdims=True)

class HMM:
    def __init__(self,M):
        self.M=M

    def fit(self,X,max_iter=30):
        np.random.seed(123)

        V=max(max(x) for x in x +1)

a_num=np.zeros((self.M,self.M))
for n in range(N):
    x=X[n]
    T=len(x)
    den1+=(alphas[n][:-1]*betas[n][:-1]).sum(axis=0,keepdims=True).T/P[n]
    a_num_n=np.zeros((self.M,self.M))
    for i in range(self.M):
        for j in range(self.M):
            for t in range(T-1):
                a_num_n[i,j]+=alphas[n][t,i]*betas[n][t+1,j]*self.A[i,j]*self.B[j,x[t+1]]
    a_num+=a_num_n/P[n]
self.A=a_num/den1

b_num=np.zeros((self.M,V))
for n in range(N):
    x=X[n]
    T=len(x)
    den2+=(alphas[n]*betas[n]).sum(axis=0,keepdims=True).T/P[n]
    b_num_n=np.zeros((self.M,V))
    for i in range(self.M):
        for j in range(V):
            for t in range(T):
                if x[t]==j:
                    b_num[i,j]+=alphas[n][t][i]*betas[n][t][i]
    b_num+=b_num_n/P[n]
self.B=b_num/den2

def generate_sequence(N):
    s=np.random.choice(range(M),p=pi) #initial state
    x=np.random.choice(range(V),p=B[s]) #initial observation
    sequence=[x]
    for n in range(N-1):
        s=np.random.choice(range(M),p=A[s]) #next state
        x=np.random.choice(range(V),p=B[s]) #next observation
        sequence.append(x)
    return sequence