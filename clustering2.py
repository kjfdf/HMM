import pandas as pd
from sklearn.preprocessing import MinMaxScaler
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
# import sqlite3
from sqlalchemy import create_engine
import pymysql

pymysql.install_as_MySQLdb()

data=pd.read_csv("D:/홍윤호교수님 연구/ALSFRS_R.csv")
df=pd.DataFrame(data)
data_points=df[['ALSFRS_R_Total','Q1_Speech',
       'Q2_Salivation', 'Q3_Swallowing', 'Q4_Handwriting', 'Q5_Cutting',
       'Q6_Dressing_and_Hygiene', 'Q7_Turning_in_Bed', 'Q8_Walking',
       'Q9_Climbing_Stairs', 'R1_Dyspnea', 'R2_Orthopnea',
       'R3_Respiratory_Insufficiency']].values
kmeans=KMeans(n_clusters=6).fit(data_points)
print(kmeans.labels_)
df['cluster_id']=kmeans.labels_
print(df)
# df.to_csv("ALSFRS_r_clustered1.csv")
#pickle로 저장
df.to_pickle("df.pkl")
#pickle load
df = pd.read_pickle("df.pkl")
# # SQLite3 DB 저장
# con = sqlite3.connect("ALSFRS_r_clustered.db")
# df.to_sql("table_name", con, if_exists="append", index=True)
# con.close()
# # SQLite3 DB 불러오기
# con = sqlite3.connect("ALSFRS_r_clustered.db")
# df = pd.read_sql("SELECT * FROM table_name", con)
# con.close()
#MySQL로 저장
engine = create_engine("mysql+mysqldb://root:"+" "+"@127.0.0.1/hmm", encoding='utf-8')  #root database 비번(4---)
conn = engine.connect()
df.to_sql(name="ALSFRS_clustered", con=engine, if_exists='append')
conn.close()
#MySQL load
host_name="127.0.0.1"
username="root"
password="" #root database 비번(4---)
database_name="hmm"
db=pymysql.connect(
       host=host_name,
       port=3306,
       user=username,
       passwd=password,
       db=database_name,
       charset='utf8'
)
SQL="SELECT * FROM hmm" #"DESC hmm" or "SELECT COUNT(*) FROM hmm
df=pd.read_sql(SQL,db)