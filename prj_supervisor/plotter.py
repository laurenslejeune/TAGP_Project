import csv
import matplotlib.pyplot as plt

data_CSV = open('data.csv', 'r')
data_CSV_reader = csv.DictReader(data_CSV)

flow1 = []
flow2 = []
temp1 = []
temp2 = []

def strList2floatList(List):
    newList = []
    i = 0
    for el in List:
        newList.append(float(el))
        i+=1
    return newList

for row in data_CSV_reader:
    flow1.append(row['Flow1'])
    flow2.append(row['Flow2'])
    temp1.append(row['Temp1'])
    temp2.append(row['Temp2'])

flow1 = strList2floatList(flow1)
flow2 = strList2floatList(flow2)
temp1 = strList2floatList(temp1)
temp2 = strList2floatList(temp2)

def maxInList(List):
    currentMax = 0
    for el in List:
        if (el>currentMax):
            currentMax = el
    return currentMax

def minInList(List):
    currentMin = 100000000000000
    for el in List:
        if (el<currentMin):
            currentMin = el
    return currentMin

print(len(flow1))
time = [tick*60/len(flow1) for tick in range(0,len(flow1))] #Time in seconds

plt.figure(1)
plt.subplot(121)
plt.plot(time,flow1,'r-',time,flow2,'bo')
plt.axis([0,60,min(1.1*minInList(flow1),1.1*minInList(flow2)),max(1.1*maxInList(flow1),1.1*maxInList(flow2))])
plt.xlabel('Time (s)')
plt.ylabel('Flow')
plt.title('Flow in function of the time',fontsize=12)

plt.subplot(122)
plt.plot(time,temp1,'r-',time,temp2,'bo')
plt.axis([0,60,min(1.1*minInList(temp1),1.1*minInList(temp2)),max(1.1*maxInList(temp1),1.1*maxInList(temp2))])
plt.title('Temperature in function of the time',fontsize=12)
plt.xlabel('Time (s)')
plt.ylabel('Temperatur (°C)')
plt.show()