import os
# importing csv module 
import csv 

rootdir = "D:/Data/Dropbox/Study/11.Semester/awf/_StudyResults"
extension = 'csv'

def getInvCount(arr, n): 
    inv_count = 0
    for i in range(n): 
        for j in range(i + 1, n): 
            if (arr[i] > arr[j]): 
                inv_count += 1
  
    return inv_count 

def differences(a, b):
    if len(a) != len(b):
        raise ValueError("Lists of different length.")
    return sum(i != j for i, j in zip(a, b))

time = 0

def getParticipantScores(partId):
    for subdir, dirs, files in os.walk(rootdir):
        if partId not in subdir: 
            continue
        part = {
            'id': partId,
            'orderingMis': 0, 
            'orderingTime': 0, 
            'matchingMis': 0, 
            'matchingTime': 0, 
            'countingMis': 0,
            'countingTime': 0
        }
        for file in files:
            if file.endswith(extension):
                if file.endswith('ordering.csv'):
                    # reading csv file 
                    with open(os.path.join(subdir, file), 'r') as csvfile:
                        # creating a csv reader object 
                        csvreader = csv.reader(csvfile, delimiter=';') 
                        next(csvreader, None)
                        lt = [e for e in csvreader]
                        ls = [e[2] for e in lt]
                        time = max([int(e[1]) for e in lt])
                        ob = {'orderingMis': getInvCount(ls, len(ls)), 'orderingTime': time}
                        part.update(ob)
                if file.endswith('matching.csv'):
                    # reading csv file 
                    with open(os.path.join(subdir, file), 'r') as csvfile:
                        # creating a csv reader object 
                        csvreader = csv.reader(csvfile, delimiter=';') 
                        next(csvreader, None)
                        lt = [e for e in csvreader]
                        ls = [e[4].lstrip() for e in lt]
                        time = max([int(e[1]) for e in lt])
                        ob = {'matchingMis': sum([0 if 'True' else 1 for x in ls]), 'matchingTime': time}
                        part.update(ob)
                if file.endswith('counting.csv'):
                    # reading csv file 
                    with open(os.path.join(subdir, file), 'r') as csvfile:
                        # creating a csv reader object 
                        csvreader = csv.reader(csvfile, delimiter=';') 
                        next(csvreader, None)
                        lt = [e for e in csvreader]
                        ls = [int(e[2].lstrip()) for e in lt]
                        time = max([int(e[1]) for e in lt])
                        ob = {'countingMis': differences(ls, [7,10,13]), 'countingTime': time}
                        part.update(ob)
        return part

headers = []
rows = []

with open(os.path.join(rootdir, 'resync.csv'), 'r') as csvfile:
    # creating a csv reader object 
    csvreader = csv.DictReader(csvfile, delimiter=';')
    headers = [e.lstrip() for e in csvreader.fieldnames]
    print(headers)
    for row in csvreader:
        ps = getParticipantScores(row['Participant_ID'])
        row['orderingMis'] = ps['orderingMis'] 
        row['orderingTime'] = ps['orderingTime'] 
        row['matchingMis'] = ps['matchingMis'] 
        row['matchingTime'] = ps['matchingTime'] 
        row['countingMis'] = ps['countingMis'] 
        row['countingTime'] = ps['countingTime'] 
        rows.append(row) 
        # df.loc[df['column_name'] == some_value]
        # print(row)

with open(os.path.join(rootdir, 'resync_clean.csv'), 'w+') as csvfile:
    csvwriter = csv.DictWriter(csvfile, delimiter=';', fieldnames=headers)
    csvwriter.writeheader()

    for row in rows:
        csvwriter.writerow(row)
