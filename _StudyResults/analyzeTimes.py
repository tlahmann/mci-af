# -*- coding: UTF-8 -*-
import os
# importing csv module 
import csv
import datetime
from collections import namedtuple
 
rootdir = ".\\_StudyResults"
extension = 'csv'

headers = []
rows = []

def getMovement(partId):
    for subdir, dirs, files in os.walk(rootdir):
        if partId not in subdir: 
            continue
        # p = namedtuple('pid', 'times ordering matching counting')
        times =[]
        ordering =[]
        matching =[]
        counting =[]
        for file in files:
            if file.endswith(extension):
                if file.endswith('headset.csv'):
                    # reading csv file 
                    with open(os.path.join(subdir, file), 'r') as csvfile:
                        # creating a csv reader object 
                        csvreader = csv.reader(csvfile, delimiter=';') 
                        next(csvreader, None)
                        next(csvreader, None)
                        lt = [e for e in csvreader]
                        time = [int(e[0].strip()) for e in lt]
                        time = [convert_dotnet_tick(t) for t in time]
                        times = time
                if file.endswith('ordering.csv'):
                    # reading csv file 
                    with open(os.path.join(subdir, file), 'r') as csvfile:
                        # creating a csv reader object 
                        csvreader = csv.reader(csvfile, delimiter=';') 
                        next(csvreader, None)
                        lt = [e for e in csvreader]
                        # time = [int(e[0].strip()) for e in lt]
                        # time = [convert_dotnet_tick(t) for t in time]
                        time = [int(e[1].strip()) for e in lt]
                        time.insert(0,0)
                        ordering = time
                if file.endswith('matching.csv'):
                    # reading csv file 
                    with open(os.path.join(subdir, file), 'r') as csvfile:
                        # creating a csv reader object 
                        csvreader = csv.reader(csvfile, delimiter=';') 
                        next(csvreader, None)
                        lt = [e for e in csvreader]
                        # time = [int(e[0].strip()) for e in lt]
                        # time = [convert_dotnet_tick(t) for t in time]
                        time = [int(e[1].strip()) for e in lt]
                        time.insert(0,0)
                        matching = time
                if file.endswith('counting.csv'):
                    # reading csv file 
                    with open(os.path.join(subdir, file), 'r') as csvfile:
                        # creating a csv reader object 
                        csvreader = csv.reader(csvfile, delimiter=';') 
                        next(csvreader, None)
                        lt = [e for e in csvreader]
                        # time = [int(e[0].strip()) for e in lt]
                        # time = [convert_dotnet_tick(t) for t in time]
                        time = [int(e[1].strip()) for e in lt]
                        time.insert(0,0)
                        counting = time
                        
        print("\"\"\"\"####,,,,", partId, ",,,,####\"\"\"\"")
        # print("started ordering: ", p.ordering[-1] - p.times[0], " and took ", p.ordering[-1] - p.ordering[0])
        # print("started matching: ", p.matching[-1] - p.times[0], " and took ", p.matching[-1] - p.matching[0])
        # print("started counting: ", p.counting[-1] - p.times[0], " and took ", p.counting[-1] - p.counting[0])
        print()
        return ordering, matching, counting

def convert_dotnet_tick(ticks):
    """Convert .NET ticks to formatted ISO8601 time
    Args:
        ticks: integer
            i.e 100 nanosecond increments since 1/1/1 AD"""
    _date = datetime.datetime(1, 1, 1) + \
        datetime.timedelta(microseconds=ticks // 10)
    if _date.year < 1900:  # strftime() requires year >= 1900
        _date = _date.replace(year=_date.year + 1900)
    return _date # _date.strftime("%Y-%m-%dT%H:%M:%S.%fZ")[:-3]

class MyClass:
	def __init__(self, val, prefix, indx):
		self.val = val

# Überprüfen, ob dieses Modul als Programm läuft und nicht in einem anderen Modul importiert wird.
if __name__ == '__main__':
    rows = []

    with open(os.path.join(rootdir, 'resync_clean_combined.csv'), 'r') as csvfile:
        # creating a csv reader object 
        csvreader = csv.DictReader(csvfile, delimiter=';')
        headers = [e.lstrip() for e in csvreader.fieldnames]
        print(headers)
        pid = ''
        for row in csvreader:
            pid = row['Participant_ID']
            times = getMovement(pid)
            ordering = [j-i for i, j in zip(times[0][:-1], times[0][1:])]
            row["o1"] = ordering[0]
            row["o2"] = ordering[1]
            row["o3"] = ordering[2]
            row["o4"] = ordering[3]
            row["o5"] = ordering[4]
            row["o6"] = ordering[5]
            row["o7"] = ordering[6]
            row["o8"] = ordering[7]
            row["o9"] = ordering[8]
            row["o10"] = ordering[9]
            matching = [j-i for i, j in zip(times[1][:-1], times[1][1:])]
            row["m1"] = matching[0]
            row["m2"] = matching[1]
            row["m3"] = matching[2]
            row["m4"] = matching[3]
            row["m5"] = matching[4]
            row["m6"] = matching[5]
            row["m7"] = matching[6]
            row["m8"] = matching[7]
            row["m9"] = matching[8]
            row["m10"] = matching[9]
            counting = [j-i for i, j in zip(times[2][:-1], times[2][1:])]
            row["c1"] = counting[0]
            row["c2"] = counting[1]
            row["c3"] = counting[2]
            rows.append(row)
    
    hdrs = [
        "o1", "o2", "o3", "o4", "o5", "o6", "o7", "o8", "o9", "o10",
        "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10",
        "c1", "c2", "c3"
    ]
    with open(os.path.join(rootdir, 'resync_complete.csv'), 'w+', newline='\n', encoding='utf-8') as csvfile:
        csvwriter = csv.DictWriter(csvfile, delimiter=';', fieldnames=headers+hdrs)
        csvwriter.writeheader()

        for row in rows:
            csvwriter.writerow(row)
