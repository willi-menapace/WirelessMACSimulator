import sys
import subprocess

from multiprocessing import Pool

def worker(x):
    subprocess.check_call(["C:\Python27\python.exe", "main.py", "-r", str(x)])

if __name__ == '__main__':
    subprocess.check_call(["C:\Python27\python.exe", "main.py", "-l"])

    p = Pool(8)
    print(p.map(worker, range(0, 4557)))

