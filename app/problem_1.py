

a = []
b = []
with open('inputs/day1.txt', 'r') as f:
    data = f.readlines()
    for d in data:
        ab = d.strip().split()
        a.append(int(ab[0]))
        b.append(int(ab[1]))
a.sort()
b.sort()
isum = 0
for i,j in zip(a,b):
    isum = isum + abs(i - j)
print('1a: sum:', isum)

isum = 0
for i,j in zip(a,b):
    isum = isum + i*b.count(i)
print('1b: sum:', isum)
