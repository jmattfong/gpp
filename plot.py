#!/usr/local/bin/python

import sys
import numpy as np
import matplotlib.pyplot as plt

f = open(sys.argv[1], 'r')
f2 = open(sys.argv[2], 'r')
xs = []
ys = []
xs2 = []
ys2 = []
for line in f.readlines() :
	parts = line.split(',')
	xs.append((parts[1]))
	ys.append(float(parts[0]))

for line in f2.readlines() :
	parts = line.split(',')
	xs2.append((parts[1]))
	ys2.append(float(parts[0]))

ind = np.arange(len(ys))
width= 0.35

print len(ys), len(ys2), len(xs), len(xs2)

fig = plt.figure()
ax = fig.add_subplot(111)
rects1 = ax.bar(ind, ys, width, color='red')
rects2 = ax.bar(ind+width, ys2, width, color='blue')

ax.set_xlabel('Feature')
ax.set_ylabel('Performance')
ax.set_title('Classifier Accuracy Predicting Stock with Single Features')
ax.set_xticks(ind + width)
ax.set_xticklabels(xs)
ax.legend((rects1[0], rects2[0]), ('Full', 'Snippets'))

def autolabel(rects):
    # attach some text labels
    for rect in rects:
        height = rect.get_height()
        ax.text(rect.get_x()+rect.get_width()/2., 1.05*height, '%d'%int(height),
                ha='center', va='bottom')
autolabel(rects1)
autolabel(rects2)

plt.show()