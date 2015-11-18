import string
import re
import operator


def readfile(filename):
    rownames, data = [], []
    for i, line in enumerate(file(filename)):
        if not i:
            colnames = line.strip().split()
        else:
            head, tail = re.split('\t', line.strip(), 1)
            rownames.append(head)
            data.append(map(float, tail.split()))
    return colnames, rownames, data


def pearson(v1, v2):
    n = len(v1)
    sum1, sum2 = map(sum, (v1, v2))

    sumq1 = sum([e ** 2.0 for e in v1])
    sumq2 = sum([e ** 2.0 for e in v2])

    psum = sum(map(lambda e: operator.mul(*e), zip(v1, v2)))

    num = n * psum - sum1 * sum2
    den = ((n * sumq1 - sum1 ** 2) * (n * sumq2 - sum2 ** 2)) ** 0.5

    return (num / den) if den else 0.0


class bicluster:

    def __init__(self, vec, left=None, right=None, distance=0.0, id=None):
        self.left = left
        self.right = right
        self.vec = vec
        self.id = id
        self.distance = distance
