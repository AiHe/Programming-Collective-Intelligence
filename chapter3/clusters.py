import string
import re
import operator
from PIL import Image, ImageDraw


def readfile(filename):
    rownames, data = [], []
    for i, line in enumerate(file(filename)):
        if not i:
            colnames = line.strip().split()
        else:
            head, tail = re.split('\t', line.strip(), 1)
            rownames.append(head)
            data.append(map(float, tail.split()))
    return rownames, colnames, data


def pearson(v1, v2):
    n = len(v1)
    sum1, sum2 = map(sum, (v1, v2))

    sumq1 = sum([e ** 2.0 for e in v1])
    sumq2 = sum([e ** 2.0 for e in v2])

    psum = sum(map(lambda e: operator.mul(*e), zip(v1, v2)))

    num = n * psum - sum1 * sum2
    den = ((n * sumq1 - sum1 ** 2) * (n * sumq2 - sum2 ** 2)) ** 0.5

    return (1 - num / den) if den else 0.0


class bicluster:

    def __init__(self, vec, left=None, right=None, distance=0.0, id=None):
        self.left = left
        self.right = right
        self.vec = vec
        self.id = id
        self.distance = distance


def hcluster(rows, distance=pearson):
    clusters = [bicluster(r, id=i) for i, r in enumerate(rows)]
    cur_id, dist_cache = -1, {}

    while len(clusters) > 1:
        close_idx = (0, 1)
        close_dist = distance(clusters[0].vec, clusters[1].vec)
        for i in xrange(len(clusters)):
            for j in xrange(i + 1, len(clusters)):
                # cache id instead of index because indices are changing every itration
                if not (clusters[i].id, clusters[j].id) in dist_cache:
                    dist_cache[(clusters[i].id, clusters[j].id)] = distance(
                        clusters[i].vec, clusters[j].vec)
                dist = dist_cache[(clusters[i].id, clusters[j].id)]
                if dist <= close_dist:
                    close_idx, close_dist = (i, j), dist
        p, q = close_idx
        new_vec = map(lambda e: (e[0] + e[1]) / 2.0, zip(clusters[p].vec, clusters[q].vec))
        new_cluster = bicluster(new_vec, left=clusters[p], right=clusters[
                                q], distance=close_dist, id=cur_id)
        cur_id -= 1
        clusters.pop(q)  # larger one goes first to prevent shifting
        clusters.pop(p)
        clusters.append(new_cluster)
    return clusters[0]


def getheight(clust):
    if not clust:
        return 0
    elif not clust.left and not clust.right:
        return 1
    else:
        return getheight(clust.left) + getheight(clust.right)


def getdepth(clust):
    if not clust:
        return 0
    elif not clust.left and not clust.right:
        return 0
    else:
        return max(getdepth(clust.left), getdepth(clust.right)) + clust.distance


def drawdendrogram(clust, labels, jpeg='clusters.jpg'):
    # height and width
    h = getheight(clust) * 20
    w = 1200
    depth = getdepth(clust)
    # width is fixed, so scale distances accordingly
    scaling = float(w - 150) / depth
    # Create a new image with a white background
    img = Image.new('RGB', (w, h), (255, 255, 255))
    draw = ImageDraw.Draw(img)
    draw.line((0, h / 2, 10, h / 2), fill=(255, 0, 0))
    # Draw the first node
    drawnode(draw, clust, 10, (h / 2), scaling, labels)
    img.save(jpeg, 'JPEG')


def drawnode(draw, clust, x, y, scaling, labels):
    if clust.id < 0:
        h1 = getheight(clust.left) * 20
        h2 = getheight(clust.right) * 20
        top = y - (h1 + h2) / 2
        bottom = y + (h1 + h2) / 2
        # Line length
        ll = clust.distance * scaling
        # Vertical line from this cluster to children
        draw.line((x, top + h1 / 2, x, bottom - h2 / 2), fill=(255, 0, 0))
        # Horizontal line to left item
        draw.line((x, top + h1 / 2, x + ll, top + h1 / 2), fill=(255, 0, 0))
        # Horizontal line to right item
        draw.line((x, bottom - h2 / 2, x + ll, bottom - h2 / 2), fill=(255, 0, 0))

        # Call the function to draw the left and right nodes
        drawnode(draw, clust.left, x + ll, top + h1 / 2, scaling, labels)
        drawnode(draw, clust.right, x + ll, bottom - h2 / 2, scaling, labels)
    else:
        # If this is an endpoint, draw the item label
        draw.text((x + 5, y - 7), labels[clust.id], (0, 0, 0))
