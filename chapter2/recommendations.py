# A dictionary of movie critics and their ratings of a small
# set of movies
critics = {'Lisa Rose': {'Lady in the Water': 2.5, 'Snakes on a Plane': 3.5,
                         'Just My Luck': 3.0, 'Superman Returns': 3.5, 'You, Me and Dupree': 2.5,
                         'The Night Listener': 3.0},
           'Gene Seymour': {'Lady in the Water': 3.0, 'Snakes on a Plane': 3.5,
                            'Just My Luck': 1.5, 'Superman Returns': 5.0, 'The Night Listener': 3.0,
                            'You, Me and Dupree': 3.5},
           'Michael Phillips': {'Lady in the Water': 2.5, 'Snakes on a Plane': 3.0,
                                'Superman Returns': 3.5, 'The Night Listener': 4.0},
           'Claudia Puig': {'Snakes on a Plane': 3.5, 'Just My Luck': 3.0,
                            'The Night Listener': 4.5, 'Superman Returns': 4.0,
                            'You, Me and Dupree': 2.5},
           'Mick LaSalle': {'Lady in the Water': 3.0, 'Snakes on a Plane': 4.0,
                            'Just My Luck': 2.0, 'Superman Returns': 3.0, 'The Night Listener': 3.0,
                            'You, Me and Dupree': 2.0},
           'Jack Matthews': {'Lady in the Water': 3.0, 'Snakes on a Plane': 4.0,
                             'The Night Listener': 3.0, 'Superman Returns': 5.0, 'You, Me and Dupree': 3.5},
           'Toby': {'Snakes on a Plane': 4.5, 'You, Me and Dupree': 1.0, 'Superman Returns': 4.0}}


def sim_distance(prefs, person1, person2):
    """Returns a distance-based similarity score for person1 and person2

    :param prefs
    :param person1
    :param person2
    :return
    """
    si = [(prefs[person1][item] - prefs[person2][item]) **
          2 for item in prefs.get(person1, []) if item in prefs.get(person2, [])]
    if not si:
        return 0.0
    sum_of_squares = sum(si)
    return 1 / (1 + sum_of_squares)


def sim_pearson(prefs, p1, p2):
    """Returns the Pearson correlation coefficient for p1 and p2

    :param
    :param
    :param
    :return
    """
    si = [item for item in prefs.get(p1, []) if item in prefs.get(p2, [])]
    if not si:
        return 0.0

    n = len(si)
    sum1, sum2 = sum([prefs[p1][it] for it in si]), sum([prefs[p2][it] for it in si])
    sumq1, sumq2 = sum([prefs[p1][it] ** 2 for it in si]), sum([prefs[p2][it] ** 2 for it in si])
    psum = sum([prefs[p1][it] * prefs[p2][it] for it in si])

    num = n * psum - sum1 * sum2
    den = ((n * sumq1 - sum1 ** 2) * (n * sumq2 - sum2 ** 2)) ** 0.5

    return (num / den) if den else 0.0

import heapq


def topMatches(prefs, person, n=5, similarity=sim_pearson):
    """Returns the best matches for person from the prefs dictionary.

    Number of results and similarity function are optional params.

    :param
    :param
    :param
    :param
    """
    pairs = [(-similarity(prefs, p, person), p) for p in prefs if p != person]
    heapq.heapify(pairs)
    ret = [heapq.heappop(pairs) for i in xrange(n) if pairs]
    return [(-pair[0], pair[1]) for pair in ret]


import itertools


def getRecommendations(prefs, person, similarity=sim_pearson):
    """Gets recommendations for a person by using a weighted average of every other user's rankings.

    :param
    :param
    :param
    :return
    """
    sims = dict([(p, similarity(prefs, p, person)) for p in prefs if p != person])
    seen = prefs.get(person, [])
    mp = sorted([(m, p) for p in prefs if p != person for m in set(
        prefs[p].keys()).difference(seen)], key=lambda e: e[0])
    scores = []
    for k, v in itertools.groupby(mp, key=lambda e: e[0]):
        num, den = 0, 0
        for m, p in v:
            num, den = num + prefs[p][m] * sims[p], den + sims[p]
        if den:
            scores.append((k, num / den))
    return sorted(scores, key=lambda e: e[1], reverse=True)


def transformPrefs(prefs):
    result = {}
    for person in prefs:
        for item in prefs[person]:
            result.setdefault(item, {})
            result[item][person] = prefs[person][item]
    return result


def calculateSimilarItems(prefs, n=10, verbose=False):
    """Create a dictionary of items showing which other items they are most similar to.

    :param
    :param
    :return
    """
    item_prefs = transformPrefs(prefs)
    ret = {}
    for i, item in enumerate(item_prefs):
        if verbose and (i + 1) % 100 == 0:
            print('{} / {}'.format(c + 1, len(item_prefs)))
        scores = topMatches(item_prefs, item, n=n, similarity=sim_pearson)
        ret[item] = scores
    return ret


def getRecommendedItems(prefs, itemMatch, user):
    """Gets recommendations for a person by using a weighted average of ratings he/she gave.

    :param
    :param
    :param
    :return
    """
    user_ratings = prefs[user]
    mat = {}
    for item, rating in user_ratings.iteritems():
        sim_items = itemMatch.get(item, [])
        for sim, it in sim_items:
            if not it in user_ratings:
                mat.setdefault(it, [])
                mat[it].append((sim * rating, sim))
    scores = []
    for k, v in mat.iteritems():
        num, den = map(sum, zip(*v))
        if den:
            scores.append((k, float(num) / den))
    return sorted(scores, key=lambda e: -e[1])
