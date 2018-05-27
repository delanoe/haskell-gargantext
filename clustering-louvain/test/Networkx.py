

import networkx as nx
import community as co
from collections import defaultdict

karate = nx.read_gexf("../src/Data/karate.gexf")
part = co.best_partition(karate)

print(karate.edges())
#result = defaultdict(list)
result = dict()

for k,v in part.items():
    if v is not None:
        r = result.get(v, [])
        r.append(k)
        result[v] = r
print(result)

