import numpy as np
import math
from itertools import islice

if __name__ == '__main__':

    # read the undirected graph
    f = open('test-2.txt', 'r')
    edges = [line.strip('\n').split(' ') for line in islice(f, 4, None)]
    print("Edges:")
    print(edges)

    # get nodes
    nodes = []
    for edge in edges:
        if edge[0] not in nodes:
            nodes.append(edge[0])
        if edge[1] not in nodes:
            nodes.append(edge[1])
    print("Nodes:")
    print(nodes)

    N = len(nodes)

    # turn node Symbol to numbers from 0
    v = 0
    node_to_num = {}
    for node in nodes:
        node_to_num[node] = v
        v += 1
    for edge in edges:
        edge[0] = node_to_num[edge[0]]
        edge[1] = node_to_num[edge[1]]
    """print(edges)"""

    # init transition matrix T
    T = np.zeros([N, N])
    for edge in edges:
        T[edge[1], edge[0]] = 1
    # calculation of out degree value
    for u in range(N):
        sum_of_col = sum(T[:, u])
        for v in range(N):
            if sum_of_col != 0:
                T[v, u] /= sum_of_col
            else:
                T[v, u] = 1 / N
    print("Transition matrix T':")
    print(T)

    # Matrix A: (1 − alpha) * T
    alpha = 0.15
    A = (1 - alpha) * T
    print("Matrix A: (1 − alpha) * T")
    print(A)

    P_n = np.ones(N) / N
    P_n1 = np.zeros(N)

    e = 100000  # difference inaccuracy between two iterations
    k = 0  # iteration times
    print('loop...')

    while e > 0.00000001:  # 误差在0.00000001
        P_n1 = np.dot(A, P_n) + alpha * 1 / N
        norm = np.linalg.norm(P_n1, 1)  # ||P||_1
        print("norm: ", norm)
        P_n1 += (1 - norm) / N  # normalize
        e = P_n1 - P_n
        e = max(map(abs, e))  # 计算误差
        P_n = P_n1
        k += 1
        # print('iteration %s:' % str(k), P_n1)
    print("iteration times:", k)
    print("final result: ", P_n)
