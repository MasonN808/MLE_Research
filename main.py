# This file implements the method used in Statistical Inference by Dr. Francesco Javier Rubio.
import math
import logging
import numpy as np
import sympy as syp
from math import factorial

class MLE:
    def __init__(self, pdf, params, data):
        self.pdf = pdf
        self.params = params
        self.data = data

    def l(self, pdf, data):
        instance = 0
        for i in data[0].size:
            instance += pdf

def comb(n, k):
    return factorial(n) / factorial(k) / factorial(n - k)

if __name__ == '__main__':
    xdata = np.array([1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839])
    ydata = np.array([6, 13, 18, 28, 52, 53, 61, 60]).astype(int)
    ndata = np.array([59, 60, 62, 56, 63, 59, 62, 60]).astype(int)
    data = np.array([xdata, ydata, ndata])
    theta1 = syp.Symbol("theta1")
    theta2 = syp.Symbol("theta2")
    x = syp.Symbol("x")
    y = syp.Symbol("y")
    params = np.array([theta1, theta2])

    if xdata.size != ndata.size != ydata.size:
        logging.warning("xdata, ndata, and ydata are not of the same size")
    seq = np.array(np.arange(xdata.size))
    l = 0
    probs = []
    for i in seq:
        probs.append((syp.exp(theta1 + theta2*data[0][i]))/(1 + syp.exp(theta1 + theta2*data[0][i])))
    print("PROBS: " + str(probs))
    for i in seq:
        l += comb(data[2][i], data[1][i])*probs[i]**data[1][i]*(1-probs[i])**(data[2][i] - data[1][i])
    print("LIKELIHOOD FUNCTION: " + str(l))
    print("DATA: " + str(data))
    lnl = syp.ln(l)
    score1 = syp.diff(lnl, theta1)
    score2 = syp.diff(lnl, theta2)
    print("Score Function wrt THETA1: " + str(score1))
    print("Score Function wrt THETA2: " + str(score2))
    info_matrix = np.matrix([[score1**2, score1*score2], [score2*score1, score2**2]])
    print("INFO_MATRIX" + str(info_matrix))
    print(info_matrix.shape)
    initial = np.array([0, 0])
    nxt = np.array([0, 0])
    for i in np.arange(100):
        print(i)
        subed_matrix = np.vectorize(lambda z: z.subs({theta1: initial[0], theta2: initial[1]}))(info_matrix)
        print(subed_matrix)
        print(np.linalg.inv(subed_matrix))
        nxt += initial + np.linalg.inv(subed_matrix)*np.array([score1.subs(theta1, initial[0]).subs(theta2, initial[1]), score2.subs(theta1, initial[0]).subs(theta2, initial[1])]).transpose
        initial = nxt
    print(nxt)
    # MLE(pdf, params, data)
