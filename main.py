# This file implements the method used in Statistical Inference by Dr. Francesco Javier Rubio.
from math import comb
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


# def comb(n, k):
#     return factorial(n) / factorial(k) / factorial(n - k)


if __name__ == '__main__':
    xdata = [1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839]
    ydata = [6, 13, 18, 28, 52, 53, 61, 60]
    ndata = [59, 60, 62, 56, 63, 59, 62, 60]
    data = [xdata, ydata, ndata]
    theta1 = syp.Symbol("theta1")
    theta2 = syp.Symbol("theta2")
    x = syp.Symbol("x")
    y = syp.Symbol("y")
    params = [theta1, theta2]
    # if xdata.size != ndata.size != ydata.size:
    #     logging.warning("xdata, ndata, and ydata are not of the same size")
    seq = np.arange(len(xdata))
    likelihood = 1
    pi = []
    for i in seq:
        # Create the pi_i's
        pi.append((syp.exp(theta1 + theta2 * data[0][i])) / (1 + syp.exp(theta1 + theta2 * data[0][i])))
    print("PI_i: " + str(pi))
    for i in seq:
        # Compute the likelihood function
        likelihood *= comb(data[2][i], data[1][i]) * (pi[i] ** data[1][i]) * (1 - pi[i]) ** (
                    data[2][i] - data[1][i])
    print("LIKELIHOOD FUNCTION: " + str(likelihood))
    # print("DATA: " + str(data))
    # Get the log-likelihood function
    lnl = syp.ln(likelihood)
    # Get the score functions
    score1 = syp.diff(lnl, theta1)
    score2 = syp.diff(lnl, theta2)
    print("Score Function wrt THETA1: " + str(score1))
    print("Score Function wrt THETA2: " + str(score2))
    # info_matrix = np.matrix([[score1 ** 2, score1 * score2], [score2 * score1, score2 ** 2]])
    # info_matrix = np.matrix([[lnl.diff(theta1).diff(theta2) for x in params] for y in params])
    info_matrix = syp.Matrix(syp.hessian(lnl, params))
    # info_matrix = np.matrix([[lnl.diff(params[0])], lnl.diff[params[1]]])
    # print("INFO_MATRIX" + str(info_matrix))

    print("INFO_MATRIX_SHAPE: " + str(info_matrix.shape))
    initial = [0, 0]
    nxt = None
    for i in np.arange(100):
        print(i)
        # information matrix subbed in with values for theta1 and theta2, then converted to float for consistency
        # subed_info_matrix = np.vectorize(lambda z: z.subs({theta1: initial[0], theta2: initial[1]}))(
        #     info_matrix).astype(dtype=np.float64)
        subed_info_matrix = np.matrix(info_matrix.subs(theta1, initial[0]).subs(theta2, initial[1]))
        print(subed_info_matrix)
        scores_array = [score1.subs(theta1, initial[0]).subs(theta2, initial[1]),
             score2.subs(theta1, initial[0]).subs(theta2, initial[1])]
        # scores_array_transpose = np.transpose(scores_array)
        nxt = initial + np.dot(np.linalg.inv(subed_info_matrix.astype(dtype= np.float64)), scores_array)
        # Convert matrix to array with two elements
        nxt = np.asarray(nxt).flatten()
        print(initial)
        initial = nxt
    # MLE(pdf, params, data)
