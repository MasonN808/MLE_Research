# This file implements the method used in Statistical Inference by Dr. Francesco Javier Rubio.
from math import comb
import logging
import numpy as np
import pandas
import sympy as syp
import pandas as pd
from rdatasets import data
import statsmodels.api as sm
from math import factorial


class MLE:

    def __init__(self, data: np.array, iterations: int):
        self.iterations = iterations
        self.data = data
        xdata = data[0]
        ydata = data[1]
        ndata = data[2]
        self.theta1 = syp.Symbol("theta1")
        self.theta2 = syp.Symbol("theta2")
        self.x = syp.Symbol("x")
        self.y = syp.Symbol("y")
        self.params = [self.theta1, self.theta2]
        if len(xdata) != len(ndata) != len(ydata):
            logging.warning("xdata, ndata, and ydata are not of the same size")
        self.num_data = np.arange(len(xdata))

    def get_probabilities(self) -> syp.symbols:
        """
        :return: Probabilities
        :rtype: syp.symbols
        """
        pi = []
        for i in self.num_data:
            pi.append((syp.exp(self.theta1 + self.theta2 * self.data[0][i])) / (1 + syp.exp(
                self.theta1 + self.theta2 * self.data[0][i])))
        return pi

    def get_likelihoodFn(self) -> syp.symbols:
        """
        :return: Likelihood Function
        :rtype: syp.symbols
        """
        likelihood = 1
        for i in self.num_data:
            # Compute the likelihood function
            likelihood *= comb(self.data[2][i], self.data[1][i]) * (
                    self.get_probabilities()[i] ** self.data[1][i]) * (1 - self.get_probabilities()[i]) ** (
                                  self.data[2][i] - self.data[1][i])
        return likelihood

    def get_logLikelihoodFn(self) -> syp.symbols:
        """
        :return: The Log-likelihood Function
        :rtype: syp.symbols
        """
        lnl = syp.log(self.get_likelihoodFn())
        return lnl

    def get_score1(self) -> syp.symbols:
        """
        :return: Score Function 2
        :rtype: syp.symbols
        """
        score1 = syp.diff(self.get_logLikelihoodFn(), self.theta1)
        return score1

    def get_score2(self) -> syp.symbols:
        """

        :return: Score Function 2
        :rtype: syp.symbols
        """
        score2 = syp.diff(self.get_logLikelihoodFn(), self.theta2)
        return score2

    def get_infoMatrix(self) -> syp.Matrix:
        """
        :return: Information Matrix / Hessian Matrix
        :rtype: syp.Matrix
        """
        # Another way to get Hessian
        # info_matrix = (syp.derive_by_array(syp.derive_by_array(self.get_logLikelihoodFn(), self.params), self.params))
        info_matrix = syp.Matrix(syp.hessian(self.get_logLikelihoodFn(), self.params))
        return info_matrix

    def newtowns_method(self) -> []:
        initial = [0, 0]
        nxt = None
        for i in np.arange(self.iterations):
            print(i)
            # information matrix subbed in with values for theta1 and theta2, then converted to float for consistency
            # subed_info_matrix = np.vectorize(lambda z: z.subs({theta1: initial[0], theta2: initial[1]}))(
            #     info_matrix).astype(dtype=np.float64)
            subed_info_matrix = np.matrix(
                self.get_infoMatrix().subs(self.theta1, initial[0]).subs(self.theta2, initial[1]))
            scores_array = [self.get_score1().subs(self.theta1, initial[0]).subs(self.theta2, initial[1]),
                            self.get_score2().subs(self.theta1, initial[0]).subs(self.theta2, initial[1])]
            # scores_array_transpose = np.transpose(scores_array)
            nxt = initial - np.dot(np.linalg.inv(subed_info_matrix.astype(dtype=np.float64)), scores_array)
            # Convert matrix to array with two elements
            nxt = np.asarray(nxt).flatten()
            initial = nxt
            print(initial)
        return initial


if __name__ == '__main__':

    # xdata = [1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839]
    # ydata = [6, 13, 18, 28, 52, 53, 61, 60]
    # ndata = [59, 60, 62, 56, 63, 59, 62, 60]
    # data = [xdata, ydata, ndata]
    # data = sm.datasets.get_rdataset("Default", "ISLR")

    # Import Default dataset with pandas from downloaded csv file from R
    df = pd.read_csv(r'C:/Users/Mason/Desktop/Default.csv')
    df = df[["balance", "income"]]
    print(df)

    model = MLE(data=data, iterations=100)
    print(model.get_probabilities())
    print(model.newtowns_method())

    # xdata = [1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839]
    # ydata = [6, 13, 18, 28, 52, 53, 61, 60]
    # ndata = [59, 60, 62, 56, 63, 59, 62, 60]
    # data = [xdata, ydata, ndata]
    # theta1 = syp.Symbol("theta1")
    # theta2 = syp.Symbol("theta2")
    # x = syp.Symbol("x")
    # y = syp.Symbol("y")
    # params = [theta1, theta2]
    # # if xdata.size != ndata.size != ydata.size:
    # #     logging.warning("xdata, ndata, and ydata are not of the same size")
    # seq = np.arange(len(xdata))
    # likelihood = 1
    # pi = []
    # for i in seq:
    #     # Create the pi_i's
    #     pi.append((syp.exp(theta1 + theta2 * data[0][i])) / (1 + syp.exp(theta1 + theta2 * data[0][i])))
    # print("PI_i: " + str(pi))
    # for i in seq:
    #     # Compute the likelihood function
    #     likelihood *= comb(data[2][i], data[1][i]) * (pi[i] ** data[1][i]) * (1 - pi[i]) ** (
    #                 data[2][i] - data[1][i])
    # print("LIKELIHOOD FUNCTION: " + str(likelihood))
    # # print("DATA: " + str(data))
    # # Get the log-likelihood function
    # lnl = syp.ln(likelihood)
    # # Get the score functions
    # score1 = syp.diff(lnl, theta1)
    # score2 = syp.diff(lnl, theta2)
    # print("Score Function wrt THETA1: " + str(score1))
    # print("Score Function wrt THETA2: " + str(score2))
    # # info_matrix = np.matrix([[score1 ** 2, score1 * score2], [score2 * score1, score2 ** 2]])
    # # info_matrix = np.matrix([[lnl.diff(theta1).diff(theta2) for x in params] for y in params])
    # info_matrix = syp.Matrix(syp.hessian(lnl, params))
    # # info_matrix = np.matrix([[lnl.diff(params[0])], lnl.diff[params[1]]])
    # # print("INFO_MATRIX" + str(info_matrix))
    #
    # print("INFO_MATRIX_SHAPE: " + str(info_matrix.shape))
    # initial = [0, 0]
    # nxt = None
    # for i in np.arange(100):
    #     print(i)
    #     # information matrix subbed in with values for theta1 and theta2, then converted to float for consistency
    #     # subed_info_matrix = np.vectorize(lambda z: z.subs({theta1: initial[0], theta2: initial[1]}))(
    #     #     info_matrix).astype(dtype=np.float64)
    #     subed_info_matrix = np.matrix(info_matrix.subs(theta1, initial[0]).subs(theta2, initial[1]))
    #     print(subed_info_matrix)
    #     scores_array = [score1.subs(theta1, initial[0]).subs(theta2, initial[1]),
    #          score2.subs(theta1, initial[0]).subs(theta2, initial[1])]
    #     # scores_array_transpose = np.transpose(scores_array)
    #     nxt = initial + np.dot(np.linalg.inv(subed_info_matrix.astype(dtype = np.float64)), scores_array)
    #     # Convert matrix to array with two elements
    #     nxt = np.asarray(nxt).flatten()
    #     print(initial)
    #     initial = nxt
    # # MLE(pdf, params, data)
