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


class MLE_2:

    def __init__(self, data: pandas.DataFrame, iterations: int):
        self.i = syp.Symbol('i', integer=True)
        self.iterations = iterations
        self.data = data
        self.thetas = [syp.Symbol("theta0")]
        self.symbols = []
        # -1 indicates not including the n data/instances
        # if len(data)
        for k in range(0, len(self.data.columns)-1):
            # thetas/parameters/weights
            self.thetas.append(syp.Symbol("theta{}".format(k+1)))
            # self.symbols.append(syp.Symbol("x{}".format(i)))
            self.symbols.append(syp.Indexed('x', self.i))
        self.xSymbols = self.symbols[:]
        # instances
        self.symbols.append(syp.Indexed("n", self.i))
        # variable to predict
        self.symbols.append(syp.Indexed("y"), self.i)
        # add array of just x variables (i.e. the predictor data) and add 1 to the 0th index while deleting y variable
        self.xSymbols.insert(0, 1)

    def get_probabilities(self) -> syp.symbols:
        """
        :return: Probabilities
        :rtype: syp.symbols
        """
        # pi = []
        #
        exponents = np.dot(self.thetas, self.xSymbols)
        #
        # x_sequence = syp.Sum(self.xSymbols)
        # pi.append((syp.exp(exponents)) / (1 + syp.exp(exponents)))


        init_pi = (syp.exp(exponents)) / (1 + syp.exp(exponents))
        # i = syp.Symbol('i', integer=True)
        pi = syp.Indexed('pi', self.i)

        return pi

    def get_likelihoodFn(self) -> syp.symbols:
        """
        :return: Likelihood Function
        :rtype: syp.symbols
        """
        likelihood = 1
        a_seq = [-1, 3, 23, 8]
        n, r = sympy.symbols('n, r')
        a_n = sympy.Function('a')(n)
        terms = 4
        short_expr = sympy.Sum(a_n * r ** n, (n, 0, terms - 1))
        coeffed_short_expr = short_expr.doit().subs(
            (a_n.subs(n, i), a_seq[i]) for i in range(terms))  # 8*r**3 + 23*r**2 + 3*r - 1
        func_short_expr = sympy.lambdify(r, coeffed_short_expr, 'numpy')

        # Compute the likelihood function
        likelihood - syp.Product(a_n * r ** n, (n, 0, terms - 1))
        likelihood *= syp.nC(self.symbols[len(self.symbols)], self.symbols[len(self.symbols)-1]) * (
                self.get_probabilities() ** self.symbols[len(self.symbols)]) * (1 - self.get_probabilities()) ** (
                              self.symbols[len(self.symbols)] - self.symbols[len(self.symbols)-1])
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

    df['default'] = df['default'].map({'Yes':1, 'No':0})
    print(df)
    df["instances"] = np.ones(len(df.index))
    df = df[["income", "default", "instances"]]
    print(df)
    # print(df["income"].dtype)
    model = MLE_2(data=df, iterations=100)
    print(model.xSymbols)
    print(model.symbols)
    print(model.thetas)
    print("PROBABILITIES: " + str(model.get_probabilities()))
    print("LIKELIHOOD: " + str(model.get_likelihoodFn()))
    # print(model.newtowns_method())