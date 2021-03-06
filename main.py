# This file implements the method used in Statistical Inference by Dr. Francesco Javier Rubio.
from math import comb
import logging
import numpy as np
import pandas
import sympy as sym
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
        self.theta1 = sym.Symbol("theta1")
        self.theta2 = sym.Symbol("theta2")
        self.x = sym.Symbol("x")
        self.y = sym.Symbol("y")
        self.params = [self.theta1, self.theta2]
        if len(xdata) != len(ndata) != len(ydata):
            logging.warning("xdata, ndata, and ydata are not of the same size")
        self.num_data = np.arange(len(xdata))

    def get_probabilities(self) -> sym.symbols:
        """
        :return: Probabilities
        :rtype: sym.symbols
        """
        pi = []
        for i in self.num_data:
            pi.append((sym.exp(self.theta1 + self.theta2 * self.data[0][i])) / (1 + sym.exp(
                self.theta1 + self.theta2 * self.data[0][i])))
        return pi

    def get_likelihoodFn(self) -> sym.symbols:
        """
        :return: Likelihood Function
        :rtype: sym.symbols
        """
        likelihood = 1
        for i in self.num_data:
            # Compute the likelihood function
            likelihood *= comb(self.data[2][i], self.data[1][i]) * (
                    self.get_probabilities()[i] ** self.data[1][i]) * (1 - self.get_probabilities()[i]) ** (
                                  self.data[2][i] - self.data[1][i])
        return likelihood

    def get_logLikelihoodFn(self) -> sym.symbols:
        """
        :return: The Log-likelihood Function
        :rtype: sym.symbols
        """
        lnl = sym.log(self.get_likelihoodFn())
        return lnl

    def get_score1(self) -> sym.symbols:
        """
        :return: Score Function 2
        :rtype: sym.symbols
        """
        score1 = sym.diff(self.get_logLikelihoodFn(), self.theta1)
        return score1

    def get_score2(self) -> sym.symbols:
        """

        :return: Score Function 2
        :rtype: sym.symbols
        """
        score2 = sym.diff(self.get_logLikelihoodFn(), self.theta2)
        return score2

    def get_infoMatrix(self) -> sym.Matrix:
        """
        :return: Information Matrix / Hessian Matrix
        :rtype: sym.Matrix
        """
        # Another way to get Hessian
        # info_matrix = (sym.derive_by_array(sym.derive_by_array(self.get_logLikelihoodFn(), self.params), self.params))
        info_matrix = sym.Matrix(sym.hessian(self.get_logLikelihoodFn(), self.params))
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
        self.i = sym.Symbol('i', integer=True)
        self.iterations = iterations
        self.data = data
        self.thetas = [sym.Symbol("theta0")]
        self.symbols = []
        # -1 indicates not including the n data/instances
        # if len(data)
        for k in range(0, len(self.data.columns)-1):
            # thetas/parameters/weight
            self.thetas.append(sym.Symbol("theta{}".format(k+1)))
            # self.symbols.append(sym.Symbol("x{}".format(i)))
            self.symbols.append(sym.Indexed('x{}'.format(k), self.i))
        self.xSymbols = self.symbols[:]
        # instances
        self.symbols.append(sym.Indexed("n", self.i))
        # variable to predict
        self.symbols.append(sym.Indexed("y", self.i))
        # add array of just x variables (i.e. the predictor data) and add 1 to the 0th index while deleting y variable
        self.xSymbols.insert(0, 1)
        # get the number of rows/data
        self.terms = len(data.index)
        self.indexed_data = []
        for k in range(0, len(self.data.columns)):
            temp_data = data.columns[k]
            self.indexed_data.append(lambda index: temp_data[index])

    def get_probabilities(self) -> sym.symbols:
        """
        :return: Probabilities
        :rtype: sym.symbols
        """
        # pi = []
        #
        exponents = np.dot(self.thetas, self.xSymbols)
        #
        # x_sequence = sym.Sum(self.xSymbols)
        # pi.append((sym.exp(exponents)) / (1 + sym.exp(exponents)))


        init_pi = (sym.exp(exponents)) / (1 + sym.exp(exponents))
        # i = sym.Symbol('i', integer=True)
        # pi = sym.Indexed('pi', self.i)
        pi = init_pi
        # pi = lambda x: sym.Subs(init_pi.doit(), [init_pi.function.subs(self.xSymbols, j) for j in range(s.limits[0][1], s.limits[0][2] + 1)], x).doit()
        return pi

    def get_likelihoodFn(self) -> sym.symbols:
        """
        :return: Likelihood Function
        :rtype: sym.symbols
        """
        likelihood = 1
        # a_seq = [-1, 3, 23, 8]
        # n, r = sympy.symbols('n, r')
        # a_n = sympy.Function('a')(n)
        # terms = 4
        # short_expr = sympy.Sum(a_n * r ** n, (n, 0, terms - 1))
        # coeffed_short_expr = short_expr.doit().subs(
        #     (a_n.subs(n, i), a_seq[i]) for i in range(terms))  # 8*r**3 + 23*r**2 + 3*r - 1
        # func_short_expr = sympy.lambdify(r, coeffed_short_expr, 'numpy')

        # Compute the likelihood function with out the combination --- sym.functions.combinatorial.nC(self.symbols[len(self.symbols)-1], self.symbols[len(self.symbols)-2])
        likelihood = sym.Product((self.get_probabilities() ** self.symbols[len(self.symbols)-1]) * (1 - self.get_probabilities()) ** (
                              self.symbols[len(self.symbols)-1] - self.symbols[len(self.symbols)-2]), (self.i, 0, self.terms - 1))
        # likelihood *= sym.nC(self.symbols[len(self.symbols)], self.symbols[len(self.symbols)-1]) * (
        #         self.get_probabilities() ** self.symbols[len(self.symbols)]) * (1 - self.get_probabilities()) ** (
        #                       self.symbols[len(self.symbols)] - self.symbols[len(self.symbols)-1])
        return likelihood

    def get_logLikelihoodFn(self) -> sym.symbols:
        """
        :return: The Log-likelihood Function
        :rtype: sym.symbols
        """
        lnl = sym.log(self.get_likelihoodFn())
        return lnl

    def get_scores(self):
        scores = []
        for k in self.thetas:
            scores.append(sym.diff(self.get_logLikelihoodFn(), k))
        return scores

    def get_infoMatrix(self) -> sym.Matrix:
        """
        :return: Information Matrix / Hessian Matrix
        :rtype: sym.Matrix
        """
        # Another way to get Hessian
        # info_matrix = (sym.derive_by_array(sym.derive_by_array(self.get_logLikelihoodFn(), self.params), self.params))
        info_matrix = sym.Matrix(sym.hessian(self.get_logLikelihoodFn(), self.thetas))
        return info_matrix

    def newtowns_method(self) -> []:
        initial = [0, 0, 0]
        nxt = None
        for i in np.arange(self.iterations):
            print(i)
            # information matrix subbed in with values for theta1 and theta2, then converted to float for consistency
            # subed_info_matrix = np.vectorize(lambda z: z.subs({theta1: initial[0], theta2: initial[1]}))(
            #     info_matrix).astype(dtype=np.float64)
            # subbed_thetas = []
            # for k in range(len(self.thetas)):
            #     subbed_thetas.append(self.thetas[k]: initial[k])

            subed_info_matrix = np.matrix(
                self.get_infoMatrix().subs({self.thetas[k]: initial[k] for k in range(len(self.thetas)-1)}).replace(
                    self.symbols[j], self.indexed_data[j]) for j in range(len(self.symbols)-1))
            scores_array = []
            print(subed_info_matrix)
            for l in range(len(self.get_scores()) - 1):
                scores_array.append(self.get_scores()[l].subs({self.thetas[k]: initial[k] for k in range(len(self.thetas)-1)}).replace(
                    self.symbols[j], self.indexed_data[j]) for j in range(len(self.symbols)-1))
            # scores_array_transpose = np.transpose(scores_array)
            print(str(scores_array))
            nxt = initial - np.dot(np.linalg.inv(subed_info_matrix.astype(dtype=np.float64)), scores_array)
            # Convert matrix to array with two elements
            nxt = np.asarray(nxt).flatten()
            initial = nxt
            print(initial)
        return initial

# THIRD IMPLEMENTATION
class MLE_3:

    def __init__(self, data: pandas.DataFrame, iterations: int):
        self.i = sym.Symbol('i', integer=True)
        self.iterations = iterations
        self.data = data
        self.thetas = [sym.Symbol("theta0")]
        self.symbols = []
        # -1 indicates not including the n data/instances
        # if len(data)
        for k in range(0, len(self.data.columns)-1):
            # thetas/parameters/weight
            self.thetas.append(sym.Symbol("theta{}".format(k+1)))
            # self.symbols.append(sym.Symbol("x{}".format(i)))
            self.symbols.append(sym.Indexed('x{}'.format(k), self.i))
        self.xSymbols = self.symbols[:]
        # instances
        self.symbols.append(sym.Indexed("n", self.i))
        # variable to predict
        self.symbols.append(sym.Indexed("y", self.i))
        # add array of just x variables (i.e. the predictor data) and add 1 to the 0th index while deleting y variable
        self.xSymbols.insert(0, 1)
        # get the number of rows/data
        self.terms = len(data.index)
        self.indexed_data = []
        #
        for k in self.data.columns:
            temp_data = data[k]
            self.indexed_data.append(temp_data)

    def get_probabilities(self) -> sym.symbols:
        """
        :return: Probabilities
        :rtype: sym.symbols
        """
        exponents = np.dot(self.thetas, self.xSymbols)
        pi = (sym.exp(exponents)) / (1 + sym.exp(exponents))
        return pi

    def get_likelihoodFn(self) -> sym.symbols:
        """
        :return: Likelihood Function
        :rtype: sym.symbols
        """
        likelihood = sym.Product((self.get_probabilities() ** self.symbols[len(self.symbols)-1]) * (1 - self.get_probabilities()) ** (
                              self.symbols[len(self.symbols)-1] - self.symbols[len(self.symbols)-2]), (self.i, 0, self.terms - 1))
        return likelihood

    def get_logLikelihoodFn(self) -> sym.symbols:
        """
        :return: The Log-likelihood Function
        :rtype: sym.symbols
        """
        lnl = sym.log(self.get_likelihoodFn())
        return lnl

    def get_scores(self):
        # derive_by_array returns a gradient matrix for multivariable function
        scores = sym.derive_by_array(self.get_logLikelihoodFn(), self.thetas)
        # scores = []
        # for theta in self.thetas:
        #     scores.append(sym.diff(self.get_logLikelihoodFn(), theta))
        return scores

    def get_infoMatrix(self) -> sym.Matrix:
        """
        :return: Information Matrix / Hessian Matrix
        :rtype: sym.Matrix
        """
        # derive_by_array returns a Hessian matrix for multivariable function
        info_matrix = sym.Matrix(sym.hessian(self.get_logLikelihoodFn(), self.thetas))
        return info_matrix

    def newtowns_method(self) -> []:
        initial = np.zeros(3)
        for i in np.arange(self.iterations):
            print(i)


            # subed_info_matrix = np.matrix(
            #     self.get_infoMatrix().subs({self.thetas[k]: initial[k] for k in range(len(self.thetas)-1)}).replace(
            #         self.symbols[j], self.indexed_data[j]) for j in range(len(self.symbols)-1))
            # subed_info_matrix = self.get_infoMatrix().subs({self.thetas[k]: initial[k] for k in range(len(self.thetas))})
            # for j in range(len(self.symbols) - 1):
            #     replaced_info_matrix = subed_info_matrix.replace(self.symbols[j], self.indexed_data[j])


            # Substitutes the thetas with initial vector
            subed_info_matrix = sym.Matrix(self.get_infoMatrix().subs(zip(self.thetas, initial))).doit().subs([(self.symbols[j], self.indexed_data[j]) for j in range(len(self.symbols)-1)])
            print(str(subed_info_matrix))
            # Substitutes the thetas with initial vector
            scores_array = self.get_scores().subs(zip(self.thetas, initial))
            # scores_array_transpose = np.transpose(scores_array)
            print(str(scores_array))
            nxt = initial - np.dot(np.linalg.inv(subed_info_matrix.astype(dtype=np.float64)), scores_array)
            # Convert matrix to array with n elements
            nxt = np.asarray(nxt).flatten()
            # set the initial thetas as the next thetas
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
    # print(df)
    df["instances"] = np.ones(len(df.index))
    df = df[["income", "default", "instances"]]
    print(df)
    # print(df["income"].dtype)
    model = MLE_3(data=df, iterations=100)
    print(model.xSymbols)
    print(model.symbols)
    print(model.thetas)
    print("INDEXED DATA 0: " + str(model.indexed_data[0]))
    print("PROBABILITIES: " + str(model.get_probabilities()))
    print("LIKELIHOOD: " + str(model.get_likelihoodFn()))
    print("LOG - LIKELIHOOD: " + str(model.get_logLikelihoodFn()))
    print("SCORES: " + str(model.get_scores()))
    # print("INFORMATION MATRIX: " + str(model.get_infoMatrix()))
    print("NEWTONS METHOD: " + str(model.newtowns_method()))

    # print(model.newtowns_method())