import numpy as np
import pandas as pd
import sympy as sym

def stochastic_newton_algorithm(dataframe: pd.DataFrame,n = 10):
    d = len(dataframe.columns)
    thetas_init = np.ones(d+1)
    S_inv_init = np.identity(d+1)
    # thetas_init = [sym.Symbol("theta0")]
    vectors = []

    for k in range(0, d):
        # thetas/parameters/weights
        # thetas_init.append(sym.Symbol("theta{}".format(k + 1)))
        # self.symbols.append(sym.Symbol("x{}".format(i)))
        # vectors.append(sym.Symbol('x{}'.format(k)))
        number_column = dataframe.loc[:, k]
        numbers = number_column.values
        vectors.append(numbers)
    xVectors = vectors[:]
    xVectors.insert(0, 1)
    # vectors.append(sym.Symbol("y"))
    number_column = dataframe.loc[:, d]
    ynumbers = number_column.values
    vectors.append(ynumbers)
    print(xVectors)
    S_inv = 0
    for i in range(1, n):
        if i > 1:
            None
        else:
            exponents = np.dot(thetas_init, xVectors)
            print(exponents)
            pi = (sym.exp(exponents)) / (1 + sym.exp(exponents))
            a = pi*(1-pi)
            S_inv = S_inv_init - a*(1/(1+a*np.dot(np.dot(xVectors, S_inv_init),xVectors)))*(np.dot(np.dot(np.dot(S_inv_init, xVectors), xVectors), S_inv_init))
            thetas = thetas_init + np.dot(np.dot(S_inv, xVectors), vectors[len(vectors)-1] - pi)
            print(thetas)


if __name__ == '__main__':
    xdata = [1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839]
    ydata = [6, 13, 18, 28, 52, 53, 61, 60]
    data = {'xdata': xdata,
            'ydata': ydata}
    df = pd.DataFrame(data)
    print(df)
    stochastic_newton_algorithm(df, 10)