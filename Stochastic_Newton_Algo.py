import numpy as np

def stochastic_newton_algorithm(d,n = 10):
    init_theta = np.zeros(d+1)
    S_init = np.identity(d+1)
    for i in range(n):
        if i > 1:
            S_init



if __name__ == '__main__':
    stochastic_newton_algorithm(5, 10)