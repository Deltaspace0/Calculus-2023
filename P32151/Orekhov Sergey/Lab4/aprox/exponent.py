import math
from aprox.dop import ln
from gauss import gauss


class Exponent:
    name = "Exponent"
    x_array = []
    y_array = []
    phi_array = []
    n = 0
    dm = 0
    sko = 0

    sum_xi = 0
    sum_xi2 = 0

    sum_yi = 0
    sum_xi_yi = 0

    a = 0
    b = 0

    def __init__(self, X, Y, N):
        self.x_array, self.y_array = X.copy(), Y.copy()
        self.n = N

    def calculate_sums(self):
        for i in range(self.n):
            self.sum_xi += self.x_array[i]
            self.sum_xi2 += self.x_array[i] ** 2

            self.sum_yi += ln(self.y_array[i])
            self.sum_xi_yi += self.x_array[i] * ln(self.y_array[i])

    def calculate_coeff(self):
        matrix = [[self.sum_xi2, self.sum_xi, self.sum_xi_yi], [self.sum_xi, self.n, self.sum_yi]]
        gauss_answer = gauss(matrix)
        self.a, self.b = math.e ** gauss_answer[1], gauss_answer[0]

    def func(self, x):
        return self.a * math.e ** (x * self.b)

    def func_string(self):
        return str(self.a) + " * e ** (x * " + str(self.b) + ")"
