package Method;

import IO.InputSet;
import IO.Result;
import Model.Equation;
import Model.Method;

public class TrapezoidMethod implements Method {
    private String name = "Trapezoid Method";
    @Override
    public Result execute(Equation equation,
                          Double left,
                          Double right,
                          Integer n,
                          Double accuracy) {
        Integer numOfStep = numOfStep(equation, left, right ,accuracy);
        return new Result(
                calculate(equation, left, right, n),
                numOfStep,
                error(equation, left, right, n));
    }

    @Override
    public Double error(Equation equation, Double left, Double right,  Integer n) {
        Double new_intergral = calculate(equation, left, right, n);
        Double old_intergral = calculate(equation, left, right, n * 2);
        return Math.abs((new_intergral - old_intergral) / 7);
    }
    @Override
    public Integer numOfStep(Equation equation,Double left, Double right, Double Accuracy) {
        Integer res = 1;
        Double new_intergral = calculate(equation, left, right, res);
        Double old_intergral = calculate(equation, left, right, res * 2);
        while(Math.abs(new_intergral - old_intergral) >= Accuracy && res <= 10000000) {
            res *= 2;
            new_intergral = calculate(equation, left, right, res);
            old_intergral = calculate(equation, left, right, res * 2);
        }

        return res * 2;
    }

    @Override
    public Double calculate(Equation equation, Double left, Double right, Integer n) {
        Double h = (right- left) / n;
        Double res = (equation.valueAt(left) + equation.valueAt(right)) / 2;
        for(int i = 1; i < n; ++i) {
            res += equation.valueAt(left + i * h) * h;
        }

        return res;
    }

    @Override
    public String toString() {
        return name;
    }
}
