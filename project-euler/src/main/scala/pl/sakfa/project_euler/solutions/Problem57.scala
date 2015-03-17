package pl.sakfa.project_euler.solutions

import org.apache.commons.math3.fraction.BigFraction;

object Problem57 extends App {
    //this will make BigFraction work with normal math operators, out of laziness I only implemented those I actually used
    implicit class PimpMyInt(i: Int) {
        def +(that: BigFraction) = that.add(1);
        def /(that: BigFraction) = new BigFraction(i,1).divide(that);
    }

    var current = new BigFraction(3, 2);
    var iteration = 1;
    var counter = 0;

    while (iteration <= 1000) {
        if (current.getNumerator.toString.length > current.getDenominator.toString.length) {
            counter += 1;
        }

        val newDenominator = current.getNumerator.add(current.getDenominator);
        current = new BigFraction(current.getDenominator.add(newDenominator), newDenominator )
        current.reduce
        iteration += 1;
    }

    println(counter);
}