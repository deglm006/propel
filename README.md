# The Problem

Evolving a program to solve a derivative. Specifically by applying the power rule.

## Explanation

To start with, we chose this problem because it was an interesting area to work with evolution.  There is also the ability to add complexity as necessary to the problem or simplify it to generate results.  We did not get to tackling solving a derivative given as a string, but that was a future goal.  At the moment we settled on solving a single term derivative with a specific term stack.  Then we proceeded to work with polynomials using the same term stack structure.  This was less successful as we believe our error function is working incorrectly. Also, extending the problem to polynomials in general significantly increases the complexity of the problem, so even if it is theoretically able to be solved with the given setup in practice it might be somewhat infeasible in practice. We didn't give it any looping to work with, and while it shouldn't be technically necessary to have to solve the problem with polynomials of at most a given degree, it could probably make doing so easier.

## Problem Setup

Our problem requires a term to be looked at, this consists of an x, one (1) coefficient and one (1) exponent.  As the x is only relevant when writing the term out and not explicitly necessary for taking the derivative we have chosen to ignore it in these circumstances.  This is the basis of how we set up the evolution.  From this set of contingencies, we created a new stack for the program to work with.  It's simply called the term stack.  The term stack is a vector consisting of two numbers, the first to be the coefficient, and the second to be the exponent.  There are only two ways to interact with this stack at the moment being to put the two numbers onto the integer stack or to put the top two numbers into a term and place it on the term stack.

Besides the term stack we allowed the program to have access to all basic integer functions as well as exec dupe and integers 1 and 0.

Our error function was setup in a way to penalize an incorrect coefficient stronger than an incorrect exponent as the coefficient requires multiplication of two values which has the possibility to result in a wide variety of coefficient.  This is done by seeing if the coefficient is a multiple of either the starting coefficient or 1 of the starting exponent.  If it is not a flat penalty is given along with a general distance from correct value deduction.  In contrast, the exponent only needs to be subtracted to get the right value and as such we judge the distance alone.  Our intention is to try and give the program the information is needs to know that it is required to multiply one value, but only subtract with the other.

## Results

Our results of 10 test runs can be found in the single-term folder of this repository.  There were 10 successful attempts at evolving a solution.  The solutions themselves were not always efficient and that is due to some extraneous division and addition functions being used by the program.  We debated over including them or excluding them and decided we would let it have access to all basic operations, as stated prior.  We ran these tests with a population size of 200.

## Changes or Alterations

We had a pretty solid idea of how we wanted to set up our program coming into the problem and as such there was not much alteration to the method of doing things.  The only significant changes we made were to increase efficiency one of which we eventually reverted.
The first was to remove extraneous integer operations.  This greatly increased the ability for the program to solve the given test cases and we felt it would be more interesting to see how it worked with all of them given.  

The second major change involved the way we created our test case.  It was initially a loop that each generation would need to recreate and we realized this was extremely inefficient and did the loop once beforehand and then supplied the values and the correct values which didn't overall affect the accuracy, but increased the speed that it can run each generation.

The other major change that stuck was to separate the error values of the coefficient and the exponent.  This helped with the ability for the program to generate a solution.  We did this in an attempt to better separate the needed functions to get the correct value for the coefficient and the exponent independently.  
