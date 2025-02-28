/*
Define the following DSL in Scala without using the Parser Combinators:
    Define a simple calculator DSL that parses (and evaluates) scripts containing several expressions one each line.
        The admitted operators are: +, -, *, /, ^, sqrt, sin, cos, tan and parentheses with the traditional meanings;
        all of them can work on integers and reals value.
        The result for the script evaluation is a printout of the results of the single values.
    Extend the DSL developed in the first point and add it the support for variables with assignments.
        Variables are 1-character long strings that need to be assigned to an expression
            and later can be used into other expressions; a second assignment will replace the value
            and an attempt to use an uninitialized variable will raise an error.
        Basically, the assignment has the traditional behavior, i.e.,
            it is evaluated from right to left whereas all the other expressions are evaluated from left to right,
            therefore A = B = 5+7 is a correct and admissible instruction in this DSL.
        Variables are untyped or better all of them are numbers.
 */
