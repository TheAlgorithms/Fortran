!> Factorial Example Program
!! This program demonstrates the use of the factorial functions
!! defined in the `factorial_module`. It calculates and prints
!! the factorial of a number using both iterative and recursive methods.

program factorial_program
    use factorial_module
    implicit none

    Print*, factorial(5)
    Print*, recursive_factorial(5)

end program factorial_program
