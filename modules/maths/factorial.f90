!> Factorial Module
!! This module implements factorial calculation functions.
!! https://en.wikipedia.org/wiki/Factorial
!! The module contains two functions: one for calculating factorial iteratively
!! and another one recursively.

module factorial_module
    implicit none

contains

    !! This function calculates the factorial of a given number using a loop.
    function factorial(number) result(factorial_number)
        integer, intent(in) :: number               !! Number to calculate factorial of
        integer             :: factorial_number     !! Resulting factorial

        integer :: counter
        counter = number

        factorial_number = 1
        do while (counter > 1)
            factorial_number = factorial_number*counter
            counter = counter - 1
        end do

    end function factorial

    !! This function calculates the factorial of a given number using a recursive function.
    recursive function recursive_factorial(number) result(factorial_number)
        integer, intent(in) :: number               !! Number to calculate factorial of
        integer             :: factorial_number     !! Resulting factorial

        if (number .lt. 1) then
            factorial_number = 1
        else
            factorial_number = number*recursive_factorial(number - 1)
        end if

    end function recursive_factorial

end module factorial_module
