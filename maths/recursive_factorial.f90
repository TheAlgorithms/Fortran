!> Recursive Factorial Function
!! https://en.wikipedia.org/wiki/Factorial
!! This program calculates the factorial of a given number using a recursive function.
!! The program takes an integer input and returns the factorial of that number.
!! If the input is less than 1, the function returns 1.
program recursive_factorial_program
    implicit none

    Print*, recursive_factorial(5)

contains
    recursive function recursive_factorial(number) result(factorial_number)
        integer, intent(in) :: number               !! Number to calculate factorial of
        integer             :: factorial_number     !! Resulting factorial

        if (number .lt. 1) then
            factorial_number = 1
        else
            factorial_number = number * recursive_factorial(number - 1)
        end if

    end function recursive_factorial
end program recursive_factorial_program
