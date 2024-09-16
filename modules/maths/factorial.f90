!> Factorial Function
!! https://en.wikipedia.org/wiki/Factorial
!! The program takes an integer input and returns the factorial of that number.
!! If the input is less than 1, the function returns 1.
program factorial_program
    implicit none

    Print*, factorial(5)
    Print*, recursive_factorial(5)

contains
    !! This function calculates the factorial of a given number using a loop.
    function factorial(number) result(factorial_number)
        integer, intent(in) :: number               !! Number to calculate factorial of
        integer             :: factorial_number     !! Resulting factorial

        integer :: counter
        counter = number

        factorial_number = 1
        do while (counter > 1)
            factorial_number = factorial_number * counter
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
            factorial_number = number * recursive_factorial(number - 1)
        end if

    end function recursive_factorial
end program factorial_program
