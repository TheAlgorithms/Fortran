!> Example program to use the Fibonacci module
!> Prints the nth Fibonacci number using both recursive and iterative implementations.

program example_fibonacci
    use fibonacci_module
    implicit none
    integer :: n

    print *, 'Enter a number: '
    read *, n
    if (n <= 0) then
        print *, 'Number must be a positive integer.'
        stop 1
    end if

    print *, 'The Fibonacci number for the specified position is:'
    print *, 'Recursive solution: ', fib_rec(n)
    print *, 'Iterative solution: ', fib_itr(n)

end program example_fibonacci
