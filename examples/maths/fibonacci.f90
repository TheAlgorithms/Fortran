!> Example program to use the Fibonacci module
!> Prints the nth Fibonacci number using both recursive and iterative implementations.

program example_fibonacci
    use fibonacci_module
    implicit none
    integer :: n

    n = 7

    print *, 'The Fibonacci number for the position', n, ' is:'
    print *, 'Recursive solution: ', fib_rec(n)
    print *, 'Iterative solution: ', fib_itr(n)

end program example_fibonacci
