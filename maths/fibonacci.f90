!> Program to print the nth term of the Fibonacci series.
!> Has the iterative and recursive implementations.
!!
! To run the program using the gnu compiler, run the following
! gfortran -o fibonacci fibonacci.f90
! ./fibonacci
program fibonacci
    ! i8 -> double precision integers
    ! error_unit -> used for writing error statements
    use, intrinsic :: iso_fortran_env, only : i8=>int64, error_unit
    implicit none ! an artifact from Fortran 77
    integer :: n

    print *, 'Enter a number: '
    read *, n
    if (n <=0) then
        write(error_unit, *) 'number must be a positive integer'
            stop 1
    end if
    print *, 'The fibonacci number for the specified position is'
    print *, 'Recursive solution: ', fib_rec(n)
    print *, 'Iterative solution: ', fib_itr(n)

    contains
    ! Recursive solution
    recursive function fib_rec(n) result(f)
        integer, intent(in), value :: n
        integer(i8), dimension(n) :: f
        if (n<=1) then
            f = 1
            return
        else
            f = fib_rec(n-1) + fib_rec(n-2)
        end if
    end function fib_rec

    ! Iterative solution
    function fib_itr(n) result(f) ! iterative
        integer,intent(in) :: n
        integer(i8) :: f, tmp, f_1
        integer :: i
        f_1 = 1 ! Initialization in separate line is necessary. Checkout following discussion
        f = f_1   ! https://fortran-lang.discourse.group/t/fortran-function-remembers-values-newbie-help/2018

        do i = 2, n
            tmp=f
            f = f + f_1
            f_1 = tmp
        end do
    end function fib_itr
end program fibonacci