!> Recursive Bubble Sort
!!
!! This is a simple example for recursive bubble sort algorithm in Fortran.

program recursive_bubble_sort_program
implicit none

    real :: array(5)

    !! random_number subroutine fills the argument with random numbers.
    call random_number(array)

    print*, "Before:", array

    !! Bubble sort subroutine call.
    call recursive_bubble_sort(array, size(array))

    print*, "After:", array

contains

    !! This subroutine sorts the collection recursively using bubble sort.
    !! Recursive keyword at the start declares that the function is recursive.
    recursive subroutine recursive_bubble_sort(collection, collection_size) 
        real, dimension(:), intent(inout) :: collection !! A collection for elements of type real
        integer, intent(in) :: collection_size          !! Collection's size.

        integer :: i
        real :: temp

        do i = 1, collection_size
            if (collection(i) .gt. collection(i+1)) then
                !! If collection is out of order in [i, i+1] region,
                !! swap these values using a temp variable as follows.

                temp = collection(i)
                collection(i) = collection(i+1)
                collection(i+1) = temp
            end if
        end do

        if (collection_size .gt. 2) then
            !! If collection's size is greater then 2, 
            !! this subroutine recursively calls itself with (collection_size - 1) for size argument.
            call recursive_bubble_sort(collection, collection_size - 1)
        end if

    end subroutine recursive_bubble_sort

end program recursive_bubble_sort_program

