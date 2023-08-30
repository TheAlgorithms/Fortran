!> Bubble Sort
!!
!! This is a simple example for iterative bubble sort algorithm in Fortran.

program bubble_sort_program
implicit none

    real :: array(5)

    !! random_number subroutine fills the argument with random numbers.
    call random_number(array)

    print*, "Before:", array

    !! Bubble sort subroutine call.
    call bubble_sort(array)

    print*, "After:", array

contains
    
    !! This subroutine sorts the collection using bubble sort.
    subroutine bubble_sort (collection)
        real, dimension(:), intent(inout) :: collection !! A collection for elements of type real.

        integer :: i, j, collection_size
        real :: temp
        logical :: swapped

        !! Size function returns the size of an array in given dimension.
        collection_size = size(collection) 

        do j = collection_size-1, 1, -1

            swapped = .false.

            do i = 1, j
                if (collection(i) .gt. collection(i+1)) then
                    !! If collection is out of order in [i, i+1] region,
                    !! swap these values using a temp variable as follows.

                    temp = collection(i)
                    collection(i) = collection(i+1)
                    collection(i+1) = temp

                    swapped = .true. !! Swapped flag is set to true.
                end if
            end do

            !! Stop iterating if collection is sorted.
            if (.not. swapped) exit
        end do

    end subroutine bubble_sort

end program bubble_sort_program