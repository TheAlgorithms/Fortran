program bubble_sort_program
implicit none

    real :: array(5)

    call random_number(array)

    print*, "Before:", array

    call bubble_sort(array)

    print*, "After:", array

contains
    
    subroutine bubble_sort(collection)
        real, dimension(:), intent(inout) :: collection

        integer :: i, j, collection_size
        real :: temp
        logical :: swapped

        collection_size = size(collection)

        do j = collection_size-1, 1, -1

            swapped = .false.

            do i = 1, j
                if (collection(i) .gt. collection(i+1)) then
                    temp = collection(i)
                    collection(i) = collection(i+1)
                    collection(i+1) = temp

                    swapped = .true.
                end if
            end do
            if (.not. swapped) exit
        end do

    end subroutine bubble_sort

end program bubble_sort_program

