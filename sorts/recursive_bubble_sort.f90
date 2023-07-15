program recursive_bubble_sort_program
implicit none

    real :: array(5)

    call random_number(array)

    print*, "Before:", array

    call recursive_bubble_sort(array, size(array))

    print*, "After:", array

contains
    
    recursive subroutine recursive_bubble_sort(collection, collection_size)
        real, dimension(:), intent(inout) :: collection
        integer, intent(in) :: collection_size

        integer :: i
        real :: temp

        do i = 1, collection_size
            if (collection(i) .gt. collection(i+1)) then
                temp = collection(i)
                collection(i) = collection(i+1)
                collection(i+1) = temp
            end if
        end do

        if (collection_size .gt. 2) then
            call recursive_bubble_sort(collection, collection_size - 1)
        end if

    end subroutine recursive_bubble_sort

end program recursive_bubble_sort_program

