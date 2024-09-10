!> Test program for the Gnome Sort algorithm

!! This program demonstrates the use of the gnome_sort_module by sorting an array of integers.

program test_gnome_sort
    use gnome_sort_module
    implicit none
    integer, dimension(10) :: array      ! Test array
    integer :: n, i

    ! Initialize the test array
    array = (/ -5, 2, 9, 1, 5, 6, -7, 8, 15, -20 /)
    n = size(array)

    ! Call gnome_sort from the module to sort the array
    call gnome_sort(array)

    print *, "Sorted array:"
    do i = 1, n
        print *, array(i)
    end do
end program test_gnome_sort