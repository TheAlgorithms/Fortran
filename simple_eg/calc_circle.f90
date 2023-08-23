!> Calculating A Circle's Area
!!
!! This is a simple program that calcuates circles' areas

program calculate_circle_area

    real :: a
    a = get_area(5.0)

    Print *, "The area of a circle that has a radius of 5.0 is"
    Print *, a

end program calculate_circle_area

!! this function computes the area of a circle with radius r  
function get_area (r)  !! There is a space between the func name and the parenthesis

!! function result     
implicit none      

    !! dummy arguments        
    real :: get_area   
   
    !! local variables 
    real :: r     
    real :: pi

    pi = 4 * atan (1.0)     
    get_area = pi * r**2  
   
end function get_area