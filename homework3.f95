program homework3
    ! Nicholas Maynard
    ! CSI 501
    ! Homework 3
    ! 02/09/23

    ! This program is meant to show familiarity with arrays and begin more complex mathematics
    ! within Fortran land.

    ! Clean up memory
    implicit none
    
    ! Initialize values
    integer :: i, n
    real :: Mu, Q = 0.0
    real, allocatable :: A(:)
    ! Open the file reader
    open(unit=13, file='Numbers1.txt')

    ! Read the first line of the file
    read(13,*) n
    
    ! Allocate memory for the array
    allocate(A(n))

    ! Add data from file to array
    do i=1,n
        read(13,*) A(i)
        if ( i < 2 ) then
            Mu = A(i)
        else if ( i >= 2) then
            Mu = Mu + (A(i) - Mu) / float(i)
        end if
    enddo
    Q = 0.0

    ! Print our values
    print*, 'The Average is: ', Mu
    print*, 'The Standard Deviation is: ', Q
    
    ! Close the file reader
    close(13)

    ! Deallocate the memory used
    deallocate(A)

end program homework3