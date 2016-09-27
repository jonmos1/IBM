module GLOBALMODULEVARS
!MODULE HEADER
!
!
!
!===========================================================

    !counter variables
    integer :: i, j, k  ! nodal counters in x,y and z-dir
    integer :: c, cc    ! counting in general
    integer :: d, dd    ! directional counter (1,2 and 3)

    !Parameters and constants
    real, parameter :: pi = 4*atan(1.0)

    real, parameter, dimension(1,3) :: u_zero(1,3) = 0
    real, parameter                 :: p_zero   = 0

    !Logicals
    logical ::  flag_exit   !flag for exiting programs if fatal error occurs


!===========================================================



end module GLOBALMODULEVARS
