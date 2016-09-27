module INPUT
!MODULE HEADER
!TODO(JON): Take away all parsing, and let this be done by the body and Domain subroutines contained in the modules of the same name
!
!
!===========================================================
    use GLOBALMODULEVARS
    use CHECKS
    use MESSAGE
    implicit none

    !dummy variables:
    integer, dimension(:,:) :: x_N
    real, dimension(:,:)    :: x_min, x_max
    real, dimension(:,:)    :: dx

    !internal variables:
    logical :: flag_2D
    integer :: x1_N, x2_N, x3_N     !Number of nodes in each direction
    real    :: x1_min, x1_Max       !Min/max bounds in 1-direction
    real    :: x2_min, x2_Max       !Min/max bounds in 2-direction
    real    :: x3_min, x3_Max       !Min/max bounds in 3-direction


    !TODO(JON): generate domain automatically based on the characteristic size of the body to study (3D, 10D, etc...)
    !TODO(JON): 3D-grid generation is only showing a surface mesh in tecplot (that is  -mesh only showed on the surface of the cube..
    !DOMAIN INPUT:________________________________________________
    x1_N = 10       !Number of computational points in X-direction
    x2_N = 10       !Number of computational points in Y-direction
    x3_N = 1        !Number of computational points in Z-direction

    x1_min = -10   !lower domain bound in X-direction
    x1_max = 10    !upper domain bound in X-direction

    x2_min = -10   !lower domain bound in Y-direction
    x2_max = 10    !upper domain bound in Y-direction

    !TODO(JON): Make checks so that a 2D-problem will be redefined into XY-plane. For now, I'll just keep the problem 2D by z-direction equal to zero
    x3_min = 0      !lower domain bound in Z-direction
    x3_max = 0      !upper domain bound in Z-direction




    subroutine InputBody(b,flag_2D)
        !TODO(JON): Only for 2D bodies
        !TODO(JON): only for cylinder yet - Implement foil as well
        !BODIES DEFINED COUNTERCLOCKWISELY
    logical :: flag_2D
    integer, parameter :: is_2D = 1

    real, dimension(1,3) :: b_xc    !COG-coordinates and
    real                 :: b_r     !radius of body
    real :: dTheta, theta           !Angular increment and actual angle
    integer :: b_N                  !Number of defining points on body
    real, allocatable    :: b(:,:,:)  !Body matrix (automatic dimension in x-dir

    !BODY_INPUT:__________________________________________________
    !Cylinder dimensions:
    b_xc(1,:) = (/ 0, 0, 0/)
    b_r  = 7
    !Body-array:
    b_N = 8

    !Foil dimensions:



    !PARSING INPUT INTO VECTOR FORM:______________________________
    if(.NOT.flag_2D) then
        print*, '3-Dimensional problems not supported yet. Redefine your input'
        call MessageExit()
    end if


    !allocate: (number of body points , number of dimensional axes , 1(2D or 2(3d) )
    allocate(b(b_N,3,is_2D))

    !Calculate:
    dTheta = 2*pi/(b_N-1) !angle-increment (uses b_N from zero to zero, thus b_N-1 in denominator)

    do c = 1,b_N
        theta = (c-1)*dTheta
        b(c,1,is_2D) = b_xc(1,1) + b_r*cos(theta)     !(b_xc +...)
        b(c,2,is_2D) = b_xc(1,2) + b_r*sin(theta)     !(b_yc +...)
        !(c,2,is_2D) = b_xc(1,2) + 3*b_r*sin(theta)    !ELLIPSE
        b(c,3,is_2D) = 0                              !(b_zc +...)
    end do



    end subroutine InputBody
!====================================================































!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
!SUBROUTINE ARCHIVE FOR THIS MODULE_____________________________________________________________!
!                                                                                               !
!_______________________________________________________________________________________________!




end module
