!An Immersed Boundary Method module for CFD-applications
!By Jon Coll Mossige
!V.0.0
!31.08.2016

!WISHED PROGRAM STRUCTURE:
!
!INPUT
!   call Input*             !take in user input (* stands for domain, body, etc.)
!
!INITIATION
!   call Cecksinit          !perform initial checks to state the kind of problem (2D,3D) and detect errors in input
!   call FilesInit          !initiate file with title and number of variables etc.
!
!   call GridGen            !generate the grid and write to file
!   call BodyGen            !generate the initial body configuration and write to file
!
!   call BCsDomain          !Implement Domain Boundary Conditions
!   call ICsDomain          !Implement Domain Initial Conditions
!
!
!CALCULATION INITIATION
!   call Solver*            !Solve the flow field (first iteration, the flow field is solved without body present) *(stands for different solver subroutines)
!
!   call IBMInterSect       !Detection of intersection points between body and grid
!   call IBMImplementBCs    !Implement body BCs by IBM method - includes the manipulation of the flow field
!
!ITERATIVE CALCULATION
!   call Solver*
!
!   call IBMInterSect
!   call IBMImplementBCs
!
!   call BodyForce          !Calculates forces on the body from the fluid
!(*)call BodyResponse       !Calculates the response of the body from the forces
!       (*)-RESULTS IN NEW BODY CONFIGURATION that will be used for the next time step
!
!
!
!Improve performance:
!   define when to use double and single precision
!   reduce number of open-close statements
!   make information flow through fewest possible nodes
!   use preallocated variables
!
!Improve readability
!   better heading texts
!   calculate in steps (tradeoff with performance?)
!
!
!
program main
! TODO (JON): Use safe-division in IBMIntersect_2D (see http://wiki.seas.harvard.edu/geos-chem/index.php/Floating_point_math_issues)
! TODO (JON): Use real*8 by default? ( if so, it must be changed in code) Needed for all values or not?
! TODO (JON): Improve file-handling with more generic subroutines
! TODO (JON): GridGen - also make points for cell pressure centers and for mark each line with computational number
! TODO (JON): BodyGen - Makr body points with indice-number
! TODO (JON): in GridGen and FilesZoneInit - format labeled 910 - make generic choice of integer length
! TODO (JON): SEPARATE 2D and 3D modules - if 2D mesh is detected, then use flag_2D=.true. and use only 2D-modules - Also make all matrices 2D with reSIZE
!
!
! CLOSED 23.09.16 (JON): Tecplot - status: not valid license - response from orakel pending
! CLOSED 23.09.16 (JON): In the TestGenGrid Subroutine - Use block instead of point? Hong says no
! CLOSED 23.09.16 (JON): Make vectors of all 3D variables such as dx,dy,dz and x_N,y_n,z_N to make use of loops in programming
!________________________________________________________________________


  !MODULES---------------------------------------------------------------
  !----------------------------------------------------------------------
  !external modules   :

  !--------------------
  !self-made modules  :
  use FILES             !contains file-handling subroutines
  use INPUT             !contains the input of the problems
  use GRID              !contains subroutines for generating the grid
  use BODY              !contains subroutines for generating the body
  use SOLVER            !contains subroutines and functions for numerical solution
  use IBM               !contains subroutines for implementation of the IBM method
  use CHECKS            !contains subroutines for different checks of the program
  use MESSAGE
  implicit none


  !SYSTEM VARS-----------------------------------------------------------
  !----------------------------------------------------------------------
  integer :: A

  !LOGICAL VARS----------------------------------------------------------
  !----------------------------------------------------------------------
  logical :: flag_2D                                                     !a flag that states the dimensionality of the problem

  !----------------------------------------------------------------------
  !FILE VARS-------------------------------------------------------------
  character(len=200) :: file_name                                        !Contains filename of tec-plot file

  !----------------------------------------------------------------------
  !DOMAIN VARS-----------------------------------------------------------
  integer, dimension(1,3) :: x_N                                         !Mesh resolution in each direction
  real,    dimension(1,3) :: x_min, x_max                                !Domain limits
  real,    dimension(1,3) :: dx                                          !Mesh-size

  !----------------------------------------------------------------------
  !FIELD VARS------------------------------------------------------------
  real, allocatable       :: u(:,:)                                      !Velocity field
  real, allocatable       :: p(:,:)                                      !Pressure field

  !----------------------------------------------------------------------
  !BODY-VARS-------------------------------------------------------------
  !2Dbody:
  real, allocatable   :: b(:,:,:)                                          !body array


  !GENERATE INITIAL .dat-FILE
  !TODO(JON): this should be done after the input and the initial checking
  call FilesInit(file_name) !generates folder and file


  !INPUT
  !Domain input
  call InputDomain(x_N,x_min,x_max,dx,flag_2D)  !Input domain parameters - flag_2D=.true. if 2D-problem
  !Body input
  call InputBody(b,flag_2D)                     !Input body parameters   - flag_2D=.true. if 2D-problem


  !GENERATE GRID and BODY
  !Generate domain grid and write to file
  call GridGen(file_name,x_N,x_min,x_max,dx)
  !Generate body array and write to file - not actually, as body array is per now made by InputBody
  call BodyGen(file_name,b)

  !DETECT BOUNDARY
  call IBMInterSect_2D(file_name,flag_2D,x_min,dx,x_N,b)


  !call Final_Deallocation(): (if allocated(....) deallocate all) somethng.
  deallocate(b)
  A = system("pause")
end program main
