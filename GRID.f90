module GRID
!MODULE HEADER
!
!
!
!===========================================================
    use INPUT
    use FILES
    use GLOBALMODULEVARS
    implicit none


    contains
!==================================================================


    !TODO(JON): Separate file-management and grid generation in two subroutines (y/n?)
    subroutine GridGen(file_name,x_N,x_min,x_max,dx)

        character(len=*)    :: file_name

        integer, dimension(:,:) :: x_N

        real, dimension(:,:)    :: x_min, x_max
        real, dimension(:,:)    :: dx



!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        !TODO(JON): make a generic FilseWrite subroutine that generates new zones on the tecplot file...!
        call FilesZoneInit(file_name,'Physical Grid', 'RED', x_N(1,1), x_N(1,2), x_N(1,3) )

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        !Parsing mesh-data into tecplot points
        !TODO(JON): - implement third dimension later
        !TODO(JON): make use of the vectorized input here..
        !TODO(JON): Also write out the cell pressure centers to a TecPlot Zone

!WRITE POSITION MESH (PHYSICAL GRID)
        open(unit=101, file=file_name, action='write', position='append')  !
        do k= 1,x_N(3,1)
            do j = 1,x_N(2,1)
                do i=1,x_N(1,1)
                    !call a write zone Files subroutine
                    write(101,*) x_min(1,1) + (i-1)*dx(1,1) , x_min(2,1) + (j-1)*dx(2,1), x_min(3,1) +(k-1)*dx(3,1) , u_zero,p_zero
                end do
            end do
        end do
        close(101)

!WRITE FIELD VARIABLE STAGGERED GRID POSITIONS
!TODO(JON): This is written in a hurry... Some staggered points fall outside physical grid... (check loop limits)
        !U-velocity points
        call FilesZoneInit(file_name,'u-velocity points', 'GREEN', x_N(1,1), x_N(1,2), x_N(1,3) )
        open(unit=101, file=file_name, action='write', position='append')  !
        do k= 1,x_N(3,1)
            do j = 1,x_N(2,1)
                do i=1,x_N(1,1)
                    !call a write zone Files subroutine
                    write(101,*) x_min(1,1) + (i)*dx(1,1) , x_min(2,1) + (j-1+0.5)*dx(2,1), x_min(3,1) +(k-1+0.5)*dx(3,1) &
                    , u_zero,p_zero
                end do
            end do
        end do
        close(101)

        !V-velocity points
        call FilesZoneInit(file_name,'v-velocity points', 'PURPLE', x_N(1,1), x_N(1,2), x_N(1,3) )
        open(unit=101, file=file_name, action='write', position='append')  !
        do k= 1,x_N(3,1)
            do j = 1,x_N(2,1)
                do i=1,x_N(1,1)
                    !call a write zone Files subroutine
                    write(101,*) x_min(1,1) + (i-1+0.5)*dx(1,1) , x_min(2,1) + (j)*dx(2,1), x_min(3,1) +(k-1+0.5)*dx(3,1) &
                    ,u_zero,p_zero
                end do
            end do
        end do
        close(101)

        !W-velocity points
        call FilesZoneInit(file_name,'w-velocity points', 'BLACK', x_N(1,1), x_N(1,2), x_N(1,3) )
        open(unit=101, file=file_name, action='write', position='append')  !
        do k= 1,x_N(3,1)
            do j = 1,x_N(2,1)
                do i=1,x_N(1,1)
                    !call a write zone Files subroutine
                    write(101,*) x_min(1,1) + (i-1+0.5)*dx(1,1) , x_min(2,1) + (j-1+0.5)*dx(2,1), x_min(3,1) +(k)*dx(3,1) &
                    ,u_zero,p_zero
                end do
            end do
        end do
        close(101)


        !W-velocity points
        call FilesZoneInit(file_name,'w-velocity points', 'BLACK', x_N(1,1), x_N(1,2), x_N(1,3) )
        open(unit=101, file=file_name, action='write', position='append')  !
        do k= 1,x_N(3,1)
            do j = 1,x_N(2,1)
                do i=1,x_N(1,1)
                    !call a write zone Files subroutine
                    write(101,*) x_min(1,1) + (i-1+0.5)*dx(1,1) , x_min(2,1) + (j-1+0.5)*dx(2,1), x_min(3,1) + (k-1+0.5)*dx(3,1) &
                    ,u_zero,p_zero
                end do
            end do
        end do
        close(101)
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    end subroutine GridGen
!====================================================================































!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
!SUBROUTINE ARCHIVE FOR THIS MODULE_____________________________________________________________!
!                                                                                               !
!_______________________________________________________________________________________________!

    subroutine GridGenOld(file_name,x_N,y_N,z_N,x_min,x_max,y_min,y_max,z_min,z_max,dx,dy,dz)

        integer :: x_N, y_N, z_N
        real    :: x_min, x_max, y_min, y_max, z_min, z_max
        real    :: dx, dy, dz

        character(len=*)    :: file_name


        dx = (x_max-x_min)/(x_N -1)
        dy = (y_max-y_min)/(y_N -1)
        dz = (z_max-z_min)/(z_N -1)

        open(unit=101, file=file_name, action='write')
        write(101,80,advance='no') 'TITLE = '
        write(101,80) file_name
        write(101,80) 'VARIABLES = "X", "Z"'
        write(101,80) ' '
        write(101,80) 'ZONE'
        write(101,80) 'T="MeshGrid"'
        write(101,80) 'C="RED"'
        write(101,910) 'I = ', x_N, ', J= ', y_N, ', K= ', z_N
        write(101,80) 'F=POINT' !TODO(JON) : Change to tecplot Block-type input


80 format(A)
910 format(A,I9,A,I9,A,I9)

        !TODO(JON): Add third dimension if that is manageable
        do k= 1,z_N
            do i=1,x_N
                write(101,*) x_min + (i-1)*dx , z_min +(k-1)*dz
            end do
        end do

        close(101)

    end subroutine GridGenOld
!====================================================================

!TODO(JON): before using this subroutine - modify to 3Dimensions (include y_N etc.
    subroutine GridMeshGen(x_N, z_N, x_min, x_max, z_min, z_max, X_comp, Z_comp, X_real, Z_real)


        !COPY THIS TO MAIN TO USE SUBROUTINE
        !integer, allocatable  :: X_comp(:,:), Z_comp(:,:) !
        !real, allocatable     :: X_real(:,:), Z_real(:,:) !
        !GENERATE DOMAIN GRID (2D cartesian)
        !allocate(X_comp(x_N,z_N), Z_comp(x_N,z_N), X_real(x_N,z_N), Z_real(x_N,z_N))
        !call GridMeshGen(x_N, z_N, x_min, x_max, z_min, z_max, X_comp, Z_comp, X_real, Z_real)
        !SAVE DATA TO FILE
        !call GridMeshSave(x_N,z_N,X_real,Z_real)


        !--------------------------------------------------------
        integer         :: x_N, z_N         !May be declared in module instead? -NO!
        real            :: x_min, x_max, z_min, z_max
        integer, dimension(x_N,z_N) :: X_comp, Z_comp
        real, dimension(x_N,z_N)    :: X_real, Z_real

        real    :: dx, dz !TODO (JON): dx and dz - Keep these as well - might come in handy later in program
        !--------------------------------------------------------

        !--------------------------------------------------------

        !Create meshgrid
        dx = (x_max-x_min)/(x_N -1)
        dz = (z_max-z_min)/(z_N -1)

        !Generate computational grid
        !TODO(JON): Reduce number of iterations by making ONE array and copying this to get the matrix (Now nested loops are used)

        do k=1,z_N
            !TODO(JON): Let indices start from 0 or 1. let comp grid start from 0 or one?
            !computational_______________________________________
            !X_comp(:,k) = (/(i, i=1,x_N)/)      !Sets up X-part of comp grid
            !Z_comp(:,k) = (/(z_N-k+1, i=1,x_N)/)!Sets up Z-part of comp grid
            !Physical____________________________________________
            X_real(:,k) = (/ (x_min + i*dx , i=0,x_N-1) /)
            Z_real(:,k) = (/ (z_max - (k-1)*dz , i=0,x_N-1) /)
        end do

!        do k= 1,z_N
!            !print*, Z_comp(:,k)
!            print*, Z_real(:,k)
!            print*,'-------------------------------------'
!        end do
!        print*, '==============================================================='
!        do k= 1,z_N
!            !print*, X_comp(:,k)
!            print*, X_real(:,k)
!            print*,'-------------------------------------'
!        end do

    end subroutine GridMeshGen
!=======================================================================

!TODO(JON): before using this subroutine - modify to 3Dimensions (include y_N etc.
    subroutine GridMeshSave(x_N, z_N, X_real, Z_real)


        !COPY THIS TO MAIN TO USE SUBROUTINE
        !integer, allocatable  :: X_comp(:,:), Z_comp(:,:) !
        !real, allocatable     :: X_real(:,:), Z_real(:,:) !
        !GENERATE DOMAIN GRID (2D cartesian)
        !allocate(X_comp(x_N,z_N), Z_comp(x_N,z_N), X_real(x_N,z_N), Z_real(x_N,z_N))
        !call GridMeshGen(x_N, z_N, x_min, x_max, z_min, z_max, X_comp, Z_comp, X_real, Z_real)
        !SAVE DATA TO FILE
        !call GridMeshSave(x_N,z_N,X_real,Z_real)




        integer :: x_N, z_N
        real :: X_real(x_N,z_N), Z_real(x_N,z_N)

        open(unit=98, file='TestXgrid.dat', action='write')
        open(unit=99, file='TestZgrid.dat', action='write')

        do k= 1,z_N
            !print*, X_comp(:,k)
            write(98,*) X_real(:,k)
            !print*, Z_comp(:,k)
            write(99,*) Z_real(:,k)
        end do

        close(98)
        close(99)
    end subroutine GridMeshSave
!=======================================================================


end module GRID
