cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Test routines for I/O for the ABL.for model
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      PROGRAM test_io

        USE io
        USE datetime_module

        IMPLICIT NONE

        CHARACTER(LEN=256) :: fname, lon_name, lat_name, mask_name
        INTEGER :: mgr, ngr, i, j
        REAL, ALLOCATABLE, DIMENSION(:,:) :: rlat, rlon, output2D
        REAL, ALLOCATABLE, DIMENSION(:,:,:) :: output3D
        INTEGER, ALLOCATABLE, DIMENSION(:,:) :: mask

        TYPE(datetime) :: time

        TYPE(input_var_2D) :: mslp

! Test reading of grid

        fname = "grid.nc"
        lon_name = "plon"
        lat_name = "plat"
        mask_name = "mask"

        CALL read_grid(fname, lon_name, lat_name, mask_name, mgr, ngr,
     1                                                  rlon, rlat,mask)

        print *, "Read ", fname
        print *, "mgr = ", mgr
        print *, "ngr = ", ngr

        print *, "maxval(rlon) = ", maxval(rlon), 
     1           "minval(rlon) = ", minval(rlon)
        print *, "maxval(rlat) = ", maxval(rlat), 
     1           "minval(rlat) = ", minval(rlat)


        do i = 1, mgr, mgr/100
          do j = 1, ngr, ngr/100
            if ( mask(i,j) .eq. 0) then
              write(6, "(A)", advance="no") "X"
            else
              write(6, "(A)", advance="no") " "
            end if
          end do
          write(6, *)
        end do

! Test the output routines

! Create file and initialise variables
        fname = "out_test.nc"
        time = datetime(2015,12,15,12,15,12,15)
        print *, time%isoformat()
        call init_netCDF(fname, mgr, ngr, mask, rlon, rlat, time, 100)
        call init_netCDF_var(fname, "test2D", 2,
     1      long_name="test_for_a_2D_case",
     1      standard_name="test for a 2D case",
     1      units = "-")
        call init_netCDF_var(fname, "test3D", 3,
     1      long_name="test_for_a_3D_case",
     1      standard_name="test for a 3D case",
     1      units = "-")

        allocate(output2D(size(rlon,1),size(rlon,2)))
        allocate(output3D(size(rlon,1),size(rlon,2),100))

! First step and output
        output2D = 0.0
        output3D = 0.0

        call write_netCDF_var(fname, "test2D", output2D)
        call write_netCDF_var(fname, "test3D", output3D)

! Second step and output - using the absolute time for
! append_netCDF_time
        output2D = 1.0
        output3D = 1.0
        time = time + timedelta(days=1)
        call append_netCDF_time(fname, time)
        call write_netCDF_var(fname, "test2D", output2D)
        call write_netCDF_var(fname, "test3D", output3D)

! Third step and output - this time using the delta-t option for
! append_netCDF_time
        output2D = 1.0
        output3D = 1.0
        time = time + timedelta(days=1)
        call append_netCDF_time(fname, time)
        call write_netCDF_var(fname, "test2D", output2D)
        call write_netCDF_var(fname, "test3D", output3D)

        time = datetime(2007,01,01)
        call mslp%init("msl", rlon, rlat)
        call mslp%read_input(time)

        fname = "msl_interp.nc"
        call init_netCDF(fname, mgr, ngr, mask, rlon, rlat, time)
        call write_netCDF_var(fname, "msl", mslp%data)

      END PROGRAM test_io

