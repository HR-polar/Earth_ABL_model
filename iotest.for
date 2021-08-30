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
        REAL, ALLOCATABLE, DIMENSION(:,:) :: rlat, rlon
        INTEGER, ALLOCATABLE, DIMENSION(:,:) :: mask

        TYPE(datetime) :: time

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
     1           " minval(rlon) = ", minval(rlon)
        print *, "maxval(rlat) = ", maxval(rlat), 
     1           " minval(rlat) = ", minval(rlat)


        do i = 1, mgr, mgr/100
          do j = 1, ngr, ngr/100
            write(6, "(I2)", advance="no") mask(i,j)
          end do
          write(6, *)
        end do

      END PROGRAM test_io

