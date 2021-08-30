!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   Routines for I/O for the ABL.for model
!
! * We use ncio
! * Read and write netCDF files in 2D and 3D
! * Calculate geostrophic winds from SLP
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE io

  USE NCIO

  PUBLIC :: read_grid

  CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to read the grid - lon, lat, and mask
! - We expect this to be called at init. It checks dimensions and
!   returns them
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE read_grid(fname, lon_name, lat_name, mask_name, mgr, ngr, rlon, rlat, mask)

    IMPLICIT NONE

    ! Parameters
    CHARACTER(LEN=*), INTENT(IN) :: fname, lon_name, lat_name, mask_name
    INTEGER, INTENT(OUT) :: mgr, ngr
    REAL, DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: rlat, rlon
    INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: mask

    ! Working variables
    CHARACTER(LEN=32), ALLOCATABLE :: dimnames(:)
    INTEGER, ALLOCATABLE :: dimlens(:)

    ! Get the dims
    CALL nc_dims(fname, lon_name, dimnames, dimlens)
    mgr = dimlens(1)
    ngr = dimlens(2)

    ! Allocate rlon and rlat
    ALLOCATE(rlon(mgr,ngr))
    ALLOCATE(rlat, mold=rlon)
    ALLOCATE(mask(mgr,ngr))

    ! Read from file
    CALL nc_read(fname, lon_name, rlon)
    CALL nc_read(fname, lat_name, rlat)
    CALL nc_read(fname, mask_name, mask)

  END SUBROUTINE read_grid

END MODULE io
