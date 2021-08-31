!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   Routines for I/O for the ABL.for model
!
! * We use ncio
! * Read and write netCDF files in 2D and 3D
! * Calculate geostrophic winds from SLP
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module io

  use ncio

  implicit none

  character(len=3), parameter :: msl_name = "msl"
  character(len=256), parameter :: msl_file = "era5_"//msl_name//"_y"

  interface write_netCDF_var
    module procedure write_netCDF_2D, write_netCDF_3D
  end interface

  public :: init_netCDF, append_netCDF_time, write_netCDF_var
  public :: read_grid

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                               OUTPUTS
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to initialise a netCDF file
!   - We write the x, y, and time dimensions, mask, and lat/lon coords
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init_netCDF(fname, mgr, ngr, mask, lon, lat, time0, nz)

    implicit none

    character(len=*), intent(in) :: fname
    integer, intent(in) :: mgr, ngr
    integer, dimension(:,:), intent(in) :: mask
    real, dimension(:,:), intent(in) :: lon, lat
    real, intent(in) :: time0
    integer, intent(in), optional :: nz

    ! Create the file
    call nc_create(fname, overwrite=.true., netcdf4=.true.)

    ! Write dimensions, either three or four, depending on inputs
    call nc_write_dim(fname, "x", x=1, dx=1, nx=mgr)
    call nc_write_dim(fname, "y", 1, nx=ngr)
    if ( present(nz) ) then
      call nc_write_dim(fname, "z", 1, nx=nz)
    endif
    call nc_write_dim(fname, "time", 0, unlimited=.true.)

    ! Write lon, lat, time, and mask variables
    call nc_write(fname, "mask", mask, dim1="x", dim2="y")
    call nc_write(fname, "longitude", lon, dim1="x", dim2="y", &
      long_name="longitude", standard_name="longitude", units="degrees_north")
    call nc_write(fname, "latitude", lat, dim1="x", dim2="y", &
      long_name="latitude", standard_name="latitude", units="degrees_north")
    call nc_write(fname, "time", time0, dim1="time", &
      long_name="time", standard_name="time", units="hours since 1900-01-01 00:00:00")

  end subroutine init_netCDF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to create a new netCDF variable.
!   - I put a 0. at the first grid point (limitation of ncio)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine init_netCDF_var(fname, vname, ndims, long_name, standard_name, units, grid_mapping, missing_value)

    implicit none

    character(len=*), intent(in) :: fname, vname
    integer, intent(in) :: ndims
    character(len=*), intent(in), optional :: long_name, standard_name, units, grid_mapping
    real, intent(in), optional :: missing_value

    real, parameter :: dummy2D(1,1) = 0., dummy3D(1,1,1) = 0.

    if ( ndims .eq. 2 ) then
      call nc_write(fname, vname, dummy2D, dim1="x", dim2="y", dim3="time", &
        start=[1, 1, 1], count=[1, 1, 1], &
        long_name=long_name, standard_name=standard_name, units=units, grid_mapping=grid_mapping, missing_value=missing_value)
    elseif ( ndims .eq. 3 ) then
      call nc_write(fname, vname, dummy3D, dim1="x", dim2="y", dim3="z", dim4="time", &
        start=[1, 1, 1, 1], count=[1, 1, 1, 1], &
        long_name=long_name, standard_name=standard_name, units=units, grid_mapping=grid_mapping, missing_value=missing_value)
    else
      stop "mod_io:init_netCDF_var: Only 2D and 3D variables supported"
    endif

  end subroutine init_netCDF_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to append to the netCDF time variable
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine append_netCDF_time(fname, time)

    implicit none

    character(len=*), intent(in) :: fname
    real, intent(in), optional :: time

    integer :: time_slice

    time_slice = nc_size(fname, "time")

    call nc_write(fname, "time", time, dim1="time", start=[time_slice+1], count=[1])

  end subroutine append_netCDF_time

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routines to append to netCDF variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine write_netCDF_2D(fname, vname, values)

    implicit none

    character(len=*), intent(in) :: fname, vname
    real, dimension(:,:), intent(in) :: values

    integer :: time_slice

    time_slice = nc_size(fname, "time")
    call nc_write(fname, vname, values, dim1="x", dim2="y", dim3="time", &
      start=[1,1,time_slice], count=[size(values,1), size(values,2), 1])

  end subroutine write_netCDF_2D

  subroutine write_netCDF_3D(fname, vname, values)

    implicit none

    character(len=*), intent(in) :: fname, vname
    real, dimension(:,:,:), intent(in) :: values

    integer :: time_slice

    time_slice = nc_size(fname, "time")
    call nc_write(fname, vname, values, dim1="x", dim2="y", dim3="z", dim4="time", &
      start=[1,1,1,time_slice], count=[size(values,1), size(values,2), size(values,3), 1])

  end subroutine write_netCDF_3D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to read the grid - lon, lat, and mask
!   - We expect this to be called at init. It checks dimensions and
!      returns them
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine read_grid(fname, lon_name, lat_name, mask_name, mgr, ngr, rlon, rlat, mask)

    implicit none

    ! parameters
    character(len=*), intent(in) :: fname, lon_name, lat_name, mask_name
    integer, intent(out) :: mgr, ngr
    real, dimension(:,:), allocatable, intent(out) :: rlat, rlon
    integer, dimension(:,:), allocatable, intent(out) :: mask

    ! working variables
    character(len=32), allocatable :: dimnames(:)
    integer, allocatable :: dimlens(:)

    ! get the dims
    call nc_dims(fname, lon_name, dimnames, dimlens)
    mgr = dimlens(1)
    ngr = dimlens(2)

    ! allocate rlon and rlat
    allocate(rlon(mgr,ngr))
    allocate(rlat, mold=rlon)
    allocate(mask(mgr,ngr))

    ! read from file
    call nc_read(fname, lon_name, rlon)
    call nc_read(fname, lat_name, rlat)
    call nc_read(fname, mask_name, mask)

  end subroutine read_grid

end module io
