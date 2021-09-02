!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   Routines for I/O for the ABL.for model
!
! * We use ncio and datetime_module
! * Read and write netCDF files in 2D and 3D
! * Calculate geostrophic winds from SLP
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module io

  use ncio, only: nc_create, nc_write_dim, nc_write, nc_size, nc_dims, nc_read
  use datetime_module, only: datetime, timedelta, strptime

  implicit none

  private

  ! Time keeping constants
  character(len=8), parameter :: time_unit = "days"
  character(len=8), parameter :: calendar = "standard"
  character(len=19), parameter :: reference_time = "1900-01-01 00:00:00"
  character(len=17), parameter :: time_format= "%Y-%m-%d %H:%M:%S"
  character(len=34), parameter :: time_string = trim(time_unit)//" since "//reference_time

  interface write_netCDF_var
    module procedure write_netCDF_2D, write_netCDF_3D
  end interface

  type :: input_var_2D
    integer, private :: m, n
    real, private, dimension(:,:), allocatable :: lon, lat
    character(len=32), private :: vname
    character(len=512), private :: fname

    real, public, dimension(:,:), allocatable :: data

    contains
      procedure, public :: init, read_input
  end type

  character(len=37), parameter :: ERA5_prefix = "data/ERA5_"
  character(len=9), parameter :: ERA5_lon_name = "longitude"
  character(len=8), parameter :: ERA5_lat_name = "latitude"

  public :: init_netCDF, init_netCDF_var, append_netCDF_time, write_netCDF_var
  public :: read_grid, input_var_2D

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Time handling
!   - We calculate the time in netCDF reference (see time_unit and reference_time parameters)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function netCDF_time(time_in) result(time_out)

    implicit none

    type(datetime), intent(in) :: time_in

    type(timedelta) :: dt

    ! Express the difference between time_in and reference time in time_unit
    dt = time_in - strptime(trim(reference_time), trim(time_format))

    if ( time_unit .eq. "seconds" ) then
      time_out = dt%total_seconds()
      return
    elseif ( time_unit .eq. "hours" ) then
      time_out = dt%total_seconds()/3600.
      return
    elseif ( time_unit .eq. "days" ) then
      time_out = dt%total_seconds()/86400.
    else
      stop "mod_io: netCDF_time: Case "//trim(time_unit)//" not recognised"
    endif

    end function netCDF_time

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                               OUTPUTS
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to initialise a netCDF file
!   - We write the x, y, and time dimensions, mask, and lat/lon coords
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init_netCDF(fname, mgr, ngr, mask, lon, lat, time_in, nz)

    implicit none

    character(len=*), intent(in) :: fname
    integer, intent(in) :: mgr, ngr
    integer, dimension(:,:), intent(in) :: mask
    real, dimension(:,:), intent(in) :: lon, lat
    type(datetime), intent(in) :: time_in
    integer, intent(in), optional :: nz

    double precision :: time

    ! Get the time in netCDF format
    time = netCDF_time(time_in)

    ! Create the file
    call nc_create(fname, overwrite=.true., netcdf4=.true.)

    ! Write dimensions, either three or four, depending on inputs
    call nc_write_dim(fname, "x", x=1, dx=1, nx=mgr)
    call nc_write_dim(fname, "y", 1, nx=ngr)
    if ( present(nz) ) then
      call nc_write_dim(fname, "z", 1, nx=nz)
    endif
    call nc_write_dim(fname, "time", time, unlimited=.true., units=trim(time_string), calendar=trim(calendar))

    ! Write lon, lat, time, and mask variables
    call nc_write(fname, "mask", mask, dim1="x", dim2="y")
    call nc_write(fname, "longitude", lon, dim1="x", dim2="y", &
      long_name="longitude", standard_name="longitude", units="degrees_north")
    call nc_write(fname, "latitude", lat, dim1="x", dim2="y", &
      long_name="latitude", standard_name="latitude", units="degrees_north")

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

  subroutine append_netCDF_time(fname, time_in)

    implicit none

    character(len=*), intent(in) :: fname
    type(datetime), intent(in) :: time_in

    double precision :: time
    integer :: time_slice

    ! Get the time in netCDF format
    time = netCDF_time(time_in)

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                               INPUTS
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initialise the iput_var_2D object
!   - This just sets some arrays
!   - TODO: Calculate interpolation weights here
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init(self, varname, lon, lat)

    implicit none

    class(input_var_2D), intent(inout) :: self
    character(len=*), intent(in) :: varname
    real, dimension(:,:), allocatable :: lon, lat

    ! Save the variable name and lon and lat in the object
    self%vname = varname
    allocate(self%lon, self%lat, mold=lon)
    self%lon = lon
    self%lat = lat

    ! Save the size of the grid
    self%m = size(lon,1)
    self%n = size(lon,2)

  end subroutine init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read and interpolate input
!   - We read from data and call interp2D to interpolate onto the model grid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine read_input(self, time)

    implicit none

    class(input_var_2D), intent(inout) :: self
    type(datetime), intent(in) :: time

    ! working variables
    character(len=32), allocatable :: dimnames(:)
    integer, allocatable :: dimlens(:)
    real, dimension(:,:), allocatable :: data_ll, rlon_fx
    real, dimension(:), allocatable :: elon, elat
    type(datetime) :: t0
    type(timedelta) :: dt
    integer :: time_slice, i, j

    ! Deduce the file name
    write(self%fname, "(i4)") time%getYear()
    self%fname = trim(ERA5_prefix)//trim(self%vname)//"_y"//trim(self%fname)//".nc"

    ! Deduce the time slice
    t0 = datetime(time%getYear(), 01, 01)
    dt = time - t0
    time_slice = nint(dt%total_seconds()/3600.) + 1

    ! get the dims and allocate and read from file
    call nc_dims(self%fname, ERA5_lon_name, dimnames, dimlens)
    allocate(elon(dimlens(1)+1))
    call nc_read(self%fname, ERA5_lon_name, elon(1:dimlens(1)))
    elon(dimlens(1)+1) = 360. ! to get periodic boundary

    call nc_dims(self%fname, ERA5_lat_name, dimnames, dimlens)
    allocate(elat(dimlens(1)))
    call nc_read(self%fname, ERA5_lat_name, elat)

    call nc_dims(self%fname, self%vname, dimnames, dimlens)
    allocate(data_ll(dimlens(1)+1, dimlens(2)))

    ! Read from file
    call nc_read(self%fname, self%vname, data_ll(1:dimlens(1),1:dimlens(2)), &
      start=[1, 1, time_slice], count=[dimlens(1), dimlens(2), 1])

    data_ll(dimlens(1)+1,:) = data_ll(1,:) ! To get periodic boundary

    ! We need input longitudes in [0, 360] - like ERA
    allocate(rlon_fx, mold=self%lon)
    do i = 1, size(self%lon,1)
      do j = 1, size(self%lon,2)
        if ( self%lon(i,j) < 0. ) then
          rlon_fx(i,j) = self%lon(i,j)+360.
        else
          rlon_fx(i,j) = self%lon(i,j)
        endif
      enddo
    enddo

    ! Interpolate to grid
    if ( .not. allocated(self%data) ) allocate( self%data( size(self%lat,1), size(self%lat,2) ) )
    call interp2D(data_ll, elon, elat, self%data, rlon_fx, self%lat)

  end subroutine read_input

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Bilinear interpolation in lat/lon - "it's good enough for government work"
!   - This should be split up so that weights are calculated only once
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine interp2D(data_in, lon_in, lat_in, data_out, lon_out, lat_out)

    implicit none

    real, dimension(:,:), intent(in) :: data_in, lat_out, lon_out
    real, dimension(:), intent(in) :: lon_in, lat_in
    real, dimension(:,:), intent(out) :: data_out

    ! Working variables
    integer :: a_lon, b_lon, a_lat, b_lat, i, j
    real :: x1, x2, y1, y2, x, y, r, s

    do i = 1, size(lon_out,1)
      do j = 1, size(lon_out,2)
        call bisect(lon_out(i,j), lon_in, a_lon, b_lon)
        call bisect(lat_out(i,j), lat_in, a_lat, b_lat)

        x = lon_out(i,j)
        y = lat_out(i,j)
        x1 = lon_in(a_lon)
        x2 = lon_in(b_lon)
        y1 = lat_in(a_lat)
        y2 = lat_in(b_lat)

        r = (x - x1)/(x2 - x1)
        s = (y - y1)/(y2 - y1)

        data_out(i,j) = data_in(a_lon, a_lat)*(1-r)*(1-s) &
                      + data_in(a_lon, b_lat)*r*(1-s)     &
                      + data_in(b_lon, b_lat)*r*s         &
                      + data_in(b_lon, a_lat)*(1-r)*s

      enddo
    enddo

  end subroutine interp2D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Bisect search to find the lat/lon point on the ERA grid surrounding the model grid point
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine bisect(x, vx, a, b)

    implicit none

    real, intent(in) :: x, vx(:)
    integer, intent(out) :: a, b

    ! local variables
    integer :: test
    logical :: check, is_increasing

    is_increasing = vx(2) .gt. vx(1)

    a = 1
    b = size(vx)

    test = (b-a)/2
    do while ( b-a .gt. 1 )
      ! check if we're increasing or decreasing
      ! The only difference between the two cases is the .gt./.lt. in the if clause - can this be done more elegantly?
      if ( is_increasing ) then
        check = vx(test) .gt. x
      else
        check = vx(test) .lt. x
      endif

      if ( check ) then
        b = test
        test = b - (b-a)/2
      else
        a = test
        test = a + (b-a)/2
      endif
    enddo

  end subroutine bisect

end module io
