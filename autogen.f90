program autogen

  implicit none

  integer :: n, nn
  real :: x, y

  open(10, file='Fast_subfec.for')

  call subfecgen("subfec", 10, -7.3, 3.6)

  write(10, '(A)') ""

  call subfecgen("subfec2", 10, -30., 1.7)

  close(10)


end program autogen

subroutine subfecgen(name, fid, xmin, xmax)

  implicit none

  character(len=*), intent(in) :: name
  integer, intent(in) :: fid
  real, intent(in) :: xmin, xmax

  ! Work variables
  integer :: n, nn
  real :: x, y

  ! The lenght of the resulting vector
  nn = (xmax-xmin)*10 + 1

  write(fid, '(A)') "c======================================================================="
  write(fid, '(A,A)') "c                          Subroutine ", trim(name)
  write(fid, '(A)') "c----------------------------------------------------------------------="
  write(fid, '(A)') "c   Calculating the flux emissivity for CO2 from the emissivity data   ="
  write(fid, '(A)') "c tabulated by Staley & Jurica, 1970, J. Applied Meteorol., 9, 365-372 ="
  write(fid, '(A)') "c======================================================================="
  write(fid, '(A,A,A)') "      SUBROUTINE ", trim(name), "(x,y)"
  write(fid, '(A)') "      IMPLICIT none"
  write(fid, '(A)') "      REAL x,y,xmin"
  write(fid, '(A)') "      INTEGER nn,n"
  write(fid, '(A,I3,A)') "      PARAMETER (nn=", nn, ")"
  write(fid, '(A,F5.1,A)') "      PARAMETER (xmin=", xmin, ")"
  write(fid, '(A)') "      REAL yem(nn)"

  ! Build and write yem
  if ( name == "subfec" ) then
    call subfec_org(xmin, y)
  elseif ( name == "subfec2" ) then
    call subfec2_org(xmin, y)
  else
    stop "Function call to "//trim(name)//" not implemented"
  endif

  write(fid, '(A,E12.6)', advance='no') "      DATA yem/", y
  
  do n = 2, nn
    x = xmin + (n-1.)/10.

    if ( name == "subfec" ) then
      call subfec_org(x, y)
    elseif ( name == "subfec2" ) then
      call subfec2_org(x, y)
    else
      stop "Function call to "//trim(name)//" not implemented"
    endif

    if ( modulo(n-1,4) .eq. 0 ) then
      write(fid, '(A)') ","
      write(fid, '(A,E12.6)', advance='no')  "     C         ", y
    else
      write(fid, '(A,E12.6)', advance='no') ",", y
    endif
  enddo

  write(fid, '(A)') "/"

  write(fid, '(A)') ""
  write(fid, '(A)') "      n = floor(10.*(x - xmin)) + 1"
  write(fid, '(A)') ""
  write(fid, '(A)') "      if ( n<1 .or. n>nn ) then"
  write(fid, '(A)') "          write(6,'(10x,""The log of optical path is out ouf the data"
  write(fid, '(A)') "     C range ..."",f10.5)') x"
  write(fid, '(A)') "          n = min(nn, max(1, n))"
  write(fid, '(A)') "      endif"
  write(fid, '(A)') ""
  write(fid, '(A)') "! Here (x-xop(nn))/(xop(nn)-xop(nn+1) is converted to 10.*(x-xmin)-n+1."
  write(fid, '(A)') "! because x-xop(nn) = x - xmin + (n-1.)/10. and xop(nn)-xop(nn+1) = 0.1"
  write(fid, '(A)') "      y = yem(n)+(yem(n+1)-yem(n))*(10.*(x - xmin) - n + 1.)"
  write(fid, '(A)') ""
  write(fid, '(A)') "      END SUBROUTINE"
  write(fid, '(A)') ""

end subroutine subfecgen
