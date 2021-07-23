
!==== C interface for Initialization_module_NeXtSIM
subroutine initabl_c(albedo, ug, vg, slon, semis, rlat, z0, taur, p0, q0, t0, nj, &
    dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, ustar, tld) &
    bind(c)

  use iso_c_binding
  implicit none

  ! Arguments
  ! Inputs
  REAL(c_double), INTENT(IN) :: albedo, ug, vg, slon, semis, rlat, z0, taur, p0, q0, t0
  INTEGER(c_int), INTENT(IN) :: nj

  ! Outputs:
  REAL(c_double), DIMENSION(nj), INTENT(OUT) :: dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, tld
  REAL(c_double),                INTENT(OUT) :: ustar

  ! Call the FORTRAN 77 function
  CALL Initialize_NeXtSIM_ABL(albedo,ug,vg,slon,semis,rlat,z0, &
    taur,p0,q0,t0, &
    Nj,dedzm,dedzt,zm,zt,u,v,t,q,qi,e,ep,uw,vw,wt,wq,wqi,km,kh,ustar,tld)

end subroutine initABL_c

!==== C interface for Command_module_for_NeXtSIM
subroutine stepabl_c(albedo, ug, vg, slon, semis, rlat, z0, taur, p0, ds, ha, jd, nj, &
    dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, ustar, gflux,tld) &
    bind(c)

  use iso_c_binding
  implicit none

  ! Arguments
  ! Inputs
  REAL(c_double), INTENT(IN) :: albedo, ug, vg, slon, semis, rlat, z0, taur, p0, ds, ha, jd
  INTEGER(c_int), INTENT(IN) :: nj
  REAL(c_double), DIMENSION(nj), INTENT(IN) :: dedzm, dedzt, zm, zt

  ! In-out
  REAL(c_double), DIMENSION(nj), INTENT(INOUT) :: u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, tld
  REAL(c_double),                INTENT(INOUT) :: ustar, gflux

  ! Call the FORTRAN 77 function
  CALL Integrate_NeXtSIM_ABL(albedo,ug,vg,slon,semis,rlat,z0, &
    taur,p0,ds,ha,jd, &
    nj,dedzm,dedzt,zm,zt,u,v,t,q,qi,e,ep,uw,vw,wt,wq,wqi,km,kh,ustar, gflux,tld)

end subroutine stepabl_c

!==== C interface for subsoilt
subroutine subsoilt_c(dedzs,tsoil,zsoil,dzeta,t0,z0,ni) bind(c)

  use iso_c_binding
  implicit none

  ! Arguments
  ! Inputs
  REAL(c_double), INTENT(IN) :: t0,z0
  INTEGER(c_int), INTENT(IN) :: ni

  ! Outputs
  REAL(c_double), DIMENSION(ni), INTENT(OUT) :: dedzs,tsoil,zsoil
  REAL(c_double), INTENT(OUT) :: dzeta

  ! Call the FORTRAN 77 subroutine
  CALL subsoilt(dedzs,tsoil,zsoil,dzeta,t0,z0,ni)

end subroutine subsoilt_c

!==== C interface for soiltdm
subroutine soiltdm_c(dedzs,tsoil,zsoil,dzeta,gflux,dt,ni) bind(c)

  use iso_c_binding
  implicit none

  ! Arguments
  ! Inputs
  INTEGER(c_int),                INTENT(IN)    :: ni
  REAL(c_double), DIMENSION(ni), INTENT(IN)    :: dedzs,zsoil
  REAL(c_double),                INTENT(IN)    :: dzeta, gflux, dt

  ! In-Out
  REAL(c_double), DIMENSION(ni), INTENT(INOUT) :: tsoil

  ! Call the FORTRAN 77 subroutine
  CALL soiltdm(dedzs,tsoil,zsoil,dzeta,gflux,dt,ni)

end subroutine soiltdm_c
