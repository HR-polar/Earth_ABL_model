
!==== C interface for Initialization_module_NeXtSIM
subroutine initabl_c(albedo, ug, vg, slon, semis, rlat, z0, taur, p0, q0, t0, nj, &
    dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, ustar) &
    bind(c)

  use iso_c_binding
  implicit none

  ! Arguments
  ! Inputs
  REAL(c_double), INTENT(IN) :: albedo, ug, vg, slon, semis, rlat, z0, taur, p0, q0, t0
  INTEGER(c_int), INTENT(IN) :: nj

  ! Outputs:
  REAL(c_double), DIMENSION(nj), INTENT(OUT) :: dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh
  REAL(c_double),                INTENT(OUT) :: ustar

  ! Call the FORTRAN 77 function
  CALL Initialize_NeXtSIM_ABL(albedo,ug,vg,slon,semis,rlat,z0, &
    taur,p0,q0,t0, &
    Nj,dedzm,dedzt,zm,zt,u,v,t,q,qi,e,ep,uw,vw,wt,wq,wqi,km,kh,ustar)

end subroutine initABL_c

!==== C interface for Command_module_for_NeXtSIM
subroutine stepabl_c(albedo, ug, vg, slon, semis, rlat, z0, taur, p0, ds, ha, jd, nj, &
    dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, ustar) &
    bind(c)

  use iso_c_binding
  implicit none

  ! Arguments
  ! Inputs
  REAL(c_double), INTENT(IN) :: albedo, ug, vg, slon, semis, rlat, z0, taur, p0, ds, ha, jd
  INTEGER(c_int), INTENT(IN) :: nj

  ! Outputs:
  REAL(c_double), DIMENSION(nj), INTENT(OUT) :: dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh
  REAL(c_double),                INTENT(OUT) :: ustar

  ! Call the FORTRAN 77 function
  CALL Integrate_NeXtSIM_ABL(albedo,ug,vg,slon,semis,rlat,z0, &
    taur,p0,ds,ha,jd, &
    nj,dedzm,dedzt,zm,zt,u,v,t,q,qi,e,ep,uw,vw,wt,wq,wqi,km,kh,ustar)

end subroutine stepabl_c

