PROGRAM tester

  IMPLICIT NONE

  ! Initial and forcing values
  REAL, PARAMETER :: albedo = 0.367
  REAL, PARAMETER :: ug     = 5.
  REAL, PARAMETER :: vg     = 0.
  REAL, PARAMETER :: slon   = 226.
  REAL, PARAMETER :: semis  = 0.96
  REAL, PARAMETER :: rlat   = 29.
  REAL, PARAMETER :: z0     = 0.001
  REAL, PARAMETER :: taur   = 0.
  REAL, PARAMETER :: p0     = 1015
  REAL, PARAMETER :: q0     = 0.1
  REAL, PARAMETER :: t0     = 300
  REAL, PARAMETER :: ds     = 30
  REAL, PARAMETER :: ha     = 3.141592654
  REAL, PARAMETER :: jd     = 1

  INTEGER, PARAMETER :: nj  = 241

  REAL, DIMENSION(nj) :: dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh
  REAL :: ustar

  CALL Initialize_NeXtSIM_ABL(albedo, ug, vg, slon, semis, rlat, z0, taur, p0, q0, t0, nj, &
    dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, ustar)

  print *, "Init done"

  CALL Integrate_NeXtSIM_ABL(albedo, ug, vg, slon, semis, rlat, z0, taur, p0, ds, ha, jd, nj, &
    dedzm,dedzt,zm,zt,u,v,t,q,qi,e,ep,uw,vw,wt,wq,wqi,km,kh, ustar)

  print *, "Step done"

END PROGRAM tester
