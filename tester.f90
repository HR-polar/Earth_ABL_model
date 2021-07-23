PROGRAM tester

  IMPLICIT NONE

  ! Initial and forcing values - ABL
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
  INTEGER, PARAMETER :: ni  = 11

  REAL, DIMENSION(nj) :: dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, tld
  REAL :: ustar

  ! Soil model variables
  REAL, DIMENSION(ni) :: dedzs,tsoil,zsoil
  REAL :: dzeta, gflux

  INTEGER :: i

  ! Initialise the ABL model
  CALL Initialize_NeXtSIM_ABL(albedo, ug, vg, slon, semis, rlat, z0, taur, p0, q0, t0, nj, &
    dedzm, dedzt, zm, zt, u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, ustar, tld)

  ! Initialise the soil model
  CALL subsoilt(dedzs,tsoil,zsoil,dzeta,t(1),z0,ni)

  print *, "Init done"

  do i = 1, 10
    ! Step the ABL model
    CALL Integrate_NeXtSIM_ABL(albedo, ug, vg, slon, semis, rlat, z0, taur, p0, ds, ha, jd, nj, &
      dedzm,dedzt,zm,zt,u,v,t,q,qi,e,ep,uw,vw,wt,wq,wqi,km,kh, ustar, gflux, tld)

    ! Step the soil model
    CALL soiltdm(dedzs,tsoil,zsoil,dzeta,gflux,ds,ni)
    print *, t(1), gflux
    t(1)=tsoil(1)

    !print *, i, dedzm(1),dedzt(1),zm(1),zt(1),u(1),v(1),t(1),q(1),qi(1),e(1),ep(1),uw(1),vw(1),wt(1),wq(1),wqi(1),km(1),kh(1), &
    !  ustar, gflux, tld(1)
  end do

  print *, "Step done"

END PROGRAM tester
