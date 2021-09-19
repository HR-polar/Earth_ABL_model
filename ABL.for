C     Last change:  R    25 May 2008    5:55 pm
c***********************************************************************
c                            MAIN PROGRAM                              *
c----------------------------------------------------------------------*
c        file: EL1D1.for                                               *
c     created: August 2001                                             *
c    includes: EL1D2.for, SOLVER.for                                   *
c       input: none                                                    *
c       notes: 1d, neutral stratification with E-epsiolon-tau closure  *
c----------------------------------------------------------------------*
c             psi(j,1) = u (j) mean velocity                           *
c             psi(j,2) = v (j) mean velocity                           *
c             psi(j,3) = t (j) potential temperature                   *
c             psi(j,4) = q (j) specific humidity                       *
c             psi(j,5) = qi(j) icewater mixing ratio                   *
c             psi(j,6) = e (j) turbulent kinetic energy (TKE)          *
c----------------------------------------------------------------------*
c  ni   -  number of vertical grid points for soil temperature model   *
c  nj   -  number of vertical grid points for PBL model                *
c  nv   -  number of variables                                         *
c  zm(j)-  mean varable nodes coordinates (u, v, t, q & qi)            *
c  zt(j)-  turbulent quantities (e, ep, uw, vw, wt, wq, wqi, km & kh)  *
c***********************************************************************
      PROGRAM ABL

      use io
      use datetime_module, only: datetime, timedelta

      IMPLICIT none
      INTEGER nj,nv,nw,ni,ir
      PARAMETER(nj=241,nv=6,nw=0,ni=11,ir=121)

! Variables for the three dimensional model
! Grid points of the model are in a single vector (2D in practice)
! Everything here's allocatable as we read in the grid description and
! then decide the size.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Grid information
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      INTEGER :: ngr, mgr, igr, jgr
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: mask
      REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: lat, lon

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Inputs from forcing files
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! First are "Select Pressure levels: 850hPa and upwards (to ABL model
! top – check Pressure at highest level, default was 30km and 10hPa):
! Temperature; U-component of wind; V-component of wind"
      REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE ::
     1      u_in, v_in, t_in
! Now "Full column (all pressure levels): Specific humidity; Specific
! cloud liquid content; Specific cloud ice water content"
      REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE ::
     1      sp_cloud_liquid, sp_cloud_ice
! Finally "Surface field: Mean surface downward long-wave radiation
! flux; Mean surface downward short-wave radiation flux
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE ::
     1      sdlw, sdsw, q0, t0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Prognostic variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Full column
      REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE ::
     1      u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, p, tld,
     2      qold, qiold

! Surface only
      REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE ::
     1      ustar_2D

! Soil model
      REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE ::
     1      tsoil

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Diagnostic variables that we want to store and output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Full column
      REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: theta_out

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Output files
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      type(datetime) :: time
      type(timedelta) :: dt
      type(output_file) :: init_cond, srfv_all, SNetRad, Turb, Met

      REAL alpha,betag,ds,fc,grav,rl0,tg,ug,vg,vk,zero
      COMMON /consta/alpha,betag,ds,fc,grav,rl0,tg,ug,vg,vk,zero
      REAL betam,betah,gammam,gammah,pr
      COMMON /constb/betam,betah,gammam,gammah,pr
      REAL z0c,z0,zref,ztop,eta1,deta,rlb
      COMMON /constc/z0c,z0,zref,ztop,eta1,deta,rlb
      REAL uw0,vw0,wt0,wq0,wqi0,ustar,tstar,qstar,qistar
      COMMON /flxsrf/uw0,vw0,wt0,wq0,wqi0,ustar,tstar,qstar,qistar
      REAL a(nv,nv),alfa(nj,nv,nv),b(nv,nv),beta(nj,nv),c(nv,nv),
     1     d(nv),psi(nj,nv)
!     REAL q(nj),qi(nj),t(nj),theta(nj),tvis(nj),u(nj),v(nj)
      REAL theta(nj),tvis(nj)
      REAL tl(nj),
     1     hlw(nj),hsw(nj),rnet(nj)
      REAL turbhr(nj),hu(nj),hd(nj),fu(nj+1),fd(nj+1),su(nj+1),sd(nj+1)
      REAL dudz(nj),dvdz(nj),dthdz(nj),rif(nj),rlmo(nj)
      REAL zm(nj),zt(nj),dedzm(nj),dedzt(nj),wa(nv)
      REAL endTime,sol1, taur
      REAL pi,rpi
      INTEGER ipvt(nv),i,j,k,l,ttd
      REAL aconst,angle,emin,eps,blh,rlmin,rln,rls,rifc,wlo,zm0
      REAL fphi_m
      EXTERNAL fphi_m
      REAL zout(3),tout(3),wind,zwind
      CHARACTER*30 dname,fname
      DATA zout,zwind/0.25,0.5,1.,1.3/
      REAL t01,q01,qi01,blht,ss05,ssz1,ssz2
      REAL*8 forcing

      REAL dblht,dL,ustarp(nj),qstarp(nj),qistarp(nj),tstarp(nj)
      REAL*8 dtvis(nj),tvisk(ir)
      REAL*8 wc(ir,nj)
      REAL*8 conc1(ir,nj),conc2(ir,nj),dlamb,dzetad,p0
      REAL*8 zd(nj),rad(ir),scaled(ir),zmd(nj),F(ir,nj),F1(ir,nj)
      REAL*8 value

      REAL*8 tice(nj)

c---------Declaration of variables and arrays - NEW
c    angv - angle velocity
c  albedo - albedo
c      cp - gas heat capacity at constant pressure
c    grav - gravity
c  latent - latent heat of sublimation/condensation
c     qs0 - regolith (soil) moisture content (wetness)
c    rgas - gas constant
c    rlat - latitude
c    slon - initial longitude
c      s0 - Solar flux at TOA
c     sbc - Stefan-Boltzmann constant
c   semis - surface emissivity
c---------Specifying the location and the constants
! neXtSIM link:
! * We'll calculate albedo, semis, and z0 through a function call on a per-grid cell
!   basis
! * We need to calculate slon correctly
! * We'll calculate semis throug
      REAL albedo,cp,latent,rgas,rlat,slon,tgamma,s00,sbc,semis
      DATA albedo,cp,latent,rgas,rlat,slon,tgamma,s00,sbc,semis
c     1    /.21,736.,2.84e6,191.,19.3,143.,.002,591.,5.67e-8,.96/
c     1    /.21,736.,2.84e6,191.,70.,90.,.002,591.,5.67e-8,.96/
     1    /.367,1010.,2.50e6,287.,29,226,.007,1373,5.67e-8,.96/         !rlat=-12.41,slon=130.88
c      DATA p(1),q(1),qi(1),t(1)/1015,.01,0.,300./ ! T0=225  (q(1)=0.00003)  q in kg/kg    ,.026
c---------Array used in soil temperature model
      REAL dedzs(ni),zsoil(ni),dzeta
c      DATA tsoil,zsoil/5*225.,.0,-.006,-.012,-.045,-.13/
c---------Integer variables used for the do-loops
      INTEGER jd,jh,jm,nds,nhrs,nmts,j10,jmout,jd10
      REAL daysec,hoursec
      DATA nhrs,daysec/24,86165./   !86164, 86400
c---------Some variables used in surface energy balance calculations
      REAL albedo1,angv,ar,cc,cdec,cdh,dlw,dsw,e0,gflux,h0,ha,lw,rd,rho,
     1     s0c,sdec,sdir,sh,ss,sw,swi,fnqs
c---------Function used for calculating saturated specific humidity
      EXTERNAL fnqs

c===================Allocate arrays
! neXtSIM link: Grid size and info will be read in from file. Initial
! time from namelist
      mgr = 2
      ngr = 2
      time = datetime(2007,01,01)

! Inputs from forcing files
      ALLOCATE(t_in(mgr,ngr,nj))
      ALLOCATE(u_in, v_in, sp_cloud_liquid, sp_cloud_ice, mold=t_in)

      ALLOCATE(sdlw(mgr,ngr))
      ALLOCATE(sdsw, q0, t0, mold=sdlw)

! Prognostic variables (and theta)
      ALLOCATE(ustar_2D(mgr,ngr))
      ALLOCATE(tld(mgr,ngr,nj))
      ALLOCATE(u, v, t, q, qi, e, ep, uw, vw, wt, wq, wqi, km, kh, p,
     1  qold, qiold, theta_out, mold = tld)
! Mask and grid info
      ALLOCATE(lat, lon, mold=ustar_2D)
      ALLOCATE(mask(mgr,ngr))

! Soil model
      ALLOCATE(tsoil(mgr,ngr,ni))

c===================Set constants
      ttd=0                          !Initializing time step for dust subroutine
      taur=0.                     !Define initial surface(1m) optical depth (scales linearly with Qext(lambda))
      do igr=1,mgr
        do jgr=1,ngr
          p(igr,jgr,1) = 1015.
          q(igr,jgr,1) = 0.01
          qi(igr,jgr,1) = 0.
          t(igr,jgr,1) = 300.
        end do
      end do

      grav=9.807
! neXtSIM link: We'll set ug = u_in(i,nj) on a per-grid cell basis.
      z0=0.001
      ug=5.
      vg=0.
      vk=.4
      forcing=0
      zero=0.
      emin =1.e-7
      eps=1.e-7
      rlmin=1.e-10
      rln=1.e-7
      rls=10.
      pi=4.*ATAN(1.)
      rpi=pi/180.
      angv=2.*pi/daysec
      hoursec=daysec/nhrs
      lat = 80.
      lon = 0.
      mask = 1
      mask(1,1) = 0
      rlat = lat(1,1)
      fc=2.*angv*SIN(rlat*rpi)

! TODO: Ask Richard about this!
      rlat=0.-rlat

c---------Constants used in similarity functions
        betam =5.0                   ! Others : 6.00      4.7      5.0
        betah =5.0                   !          8.21      4.7      5.0
        gammam=16.                   !          19.3      15.      16.
        gammah=16.                   !          11.6      15.      16.
        pr    =.85                   !          0.95      .85      1.0
c===================Specify the problem of the similation
      WRITE(6,'(10x," This Is a Time-Integration Nocturnal Cycle Calcula
     1tion.",//,15x,"                 Enter total Earth days : ")')
c        READ(5,*) nds        
! TODO: nds and ds should be read in from a namelist - aren't the two
! related?
      nds=100
c      WRITE(6,'(15x,"     Number of step for each Earth hour : ")')
c        READ(5,*) nmts
c	  ds=hoursec/nmts                        ! Integration time step
      ds=5
      nmts=hoursec/ds
! TODO: Read jmout in from namelist
      jmout=nmts/6               ! Number of output per Earth hour
c      WRITE(6,'(/,15x," Define the Turbulent Closure constants ",//,
c     1       12x,"Specify the alpha (=u*^2/E, .17, .25, .3) : ")')
c        READ(5,*) alpha
      alpha=.3
c      WRITE(6,'(15x,"        Surface roughness length (z_0) : ")')
c        READ(5,*) z0
c     z0=0.001
c
c      WRITE(6,'(15x,"        Geostrophic wind(Ug) : ")')
c      READ(5,*) ug
c
c      betag=grav/t(1)
      rifc=1./betam
      rl0=100.           !300
c        rl0=.00027*ug/fc
c        PRINT*,'rl0=',rl0
c        pause
c---------Calculate grid mesh (zm,zt)
      rlb=100             ! =300 for nj=121, =100 for nj=241, =80 for nj=361
      z0c=.01              ! =0.1 (Roughness for coordinate tranform)
      zref=0
      ztop=30000.
      eta1=alog(zref/z0c+1.)+zref/rlb
      deta=(alog(ztop/z0c+1.)+ztop/rlb)/(nj-1.)

      WRITE(6,'(15x,"In coordinate transform, calculated deta & deta1 ar
     1e:",/,25x,2e16.8)') deta,eta1
      CALL subgrid(dedzm,dedzt,zm,zt,zm0,nj,nw)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Define initial dust properties                                                                    !
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      call dustzeds(nj,zm,zd,zmd,dzetad,z0,dlamb)            !Define zd,zmd for Dust calculations   !
c      call dustrads(ir,rad)                                  !Define dust paricle sizes             !
c      call dustscale(ir,rad,scaled)                          !Define dust scaling factor            !
c      call initdist(ir,nj,scaled,zd,F1,rad)                  !Define initial distribution of dust   !
c      call valcalc(ir,nj,taur,F1,zd,rad,value)               !Define val used in dust calculations  !
c      call Opdepth(ir,nj,taur,F1,zd,zm,rad,tvis,tvisk,value) !Define initial optical depth profile  !
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! TODO: taur will presumably be read in from file at some point. When we
! do that both taur and tvis will have to get an extra dimension.
      do i=1,nj
         tvis(i)=taur*(EXP(0.-(zm(i)/8786)))
      end do

c---------Calculating initial profiles
      do igr = 1, mgr
      do jgr = 1, ngr

! betag is in the consta common block and needs to be updated for every
! grid point and at every time step
        betag=grav/t(igr,jgr,1)

c---------Calculating initial u* etc from Geostrophic Drag Laws
        fc=2.*angv*SIN(lat(igr,jgr)*rpi)
        CALL subgdl(fc,z0,angle,aconst,ustar)

        ustar_2D(igr,jgr) = ustar

        call subprof(p(igr,jgr,:),q(igr,jgr,:),qi(igr,jgr,:),tvis,
     1      t(igr,jgr,:),theta,u(igr,jgr,:),v(igr,jgr,:),e(igr,jgr,:),
     2      ep(igr,jgr,:),uw(igr,jgr,:),vw(igr,jgr,:),wq(igr,jgr,:),
     3      wqi(igr,jgr,:),wt(igr,jgr,:),kh(igr,jgr,:),km(igr,jgr,:),
     4      tl,tld(igr,jgr,:),rnet,dedzt,zm,zt,aconst,angle,cp,rgas,rpi,
     5      tgamma,nj)

        theta_out(igr,jgr,:) = theta

!       wlo=-vk*betag*wt(igr,jgr,1)/ustar(igr,jgr)**3
c
        call subsoilt(dedzs,tsoil(igr,jgr,:),zsoil,dzeta,t(igr,jgr,1),
     1      z0,ni)

        do i=2,nj
         qold(igr,jgr,i)=q(igr,jgr,i)
         qiold(igr,jgr,i)=qi(igr,jgr,i)
        end do
      end do
      end do

c---------Output initial data and profiles
        open(11,file='CONSTANT.dat')
        write(11,'(/,12x,"Time step (dt)=",f5.2)') ds
        write(11,'(//,"   alpha,deta,z0 =",5e14.6)') alpha,deta,z0
! Line below should be writen in 2D into a netCDF file
        write(11,'(/,"   rssby,rl0,bl  =",5e14.6)') ug/fc/z0,rl0,rlb
        write(11,'(/,"   blh,nj,nv     =",e14.6,2i10)') ztop,nj,nv
        close(11)

      call init_cond%init('INIT_COND.nc', mgr, ngr, mask, lon, lat,
     1                                         zm=zm, zt=zt, zgnd=zsoil)
      call init_cond%add_var("U", "zm")
      call init_cond%add_var("V", "zm")
      call init_cond%add_var("Theta", "zm")
      call init_cond%add_var("P", "zm")
      call init_cond%add_var("Q", "zm")
      call init_cond%add_var("QI", "zm")

      call init_cond%add_var("E", "zt")
      call init_cond%add_var("uw", "zt")
      call init_cond%add_var("vw", "zt")
      call init_cond%add_var("ep", "zt")
      call init_cond%add_var("wt", "zt")
      call init_cond%add_var("wq", "zt")
      call init_cond%add_var("wqi", "zt")
      call init_cond%add_var("km", "zt")
      call init_cond%add_var("kh", "zt")
      call init_cond%add_var("tld", "zt")

      call init_cond%add_var("tsoil", "zgnd")

      call init_cond%append_time(time)
      call init_cond%append_var("U", u)
      call init_cond%append_var("V", v)
      call init_cond%append_var("Theta", theta_out)
      call init_cond%append_var("P", p)
      call init_cond%append_var("Q", q)
      call init_cond%append_var("QI", qi)

      call init_cond%append_var("E", e)
      call init_cond%append_var("uw", uw)
      call init_cond%append_var("vw", vw)
      call init_cond%append_var("ep", ep)
      call init_cond%append_var("wt", wt)
      call init_cond%append_var("wq", wq)
      call init_cond%append_var("wqi", wqi)
      call init_cond%append_var("km", km)
      call init_cond%append_var("kh", kh)
      call init_cond%append_var("tld", tld)

      call init_cond%append_var("tsoil", tsoil)

c---------Initialization array used in solving matrix
      do 80 l=1,nv
      do 80 j=1,nv
      do 80 i=1,nj
        alfa(i,j,l)=zero
 80   continue

      do 90 l=1,nv
      do 90 i=1,nj
        beta(i,l)=zero
 90   continue

c---------Outputing iteration information
      WRITE(6,'(//,16x,"              Number of Earth Days : ",i4,/,
     1             16x,"     Number of Hours Per Earth Day : ",i4,/,
     2             16x,"Number of Time Step Per Earth Hour : ",i4)')
     3  nds,nhrs,nmts

c---------Open files for data output during the time integration
      call srfv_all%init('SRFV-ALL.nc', mgr, ngr, mask, lon, lat)
      call srfv_all%add_var("E0")
      call srfv_all%add_var("u*")
      call srfv_all%add_var("uw")
      call srfv_all%add_var("vw")
      call srfv_all%add_var("wt")
      call srfv_all%add_var("km")
      call srfv_all%add_var("kh")
!      call srfv_all%add_var("1/LO")
      call srfv_all%add_var("T0")
!      call srfv_all%add_var("blht")

!      call SNetRad%init('SNetRad.nc', mgr, ngr, mask, lon, lat)
!      call SNetRad%add_var("HLW")
!      call SNetRad%add_var("HSW")
!      call SNetRad%add_var("RNet")

      call Turb%init('Turb.nc', mgr, ngr, mask, lon, lat, zt=zt)
      call Turb%add_var("e", "zt")
      call Turb%add_var("uw", "zt")
      call Turb%add_var("vw", "zt")
      call Turb%add_var("tld", "zt")
      call Turb%add_var("ep", "zt")
      call Turb%add_var("km", "zt")
      call Turb%add_var("kh", "zt")
      call Turb%add_var("wq", "zt")
      call Turb%add_var("wqi", "zt")
!      call Turb%add_var("rnet", "zt")
      call Turb%add_var("wt", "zt")

      call Met%init('Met.nc', mgr, ngr, mask, lon, lat, zm=zm)
      call Met%add_var("u", "zm")
      call Met%add_var("v", "zm")
      call Met%add_var("t", "zm")
      call Met%add_var("p", "zm")
      call Met%add_var("q", "zm")
      call Met%add_var("qi", "zm")

c===================The Beginning of the Time Integration
      do 9999 jd=1,nds                                       ! Earth days
      jd10=jd/10

      do 9998 jh=1,nhrs                                                    ! Earth hours of each day
      WRITE(6,'(//,10x,"TIME INTEGRATION for Earth Day  ",i2,
     1                   "  and Earth Hour  ",i2)') jd,jh
      do 9997 jm=1,nmts                                                    ! One Earth hour integration

! Increment the calendar-aware time variable
      time = time + timedelta(seconds=int(ds))

      do 9996 igr=1,mgr
      do 9996 jgr=1,ngr

! betag and ustar are in the flxsrf common block and and fc is in the
! consta block. Those need, therefore, to be updated for every grid
! point and at every time step
      betag=grav/t(igr,jgr,1)
      ustar=ustar_2D(igr,jgr)
      fc=2.*angv*SIN(lat(igr,jgr)*rpi)

c---------Calculate celestial mechanics for the current solar day
      ar=slon*rpi                   ! Areocentric longitude in radians
      ar=2*pi*(jd+311)/365          ! Model begins 9th November, Pielke 1984 p211
      rd=1.000110+0.034221*COS(ar)+0.001280*SIN(ar)
     1      +0.000719*COS(2.*ar)+0.000077*SIN(2.*ar)
c	rd=3.03409791/2.+.046215*COS(ar)-.005188*COS(2.*ar)
c     1                  +.134208*SIN(ar)+.004177*SIN(2.*ar)
      ar=slon*rpi
      sdec=.3979486*SIN(ar)                   ! sin(sun decline angle)
      cdec=SQRT(1.-sdec*sdec)                 ! cos(sun decline angle)
      s0c=1373.*rd                      ! current solar flux at TOA
      slon=slon!+.986!.538      ! Areocentric longitude for next solar day
      cc=cdec*COS(lat(igr,jgr)*rpi)
      ss=sdec*SIN(lat(igr,jgr)*rpi)
c
c---------Calculating surface fluxes using Monin-Obukhov similarity
      ha=(1.*jm/nmts+jh-1.)/24.*2.*pi-pi     ! Hour angle in radians
      sh=cc*COS(ha)+ss                   ! Sin of solar height angle
c==============Calculating hydrostatic pressure (mb)
      do j=2,nj
        p(igr,jgr,j)=p(igr,jgr,j-1)
     1      -p(igr,jgr,j-1)/t(igr,jgr,j)*grav/rgas*deta/dedzt(j-1)
        turbhr(j)=t(igr,jgr,j)                           !  Store T for output
      enddo
c==============Calculating boundary layer dynamics
c---------Converting temperature to potential temperature
      do j=1,nj
        theta(j)=t(igr,jgr,j)*(p(igr,jgr,1)/p(igr,jgr,j))**(rgas/cp)
      enddo
      q(igr,jgr,1)=0.01!16351195        ! specified constant ground wetness to q(1)
      theta_out(igr,jgr,:) = theta
c      open (UNIT=55,FILE='q.DAT')
c      write (55,*) q(1)
c      close (55)
c---------Calculating surface fluxes using Monin-Obukhov similarity
c---------theory. It is applied between the surface and gridpoint zm(nw)
c      CALL subsrf(u,v,theta,q,qi,dedzt,zm,zt,e,ep,kh,km,rif,rlmo,tl,
c     1              tld,uw,vw,wt,wq,wqi,rifc,wlo,nj,nw)
      do i=2,nj
        q(igr,jgr,i)=qold(igr,jgr,i)
        qi(igr,jgr,i)=qiold(igr,jgr,i)
      end do
c---------Calculating finite difference matrix and lu decomposition
      wlo=-vk*betag*wt(igr,jgr,1)/ustar_2D(igr,jgr)**3
      CALL coeffi(a,alfa,b,beta,c,d,e(igr,jgr,:),ep(igr,jgr,:),
     1  p(igr,jgr,:),q(igr,jgr,:),qi(igr,jgr,:),theta,u(igr,jgr,:),
     2  v(igr,jgr,:),uw(igr,jgr,:),vw(igr,jgr,:),dedzm,dedzt,rnet,
     3  kh(igr,jgr,:),km(igr,jgr,:),tld(igr,jgr,:),zm,wa,wlo,ipvt,nj,nv,
     4  nw)

c---------Solving tridiagonal equations
      CALL solve(psi,alfa,beta,nv,nj)

c---------Updating the solution
      do 110 j=1,nj
        u(igr,jgr,j)    =psi(j,1)
        v(igr,jgr,j)    =psi(j,2)
        theta(j)=psi(j,3)
        q(igr,jgr,j)    =MAX(0.,psi(j,4))
        qi(igr,jgr,j)   =MAX(0.,psi(j,5))
        e(igr,jgr,j)    =MAX(psi(j,6),emin)
        ep(igr,jgr,j)=(alpha*e(igr,jgr,j))**1.5/tld(igr,jgr,j)
110   CONTINUE
      theta_out(igr,jgr,:) = theta
c
      do i=2,nj
        if (zm(j).gt.10000) then
          e(igr,jgr,:j)=emin
        end if
        q(igr,jgr,i)=qold(igr,jgr,i)
        qi(igr,jgr,i)=qiold(igr,jgr,i)
      end do

c---------Converting potential temperature to the temperature
      do 120 j=1,nj
        t(igr,jgr,j)=theta(j)*(p(igr,jgr,j)/p(igr,jgr,1))**(rgas/cp)
120   CONTINUE

c---------Calculating turbulent length scales, eddy diffusivity & fluxes
      CALL sublkf(u(igr,jgr,:),v(igr,jgr,:),theta,q(igr,jgr,:),
     1  qi(igr,jgr,:),dudz,dvdz,dthdz,dedzt,zm,zt,e(igr,jgr,:),
     2  ep(igr,jgr,:),kh(igr,jgr,:),km(igr,jgr,:),rif,rlmo,tl,
     3  tld(igr,jgr,:),uw(igr,jgr,:),vw(igr,jgr,:),wt(igr,jgr,:),
     4  wq(igr,jgr,:),wqi(igr,jgr,:),rifc,wlo,nj,nw)

c     do i=1,nj
c       ustarp(i)=(uw(i)*uw(i)+vw(i)*vw(i))**.25
c       tstarp(i)=-wt(i)/ustarp(i)
c       qstarp(i)=-wq(i)/ustarp(i)
c       qistarp(i)=-wqi(i)/ustarp(i)
c     end do

      do i=2,nj
        q(igr,jgr,i)=qold(igr,jgr,i)
        qi(igr,jgr,i)=qiold(igr,jgr,i)
      end do

      ustar_2D(igr,jgr)=
     1  (uw(igr,jgr,1)*uw(igr,jgr,1)+vw(igr,jgr,1)*vw(igr,jgr,1))**.25
      tstar=-wt(igr,jgr,1)/ustar_2D(igr,jgr)
      qstar=-wq(igr,jgr,1)/ustar_2D(igr,jgr)
      qistar=-wqi(igr,jgr,1)/ustar_2D(igr,jgr)
      uw0=uw(igr,jgr,1)
      vw0=vw(igr,jgr,1)
      wt0=wt(igr,jgr,1)
      wq0=wq(igr,jgr,1)
      wqi0=wqi(igr,jgr,1)

c==============Calculating radiative heating rates hlw, hsw,
c==============and surface fluxes dlw, dsw, sdir
      q(igr,jgr,1)=q(igr,jgr,2)     ! surface air-q is made equal to first air-level
      albedo1=albedo+.1*(1.-sh)   ! albedo is 10% higher for low sun
      CALL radia(p(igr,jgr,:),q(igr,jgr,:),t(igr,jgr,:),tvis,hsw,hlw,fu,
     1  fd,su,sd,hu,hd,nj,s0c,sh,albedo1,cp,grav,sbc,semis,dlw,dsw,sdir)

      do i=2,nj
        q(igr,jgr,i)=qold(igr,jgr,i)
        qi(igr,jgr,i)=qiold(igr,jgr,i)
      end do

c---------Converting Rnet from T to potential temperature
      do j=2,nj-1
        turbhr(j)=(t(igr,jgr,j)-turbhr(j))/ds ! turbulent heating for output
        rnet(j)=hlw(j)+hsw(j)
c        rnet(j)=(p(1)/p(j))**(rgas/cp)*rnet(j)      ! used in COEFF
        t(igr,jgr,j)=t(igr,jgr,j)+ds*rnet(j)    ! add radiation heating to air temp
      enddo

c==============Calculating waterice cloud formation/sublimation
      qi(igr,jgr,1)=qi(igr,jgr,2)
      CALL swcond(p(igr,jgr,:),q(igr,jgr,:),qi(igr,jgr,:),t(igr,jgr,:),
     1  cp,latent,nj)

      do i=2,nj
        q(igr,jgr,i)=qold(igr,jgr,i)
        qi(igr,jgr,i)=qiold(igr,jgr,i)
      end do

c==============Calculating ground energy fluxes
c---------Computing short wave irradiation on slant ground
      CALL swisg(dsw,ha,rlat,sdec,sdir,sh,swi)
      lw=dlw-sbc*semis*t(igr,jgr,1)**4         ! LW net radiation at surface
      sw=(1.-albedo1)*swi            ! sw net rad at slant sfc, w/m2
      rho=100.*(p(igr,jgr,1)
     1      +p(igr,jgr,2))/rgas/(t(igr,jgr,1)+t(igr,jgr,2))
c      forcing=40.*SIN(ha + 2.*pi/6)
      h0=rho*cp*wt(igr,jgr,1)! + forcing        ! sfc heat flux w/m2
      e0=rho*latent*wq(igr,jgr,1)                ! sfc latent heat flux w/m2
c
      gflux=lw+sw-h0-e0                    ! net surface energy flux

c==============Calculating soil temperature
c
c---------Calculating Boundary-Layer Height
        ss05=.05*SQRT(uw(igr,jgr,1)*uw(igr,jgr,1)
     1          +vw(igr,jgr,1)*vw(igr,jgr,1))
        ssz1=0.
c        ss05=wt(1)
        do 202 j=2,nj-1
          ssz2=.5*SQRT(uw(igr,jgr,j)*uw(igr,jgr,j)
     1           +vw(igr,jgr,j)*vw(igr,jgr,j))
c          ssz2=wt(j)
          IF( (ss05.le.ssz1).and.(ss05.ge.ssz2) ) THEN
            blht=zt(j-1)+(zt(j)-zt(j-1))*(ss05-ssz1)/(ssz2-ssz1)
            GOTO 202
          ENDIF
          ssz1=ssz2
202     CONTINUE
c      PRINT*,t(1)                                                      !**************************************** T ****************************
c      pause
c++++++++++++++Calculating soil temperature
      CALL soiltdm(dedzs,tsoil(igr,jgr,:),zsoil,dzeta,gflux,ds,ni)
      t(igr,jgr,1)=tsoil(igr,jgr,1)
c      PRINT*,'T0=',t(1)

      betag=grav/t(igr,jgr,1)
c
c      p0=p(1)                        !Surface pressure for dust component
c      CALL Dust(ir,nj,ustar,zm,ttd,tvis,Conc1,Km,t,taur
c     1     ,rad,zd,zmd,scaled,dzetad,dlamb,z0,ds,F1,grav,p0,tvisk,value)
c
c      do i=1,nj-1
c        tice(i)=0.001*(qi(i+1)+qi(i))*(100.*p(i+1)/t(i+1)+
c     1             100.*p(i)/t(i))*((zm(i+1)-zm(i))**2)
c      end do
c      do i=nj,1,-1
c        tice(i)=tice(i)+tice(i+1)
c      end do

      ustar_2D(igr,jgr) = ustar

c
 9996 CONTINUE                           ! This is the spatial do-loop

      ttd=ttd+ds

c==============Outputing at time step level

c---------Outputing surface values

!      call SNetRad%append_time(time)
!      call SNetRad%append_var("HLW", hlw(:,:,2))
!      call SNetRad%append_var("HSW", hsw(:,:,2))
!      call SNetRad%append_var("RNet", rnet(:,:,2))

      IF(MOD(jm,jmout).eq.0) then
        call srfv_all%append_time(time)
        call srfv_all%append_var("E0", e(:,:,1))
        call srfv_all%append_var("u*", ustar_2D)
        call srfv_all%append_var("uw", uw(:,:,1))
        call srfv_all%append_var("vw", vw(:,:,1))
        call srfv_all%append_var("wt0", wt(:,:,1))
        call srfv_all%append_var("km", km(:,:,1))
        call srfv_all%append_var("kh", kh(:,:,1))
!        call srfv_all%append_var("1/LO", wlo)
        call srfv_all%append_var("T0", t(:,:,1))
!        call srfv_all%append_var("blht", blht)
      ENDIF

c
 9997 CONTINUE                           ! This is the time-step do-loop
c
c---------Writing hourly data output can be done here
      j10=jh/10
c
      call Turb%append_time(time)
      call Turb%append_var("e", e)
      call Turb%append_var("uw",uw )
      call Turb%append_var("vw", vw)
      call Turb%append_var("tld", tld)
      call Turb%append_var("ep", ep)
      call Turb%append_var("km", km)
      call Turb%append_var("kh", kh)
      call Turb%append_var("wq", wq)
      call Turb%append_var("wqi", wqi)
!      call Turb%append_var("rnet", rnet)
      call Turb%append_var("wt", wt)

      call Met%append_time(time)
      call Met%append_var("u", u)
      call Met%append_var("v", v)
      call Met%append_var("t", t)
      call Met%append_var("p", p)
      call Met%append_var("q", q)
      call Met%append_var("qi", qi)
c
 9998 CONTINUE                        ! This is the Earth hour do-loop

c---------Writing daily output can be done here

 9999 CONTINUE                           ! This is the Solar-day do-step

c
c
      PRINT*,'Program complete'
c      pause
      STOP
      END PROGRAM ABL
