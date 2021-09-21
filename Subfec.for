c=======================================================================
c                          Subroutine SUBFEC                           =
c----------------------------------------------------------------------=
c   Calculating the flux emissivity for CO2 from the emissivity data   =
c tabulated by Staley & Jurica, 1970, J. Applied Meteorol., 9, 365-372 =
c=======================================================================
      SUBROUTINE subfec_org(x,y)
      IMPLICIT none
      REAL x,y
      INTEGER nn,n
      PARAMETER (nn=34)  !23,30
      REAL xop(nn),yem(nn)
c      DATA xop/-4.,-3.7,-3.3,-3.,-2.7,-2.3,-2.,-1.7,-1.3,-1.,-.7,-.3,0.,    !WARNING! Heavily extrapolated
c     1         .3,.7,1.,1.3,1.7,2.,2.3,2.7,3.,3.3,3.7,4.,4.3,4.7,5.,5.3,
c     2         5.7/
c      DATA yem/.00108,.00158,.00248,.00338,.00454,.00672,.00907,.0127,
c     1         .0207,.0303,.0432,.0649,.0823,.1,.124,.141,.158,.18,
c     2         .196,.211,.231,.244,.257,.275,.287,.299,.314,.325,.335,  ,.308,.306,.296
c     3         .349/
      DATA xop/-7.3,-7.,-6.7,-6.3,-6.,-5.7,-5.3,-5.,-4.7,-4.3,-4.,-3.7,   !Extrapolated below -4
     1         -3.3,-3.,-2.7,-2.3,-2.,-1.7,-1.3,-1.,-.7,-.3,0.,.3,.7,1.,
     2         1.3,1.7,2.,2.3,2.7,3.,3.3,3.6/
      DATA yem/.0000355,.0000486,.0000667,.000102,.000139,.000191,
     1         .000291,.000398,.000546,.000832,.00108,.00158,.00248,         !data at T=20C
     1        .00338,.00454,.00672,.00907,.0127,.0207,.0303,.0432,.0649,
     3         .0823,.1,.124,.141,.158,.18,.196,.211,.231,.244,.257,.27/
c      DATA xop/-4.,-3.7,-3.3,-3.,-2.7,-2.3,-2.,-1.7,-1.3,-1.,-.7,-.3,0.,
c     1         .3,.7,1.,1.3,1.7,2.,2.3,2.7,3.,3.3,3.6/
c      DATA yem/.00108,.00158,.00248,.00338,.00454,.00672,.00907,.0127,    !data at T=20C
c     1         .0207,.0303,.0432,.0649,.0823,.1,.124,.141,.158,.18,
c     2         .196,.211,.231,.244,.257,.27/
c
c      DATA yem/.00114,.00162,.00247,.00331,.00439,.00641,.00859,.0119,      !data at T=-70C
c     1         .0192,.0278,.0392,.0576,.0719,.086,.104,.117,.13,.147,
c     2         .159,.17,.186,.197,.207/
c
      IF( (x.lt.-7.3).or.(x.gt.3.6) ) THEN     !gt.3.3
        write(6,'(10x,"The log of optical path is out of the date range
     1...",f10.5)') x
c          pause
        IF(x.gt.3.6) THEN
	  y=yem(nn)+(yem(nn)-yem(nn-1))*(x-xop(nn))/(xop(nn)-xop(nn-1))
	  RETURN
        ELSEIF(x.lt.-7.3) THEN
	  y=0.0765*EXP(1.0515*x)!yem(1)+(yem(1)-yem(2))*(x-xop(1))/(xop(1)-xop(2))
        ENDIF
      ENDIF
      do 10 n=1,nn-1
	if( (x.ge.xop(n)).and.(x.le.xop(n+1)) ) goto 19
 10   continue
 19     y=yem(n)+(yem(n+1)-yem(n))*(x-xop(n))/(xop(n+1)-xop(n))
c
      RETURN
      END
c=======================================================================
c                          Subroutine SUBFEC2                           =
c----------------------------------------------------------------------=
c   Calculating the flux emissivity for Water from the emissivity data =
c tabulated by Staley & Jurica, 1970, J. Applied Meteorol., 9, 365-372 =
c=======================================================================
      SUBROUTINE subfec2_org(x,y)
      IMPLICIT none
      REAL x,y
      INTEGER nn,n
      PARAMETER (nn=31)  !23,30
      REAL xop(nn),yem(nn)
c
      DATA xop/-30,-8,-7.7,-7.3,-7,-6.7,-6.3,-6.,-5.7,-5.3,-5,-4.7,-4.3,
     1         -4.,-3.7,-3.3,-3.,-2.7,-2.3,-2.,-1.7,-1.3,
     2         -1.,-.7,-.3,0.,.3,.7,1.,1.3,1.7/
      DATA yem/.0000000000002126,.00096,.0013,.00195,.00264,.00357,
     1         .00535,.00725,.00981,.0147,.0196,.0262,
     2         .0409,.0565,.0768,.112,.143,.180,.232,.273,
     3         .316,.373,.418,.465,.536,.600,.677,.798,.888,.954,.987/
c
      IF( (x.lt.-31.).or.(x.gt.1.7) ) THEN     !gt.3.3
        write(6,'(10x,"The log of optical path is out of the date range
     1...",f10.5)') x
c          pause
        IF(x.gt.1.7) THEN
	  y=yem(nn)+(yem(nn)-yem(nn-1))*(x-xop(nn))/(xop(nn)-xop(nn-1))
	  RETURN
        ELSEIF(x.lt.-31.) THEN
	  y=yem(1)+(yem(1)-yem(2))*(x-xop(1))/(xop(1)-xop(2))
        ENDIF
      ENDIF
      do 10 n=1,nn-1
	if( (x.ge.xop(n)).and.(x.le.xop(n+1)) ) goto 19
 10   continue
 19     y=yem(n)+(yem(n+1)-yem(n))*(x-xop(n))/(xop(n+1)-xop(n))
c
      RETURN
      END
