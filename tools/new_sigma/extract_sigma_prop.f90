      module input_chi_sigma
      implicit	none
      real(kind(0.d0)),parameter:: pi	=	3.14159265358979323846d0
      complex(kind(0.d0))::	   ci	=	dcmplx(0.d0,1.d0)
      real(kind(0.d0)),parameter:: mpi	=	138.03d0
      real(kind(0.d0)),parameter:: fpi	=	93.d0
      integer, parameter::	   n	=	82
      real(kind(0.d0)),parameter:: c	=	700.d0
      real(kind(0.d0)),parameter:: beta	=	0.2d0
      real(kind(0.d0)),parameter:: mbsig=	900.1d0
      real(kind(0.d0)),parameter:: lam	=	1500.d0**2
      real(kind(0.d0)),parameter:: gpps	=	9.5d0
      real(kind(0.d0)),parameter:: mka  =	495.6d0
      real(kind(0.d0)),parameter:: mnuc = 	938.926d0
      real(kind(0.d0)),parameter:: repol=	468.d0	! Pole poart of sigma pole position
      real(kind(0.d0))		:: kon 		! 1: KKbar is on, 0: Kkbar is not included (USE WHEN FITTING)
      complex(kind(0.d0)),parameter:: lamchi=dcmplx(1130.d0,0.d0) ! Cut-off for unitary chiral model.
      complex(kind(0.d0)),parameter:: zo=dcmplx(467.896d0,-202.568d0)  ! pole position; DEPENDS ON CUT-OFF!
      real(kind(0.d0)),parameter:: r    =	3.d0 ! radius of integration contour of residue calculation in MeV.    
      complex(kind(0.d0)),parameter:: adler=	dcmplx(275.466,0.d0)
      integer,parameter ::	   nch	=	2 ! number of coupled channels
      integer,parameter ::	 simul 	=	0 ! If 1, simulate KKbar in pure pipi by including in inner loops. 
      						  ! Unfortunately, it simply doesn't work (no good fit to data in f_o-region).
      integer	:: switch ! Once this is 1, always add 180 degree to phase shift	
      complex(kind(0.d0)) :: res	! Residue of CHIUPT (calculated onece at beginning of program)
      real(kind(0.d0)),parameter:: phirot=1.1781d0  ! = 3/8 pi angle by which the cut in the energy plane of the pi-pi c.m. scattering
      real(kind(0.d0)),parameter:: kamax =1500.d0  ! upper integration limit for k-integration (is OK, because only difference between 2nd and first sheet is needed).
      						! energy is rotated (-3/8 pi).
      end module input_chi_sigma

!----------------------------------------------
!
!      program	reproduce
!      call testchisigma
!      end !End main program
!
!----------------------------------------------
      
      subroutine testchisigma
      use input_chi_sigma
      implicit	none
!      real(kind(0.d0))::	ene,eneim
      complex(kind(0.d0))::	ecm,diffe,diffsupchiral
      integer	::		i,ene,eneim
      ! The following check with the Mathematica notebook "check_chiral_g.nb" has turned out SUCCESSFULL !!
      open(unit=99,file='check_diffsupchiral.dat')      
      do ene=1300,2100,100
       ecm=dcmplx(ene,100.d0)
       diffe=diffsupchiral(ecm)
       write(99,135)ecm,diffe		   
      end do
      close(99)
      open(unit=99,file='diffsupchiral_plane.dat')      
      do ene=1300,2100,20
      do eneim=10,400,20
       ecm=dcmplx(ene,eneim)
       diffe=diffsupchiral(ecm)
       write(99,135)ecm,diffe		   
      end do
      end do
      close(99)
135   format(15g15.7)      
      end subroutine testchisigma

!----------------------------------------------

      ! sigma N propagator on the first sheet
      function 		snprop1xxchi(ztot)
      use input_chi_sigma
      implicit		none
      real(kind(0.0d0))::	low,up	
      complex(kind(0.0d0))::	ztot,inte,snprop1xxchi,intesnx(2),kcmplx
      real(kind(0.0d0)),dimension(1000)::	xx
      complex(kind(0.0d0)),dimension(1000) :: df 	
      integer			np,kk
      low=0.d0
      up=kamax
      call dsg20R(low,up,7,xx,np)
      do kk=1,np
       kcmplx=dcmplx(xx(kk),0.d0)
       call intesnxx(ztot,kcmplx,intesnx)
       df(kk)=intesnx(1)
      end do
      call drg20c(low,up,7,df,inte)
      snprop1xxchi=inte
      return
      end function snprop1xxchi
	
      function ktu(ztot)
      use input_chi_sigma
      implicit none
      complex(kind(0.d0)) :: ktu,ztot,zprime
      if (dble(ztot).gt.dble(zo)+mnuc) then
       zprime=2.d0*mpi+100.d0+100.d0*ci
      else
       zprime=2.d0*mpi+200.d0+100.d0*ci
      end if
      ktu=sqrt((mnuc**4 + (zprime**2 - ztot**2)**2 - 2.d0*mnuc**2*(zprime**2 + ztot**2))/ztot**2)/2.d0
      return
      end function ktu
	
      function kimzs(ztot)
      use input_chi_sigma
      implicit none
      complex(kind(0.d0)) :: kimzs,ztot
      if (dble(ztot).gt.dble(zo)+mnuc) then
       kimzs=100.d0-500.d0*ci
      else
       kimzs=dcmplx(100.d0,0.d0)
      end if
      return
      end function kimzs
      
      ! sigma N propagator on second sheet (works only in lower z half plane!)
      function 		snprop2xxchi(ztot)
      use input_chi_sigma
      implicit		none
      real(kind(0.0d0))::	low,up,t,ksend	
      complex(kind(0.0d0))::	ztot,inte,snprop2xxchi,ktu,kimzs,k0,k1,k2xx,k0val,k1val,k2val,intesnx(2)
      real(kind(0.0d0)),dimension(1000)::	xx
      complex(kind(0.0d0)),dimension(1000) :: df 	
      integer			np,tt
      ksend=kamax
      low=0.d0
      up=1.d0
      call dsg20R(low,up,5,xx,np)
      do tt=1,np
       t=xx(tt)
       k0  =kimzs(ztot)*t
       k1  =kimzs(ztot)+t*(ktu(ztot)-kimzs(ztot))
       k2xx=ktu(ztot)  +t*(ksend    -  ktu(ztot))
       call intesnxx(ztot,k0,intesnx)
       k0val=intesnx(2)
       call intesnxx(ztot,k1,intesnx)
       k1val=intesnx(2)
       call intesnxx(ztot,k2xx,intesnx)
       k2val=intesnx(2)
       df(tt)=kimzs(ztot) *k0val +(ktu(ztot)-kimzs(ztot))*k1val+(ksend-ktu(ztot))*k2val
      end do
      call drg20c(low,up,5,df,inte)
      snprop2xxchi=inte	
      return
      end function snprop2xxchi
	
      ! WORKS ONLY IN THE UPPER z HALF PLANE
      function		diffsupchiral(ztot)
      use input_chi_sigma
      implicit		none
      complex(kind(0.0d0)):: diffsupchiral,ztot,snprop1xxchi,snprop2xxchi,esig,epion,ppcm,qcmright
      ppcm=qcmright(ztot,2.d0*mpi,mnuc)
      esig= sqrt(ppcm**2+mbsig**2)	! Energy of the sigma meson 
      epion=sqrt(ppcm**2+mpi**2)	! Energy of the pion
!      diffsupchiral=8.d0*pi*esig/(2.d0*epion)**2*Conjg(snprop2xxchi(Conjg(ztot))-snprop1xxchi(Conjg(ztot)))
      diffsupchiral=8.d0*pi/(2.d0)**2*Conjg(snprop2xxchi(Conjg(ztot))-snprop1xxchi(Conjg(ztot)))

	! For checks with Mathematica notebook:
!      write(0,*)'snprop2xxchi(Conjg(ztot))',snprop2xxchi(Conjg(ztot))
!      write(0,*)'snprop1xxchi(Conjg(ztot))',snprop1xxchi(Conjg(ztot))
!      write(0,*)'esig,epion',esig,epion
!      write(0,*)'mpi,mnuc'
!      write(0,*)'diffsupchiral',diffsupchiral
      return
      end function diffsupchiral

!----------------------------------------------

      subroutine intesnxx(ztot,pex,intesnx)
      use input_chi_sigma
      implicit none
      complex(kind(0.d0)) :: intesnx(2),inv_mass,z,ztot,pex,prop
      integer,dimension(nch):: sheet
      z=inv_mass(pex,ztot)
      sheet(1)=1
      sheet(2)=1 			! Don't use on physical axis!
      call selfchi(z,sheet,prop)
!      intesnx(1)=pex**2*prop
      intesnx(1)=pex**2*prop*sqrt(pex**2+mbsig**2)/(pex**2+mpi**2)
      sheet(1)=2
      sheet(2)=2 			
      call selfchi(z,sheet,prop)
!      intesnx(2)=pex**2*prop
      intesnx(2)=pex**2*prop*sqrt(pex**2+mbsig**2)/(pex**2+mpi**2)
      end subroutine intesnxx

!----------------------------------------------
!----------------------------------------------
      
      subroutine selfsigma_chiral(ztot,pex,newprop)
      use input_chi_sigma
      implicit	none
      complex(kind(0.0d0)) :: z,ztot,pex,newprop,prop,esig,epion,inv_mass
      integer,dimension(nch):: sheet
      sheet(1)=2
      sheet(2)=2 			! This is the "second sheet" which in fact is the first sheet on the physical axis (cut rotated by -3/8 pi). Use this to get physical result!!
      esig= sqrt(pex**2+mbsig**2)	! Energy of the sigma meson 
      epion=sqrt(pex**2+mpi**2)		! Energy of the pion
!      z=ztot+450.d0-sqrt(pex**2+450.d0**2)-cdsqrt(938.9260d0**2+pex**2)
!      z=sqrt(ztot**2+mnuc**2-2.d0*ztot*sqrt(mnuc**2+pex**2))
      z=inv_mass(pex,ztot)
      call selfchi(z,sheet,prop)
      newprop=8.d0*pi*esig/(2.d0*epion)**2*prop
      return
      end subroutine selfsigma_chiral

      function inv_mass(pex,ztot)
      use input_chi_sigma
      implicit none
      complex(kind(0.d0)) :: pex,ztot,inv_mass
      inv_mass=sqrt(ztot**2+mnuc**2-2.d0*ztot*sqrt(mnuc**2+pex**2))
      end function inv_mass

!----------------------------------------------

      ! g/(1-vg) from the Chiral unitary amplitude (still that normalization!)
      subroutine selfchi(ecm,sheet,prop)
      use input_chi_sigma
      implicit none
      complex(kind(0.d0)):: ecm,v,g,prop
      integer,dimension(nch):: sheet
      call gandv(ecm,sheet,v,g) 
      prop=g/(1.d0-v*g)
      return
      end subroutine selfchi

!----------------------------------------------

      ! V_pipi and G_pipi
      subroutine gandv(ecm,sheet,v,g)
      use input_chi_sigma
      implicit 	none
      complex(kind(0.d0)),dimension(nch,nch)::vau
      complex(kind(0.d0)):: ecm,v,g,loop
      integer,dimension(nch):: sheet
      call gloop(ecm,2,sheet(2),loop)
      call potential(ecm,vau)     
      v=vau(2,2)
      g=loop
      return
      end

!----------------------------------------------

      ! V
      subroutine potential(ecm,v)
      use input_chi_sigma
      implicit none
      complex(kind(0.d0)):: ecm
      complex(kind(0.d0)),dimension(nch,nch):: v
      v(1,1)=-1.d0/(4.d0*fpi**2)*		(3.d0*ecm**2+4.d0*mka**2-(4.d0*mka**2))
      v(1,2)=-1.d0/(3.d0*sqrt(12.d0)*fpi**2)*	(9.d0/2.d0*ecm**2+3.d0*mka**2+3.d0*mpi**2-3.d0/2.d0*(2.d0*mpi**2+2.d0*mka**2))
      v(2,1)=v(1,2)
      v(2,2)=-1.d0/(9.d0*fpi**2)*		(9.d0*ecm**2+15.d0*mpi**2/2.d0-3.d0*(4.d0*mpi**2))
      return
      end subroutine potential

!----------------------------------------------
      
      ! Loop function G (cut-off scheme)
      subroutine gloop(ecm,k,j,g) ! k: channel, j: sheet (1 or 2)
      use input_chi_sigma
      implicit	none
      complex(kind(0.d0)) ::	ecm,g,inte,qcmright,low,inter,up
      real(kind(0.d0)):: m1,m2
      complex(kind(0.0d0)),dimension(199) :: p,dp,om1,om2     
      integer ::		i,j,k
       if (k.eq.1) then
       m1=mka
       m2=mka
       else
       m1=mpi
       m2=mpi
       end if
      low=0.d0
      if (j==1) then		! First sheet; DO NOT USE AT PHYSICAL AXIS, JUST FOR DIFFSUP.
        inter=lamchi/2.d0
      else if (j==2) then	! "Second" sheet; ON PHYSICAL AXIS, this is first sheet; use this for physical situation.
        if ((tan(phirot)*dble(ecm-m1-m2).gt.-aimag(ecm)).and.(dble(ecm-m1-m2).ge.0.d0)) then
         inter=qcmright(ecm,m1,m2)+dcmplx(0.d0,-100.d0)
        else
         inter=lamchi/2.d0 	! Below pipi threshold, use first sheet.
        end if ! "Second" sheet: right-hand cut rotated to phi=-3/8 pi
      end if   ! Sheet selection j=1 or 2
      up=sqrt(lamchi**2-mka**2)
      call gau_inter(low,inter,up,p,dp,n)
      do i=1,n
       om1(i)=sqrt(m1**2+p(i)**2)
       om2(i)=sqrt(m2**2+p(i)**2)
      end do
      inte=0.d0
      do i=1,n
       inte=inte+p(i)**2*dp(i)*(om1(i)+om2(i))/(om1(i)*om2(i)*(ecm**2-(om1(i)+om2(i))**2))
      enddo
      
      g=1/(2.d0*pi)**2*inte      
      
      return
      end subroutine gloop

!----------------------------------------------

      ! ON-shell three momentum with positive imaginary part (=qon^>)
      function	qcmright(ecm,m1,m2)
      implicit	none
      complex(kind(0.d0))::qcmright,ecm,qq
      real(kind(0.d0))::m1,m2
      qq=1.d0/(2.d0*ecm)*sqrt((ecm**2-(m1+m2)**2)*(ecm**2-(m1-m2)**2))
      if (dble(qq).lt.0.d0) then
      qcmright=-qq
      else
      qcmright= qq
      end if
      return
      end

!----------------------------------------------
      
	! IMPORTANT: ngmax MUST BE EVEN
	subroutine gau_inter(low,inter,up,xg,wg,ngmax)
	implicit   none
	complex(kind(0.d0))::	low,inter,up,XG (199), WG (199)
	real(kind(0.d0))::	xgg (199), wgg (199)
	integer ngmax,i
	wg=0.d0
	call lgauss(xgg,wgg,ngmax/2)
	do i=1,ngmax/2
	xg(i)=low+(inter-low)*(xgg(i)+1.d0)/2.d0
	wg(i)=(inter-low)/2.d0*wgg(i)
	xg(i+ngmax/2)=inter+(up-inter)*(xgg(i)+1.d0)/2.d0
	wg(i+ngmax/2)=(up-inter)/2.d0*wgg(i)	
	end do
	return
	end
