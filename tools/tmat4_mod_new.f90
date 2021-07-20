!=============================================!
!  numerical evaluation of non-pole t matrix  !
!=============================================!

      module tmat4
      use input4
      use cont_unstable
      use potential3
      use steuerung   			!   (label:maketables)
!      use mpi		![UNCOMMENT THIS LINE!]

      contains

!=======================================================================!

      subroutine tmat(ecm,ivb,ive,k_j) ! Here, we introduced an "if" condition to fill the variable "tmatrix" with the interpolated values from the read-in table (label:maketables)
      implicit none 
      integer :: ivb,ive,k_j,iv
      complex(kind(0.d0)) :: ecm
      integer		  :: k,ierr,ic,jc,ni,nj,n1
      n1=n+1
      if (readit==1) then
	    tmatrix=0.
	    do iv=ivb,ive
	    do ic=1,n_c
	    do jc=1,n_c
	    do ni=1,n1
	    do nj=1,n1
	     tmatrix(nj,ni,iv,ic,jc)=ramtab(nj,ni,jc,ic,iv,k_j)
	    end do
	    end do
	    end do
	    end do
	    end do
      
      else				! If "readit" is not one, jus proceed normally and calculate "tmatrix" as before without using the table.
             call tmat_zero 
             call prop_cc(ecm)    ! propagator
	     call qon_c(ecm,qon)  ! on-shell momentum for each channel
             call tmat_gauss(ecm)
             call vmat(ecm,ivb,ive,k_j)  ! potential (non-pole part)
             call tmat_non_pole(ecm,ivb,ive,k_j)
      end if
      end subroutine tmat

!=======================================================================!

      subroutine tmat_zero
      implicit none

      integer :: iv,ix,iy,i1,i2
      integer :: n1

      n1 = n+1
      do iv=1,4
        do i1=1,n_c
          do i2=1,n_c
            do ix=1,n1
              do iy=1,n1
                vmatrix(iv,i1,i2,ix,iy)=(0.d0,0.d0)  
                tmatrix(iy,ix,iv,i1,i2)=(0.d0,0.d0)
              end do
            end do
          end do
        end do
      end do        
      end subroutine tmat_zero

!=======================================================================!

      subroutine tmat_gauss(ecm)
      implicit none
      complex(kind(0.0d0)) :: ecm,diffsupchiral
      real(kind(0.d0))	   :: thres(13)
      integer :: i,ic
      integer :: n1

      n1 = n+1

      do ic=1,n_c
        do i=1,n
          xgau(ic,i) = xgaus(i)
          ygau(ic,i) = xgaus(i)
        end do
	    xgau(ic,n1) = qon(ic)
	    ygau(ic,n1) = qon(ic)
	    u(ic,n1) = (0.d0, 0.d0)
      end do

!==================================================================================
!
!     Second sheet is added here:
!
!==================================================================================
      
      if (rsheet==2) then
       thres( 1) =      ms_b_c( 1)+ms_f_c( 1)	!   mpi+mnu
       thres( 2) = 2.d0*ms_b_c( 1)+ms_f_c( 1)	! 2 mpi+mnu
       thres( 3) = thres(2)
       thres( 4) = thres(2)
       thres( 5) =      ms_b_c( 5)+ms_f_c( 5)	!   meta+mnu
       thres( 6) = thres(2)			! DELTA ON FIRST pipiN SHEET
       thres( 7) = thres(2)
       thres( 8) = thres(2)
       thres(12) =      ms_b_c(12)+ms_f_c(12)	!   mK+mL
       thres(13) =      ms_b_c(13)+ms_f_c(13)	!   mK+mSigma
       
       ! Be careful, the index of qon_c has changed, goes now from 1 to 13,
       ! while before, (3) was, e.g., the etaN channel, now it is (5)!! 
       if (dble(ecm).gt.(thres( 1))) then
       u( 1,n1) = +(0.d0,1.d0)*tpi*qon(1) &	! piN channel 2nd sheet
              & *sqrt(qon( 1)**2+ms_b_c( 1)**2)*sqrt(qon(1)**2+ms_f_c( 1)**2)/ecm 
       end if
       if (dble(ecm).gt.(thres( 2)+20.d0)) then
       u( 2,n1) = diffsupr(ecm,1.d0)		! rhoN channels 2nd sheet
       u( 3,n1) = diffsupr(ecm,1.d0)
       u( 4,n1) = diffsupr(ecm,1.d0)
       end if
       if (dble(ecm).gt.(thres( 5))) then
       u( 5,n1) = +(0.d0,1.d0)*tpi*qon( 5) &	! etaN channel 2nd sheet
              & *sqrt(qon( 5)**2+ms_b_c( 5)**2)*sqrt(qon(5)**2+ms_f_c( 5)**2)/ecm 
       end if
       if (dble(ecm).gt.(thres( 6)+20.d0)) then
       u( 6,n1) = diffsupd(ecm,1.d0)		! piD channels 2nd sheet
       u( 7,n1) = diffsupd(ecm,1.d0)
       end if
       if (dble(ecm).gt.(thres( 8))) then
         if (chiral_sigma==1) then
       	 u( 8,n1) = diffsupchiral(ecm)		! sN channel 2nd sheet, CHIRAL sigma
         else
         u( 8,n1) = diffsup(ecm,1.d0) 		! sN channel 2nd sheet, NON-CHIRAL sigma
         end if
       end if 
       if (dble(ecm).gt.(thres(12))) then
       u(12,n1) = +(0.d0,1.d0)*tpi*qon(12) &	! KL channel 2nd sheet
              & *sqrt(qon(12)**2+ms_b_c(12)**2)*sqrt(qon(12)**2+ms_f_c(12)**2)/ecm 
       end if
       if (dble(ecm).gt.thres(13)) then
       u(13,n1) = +(0.d0,1.d0)*tpi*qon(13) &	! KS channel 2nd sheet				SHOULD GT, DONT FORGET TO CHANGE BACK!!!!
              & *sqrt(qon(13)**2+ms_b_c(13)**2)*sqrt(qon(13)**2+ms_f_c(13)**2)/ecm 
       end if
      end if 				! (rsheet==2) Sheet 2
      end subroutine tmat_gauss

!=======================================================================!

      subroutine prop_cc(ecm)
      implicit none

      complex(kind(0.d0)) :: ecm,cecm,epsilo,e1,e2,xg2,sigrho,signuc,sigdel,sigsig,sigd13,sigp11, e1pi0p, e2pi0p, e1pipn, e2pipn
      integer :: i,j
      integer :: n1

      cecm=ecm
      
      if (finepsi==1) then
      epsilo=dcmplx(0.d0,0.1d0) ! besser 10.d0? 12/16/2011
      else
      epsilo=dcmplx(0.d0,0.d0)
      end if
      
      ! compute propagator for all channels
      ! the sigmaN, rhoN and piDelta propagators are modified by including selfenergy terms
	  ! this is done in order to treat these states as effective pipiN states
      do i=1,n
         xg2 = xgaus(i)*xgaus(i)
         e1 = cdsqrt(xg2+ms_f_c(1)**2)
         e2 = cdsqrt(xg2+ms_b_c(1)**2)

         e1pi0p = cdsqrt(xg2 + 938.272046d0**2) ! used in photo observables where isospin corrections are included
         e1pipn = cdsqrt(xg2 + 939.565379d0**2)
         
         e2pi0p = cdsqrt(xg2 + 134.9766d0**2) ! used in photo observables where isospin corrections are included
         e2pipn = cdsqrt(xg2 + 139.57018d0**2)
         
!         u(1,i) =wgaus(i)*xg2/(ecm-e1-e2+epsilo)    ! piN prop with small imaginary part
!         u(1,i) =wgaus(i)*xg2/(ecm-e1pi0p-e2pi0p+epsilo)    ! piN prop with small imaginary part
	 if (isoon==0) then
           u(1,i) =wgaus(i)*xg2/(ecm-e1pipn-e2pipn+epsilo)    ! piN prop with small imaginary part
           g_pi0p(1,i) =wgaus(i)*xg2/(ecm-e1pi0p-e2pi0p+epsilo)    ! pi0 p prop, used in obs_photo
           g_pipn(1,i) =wgaus(i)*xg2/(ecm-e1pipn-e2pipn+epsilo)    ! pi+ n prop, used in obs_photo        
	 else
           u(1,i) =wgaus(i)*xg2/(ecm-e1-e2+epsilo)    ! piN prop with small imaginary part
           g_pi0p(1,i) =u(1,i)    ! pi0 p prop, used in obs_photo
           g_pipn(1,i) =u(1,i)    ! pi+ n prop, used in obs_photo	 
	 end if
         
         
         call selfrho(cecm,xgaus(i),sigrho)
         e1 = cdsqrt(xg2+ms_f_c(2)**2)
         e2 = cdsqrt(xg2+mbr*mbr)    ! bare rho mass
         u(2,i) =wgaus(i)*xg2/(ecm-e1-e2-sigrho)    ! rhoN prop
         u(3,i)=u(2,i)
         u(4,i)=u(2,i)

         e1 = cdsqrt(xg2+ms_f_c(5)**2)
         e2 = cdsqrt(xg2+ms_b_c(5)**2)
         u(5,i) =1.d0*  wgaus(i)*xg2/(ecm-e1-e2+epsilo)    ! etaN prop !!! SET TO 5 !!!

         call selfdelta(ecm,xgaus(i),sigdel)
         e1 = cdsqrt(xg2+mbd*mbd)    ! bare delta
         e2 = cdsqrt(xg2+ms_b_c(6)**2)
         u(6,i) =wgaus(i)*xg2/(ecm-e1-e2-sigdel)    ! piD prop
         u(7,i) = u(6,i)

         if (chiral_sigma==1) then
	  call selfsigma_chiral(cecm,xgaus(i),sigsig)
          u(8,i) = wgaus(i)*xg2*sigsig    ! sigsig is g/(1/vg) x normalization
	 else
	  call selfsigma(cecm,xgaus(i),sigsig)
          e1 = cdsqrt(xg2+mbs*mbs)    ! bare sigma
          e2 = cdsqrt(xg2+ms_f_c(8)**2)
          u(8,i) =wgaus(i)*xg2/(ecm-e1-e2-sigsig)    ! sigmaN prop
	 end if

         e1 = cdsqrt(xg2+ms_f_c(9)**2)
         e2 = cdsqrt(xg2+ms_b_c(9)**2)
         u(9,i) =wgaus(i)*xg2/(ecm-e1-e2)    ! omegaN prop
	     u(10,i) = u(9,i)
	     u(11,i) = u(9,i)

         e1 = cdsqrt(xg2+ms_f_c(12)**2)
         e2 = cdsqrt(xg2+ms_b_c(12)**2)
         u(12,i) =wgaus(i)*xg2/(ecm-e1-e2+epsilo)    ! LambdaK prop

         e1 = cdsqrt(xg2+ms_f_c(13)**2)
         e2 = cdsqrt(xg2+ms_b_c(13)**2)
         u(13,i) =wgaus(i)*xg2/(ecm-e1-e2+epsilo)    ! SigmaK prop
	
	 ! Switch off KY propagators:
	
	 if (switchon_KY==0) then
	  do j=12,13
	  u(j,i) =dcmplx(0.d0,0.d0)
	  end do
	 end if

      end do

      end subroutine  prop_cc

!=======================================================================!

      subroutine selfsigma(ztot,pex,self)  ! calculating the sigma selfenergy for a sigma in rest
      implicit none                        ! a modified on shell energy for the sigma (see Schuetz Diss)

      logical :: lfirst = .true.
      integer :: n,n1,i,kwrite
      real(kind(0.d0)) ::  c,beta,wpiq,rd,wpi
      real(kind(0.d0)), dimension(48) :: rquad,rwght,requad,rewght
      real(kind(0.d0)) :: twopi3,gpps,lam,msig,mbsig,ffz,del1,del2,delta
      complex(kind(0.d0)),dimension(49) :: p,dp,eq,ver,prop
      complex(kind(0.d0)) :: ff,self,tau,tmat,scms,pex,esig,z,ztot
      save :: p,dp,n,n1,twopi3,gpps,lam,ffz,wpiq,msig,mbsig,kwrite,lfirst,wpi,rd

      if (lfirst) then
        lfirst=.false.
        kwrite=99
        n=48
        N1=N+1
        c=700.d0
        beta=0.2d0
        RD=90.d0/PIH
        twopi3=(2.d0*pi)**3
        gpps=9.5d0
        lam=1500.d0**2
        WPIQ=138.03d0**2
        wpi=138.03d0
        msig=850.d0
        mbsig=900.1d0
        ffz=lam  ! ffz=lam+msig**2
        call gauss_tan(n,c,beta,p,dp)  !  get gauss points and weights
      end if

      esig=cdsqrt(pex**2+mbsig**2)
      z=ztot+mbsig-esig-cdsqrt(938.9260d0**2+pex**2)

      !  calculate vertexfunction for rho polegraph
      do i=1,n
         eq(i)=cdsqrt(p(i)**2+wpiq)
         ff=ffz/(lam+p(i)**2)
         ver(i)=gpps*dsqrt(3.d0)*wpi*ff/(pi*2.d0*eq(i)*cdsqrt(esig))
      end do

      !  calculate selfenergy 
      self=cmplx(0.d0,0.d0)
      do i=1,n
         self=self+p(i)*p(i)*dp(i)*ver(i)*ver(i)/(z-2.d0*eq(i))
      end do

      return
      end subroutine selfsigma

!=======================================================================!

      subroutine selfrho(ztot,pex,self)
      implicit none 

      logical :: lfirst = .true.
      integer :: n,n1,i,kwrite
      real(kind(0.d0)) ::  c,beta,wpiq,rd
      real(kind(0.d0)), dimension(96) :: rquad,rwght,requad,rewght
      real(kind(0.d0)) :: twopi3,gppr,lam,mrho,mbrho,twosrt23,ffz,del1,del2,delta,ffpi 
      complex(kind(0.d0)),dimension(97) :: p,dp,eq,ver,prop
      complex(kind(0.d0)) :: ff,selfz,self,tau,tmat,scms,pex,z,ztot
      save :: selfz,p,dp,ver,prop,n,n1,twopi3,twosrt23,gppr,lam,ffz,wpiq,mrho,mbrho,eq

      if (lfirst) then
        lfirst=.false.
        kwrite=99
        n=96
        N1=N+1
        c=1500.d0
        beta=0.3d0
        RD=90.d0/PIH
        twopi3=(2.d0*pi)**3
        twosrt23=dsqrt(pi/3.d0)*4.d0
        gppr=dsqrt(4.d0*pi*2.9d0)
        lam=1800.d0**2
        WPIQ=138.03d0**2
        mrho=770.d0
        mbrho=911.1d0
        ffz=lam+mrho**2
        call gauss_tan(n,c,beta,p,dp)
      end if

      z=ztot+mbrho-cdsqrt(pex**2+mbrho**2)-cdsqrt(938.9260d0**2+pex**2)

      !  calculate vertexfunction for rho polegraph
      do i=1,n
        eq(i)=cdsqrt(p(i)**2+wpiq)
        ff=ffz/(lam+4.d0*eq(i)**2)
        ver(i)=gppr*twosrt23*p(i)*ff/(2.d0*eq(i)*cdsqrt(twopi3*2.d0*cdsqrt(mbrho**2+pex**2)))
      end do

      !  calculate selfenergy 
      self=cmplx(0.d0,0.d0)
      do i=1,n
        self=self+p(i)**2*dp(i)*(ver(i))**2/(z-2.d0*eq(i))
      end do

      return
      end subroutine selfrho

!=======================================================================!

      subroutine selfdelta(ztot,pex,self)
      implicit none 

      logical :: lfirst = .true.
      integer :: ns,ns1,i
      real(kind(0.d0)) ::  cs,betas,wpiq,wnuq,wpi,wnu
      real(kind(0.d0)) :: fcoup,lam,mdelt,mdelto,ffz
      complex(kind(0.d0)) :: ztot 
      complex(kind(0.d0)),dimension(48) :: p,dp,ver,ff
      complex(kind(0.d0)),dimension(48) :: epi,enuk
      complex(kind(0.d0)) :: self,pex,z
      save :: p,dp,ns,ns1,fcoup,lam,ffz,wpiq,wpi,wnuq,wnu,mdelt,mdelto,lfirst

      if (lfirst) then
        lfirst=.false.
        ns=32
        Ns1=Ns+1
        cs=700.d0
        betas=0.5d0
        ! Fit of Schuetz (see Diss) 
        fcoup=0.36d0
        lam=1500.d0
        mdelt=1232.d0
        mdelto=1415.1d0
        WPI=138.03d0
        WPIQ=138.03d0**2
        wnu=938.926d0
        wnuq=938.926d0**2
        ffz=lam**4+mdelt**4
        call gauss_tan(ns,cs,betas,p,dp)
      end if

      z=ztot+mdelto-cdsqrt(pex**2+mdelto**2)-cdsqrt(wpiq+pex**2)
      do i=1,ns
        epi(i)=cdsqrt(p(i)**2+wpiq)
        enuk(i)=cdsqrt(p(i)**2+wnuq)
      end do

      ! calculate formfactor
      do i=1,ns
        ff(i)=ffz/(lam**4+(enuk(i)+epi(i))**4)
      end do

      !  calculate vertexfunction for delta polegraph
      do i=1,ns
        ver(i)=dsqrt(fcoup/6.d0/pi)/wpi*p(i)*ff(i)*cdsqrt((enuk(i)+wnu)/enuk(i)/epi(i))
      end do

      !  calculate selfenergy 
      self=cmplx(0.d0,0.d0)
      do i=1,ns
        self=self+p(i)**2*dp(i)*(ver(i))**2/(z-enuk(i)-epi(i))
      end do

      return
      end subroutine selfdelta

!=======================================================================!

      subroutine vmat(ecm,ivb,ive,k_j)
      implicit none

      integer :: ivb,ive,k_j
      complex(kind(0.d0)) :: ecm
      complex(kind(0.d0)) :: cecm
      complex(kind(0.d0)) :: xmev,ymev
      integer :: iv,ix,iy,i1,i2
      integer :: n1
      complex(kind(0.d0)), dimension(36) :: v

      n1 = n+1
      cecm=ecm

! diagonal part: piN,etaN,rhoN,sigN,piDelta,omegaN,LamK,SigK
      do ix=1,n1
        do iy=ix,n1
          xmev=xgau(1,ix)
          ymev=xgau(1,iy)
          call ccpot(v,ymev,xmev,ecm,1,1)
          do iv=ivb,ive
             vmatrix(iv,1,1,ix,iy)=v(iv)   ! piN->piN
             vmatrix(iv,1,1,iy,ix)=v(iv)
          end do
          xmev=xgau(5,ix)
          ymev=ygau(5,iy)
          call ccpot(v,ymev,xmev,ecm,3,3)
          do iv=ivb,ive
             vmatrix(iv,5,5,ix,iy)=v(iv)   ! etaN->etaN
             vmatrix(iv,5,5,iy,ix)=v(iv)
          end do
          xmev=xgau(2,ix)
          ymev=ygau(2,iy)
          call ccpot(v,ymev,xmev,ecm,2,2)
          do iv=ivb,ive
             vmatrix(iv,2,2,ix,iy)=v(iv)    ! rhoN->rhoN <j-+1/2,1/2| |j-+1/2,1/2>
             vmatrix(iv,2,2,iy,ix)=v(iv)
             vmatrix(iv,3,3,ix,iy)=v(iv+12) ! rhoN->rhoN <j-+1/2,3/2| |j-+1/2,3/2>
             vmatrix(iv,3,3,iy,ix)=v(iv+12)
             vmatrix(iv,4,4,ix,iy)=v(iv+20) ! rhoN->rhoN <j+-3/2,3/2| |j+-3/2,3/2>
             vmatrix(iv,4,4,iy,ix)=v(iv+20)

             vmatrix(iv,3,2,ix,iy)=v(iv+4)  ! rhoN->rhoN <j-+1/2,3/2| |j-+1/2,1/2>
             vmatrix(iv,3,2,iy,ix)=v(iv+24)
             vmatrix(iv,2,3,ix,iy)=v(iv+24)
             vmatrix(iv,2,3,iy,ix)=v(iv+4)

             vmatrix(iv,4,2,ix,iy)=v(iv+8)  ! rhoN->rhoN <j+-3/2,3/2| |j-+1/2,1/2>
             vmatrix(iv,4,2,iy,ix)=v(iv+28)
             vmatrix(iv,2,4,ix,iy)=v(iv+28)
             vmatrix(iv,2,4,iy,ix)=v(iv+8)

             vmatrix(iv,4,3,ix,iy)=v(iv+16) ! rhoN->rhoN <j+-3/2,3/2| |j-+1/2,3/2>
             vmatrix(iv,4,3,iy,ix)=v(iv+32)
             vmatrix(iv,3,4,ix,iy)=v(iv+32)
             vmatrix(iv,3,4,iy,ix)=v(iv+16)
          end do
          xmev=xgau(6,ix)
          ymev=ygau(6,iy)
          call ccpot(v,ymev,xmev,ecm,4,4)
          do iv=ivb,ive
             vmatrix(iv,6,6,ix,iy)=v(iv)    ! piD->piD <j-+1/2,3/2| |j-+1/2,3/2>
             vmatrix(iv,6,6,iy,ix)=v(iv)
             vmatrix(iv,7,7,ix,iy)=v(iv+8)  ! piD->piD <j+-3/2,3/2| |j+-3/2,3/2>
             vmatrix(iv,7,7,iy,ix)=v(iv+8)

             vmatrix(iv,7,6,ix,iy)=v(iv+4)  ! piD->piD <j+-3/2,3/2| |j-+1/2,3/2>
             vmatrix(iv,7,6,iy,ix)=v(iv+12)
             vmatrix(iv,6,7,ix,iy)=v(iv+12)
             vmatrix(iv,6,7,iy,ix)=v(iv+4)
          end do
          xmev=xgau(8,ix)
          ymev=ygau(8,iy)
          call ccpot(v,ymev,xmev,ecm,5,5)
          do iv=ivb,ive
             vmatrix(iv,8,8,ix,iy)=v(iv)    ! sigN->sigN
             vmatrix(iv,8,8,iy,ix)=v(iv)
          end do
          xmev=xgau(9,ix)
          ymev=ygau(9,iy)
          call ccpot(v,ymev,xmev,ecm,6,6)
          do iv=ivb,ive
             vmatrix(iv,9,9,ix,iy)=v(iv)      ! omgN->omgN <j-+1/2,1/2| |j-+1/2,1/2>
             vmatrix(iv,9,9,iy,ix)=v(iv)
             vmatrix(iv,10,10,ix,iy)=v(iv+12) ! omgN->omgN <j-+1/2,3/2| |j-+1/2,3/2>
             vmatrix(iv,10,10,iy,ix)=v(iv+12)
             vmatrix(iv,11,11,ix,iy)=v(iv+20) ! omgN->omgN <j+-3/2,3/2| |j+-3/2,3/2>
             vmatrix(iv,11,11,iy,ix)=v(iv+20)

             vmatrix(iv,10,9,ix,iy)=v(iv+4)   ! omgN->omgN <j-+1/2,3/2| |j-+1/2,1/2>
             vmatrix(iv,10,9,iy,ix)=v(iv+24)
             vmatrix(iv,9,10,ix,iy)=v(iv+24)
             vmatrix(iv,9,10,iy,ix)=v(iv+4)

             vmatrix(iv,11,9,ix,iy)=v(iv+8)   ! omgN->omgN <j+-3/2,3/2| |j-+1/2,1/2>
             vmatrix(iv,11,9,iy,ix)=v(iv+28)
             vmatrix(iv,9,11,ix,iy)=v(iv+28)
             vmatrix(iv,9,11,iy,ix)=v(iv+8)

             vmatrix(iv,11,10,ix,iy)=v(iv+16) ! omgN->omgN <j+-3/2,3/2| |j-+1/2,3/2>
             vmatrix(iv,11,10,iy,ix)=v(iv+32)
             vmatrix(iv,10,11,ix,iy)=v(iv+32)
             vmatrix(iv,10,11,iy,ix)=v(iv+16)
          end do
          xmev=xgau(12,ix)
          ymev=xgau(12,iy)
          call ccpot(v,ymev,xmev,ecm,7,7)
          do iv=ivb,ive
             vmatrix(iv,12,12,ix,iy)=v(iv)    ! LamK->LamK
             vmatrix(iv,12,12,iy,ix)=v(iv)
          end do
          xmev=xgau(13,ix)
          ymev=xgau(13,iy)
          call ccpot(v,ymev,xmev,ecm,8,8)
          do iv=ivb,ive
             vmatrix(iv,13,13,ix,iy)=v(iv)    ! SigK->SigK
             vmatrix(iv,13,13,iy,ix)=v(iv)
          end do
        end do

! off-diagonal parts
        do iy=1,n1
          xmev=xgau(2,ix)  ! N rho
          ymev=ygau(1,iy)  ! N pi
          call ccpot(v,ymev,xmev,ecm,1,2)
          do iv=ivb,ive
             vmatrix(iv,2,1,ix,iy)=v(iv)   ! piN->rhoN <j-+1/2,1/2| |j-+1/2,1/2>
             vmatrix(iv,1,2,iy,ix)=-v(iv)
             vmatrix(iv,3,1,ix,iy)=v(iv+4) ! piN->rhoN <j-+1/2,3/2| |j-+1/2,1/2>
             vmatrix(iv,1,3,iy,ix)=-v(iv+4)
             vmatrix(iv,4,1,ix,iy)=v(iv+8)*gammacc  ! piN->rhoN <j+-3/2,3/2| |j-+1/2,1/2>
             vmatrix(iv,1,4,iy,ix)=-v(iv+8)*gammacc
             ! the potential V has to be hermitian (to have unitariy) therefore we have to
             ! multiply the transition potential piN -> rhoN with a - sign because it is imaginary
          end do
          xmev=xgau(6,ix)  ! Delta pi
          ymev=ygau(1,iy)  ! N pi
          call ccpot(v,ymev,xmev,ecm,1,4)
          do iv=ivb,ive
             vmatrix(iv,6,1,ix,iy)=v(iv)   ! piN->piD <j-+1/2,3/2| |j-+1/2,1/2>
             vmatrix(iv,1,6,iy,ix)=v(iv)
             vmatrix(iv,7,1,ix,iy)=v(iv+4) ! piN->piD <j+-3/2,3/2| |j-+1/2,3/2>
             vmatrix(iv,1,7,iy,ix)=v(iv+4)
          end do
          xmev=xgau(5,ix)  ! N eta
          ymev=ygau(1,iy)  ! N pi
          call ccpot(v,ymev,xmev,ecm,1,3)
          do iv=ivb,ive
             vmatrix(iv,5,1,ix,iy)=v(iv)   ! piN->etaN
             vmatrix(iv,1,5,iy,ix)=v(iv)
          end do
          xmev=xgau(6,ix)  ! Delta pi
          ymev=ygau(2,iy)  ! N rho
          call ccpot(v,ymev,xmev,ecm,2,4)
          do iv=ivb,ive
             vmatrix(iv,6,2,ix,iy)=v(iv)   ! rhoN->piD
             vmatrix(iv,2,6,iy,ix)=-v(iv)
             vmatrix(iv,6,3,ix,iy)=v(iv+4)
             vmatrix(iv,3,6,iy,ix)=-v(iv+4)
             vmatrix(iv,6,4,ix,iy)=v(iv+8)
             vmatrix(iv,4,6,iy,ix)=-v(iv+8)

             vmatrix(iv,7,2,ix,iy)=v(iv+12)
             vmatrix(iv,2,7,iy,ix)=-v(iv+12)
             vmatrix(iv,7,3,ix,iy)=v(iv+16)
             vmatrix(iv,3,7,iy,ix)=-v(iv+16)
             vmatrix(iv,7,4,ix,iy)=v(iv+20)
             vmatrix(iv,4,7,iy,ix)=-v(iv+20)
          end do
          xmev=xgau(12,ix)  ! Lambda K
          ymev=ygau(5,iy)   ! N eta
          call ccpot(v,ymev,xmev,ecm,3,7)
          do iv=ivb,ive
             vmatrix(iv,12,5,ix,iy)=v(iv) ! etaN->LamK 
             vmatrix(iv,5,12,iy,ix)=v(iv)
          end do
          xmev=xgau(13,ix)  ! Sigma K
          ymev=ygau(5,iy)   ! N eta
          call ccpot(v,ymev,xmev,ecm,3,8)
          do iv=ivb,ive
             vmatrix(iv,13,5,ix,iy)=v(iv) ! etaN->SigK 
             vmatrix(iv,5,13,iy,ix)=v(iv)
          end do   
          xmev=xgau(6,ix)   ! Delta pi
          ymev=ygau(8,iy)   ! N sigma
          call ccpot(v,ymev,xmev,ecm,5,4)
          do iv=ivb,ive
             vmatrix(iv,6,8,ix,iy)=v(iv)   ! sigN->piD
             vmatrix(iv,8,6,iy,ix)=-v(iv)
             vmatrix(iv,7,8,ix,iy)=v(iv+4)
             vmatrix(iv,8,7,iy,ix)=-v(iv+4)
          end do
          xmev=xgau(8,ix)   ! N sigma
          ymev=ygau(1,iy)   ! N pi
          call ccpot(v,ymev,xmev,ecm,1,5)
          do iv=ivb,ive
             vmatrix(iv,8,1,ix,iy)=v(iv)   ! piN->sigN
             vmatrix(iv,1,8,iy,ix)=-v(iv)
          end do
          xmev=xgau(12,ix)  ! Lambda K
          ymev=ygau(1,iy)   ! N pi
          call ccpot(v,ymev,xmev,ecm,1,7)
          do iv=ivb,ive
             vmatrix(iv,12,1,ix,iy)=v(iv)  ! piN->LamK 
             vmatrix(iv,1,12,iy,ix)=v(iv)
          end do
          xmev=xgau(13,ix)  ! Sigma K
          ymev=ygau(1,iy)   ! N pi
          call ccpot(v,ymev,xmev,ecm,1,8)
          do iv=ivb,ive
             vmatrix(iv,13,1,ix,iy)=v(iv)  ! piN->SigK 
             vmatrix(iv,1,13,iy,ix)=v(iv)
          end do
          xmev=xgau(13,ix)   ! Sigma K
          ymev=ygau(12,iy)   ! Lambda K
          call ccpot(v,ymev,xmev,ecm,7,8)
          do iv=ivb,ive
             vmatrix(iv,13,12,ix,iy)=v(iv)  ! LamK->SigK 
             vmatrix(iv,12,13,iy,ix)=v(iv)
          end do
          xmev=xgau(2,ix)   ! N rho
          ymev=ygau(12,iy)  ! Lambda K
          call ccpot(v,ymev,xmev,ecm,7,2)
          do iv=ivb,ive
             vmatrix(iv,2,12,ix,iy)=v(iv)   ! LamK->rhoN <j-+1/2,1/2| |j-+1/2,1/2>
             vmatrix(iv,12,2,iy,ix)=-v(iv)
             vmatrix(iv,3,12,ix,iy)=v(iv+4) ! LamK->rhoN <j-+1/2,3/2| |j-+1/2,1/2>
             vmatrix(iv,12,3,iy,ix)=-v(iv+4)
             vmatrix(iv,4,12,ix,iy)=v(iv+8)*gammacc  ! LamK->rhoN <j+-3/2,3/2| |j-+1/2,1/2>
             vmatrix(iv,12,4,iy,ix)=-v(iv+8)*gammacc
             ! the potential V has to be hermitian (to have unitariy) therefore we have to
             ! multiply the transition potential LamK -> rhoN with a - sign because it is imaginary
          end do
          xmev=xgau(6,ix)   ! Delta pi
          ymev=ygau(12,iy)  ! Lambda K
          call ccpot(v,ymev,xmev,ecm,7,4)
          do iv=ivb,ive
             vmatrix(iv,6,12,ix,iy)=v(iv)    ! LamK->piD <j-+1/2,3/2| |j-+1/2,1/2>
             vmatrix(iv,12,6,iy,ix)=v(iv)
             vmatrix(iv,7,12,ix,iy)=v(iv+4)  ! LamK->piD <j+-3/2,3/2| |j-+1/2,3/2>
             vmatrix(iv,12,7,iy,ix)=v(iv+4)
          end do
        end do

      end do

      end subroutine vmat

!=======================================================================!

      subroutine tmat_non_pole(ecm,ivb,ive,k_j)
      implicit none

      integer,parameter :: maxdim = 923   ! n_c*(max_q+1), 923 for 70 points
      integer, parameter :: wspace = maxdim*maxdim + 4*maxdim
      real(kind(0.d0)) :: error
      complex(kind(0.d0)) :: work(wspace)
      integer :: ivb,ive,k_j
      complex(kind(0.d0)) :: ecm
      integer :: iv,ix,iy,i1,i2,i,ic,xindex,yindex
      integer :: n1,ndi
      complex(kind(0.d0)), dimension(36) :: v
      complex(kind(0.d0)), dimension(maxdim,maxdim) :: a,ainv,ccmat,bbc

      n1 = n+1
      ndi = n_c*n1
      do iv=ivb,ive

        ! build (1-VG) matrix
        do i1=1,n_c
           do ix=1,n1
              xindex=ix+(i1-1)*n1
              do i2=1,n_c
                 do iy=1,n1
                    yindex=iy+(i2-1)*n1
                    a(xindex,yindex)= - vmatrix(iv,i1,i2,ix,iy)*u(i2,iy)
                    ccmat(xindex,yindex)=vmatrix(iv,i1,i2,ix,iy)
                 end do
              end do
           end do
        end do

        do i1=1,n_c
           do ix=1,n1
              xindex=ix+(i1-1)*n1
              a(xindex,xindex) = a(xindex,xindex) + 1.d0
           end do
        end do

        ! imsl invert-routine, attention will not work in debug mode!!!
        ! LOCAL        call dlincg(ndi,a,maxdim,ainv,maxdim)
        ! invert from unilib ! USE LOCAL PROGRAMS
        call invert(a, ndi, maxdim, ainv, maxdim, work, .true., error)

        ! multiply with V to get tmatrix (imsl routine)
        !LOCAL        CALL dMCRCR(ndi,ndi,ainv,maxdim,ndi,ndi,ccmat,maxdim,&
        !LOCAL         &ndi,ndi,bbc,maxdim)
        !         CALL dMCRCR(ndi,ndi,ainv,maxdim,ndi,ndi,ccmat,maxdim,&
        !          &ndi,ndi,bbc,maxdim)

        ! multiply by hand: ! LOCAL PROGRAM BEGIN
        do ix = 1,ndi
           do iy = 1,ndi
              bbc(ix,iy)=(0.d0,0.d0)
              do ic = 1,ndi
                 bbc(ix,iy) = bbc(ix,iy) + ainv(ix,ic)*ccmat(ic,iy)
              end do
           end do
        end do

        ! store T-matrix
        do i1=1,n_c
           do ix=1,n1
              xindex=ix+(i1-1)*n1
              do i2=1,n_c
                 do iy=1,n1
                    yindex=iy+(i2-1)*n1
                    tmatrix(iy,ix,iv,i1,i2)=bbc(xindex,yindex)
                 end do !iy
              end do !i2
           end do !ix
        end do !i1

      end do !iv 

      end subroutine tmat_non_pole

!=======================================================================!

      end module tmat4
