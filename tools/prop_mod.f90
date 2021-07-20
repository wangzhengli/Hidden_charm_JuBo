! MODULE pot_prop_mod V2.0 (16.9.97)

      module prop
! this module contains most of the propagators, spinors, polarisation vectors and vertices
! needed in the piN coupled channel potential

! parameters:
      real(kind(0.0d0)), dimension(0:3), parameter :: metric = (/1.0d0,-1.0d0,-1.0d0,-1.0d0/)
      real(kind(0.0d0)), dimension(0:3,0:3), parameter :: gmunu=reshape(&
      &(/1.0d0,0.0d0,0.0d0,0.0d0,&
      &0.0d0,-1.0d0,0.0d0,0.0d0,&
      &0.0d0,0.0d0,-1.0d0,0.0d0,&
      &0.0d0,0.0d0,0.0d0,-1.0d0/),(/4,4/))
      real(kind(0.0d0)), dimension(1:4,1:4), parameter :: one4=reshape(&
      &(/1.0d0,0.0d0,0.0d0,0.0d0,&
      &0.0d0,1.0d0,0.0d0,0.0d0,&
      &0.0d0,0.0d0,1.0d0,0.0d0,&
      &0.0d0,0.0d0,0.0d0,1.0d0/),(/4,4/))
      complex(kind(0.0d0)), dimension(1:4,1:4,0:4), parameter :: gamma=reshape(&
      &(/(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),&
      &(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),&
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),&    ! gamma0
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),&    !______   It seems that gamma1 and gamma3 have additional minus sign.
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),&    !         however, this will produce the right (BD, Mandl&Shaw) gamma
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),&    !         matrices: gamma(zeile,spalte,lorentzindex)
      &(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),&     ! gamma1  the reason for this is: the reshape command stores the
      &(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),&     !______   collums. So you have to store here transposed(gamma)
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),&
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),&
      &(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),&     ! gamma2
      &(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),&
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),&
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),&
      &(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),&     ! gamma3
      &(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),&
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),&
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),&
      &(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),&     ! gamma5
      &(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0)&
      &/),(/4,4,5/))

! looks chaotic but only 19 following lines are allowed in fixed mode
      complex(kind(0.0d0)), dimension(1:4,1:4,0:3,0:3), parameter :: sig=reshape((/&
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=0,nu=0)
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=1,nu=0)
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=2,nu=0)
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=3,nu=0)
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=0,nu=1)
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=1,nu=1)
      &(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),& !sig(mu=2,nu=1)
      &(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),& !sig(mu=3,nu=1)
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=0,nu=2)
      &(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),& !sig(mu=1,nu=2)
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=2,nu=2)
      &(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(-1.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=3,nu=2)
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=0,nu=3)
      &(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,-1.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,1.0d0),(0.0d0,0.0d0),& !sig(mu=1,nu=3)
      &(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0),(0.0d0,0.0d0),& !sig(mu=2,nu=3)
      &(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0),(0.0d0,0.0d0)& !sig(mu=3,nu=3)
      &/),(/4,4,4,4/))

      complex(kind(0.0d0)), parameter :: ci=(0.0d0,1.0d0)
! polaristion vector of outgoing rho
      complex(kind(0.0d0)), dimension(0:3,-1:1) :: eps_mp_star,eps_mp
      complex(kind(0.0d0)), dimension(1:4,1:4,0:3,0:3) :: gammunu
      logical :: lgammamunu =.true.

      contains

!>>>>>>>>>>>>>>>>>>>>> auxilary <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      subroutine psl_m ( sf, p, am )
!     sf = p_slash + am ; numerator of Feynman propagator for cm
      complex(kind(0.0d0)) :: sf ( 1:4, 1:4 )        ! output
      complex(kind(0.0d0)) :: p ( 0:3 )              ! input
      real(kind(0.0d0)) ::     am                     ! input
      integer ::  i         ! intermediate
      call p_slash(sf,p)
      do i=1,4
       sf(i,i)=sf(i,i)+am
      enddo
      return
      end subroutine psl_m

!======================================================================!

      subroutine p_slash ( psl, p )
!
!       psl = p_mu * gamma^mu  in cm
!
      complex(kind(0.0d0)) :: psl ( 1:4, 1:4 )         ! output
      complex(kind(0.0d0)) :: p   ( 0:3 )              ! input
      psl ( 1, 1 ) =  p ( 0 )
      psl ( 1, 2 ) =  (0.0d0,0.0d0)
      psl ( 1, 3 ) = -p ( 3 )
      psl ( 1, 4 ) = -p ( 1 )+ci*p(2)
      psl ( 2, 1 ) =  (0.0d0,0.0d0)
      psl ( 2, 2 ) =  p ( 0 )
      psl ( 2, 3 ) = -p ( 1 )-ci*p(2)
      psl ( 2, 4 ) =  p ( 3 )
      psl ( 3, 1 ) =  p ( 3 )
      psl ( 3, 2 ) =  p ( 1 )-ci*p(2)
      psl ( 3, 3 ) = -p ( 0 )
      psl ( 3, 4 ) =  (0.0d0,0.0d0)
      psl ( 4, 1 ) =  p ( 1 )+ci*p(2)
      psl ( 4, 2 ) = -p ( 3 )
      psl ( 4, 3 ) =  (0.0d0,0.0d0)
      psl ( 4, 4 ) = -p ( 0 ) 
      return
      end subroutine p_slash

!====================Delta Propagator==================================!

      subroutine prop_delta_rs(propp,p,masdel)
! Rarita Schwinger Propagator: (psl+m)*{-g^munu+...}
      implicit none
      complex(kind(0.0d0)), dimension(1:4,1:4,0:3,0:3) :: propp
      complex(kind(0.0d0)),dimension(0:3) :: p
      complex(kind(0.0d0)),dimension(1:4,1:4) :: pslmp,proptmp
      complex(kind(0.0d0)) :: psq,denomp
      real(kind(0.0d0)) :: masdel
      logical :: ons
      real(kind(0.0d0)) :: mdel2_23,mdel_3
      integer :: is,js,ks,mu,nu

      mdel2_23=2.0d0/(3.0d0*masdel**2)
      mdel_3=1.0d0/(3.0d0*masdel)

      psq=p(0)**2-p(1)**2-p(2)**2-p(3)**2


      call psl_m(pslmp,p,masdel)


      if (lgammamunu) then
        do mu=0,3
          do nu=0,3
            call mat_mat(gammunu(:,:,mu,nu),gamma(:,:,mu),gamma(:,:,nu))
          enddo
        enddo
       lgammamunu = .false.
      endif


! this has been checked!
      do mu=0,3
         do nu=0,3 
            do is=1,4
               do js=1,4
                  proptmp(is,js)=-gmunu(mu,nu)*one4(is,js)+&
                       &1.0d0/3.0d0*gammunu(is,js,mu,nu)+&
                       &mdel2_23*one4(is,js)*p(mu)*p(nu)-&
                       &mdel_3*(p(mu)*gamma(is,js,nu)-p(nu)*gamma(is,js,mu))
               end do
            end do
            call mat_mat(propp(:,:,mu,nu),pslmp,proptmp)
         end do
      end do

      return
      end subroutine prop_delta_rs

!======================================================================!

      subroutine prop_delta_pos(propp,p,masdel,ons)
      implicit none
! pos energy projector and depinding on ons spin3/2 projector
! or rs propagator
      complex(kind(0.0d0)), dimension(1:4,1:4,0:3,0:3) :: propp
      complex(kind(0.0d0)),dimension(0:3) :: p,pp
      complex(kind(0.0d0)),dimension(1:4,1:4) :: pslmp,proptmp
      complex(kind(0.0d0)) :: psq,denomp
      real(kind(0.0d0)) :: masdel
      logical :: ons
      real(kind(0.0d0)) :: mdel2_23,mdel_3
      integer :: is,js,ks,mu,nu

      mdel2_23=2.0d0/(3.0d0*masdel**2)
      mdel_3=1.0d0/(3.0d0*masdel)

      psq=p(0)**2-p(1)**2-p(2)**2-p(3)**2

      pp=p

      pp(0) = cdsqrt(p(1)**2+p(2)**2+p(3)**2+masdel**2)


      call psl_m(pslmp,pp,masdel)


! here we could do the same as for the vertices: get rid of is and js!
! but first check if this is faster and correct
! maybe we should do the multiplication with denom within the loop, because
! this seems to be time consuming (at least while debugging)

      if (lgammamunu) then
        do mu=0,3
          do nu=0,3
            call mat_mat(gammunu(:,:,mu,nu),gamma(:,:,mu),gamma(:,:,nu))
          enddo
        enddo
       lgammamunu = .false.
      endif

      if (ons) then
! this has been checked!
      do mu=0,3
         do nu=0,3 
            do is=1,4
               do js=1,4
                  proptmp(is,js)=-gmunu(mu,nu)*one4(is,js)+&
                       &1.0d0/3.0d0*gammunu(is,js,mu,nu)+&
                       &mdel2_23*one4(is,js)*pp(mu)*pp(nu)-&
                       &mdel_3*(pp(mu)*gamma(is,js,nu)-pp(nu)*gamma(is,js,mu))
               end do
            end do
            call mat_mat(propp(:,:,mu,nu),pslmp,proptmp)
         end do
      end do
      else
      do mu=0,3
         do nu=0,3 
            do is=1,4
               do js=1,4
                  proptmp(is,js)=-gmunu(mu,nu)*one4(is,js)+&
                       &1.0d0/3.0d0*gammunu(is,js,mu,nu)+&
                       &mdel2_23*one4(is,js)*p(mu)*p(nu)-&
                       &mdel_3*(p(mu)*gamma(is,js,nu)-p(nu)*gamma(is,js,mu))
               end do
            end do
            call mat_mat(propp(:,:,mu,nu),pslmp,proptmp)
         end do
      end do
      endif

      return
      end subroutine prop_delta_pos

!======================================================================!

      subroutine prop_delta_neg(propp,p,masdel,ons)
      implicit none
! neg energy projector and depinding on ons spin3/2 projector
! or rs propagator
      complex(kind(0.0d0)), dimension(1:4,1:4,0:3,0:3) :: propp
      complex(kind(0.0d0)),dimension(0:3) :: p,pp
      complex(kind(0.0d0)),dimension(1:4,1:4) :: pslmp,proptmp
      complex(kind(0.0d0)) :: psq,denomp
      real(kind(0.0d0)) :: masdel
      logical :: ons
      real(kind(0.0d0)) :: mdel2_23,mdel_3
      integer :: is,js,ks,mu,nu

      mdel2_23=2.0d0/(3.0d0*masdel**2)
      mdel_3=1.0d0/(3.0d0*masdel)

      psq=p(0)**2-p(1)**2-p(2)**2-p(3)**2

      pp=p

      pp(0) = -1.0d0*cdsqrt(p(1)**2+p(2)**2+p(3)**2+masdel**2)


      call psl_m(pslmp,pp,masdel)

      if (lgammamunu) then
        do mu=0,3
          do nu=0,3
            call mat_mat(gammunu(:,:,mu,nu),gamma(:,:,mu),gamma(:,:,nu))
          enddo
        enddo
       lgammamunu = .false.
      endif

      if (ons) then
      do mu=0,3
         do nu=0,3 
            do is=1,4
               do js=1,4
                  proptmp(is,js)=-gmunu(mu,nu)*one4(is,js)+&
                       &1.0d0/3.0d0*gammunu(is,js,mu,nu)+&
                       &mdel2_23*one4(is,js)*pp(mu)*pp(nu)-&
                       &mdel_3*(pp(mu)*gamma(is,js,nu)-pp(nu)*gamma(is,js,mu))
               end do
            end do
            call mat_mat(propp(:,:,mu,nu),pslmp,proptmp)
         end do
      end do
      else
      do mu=0,3
         do nu=0,3 
            do is=1,4
               do js=1,4
                  proptmp(is,js)=-gmunu(mu,nu)*one4(is,js)+&
                       &1.0d0/3.0d0*gammunu(is,js,mu,nu)+&
                       &mdel2_23*one4(is,js)*p(mu)*p(nu)-&
                       &mdel_3*(p(mu)*gamma(is,js,nu)-p(nu)*gamma(is,js,mu))
               end do
            end do
            call mat_mat(propp(:,:,mu,nu),pslmp,proptmp)
         end do
      end do
      endif


      return
      end subroutine prop_delta_neg


!>>>>>>>>>>>>>>>>>>>> spinors <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      subroutine spinor_cm(u,p_cm,e_cm,cs_h,sn_h,am)
!
!     spinor in the x-z plane of a right-handed coordinate system
!            normalization factor not attached here
!      
      complex(kind(0.0d0)) :: u ( 1:4, 1:2 )   ! output
      complex(kind(0.0d0)) :: p_cm   ! input
      complex(kind(0.0d0)) :: e_cm   ! input
	  real(kind(0.0d0)) :: am
      real(kind(0.0d0)) ::     cs_h,sn_h    ! input
      complex(kind(0.0d0)) :: pauli ( 1:2, 1:2 )        ! intermediate storage
      complex(kind(0.0d0)) :: xi
!
      pauli(1,1) =   cs_h
      pauli(2,1) =   sn_h
      pauli(1,2) = - sn_h
      pauli(2,2) =   cs_h

      xi  = p_cm / ( e_cm + am )

      u(1,1) =   pauli(1,1)      
      u(2,1) =   pauli(2,1)     
      u(3,1) =   pauli(1,1) * xi 
      u(4,1) =   pauli(2,1) * xi 
      u(1,2) =   pauli(1,2)  
      u(2,2) =   pauli(2,2)  
      u(3,2) = - pauli(1,2) * xi 
      u(4,2) = - pauli(2,2) * xi 

      return
      end subroutine spinor_cm

!======================================================================!

      subroutine spinor_plain_cm(u_plain,p,am)
!
!     spinor in the x-z plane of a right-handed coordinate system
!            normalization factor not attached here
!     plain wave spinor!!!
 
      complex(kind(0.0d0)) :: u ( 1:4, 1:2 )   ! output
      complex(kind(0.0d0)),dimension(0:3) :: p   ! input
	  real(kind(0.0d0)) :: am
      complex(kind(0.0d0)) :: xi

      xi  = 1.0d0 / ( p(0) + am )

      u(1,1) =   1.0d0
      u(2,1) =   0.0d0
      u(3,1) =   p(3) * xi 
      u(4,1) =   (p(1)+ci*p(2)) * xi 
      u(1,2) =   0.0d0
      u(2,2) =   1.0d0
      u(3,2) =   (p(1)-ci*p(2)) * xi 
      u(4,2) = - p(3) * xi 

      return
      end subroutine spinor_plain_cm

!======================================================================!

      subroutine spinor_rsin_cm(u,p_cm,e_cm,cs,sn,cs_h,sn_h,am)
!
!     u(id ,   mu ,     ihel)
!       dirac  lorentz  helicity
!       1..4   0..3     0..3
!                       2*hel_phys=3-2*ihel
!!
!     spinor in the x-z plane of a right-handed coordinate system
!            normalization factor not attached here
!      
      complex(kind(0.0d0)) :: u ( 1:4, 0:3, 0:3 )   ! output
!                  dirac lorentz |  
      complex(kind(0.0d0)) :: p_cm   ! input  helicity 3-ihel*2 = 2*hel_phys
      complex(kind(0.0d0)) :: e_cm   ! input
      real(kind(0.0d0)) ::     cs,sn,cs_h,sn_h    ! input
	  real(kind(0.0d0)) :: am
      complex(kind(0.0d0)) :: pauli ( 1:2, 1:2 )        ! intermediate storage
      complex(kind(0.0d0)) :: xi
      complex(kind(0.0d0)),dimension(1:4,1:2) :: uh
      integer :: i
      real(kind(0.0d0)) :: s2
      real(kind(0.0d0)) :: s3
      real(kind(0.0d0)) :: s23
      s2=1.0d0/dsqrt(2.0d0)
      s3=1.0d0/dsqrt(3.0d0)
      s23=dsqrt(2.0d0/3.0d0)

      xi  = p_cm / ( e_cm + am )

      call spinor_cm(uh,p_cm,e_cm,cs_h,sn_h,am)

!hel=1/2
      do i=1,4
       u(i,0,1)=s3*uh(i,2)*0.0d0 + s23*uh(i,1)*p_cm/am
       u(i,1,1)=-s3*uh(i,2)*s2*cs + s23*uh(i,1)*e_cm/am*sn
       u(i,2,1)=-s3*uh(i,2)*s2*ci + s23*uh(i,1)*0.0d0
       u(i,3,1)=s3*uh(i,2)*s2*sn + s23*uh(i,1)*e_cm/am*cs
      enddo
! hel=-1/2
      do i=1,4
       u(i,0,2)=s3*uh(i,1)*0.0d0 + s23*uh(i,2)*p_cm/am
       u(i,1,2)=s3*uh(i,1)*s2*cs + s23*uh(i,2)*e_cm/am*sn
       u(i,2,2)=-s3*uh(i,1)*s2*ci + s23*uh(i,2)*0.0d0
       u(i,3,2)=-s3*uh(i,1)*s2*sn + s23*uh(i,2)*e_cm/am*cs
      enddo
! hel=3/2
      do i=1,4
       u(i,0,0)=uh(i,1)*0.0d0
       u(i,1,0)=-uh(i,1)*s2*cs 
       u(i,2,0)=-uh(i,1)*s2*ci 
       u(i,3,0)=uh(i,1)*s2*sn 
      enddo
! hel=-3/2
      do i=1,4
       u(i,0,3)=uh(i,2)*0.0d0
       u(i,1,3)=uh(i,2)*s2*cs 
       u(i,2,3)=-uh(i,2)*s2*ci 
       u(i,3,3)=-uh(i,2)*s2*sn 
      enddo
      return
      end subroutine spinor_rsin_cm

!======================================================================!

      subroutine spinor_rsout_cm(u,p_cm,e_cm,cs,sn,cs_h,sn_h,am)
!
!     u(id ,   mu ,     ihel)
!       dirac  lorentz  helicity
!       1..4   0..3     0..3
!                       2*hel_phys=3-2*ihel
!!
!     spinor in the x-z plane of a right-handed coordinate system
!            normalization factor not attached here
!      
      complex(kind(0.0d0)) :: u ( 1:4, 0:3, 0:3 )   ! output
!                  dirac lorentz |  
      complex(kind(0.0d0)) :: p_cm   ! input  helicity 3-ihel*2 = 2*hel_phys
      complex(kind(0.0d0)) :: e_cm   ! input
      real(kind(0.0d0)) ::     cs,sn,cs_h,sn_h    ! input
	  real(kind(0.0d0)) :: am
      complex(kind(0.0d0)) :: pauli ( 1:2, 1:2 )        ! intermediate storage
      complex(kind(0.0d0)) :: xi
      complex(kind(0.0d0)),dimension(1:4,1:2) :: uh
      integer :: i
      real(kind(0.0d0)) :: s2
      real(kind(0.0d0)) :: s3
      real(kind(0.0d0)) :: s23
      s2=1.0d0/dsqrt(2.0d0)
      s3=1.0d0/dsqrt(3.0d0)
      s23=dsqrt(2.0d0/3.0d0)

      call spinor_cm(uh,p_cm,e_cm,cs_h,sn_h,am)

!hel=1/2
      do i=1,4
       u(i,0,1)=s3*uh(i,2)*0.0d0 + s23*uh(i,1)*p_cm/am
       u(i,1,1)=-s3*uh(i,2)*s2*cs + s23*uh(i,1)*e_cm/am*sn
       u(i,2,1)=s3*uh(i,2)*s2*ci + s23*uh(i,1)*0.0d0
       u(i,3,1)=s3*uh(i,2)*s2*sn + s23*uh(i,1)*e_cm/am*cs
      enddo
! hel=-1/2
      do i=1,4
       u(i,0,2)=s3*uh(i,1)*0.0d0 + s23*uh(i,2)*p_cm/am
       u(i,1,2)=s3*uh(i,1)*s2*cs + s23*uh(i,2)*e_cm/am*sn
       u(i,2,2)=s3*uh(i,1)*s2*ci + s23*uh(i,2)*0.0d0
       u(i,3,2)=-s3*uh(i,1)*s2*sn + s23*uh(i,2)*e_cm/am*cs
      enddo
! hel=3/2
      do i=1,4
       u(i,0,0)=uh(i,1)*0.0d0
       u(i,1,0)=-uh(i,1)*s2*cs 
       u(i,2,0)=uh(i,1)*s2*ci 
       u(i,3,0)=uh(i,1)*s2*sn 
      enddo
! hel=-3/2
      do i=1,4
       u(i,0,3)=uh(i,2)*0.0d0
       u(i,1,3)=uh(i,2)*s2*cs 
       u(i,2,3)=uh(i,2)*s2*ci 
       u(i,3,3)=-uh(i,2)*s2*sn 
      enddo
      return
      end subroutine spinor_rsout_cm

! >>>>>>>>>>>>>> multiplications <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      subroutine mat_spinor(u_ex,amat,u_in)
! sum_j A_ij u_j
      complex(kind(0.0d0)) ::  u_ex(1:4)
      complex(kind(0.0d0)) ::  amat(1:4,1:4)
      complex(kind(0.0d0)) ::  u_in(1:4)
      integer ::   i,j
      do i = 1,4
      u_ex(i)=(0.0d0,0.0d0)
       do j = 1,4
       u_ex(i) = u_ex(i) + amat(i,j)*u_in(j)
       enddo
      enddo
      return
      end subroutine mat_spinor


      subroutine mat_mat(cmat,amat,bmat)
! A*B matrix multiplication
      complex(kind(0.0d0)) ::  amat(1:4,1:4)
      complex(kind(0.0d0)) ::  bmat(1:4,1:4)
      complex(kind(0.0d0)) ::  cmat(1:4,1:4)
      integer ::   i,j,k
      do i = 1,4
      do k = 1,4
      cmat(i,k)=(0.0d0,0.0d0)
       do j = 1,4
       cmat(i,k) = cmat(i,k) + amat(i,j)*bmat(j,k)
       enddo
      enddo
      enddo
      return
      end subroutine mat_mat


      subroutine spinor_bar_spinor ( scal, s1, s2 )
      complex(kind(0.0d0)) :: s1(1:4),s2(1:4),scal  ! (spinor s1)bar spinor s2
! do not use this on complex contour
!     scal =  conjg(s1(1)) * s2(1)&
!          &+ conjg(s1(2)) * s2(2)&
!          &- conjg(s1(3)) * s2(3)&
!          &- conjg(s1(4)) * s2(4)
      scal =  s1(1) * s2(1)&
           &+ s1(2) * s2(2)&
           &- s1(3) * s2(3)&
           &- s1(4) * s2(4)
      return
      end subroutine spinor_bar_spinor

!======================================================================!

      subroutine spinor_dag_spinor ( vec0, s1, s2 )
      complex(kind(0.0d0)) :: s1(1:4),s2(1:4),vec0  ! (spinor s1)dagger spinor s2
!     vec0 =  conjg(s1(1)) * s2(1)  ! no complex conjugation
!    1      + conjg(s1(2)) * s2(2)  ! because we are in the cm-system
!    1      + conjg(s1(3)) * s2(3)  ! and
!    1      + conjg(s1(4)) * s2(4)  ! we want to contour-integrate
      vec0 =  s1(1) * s2(1)&
           &+ s1(2) * s2(2)&
           &+ s1(3) * s2(3)&
           &+ s1(4) * s2(4)
      return
      end subroutine spinor_dag_spinor


      subroutine vector_dot_vector ( scal, v1, v2 )
      complex(kind(0.0d0)) :: v1(0:3),v2(0:3),scal  ! (vector v1)dot vector s2
      scal =  v1(0) * v2(0)&
          & - v1(1) * v2(1)&
          & - v1(2) * v2(2)&
          & - v1(3) * v2(3)
      return
      end subroutine vector_dot_vector


!>>>>>>>>>>>> Polarisation vectors <<<<<<<<<<<<<<<<<<<<<<<<<

      subroutine pol_vec ( csth,snth,phot )
      real(kind(0.0d0)) ::  csth,snth                ! input
      complex(kind(0.0d0)) :: phot(0:3,-1:1)           ! output
      do ilam=-1,1,2
       phot(0,ilam) = cmplx( 0.0d0, 0.0d0 )
       phot(1,ilam) = cmplx( float(ilam)/dsqrt(2.0d0) * csth, 0.0d0 )
       phot(2,ilam) = cmplx( 0.0d0, -1.0d0/dsqrt(2.0d0) )
       phot(3,ilam) = cmplx(-1.0d0*float(ilam)/dsqrt(2.0d0) * snth, 0.0d0 )
      enddo
      return
      end subroutine pol_vec

!======================================================================!

      subroutine epsvec_mp(eps_mp,p,am,sn_th,cs_th)
! eps(-p,lambda)
! this definition of the polarisation vector for -p ensures not only
! sum(eps^mu(-p,la)*eps^*^nu(-p,la),la=-1..1)=-g^mu^nu+k^mu*k^nu/m^2, but also
! the right behaviour for NN->NDelta and NN->DeltaN transitions (namely only a sign
! and not a factor of 3) and agrees with the conventions of Jacob and Wick
      implicit none
      complex(kind(0.0d0)), dimension(0:3,-1:1) :: eps_mp
      complex(kind(0.0d0)) :: p,omp
      real(kind(0.0d0)) :: am,sn_th,cs_th
      real(kind(0.0d0)) :: sqrt2

      sqrt2=dsqrt(2.0d0)
      
      eps_mp(0,1) = cmplx(0.0d0,0.0d0)
      eps_mp(1,1) = cmplx(cs_th/sqrt2,0.0d0)
      eps_mp(2,1) = cmplx(0.0d0,-1/sqrt2)
      eps_mp(3,1) = cmplx(-sn_th/sqrt2,0.0d0)

      omp=cdsqrt(p**2+am**2)
      eps_mp(0,0) = -p/am
      eps_mp(1,0) = omp/am*sn_th
      eps_mp(2,0) = (0.0d0,0.0d0)
      eps_mp(3,0) = omp/am*cs_th
! change to Mull's vectors
!      eps_mp(0,0) = p/am
!      eps_mp(1,0) = -omp/am*sn_th
!      eps_mp(2,0) = (0.0d0,0.0d0)
!      eps_mp(3,0) = -omp/am*cs_th

      eps_mp(0,-1) = cmplx(0.0d0,0.0d0)
      eps_mp(1,-1) = cmplx(-cs_th/sqrt2,0.0d0)
      eps_mp(2,-1) = cmplx(0.0d0,-1.0d0/sqrt2)
      eps_mp(3,-1) = cmplx(sn_th/sqrt2,0.0d0)

      end subroutine epsvec_mp

!======================================================================!

      subroutine epsvec_mp_star(eps_mp,p,am,sn_th,cs_th)
! eps^*(-p,lambda)
      implicit none
      complex(kind(0.0d0)), dimension(0:3,-1:1) :: eps_mp
      complex(kind(0.0d0)) :: p,omp
      real(kind(0.0d0)) :: am,sn_th,cs_th
      real(kind(0.0d0)) :: sqrt2

      sqrt2=dsqrt(2.0d0)
      
      eps_mp(0,1) = cmplx(0.0d0,0.0d0)
      eps_mp(1,1) = cmplx(cs_th/sqrt2,0.0d0)
      eps_mp(2,1) = cmplx(0.0d0,+1/sqrt2)
      eps_mp(3,1) = cmplx(-sn_th/sqrt2,0.0d0)

      omp=cdsqrt(p**2+am**2)
      eps_mp(0,0) = -p/am
      eps_mp(1,0) = omp/am*sn_th
      eps_mp(2,0) = (0.0d0,0.0d0)
      eps_mp(3,0) = omp/am*cs_th

! change to Mull's vectors
!      eps_mp(0,0) = p/am
!      eps_mp(1,0) = -omp/am*sn_th
!      eps_mp(2,0) = (0.0d0,0.0d0)
!      eps_mp(3,0) = -omp/am*cs_th

      eps_mp(0,-1) = cmplx(0.0d0,0.0d0)
      eps_mp(1,-1) = cmplx(-cs_th/sqrt2,0.0d0)
      eps_mp(2,-1) = cmplx(0.0d0,+1.0d0/sqrt2)
      eps_mp(3,-1) = cmplx(sn_th/sqrt2,0.0d0)

      end subroutine epsvec_mp_star


! >>>>>>>>>>>>>>>>>> vertices <<<<<<<<<<<<<<<<<<<<<<<<<<

      subroutine vertex_pipirho(g_ppr,p1,p2,eps_star)
! this routine calculates the pipirho vertexfunction for outgoing rho
! g_ppr=i*eps^*^mu*(p1+p2)_mu (piN a)
      implicit none
      complex(kind(0.0d0)) :: g_ppr (-1:1)
      complex(kind(0.0d0)) :: p1(0:3),p2(0:3)
      complex(kind(0.0d0)) :: eps_star(0:3,-1:1)
      integer :: mu,lam

      do lam=-1,1
       g_ppr(lam) = (0.0d0,0.0d0)
        do mu=0,3
          g_ppr(lam)=g_ppr(lam) + eps_star(mu,lam)*metric(mu)*(p1(mu)+p2(mu))
        enddo
       g_ppr(lam)=g_ppr(lam)*ci
      enddo
      return
      end subroutine vertex_pipirho

!=======================================================================!

      subroutine vertex_pv(g_pv,p)  ! pi-N-N vertex function for incoming pion
      implicit none                 ! g_pv = -i * gamma_5 * gamma^mu * p_mu

      complex(kind(0.0d0)) :: g_pv ( 1:4, 1:4 )   ! output
      complex(kind(0.0d0)) :: p    ( 0:3 )        ! input

      g_pv ( 1, 1 ) =  p ( 3 )
      g_pv ( 1, 2 ) =  p ( 1 )-ci*p(2)    ! cm_calculation: p(2) = 0
      g_pv ( 1, 3 ) = -p ( 0 )
      g_pv ( 1, 4 ) =  (0.0d0,0.0d0)
      g_pv ( 2, 1 ) =  p ( 1 )+ci*p(2)    ! cm
      g_pv ( 2, 2 ) = -p ( 3 )
      g_pv ( 2, 3 ) =  (0.0d0,0.0d0)
      g_pv ( 2, 4 ) = -p ( 0 )
      g_pv ( 3, 1 ) =  p ( 0 )
      g_pv ( 3, 2 ) =  (0.0d0,0.0d0)
      g_pv ( 3, 3 ) = -p ( 3 )
      g_pv ( 3, 4 ) = -p ( 1 )+ci*p(2)    ! cm
      g_pv ( 4, 1 ) =  (0.0d0,0.0d0)
      g_pv ( 4, 2 ) =  p ( 0 )
      g_pv ( 4, 3 ) = -p ( 1 )-ci*p(2)    ! cm
      g_pv ( 4, 4 ) =  p ( 3 )
      g_pv=-1.0d0*ci*g_pv
           ! factor -i from vertex function

      return
      end subroutine vertex_pv

!=======================================================================!

      subroutine vertex_pvct ( g_pv )
! g_pv=gamma_5*gamma^0
      implicit none
      complex(kind(0.0d0)) :: g_pv ( 1:4, 1:4 )         ! output
      g_pv ( 1, 1 ) =  (0.0d0,0.0d0)
      g_pv ( 1, 2 ) =  (0.0d0,0.0d0)
      g_pv ( 1, 3 ) =  (-1.0d0,0.0d0)
      g_pv ( 1, 4 ) =  (0.0d0,0.0d0)
      g_pv ( 2, 1 ) =  (0.0d0,0.0d0)
      g_pv ( 2, 2 ) =  (0.0d0,0.0d0)
      g_pv ( 2, 3 ) =  (0.0d0,0.0d0)
      g_pv ( 2, 4 ) =  (-1.0d0,0.0d0)
      g_pv ( 3, 1 ) =  (1.0d0,0.0d0)
      g_pv ( 3, 2 ) =  (0.0d0,0.0d0)
      g_pv ( 3, 3 ) =  (0.0d0,0.0d0)
      g_pv ( 3, 4 ) =  (0.0d0,0.0d0)
      g_pv ( 4, 1 ) =  (0.0d0,0.0d0)
      g_pv ( 4, 2 ) =  (1.0d0,0.0d0)
      g_pv ( 4, 3 ) =  (0.0d0,0.0d0)
      g_pv ( 4, 4 ) =  (0.0d0,0.0d0)
      return
      end subroutine vertex_pvct

      subroutine vertex_rhoNNct ( g_rnn )
! g_pv=gamma^0
      implicit none
      complex(kind(0.0d0)) :: g_rnn ( 1:4, 1:4 )         ! output
      g_rnn ( 1, 1 ) =  (1.0d0,0.0d0)
      g_rnn ( 1, 2 ) =  (0.0d0,0.0d0)
      g_rnn ( 1, 3 ) =  (0.0d0,0.0d0)
      g_rnn ( 1, 4 ) =  (0.0d0,0.0d0)
      g_rnn ( 2, 1 ) =  (0.0d0,0.0d0)
      g_rnn ( 2, 2 ) =  (1.0d0,0.0d0)
      g_rnn ( 2, 3 ) =  (0.0d0,0.0d0)
      g_rnn ( 2, 4 ) =  (0.0d0,0.0d0)
      g_rnn ( 3, 1 ) =  (0.0d0,0.0d0)
      g_rnn ( 3, 2 ) =  (0.0d0,0.0d0)
      g_rnn ( 3, 3 ) =  (-1.0d0,0.0d0)
      g_rnn ( 3, 4 ) =  (0.0d0,0.0d0)
      g_rnn ( 4, 1 ) =  (0.0d0,0.0d0)
      g_rnn ( 4, 2 ) =  (0.0d0,0.0d0)
      g_rnn ( 4, 3 ) =  (0.0d0,0.0d0)
      g_rnn ( 4, 4 ) =  (-1.0d0,0.0d0)
      return
      end subroutine vertex_rhoNNct

!======================================================================!
      
      subroutine vertex_rhoNN ( g_rnn, p,am,kappa )
! this routine calculates the rhoNN Vertexfunction for outgoing rho
! gamma^mu-i*kappa/2/m*sigma^mn^nu*p_nu (piN c) without eps_vec

      implicit none
      complex(kind(0.0d0)) :: g_rnn (1:4, 1:4, 0:3)         ! output
      complex(kind(0.0d0)) :: p    ( 0:3 )              ! input
      real(kind(0.0d0)) :: kappa,am                     ! input
      integer :: is,js,mu,nu
      complex(kind(0.0d0)),dimension(1:4,1:4) :: shelp


! calculate the sig_mu_nu eps_mu q_nu term and the slash_eps term
         do is=1,4
            do js=1,4
               do mu=0,3
               shelp(is,js)=(0.0d0,0.0d0)
                  do nu=0,3
                     shelp(is,js)=shelp(is,js)+&
                     &sig(is,js,mu,nu)*p(nu)*metric(nu)
                  enddo
               g_rnn(is,js,mu)=gamma(is,js,mu)-ci*kappa/2.0d0/am*shelp(is,js)
               enddo
            enddo
         enddo

        
      return
      end subroutine vertex_rhoNN

!======================================================================!

      subroutine vertex_rhoNN2 ( g_rnn, p,eps_star,am,kappa )
! this routine calculates the rhoNN Vertexfunction for outgoing rho
! gamma^mu-i*kappa/2/m*sigma^mn^nu*p_nu*eps^*_mu(lambda) (piN c)
!c! checked
      implicit none
      complex(kind(0.0d0)) :: g_rnn ( 1:4, 1:4, -1:1 )         ! output
      complex(kind(0.0d0)) :: p    ( 0:3 )              ! input
      complex(kind(0.0d0)) :: eps_star (0:3,-1:1)            ! input
      real(kind(0.0d0)) :: kappa,am                     ! input
      integer :: lam,is,js,mu,nu
      complex(kind(0.0d0)),dimension(1:4,1:4) :: esl,shelp
      do lam=-1,1

! calculate the sig_mu_nu eps_mu q_nu term and the slash_eps term
         call p_slash(esl,eps_star(:,lam))
         do is=1,4
            do js=1,4
               shelp(is,js)=(0.0d0,0.0d0)
               do mu=0,3
                  do nu=0,3
                     shelp(is,js)=shelp(is,js)+&
                     &sig(is,js,mu,nu)*eps_star(mu,lam)*p(nu)*metric(mu)*metric(nu)
                  enddo
               enddo
! build up the matrix
             g_rnn(is,js,lam)=esl(is,js)-ci*kappa/(2.0d0*am)*shelp(is,js)
            enddo
         enddo


      enddo

      return
      end subroutine vertex_rhoNN2

!======================================================================!

      subroutine vertex_rhoNstN ( g_rnn, p,eps_star,am,kappa )
! this routine calculates the rhoN*N Vertexfunction 
! gamma^5*gamma^mu+i*kappa/2/m*gamma^5*sigma^mn^nu*p_nu*eps^_mu(lambda) 
      implicit none
      complex(kind(0.0d0)) :: g_rnn ( 1:4, 1:4, -1:1 )         ! output
      complex(kind(0.0d0)) :: p    ( 0:3 )              ! input
      complex(kind(0.0d0)) :: eps_star (0:3,-1:1)            ! input
      real(kind(0.0d0)) :: kappa,am                     ! input
      integer :: lam,is,js,mu,nu
      complex(kind(0.0d0)),dimension(1:4,1:4) :: esl,esl5,shelp,shelp5
      do lam=-1,1

! calculate the sig_mu_nu eps_mu q_nu term and the slash_eps term
         call p_slash(esl,eps_star(:,lam))
         do is=1,4
            do js=1,4
               shelp(is,js)=(0.0d0,0.0d0)
               do mu=0,3
                  do nu=0,3
                     shelp(is,js)=shelp(is,js)+&
                     &sig(is,js,mu,nu)*eps_star(mu,lam)*p(nu)*metric(mu)*metric(nu)
                  enddo
               enddo
            enddo
         enddo


! multiply with gamma5
         call mat_mat(esl5,gamma(:,:,4),esl)
         call mat_mat(shelp5,gamma(:,:,4),shelp)
         do is=1,4
            do js=1,4
! build up the matrix
             g_rnn(is,js,lam)=esl5(is,js)+ci*kappa/(2.0d0*am)*shelp5(is,js)
            enddo
         enddo
      enddo

      return
      end subroutine vertex_rhoNstN

!======================================================================!

      subroutine vertex_pipisig ( g_pps, p1,p2)
! this routine calculates the pipisigma Vertexfunction p1=in, p2=out
! -2*p1_mu*p2^mu (piN d)

      implicit none
      complex(kind(0.0d0)) :: g_pps        ! output
      complex(kind(0.0d0)), dimension(0:3) :: p1,p2          ! input
      integer :: mu

      g_pps=(0.0d0,0.0d0)
      do mu=0,3
         g_pps=g_pps - 2.0d0*p1(mu)*p2(mu)*metric(mu)
      enddo

      return
      end subroutine vertex_pipisig

! the sigNN coupling is too trivial to store as a subroutine
! here just the reminder g_NNs = -1 (piN e)

      subroutine vertex_pipiNN ( g_ppNN, p1,p2)
! this routine calculates the pipiNN contact vertex
! -2*sig^mu^nu*p1_mu*p2_nu (piN f)

      implicit none
      complex(kind(0.0d0)) :: g_ppNN (1:4,1:4)        ! output
      complex(kind(0.0d0)),dimension(0:3) :: p1,p2             ! input
      integer :: mu,nu,is,js

      do is=1,4
         do js=1,4
            g_ppNN(is,js)=(0.0d0,0.0d0)
            do mu=0,3
               do nu=0,3
                  g_ppNN(is,js)=g_ppNN(is,js) -& 
                       &2.0d0*sig(is,js,mu,nu)*p1(mu)*metric(mu)*p2(nu)*metric(nu)
               enddo
            enddo
         enddo
      enddo

      return
      end subroutine vertex_pipiNN
     
! the piNDelta coupling is too trivial to store as a subroutine
! (the propagator is the untrivial thing)
! here just the reminder g_piND = i*q_mu (piN g)

      subroutine vertex_pind13 ( g_pnd )
! this routine calculates the piNN*(D13) vertexfunction 
! gamma_5 (piN-piN i)

      implicit none
      complex(kind(0.0d0)) :: g_pnd ( 1:4, 1:4)         ! output
      integer :: is,js


      do is=1,4
         do js=1,4
            g_pnd(is,js)=gamma(is,js,4)
         enddo
      enddo

      return
      end subroutine vertex_pind13

      subroutine vertex_pind13A ( g_pnd,p )
! this routine calculates another piNN*(D13) vertexfunction 
! sig^mu^nu * p_nu

      implicit none
      complex(kind(0.0d0)) :: g_pnd ( 1:4, 1:4,0:3)         ! output
      complex(kind(0.0d0)) :: p(0:3)
      integer :: is,js,mu,nu

         do is=1,4
            do js=1,4
               do mu=0,3
               g_pnd(is,js,mu)=(0.0d0,0.0d0)
                  do nu=0,3
                     g_pnd(is,js,mu)=g_pnd(is,js,mu)+&
                     &sig(is,js,mu,nu)*p(nu)*metric(nu)
                  enddo
               enddo
            enddo
         enddo

      return
      end subroutine vertex_pind13A

      subroutine vertex_rhopiNN ( g_rpnn,eps_star )
! this routine calculates the rhopiNN Vertexfunction for outgoing rho
! -1*gamma_5*gamma^mu*eps^*_mu (piN->rhoN c)
!= eps^*_mu*gamma^mu*gamma5 (this saves the multiplication with -1
!c! checked
      implicit none
      complex(kind(0.0d0)) :: g_rpnn ( 1:4, 1:4, -1:1 )         ! output
      complex(kind(0.0d0)) :: p    ( 0:3 )              ! input
      complex(kind(0.0d0)) :: eps_star (0:3,-1:1)            ! input
      integer :: lam
      complex(kind(0.0d0)),dimension(1:4,1:4) :: esl


      do lam=-1,1
         g_rpnn(1,1,lam)=-eps_star(3,lam)
         g_rpnn(1,2,lam)=-eps_star(1,lam)+ci*eps_star(2,lam)
         g_rpnn(1,3,lam)= eps_star(0,lam)
         g_rpnn(1,4,lam)= (0.0d0,0.0d0)

         g_rpnn(2,1,lam)=-eps_star(1,lam)-ci*eps_star(2,lam)
         g_rpnn(2,2,lam)= eps_star(3,lam)
         g_rpnn(2,3,lam)= (0.0d0,0.0d0)
         g_rpnn(2,4,lam)= eps_star(0,lam)

         g_rpnn(3,1,lam)=-eps_star(0,lam)
         g_rpnn(3,2,lam)= (0.0d0,0.0d0)
         g_rpnn(3,3,lam)= eps_star(3,lam)
         g_rpnn(3,4,lam)= eps_star(1,lam)-ci*eps_star(2,lam)

         g_rpnn(4,1,lam)= (0.0d0,0.0d0)
         g_rpnn(4,2,lam)=-eps_star(0,lam)
         g_rpnn(4,3,lam)= eps_star(1,lam)+ci*eps_star(2,lam)
         g_rpnn(4,4,lam)=-eps_star(3,lam)

      enddo

      return
      end subroutine vertex_rhopiNN

!=======================================================================!

      subroutine vertex_pirhoomega(g_pro,p1,p2,eps_star2) ! pi-rho-omega vertex function
      implicit none                                       ! epsilon_mu_nu_la_ta * p1^mu * eps_star2^nu * p2^la

      complex(kind(0.0d0)) :: g_pro (0:3,-1:1)                ! output
      complex(kind(0.0d0)), dimension(0:3) :: p1,p2           ! input
      complex(kind(0.0d0)), dimension(0:3,-1:1) :: eps_star2  ! input
      integer :: lam1

      do lam1=-1,1

        g_pro(0,lam1) =   p1(2)*eps_star2(1,lam1)*p2(3) - p1(3)*eps_star2(1,lam1)*p2(2) &
                      & - p1(1)*eps_star2(2,lam1)*p2(3) + p1(3)*eps_star2(2,lam1)*p2(1) &
                      & + p1(1)*eps_star2(3,lam1)*p2(2) - p1(2)*eps_star2(3,lam1)*p2(1)

        g_pro(1,lam1) = - p1(2)*eps_star2(0,lam1)*p2(3) + p1(3)*eps_star2(0,lam1)*p2(2) &
                      & + p1(0)*eps_star2(2,lam1)*p2(3) - p1(3)*eps_star2(2,lam1)*p2(0) &
                      & - p1(0)*eps_star2(3,lam1)*p2(2) + p1(2)*eps_star2(3,lam1)*p2(0)

        g_pro(2,lam1) =   p1(1)*eps_star2(0,lam1)*p2(3) - p1(3)*eps_star2(0,lam1)*p2(1) &
                      & - p1(0)*eps_star2(1,lam1)*p2(3) + p1(3)*eps_star2(1,lam1)*p2(0) &
                      & + p1(0)*eps_star2(3,lam1)*p2(1) - p1(1)*eps_star2(3,lam1)*p2(0)

        g_pro(3,lam1) = - p1(1)*eps_star2(0,lam1)*p2(2) + p1(2)*eps_star2(0,lam1)*p2(1) &
                      & + p1(0)*eps_star2(1,lam1)*p2(2) - p1(2)*eps_star2(1,lam1)*p2(0) &
                      & - p1(0)*eps_star2(2,lam1)*p2(1) + p1(1)*eps_star2(2,lam1)*p2(0)

      end do

      return
      end subroutine vertex_pirhoomega

!=======================================================================!

! the omegaNN vertexfunction is the same as for rhoNN despite the different
! values for kappa and a different sign of the coupling (see piN->rhoN f)

      subroutine vertex_rhoNDeps ( g_rNDeps, p , eps_st)
! this routine calculates the rhoNDelta Vertexfunction no. 1 for outgoing rho and Delta
! gamma5*(eps_slash*p_mu-p_slash*eps_mu) (piN->rhoN h)
      implicit none
      complex(kind(0.0d0)) :: g_rNDeps (1:4,1:4,0:3,-1:1)         ! output
      complex(kind(0.0d0)) :: p    ( 0:3 )              ! input
      complex(kind(0.0d0)) :: eps_st    ( 0:3,-1:1 )              ! input
      integer :: mu,is,js,lam
      complex(kind(0.0d0)),dimension(1:4,1:4) :: esl,psl,epsl5,psl5

      call p_slash(psl,p)
      call mat_mat(psl5,gamma(:,:,4),psl)
      do lam=-1,1
         call p_slash(esl,eps_st(:,lam))  
         call mat_mat(epsl5,gamma(:,:,4),esl)
         do mu=0,3
            do is=1,4
              do js=1,4
                g_rndeps(is,js,mu,lam) = epsl5(is,js)*metric(mu)*p(mu)-&
                     &psl5(is,js)*eps_st(mu,lam)*metric(mu)
              enddo
            enddo
         enddo
      enddo

      return

      end subroutine vertex_rhoNDeps

      subroutine vertex_rhoND13 ( g_rND13, p , eps_st)
! this routine calculates the rhoND13 Vertexfunction for outgoing rho and D13
! (p_slasheps^*_mu-eps_slash*p_mu) (piN->rhoN)
      implicit none
      complex(kind(0.0d0)) :: g_rND13 (1:4,1:4,0:3,-1:1)         ! output
      complex(kind(0.0d0)) :: p    ( 0:3 )              ! input
      complex(kind(0.0d0)) :: eps_st    ( 0:3,-1:1 )              ! input
      integer :: mu,is,js,lam
      complex(kind(0.0d0)),dimension(1:4,1:4) :: esl,psl,epsl5,psl5

      call p_slash(psl,p)
      do lam=-1,1
         call p_slash(esl,eps_st(:,lam))  
         do mu=0,3
            do is=1,4
              do js=1,4
                g_rnd13(is,js,mu,lam) = psl(is,js)*metric(mu)*eps_st(mu,lam)-&
                     &esl(is,js)*metric(mu)*p(mu)
              enddo
            enddo
         enddo
      enddo

      return
      end subroutine vertex_rhoND13


      subroutine vertex_rhoNDelta1 ( g_rND1, p )
! this routine calculates the rhoNDelta Vertexfunction no. 1 for outgoing rho and Delta
! gamma5*(gamma_nu*p_mu-p_slash*g_mu_nu) (piN->rhoN h)
! this has been checked!
      implicit none
      complex(kind(0.0d0)) :: g_rND1 (1:4,1:4,0:3,0:3)         ! output
      complex(kind(0.0d0)) :: p    ( 0:3 )              ! input
      integer :: nu,mu,is,js
      complex(kind(0.0d0)),dimension(1:4,1:4) :: esl,psl,gnu5,psl5

      call p_slash(psl,p)
      call mat_mat(psl5,gamma(:,:,4),psl)
      do nu=0,3
         call mat_mat(gnu5,gamma(:,:,4),gamma(:,:,nu))
         do mu=0,3
            do is=1,4
               do js=1,4
                  g_rND1(is,js,mu,nu)=gnu5(is,js)*metric(nu)*p(mu)*metric(mu)-&
                       &psl5(is,js)*gmunu(mu,nu)
               end do
            end do
         end do
      end do

      return
      end subroutine vertex_rhoNDelta1


      subroutine vertex_rhoNDelta2 ( g_rND2, p1,p2,k )
! this routine calculates the rhoNDelta Vertexfunction no. 2 for outgoing rho and Delta
! -1*gamma5*((p1+p2)_nu*k_mu-(p1+p2).dot.k*g_mu_nu)  (piN->rhoN h)

      implicit none
      complex(kind(0.0d0)) :: g_rND2 (1:4,1:4,0:3,0:3)         ! output
      complex(kind(0.0d0)), dimension(0:3) :: p1,p2,k       ! input
      integer :: nu,mu,is,js
      complex(kind(0.0d0)),dimension(0:3) :: psum
      complex(kind(0.0d0)) :: psdotk,shelp

      psum=p1+p2
      call vector_dot_vector(psdotk,psum,k)
      do nu=0,3
         do mu=0,3!this is the minus sign in front of the vertexfunction
           shelp=(-psum(nu)*metric(nu)*k(mu)*metric(mu)+&
                       &psdotk*gmunu(mu,nu)) 
           do is=1,4
               do js=1,4
                  g_rND2(is,js,mu,nu)=gamma(is,js,4)*shelp
               end do
            end do
         end do
      end do

      return
      end subroutine vertex_rhoNDelta2  

!=======================================================================!

      subroutine vertex_a1NN(g_ann)  ! a1-N-N vertex function
      implicit none                  ! gamma_5 * gamma_mu

      complex(kind(0.0d0)) :: g_ann ( 1:4, 1:4, 0:3 )   ! output
      integer :: mu

      do mu=0,3
        call mat_mat(g_ann(:,:,mu),gamma(:,:,4),metric(mu)*gamma(:,:,mu))
      end do

      return
      end subroutine vertex_a1NN

!=======================================================================!

      subroutine vertex_pirhoa1(g_pra,q,k1,k2,eps_star2)  ! pi-rho-a1 vertex function
      implicit none   ! (eps^star2_mu*(q-0.5*k1).dot.k2 - k2_mu*(q-0.5*k1).dot.eps^star2)

      complex(kind(0.0d0)) :: g_pra ( 0:3,-1:1 )              ! output
      complex(kind(0.0d0)),dimension(0:3,-1:1) :: eps_star2   ! input
      complex(kind(0.0d0)),dimension(0:3) :: q,k1,k2          ! input
      complex(kind(0.0d0)),dimension(0:3) :: qk
      complex(kind(0.0d0)) :: qkk2
      complex(kind(0.0d0)) :: qke2(-1:1)
      integer :: mu,lam2

      qk=q-0.5d0*k1
      call vector_dot_vector(qkk2,qk,k2)

      do lam2=-1,1
        call vector_dot_vector(qke2(lam2),qk,eps_star2(:,lam2))
      end do

      do mu=0,3
        do lam2=-1,1
          g_pra(mu,lam2)=(eps_star2(mu,lam2)*metric(mu)*qkk2-k2(mu)*metric(mu)*qke2(lam2))
        end do
      end do

      return
      end subroutine vertex_pirhoa1

!=======================================================================!

      subroutine vertex_rhorhorho ( g_rrr,k1,k2,k3,eps2,eps3 )
! this routine calculates the rhorhorho Vertexfunction for INCOMING rho's
! i*(eps3.dot.eps2*(k3-k2)_mu+eps3_mu*(k1-k3).dot.eps2+eps2_mu*(k2-k1).dot.eps3)
! (rhoN->rhoN a)

      implicit none
      complex(kind(0.0d0)) :: g_rrr ( -1:1,-1:1,0:3 )         ! output
      complex(kind(0.0d0)),dimension(0:3,-1:1)  :: eps2,eps3             ! input
      complex(kind(0.0d0)),dimension(0:3)  :: k1,k2,k3             ! input
      complex(kind(0.0d0)),dimension(0:3)  :: k32,k13,k21
      complex(kind(0.0d0)),dimension(-1:1,-1:1) :: e32
      complex(kind(0.0d0)),dimension(-1:1) :: ke132,ke213
      integer :: mu,lam1,lam2,lam3

      k32=k3-k2
      k13=k1-k3
      k21=k2-k1
      do lam1=-1,1
         call vector_dot_vector(ke132(lam1),k13,eps2(:,lam1))
         call vector_dot_vector(ke213(lam1),k21,eps3(:,lam1))
         do lam2=-1,1
            call vector_dot_vector(e32(lam1,lam2),eps3(:,lam1),eps2(:,lam2))
         enddo
      enddo

      do mu=0,3
         do lam2=-1,1
            do lam3=-1,1
            g_rrr(lam2,lam3,mu)=ci*e32(lam3,lam2)*k32(mu)*metric(mu)+&
                 &ci*eps3(mu,lam3)*metric(mu)*ke132(lam2)+&
                 &ci*eps2(mu,lam2)*metric(mu)*ke213(lam3)
            enddo
         enddo
      enddo

      return
      end subroutine vertex_rhorhorho


      subroutine vertex_rhorhoNN ( g_rrnn,eps2,eps4 )
! this routine calculates the rhorhoNN contact vertexfunction 
! -1*sig^mu^nueps_mu(la2)eps^*_nu(la4) (rhoN->rhoN c)

      implicit none
      complex(kind(0.0d0)) :: g_rrnn ( 1:4,1:4,-1:1,-1:1 )         ! output
      complex(kind(0.0d0)),dimension(0:3,-1:1)  :: eps2,eps4             ! input
      integer :: lam2,lam4,is,js,mu,nu

      do is=1,4
         do js=1,4
            do lam2=-1,1
               do lam4=-1,1
                  g_rrnn(is,js,lam2,lam4)=(0.0d0,0.0d0)
                  do mu=0,3
                     do nu=0,3
                       g_rrnn(is,js,lam2,lam4)=g_rrnn(is,js,lam2,lam4)+&
                       &sig(is,js,mu,nu)*eps2(mu,lam2)*metric(mu)*&
                       &eps4(nu,lam4)*metric(nu)
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo

      return
      end subroutine vertex_rhorhoNN


      subroutine vertex_em ( phot, p , gamma, akap_m, v_em)
      complex(kind(0.0d0)) :: phot  ( 0:3 )                ! input
      complex(kind(0.0d0)) :: p     ( 0:3 )                ! input
      complex(kind(0.0d0)) :: gamma ( 1:4, 1:4, 0:3 )      ! input
      real(kind(0.0d0)) ::    akap_m                       ! input
      complex(kind(0.0d0)) :: v_em  ( 1:4, 1:4 )            ! output
      complex(kind(0.0d0)) :: vpl   ( 1:4, 1:4, 0:3 )
!
      call vertex_pauli ( p , vpl )
      do iy =    1, 4
        do ix =  1, 4
         v_em ( ix, iy ) =&
        &phot(0)*(gamma(ix,iy,0)+akap_m*vpl(ix,iy,0))&
       &-phot(1)*(gamma(ix,iy,1)+akap_m*vpl(ix,iy,1))&
       &-phot(2)*(gamma(ix,iy,2)+akap_m*vpl(ix,iy,2))&
       &-phot(3)*(gamma(ix,iy,3)+akap_m*vpl(ix,iy,3))
        enddo
      enddo
      return
      end subroutine vertex_em


      subroutine vertex_pauli ( p , vpl )
      complex(kind(0.0d0)) :: p   ( 0:3 )               ! input
      complex(kind(0.0d0)) :: vpl ( 1:4, 1:4, 0:3 )     ! output
      complex(kind(0.0d0)) :: fac
      do i3 = 0,3
      do i2 = 1,4
      do i1 = 1,4
       vpl ( i1, i2, i3 ) = 0.0d0
      enddo
      enddo
      enddo
!
       vpl ( 3, 1, 0 ) =   p ( 3 )
       vpl ( 4, 1, 0 ) =   p ( 1 )  ! p(2)=0 in cm calculation
       vpl ( 3, 2, 0 ) =   p ( 1 )
       vpl ( 4, 2, 0 ) = - p ( 3 )
       vpl ( 1, 3, 0 ) =   p ( 3 )
       vpl ( 2, 3, 0 ) =   p ( 1 )
       vpl ( 1, 4, 0 ) =   p ( 1 )
       vpl ( 2, 4, 0 ) = - p ( 3 )
!
       vpl ( 2, 1, 1 ) = - p ( 3 )
       vpl ( 4, 1, 1 ) =   p ( 0 )
       vpl ( 1, 2, 1 ) =   p ( 3 )
       vpl ( 3, 2, 1 ) =   p ( 0 )
       vpl ( 2, 3, 1 ) =   p ( 0 )
       vpl ( 4, 3, 1 ) = - p ( 3 )
       vpl ( 1, 4, 1 ) =   p ( 0 )
       vpl ( 3, 4, 1 ) =   p ( 3 )
!
       fac = -ci
       vpl ( 1, 1, 2 )  = - p ( 1 ) * fac  
       vpl ( 2, 1, 2 )  =   p ( 3 ) * fac
       vpl ( 4, 1, 2 )  = - p ( 0 ) * fac
       vpl ( 1, 2, 2 )  =   p ( 3 ) * fac
       vpl ( 2, 2, 2 )  =   p ( 1 ) * fac
       vpl ( 3, 2, 2 )  =   p ( 0 ) * fac
       vpl ( 2, 3, 2 )  = - p ( 0 ) * fac
       vpl ( 3, 3, 2 )  = - p ( 1 ) * fac
       vpl ( 4, 3, 2 )  =   p ( 3 ) * fac
       vpl ( 1, 4, 2 )  =   p ( 0 ) * fac
       vpl ( 3, 4, 2 )  =   p ( 3 ) * fac
       vpl ( 4, 4, 2 )  =   p ( 1 ) * fac
!
       vpl ( 2, 1, 3 ) =   p ( 1 )
       vpl ( 3, 1, 3 ) =   p ( 0 )
       vpl ( 1, 2, 3 ) = - p ( 1 )
       vpl ( 4, 2, 3 ) = - p ( 0 )
       vpl ( 1, 3, 3 ) =   p ( 0 )
       vpl ( 4, 3, 3 ) =   p ( 1 )
       vpl ( 2, 4, 3 ) = - p ( 0 )
       vpl ( 3, 4, 3 ) = - p ( 1 )
!
      return
      end subroutine vertex_pauli


      end module prop


