!
! potential in JLS basis starting from helicity basis. In JLS basis we have the
! following notation for the transition potentials:
!                     Npi->Nrho
!          final J   S         J        S initial
! v(1)  = < l=j-1/2, 1/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(2)  = < l=j+1/2, 1/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(3)  = < l=j-1/2, 1/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(4)  = < l=j+1/2, 1/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! v(5)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(6)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(7)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(8)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! v(9)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 1/2 > I=1/2  pay attention here
! v(10) = < l=j-3/2, 3/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(11) = < l=j+3/2, 3/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(12) = < l=j-3/2, 3/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! and for the Nrho-Nrho potentials:
!          final J   S         J        S initial
! v(1)  = < l=j-1/2, 1/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(2)  = < l=j+1/2, 1/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(3)  = < l=j-1/2, 1/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(4)  = < l=j+1/2, 1/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! v(5)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(6)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(7)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(8)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! v(9)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 1/2 > I=1/2  pay attention here
! v(10) = < l=j-3/2, 3/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(11) = < l=j+3/2, 3/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(12) = < l=j-3/2, 3/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! v(13) = < l=j-1/2, 3/2 |V^J| l=j-1/2, 3/2 > I=1/2
! v(14) = < l=j+1/2, 3/2 |V^J| l=j+1/2, 3/2 > I=1/2
! v(15) = < l=j-1/2, 3/2 |V^J| l=j-1/2, 3/2 > I=3/2
! v(16) = < l=j+1/2, 3/2 |V^J| l=j+1/2, 3/2 > I=3/2
!
! v(17) = < l=j+3/2, 3/2 |V^J| l=j-1/2, 3/2 > I=1/2 pay attention here
! v(18) = < l=j-3/2, 3/2 |V^J| l=j+1/2, 3/2 > I=1/2
! v(19) = < l=j+3/2, 3/2 |V^J| l=j-1/2, 3/2 > I=3/2
! v(20) = < l=j-3/2, 3/2 |V^J| l=j+1/2, 3/2 > I=3/2
!
! v(21) = < l=j+3/2, 3/2 |V^J| l=j+3/2, 3/2 > I=1/2 pay attention here
! v(22) = < l=j-3/2, 3/2 |V^J| l=j-3/2, 3/2 > I=1/2
! v(23) = < l=j+3/2, 3/2 |V^J| l=j+3/2, 3/2 > I=3/2
! v(24) = < l=j-3/2, 3/2 |V^J| l=j-3/2, 3/2 > I=3/2
!
! v(25) = < l=j-1/2, 1/2 |V^J| l=j-1/2, 3/2 > I=1/2 pay attention here
! v(26) = < l=j+1/2, 1/2 |V^J| l=j+1/2, 3/2 > I=1/2
! v(27) = < l=j-1/2, 1/2 |V^J| l=j-1/2, 3/2 > I=3/2
! v(28) = < l=j+1/2, 1/2 |V^J| l=j+1/2, 3/2 > I=3/2
!
! v(29) = < l=j-1/2, 1/2 |V^J| l=j+3/2, 3/2 > I=1/2 pay attention here
! v(30) = < l=j+1/2, 1/2 |V^J| l=j-3/2, 3/2 > I=1/2
! v(31) = < l=j-1/2, 1/2 |V^J| l=j+3/2, 3/2 > I=3/2
! v(32) = < l=j+1/2, 1/2 |V^J| l=j-3/2, 3/2 > I=3/2
!
! v(33) = < l=j-1/2, 3/2 |V^J| l=j+3/2, 3/2 > I=1/2 pay attention here
! v(34) = < l=j+1/2, 3/2 |V^J| l=j-3/2, 3/2 > I=1/2
! v(35) = < l=j-1/2, 3/2 |V^J| l=j+3/2, 3/2 > I=3/2
! v(36) = < l=j+1/2, 3/2 |V^J| l=j-3/2, 3/2 > I=3/2
!
! and for the Npi->Dpi potentials:
!          final J   S         J        S initial
! v(1)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(2)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(3)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(4)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! v(5)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(6)  = < l=j-3/2, 3/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(7)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(8)  = < l=j-3/2, 3/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! and for the Dpi>Dpi potentials:
!          final J   S         J        S initial
! v(1)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 3/2 > I=1/2
! v(2)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 3/2 > I=1/2
! v(3)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 3/2 > I=3/2
! v(4)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 3/2 > I=3/2
!
! v(5)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 3/2 > I=1/2
! v(6)  = < l=j-3/2, 3/2 |V^J| l=j+1/2, 3/2 > I=1/2
! v(7)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 3/2 > I=3/2
! v(8)  = < l=j-3/2, 3/2 |V^J| l=j+1/2, 3/2 > I=3/2
!
! v(9)   = < l=j+3/2, 3/2 |V^J| l=j+3/2, 3/2 > I=1/2
! v(10)  = < l=j-3/2, 3/2 |V^J| l=j-3/2, 3/2 > I=1/2
! v(11)  = < l=j+3/2, 3/2 |V^J| l=j+3/2, 3/2 > I=3/2
! v(12)  = < l=j-3/2, 3/2 |V^J| l=j-3/2, 3/2 > I=3/2
!
! v(13)  = < l=j-1/2, 3/2 |V^J| l=j+3/2, 3/2 > I=1/2
! v(14)  = < l=j+1/2, 3/2 |V^J| l=j-3/2, 3/2 > I=1/2
! v(15)  = < l=j-1/2, 3/2 |V^J| l=j+3/2, 3/2 > I=3/2
! v(16)  = < l=j+1/2, 3/2 |V^J| l=j-3/2, 3/2 > I=3/2
!
!
! and for the Nrho->Dpi potentials:
!          final J   S         J        S initial
! v(1)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(2)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(3)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(4)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! v(5)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 3/2 > I=1/2
! v(6)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 3/2 > I=1/2
! v(7)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 3/2 > I=3/2
! v(8)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 3/2 > I=3/2
!
! v(9)   = < l=j-1/2, 3/2 |V^J| l=j+3/2, 3/2 > I=1/2
! v(10)  = < l=j+1/2, 3/2 |V^J| l=j-3/2, 3/2 > I=1/2
! v(11)  = < l=j-1/2, 3/2 |V^J| l=j+3/2, 3/2 > I=3/2
! v(12)  = < l=j+1/2, 3/2 |V^J| l=j-3/2, 3/2 > I=3/2
!
! v(13)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(14)  = < l=j-3/2, 3/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(15)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(16)  = < l=j-3/2, 3/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! v(17)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 3/2 > I=1/2
! v(18)  = < l=j-3/2, 3/2 |V^J| l=j+1/2, 3/2 > I=1/2
! v(19)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 3/2 > I=3/2
! v(20)  = < l=j-3/2, 3/2 |V^J| l=j+1/2, 3/2 > I=3/2
!
! v(21)  = < l=j+3/2, 3/2 |V^J| l=j+3/2, 3/2 > I=1/2
! v(22)  = < l=j-3/2, 3/2 |V^J| l=j-3/2, 3/2 > I=1/2
! v(23)  = < l=j+3/2, 3/2 |V^J| l=j+3/2, 3/2 > I=3/2
! v(24)  = < l=j-3/2, 3/2 |V^J| l=j-3/2, 3/2 > I=3/2
!
! piN->sigmaN
!          final J   S         J        S initial
! v(1)  = < l=j+1/2, 1/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(2)  = < l=j-1/2, 1/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(3)  = 0 (no I=3/2)
! v(4)  = 0
!
! sigmaN->sigmaN
!          final J   S         J        S initial
! v(1)  = < l=j+1/2, 1/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(2)  = < l=j-1/2, 1/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(3)  = 0 (no I=3/2)
! v(4)  = 0
!
! and for the Npi->N^*pi potentials:
!          final J   S         J        S initial
! v(1)  = < l=j+1/2, 3/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(2)  = < l=j-1/2, 3/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(3)  = < l=j+1/2, 3/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(4)  = < l=j-1/2, 3/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! v(5)  = < l=j-3/2, 3/2 |V^J| l=j-1/2, 1/2 > I=1/2
! v(6)  = < l=j+3/2, 3/2 |V^J| l=j+1/2, 1/2 > I=1/2
! v(7)  = < l=j-3/2, 3/2 |V^J| l=j-1/2, 1/2 > I=3/2
! v(8)  = < l=j+3/2, 3/2 |V^J| l=j+1/2, 1/2 > I=3/2
!
! and for the Dpi>Dpi potentials:
!          final J   S         J        S initial
! v(1)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 3/2 > I=1/2
! v(2)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 3/2 > I=1/2
! v(3)  = < l=j+1/2, 3/2 |V^J| l=j+1/2, 3/2 > I=3/2
! v(4)  = < l=j-1/2, 3/2 |V^J| l=j-1/2, 3/2 > I=3/2
!
! v(5)  = < l=j-3/2, 3/2 |V^J| l=j+1/2, 3/2 > I=1/2
! v(6)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 3/2 > I=1/2
! v(7)  = < l=j-3/2, 3/2 |V^J| l=j+1/2, 3/2 > I=3/2
! v(8)  = < l=j+3/2, 3/2 |V^J| l=j-1/2, 3/2 > I=3/2
!
! v(9)   = < l=j-3/2, 3/2 |V^J| l=j-3/2, 3/2 > I=1/2
! v(10)  = < l=j+3/2, 3/2 |V^J| l=j+3/2, 3/2 > I=1/2
! v(11)  = < l=j-3/2, 3/2 |V^J| l=j-3/2, 3/2 > I=3/2
! v(12)  = < l=j+3/2, 3/2 |V^J| l=j+3/2, 3/2 > I=3/2
!
! v(13)  = < l=j+1/2, 3/2 |V^J| l=j-3/2, 3/2 > I=1/2
! v(14)  = < l=j-1/2, 3/2 |V^J| l=j+3/2, 3/2 > I=1/2
! v(15)  = < l=j+1/2, 3/2 |V^J| l=j-3/2, 3/2 > I=3/2
! v(16)  = < l=j-1/2, 3/2 |V^J| l=j+3/2, 3/2 > I=3/2

module potential3
  use input4
  use prop

  complex(kind(0.d0)), dimension(4,2) :: u3,u1
  complex(kind(0.d0)), dimension(1:4,0:3,0:3) :: urs1,urs3
  complex(kind(0.d0)), dimension(0:3) ::  p1_cm,p2_cm,p3_cm,p4_cm
  complex(kind(0.d0)) ::  eps1,eps2,eps3,eps4
  real(kind(0.d0)) :: wnu,wnu2,wpi,wpi2,wrho,wrho2,weta,weta2,wdel,wdel2,wsig,&
      &wsig2,wns,wns2,wrp,wrp2,wk,wk2,wlam,wlam2,&
      &wd,wd2,wsigc,wsigc2,wlamc,wlamc2,wds,wds2
  real(kind(0.d0)) :: wom,wom2
  real(kind(0.d0)) :: cs_cm,sn_cm,cs_cm_h,sn_cm_h
  complex(kind(0.d0)) :: pcmf,pcmi,ptildef,ptildei

  private :: u3,u1,p1_cm,p2_cm,p3_cm,p4_cm,eps1,eps2,eps3,eps4,wnu,wnu2,wpi,wpi2,&
      &wrho,wrho2,weta,weta2,wdel,wdel2,wsig,wsig2,wns,wns2,wrp,wrp2,wk,wk2,&
      &wlam,wlam2,cs_cm,sn_cm,cs_cm_h,sn_cm_h,pcmf,pcmi,urs1,urs3,&
      &wd,wd2,wsigc,wsigc2,wlamc,wlamc2,wds,wds2

contains

  !=======================================================================!

  subroutine ccpot(v,pi_cm,pf_cm,ecm,i1,i2)
      implicit none

      complex(kind(0.d0)) :: v(36)         ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm   ! input
      complex(kind(0.d0)) ::     ecm       ! input
      integer :: i1,i2                     ! input

      ! here we have to call the potentials according to the channels i1,i2

      pcmi=pi_cm
      pcmf=pf_cm

      !    p  r  e  p  s  o  L  S  L  S
      !    i  h  t  i  i  m  a  i  c  i
      !    N  o  a  D  g  g  m  g  m  g
      !       N  N     N  N  K  K  c  c
      !                            D  D
      !
      ! ( 1  2  3  4  5  6  7  8  9 10 11 12)  ccrel
      ! ( 2 13 14 15 16 17 18 19 20 21 22 23)
      ! ( 3 14 24 25 26 27 28 29 30 31 32 33)
      ! ( 4 15 25 34 35 36 37 38 39 40 41 42)
      ! ( 5 16 26 35 43 44 45 46 47 48 49 50)
      ! ( 6 17 27 36 44 51 52 53 54 55 56 57)
      ! ( 7 18 28 37 45 52 58 59 60 61 62 63)
      ! ( 8 19 29 38 46 53 59 64 65 66 67 68)
      ! ( 9 20 30 39 47 54 60 65 69 70 71 72)
      ! (10 21 31 40 48 55 61 66 70 73 74 75)
      ! (11 22 32 41 49 56 62 67 71 74 76 77)
      ! (12 23 33 42 50 57 63 68 72 75 77 78)

      select case (ccrel(i1,i2))

      case(1)   ! 1->1
        call v_piN_piN(v,pf_cm,pi_cm,ecm)
      case(2)   ! 1->2, 2->1
        call v_piN_rhoN(v,pf_cm,pi_cm,ecm)
      case(3)   ! 1->3, 3->1
        call v_piN_etaN(v,pf_cm,pi_cm,ecm)
      case(4)   ! 1->4, 4->1
        call v_piN_piD(v,pf_cm,pi_cm,ecm)
      case(5)   ! 1->5, 5->1
        call v_piN_sigN(v,pf_cm,pi_cm,ecm)
      case(7)   ! 1->7, 7->1
        call v_piN_LamK(v,pf_cm,pi_cm,ecm)
      case(8)   ! 1->8, 8->1
        call v_piN_SigK(v,pf_cm,pi_cm,ecm)

      case(13)   ! 2->2
        call v_rhoN_rhoN(v,pf_cm,pi_cm,ecm)
      case(15)  ! 2->4
        call v_rhoN_piD(v,pf_cm,pi_cm,ecm)

      case(24)  ! 3->3
        call v_etaN_etaN(v,pf_cm,pi_cm,ecm)

      case(28)  ! 3->7, 7->3
        call v_Neta_LamK(v,pf_cm,pi_cm,ecm)     ! new Dec. 2, 2011. Naming: Neta because of Baryon first convention
      case(29)  ! 3->8, 8->3
        call v_Neta_SigK(v,pf_cm,pi_cm,ecm)

      case(34)  ! 4->4
        call v_piD_piD(v,pf_cm,pi_cm,ecm)
      case(35)  ! 5->4, 4->5
        call v_sigN_piD(v,pf_cm,pi_cm,ecm)

      case(43)  ! 5->5
        call v_sigN_sigN(v,pf_cm,pi_cm,ecm)

      case(58)  ! 7->7
        call v_LamK_LamK(v,pf_cm,pi_cm,ecm)
      case(59)  ! 7->8
        call v_LamK_SigK(v,pf_cm,pi_cm,ecm)

      case(64)  ! 8->8
        call v_SigK_SigK(v,pf_cm,pi_cm,ecm)

      case(69)  ! 9->9
        call v_LamcD_LamcD(v,pf_cm,pi_cm,ecm)
      case(70)  ! 9->10
        call v_LamcD_SigcD(v,pf_cm,pi_cm,ecm)

      case(73)  ! 10->10
        call v_SigcD_SigcD(v,pf_cm,pi_cm,ecm)

      case(71)  ! 9->11
        call v_LamcD_LamcDs(v,pf_cm,pi_cm,ecm)
      case(72)  ! 9->12
        call v_LamcD_SigcDs(v,pf_cm,pi_cm,ecm)

      case(74)  ! 10->11
        call v_SigcD_LamcDs(v,pf_cm,pi_cm,ecm)
      case(75)  ! 10->12
        call v_SigcD_SigcDs(v,pf_cm,pi_cm,ecm)

      case(76)  ! 11->11
        call v_LamcDs_LamcDs(v,pf_cm,pi_cm,ecm)
      case(77)  ! 11->12
        call v_LamcDs_SigcDs(v,pf_cm,pi_cm,ecm)

      case(78)  ! 12->12
        call v_SigcDs_SigcDs(v,pf_cm,pi_cm,ecm)

      case default
        v=(0.d0,0.d0)

      end select

      return
  end subroutine ccpot

  !=======================================================================!
  !  piN -> piN potential
  !=======================================================================!

  subroutine v_piN_piN(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)        ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm  ! input
      complex(kind(0.d0)) :: ecm          ! input
      type(couplings)     :: pc,pc2
      complex(kind(0.d0)) :: cnorm        ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:16)
      complex(kind(0.d0)) :: w(16,2)
      complex(kind(0.d0)) :: w_iso(16,2,2)
      complex(kind(0.d0)) :: w_lsj(16,4)
      integer             :: i,ityp,ihel,ng

      !  masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)

      !  on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wpi2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wpi2 -wnu2 )/(2.d0*ecm)
      eps3 = eps1
      eps4 = eps2
      !  momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wpi2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      !  energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wnu2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wpi2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wnu)

      !  angle-independent quantities

      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wnu)/p3_cm(0) &
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      !  spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(1,1))
          w(ityp,ihel) = (0.d0, 0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt
        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        !  momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing nucleon
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wnu)

        if(coupl(1,1,1)%idiag == 1) then        ! u-channel nucleon exchange
          call d_b(v_hel(:,1),coupl(1,1,1),ecm)
        end if

        if(coupl(1,1,2)%idiag == 1) then        ! sigma (pi-pi) exchange
          call d_c(v_hel(:,2),coupl(1,1,2),ecm)
        else if(coupl(1,1,2)%idiag == 2) then   ! stable sigma exchange
          call d_cs(v_hel(:,2),coupl(1,1,2),ecm,a0_cd)
        else if(coupl(1,1,2)%idiag == 3) then   ! modified sigma exchange
          call d_cr(v_hel(:,2),coupl(1,1,2),ecm,cs_cm)
        end if

        if(coupl(1,1,3)%idiag == 1) then        ! rho (pi-pi) exchange
          call d_d(v_hel(:,3),coupl(1,1,3),ecm)
        else if(coupl(1,1,3)%idiag == 2) then   ! stable rho exchange
          call d_ds(v_hel(:,3),coupl(1,1,3),ecm)
        else if(coupl(1,1,3)%idiag == 3) then   ! modified rho exchange
          call d_dr(v_hel(:,3),coupl(1,1,3),ecm)
        else if(coupl(1,1,3)%idiag == 4) then   ! rho exchange a la hoehler
          call d_dh(v_hel(:,3),coupl(1,1,3),ecm)
        else if(coupl(1,1,3)%idiag == 5) then   ! rho exchange a la hoehler modified
          call d_dhr(v_hel(:,3),coupl(1,1,3),ecm,cs_cm)
        end if

        if(coupl(1,1,4)%idiag == 1) then        ! u-channel delta exchange
          call d_f(v_hel(:,4),coupl(1,1,4),ecm)
        end if

        if(coupl(1,1,5)%idiag == 1) then        ! contact graph
          call d_g(v_hel(:,5),coupl(1,1,5),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(1,1))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do  ! ng-loop

      !  isospin factors

      do ihel = 1,2

        w_iso(1,ihel,1) = - w(1,ihel)              ! I = 1/2  nucleon u
        w_iso(1,ihel,2) =   w(1,ihel) * 2.d0       ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel)              ! I = 1/2  sigma t
        w_iso(2,ihel,2) =   w(2,ihel)              ! I = 3/2

        w_iso(3,ihel,1) =   w(3,ihel) * 2.d0       ! I = 1/2  rho t
        w_iso(3,ihel,2) = - w(3,ihel)              ! I = 3/2

        w_iso(4,ihel,1) =   w(4,ihel) * 4.d0/3.d0  ! I = 1/2  delta u
        w_iso(4,ihel,2) =   w(4,ihel)       /3.d0  ! I = 3/2

        w_iso(5,ihel,1) = - w(5,ihel) * 2.d0*ci    ! I = 1/2  contact
        w_iso(5,ihel,2) =   w(5,ihel) * ci         ! I = 3/2

      end do

      !  transformation to lsj
      do i=1,itypmax(ccrel(1,1))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)  ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)  ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)  ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)  ! l=j+1/2; I=3/2
      end do

      !  summing all contributions  w(ityp, ihel, iso )

      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(1,1))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_piN_piN

  !=======================================================================!

  subroutine monopole2(ff,alf,amf,psq)
      implicit none

      complex(kind(0.d0)) :: ff             !output
      real(kind(0.d0)) ::    alf,amf        ! input
      complex(kind(0.d0)) :: psq            ! input

      ff=(alf**2-amf**2)/(alf**2+psq)

      return
  end subroutine monopole2

  !=======================================================================!

  subroutine tr_del(s_del,p_a,p_b,p_r,uf,ui,amr)
      implicit none
      !
      ! contraction:
      !   uf_bar * p_a^mu * p_b^nu * ( p_r(slash) + amr ) * delta_mu_nu * ui
      !
      !   this routine uses the rarita schwinger propagator for the delta
      !   see schuetz (3.12)
      !
      complex(kind(0.d0)) :: s_del(1:2)                ! output
      !     momenta
      complex(kind(0.d0)) :: p_a(0:3),p_b(0:3),p_r(0:3)
      real(kind(0.d0)) ::     amr
      !     spinors
      complex(kind(0.d0)) :: uf(1:4,1:2),ui(1:4,1:2)
      !
      ! intermediate storage
      complex(kind(0.d0)) :: ab,ar,br,ca,cb,diag
      complex(kind(0.d0)) :: a   ( 1:4, 1:4 )
      complex(kind(0.d0)) :: b   ( 1:4, 1:4 )
      complex(kind(0.d0)) :: c   ( 1:4, 1:4 )
      complex(kind(0.d0)) :: sf  ( 1:4, 1:4 )
      complex(kind(0.d0)) :: u_a1(1:4),u_a2(1:4)
      complex(kind(0.d0)) :: scal
      integer :: i,j,ihel
      !
      !  scalar products
      call vector_dot_vector ( ab, p_a, p_b )
      call vector_dot_vector ( ar, p_a, p_r )
      call vector_dot_vector ( br, p_b, p_r )
      ca=ar/(3.d0*amr)
      cb=br/(3.d0*amr)
      diag = ar*br*2.d0/(3.d0*amr**2) - ab
      !
      !  matrices
      call p_slash (  a, p_a )
      call p_slash (  b, p_b )
      call p_slash ( sf, p_r )   ! numerator of feynman propagator
      do i=1,4
        sf(i,i)=sf(i,i)+amr
      enddo
      !
      call mat_mat(c,a,b)
      !
      do i = 1,4
        do j = 1,4
          c(i,j)=c(i,j)/3.d0 + a(i,j)*cb - b(i,j)*ca
        enddo
      enddo
      do i = 1,4
        c(i,i)=c(i,i) + diag
      enddo
      !
      call mat_spinor ( u_a1, c , ui(:,1) )
      call mat_spinor ( u_a2, sf, u_a1    )
      do ihel=1,2
        call spinor_bar_spinor ( scal, uf(:,ihel), u_a2 )
        s_del(ihel)=scal
      enddo
      !
      return
  end subroutine tr_del

  !=======================================================================!

  subroutine d_b(hel,pc,ecms)   ! N-exchange in pi N --> pi N
      implicit none

      ! output
      complex(kind(0.d0)), dimension(1:2) :: hel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)) :: g_pv_2(1:4,1:4)
      complex(kind(0.d0)) :: g_pv_4(1:4,1:4)
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer ::  i,ihel
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      end do
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      e_u   = cdsqrt(pc%wf**2+pu_sq)

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.d0 * e_u )

      call vertex_pv(g_pv_2,p2_cm )
      call vertex_pv(g_pv_4,-1.d0*p4_cm )

      call mat_spinor(ux,g_pv_4,u1(:,1))
      call mat_spinor(uy,sf,ux)
      call mat_spinor(ux,g_pv_2,uy)
      do i=1,2
        call spinor_bar_spinor(hel(i),u3(:,i),ux)
      end do

      aux=(wnu2-wpi2)/wnu
      ff=(pc%al1**2-wnu2)/(pc%al1**2-aux**2+pu_sq)
      fi=ff*ff
      ff=fi**pc%npow
      if(imodel==3) then
        call ff_3(ff,p_u(0),p_u,eps1,p1_cm,eps4,p4_cm,2,2,5)
        call ff_3(fi,p_u(0),p_u,eps3,p3_cm,eps2,p2_cm,2,2,5)
        ff = fi*ff
      end if

      cc = ff * pc%gc
      do i=1,2
        hel(i)=hel(i)*cc
      end do

      return
  end subroutine d_b

  !=======================================================================!

  subroutine d_c(hel,pc,ecms)  ! correlated pi-pi for sigma in piN -> piN
      implicit none

      complex(kind(0.d0)) :: hel(1:2)   ! output
      ! input
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      !  intermediate result
      complex(kind(0.d0)) :: a0
      !  intermediate storage
      complex(kind(0.d0)) :: t0         ! subtracted dispersion relation
      complex(kind(0.d0)) :: p_t(0:3),t_off
      complex(kind(0.d0)) :: pt_sq
      integer ::  itp
      complex(kind(0.d0)) :: e_exc, prop
      complex(kind(0.d0)) :: ff,fi
      complex(kind(0.d0)) :: fac_sub
      complex(kind(0.d0)) :: scal
      integer :: ihel

      t0    = 2.d0*wpi2
      p_t(1)= p1_cm(1) - p3_cm(1)
      p_t(3)= p1_cm(3) - p3_cm(3)
      t_off = - p_t(1)**2 - p_t(3)**2  ! cm

      a0=a0_cd

      ff=(1.d0,0.d0)  ! for those models attaching the ff outside loop
      do itp = 1,nd
        e_exc = cdsqrt ( t_dsp(itp) - t_off )
        prop  = ( 1.d0 / ( ecms - e_exc - p1_cm(0) -p4_cm(0) )&
            &+ 1.d0 / ( ecms - e_exc - p2_cm(0) -p3_cm(0) ) ) / ( 2.d0 * e_exc )
        ! see Schuetz Diss (2.32)
        fac_sub=(t_off-t0)*4.d0/(wnu2-0.25d0*t_dsp(itp))/(t_dsp(itp)-t0)

        !       if(imodel==1)then
        !         ff =(pc%al1**2 - t_dsp(itp)) / (pc%al1**2 - t_off)
        !         ff = ff*ff
        !       end if
        a0=a0-w_dsp(itp)*imf0(itp)*prop*fac_sub*ff
      end do
      ff=(1.d0,0.d0)  ! for those models attaching the ff inside loop
      !     fac_sub=(t_off-t0)*4.d0
      !
      if(imodel==3) then
        p_t(0)=p1_cm(0)-p3_cm(0)
        call ff_3(ff,eps1-eps3,p_t,eps2,p2_cm,eps4,p4_cm,6,5,5)
        call ff_3(fi,eps1-eps3,p_t,eps1,p1_cm,eps3,p3_cm,6,2,2)
        ff = ff*fi
      end if

      ff = pc%al1**4 / (pc%al1**2 + pcmi**2) / (pc%al1**2 + pcmf**2)
      ff = ff**pc%npow

      a0=a0*ff  ! the form factor is attached outside.....

      do ihel = 1,2
        call spinor_bar_spinor(scal,u3(:,ihel),u1(:,1))
        hel(ihel)=-scal*a0
      end do

      return
  end subroutine d_c

  !=======================================================================!

  subroutine d_cr(hel,pc,ecms,cost)  ! correlated pi-pi for sigma in piN -> piN
      implicit none                 ! modified version: replaced t-2*m_pi^2 -> -2*p2.p4

      ! output
      complex(kind(0.d0)) :: hel(1:2)
      ! input
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate result
      complex(kind(0.d0)) :: a0,pcmonsq
      real(kind(0.d0))    :: cost
      ! intermediate storage
      real(kind(0.d0)) :: t0  ! subtracted dispersion relation
      complex(kind(0.d0)) :: p_t(0:3),t_off,t_pp
      complex(kind(0.d0)) :: pt_sq
      integer ::  itp
      complex(kind(0.d0)) :: e_exc, prop
      complex(kind(0.d0)) :: ff,fi
      complex(kind(0.d0)) :: fac_sub
      complex(kind(0.d0)) :: scal
      integer :: ihel

      t0    = 2.d0*wpi2
      p_t(1)= p1_cm(1) - p3_cm(1)
      p_t(3)= p1_cm(3) - p3_cm(3)
      t_off = - p_t(1)**2 - p_t(3)**2   ! cm
      ! corresponds to t-2*mpi^2 -> -2p2.p4
      t_pp = -2.d0*p2_cm(0)*p4_cm(0)+2.d0*p2_cm(1)*p4_cm(1)+2.d0*p2_cm(3)*p4_cm(3)  ! cm
      ! PUT BACK ON-SHELL:
      !      pcmonsq=1.d0/(4.d0*ecms**2)*(ecms**2-(wnu+wpi)**2)*(ecms**2-(wnu-wpi)**2)
      !      t_pp = -2.d0*(wpi**2+pcmonsq*(1.d0-cost))

      a0=a0_cd

      ff=(1.d0,0.d0)  ! for those models attaching the ff outside loop
      do itp = 1,nd
        e_exc = cdsqrt ( t_dsp(itp) - t_off )
        prop  = ( 1.d0 / ( ecms - e_exc - p1_cm(0) -p4_cm(0) )&
            &+ 1.d0 / ( ecms - e_exc - p2_cm(0) -p3_cm(0) ) ) / ( 2.d0 * e_exc )
        ! see Schuetz Diss (2.32)
        fac_sub=t_pp*4.d0/(wnu2-0.25d0*t_dsp(itp))/(t_dsp(itp)-t0)

        !       if(imodel==1)then
        !         ff =(pc%al1**2 - t_dsp(itp)) / (pc%al1**2 - t_off)
        !         ff = ff*ff
        !       end if
        a0=a0-w_dsp(itp)*imf0(itp)*prop*fac_sub*ff
      end do
      ff=(1.d0,0.d0)  ! for those models attaching the ff inside loop

      if(imodel==3) then
        p_t(0)=p1_cm(0)-p3_cm(0)
        call ff_3(ff,eps1-eps3,p_t,eps2,p2_cm,eps4,p4_cm,6,5,5)
        call ff_3(fi,eps1-eps3,p_t,eps1,p1_cm,eps3,p3_cm,6,2,2)
        ff = ff*fi
      end if

      ff = pc%al1**4 / (pc%al1**2 + pcmi**2) / (pc%al1**2 + pcmf**2)
      ff = ff**pc%npow

      a0=a0*ff  ! the form factor is attached outside.....

      do ihel = 1,2
        call spinor_bar_spinor(scal,u3(:,ihel),u1(:,1))
        hel(ihel)=-scal*a0
      end do

      return
  end subroutine d_cr

  !=======================================================================!

  subroutine d_cs(hel,pc,ecms,a0cd)  ! stable sigma exchange in piN -> piN
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2)
      ! input
      type(couplings) :: pc
      complex(kind(0.d0)) ::  ecms
      real(kind(0.d0)) ::  a0cd
      ! intermediate storage
      complex(kind(0.d0)) :: p_t(0:3)
      complex(kind(0.d0)) :: p_tsq,p_t4sq,omega_t
      complex(kind(0.d0)) :: prop
      complex(kind(0.d0)) :: gpps,gnns
      complex(kind(0.d0)) :: ff,fi
      complex(kind(0.d0)) :: scal
      integer :: ihel,i

      do i=1,3
        p_t(i)=p1_cm(i)-p3_cm(i)
      end do

      p_tsq=p_t(1)**2+p_t(2)**2+p_t(3)**2  ! 3 momentum squared

      omega_t=cdsqrt(p_tsq+pc%wf**2)

      prop  = ( 1.d0 / ( ecms - omega_t - p1_cm(0) -p4_cm(0) )&
          &+ 1.d0 / ( ecms - omega_t - p2_cm(0) -p3_cm(0) ) ) / ( 2.d0 * omega_t )

      ! call sigmapipi Vertexfunction
      call vertex_pipisig(gpps,p2_cm,p4_cm)

      ! the sigmaNN vertex is trivial
      gNNs = 1.d0

      ! the formfactors:
      ff=(( pc%al1**2 - pc%aw**2 )/( pc%al1**2 + p_tsq ))**pc%npow
      if (imodel==3) then
        call ff_3(ff,eps1-eps3,p_t,eps2,p2_cm,eps4,p4_cm,6,5,5)
        call ff_3(fi,eps1-eps3,p_t,eps1,p1_cm,eps3,p3_cm,6,2,2)
        ff = ff*fi
      end if

      ! this is for the cheng dashen contact graph
      fi=(pc%al2**2-pc%aw**2)/(pc%al2**2+p_tsq)
      fi=fi*fi

      do ihel = 1,2
        call spinor_bar_spinor(scal,u3(:,ihel),u1(:,1))
        hel(ihel)=scal*(gpps*prop*gNNs*pc%gc*ff+a0cd*fi)
      end do
      return
  end subroutine d_cs

  !=======================================================================!

  subroutine d_d(hel,pc,ecms)  ! correlated pi-pi for rho in piN -> piN
      implicit none                ! a la frazer & fulco (using f-amplitudes dispersion relations)

      ! output
      complex(kind(0.d0)) :: hel(1:2)
      ! input
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate result
      complex(kind(0.d0)) :: f1p,f1m
      complex(kind(0.d0)) :: a_dsp,b_dsp
      ! intermediate storage
      complex(kind(0.d0)) :: s_off,p_t(0:3),t_off,p,q,x
      complex(kind(0.d0)) :: pt_sq
      integer ::  itp
      complex(kind(0.d0)) :: e_exc, prop
      complex(kind(0.d0)) :: ff,fi  ! form factor
      complex(kind(0.d0)) :: e_sum_h,c_v_t,c_s_t
      complex(kind(0.d0)) :: scal,vec0
      integer :: ihel

      s_off = ( p1_cm(0) + p2_cm(0) ) * ( p3_cm(0) + p4_cm(0) )
      p_t(1)= p1_cm(1) - p3_cm(1)
      p_t(3)= p1_cm(3) - p3_cm(3)
      t_off = - p_t(1)**2 - p_t(3)**2   ! cm
      p  = cdsqrt ( 0.25d0 * t_off - wnu2 )
      q  = cdsqrt ( 0.25d0 * t_off - wpi2 )
      x  = ( s_off + 0.5d0 * t_off - wnu2 - wpi2) / (2.d0*p*q)
      f1p=(0.d0,0.d0)
      f1m=(0.d0,0.d0)

      ff=(1.d0,0.d0)  ! for those models attaching the ff outside loop
      do itp = 1,nd
        e_exc = cdsqrt ( t_dsp(itp) - t_off )
        prop  = ( 1.d0 / ( ecms - e_exc - p1_cm(0) -p4_cm(0) )&
            &+ 1.d0 / ( ecms - e_exc - p2_cm(0) -p3_cm(0) ) ) / ( 2.d0 * e_exc )

        !       if(imodel==1)then
        !         ff =(pc%al1**2 - t_dsp(itp)) / (pc%al1**2 - t_off)
        !         ff = ff*ff
        !       end if
        !
        f1p=f1p-w_dsp(itp)*imf1p(itp)*prop*ff
        f1m=f1m-w_dsp(itp)*imf1m(itp)*prop*ff
      end do
      ff=(1.d0,0.d0)  ! for those models attaching the ff inside loop

      if (imodel==3) then
        p_t(0)=p1_cm(0)-p3_cm(0)
        call ff_3(ff,eps1-eps3,p_t,eps2,p2_cm,eps4,p4_cm,7,5,5)
        call ff_3(fi,eps1-eps3,p_t,eps1,p1_cm,eps3,p3_cm,7,2,2)
        ff = ff*fi
      end if

      ff = pc%al1**4 / (pc%al1**2 + pcmi**2) / (pc%al1**2 + pcmf**2)
      ff = ff**pc%npow

      f1p=f1p*ff
      f1m=f1m*ff  ! form factors attached outside

      a_dsp = 12.d0*q/p*x * ( wnu/dsqrt(2.d0)*f1m-f1p )
      b_dsp = 12.d0/dsqrt(2.d0)*f1m

      e_sum_h = 0.5d0*(p1_cm(0)+p2_cm(0)+p3_cm(0)+p4_cm(0)) !t-channel
      c_v_t = b_dsp*e_sum_h
      c_s_t = a_dsp - b_dsp*wnu
      do ihel = 1,2
        call spinor_bar_spinor(scal,u3(:,ihel),u1(:,1))
        call spinor_dag_spinor(vec0,u3(:,ihel),u1(:,1))
        hel(ihel) = - (scal*c_s_t + vec0*c_v_t)
      end do

      return
  end subroutine d_d

  !=======================================================================!

  subroutine d_dr(hel,pc,ecms)  ! correlated pi-pi for rho in piN -> piN
      implicit none                 ! a la frazer & fulco (using f-amplitudes dispersion relations)
      ! modified version: different kinematical factors

      ! output
      complex(kind(0.d0)) :: hel(1:2)
      ! input
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate result
      complex(kind(0.d0)) :: f1p,f1ma,f1mb
      complex(kind(0.d0)) :: a_dsp,b_dsp
      ! intermediate storage
      complex(kind(0.d0)) :: s_off,p_t(0:3),t_off,pqx,psum2,t_off_4,pt2
      complex(kind(0.d0)) :: pt_sq
      integer ::  itp
      complex(kind(0.d0)) :: e_exc, prop,fac_sub
      complex(kind(0.d0)) :: ff,fi   ! form factor
      complex(kind(0.d0)) :: e_sum_h,c_v_t,c_s_t
      complex(kind(0.d0)) :: scal,vec0
      integer :: ihel

      s_off = ( p1_cm(0) + p2_cm(0) ) * ( p3_cm(0) + p4_cm(0) )
      p_t(0)= p1_cm(0) - p3_cm(0)
      p_t(1)= p1_cm(1) - p3_cm(1)
      p_t(3)= p1_cm(3) - p3_cm(3)
      t_off = - p_t(1)**2 - p_t(3)**2   ! cm
      t_off_4 = p_t(0)**2- p_t(1)**2 - p_t(3)**2   ! cm

      psum2=(p1_cm(1)+p3_cm(1))*(p1_cm(1)+p3_cm(1))+(p1_cm(3)+p3_cm(3))*(p1_cm(3)+p3_cm(3))
      pqx=0.25d0*((p1_cm(0)+p3_cm(0))*(p2_cm(0)+p4_cm(0))+psum2)
      pt2=0.25d0*t_off_4 - wnu2

      f1p=(0.d0,0.d0)
      f1ma=(0.d0,0.d0)
      f1mb=(0.d0,0.d0)

      ff=(1.d0,0.d0)   ! for those models attaching the ff outside loop
      do itp = 1,nd
        e_exc = cdsqrt ( t_dsp(itp) - t_off )
        prop  = ( 1.d0 / ( ecms - e_exc - p1_cm(0) -p4_cm(0) )&
            &+ 1.d0 / ( ecms - e_exc - p2_cm(0) -p3_cm(0) ) ) / ( 2.d0 * e_exc )

        !       if(imodel==1)then
        !         ff =(pc%al1**2 - t_dsp(itp)) / (pc%al1**2 - t_off)
        !         ff = ff*ff
        !       end if

        fac_sub=1.d0/(0.25d0*t_dsp(itp)-wnu2)
        f1p=f1p-w_dsp(itp)*imf1p(itp)*prop*ff*fac_sub
        f1ma=f1ma-w_dsp(itp)*imf1m(itp)*prop*ff*fac_sub
        f1mb=f1mb-w_dsp(itp)*imf1m(itp)*prop*ff
      end do
      ff=(1.d0,0.d0)   ! for those models attaching the ff inside loop

      if (imodel==3) then
        p_t(0)=p1_cm(0)-p3_cm(0)
        call ff_3(ff,eps1-eps3,p_t,eps2,p2_cm,eps4,p4_cm,7,5,5)
        call ff_3(fi,eps1-eps3,p_t,eps1,p1_cm,eps3,p3_cm,7,2,2)
        ff = ff*fi
      end if

      ff = pc%al1**4 / (pc%al1**2 + pcmi**2) / (pc%al1**2 + pcmf**2)
      ff = ff**pc%npow

      f1p=f1p*ff
      f1ma=f1ma*ff  ! form factors attached outside
      f1mb=f1mb*ff  ! form factors attached outside

      a_dsp = 12.d0*pqx * ( wnu/dsqrt(2.d0)*f1ma-f1p )
      b_dsp = 12.d0/dsqrt(2.d0)*f1mb

      e_sum_h = 0.5d0*(p1_cm(0)+p2_cm(0)+p3_cm(0)+p4_cm(0))  ! t-channel
      c_v_t = b_dsp*e_sum_h
      c_s_t = a_dsp - b_dsp*wnu
      do ihel = 1,2
        call spinor_bar_spinor(scal,u3(:,ihel),u1(:,1))
        call spinor_dag_spinor(vec0,u3(:,ihel),u1(:,1))
        hel(ihel) = - (scal*c_s_t + vec0*c_v_t)
      end do

      return
  end subroutine d_dr

  !=======================================================================!

  subroutine d_dh(hel,pc,ecms)  ! correlated pi-pi for rho in piN -> piN
      implicit none                 ! a la Hoehler (using combination of f amplitudes)

      ! output
      complex(kind(0.d0)) :: hel(1:2)
      ! input
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate result
      complex(kind(0.d0)) :: gamma1,gamma2
      complex(kind(0.d0)) :: a_dsp,b_dsp
      ! intermediate storage
      real(kind(0.d0)) :: gamma1dsp,gamma2dsp
      complex(kind(0.d0)) :: s_off,p_t(0:3),t_off,p,q,x
      complex(kind(0.d0)) :: pt_sq,pt2
      integer ::  itp
      complex(kind(0.d0)) :: e_exc, prop
      complex(kind(0.d0)) :: ff,ff1,ff2,fi   ! form factor
      complex(kind(0.d0)) :: e_sum_h,c_v_t,c_s_t
      complex(kind(0.d0)) :: scal,vec0
      integer :: ihel
      complex(kind(0.d0)) :: dwnu4sqrt2,wnudsqrt2

      s_off = ( p1_cm(0) + p2_cm(0) ) * ( p3_cm(0) + p4_cm(0) )
      p_t(1)= p1_cm(1) - p3_cm(1)
      p_t(3)= p1_cm(3) - p3_cm(3)
      t_off = - p_t(1)**2 - p_t(3)**2   ! cm
      p = cdsqrt ( 0.25d0 * t_off - wnu2 )
      q = cdsqrt ( 0.250 * t_off - wpi2 )
      x = ( s_off + 0.5d0 * t_off - wnu2 - wpi2) / (2.d0*p*q)

      dwnu4sqrt2 = 1.d0/(dsqrt(32.d0)*wnu)
      wnudsqrt2 = wnu/dsqrt(2.d0)

      gamma1=(0.d0,0.d0)
      gamma2=(0.d0,0.d0)

      ff=(1.d0,0.d0)   ! for those models attaching the ff outside loop
      do itp = 1,nd
        e_exc = cdsqrt ( t_dsp(itp) - t_off )
        prop  = ( 1.d0 / ( ecms - e_exc - p1_cm(0) -p4_cm(0) )&
            &+ 1.d0 / ( ecms - e_exc - p2_cm(0) -p3_cm(0) ) ) / ( 2.d0 * e_exc )

        !       if(imodel==1)then
        !         ff =(pc%al1**2 - t_dsp(itp)) / (pc%al1**2 - t_off)
        !         ff = ff*ff
        !       end if

        pt2=t_dsp(itp)*0.25d0-wnu2
        gamma1dsp=-wnu/pt2*(imf1p(itp)-t_dsp(itp)*dwnu4sqrt2*imf1m(itp))
        gamma2dsp=wnu/pt2*(imf1p(itp)-wnudsqrt2*imf1m(itp))
        gamma1=gamma1-w_dsp(itp)*gamma1dsp*prop*ff
        gamma2=gamma2-w_dsp(itp)*gamma2dsp*prop*ff
      end do
      ff=(1.d0,0.d0)  ! for those models attaching the ff inside loop

      ff1 = pc%al1**4 / (pc%al1**2 + pcmi**2) / (pc%al1**2 + pcmf**2)
      ff1 = ff1**pc%npow

      ff2 = pc%al2**4 / (pc%al2**2 + pcmi**2) / (pc%al2**2 + pcmf**2)
      ff2 = ff2**pc%npow

      gamma1=gamma1*ff1
      gamma2=gamma2*ff2  ! form factors attached outside

      a_dsp = -12.d0*q*p*x/wnu * gamma2
      b_dsp = 12.d0*(gamma1+gamma2)

      e_sum_h = 0.5d0*(p1_cm(0)+p2_cm(0)+p3_cm(0)+p4_cm(0)) !t-channel
      c_v_t = b_dsp*e_sum_h
      c_s_t = a_dsp - b_dsp*wnu
      do ihel = 1,2
        call spinor_bar_spinor(scal,u3(:,ihel),u1(:,1))
        call spinor_dag_spinor(vec0,u3(:,ihel),u1(:,1))
        hel(ihel) = - (scal*c_s_t + vec0*c_v_t)
      end do

      return
  end subroutine d_dh

  !=======================================================================!

  subroutine d_dhr(hel,pc,ecms,cost)  ! correlated pi-pi for rho in piN -> piN
      implicit none                  ! a la Hoehler (using combination of f amplitudes)

      ! output
      complex(kind(0.d0)) :: hel(1:2)
      ! input
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate result
      complex(kind(0.d0)) :: gamma1,gamma2,pcmonsq,epion,enon
      complex(kind(0.d0)) :: a_dsp,b_dsp
      real(kind(0.d0))    :: cost
      ! intermediate storage
      complex(kind(0.d0)) :: gamma1dsp,gamma2dsp
      complex(kind(0.d0)) :: s_off,p_t(0:3),t_off,p,q,x
      complex(kind(0.d0)) :: pt_sq,pt2,psum2,pqx
      integer :: itp
      complex(kind(0.d0)) :: e_exc, prop
      complex(kind(0.d0)) :: ff,ff1,ff2,fi   ! form factor
      complex(kind(0.d0)) :: e_sum_h,c_v_t,c_s_t
      complex(kind(0.d0)) :: scal,vec0
      integer :: ihel
      complex(kind(0.d0)) :: dwnu4sqrt2,wnudsqrt2

      s_off = ( p1_cm(0) + p2_cm(0) ) * ( p3_cm(0) + p4_cm(0) )
      p_t(1)= p1_cm(1) - p3_cm(1)
      p_t(3)= p1_cm(3) - p3_cm(3)
      t_off = - p_t(1)**2 - p_t(3)**2   ! cm

      ! Setting on-shell here: ..... to be done. First, set here -psum2 in second line and see what happens --> Catastrophy!!

      psum2=(p1_cm(1)+p3_cm(1))*(p1_cm(1)+p3_cm(1))+(p1_cm(3)+p3_cm(3))*(p1_cm(3)+p3_cm(3))
      pqx=0.25d0*((p1_cm(0)+p3_cm(0))*(p2_cm(0)+p4_cm(0))+psum2)  ! psum2 should be added with minus, but was programmed as +.

      ! ON-SHELL:

      !      pcmonsq=1.d0/(4.d0*ecms**2)*(ecms**2-(wnu+wpi)**2)*(ecms**2-(wnu-wpi)**2)
      !      epion=sqrt(wpi**2+pcmonsq)
      !      enon =sqrt(wnu**2+pcmonsq)
      !      pqx=epion*enon-pcmonsq/2.d0*(1.d0+cost) ! On-shell. Minus corresponds to programmed case.

      dwnu4sqrt2 = 1.d0/(dsqrt(32.d0)*wnu)
      wnudsqrt2 = wnu/dsqrt(2.d0)

      gamma1=(0.d0,0.d0)
      gamma2=(0.d0,0.d0)

      ff=(1.d0,0.d0)   ! for those models attaching the ff outside loop
      do itp = 1,nd
        e_exc = cdsqrt ( t_dsp(itp) - t_off )
        prop  = ( 1.d0 / ( ecms - e_exc - p1_cm(0) -p4_cm(0) )&
            &+ 1.d0 / ( ecms - e_exc - p2_cm(0) -p3_cm(0) ) ) / ( 2.d0 * e_exc )

        !       if(imodel==1)then
        !         ff =(pc%al1**2 - t_dsp(itp)) / (pc%al1**2 - t_off)
        !         ff = ff*ff
        !       end if

        pt2=t_dsp(itp)*0.25d0-wnu2
        gamma1dsp=-wnu/pt2*(imf1p(itp)-t_dsp(itp)*dwnu4sqrt2*imf1m(itp))
        gamma2dsp=wnu/pt2*(imf1p(itp)-wnudsqrt2*imf1m(itp))
        gamma1=gamma1-w_dsp(itp)*gamma1dsp*prop*ff
        gamma2=gamma2-w_dsp(itp)*gamma2dsp*prop*ff
        !       f1p=f1p-w_dsp(itp)*imf1p(itp)*prop*ff
        !       f1m=f1m-w_dsp(itp)*imf1m(itp)*prop*ff
      end do
      ff=(1.d0,0.d0)  ! for those models attaching the ff inside loop

      ff1 = pc%al1**4 / (pc%al1**2 + pcmi**2) / (pc%al1**2 + pcmf**2)
      ff1 = ff1**pc%npow

      ff2 = pc%al2**4 / (pc%al2**2 + pcmi**2) / (pc%al2**2 + pcmf**2)
      ff2 = ff2**pc%npow

      gamma1=gamma1*ff1
      gamma2=gamma2*ff2  ! form factors attached outside

      a_dsp = -12.d0*pqx/wnu * gamma2
      b_dsp = 12.d0*(gamma1+gamma2)

      e_sum_h = 0.5d0*(p1_cm(0)+p2_cm(0)+p3_cm(0)+p4_cm(0))  ! t-channel
      !      e_sum_h = 0.5d0*(2.d0*epion+2.d0*enon)  ! t-channel  !!! DON't FORGET FOR ON_SHELL SETTING !!!!!!!!!
      c_v_t = b_dsp*e_sum_h
      c_s_t = a_dsp - b_dsp*wnu
      do ihel = 1,2
        call spinor_bar_spinor(scal,u3(:,ihel),u1(:,1))
        call spinor_dag_spinor(vec0,u3(:,ihel),u1(:,1))
        hel(ihel) = - (scal*c_s_t + vec0*c_v_t)
      end do

      return
  end subroutine d_dhr

  !=======================================================================!

  subroutine d_ds(hel,pc,ecms)  ! stable rho exchange in pi N --> pi N
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2)
      ! input
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: p_tsq,p_t4sq
      complex(kind(0.d0)) :: omega_t,prop1,prop2,scal1,scal2
      complex(kind(0.d0)) :: ff,fi,cc    ! form factor
      complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: grnn1,grnn2
      complex(kind(0.d0)) :: gmat1(1:4,1:4),gmat2(1:4,1:4)
      complex(kind(0.d0)),dimension(1:4) :: ux,uy
      complex(kind(0.d0)) :: gppr (0:3)
      complex(kind(0.d0)),dimension(0:3) :: shel1,shel2
      integer :: ihel,i,nu,mu,is,js

      do i=1,3
        p_t1(i)=p1_cm(i)-p3_cm(i)
        p_t2(i)=p1_cm(i)-p3_cm(i)
      end do

      p_tsq = p_t1(1)**2+p_t1(2)**2+p_t1(3)**2   ! 3 momentum squared

      omega_t = cdsqrt ( p_tsq+pc%wf**2)
      p_t1(0)=omega_t
      p_t2(0)=-omega_t

      ff =((pc%al1**2 - pc%aw**2) / (pc%al1**2 + p_tsq))**pc%npow
      fi=((pc%al2**2-pc%aw**2)/(pc%al2**2+p_tsq))**pc%npow
      ff=ff*fi
      if (imodel==3) then
        call ff_3(ff,eps1-eps3,p_t1,eps2,p2_cm,eps4,p4_cm,7,5,5)
        call ff_3(fi,eps1-eps3,p_t1,eps1,p1_cm,eps3,p3_cm,7,2,2)
        ff = ff*fi
      end if

      ! call rhoNN vertexfunction without epsvec for outgoing rho
      call vertex_rhoNN(grnn1,p_t1,wnu,pc%ka)

      call vertex_rhoNN(grnn2,p_t2,wnu,pc%ka)

      ! the pipirho vertexfunction without epsvec is i*(p2+p4)^nu
      ! but a factor of i is 'missing' (absorbed) in the isospinfactor
      ! therefore put here an - sign =i*i and it should be ok again
      do nu=0,3
        gppr(nu)= -1.d0*(p2_cm(nu)+p4_cm(nu))
      end do

      ! contract lorentz indices:
      do is=1,4
        do js=1,4
          gmat1(is,js)=(0.d0,0.d0)
          gmat2(is,js)=(0.d0,0.d0)
          do mu=0,3                   ! this is only -g_mu_nu term
            gmat1(is,js)=gmat1(is,js)-gppr(mu)*grnn1(is,js,mu)*metric(mu)
            gmat2(is,js)=gmat2(is,js)-gppr(mu)*grnn2(is,js,mu)*metric(mu)
          end do
        end do
      end do

      call mat_spinor(ux,gmat1,u1(:,1))
      call mat_spinor(uy,gmat2,u1(:,1))

      prop1 = 1.d0/( ecms - omega_t - p2_cm(0) -p3_cm(0) +0.d0* dcmplx(0.d0,10.d0))/(2.d0*omega_t)
      prop2 = 1.d0/( ecms - omega_t - p1_cm(0) -p4_cm(0) +0.d0* dcmplx(0.d0,10.d0))/(2.d0*omega_t)

      do ihel = 1,2
        call spinor_bar_spinor(scal1,u3(:,ihel),ux)
        call spinor_bar_spinor(scal2,u3(:,ihel),uy)
        hel(ihel) = (scal1*prop1+scal2*prop2)*ff*pc%gc
      end do

      return
  end subroutine d_ds

  !=======================================================================!

  subroutine d_f(hel,pc,ecms)  ! delta-exchange in pi N --> pi N
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2)
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pusq,pusq4
      complex(kind(0.d0)) :: eres
      complex(kind(0.d0)) :: propa
      integer :: i,ihel
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff,f1   ! form factor
      ! for the full version (no use of tr_del)
      complex(kind(0.d0)), dimension(0:3) :: pnd1,pnd2
      complex(kind(0.d0)), dimension(1:4,1:4,0:3,0:3) :: propd
      complex(kind(0.d0)), dimension(1:4,1:4) :: shelp
      complex(kind(0.d0)), dimension(1:4) :: ux
      integer :: is,js,mu,nu

      ! set energy component on mass shell (see Schuetz Diss)
      p_u(0)=eps1-eps4
      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      enddo
      pusq = p_u(1)**2 + p_u(3)**2

      pusq4= p_u(0)**2 - pusq
      eres = cdsqrt((pc%wf-dcmplx(0.d0,0.d0))**2+pusq )
      propa=  1.d0/(ecms-eres-p1_cm(0)-p3_cm(0))&
          &+1.d0/(ecms-eres-p2_cm(0)-p4_cm(0))
      propa =propa/(2.d0*eres)

      call tr_del(hel,p2_cm,p4_cm,p_u,u3,u1,pc%wf)

      ! the piNDelta vertices:
      !      pnd1=p4_cm
      !      pnd2=p2_cm

      ! the Delta Propagator
      !      call prop_delta_rs(propd,p_u,pc%wf) ! Rarita Schwinger

      ! contract lorentz indices:
      !      do is=1,4
      !       do js=1,4
      !        shelp(is,js)=(0.d0,0.d0)
      !        do mu=0,3
      !         do nu=0,3
      !          shelp(is,js)=shelp(is,js)+metric(mu)*pnd2(mu)*propd(is,js,mu,nu)*&
      !                       &pnd1(nu)*metric(nu)
      !         enddo
      !        enddo
      !       enddo
      !      enddo

      ! sandwich between ubar and u:
      !     call mat_spinor(ux,shelp,u1(:,1))
      !     do ihel=1,2
      !      call spinor_bar_spinor(hel(ihel),u3(:,ihel),ux)
      !     enddo


      !      f1=(pc%al1**2-pc%aw**2)/(pc%al1**2+pusq) ! three-vector squared
      !      fi = f1**pc%npow
      !      ff=fi*fi
      fi=(pc%al1**2-pc%aw**2)/(pc%al1**2+pusq)
      ff=(pc%al2**2-pc%aw**2)/(pc%al2**2+pusq)
      ff=(fi*ff)**pc%npow
      if (imodel==3) then
        call ff_3(ff,eps1-eps4,p_u,eps1,p1_cm,eps4,p4_cm,4,2,5)
        call ff_3(fi,eps1-eps4,p_u,eps3,p3_cm,eps2,p2_cm,4,2,5)
        ff = ff*fi
      end if

      do ihel=1,2
        hel(ihel)=hel(ihel)*propa*ff*pc%gc
      end do

      return
  end subroutine d_f

  !=======================================================================!

  subroutine d_g(hel,pc,ecms)
      implicit none
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! pipiNN contact term
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      complex(kind(0.d0)) :: hel(1:2)            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: shelp(1:4,1:4)
      integer mu,nu,is,js
      complex(kind(0.d0)) :: psq
      complex(kind(0.d0)) :: ptilde
      complex(kind(0.d0)) :: u_x(0:3)
      integer :: i,ihel
      complex(kind(0.d0)) :: fi,ff,f1            ! form factor

      call vertex_pipiNN(shelp,p4_cm,p2_cm)
      call mat_spinor(u_x,shelp,u1(:,1))
      do i=1,2
        call spinor_bar_spinor(hel(i),u3(:,i),u_x)
      enddo

      !      if(imodel==1)then
      psq = p2_cm(1)**2+p2_cm(2)**2+p2_cm(3)**2
      f1=(pc%al1**2-pc%aw**2)/(pc%al1**2+psq)
      fi = f1**pc%npow
      psq = p4_cm(1)**2+p4_cm(2)**2+p4_cm(3)**2
      f1=(pc%al1**2-pc%aw**2)/(pc%al1**2+psq)
      ff = f1**pc%npow
      !      end if
      !      if(imodel==3)then
      !        call ff_3(ff,p_u,2,1,2,1,3)
      !      end if
      !
      do ihel=1,2
        hel(ihel)=hel(ihel)*fi*ff*pc%gc
      enddo
      return
  end subroutine d_g

  !=======================================================================!

  subroutine ff_3(ff,pl10,pl1,pl20,pl2,pl30,pl3,i1,i2,i3)

      ! output:
      complex(kind(0.d0)) :: ff
      ! input:
      complex(kind(0.d0)), dimension(0:3) :: pl1,pl2,pl3
      complex(kind(0.d0)) :: pl10,pl20,pl30
      integer :: i1,i2,i3

      ! intermediate storage
      complex(kind(0.d0)) :: psq(1:3),fi(1:3)
      integer ::  i

      ! calculate 4 momentum squared
      psq(1)=pl10**2 - pl1(1)**2 -pl1(2)**2-pl1(3)**2
      psq(2)=pl20**2 - pl2(1)**2 -pl2(2)**2-pl2(3)**2
      psq(3)=pl30**2 - pl3(1)**2 -pl3(2)**2-pl3(3)**2

      ! each legs ff:
      fi(1)=((trifo(i1)%al**2-trifo(i1)%aw**2)/&
          &(trifo(i1)%al**2-psq(1)))**trifo(i1)%npow
      fi(2)=((trifo(i2)%al**2-trifo(i2)%aw**2)/&
          &(trifo(i2)%al**2-psq(1)))**trifo(i2)%npow
      fi(3)=((trifo(i3)%al**2-trifo(i3)%aw**2)/&
          &(trifo(i3)%al**2-psq(1)))**trifo(i3)%npow

      ! the ff:
      ff=fi(1)*fi(2)*fi(3)

      return
  end subroutine ff_3


  !=======================================================================!
  !  piN -> rhoN potential
  !=======================================================================!

  subroutine v_piN_rhoN(v,pf_cm,pi_cm,ecm)

      implicit none
      complex(kind(0.d0)) :: v(36)         ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm   ! input
      complex(kind(0.d0)) ::     ecm       ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm         ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,1:14)
      complex(kind(0.d0)) :: w(14,2,-1:1)
      complex(kind(0.d0)) :: w_iso(14,2,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,2)
      integer :: i,ityp,ihel,ng,lam,lad,iso,isoindx

      ! masses
      wnu2  = masses(1,1)*masses(1,1)
      wpi2  = masses(1,2)*masses(1,2)
      wnu   = masses(1,1)
      wpi   = masses(1,2)
      wrho  = masses(3,2)
      wrho2 = masses(3,2)*masses(3,2)

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wpi2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wpi2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wnu2 -wrho2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wrho2 -wnu2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wpi2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wnu2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wrho2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wnu)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wnu)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do ihel = 1,2
        do lam=-1,1
          do ityp = 1,itypmax(ccrel(1,2))
            w(ityp,ihel,lam) = (0.d0, 0.d0)
            v_hel(ihel,lam,ityp)=(0.d0,0.d0)
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wnu)

        ! call the epsvec_mp_star, which we need in ALL dr's
        call epsvec_mp_star(eps_mp_star,pcmf,wrho,sn_cm,cs_cm)

        if(coupl(1,2,1)%idiag == 1) then
          call dr_b(v_hel(:,:,1),coupl(1,2,1),ecm)  ! u-channel nucleon exchange
        end if

        if(coupl(1,2,2)%idiag == 1) then            ! pi-rho-N-N contact term
          call dr_c(v_hel(:,:,2),coupl(1,2,2),ecm)
        end if

        if((coupl(1,2,3)%idiag == 1).or.(coupl(1,2,3)%idiag == 2)) then
          call dr_d(v_hel(:,:,3),coupl(1,2,3),ecm)  ! t-channel pi exchange
        end if

        if(coupl(1,2,4)%idiag == 1) then
          call dr_e(v_hel(:,:,4),coupl(1,2,4),ecm)  ! t-channel omega exchange
        end if

        if(coupl(1,2,5)%idiag == 1) then
          call dr_f(v_hel(:,:,5),coupl(1,2,5),ecm)  ! t-cahnnel a1 exchange
        end if

        if(coupl(1,2,6)%idiag == 1) then
          call dr_h(v_hel(:,:,6),coupl(1,2,6),ecm)  ! u-channel Delta exchange
        end if

        ! do the cos theta integration:
        do lam=-1,1
          do ihel = 1,2
            lad=3-2*ihel-2*lam
            if (abs(lad)<=j1) then
              do ityp = 1,itypmax(ccrel(1,2))
                w(ityp,ihel,lam)=w(ityp,ihel,lam)+djw(ng,lad,1)*v_hel(ihel,lam,ityp)
                ! this is 2*(la_N-la_rho)
                ! the initial Npi state has |1/2,0>
              end do
            end if
          end do
        end do

      end do  ! of ng loop

      ! isospin factors
      do ihel = 1,2
        do lam=-1,1

          w_iso(1,ihel,lam,1) = - w(1,ihel,lam)              ! I = 1/2  nucleon u
          w_iso(1,ihel,lam,2) =   w(1,ihel,lam) * 2.d0       ! I = 3/2

          w_iso(2,ihel,lam,1) = - w(2,ihel,lam) * 2.d0*ci    ! I = 1/2  contact
          w_iso(2,ihel,lam,2) =   w(2,ihel,lam) * ci         ! I = 3/2

          w_iso(3,ihel,lam,1) = - w(3,ihel,lam) * 2.d0*ci    ! I = 1/2  pion exchange
          w_iso(3,ihel,lam,2) =   w(3,ihel,lam) * ci         ! I = 3/2

          w_iso(4,ihel,lam,1) =   w(4,ihel,lam)              ! I = 1/2  omega exchange
          w_iso(4,ihel,lam,2) =   w(4,ihel,lam)              ! I = 3/2

          w_iso(5,ihel,lam,1) = - w(5,ihel,lam) * 2.d0*ci    ! I = 1/2  a1 exchange
          w_iso(5,ihel,lam,2) =   w(5,ihel,lam) * ci         ! I = 3/2

          w_iso(6,ihel,lam,1) =   w(6,ihel,lam) * 4.d0/3.d0  ! I = 1/2  Delta u
          w_iso(6,ihel,lam,2) =   w(6,ihel,lam)       /3.d0  ! I = 3/2

          w_isosum(ihel,lam,1)=(0.d0,0.d0)
          w_isosum(ihel,lam,2)=(0.d0,0.d0)
          do ityp=1,itypmax(ccrel(1,2))
            w_isosum(ihel,lam,1)=w_isosum(ihel,lam,1)+w_iso(ityp,ihel,lam,1)*cnorm
            w_isosum(ihel,lam,2)=w_isosum(ihel,lam,2)+w_iso(ityp,ihel,lam,2)*cnorm
          end do

        end do  ! lam
      end do  ! ihel

      ! trafo to jls
      do iso=1,2

        isoindx=2*iso-2

        ! transform to JLS
        v(1+isoindx) = 2.d0*(a(2,-1)*w_isosum(1,0,iso)-a(2,-1)*w_isosum(2,0,i&
            &so)+a(1,-1)*w_isosum(2,-1,iso)-a(1,-1)*w_isosum(1,1,iso))*d(1,-1)
        v(2+isoindx) = 2.d0*(a(2,1)*w_isosum(1,0,iso)+a(2,1)*w_isosum(2,0,iso&
            &)+a(1,1)*w_isosum(2,-1,iso)+a(1,1)*w_isosum(1,1,iso))*d(1,1)
        v(5+isoindx) = 2.d0*(c(3,-1)*w_isosum(1,0,iso)-c(3,-1)*w_isosum(2,0,i&
            &so)+c(2,-1)*w_isosum(2,-1,iso)-c(2,-1)*w_isosum(1,1,iso)+c(1,-1)*w&
            &_isosum(1,-1,iso)-c(1,-1)*w_isosum(2,1,iso))*d(1,-1)
        v(6+isoindx) = 2.d0*(c(3,1)*w_isosum(1,0,iso)+c(3,1)*w_isosum(2,0,iso&
            &)+c(2,1)*w_isosum(2,-1,iso)+c(2,1)*w_isosum(1,1,iso)+c(1,1)*w_isos&
            &um(1,-1,iso)+c(1,1)*w_isosum(2,1,iso))*d(1,1)
        v(9+isoindx) = 2.d0*(b(3,1)*w_isosum(1,0,iso)-b(3,1)*w_isosum(2,0,iso&
            &)+b(2,1)*w_isosum(2,-1,iso)-b(2,1)*w_isosum(1,1,iso)+b(1,1)*w_isos&
            &um(1,-1,iso)-b(1,1)*w_isosum(2,1,iso))*d(1,-1)
        v(10+isoindx) = 2.d0*(b(3,-1)*w_isosum(1,0,iso)+b(3,-1)*w_isosum(2,0,&
            &iso)+b(2,-1)*w_isosum(2,-1,iso)+b(2,-1)*w_isosum(1,1,iso)+b(1,-1)*&
            &w_isosum(1,-1,iso)+b(1,-1)*w_isosum(2,1,iso))*d(1,1)

      end do  ! iso=1,2

  end subroutine v_piN_rhoN

  !=======================================================================!

  subroutine dr_b(hel,pc,ecms)  ! N-exchange for pi N --> rho N
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2,-1:1)
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)) :: g_rho_4(1:4,1:4,-1:1)
      complex(kind(0.d0)) :: g_pv_2( 1:4, 1:4 )
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer :: i,lam
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff,fff  ! form factor

      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      end do
      pu_sq = p_u(1)**2 + p_u(3)**2   ! cm

      e_u = cdsqrt(pc%wf**2+pu_sq)

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)

      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.d0 * e_u )

      call vertex_pv(g_pv_2,p2_cm )

      call vertex_rhoNN2(g_rho_4,p4_cm,eps_mp_star,wnu,pc%ka)

      do lam=-1,1
        call mat_spinor(ux,g_rho_4(:,:,lam),u1(:,1))
        call mat_spinor(uy,sf,ux)
        call mat_spinor(ux,g_pv_2,uy)
        do i=1,2
          call spinor_bar_spinor(hel(i,lam),u3(:,i),ux)
        end do
      end do

      ! We are using the formfactors as Kanzo uses, which means: there is no energy dependence
      ! in due to the gauge invariance problem by coupling the photon
      aux=(wnu2-wpi2)/wnu
      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2-aux**2+pu_sq))    ! piNN
      ff=((pc%al2**2-pc%aw**2)/(pc%al2**2+pu_sq))**pc%npow  ! rhoNN
      fff=fi*ff
      if (imodel==2) then
        fff=(lamrho**2-normrho**2)/(lamrho**2+p1_cm(1)**2+p1_cm(3)**2)
      endif
      !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)

      cc = fff * pc%gc
      do lam=-1,1
        do i=1,2
          hel(i,lam)=hel(i,lam)*cc
        end do
      end do

      return
  end subroutine dr_b

  !=======================================================================!

  subroutine dr_c(hel,pc,ecms)  ! pi-rho-N-N contact term in piN --> rho N
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2,-1:1)
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)),dimension(1:4,1:4,-1:1) :: grpnn   ! grpnn2
      complex(kind(0.d0)) :: shel(1:2,-1:1)   ! output
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: cc,p_t(0:3),pt_sq
      integer :: i,lam
      complex(kind(0.d0)) :: fi,ff   ! form factor
      complex(kind(0.d0)) :: s1,s2


      call vertex_rhopinn(grpnn,eps_mp_star)
      do lam=-1,1
        call mat_spinor(ux,grpnn(:,:,lam),u1(:,1))
        do i=1,2
          call spinor_bar_spinor(hel(i,lam),u3(:,i),ux)
        end do
      end do

      do i=1,3
        p_t( i ) = p1_cm ( i ) - p3_cm ( i )
      end do
      pt_sq = p_t(1)**2 + p_t(3)**2   ! cm

      ff=(pc%al1**2-wpi2)/(pc%al1**2+pcmi**2)*&
          &((pc%al1**2-wrho2)/(pc%al1**2+pcmf**2))**pc%npow
      if (pc%idiag==2) then
        ff=((pc%al1**2)/(pc%al1**2+pcmi**2)*&
            &(pc%al1**2)/(pc%al1**2+pcmf**2))**pc%npow
      else if (pc%idiag==3) then
        ff=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow
        fi=((pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq))**pc%npow
        ff=fi*ff
      end if

      if (imodel==2) then
        ff=(lamrho**2-normrho**2)/(lamrho**2+p1_cm(1)**2+p1_cm(3)**2)
      end if

      cc = ff * pc%gc
      do lam=-1,1
        do i=1,2
          hel(i,lam)=hel(i,lam)*cc
        end do
      end do

      return
  end subroutine dr_c

  !=======================================================================!

  subroutine dr_d(hel,pc,ecms)  ! pi-exchange in pi N --> rho N
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2,-1:1)
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: shel(2)
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: pt_sq
      complex(kind(0.d0)) :: e_t
      complex(kind(0.d0)),dimension(-1:1) :: g_ppr1,g_ppr2
      complex(kind(0.d0)),dimension(1:4,1:4) :: g_pv_t1,g_pv_t2,g_pv_ct
      complex(kind(0.d0)),dimension(1:4) :: ux,uy,uz
      complex(kind(0.d0)) :: cc,shel1,shel2,shelct
      integer :: i,lam
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
        p_t2 ( i ) = p1_cm ( i ) - p3_cm ( i )
      end do
      pt_sq = p_t1(1)**2 + p_t1(3)**2   ! cm

      e_t   = cdsqrt(pc%wf**2+pt_sq)

      p_t1(0)=e_t
      p_t2(0)=-e_t

      ! calculate the piNN vertexfunction
      call vertex_pv(g_pv_t1,-1.d0*p_t1)
      call vertex_pv(g_pv_t2,-1.d0*p_t2)
      ! outgoing but routine calc incoming
      call vertex_pvct(g_pv_ct)

      ! call pipirho vertex
      call vertex_pipirho(g_ppr1,p2_cm,-1.d0*p_t1,eps_mp_star)
      call vertex_pipirho(g_ppr2,p2_cm,-1.d0*p_t2,eps_mp_star)
      ! the intermediate pion goes in
      ! the routine calculates for outgoing

      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))  ! NNpi
      ff=((pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq))**pc%npow  ! rho_pi_pi

      if (imodel==2) then
        ff=(lamrho**2-normrho**2)/(lamrho**2+p1_cm(1)**2+p1_cm(3)**2)
        fi=1.d0
      end if
      cc = ff * fi * pc%gc

      call mat_spinor(ux,g_pv_t1,u1(:,1))
      call mat_spinor(uy,g_pv_t2,u1(:,1))
      call mat_spinor(uz,g_pv_ct,u1(:,1))

      do i=1,2
        call spinor_bar_spinor(shel1,u3(:,i),ux)
        call spinor_bar_spinor(shel2,u3(:,i),uy)
        call spinor_bar_spinor(shelct,u3(:,i),uz)
        do lam=-1,1
          if (pc%idiag==1) then
            hel(i,lam)=(shel1*g_ppr1(lam)/(ecms-e_t-p3_cm(0)-p2_cm(0))&
                &+shel2*g_ppr2(lam)/(ecms-e_t-p1_cm(0)-p4_cm(0)))*cc/(2.d0*e_t)&
                &+shelct*eps_mp_star(0,lam)*cc
          else
            hel(i,lam)=(shel1*g_ppr1(lam))/(e_t**2)*cc
          end if
        end do
      end do

      return
  end subroutine dr_d

  !=======================================================================!

  subroutine dr_e(hel,pc,ecms)  ! omega-exchange in piN -> rhoN
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2,-1:1)
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: pt_sq
      complex(kind(0.d0)) :: e_t
      complex(kind(0.d0)) :: g_pro (0:3,-1:1)
      complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: g_rnn1,g_rnn2
      complex(kind(0.d0)),dimension(1:4) :: ux,uy
      complex(kind(0.d0)),dimension(1:2,0:3) :: shel1,shel2
      complex(kind(0.d0)) :: cc
      integer :: i,lam,mu,nu
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
        p_t2 ( i ) = p1_cm ( i ) - p3_cm ( i )
      end do
      pt_sq = p_t1(1)**2 + p_t1(3)**2   ! cm

      e_t   = cdsqrt(pc%wf**2+pt_sq)

      p_t1(0)=e_t
      p_t2(0)=-e_t

      ! call omegapirho vertex           ! this is the rho mementum
      call vertex_pirhoomega(g_pro,p4_cm,p2_cm,eps_mp_star)
      !this is the incoming pion mementum

      ! omega NN vertex
      call vertex_rhoNN(g_rnn1,p_t1,wnu,pc%ka)
      call vertex_rhoNN(g_rnn2,p_t2,wnu,pc%ka)

      do mu=0,3
        call mat_spinor(ux,g_rnn1(:,:,mu),u1(:,1))
        call mat_spinor(uy,g_rnn2(:,:,mu),u1(:,1))
        do i=1,2
          call spinor_bar_spinor(shel1(i,mu),u3(:,i),ux)
          call spinor_bar_spinor(shel2(i,mu),u3(:,i),uy)
        end do
      end do

      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))   ! omegaNN
      ff=((pc%al2**2)/(pc%al2**2+pt_sq))**pc%npow   ! omega_rho_pi
      !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
      if (imodel==2) then
        ff=(lamrho**2-normrho**2)/(lamrho**2+p1_cm(1)**2+p1_cm(3)**2)
        fi=1.d0
      end if

      cc =  ff * fi * pc%gc

      do lam=-1,1
        do i=1,2
          hel(i,lam)=(0.d0,0.d0)
          ! only -g^mu^nu as propagator
          do mu=0,3
            hel(i,lam)=hel(i,lam)+(shel1(i,mu)*g_pro(mu,lam)/(ecms-e_t-p3_cm(0)-p2_cm(0))&
                &+shel2(i,mu)*g_pro(mu,lam)/(ecms-e_t-p4_cm(0)-p1_cm(0)))*cc/(2.d0*e_t)
          end do
        end do
      end do

      return
  end subroutine dr_e

  !=======================================================================!

  subroutine dr_f(hel,pc,ecms)  ! a1-exchange in pi N --> rho N
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2,-1:1)
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: pt_sq
      complex(kind(0.d0)) :: e_t
      complex(kind(0.d0)),dimension(0:3,-1:1) :: g_pra1,g_pra2,g_pract
      complex(kind(0.d0)) :: g_ann(1:4,1:4,0:3)
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: shel(1:2,0:3)
      complex(kind(0.d0)) :: prop1,prop2
      complex(kind(0.d0)) :: cc
      integer :: i,lam,mu,nu
      complex(kind(0.d0)) :: fi,ff  ! form factor

      do i=1,3
        p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
        p_t2 ( i ) = p1_cm ( i ) - p3_cm ( i )
      end do

      pt_sq = p_t1(1)**2 + p_t1(3)**2   ! cm
      e_t   = cdsqrt(pc%wf**2+pt_sq)

      p_t1(0)=e_t
      p_t2(0)=-e_t

      ! call pirhoa1 vertex 1. TO
      call vertex_pirhoa1(g_pra1,p2_cm,-1.d0*p_t1,p4_cm,eps_mp_star)
      ! the intermediate a1 goes in
      ! the routine calculates for outgoing
      ! call pirhoa1 vertex 2. TO
      call vertex_pirhoa1(g_pra2,p2_cm,-1.d0*p_t2,p4_cm,eps_mp_star)

      call vertex_a1NN(g_ann)

      ! call 1.Term of a1pirho coupling for the ct correction
      call vertex_pirhoa1(g_pract,p2_cm,0.d0*p_t2,p4_cm,eps_mp_star)

      do mu=0,3
        call mat_spinor(ux,g_ann(:,:,mu),u1(:,1))
        do i=1,2
          call spinor_bar_spinor(shel(i,mu),u3(:,i),ux)
        end do
      end do

      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))           ! N-N-a1
      ff=((pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq))**pc%npow  ! a1-rho-pi
      !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
      if (imodel==2) then
        ff=(lamrho**2-normrho**2)/(lamrho**2+p1_cm(1)**2+p1_cm(3)**2)
        fi=1.d0
      end if

      cc = ff * fi * pc%gc

      do lam=-1,1
        do i=1,2
          hel(i,lam)=(0.d0,0.d0)
          ! propagator with k^mu*k^nu term
          do mu=0,3
            do nu=0,3
              prop1=(-gmunu(mu,nu)+p_t1(mu)*p_t1(nu)/masses(7,2)**2)&
                  &/(2.d0*e_t)/(ecms-e_t-p3_cm(0)-p2_cm(0))
              prop2=(-gmunu(mu,nu)+p_t2(mu)*p_t2(nu)/masses(7,2)**2)&
                  &/(2.d0*e_t)/(ecms-e_t-p1_cm(0)-p4_cm(0))

              hel(i,lam)=hel(i,lam)+(shel(i,mu)*prop1*g_pra1(nu,lam)&
                  + shel(i,mu)*prop2*g_pra2(nu,lam))*cc
            end do
          end do
          ! add the correction term from the Legendre trafo from L->H (ct term of a1-t exchange)
          hel(i,lam)=hel(i,lam)+shel(i,0)*g_pract(0,lam)/masses(7,2)**2*cc
        end do
      end do

      return
  end subroutine dr_f

  !=======================================================================!

  subroutine dr_h(hel,pc,ecms)
      implicit none
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Delta exchange
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      complex(kind(0.d0)) :: hel(1:2,-1:1),hel2(1:2,-1:1)            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)), dimension(1:4,1:4,0:3,0:3) :: propp,propm,propa
      complex(kind(0.d0)),dimension(1:4,1:4,0:3,0:3) :: g_rnd1,g_rnd2
      complex(kind(0.d0)),dimension(1:4,1:4,0:3,-1:1) :: grnd
      complex(kind(0.d0)),dimension(0:3) :: gpnd
      complex(kind(0.d0)),dimension(1:4) :: ux
      complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: shelp1
      complex(kind(0.d0)),dimension(1:4,1:4,-1:1) :: shelp2
      complex(kind(0.d0)) :: pu_sq
      complex(kind(0.d0)) :: denom1,denom2,omp_u
      integer :: i,lam,mu,nu,is,js,ks,inhel
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff            ! form factor
      complex(kind(0.d0)),dimension(1:2,-1:1) :: help,heln
      complex(kind(0.d0)) :: omp4
      complex(kind(0.d0)) :: mdel
      !

      ! to get what kanzo did use this
      p_u(0)=p1_cm(0)-p4_cm(0)
      do i=1,3
        p_u(i) = p1_cm(i)-p4_cm(i)
      enddo

      omp_u=cdsqrt(p_u(1)**2+p_u(2)**2+p_u(3)**2+pc%wf**2)
      denom1=2.d0*omp_u*(ecms-omp_u-p2_cm(0)-p4_cm(0))
      denom2=2.d0*omp_u*(ecms-omp_u-p1_cm(0)-p3_cm(0))
      call prop_delta_pos(propp,p_u,pc%wf,.true.)
      call prop_delta_neg(propm,p_u,pc%wf,.true.)
      propa = pc%gc*(propp/denom1+propm/denom2)


      ! call rhoNDelta vertex
      call vertex_rhoNDeps(grnd,p4_cm,eps_mp_star)

      ! the piNDelta vertex
      gpnd=p2_cm*ci

      do lam=-1,1
        do is=1,4
          do js=1,4
            shelp2(is,js,lam)=(0.d0,0.d0)
            do ks=1,4
              do mu=0,3
                do nu=0,3
                  shelp2(is,js,lam)=shelp2(is,js,lam)+&
                      &gpnd(mu)*metric(mu)*propa(is,ks,mu,nu)*grnd(ks,js,nu,lam)
                end do
              end do
            end do
          end do
        end do
      end do

      do lam=-1,1
        call mat_spinor(ux,shelp2(:,:,lam),u1(:,1))
        do i=1,2
          call spinor_bar_spinor(hel(i,lam),u3(:,i),ux)
        enddo
      enddo

      !      if(imodel==1)then
      pu_sq=p_u(1)**2+p_u(2)**2+p_u(3)**2
      aux=(wnu2-wpi2)/wnu
      !       fi=(pc%al1**2-pc%aw**2)/(pc%al1**2-aux**2+pu_sq)
      fi=(pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq) 		! changed Nov. 16, 2011
      fi = fi**pc%npow
      !       aux=(wnu2-wrho2)/wnu
      ff=(pc%al2**2-pc%aw**2)/(pc%al2**2+pu_sq)
      ff = ff**pc%npow

      !      end if
      !      if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
      if (imodel==2) then
        ff=(lamrho**2-normrho**2)/(lamrho**2+p1_cm(1)**2+p1_cm(3)**2)
      endif
      !

      do lam=-1,1
        do i=1,2
          hel(i,lam)=hel(i,lam)*ff*fi
        enddo
      enddo
      return
  end subroutine dr_h

  !=======================================================================!
  !  rhoN -> rhoN potential
  !=======================================================================!

  subroutine v_rhoN_rhoN(v,pf_cm,pi_cm,ecm)

      implicit none
      complex(kind(0.d0)) :: v(36)        ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm  ! input
      complex(kind(0.d0)) ::     ecm      ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm        ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,-1:1,1:40)
      complex(kind(0.d0)) :: w(40,2,-1:1,-1:1)
      complex(kind(0.d0)) :: w_iso(40,2,-1:1,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,-1:1,2)
      integer :: i,ityp,ihel,ng,lam1,lam2,lad1,lad2,iso,isoindx

      ! masses
      wnu2  = masses(1,1)*masses(1,1)
      wpi2  = masses(1,2)*masses(1,2)
      wnu   = masses(1,1)
      wpi   = masses(1,2)
      wrho  = masses(3,2)
      wrho2 = masses(3,2)*masses(3,2)

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wrho2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wrho2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wnu2 -wrho2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wrho2 -wnu2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wrho2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wnu2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wrho2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wnu)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wnu)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      ! call eps for the incoming rho
      call epsvec_mp(eps_mp,pcmi,wrho,0.d0,1.d0)

      do ihel = 1,2
        do lam1=-1,1
          do lam2=-1,1
            do ityp = 1,itypmax(ccrel(2,2))
              w(ityp,ihel,lam1,lam2)    =(0.d0,0.d0)
              v_hel(ihel,lam1,lam2,ityp)=(0.d0,0.d0)
            end do
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wnu)

        ! call eps* for the outgoing rho
        call epsvec_mp_star(eps_mp_star,pcmf,wrho,sn_cm,cs_cm)

        if(coupl(2,2,1)%idiag == 1) then               ! N exchange
          call drr_b(v_hel(:,:,:,1),coupl(2,2,1),ecm)
        end if

        if(coupl(2,2,2)%idiag == 1) then               ! rho-rho-N-N contact term
          call drr_c(v_hel(:,:,:,2),coupl(2,2,2),ecm)
        end if

        if ((coupl(2,2,3)%idiag == 1).or.(coupl(2,2,3)%idiag == 2)) then
          call drr_d(v_hel(:,:,:,3),coupl(2,2,3),ecm)  ! rho exchange
        end if

        if(coupl(2,2,4)%idiag == 1) then               ! Delta exchange
          call drr_f(v_hel(:,:,:,4),coupl(2,2,4),ecm)
        end if

        ! do the cos theta integration:
        do lam1=-1,1
          do lam2=-1,1
            do ihel = 1,2
              lad2=3-2*ihel-2*lam2
              lad1=1-2*lam1
              if ((abs(lad1)<=j1).and.(abs(lad2)<=j1)) then
                do ityp = 1,itypmax(ccrel(2,2))
                  w(ityp,ihel,lam2,lam1)=w(ityp,ihel,lam2,lam1)+djw(ng,lad2,lad1)*v_hel(ihel,lam2,lam1,ityp)
                  ! this is 2*(la_N-la_rho)
                  ! the initial Npi state has |1/2,0>
                end do
              end if
            end do
          end do
        end do

      end do  ! of ng loop

      ! isospin factors
      do ihel = 1,2
        do lam1=-1,1
          do lam2=-1,1

            w_iso(1,ihel,lam2,lam1,1) = - w(1,ihel,lam2,lam1)             ! I = 1/2  nucleon u
            w_iso(1,ihel,lam2,lam1,2) =   w(1,ihel,lam2,lam1) * 2.d0      ! I = 3/2

            w_iso(2,ihel,lam2,lam1,1) = - w(2,ihel,lam2,lam1) *2.d0*ci    ! I = 1/2  contact
            w_iso(2,ihel,lam2,lam1,2) =   w(2,ihel,lam2,lam1) *ci         ! I = 3/2

            w_iso(3,ihel,lam2,lam1,1) =   w(3,ihel,lam2,lam1) * 2.d0*ci   ! I = 1/2  rho exchange
            w_iso(3,ihel,lam2,lam1,2) = - w(3,ihel,lam2,lam1) * ci        ! I = 3/2

            w_iso(4,ihel,lam2,lam1,1) =   w(4,ihel,lam2,lam1) *4.d0/3.d0  ! I = 1/2  Delta u
            w_iso(4,ihel,lam2,lam1,2) =   w(4,ihel,lam2,lam1) /3.d0       ! I = 3/2

            w_isosum(ihel,lam2,lam1,1)=(0.d0,0.d0)
            w_isosum(ihel,lam2,lam1,2)=(0.d0,0.d0)
            do ityp=1,itypmax(ccrel(2,2))
              w_isosum(ihel,lam2,lam1,1)=w_isosum(ihel,lam2,lam1,1)+w_iso(ityp,ihel,lam2,lam1,1)*cnorm
              w_isosum(ihel,lam2,lam1,2)=w_isosum(ihel,lam2,lam1,2)+w_iso(ityp,ihel,lam2,lam1,2)*cnorm
            end do
          end do
        end do
      end do

      ! transformation in JLS basis:
      do iso=1,2

        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*a(2,-1)**2*w_isosum(1,0,0,iso)-2.d0*a(2,-1)**2*w_iso&
            &sum(2,0,0,iso)+2.d0*a(2,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*a(2,-1)*a(&
            &1,-1)*w_isosum(1,1,0,iso)+2.d0*a(1,-1)*a(2,-1)*w_isosum(2,0,1,iso)-2.d0*&
            &a(1,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)**2*w_isosum(1,1,1,is&
            &o)-2.d0*a(1,-1)**2*w_isosum(2,-1,1,iso)

        v(2+isoindx) = 2.d0*a(2,1)**2*w_isosum(1,0,0,iso)+2.d0*a(2,1)**2*w_isosu&
            &m(2,0,0,iso)+2.d0*a(2,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)*a(1,1)*&
            &w_isosum(1,1,0,iso)+2.d0*a(1,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0*a(1,1)*a&
            &(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)**2*w_isosum(1,1,1,iso)+2.d0*a(1,1)&
            &**2*w_isosum(2,-1,1,iso)

        v(5+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(3&
            &,-1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*&
            &a(2,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*c(1,-1)*w_isosum(1,-&
            &1,0,iso)-2.d0*a(2,-1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*c(3,-1)*w&
            &_isosum(2,0,1,iso)-2.d0*a(1,-1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)&
            &*c(2,-1)*w_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso&
            &)+2.d0*a(1,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*c(1,-1)*w_isosum&
            &(1,-1,1,iso)

        v(6+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1)&
            &*w_isosum(2,0,0,iso)+2.d0*a(2,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)&
            &*c(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0&
            &*a(2,1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*c(3,1)*w_isosum(2,0,1,&
            &iso)+2.d0*a(1,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(&
            &1,1,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1)*c(1,1)*w_&
            &isosum(2,1,1,iso)+2.d0*a(1,1)*c(1,1)*w_isosum(1,-1,1,iso)

        v(9+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3,&
            &1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*a(2&
            &,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*b(1,1)*w_isosum(1,-1,0,i&
            &so)-2.d0*a(2,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*b(3,1)*w_isosum&
            &(2,0,1,iso)-2.d0*a(1,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*&
            &w_isosum(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,-1&
            &)*b(1,1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*b(1,1)*w_isosum(1,-1,1,iso)

        v(10+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*a(2,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*a(&
            &2,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*a(1,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*a(1,1)*b(1,-1)*w_isosum(1,-1,1,iso)

        v(13+isoindx) = 2.d0*c(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*c(2,-1)**2*w_is&
            &osum(1,1,1,iso)+2.d0*c(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*c(1,-1)*c(2,-&
            &1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*&
            &c(2,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*c(3,-1)*c(1,-1)*w_isosum(2,1&
            &,0,iso)+2.d0*c(3,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*c(1,-1)*c(2,-1)*w&
            &_isosum(1,1,-1,iso)-2.d0*c(2,-1)*c(1,-1)*w_isosum(1,-1,1,iso)-2.d0*c(2,-&
            &1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*c(1,-1)*w_isosum(1,-1,0,i&
            &so)-2.d0*c(3,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(1,-1)*c(3,-1)*w_isos&
            &um(1,0,-1,iso)-2.d0*c(1,-1)**2*w_isosum(2,1,-1,iso)-2.d0*c(3,-1)**2*w_is&
            &osum(2,0,0,iso)-2.d0*c(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,-1)*c(3,-1&
            &)*w_isosum(2,0,1,iso)

        v(14+isoindx) = 2.d0*c(3,1)**2*w_isosum(1,0,0,iso)+2.d0*c(3,1)**2*w_isos&
            &um(2,0,0,iso)+2.d0*c(3,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1)*c(2,1)&
            &*w_isosum(1,1,0,iso)+2.d0*c(3,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0*c(3,1)&
            &*c(1,1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*c(3,1)*w_isosum(2,0,1,iso)+2.d0*&
            &c(2,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)**2*w_isosum(1,1,1,iso)+&
            &2.d0*c(2,1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,1)*c(1,1)*w_isosum(2,1,1,is&
            &o)+2.d0*c(2,1)*c(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(1&
            &,0,-1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*c(2,1)*w_&
            &isosum(2,-1,-1,iso)+2.d0*c(1,1)*c(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*&
            &*2*w_isosum(1,-1,-1,iso)+2.d0*c(1,1)**2*w_isosum(2,1,-1,iso)

        v(17+isoindx) = 2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,iso)-2.d0*c(1,-1)*b&
            &(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,-1)*b(3,1)*w_isosum(1,0,-1,iso)-2.d0&
            &*c(2,-1)*b(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(2,-1)*b(2,1)*w_isosum(1,1&
            &,1,iso)-2.d0*c(2,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*b(1,1)*w_is&
            &osum(1,-1,0,iso)-2.d0*c(3,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(3,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*c(&
            &3,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,&
            &iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)+2.d0*c(1,-1)*b(2,1)*w_isos&
            &um(2,-1,-1,iso)-2.d0*c(1,-1)*b(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(2,-1)*b(&
            &1,1)*w_isosum(2,1,1,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(3,1)*w_isosum(2,0,1,iso)

        v(18+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*c(&
            &3,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*c(2,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(2,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*c(2,1)*b(1,-1)*w_isosum(1,-1,1,iso&
            &)+2.d0*c(1,1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*c(1,1)*b(3,-1)*w_isosum(&
            &2,0,-1,iso)+2.d0*c(1,1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*b(2,-1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)

        v(21+isoindx) = 2.d0*b(1,1)*b(2,1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*b(3&
            &,1)*w_isosum(2,0,-1,iso)+2.d0*b(2,1)*b(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2&
            &,1)*b(3,1)*w_isosum(2,0,1,iso)-2.d0*b(3,1)*b(1,1)*w_isosum(2,1,0,iso)&
            &+2.d0*b(3,1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*b(2,1)**2*w_isosum(2,-1,1,&
            &iso)-2.d0*b(1,1)**2*w_isosum(2,1,-1,iso)+2.d0*b(3,1)*b(1,1)*w_isosum(1,-&
            &1,0,iso)-2.d0*b(1,1)*b(2,1)*w_isosum(1,1,-1,iso)+2.d0*b(1,1)*b(3,1)*w_is&
            &osum(1,0,-1,iso)-2.d0*b(2,1)*b(1,1)*w_isosum(1,-1,1,iso)-2.d0*b(3,1)**2*&
            &w_isosum(2,0,0,iso)-2.d0*b(2,1)*b(3,1)*w_isosum(1,0,1,iso)-2.d0*b(3,1)*b&
            &(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,1)**2*w_isosum(1,0,0,iso)+2.d0*b(2,1)&
            &**2*w_isosum(1,1,1,iso)+2.d0*b(1,1)**2*w_isosum(1,-1,-1,iso)

        v(22+isoindx) = 2.d0*b(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*b(3,-1)**2*w_is&
            &osum(2,0,0,iso)+2.d0*b(3,-1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*b(3,-1)*b&
            &(2,-1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*b(1,-1)*w_isosum(1,-1,0,iso)+&
            &2.d0*b(3,-1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(2&
            &,0,1,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,-1)**2*w_iso&
            &sum(1,1,1,iso)+2.d0*b(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*b(2,-1)*b(1,-1)&
            &*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*b(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(1,&
            &-1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*b(3,-1)*w_isosum(2,0,-1&
            &,iso)+2.d0*b(1,-1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*b(2,-1)*w_&
            &isosum(1,1,-1,iso)+2.d0*b(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*b(1,-1)**2&
            &*w_isosum(2,1,-1,iso)

        v(25+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(&
            &3,-1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0&
            &*c(3,-1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*c(2,-1)*a(2,-1)*w_isosum(2,&
            &0,1,iso)-2.d0*c(2,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*c(2,-1)*w&
            &_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(1,-1&
            &)*a(2,-1)*w_isosum(1,0,-1,iso)-2.d0*c(1,-1)*a(2,-1)*w_isosum(2,0,-1,i&
            &so)+2.d0*c(1,-1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*a(1,-1)*w_is&
            &osum(1,1,-1,iso)

        v(26+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1&
            &)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1&
            &)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*c(2,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0&
            &*c(2,1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(1,1,1,&
            &iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum&
            &(1,0,-1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*a(1,1)*&
            &w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*a(1,1)*w_isosum(1,1,-1,iso)

        v(29+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3&
            &,1)*w_isosum(2,0,0,iso)+2.d0*b(3,1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*b(&
            &3,1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*b(2,1)*a(2,-1)*w_isosum(2,0,1,i&
            &so)-2.d0*b(2,1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*w_isosum&
            &(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*b(1,1)*a(2,-1)&
            &*w_isosum(1,0,-1,iso)-2.d0*b(1,1)*a(2,-1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*a(1,-1)*w_isosum(1,1,-1,iso)

        v(30+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*b(2,-1)*a(2,1)*w_isosum(2,0,1,i&
            &so)+2.d0*b(2,-1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum&
            &(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(1,-1)*a(2,1)&
            &*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &-1)*a(1,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*a(1,1)*w_isosum(1,1,-1,iso)

        v(33+isoindx) = -2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(2,1)*w_isosum(1,1,1,iso)+2.d0*b(1,1)*c(3,-1)*w_isosum(1,0,-1&
            &,iso)-2.d0*b(2,1)*c(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(2,1)*c(1,-1)*w_iso&
            &sum(2,1,1,iso)-2.d0*b(2,1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,1)*c(3,-&
            &1)*w_isosum(2,0,1,iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)-2.d0*b(3&
            &,1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,&
            &iso)+2.d0*b(3,1)*c(1,-1)*w_isosum(1,-1,0,iso)-2.d0*b(3,1)*c(2,-1)*w_isos&
            &um(1,1,0,iso)+2.d0*b(3,1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*b(1,1)*c(2,-&
            &1)*w_isosum(1,1,-1,iso)-2.d0*b(1,1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*b(&
            &1,1)*c(2,-1)*w_isosum(2,-1,-1,iso)

        v(34+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*c(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(1,-1,0,&
            &iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*c(3,1)*w_isosu&
            &m(2,0,1,iso)+2.d0*b(2,-1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(2,-&
            &1)*c(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*c(1,1)*w_isosum(1,-1,1,iso&
            &)+2.d0*b(1,-1)*c(3,1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*c(3,1)*w_isosum(&
            &2,0,-1,iso)+2.d0*b(1,-1)*c(2,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*c(2,1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)


      end do ! iso loop

  end subroutine v_rhoN_rhoN

  !=======================================================================!

  subroutine drr_b(hel,pc,ecms)  ! N-exchange in rho N --> rho N
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)) :: propa
      complex(kind(0.d0)),dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)),dimension(1:4,1:4,-1:1) :: g_rho_4,g_rho_2
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer :: i,lam1,lam2
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff   ! form factor

      p_u(0)=p1_cm(0)-p4_cm(0)    ! on-mass shell
      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      end do
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      e_u   = cdsqrt(pc%wf**2+pu_sq)

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.0 * e_u )

      call vertex_rhoNN2(g_rho_2,-1.d0*p2_cm,eps_mp,wnu,pc%ka)

      call vertex_rhoNN2(g_rho_4,p4_cm,eps_mp_star,wnu,pc%ka)

      do lam2=-1,1
        call mat_spinor(ux,g_rho_4(:,:,lam2),u1(:,1))
        call mat_spinor(uy,sf,ux)
        do lam1=-1,1
          call mat_spinor(ux,g_rho_2(:,:,lam1),uy)
          do i=1,2
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          end do
        end do
      enddo

      ! We are using the formfactors as Kanzo uses, which means: there is no energy dependence
      ! in due to the gauge invariance problem by coupling the photon
      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq))**pc%npow
      ff=fi*fi
      !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
      if (imodel==4) then
        ff=(lamrr**2-normrr**2)/(lamrr**2+p1_cm(1)**2+p1_cm(3)**2)
      end if

      cc = ff * pc%gc
      do lam1=-1,1
        do lam2=-1,1
          do i=1,2
            hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
          end do
        end do
      end do

      return
  end subroutine drr_b

  !=======================================================================!

  subroutine drr_c(hel,pc,ecms)  ! rho-rho-N-N contact term in rho N --> rho N
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: grrnn(1:4,1:4,-1:1,-1:1)
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer :: i,mu,nu,lam1,lam2
      complex(kind(0.d0)) :: fi,ff  ! form factor

      call vertex_rhorhonn(grrnn,eps_mp,eps_mp_star)
      do lam1=-1,1
        do lam2=-1,1
          call mat_spinor(ux,grrnn(:,:,lam1,lam2),u1(:,1))
          do i=1,2
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          end do
        end do
      end do

      ff=((pc%al1**2-wrho2)/(pc%al1**2+pcmi**2)*&
          &(pc%al1**2-wrho2)/(pc%al1**2+pcmf**2))**pc%npow
      !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
      if (imodel==4) then
        ff=(lamrr**2-normrr**2)/(lamrr**2+p1_cm(1)**2+p1_cm(3)**2)
      end if

      cc = ff * pc%gc
      do lam1=-1,1
        do lam2=-1,1
          do i=1,2
            hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
          end do
        end do
      end do

      return
  end subroutine drr_c

  !=======================================================================!

  subroutine drr_d(hel,pc,ecms)  ! rho-exchange in rho N --> rho N
      implicit none

      ! output
      complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_t1(0:3),p_t2(0:3),pdum(0:3)
      complex(kind(0.d0)) :: pt_sq,psq
      complex(kind(0.d0)) :: e_t
      complex(kind(0.d0)),dimension(-1:1,-1:1,0:3) :: g_rrr1,g_rrr2,g_rrrct
      complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: g_rnn1,g_rnn2,sigmunil
      complex(kind(0.d0)) :: ux(1:4),uy(1:4)
      complex(kind(0.d0)),dimension(1:4,1:4,-1:1,-1:1) :: shelp
      complex(kind(0.d0)), dimension(1:4,1:4) :: gam0
      complex(kind(0.d0)), dimension(1:2) :: scalct1
      complex(kind(0.d0)), dimension(0:3,1:2) :: scalct2
      complex(kind(0.d0)) :: prop1,prop2
      complex(kind(0.d0)) :: cc,sdum
      integer :: i,is,js,lam1,lam2,mu,nu
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
        p_t2 ( i ) = p1_cm ( i ) - p3_cm ( i )
      end do
      pt_sq = p_t1(1)**2 + p_t1(3)**2   ! cm
      e_t   = cdsqrt(pc%wf**2+pt_sq)

      p_t1(0)= e_t
      p_t2(0)= -e_t

      call vertex_rhorhorho(g_rrr1,p_t1,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)
      !we need the outgoing rho
      call vertex_rhorhorho(g_rrr2,p_t2,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)
      !we need the outgoing rho
      call vertex_rhoNN(g_rnn1,p_t1,wnu,pc%ka)

      call vertex_rhoNN(g_rnn2,p_t2,wnu,pc%ka)

      ! idiag=2 calculates without q^mu q^nu term in the rho propagator
      ! as done in the thesis. idiag=1 calculates with q^muq^nu term and additional TOPT
      ! contact term. The difference is small and the changes in the fit
      ! can be absorbed in the N^*(1650) bare mass

      if (pc%idiag==2) then

        do lam1=-1,1
          do lam2=-1,1
            do is=1,4
              do js=1,4
                shelp(is,js,lam1,lam2)=(0.d0,0.d0)
                do mu=0,3
                  ! without k^muk^nu term
                  shelp(is,js,lam1,lam2)=shelp(is,js,lam1,lam2)-&
                      &g_rnn1(is,js,mu)*g_rrr1(lam1,lam2,mu)&
                      &/(ecms-e_t-p3_cm(0)-p2_cm(0))-&
                      &g_rnn2(is,js,mu)*g_rrr2(lam1,lam2,mu)&
                      &/(ecms-e_t-p1_cm(0)-p4_cm(0))
                end do
              end do
            end do
            call mat_spinor(ux,shelp(:,:,lam1,lam2),u1(:,1))
            do i=1,2
              call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
            enddo
          end do
        end do

      else if (pc%idiag==1) then

        do lam1=-1,1
          do lam2=-1,1
            do is=1,4
              do js=1,4
                shelp(is,js,lam1,lam2)=(0.d0,0.d0)
                do mu=0,3
                  ! with k^muk^nu term
                  do nu=0,3
                    prop1=-gmunu(mu,nu)+p_t1(mu)*p_t1(nu)/masses(3,2)**2
                    prop2=-gmunu(mu,nu)+p_t2(mu)*p_t2(nu)/masses(3,2)**2
                    shelp(is,js,lam1,lam2)=shelp(is,js,lam1,lam2)+&
                        &g_rnn1(is,js,mu)*metric(mu)*prop1*g_rrr1(lam1,lam2,nu)&
                        !                          &*2.d0*e_t/((eps1-eps3)**2-e_t**2) !Fynm
                    &/(ecms-e_t-p3_cm(0)-p2_cm(0))+&
                        &g_rnn2(is,js,mu)*metric(mu)*prop2*g_rrr2(lam1,lam2,nu)&
                        &/(ecms-e_t-p1_cm(0)-p4_cm(0))
                  end do
                end do
              end do
            end do
            call mat_spinor(ux,shelp(:,:,lam1,lam2),u1(:,1))
            do i=1,2
              call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
            enddo
          end do
        end do

        ! get vertices for ct terms:
        call vertex_rhoNNct(gam0)

        pdum(0)=(1.d0,0.d0)
        call vertex_pind13A(sigmunil,pdum)

        pdum=(0.d0,0.d0)
        call vertex_rhorhorho(g_rrrct,pdum,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)

        call mat_spinor(ux,gam0,u1(:,1))
        do i=1,2
          call spinor_bar_spinor(scalct1(i),u3(:,i),ux)
        end do
        do mu=0,3
          call mat_spinor(ux,sigmunil(:,:,mu),u1(:,1))
          do i=1,2
            call spinor_bar_spinor(scalct2(mu,i),u3(:,i),ux)
          end do
        end do

      endif

      ff=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow
      fi=((pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq))**pc%npow
      ff=ff*fi
      !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
      if (imodel==4) then
        ff=(lamrr**2-normrr**2)/(lamrr**2+p1_cm(1)**2+p1_cm(3)**2)
      end if

      cc = ff * pc%gc / (2.d0*e_t)
      do lam1=-1,1
        do lam2=-1,1
          do i=1,2
            hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
          end do
        end do
      end do
      ! additional TOPT contact terms
      if (pc%idiag==1) then
        do lam1=-1,1
          do i=1,2
            sdum=(0.d0,0.d0)
            do mu=0,3
              sdum=sdum+scalct2(mu,i)*eps_mp_star(0,lam1)
            end do
            do lam2=-1,1
              hel(i,lam2,lam1)=hel(i,lam2,lam1)+&
                  &scalct1(i)*g_rrrct(lam1,lam2,0)/masses(3,2)**2*ff*pc%gc&
                  &+sdum*eps_mp_star(0,lam2)*ff*pc%gc*pc%ka/2.d0/masses(1,2)
            end do
          end do
        end do
      end if

      return
  end subroutine drr_d

  !=======================================================================!

  subroutine drr_f(hel,pc,ecms)
      implicit none
      ! Delta exchange
      complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)), dimension(1:4,1:4,0:3,0:3) :: propp,propm,propa
      complex(kind(0.d0)),dimension(1:4,1:4,0:3,0:3) :: g_rnd21,g_rnd22,g_rnd41,g_rnd42,g_rnd4,g_rnd2
      complex(kind(0.d0)),dimension(1:4,1:4,0:3,-1:1) :: grnd2,grnd4
      complex(kind(0.d0)),dimension(1:4) :: ux
      complex(kind(0.d0)),dimension(1:4,1:4,-1:1,-1:1) :: shelp2
      complex(kind(0.d0)) :: pu_sq
      complex(kind(0.d0)) :: denom1,denom2,omp_u
      complex(kind(0.d0)) :: aux
      integer :: i,lam1,lam2,mu,nu,is,js,ks,ls,lam
      complex(kind(0.d0)) :: fi,ff            ! form factor

      ! to get what kanzo did use this
      p_u(0)=p1_cm(0)-p4_cm(0)
      do i=1,3
        p_u(i) = p1_cm(i)-p4_cm(i)
      enddo

      omp_u=cdsqrt(p_u(1)**2+p_u(2)**2+p_u(3)**2+pc%wf**2)
      denom1=2.d0*omp_u*(ecms-omp_u-p2_cm(0)-p4_cm(0))
      denom2=2.d0*omp_u*(ecms-omp_u-p1_cm(0)-p3_cm(0))
      call prop_delta_pos(propp,p_u,pc%wf,.true.)
      call prop_delta_neg(propm,p_u,pc%wf,.true.)

      ! formfactor
      pu_sq=p_u(1)**2+p_u(2)**2+p_u(3)**2
      !      aux=(wnu2-wrho2)/wnu
      fi=(pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq)
      fi = fi**pc%npow
      ff=fi*fi
      if (imodel==4) then
        ff=(lamrr**2-normrr**2)/(lamrr**2+p1_cm(1)**2+p1_cm(3)**2)
      endif
      propa = ff*pc%gc*(propp/denom1+propm/denom2)
      ! each vertex has a pc%gc. this methode ensures, that the
      ! g2 coupling can be included without problems (see below)

      ! call rhoNDelta vertex
      ! no g2 coupling in the DeltarhoN vertex function

      call vertex_rhoNDelta1(g_rnd4,p4_cm)
      !      call vertex_rhoNDelta2(g_rnd42,p1_cm,p_u,p4_cm)

      !      g_rnd4=g_rnd41+pc%ka*g_rnd42

      ! no - sign because we have to include (-1)
      ! to get th h.c. part and another (-1) to
      ! get the incoming rho (instead of the outgoing)
      call vertex_rhoNDelta1(g_rnd2,p2_cm)
      !      call vertex_rhoNDelta2(g_rnd22,p_u,p3_cm,-1.d0*p2_cm)
      ! we need the incoming rho
      !      g_rnd2=g_rnd21+pc%ka*g_rnd22

      ! multiply rhoNDelta vertex with the polarisation vektor of the rho
      do lam=-1,1
        do mu=0,3
          do is=1,4
            do js=1,4
              grnd2(is,js,mu,lam)=(0.d0,0.d0)
              grnd4(is,js,mu,lam)=(0.d0,0.d0)
              do nu=0,3
                grnd2(is,js,mu,lam)=grnd2(is,js,mu,lam)+&
                    &g_rnd2(is,js,mu,nu)*eps_mp(nu,lam)
                grnd4(is,js,mu,lam)=grnd4(is,js,mu,lam)+&
                    &g_rnd4(is,js,mu,nu)*eps_mp_star(nu,lam)
              end do
            end do
          end do
        end do
      end do


      ! contract all lorentz indices
      do lam1=-1,1
        do lam2=-1,1
          do is=1,4
            do js=1,4
              shelp2(is,js,lam1,lam2)=(0.d0,0.d0)
              do ks=1,4
                do ls=1,4
                  do mu=0,3
                    do nu=0,3
                      shelp2(is,js,lam1,lam2)=shelp2(is,js,lam1,lam2)+&
                          &grnd4(is,ks,mu,lam1)*propa(ks,ls,mu,nu)*grnd2(ls,js,nu,lam2)
                    end do
                  end do
                end do
              end do
            end do
          end do
        end do
      end do

      do lam1=-1,1
        do lam2=-1,1
          call mat_spinor(ux,shelp2(:,:,lam1,lam2),u1(:,1))
          do i=1,2
            !            hel(i,lam)=(0.d0,0.d0)
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          enddo
        enddo
      enddo

      return
  end subroutine drr_f

  !=======================================================================!
  !  rhoN -> piDelta potential
  !=======================================================================!

  subroutine v_rhoN_piD(v,pf_cm,pi_cm,ecm)

      implicit none
      complex(kind(0.d0)) :: v(36)         ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm   ! input
      complex(kind(0.d0)) ::     ecm       ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm         ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(0:3,-1:1,1:8)
      complex(kind(0.d0)) :: w(8,0:3,-1:1)
      complex(kind(0.d0)) :: w_iso(8,0:3,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(0:3,-1:1,2)
      integer :: i,ityp,idel,ng,lam1,lad1,lad2,iso,isoindx

      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wrho = masses(3,2)
      wrho2 = masses(3,2)*masses(3,2)
      wdel = masses(2,1)
      wdel2 = wdel*wdel

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wrho2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wrho2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wdel2 -wpi2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wpi2 -wdel2 )/(2.d0*ecm)
      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wrho2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wdel2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wpi2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wdel)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wdel)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      ! call eps for the incoming rho
      call epsvec_mp(eps_mp,pcmi,wrho,0.d0,1.d0)

      do idel = 0,3
        do lam1=-1,1
          do ityp = 1,itypmax(ccrel(2,4))
            w(ityp,idel,lam1) = (0.d0, 0.d0)
            v_hel(idel,lam1,ityp)=(0.d0,0.d0)
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! Delta spinor
        call spinor_rsout_cm(urs3,pf_cm,p3_cm(0),cs_cm,sn_cm,cs_cm_h,sn_cm_h,wdel)

        if((coupl(2,4,1)%idiag == 1)) then           ! pi exchange
          call drd_a(v_hel(:,:,1),coupl(2,4,1),ecm)
        end if

        if((coupl(2,4,2)%idiag == 1)) then           ! N exchange
          call drd_b(v_hel(:,:,2),coupl(2,4,2),ecm)
        end if

        ! the v_hel's from the subroutine have the following notation:
        ! v_hel(idel,lam1,ityp)
        !       !    !    !
        !       !    !    rho in
        !       !    Delta out
        !       N out

        ! do the cos theta integration:
        do lam1=-1,1
          do idel = 0,3
            lad1=1-2*lam1 !ihel from incoming nucleon is always 1
            lad2=3-2*idel
            if ((abs(lad1)<=j1).and.(abs(lad2)<=j1)) then
              do ityp = 1,itypmax(ccrel(2,4))
                w(ityp,idel,lam1)=w(ityp,idel,lam1)+djw(ng,lad2,lad1)*v_hel(idel,lam1,ityp)
                ! this is 2*(la_N-la_rho)
                ! the initial Npi state has |1/2,0>
              end do
            end if
          end do
        end do

      end do  ! of ng loop

      !  isospin factors
      do idel = 0,3
        do lam1=-1,1

          w_iso(1,idel,lam1,1) = -w(1,idel,lam1)*ci*dsqrt(2.d0/3.d0)   ! I = 1/2  pi-t
          w_iso(1,idel,lam1,2) = -w(1,idel,lam1)*ci*dsqrt(5.d0/3.d0)   ! I = 3/2

          w_iso(2,idel,lam1,1) = -w(2,idel,lam1)*dsqrt(8.d0/3.d0)      ! I = 1/2  N-u
          w_iso(2,idel,lam1,2) =  w(2,idel,lam1)*dsqrt(5.d0/3.d0)      ! I = 3/2

          w_isosum(idel,lam1,1)=(0.d0,0.d0)
          w_isosum(idel,lam1,2)=(0.d0,0.d0)
          do ityp=1,itypmax(ccrel(2,4))
            w_isosum(idel,lam1,1)=w_isosum(idel,lam1,1)+w_iso(ityp,idel,lam1,1)*cnorm
            w_isosum(idel,lam1,2)=w_isosum(idel,lam1,2)+w_iso(ityp,idel,lam1,2)*cnorm
          end do
        end do
      end do

      ! transformation in JLS basis:
      do iso=1,2

        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*(a(2,-1)*e(1,-1)*w_isosum(1,0,iso)-a(2,-&
            &1)*e(1,-1)*w_isosum(2,0,iso)+a(2,-1)*e(2,-1)*w_isosum(0,0,iso&
            &)-a(2,-1)*e(2,-1)*w_isosum(3,0,iso)+a(1,-1)*e(1,-1)*w_isosum(&
            &2,1,iso)-a(1,-1)*e(1,-1)*w_isosum(1,1,iso)+a(1,-1)*e(2,-1)*w_&
            &isosum(3,1,iso)-a(1,-1)*e(2,-1)*w_isosum(0,1,iso))

        v(2+isoindx) = 2.d0*(a(2,1)*e(1,1)*w_isosum(1,0,iso)+a(2,1)*&
            &e(1,1)*w_isosum(2,0,iso)+a(2,1)*e(2,1)*w_isosum(0,0,iso)+a(2,&
            &1)*e(2,1)*w_isosum(3,0,iso)+a(1,1)*e(1,1)*w_isosum(2,1,iso)+a&
            &(1,1)*e(1,1)*w_isosum(1,1,iso)+a(1,1)*e(2,1)*w_isosum(3,1,iso&
            &)+a(1,1)*e(2,1)*w_isosum(0,1,iso))

        v(5+isoindx) = 2.d0*(c(3,-1)*e(1,-1)*w_isosum(1,0,iso)-c(3,-&
            &1)*e(1,-1)*w_isosum(2,0,iso)+c(3,-1)*e(2,-1)*w_isosum(0,0,iso&
            &)-c(3,-1)*e(2,-1)*w_isosum(3,0,iso)+c(2,-1)*e(1,-1)*w_isosum(&
            &2,1,iso)-c(2,-1)*e(1,-1)*w_isosum(1,1,iso)+c(2,-1)*e(2,-1)*w_&
            &isosum(3,1,iso)-c(2,-1)*e(2,-1)*w_isosum(0,1,iso)+c(1,-1)*e(1&
            &,-1)*w_isosum(1,-1,iso)-c(1,-1)*e(1,-1)*w_isosum(2,-1,iso)+c(&
            &1,-1)*e(2,-1)*w_isosum(0,-1,iso)-c(1,-1)*e(2,-1)*w_isosum(3,-&
            &1,iso))

        v(6+isoindx) = 2.d0*(c(3,1)*e(1,1)*w_isosum(1,0,iso)+c(3,1)*&
            &e(1,1)*w_isosum(2,0,iso)+c(3,1)*e(2,1)*w_isosum(0,0,iso)+c(3,&
            &1)*e(2,1)*w_isosum(3,0,iso)+c(2,1)*e(1,1)*w_isosum(2,1,iso)+c&
            &(2,1)*e(1,1)*w_isosum(1,1,iso)+c(2,1)*e(2,1)*w_isosum(3,1,iso&
            &)+c(2,1)*e(2,1)*w_isosum(0,1,iso)+c(1,1)*e(1,1)*w_isosum(1,-1&
            &,iso)+c(1,1)*e(1,1)*w_isosum(2,-1,iso)+c(1,1)*e(2,1)*w_isosum&
            &(0,-1,iso)+c(1,1)*e(2,1)*w_isosum(3,-1,iso))

        v(9+isoindx) = 2.d0*(b(3,1)*e(1,-1)*w_isosum(1,0,iso)-b(3,1)&
            &*e(1,-1)*w_isosum(2,0,iso)+b(3,1)*e(2,-1)*w_isosum(0,0,iso)-b&
            &(3,1)*e(2,-1)*w_isosum(3,0,iso)+b(2,1)*e(1,-1)*w_isosum(2,1,i&
            &so)-b(2,1)*e(1,-1)*w_isosum(1,1,iso)+b(2,1)*e(2,-1)*w_isosum(&
            &3,1,iso)-b(2,1)*e(2,-1)*w_isosum(0,1,iso)+b(1,1)*e(1,-1)*w_is&
            &osum(1,-1,iso)-b(1,1)*e(1,-1)*w_isosum(2,-1,iso)+b(1,1)*e(2,-&
            &1)*w_isosum(0,-1,iso)-b(1,1)*e(2,-1)*w_isosum(3,-1,iso))

        v(10+isoindx) = 2.d0*(b(3,-1)*e(1,1)*w_isosum(1,0,iso)+b(3,-&
            &1)*e(1,1)*w_isosum(2,0,iso)+b(3,-1)*e(2,1)*w_isosum(0,0,iso)+&
            &b(3,-1)*e(2,1)*w_isosum(3,0,iso)+b(2,-1)*e(1,1)*w_isosum(2,1,&
            &iso)+b(2,-1)*e(1,1)*w_isosum(1,1,iso)+b(2,-1)*e(2,1)*w_isosum&
            &(3,1,iso)+b(2,-1)*e(2,1)*w_isosum(0,1,iso)+b(1,-1)*e(1,1)*w_i&
            &sosum(1,-1,iso)+b(1,-1)*e(1,1)*w_isosum(2,-1,iso)+b(1,-1)*e(2&
            &,1)*w_isosum(0,-1,iso)+b(1,-1)*e(2,1)*w_isosum(3,-1,iso))

        v(13+isoindx) = 2.d0*(a(2,-1)*f(1,1)*w_isosum(1,0,iso)-a(2,-&
            &1)*f(1,1)*w_isosum(2,0,iso)+a(2,-1)*f(2,1)*w_isosum(0,0,iso)-&
            &a(2,-1)*f(2,1)*w_isosum(3,0,iso)+a(1,-1)*f(1,1)*w_isosum(2,1,&
            &iso)-a(1,-1)*f(1,1)*w_isosum(1,1,iso)+a(1,-1)*f(2,1)*w_isosum&
            &(3,1,iso)-a(1,-1)*f(2,1)*w_isosum(0,1,iso))

        v(14+isoindx) = 2.d0*(a(2,1)*f(1,-1)*w_isosum(1,0,iso)+a(2,1&
            &)*f(1,-1)*w_isosum(2,0,iso)+a(2,1)*f(2,-1)*w_isosum(0,0,iso)+&
            &a(2,1)*f(2,-1)*w_isosum(3,0,iso)+a(1,1)*f(1,-1)*w_isosum(2,1,&
            &iso)+a(1,1)*f(1,-1)*w_isosum(1,1,iso)+a(1,1)*f(2,-1)*w_isosum&
            &(3,1,iso)+a(1,1)*f(2,-1)*w_isosum(0,1,iso))

        v(17+isoindx) = 2.d0*(c(3,-1)*f(1,1)*w_isosum(1,0,iso)-c(3,-&
            &1)*f(1,1)*w_isosum(2,0,iso)+c(3,-1)*f(2,1)*w_isosum(0,0,iso)-&
            &c(3,-1)*f(2,1)*w_isosum(3,0,iso)+c(2,-1)*f(1,1)*w_isosum(2,1,&
            &iso)-c(2,-1)*f(1,1)*w_isosum(1,1,iso)+c(2,-1)*f(2,1)*w_isosum&
            &(3,1,iso)-c(2,-1)*f(2,1)*w_isosum(0,1,iso)+c(1,-1)*f(1,1)*w_i&
            &sosum(1,-1,iso)-c(1,-1)*f(1,1)*w_isosum(2,-1,iso)+c(1,-1)*f(2&
            &,1)*w_isosum(0,-1,iso)-c(1,-1)*f(2,1)*w_isosum(3,-1,iso))

        v(18+isoindx) = 2.d0*(c(3,1)*f(1,-1)*w_isosum(1,0,iso)+c(3,1&
            &)*f(1,-1)*w_isosum(2,0,iso)+c(3,1)*f(2,-1)*w_isosum(0,0,iso)+&
            &c(3,1)*f(2,-1)*w_isosum(3,0,iso)+c(2,1)*f(1,-1)*w_isosum(2,1,&
            &iso)+c(2,1)*f(1,-1)*w_isosum(1,1,iso)+c(2,1)*f(2,-1)*w_isosum&
            &(3,1,iso)+c(2,1)*f(2,-1)*w_isosum(0,1,iso)+c(1,1)*f(1,-1)*w_i&
            &sosum(1,-1,iso)+c(1,1)*f(1,-1)*w_isosum(2,-1,iso)+c(1,1)*f(2,&
            &-1)*w_isosum(0,-1,iso)+c(1,1)*f(2,-1)*w_isosum(3,-1,iso))

        v(21+isoindx) = 2.d0*(b(3,1)*f(1,1)*w_isosum(1,0,iso)-b(3,1)&
            &*f(1,1)*w_isosum(2,0,iso)+b(3,1)*f(2,1)*w_isosum(0,0,iso)-b(3&
            &,1)*f(2,1)*w_isosum(3,0,iso)+b(2,1)*f(1,1)*w_isosum(2,1,iso)-&
            &b(2,1)*f(1,1)*w_isosum(1,1,iso)+b(2,1)*f(2,1)*w_isosum(3,1,is&
            &o)-b(2,1)*f(2,1)*w_isosum(0,1,iso)+b(1,1)*f(1,1)*w_isosum(1,-&
            &1,iso)-b(1,1)*f(1,1)*w_isosum(2,-1,iso)+b(1,1)*f(2,1)*w_isosu&
            &m(0,-1,iso)-b(1,1)*f(2,1)*w_isosum(3,-1,iso))

        v(22+isoindx) = 2.d0*(b(3,-1)*f(1,-1)*w_isosum(1,0,iso)+b(3,&
            &-1)*f(1,-1)*w_isosum(2,0,iso)+b(3,-1)*f(2,-1)*w_isosum(0,0,is&
            &o)+b(3,-1)*f(2,-1)*w_isosum(3,0,iso)+b(2,-1)*f(1,-1)*w_isosum&
            &(2,1,iso)+b(2,-1)*f(1,-1)*w_isosum(1,1,iso)+b(2,-1)*f(2,-1)*w&
            &_isosum(3,1,iso)+b(2,-1)*f(2,-1)*w_isosum(0,1,iso)+b(1,-1)*f(&
            &1,-1)*w_isosum(1,-1,iso)+b(1,-1)*f(1,-1)*w_isosum(2,-1,iso)+b&
            &(1,-1)*f(2,-1)*w_isosum(0,-1,iso)+b(1,-1)*f(2,-1)*w_isosum(3,&
            &-1,iso))

      end do  ! iso loop

  end subroutine v_rhoN_piD

  !=======================================================================!

  subroutine drd_a(hel,pc,ecms)
      implicit none
      ! pi exchange
      complex(kind(0.d0)) :: hel(0:3,-1:1)            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_t1(0:3),p_t2(0:3)
      complex(kind(0.d0)) :: pt_sq,psq
      complex(kind(0.d0)) :: e_t
      complex(kind(0.d0)),dimension(1:4,0:3) :: uhelp1,uhelp2
      complex(kind(0.d0)),dimension(-1:1) :: epshelp1,epshelp2
      complex(kind(0.d0)),dimension(0:3) :: ndpi1,ndpi2,scalct
      complex(kind(0.d0)) :: prop1,prop2
      complex(kind(0.d0)) :: cc
      integer :: i,is,lam1,mu,idel
      complex(kind(0.d0)) :: fi,ff            ! form factor
      !
      do i=1,3
        p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
        p_t2 ( i ) = p1_cm ( i ) - p3_cm ( i )
      enddo
      pt_sq = p_t1(1)**2 + p_t1(3)**2     ! cm
      e_t   = cdsqrt(pc%wf**2+pt_sq)

      !      p_t1(0)=eps1-eps3    ! on-energy shell
      !      p_t2(0)=eps1-eps3    ! on-energy shell
      !      p_t1(0)=p1_cm(0)-p3_cm(0)    ! on-mass shell
      !      p_t2(0)=p1_cm(0)-p3_cm(0)    ! on-mass shell
      p_t1(0)= e_t
      p_t2(0)= -e_t


      ! construct NDpi vertex (ubar^mu u q_mu): contact lorentz index of the rs spinor:
      do idel=0,3
        do is=1,4
          uhelp1(is,idel)=(0.d0,0.d0)
          uhelp2(is,idel)=(0.d0,0.d0)
          do mu=0,3
            uhelp1(is,idel)=urs3(is,mu,idel)*p_t1(mu)*metric(mu)
            uhelp2(is,idel)=urs3(is,mu,idel)*p_t2(mu)*metric(mu)
          enddo
        enddo
        call spinor_bar_spinor(ndpi1(idel),uhelp1(:,idel),u1(:,1))
        call spinor_bar_spinor(ndpi2(idel),uhelp2(:,idel),u1(:,1))
        ! ct term
        call spinor_bar_spinor(scalct(idel),urs3(:,0,idel),u1(:,1))
      enddo

      ! construct the pipirho vertex by contracting the lorentz index: (p4-q)_nu*eps^nu
      do lam1=-1,1
        epshelp1(lam1)=(0.d0,0.d0)
        epshelp2(lam1)=(0.d0,0.d0)
        do mu=0,3
          epshelp1(lam1)=(p4_cm(mu)-p_t1(mu))*eps_mp(mu,lam1)*metric(mu)
          epshelp2(lam1)=(p4_cm(mu)-p_t2(mu))*eps_mp(mu,lam1)*metric(mu)
        enddo
      enddo

      ! pi propagator:
      prop1=1.d0/(2.d0*e_t)/(ecms-e_t-p3_cm(0)-p2_cm(0))
      prop2=1.d0/(2.d0*e_t)/(ecms-e_t-p1_cm(0)-p4_cm(0))


      ! formfactor

      ff=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow !piND
      fi=((pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq)) !pipirho
      ff=ff*fi

      cc = ff * pc%gc
      do lam1=-1,1
        do idel=0,3
          hel(idel,lam1)=(ndpi1(idel)*epshelp1(lam1)*prop1+&
              &ndpi2(idel)*epshelp2(lam1)*prop2-&
              ! ct term
          &scalct(idel)*eps_mp(0,lam1))*cc
        enddo
      enddo
      return
  end subroutine drd_a

  !=======================================================================!

  subroutine drd_b(hel,pc,ecms)
      implicit none
      ! N exchange
      complex(kind(0.d0)) :: hel(0:3,-1:1)            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)), dimension(1:4,1:4) :: gpv2,gpv4
      complex(kind(0.d0)), dimension(1:4,1:4,-1:1) :: gpve
      complex(kind(0.d0)),dimension(1:4,0:3) :: uhelp1
      complex(kind(0.d0)),dimension(1:4,0:3,-1:1) :: uhelp2
      complex(kind(0.d0)),dimension(1:4) :: ux,uy,ue
      complex(kind(0.d0)) :: cc,scal1,scal2
      integer :: i,is,lam,mu,idel
      complex(kind(0.d0)) :: fi,ff            ! form factor
      !
      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      enddo
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      e_u   = cdsqrt(pc%wf**2+pu_sq)

      !      p_u(0)=eps1-eps3    ! on-energy shell
      !      p_u(0)=p1_cm(0)-p3_cm(0)    ! on-mass shell

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) )&
          &/( 2.d0 * e_u )

      ! construct vertices
      call vertex_pv(gpv2,p2_cm)
      call vertex_pv(gpv4,p2_cm)
      do lam=-1,1
        call vertex_pv(gpve(:,:,lam),eps_mp(:,lam))
      enddo

      ! contract lorentz index of the rs spinor
      do idel=0,3
        do is=1,4
          uhelp1(is,idel)=(0.d0,0.d0)
          do mu=0,3
            uhelp1(is,idel)=urs3(is,mu,idel)*p2_cm(mu)*metric(mu)
          enddo
        enddo
      enddo

      do idel=0,3
        do lam=-1,1
          do is=1,4
            uhelp2(is,idel,lam)=(0.d0,0.d0)
            do mu=0,3
              uhelp2(is,idel,lam)=urs3(is,mu,idel)*eps_mp(mu,lam)*metric(mu)
            enddo
          enddo
        enddo
      enddo

      ! formfactor
      ff=((pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq)) !piNN
      fi=((pc%al2**2-pc%aw**2)/(pc%al2**2+pu_sq))**pc%npow !rhoND
      ff=ff*fi

      cc = ci * ff * pc%gc

      call mat_spinor(ux,gpv4,u1(:,1))
      call mat_spinor(uy,sf,ux)

      call mat_spinor(ux,gpv2,uy)

      do lam=-1,1
        call mat_spinor(ue,gpve(:,:,lam),uy)
        do idel=0,3
          call spinor_bar_spinor(scal1,uhelp1(:,idel),ue)
          call spinor_bar_spinor(scal2,uhelp2(:,idel,lam),ux)
          hel(idel,lam)=(scal1-scal2)*cc
        enddo
      enddo

      return
  end subroutine drd_b

  !=======================================================================!
  !  piN -> etaN potential
  !=======================================================================!

  subroutine v_piN_etaN(v,pf_cm,pi_cm,ecm)

      implicit none
      complex(kind(0.d0)) :: v(36)         ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm   ! input
      complex(kind(0.d0)) ::     ecm       ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm         ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:10)
      complex(kind(0.d0)) :: w(10,2)
      complex(kind(0.d0)) :: w_iso(10,2,2)
      complex(kind(0.d0)) :: w_lsj(10,4)
      integer :: i,ityp,ihel,ng

      ! masses
      wnu2  = masses(1,1)*masses(1,1)
      wpi2  = masses(1,2)*masses(1,2)
      wnu   = masses(1,1)
      wpi   = masses(1,2)
      weta2 = masses(5,2)*masses(5,2)
      weta  = masses(5,2)

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wpi2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wpi2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wnu2 -weta2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + weta2 -wnu2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wpi2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wnu2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + weta2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wnu)

      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wnu)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(1,3))
          w(ityp,ihel)    =(0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm

        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing nucleon
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wnu)

        if(coupl(1,3,1)%idiag == 1) then        ! nucleon exchange
          call de_b(v_hel(:,1),coupl(1,3,1),ecm)
        end if

        if(coupl(1,3,2)%idiag == 1) then        ! a0 exchange ! derivative coupling
          call de_c(v_hel(:,2),coupl(1,3,2),ecm)
        else if(coupl(1,3,2)%idiag == 2) then   ! a0 exchange ! scalar coupling
          call de_c1(v_hel(:,2),coupl(1,3,2),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(1,3))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do  ! ng-loop

      ! isospin factors
      do ihel = 1,2
        w_iso(1,ihel,1) =  w(1,ihel) * dsqrt(3.d0)  ! I = 1/2  nucleon u
        w_iso(2,ihel,1) =  w(2,ihel) * dsqrt(3.d0)  ! I = 1/2  a0 t
      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(1,3))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)     ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)     ! l=j+1/2; I=1/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
      end do
      do i = 1,2
        do ityp = 1,itypmax(ccrel(1,3))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_piN_etaN

  !=======================================================================!

  subroutine de_b(hel,pc,ecms)  ! N-exchange in pi N --> eta N
      implicit none

      ! output
      complex(kind(0.d0)), dimension(1:2) :: hel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)) :: g_pv_2( 1:4, 1:4 )
      complex(kind(0.d0)) :: g_pv_4( 1:4, 1:4 )
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer ::  i,ihel
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      end do
      pu_sq = p_u(1)**2 + p_u(3)**2   ! cm
      e_u   = cdsqrt((pc%wf-dcmplx(0.d0,0.d0))**2+pu_sq)

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.d0 * e_u )

      call vertex_pv(g_pv_2,p2_cm )
      call vertex_pv(g_pv_4,-1.d0*p4_cm )

      call mat_spinor(ux,g_pv_4,u1(:,1))
      call mat_spinor(uy,sf,ux)
      call mat_spinor(ux,g_pv_2,uy)
      do i=1,2
        call spinor_bar_spinor(hel(i),u3(:,i),ux)
      end do

      !      aux=(wnu2-wpi2)/wnu
      !      fi=(pc%al1**2-wnu2)/(pc%al1**2-aux**2+pu_sq)
      call monopole2(fi,pc%al1,pc%aw,pu_sq)   ! three vector squared
      call monopole2(ff,pc%al2,pc%aw,pu_sq)   ! three vector squared
      fi=ff*fi
      ff=fi**pc%npow
      if(imodel==3) then
        call ff_3(ff,p_u(0),p_u,eps1,p1_cm,eps4,p4_cm,2,2,5)
        call ff_3(fi,p_u(0),p_u,eps3,p3_cm,eps2,p2_cm,2,2,5)
        ff = fi*ff
      end if

      cc = ff * pc%gc
      do i=1,2
        hel(i)=hel(i)*cc
      end do

      return
  end subroutine de_b

  !=======================================================================!

  subroutine de_c1(hel,pc,ecms)  ! a0 exchange (scalar coupling) in piN -> etaN
      implicit none

      ! output
      complex(kind(0.d0)), dimension(1:2) :: hel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_t(0:3),p_tsq,p_tsq4,omega_t
      complex(kind(0.d0)) :: propa
      complex(kind(0.d0)) :: cc
      integer :: i,ihel
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_t(i)=p1_cm(i)-p3_cm(i)
      end do

      p_tsq=p_t(1)**2+p_t(2)**2+p_t(3)**2   ! 3 momentum squared

      omega_t=cdsqrt(p_tsq+pc%wf**2)

      propa  = ( 1.d0 / ( ecms - omega_t - p1_cm(0) -p4_cm(0) )&
          &+ 1.d0 / ( ecms - omega_t - p2_cm(0) -p3_cm(0) ) ) / ( 2.d0 * omega_t )

      do i=1,2
        call spinor_bar_spinor(hel(i),u3(:,i),u1(:,1))
      end do

      call monopole2(fi,pc%al1,pc%aw,p_tsq)   ! three vector squared
      call monopole2(ff,pc%al2,pc%aw,p_tsq)   ! three vector squared
      ff=fi*ff
      if(imodel==3) then
        call ff_3(ff,p_t(0),p_t,eps1,p1_cm,eps4,p4_cm,2,2,5)
        call ff_3(fi,p_t(0),p_t,eps3,p3_cm,eps2,p2_cm,2,2,5)
        ff = fi*ff
      end if

      cc = ff * propa * pc%gc
      do i=1,2
        hel(i)=hel(i)*cc
      end do

      return
  end subroutine de_c1

  !=======================================================================!

  subroutine de_c(hel,pc,ecms)
      implicit none
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! a0 exchange : derivative coupling
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      complex(kind(0.d0)) :: hel(1:2)               ! output
      ! input
      type(couplings) :: pc
      complex(kind(0.d0)) ::  ecms
      !  intermediate storage
      complex(kind(0.d0)) :: p_t(0:3)
      complex(kind(0.d0)) :: p_tsq,p_t4sq,omega_t
      complex(kind(0.d0)) :: prop
      complex(kind(0.d0)) :: gpps,gnns
      complex(kind(0.d0)) :: ff,fi
      complex(kind(0.d0)) :: scal
      integer :: ihel,i
      !
      !      p_t(0)= p1_cm(0) - p3_cm(0)
      p_t(0)=eps1-eps3
      do i=1,3
        p_t(i)=p1_cm(i)-p3_cm(i)
      enddo

      p_tsq=p_t(1)**2+p_t(2)**2+p_t(3)**2 ! 3 momentum squared
      p_t4sq=p_t(0)**2-p_tsq               ! 4 momentum squared

      omega_t=cdsqrt(p_tsq+pc%wf**2)

      prop  = ( 1.d0 / ( ecms - omega_t - p1_cm(0) -p4_cm(0) )&
          &+ 1.d0 / ( ecms - omega_t - p2_cm(0) -p3_cm(0) ) )&
          &/ ( 2.d0 * omega_t )


      ! call sigmapipi Vertexfunction
      call vertex_pipisig(gpps,p2_cm,p4_cm)
      gpps = -gpps/2.d0  !(it's just p2p4)

      ! the a0NN vertex is trivial
      gNNs = 1.d0

      ! the formfactors:
      !       if (imodel==1) then
      ff=(( pc%al1**2 - pc%aw**2 )/( pc%al1**2 + p_tsq ))**pc%npow
      !       endif
      if (imodel==3) then
        call ff_3(ff,eps1-eps3,p_t,eps2,p2_cm,eps4,p4_cm,6,5,5)
        call ff_3(fi,eps1-eps3,p_t,eps1,p1_cm,eps3,p3_cm,6,2,2)
        ff = ff*fi
      end if

      !      fi=(pc%al2**2-2.d0*masses(1,2)**2)/(pc%al2**2+p_tsq)
      ! this is for the cheng dashen contact graph
      fi=((pc%al2**2-pc%aw**2)/(pc%al2**2+p_tsq))**pc%npow
      ff=ff*fi

      do ihel = 1,2
        call spinor_bar_spinor(scal,u3(:,ihel),u1(:,1))
        hel(ihel)=scal*(gpps*prop*gNNs*pc%gc*ff)

      enddo
      return
  end subroutine de_c

  !=======================================================================!
  !  etaN -> etaN potential
  !=======================================================================!

  subroutine v_etaN_etaN(v,pf_cm,pi_cm,ecm)

      implicit none
      complex(kind(0.d0)) :: v(36)         ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm   ! input
      complex(kind(0.d0)) ::     ecm       ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm         ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng
      integer :: itypmx

      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      weta2 = masses(5,2)*masses(5,2)
      weta = masses(5,2)

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -weta2 )/(2.0*ecm)
      eps2 = ( ecm**2 + weta2 -wnu2 )/(2.0*ecm)
      eps3 =  eps1
      eps4 =  eps2

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + weta2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wnu2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + weta2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wnu)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wnu)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      itypmx=itypmax(ccrel(3,3))
      do ihel = 1,2
        do ityp = 1,itypmx
          w(ityp,ihel)    =(0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing nucleon
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wnu)

        if(coupl(3,3,1)%idiag == 1) then   ! nucleon exchange
          call dee_b(v_hel(:,1),coupl(3,3,1),ecm)
        end if

        if(coupl(3,3,2)%idiag == 1) then   ! f0 exchange
          call dee_c(v_hel(:,2),coupl(3,3,2),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmx
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do   ! ng-loop

      ! isospin factors
      do ihel = 1,2
        w_iso(1,ihel,1) =   w(1,ihel)   ! I = 1/2  nucleon u
        w_iso(2,ihel,1) =   w(2,ihel)   ! I = 1/2  a0 t
      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(3,3))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)  ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)  ! l=j+1/2; I=1/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
      end do
      do i = 1,2
        do ityp = 1,itypmax(ccrel(3,3))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_etaN_etaN

  !=======================================================================!

  subroutine dee_b(hel,pc,ecms)
      implicit none
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! nucleon exchange
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      complex(kind(0.d0)), dimension(1:2) :: hel            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      !      complex(kind(0.d0)) :: propa
      complex(kind(0.d0)), dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)) :: g_pv_2( 1:4, 1:4 )
      complex(kind(0.d0)) :: g_pv_4( 1:4, 1:4 )
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer ::  i,ihel
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff            ! form factor
      !
      p_u(0)=eps1-eps4    ! on-energy shell
      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      enddo
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      psq   = p_u(0)**2 - pu_sq         ! four vector squared
      e_u   = cdsqrt(pc%wf**2+pu_sq)

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      ! only pos energy part
      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) )&
          &/( 2.d0 * e_u )

      call vertex_pv(g_pv_2,p2_cm )
      call vertex_pv(g_pv_4,-1.d0*p4_cm )
      !
      call mat_spinor(ux,g_pv_4,u1(:,1))
      call mat_spinor(uy,sf,ux)
      call mat_spinor(ux,g_pv_2,uy)
      do i=1,2
        call spinor_bar_spinor(hel(i),u3(:,i),ux)
      enddo
      !
      !      if(imodel==1)then
      call monopole2(ff,pc%al1,pc%aw,pu_sq) ! three vector squared
      call monopole2(fi,pc%al1,pc%aw,pu_sq) ! three vector squared
      fi=ff*fi
      ff=fi**pc%npow
      !      end if
      if(imodel==3) then
        call ff_3(ff,p_u(0),p_u,eps1,p1_cm,eps4,p4_cm,2,2,5)
        call ff_3(fi,p_u(0),p_u,eps3,p3_cm,eps2,p2_cm,2,2,5)
        ff = fi*ff
      endif

      cc = ff * pc%gc
      do i=1,2
        hel(i)=hel(i)*cc
      enddo
      return
  end subroutine dee_b

  !=======================================================================!

  subroutine dee_c(hel,pc,ecms)
      implicit none
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! f0 exchange
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      complex(kind(0.d0)), dimension(1:2) :: hel            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_t(0:3),p_tsq,p_tsq4,omega_t
      complex(kind(0.d0)) :: propa
      complex(kind(0.d0)) :: cc
      integer ::  i,ihel
      complex(kind(0.d0)) :: fi,ff            ! form factor
      !
      !
      !      p_t(0)= p1_cm(0) - p3_cm(0)
      p_t(0)=eps1-eps3
      do i=1,3
        p_t(i)=p1_cm(i)-p3_cm(i)
      enddo

      p_tsq=p_t(1)**2+p_t(2)**2+p_t(3)**2 ! 3 momentum squared
      p_tsq4=p_t(0)**2-p_tsq               ! 4 momentum squared

      omega_t=cdsqrt(p_tsq+pc%wf**2)

      propa  = ( 1.d0 / ( ecms - omega_t - p1_cm(0) -p4_cm(0) )&
          &+ 1.d0 / ( ecms - omega_t - p2_cm(0) -p3_cm(0) ) )&
          &/ ( 2.d0 * omega_t )

      do i=1,2
        call spinor_bar_spinor(hel(i),u3(:,i),u1(:,1))
      enddo
      !
      !      if(imodel==1)then
      call monopole2(fi,pc%al1,pc%aw,p_tsq)   ! three vector squared
      call monopole2(ff,pc%al1,pc%aw,p_tsq)   ! three vector squared
      ff=fi*ff
      !      end if
      if(imodel==3) then
        call ff_3(ff,p_t(0),p_t,eps1,p1_cm,eps4,p4_cm,2,2,5)
        call ff_3(fi,p_t(0),p_t,eps3,p3_cm,eps2,p2_cm,2,2,5)
        ff = fi*ff
      endif

      cc = ff * propa * pc%gc
      do i=1,2
        hel(i)=hel(i)*cc
      enddo
      return
  end subroutine dee_c

  !=======================================================================!
  !  piN -> piDelta potential
  !=======================================================================!

  subroutine v_piN_piD(v,pf_cm,pi_cm,ecm)

      implicit none
      complex(kind(0.d0)) :: v(36)        ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm  ! input
      complex(kind(0.d0)) ::     ecm      ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm        ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(0:3)
      complex(kind(0.d0)) :: v_hel(0:3,1:12)
      complex(kind(0.d0)) :: w(12,0:3)
      complex(kind(0.d0)) :: w_iso(12,0:3,2)
      complex(kind(0.d0)) :: w_isosum(0:3,2)

      integer :: i,ityp,ihel,ng,idel,ldel,iso,isoindx,ldel2,onshe

      if (imag(pf_cm).gt.0.d0) then		! (offresi)
        onshe=1
      else
        onshe=0
      end if

      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wrho = masses(3,2)
      wrho2 = masses(3,2)*masses(3,2)
      wdel = masses(2,1)
      wdel2 = wdel*wdel

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wpi2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wpi2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wdel2 -wpi2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wpi2 -wdel2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wpi2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wdel2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wpi2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wdel)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wdel)/p3_cm(0)&
          &/ (p4_cm(0) *    p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do idel= 0,3
        do ityp = 1,itypmax(ccrel(1,4))
          w(ityp,idel) = (0.d0, 0.d0)
          v_hel(idel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        !   momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        call spinor_rsout_cm(urs3,pf_cm,p3_cm(0),cs_cm,sn_cm,cs_cm_h,sn_cm_h,wdel)

        if(coupl(1,4,1)%idiag == 1) then           ! N-u
          call dpd_b(v_hel(:,1),coupl(1,4,1),ecm,onshe)
        end if

        if(coupl(1,4,2)%idiag == 1) then           ! rho-t
          call dpd_c(v_hel(:,2),coupl(1,4,2),ecm)
        end if

        if(coupl(1,4,3)%idiag == 1) then           ! Delta-u
          call dpd_d(v_hel(:,3),coupl(1,4,3),ecm)
        end if

        ! do the cos theta integration:
        do idel = 0,3
          ldel=3-2*idel
          ! ldel=3-2*idel leads to the following realtion between physival helicity and the one used here:
          ! la_phys   ldel  idel
          ! 3/2       3     0
          ! 1/2       1     1
          ! -1/2     -1     2
          ! 3/2      -3     3
          if (abs(ldel)<=j1) then
            do ityp = 1,itypmax(ccrel(1,4))
              w(ityp,idel)=w(ityp,idel)+djw(ng,ldel,1)*v_hel(idel,ityp)
              ! the initial Npi state has |1/2,0>
            end do
          end if
        end do  ! idel

      end do  ! of ng loop

      ! isospin factors
      do idel = 0,3

        w_iso(1,idel,1) = - w(1,idel)*dsqrt(8.d0/3.d0)            ! I = 1/2  nucleon u
        w_iso(1,idel,2) =   w(1,idel)*dsqrt(5.d0/3.d0)            ! I = 3/2

        w_iso(2,idel,1) =   w(2,idel)*dsqrt(2.d0/3.d0)*ci         ! I = 1/2  rho-t
        w_iso(2,idel,2) =   w(2,idel)*dsqrt(5.d0/3.d0)*ci         ! I = 3/2

        w_iso(3,idel,1) = - w(3,idel)*5.d0/3.d0*dsqrt(2.d0/3.d0)  ! I = 1/2  Delta-u
        w_iso(3,idel,2) = - w(3,idel)*10.d0/3.d0/dsqrt(15.d0)     ! I = 3/2

        w_isosum(idel,1)=(0.d0,0.d0)
        w_isosum(idel,2)=(0.d0,0.d0)
        do ityp=1,itypmax(ccrel(1,4))
          w_isosum(idel,1)=w_isosum(idel,1)+w_iso(ityp,idel,1)*cnorm
          w_isosum(idel,2)=w_isosum(idel,2)+w_iso(ityp,idel,2)*cnorm
        end do

      end do

      ! transformation in JLS basis:
      do iso=1,2
        isoindx=2*iso-2
        v(1+isoindx) = 2.d0*(e(1,-1)*w_isosum(1,iso)-e(1,-1)*w_isosum(2,iso)+&
            &e(2,-1)*w_isosum(0,iso)-e(2,-1)*w_isosum(3,iso))*d(1,-1)
        v(2+isoindx) = 2.d0*(e(1,1)*w_isosum(1,iso)+e(1,1)*w_isosum(2,iso)+e(&
            &2,1)*w_isosum(0,iso)+e(2,1)*w_isosum(3,iso))*d(1,1)
        v(5+isoindx) = 2.d0*(f(1,1)*w_isosum(1,iso)-f(1,1)*w_isosum(2,iso)+f(&
            &2,1)*w_isosum(0,iso)-f(2,1)*w_isosum(3,iso))*d(1,-1)
        v(6+isoindx) = 2.d0*(f(1,-1)*w_isosum(1,iso)+f(1,-1)*w_isosum(2,iso)+&
            &f(2,-1)*w_isosum(0,iso)+f(2,-1)*w_isosum(3,iso))*d(1,1)
      enddo  ! iso loop

  end subroutine v_piN_piD

  !=======================================================================!

  subroutine dpd_b(hel,pc,ecms,onshe)  ! N-exchange in pi N --> pi Delta
      implicit none

      ! output
      complex(kind(0.d0)), dimension(0:3) :: hel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)) :: g_pnd_2(0:3)
      complex(kind(0.d0)) :: g_pv_4( 1:4, 1:4 )
      complex(kind(0.d0)) :: uh(1:4,0:3)
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer :: i,idel,is,mu,onshe
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      end do
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      e_u   = cdsqrt(pc%wf**2+pu_sq)				! This is now modified:

      if (onshe==1.and.manip_v==1) then				! (offresi)
        e_u   = cdsqrt(dcmplx(pc%wf,-60.d0)**2+pu_sq)
      else
        e_u   = cdsqrt(dcmplx(pc%wf,0.d0)**2+pu_sq)
      end if

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.d0 * e_u )

      ! piNN vertex
      call vertex_pv(g_pv_4,p4_cm )
      ! piNDelta vertex
      g_pnd_2 = -ci*p2_cm

      ! contract lorentz index:
      do is=1,4
        do idel=0,3
          uh(is,idel)=(0.d0,0.d0)
          do mu=0,3
            uh(is,idel)=uh(is,idel)+urs3(is,mu,idel)*metric(mu)*g_pnd_2(mu)
          end do
        end do
      end do

      call mat_spinor(ux,g_pv_4,u1(:,1))
      call mat_spinor(uy,sf,ux)
      do idel=0,3
        call spinor_bar_spinor(hel(idel),uh(:,idel),uy)
      end do

      aux=(wnu2-wpi2)/wnu
      fi=(pc%al1**2-wnu2)/(pc%al1**2-aux**2+pu_sq)       ! piNN
      ff=((pc%al2**2-wnu2)/(pc%al2**2+pu_sq))**pc%npow   ! piND npow=2
      ff=ff*fi
      !     if(imodel==3) then
      !       call ff_3(ff,p_u(0),p_u,eps1,p1_cm,eps4,p4_cm,2,2,5)
      !       call ff_3(fi,p_u(0),p_u,eps3,p3_cm,eps2,p2_cm,2,2,5)
      !       ff = fi*ff
      !     end if

      cc = ff * pc%gc
      do idel=0,3
        hel(idel)=hel(idel)*cc
      end do

      return
  end subroutine dpd_b

  !=======================================================================!

  subroutine dpd_c(hel,pc,ecms)  ! rho-exchange in pi N --> pi Delta
      implicit none

      ! output
      complex(kind(0.d0)), dimension(0:3) :: hel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)),dimension(0:3) :: p_u1,p_u2
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)),dimension(1:4,1:4,0:3,0:3) :: g_rnd_1,g_rnd_2
      complex(kind(0.d0)) :: g_ppr(0:3)
      complex(kind(0.d0)),dimension(0:3,0:3) :: prop1,prop2
      complex(kind(0.d0)),dimension(0:3) :: shelp1,shelp2
      complex(kind(0.d0)), dimension(1:4,1:4,0:3) :: saux1,saux2
      complex(kind(0.d0)),dimension(1:4,0:3) :: uh1,uh2
      complex(kind(0.d0)) :: cc,scal1,scal2
      integer :: i,idel,is,js,mu,nu
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_u1 ( i ) = p1_cm ( i ) - p3_cm ( i )
        p_u2 ( i ) = p_u1(i)
      end do
      pu_sq = p_u1(1)**2 + p_u1(3)**2     ! cm
      e_u   = cdsqrt(pc%wf**2+pu_sq)

      !Schuetz
      p_u1(0)=e_u
      call vertex_rhoNDelta1(g_rnd_1,p_u1)

      !TOPT
      ! piND vertex
      !     p_u1(0)=e_u
      !     call vertex_rhoNDelta1(g_rnd_1,p_u1)
      !     p_u2(0)=-e_u
      !     call vertex_rhoNDelta1(g_rnd_2,p_u2)

      ! pipirho vertex
      g_ppr = ci*(p2_cm+p4_cm)


      ! propagator:
      do mu=0,3
        do nu=0,3
          !SCHUETZ
          prop1(mu,nu)=1.d0/(2.d0*e_u)*(-gmunu(mu,nu))*&
              &(1.d0/(ecms-e_u-p3_cm(0)-p2_cm(0))+1.d0/(ecms-e_u-p1_cm(0)-p4_cm(0)))
          !TOPT
          !            prop1(mu,nu)=1.d0/(2.d0*e_u)*&
          !                 &(-gmunu(mu,nu)+p_u1(mu)*p_u1(nu)/wrho2)/(ecms-e_u-p3_cm(0)-p2_cm(0))
          !            prop2(mu,nu)=1.d0/(2.d0*e_u)*&
          !                 &(-gmunu(mu,nu)+p_u2(mu)*p_u2(nu)/wrho2)/(ecms-e_u-p1_cm(0)-p4_cm(0))
        end do
      end do

      ! contract lorentz index:
      do mu=0,3
        shelp1(mu)=(0.d0,0.d0)
        !TOPT
        !         shelp2(mu)=(0.d0,0.d0)
        do nu=0,3
          shelp1(mu)=shelp1(mu)+prop1(mu,nu)*g_ppr(nu)*metric(nu)
          !TOPT
          !            shelp2(mu)=shelp2(mu)+prop2(mu,nu)*g_ppr(nu)*metric(nu)
        end do
      end do

      do mu=0,3
        do is=1,4
          do js=1,4
            saux1(is,js,mu)=(0.d0,0.d0)
            !TOPT
            !               saux2(is,js,mu)=(0.d0,0.d0)
            do nu=0,3
              saux1(is,js,mu)=saux1(is,js,mu)+g_rnd_1(is,js,mu,nu)*shelp1(nu)
              !TOPT
              !                  saux2(is,js,mu)=saux2(is,js,mu)+g_rnd_2(is,js,mu,nu)*shelp2(nu)
            end do
          end do
        end do
      end do

      do mu=0,3
        call mat_spinor(uh1(:,mu),saux1(:,:,mu),u1(:,1))
        !TOPT
        !         call mat_spinor(uh2(:,mu),saux2(:,:,mu),u1(:,1))
      end do

      do idel=0,3
        hel(idel)=(0.d0,0.d0)
        do mu=0,3
          call spinor_bar_spinor(scal1,urs3(:,mu,idel),uh1(:,mu))
          scal2=(0.d0,0.d0)
          !TOPT
          !         call spinor_bar_spinor(scal2,urs3(:,mu,idel),uh2(:,mu))
          hel(idel)=hel(idel)+scal1+scal2
        end do
      end do

      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq))**pc%npow !rhoND npow = 2
      ff=(pc%al2**2-pc%aw**2)/(pc%al2**2+pu_sq) !pipirho
      ff=ff*fi
      !     if(imodel==3) then
      !       call ff_3(ff,p_u(0),p_u,eps1,p1_cm,eps4,p4_cm,2,2,5)
      !       call ff_3(fi,p_u(0),p_u,eps3,p3_cm,eps2,p2_cm,2,2,5)
      !       ff = fi*ff
      !     end if

      cc = ff * pc%gc
      do idel=0,3
        hel(idel)=hel(idel)*cc
      end do

      return
  end subroutine dpd_c

  !=======================================================================!

  subroutine dpd_d(hel,pc,ecms)  ! Delta-exchange in pi N --> pi Delta
      implicit none

      ! output
      complex(kind(0.d0)), dimension(0:3) :: hel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4,0:3,0:3) :: sf,sfpos,sfneg
      complex(kind(0.d0)) :: g_pnd_4(0:3)
      complex(kind(0.d0)) :: g_pv_2( 1:4, 1:4 )
      complex(kind(0.d0)) :: uh(1:4,0:3)
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: shelp
      complex(kind(0.d0)) :: cc,scal
      integer :: i,idel,is,js,mu,nu
      complex(kind(0.d0)) :: fi,ff   ! form factor

      p_u(0)=eps1-eps4    ! on-energy shell
      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      end do
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      e_u   = cdsqrt(pc%wf**2+pu_sq)

      ! Schuetz:
      call prop_delta_rs(sfpos,p_u,pc%wf)
      sf=sfpos*(1.d0/(ecms-e_u-p2_cm(0)-p4_cm(0))+1.d0/(ecms-e_u-p1_cm(0)-p3_cm(0)))/(2.d0*e_u)

      ! TOPT:
      !     p_u(0)=e_u
      !     call prop_delta_pos(sfpos,p_u,pc%wf,.true.)
      !     p_u(0)=-e_u
      !     call prop_delta_neg(sfneg,p_u,pc%wf,.true.)
      !
      !     sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
      !          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) )&
      !             &/( 2.d0 * e_u )

      ! piDD vertex
      call vertex_pv(g_pv_2,p2_cm )

      ! piNDelta vertex
      g_pnd_4 = ci*p4_cm  ! there is a relative sign between piNN and piDD coupling

      ! contract lorentz index:
      do mu=0,3
        do is=1,4
          do js=1,4
            shelp(is,js,mu)=(0.d0,0.d0)
            do nu=0,3
              shelp(is,js,mu)=shelp(is,js,mu)+sf(is,js,mu,nu)*metric(nu)*g_pnd_4(nu)
            end do
          end do
        end do
      end do

      do mu=0,3
        call mat_spinor(ux,shelp(:,:,mu),u1(:,1))
        call mat_spinor(uh(:,mu),g_pv_2,ux)
      end do

      do idel=0,3
        hel(idel)=(0.d0,0.d0)
        do mu=0,3
          call spinor_bar_spinor(scal,urs3(:,mu,idel),uh(:,mu))
          hel(idel)=hel(idel)+scal*metric(mu)
        end do
      end do

      fi=(pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq)  ! piND
      ff=(pc%al2**2-pc%aw**2)/(pc%al2**2+pu_sq)  ! piDD
      fi=ff*fi
      ff=fi**pc%npow   ! npow = 2
      !     if(imodel==3) then
      !       call ff_3(ff,p_u(0),p_u,eps1,p1_cm,eps4,p4_cm,2,2,5)
      !       call ff_3(fi,p_u(0),p_u,eps3,p3_cm,eps2,p2_cm,2,2,5)
      !       ff = fi*ff
      !     endif

      cc = ff * pc%gc
      do idel=0,3
        hel(idel)=hel(idel)*cc
      end do

      return
  end subroutine dpd_d

  !=======================================================================!
  ! sigN -> piD potential
  !=======================================================================!

  subroutine v_sigN_piD(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)         ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm   ! input
      complex(kind(0.d0)) ::     ecm       ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm         ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(0:3)
      complex(kind(0.d0)) :: v_hel(0:3,1:5)
      complex(kind(0.d0)) :: w(5,0:3)
      complex(kind(0.d0)) :: w_iso(5,0:3,2)
      complex(kind(0.d0)) :: w_isosum(0:3,2)
      integer :: i,ityp,ihel,ng,idel,ldel,iso,isoindx,ldel2

      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wrho = masses(3,2)
      wrho2 = masses(3,2)*masses(3,2)
      wdel = masses(2,1)
      wdel2 = wdel*wdel
      wsig2 = masses(2,2)*masses(2,2)
      wsig = masses(2,2)

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wsig2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wsig2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wdel2 -wpi2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wpi2 -wdel2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wsig2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wdel2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wpi2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wdel)

      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wdel)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do idel= 0,3
        do ityp = 1,5
          w(ityp,idel)    =(0.d0,0.d0)
          v_hel(idel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        call spinor_rsout_cm(urs3,pf_cm,p3_cm(0),cs_cm,sn_cm,cs_cm_h,sn_cm_h,wdel)

        if(coupl(4,5,1)%idiag == 1) then           ! pi-t
          call dsd_c(v_hel(:,1),coupl(4,5,1),ecm)
        end if

        ! do the cos theta integration:
        do idel = 0,3
          ldel=3-2*idel
          ! ldel=3-2*idel leads to the following realtion between physival helicity and the one used here:
          ! la_phys   ldel  idel
          ! 3/2       3     0
          ! 1/2       1     1
          ! -1/2     -1     2
          ! 3/2      -3     3
          if (abs(ldel)<=j1) then
            do ityp = 1,itypmax(ccrel(5,4))
              w(ityp,idel)=w(ityp,idel)&
                  &+djw(ng,ldel,1)*v_hel(idel,ityp)
              ! the initial Npi state has |1/2,0>
            end do
          end if
        end do

      end do  ! of ng loop

      !  isospin factors
      do idel = 0,3

        w_iso(1,idel,1) =   -w(1,idel)*dsqrt(2.d0) ! I = 1/2  pi-t
        w_iso(1,idel,2) =   (0.d0,0.d0)

        w_isosum(idel,1)=(0.d0,0.d0)
        w_isosum(idel,2)=(0.d0,0.d0)
        do ityp=1,itypmax(ccrel(5,4))
          w_isosum(idel,1)=w_isosum(idel,1)+w_iso(ityp,idel,1)*cnorm
          w_isosum(idel,2)=w_isosum(idel,2)+w_iso(ityp,idel,2)*cnorm
        end do

      end do

      ! transformation in JLS basis:
      do iso=1,2

        isoindx=2*iso-2

        v(1+isoindx) = 2*(e(1,-1)*w_isosum(1,iso)-e(1,-1)*w_isosum(2,iso)+&
            &e(2,-1)*w_isosum(0,iso)-e(2,-1)*w_isosum(3,iso))*d(1,1)

        v(2+isoindx) = 2*(e(1,1)*w_isosum(1,iso)+e(1,1)*w_isosum(2,iso)+e(&
            &2,1)*w_isosum(0,iso)+e(2,1)*w_isosum(3,iso))*d(1,-1)

        v(5+isoindx) = 2*(f(1,1)*w_isosum(1,iso)-f(1,1)*w_isosum(2,iso)+f(&
            &2,1)*w_isosum(0,iso)-f(2,1)*w_isosum(3,iso))*d(1,1)

        v(6+isoindx) = 2*(f(1,-1)*w_isosum(1,iso)+f(1,-1)*w_isosum(2,iso)+&
            &f(2,-1)*w_isosum(0,iso)+f(2,-1)*w_isosum(3,iso))*d(1,-1)

      end do  ! iso loop

  end subroutine v_sigN_piD

  !=======================================================================!

  subroutine dsd_c(hel,pc,ecms)
      implicit none
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! pi exchange
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      complex(kind(0.d0)), dimension(0:3) :: hel            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: pt_sq,psq
      complex(kind(0.d0)) :: e_t,pps1,pps2
      complex(kind(0.d0)),dimension(1:4,0:3) :: uhelp1,uhelp2
      complex(kind(0.d0)) ,dimension(0:3) :: ndpi1,ndpi2,scalct
      integer ::  i,idel,mu,is
      complex(kind(0.d0)) :: fi,ff,cc
      !
      p_t1(0)=eps1-eps3    ! on-energy shell
      do i=1,3
        p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
        p_t2 ( i ) = p_t1(i)
      enddo
      pt_sq = p_t1(1)**2 + p_t1(3)**2     ! cm
      psq   = p_t1(0)**2 - pt_sq         ! four vector squared
      e_t   = cdsqrt(pc%wf**2+pt_sq)

      p_t1(0)=e_t
      p_t2(0)=-e_t

      ! construct NDpi vertex (ubar^mu u q_mu): contact lorentz index of the rs spinor:
      do idel=0,3
        do is=1,4
          uhelp1(is,idel)=(0.d0,0.d0)
          uhelp2(is,idel)=(0.d0,0.d0)
          do mu=0,3
            uhelp1(is,idel)=urs3(is,mu,idel)*p_t1(mu)*metric(mu)
            uhelp2(is,idel)=urs3(is,mu,idel)*p_t2(mu)*metric(mu)
          enddo
        enddo
        call spinor_bar_spinor(ndpi1(idel),uhelp1(:,idel),u1(:,1))
        call spinor_bar_spinor(ndpi2(idel),uhelp2(:,idel),u1(:,1))
        ! ct term
        call spinor_bar_spinor(scalct(idel),urs3(:,0,idel),u1(:,1))
      enddo

      ! pipisigma vertex:
      pps1=(0.d0,0.d0)
      pps2=(0.d0,0.d0)
      do mu=0,3
        pps1=pps1+p_t1(mu)*p4_cm(mu)*metric(mu)
        pps2=pps2+p_t2(mu)*p4_cm(mu)*metric(mu)
      enddo


      !      if(imodel==1)then
      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow !piND npow = 2
      ff=(pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq) !pipisig
      ff=ff*fi
      !      end if

      cc = ff * pc%gc * ci /(2.d0*e_t)

      do idel=0,3
        hel(idel)=(ndpi1(idel)*pps1/(ecms-e_t-p2_cm(0)-p3_cm(0))+&
            &ndpi2(idel)*pps2/(ecms-e_t-p4_cm(0)-p1_cm(0))+&
            &scalct(idel)*p4_cm(0)*2.d0*e_t)*cc
      enddo
      return
  end subroutine dsd_c


  !=======================================================================!
  !  piD -> piD potential
  !=======================================================================!

  subroutine v_piD_piD(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)         ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm   ! input
      complex(kind(0.d0)) ::     ecm       ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm         ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(0:3)
      complex(kind(0.d0)) :: v_hel(0:3,0:1,1:14)
      complex(kind(0.d0)) :: w(14,0:3,0:1)
      complex(kind(0.d0)) :: w_iso(14,0:3,0:1,2)
      complex(kind(0.d0)) :: w_isosum(0:3,0:1,2)
      integer :: i,ityp,ihel,ng,idel,ldel,iso,isoindx,idel2,ldel2,onshe

      if (imag(pf_cm).gt.0.d0) then		! (offresi)
        onshe=1
      else
        onshe=0
      end if

      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wrho = masses(3,2)
      wrho2 = masses(3,2)*masses(3,2)
      wdel = masses(2,1)
      wdel2 = wdel*wdel

      ! on-energy shell energies
      eps1 = ( ecm**2 + wdel2 -wpi2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wpi2 -wdel2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wdel2 -wpi2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wpi2 -wdel2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wdel2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wpi2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wdel2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wpi2 )

      ptildei = pcmi/(p1_cm(0)+wdel)
      ptildef = pcmf/(p3_cm(0)+wdel)

      cnorm = cdsqrt( (p1_cm(0)+wdel)/p1_cm(0) * (p3_cm(0)+wdel)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Delta
      call spinor_rsin_cm(urs1,pi_cm,p1_cm(0),1.d0,0.d0,1.d0,0.d0,wdel)

      do idel2= 0,3
        do idel=0,1
          do ityp = 1,itypmax(ccrel(4,4))
            w(ityp,idel2,idel)    =(0.d0,0.d0)
            v_hel(idel2,idel,ityp)=(0.d0,0.d0)
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        call spinor_rsout_cm(urs3,pf_cm,p3_cm(0),cs_cm,sn_cm,cs_cm_h,sn_cm_h,wdel)

        if(coupl(4,4,1)%idiag == 1) then            ! n-u
          call ddd_b(v_hel(:,:,1),coupl(4,4,1),ecm,onshe)
        endif

        if(coupl(4,4,2)%idiag == 1) then            ! rho-t
          call ddd_c(v_hel(:,:,2),coupl(4,4,2),ecm)
        else if(coupl(4,4,2)%idiag == 2) then
          call ddd_c2(v_hel(:,:,2),coupl(4,4,2),ecm)
        endif

        if(coupl(4,4,3)%idiag == 1) then            ! D-u
          call ddd_d(v_hel(:,:,3),coupl(4,4,3),ecm)
        endif

        ! the v_hel's from the subroutine have the following notation:
        ! v_hel(idel2,idel1,ityp)
        !       !     !     !
        !       !     !     diagram
        !       !     D in
        !       D out

        ! do the cos theta integration:
        do idel = 0,1
          do idel2 = 0,3
            ldel=3-2*idel
            ldel2=3-2*idel2
            ! ldel=3-2*idel leads to the following realtion between physival helicity and the one used here:
            ! la_phys   ldel  idel
            ! 3/2       3     0
            ! 1/2       1     1
            ! -1/2     -1     2
            ! 3/2      -3     3
            if ((abs(ldel)<=j1).and.(abs(ldel2)<=j1)) then
              do ityp = 1,itypmax(ccrel(4,4))
                w(ityp,idel2,idel)=w(ityp,idel2,idel)+djw(ng,ldel2,ldel)*v_hel(idel2,idel,ityp)
                ! the initial Dpi state has |1/2,0> or |3/2,0>
              end do
            end if
          end do
        end do

      end do  ! of ng loop

      !  isospin factors
      do idel2 = 0,3
        do idel = 0,1

          w_iso(1,idel2,idel,1) =   w(1,idel2,idel)/3.d0          ! I = 1/2  nucleon u
          w_iso(1,idel2,idel,2) = - w(1,idel2,idel)*2.d0/3.d0     ! I = 3/2

          w_iso(2,idel2,idel,1) =   w(2,idel2,idel)*5.d0/3.d0*ci  ! I = 1/2  rho-t
          w_iso(2,idel2,idel,2) =   w(2,idel2,idel)*2.d0/3.d0*ci  ! I = 3/2

          w_iso(3,idel2,idel,1) = - w(3,idel2,idel)*10.d0/9.d0    ! I = 1/2  Delta-u
          w_iso(3,idel2,idel,2) =   w(3,idel2,idel)*11.d0/9.d0    ! I = 3/2

          w_isosum(idel2,idel,1)=(0.d0,0.d0)
          w_isosum(idel2,idel,2)=(0.d0,0.d0)

          do ityp=1,itypmax(ccrel(4,4))
            w_isosum(idel2,idel,1)=w_isosum(idel2,idel,1)+w_iso(ityp,idel2,idel,1)*cnorm
            w_isosum(idel2,idel,2)=w_isosum(idel2,idel,2)+w_iso(ityp,idel2,idel,2)*cnorm
          end do

        end do
      end do

      ! transformation in JLS basis:
      do iso=1,2

        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*e(1,-1)**2*w_isosum(1,1,iso)-2.d0*e(1,-1)**2*w_isosu&
            &m(2,1,iso)+2.d0*e(1,-1)*e(2,-1)*w_isosum(0,1,iso)-2.d0*e(1,-1)*e(2,-1)*w&
            &_isosum(3,1,iso)+2.d0*e(2,-1)*e(1,-1)*w_isosum(1,0,iso)-2.d0*e(2,-1)*e(1&
            &,-1)*w_isosum(2,0,iso)+2.d0*e(2,-1)**2*w_isosum(0,0,iso)-2.d0*e(2,-1)**2&
            &*w_isosum(3,0,iso)

        v(2+isoindx) = 2.d0*e(1,1)**2*w_isosum(1,1,iso)+2.d0*e(1,1)**2*w_isosum(&
            &2,1,iso)+2.d0*e(1,1)*e(2,1)*w_isosum(0,1,iso)+2.d0*e(1,1)*e(2,1)*w_isosu&
            &m(3,1,iso)+2.d0*e(2,1)*e(1,1)*w_isosum(1,0,iso)+2.d0*e(2,1)*e(1,1)*w_iso&
            &sum(2,0,iso)+2.d0*e(2,1)**2*w_isosum(0,0,iso)+2.d0*e(2,1)**2*w_isosum(3,&
            &0,iso)

        v(5+isoindx) = 2.d0*e(1,-1)*f(1,1)*w_isosum(1,1,iso)-2.d0*e(1,-1)*f(1,1)&
            &*w_isosum(2,1,iso)+2.d0*e(1,-1)*f(2,1)*w_isosum(0,1,iso)-2.d0*e(1,-1)*f(&
            &2,1)*w_isosum(3,1,iso)+2.d0*e(2,-1)*f(1,1)*w_isosum(1,0,iso)-2.d0*e(2,-1&
            &)*f(1,1)*w_isosum(2,0,iso)+2.d0*e(2,-1)*f(2,1)*w_isosum(0,0,iso)-2.d0*e(&
            &2,-1)*f(2,1)*w_isosum(3,0,iso)

        v(6+isoindx) = 2.d0*e(1,1)*f(1,-1)*w_isosum(1,1,iso)+2.d0*e(1,1)*f(1,-1)&
            &*w_isosum(2,1,iso)+2.d0*e(1,1)*f(2,-1)*w_isosum(0,1,iso)+2.d0*e(1,1)*f(2&
            &,-1)*w_isosum(3,1,iso)+2.d0*e(2,1)*f(1,-1)*w_isosum(1,0,iso)+2.d0*e(2,1)&
            &*f(1,-1)*w_isosum(2,0,iso)+2.d0*e(2,1)*f(2,-1)*w_isosum(0,0,iso)+2.d0*e(&
            &2,1)*f(2,-1)*w_isosum(3,0,iso)

        v(9+isoindx) = 2.d0*f(1,1)**2*w_isosum(1,1,iso)-2.d0*f(1,1)**2*w_isosum(&
            &2,1,iso)+2.d0*f(1,1)*f(2,1)*w_isosum(0,1,iso)-2.d0*f(1,1)*f(2,1)*w_isosu&
            &m(3,1,iso)+2.d0*f(2,1)*f(1,1)*w_isosum(1,0,iso)-2.d0*f(2,1)*f(1,1)*w_iso&
            &sum(2,0,iso)+2.d0*f(2,1)**2*w_isosum(0,0,iso)-2.d0*f(2,1)**2*w_isosum(3,&
            &0,iso)

        v(10+isoindx) = 2.d0*f(1,-1)**2*w_isosum(1,1,iso)+2.d0*f(1,-1)**2*w_isos&
            &um(2,1,iso)+2.d0*f(1,-1)*f(2,-1)*w_isosum(0,1,iso)+2.d0*f(1,-1)*f(2,-1)*&
            &w_isosum(3,1,iso)+2.d0*f(2,-1)*f(1,-1)*w_isosum(1,0,iso)+2.d0*f(2,-1)*f(&
            &1,-1)*w_isosum(2,0,iso)+2.d0*f(2,-1)**2*w_isosum(0,0,iso)+2.d0*f(2,-1)**&
            &2*w_isosum(3,0,iso)

        v(13+isoindx) = 2.d0*e(1,-1)*f(1,1)*w_isosum(1,1,iso)-2.d0*e(1,-1)*f(1,1&
            &)*w_isosum(2,1,iso)+2.d0*f(1,1)*e(2,-1)*w_isosum(0,1,iso)-2.d0*f(1,1)*e(&
            &2,-1)*w_isosum(3,1,iso)+2.d0*f(2,1)*e(1,-1)*w_isosum(1,0,iso)-2.d0*f(2,1&
            &)*e(1,-1)*w_isosum(2,0,iso)+2.d0*e(2,-1)*f(2,1)*w_isosum(0,0,iso)-2.d0*e&
            &(2,-1)*f(2,1)*w_isosum(3,0,iso)

        v(14+isoindx) = 2.d0*e(1,1)*f(1,-1)*w_isosum(1,1,iso)+2.d0*e(1,1)*f(1,-1&
            &)*w_isosum(2,1,iso)+2.d0*f(1,-1)*e(2,1)*w_isosum(0,1,iso)+2.d0*f(1,-1)*e&
            &(2,1)*w_isosum(3,1,iso)+2.d0*f(2,-1)*e(1,1)*w_isosum(1,0,iso)+2.d0*f(2,-&
            &1)*e(1,1)*w_isosum(2,0,iso)+2.d0*e(2,1)*f(2,-1)*w_isosum(0,0,iso)+2.d0*e&
            &(2,1)*f(2,-1)*w_isosum(3,0,iso)

      end do  ! iso loop

  end subroutine v_piD_piD

  !=======================================================================!

  subroutine ddd_b(hel,pc,ecms,onshe)  ! N-exchange in pi Delta --> pi Delta
      implicit none

      ! output
      complex(kind(0.d0)), dimension(0:3,0:1) :: hel
      complex(kind(0.d0)), dimension(0:3,0:3) :: dhel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)) :: g_pnd_2(0:3),g_pnd_4(0:3)
      complex(kind(0.d0)),dimension(1:4,0:3) :: uh1,uh3
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: cc,cn
      integer :: i,idel,is,mu,idel2
      complex(kind(0.d0)) :: fi,ff   ! form factor
      complex(kind(0.d0)) :: temp2pos,temp2neg,e1,e2,e3,e4,alpha1,alpha3,p,k,eex
      real(kind(0.d0)) :: m1,m2,m3,m4,ct(1),sinth,cost2,sint2,mex,m
      integer :: th,onshe

      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      end do
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      e_u   = cdsqrt(pc%wf**2+pu_sq)			! This is now manipulated:

      if (onshe==1.and.manip_v==1) then				! (offresi)
        e_u   = cdsqrt(dcmplx(pc%wf,-60.d0)**2+pu_sq)
      else
        e_u   = cdsqrt(dcmplx(pc%wf,0.d0)**2+pu_sq)
      end if

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.d0 * e_u )

      ! piND vertex
      g_pnd_4 = p4_cm
      ! piNDelta vertex
      g_pnd_2 = p2_cm

      ! contract lorentz index:
      do is=1,4
        do idel=0,3
          uh1(is,idel)=(0.d0,0.d0)
          uh3(is,idel)=(0.d0,0.d0)
          do mu=0,3
            uh1(is,idel)=uh1(is,idel)+urs1(is,mu,idel)*metric(mu)*g_pnd_4(mu)
            uh3(is,idel)=uh3(is,idel)+urs3(is,mu,idel)*metric(mu)*g_pnd_2(mu)
          end do
        end do
      end do

      do idel=0,1
        call mat_spinor(ux,sf,uh1(:,idel))
        do idel2=0,3
          call spinor_bar_spinor(hel(idel2,idel),uh3(:,idel2),ux)
          !         call spinor_bar_spinor(dhel(idel2,idel),uh3(:,idel2),ux)
        end do
      end do
      !
      fi=((pc%al1**2-wnu2)/(pc%al1**2+pu_sq))**pc%npow   !piND npow=2
      ff=((pc%al1**2-wnu2)/(pc%al1**2+pu_sq))**pc%npow   !piND npow=2
      ff=ff*fi
      !     if(imodel==3) then
      !       call ff_3(ff,p_u(0),p_u,eps1,p1_cm,eps4,p4_cm,2,2,5)
      !       call ff_3(fi,p_u(0),p_u,eps3,p3_cm,eps2,p2_cm,2,2,5)
      !       ff = fi*ff
      !     end if

      cc = ff * pc%gc
      do idel=0,1
        do idel2=0,3
          hel(idel2,idel)=hel(idel2,idel)*cc
          !         hel(idel2,idel)=dhel(idel2,idel)
        end do
      end do

      return
  end subroutine ddd_b

  !=======================================================================!

  subroutine ddd_c(hel,pc,ecms)  ! rho-exchange in pi Delta --> pi Delta
      implicit none

      ! output
      complex(kind(0.d0)), dimension(0:3,0:1) :: hel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: pt_sq,psq
      complex(kind(0.d0)) :: e_t
      complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: g_rdd_1,g_rdd_2
      complex(kind(0.d0)) :: g_ppr(0:3)
      complex(kind(0.d0)),dimension(0:3,0:3) :: prop1,prop2
      complex(kind(0.d0)),dimension(0:3) :: shelp1,shelp2
      complex(kind(0.d0)), dimension(1:4,1:4) :: saux1,saux2
      complex(kind(0.d0)),dimension(1:4,0:3,0:3) :: uh1,uh2
      complex(kind(0.d0)) :: cc,scal1,scal2
      integer :: i,idel,is,js,mu,nu,idel2
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
        p_t2 ( i ) = p_t1(i)
      end do
      pt_sq = p_t1(1)**2 + p_t1(3)**2     ! cm
      e_t   = cdsqrt(pc%wf**2+pt_sq)

      ! rhoDD vertex
      p_t1(0)=e_t
      call vertex_rhoNN(g_rdd_1,p_t1,wdel,pc%ka)
      p_t2(0)=-e_t
      call vertex_rhoNN(g_rdd_2,p_t2,wdel,pc%ka)

      ! pipirho vertex
      do i=0,3
        g_ppr(i) = -ci*(p2_cm(i)+p4_cm(i))
      end do       ! minus sign comes from the relative minus sign between piNN and DDrho coupling

      ! propagator:
      do mu=0,3
        do nu=0,3
          prop1(mu,nu)=1.d0/(2.d0*e_t)*&
              &(-gmunu(mu,nu))/(ecms-e_t-p3_cm(0)-p2_cm(0))  ! only gmunu
          !                &(-gmunu(mu,nu)+p_t1(mu)*p_t1(nu)/wrho2)/(ecms-e_t-p3_cm(0)-p2_cm(0))  ! with k_mu k_nu
          prop2(mu,nu)=1.d0/(2.d0*e_t)*&
              &(-gmunu(mu,nu))/(ecms-e_t-p1_cm(0)-p4_cm(0))  ! only gmunu
          !                &(-gmunu(mu,nu)+p_t2(mu)*p_t2(nu)/wrho2)/(ecms-e_t-p1_cm(0)-p4_cm(0))  ! with k_mu k_nu
        end do
      end do

      ! contract lorentz index:
      do mu=0,3
        shelp1(mu)=(0.d0,0.d0)
        shelp2(mu)=(0.d0,0.d0)
        do nu=0,3
          shelp1(mu)=shelp1(mu)+prop1(mu,nu)*g_ppr(nu)*metric(nu)
          shelp2(mu)=shelp2(mu)+prop2(mu,nu)*g_ppr(nu)*metric(nu)
        end do
      end do

      do is=1,4
        do js=1,4
          saux1(is,js)=(0.d0,0.d0)
          saux2(is,js)=(0.d0,0.d0)
          do mu=0,3
            saux1(is,js)=saux1(is,js)+g_rdd_1(is,js,mu)*shelp1(mu)*metric(mu)
            saux2(is,js)=saux2(is,js)+g_rdd_2(is,js,mu)*shelp2(mu)*metric(mu)
          end do
        end do
      end do


      do mu=0,3
        do idel=0,1
          call mat_spinor(uh1(:,mu,idel),saux1(:,:),urs1(:,mu,idel))
          call mat_spinor(uh2(:,mu,idel),saux2(:,:),urs1(:,mu,idel))
        end do
      end do


      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow !rhoDD npow = 2
      ff=(pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq) !pipirho
      ff=ff*fi
      !     if(imodel==3) then
      !       call ff_3(ff,p_u(0),p_u,eps1,p1_cm,eps4,p4_cm,2,2,5)
      !       call ff_3(fi,p_u(0),p_u,eps3,p3_cm,eps2,p2_cm,2,2,5)
      !       ff = fi*ff
      !     end if

      cc = ff * pc%gc

      do idel=0,1
        do idel2=0,3
          hel(idel2,idel)=(0.d0,0.d0)
          do mu=0,3
            call spinor_bar_spinor(scal1,urs3(:,mu,idel2),uh1(:,mu,idel))
            call spinor_bar_spinor(scal2,urs3(:,mu,idel2),uh2(:,mu,idel))
            hel(idel2,idel)=hel(idel2,idel)+scal1*cc*metric(mu)+scal2*cc*metric(mu)
          end do
        end do
      end do

      return
  end subroutine ddd_c

  !=======================================================================!

  subroutine ddd_c2(hel,pc,ecms)
      implicit none
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! rho exchange a la Schuetz (using Gordon decomposition
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      complex(kind(0.d0)), dimension(0:3,0:1) :: hel            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)),dimension(0:3) :: p_b,p_m,p_t
      complex(kind(0.d0)) :: pt_sq,psq
      complex(kind(0.d0)) :: e_t,propa,scalt,scalvt,gvt,gt,pmpb
      complex(kind(0.d0)),dimension(1:4,1:4) :: pmsl
      complex(kind(0.d0)), dimension(1:4) :: ux
      integer ::  i,idel,mu,nu,idel2
      complex(kind(0.d0)) :: fi,ff            ! form factor
      !

      p_m(0)=p2_cm(0)+p4_cm(0)
      p_b(0)=p1_cm(0)+p3_cm(0)
      p_t(0)=eps1-eps3
      do i=1,3
        p_m ( i ) = p2_cm ( i ) + p4_cm ( i )
        p_b ( i ) = p1_cm ( i ) + p3_cm ( i )
        p_t ( i ) = p1_cm(i)-p3_cm(i)
      enddo
      pt_sq = p_t(1)**2 + p_t(3)**2     ! cm
      psq   = p_t(0)**2 - pt_sq         ! four vector squared
      e_t   = cdsqrt(pc%wf**2+pt_sq)


      ! propa
      propa=(1.d0/(ecms-e_t-p3_cm(0)-p2_cm(0))+1.d0/(ecms-e_t-p4_cm(0)-p1_cm(0)))/2.d0/e_t

      ! gv+gt part of the coupling:
      call p_slash(pmsl,p_m)

      ! gt part:
      pmpb=(0.d0,0.d0)
      do mu=0,3
        pmpb=pmpb+p_m(mu)*p_b(mu)*metric(mu)
      enddo

      ! ff
      !      if(imodel==1)then
      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow !rhoDD npow = 2
      ff=(pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq) !pipirho
      ff=ff*fi
      !      end if


      do idel=0,1
        do idel2=0,3
          gt=(0.d0,0.d0)
          gvt=(0.d0,0.d0)
          do nu=0,3
            call mat_spinor(ux,pmsl,urs1(:,nu,idel))
            call spinor_bar_spinor(scalvt,urs3(:,nu,idel2),ux)
            call spinor_bar_spinor(scalt,urs3(:,nu,idel2),urs1(:,nu,idel))
            ! sum them up
            gvt=gvt+scalvt*metric(nu)
            gt=gt+scalt*metric(nu)
          enddo
          hel(idel2,idel)=ci*pc%gc*((1.d0+pc%ka)*gvt-pc%ka/2.d0/wdel*gt*pmpb)*ff*propa
        enddo
      enddo

      !
      return
  end subroutine ddd_c2

  !=======================================================================!

  subroutine ddd_d(hel,pc,ecms)
      implicit none
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Delta exchange
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! agrees with Schuetz
      complex(kind(0.d0)), dimension(0:3,0:1) :: hel            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4,0:3,0:3) :: sf,sfpos,sfneg
      complex(kind(0.d0)),dimension( 1:4, 1:4 ) :: g_pv_2,g_pv_4
      complex(kind(0.d0)) :: uh(1:4,0:3)
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: shelp
      complex(kind(0.d0)), dimension(1:4,0:3,0:1) :: shelp1,shelp2,shelp3
      complex(kind(0.d0)) :: cc,scal
      integer ::  i,idel,is,js,mu,nu,idel2
      complex(kind(0.d0)) :: fi,ff            ! form factor
      !
      p_u(0)=eps1-eps4    ! on-energy shell
      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      enddo
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      psq   = p_u(0)**2 - pu_sq         ! four vector squared
      e_u   = cdsqrt(pc%wf**2+pu_sq)

      ! Schuetz:
      call prop_delta_rs(sfpos,p_u,pc%wf)
      sf=sfpos*(1.d0/(ecms-e_u-p2_cm(0)-p4_cm(0))+1.d0/(ecms-e_u-p1_cm(0)-p3_cm(0)))/(2.d0*e_u)

      ! TOPT:
      !      p_u(0)=e_u
      !      call prop_delta_pos(sfpos,p_u,pc%wf,.true.)
      !      p_u(0)=-e_u
      !      call prop_delta_neg(sfneg,p_u,pc%wf,.true.)
      !
      !      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
      !               &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) )&
      !             &/( 2.d0 * e_u )

      ! piDD vertex
      call vertex_pv(g_pv_2,p2_cm )
      ! piDD vertex
      call vertex_pv(g_pv_4,-1.d0*p4_cm )


      ! multiply dirac matrix with spinor
      do nu=0,3
        do idel=0,1
          call mat_spinor(shelp1(:,nu,idel),g_pv_4,urs1(:,nu,idel))
        enddo
      enddo

      ! contract lorentz index of the initial spinor:
      shelp2=(0.d0,0.d0)
      do mu=0,3
        do idel=0,1
          do nu=0,3
            call mat_spinor(ux,sf(:,:,mu,nu),shelp1(:,nu,idel))
            do is=1,4
              shelp2(is,mu,idel)=shelp2(is,mu,idel)+ux(is)*metric(nu)
            enddo
          enddo
          call mat_spinor(shelp3(:,mu,idel),g_pv_2,shelp2(:,mu,idel))
        enddo
      enddo

      !      if(imodel==1)then
      fi=(pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq) !piDD
      ff=(pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq) !piDD
      fi=ff*fi
      ff=fi**pc%npow ! npow = 2
      !      end if
      !      if(imodel==3) then
      !       call ff_3(ff,p_u(0),p_u,eps1,p1_cm,eps4,p4_cm,2,2,5)
      !       call ff_3(fi,p_u(0),p_u,eps3,p3_cm,eps2,p2_cm,2,2,5)
      !       ff = fi*ff
      !      endif

      cc = ff * pc%gc

      ! build scalar
      do idel=0,1
        do idel2=0,3
          hel(idel2,idel)=(0.d0,0.d0)
          do mu=0,3
            call spinor_bar_spinor(scal,urs3(:,mu,idel2),shelp3(:,mu,idel))
            hel(idel2,idel)=hel(idel2,idel)+scal*metric(mu)*cc
          enddo
        enddo
      enddo
      !
      return
  end subroutine ddd_d


  !=======================================================================!
  !  piN -> sigN potential
  !=======================================================================!

  subroutine v_piN_sigN(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)         ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm   ! input
      complex(kind(0.d0)) ::     ecm       ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm         ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:8)
      complex(kind(0.d0)) :: w(8,2)
      complex(kind(0.d0)) :: w_iso(8,2,2)
      complex(kind(0.d0)) :: w_lsj(8,4)
      integer :: i,ityp,ihel,ng,onshe

      if (imag(pf_cm).gt.0.d0) then		! (offresi)
        onshe=1
      else
        onshe=0
      end if

      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wsig2 = masses(2,2)*masses(2,2)
      wsig = masses(2,2)

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wpi2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wpi2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wnu2 -wsig2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wsig2 -wnu2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wpi2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wnu2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsig2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wnu)

      ! angle-independent quantities

      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wnu)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(1,5))
          w(ityp,ihel)    =(0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing nucleon
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wnu)

        if(coupl(1,5,1)%idiag == 1) then           ! nucleon exchange
          call ds_b(v_hel(:,1),coupl(1,5,1),ecm)
        end if

        if(coupl(1,5,2)%idiag == 1) then           ! pion exchange
          call ds_c(v_hel(:,2),coupl(1,5,2),ecm,onshe)
        else if(coupl(1,5,2)%idiag == 2) then      ! pion exchange Feynman
          call ds_c2(v_hel(:,2),coupl(1,5,2),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(1,5))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do  ! ng-loop

      ! isospin factors
      do ihel = 1,2
        w_iso(1,ihel,1) =   w(1,ihel) * dsqrt(3.d0)  ! I = 1/2  nucleon u
        w_iso(2,ihel,1) =   w(2,ihel) * dsqrt(3.d0)  ! I = 1/2  pion t
      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(1,5))
        w_lsj(i,1)= -w_iso(i,1,1) + w_iso(i,2,1)   ! l=j-1/2; I=1/2
        w_lsj(i,2)= -w_iso(i,1,1) - w_iso(i,2,1)   ! l=j+1/2; I=1/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
      end do
      do i = 1,2
        do ityp = 1,itypmax(ccrel(1,5))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_piN_sigN

  !=======================================================================!

  subroutine ds_b(hel,pc,ecms)  ! N-exchange in pi N --> sigma N
      implicit none

      ! output
      complex(kind(0.d0)), dimension(1:2) :: hel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)) :: g_pv_2( 1:4, 1:4 )
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc,aux
      integer :: i,ihel
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      end do
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      e_u   = cdsqrt(pc%wf**2+pu_sq)

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      ! only pos energy part
      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.d0 * e_u )

      call vertex_pv(g_pv_2,p2_cm )

      call mat_spinor(ux,sf,u1(:,1))
      call mat_spinor(uy,g_pv_2,ux)
      do i=1,2
        call spinor_bar_spinor(hel(i),u3(:,i),uy)
      end do

      aux=(wnu2-wpi2)/wnu
      fi=(pc%al1**2-wnu2)/(pc%al1**2-aux**2+pu_sq) !piNN vertex
      ff=(pc%al2**2-wnu2)/(pc%al2**2+pu_sq) ! sigmaNN vertex

      fi=ff*fi
      ff=fi**pc%npow

      cc = ff * pc%gc

      do i=1,2
        hel(i)=hel(i)*cc
      end do

      return
  end subroutine ds_b

  !=======================================================================!

  subroutine ds_c(hel,pc,ecms,onshe)  ! pi-exchange in pi N --> sigma N
      implicit none

      ! output
      complex(kind(0.d0)), dimension(1:2) :: hel,scal1,scal2,scalct
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_t1(0:3),p_t2(0:3),p_tsq,omega_t
      complex(kind(0.d0)) :: cc,pq1,pq2
      complex(kind(0.d0)),dimension(1:4) :: ux,uy,uz
      complex(kind(0.d0)),dimension(1:4,1:4) :: gpv1,gpv2,gpvct
      integer :: i,ihel,mu,onshe
      complex(kind(0.d0)) :: fi,ff,fineps   ! form factor

      do i=1,3
        p_t1(i)=p1_cm(i)-p3_cm(i)
        p_t2(i)=p1_cm(i)-p3_cm(i)
      end do

      p_tsq=p_t1(1)**2+p_t1(2)**2+p_t1(3)**2   ! 3 momentum squared

      omega_t=cdsqrt(p_tsq+pc%wf**2)

      p_t1(0)=omega_t
      p_t2(0)=-omega_t

      ! piNN vertex
      call vertex_pv(gpv1,p_t1)
      call vertex_pv(gpv2,p_t2)
      ! ct term
      call vertex_pvct(gpvct)

      ! pipisigma vertex
      pq1=(0.d0,0.d0)
      pq2=(0.d0,0.d0)
      do mu=0,3
        pq1=pq1+p_t1(mu)*p2_cm(mu)*metric(mu)
        pq2=pq2+p_t2(mu)*p2_cm(mu)*metric(mu)
      end do

      call mat_spinor(ux,gpv1,u1(:,1))
      call mat_spinor(uy,gpv2,u1(:,1))
      ! ct term
      call mat_spinor(uz,gpvct,u1(:,1))

      do i=1,2
        call spinor_bar_spinor(scal1(i),u3(:,i),ux)
        call spinor_bar_spinor(scal2(i),u3(:,i),uy)
        ! ct term
        call spinor_bar_spinor(scalct(i),u3(:,i),uz)
      end do

      ff=(pc%al1**2-wpi2)/(pc%al1**2+p_tsq) ! piNN
      fi=(pc%al2**2-wpi2)/(pc%al2**2+p_tsq) ! pipisigma
      fi=ff*fi
      ff=fi**pc%npow

      cc = - ff * pc%gc/(2.d0*omega_t)

      if (onshe==1.and.manip_v==1) then			! (offresi)
        fineps=dcmplx(0.d0,0.d0*10.d0)
      else
        fineps=dcmplx(0.d0,0.d0)
      end if

      do i=1,2
        hel(i)=(pq1*scal1(i)/(ecms - omega_t - p2_cm(0) -p3_cm(0)+fineps)+&
            &pq2*scal2(i)/(ecms - omega_t - p1_cm(0) -p4_cm(0))-&
            &ci*p2_cm(0)*scalct(i)*2.d0*omega_t)*cc   ! last term is ct term
      end do

      return
  end subroutine ds_c

  !=======================================================================!

  subroutine ds_c2(hel,pc,ecms)
      implicit none
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! pion exchange Feynman
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      complex(kind(0.d0)), dimension(1:2) :: hel,scal1,scal2,scalct   ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_t1(0:3),p_tsq,omega_t
      complex(kind(0.d0)) :: cc,pq1,pq2
      complex(kind(0.d0)),dimension(1:4) :: ux,uy,uz
      complex(kind(0.d0)),dimension(1:4,1:4) :: gpv1,gpv2,gpvct
      integer ::  i,ihel,mu
      complex(kind(0.d0)) :: fi,ff            ! form factor
      !
      !
      do i=1,3
        p_t1(i)=p1_cm(i)-p3_cm(i)
      enddo

      p_tsq=p_t1(1)**2+p_t1(2)**2+p_t1(3)**2 ! 3 momentum squared

      omega_t=cdsqrt(p_tsq+pc%wf**2)

      p_t1(0)=eps1-eps3


      ! piNN vertex
      call vertex_pv(gpv1,p_t1)

      ! pipisigma vertex
      pq1=(0.d0,0.d0)
      do mu=0,3
        pq1=pq1+p_t1(mu)*p2_cm(mu)*metric(mu)
      enddo

      call mat_spinor(ux,gpv1,u1(:,1))

      do i=1,2
        call spinor_bar_spinor(scal1(i),u3(:,i),ux)
      enddo
      !

      !      if(imodel==1)then
      ff=(pc%al1**2-wpi2)/(pc%al1**2+p_tsq) ! piNN
      fi=(pc%al2**2-wpi2)/(pc%al2**2+p_tsq) ! pipisigma
      fi=ff*fi
      ff=fi**pc%npow
      !      end if

      cc = - ff * pc%gc

      do i=1,2
        hel(i)=cc*pq1*scal1(i)/(p_t1(0)**2-p_tsq-pc%wf**2)
        ! static prop
        !       hel(i)=cc*pq1*scal1(i)/(-p_tsq-pc%wf**2)
      enddo
      return
  end subroutine ds_c2

  !=======================================================================!
  !  sigN -> sigN potential
  !=======================================================================!

  subroutine v_sigN_sigN(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)         ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm   ! input
      complex(kind(0.d0)) ::     ecm       ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:8)
      complex(kind(0.d0)) :: w(8,2)
      complex(kind(0.d0)) :: w_iso(8,2,2)
      complex(kind(0.d0)) :: w_lsj(8,4)
      integer :: i,ityp,ihel,ng

      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wsig2 = masses(2,2)*masses(2,2)
      wsig = masses(2,2)

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wsig2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wpi2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wnu2 -wsig2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wsig2 -wnu2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wsig2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wnu2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsig2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wnu)

      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wnu)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(5,5))
          w(ityp,ihel)    =(0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing nucleon
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wnu)

        if(coupl(5,5,1)%idiag == 1) then   ! nucleon exchange
          call dss_b(v_hel(:,1),coupl(5,5,1),ecm)
        end if

        if(coupl(5,5,2)%idiag == 1) then   ! sigma exchange
          call dss_c(v_hel(:,2),coupl(5,5,2),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(5,5))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do   ! ng-loop

      ! isospin factors
      do ihel = 1,2
        w_iso(1,ihel,1) =   w(1,ihel)  ! I = 1/2  nucleon u
        w_iso(2,ihel,1) =   w(2,ihel)  ! I = 1/2  sigma t
      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(5,5))
        w_lsj(i,1)= w_iso(i,1,1) - w_iso(i,2,1)  ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) + w_iso(i,2,1)  ! l=j+1/2; I=1/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
      end do
      do i = 1,2
        do ityp = 1,itypmax(ccrel(5,5))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_sigN_sigN

  !=======================================================================!

  subroutine dss_b(hel,pc,ecms)  ! N-exchange in sigma N --> sigma N
      implicit none

      ! output
      complex(kind(0.d0)), dimension(1:2) :: hel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: cc
      integer :: i,ihel
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      enddo
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      e_u   = cdsqrt(pc%wf**2+pu_sq)

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.d0 * e_u )

      call mat_spinor(ux,sf,u1(:,1))
      do i=1,2
        call spinor_bar_spinor(hel(i),u3(:,i),ux)
      end do

      fi=(pc%al1**2-wnu2)/(pc%al1**2+pu_sq)  ! sigmaNN vertex
      ff=(pc%al1**2-wnu2)/(pc%al1**2+pu_sq)  ! sigmaNN vertex
      fi=ff*fi
      ff=fi**pc%npow

      cc = ff * pc%gc

      do i=1,2
        hel(i)=hel(i)*cc
      end do

      return
  end subroutine dss_b

  !=======================================================================!

  subroutine dss_c(hel,pc,ecms)  ! sigma-exchange in sigma N --> sigma N
      implicit none

      ! output
      complex(kind(0.d0)), dimension(1:2) :: hel
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_t(0:3),p_tsq,omega_t
      complex(kind(0.d0)) :: cc,propa
      complex(kind(0.d0)),dimension(1:4) :: ux
      integer :: i,ihel
      complex(kind(0.d0)) :: fi,ff   ! form factor

      do i=1,3
        p_t(i)=p1_cm(i)-p3_cm(i)
      end do

      p_tsq=p_t(1)**2+p_t(2)**2+p_t(3)**2   ! 3 momentum squared

      omega_t=cdsqrt(p_tsq+pc%wf**2)

      p_t(0)=omega_t

      propa  = ( 1.d0 / ( ecms - omega_t - p1_cm(0) -p4_cm(0) )&
          &+ 1.d0 / ( ecms - omega_t - p2_cm(0) -p3_cm(0) ) ) / ( 2.d0 * omega_t )

      do i=1,2
        call spinor_bar_spinor(hel(i),u3(:,i),u1(:,1))
      end do

      ff=(pc%al1**2-pc%aw**2)/(pc%al1**2+p_tsq)   ! sigmaNN
      fi=(pc%al2**2-pc%aw**2)/(pc%al2**2+p_tsq)   ! triple sigma
      fi=ff*fi
      ff=fi**pc%npow

      cc =  ff * pc%gc * propa

      do i=1,2
        hel(i)=hel(i)*cc
      end do

      return
  end subroutine dss_c


  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! piN -> omegaN potential
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine v_piN_omegaN(v,pf_cm,pi_cm,ecm)
      implicit none
      complex(kind(0.d0)) :: v(36)            ! output
      !
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm            ! input

      !
      type(couplings) :: pc,pc2

      !     normalization of spinors and pions
      complex(kind(0.d0)) :: cnorm

      !     t-matrices
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,1:11)
      complex(kind(0.d0)) :: w(11,2,-1:1)
      complex(kind(0.d0)) :: w_iso(11,2,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,2)


      integer :: i,ityp,ihel,ng,lam,lad,iso,isoindx


      !
      !------------------------------------------------------------
      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wom = masses(4,2)
      wom2 = masses(4,2)*masses(4,2)


      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wpi2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wpi2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wnu2 -wom2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wom2 -wnu2 )/(2.d0*ecm)
      !   momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wpi2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm
      !
      !   energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wnu2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wom2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wnu)
      !
      !   angle-independent quantities
      !
      cnorm =   cdsqrt( (p1_cm(0)+wnu)/p1_cm(0)&
          & *(p3_cm(0)+wnu)/p3_cm(0)&
          &/ (p4_cm(0) *    p2_cm(0)) )/(4.d0*tpi3)

      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)
      !
      do ihel = 1,2
        do lam=-1,1
          do ityp = 1,itypmax(ccrel(1,6))
            w(ityp,ihel,lam) = (0.d0, 0.d0)
            v_hel(ihel,lam,ityp)=(0.d0,0.d0)
          enddo
        enddo
      enddo
      !
      do ng = 1,nt
        cs_cm   = xg ( ng )

        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )
        !
        !   momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        enddo
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wnu)

        !
        ! call the epsvec_mp_star, which we need in ALL dr's
        call epsvec_mp_star(eps_mp_star,pcmf,wom,sn_cm,cs_cm)

        if(coupl(1,6,1)%idiag == 1) then
          call do_b(v_hel(:,:,1),coupl(1,6,1),ecm) !N-u
        endif
        if(coupl(1,6,2)%idiag == 1) then
          call do_c(v_hel(:,:,2),coupl(1,6,2),ecm) !rho-t
        endif


        ! do the cos theta integration:
        do lam=-1,1
          do ihel = 1,2
            lad=3-2*ihel-2*lam
            if (abs(lad)<=j1) then
              do ityp = 1,itypmax(ccrel(1,6))
                ! which need two sets of parameters
                w(ityp,ihel,lam)=w(ityp,ihel,lam)&
                    &+djw(ng,lad,1)*v_hel(ihel,lam,ityp)
                ! this is 2*(la_N-la_rho)
                ! the initial Npi state has |1/2,0>
              enddo
            end if
          enddo
        enddo

      enddo ! of ng loop

      !
      !  isospin factors
      !
      do ihel = 1,2
        do lam=-1,1
          w_iso(1,ihel,lam,1) =   w(1,ihel,lam)*dsqrt(3.d0) ! I = 1/2  nucleon u
          w_iso(1,ihel,lam,2) =   (0.d0,0.d0)
          !
          w_iso(2,ihel,lam,1) =   w(2,ihel,lam)*dsqrt(3.d0)! I = 1/2  rho-t
          w_iso(2,ihel,lam,2) =   (0.d0,0.d0)
          !
          !
          w_isosum(ihel,lam,1)=(0.d0,0.d0)
          w_isosum(ihel,lam,2)=(0.d0,0.d0)
          do ityp=1,itypmax(ccrel(1,6))
            ! for tesing give out -v
            w_isosum(ihel,lam,1)=w_isosum(ihel,lam,1)+w_iso(ityp,ihel,lam,1)*cnorm
            w_isosum(ihel,lam,2)=w_isosum(ihel,lam,2)+w_iso(ityp,ihel,lam,2)*cnorm
          enddo
        end do
      end do
      ! trafo to jls
      do iso=1,2
        isoindx=2*iso-2

        ! transform to JLS
        v(1+isoindx) = 2.d0*(a(2,-1)*w_isosum(1,0,iso)-a(2,-1)*w_isosum(2,0,i&
            &so)+a(1,-1)*w_isosum(2,-1,iso)-a(1,-1)*w_isosum(1,1,iso))*d(1,-1)
        v(2+isoindx) = 2.d0*(a(2,1)*w_isosum(1,0,iso)+a(2,1)*w_isosum(2,0,iso&
            &)+a(1,1)*w_isosum(2,-1,iso)+a(1,1)*w_isosum(1,1,iso))*d(1,1)
        v(5+isoindx) = 2.d0*(c(3,-1)*w_isosum(1,0,iso)-c(3,-1)*w_isosum(2,0,i&
            &so)+c(2,-1)*w_isosum(2,-1,iso)-c(2,-1)*w_isosum(1,1,iso)+c(1,-1)*w&
            &_isosum(1,-1,iso)-c(1,-1)*w_isosum(2,1,iso))*d(1,-1)
        v(6+isoindx) = 2.d0*(c(3,1)*w_isosum(1,0,iso)+c(3,1)*w_isosum(2,0,iso&
            &)+c(2,1)*w_isosum(2,-1,iso)+c(2,1)*w_isosum(1,1,iso)+c(1,1)*w_isos&
            &um(1,-1,iso)+c(1,1)*w_isosum(2,1,iso))*d(1,1)
        v(9+isoindx) = 2.d0*(b(3,1)*w_isosum(1,0,iso)-b(3,1)*w_isosum(2,0,iso&
            &)+b(2,1)*w_isosum(2,-1,iso)-b(2,1)*w_isosum(1,1,iso)+b(1,1)*w_isos&
            &um(1,-1,iso)-b(1,1)*w_isosum(2,1,iso))*d(1,-1)
        v(10+isoindx) = 2.d0*(b(3,-1)*w_isosum(1,0,iso)+b(3,-1)*w_isosum(2,0,&
            &iso)+b(2,-1)*w_isosum(2,-1,iso)+b(2,-1)*w_isosum(1,1,iso)+b(1,-1)*&
            &w_isosum(1,-1,iso)+b(1,-1)*w_isosum(2,1,iso))*d(1,1)
      enddo

  end subroutine v_piN_omegaN

  !=======================================================================!

  subroutine do_b(hel,pc,ecms)
      implicit none
      ! nucleon pole diagram exchange
      complex(kind(0.d0)) :: hel(1:2,-1:1)            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)), dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)) :: g_rho_4(1:4,1:4,-1:1)
      complex(kind(0.d0)) :: g_pv_2( 1:4, 1:4 )
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer :: i,lam
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff,fff         ! form factor
      ! for testing
      integer :: inhel

      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      enddo
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm

      e_u   = cdsqrt(pc%wf**2+pu_sq)

      !      p_u(0)=eps1-eps4    ! on-energy shell
      !      p_u(0)=p1_cm(0)-p4_cm(0)
      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)

      !      p_u(0)=eps1-eps4    ! on-energy shell
      !      p_u(0)=p1_cm(0)-p4_cm(0)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) )&
          &/( 2.d0 * e_u )

      call vertex_pv(g_pv_2,p2_cm )

      call vertex_rhoNN2(g_rho_4,p4_cm,eps_mp_star,wnu,pc%ka)

      do lam=-1,1
        call mat_spinor(ux,g_rho_4(:,:,lam),u1(:,1))
        call mat_spinor(uy,sf,ux)
        call mat_spinor(ux,g_pv_2,uy)
        do i=1,2
          call spinor_bar_spinor(hel(i,lam),u3(:,i),ux)
        enddo
      enddo

      !
      ! We are using the formfactors as Kanzo uses, which means: there is no energy dependence
      ! in due to the gauge invariance problem by coupling the photon
      aux=(wnu2-wpi2)/wnu
      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2-aux**2+pu_sq))**pc%npow !piNN
      ff=((pc%al2**2-pc%aw**2)/(pc%al2**2+pu_sq))**pc%npow !omegaNN
      fff=fi*ff
      !
      cc = fff * pc%gc

      do lam=-1,1
        do i=1,2
          hel(i,lam)=hel(i,lam)*cc
        enddo
      enddo
      return
  end subroutine do_b

  !=======================================================================!

  subroutine do_c(hel,pc,ecms)
      implicit none
      ! rho exchange
      complex(kind(0.d0)) :: hel(1:2,-1:1)            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: shel(2)
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: pt_sq
      complex(kind(0.d0)) :: e_t
      complex(kind(0.d0)) :: gpro (0:3,-1:1)
      complex(kind(0.d0)),dimension(1:4, 1:4, 0:3) :: grnn1,grnn2
      complex(kind(0.d0)),dimension(1:4) :: ux,uy
      complex(kind(0.d0)),dimension(0:3,1:2) :: shel1,shel2
      complex(kind(0.d0)) :: cc
      integer :: i,lam,mu
      complex(kind(0.d0)) :: fi,ff            ! form factor
      !
      do i=1,3
        p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
        p_t2 ( i ) = p1_cm ( i ) - p3_cm ( i )
      enddo
      pt_sq = p_t1(1)**2 + p_t1(3)**2     ! cm

      e_t   = cdsqrt(pc%wf**2+pt_sq)

      !      p_t1(0)=eps1-eps3    ! on-energy shell
      !      p_t2(0)=eps1-eps3    ! on-energy shell
      p_t1(0)=e_t
      p_t2(0)=-e_t



      ! calculate the rhoNN vertexfunction
      call vertex_rhoNN(grnn1,p_t1,wnu,pc%ka)
      call vertex_rhoNN(grnn2,p_t2,wnu,pc%ka)


      ! call pirhoomega vertex
      call vertex_pirhoomega(gpro,p4_cm,p2_cm,eps_mp_star)

      ff=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow !rhoNN
      fi=((pc%al2**2)/(pc%al2**2+pt_sq))**pc%npow !pirhoomega

      cc = ff * fi * pc%gc/(2.d0*e_t)

      do mu=0,3
        call mat_spinor(ux,grnn1(:,:,mu),u1(:,1))
        call mat_spinor(uy,grnn2(:,:,mu),u1(:,1))

        do i=1,2
          call spinor_bar_spinor(shel1(mu,i),u3(:,i),ux)
          call spinor_bar_spinor(shel2(mu,i),u3(:,i),uy)
        enddo
      enddo

      do i=1,2
        do lam=-1,1
          hel(i,lam)=(0.d0,0.d0)
          do mu=0,3
            hel(i,lam)=hel(i,lam)+(shel1(mu,i)*gpro(mu,lam)/(ecms-e_t-p3_cm(0)-p2_cm(0))&
                &+shel2(mu,i)*gpro(mu,lam)/(ecms-e_t-p1_cm(0)-p4_cm(0)))*cc
          enddo
        enddo
      enddo

      return
  end subroutine do_c

  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! omegaN -> omegaN potential
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine v_omegaN_omegaN(v,pf_cm,pi_cm,ecm)
      implicit none
      complex(kind(0.d0)) :: v(36)            ! output
      !
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm            ! input

      !
      type(couplings) :: pc,pc2

      !     normalization of spinors and pions
      complex(kind(0.d0)) :: cnorm

      !     t-matrices
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,-1:1,1:9)
      complex(kind(0.d0)) :: w(9,2,-1:1,-1:1)
      complex(kind(0.d0)) :: w_iso(9,2,-1:1,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,-1:1,2)

      integer :: i,ityp,ihel,ng,lam1,lam2,lad1,lad2,iso,isoindx


      !
      !------------------------------------------------------------
      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wom = masses(4,2)
      wom2 = masses(4,2)*masses(4,2)


      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wom2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wom2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wnu2 -wom2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wom2 -wnu2 )/(2.d0*ecm)
      !   momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wom2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm
      !
      !   energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wnu2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wom2 )


      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wnu)
      !
      !   angle-independent quantities
      !
      cnorm =   cdsqrt( (p1_cm(0)+wnu)/p1_cm(0)&
          & *(p3_cm(0)+wnu)/p3_cm(0)&
          &/ (p4_cm(0) *    p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)
      !
      ! call eps for the incoming rho
      call epsvec_mp(eps_mp,pcmi,wom,0.d0,1.d0)
      !
      do ihel = 1,2
        do lam1=-1,1
          do lam2=-1,1
            do ityp = 1,itypmax(ccrel(6,6))
              w(ityp,ihel,lam1,lam2) = (0.d0, 0.d0)
              v_hel(ihel,lam1,lam2,ityp)=(0.d0,0.d0)
            enddo
          enddo
        enddo
      enddo
      !
      do ng = 1,nt
        cs_cm   = xg ( ng )

        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )
        !
        !   momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        enddo
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wnu)

        ! call eps* for the outgoing rho
        call epsvec_mp_star(eps_mp_star,pcmf,wom,sn_cm,cs_cm)
        !
        if(coupl(6,6,1)%idiag == 1) then
          call doo_b(v_hel(:,:,:,1),coupl(6,6,1),ecm)
        endif

        ! the v_hel's from the subroutine have the following notation:
        ! v_hel(ihel,lam2,lam1,ityp)
        !       !    !    !
        !       !    !    rho in
        !       !    rho out
        !       N out

        ! do the cos theta integration:
        do lam1=-1,1
          do lam2=-1,1
            do ihel = 1,2
              lad2=3-2*ihel-2*lam2
              lad1=1-2*lam1
              if ((abs(lad1)<=j1).and.(abs(lad2)<=j1)) then
                do ityp = 1,itypmax(ccrel(6,6))
                  ! which need two sets of parameters
                  ! we switch the notation here to have the final state helicities first
                  w(ityp,ihel,lam2,lam1)=w(ityp,ihel,lam2,lam1)&
                      &+djw(ng,lad2,lad1)*v_hel(ihel,lam2,lam1,ityp)
                  ! this is 2*(la_N-la_rho)
                  ! the initial Npi state has |1/2,0>
                enddo
              endif
            enddo
          enddo
        enddo

      enddo ! of ng loop

      !
      !
      !  isospin factors
      !
      do ihel = 1,2
        do lam1=-1,1
          do lam2=-1,1
            w_iso(1,ihel,lam2,lam1,1) =   w(1,ihel,lam2,lam1)        ! I = 1/2  nucleon u
            w_iso(1,ihel,lam2,lam1,2) =   0.d0                      ! I = 3/2

            w_isosum(ihel,lam2,lam1,1)=(0.d0,0.d0)
            w_isosum(ihel,lam2,lam1,2)=(0.d0,0.d0)
            do ityp=1,itypmax(ccrel(6,6))
              w_isosum(ihel,lam2,lam1,1)=w_isosum(ihel,lam2,lam1,1)+w_iso(ityp,ihel,lam2,lam1,1)*cnorm
              w_isosum(ihel,lam2,lam1,2)=w_isosum(ihel,lam2,lam1,2)+w_iso(ityp,ihel,lam2,lam1,2)*cnorm
            enddo     !    !    ! rho in
          end do       !    ! rho out
        end do       ! N out
      end do

      ! transformation in JLS basis:
      do iso=1,2
        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*a(2,-1)**2*w_isosum(1,0,0,iso)-2.d0*a(2,-1)**2*w_iso&
            &sum(2,0,0,iso)+2.d0*a(2,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*a(2,-1)*a(&
            &1,-1)*w_isosum(1,1,0,iso)+2.d0*a(1,-1)*a(2,-1)*w_isosum(2,0,1,iso)-2.d0*&
            &a(1,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)**2*w_isosum(1,1,1,is&
            &o)-2.d0*a(1,-1)**2*w_isosum(2,-1,1,iso)
        v(2+isoindx) = 2.d0*a(2,1)**2*w_isosum(1,0,0,iso)+2.d0*a(2,1)**2*w_isosu&
            &m(2,0,0,iso)+2.d0*a(2,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)*a(1,1)*&
            &w_isosum(1,1,0,iso)+2.d0*a(1,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0*a(1,1)*a&
            &(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)**2*w_isosum(1,1,1,iso)+2.d0*a(1,1)&
            &**2*w_isosum(2,-1,1,iso)
        v(5+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(3&
            &,-1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*&
            &a(2,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*c(1,-1)*w_isosum(1,-&
            &1,0,iso)-2.d0*a(2,-1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*c(3,-1)*w&
            &_isosum(2,0,1,iso)-2.d0*a(1,-1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)&
            &*c(2,-1)*w_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso&
            &)+2.d0*a(1,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*c(1,-1)*w_isosum&
            &(1,-1,1,iso)
        v(6+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1)&
            &*w_isosum(2,0,0,iso)+2.d0*a(2,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)&
            &*c(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0&
            &*a(2,1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*c(3,1)*w_isosum(2,0,1,&
            &iso)+2.d0*a(1,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(&
            &1,1,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1)*c(1,1)*w_&
            &isosum(2,1,1,iso)+2.d0*a(1,1)*c(1,1)*w_isosum(1,-1,1,iso)
        v(9+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3,&
            &1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*a(2&
            &,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*b(1,1)*w_isosum(1,-1,0,i&
            &so)-2.d0*a(2,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*b(3,1)*w_isosum&
            &(2,0,1,iso)-2.d0*a(1,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*&
            &w_isosum(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,-1&
            &)*b(1,1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*b(1,1)*w_isosum(1,-1,1,iso)
        v(10+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*a(2,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*a(&
            &2,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*a(1,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*a(1,1)*b(1,-1)*w_isosum(1,-1,1,iso&
            &)
        v(13+isoindx) = 2.d0*c(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*c(2,-1)**2*w_is&
            &osum(1,1,1,iso)+2.d0*c(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*c(1,-1)*c(2,-&
            &1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*&
            &c(2,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*c(3,-1)*c(1,-1)*w_isosum(2,1&
            &,0,iso)+2.d0*c(3,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*c(1,-1)*c(2,-1)*w&
            &_isosum(1,1,-1,iso)-2.d0*c(2,-1)*c(1,-1)*w_isosum(1,-1,1,iso)-2.d0*c(2,-&
            &1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*c(1,-1)*w_isosum(1,-1,0,i&
            &so)-2.d0*c(3,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(1,-1)*c(3,-1)*w_isos&
            &um(1,0,-1,iso)-2.d0*c(1,-1)**2*w_isosum(2,1,-1,iso)-2.d0*c(3,-1)**2*w_is&
            &osum(2,0,0,iso)-2.d0*c(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,-1)*c(3,-1&
            &)*w_isosum(2,0,1,iso)
        v(14+isoindx) = 2.d0*c(3,1)**2*w_isosum(1,0,0,iso)+2.d0*c(3,1)**2*w_isos&
            &um(2,0,0,iso)+2.d0*c(3,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1)*c(2,1)&
            &*w_isosum(1,1,0,iso)+2.d0*c(3,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0*c(3,1)&
            &*c(1,1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*c(3,1)*w_isosum(2,0,1,iso)+2.d0*&
            &c(2,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)**2*w_isosum(1,1,1,iso)+&
            &2.d0*c(2,1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,1)*c(1,1)*w_isosum(2,1,1,is&
            &o)+2.d0*c(2,1)*c(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(1&
            &,0,-1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*c(2,1)*w_&
            &isosum(2,-1,-1,iso)+2.d0*c(1,1)*c(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*&
            &*2*w_isosum(1,-1,-1,iso)+2.d0*c(1,1)**2*w_isosum(2,1,-1,iso)
        v(17+isoindx) = 2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,iso)-2.d0*c(1,-1)*b&
            &(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,-1)*b(3,1)*w_isosum(1,0,-1,iso)-2.d0&
            &*c(2,-1)*b(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(2,-1)*b(2,1)*w_isosum(1,1&
            &,1,iso)-2.d0*c(2,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*b(1,1)*w_is&
            &osum(1,-1,0,iso)-2.d0*c(3,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(3,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*c(&
            &3,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,&
            &iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)+2.d0*c(1,-1)*b(2,1)*w_isos&
            &um(2,-1,-1,iso)-2.d0*c(1,-1)*b(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(2,-1)*b(&
            &1,1)*w_isosum(2,1,1,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(3,1)*w_isosum(2,0,1,iso)
        v(18+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*c(&
            &3,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*c(2,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(2,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*c(2,1)*b(1,-1)*w_isosum(1,-1,1,iso&
            &)+2.d0*c(1,1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*c(1,1)*b(3,-1)*w_isosum(&
            &2,0,-1,iso)+2.d0*c(1,1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*b(2,-1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)
        v(21+isoindx) = 2.d0*b(1,1)*b(2,1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*b(3&
            &,1)*w_isosum(2,0,-1,iso)+2.d0*b(2,1)*b(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2&
            &,1)*b(3,1)*w_isosum(2,0,1,iso)-2.d0*b(3,1)*b(1,1)*w_isosum(2,1,0,iso)&
            &+2.d0*b(3,1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*b(2,1)**2*w_isosum(2,-1,1,&
            &iso)-2.d0*b(1,1)**2*w_isosum(2,1,-1,iso)+2.d0*b(3,1)*b(1,1)*w_isosum(1,-&
            &1,0,iso)-2.d0*b(1,1)*b(2,1)*w_isosum(1,1,-1,iso)+2.d0*b(1,1)*b(3,1)*w_is&
            &osum(1,0,-1,iso)-2.d0*b(2,1)*b(1,1)*w_isosum(1,-1,1,iso)-2.d0*b(3,1)**2*&
            &w_isosum(2,0,0,iso)-2.d0*b(2,1)*b(3,1)*w_isosum(1,0,1,iso)-2.d0*b(3,1)*b&
            &(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,1)**2*w_isosum(1,0,0,iso)+2.d0*b(2,1)&
            &**2*w_isosum(1,1,1,iso)+2.d0*b(1,1)**2*w_isosum(1,-1,-1,iso)
        v(22+isoindx) = 2.d0*b(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*b(3,-1)**2*w_is&
            &osum(2,0,0,iso)+2.d0*b(3,-1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*b(3,-1)*b&
            &(2,-1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*b(1,-1)*w_isosum(1,-1,0,iso)+&
            &2.d0*b(3,-1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(2&
            &,0,1,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,-1)**2*w_iso&
            &sum(1,1,1,iso)+2.d0*b(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*b(2,-1)*b(1,-1)&
            &*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*b(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(1,&
            &-1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*b(3,-1)*w_isosum(2,0,-1&
            &,iso)+2.d0*b(1,-1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*b(2,-1)*w_&
            &isosum(1,1,-1,iso)+2.d0*b(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*b(1,-1)**2&
            &*w_isosum(2,1,-1,iso)
        !
        v(25+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(&
            &3,-1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0&
            &*c(3,-1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*c(2,-1)*a(2,-1)*w_isosum(2,&
            &0,1,iso)-2.d0*c(2,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*c(2,-1)*w&
            &_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(1,-1&
            &)*a(2,-1)*w_isosum(1,0,-1,iso)-2.d0*c(1,-1)*a(2,-1)*w_isosum(2,0,-1,i&
            &so)+2.d0*c(1,-1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*a(1,-1)*w_is&
            &osum(1,1,-1,iso)
        v(26+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1&
            &)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1&
            &)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*c(2,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0&
            &*c(2,1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(1,1,1,&
            &iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum&
            &(1,0,-1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*a(1,1)*&
            &w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*a(1,1)*w_isosum(1,1,-1,iso)
        v(29+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3&
            &,1)*w_isosum(2,0,0,iso)+2.d0*b(3,1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*b(&
            &3,1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*b(2,1)*a(2,-1)*w_isosum(2,0,1,i&
            &so)-2.d0*b(2,1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*w_isosum&
            &(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*b(1,1)*a(2,-1)&
            &*w_isosum(1,0,-1,iso)-2.d0*b(1,1)*a(2,-1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*a(1,-1)*w_isosum(1,1,-1,&
            &iso)
        v(30+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*b(2,-1)*a(2,1)*w_isosum(2,0,1,i&
            &so)+2.d0*b(2,-1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum&
            &(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(1,-1)*a(2,1)&
            &*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &-1)*a(1,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*a(1,1)*w_isosum(1,1,-1,&
            &iso)
        v(33+isoindx) = -2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(2,1)*w_isosum(1,1,1,iso)+2.d0*b(1,1)*c(3,-1)*w_isosum(1,0,-1&
            &,iso)-2.d0*b(2,1)*c(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(2,1)*c(1,-1)*w_iso&
            &sum(2,1,1,iso)-2.d0*b(2,1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,1)*c(3,-&
            &1)*w_isosum(2,0,1,iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)-2.d0*b(3&
            &,1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,&
            &iso)+2.d0*b(3,1)*c(1,-1)*w_isosum(1,-1,0,iso)-2.d0*b(3,1)*c(2,-1)*w_isos&
            &um(1,1,0,iso)+2.d0*b(3,1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*b(1,1)*c(2,-&
            &1)*w_isosum(1,1,-1,iso)-2.d0*b(1,1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*b(&
            &1,1)*c(2,-1)*w_isosum(2,-1,-1,iso)
        v(34+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*c(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(1,-1,0,&
            &iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*c(3,1)*w_isosu&
            &m(2,0,1,iso)+2.d0*b(2,-1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(2,-&
            &1)*c(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*c(1,1)*w_isosum(1,-1,1,iso&
            &)+2.d0*b(1,-1)*c(3,1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*c(3,1)*w_isosum(&
            &2,0,-1,iso)+2.d0*b(1,-1)*c(2,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*c(2,1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)


      enddo ! iso loop

  end subroutine v_omegaN_omegaN

  !=======================================================================!

  subroutine doo_b(hel,pc,ecms)
      implicit none
      ! nucleon pole diagram exchange
      complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)),dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)),dimension(1:4,1:4,-1:1) :: g_rho_4,g_rho_2
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer :: i,lam1,lam2
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff            ! form factor

      !      p_u(0)=eps1-eps4    ! on-energy shell
      p_u(0)=p1_cm(0)-p4_cm(0)    ! on-mass shell
      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      enddo
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      psq   = p_u(0)**2 - pu_sq         ! four vector squared
      e_u   = cdsqrt(pc%wf**2+pu_sq)

      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) )&
          &/( 2.0 * e_u )


      call vertex_rhoNN2(g_rho_2,-1.d0*p2_cm,eps_mp,wnu,pc%ka)

      call vertex_rhoNN2(g_rho_4,p4_cm,eps_mp_star,wnu,pc%ka)



      do lam2=-1,1
        call mat_spinor(ux,g_rho_4(:,:,lam2),u1(:,1))
        call mat_spinor(uy,sf,ux)
        do lam1=-1,1
          call mat_spinor(ux,g_rho_2(:,:,lam1),uy)
          do i=1,2
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          enddo
        enddo
      enddo
      !
      ! We are using the formfactors as Kanzo uses, which means: there is no energy dependence
      ! in due to the gauge invariance problem by coupling the photon
      !       aux=(wnu2-wrho2)/wnu
      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq))**pc%npow
      ff=fi*fi
      cc = ff * pc%gc
      do lam1=-1,1
        do lam2=-1,1
          do i=1,2
            hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
          enddo
        enddo
      enddo
      return
  end subroutine doo_b

  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! rhoN -> omegaN potential
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  subroutine v_rhoN_omegaN(v,pf_cm,pi_cm,ecm)
      implicit none
      complex(kind(0.d0)) :: v(36)            ! output
      !
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm            ! input

      !
      type(couplings) :: pc,pc2

      !     normalization of spinors and pions
      complex(kind(0.d0)) :: cnorm

      !     t-matrices
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,-1:1,1:9)
      complex(kind(0.d0)) :: w(9,2,-1:1,-1:1)
      complex(kind(0.d0)) :: w_iso(9,2,-1:1,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,-1:1,2)

      integer :: i,ityp,ihel,ng,lam1,lam2,lad1,lad2,iso,isoindx


      !
      !------------------------------------------------------------
      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wom = masses(4,2)
      wom2 = masses(4,2)*masses(4,2)
      wrho = masses(3,2)
      wrho2 = masses(3,2)*masses(3,2)


      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wrho2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wrho2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wnu2 -wom2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wom2 -wnu2 )/(2.d0*ecm)
      !   momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wrho2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm
      !
      !   energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wnu2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wom2 )


      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wnu)
      !
      !   angle-independent quantities
      !
      cnorm =   cdsqrt( (p1_cm(0)+wnu)/p1_cm(0)&
          & *(p3_cm(0)+wnu)/p3_cm(0)&
          &/ (p4_cm(0) *    p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)
      !
      ! call eps for the incoming rho
      call epsvec_mp(eps_mp,pcmi,wrho,0.d0,1.d0)
      !
      do ihel = 1,2
        do lam1=-1,1
          do lam2=-1,1
            do ityp = 1,itypmax(ccrel(2,6))
              w(ityp,ihel,lam1,lam2) = (0.d0, 0.d0)
              v_hel(ihel,lam1,lam2,ityp)=(0.d0,0.d0)
            enddo
          enddo
        enddo
      enddo
      !
      do ng = 1,nt
        cs_cm   = xg ( ng )

        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )
        !
        !   momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        enddo
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wnu)

        ! call eps* for the outgoing rho
        call epsvec_mp_star(eps_mp_star,pcmf,wom,sn_cm,cs_cm)
        !
        if(coupl(2,6,1)%idiag == 1) then
          call dro_b(v_hel(:,:,:,1),coupl(2,6,1),ecm) !N-u
        endif
        if(coupl(2,6,2)%idiag == 1) then
          call dro_c(v_hel(:,:,:,2),coupl(2,6,2),ecm) !pi-t
        endif

        ! the v_hel's from the subroutine have the following notation:
        ! v_hel(ihel,lam2,lam1,ityp)
        !       !    !    !
        !       !    !    rho in
        !       !    rho out
        !       N out

        ! do the cos theta integration:
        do lam1=-1,1
          do lam2=-1,1
            do ihel = 1,2
              lad2=3-2*ihel-2*lam2
              lad1=1-2*lam1
              if ((abs(lad1)<=j1).and.(abs(lad2)<=j1)) then
                do ityp = 1,itypmax(ccrel(2,6))
                  ! which need two sets of parameters
                  ! we switch the notation here to have the final state helicities first
                  w(ityp,ihel,lam2,lam1)=w(ityp,ihel,lam2,lam1)&
                      &+djw(ng,lad2,lad1)*v_hel(ihel,lam2,lam1,ityp)
                  ! this is 2*(la_N-la_rho)
                  ! the initial Npi state has |1/2,0>
                enddo
              endif
            enddo
          enddo
        enddo

      enddo ! of ng loop

      !
      !
      !  isospin factors
      !
      do ihel = 1,2
        do lam1=-1,1
          do lam2=-1,1
            w_iso(1,ihel,lam2,lam1,1) =   w(1,ihel,lam2,lam1)*dsqrt(3.d0)        ! I = 1/2  nucleon u
            w_iso(1,ihel,lam2,lam1,2) =   0.d0                      ! I = 3/2

            w_iso(2,ihel,lam2,lam1,1) =   w(2,ihel,lam2,lam1)*dsqrt(3.d0)        ! I = 1/2  nucleon u
            w_iso(2,ihel,lam2,lam1,2) =   0.d0                      ! I = 3/2

            w_isosum(ihel,lam2,lam1,1)=(0.d0,0.d0)
            w_isosum(ihel,lam2,lam1,2)=(0.d0,0.d0)
            do ityp=1,itypmax(ccrel(2,6))
              w_isosum(ihel,lam2,lam1,1)=w_isosum(ihel,lam2,lam1,1)+w_iso(ityp,ihel,lam2,lam1,1)*cnorm
              w_isosum(ihel,lam2,lam1,2)=w_isosum(ihel,lam2,lam1,2)+w_iso(ityp,ihel,lam2,lam1,2)*cnorm
            enddo     !    !    ! rho in
          end do       !    ! rho out
        end do       ! N out
      end do

      ! transformation in JLS basis:
      do iso=1,2
        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*a(2,-1)**2*w_isosum(1,0,0,iso)-2.d0*a(2,-1)**2*w_iso&
            &sum(2,0,0,iso)+2.d0*a(2,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*a(2,-1)*a(&
            &1,-1)*w_isosum(1,1,0,iso)+2.d0*a(1,-1)*a(2,-1)*w_isosum(2,0,1,iso)-2.d0*&
            &a(1,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)**2*w_isosum(1,1,1,is&
            &o)-2.d0*a(1,-1)**2*w_isosum(2,-1,1,iso)
        v(2+isoindx) = 2.d0*a(2,1)**2*w_isosum(1,0,0,iso)+2.d0*a(2,1)**2*w_isosu&
            &m(2,0,0,iso)+2.d0*a(2,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)*a(1,1)*&
            &w_isosum(1,1,0,iso)+2.d0*a(1,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0*a(1,1)*a&
            &(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)**2*w_isosum(1,1,1,iso)+2.d0*a(1,1)&
            &**2*w_isosum(2,-1,1,iso)
        v(5+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(3&
            &,-1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*&
            &a(2,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*c(1,-1)*w_isosum(1,-&
            &1,0,iso)-2.d0*a(2,-1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*c(3,-1)*w&
            &_isosum(2,0,1,iso)-2.d0*a(1,-1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)&
            &*c(2,-1)*w_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso&
            &)+2.d0*a(1,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*c(1,-1)*w_isosum&
            &(1,-1,1,iso)
        v(6+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1)&
            &*w_isosum(2,0,0,iso)+2.d0*a(2,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)&
            &*c(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0&
            &*a(2,1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*c(3,1)*w_isosum(2,0,1,&
            &iso)+2.d0*a(1,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(&
            &1,1,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1)*c(1,1)*w_&
            &isosum(2,1,1,iso)+2.d0*a(1,1)*c(1,1)*w_isosum(1,-1,1,iso)
        v(9+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3,&
            &1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*a(2&
            &,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*b(1,1)*w_isosum(1,-1,0,i&
            &so)-2.d0*a(2,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*b(3,1)*w_isosum&
            &(2,0,1,iso)-2.d0*a(1,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*&
            &w_isosum(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,-1&
            &)*b(1,1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*b(1,1)*w_isosum(1,-1,1,iso)
        v(10+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*a(2,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*a(&
            &2,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*a(1,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*a(1,1)*b(1,-1)*w_isosum(1,-1,1,iso&
            &)
        v(13+isoindx) = 2.d0*c(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*c(2,-1)**2*w_is&
            &osum(1,1,1,iso)+2.d0*c(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*c(1,-1)*c(2,-&
            &1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*&
            &c(2,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*c(3,-1)*c(1,-1)*w_isosum(2,1&
            &,0,iso)+2.d0*c(3,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*c(1,-1)*c(2,-1)*w&
            &_isosum(1,1,-1,iso)-2.d0*c(2,-1)*c(1,-1)*w_isosum(1,-1,1,iso)-2.d0*c(2,-&
            &1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*c(1,-1)*w_isosum(1,-1,0,i&
            &so)-2.d0*c(3,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(1,-1)*c(3,-1)*w_isos&
            &um(1,0,-1,iso)-2.d0*c(1,-1)**2*w_isosum(2,1,-1,iso)-2.d0*c(3,-1)**2*w_is&
            &osum(2,0,0,iso)-2.d0*c(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,-1)*c(3,-1&
            &)*w_isosum(2,0,1,iso)
        v(14+isoindx) = 2.d0*c(3,1)**2*w_isosum(1,0,0,iso)+2.d0*c(3,1)**2*w_isos&
            &um(2,0,0,iso)+2.d0*c(3,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1)*c(2,1)&
            &*w_isosum(1,1,0,iso)+2.d0*c(3,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0*c(3,1)&
            &*c(1,1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*c(3,1)*w_isosum(2,0,1,iso)+2.d0*&
            &c(2,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)**2*w_isosum(1,1,1,iso)+&
            &2.d0*c(2,1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,1)*c(1,1)*w_isosum(2,1,1,is&
            &o)+2.d0*c(2,1)*c(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(1&
            &,0,-1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*c(2,1)*w_&
            &isosum(2,-1,-1,iso)+2.d0*c(1,1)*c(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*&
            &*2*w_isosum(1,-1,-1,iso)+2.d0*c(1,1)**2*w_isosum(2,1,-1,iso)
        v(17+isoindx) = 2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,iso)-2.d0*c(1,-1)*b&
            &(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,-1)*b(3,1)*w_isosum(1,0,-1,iso)-2.d0&
            &*c(2,-1)*b(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(2,-1)*b(2,1)*w_isosum(1,1&
            &,1,iso)-2.d0*c(2,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*b(1,1)*w_is&
            &osum(1,-1,0,iso)-2.d0*c(3,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(3,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*c(&
            &3,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,&
            &iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)+2.d0*c(1,-1)*b(2,1)*w_isos&
            &um(2,-1,-1,iso)-2.d0*c(1,-1)*b(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(2,-1)*b(&
            &1,1)*w_isosum(2,1,1,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(3,1)*w_isosum(2,0,1,iso)
        v(18+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*c(&
            &3,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*c(2,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(2,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*c(2,1)*b(1,-1)*w_isosum(1,-1,1,iso&
            &)+2.d0*c(1,1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*c(1,1)*b(3,-1)*w_isosum(&
            &2,0,-1,iso)+2.d0*c(1,1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*b(2,-1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)
        v(21+isoindx) = 2.d0*b(1,1)*b(2,1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*b(3&
            &,1)*w_isosum(2,0,-1,iso)+2.d0*b(2,1)*b(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2&
            &,1)*b(3,1)*w_isosum(2,0,1,iso)-2.d0*b(3,1)*b(1,1)*w_isosum(2,1,0,iso)&
            &+2.d0*b(3,1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*b(2,1)**2*w_isosum(2,-1,1,&
            &iso)-2.d0*b(1,1)**2*w_isosum(2,1,-1,iso)+2.d0*b(3,1)*b(1,1)*w_isosum(1,-&
            &1,0,iso)-2.d0*b(1,1)*b(2,1)*w_isosum(1,1,-1,iso)+2.d0*b(1,1)*b(3,1)*w_is&
            &osum(1,0,-1,iso)-2.d0*b(2,1)*b(1,1)*w_isosum(1,-1,1,iso)-2.d0*b(3,1)**2*&
            &w_isosum(2,0,0,iso)-2.d0*b(2,1)*b(3,1)*w_isosum(1,0,1,iso)-2.d0*b(3,1)*b&
            &(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,1)**2*w_isosum(1,0,0,iso)+2.d0*b(2,1)&
            &**2*w_isosum(1,1,1,iso)+2.d0*b(1,1)**2*w_isosum(1,-1,-1,iso)
        v(22+isoindx) = 2.d0*b(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*b(3,-1)**2*w_is&
            &osum(2,0,0,iso)+2.d0*b(3,-1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*b(3,-1)*b&
            &(2,-1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*b(1,-1)*w_isosum(1,-1,0,iso)+&
            &2.d0*b(3,-1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(2&
            &,0,1,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,-1)**2*w_iso&
            &sum(1,1,1,iso)+2.d0*b(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*b(2,-1)*b(1,-1)&
            &*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*b(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(1,&
            &-1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*b(3,-1)*w_isosum(2,0,-1&
            &,iso)+2.d0*b(1,-1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*b(2,-1)*w_&
            &isosum(1,1,-1,iso)+2.d0*b(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*b(1,-1)**2&
            &*w_isosum(2,1,-1,iso)
        !
        v(25+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(&
            &3,-1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0&
            &*c(3,-1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*c(2,-1)*a(2,-1)*w_isosum(2,&
            &0,1,iso)-2.d0*c(2,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*c(2,-1)*w&
            &_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(1,-1&
            &)*a(2,-1)*w_isosum(1,0,-1,iso)-2.d0*c(1,-1)*a(2,-1)*w_isosum(2,0,-1,i&
            &so)+2.d0*c(1,-1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*a(1,-1)*w_is&
            &osum(1,1,-1,iso)
        v(26+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1&
            &)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1&
            &)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*c(2,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0&
            &*c(2,1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(1,1,1,&
            &iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum&
            &(1,0,-1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*a(1,1)*&
            &w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*a(1,1)*w_isosum(1,1,-1,iso)
        v(29+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3&
            &,1)*w_isosum(2,0,0,iso)+2.d0*b(3,1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*b(&
            &3,1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*b(2,1)*a(2,-1)*w_isosum(2,0,1,i&
            &so)-2.d0*b(2,1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*w_isosum&
            &(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*b(1,1)*a(2,-1)&
            &*w_isosum(1,0,-1,iso)-2.d0*b(1,1)*a(2,-1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*a(1,-1)*w_isosum(1,1,-1,&
            &iso)
        v(30+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*b(2,-1)*a(2,1)*w_isosum(2,0,1,i&
            &so)+2.d0*b(2,-1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum&
            &(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(1,-1)*a(2,1)&
            &*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &-1)*a(1,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*a(1,1)*w_isosum(1,1,-1,&
            &iso)
        v(33+isoindx) = -2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(2,1)*w_isosum(1,1,1,iso)+2.d0*b(1,1)*c(3,-1)*w_isosum(1,0,-1&
            &,iso)-2.d0*b(2,1)*c(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(2,1)*c(1,-1)*w_iso&
            &sum(2,1,1,iso)-2.d0*b(2,1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,1)*c(3,-&
            &1)*w_isosum(2,0,1,iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)-2.d0*b(3&
            &,1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,&
            &iso)+2.d0*b(3,1)*c(1,-1)*w_isosum(1,-1,0,iso)-2.d0*b(3,1)*c(2,-1)*w_isos&
            &um(1,1,0,iso)+2.d0*b(3,1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*b(1,1)*c(2,-&
            &1)*w_isosum(1,1,-1,iso)-2.d0*b(1,1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*b(&
            &1,1)*c(2,-1)*w_isosum(2,-1,-1,iso)
        v(34+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*c(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(1,-1,0,&
            &iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*c(3,1)*w_isosu&
            &m(2,0,1,iso)+2.d0*b(2,-1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(2,-&
            &1)*c(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*c(1,1)*w_isosum(1,-1,1,iso&
            &)+2.d0*b(1,-1)*c(3,1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*c(3,1)*w_isosum(&
            &2,0,-1,iso)+2.d0*b(1,-1)*c(2,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*c(2,1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)


      enddo ! iso loop

  end subroutine v_rhoN_omegaN

  !=======================================================================!

  subroutine dro_b(hel,pc,ecms)
      implicit none
      ! nucleon pole diagram exchange
      complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_u(0:3)
      complex(kind(0.d0)) :: pu_sq,psq
      complex(kind(0.d0)) :: e_u
      complex(kind(0.d0)) :: propa
      complex(kind(0.d0)),dimension(1:4,1:4) :: sf,sfpos,sfneg
      complex(kind(0.d0)),dimension(1:4,1:4,-1:1) :: g_rho_4,g_rho_2
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer :: i,lam1,lam2
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff            ! form factor

      !      p_u(0)=eps1-eps4    ! on-energy shell
      p_u(0)=p1_cm(0)-p4_cm(0)    ! on-mass shell
      do i=1,3
        p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
      enddo
      pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
      psq   = p_u(0)**2 - pu_sq         ! four vector squared
      e_u   = cdsqrt(pc%wf**2+pu_sq)
      p_u(0)=e_u
      call psl_m(sfpos,p_u,pc%wf)
      p_u(0)=-e_u
      call psl_m(sfneg,p_u,pc%wf)

      sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
          &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) )&
          &/( 2.0 * e_u )

      !rhoNN kappa
      call vertex_rhoNN2(g_rho_2,-1.d0*p2_cm,eps_mp,wnu,pc%ka)

      call vertex_rhoNN2(g_rho_4,p4_cm,eps_mp_star,wnu,pc%ka2)
      !omegaNN kappa


      do lam2=-1,1
        call mat_spinor(ux,g_rho_4(:,:,lam2),u1(:,1))
        call mat_spinor(uy,sf,ux)
        do lam1=-1,1
          call mat_spinor(ux,g_rho_2(:,:,lam1),uy)
          do i=1,2
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          enddo
        enddo
      enddo
      !
      ! We are using the formfactors as Kanzo uses, which means: there is no energy dependence
      ! in due to the gauge invariance problem by coupling the photon
      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq))**pc%npow !rhoNN
      ff=((pc%al2**2-pc%aw**2)/(pc%al2**2+pu_sq))**pc%npow !omegaNN
      ff=fi*ff

      cc = ff * pc%gc
      do lam1=-1,1
        do lam2=-1,1
          do i=1,2
            hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
          enddo
        enddo
      enddo
      return
  end subroutine dro_b

  !=======================================================================!

  subroutine dro_c(hel,pc,ecms)
      implicit none
      ! pion exchange
      complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)            ! output
      ! input:
      type(couplings) :: pc
      complex(kind(0.d0)) ::     ecms
      ! intermediate storage:
      complex(kind(0.d0)) :: p_t1(0:3),p_t2(0:3)
      complex(kind(0.d0)) :: pt_sq
      complex(kind(0.d0)) :: e_t,s1
      complex(kind(0.d0)),dimension(1:4,1:4) :: gpv1,gpv2
      complex(kind(0.d0)),dimension(-1:1,-1:1) :: gpro
      complex(kind(0.d0)),dimension(1:2) :: scal1,scal2
      complex(kind(0.d0)) :: ux(1:4)
      complex(kind(0.d0)) :: uy(1:4)
      complex(kind(0.d0)) :: cc
      integer :: i,lam1,lam2
      complex(kind(0.d0)) :: aux
      complex(kind(0.d0)) :: fi,ff            ! form factor

      do i=1,3
        p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
        p_t2 ( i ) = p_t1(i)
      enddo
      pt_sq = p_t1(1)**2 + p_t1(3)**2     ! cm
      e_t   = cdsqrt(pc%wf**2+pt_sq)

      p_t1(0)=e_t
      p_t2(0)=-e_t


      call vertex_pv(gpv1,p_t1)
      call vertex_pv(gpv2,p_t2)

      call mat_spinor(ux,gpv1,u1(:,1))
      call mat_spinor(uy,gpv2,u1(:,1))

      do i=1,2
        call spinor_bar_spinor(scal1(i),u3(:,i),ux)
        call spinor_bar_spinor(scal2(i),u3(:,i),uy)
      enddo

      ! omegapirho vertex
      ! eps_mu,al,la,nu p2^al * p4^la * eps^mu * eps_star^nu ; eps_0123=-1
      do lam1=-1,1
        do lam2=-1,1
          s1 = p2_cm(3)*p4_cm(1)*eps_mp_star(0,lam2)*eps_mp(2,lam1)+p2_cm(1)&
              &*p4_cm(2)*eps_mp_star(0,lam2)*eps_mp(3,lam1)+p2_cm(2)*p4_cm(3)*eps&
              &_mp_star(0,lam2)*eps_mp(1,lam1)-p2_cm(2)*p4_cm(1)*eps_mp_star(0,la&
              &m2)*eps_mp(3,lam1)-p2_cm(3)*p4_cm(2)*eps_mp_star(0,lam2)*eps_mp(1,&
              &lam1)-p2_cm(1)*p4_cm(3)*eps_mp_star(0,lam2)*eps_mp(2,lam1)+p2_cm(3&
              &)*p4_cm(2)*eps_mp_star(1,lam2)*eps_mp(0,lam1)+p2_cm(0)*p4_cm(3)*ep&
              &s_mp_star(1,lam2)*eps_mp(2,lam1)+p2_cm(2)*p4_cm(0)*eps_mp_star(1,l&
              &am2)*eps_mp(3,lam1)-p2_cm(0)*p4_cm(2)*eps_mp_star(1,lam2)*eps_mp(3&
              &,lam1)-p2_cm(2)*p4_cm(3)*eps_mp_star(1,lam2)*eps_mp(0,lam1)-p2_cm(&
              &3)*p4_cm(0)*eps_mp_star(1,lam2)*eps_mp(2,lam1)
          gpro(lam2,lam1) = s1+p2_cm(1)*p4_cm(3)*eps_mp_star(2,lam2)*eps_mp(0,lam1)+p2_cm&
              &(3)*p4_cm(0)*eps_mp_star(2,lam2)*eps_mp(1,lam1)+p2_cm(0)*p4_cm(1)*&
              &eps_mp_star(2,lam2)*eps_mp(3,lam1)-p2_cm(0)*p4_cm(3)*eps_mp_star(2&
              &,lam2)*eps_mp(1,lam1)-p2_cm(3)*p4_cm(1)*eps_mp_star(2,lam2)*eps_mp&
              &(0,lam1)-p2_cm(1)*p4_cm(0)*eps_mp_star(2,lam2)*eps_mp(3,lam1)+p2_c&
              &m(0)*p4_cm(2)*eps_mp_star(3,lam2)*eps_mp(1,lam1)+p2_cm(2)*p4_cm(1)&
              &*eps_mp_star(3,lam2)*eps_mp(0,lam1)+p2_cm(1)*p4_cm(0)*eps_mp_star(&
              &3,lam2)*eps_mp(2,lam1)-p2_cm(0)*p4_cm(1)*eps_mp_star(3,lam2)*eps_m&
              &p(2,lam1)-p2_cm(1)*p4_cm(2)*eps_mp_star(3,lam2)*eps_mp(0,lam1)-p2_&
              &cm(2)*p4_cm(0)*eps_mp_star(3,lam2)*eps_mp(1,lam1)
        enddo
      enddo


      !
      ! We are using the formfactors as Kanzo uses, which means: there is no energy dependence
      ! in due to the gauge invariance problem by coupling the photon
      !       aux=(wnu2-wrho2)/wnu
      fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow !piNN
      ff=((pc%al2**2)/(pc%al2**2+pt_sq))**pc%npow !pirhoomega
      ff=fi*ff

      cc = - ff * pc%gc/(2.d0*e_t)

      do lam1=-1,1
        do lam2=-1,1
          do i=1,2
            hel(i,lam2,lam1)=(scal1(i)*gpro(lam2,lam1)/(ecms-e_t-p2_cm(0)-p3_cm(0))+&
                &scal2(i)*gpro(lam2,lam1)/(ecms-e_t-p4_cm(0)-p1_cm(0)))*cc
          enddo
        enddo
      enddo
      return
  end subroutine dro_c

  !=======================================================================!
  !  piN -> LamK potential
  !=======================================================================!

  subroutine v_piN_LamK(v,pf_cm,pi_cm,ecm)

      implicit none
      complex(kind(0.d0)) :: v(36)        ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm  ! input
      complex(kind(0.d0)) ::     ecm      ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm        ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng

      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wk = masses(6,2)
      wk2 = masses(6,2)*masses(6,2)
      wlam = masses(3,1)
      wlam2 = wlam*wlam

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wpi2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wpi2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wlam2 -wk2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wk2 -wlam2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wpi2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wlam2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wk2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wlam)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wlam)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(1,7))
          w(ityp,ihel) = (0.d0, 0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt
        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wlam)

        if(coupl(1,7,1)%idiag == 1) then         ! K* - exchange
          call d_ds(v_hel(:,1),coupl(1,7,1),ecm)
        endif

        if(coupl(1,7,2)%idiag == 1) then         ! Sigma - exchange (u-chanel)
          call de_b(v_hel(:,2),coupl(1,7,2),ecm)
        endif

        if(coupl(1,7,3)%idiag == 1) then         ! kappa - exchange
          call d_cs(v_hel(:,3),coupl(1,7,3),ecm,a0_cd)
        endif

        if(coupl(1,7,4)%idiag == 1) then         ! Sigma* - exchange (u-chanel)
          call d_f(v_hel(:,4),coupl(1,7,4),ecm)
        endif

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(1,7))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do  ! ng-loop

      ! isospin factors
      do ihel = 1,2

        w_iso(1,ihel,1) =   w(1,ihel) * dsqrt(3.d0)   ! I = 1/2  K*-exhange
        w_iso(1,ihel,2) =   w(1,ihel) * 0.d0          ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel) * dsqrt(3.d0)   ! I = 1/2  Sigma-exhange
        w_iso(2,ihel,2) =   w(2,ihel) * 0.d0          ! I = 3/2

        w_iso(3,ihel,1) =   w(3,ihel) * dsqrt(3.d0)   ! I = 1/2  kappa-exhange
        w_iso(3,ihel,2) =   w(3,ihel) * 0.d0          ! I = 3/2

        w_iso(4,ihel,1) =   w(4,ihel) * dsqrt(3.d0)   ! I = 1/2  Sigma*-exhange
        w_iso(4,ihel,2) =   w(4,ihel) * 0.d0          ! I = 3/2

      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(1,7))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)  ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)  ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)  ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)  ! l=j+1/2; I=3/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(1,7))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_piN_LamK

  !=======================================================================!
  !  piN -> SigK potential
  !=======================================================================!

  subroutine v_piN_SigK(v,pf_cm,pi_cm,ecm)

      implicit none
      complex(kind(0.d0)) :: v(36)         ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm   ! input
      complex(kind(0.d0)) ::     ecm       ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm         ! normalization of spinors and pions
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng

      ! masses
      wnu2 = masses(1,1)*masses(1,1)
      wpi2 = masses(1,2)*masses(1,2)
      wnu = masses(1,1)
      wpi = masses(1,2)
      wk = masses(6,2)
      wk2 = masses(6,2)*masses(6,2)
      wsig = masses(4,1)
      wsig2 = wsig*wsig

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2 -wpi2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wpi2 -wnu2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wsig2 -wk2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wk2 -wsig2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wpi2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsig2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wk2 )

      ptildei = pcmi/(p1_cm(0)+wnu)
      ptildef = pcmf/(p3_cm(0)+wsig)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wsig)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(1,8))
          w(ityp,ihel) = (0.d0, 0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Sigma
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wsig)

        if(coupl(1,8,1)%idiag == 1) then        ! K* -exchange
          call d_ds(v_hel(:,1),coupl(1,8,1),ecm)
        end if

        if(coupl(1,8,2)%idiag == 1) then        ! Sigma - exchange
          call de_b(v_hel(:,2),coupl(1,8,2),ecm)
        end if

        if(coupl(1,8,3)%idiag == 1) then        ! Lambda - exchange
          call de_b(v_hel(:,3),coupl(1,8,3),ecm)
        end if

        if(coupl(1,8,4)%idiag == 1) then        ! kappa - exchange
          call d_cs(v_hel(:,4),coupl(1,8,4),ecm,a0_cd)
        end if

        if(coupl(1,8,5)%idiag == 1) then        ! Sigma* - exchange
          call d_f(v_hel(:,5),coupl(1,8,5),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(1,8))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do  ! ng-loop

      ! isospin factors
      do ihel = 1,2

        w_iso(1,ihel,1) =   w(1,ihel) * 1.d0    ! I = 1/2  K* - exhange
        w_iso(1,ihel,2) =   w(1,ihel) * 2.d0    ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel) * 2.d0    ! I = 1/2  Sigma - exhange
        w_iso(2,ihel,2) =   w(2,ihel) * 1.d0    ! I = 3/2

        w_iso(3,ihel,1) = - w(3,ihel) * 1.d0    ! I = 1/2  Lambda - exhange
        w_iso(3,ihel,2) =   w(3,ihel) * 1.d0    ! I = 3/2

        w_iso(4,ihel,1) =   w(4,ihel) * 1.d0    ! I = 1/2  kappa - exhange
        w_iso(4,ihel,2) =   w(4,ihel) * 2.d0    ! I = 3/2

        w_iso(5,ihel,1) =   w(5,ihel) * 2.d0    ! I = 1/2  Sigma* - exhange
        w_iso(5,ihel,2) =   w(5,ihel) * 1.d0    ! I = 3/2

      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(1,8))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)  ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)  ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)  ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)  ! l=j+1/2; I=3/2
      end do

      !  summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(1,8))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_piN_SigK

  !=======================================================================!
  !  LamK -> LamK potential
  !=======================================================================!

  subroutine v_LamK_LamK(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)           ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm         ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng

      ! masses
      wk = masses(6,2)
      wk2 = masses(6,2)*masses(6,2)
      wlam = masses(3,1)  ! Lambda
      wlam2 = wlam*wlam

      ! on-energy shell energies
      eps1 = ( ecm**2 + wlam2 -wk2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wk2 -wlam2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wlam2 -wk2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wk2 -wlam2 )/(2.d0*ecm)
      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wlam2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wk2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wlam2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wk2 )

      ptildei = pcmi/(p1_cm(0)+wlam)
      ptildef = pcmf/(p3_cm(0)+wlam)

      cnorm = cdsqrt( (p1_cm(0)+wlam)/p1_cm(0) * (p3_cm(0)+wlam)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambda
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wlam)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(7,7))
          w(ityp,ihel)    =(0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wlam)

        if(coupl(7,7,1)%idiag == 1) then   ! sigma - t
          call d_cs(v_hel(:,1),coupl(7,7,1),ecm,a0_cd)
        end if

        if(coupl(7,7,2)%idiag == 1) then   ! omega - t
          call d_ds(v_hel(:,2),coupl(7,7,2),ecm)
        end if

        if(coupl(7,7,3)%idiag == 1) then   ! phi - t
          call d_ds(v_hel(:,3),coupl(7,7,3),ecm)
        end if

        if(coupl(7,7,4)%idiag == 1) then   ! Xi - u
          call de_b(v_hel(:,4),coupl(7,7,4),ecm)
        end if

        if(coupl(7,7,5)%idiag == 1) then   ! Xi* - u
          call d_f(v_hel(:,5),coupl(7,7,5),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(7,7))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do   ! ng-loop

      !  isospin factors
      do ihel = 1,2

        w_iso(1,ihel,1) =   w(1,ihel) * 1.d0     ! I = 1/2  sigma-exhange
        w_iso(1,ihel,2) =   w(1,ihel) * 0.d0     ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel) * 1.d0     ! I = 1/2  omega-exhange
        w_iso(2,ihel,2) =   w(2,ihel) * 0.d0     ! I = 3/2

        w_iso(3,ihel,1) =   w(3,ihel) * 1.d0     ! I = 1/2  phi-exhange
        w_iso(3,ihel,2) =   w(3,ihel) * 0.d0     ! I = 3/2

        w_iso(4,ihel,1) =   w(4,ihel) * 1.d0     ! I = 1/2  Xi-exhange
        w_iso(4,ihel,2) =   w(4,ihel) * 0.d0     ! I = 3/2

        w_iso(5,ihel,1) =   w(5,ihel) * 1.d0     ! I = 1/2  Xi*-exhange
        w_iso(5,ihel,2) =   w(5,ihel) * 0.d0     ! I = 3/2

      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(7,7))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)   ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)   ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)   ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)   ! l=j+1/2; I=3/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(7,7))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_LamK_LamK

  !=======================================================================!
  !  LamK -> SigK potential
  !=======================================================================!

  subroutine v_LamK_SigK(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)          ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm    ! input
      complex(kind(0.d0)) ::     ecm        ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng

      ! masses
      wk = masses(6,2)
      wk2 = masses(6,2)*masses(6,2)
      wlam = masses(3,1)
      wlam2 = wlam*wlam
      wsig = masses(4,1)
      wsig2 = wsig*wsig

      ! on-energy shell energies
      eps1 = ( ecm**2 + wlam2 -wk2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wk2 -wlam2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wsig2 -wk2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wk2 -wsig2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wlam2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wk2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsig2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wk2 )

      ptildei = pcmi/(p1_cm(0)+wlam)
      ptildef = pcmf/(p3_cm(0)+wsig)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wlam)/p1_cm(0) * (p3_cm(0)+wsig)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambda
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wlam)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(7,8))
          w(ityp,ihel) =   (0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Sigma
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wsig)

        if(coupl(7,8,1)%idiag == 1) then          	! rho-exchange
          call d_ds(v_hel(:,1),coupl(7,8,1),ecm)
        end if

        if(coupl(7,8,2)%idiag == 1) then          ! Xi - u
          call de_b(v_hel(:,2),coupl(7,8,2),ecm)
        end if

        if(coupl(7,8,3)%idiag == 1) then          ! Xi* - u
          call d_f(v_hel(:,3),coupl(7,8,3),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(7,8))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do   ! ng-loop

      ! isospin factors
      do ihel = 1,2
        w_iso(1,ihel,1) = - w(1,ihel) * dsqrt(3.d0)   ! I = 1/2  rho-exhange
        w_iso(1,ihel,2) =   w(1,ihel) * 0.d0          ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel) * dsqrt(3.d0)   ! I = 1/2  Xi-exhange
        w_iso(2,ihel,2) =   w(2,ihel) * 0.d0          ! I = 3/2

        w_iso(3,ihel,1) =   w(3,ihel) * dsqrt(3.d0)   ! I = 1/2  Xi*-exhange
        w_iso(3,ihel,2) =   w(3,ihel) * 0.d0          ! I = 3/2
      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(7,8))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)   ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)   ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)   ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)   ! l=j+1/2; I=3/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(7,8))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_LamK_SigK

  !=======================================================================!
  !  SigK -> SigK potential
  !=======================================================================!

  subroutine v_SigK_SigK(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)          ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm    ! input
      complex(kind(0.d0)) ::     ecm        ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng

      ! masses
      wk = masses(6,2)
      wk2 = masses(6,2)*masses(6,2)
      wsig = masses(4,1)
      wsig2 = wsig*wsig

      ! on-energy shell energies
      eps1 = ( ecm**2 + wsig2 -wk2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wk2 -wsig2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wsig2 -wk2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wk2 -wsig2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wsig2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wk2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsig2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wk2 )

      ptildei = pcmi/(p1_cm(0)+wsig)
      ptildef = pcmf/(p3_cm(0)+wsig)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wsig)/p1_cm(0) * (p3_cm(0)+wsig)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Sigma
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wsig)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(8,8))
          w(ityp,ihel)    =(0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Sigma
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wsig)

        if(coupl(8,8,1)%idiag == 1) then         ! sigma-exchange
          call d_cs(v_hel(:,1),coupl(8,8,1),ecm,a0_cd)
        end if

        if(coupl(8,8,2)%idiag == 1) then         ! omega- exchange
          call d_ds(v_hel(:,2),coupl(8,8,2),ecm)
        end if

        if(coupl(8,8,3)%idiag == 1) then         ! phi- exchange
          call d_ds(v_hel(:,3),coupl(8,8,3),ecm)
        end if

        if(coupl(8,8,4)%idiag == 1) then         ! rho- exchange
          call d_ds(v_hel(:,4),coupl(8,8,4),ecm)
        end if

        if(coupl(8,8,5)%idiag == 1) then         ! Xi - u
          call de_b(v_hel(:,5),coupl(8,8,5),ecm)
        end if

        if(coupl(8,8,6)%idiag == 1) then         ! Xi* - u
          call d_f(v_hel(:,6),coupl(8,8,6),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(8,8))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do   ! ng-loop

      ! isospin factors
      do ihel = 1,2

        w_iso(1,ihel,1) =   w(1,ihel) * 1.d0    ! I = 1/2  sigma-exhange
        w_iso(1,ihel,2) =   w(1,ihel) * 1.d0    ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel) * 1.d0    ! I = 1/2  omega-exhange
        w_iso(2,ihel,2) =   w(2,ihel) * 1.d0    ! I = 3/2

        w_iso(3,ihel,1) =   w(3,ihel) * 1.d0    ! I = 1/2  phi-exhange
        w_iso(3,ihel,2) =   w(3,ihel) * 1.d0    ! I = 3/2

        w_iso(4,ihel,1) =   w(4,ihel) * 2.d0    ! I = 1/2  rho-exhange
        w_iso(4,ihel,2) = - w(4,ihel) * 1.d0    ! I = 3/2

        w_iso(5,ihel,1) = - w(5,ihel) * 1.d0    ! I = 1/2  Xi-exhange
        w_iso(5,ihel,2) =   w(5,ihel) * 2.d0    ! I = 3/2

        w_iso(6,ihel,1) = - w(6,ihel) * 1.d0    ! I = 1/2  Xi*-exhange
        w_iso(6,ihel,2) =   w(6,ihel) * 2.d0    ! I = 3/2

      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(8,8))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)   ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)   ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)   ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)   ! l=j+1/2; I=3/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(8,8))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_SigK_SigK

  !=======================================================================!
  !  Neta -> SigK potential
  !=======================================================================!

  subroutine v_Neta_SigK(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)          ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm    ! input
      complex(kind(0.d0)) :: ecm            ! input
      type(couplings)     :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng

      ! masses
      weta  = masses(5,2)
      weta2 = weta*weta
      wk    = masses(6,2)
      wk2   = wk*wk
      wnu   = masses(1,1)
      wnu2  = wnu*wnu
      wsig  = masses(4,1)
      wsig2 = wsig*wsig

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2  - weta2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + weta2 - wnu2  )/(2.d0*ecm)
      eps3 = ( ecm**2 + wsig2 - wk2   )/(2.d0*ecm)
      eps4 = ( ecm**2 + wk2   - wsig2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + weta2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsig2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wk2 )

      ptildei = pcmi/(p1_cm(0)+wnu )
      ptildef = pcmf/(p3_cm(0)+wsig)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wsig)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(3,8))
          w(ityp,ihel)    =(0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Sigma
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wsig)

        if(coupl(3,8,1)%idiag == 1) then         ! K* exchange
          call d_ds(v_hel(:,1),coupl(3,8,1),ecm)
        end if

        if(coupl(3,8,2)%idiag == 1) then         ! Sig exchange
          call de_b(v_hel(:,2),coupl(3,8,2),ecm)
        end if

        if(coupl(3,8,3)%idiag == 1) then         ! Sig* exchange
          call d_f(v_hel(:,3),coupl(3,8,3),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(3,8))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do   ! ng-loop

      ! isospin factors
      do ihel = 1,2

        w_iso(1,ihel,1) =   w(1,ihel) * (-dsqrt(3.d0))    ! I = 1/2  K*-exchange
        w_iso(1,ihel,2) =   w(1,ihel) * 0.d0    	  ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel) * (-dsqrt(3.d0))    ! I = 1/2  Sigma-exchange
        w_iso(2,ihel,2) =   w(2,ihel) * 0.d0    	  ! I = 3/2

        w_iso(3,ihel,1) =   w(3,ihel) * (-dsqrt(3.d0))    ! I = 1/2 Sigma*-exchange
        w_iso(3,ihel,2) =   w(3,ihel) * 0.d0   	 	  ! I = 3/2

      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(3,8))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)   ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)   ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)   ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)   ! l=j+1/2; I=3/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(3,8))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_Neta_SigK

  !=======================================================================!
  !  Neta -> LamK potential
  !=======================================================================!

  subroutine v_Neta_LamK(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)          ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm    ! input
      complex(kind(0.d0)) :: ecm            ! input
      type(couplings)     :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng

      ! masses
      weta  = masses(5,2)
      weta2 = weta*weta
      wk    = masses(6,2)
      wk2   = wk*wk
      wnu   = masses(1,1)
      wnu2  = wnu*wnu
      wlam  = masses(3,1)
      wlam2 = wlam*wlam

      ! on-energy shell energies
      eps1 = ( ecm**2 + wnu2  - weta2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + weta2 - wnu2  )/(2.d0*ecm)
      eps3 = ( ecm**2 + wlam2 - wk2   )/(2.d0*ecm)
      eps4 = ( ecm**2 + wk2   - wlam2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wnu2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + weta2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wlam2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wk2 )

      ptildei = pcmi/(p1_cm(0)+wnu )
      ptildef = pcmf/(p3_cm(0)+wlam)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wnu)/p1_cm(0) * (p3_cm(0)+wlam)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial nucleon
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wnu)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(3,7))
          w(ityp,ihel)    =(0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wlam)

        if(coupl(3,7,1)%idiag == 1) then         ! K* exchange
          call d_ds(v_hel(:,1),coupl(3,7,1),ecm)
        end if

        if(coupl(3,7,2)%idiag == 1) then         ! Lam exchange
          call de_b(v_hel(:,2),coupl(3,7,2),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(3,7))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do   ! ng-loop

      ! isospin factors
      do ihel = 1,2

        w_iso(1,ihel,1) =   w(1,ihel) * 1.d0    ! I = 1/2  K*-exchange
        w_iso(1,ihel,2) =   w(1,ihel) * 0.d0    ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel) * 1.d0    ! I = 1/2  Lam-exchange
        w_iso(2,ihel,2) =   w(2,ihel) * 0.d0    ! I = 3/2

      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(3,7))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)   ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)   ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)   ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)   ! l=j+1/2; I=3/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(3,7))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_Neta_LamK

  !=======================================================================!
  !  LamcD -> LamcD potential
  !=======================================================================!

  subroutine v_LamcD_LamcD(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)           ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm         ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng

      ! masses
      wd = masses(9,2)
      wd2 = masses(9,2)*masses(9,2)
      wlamc = masses(5,1)  ! Lambdac
      wlamc2 = wlamc*wlamc

      ! on-energy shell energies
      eps1 = ( ecm**2 + wlamc2 -wd2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wd2 -wlamc2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wlamc2 -wd2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wd2 -wlamc2 )/(2.d0*ecm)
      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wlamc2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wd2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wlamc2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wd2 )

      ptildei = pcmi/(p1_cm(0)+wlamc)
      ptildef = pcmf/(p3_cm(0)+wlamc)

      cnorm = cdsqrt( (p1_cm(0)+wlamc)/p1_cm(0) * (p3_cm(0)+wlamc)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambda
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wlamc)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(9,9))
          w(ityp,ihel)    =(0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wlamc)

        if(coupl(9,9,1)%idiag == 1) then   ! sigma - t
          call d_cs(v_hel(:,1),coupl(9,9,1),ecm,a0_cd)
        end if

        if(coupl(9,9,2)%idiag == 1) then   ! omega - t
          call d_ds(v_hel(:,2),coupl(9,9,2),ecm)
        end if

        if(coupl(9,9,3)%idiag == 1) then   ! phi - t
          call d_ds(v_hel(:,3),coupl(9,9,3),ecm)
        end if

        if(coupl(9,9,4)%idiag == 1) then   ! Xicc - u
          call de_b(v_hel(:,4),coupl(9,9,4),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(9,9))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do   ! ng-loop

      !  isospin factors
      do ihel = 1,2

        w_iso(1,ihel,1) =   w(1,ihel) * 1.d0     ! I = 1/2  sigma-exhange
        w_iso(1,ihel,2) =   w(1,ihel) * 0.d0     ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel) * 1.d0     ! I = 1/2  omega-exhange
        w_iso(2,ihel,2) =   w(2,ihel) * 0.d0     ! I = 3/2

        w_iso(3,ihel,1) =   w(3,ihel) * 1.d0     ! I = 1/2  phi-exhange
        w_iso(3,ihel,2) =   w(3,ihel) * 0.d0     ! I = 3/2

        w_iso(4,ihel,1) =   w(4,ihel) * 1.d0     ! I = 1/2  Xi-exhange
        w_iso(4,ihel,2) =   w(4,ihel) * 0.d0     ! I = 3/2

      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(9,9))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)   ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)   ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)   ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)   ! l=j+1/2; I=3/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(9,9))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_LamcD_LamcD

  !=======================================================================!
  !  LamcD -> SigcD potential
  !=======================================================================!

  subroutine v_LamcD_SigcD(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)          ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm    ! input
      complex(kind(0.d0)) ::     ecm        ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng

      ! masses
      wd = masses(9,2)
      wd2 = masses(9,2)*masses(9,2)
      wlamc = masses(5,1)
      wlamc2 = wlamc*wlamc
      wsigc = masses(6,1)
      wsigc2 = wsigc*wsigc

      ! on-energy shell energies
      eps1 = ( ecm**2 + wlamc2 -wd2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wd2 -wlamc2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wsigc2 -wd2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wd2 -wsigc2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wlamc2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wd2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsigc2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wd2 )

      ptildei = pcmi/(p1_cm(0)+wlamc)
      ptildef = pcmf/(p3_cm(0)+wsigc)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wlamc)/p1_cm(0) * (p3_cm(0)+wsigc)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambda
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wlamc)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(9,10))
          w(ityp,ihel) =   (0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Sigma
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wsigc)

        if(coupl(9,10,1)%idiag == 1) then          	! rho-exchange
          call d_ds(v_hel(:,1),coupl(9,10,1),ecm)
        end if

        if(coupl(9,10,2)%idiag == 1) then          ! Xi - u
          call de_b(v_hel(:,2),coupl(9,10,2),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(9,10))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do   ! ng-loop

      ! isospin factors
      do ihel = 1,2
        w_iso(1,ihel,1) = - w(1,ihel) * dsqrt(3.d0)   ! I = 1/2  rho-exhange
        w_iso(1,ihel,2) =   w(1,ihel) * 0.d0          ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel) * dsqrt(3.d0)   ! I = 1/2  Xi-exhange
        w_iso(2,ihel,2) =   w(2,ihel) * 0.d0          ! I = 3/2

      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(9,10))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)   ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)   ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)   ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)   ! l=j+1/2; I=3/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(9,10))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_LamcD_SigcD

  !=======================================================================!
  !  SigcD -> SigcD potential
  !=======================================================================!

  subroutine v_SigcD_SigcD(v,pf_cm,pi_cm,ecm)
      implicit none

      complex(kind(0.d0)) :: v(36)          ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm    ! input
      complex(kind(0.d0)) ::     ecm        ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,1:12)
      complex(kind(0.d0)) :: w(12,2)
      complex(kind(0.d0)) :: w_iso(12,2,2)
      complex(kind(0.d0)) :: w_lsj(12,4)
      integer :: i,ityp,ihel,ng

      ! masses
      wd = masses(9,2)
      wd2 = masses(9,2)*masses(9,2)
      wsigc = masses(6,1)
      wsigc2 = wsigc*wsigc

      ! on-energy shell energies
      eps1 = ( ecm**2 + wsigc2 -wd2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wd2 -wsigc2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wsigc2 -wd2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wd2 -wsigc2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wsigc2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wd2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsigc2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wd2 )

      ptildei = pcmi/(p1_cm(0)+wsigc)
      ptildef = pcmf/(p3_cm(0)+wsigc)

      ! angle-independent quantities
      cnorm = cdsqrt( (p1_cm(0)+wsigc)/p1_cm(0) * (p3_cm(0)+wsigc)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Sigma
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wsigc)

      do ihel = 1,2
        do ityp = 1,itypmax(ccrel(10,10))
          w(ityp,ihel)    =(0.d0,0.d0)
          v_hel(ihel,ityp)=(0.d0,0.d0)
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Sigma
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wsigc)

        if(coupl(10,10,1)%idiag == 1) then         ! sigma-exchange
          call d_cs(v_hel(:,1),coupl(10,10,1),ecm,a0_cd)
        end if

        if(coupl(10,10,2)%idiag == 1) then         ! omega- exchange
          call d_ds(v_hel(:,2),coupl(10,10,2),ecm)
        end if

        if(coupl(10,10,3)%idiag == 1) then         ! phi- exchange
          call d_ds(v_hel(:,3),coupl(10,10,3),ecm)
        end if

        if(coupl(10,10,4)%idiag == 1) then         ! rho- exchange
          call d_ds(v_hel(:,4),coupl(10,10,4),ecm)
        end if

        if(coupl(10,10,5)%idiag == 1) then         ! Xi - u
          call de_b(v_hel(:,5),coupl(10,10,5),ecm)
        end if

        do ihel = 1,2
          do ityp = 1,itypmax(ccrel(10,10))
            w(ityp,ihel)=w(ityp,ihel)+djw(ng,3-ihel*2,1)*v_hel(ihel,ityp)
          end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
        end do

      end do   ! ng-loop

      ! isospin factors
      do ihel = 1,2

        w_iso(1,ihel,1) =   w(1,ihel) * 1.d0    ! I = 1/2  sigma-exhange
        w_iso(1,ihel,2) =   w(1,ihel) * 1.d0    ! I = 3/2

        w_iso(2,ihel,1) =   w(2,ihel) * 1.d0    ! I = 1/2  omega-exhange
        w_iso(2,ihel,2) =   w(2,ihel) * 1.d0    ! I = 3/2

        w_iso(3,ihel,1) =   w(3,ihel) * 1.d0    ! I = 1/2  phi-exhange
        w_iso(3,ihel,2) =   w(3,ihel) * 1.d0    ! I = 3/2

        w_iso(4,ihel,1) =   w(4,ihel) * 2.d0    ! I = 1/2  rho-exhange
        w_iso(4,ihel,2) = - w(4,ihel) * 1.d0    ! I = 3/2

        w_iso(5,ihel,1) = - w(5,ihel) * 1.d0    ! I = 1/2  Xi-exhange
        w_iso(5,ihel,2) =   w(5,ihel) * 2.d0    ! I = 3/2

      end do

      ! transformation to lsj
      do i=1,itypmax(ccrel(10,10))
        w_lsj(i,1)= w_iso(i,1,1) + w_iso(i,2,1)   ! l=j-1/2; I=1/2
        w_lsj(i,2)= w_iso(i,1,1) - w_iso(i,2,1)   ! l=j+1/2; I=1/2
        w_lsj(i,3)= w_iso(i,1,2) + w_iso(i,2,2)   ! l=j-1/2; I=3/2
        w_lsj(i,4)= w_iso(i,1,2) - w_iso(i,2,2)   ! l=j+1/2; I=3/2
      end do

      ! summing all contributions  w(ityp, ihel, iso )
      do i = 1,4
        v(i) = ( 0.d0, 0.d0 )
        do ityp = 1,itypmax(ccrel(10,10))
          v(i)=v(i) + w_lsj(ityp,i)*cnorm
        end do
      end do

      return
  end subroutine v_SigcD_SigcD


  !=======================================================================!
  !  LamcD -> LamcDs potential
  !=======================================================================!

  subroutine v_LamcD_LamcDs(v,pf_cm,pi_cm,ecm)
      implicit none
      complex(kind(0.d0)) :: v(36)           ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm         ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,1:12)
      complex(kind(0.d0)) :: w(12,2,-1:1)
      complex(kind(0.d0)) :: w_iso(12,2,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,2)
      integer :: i,ityp,ihel,ng,lam,lad,iso,isoindx

      ! masses
      wd = masses(9,2)
      wd2 = masses(9,2)*masses(9,2)
      wlamc = masses(5,1)  ! Lambdac
      wlamc2 = wlamc*wlamc
      wds = masses(10,2)
      wds2 = masses(10,2)*masses(10,2)

      ! on-energy shell energies
      eps1 = ( ecm**2 + wlamc2 -wd2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wd2 -wlamc2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wlamc2 -wds2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wds2 -wlamc2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wlamc2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wd2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wlamc2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wds2 )

      ptildei = pcmi/(p1_cm(0)+wlamc)
      ptildef = pcmf/(p3_cm(0)+wlamc)

      cnorm = cdsqrt( (p1_cm(0)+wlamc)/p1_cm(0) * (p3_cm(0)+wlamc)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambdac
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wlamc)

      do ihel = 1,2
        do lam = -1,1
          do ityp = 1,itypmax(ccrel(9,11))
            w(ityp,ihel,lam)    =(0.d0,0.d0)
            v_hel(ihel,lam,ityp)=(0.d0,0.d0)
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wlamc)

        ! call the epsvec_mp_star
        call epsvec_mp_star(eps_mp_star,pcmf,wds,sn_cm,cs_cm)

        if(coupl(9,11,1)%idiag == 1) then   ! omega - t
          call d_tv(v_hel(:,:,1),coupl(9,11,1),ecm)
        end if

        if(coupl(9,11,2)%idiag == 1) then   ! Xicc - u
          call d_ub(v_hel(:,:,2),coupl(9,11,2),ecm)
        end if

        do lam=-1,1
          do ihel = 1,2
            lad=3-2*ihel-2*lam
            if (abs(lad)<=j1) then
              do ityp = 1,itypmax(ccrel(9,11))
                w(ityp,ihel,lam)=w(ityp,ihel,lam)+djw(ng,lad,1)*v_hel(ihel,lam,ityp)
              end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
            end if
          end do  !ihel
        end do  !lam

      end do   ! ng-loop

      !  isospin factors
      do ihel = 1,2
        do lam=-1,1

          w_iso(1,ihel,lam,1) =   w(1,ihel,lam) * 1.d0     ! I = 1/2  omega-exhange
          w_iso(1,ihel,lam,2) =   w(1,ihel,lam) * 0.d0     ! I = 3/2

          w_iso(2,ihel,lam,1) =   w(2,ihel,lam) * 1.d0     ! I = 1/2  Xi-exhange
          w_iso(2,ihel,lam,2) =   w(2,ihel,lam) * 0.d0     ! I = 3/2

          w_isosum(ihel,lam,1)=(0.d0,0.d0)
          w_isosum(ihel,lam,2)=(0.d0,0.d0)

          do ityp=1,itypmax(ccrel(9,11))
            w_isosum(ihel,lam,1)=w_isosum(ihel,lam,1)+w_iso(ityp,ihel,lam,1)*cnorm
            w_isosum(ihel,lam,2)=w_isosum(ihel,lam,2)+w_iso(ityp,ihel,lam,2)*cnorm
          end do

        end do  !lam
      end do  !ihel

      ! transformation to lsj
      do iso=1,2
        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*(a(2,-1)*w_isosum(1,0,iso)-a(2,-1)*w_isosum(2,0,i&
            &so)+a(1,-1)*w_isosum(2,-1,iso)-a(1,-1)*w_isosum(1,1,iso))*d(1,-1)
        v(2+isoindx) = 2.d0*(a(2,1)*w_isosum(1,0,iso)+a(2,1)*w_isosum(2,0,iso&
            &)+a(1,1)*w_isosum(2,-1,iso)+a(1,1)*w_isosum(1,1,iso))*d(1,1)
        v(5+isoindx) = 2.d0*(c(3,-1)*w_isosum(1,0,iso)-c(3,-1)*w_isosum(2,0,i&
            &so)+c(2,-1)*w_isosum(2,-1,iso)-c(2,-1)*w_isosum(1,1,iso)+c(1,-1)*w&
            &_isosum(1,-1,iso)-c(1,-1)*w_isosum(2,1,iso))*d(1,-1)
        v(6+isoindx) = 2.d0*(c(3,1)*w_isosum(1,0,iso)+c(3,1)*w_isosum(2,0,iso&
            &)+c(2,1)*w_isosum(2,-1,iso)+c(2,1)*w_isosum(1,1,iso)+c(1,1)*w_isos&
            &um(1,-1,iso)+c(1,1)*w_isosum(2,1,iso))*d(1,1)
        v(9+isoindx) = 2.d0*(b(3,1)*w_isosum(1,0,iso)-b(3,1)*w_isosum(2,0,iso&
            &)+b(2,1)*w_isosum(2,-1,iso)-b(2,1)*w_isosum(1,1,iso)+b(1,1)*w_isos&
            &um(1,-1,iso)-b(1,1)*w_isosum(2,1,iso))*d(1,-1)
        v(10+isoindx) = 2.d0*(b(3,-1)*w_isosum(1,0,iso)+b(3,-1)*w_isosum(2,0,&
            &iso)+b(2,-1)*w_isosum(2,-1,iso)+b(2,-1)*w_isosum(1,1,iso)+b(1,-1)*&
            &w_isosum(1,-1,iso)+b(1,-1)*w_isosum(2,1,iso))*d(1,1)
      end do

      return
  end subroutine v_LamcD_LamcDs

  !=======================================================================!
  !  LamcD -> SigcDs potential
  !=======================================================================!

  subroutine v_LamcD_SigcDs(v,pf_cm,pi_cm,ecm)
      implicit none
      complex(kind(0.d0)) :: v(36)           ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm         ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,1:12)
      complex(kind(0.d0)) :: w(12,2,-1:1)
      complex(kind(0.d0)) :: w_iso(12,2,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,2)
      integer :: i,ityp,ihel,ng,lam,lad,iso,isoindx

      ! masses
      wd = masses(9,2)
      wd2 = masses(9,2)*masses(9,2)
      wlamc = masses(5,1)  ! Lambdac
      wlamc2 = wlamc*wlamc
      wds = masses(10,2)
      wds2 = masses(10,2)*masses(10,2)
      wsigc = masses(6,1)  ! Sigmac
      wsigc2 = wsigc*wsigc

      ! on-energy shell energies
      eps1 = ( ecm**2 + wlamc2 -wd2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wd2 -wlamc2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wsigc2 -wds2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wds2 -wsigc2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wlamc2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wd2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsigc2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wds2 )

      ptildei = pcmi/(p1_cm(0)+wlamc)
      ptildef = pcmf/(p3_cm(0)+wsigc)

      cnorm = cdsqrt( (p1_cm(0)+wlamc)/p1_cm(0) * (p3_cm(0)+wsigc)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambdac
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wlamc)

      do ihel = 1,2
        do lam = -1,1
          do ityp = 1,itypmax(ccrel(9,12))
            w(ityp,ihel,lam)    =(0.d0,0.d0)
            v_hel(ihel,lam,ityp)=(0.d0,0.d0)
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wsigc)

        ! call the epsvec_mp_star
        call epsvec_mp_star(eps_mp_star,pcmf,wds,sn_cm,cs_cm)

        if(coupl(9,12,1)%idiag == 1) then   ! rho - t
          call d_tv(v_hel(:,:,1),coupl(9,12,1),ecm)
        end if

        if(coupl(9,12,2)%idiag == 1) then   ! pi - t
          call d_tp(v_hel(:,:,2),coupl(9,12,2),ecm)
        end if

        if(coupl(9,12,3)%idiag == 1) then   ! Xicc - u
          call d_ub(v_hel(:,:,3),coupl(9,12,3),ecm)
        end if

        do lam=-1,1
          do ihel = 1,2
            lad=3-2*ihel-2*lam
            if (abs(lad)<=j1) then
              do ityp = 1,itypmax(ccrel(9,12))
                w(ityp,ihel,lam)=w(ityp,ihel,lam)+djw(ng,lad,1)*v_hel(ihel,lam,ityp)
              end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
            end if
          end do  !ihel
        end do  !lam

      end do   ! ng-loop

      !  isospin factors
      do ihel = 1,2
        do lam=-1,1

          w_iso(1,ihel,lam,1) = - w(1,ihel,lam) * dsqrt(3.d0)  ! I = 1/2  rho-exhange
          w_iso(1,ihel,lam,2) =   w(1,ihel,lam) * 0.d0         ! I = 3/2

          w_iso(2,ihel,lam,1) = - w(2,ihel,lam) * dsqrt(3.d0)  ! I = 1/2  pi-exhange
          w_iso(2,ihel,lam,2) =   w(2,ihel,lam) * 0.d0         ! I = 3/2

          w_iso(3,ihel,lam,1) =   w(3,ihel,lam) * dsqrt(3.d0)  ! I = 1/2  Xi-exhange
          w_iso(3,ihel,lam,2) =   w(3,ihel,lam) * 0.d0         ! I = 3/2

          w_isosum(ihel,lam,1)=(0.d0,0.d0)
          w_isosum(ihel,lam,2)=(0.d0,0.d0)

          do ityp=1,itypmax(ccrel(9,12))
            w_isosum(ihel,lam,1)=w_isosum(ihel,lam,1)+w_iso(ityp,ihel,lam,1)*cnorm
            w_isosum(ihel,lam,2)=w_isosum(ihel,lam,2)+w_iso(ityp,ihel,lam,2)*cnorm
          end do

        end do  !lam
      end do  !ihel

      ! transformation to lsj
      do iso=1,2
        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*(a(2,-1)*w_isosum(1,0,iso)-a(2,-1)*w_isosum(2,0,i&
            &so)+a(1,-1)*w_isosum(2,-1,iso)-a(1,-1)*w_isosum(1,1,iso))*d(1,-1)
        v(2+isoindx) = 2.d0*(a(2,1)*w_isosum(1,0,iso)+a(2,1)*w_isosum(2,0,iso&
            &)+a(1,1)*w_isosum(2,-1,iso)+a(1,1)*w_isosum(1,1,iso))*d(1,1)
        v(5+isoindx) = 2.d0*(c(3,-1)*w_isosum(1,0,iso)-c(3,-1)*w_isosum(2,0,i&
            &so)+c(2,-1)*w_isosum(2,-1,iso)-c(2,-1)*w_isosum(1,1,iso)+c(1,-1)*w&
            &_isosum(1,-1,iso)-c(1,-1)*w_isosum(2,1,iso))*d(1,-1)
        v(6+isoindx) = 2.d0*(c(3,1)*w_isosum(1,0,iso)+c(3,1)*w_isosum(2,0,iso&
            &)+c(2,1)*w_isosum(2,-1,iso)+c(2,1)*w_isosum(1,1,iso)+c(1,1)*w_isos&
            &um(1,-1,iso)+c(1,1)*w_isosum(2,1,iso))*d(1,1)
        v(9+isoindx) = 2.d0*(b(3,1)*w_isosum(1,0,iso)-b(3,1)*w_isosum(2,0,iso&
            &)+b(2,1)*w_isosum(2,-1,iso)-b(2,1)*w_isosum(1,1,iso)+b(1,1)*w_isos&
            &um(1,-1,iso)-b(1,1)*w_isosum(2,1,iso))*d(1,-1)
        v(10+isoindx) = 2.d0*(b(3,-1)*w_isosum(1,0,iso)+b(3,-1)*w_isosum(2,0,&
            &iso)+b(2,-1)*w_isosum(2,-1,iso)+b(2,-1)*w_isosum(1,1,iso)+b(1,-1)*&
            &w_isosum(1,-1,iso)+b(1,-1)*w_isosum(2,1,iso))*d(1,1)
      end do

      return
  end subroutine v_LamcD_SigcDs

  !=======================================================================!
  !  SigcD -> LamcDs potential
  !=======================================================================!

  subroutine v_SigcD_LamcDs(v,pf_cm,pi_cm,ecm)
      implicit none
      complex(kind(0.d0)) :: v(36)           ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm         ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,1:12)
      complex(kind(0.d0)) :: w(12,2,-1:1)
      complex(kind(0.d0)) :: w_iso(12,2,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,2)
      integer :: i,ityp,ihel,ng,lam,lad,iso,isoindx

      ! masses
      wd = masses(9,2)
      wd2 = masses(9,2)*masses(9,2)
      wsigc = masses(6,1)  ! Sigmac
      wsigc2 = wsigc*wsigc
      wds = masses(10,2)
      wds2 = masses(10,2)*masses(10,2)
      wlamc = masses(5,1)  ! Lambdac
      wlamc2 = wlamc*wlamc

      ! on-energy shell energies
      eps1 = ( ecm**2 + wsigc2 -wd2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wd2 -wsigc2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wlamc2 -wds2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wds2 -wlamc2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wsigc2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wd2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wlamc2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wds2 )

      ptildei = pcmi/(p1_cm(0)+wsigc)
      ptildef = pcmf/(p3_cm(0)+wlamc)

      cnorm = cdsqrt( (p1_cm(0)+wsigc)/p1_cm(0) * (p3_cm(0)+wlamc)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambdac
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wsigc)

      do ihel = 1,2
        do lam = -1,1
          do ityp = 1,itypmax(ccrel(10,11))
            w(ityp,ihel,lam)    =(0.d0,0.d0)
            v_hel(ihel,lam,ityp)=(0.d0,0.d0)
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wlamc)

        ! call the epsvec_mp_star
        call epsvec_mp_star(eps_mp_star,pcmf,wds,sn_cm,cs_cm)

        if(coupl(10,11,1)%idiag == 1) then   ! omega - t
          call d_tv(v_hel(:,:,1),coupl(10,11,1),ecm)
        end if

        if(coupl(10,11,2)%idiag == 1) then   ! pi - t
          call d_tp(v_hel(:,:,2),coupl(10,11,2),ecm)
        end if

        if(coupl(10,11,3)%idiag == 1) then   ! Xicc - u
          call d_ub(v_hel(:,:,3),coupl(10,11,3),ecm)
        end if

        do lam=-1,1
          do ihel = 1,2
            lad=3-2*ihel-2*lam
            if (abs(lad)<=j1) then
              do ityp = 1,itypmax(ccrel(10,11))
                w(ityp,ihel,lam)=w(ityp,ihel,lam)+djw(ng,lad,1)*v_hel(ihel,lam,ityp)
              end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
            end if
          end do  !ihel
        end do  !lam

      end do   ! ng-loop

      !  isospin factors
      do ihel = 1,2
        do lam=-1,1

          w_iso(1,ihel,lam,1) = - w(1,ihel,lam) * dsqrt(3.d0)  ! I = 1/2  rho-exhange
          w_iso(1,ihel,lam,2) =   w(1,ihel,lam) * 0.d0         ! I = 3/2

          w_iso(2,ihel,lam,1) = - w(2,ihel,lam) * dsqrt(3.d0)  ! I = 1/2  pi-exhange
          w_iso(2,ihel,lam,2) =   w(2,ihel,lam) * 0.d0         ! I = 3/2

          w_iso(3,ihel,lam,1) =   w(3,ihel,lam) * dsqrt(3.d0)  ! I = 1/2  Xi-exhange
          w_iso(3,ihel,lam,2) =   w(3,ihel,lam) * 0.d0         ! I = 3/2

          w_isosum(ihel,lam,1)=(0.d0,0.d0)
          w_isosum(ihel,lam,2)=(0.d0,0.d0)

          do ityp=1,itypmax(ccrel(10,11))
            w_isosum(ihel,lam,1)=w_isosum(ihel,lam,1)+w_iso(ityp,ihel,lam,1)*cnorm
            w_isosum(ihel,lam,2)=w_isosum(ihel,lam,2)+w_iso(ityp,ihel,lam,2)*cnorm
          end do

        end do  !lam
      end do  !ihel

      ! transformation to lsj
      do iso=1,2
        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*(a(2,-1)*w_isosum(1,0,iso)-a(2,-1)*w_isosum(2,0,i&
            &so)+a(1,-1)*w_isosum(2,-1,iso)-a(1,-1)*w_isosum(1,1,iso))*d(1,-1)
        v(2+isoindx) = 2.d0*(a(2,1)*w_isosum(1,0,iso)+a(2,1)*w_isosum(2,0,iso&
            &)+a(1,1)*w_isosum(2,-1,iso)+a(1,1)*w_isosum(1,1,iso))*d(1,1)
        v(5+isoindx) = 2.d0*(c(3,-1)*w_isosum(1,0,iso)-c(3,-1)*w_isosum(2,0,i&
            &so)+c(2,-1)*w_isosum(2,-1,iso)-c(2,-1)*w_isosum(1,1,iso)+c(1,-1)*w&
            &_isosum(1,-1,iso)-c(1,-1)*w_isosum(2,1,iso))*d(1,-1)
        v(6+isoindx) = 2.d0*(c(3,1)*w_isosum(1,0,iso)+c(3,1)*w_isosum(2,0,iso&
            &)+c(2,1)*w_isosum(2,-1,iso)+c(2,1)*w_isosum(1,1,iso)+c(1,1)*w_isos&
            &um(1,-1,iso)+c(1,1)*w_isosum(2,1,iso))*d(1,1)
        v(9+isoindx) = 2.d0*(b(3,1)*w_isosum(1,0,iso)-b(3,1)*w_isosum(2,0,iso&
            &)+b(2,1)*w_isosum(2,-1,iso)-b(2,1)*w_isosum(1,1,iso)+b(1,1)*w_isos&
            &um(1,-1,iso)-b(1,1)*w_isosum(2,1,iso))*d(1,-1)
        v(10+isoindx) = 2.d0*(b(3,-1)*w_isosum(1,0,iso)+b(3,-1)*w_isosum(2,0,&
            &iso)+b(2,-1)*w_isosum(2,-1,iso)+b(2,-1)*w_isosum(1,1,iso)+b(1,-1)*&
            &w_isosum(1,-1,iso)+b(1,-1)*w_isosum(2,1,iso))*d(1,1)
      end do

      return
  end subroutine v_SigcD_LamcDs

  !=======================================================================!
  !  SigcD -> SigcDs potential
  !=======================================================================!

  subroutine v_SigcD_SigcDs(v,pf_cm,pi_cm,ecm)
      implicit none
      complex(kind(0.d0)) :: v(36)           ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm         ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,1:12)
      complex(kind(0.d0)) :: w(12,2,-1:1)
      complex(kind(0.d0)) :: w_iso(12,2,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,2)
      integer :: i,ityp,ihel,ng,lam,lad,iso,isoindx

      ! masses
      wd = masses(9,2)
      wd2 = masses(9,2)*masses(9,2)
      wsigc = masses(6,1)  ! Sigmac
      wsigc2 = wsigc*wsigc
      wds = masses(10,2)
      wds2 = masses(10,2)*masses(10,2)

      ! on-energy shell energies
      eps1 = ( ecm**2 + wsigc2 -wd2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wd2 -wsigc2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wsigc2 -wds2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wds2 -wsigc2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wsigc2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wd2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsigc2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wds2 )

      ptildei = pcmi/(p1_cm(0)+wsigc)
      ptildef = pcmf/(p3_cm(0)+wsigc)

      cnorm = cdsqrt( (p1_cm(0)+wsigc)/p1_cm(0) * (p3_cm(0)+wsigc)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambdac
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wsigc)

      do ihel = 1,2
        do lam = -1,1
          do ityp = 1,itypmax(ccrel(10,12))
            w(ityp,ihel,lam)    =(0.d0,0.d0)
            v_hel(ihel,lam,ityp)=(0.d0,0.d0)
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wsigc)

        ! call the epsvec_mp_star
        call epsvec_mp_star(eps_mp_star,pcmf,wds,sn_cm,cs_cm)

        if(coupl(10,12,1)%idiag == 1) then   ! rho - t
          call d_tv(v_hel(:,:,1),coupl(10,12,1),ecm)
        end if

        if(coupl(10,12,2)%idiag == 1) then   ! omega - t
          call d_tv(v_hel(:,:,2),coupl(10,12,2),ecm)
        end if

        if(coupl(10,12,3)%idiag == 1) then   ! pi - t
          call d_tp(v_hel(:,:,3),coupl(10,12,3),ecm)
        end if

        if(coupl(10,12,4)%idiag == 1) then   ! Xicc - u
          call d_ub(v_hel(:,:,4),coupl(10,12,4),ecm)
        end if

        do lam=-1,1
          do ihel = 1,2
            lad=3-2*ihel-2*lam
            if (abs(lad)<=j1) then
              do ityp = 1,itypmax(ccrel(10,12))
                w(ityp,ihel,lam)=w(ityp,ihel,lam)+djw(ng,lad,1)*v_hel(ihel,lam,ityp)
              end do                             ! this leads to 1(/2) for ihel=1 and -1(/2) for ihel=2
            end if
          end do  !ihel
        end do  !lam

      end do   ! ng-loop

      !  isospin factors
      do ihel = 1,2
        do lam=-1,1

          w_iso(1,ihel,lam,1) =   w(1,ihel,lam) * 2.d0     ! I = 1/2  rho-exhange
          w_iso(1,ihel,lam,2) = - w(1,ihel,lam) * 1.d0     ! I = 3/2

          w_iso(2,ihel,lam,1) =   w(2,ihel,lam) * 1.d0     ! I = 1/2  omega-exhange
          w_iso(2,ihel,lam,2) =   w(2,ihel,lam) * 1.d0     ! I = 3/2

          w_iso(3,ihel,lam,1) =   w(3,ihel,lam) * 2.d0     ! I = 1/2  pi-exhange
          w_iso(3,ihel,lam,2) = - w(3,ihel,lam) * 1.d0     ! I = 3/2

          w_iso(4,ihel,lam,1) = - w(4,ihel,lam) * 1.d0     ! I = 1/2  Xi-exhange
          w_iso(4,ihel,lam,2) =   w(4,ihel,lam) * 2.d0     ! I = 3/2

          w_isosum(ihel,lam,1)=(0.d0,0.d0)
          w_isosum(ihel,lam,2)=(0.d0,0.d0)

          do ityp=1,itypmax(ccrel(10,12))
            w_isosum(ihel,lam,1)=w_isosum(ihel,lam,1)+w_iso(ityp,ihel,lam,1)*cnorm
            w_isosum(ihel,lam,2)=w_isosum(ihel,lam,2)+w_iso(ityp,ihel,lam,2)*cnorm
          end do

        end do  !lam
      end do  !ihel

      ! transformation to lsj
      do iso=1,2
        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*(a(2,-1)*w_isosum(1,0,iso)-a(2,-1)*w_isosum(2,0,i&
            &so)+a(1,-1)*w_isosum(2,-1,iso)-a(1,-1)*w_isosum(1,1,iso))*d(1,-1)
        v(2+isoindx) = 2.d0*(a(2,1)*w_isosum(1,0,iso)+a(2,1)*w_isosum(2,0,iso&
            &)+a(1,1)*w_isosum(2,-1,iso)+a(1,1)*w_isosum(1,1,iso))*d(1,1)
        v(5+isoindx) = 2.d0*(c(3,-1)*w_isosum(1,0,iso)-c(3,-1)*w_isosum(2,0,i&
            &so)+c(2,-1)*w_isosum(2,-1,iso)-c(2,-1)*w_isosum(1,1,iso)+c(1,-1)*w&
            &_isosum(1,-1,iso)-c(1,-1)*w_isosum(2,1,iso))*d(1,-1)
        v(6+isoindx) = 2.d0*(c(3,1)*w_isosum(1,0,iso)+c(3,1)*w_isosum(2,0,iso&
            &)+c(2,1)*w_isosum(2,-1,iso)+c(2,1)*w_isosum(1,1,iso)+c(1,1)*w_isos&
            &um(1,-1,iso)+c(1,1)*w_isosum(2,1,iso))*d(1,1)
        v(9+isoindx) = 2.d0*(b(3,1)*w_isosum(1,0,iso)-b(3,1)*w_isosum(2,0,iso&
            &)+b(2,1)*w_isosum(2,-1,iso)-b(2,1)*w_isosum(1,1,iso)+b(1,1)*w_isos&
            &um(1,-1,iso)-b(1,1)*w_isosum(2,1,iso))*d(1,-1)
        v(10+isoindx) = 2.d0*(b(3,-1)*w_isosum(1,0,iso)+b(3,-1)*w_isosum(2,0,&
            &iso)+b(2,-1)*w_isosum(2,-1,iso)+b(2,-1)*w_isosum(1,1,iso)+b(1,-1)*&
            &w_isosum(1,-1,iso)+b(1,-1)*w_isosum(2,1,iso))*d(1,1)
      end do

      return
  end subroutine v_SigcD_SigcDs

  !=======================================================================!
  !  LamcDs -> LamcDs potential
  !=======================================================================!

  subroutine v_LamcDs_LamcDs(v,pf_cm,pi_cm,ecm)
      implicit none
      complex(kind(0.d0)) :: v(36)           ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm         ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,-1:1,1:12)
      complex(kind(0.d0)) :: w(12,2,-1:1,-1:1)
      complex(kind(0.d0)) :: w_iso(12,2,-1:1,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,-1:1,2)
      integer :: i,ityp,ihel,ng,lam1,lam2,lad1,lad2,iso,isoindx

      ! v_hel(ihel,lam2,lam1,ityp)
      !       !    !    !
      !       !    !    Ds in
      !       !    Ds out
      !       Lamc out

      ! masses
      wds = masses(10,2)
      wds2 = masses(10,2)*masses(10,2)
      wlamc = masses(5,1)  ! Lambdac
      wlamc2 = wlamc*wlamc

      ! on-energy shell energies
      eps1 = ( ecm**2 + wlamc2 -wds2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wds2 -wlamc2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wlamc2 -wds2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wds2 -wlamc2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wlamc2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wds2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wlamc2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wds2 )

      ptildei = pcmi/(p1_cm(0)+wlamc)
      ptildef = pcmf/(p3_cm(0)+wlamc)

      cnorm = cdsqrt( (p1_cm(0)+wlamc)/p1_cm(0) * (p3_cm(0)+wlamc)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambdac
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wlamc)

      ! call eps for the incoming rho
      call epsvec_mp(eps_mp,pcmi,wds,0.d0,1.d0)

      do ihel = 1,2
        do lam1 = -1,1
          do lam2 = -1,1
            do ityp = 1,itypmax(ccrel(11,11))
              w(ityp,ihel,lam2,lam1)    =(0.d0,0.d0)
              v_hel(ihel,lam2,lam1,ityp)=(0.d0,0.d0)
            end do
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wlamc)

        ! call the epsvec_mp_star
        call epsvec_mp_star(eps_mp_star,pcmf,wds,sn_cm,cs_cm)

        if(coupl(11,11,1)%idiag == 2) then   ! omega - t
          call dlam_tv(v_hel(:,:,:,1),coupl(11,11,1),ecm)
        end if

        if(coupl(11,11,2)%idiag == 1) then   ! Xicc - u
          call dlam_ub(v_hel(:,:,:,2),coupl(11,11,2),ecm)
        end if

        do lam1=-1,1
          do lam2=-1,1
            do ihel = 1,2
              lad2=3-2*ihel-2*lam2
              lad1=1-2*lam1
              if ((abs(lad1)<=j1).and.(abs(lad2)<=j1)) then
                do ityp = 1,itypmax(ccrel(11,11))
                  w(ityp,ihel,lam2,lam1)=w(ityp,ihel,lam2,lam1)&
                      &+djw(ng,lad2,lad1)*v_hel(ihel,lam2,lam1,ityp)
                end do
              end if
            end do  !ihel
          end do  !lam2
        end do  !lam1

      end do   ! ng-loop

      !  isospin factors
      do ihel = 1,2
        do lam1=-1,1
          do lam2=-1,1

            w_iso(1,ihel,lam2,lam1,1) =   w(1,ihel,lam2,lam1) * 1.d0     ! I = 1/2  omega-exhange
            w_iso(1,ihel,lam2,lam1,2) =   w(1,ihel,lam2,lam1) * 0.d0     ! I = 3/2

            w_iso(2,ihel,lam2,lam1,1) =   w(2,ihel,lam2,lam1) * 1.d0     ! I = 1/2  Xi-exhange
            w_iso(2,ihel,lam2,lam1,2) =   w(2,ihel,lam2,lam1) * 0.d0     ! I = 3/2

            w_isosum(ihel,lam2,lam1,1)=(0.d0,0.d0)
            w_isosum(ihel,lam2,lam1,2)=(0.d0,0.d0)

            do ityp=1,itypmax(ccrel(11,11))
              w_isosum(ihel,lam2,lam1,1)=w_isosum(ihel,lam2,lam1,1)+w_iso(ityp,ihel,lam2,lam1,1)*cnorm
              w_isosum(ihel,lam2,lam1,2)=w_isosum(ihel,lam2,lam1,2)+w_iso(ityp,ihel,lam2,lam1,2)*cnorm
            end do
          end do
        end do  !lam
      end do  !ihel

      ! transformation to lsj
      do iso=1,2
        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*a(2,-1)**2*w_isosum(1,0,0,iso)-2.d0*a(2,-1)**2*w_iso&
            &sum(2,0,0,iso)+2.d0*a(2,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*a(2,-1)*a(&
            &1,-1)*w_isosum(1,1,0,iso)+2.d0*a(1,-1)*a(2,-1)*w_isosum(2,0,1,iso)-2.d0*&
            &a(1,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)**2*w_isosum(1,1,1,is&
            &o)-2.d0*a(1,-1)**2*w_isosum(2,-1,1,iso)

        v(2+isoindx) = 2.d0*a(2,1)**2*w_isosum(1,0,0,iso)+2.d0*a(2,1)**2*w_isosu&
            &m(2,0,0,iso)+2.d0*a(2,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)*a(1,1)*&
            &w_isosum(1,1,0,iso)+2.d0*a(1,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0*a(1,1)*a&
            &(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)**2*w_isosum(1,1,1,iso)+2.d0*a(1,1)&
            &**2*w_isosum(2,-1,1,iso)

        v(5+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(3&
            &,-1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*&
            &a(2,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*c(1,-1)*w_isosum(1,-&
            &1,0,iso)-2.d0*a(2,-1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*c(3,-1)*w&
            &_isosum(2,0,1,iso)-2.d0*a(1,-1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)&
            &*c(2,-1)*w_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso&
            &)+2.d0*a(1,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*c(1,-1)*w_isosum&
            &(1,-1,1,iso)

        v(6+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1)&
            &*w_isosum(2,0,0,iso)+2.d0*a(2,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)&
            &*c(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0&
            &*a(2,1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*c(3,1)*w_isosum(2,0,1,&
            &iso)+2.d0*a(1,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(&
            &1,1,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1)*c(1,1)*w_&
            &isosum(2,1,1,iso)+2.d0*a(1,1)*c(1,1)*w_isosum(1,-1,1,iso)

        v(9+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3,&
            &1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*a(2&
            &,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*b(1,1)*w_isosum(1,-1,0,i&
            &so)-2.d0*a(2,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*b(3,1)*w_isosum&
            &(2,0,1,iso)-2.d0*a(1,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*&
            &w_isosum(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,-1&
            &)*b(1,1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*b(1,1)*w_isosum(1,-1,1,iso)

        v(10+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*a(2,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*a(&
            &2,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*a(1,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*a(1,1)*b(1,-1)*w_isosum(1,-1,1,iso)

        v(13+isoindx) = 2.d0*c(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*c(2,-1)**2*w_is&
            &osum(1,1,1,iso)+2.d0*c(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*c(1,-1)*c(2,-&
            &1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*&
            &c(2,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*c(3,-1)*c(1,-1)*w_isosum(2,1&
            &,0,iso)+2.d0*c(3,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*c(1,-1)*c(2,-1)*w&
            &_isosum(1,1,-1,iso)-2.d0*c(2,-1)*c(1,-1)*w_isosum(1,-1,1,iso)-2.d0*c(2,-&
            &1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*c(1,-1)*w_isosum(1,-1,0,i&
            &so)-2.d0*c(3,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(1,-1)*c(3,-1)*w_isos&
            &um(1,0,-1,iso)-2.d0*c(1,-1)**2*w_isosum(2,1,-1,iso)-2.d0*c(3,-1)**2*w_is&
            &osum(2,0,0,iso)-2.d0*c(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,-1)*c(3,-1&
            &)*w_isosum(2,0,1,iso)

        v(14+isoindx) = 2.d0*c(3,1)**2*w_isosum(1,0,0,iso)+2.d0*c(3,1)**2*w_isos&
            &um(2,0,0,iso)+2.d0*c(3,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1)*c(2,1)&
            &*w_isosum(1,1,0,iso)+2.d0*c(3,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0*c(3,1)&
            &*c(1,1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*c(3,1)*w_isosum(2,0,1,iso)+2.d0*&
            &c(2,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)**2*w_isosum(1,1,1,iso)+&
            &2.d0*c(2,1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,1)*c(1,1)*w_isosum(2,1,1,is&
            &o)+2.d0*c(2,1)*c(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(1&
            &,0,-1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*c(2,1)*w_&
            &isosum(2,-1,-1,iso)+2.d0*c(1,1)*c(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*&
            &*2*w_isosum(1,-1,-1,iso)+2.d0*c(1,1)**2*w_isosum(2,1,-1,iso)

        v(17+isoindx) = 2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,iso)-2.d0*c(1,-1)*b&
            &(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,-1)*b(3,1)*w_isosum(1,0,-1,iso)-2.d0&
            &*c(2,-1)*b(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(2,-1)*b(2,1)*w_isosum(1,1&
            &,1,iso)-2.d0*c(2,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*b(1,1)*w_is&
            &osum(1,-1,0,iso)-2.d0*c(3,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(3,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*c(&
            &3,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,&
            &iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)+2.d0*c(1,-1)*b(2,1)*w_isos&
            &um(2,-1,-1,iso)-2.d0*c(1,-1)*b(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(2,-1)*b(&
            &1,1)*w_isosum(2,1,1,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(3,1)*w_isosum(2,0,1,iso)

        v(18+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*c(&
            &3,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*c(2,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(2,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*c(2,1)*b(1,-1)*w_isosum(1,-1,1,iso&
            &)+2.d0*c(1,1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*c(1,1)*b(3,-1)*w_isosum(&
            &2,0,-1,iso)+2.d0*c(1,1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*b(2,-1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)

        v(21+isoindx) = 2.d0*b(1,1)*b(2,1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*b(3&
            &,1)*w_isosum(2,0,-1,iso)+2.d0*b(2,1)*b(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2&
            &,1)*b(3,1)*w_isosum(2,0,1,iso)-2.d0*b(3,1)*b(1,1)*w_isosum(2,1,0,iso)&
            &+2.d0*b(3,1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*b(2,1)**2*w_isosum(2,-1,1,&
            &iso)-2.d0*b(1,1)**2*w_isosum(2,1,-1,iso)+2.d0*b(3,1)*b(1,1)*w_isosum(1,-&
            &1,0,iso)-2.d0*b(1,1)*b(2,1)*w_isosum(1,1,-1,iso)+2.d0*b(1,1)*b(3,1)*w_is&
            &osum(1,0,-1,iso)-2.d0*b(2,1)*b(1,1)*w_isosum(1,-1,1,iso)-2.d0*b(3,1)**2*&
            &w_isosum(2,0,0,iso)-2.d0*b(2,1)*b(3,1)*w_isosum(1,0,1,iso)-2.d0*b(3,1)*b&
            &(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,1)**2*w_isosum(1,0,0,iso)+2.d0*b(2,1)&
            &**2*w_isosum(1,1,1,iso)+2.d0*b(1,1)**2*w_isosum(1,-1,-1,iso)

        v(22+isoindx) = 2.d0*b(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*b(3,-1)**2*w_is&
            &osum(2,0,0,iso)+2.d0*b(3,-1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*b(3,-1)*b&
            &(2,-1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*b(1,-1)*w_isosum(1,-1,0,iso)+&
            &2.d0*b(3,-1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(2&
            &,0,1,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,-1)**2*w_iso&
            &sum(1,1,1,iso)+2.d0*b(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*b(2,-1)*b(1,-1)&
            &*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*b(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(1,&
            &-1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*b(3,-1)*w_isosum(2,0,-1&
            &,iso)+2.d0*b(1,-1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*b(2,-1)*w_&
            &isosum(1,1,-1,iso)+2.d0*b(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*b(1,-1)**2&
            &*w_isosum(2,1,-1,iso)

        v(25+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(&
            &3,-1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0&
            &*c(3,-1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*c(2,-1)*a(2,-1)*w_isosum(2,&
            &0,1,iso)-2.d0*c(2,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*c(2,-1)*w&
            &_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(1,-1&
            &)*a(2,-1)*w_isosum(1,0,-1,iso)-2.d0*c(1,-1)*a(2,-1)*w_isosum(2,0,-1,i&
            &so)+2.d0*c(1,-1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*a(1,-1)*w_is&
            &osum(1,1,-1,iso)

        v(26+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1&
            &)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1&
            &)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*c(2,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0&
            &*c(2,1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(1,1,1,&
            &iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum&
            &(1,0,-1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*a(1,1)*&
            &w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*a(1,1)*w_isosum(1,1,-1,iso)

        v(29+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3&
            &,1)*w_isosum(2,0,0,iso)+2.d0*b(3,1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*b(&
            &3,1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*b(2,1)*a(2,-1)*w_isosum(2,0,1,i&
            &so)-2.d0*b(2,1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*w_isosum&
            &(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*b(1,1)*a(2,-1)&
            &*w_isosum(1,0,-1,iso)-2.d0*b(1,1)*a(2,-1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*a(1,-1)*w_isosum(1,1,-1,iso)

        v(30+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*b(2,-1)*a(2,1)*w_isosum(2,0,1,i&
            &so)+2.d0*b(2,-1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum&
            &(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(1,-1)*a(2,1)&
            &*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &-1)*a(1,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*a(1,1)*w_isosum(1,1,-1,iso)

        v(33+isoindx) = -2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(2,1)*w_isosum(1,1,1,iso)+2.d0*b(1,1)*c(3,-1)*w_isosum(1,0,-1&
            &,iso)-2.d0*b(2,1)*c(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(2,1)*c(1,-1)*w_iso&
            &sum(2,1,1,iso)-2.d0*b(2,1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,1)*c(3,-&
            &1)*w_isosum(2,0,1,iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)-2.d0*b(3&
            &,1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,&
            &iso)+2.d0*b(3,1)*c(1,-1)*w_isosum(1,-1,0,iso)-2.d0*b(3,1)*c(2,-1)*w_isos&
            &um(1,1,0,iso)+2.d0*b(3,1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*b(1,1)*c(2,-&
            &1)*w_isosum(1,1,-1,iso)-2.d0*b(1,1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*b(&
            &1,1)*c(2,-1)*w_isosum(2,-1,-1,iso)

        v(34+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*c(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(1,-1,0,&
            &iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*c(3,1)*w_isosu&
            &m(2,0,1,iso)+2.d0*b(2,-1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(2,-&
            &1)*c(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*c(1,1)*w_isosum(1,-1,1,iso&
            &)+2.d0*b(1,-1)*c(3,1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*c(3,1)*w_isosum(&
            &2,0,-1,iso)+2.d0*b(1,-1)*c(2,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*c(2,1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)
      end do

      return
  end subroutine v_LamcDs_LamcDs

  !=======================================================================!
  !  LamcDs -> SigcDs potential
  !=======================================================================!

  subroutine v_LamcDs_SigcDs(v,pf_cm,pi_cm,ecm)
      implicit none
      complex(kind(0.d0)) :: v(36)           ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm         ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,-1:1,1:12)
      complex(kind(0.d0)) :: w(12,2,-1:1,-1:1)
      complex(kind(0.d0)) :: w_iso(12,2,-1:1,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,-1:1,2)
      integer :: i,ityp,ihel,ng,lam1,lam2,lad1,lad2,iso,isoindx

      ! v_hel(ihel,lam2,lam1,ityp)
      !       !    !    !
      !       !    !    Ds in
      !       !    Ds out
      !       Lamc out

      ! masses
      wds = masses(10,2)
      wds2 = masses(10,2)*masses(10,2)
      wlamc = masses(5,1)  ! Lambdac
      wlamc2 = wlamc*wlamc
      wsigc = masses(6,1)  ! Sigmac
      wsigc2 = wsigc*wsigc

      ! on-energy shell energies
      eps1 = ( ecm**2 + wlamc2 -wds2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wds2 -wlamc2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wsigc2 -wds2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wds2 -wsigc2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wlamc2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wds2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsigc2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wds2 )

      ptildei = pcmi/(p1_cm(0)+wlamc)
      ptildef = pcmf/(p3_cm(0)+wsigc)

      cnorm = cdsqrt( (p1_cm(0)+wlamc)/p1_cm(0) * (p3_cm(0)+wsigc)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambdac
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wlamc)

      ! call eps for the incoming rho
      call epsvec_mp(eps_mp,pcmi,wds,0.d0,1.d0)

      do ihel = 1,2
        do lam1 = -1,1
          do lam2 = -1,1
            do ityp = 1,itypmax(ccrel(11,12))
              w(ityp,ihel,lam2,lam1)    =(0.d0,0.d0)
              v_hel(ihel,lam2,lam1,ityp)=(0.d0,0.d0)
            end do
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wsigc)

        ! call the epsvec_mp_star
        call epsvec_mp_star(eps_mp_star,pcmf,wds,sn_cm,cs_cm)

        if(coupl(11,12,1)%idiag == 2) then   ! rho - t
          call dlsc_tv(v_hel(:,:,:,1),coupl(11,12,1),ecm)
        end if

        if(coupl(11,12,2)%idiag == 1) then   ! pi - u
          call dd_tp(v_hel(:,:,:,2),coupl(11,12,2),ecm)
        end if

        if(coupl(11,12,3)%idiag == 1) then   ! Xicc - u
          call dlsc_ub(v_hel(:,:,:,3),coupl(11,12,3),ecm)
        end if

        do lam1=-1,1
          do lam2=-1,1
            do ihel = 1,2
              lad2=3-2*ihel-2*lam2
              lad1=1-2*lam1
              if ((abs(lad1)<=j1).and.(abs(lad2)<=j1)) then
                do ityp = 1,itypmax(ccrel(11,12))
                  w(ityp,ihel,lam2,lam1)=w(ityp,ihel,lam2,lam1)&
                      &+djw(ng,lad2,lad1)*v_hel(ihel,lam2,lam1,ityp)
                end do
              end if
            end do  !ihel
          end do  !lam2
        end do  !lam1

      end do   ! ng-loop

      !  isospin factors
      do ihel = 1,2
        do lam1=-1,1
          do lam2=-1,1

            w_iso(1,ihel,lam2,lam1,1) = - w(1,ihel,lam2,lam1) * dsqrt(3.d0)  ! I = 1/2  rho-exhange
            w_iso(1,ihel,lam2,lam1,2) =   w(1,ihel,lam2,lam1) * 0.d0         ! I = 3/2

            w_iso(2,ihel,lam2,lam1,1) = - w(2,ihel,lam2,lam1) * dsqrt(3.d0)  ! I = 1/2  pi-exhange
            w_iso(2,ihel,lam2,lam1,2) =   w(2,ihel,lam2,lam1) * 0.d0         ! I = 3/2

            w_iso(3,ihel,lam2,lam1,1) =   w(3,ihel,lam2,lam1) * dsqrt(3.d0)  ! I = 1/2  Xi-exhange
            w_iso(3,ihel,lam2,lam1,2) =   w(3,ihel,lam2,lam1) * 0.d0         ! I = 3/2

            w_isosum(ihel,lam2,lam1,1)=(0.d0,0.d0)
            w_isosum(ihel,lam2,lam1,2)=(0.d0,0.d0)

            do ityp=1,itypmax(ccrel(11,12))
              w_isosum(ihel,lam2,lam1,1)=w_isosum(ihel,lam2,lam1,1)+w_iso(ityp,ihel,lam2,lam1,1)*cnorm
              w_isosum(ihel,lam2,lam1,2)=w_isosum(ihel,lam2,lam1,2)+w_iso(ityp,ihel,lam2,lam1,2)*cnorm
            end do
          end do
        end do  !lam
      end do  !ihel

      ! transformation to lsj
      do iso=1,2
        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*a(2,-1)**2*w_isosum(1,0,0,iso)-2.d0*a(2,-1)**2*w_iso&
            &sum(2,0,0,iso)+2.d0*a(2,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*a(2,-1)*a(&
            &1,-1)*w_isosum(1,1,0,iso)+2.d0*a(1,-1)*a(2,-1)*w_isosum(2,0,1,iso)-2.d0*&
            &a(1,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)**2*w_isosum(1,1,1,is&
            &o)-2.d0*a(1,-1)**2*w_isosum(2,-1,1,iso)

        v(2+isoindx) = 2.d0*a(2,1)**2*w_isosum(1,0,0,iso)+2.d0*a(2,1)**2*w_isosu&
            &m(2,0,0,iso)+2.d0*a(2,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)*a(1,1)*&
            &w_isosum(1,1,0,iso)+2.d0*a(1,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0*a(1,1)*a&
            &(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)**2*w_isosum(1,1,1,iso)+2.d0*a(1,1)&
            &**2*w_isosum(2,-1,1,iso)

        v(5+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(3&
            &,-1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*&
            &a(2,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*c(1,-1)*w_isosum(1,-&
            &1,0,iso)-2.d0*a(2,-1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*c(3,-1)*w&
            &_isosum(2,0,1,iso)-2.d0*a(1,-1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)&
            &*c(2,-1)*w_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso&
            &)+2.d0*a(1,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*c(1,-1)*w_isosum&
            &(1,-1,1,iso)

        v(6+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1)&
            &*w_isosum(2,0,0,iso)+2.d0*a(2,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)&
            &*c(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0&
            &*a(2,1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*c(3,1)*w_isosum(2,0,1,&
            &iso)+2.d0*a(1,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(&
            &1,1,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1)*c(1,1)*w_&
            &isosum(2,1,1,iso)+2.d0*a(1,1)*c(1,1)*w_isosum(1,-1,1,iso)

        v(9+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3,&
            &1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*a(2&
            &,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*b(1,1)*w_isosum(1,-1,0,i&
            &so)-2.d0*a(2,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*b(3,1)*w_isosum&
            &(2,0,1,iso)-2.d0*a(1,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*&
            &w_isosum(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,-1&
            &)*b(1,1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*b(1,1)*w_isosum(1,-1,1,iso)

        v(10+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*a(2,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*a(&
            &2,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*a(1,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*a(1,1)*b(1,-1)*w_isosum(1,-1,1,iso)

        v(13+isoindx) = 2.d0*c(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*c(2,-1)**2*w_is&
            &osum(1,1,1,iso)+2.d0*c(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*c(1,-1)*c(2,-&
            &1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*&
            &c(2,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*c(3,-1)*c(1,-1)*w_isosum(2,1&
            &,0,iso)+2.d0*c(3,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*c(1,-1)*c(2,-1)*w&
            &_isosum(1,1,-1,iso)-2.d0*c(2,-1)*c(1,-1)*w_isosum(1,-1,1,iso)-2.d0*c(2,-&
            &1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*c(1,-1)*w_isosum(1,-1,0,i&
            &so)-2.d0*c(3,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(1,-1)*c(3,-1)*w_isos&
            &um(1,0,-1,iso)-2.d0*c(1,-1)**2*w_isosum(2,1,-1,iso)-2.d0*c(3,-1)**2*w_is&
            &osum(2,0,0,iso)-2.d0*c(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,-1)*c(3,-1&
            &)*w_isosum(2,0,1,iso)

        v(14+isoindx) = 2.d0*c(3,1)**2*w_isosum(1,0,0,iso)+2.d0*c(3,1)**2*w_isos&
            &um(2,0,0,iso)+2.d0*c(3,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1)*c(2,1)&
            &*w_isosum(1,1,0,iso)+2.d0*c(3,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0*c(3,1)&
            &*c(1,1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*c(3,1)*w_isosum(2,0,1,iso)+2.d0*&
            &c(2,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)**2*w_isosum(1,1,1,iso)+&
            &2.d0*c(2,1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,1)*c(1,1)*w_isosum(2,1,1,is&
            &o)+2.d0*c(2,1)*c(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(1&
            &,0,-1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*c(2,1)*w_&
            &isosum(2,-1,-1,iso)+2.d0*c(1,1)*c(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*&
            &*2*w_isosum(1,-1,-1,iso)+2.d0*c(1,1)**2*w_isosum(2,1,-1,iso)

        v(17+isoindx) = 2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,iso)-2.d0*c(1,-1)*b&
            &(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,-1)*b(3,1)*w_isosum(1,0,-1,iso)-2.d0&
            &*c(2,-1)*b(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(2,-1)*b(2,1)*w_isosum(1,1&
            &,1,iso)-2.d0*c(2,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*b(1,1)*w_is&
            &osum(1,-1,0,iso)-2.d0*c(3,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(3,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*c(&
            &3,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,&
            &iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)+2.d0*c(1,-1)*b(2,1)*w_isos&
            &um(2,-1,-1,iso)-2.d0*c(1,-1)*b(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(2,-1)*b(&
            &1,1)*w_isosum(2,1,1,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(3,1)*w_isosum(2,0,1,iso)

        v(18+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*c(&
            &3,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*c(2,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(2,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*c(2,1)*b(1,-1)*w_isosum(1,-1,1,iso&
            &)+2.d0*c(1,1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*c(1,1)*b(3,-1)*w_isosum(&
            &2,0,-1,iso)+2.d0*c(1,1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*b(2,-1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)

        v(21+isoindx) = 2.d0*b(1,1)*b(2,1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*b(3&
            &,1)*w_isosum(2,0,-1,iso)+2.d0*b(2,1)*b(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2&
            &,1)*b(3,1)*w_isosum(2,0,1,iso)-2.d0*b(3,1)*b(1,1)*w_isosum(2,1,0,iso)&
            &+2.d0*b(3,1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*b(2,1)**2*w_isosum(2,-1,1,&
            &iso)-2.d0*b(1,1)**2*w_isosum(2,1,-1,iso)+2.d0*b(3,1)*b(1,1)*w_isosum(1,-&
            &1,0,iso)-2.d0*b(1,1)*b(2,1)*w_isosum(1,1,-1,iso)+2.d0*b(1,1)*b(3,1)*w_is&
            &osum(1,0,-1,iso)-2.d0*b(2,1)*b(1,1)*w_isosum(1,-1,1,iso)-2.d0*b(3,1)**2*&
            &w_isosum(2,0,0,iso)-2.d0*b(2,1)*b(3,1)*w_isosum(1,0,1,iso)-2.d0*b(3,1)*b&
            &(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,1)**2*w_isosum(1,0,0,iso)+2.d0*b(2,1)&
            &**2*w_isosum(1,1,1,iso)+2.d0*b(1,1)**2*w_isosum(1,-1,-1,iso)

        v(22+isoindx) = 2.d0*b(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*b(3,-1)**2*w_is&
            &osum(2,0,0,iso)+2.d0*b(3,-1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*b(3,-1)*b&
            &(2,-1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*b(1,-1)*w_isosum(1,-1,0,iso)+&
            &2.d0*b(3,-1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(2&
            &,0,1,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,-1)**2*w_iso&
            &sum(1,1,1,iso)+2.d0*b(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*b(2,-1)*b(1,-1)&
            &*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*b(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(1,&
            &-1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*b(3,-1)*w_isosum(2,0,-1&
            &,iso)+2.d0*b(1,-1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*b(2,-1)*w_&
            &isosum(1,1,-1,iso)+2.d0*b(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*b(1,-1)**2&
            &*w_isosum(2,1,-1,iso)

        v(25+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(&
            &3,-1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0&
            &*c(3,-1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*c(2,-1)*a(2,-1)*w_isosum(2,&
            &0,1,iso)-2.d0*c(2,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*c(2,-1)*w&
            &_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(1,-1&
            &)*a(2,-1)*w_isosum(1,0,-1,iso)-2.d0*c(1,-1)*a(2,-1)*w_isosum(2,0,-1,i&
            &so)+2.d0*c(1,-1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*a(1,-1)*w_is&
            &osum(1,1,-1,iso)

        v(26+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1&
            &)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1&
            &)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*c(2,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0&
            &*c(2,1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(1,1,1,&
            &iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum&
            &(1,0,-1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*a(1,1)*&
            &w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*a(1,1)*w_isosum(1,1,-1,iso)

        v(29+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3&
            &,1)*w_isosum(2,0,0,iso)+2.d0*b(3,1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*b(&
            &3,1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*b(2,1)*a(2,-1)*w_isosum(2,0,1,i&
            &so)-2.d0*b(2,1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*w_isosum&
            &(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*b(1,1)*a(2,-1)&
            &*w_isosum(1,0,-1,iso)-2.d0*b(1,1)*a(2,-1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*a(1,-1)*w_isosum(1,1,-1,iso)

        v(30+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*b(2,-1)*a(2,1)*w_isosum(2,0,1,i&
            &so)+2.d0*b(2,-1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum&
            &(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(1,-1)*a(2,1)&
            &*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &-1)*a(1,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*a(1,1)*w_isosum(1,1,-1,iso)

        v(33+isoindx) = -2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(2,1)*w_isosum(1,1,1,iso)+2.d0*b(1,1)*c(3,-1)*w_isosum(1,0,-1&
            &,iso)-2.d0*b(2,1)*c(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(2,1)*c(1,-1)*w_iso&
            &sum(2,1,1,iso)-2.d0*b(2,1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,1)*c(3,-&
            &1)*w_isosum(2,0,1,iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)-2.d0*b(3&
            &,1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,&
            &iso)+2.d0*b(3,1)*c(1,-1)*w_isosum(1,-1,0,iso)-2.d0*b(3,1)*c(2,-1)*w_isos&
            &um(1,1,0,iso)+2.d0*b(3,1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*b(1,1)*c(2,-&
            &1)*w_isosum(1,1,-1,iso)-2.d0*b(1,1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*b(&
            &1,1)*c(2,-1)*w_isosum(2,-1,-1,iso)

        v(34+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*c(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(1,-1,0,&
            &iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*c(3,1)*w_isosu&
            &m(2,0,1,iso)+2.d0*b(2,-1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(2,-&
            &1)*c(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*c(1,1)*w_isosum(1,-1,1,iso&
            &)+2.d0*b(1,-1)*c(3,1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*c(3,1)*w_isosum(&
            &2,0,-1,iso)+2.d0*b(1,-1)*c(2,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*c(2,1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)
      end do

      return
  end subroutine v_LamcDs_SigcDs

  !=======================================================================!
  !  SigcDs -> SigcDs potential
  !=======================================================================!

  subroutine v_SigcDs_SigcDs(v,pf_cm,pi_cm,ecm)
      implicit none
      complex(kind(0.d0)) :: v(36)           ! output
      complex(kind(0.d0)) :: pf_cm,pi_cm     ! input
      complex(kind(0.d0)) ::     ecm         ! input
      type(couplings) :: pc,pc2
      complex(kind(0.d0)) :: cnorm
      complex(kind(0.d0)) :: hel(1:2)
      complex(kind(0.d0)) :: v_hel(1:2,-1:1,-1:1,1:12)
      complex(kind(0.d0)) :: w(12,2,-1:1,-1:1)
      complex(kind(0.d0)) :: w_iso(12,2,-1:1,-1:1,2)
      complex(kind(0.d0)) :: w_isosum(2,-1:1,-1:1,2)
      integer :: i,ityp,ihel,ng,lam1,lam2,lad1,lad2,iso,isoindx

      ! v_hel(ihel,lam2,lam1,ityp)
      !       !    !    !
      !       !    !    Ds in
      !       !    Ds out
      !       Lamc out

      ! masses
      wds = masses(10,2)
      wds2 = masses(10,2)*masses(10,2)
      wsigc = masses(6,1)  ! Sigmac
      wsigc2 = wsigc*wsigc

      ! on-energy shell energies
      eps1 = ( ecm**2 + wsigc2 -wds2 )/(2.d0*ecm)
      eps2 = ( ecm**2 + wds2 -wsigc2 )/(2.d0*ecm)
      eps3 = ( ecm**2 + wsigc2 -wds2 )/(2.d0*ecm)
      eps4 = ( ecm**2 + wds2 -wsigc2 )/(2.d0*ecm)

      ! momenta of incoming particles 1 and 2
      p1_cm ( 0 ) = cdsqrt ( pi_cm**2 + wsigc2 )
      p1_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p1_cm ( 3 ) =   pi_cm
      p2_cm ( 0 ) = cdsqrt ( pi_cm**2 + wds2 )
      p2_cm ( 1 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 2 ) =  ( 0.d0, 0.d0 )
      p2_cm ( 3 ) = - pi_cm

      ! energies of outgoing particles 3 and 4
      p3_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wsigc2 )
      p4_cm ( 0 ) =  cdsqrt ( pf_cm**2 + wds2 )

      ptildei = pcmi/(p1_cm(0)+wsigc)
      ptildef = pcmf/(p3_cm(0)+wsigc)

      cnorm = cdsqrt( (p1_cm(0)+wsigc)/p1_cm(0) * (p3_cm(0)+wsigc)/p3_cm(0)&
          &/ (p4_cm(0) * p2_cm(0)) )/(4.d0*tpi3)

      ! spinor of initial Lambdac
      call spinor_cm(u1,pi_cm,p1_cm(0),1.d0,0.d0,wsigc)

      ! call eps for the incoming rho
      call epsvec_mp(eps_mp,pcmi,wds,0.d0,1.d0)

      do ihel = 1,2
        do lam1 = -1,1
          do lam2 = -1,1
            do ityp = 1,itypmax(ccrel(12,12))
              w(ityp,ihel,lam2,lam1)    =(0.d0,0.d0)
              v_hel(ihel,lam2,lam1,ityp)=(0.d0,0.d0)
            end do
          end do
        end do
      end do

      do ng = 1,nt

        cs_cm   = xg ( ng )
        sn_cm   = dsqrt(   1.d0 - cs_cm**2 )
        cs_cm_h = dsqrt( ( 1.d0 + cs_cm ) / 2.d0 )
        sn_cm_h = dsqrt( ( 1.d0 - cs_cm ) / 2.d0 )

        ! momenta of outgoing particles 3 and 4
        p3_cm ( 1 ) =   pf_cm * sn_cm
        p3_cm ( 2 ) =   (0.d0,0.d0)
        p3_cm ( 3 ) =   pf_cm * cs_cm
        do i=1,3
          p4_cm ( i) = - p3_cm(i)
        end do

        ! spinor of outgoing Lambda
        call spinor_cm(u3,pf_cm,p3_cm(0),cs_cm_h,sn_cm_h,wsigc)

        ! call the epsvec_mp_star
        call epsvec_mp_star(eps_mp_star,pcmf,wds,sn_cm,cs_cm)

        if(coupl(12,12,1)%idiag == 2) then   ! omega - t
          call dsig_tv(v_hel(:,:,:,1),coupl(12,12,1),ecm)
        end if

        if(coupl(12,12,2)%idiag == 2) then   ! rho - t
          call dsig_tv(v_hel(:,:,:,2),coupl(12,12,2),ecm)
        end if

        if(coupl(12,12,3)%idiag == 1) then   ! pi - u
          call dd_tp(v_hel(:,:,:,3),coupl(12,12,3),ecm)
        end if

        if(coupl(12,12,4)%idiag == 1) then   ! Xicc - u
          call dsig_ub(v_hel(:,:,:,4),coupl(12,12,4),ecm)
        end if

        do lam1=-1,1
          do lam2=-1,1
            do ihel = 1,2
              lad2=3-2*ihel-2*lam2
              lad1=1-2*lam1
              if ((abs(lad1)<=j1).and.(abs(lad2)<=j1)) then
                do ityp = 1,itypmax(ccrel(12,12))
                  w(ityp,ihel,lam2,lam1)=w(ityp,ihel,lam2,lam1)&
                      &+djw(ng,lad2,lad1)*v_hel(ihel,lam2,lam1,ityp)
                end do
              end if
            end do  !ihel
          end do  !lam2
        end do  !lam1

      end do   ! ng-loop

      !  isospin factors
      do ihel = 1,2
        do lam1=-1,1
          do lam2=-1,1

            w_iso(1,ihel,lam2,lam1,1) =   w(1,ihel,lam2,lam1) * 1.d0     ! I = 1/2  omega-exhange
            w_iso(1,ihel,lam2,lam1,2) =   w(1,ihel,lam2,lam1) * 1.d0     ! I = 3/2

            w_iso(2,ihel,lam2,lam1,1) =   w(2,ihel,lam2,lam1) * 2.d0     ! I = 1/2  rho-exhange
            w_iso(2,ihel,lam2,lam1,2) = - w(2,ihel,lam2,lam1) * 1.d0     ! I = 3/2

            w_iso(3,ihel,lam2,lam1,1) =   w(3,ihel,lam2,lam1) * 2.d0     ! I = 1/2  pi-exhange
            w_iso(3,ihel,lam2,lam1,2) = - w(3,ihel,lam2,lam1) * 1.d0     ! I = 3/2

            w_iso(4,ihel,lam2,lam1,1) = - w(4,ihel,lam2,lam1) * 1.d0     ! I = 1/2  Xi-exhange
            w_iso(4,ihel,lam2,lam1,2) =   w(4,ihel,lam2,lam1) * 2.d0     ! I = 3/2

            w_isosum(ihel,lam2,lam1,1)=(0.d0,0.d0)
            w_isosum(ihel,lam2,lam1,2)=(0.d0,0.d0)

            do ityp=1,itypmax(ccrel(12,12))
              w_isosum(ihel,lam2,lam1,1)=w_isosum(ihel,lam2,lam1,1)+w_iso(ityp,ihel,lam2,lam1,1)*cnorm
              w_isosum(ihel,lam2,lam1,2)=w_isosum(ihel,lam2,lam1,2)+w_iso(ityp,ihel,lam2,lam1,2)*cnorm
            end do
          end do
        end do  !lam
      end do  !ihel

      ! transformation to lsj
      do iso=1,2
        isoindx=2*iso-2

        v(1+isoindx) = 2.d0*a(2,-1)**2*w_isosum(1,0,0,iso)-2.d0*a(2,-1)**2*w_iso&
            &sum(2,0,0,iso)+2.d0*a(2,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*a(2,-1)*a(&
            &1,-1)*w_isosum(1,1,0,iso)+2.d0*a(1,-1)*a(2,-1)*w_isosum(2,0,1,iso)-2.d0*&
            &a(1,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)**2*w_isosum(1,1,1,is&
            &o)-2.d0*a(1,-1)**2*w_isosum(2,-1,1,iso)

        v(2+isoindx) = 2.d0*a(2,1)**2*w_isosum(1,0,0,iso)+2.d0*a(2,1)**2*w_isosu&
            &m(2,0,0,iso)+2.d0*a(2,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)*a(1,1)*&
            &w_isosum(1,1,0,iso)+2.d0*a(1,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0*a(1,1)*a&
            &(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)**2*w_isosum(1,1,1,iso)+2.d0*a(1,1)&
            &**2*w_isosum(2,-1,1,iso)

        v(5+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(3&
            &,-1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*&
            &a(2,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*c(1,-1)*w_isosum(1,-&
            &1,0,iso)-2.d0*a(2,-1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*c(3,-1)*w&
            &_isosum(2,0,1,iso)-2.d0*a(1,-1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)&
            &*c(2,-1)*w_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso&
            &)+2.d0*a(1,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*c(1,-1)*w_isosum&
            &(1,-1,1,iso)

        v(6+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1)&
            &*w_isosum(2,0,0,iso)+2.d0*a(2,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*a(2,1)&
            &*c(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0&
            &*a(2,1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*c(3,1)*w_isosum(2,0,1,&
            &iso)+2.d0*a(1,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(&
            &1,1,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1)*c(1,1)*w_&
            &isosum(2,1,1,iso)+2.d0*a(1,1)*c(1,1)*w_isosum(1,-1,1,iso)

        v(9+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3,&
            &1)*w_isosum(2,0,0,iso)+2.d0*a(2,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*a(2&
            &,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*a(2,-1)*b(1,1)*w_isosum(1,-1,0,i&
            &so)-2.d0*a(2,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*a(1,-1)*b(3,1)*w_isosum&
            &(2,0,1,iso)-2.d0*a(1,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*&
            &w_isosum(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*a(1,-1&
            &)*b(1,1)*w_isosum(2,1,1,iso)-2.d0*a(1,-1)*b(1,1)*w_isosum(1,-1,1,iso)

        v(10+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*a(2,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*a(&
            &2,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*a(2,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*a(1,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*a(1,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*a(1,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*a(1,1)*b(1,-1)*w_isosum(1,-1,1,iso)

        v(13+isoindx) = 2.d0*c(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*c(2,-1)**2*w_is&
            &osum(1,1,1,iso)+2.d0*c(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*c(1,-1)*c(2,-&
            &1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*&
            &c(2,-1)*c(1,-1)*w_isosum(2,1,1,iso)-2.d0*c(3,-1)*c(1,-1)*w_isosum(2,1&
            &,0,iso)+2.d0*c(3,-1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*c(1,-1)*c(2,-1)*w&
            &_isosum(1,1,-1,iso)-2.d0*c(2,-1)*c(1,-1)*w_isosum(1,-1,1,iso)-2.d0*c(2,-&
            &1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*c(1,-1)*w_isosum(1,-1,0,i&
            &so)-2.d0*c(3,-1)*c(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(1,-1)*c(3,-1)*w_isos&
            &um(1,0,-1,iso)-2.d0*c(1,-1)**2*w_isosum(2,1,-1,iso)-2.d0*c(3,-1)**2*w_is&
            &osum(2,0,0,iso)-2.d0*c(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,-1)*c(3,-1&
            &)*w_isosum(2,0,1,iso)

        v(14+isoindx) = 2.d0*c(3,1)**2*w_isosum(1,0,0,iso)+2.d0*c(3,1)**2*w_isos&
            &um(2,0,0,iso)+2.d0*c(3,1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1)*c(2,1)&
            &*w_isosum(1,1,0,iso)+2.d0*c(3,1)*c(1,1)*w_isosum(1,-1,0,iso)+2.d0*c(3,1)&
            &*c(1,1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*c(3,1)*w_isosum(2,0,1,iso)+2.d0*&
            &c(2,1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)**2*w_isosum(1,1,1,iso)+&
            &2.d0*c(2,1)**2*w_isosum(2,-1,1,iso)+2.d0*c(2,1)*c(1,1)*w_isosum(2,1,1,is&
            &o)+2.d0*c(2,1)*c(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(1&
            &,0,-1,iso)+2.d0*c(1,1)*c(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*c(2,1)*w_&
            &isosum(2,-1,-1,iso)+2.d0*c(1,1)*c(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*&
            &*2*w_isosum(1,-1,-1,iso)+2.d0*c(1,1)**2*w_isosum(2,1,-1,iso)

        v(17+isoindx) = 2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,iso)-2.d0*c(1,-1)*b&
            &(2,1)*w_isosum(1,1,-1,iso)+2.d0*c(1,-1)*b(3,1)*w_isosum(1,0,-1,iso)-2.d0&
            &*c(2,-1)*b(1,1)*w_isosum(1,-1,1,iso)+2.d0*c(2,-1)*b(2,1)*w_isosum(1,1&
            &,1,iso)-2.d0*c(2,-1)*b(3,1)*w_isosum(1,0,1,iso)+2.d0*c(3,-1)*b(1,1)*w_is&
            &osum(1,-1,0,iso)-2.d0*c(3,-1)*b(2,1)*w_isosum(1,1,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(3,-1)*b(1,1)*w_isosum(2,1,0,iso)+2.d0*c(&
            &3,-1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,&
            &iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)+2.d0*c(1,-1)*b(2,1)*w_isos&
            &um(2,-1,-1,iso)-2.d0*c(1,-1)*b(3,1)*w_isosum(2,0,-1,iso)+2.d0*c(2,-1)*b(&
            &1,1)*w_isosum(2,1,1,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(3,1)*w_isosum(2,0,1,iso)

        v(18+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*c(&
            &3,1)*b(2,-1)*w_isosum(1,1,0,iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(1,-1,0,&
            &iso)+2.d0*c(3,1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(2,1)*b(3,-1)*w_isosu&
            &m(2,0,1,iso)+2.d0*c(2,1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(2,1&
            &)*b(1,-1)*w_isosum(2,1,1,iso)+2.d0*c(2,1)*b(1,-1)*w_isosum(1,-1,1,iso&
            &)+2.d0*c(1,1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*c(1,1)*b(3,-1)*w_isosum(&
            &2,0,-1,iso)+2.d0*c(1,1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*b(2,-1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)

        v(21+isoindx) = 2.d0*b(1,1)*b(2,1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*b(3&
            &,1)*w_isosum(2,0,-1,iso)+2.d0*b(2,1)*b(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2&
            &,1)*b(3,1)*w_isosum(2,0,1,iso)-2.d0*b(3,1)*b(1,1)*w_isosum(2,1,0,iso)&
            &+2.d0*b(3,1)*b(2,1)*w_isosum(2,-1,0,iso)-2.d0*b(2,1)**2*w_isosum(2,-1,1,&
            &iso)-2.d0*b(1,1)**2*w_isosum(2,1,-1,iso)+2.d0*b(3,1)*b(1,1)*w_isosum(1,-&
            &1,0,iso)-2.d0*b(1,1)*b(2,1)*w_isosum(1,1,-1,iso)+2.d0*b(1,1)*b(3,1)*w_is&
            &osum(1,0,-1,iso)-2.d0*b(2,1)*b(1,1)*w_isosum(1,-1,1,iso)-2.d0*b(3,1)**2*&
            &w_isosum(2,0,0,iso)-2.d0*b(2,1)*b(3,1)*w_isosum(1,0,1,iso)-2.d0*b(3,1)*b&
            &(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,1)**2*w_isosum(1,0,0,iso)+2.d0*b(2,1)&
            &**2*w_isosum(1,1,1,iso)+2.d0*b(1,1)**2*w_isosum(1,-1,-1,iso)

        v(22+isoindx) = 2.d0*b(3,-1)**2*w_isosum(1,0,0,iso)+2.d0*b(3,-1)**2*w_is&
            &osum(2,0,0,iso)+2.d0*b(3,-1)*b(2,-1)*w_isosum(2,-1,0,iso)+2.d0*b(3,-1)*b&
            &(2,-1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*b(1,-1)*w_isosum(1,-1,0,iso)+&
            &2.d0*b(3,-1)*b(1,-1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(2&
            &,0,1,iso)+2.d0*b(2,-1)*b(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,-1)**2*w_iso&
            &sum(1,1,1,iso)+2.d0*b(2,-1)**2*w_isosum(2,-1,1,iso)+2.d0*b(2,-1)*b(1,-1)&
            &*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*b(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(1,&
            &-1)*b(3,-1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*b(3,-1)*w_isosum(2,0,-1&
            &,iso)+2.d0*b(1,-1)*b(2,-1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*b(2,-1)*w_&
            &isosum(1,1,-1,iso)+2.d0*b(1,-1)**2*w_isosum(1,-1,-1,iso)+2.d0*b(1,-1)**2&
            &*w_isosum(2,1,-1,iso)

        v(25+isoindx) = 2.d0*a(2,-1)*c(3,-1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*c(&
            &3,-1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0&
            &*c(3,-1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*c(2,-1)*a(2,-1)*w_isosum(2,&
            &0,1,iso)-2.d0*c(2,-1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*c(2,-1)*w&
            &_isosum(1,1,1,iso)-2.d0*a(1,-1)*c(2,-1)*w_isosum(2,-1,1,iso)+2.d0*c(1,-1&
            &)*a(2,-1)*w_isosum(1,0,-1,iso)-2.d0*c(1,-1)*a(2,-1)*w_isosum(2,0,-1,i&
            &so)+2.d0*c(1,-1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*c(1,-1)*a(1,-1)*w_is&
            &osum(1,1,-1,iso)

        v(26+isoindx) = 2.d0*a(2,1)*c(3,1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*c(3,1&
            &)*w_isosum(2,0,0,iso)+2.d0*c(3,1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*c(3,1&
            &)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*c(2,1)*a(2,1)*w_isosum(2,0,1,iso)+2.d0&
            &*c(2,1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*c(2,1)*w_isosum(1,1,1,&
            &iso)+2.d0*a(1,1)*c(2,1)*w_isosum(2,-1,1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum&
            &(1,0,-1,iso)+2.d0*c(1,1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*c(1,1)*a(1,1)*&
            &w_isosum(2,-1,-1,iso)+2.d0*c(1,1)*a(1,1)*w_isosum(1,1,-1,iso)

        v(29+isoindx) = 2.d0*a(2,-1)*b(3,1)*w_isosum(1,0,0,iso)-2.d0*a(2,-1)*b(3&
            &,1)*w_isosum(2,0,0,iso)+2.d0*b(3,1)*a(1,-1)*w_isosum(2,-1,0,iso)-2.d0*b(&
            &3,1)*a(1,-1)*w_isosum(1,1,0,iso)+2.d0*b(2,1)*a(2,-1)*w_isosum(2,0,1,i&
            &so)-2.d0*b(2,1)*a(2,-1)*w_isosum(1,0,1,iso)+2.d0*a(1,-1)*b(2,1)*w_isosum&
            &(1,1,1,iso)-2.d0*a(1,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*b(1,1)*a(2,-1)&
            &*w_isosum(1,0,-1,iso)-2.d0*b(1,1)*a(2,-1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &1)*a(1,-1)*w_isosum(2,-1,-1,iso)-2.d0*b(1,1)*a(1,-1)*w_isosum(1,1,-1,iso)

        v(30+isoindx) = 2.d0*a(2,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*a(2,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*a(1,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*a(1,1)*w_isosum(1,1,0,iso)+2.d0*b(2,-1)*a(2,1)*w_isosum(2,0,1,i&
            &so)+2.d0*b(2,-1)*a(2,1)*w_isosum(1,0,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum&
            &(1,1,1,iso)+2.d0*a(1,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(1,-1)*a(2,1)&
            &*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*a(2,1)*w_isosum(2,0,-1,iso)+2.d0*b(1,&
            &-1)*a(1,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*a(1,1)*w_isosum(1,1,-1,iso)

        v(33+isoindx) = -2.d0*c(3,-1)*b(3,1)*w_isosum(2,0,0,iso)+2.d0*c(3,-1)*b(&
            &3,1)*w_isosum(1,0,0,iso)-2.d0*c(2,-1)*b(2,1)*w_isosum(2,-1,1,iso)+2.d0*c&
            &(2,-1)*b(2,1)*w_isosum(1,1,1,iso)+2.d0*b(1,1)*c(3,-1)*w_isosum(1,0,-1&
            &,iso)-2.d0*b(2,1)*c(1,-1)*w_isosum(1,-1,1,iso)+2.d0*b(2,1)*c(1,-1)*w_iso&
            &sum(2,1,1,iso)-2.d0*b(2,1)*c(3,-1)*w_isosum(1,0,1,iso)+2.d0*b(2,1)*c(3,-&
            &1)*w_isosum(2,0,1,iso)-2.d0*c(1,-1)*b(1,1)*w_isosum(2,1,-1,iso)-2.d0*b(3&
            &,1)*c(1,-1)*w_isosum(2,1,0,iso)+2.d0*c(1,-1)*b(1,1)*w_isosum(1,-1,-1,&
            &iso)+2.d0*b(3,1)*c(1,-1)*w_isosum(1,-1,0,iso)-2.d0*b(3,1)*c(2,-1)*w_isos&
            &um(1,1,0,iso)+2.d0*b(3,1)*c(2,-1)*w_isosum(2,-1,0,iso)-2.d0*b(1,1)*c(2,-&
            &1)*w_isosum(1,1,-1,iso)-2.d0*b(1,1)*c(3,-1)*w_isosum(2,0,-1,iso)+2.d0*b(&
            &1,1)*c(2,-1)*w_isosum(2,-1,-1,iso)

        v(34+isoindx) = 2.d0*c(3,1)*b(3,-1)*w_isosum(1,0,0,iso)+2.d0*c(3,1)*b(3,&
            &-1)*w_isosum(2,0,0,iso)+2.d0*b(3,-1)*c(2,1)*w_isosum(2,-1,0,iso)+2.d0*b(&
            &3,-1)*c(2,1)*w_isosum(1,1,0,iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(1,-1,0,&
            &iso)+2.d0*b(3,-1)*c(1,1)*w_isosum(2,1,0,iso)+2.d0*b(2,-1)*c(3,1)*w_isosu&
            &m(2,0,1,iso)+2.d0*b(2,-1)*c(3,1)*w_isosum(1,0,1,iso)+2.d0*c(2,1)*b(2,-1)&
            &*w_isosum(1,1,1,iso)+2.d0*c(2,1)*b(2,-1)*w_isosum(2,-1,1,iso)+2.d0*b(2,-&
            &1)*c(1,1)*w_isosum(2,1,1,iso)+2.d0*b(2,-1)*c(1,1)*w_isosum(1,-1,1,iso&
            &)+2.d0*b(1,-1)*c(3,1)*w_isosum(1,0,-1,iso)+2.d0*b(1,-1)*c(3,1)*w_isosum(&
            &2,0,-1,iso)+2.d0*b(1,-1)*c(2,1)*w_isosum(2,-1,-1,iso)+2.d0*b(1,-1)*c(2,1&
            &)*w_isosum(1,1,-1,iso)+2.d0*c(1,1)*b(1,-1)*w_isosum(1,-1,-1,iso)+2.d0*c(&
            &1,1)*b(1,-1)*w_isosum(2,1,-1,iso)
      end do

      return
  end subroutine v_SigcDs_SigcDs

  !=======================================================================!

  subroutine dsig_tv(hel,pc,ecms)  ! vector meson exchange in V B --> V B
    implicit none

    ! output
    complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
    ! input:
    type(couplings) :: pc
    complex(kind(0.d0)) :: ecms
    ! intermediate storage:
    complex(kind(0.d0)) :: p_t1(0:3),p_t2(0:3),pdum(0:3)
    complex(kind(0.d0)) :: pt_sq,psq
    complex(kind(0.d0)) :: e_t
    complex(kind(0.d0)),dimension(-1:1,-1:1,0:3) :: g_rrr1,g_rrr2,g_rrrct
    complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: g_rnn1,g_rnn2,sigmunil
    complex(kind(0.d0)) :: ux(1:4),uy(1:4)
    complex(kind(0.d0)),dimension(1:4,1:4,-1:1,-1:1) :: shelp
    complex(kind(0.d0)), dimension(1:4,1:4) :: gam0
    complex(kind(0.d0)), dimension(1:2) :: scalct1
    complex(kind(0.d0)), dimension(0:3,1:2) :: scalct2
    complex(kind(0.d0)) :: prop1,prop2
    complex(kind(0.d0)) :: cc,sdum
    integer :: i,is,js,lam1,lam2,mu,nu
    complex(kind(0.d0)) :: fi,ff   ! form factor

    do i=1,3
      p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
      p_t2 ( i ) = p1_cm ( i ) - p3_cm ( i )
    end do
    pt_sq = p_t1(1)**2 + p_t1(3)**2   ! cm
    e_t   = cdsqrt(pc%wf**2+pt_sq)

    p_t1(0)= e_t
    p_t2(0)= -e_t

    call vertex_rhorhorho(g_rrr1,p_t1,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)
    !we need the outgoing rho
    call vertex_rhorhorho(g_rrr2,p_t2,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)
    !we need the outgoing rho
    call vertex_rhoNN(g_rnn1,p_t1,wsigc,pc%ka)

    call vertex_rhoNN(g_rnn2,p_t2,wsigc,pc%ka)

    ! idiag=2 calculates without q^mu q^nu term in the rho propagator
    ! as done in the thesis. idiag=1 calculates with q^muq^nu term and additional TOPT
    ! contact term. The difference is small and the changes in the fit
    ! can be absorbed in the N^*(1650) bare mass

    if (pc%idiag==2) then

      do lam1=-1,1
        do lam2=-1,1
          do is=1,4
            do js=1,4
              shelp(is,js,lam1,lam2)=(0.d0,0.d0)
              do mu=0,3
                ! without k^muk^nu term
                shelp(is,js,lam1,lam2)=shelp(is,js,lam1,lam2)-&
                    &g_rnn1(is,js,mu)*g_rrr1(lam1,lam2,mu)&
                    &/(ecms-e_t-p3_cm(0)-p2_cm(0))-&
                    &g_rnn2(is,js,mu)*g_rrr2(lam1,lam2,mu)&
                    &/(ecms-e_t-p1_cm(0)-p4_cm(0))
              end do
            end do
          end do
          call mat_spinor(ux,shelp(:,:,lam1,lam2),u1(:,1))
          do i=1,2
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          enddo
        end do
      end do

    else if (pc%idiag==1) then

      do lam1=-1,1
        do lam2=-1,1
          do is=1,4
            do js=1,4
              shelp(is,js,lam1,lam2)=(0.d0,0.d0)
              do mu=0,3
                ! with k^muk^nu term
                do nu=0,3
                  prop1=-gmunu(mu,nu)+p_t1(mu)*p_t1(nu)/masses(3,2)**2
                  prop2=-gmunu(mu,nu)+p_t2(mu)*p_t2(nu)/masses(3,2)**2
                  shelp(is,js,lam1,lam2)=shelp(is,js,lam1,lam2)+&
                      &g_rnn1(is,js,mu)*metric(mu)*prop1*g_rrr1(lam1,lam2,nu)&
                      !                          &*2.d0*e_t/((eps1-eps3)**2-e_t**2) !Fynm
                  &/(ecms-e_t-p3_cm(0)-p2_cm(0))+&
                      &g_rnn2(is,js,mu)*metric(mu)*prop2*g_rrr2(lam1,lam2,nu)&
                      &/(ecms-e_t-p1_cm(0)-p4_cm(0))
                end do
              end do
            end do
          end do
          call mat_spinor(ux,shelp(:,:,lam1,lam2),u1(:,1))
          do i=1,2
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          enddo
        end do
      end do

      ! get vertices for ct terms:
      call vertex_rhoNNct(gam0)

      pdum(0)=(1.d0,0.d0)
      call vertex_pind13A(sigmunil,pdum)

      pdum=(0.d0,0.d0)
      call vertex_rhorhorho(g_rrrct,pdum,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)

      call mat_spinor(ux,gam0,u1(:,1))
      do i=1,2
        call spinor_bar_spinor(scalct1(i),u3(:,i),ux)
      end do
      do mu=0,3
        call mat_spinor(ux,sigmunil(:,:,mu),u1(:,1))
        do i=1,2
          call spinor_bar_spinor(scalct2(mu,i),u3(:,i),ux)
        end do
      end do

    endif

    ff=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow
    fi=((pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq))**pc%npow
    ff=ff*fi
    !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
    if (imodel==4) then
      ff=(lamrr**2-normrr**2)/(lamrr**2+p1_cm(1)**2+p1_cm(3)**2)
    end if

    cc = ff * pc%gc / (2.d0*e_t)
    do lam1=-1,1
      do lam2=-1,1
        do i=1,2
          hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
        end do
      end do
    end do
    ! additional TOPT contact terms
    if (pc%idiag==1) then
      do lam1=-1,1
        do i=1,2
          sdum=(0.d0,0.d0)
          do mu=0,3
            sdum=sdum+scalct2(mu,i)*eps_mp_star(0,lam1)
          end do
          do lam2=-1,1
            hel(i,lam2,lam1)=hel(i,lam2,lam1)+&
                &scalct1(i)*g_rrrct(lam1,lam2,0)/masses(3,2)**2*ff*pc%gc&
                &+sdum*eps_mp_star(0,lam2)*ff*pc%gc*pc%ka/2.d0/masses(6,2)
          end do
        end do
      end do
    end if

    return
  end subroutine dsig_tv

  !=======================================================================!

  subroutine dsig_ub(hel,pc,ecms)  ! N-exchange exchange in V B --> V B
    implicit none

    ! output
    complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
    ! input:
    type(couplings) :: pc
    complex(kind(0.d0)) :: ecms
    ! intermediate storage:
    complex(kind(0.d0)) :: p_u(0:3)
    complex(kind(0.d0)) :: pu_sq,psq
    complex(kind(0.d0)) :: e_u
    complex(kind(0.d0)) :: propa
    complex(kind(0.d0)),dimension(1:4,1:4) :: sf,sfpos,sfneg
    complex(kind(0.d0)),dimension(1:4,1:4,-1:1) :: g_rho_4,g_rho_2
    complex(kind(0.d0)) :: ux(1:4)
    complex(kind(0.d0)) :: uy(1:4)
    complex(kind(0.d0)) :: cc
    integer :: i,lam1,lam2
    complex(kind(0.d0)) :: aux
    complex(kind(0.d0)) :: fi,ff   ! form factor

    p_u(0)=p1_cm(0)-p4_cm(0)    ! on-mass shell
    do i=1,3
      p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
    end do
    pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
    e_u   = cdsqrt(pc%wf**2+pu_sq)

    p_u(0)=e_u
    call psl_m(sfpos,p_u,pc%wf)
    p_u(0)=-e_u
    call psl_m(sfneg,p_u,pc%wf)

    sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
        &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.0 * e_u )

    call vertex_rhoNN2(g_rho_2,-1.d0*p2_cm,eps_mp,wsigc,pc%ka)

    call vertex_rhoNN2(g_rho_4,p4_cm,eps_mp_star,wsigc,pc%ka)

    do lam2=-1,1
      call mat_spinor(ux,g_rho_4(:,:,lam2),u1(:,1))
      call mat_spinor(uy,sf,ux)
      do lam1=-1,1
        call mat_spinor(ux,g_rho_2(:,:,lam1),uy)
        do i=1,2
          call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
        end do
      end do
    enddo

    ! We are using the formfactors as Kanzo uses, which means: there is no energy dependence
    ! in due to the gauge invariance problem by coupling the photon
    fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq))**pc%npow
    ff=fi*fi
    !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
    if (imodel==4) then
      ff=(lamrr**2-normrr**2)/(lamrr**2+p1_cm(1)**2+p1_cm(3)**2)
    end if

    cc = ff * pc%gc
    do lam1=-1,1
      do lam2=-1,1
        do i=1,2
          hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
        end do
      end do
    end do

    return
  end subroutine dsig_ub


  !=======================================================================!

  subroutine dlam_tv(hel,pc,ecms)  ! vector meson exchange in V B --> V B
    implicit none

    ! output
    complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
    ! input:
    type(couplings) :: pc
    complex(kind(0.d0)) :: ecms
    ! intermediate storage:
    complex(kind(0.d0)) :: p_t1(0:3),p_t2(0:3),pdum(0:3)
    complex(kind(0.d0)) :: pt_sq,psq
    complex(kind(0.d0)) :: e_t
    complex(kind(0.d0)),dimension(-1:1,-1:1,0:3) :: g_rrr1,g_rrr2,g_rrrct
    complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: g_rnn1,g_rnn2,sigmunil
    complex(kind(0.d0)) :: ux(1:4),uy(1:4)
    complex(kind(0.d0)),dimension(1:4,1:4,-1:1,-1:1) :: shelp
    complex(kind(0.d0)), dimension(1:4,1:4) :: gam0
    complex(kind(0.d0)), dimension(1:2) :: scalct1
    complex(kind(0.d0)), dimension(0:3,1:2) :: scalct2
    complex(kind(0.d0)) :: prop1,prop2
    complex(kind(0.d0)) :: cc,sdum
    integer :: i,is,js,lam1,lam2,mu,nu
    complex(kind(0.d0)) :: fi,ff   ! form factor

    do i=1,3
      p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
      p_t2 ( i ) = p1_cm ( i ) - p3_cm ( i )
    end do
    pt_sq = p_t1(1)**2 + p_t1(3)**2   ! cm
    e_t   = cdsqrt(pc%wf**2+pt_sq)

    p_t1(0)= e_t
    p_t2(0)= -e_t

    call vertex_rhorhorho(g_rrr1,p_t1,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)
    !we need the outgoing rho
    call vertex_rhorhorho(g_rrr2,p_t2,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)
    !we need the outgoing rho
    call vertex_rhoNN(g_rnn1,p_t1,wlamc,pc%ka)

    call vertex_rhoNN(g_rnn2,p_t2,wlamc,pc%ka)

    ! idiag=2 calculates without q^mu q^nu term in the rho propagator
    ! as done in the thesis. idiag=1 calculates with q^muq^nu term and additional TOPT
    ! contact term. The difference is small and the changes in the fit
    ! can be absorbed in the N^*(1650) bare mass

    if (pc%idiag==2) then

      do lam1=-1,1
        do lam2=-1,1
          do is=1,4
            do js=1,4
              shelp(is,js,lam1,lam2)=(0.d0,0.d0)
              do mu=0,3
                ! without k^muk^nu term
                shelp(is,js,lam1,lam2)=shelp(is,js,lam1,lam2)-&
                    &g_rnn1(is,js,mu)*g_rrr1(lam1,lam2,mu)&
                    &/(ecms-e_t-p3_cm(0)-p2_cm(0))-&
                    &g_rnn2(is,js,mu)*g_rrr2(lam1,lam2,mu)&
                    &/(ecms-e_t-p1_cm(0)-p4_cm(0))
              end do
            end do
          end do
          call mat_spinor(ux,shelp(:,:,lam1,lam2),u1(:,1))
          do i=1,2
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          enddo
        end do
      end do

    else if (pc%idiag==1) then

      do lam1=-1,1
        do lam2=-1,1
          do is=1,4
            do js=1,4
              shelp(is,js,lam1,lam2)=(0.d0,0.d0)
              do mu=0,3
                ! with k^muk^nu term
                do nu=0,3
                  prop1=-gmunu(mu,nu)+p_t1(mu)*p_t1(nu)/masses(3,2)**2
                  prop2=-gmunu(mu,nu)+p_t2(mu)*p_t2(nu)/masses(3,2)**2
                  shelp(is,js,lam1,lam2)=shelp(is,js,lam1,lam2)+&
                      &g_rnn1(is,js,mu)*metric(mu)*prop1*g_rrr1(lam1,lam2,nu)&
                      !                          &*2.d0*e_t/((eps1-eps3)**2-e_t**2) !Fynm
                  &/(ecms-e_t-p3_cm(0)-p2_cm(0))+&
                      &g_rnn2(is,js,mu)*metric(mu)*prop2*g_rrr2(lam1,lam2,nu)&
                      &/(ecms-e_t-p1_cm(0)-p4_cm(0))
                end do
              end do
            end do
          end do
          call mat_spinor(ux,shelp(:,:,lam1,lam2),u1(:,1))
          do i=1,2
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          enddo
        end do
      end do

      ! get vertices for ct terms:
      call vertex_rhoNNct(gam0)

      pdum(0)=(1.d0,0.d0)
      call vertex_pind13A(sigmunil,pdum)

      pdum=(0.d0,0.d0)
      call vertex_rhorhorho(g_rrrct,pdum,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)

      call mat_spinor(ux,gam0,u1(:,1))
      do i=1,2
        call spinor_bar_spinor(scalct1(i),u3(:,i),ux)
      end do
      do mu=0,3
        call mat_spinor(ux,sigmunil(:,:,mu),u1(:,1))
        do i=1,2
          call spinor_bar_spinor(scalct2(mu,i),u3(:,i),ux)
        end do
      end do

    endif

    ff=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow
    fi=((pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq))**pc%npow
    ff=ff*fi
    !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
    if (imodel==4) then
      ff=(lamrr**2-normrr**2)/(lamrr**2+p1_cm(1)**2+p1_cm(3)**2)
    end if

    cc = ff * pc%gc / (2.d0*e_t)
    do lam1=-1,1
      do lam2=-1,1
        do i=1,2
          hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
        end do
      end do
    end do
    ! additional TOPT contact terms
    if (pc%idiag==1) then
      do lam1=-1,1
        do i=1,2
          sdum=(0.d0,0.d0)
          do mu=0,3
            sdum=sdum+scalct2(mu,i)*eps_mp_star(0,lam1)
          end do
          do lam2=-1,1
            hel(i,lam2,lam1)=hel(i,lam2,lam1)+&
                &scalct1(i)*g_rrrct(lam1,lam2,0)/masses(3,2)**2*ff*pc%gc&
                &+sdum*eps_mp_star(0,lam2)*ff*pc%gc*pc%ka/2.d0/masses(6,2)
          end do
        end do
      end do
    end if

    return
  end subroutine dlam_tv

  !=======================================================================!

  subroutine dlam_ub(hel,pc,ecms)  ! N-exchange exchange in V B --> V B
    implicit none

    ! output
    complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
    ! input:
    type(couplings) :: pc
    complex(kind(0.d0)) :: ecms
    ! intermediate storage:
    complex(kind(0.d0)) :: p_u(0:3)
    complex(kind(0.d0)) :: pu_sq,psq
    complex(kind(0.d0)) :: e_u
    complex(kind(0.d0)) :: propa
    complex(kind(0.d0)),dimension(1:4,1:4) :: sf,sfpos,sfneg
    complex(kind(0.d0)),dimension(1:4,1:4,-1:1) :: g_rho_4,g_rho_2
    complex(kind(0.d0)) :: ux(1:4)
    complex(kind(0.d0)) :: uy(1:4)
    complex(kind(0.d0)) :: cc
    integer :: i,lam1,lam2
    complex(kind(0.d0)) :: aux
    complex(kind(0.d0)) :: fi,ff   ! form factor

    p_u(0)=p1_cm(0)-p4_cm(0)    ! on-mass shell
    do i=1,3
      p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
    end do
    pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
    e_u   = cdsqrt(pc%wf**2+pu_sq)

    p_u(0)=e_u
    call psl_m(sfpos,p_u,pc%wf)
    p_u(0)=-e_u
    call psl_m(sfneg,p_u,pc%wf)

    sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
        &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.0 * e_u )

    call vertex_rhoNN2(g_rho_2,-1.d0*p2_cm,eps_mp,wlamc,pc%ka)

    call vertex_rhoNN2(g_rho_4,p4_cm,eps_mp_star,wlamc,pc%ka)

    do lam2=-1,1
      call mat_spinor(ux,g_rho_4(:,:,lam2),u1(:,1))
      call mat_spinor(uy,sf,ux)
      do lam1=-1,1
        call mat_spinor(ux,g_rho_2(:,:,lam1),uy)
        do i=1,2
          call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
        end do
      end do
    enddo

    ! We are using the formfactors as Kanzo uses, which means: there is no energy dependence
    ! in due to the gauge invariance problem by coupling the photon
    fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq))**pc%npow
    ff=fi*fi
    !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
    if (imodel==4) then
      ff=(lamrr**2-normrr**2)/(lamrr**2+p1_cm(1)**2+p1_cm(3)**2)
    end if

    cc = ff * pc%gc
    do lam1=-1,1
      do lam2=-1,1
        do i=1,2
          hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
        end do
      end do
    end do

    return
  end subroutine dlam_ub

  !=======================================================================!

  subroutine dlsc_tv(hel,pc,ecms)  ! vector meson exchange in V B --> V B
    implicit none

    ! output
    complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
    ! input:
    type(couplings) :: pc
    complex(kind(0.d0)) :: ecms
    ! intermediate storage:
    complex(kind(0.d0)) :: p_t1(0:3),p_t2(0:3),pdum(0:3)
    complex(kind(0.d0)) :: pt_sq,psq
    complex(kind(0.d0)) :: e_t
    complex(kind(0.d0)),dimension(-1:1,-1:1,0:3) :: g_rrr1,g_rrr2,g_rrrct
    complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: g_rnn1,g_rnn2,sigmunil
    complex(kind(0.d0)) :: ux(1:4),uy(1:4)
    complex(kind(0.d0)),dimension(1:4,1:4,-1:1,-1:1) :: shelp
    complex(kind(0.d0)), dimension(1:4,1:4) :: gam0
    complex(kind(0.d0)), dimension(1:2) :: scalct1
    complex(kind(0.d0)), dimension(0:3,1:2) :: scalct2
    complex(kind(0.d0)) :: prop1,prop2
    complex(kind(0.d0)) :: cc,sdum
    integer :: i,is,js,lam1,lam2,mu,nu
    complex(kind(0.d0)) :: fi,ff   ! form factor

    do i=1,3
      p_t1 ( i ) = p1_cm ( i ) - p3_cm ( i )
      p_t2 ( i ) = p1_cm ( i ) - p3_cm ( i )
    end do
    pt_sq = p_t1(1)**2 + p_t1(3)**2   ! cm
    e_t   = cdsqrt(pc%wf**2+pt_sq)

    p_t1(0)= e_t
    p_t2(0)= -e_t

    call vertex_rhorhorho(g_rrr1,p_t1,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)
    !we need the outgoing rho
    call vertex_rhorhorho(g_rrr2,p_t2,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)
    !we need the outgoing rho
    call vertex_rhoNN(g_rnn1,p_t1,wlamc,pc%ka)

    call vertex_rhoNN(g_rnn2,p_t2,wsigc,pc%ka)

    ! idiag=2 calculates without q^mu q^nu term in the rho propagator
    ! as done in the thesis. idiag=1 calculates with q^muq^nu term and additional TOPT
    ! contact term. The difference is small and the changes in the fit
    ! can be absorbed in the N^*(1650) bare mass

    if (pc%idiag==2) then

      do lam1=-1,1
        do lam2=-1,1
          do is=1,4
            do js=1,4
              shelp(is,js,lam1,lam2)=(0.d0,0.d0)
              do mu=0,3
                ! without k^muk^nu term
                shelp(is,js,lam1,lam2)=shelp(is,js,lam1,lam2)-&
                    &g_rnn1(is,js,mu)*g_rrr1(lam1,lam2,mu)&
                    &/(ecms-e_t-p3_cm(0)-p2_cm(0))-&
                    &g_rnn2(is,js,mu)*g_rrr2(lam1,lam2,mu)&
                    &/(ecms-e_t-p1_cm(0)-p4_cm(0))
              end do
            end do
          end do
          call mat_spinor(ux,shelp(:,:,lam1,lam2),u1(:,1))
          do i=1,2
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          enddo
        end do
      end do

    else if (pc%idiag==1) then

      do lam1=-1,1
        do lam2=-1,1
          do is=1,4
            do js=1,4
              shelp(is,js,lam1,lam2)=(0.d0,0.d0)
              do mu=0,3
                ! with k^muk^nu term
                do nu=0,3
                  prop1=-gmunu(mu,nu)+p_t1(mu)*p_t1(nu)/masses(3,2)**2
                  prop2=-gmunu(mu,nu)+p_t2(mu)*p_t2(nu)/masses(3,2)**2
                  shelp(is,js,lam1,lam2)=shelp(is,js,lam1,lam2)+&
                      &g_rnn1(is,js,mu)*metric(mu)*prop1*g_rrr1(lam1,lam2,nu)&
                      !                          &*2.d0*e_t/((eps1-eps3)**2-e_t**2) !Fynm
                  &/(ecms-e_t-p3_cm(0)-p2_cm(0))+&
                      &g_rnn2(is,js,mu)*metric(mu)*prop2*g_rrr2(lam1,lam2,nu)&
                      &/(ecms-e_t-p1_cm(0)-p4_cm(0))
                end do
              end do
            end do
          end do
          call mat_spinor(ux,shelp(:,:,lam1,lam2),u1(:,1))
          do i=1,2
            call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
          enddo
        end do
      end do

      ! get vertices for ct terms:
      call vertex_rhoNNct(gam0)

      pdum(0)=(1.d0,0.d0)
      call vertex_pind13A(sigmunil,pdum)

      pdum=(0.d0,0.d0)
      call vertex_rhorhorho(g_rrrct,pdum,p2_cm,-1.d0*p4_cm,eps_mp,eps_mp_star)

      call mat_spinor(ux,gam0,u1(:,1))
      do i=1,2
        call spinor_bar_spinor(scalct1(i),u3(:,i),ux)
      end do
      do mu=0,3
        call mat_spinor(ux,sigmunil(:,:,mu),u1(:,1))
        do i=1,2
          call spinor_bar_spinor(scalct2(mu,i),u3(:,i),ux)
        end do
      end do

    endif

    ff=((pc%al1**2-pc%aw**2)/(pc%al1**2+pt_sq))**pc%npow
    fi=((pc%al2**2-pc%aw**2)/(pc%al2**2+pt_sq))**pc%npow
    ff=ff*fi
    !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
    if (imodel==4) then
      ff=(lamrr**2-normrr**2)/(lamrr**2+p1_cm(1)**2+p1_cm(3)**2)
    end if

    cc = ff * pc%gc / (2.d0*e_t)
    do lam1=-1,1
      do lam2=-1,1
        do i=1,2
          hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
        end do
      end do
    end do
    ! additional TOPT contact terms
    if (pc%idiag==1) then
      do lam1=-1,1
        do i=1,2
          sdum=(0.d0,0.d0)
          do mu=0,3
            sdum=sdum+scalct2(mu,i)*eps_mp_star(0,lam1)
          end do
          do lam2=-1,1
            hel(i,lam2,lam1)=hel(i,lam2,lam1)+&
                &scalct1(i)*g_rrrct(lam1,lam2,0)/masses(3,2)**2*ff*pc%gc&
                &+sdum*eps_mp_star(0,lam2)*ff*pc%gc*pc%ka/2.d0/masses(6,2)
          end do
        end do
      end do
    end if

    return
  end subroutine dlsc_tv

  !=======================================================================!

  subroutine dlsc_ub(hel,pc,ecms)  ! N-exchange exchange in V B --> V B
    implicit none

    ! output
    complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
    ! input:
    type(couplings) :: pc
    complex(kind(0.d0)) :: ecms
    ! intermediate storage:
    complex(kind(0.d0)) :: p_u(0:3)
    complex(kind(0.d0)) :: pu_sq,psq
    complex(kind(0.d0)) :: e_u
    complex(kind(0.d0)) :: propa
    complex(kind(0.d0)),dimension(1:4,1:4) :: sf,sfpos,sfneg
    complex(kind(0.d0)),dimension(1:4,1:4,-1:1) :: g_rho_4,g_rho_2
    complex(kind(0.d0)) :: ux(1:4)
    complex(kind(0.d0)) :: uy(1:4)
    complex(kind(0.d0)) :: cc
    integer :: i,lam1,lam2
    complex(kind(0.d0)) :: aux
    complex(kind(0.d0)) :: fi,ff   ! form factor

    p_u(0)=p1_cm(0)-p4_cm(0)    ! on-mass shell
    do i=1,3
      p_u ( i ) = p1_cm ( i ) - p4_cm ( i )
    end do
    pu_sq = p_u(1)**2 + p_u(3)**2     ! cm
    e_u   = cdsqrt(pc%wf**2+pu_sq)

    p_u(0)=e_u
    call psl_m(sfpos,p_u,pc%wf)
    p_u(0)=-e_u
    call psl_m(sfneg,p_u,pc%wf)

    sf = ( sfneg/ ( ecms - e_u - p1_cm(0) - p3_cm(0) )&
        &+sfpos/ ( ecms - e_u - p2_cm(0) - p4_cm(0) ) ) /( 2.0 * e_u )

    call vertex_rhoNN2(g_rho_2,-1.d0*p2_cm,eps_mp,wlamc,pc%ka)

    call vertex_rhoNN2(g_rho_4,p4_cm,eps_mp_star,wsigc,pc%ka)

    do lam2=-1,1
      call mat_spinor(ux,g_rho_4(:,:,lam2),u1(:,1))
      call mat_spinor(uy,sf,ux)
      do lam1=-1,1
        call mat_spinor(ux,g_rho_2(:,:,lam1),uy)
        do i=1,2
          call spinor_bar_spinor(hel(i,lam2,lam1),u3(:,i),ux)
        end do
      end do
    enddo

    ! We are using the formfactors as Kanzo uses, which means: there is no energy dependence
    ! in due to the gauge invariance problem by coupling the photon
    fi=((pc%al1**2-pc%aw**2)/(pc%al1**2+pu_sq))**pc%npow
    ff=fi*fi
    !     if(imodel==3) call ff_3(ff,p_u,2,1,2,1,2)
    if (imodel==4) then
      ff=(lamrr**2-normrr**2)/(lamrr**2+p1_cm(1)**2+p1_cm(3)**2)
    end if

    cc = ff * pc%gc
    do lam1=-1,1
      do lam2=-1,1
        do i=1,2
          hel(i,lam2,lam1)=hel(i,lam2,lam1)*cc
        end do
      end do
    end do

    return
  end subroutine dlsc_ub

  !=======================================================================!

  subroutine d_tv(hel,pc,ecms)  ! vector meson exchange in P B --> V B
      implicit none

      complex(kind(0.d0)) :: hel(1:2,-1:1)
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: p_tsq
      complex(kind(0.d0)) :: omega_t,prop1,prop2
      complex(kind(0.d0)) :: ff,fi,cc
      complex(kind(0.d0)) :: g_pro (0:3,-1:1)
      complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: grnn1,grnn2
      complex(kind(0.d0)),dimension(1:4) :: ux,uy
      complex(kind(0.d0)),dimension(1:2,0:3) :: shel1,shel2
      integer :: ihel,i,mu,lam

      do i=1,3
        p_t1(i)=p1_cm(i)-p3_cm(i)
        p_t2(i)=p1_cm(i)-p3_cm(i)
      end do

      p_tsq = p_t1(1)**2+p_t1(2)**2+p_t1(3)**2

      omega_t = cdsqrt( p_tsq + pc%wf**2)
      p_t1(0) = omega_t
      p_t2(0) = -omega_t

      ff = ((pc%al1**2 - pc%aw**2)/(pc%al1**2 + p_tsq))**pc%npow
      fi = ((pc%al2**2 - pc%aw**2)/(pc%al2**2 + p_tsq))**pc%npow
      cc = ff*fi*pc%gc

      call vertex_pirhoomega(g_pro,p4_cm,p2_cm,eps_mp_star) ! checked only -g_mu_nu term

      call vertex_rhoNN(grnn1,p_t1,wlam,pc%ka) ! used for charmed cases, just choose wlam now
      call vertex_rhoNN(grnn2,p_t2,wlam,pc%ka)

      do mu=0,3
        call mat_spinor(ux,grnn1(:,:,mu),u1(:,1))
        call mat_spinor(uy,grnn2(:,:,mu),u1(:,1))
        do i=1,2
          call spinor_bar_spinor(shel1(i,mu),u3(:,i),ux)
          call spinor_bar_spinor(shel2(i,mu),u3(:,i),uy)
        end do
      end do

      prop1 = 1.d0/(ecms - omega_t - p2_cm(0) - p3_cm(0))/(2.d0*omega_t)
      prop2 = 1.d0/(ecms - omega_t - p1_cm(0) - p4_cm(0))/(2.d0*omega_t)

      do lam=-1,1
        do ihel=1,2
          hel(ihel,lam)=(0.d0,0.d0)
          do mu=0,3
            hel(ihel,lam)=hel(ihel,lam)+(shel1(ihel,mu)*g_pro(mu,lam)*prop1&
                &+shel2(ihel,mu)*g_pro(mu,lam)*prop2)*cc
          end do
        end do
      end do

      return
  end subroutine d_tv

  !=======================================================================!

  subroutine d_tp(hel,pc,ecms)  ! pseudoscalar meson exchange in P B --> V B
      implicit none

      complex(kind(0.d0)) :: hel(1:2,-1:1)
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: p_tsq
      complex(kind(0.d0)) :: omega_t,prop1,prop2
      complex(kind(0.d0)) :: ff,fi,cc
      complex(kind(0.d0)),dimension(1:4,1:4) :: g_pv_t1,g_pv_t2
      complex(kind(0.d0)),dimension(-1:1) :: g_ppr1,g_ppr2
      complex(kind(0.d0)),dimension(1:4) :: ux,uy
      complex(kind(0.d0)),dimension(1:2) :: shel1,shel2
      integer :: ihel,i,lam

      do i=1,3
        p_t1(i)=p1_cm(i)-p3_cm(i)
        p_t2(i)=p1_cm(i)-p3_cm(i)
      end do

      p_tsq = p_t1(1)**2+p_t1(2)**2+p_t1(3)**2

      omega_t = cdsqrt( p_tsq + pc%wf**2)
      p_t1(0) = omega_t
      p_t2(0) = -omega_t

      ff = ((pc%al1**2 - pc%aw**2)/(pc%al1**2 + p_tsq))**pc%npow
      fi = ((pc%al2**2 - pc%aw**2)/(pc%al2**2 + p_tsq))**pc%npow
      cc = ff*fi*pc%gc

      call vertex_pv(g_pv_t1,-1.d0*p_t1)  ! i*gamma_5*Slash[pt]
      call vertex_pv(g_pv_t2,-1.d0*p_t2)

      call vertex_pipirho(g_ppr1,p2_cm,-1.d0*p_t1,eps_mp_star)  ! i*eps_mp_star[mu]*(p2-pt)[mu]
      call vertex_pipirho(g_ppr2,p2_cm,-1.d0*p_t2,eps_mp_star)

      call mat_spinor(ux,g_pv_t1,u1(:,1))
      call mat_spinor(uy,g_pv_t2,u1(:,1))

      do ihel=1,2
        call spinor_bar_spinor(shel1(ihel),u3(:,ihel),ux)
        call spinor_bar_spinor(shel2(ihel),u3(:,ihel),uy)
      end do

      prop1 = 1.d0/(ecms - omega_t - p2_cm(0) - p3_cm(0))/(2.d0*omega_t)
      prop2 = 1.d0/(ecms - omega_t - p1_cm(0) - p4_cm(0))/(2.d0*omega_t)

      do lam=-1,1
        do ihel=1,2
          hel(ihel,lam)=(shel1(ihel)*g_ppr1(lam)*prop1&
              &+shel2(ihel)*g_ppr2(lam)*prop2)*cc
        end do
      end do

      return
  end subroutine d_tp

  !=======================================================================!

  subroutine d_ub(hel,pc,ecms)  ! pseudoscalar meson exchange in P B --> V B
      implicit none

      complex(kind(0.d0)) :: hel(1:2,-1:1)
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      complex(kind(0.d0)),dimension(0:3) :: p_u1,p_u2
      complex(kind(0.d0)) :: p_usq
      complex(kind(0.d0)) :: e_u,prop1,prop2
      complex(kind(0.d0)) :: ff,fi,cc
      complex(kind(0.d0)),dimension(1:4,1:4) :: g_pv_2
      complex(kind(0.d0)),dimension(1:4,1:4) :: sf,sf1,sf2
      complex(kind(0.d0)),dimension(1:4,1:4,-1:1) :: g_rho_4
      complex(kind(0.d0)),dimension(1:4) :: ux,uy,uz
      integer :: ihel,i,lam

      do i=1,3
        p_u1(i)=p1_cm(i)-p4_cm(i)
        p_u2(i)=p1_cm(i)-p4_cm(i)
      end do

      p_usq = p_u1(1)**2+p_u1(2)**2+p_u1(3)**2

      e_u = cdsqrt(p_usq + pc%wf**2)
      p_u1(0) = e_u
      p_u2(0) = -e_u

      ff = ((pc%al1**2 - pc%aw**2)/(pc%al1**2 + p_usq))**pc%npow
      fi = ((pc%al2**2 - pc%aw**2)/(pc%al2**2 + p_usq))**pc%npow
      cc = ff*fi*pc%gc

      prop1 = 1.d0/(ecms - e_u - p2_cm(0) - p4_cm(0))/(2.d0*e_u)
      prop2 = 1.d0/(ecms - e_u - p1_cm(0) - p3_cm(0))/(2.d0*e_u)

      call vertex_pv(g_pv_2,p2_cm)  ! -i*gamma_5*Slash[p2]

      call psl_m(sf1,p_u1,pc%wf)
      call psl_m(sf2,p_u2,pc%wf)
      sf = (sf1*prop1 + sf2*prop2)*cc

      call vertex_rhoNN2(g_rho_4,p4_cm,eps_mp_star,wlam,pc%ka)  ! used for charmed cases, just choose wlam now

      do lam=-1,1
        call mat_spinor(ux,g_rho_4(:,:,lam),u1(:,1))
        call mat_spinor(uy,sf,ux)
        call mat_spinor(uz,g_pv_2,uy)
        do ihel=1,2
          call spinor_bar_spinor(hel(ihel,lam),u3(:,ihel),uz)
        end do
      end do

      return
  end subroutine d_ub

  !=======================================================================!

  subroutine dd_tv(hel,pc,ecms)  ! vector meson exchange in V B --> V B
      implicit none

      complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: p_tsq
      complex(kind(0.d0)) :: omega_t,prop1,prop2
      complex(kind(0.d0)) :: ff,fi,cc
      complex(kind(0.d0)),dimension(1:4,1:4,0:3) :: grnn1,grnn2
      complex(kind(0.d0)),dimension(-1:1,-1:1,0:3) :: g_rrr1,g_rrr2
      complex(kind(0.d0)),dimension(1:4) :: ux,uy
      complex(kind(0.d0)),dimension(1:2,0:3) :: shel1,shel2
      real(kind(0.0d0)), dimension(0:3) :: metric1
      integer :: ihel,i,mu,nu,lam1,lam2

      metric1 = (/1.0d0,-1.0d0,-1.0d0,-1.0d0/)

      do i=1,3
        p_t1(i)=p1_cm(i)-p3_cm(i)
        p_t2(i)=p1_cm(i)-p3_cm(i)
      end do

      p_tsq = p_t1(1)**2+p_t1(2)**2+p_t1(3)**2

      omega_t = cdsqrt( p_tsq + pc%wf**2)
      p_t1(0) = omega_t
      p_t2(0) = -omega_t

      ff = ((pc%al1**2 - pc%aw**2)/(pc%al1**2 + p_tsq))**pc%npow
      fi = ((pc%al2**2 - pc%aw**2)/(pc%al2**2 + p_tsq))**pc%npow
      cc = ff*fi*pc%gc

      call vertex_rhoNN(grnn1,p_t1,wlam,pc%ka) ! used for charmed cases, just choose wlam now
      call vertex_rhoNN(grnn2,p_t2,wlam,pc%ka)

      do lam1=-1,1
        do lam2=-1,1
          do mu=0,3
            g_rrr1(lam2,lam1,mu)=(0.d0,0.d0)
            g_rrr2(lam2,lam1,mu)=(0.d0,0.d0)
            do nu=0,3
              g_rrr1(lam2,lam1,mu)=g_rrr1(lam2,lam1,mu)+&
                  &eps_mp(nu,lam1)*eps_mp_star(mu,lam2)*(p4_cm(nu)+p_t1(nu))*metric1(nu)&
                  &-eps_mp(nu,lam1)*eps_mp_star(nu,lam2)*(p2_cm(mu)+p4_cm(mu))*metric1(nu)&
                  &+eps_mp(mu,lam1)*eps_mp_star(nu,lam2)*(p2_cm(nu)-p_t1(nu))*metric1(nu)
              g_rrr2(lam2,lam1,mu)=g_rrr2(lam2,lam1,mu)+&
                  &eps_mp(nu,lam1)*eps_mp_star(mu,lam2)*(p4_cm(nu)+p_t2(nu))*metric1(nu)&
                  &-eps_mp(nu,lam1)*eps_mp_star(nu,lam2)*(p2_cm(mu)+p4_cm(mu))*metric1(nu)&
                  &+eps_mp(mu,lam1)*eps_mp_star(nu,lam2)*(p2_cm(nu)-p_t2(nu))*metric1(nu)
            end do
          end do
        end do
      end do

      do mu=0,3
        call mat_spinor(ux,grnn1(:,:,mu),u1(:,1))
        call mat_spinor(uy,grnn2(:,:,mu),u1(:,1))
        do i=1,2
          call spinor_bar_spinor(shel1(i,mu),u3(:,i),ux)
          call spinor_bar_spinor(shel2(i,mu),u3(:,i),uy)
        end do
      end do

      prop1 = 1.d0/(ecms - omega_t - p2_cm(0) - p3_cm(0))/(2.d0*omega_t)
      prop2 = 1.d0/(ecms - omega_t - p1_cm(0) - p4_cm(0))/(2.d0*omega_t)

      do lam1=-1,1
        do lam2=-1,1
          do ihel=1,2
            hel(ihel,lam2,lam1)=(0.d0,0.d0)
            do mu=0,3
              hel(ihel,lam2,lam1)=hel(ihel,lam2,lam1)-ci*&
                  &(shel1(ihel,mu)*g_rrr1(lam2,lam1,mu)*metric1(mu)*prop1&
                  &+shel2(ihel,mu)*g_rrr2(lam2,lam1,mu)*metric1(mu)*prop2)*cc
            end do
          end do
        end do
      end do

      return
  end subroutine dd_tv

  !=======================================================================!

  subroutine dd_tp(hel,pc,ecms)  ! pseudoscalar meson exchange in V B --> V B
      implicit none

      complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      complex(kind(0.d0)),dimension(0:3) :: p_t1,p_t2
      complex(kind(0.d0)) :: p_tsq
      complex(kind(0.d0)) :: omega_t,prop1,prop2
      complex(kind(0.d0)) :: ff,fi,cc
      complex(kind(0.d0)),dimension(1:4,1:4) :: g_pv_t1,g_pv_t2
      complex(kind(0.d0)),dimension(0:3,-1:1) :: g_ppr
      complex(kind(0.d0)) :: gpro (-1:1,-1:1)
      complex(kind(0.d0)),dimension(1:4) :: ux,uy
      complex(kind(0.d0)),dimension(1:2) :: shel1,shel2
      integer :: ihel,i,lam1,lam2,nu

      do i=1,3
        p_t1(i)=p1_cm(i)-p3_cm(i)
        p_t2(i)=p1_cm(i)-p3_cm(i)
      end do

      p_tsq = p_t1(1)**2+p_t1(2)**2+p_t1(3)**2

      omega_t = cdsqrt( p_tsq + pc%wf**2)
      p_t1(0) = omega_t
      p_t2(0) = -omega_t

      ff = ((pc%al1**2 - pc%aw**2)/(pc%al1**2 + p_tsq))**pc%npow
      fi = ((pc%al2**2 - pc%aw**2)/(pc%al2**2 + p_tsq))**pc%npow
      cc = ff*fi*pc%gc

      call vertex_pv(g_pv_t1,-1.d0*p_t1)  ! i*gamma_5*Slash[pt]
      call vertex_pv(g_pv_t2,-1.d0*p_t2)

      ! epsilon_mu_nu_la_ta * p2^mu * eps_2^nu * p4^la
      call vertex_pirhoomega(g_ppr,p2_cm,p4_cm,eps_mp)
      ! epsilon_mu_nu_la_ta * p2^mu * eps_2^nu * p4^la * eps_star4^ta
      do lam1=-1,1
        do lam2=-1,1
          gpro(lam2,lam1)=(0.d0,0.d0)
          do nu=0,3
            gpro(lam2,lam1) = gpro(lam2,lam1) + g_ppr(nu,lam1)*eps_mp_star(nu,lam2)
          end do
        end do
      end do

      call mat_spinor(ux,g_pv_t1,u1(:,1))
      call mat_spinor(uy,g_pv_t2,u1(:,1))

      do ihel=1,2
        call spinor_bar_spinor(shel1(ihel),u3(:,ihel),ux)
        call spinor_bar_spinor(shel2(ihel),u3(:,ihel),uy)
      end do

      prop1 = 1.d0/(ecms - omega_t - p2_cm(0) - p3_cm(0))/(2.d0*omega_t)
      prop2 = 1.d0/(ecms - omega_t - p1_cm(0) - p4_cm(0))/(2.d0*omega_t)

      do lam1=-1,1
        do lam2=-1,1
          do ihel=1,2
            hel(ihel,lam2,lam1)=(shel1(ihel)*gpro(lam2,lam1)*prop1&
                &+shel2(ihel)*gpro(lam2,lam1)*prop2)*cc
          end do
        end do
      end do

      return
  end subroutine dd_tp

  !=======================================================================!

  subroutine dd_ub(hel,pc,ecms)  ! pseudoscalar meson exchange in V B --> V B
      implicit none

      complex(kind(0.d0)) :: hel(1:2,-1:1,-1:1)
      type(couplings) :: pc
      complex(kind(0.d0)) :: ecms
      complex(kind(0.d0)),dimension(0:3) :: p_u1,p_u2
      complex(kind(0.d0)) :: p_usq
      complex(kind(0.d0)) :: e_u,prop1,prop2
      complex(kind(0.d0)) :: ff,fi,cc
      complex(kind(0.d0)),dimension(1:4,1:4) :: sf,sf1,sf2
      complex(kind(0.d0)),dimension(1:4,1:4,-1:1) :: g_rho_2,g_rho_4
      complex(kind(0.d0)),dimension(1:4) :: ux,uy,uz
      integer :: ihel,i,lam1,lam2

      do i=1,3
        p_u1(i)=p1_cm(i)-p4_cm(i)
        p_u2(i)=p1_cm(i)-p4_cm(i)
      end do

      p_usq = p_u1(1)**2+p_u1(2)**2+p_u1(3)**2

      e_u = cdsqrt(p_usq + pc%wf**2)
      p_u1(0) = e_u
      p_u2(0) = -e_u

      ff = ((pc%al1**2 - pc%aw**2)/(pc%al1**2 + p_usq))**pc%npow
      fi = ((pc%al2**2 - pc%aw**2)/(pc%al2**2 + p_usq))**pc%npow
      cc = ff*fi*pc%gc

      prop1 = 1.d0/(ecms - e_u - p2_cm(0) - p4_cm(0))/(2.d0*e_u)
      prop2 = 1.d0/(ecms - e_u - p1_cm(0) - p3_cm(0))/(2.d0*e_u)

      call vertex_rhoNN2(g_rho_2,-1.d0*p2_cm,eps_mp,wlam,pc%ka)  ! used for charmed cases, just choose wlam now

      call psl_m(sf1,p_u1,pc%wf)
      call psl_m(sf2,p_u2,pc%wf)
      sf = (sf1*prop1 + sf2*prop2)*cc

      call vertex_rhoNN2(g_rho_4,p4_cm,eps_mp_star,wlam,pc%ka)  ! used for charmed cases, just choose wlam now

      do lam2=-1,1
        call mat_spinor(ux,g_rho_4(:,:,lam2),u1(:,1))
        call mat_spinor(uy,sf,ux)
        do lam1=-1,1
          call mat_spinor(uz,g_rho_2(:,:,lam1),uy)
          do ihel=1,2
            call spinor_bar_spinor(hel(ihel,lam2,lam1),u3(:,ihel),uz)
          end do
        end do
      end do

      return
  end subroutine dd_ub

  !=======================================================================!

end module potential3
