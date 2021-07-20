!=======================================================================!
! channel index:
!  (1)  N_pi        1
!  (2)  N_rho       2:(s=1/2, l=j+-1/2)   3:(s=3/2, l=j+-1/2)   4:(s=3/2, l=j+-3/2)
!  (3)  N_eta       5
!  (4)  Delta_pi    6:(s=3/2, l=j+-1/2)   7:(s=3/2, l=j+-3/2)
!  (5)  N_sigma     8
!  (6)  N_omega     9-11  ! not used
!  (7)  Lambda_K   12
!  (8)  Sigma_K    13
!  (9)  Lambdac_D  14
! (10)  Sigmac_D   15
! (11)  Lambdac_Ds 16:(s=1/2, l=j+-1/2)  17:(s=3/2, l=j+-1/2)  18:(s=3/2, l=j+-3/2)
! (12)  Sigmac_Ds  19:(s=1/2, l=j+-1/2)  20:(s=3/2, l=j+-1/2)  21:(s=3/2, l=j+-3/2)
!=======================================================================!
! qon(i): on shell momentum for 21 channels (energy fixed)
!=======================================================================!
! Each partial wave is determined by k_j and iv, for N_pi channel:
!   iv=1: I=1/2, l=j-1/2;  (j=1/2, S11;  j=3/2, P13;  j=5/2, D15)
!   iv=2: I=1/2, l=j+1/2;  (j=1/2, P11;  j=3/2, D13;  j=5/2, F15)
!   iv=3: I=3/2, l=j-1/2;  (j=1/2, S31;  j=3/2, P33;  j=5/2, D35)
!   iv=4: I=3/2, l=j+1/2;  (j=1/2, P31;  j=3/2, D33;  j=5/2, F35)
!=======================================================================!

module input4
  use steuerung
  integer,parameter :: max_j=5    ! maximal angular momentum; k_j=j+1/2; i.e. j_max=9/2
  character*16 :: bgd(1:4,max_j)  ! storage for background tau matrix and phase shifts
  character*16 :: res(1:4,max_j)  ! storage for full tau matrix and phase shifts

  ! quantum numbers (2j, parity, isospin) of partial waves: qn_xxx(iv,k_j)
  integer :: qn_2j(1:4,max_j),qn_par(1:4,max_j),qn_2iso(1:4,max_j)

  ! t-matrix, vertices, self energies, v-matrix
  integer,parameter :: max_q=26 ! maximal number of mesh points in momentum space +1 (for on-shell point)
  integer,parameter :: max_e=100 ! maximal number of points of scattering energy
  integer,parameter :: max_t=48 ! maximal number of mesh points for angular integral
  integer,parameter :: chdim=12  ! number of channels
  integer,parameter :: n_c=21   ! maximal number of channels with definit quantum numbers
  integer,parameter :: n_r=3    ! maximal number of resonances per partial wave
  complex(kind(0.d0)), dimension(4,n_c,n_c,max_q,max_q) :: vmatrix ! potential (Goldberg-Watson convention)
  complex(kind(0.d0)), dimension(max_q,max_q,4,n_c,n_c) :: tmatrix ! T matrix (Goldberg-Watson convention)
  complex(kind(0.d0)), dimension(max_j,4,max_e,n_c,n_c) :: ptnmat  ! potential (on-shell) in JLS basis
  real   (kind(0.d0)), dimension(max_j,4,max_e,n_c,n_c) :: del,eta ! phase shifts and inelasticities
  complex(kind(0.d0)), dimension(max_j,4,max_e,n_c,n_c) :: taumat, taumatnp  ! tau matirx (on-shell) in JLS basis
  real   (kind(0.d0)), dimension(max_j,4,max_e,n_c,n_c) :: delglo,etaglo ! phase shifts and inelasticities
  complex(kind(0.d0)), dimension(max_j,4,max_e,n_c,n_c) :: taumatglo, taumatglonp  ! tau matirx (on-shell) in JLS basis
  ! j   ! iv    ! initial channel
  complex(kind(0.d0)), dimension(max_q,max_q,n_c,n_c,max_e,4,max_j) :: store_t !Array to store the t-matrix for different energies. (label:maketables)
  ! Note: the first two entries should be max_e and max_j, the last two
  ! should be max_q and max_q, but we make it smaller by hand, sue to
  ! memory reasons.
  complex(kind(0.d0)), dimension(max_q,max_q,n_c,n_c,4,max_j) :: ramtab !Array to store the t-matrix for different energies. (label:maketables)
  complex(kind(0.d0)), dimension(max_q,n_c,n_c,4,max_j) :: T_transport

  ! s-channel vertices and self energy
  complex(kind(0.d0)) :: f_bar_c(n_r,n_c,max_q),f_bar_a(n_c,max_q,n_r)
  complex(kind(0.d0)) :: f_ind_c(n_r,n_c,max_q),f_ind_a(n_c,max_q,n_r)
  complex(kind(0.d0)) :: f_drs_c(n_r,n_c,max_q),f_drs_a(n_c,max_q,n_r)
  complex(kind(0.d0)) :: slf(n_r,n_r)     ! self energy with quantum numbers <--iv,k_j
  complex(kind(0.d0)) :: res_prop(n_r,n_r),res_det
  integer             :: max_nr(4,max_j)  !  max_nr(iv,k_j)= number of resonances
  !  with quantum numbers <--iv,k_j

  ! s-channel t-matrix, pole contribution
  complex(kind(0.d0)), dimension(max_q,max_q,4,n_c,n_c) :: tmatrix_pole
  complex(kind(0.d0)), dimension(4,n_c,n_c) :: tmatrix_pole_short
  complex(kind(0.d0)), dimension(n_c,n_c,4,max_j) :: tshort


  ! s-channel poles: bare couplings
  real(kind(0.d0)) :: m_bare(4,max_j,n_r)          ! bare mass of resonance
  real(kind(0.d0)) :: g_bare(4,max_j,n_r,n_c)      ! bare coupling constant
  integer          :: opt_res_c(1:4,max_j,n_r,n_c) ! resonances can be omitted by opt_res_c =0 in in1.txt
  ! read in subroutine piN_pw_read

  integer          :: ct_on(4,max_j)		! Whether or not a PW has a contact term

  ! s-channel form factors
  real(kind(0.d0)) :: m_bare_ff(4,max_j,n_r,n_c)  ! bare mass of resonance
  real(kind(0.d0)) ::    lam_ff(4,max_j,n_r,n_c)  ! cut-off, used in form factor
  real(kind(0.d0)) ::      m_ff(4,max_j,n_r,n_c)  ! physical mass of resonance, used in form factor
  integer          ::   npow_ff(4,max_j,n_r,n_c)  ! power number in form factor

  ! reaction channels: fermion masses and meson masses
  real(kind(0.d0)) :: ms_f_c(n_c),ms_b_c(n_c)

  ! Inner organization:
  ! define type which contains all information for one exchange process
  type couplings
    real(kind(0.d0)) :: gc,ka,ka2,wf,al1,al2,aw
    integer :: npow,idiag
  end type couplings

  ! define type for the triple formfactors used by b.pearce (not used here)
  type tripleformfactor
    real(kind(0.d0)) :: al,aw
    integer :: npow
  end type tripleformfactor

  integer :: nread,kread,kwrite
  integer :: imodel     ! to select the type of form factors
  integer :: n,k_b,k_e  ! number of mesh points for momentum integration
  real(kind(0.d0)) :: cst,alpha,alpha_save      ! parameters used in momentum integration
  integer, dimension(78) :: itypmax  ! numbers of interaction diagrams for each scattering channel
  real(kind(0.d0)), dimension(10,2) :: masses  ! mass for baryons and mesons
  type(couplings), dimension(chdim,chdim,10) :: coupl
  type(tripleformfactor), dimension(6) :: trifo
  real(kind(0.d0)) :: gammacc,etanthres,lamrho,normrho,lamrr,normrr

  ! for the dispersion integrals (pi-pi)
  real(kind(0.d0)) :: imf0(24),imf1p(24),imf1m(24),t_dsp(24),w_dsp(24)
  real(kind(0.d0)) :: a0_cd,cut
  integer          :: nd   ! nd=24

  ! angular momentum and integration
  integer :: nt    ! number of mesh points for angles, read here in/readinput
  integer :: j1    ! j1=2*j_physical, filled in ccmn4/main
  real(kind(0.d0)) :: xg(max_t),wg(max_t),djw(max_t,-max_j:max_j,-max_j:max_j)
  ! legendre gauss point for angular momentum integration
  ! max_t=48                   ! max_j=5, i.e. j_max=9/2

  ! djw is filled by sub util.f90/rot_mat which is called in ccmn4/main &
  !  & for each total angular momentum in the k_j loop
  ! The mesh points xg and weights wg are filled here in /readinput after comment

  ! energy and momenta
  complex(kind(0.d0)), dimension(max_e) :: e_cm        ! total energy in barycentric system
  complex(kind(0.0d0)) :: zrem			   ! To transport the energy to the setting of the Gauss points!
  complex(kind(0.d0)), dimension(n_c) :: qon           ! on shell momentum for each channel (energy fixed)
  integer :: melab   ! number of energy points, read from cc_in, melab <= max_e
  complex(kind(0.d0)), dimension(max_q) :: xgaus,wgaus ! legendre gauss point for momentum integration
  ! independent on reaction channel

  ! two-body propagator, gauss points in momentum space; q_m_n
  complex(kind(0.d0)), dimension(n_c,max_q) :: u,xgau,ygau, g_pi0p, g_pipn  ! dependent on reaction channel
  ! legendre gauss point for momentum integration
  ! the last point is the on-shell momentum

  ! bare masses, needed for calculation the self energy in two-body propagators
  real(kind(0.d0)),parameter :: mbr=911.1d0     ! bare rho mass
  real(kind(0.d0)),parameter :: mbn=1021.926d0  ! bare nucleon mass
  real(kind(0.d0)),parameter :: mbd=1415.1d0    ! bare delta mass
  real(kind(0.d0)),parameter :: mbs=900.1d0     ! bare sigma mass

  ! for init pot
  integer :: ccrel(chdim,chdim)

  ! coefficients for the trafo from helicity basis to JLS basis
  real(kind(0.d0)), dimension(1,-1:1) :: d
  real(kind(0.d0)), dimension(2,-1:1) :: a
  real(kind(0.d0)), dimension(3,-1:1) :: b
  real(kind(0.d0)), dimension(3,-1:1) :: c
  real(kind(0.d0)), dimension(2,-1:1) :: e
  real(kind(0.d0)), dimension(2,-1:1) :: f

  ! constants
  real(kind(0.d0)) :: pi, tpi, fpi, pih, tpi3, rtpi3, uf

  integer :: saidmode, isoon

  real(kind(0.d0)) :: mn_phys,gpin_phys	! If multiple renormalizations are required
  real(kind(0.d0)),parameter :: del_ez=3.d0	! For renormalization

  real(kind(0.d0))		 :: mnu_phys, mpi_phys	! Physical masses for isospin breaking in photoproduction kernel.

  real(kind(0.d0)), dimension(30) :: gcoup_global  	! To make gcoup globally available

  !==================================================================================
  !
  !     Quantities related to complex plane
  !
  !==================================================================================


  complex(kind(0.0d0)) 			:: res_det_prime,res_prop_wo_det(n_r,n_r)
  ! Needed for polpaths
  integer, parameter    			:: n_poles = 30 	! Total number of resonance poles in all PW
  integer      				:: iv_memo,k_j_memo ! needed in cpole/shoot, search for poles in partial wave '*_memo'.
  integer 		    			:: irre
  integer,dimension(n_poles)		:: resno,iv_reso,k_j_reso
  real(kind(0.d0))				:: rezreso,imzreso,mass_pion,mass_pion2,f35ison,mrho_fac,msig_fac,mdel_fac
  complex(kind(0.d0)),dimension(n_poles)	:: inipole
  character(len=40),  dimension(n_poles)	:: res_name
  character(len= 2)                 	:: kk_name
  integer          	    			:: gofourthr ! 0 = 2nd sheet, 1 = 4th sheet rhoN
  integer			    		:: gofourthd ! 0 = 2nd sheet, 1 = 4th sheet piD
  integer          				:: manip_v   ! Introduce finite epsilon in some potentials v
  complex(kind(0.d0)),dimension(n_c)	:: test_trans_bra ! Evaluate transition branching ratio directly as test
  complex(kind(0.0d0))			:: qon_f_pa(n_c)



contains

  !=======================================================================!

  subroutine readinput

      implicit none
      integer :: i,ic,jc,im  ! frazer fulco
      integer :: itd
      integer :: k
      real(kind(0.d0)) :: dum   ! frazer fulco input
      real(kind(0.d0)) :: xdum(100),wdum(100) ! angular integration, intermed. store
      complex(kind(0.d0)) :: xdumc(100),wdumc(100)
      real(kind(0.d0)), dimension(30) :: gcoup  ! coupling constants < cc_in.txt
      character*99 :: atext, btext(n_c)
      real(kind(0.d0)) :: emin,del_e  ! energy loop
      integer :: iv,k_j,ires
      integer :: iv_0,k_j_0,o(chdim),in_3
      real(kind(0.d0)) :: mas_in,a_in1,a_in2,a_in3
      character(len=*),parameter :: fmtstr(12)=(/&                         !     '(a9,i<ic*7>,<chdim-ic>i7)'
          &                            '(a9,i7,11i7) ',&
          &                            '(a9,i14,10i7)',&
          &                            '(a9,i21,9i7) ',&
          &                            '(a9,i28,8i7) ',&
          &                            '(a9,i35,7i7) ',&
          &                            '(a9,i42,6i7) ',&
          &                            '(a9,i49,5i7) ',&
          &                            '(a9,i56,4i7) ',&
          &                            '(a9,i63,3i7) ',&
          &                            '(a9,i70,2i7) ',&
          &                            '(a9,i77,1i7) ',&
          &                            '(a9,i84)     '/)

      call constants_prep

      ! masses for baryons
      read(nread,'(a70)')atext;   write(kwrite,'(a70)')atext
      do i=1,7
        read (nread,'(f10.4,a60)')masses(i,1),atext
        write(kwrite,'(a7,i1,a3,f9.3,a60)')'masses(',i,',1)',masses(i,1),atext
      end do
      ! masses for mesons
      read(nread,'(a70)')atext;   write(kwrite,'(a70)')atext
      do i=1,10
        read (nread,'(f10.4,a60)')masses(i,2),atext
        write(kwrite,'(a7,i1,a3,f9.3,a60)')'masses(',i,',2)',masses(i,2),atext
      end do

      ! frazer fulco amplitudes for rho and sigma exchange
      read(nread,'(a70)')atext;        write(kwrite,'(a70)')atext
      read(nread,'(a6,i5)')atext,nd;   write(kwrite,'(a6,i5)')atext,nd
      read(nread,'(a70)')atext;        write(kwrite,'(a70)')atext
      do itd=1,nd
        read(nread,'(f9.4,3f13.6)')dum,imf0(itd),imf1p(itd),imf1m(itd)
        write(kwrite,'(f9.4,3f13.6)')dum,imf0(itd),imf1p(itd),imf1m(itd)
      end do

      call  dispersion_prep

      ! storage name & quantum numbers for resonances
      read(nread,'(a70)')atext;        write(kwrite,'(a70)')atext
      read(nread,'(a70)')atext;        write(kwrite,'(a70)')atext
      do k_j=1,max_j  ! k_j=(2j+1)/2
        do iv=1,4
          read(nread,'(2a16,i2,4i5)')res(iv,k_j),bgd(iv,k_j),qn_2j(iv,k_j),&
              &qn_par(iv,k_j),qn_2iso(iv,k_j),iv_0,k_j_0
          write(kwrite,'(2a16,i2,4i5)')res(iv,k_j),bgd(iv,k_j),qn_2j(iv,k_j),&
              &qn_par(iv,k_j),qn_2iso(iv,k_j),iv_0,k_j_0
        end do
      end do
      read(nread,'(a70)')atext;        write(kwrite,'(a70)')atext

      ! type of form factor, print control, piN -> rhoN transition control, bare sigma term
      read(kread,'(i5)')imodel;     write(kwrite,'(a10,i5)')'imodel',imodel
      read(kread,'(f8.3)')gammacc;  write(kwrite,'(a10,f9.3)')'gammacc',gammacc
      read(kread,'(f8.3)')a0_cd;    write(kwrite,'(a10,f9.3)')'a0_cd',a0_cd

      a0_cd=a0_cd/93.d0**2

      ! number of gauss points, angular momentum, stretching, contour
      read(kread,'(a90)')atext;                write(kwrite,'(a56)')atext
      read(kread,'(a10,i5)')atext,n;           write(kwrite,'(a10,i5)')atext,n
      read(kread,'(a10,i5)')atext,nt;          write(kwrite,'(a10,i5)')atext,nt
      read(kread,'(a10,2i5)')atext,k_b,k_e;    write(kwrite,'(a10,2i5)')atext,k_b,k_e
      read(kread,'(a10,6f10.4)')atext,cst;     write(kwrite,'(a10,6f10.4)')atext,cst
      read(kread,'(a10,6f10.4)')atext,alpha;   write(kwrite,'(a10,6f10.4)')atext,alpha

      alpha_save=alpha	! alpha may be adapted during complex plane mode of the
      ! program. alpha_save always contains the value read in
      ! from cc_in_XXX.txt

      ! energies
      read(kread,'(a90)')atext;       write(kwrite,'(a56)')atext
      read(kread,'(f8.2,a10)')emin;   write(kwrite,'(f8.2)')emin
      read(kread,'(f8.2)')del_e;      write(kwrite,'(f8.2)')del_e
      read(kread,'(i5)')melab;	      write(kwrite,'(i5)')melab
      if(melab.gt.max_e) write(*,*)'Error, too many energy points'

      ! coupling constants
      read(kread,'(a90)')atext;     write(kwrite,'(a90)')atext
      do i=1,24
        read(kread,'(f10.6,a60)')gcoup(i),atext;  write(kwrite,'(f10.6,a60)')gcoup(i),atext
        write(0,*)'i,gcoup(i):',i, gcoup(i)
      end do

      ! fill ccrel array
      call init_pot

      ! the notation of the potential-matrix is symmetric

      !   p  r  e  p  s  o  L  S
      !   i  h  t  i  i  m  a  i
      !   N  o  a  D  g  g  m  g
      !      N  N     N  N  K  K

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

      ! number of exchange processes for each channel
      read(kread,'(a80)')atext;     write(kwrite,'(a80)')atext
      read(kread,'(a100)')atext;     write(kwrite,'(a100)')atext
      do ic=1,chdim
        !            fmtstr='(a9,i<ic*7>,<chdim-ic>i7)'
        read (kread, fmtstr(ic))atext,(itypmax(ccrel(ic,jc)),jc=ic,chdim)
        write(kwrite,fmtstr(ic))atext,(itypmax(ccrel(ic,jc)),jc=ic,chdim)
      end do

      gcoup_global=gcoup			! Making gcoup globally available after
      ! it is read in from cc_in. This is needed
      ! for fitting T^NP. When using, don't forget
      ! to call coupl_prep(gcoup_global)


      ! build up the couplings for each process
      call  coupl_prep(gcoup)

      ! non-triple form factor parameters, masses, diagswitches
      do ic=1,chdim
        do jc=ic,chdim
          im=itypmax(ccrel(ic,jc))
          if(im==0) go to 123
          read(kread,'(a70)')atext;   write(kwrite,'(a70)')atext
          read(kread,'(a70)')atext
          write(kwrite,'(a73)')' coupling | kappa | mass res| lam ff1 &
              &| lam ff2 | mass ff |powf|dia| type '
          do i=1,im
            read(kread,'(4f10.4,2i5,a10)')coupl(ic,jc,i)%wf,coupl(ic,jc,i)%al1,coupl(ic,jc,i)%al2,&
                &coupl(ic,jc,i)%aw,coupl(ic,jc,i)%npow,coupl(ic,jc,i)%idiag,atext
            write(kwrite,'(f10.6,f8.4,4f10.4,2i4,a10)')coupl(ic,jc,i)%gc,coupl(ic,jc,i)%ka,&
                &coupl(ic,jc,i)%wf,coupl(ic,jc,i)%al1,coupl(ic,jc,i)%al2,coupl(ic,jc,i)%aw,&
                &coupl(ic,jc,i)%npow,coupl(ic,jc,i)%idiag,atext
          end do
        123   end do
      end do

      ! switch on/off resonance coupling to each channel
      opt_res_c=0;    max_nr=0  ! default: no coupling
      read(kread,'(a95)')atext;     write(kwrite,'(a95)')atext
      read(kread,'(a95)')atext;
      read(kread,'(a95)')atext;     write(kwrite,'(3a4,21i4)')'iv','j','res',(iv_0,iv_0=1,n_c)
      234   read(kread,'(15i5,a16)')iv,k_j,ires,(o(iv_0),iv_0=1,chdim),atext
      if(iv.ne.0)then
        opt_res_c(iv,k_j,ires,1)=o(1)    ! N pi
        do i=2,4
          opt_res_c(iv,k_j,ires,i)=o(2)  ! N rho
        end do
        opt_res_c(iv,k_j,ires,5)=o(3)    ! N eta
        do i=6,7
          opt_res_c(iv,k_j,ires,i)=o(4)  ! Delta pi
        end do
        opt_res_c(iv,k_j,ires,8)=o(5)    ! N sig
        do i=9,11
          opt_res_c(iv,k_j,ires,i)=o(6)  ! N omega
        end do
        opt_res_c(iv,k_j,ires,12)=o(7)   ! Lambda K
        opt_res_c(iv,k_j,ires,13)=o(8)   ! Sigma K
        opt_res_c(iv,k_j,ires,14)=o(9)   ! Lambdac D
        opt_res_c(iv,k_j,ires,15)=o(10)   ! Sigmac D
        do i=16,18
          opt_res_c(iv,k_j,ires,i)=o(11)   ! Lambdac Ds
        end do
        do i=19,21
          opt_res_c(iv,k_j,ires,i)=o(12)   ! Sigmac Ds
        end do
        write(kwrite,'(24i4,a16)')iv,k_j,ires,(opt_res_c(iv,k_j,ires,iv_0),iv_0=1,n_c),atext
        go to 234
      end if

      ! bare mass and coupling constants for each resonances
      read(kread,'(a99)')atext;     write(kwrite,'(a99)')atext
      read(kread,'(a99)')atext;     write(kwrite,'(a99)')atext
      g_bare=0.d0;   lam_ff = 2000.d0;   npow_ff=1
      345   read(kread,'(3i5,f10.4,a16)')iv,k_j,ires,mas_in,atext
      if(iv.ne.0)then
        m_bare(iv,k_j,ires)=mas_in
        max_nr(iv,k_j)=ires
        write(kwrite,'(3i5,f10.4,a16)')iv,k_j,ires,m_bare(iv,k_j,ires),atext
        do ic=1,chdim
          read(kread,'(d15.8,2f10.4,i5,a16)')a_in1,a_in2,a_in3,in_3,atext
          if(ic==1)then  ! N pi
            g_bare (iv,k_j,ires,1) = a_in1;   m_bare_ff(iv,k_j,ires,1) = m_bare(iv,k_j,ires)
            lam_ff (iv,k_j,ires,1) = a_in2;        m_ff(iv,k_j,ires,1) = a_in3
            npow_ff(iv,k_j,ires,1) = in_3;                    btext(1) = atext
          else if(ic==2)then  ! N rho
            do i=2,4
              g_bare (iv,k_j,ires,i) = a_in1;   m_bare_ff(iv,k_j,ires,i) = m_bare(iv,k_j,ires)
              lam_ff (iv,k_j,ires,i) = a_in2;        m_ff(iv,k_j,ires,i) = a_in3
              npow_ff(iv,k_j,ires,i) = in_3;                    btext(i) = atext
            end do
          else if(ic==3)then  ! N eta
            g_bare (iv,k_j,ires,5) = a_in1;   m_bare_ff(iv,k_j,ires,5) = m_bare(iv,k_j,ires)
            lam_ff (iv,k_j,ires,5) = a_in2;        m_ff(iv,k_j,ires,5) = a_in3
            npow_ff(iv,k_j,ires,5) = in_3;                    btext(5) = atext
          else if(ic==4)then  ! Delta pi
            do i=6,7
              g_bare (iv,k_j,ires,i) = a_in1;   m_bare_ff(iv,k_j,ires,i) = m_bare(iv,k_j,ires)
              lam_ff (iv,k_j,ires,i) = a_in2;        m_ff(iv,k_j,ires,i) = a_in3
              npow_ff(iv,k_j,ires,i) = in_3;                    btext(i) = atext
            end do
          else if(ic==5)then  ! N sigma
            g_bare (iv,k_j,ires,8) = a_in1;   m_bare_ff(iv,k_j,ires,8) = m_bare(iv,k_j,ires)
            lam_ff (iv,k_j,ires,8) = a_in2;        m_ff(iv,k_j,ires,8) = a_in3
            npow_ff(iv,k_j,ires,8) = in_3;                    btext(8) = atext
          else if(ic==6)then  ! N omega
            do i=9,11
              g_bare (iv,k_j,ires,i) = a_in1;   m_bare_ff(iv,k_j,ires,i) = m_bare(iv,k_j,ires)
              lam_ff (iv,k_j,ires,i) = a_in2;        m_ff(iv,k_j,ires,i) = a_in3
              npow_ff(iv,k_j,ires,i) = in_3;                    btext(i) = atext
            end do
          else if(ic==7)then  ! Lambda K
            g_bare (iv,k_j,ires,12) = a_in1;   m_bare_ff(iv,k_j,ires,12) = m_bare(iv,k_j,ires)
            lam_ff (iv,k_j,ires,12) = a_in2;        m_ff(iv,k_j,ires,12) = a_in3
            npow_ff(iv,k_j,ires,12)= in_3;                     btext(12) = atext
          else if(ic==8)then  ! Sigma K
            g_bare (iv,k_j,ires,13) = a_in1;   m_bare_ff(iv,k_j,ires,13) = m_bare(iv,k_j,ires)
            lam_ff (iv,k_j,ires,13) = a_in2;        m_ff(iv,k_j,ires,13) = a_in3
            npow_ff(iv,k_j,ires,13) = in_3;                    btext(13) = atext
          else if(ic==9)then  ! Lambdac D
            g_bare (iv,k_j,ires,14) = a_in1;   m_bare_ff(iv,k_j,ires,14) = m_bare(iv,k_j,ires)
            lam_ff (iv,k_j,ires,14) = a_in2;        m_ff(iv,k_j,ires,14) = a_in3
            npow_ff(iv,k_j,ires,14)= in_3;                     btext(14) = atext
          else if(ic==10)then  ! Sigmac D
            g_bare (iv,k_j,ires,15) = a_in1;   m_bare_ff(iv,k_j,ires,15) = m_bare(iv,k_j,ires)
            lam_ff (iv,k_j,ires,15) = a_in2;        m_ff(iv,k_j,ires,15) = a_in3
            npow_ff(iv,k_j,ires,15) = in_3;                    btext(15) = atext
          else if(ic==11)then  ! Lambdac Ds
            do i=16,18
              g_bare (iv,k_j,ires,i) = a_in1;   m_bare_ff(iv,k_j,ires,i) = m_bare(iv,k_j,ires)
              lam_ff (iv,k_j,ires,i) = a_in2;        m_ff(iv,k_j,ires,i) = a_in3
              npow_ff(iv,k_j,ires,i)= in_3;                     btext(i) = atext
            end do
          else if(ic==12)then  ! Sigmac Ds
            do i=19,21
              g_bare (iv,k_j,ires,i) = a_in1;   m_bare_ff(iv,k_j,ires,i) = m_bare(iv,k_j,ires)
              lam_ff (iv,k_j,ires,i) = a_in2;        m_ff(iv,k_j,ires,i) = a_in3
              npow_ff(iv,k_j,ires,i) = in_3;                    btext(i) = atext
            end do
          end if
        end do  ! ic
        do i=1,n_c
          write(kwrite,'(d15.8,3f10.4,2i5,a16)')g_bare(iv,k_j,ires,i), m_bare_ff(iv,k_j,ires,i),&
              &lam_ff(iv,k_j,ires,i),m_ff(iv,k_j,ires,i),npow_ff(iv,k_j,ires,i),i,btext(i)
        end do
        go to 345
      end if

      mn_phys   = m_bare(2,1,1)			! If multiple renormalizations are required.
      gpin_phys = g_bare(2,1,1,1)

      ! definition of reaction channels
      call ms_c_def  ! fermion and boson mass for each channel
      ! fix energies for each point to be calculated
      e_cm(1)=dsqrt((1.64e-2*uf)**2+masses(1,1)**2)
      e_cm(1)=e_cm(1)+dsqrt((1.64e-2*uf)**2+masses(1,2)**2)
      e_cm(1) = emin
      do k=2,melab
        e_cm(k) = emin + (k-1)*del_e
      end do

      ! mesh-points for momentum space integration
      !
      ! mesh-points for momentum space integration. For first call, provide some zrem here (is never really used, because inside rnm, it is again
      ! set to the nucleon mass)
      !
      zrem=dcmplx(masses(1,1),0.d0)
      call mesh_q

      ! legendre gauss point for angular momentum integration
      call lgauss(xdum,wdum,nt)
      do i=1,nt
        xg(i)=xdum(i)
        wg(i)=wdum(i)
      end do

      ! initialize factorials for small-d functions
      call fact

      close(kread)
      close(nread)

      ! Which partial waves have a contact term
      ct_on     =0
      ct_on(1,1)=0  ! S11
      ct_on(3,1)=0  ! S31
      ct_on(2,1)=0  ! P11
      ct_on(4,1)=0  ! P31
      ct_on(1,2)=0  ! P13
      ct_on(2,2)=0  ! D13
      ct_on(3,2)=0  ! P33
      ct_on(4,2)=0  ! D33
      ct_on(1,3)=0  ! D15
      ct_on(2,3)=0  ! F15
      ct_on(3,3)=0  ! D35
      ct_on(4,3)=0  ! F35
      ct_on(1,4)=0  ! F17
      ct_on(2,4)=0  ! G17
      ct_on(3,4)=0  ! F37
      ct_on(4,4)=0  ! G37
      ct_on(1,5)=0  ! G19
      ct_on(2,5)=0  ! H19
      ct_on(3,5)=0  ! G39
      ct_on(4,5)=0  ! H39

      !==================================================================================
      !
      !     Quantities related to complex plane
      !
      !==================================================================================

      ! READ DATA FOR POLPATHS, INITIAL POLPOSITIONS; OPEN analytic/polepaths/check_initial_polpos.txt (polepath)
      resno=0
      iv_reso=0
      k_j_reso=0
      inipole=dcmplx(0.d0,0.d0)
      open(unit=1123,file='analytic/polepaths/initial_polpos.txt',status='old') ! File with entries: res-number, iv, k_j, ReZ_0, ImZ_0>0 of poles at physical pion mass.
      do irre=1,n_poles
        read(1123,'(3i2,2f6.0,a2,a40)')resno(irre),iv_reso(irre),k_j_reso(irre),rezreso,imzreso,kk_name,res_name(irre)
        inipole(irre)=dcmplx(rezreso,imzreso)
      end do
      close(1123)
      open(unit=1124 ,file='analytic/polepaths/check_initial_polpos.txt',status='replace')
      open(unit=11124,file='analytic/polepaths/for_table/in_mathematica.dat',status='replace')

      open(unit=1131,file='analytic/polepaths/plot_polpos/N1535_S11.dat')
      open(unit=1132,file='analytic/polepaths/plot_polpos/N1650_S11.dat')
      open(unit=1133,file='analytic/polepaths/plot_polpos/N1440_P11.dat')
      open(unit=1134,file='analytic/polepaths/plot_polpos/D1620_S31.dat')
      open(unit=1135,file='analytic/polepaths/plot_polpos/D1910_P31.dat')
      open(unit=1136,file='analytic/polepaths/plot_polpos/N1720_P13.dat')
      open(unit=1137,file='analytic/polepaths/plot_polpos/N1520_D13.dat')
      open(unit=1138,file='analytic/polepaths/plot_polpos/D1232_P33.dat')
      open(unit=1139,file='analytic/polepaths/plot_polpos/D1700_D33.dat')

      open(unit=1141,file='analytic/polepaths/plot_resi/N1535_S11.dat')
      open(unit=1142,file='analytic/polepaths/plot_resi/N1650_S11.dat')
      open(unit=1143,file='analytic/polepaths/plot_resi/N1440_P11.dat')
      open(unit=1144,file='analytic/polepaths/plot_resi/D1620_S31.dat')
      open(unit=1145,file='analytic/polepaths/plot_resi/D1910_P31.dat')
      open(unit=1146,file='analytic/polepaths/plot_resi/N1720_P13.dat')
      open(unit=1147,file='analytic/polepaths/plot_resi/N1520_D13.dat')
      open(unit=1148,file='analytic/polepaths/plot_resi/D1232_P33.dat')
      open(unit=1149,file='analytic/polepaths/plot_resi/D1700_D33.dat')

      ! Write off shell coupling strengths:
      open(unit=1211,file='analytic/off_shell_resi/plot_resi/details/N1535_S11.dat')
      open(unit=1212,file='analytic/off_shell_resi/plot_resi/details/N1650_S11.dat')
      open(unit=1213,file='analytic/off_shell_resi/plot_resi/details/N1440_P11.dat')
      open(unit=1214,file='analytic/off_shell_resi/plot_resi/details/D1620_S31.dat')
      open(unit=1215,file='analytic/off_shell_resi/plot_resi/details/D1910_P31.dat')
      open(unit=1216,file='analytic/off_shell_resi/plot_resi/details/N1720_P13.dat')
      open(unit=1217,file='analytic/off_shell_resi/plot_resi/details/N1520_D13.dat')
      open(unit=1218,file='analytic/off_shell_resi/plot_resi/details/D1232_P33.dat')
      open(unit=1219,file='analytic/off_shell_resi/plot_resi/details/D1700_D33.dat')
      open(unit=1220,file='analytic/off_shell_resi/plot_resi/details/tests.dat')

      open(unit=1221,file='analytic/off_shell_resi/plot_resi/N1535_S11.dat')
      open(unit=1222,file='analytic/off_shell_resi/plot_resi/N1650_S11.dat')
      open(unit=1223,file='analytic/off_shell_resi/plot_resi/N1440_P11.dat')
      open(unit=1224,file='analytic/off_shell_resi/plot_resi/D1620_S31.dat')
      open(unit=1225,file='analytic/off_shell_resi/plot_resi/D1910_P31.dat')
      open(unit=1226,file='analytic/off_shell_resi/plot_resi/N1720_P13.dat')
      open(unit=1227,file='analytic/off_shell_resi/plot_resi/N1520_D13.dat')
      open(unit=1228,file='analytic/off_shell_resi/plot_resi/D1232_P33.dat')
      open(unit=1229,file='analytic/off_shell_resi/plot_resi/D1700_D33.dat')
      open(unit=1230,file='analytic/off_shell_resi/plot_resi/tests.dat')

      ! Open units for writing the amplitudes of all channels (onex)
      open(unit=1150,file='extrapolation/bgd/S11_bgd.dat')
      open(unit=1151,file='extrapolation/bgd/P11_bgd.dat')
      open(unit=1152,file='extrapolation/bgd/S31_bgd.dat')
      open(unit=1153,file='extrapolation/bgd/P31_bgd.dat')
      open(unit=1154,file='extrapolation/bgd/P13_bgd.dat')
      open(unit=1155,file='extrapolation/bgd/D13_bgd.dat')
      open(unit=1156,file='extrapolation/bgd/P33_bgd.dat')
      open(unit=1157,file='extrapolation/bgd/D33_bgd.dat')   ! Put here higher partial waves later

      open(unit=1170,file='extrapolation/res/S11_res.dat')
      open(unit=1171,file='extrapolation/res/P11_res.dat')
      open(unit=1172,file='extrapolation/res/S31_res.dat')
      open(unit=1173,file='extrapolation/res/P31_res.dat')
      open(unit=1174,file='extrapolation/res/P13_res.dat')
      open(unit=1175,file='extrapolation/res/D13_res.dat')
      open(unit=1176,file='extrapolation/res/P33_res.dat')
      open(unit=1177,file='extrapolation/res/D33_res.dat')

      open(unit=2190,file='extrapolation/pa/pa_N1535_S11.dat')
      open(unit=2191,file='extrapolation/pa/pa_N1650_S11.dat')
      open(unit=2192,file='extrapolation/pa/pa_N1440_P11.dat')
      open(unit=2193,file='extrapolation/pa/pa_N1710_P11.dat')
      open(unit=2194,file='extrapolation/pa/pa_NXXXX_P11.dat')
      open(unit=2195,file='extrapolation/pa/pa_D1620_S31.dat')
      open(unit=2196,file='extrapolation/pa/pa_D1910_P31.dat')
      open(unit=2197,file='extrapolation/pa/pa_N1720_P13.dat')
      open(unit=2198,file='extrapolation/pa/pa_N1520_D13.dat')
      open(unit=2199,file='extrapolation/pa/pa_D1232_P33.dat')
      open(unit=2200,file='extrapolation/pa/pa_D1600_P33.dat')
      open(unit=2201,file='extrapolation/pa/pa_D1920_P33.dat')
      open(unit=2202,file='extrapolation/pa/pa_D1700_D33.dat')
      open(unit=2203,file='extrapolation/pa/pa_N1675_D15.dat')
      open(unit=2204,file='extrapolation/pa/pa_N1680_F15.dat')
      open(unit=2205,file='extrapolation/pa/pa_D1930_D35.dat')
      open(unit=2206,file='extrapolation/pa/pa_D1905_F35.dat')
      open(unit=2207,file='extrapolation/pa/pa_N1990_F17.dat')
      open(unit=2208,file='extrapolation/pa/pa_N2190_G17.dat')
      open(unit=2209,file='extrapolation/pa/pa_D1950_F37.dat')
      open(unit=2210,file='extrapolation/pa/pa_D2200_G37.dat')
      open(unit=2211,file='extrapolation/pa/pa_N2250_G19.dat')
      open(unit=2212,file='extrapolation/pa/pa_N2220_H19.dat')
      open(unit=2213,file='extrapolation/pa/pa_N2400_G39.dat')
      open(unit=2214,file='extrapolation/pa/pa_notused1.dat')
      open(unit=2215,file='extrapolation/pa/pa_notused2.dat')
      open(unit=2216,file='extrapolation/pa/pa_notused3.dat')
      open(unit=2217,file='extrapolation/pa/pa_notused4.dat')
      open(unit=2218,file='extrapolation/pa/pa_notused5.dat')
      open(unit=2219,file='extrapolation/pa/pa_notused6.dat')

      mass_pion=ms_b_c(1)
      mass_pion2=mass_pion

      open(unit=1201,file='extrapolation/readable_residues.dat')
      open(unit=1202,file='extrapolation/processable_residues.dat')

      f35ison=0			! Signal that F35(1900) resonance is investiagted (this we tried before finding out that the pole was
      ! behind the rhoN cut, not the KSigma one. This is now not needed or used any more.).
      gofourthr=0			! Don't go to 4th rhoN sheet by default
      gofourthd=0			! Don't go to 4th piD  sheet by default
      mrho_fac=2.d0!770.d0/138.d0!2.d0			! rho mass in potentials V in analytic continuation
      msig_fac=2.d0!440.d0/138.d0
      mdel_fac=1.d0
      manip_v =0 			! Don't manipulate V's by default

      isoon=0
      if (isoon==0) then
        mnu_phys =938.272d0   	! For isospin breaking in photoproduction kernel; CAUTION: applies also to hadronic
        mpi_phys =134.976d0	      	! vertices. Thus, initialize with isospin averaged masses!
      else
        mnu_phys = ms_f_c(1)
        mpi_phys = ms_b_c(1)
      end if


  end subroutine readinput

  !=======================================================================!
  !=======================================================================!

  subroutine mesh_q
      implicit none
      integer :: i
      complex(kind(0.d0)) :: xdumc(100),wdumc(100),qon_pin,qon_Dlc
      real(kind(0.d0)) :: const
      complex(kind(0.0d0)), dimension(n_c) :: q_on_she
      if(max_q.gt.100) write(*,*)'Error, too many mesh points of q'
      !	Determine angle alpha according to the complex energy considered, zrem:
      ! 	If we are in renormalization mode, don't do anything, but use the value
      ! 	of alpha read in from cc_in.txt (only sufficiently away from piN threshold,
      !	the angle alpha is modified):
!      if (dble(zrem).gt.1180.d0.and.rsheet==2.and.adapt_alpha==1) then
      if (dble(zrem).gt.4250.d0.and.rsheet==2.and.adapt_alpha==1) then
        call qon_c(zrem,q_on_she)  ! on shell momenta; we need only the piN one here.
        qon_pin=q_on_she(1)
        qon_Dlc=q_on_she(14)
!        alpha=-(imag(qon_pin)-115.d0)/dble(qon_pin) ! For 3D plots of complex plane
        alpha=-(imag(qon_Dlc)-115.d0)/dble(qon_Dlc) ! For 3D plots of complex plane
      else
        alpha=alpha_save	! Makes sure that in non-complex sheet mode
        ! The alpha is used that was read in from
        ! cc_in_XXX.txt.
      end if

      !if (gauss_opt==1.and.dble(zrem)>1130.d0) then
      if (gauss_opt==1.and.dble(zrem)>1525.d0) then
        call gauss_tan_MOD2(n,cst,alpha,xdumc,wdumc)					! Alternative distribution of Gauss points
      else if (gauss_opt==1.and.dble(zrem)>1130.d0.and.dble(zrem)<1475.d0) then
        call gauss_tan_MOD2(n,cst,alpha,xdumc,wdumc)					! Alternative distribution of Gauss points
      else if (dble(zrem)<1130.d0.and.dble(zrem)>1000.d0) then
        call gauss_tan(n,100.d0,alpha,xdumc,wdumc)
      else if (dble(zrem)<1525.d0.and.dble(zrem)>1475.d0) then
        call gauss_tan(n,100.d0,alpha,xdumc,wdumc)
      else
        call gauss_tan(n,cst,alpha,xdumc,wdumc)
      end if
      !     write(kwrite,'(a4,4a12)') 'i','xgaus(i)','xgaus(i)','wgaus(i)','wgaus(i)'
      !      const=150.d0	! This is used for the solution final_12 for the K+S+ fit.
      const=0.d0
      do i=1,n
        xgaus(i)=xdumc(i)**2/(const+xdumc(i))						! Substitution, damit Numerik in s-welle okay
        wgaus(i)=wdumc(i)*(xdumc(i)**2+2.d0*xdumc(i)*const)/(const+xdumc(i))**2		! Additionally, propagators of stable channels
        !      xgaus(i)=xdumc(i)									! carry small but finite epsilon
        !      wgaus(i)=wdumc(i)
        !      write(kwrite,'(i4,4e12.4)') i,xgaus(i),wgaus(i)
      end do
  end subroutine mesh_q


  !=======================================================================!

  subroutine init_pot
      ! get relation between matrix notation (see input) and i1,i2

      implicit none
      integer ix,iy,count

      count=0
      do ix=1,chdim
        do iy=ix,chdim
          count=count+1
          ccrel(ix,iy)=count
          ccrel(iy,ix)=count
        end do
      end do

  end subroutine init_pot

  !=======================================================================!

  subroutine set_coeff_jls(jot)
      ! this routine sets the coefficients a,b,c,d for the trafo from helicity basis to JLS basis
      ! they are needed in the potential_mod

      implicit none
      integer :: jot    ! jot = 2*j_physical
      real(kind(0.d0)) :: jr2,jmh,jph,jp3h,jr2p2

      d(1,1)  = -1.d0/dsqrt(2.d0)
      d(1,-1) = 1.d0/dsqrt(2.d0)

      a(1,1)  = 1.d0/dsqrt(3.d0)
      a(1,-1) = -1.d0/dsqrt(3.d0)
      a(2,1)  = -1.d0/dsqrt(6.d0)
      a(2,-1) = 1.d0/dsqrt(6.d0)

      jr2   = float(jot)   ! this is 2*j_phys
      jmh   = jr2/2.d0-0.5d0
      jph   = jr2/2.d0+0.5d0
      jp3h  = jr2/2.d0+1.5d0
      jr2p2 = jr2+2.d0

      b(1,1)  = -1.d0/2.d0*dsqrt(jmh/jr2p2)
      b(1,-1) = 1.d0/2.d0*dsqrt(jp3h/jr2)
      b(2,1)  = 1.d0/2.d0*dsqrt(jp3h/jr2p2)
      b(2,-1) = 1.d0/2.d0*dsqrt(jmh/jr2)
      b(3,1)  = 1.d0/dsqrt(2.d0)*dsqrt(jp3h/jr2p2)
      b(3,-1) = 1.d0/dsqrt(2.d0)*dsqrt(jmh/jr2)

      c(1,1)  = dsqrt(3.d0)/2.d0*dsqrt(jmh/jr2)
      c(1,-1) = -dsqrt(3.d0)/2.d0*dsqrt(jp3h/jr2p2)
      c(2,1)  = -1.d0/dsqrt(12.d0)*dsqrt(jp3h/jr2)
      c(2,-1) = -1.d0/dsqrt(12.d0)*dsqrt(jmh/jr2p2)
      c(3,1)  = -1.d0/dsqrt(6.d0)*dsqrt(jp3h/jr2)
      c(3,-1) = -1.d0/dsqrt(6.d0)*dsqrt(jmh/jr2p2)

      e(1,1)  = -1.d0/dsqrt(8.d0)*dsqrt((jr2+3.d0)/(jr2))
      e(1,-1) = -1.d0/dsqrt(8.d0)*dsqrt((jr2-1.d0)/(jr2+2.d0))
      e(2,1)  = dsqrt(3.d0/8.d0)*dsqrt((jr2-1.d0)/(jr2))
      e(2,-1) = -dsqrt(3.d0/8.d0)*dsqrt((jr2+3.d0)/(jr2+2.d0))

      f(1,1)  = -e(2,-1)
      f(1,-1) = e(2,1)
      f(2,1)  = e(1,-1)
      f(2,-1) = -e(1,1)

  end subroutine set_coeff_jls

  !=======================================================================!

  subroutine ms_c_def
      implicit none

      integer :: i
      real(kind(0.d0)) :: e_tr_c

      ms_f_c(1)=masses(1,1)    ! N pi
      ms_b_c(1)=masses(1,2)

      do i = 2, 4
        ms_f_c(i)=masses(1,1)  ! N rho
        ms_b_c(i)=masses(3,2)
      end do

      ms_f_c(5)=masses(1,1)    ! N eta
      ms_b_c(5)=masses(5,2)

      do i = 6, 7
        ms_f_c(i)=masses(2,1)  ! Delta pi
        ms_b_c(i)=masses(1,2)
      end do

      ms_f_c(8)=masses(1,1)    ! N sigma
      ms_b_c(8)=masses(2,2)

      do i = 9, 11
        ms_f_c(i)=masses(1,1)  ! N omega
        ms_b_c(i)=masses(4,2)
      end do

      ms_f_c(12)=masses(3,1)  ! Lambda K
      ms_b_c(12)=masses(6,2)

      ms_f_c(13)=masses(4,1)  ! Sigma K
      ms_b_c(13)=masses(6,2)

      ms_f_c(14)=masses(5,1)  ! Lambdac D
      ms_b_c(14)=masses(9,2)

      ms_f_c(15)=masses(6,1)  ! Sigmac D
      ms_b_c(15)=masses(9,2)

      do i = 16, 18
        ms_f_c(i)=masses(5,1)  ! Lambdac Ds
        ms_b_c(i)=masses(10,2)
      end do

      do i = 19, 21
        ms_f_c(i)=masses(6,1)  ! Sigmac Ds
        ms_b_c(i)=masses(10,2)
      end do

      write(kwrite,'(a40)')' ms_c_def: define masses of channels'

      do i=1,n_c
        e_tr_c=ms_f_c(i)+ms_b_c(i)
        write(kwrite,'(i10,3f15.5)')i,ms_f_c(i),ms_b_c(i),e_tr_c
      end do

  end subroutine ms_c_def

  !=======================================================================!

  subroutine qon_c(z,q_on_shell)	! CAREFUL: Index has changed in this version!

      implicit none
      integer :: i
      complex(kind(0.d0)) :: z
      complex(kind(0.d0)), dimension(n_c) :: q_on_shell

      do i=1,n_c
        call q_on_shell_calc(q_on_shell(i),ms_f_c(i),ms_b_c(i),z)
      end do
      if (dble(z)>1000.d0) call q_on_shell_calc(q_on_shell(1),mnu_phys,mpi_phys,z)  ! N pi PHYSICAL pi^0p
      call q_on_shell_calc(q_on_shell(2),ms_f_c(2),mrho_fac*ms_b_c(1),z)  ! N rho ! chbmd: set mrho->2mpi
      call q_on_shell_calc(q_on_shell(3),ms_f_c(3),mrho_fac*ms_b_c(1),z)  ! N rho ! chbmd: set mrho->2mpi
      call q_on_shell_calc(q_on_shell(4),ms_f_c(4),mrho_fac*ms_b_c(1),z)  ! N rho ! chbmd: set mrho->2mpi
      call q_on_shell_calc(q_on_shell(6),ms_f_c(6),mdel_fac*ms_b_c(1),z)  	! piD
      call q_on_shell_calc(q_on_shell(7),ms_f_c(7),mdel_fac*ms_b_c(1),z)  	! piD
      call q_on_shell_calc(q_on_shell(8),ms_f_c(8),msig_fac*ms_b_c(1),z)  ! N sig ! chbmd: set msig->2mpi

  end subroutine qon_c

  !=======================================================================!

  subroutine q_on_shell_calc(q_value,ms_1,ms_2,z)

      implicit none
      real(kind(0.d0)) :: ms_1,ms_2
      complex(kind(0.d0)) :: z
      complex(kind(0.d0)) :: q_value

      q_value=1.d0/(2.d0*z)*sqrt((z**2-(ms_1+ms_2)**2)*(z**2-(ms_1-ms_2)**2))
      if(aimag(q_value).lt.0) q_value=-q_value   ! Ensure correct sheet!

  end subroutine q_on_shell_calc

  !=======================================================================!

  subroutine constants_prep
      implicit none

      pi    = 4.d0*datan(1.d0)
      tpi   = 2.d0*pi
      fpi   = 4.d0*pi
      pih   = 0.5d0*pi
      tpi3  = tpi**3
      rtpi3 = dsqrt(tpi3)
      uf    = 197.326968d0

  end subroutine constants_prep

  !=======================================================================!

  subroutine  coupl_prep(gcoup)
      implicit none

      real(kind(0.d0)) :: grho,mu   ! shortcuts

      real(kind(0.d0)), dimension(30) :: gcoup  ! <readinput (cc_in.txt)
      real(kind(0.d0)) :: g_BBP, alpha_BBP, g_BBV, alpha_BBV, f_NNomg, kappa_NNrho, g_PPV, &
          & g_DBP, g_DBV, g_DDP, g_DDV, g_VVV, g_VVP
      real(kind(0.d0)) :: g_NNpi, g_NNeta, g_SigNK, g_SigSigpi, g_LamNK, g_LamSigpi, g_XiLamK, g_XiSigK,g_LamLameta,g_SigSigeta
      real(kind(0.d0)) :: g_NNrho, f_NNrho, g_NNomg, g_LamNKst, f_LamNKst, g_SigNKst, f_SigNKst, &
          & g_LamLamomg, f_LamLamomg, g_SigSigomg, f_SigSigomg, g_LamLamphi, f_LamLamphi, &
          & g_SigSigphi, f_SigSigphi, g_SigSigrho, f_SigSigrho, g_LamSigrho, f_LamSigrho, &
          & g_XiccSigcD, g_XiccLamcD, g_LamcLamcomg, g_LamcLamcphi, g_LamcSigcrho, g_SigcSigcrho, &
          & g_SigcSigcomg, g_SigcSigcphi, f_LamcLamcomg, f_SigcSigcomg, f_LamcLamcphi, f_SigcSigcphi, &
          & f_SigcSigcrho, f_LamcSigcrho, g_SigcSigcpi, g_LamcSigcpi, g_XiccLamcDs, g_XiccSigcDs, &
          & f_XiccLamcDs, f_XiccSigcDs
      real(kind(0.d0)) :: g_pipirho, g_KKrho, g_KpiKst, g_KKomg, g_KKphi,g_KetaKst, g_DDrho, g_DDomg, g_DDphi, &
          & g_DpiDs, g_DsDsrho, g_DsDsomg, g_DsDrho, g_DsDomg, g_DspiDs
      real(kind(0.d0)) :: g_DelNpi, g_SigstNK, g_SigstSigpi, g_SigstLampi, g_XistLamK, g_XistSigK,g_SigstSigeta
      real(kind(0.d0)) :: g_DelDelrho, kappa_DelDelrho, g_DelNrho, g_DelDelpi
      real(kind(0.d0)) :: g_omgpirho, g_NNsig, g_pipisig, g_sigsigsig, g_NNa0, g_pietaa0

      g_BBP     = gcoup(1)   ! g_NNpi
      alpha_BBP = gcoup(2)

      g_BBV       = gcoup(3)   ! g_NNrho
      alpha_BBV   = gcoup(4)
      f_NNomg     = gcoup(5)   ! f_NNomega
      kappa_NNrho = gcoup(6)   ! kappa_NNrho = f_NNrho / g_NNrho

      g_PPV = gcoup(7)   ! g_pipirho/2 = g_KKrho
      g_VVV = gcoup(7)
      g_VVP = gcoup(7)
      g_DBP = gcoup(8)   ! g_DelNpi
      g_DBV = gcoup(9)   ! g_DelNrho
      g_DDP = gcoup(10)  ! g_DelDelpi

      g_DDV           = gcoup(11)  ! g_DelDelrho
      kappa_DelDelrho = gcoup(12)  ! kappa_DelDelrho = f_DelDelrho / g_DelDelrho

      g_omgpirho  = gcoup(13)
      g_NNsig     = gcoup(14)
      g_pipisig   = gcoup(15)
      g_sigsigsig = gcoup(16)
      g_NNa0      = gcoup(17)
      g_pietaa0   = gcoup(18)

      !  coupling for octet-baryon, octet-baryon and pseudo-scalar-meson
      g_NNpi     =   g_BBP
      write(0,*)'g_NNpi:', g_NNpi
      !	  g_NNeta    =   g_BBP / dsqrt(3.d0) * (4.d0 * alpha_BBP - 1.d0)
      !  REPLACE g_NNeta by phenomenologically small value instead of using SU(3) value!:
      g_NNeta    =   g_BBP * 2.d0/13.45d0
      g_SigNK    =   g_BBP * (1.d0 - 2.d0 * alpha_BBP)
      g_SigSigpi =   g_BBP * 2.d0 * alpha_BBP
      g_LamNK    = - g_BBP / dsqrt(3.d0) * (1.d0 + 2.d0 * alpha_BBP)
      g_LamSigpi =   g_BBP * 2.d0 / dsqrt(3.d0) * (1.d0 - alpha_BBP)
      g_XiSigK   = - g_BBP
      g_XiLamK   =   g_BBP / dsqrt(3.d0) * (4.d0 * alpha_BBP - 1.d0)
      g_LamLameta = -2.d0/dsqrt(3.d0) * g_BBP * (1.d0-alpha_BBP)
      g_SigSigeta =  2.d0/dsqrt(3.d0) * g_BBP * (1.d0-alpha_BBP)
      g_XiccSigcD   = - g_BBP
      g_XiccLamcD   =   g_BBP / dsqrt(3.d0) * (4.d0 * alpha_BBP - 1.d0)
      g_SigcSigcpi =   g_BBP * 2.d0 * alpha_BBP
      g_LamcSigcpi =   g_BBP * 2.d0 / dsqrt(3.d0) * (1.d0 - alpha_BBP)

      ! vector coupling for octet-baryon, octet-baryon and vector-meson
      g_NNrho     =   g_BBV
      g_NNomg     =   g_BBV * (4.d0 * alpha_BBV - 1.d0)
      g_LamNKst   = - g_BBV / dsqrt(3.d0) * (1.d0 + 2.d0 * alpha_BBV)
      g_SigNKst   =   g_BBV * (1.d0 - 2.d0 * alpha_BBV)
      g_LamLamomg =   g_BBV * 2.d0 / 3.d0 * (5.d0 * alpha_BBV - 2.d0)
      g_SigSigomg =   g_BBV * 2.d0 * alpha_BBV
      g_LamLamphi = - g_BBV * dsqrt(2.d0) / 3.d0 * (2.d0 * alpha_BBV + 1.d0)
      g_SigSigphi = - g_BBV * dsqrt(2.d0) * (2.d0 * alpha_BBV - 1.d0)
      g_SigSigrho =   g_BBV * 2.d0 * alpha_BBV
      g_LamSigrho =   g_BBV * 2.d0 / dsqrt(3.d0) * (1.d0 - alpha_BBV)
      g_LamcLamcomg =   g_BBV * 2.d0 / 3.d0 * (5.d0 * alpha_BBV - 2.d0)
      g_LamcLamcphi = - g_BBV * dsqrt(2.d0) / 3.d0 * (2.d0 * alpha_BBV + 1.d0)
      g_LamcSigcrho =   g_BBV * 2.d0 / dsqrt(3.d0) * (1.d0 - alpha_BBV)
      g_SigcSigcrho =   g_BBV * 2.d0 * alpha_BBV
      g_SigcSigcomg =   g_BBV * 2.d0 * alpha_BBV
      g_SigcSigcphi = - g_BBV * dsqrt(2.d0) * (2.d0 * alpha_BBV - 1.d0)
      g_XiccLamcDs  =   g_BBV / dsqrt(3.d0) * (4.d0 * alpha_BBV - 1.d0)
      g_XiccSigcDs  = - g_BBV

      ! tensor coupling for octet-baryon, octet-baryon and vector-meson
      f_NNrho     =   g_NNrho * kappa_NNrho
      f_LamNKst   = - f_NNomg / 2.d0 / dsqrt(3.d0) - f_NNrho * dsqrt(3.d0) / 2.d0
      f_SigNKst   = - f_NNomg / 2.d0 + f_NNrho / 2.d0
      f_LamLamomg =   f_NNomg * 5.d0 / 6.d0 - f_NNrho / 2.d0
      f_SigSigomg =   f_NNomg / 2.d0 + f_NNrho / 2.d0
      f_LamLamphi = - f_NNomg / 2.d0 / dsqrt(3.d0) - f_NNrho / dsqrt(2.d0)
      f_SigSigphi = - f_NNomg / dsqrt(2.d0) + f_NNrho / dsqrt(2.d0)
      f_SigSigrho =   f_NNomg / 2.d0 + f_NNrho / 2.d0
      f_LamSigrho = - f_NNomg / 2.d0 / dsqrt(3.d0) + f_NNrho * dsqrt(3.d0) / 2.d0
      f_LamcLamcomg =   f_NNomg * 5.d0 / 6.d0 - f_NNrho / 2.d0
      f_SigcSigcomg =   f_NNomg / 2.d0 + f_NNrho / 2.d0
      f_LamcLamcphi = - f_NNomg / 2.d0 / dsqrt(3.d0) - f_NNrho / dsqrt(2.d0)
      f_SigcSigcphi = - f_NNomg / dsqrt(2.d0) + f_NNrho / dsqrt(2.d0)
      f_SigcSigcrho =   f_NNomg / 2.d0 + f_NNrho / 2.d0
      f_LamcSigcrho = - f_NNomg / 2.d0 / dsqrt(3.d0) + f_NNrho * dsqrt(3.d0) / 2.d0
      f_XiccLamcDs  =   0.d0
      f_XiccSigcDs  =   0.d0

      ! coupling for pseudo-scalar-meson, pseudo-scalar-meson and vector meson
      g_pipirho =   g_PPV * 2.d0
      g_KKrho   =   g_PPV
      g_KpiKst  = - g_PPV
      g_KKomg   =   g_PPV
      g_KKphi   =   g_PPV * dsqrt(2.d0)
      g_KetaKst = - dsqrt(3.d0)*g_PPV	! new on Dec. 2, Deborah
      g_DDrho   =   g_PPV
      g_DDomg   =   g_PPV
      g_DDphi   =   g_PPV * dsqrt(2.d0)
      g_DpiDs   = - g_PPV
      g_DsDsrho =   g_VVV
      g_DsDsomg =   g_VVV
      g_DsDrho  =   g_VVP
      g_DsDomg  =   g_VVP
      g_DspiDs  = - g_VVP

      ! coupling for decuplet-baryon, octet-baryon and pseudo-scalar-meson
      g_DelNpi     =   g_DBP
      g_SigstNK    = - g_DBP / dsqrt(6.d0)
      g_SigstSigpi =   g_DBP / dsqrt(6.d0)
      g_SigstLampi =   g_DBP / dsqrt(2.d0)
      g_XistLamK   =   g_DBP / dsqrt(2.d0)
      g_XistSigK   =   g_DBP / dsqrt(6.d0)
      g_SigstSigeta= - g_DBP / dsqrt(2.d0)

      ! coupling for decuplet-baryon, octet-baryon and vector-meson
      g_DelNrho = g_DBV

      ! coupling for decuplet-baryon, decuplet-baryon and pseudo-scalar-meson
      g_DelDelpi = g_DDP

      ! coupling for decuplet-baryon, decuplet-baryon and vector-meson
      g_DelDelrho = g_DDV

      ! other couplings, to be checked
      grho=2.d0*g_NNrho
      mu=-f_NNrho/(4.d0*masses(1,1))

      !------------!
      ! piN -> piN !
      !------------!

      coupl(1,1,1)%gc=g_NNpi**2/masses(1,2)**2            ! (gNNpi/mpi)**2  N-u
      !      write(0,*)'in coupl_prep: coupl(1,1,1)%gc=', coupl(1,1,1)%gc
      !      write(0,*)'in coupl_prep: masses(1,2)=', masses(1,2)
      !      write(0,*)'in coupl_prep: masses(1,2)=', masses(1,2)
      coupl(1,1,2)%gc=g_NNsig*g_pipisig/2.d0/masses(1,2)  ! gNNsig*gpipisig/2mpi  sigma-t
      coupl(1,1,3)%gc=g_NNrho*g_pipirho                   ! gNNrho*gpipirho  rho-t
      coupl(1,1,3)%ka=kappa_NNrho                         ! kappa_NNrho
      coupl(1,1,4)%gc=g_DelNpi**2/masses(1,2)**2          ! (gDNpi/mpi)**2  D-u
      ! not used, to be checked
      coupl(1,1,5)%gc=-mu*grho/(masses(7,2)**2)           ! contact term

      !-------------!
      ! piN -> rhoN !
      !-------------!
      coupl(1,2,1)%gc=g_NNrho*g_NNpi/masses(1,2)      ! N-u
      coupl(1,2,1)%ka=kappa_NNrho
      coupl(1,2,2)%gc=g_NNpi/masses(1,2)*grho         ! contact term
      ! not consistent (g_rhopipi=2g_NNrho?)
      !      coupl(1,2,3)%gc=g_NNpi/masses(1,2)*grho         ! pi-t WRONG OLD
      coupl(1,2,3)%gc=g_NNpi/masses(1,2)*g_pipirho     ! pi-t CORRECTED
      coupl(1,2,4)%gc=g_omgpirho*g_NNomg/masses(4,2)  ! omega-t
      coupl(1,2,4)%ka=f_NNomg/g_NNomg
      coupl(1,2,5)%gc=g_NNpi/masses(1,2)*grho*2.d0    ! a1-t
      ! not used, to be checked
      !      coupl(1,2,6)%gc=dsqrt(gcoup(12)*4.d0*pi)/masses(1,2)*grho*gcoup(16)*1.0d-3  ! D-u
      coupl(1,2,6)%ka=gcoup(17)/(2.d0*masses(1,1)*gcoup(16))                      ! NOT USED AND WRONG.
      coupl(1,2,6)%gc=gcoup(8)*gcoup(9)/masses(1,2)/masses(3,2)   ! D-u, updated by Deborah, 12/09/2012 .

      !-------------!
      ! piN -> etaN !
      !-------------!
      coupl(1,3,1)%gc=g_NNpi*g_NNeta/masses(1,2)**2   ! N u
      coupl(1,3,2)%gc=g_NNa0*g_pietaa0*masses(1,2)    ! a0 t scalar coupling

      !----------------!
      ! piN -> piDelta !
      !----------------!
      coupl(1,4,1)%gc=g_NNpi*g_DelNpi/masses(1,2)**2      ! N-u
      coupl(1,4,2)%gc=g_DelNrho*g_pipirho/masses(3,2)     ! rho-t
      coupl(1,4,3)%gc=g_DelNpi*g_DelDelpi/masses(1,2)**2  ! D-u

      !---------------!
      ! piN -> sigmaN !
      !---------------!
      coupl(1,5,1)%gc=g_NNpi/masses(1,2)*g_NNsig       ! N-u
      coupl(1,5,2)%gc=g_NNpi*g_pipisig/masses(1,2)**2  ! pi-t
      ! a factor 2 is absorbed in potential

      !-----------!
      ! piN->LamK !
      !-----------!
      coupl(1,7,1)%gc=g_LamNKst*g_KpiKst                     ! K*-t
      coupl(1,7,1)%ka=f_LamNKst/g_LamNKst
      coupl(1,7,2)%gc=g_SigNK*g_LamSigpi/masses(1,2)**2      ! Sigma-u
      coupl(1,7,3)%gc=gcoup(21)*4.d0*pi/2.d0/masses(1,2)     ! kappa-t
      coupl(1,7,4)%gc=g_SigstNK*g_SigstLampi/masses(1,2)**2  ! Sigma*-u

      !-----------!
      ! piN->SigK !
      !-----------!
      coupl(1,8,1)%gc=g_SigNKst*g_KpiKst                     ! K*-t
      coupl(1,8,1)%ka=f_SigNKst/g_SigNKst
      coupl(1,8,2)%gc=g_SigNK*g_SigSigpi/masses(1,2)**2      ! Sigma-u
      coupl(1,8,3)%gc=g_LamNK*g_LamSigpi/masses(1,2)**2      ! Lambda-u
      coupl(1,8,4)%gc=gcoup(22)*4.d0*pi/2.d0/masses(1,2)     ! kappa-t
      coupl(1,8,5)%gc=g_SigstNK*g_SigstSigpi/masses(1,2)**2  ! Sigma*-u

      !--------------!
      ! rhoN -> rhoN !
      !--------------!
      coupl(2,2,1)%gc=g_NNrho**2                            ! N-u
      coupl(2,2,1)%ka=kappa_NNrho
      coupl(2,2,2)%gc=grho**2/4.d0*kappa_NNrho/masses(1,1)  ! contact
      coupl(2,2,3)%gc=g_NNrho*grho                          ! rho-t
      ! a factor 2 is absorbed in potential
      coupl(2,2,3)%ka=kappa_NNrho                          ! rho-t
      ! not used, to be checked
      !      coupl(2,2,4)%gc=grho*gcoup(16)/masses(1,1)            ! D-u WRONG
      coupl(2,2,4)%ka=gcoup(17)/(2.d0*masses(1,1)*gcoup(16))
      coupl(2,2,4)%gc=gcoup(9)**2/masses(3,2)**2            ! D-u, updated 09/13/2012 .

      !-------------!
      ! rhoN -> piD !
      !-------------!
      ! not used, potential to be checked
      coupl(2,4,1)%gc=g_DelNpi*g_pipirho/masses(1,2)            ! pi-t
      coupl(2,4,2)%gc=g_NNpi*g_DelNrho/masses(1,2)/masses(3,2)  ! N-u

      !--------------!
      ! etaN -> etaN !
      !--------------!
      coupl(3,3,1)%gc=g_NNeta**2/masses(1,2)**2      ! N u
      ! not used, to be checked
      coupl(3,3,2)%gc=gcoup(22)*4.d0*pi*masses(1,2)  ! f0 t

      !------------!
      ! etaN->LamK !
      !------------!
      coupl(3,7,1)%gc=g_LamNKst*g_KetaKst            ! K*-t
      coupl(3,7,1)%ka=f_LamNKst/g_LamNKst
      coupl(3,7,2)%gc=g_LamLameta*g_LamNK/masses(1,2)**2            ! Lam-u

      !------------!
      ! etaN->SigK !
      !------------!
      coupl(3,8,1)%gc=g_SigNKst*g_KetaKst            ! K*-t
      coupl(3,8,1)%ka=f_SigNKst/g_SigNKst
      coupl(3,8,2)%gc=g_SigSigeta * g_SigNK/masses(1,2)**2 	     ! Sig-u
      coupl(3,8,3)%gc=g_SigstSigeta  *g_SigstNK/masses(1,2)**2      ! Sig*-u

      !--------------------!
      ! piDelta -> piDelta !
      !--------------------!
      coupl(4,4,1)%gc=g_DelNpi**2/masses(1,2)**2    ! N-u
      coupl(4,4,2)%gc=g_DelDelrho*g_pipirho         ! rho-t
      coupl(4,4,2)%ka=kappa_DelDelrho               ! rho-t kappa
      ! not used, potential to be checked
      coupl(4,4,3)%gc=g_DelDelpi**2/masses(1,2)**2  ! D-u

      !-------------------!
      ! piDelta -> sigmaN !
      !-------------------!
      ! not used, potential to be checked
      coupl(4,5,1)%gc=g_DelNpi*g_pipisig/masses(1,2)**2  ! pi-t

      !------------------!
      ! sigmaN -> sigmaN !
      !------------------!
      coupl(5,5,1)%gc=g_NNsig**2                            ! N-u
      coupl(5,5,2)%gc=6.d0*g_NNsig*g_sigsigsig*masses(2,2)  ! sigma-t

      !------------!
      ! LamK->LamK !
      !------------!
      coupl(7,7,1)%gc=gcoup(19)*4.d0*pi/2.d0/masses(1,2)  ! sigma-t
      coupl(7,7,2)%gc=g_LamLamomg*g_KKomg                 ! omega-t
      coupl(7,7,2)%ka=f_LamLamomg/g_LamLamomg
      coupl(7,7,3)%gc=g_LamLamphi*g_KKphi                 ! phi-t
      coupl(7,7,3)%ka=f_LamLamphi/g_LamLamphi
      coupl(7,7,4)%gc=g_XiLamK**2/masses(1,2)**2          ! Xi-u
      coupl(7,7,5)%gc=g_XistLamK**2/masses(1,2)**2        ! Xi*-u

      !------------!
      ! LamK->SigK !
      !------------!
      coupl(7,8,1)%gc=g_LamSigrho*g_KKrho                   ! rho-t
      coupl(7,8,1)%ka=f_LamSigrho/g_LamSigrho
      coupl(7,8,2)%gc=g_XiLamK*g_XiSigK/masses(1,2)**2      ! Xi-u
      coupl(7,8,3)%gc=g_XistLamK*g_XistSigK/masses(1,2)**2  ! Xi*-u

      !------------!
      ! SigK->SigK !
      !------------!
      coupl(8,8,1)%gc=gcoup(20)*4.d0*pi/2.d0/masses(1,2)  ! sigma-t
      coupl(8,8,2)%gc=g_SigSigomg*g_KKomg                 ! omega-t
      coupl(8,8,2)%ka=f_SigSigomg/g_SigSigomg
      coupl(8,8,3)%gc=g_SigSigphi*g_KKphi                 ! phi-t
      coupl(8,8,3)%ka=f_SigSigphi/g_SigSigphi
      coupl(8,8,4)%gc=g_SigSigrho*g_KKrho                 ! rho-t
      coupl(8,8,4)%ka=f_SigSigrho/g_SigSigrho
      coupl(8,8,5)%gc=g_XiSigK**2/masses(1,2)**2          ! Xi-u
      coupl(8,8,6)%gc=g_XistSigK**2/masses(1,2)**2        ! Xi*-u

      !------------!
      !LamcD->LamcD!
      !------------!
      coupl(9,9,1)%gc=gcoup(23)*4.d0*pi/2.d0/masses(9,2)  ! sigma-t
      coupl(9,9,2)%gc=g_LamcLamcomg*g_DDomg                 ! omega-t
      coupl(9,9,2)%ka=f_LamcLamcomg/g_LamcLamcomg
      coupl(9,9,3)%gc=g_LamcLamcphi*g_DDphi                 ! phi-t
      coupl(9,9,3)%ka=f_LamcLamcphi/g_LamcLamcphi
      coupl(9,9,4)%gc=g_XiccLamcD**2/masses(9,2)**2          ! Xi-u

      !------------!
      !LamcD->SigcD!
      !------------!
      coupl(9,10,1)%gc=g_LamcSigcrho*g_DDrho                   ! rho-t
      coupl(9,10,1)%ka=f_LamcSigcrho/g_LamcSigcrho
      coupl(9,10,2)%gc=g_XiccLamcD*g_XiccSigcD/masses(9,2)**2      ! Xi-u

      !------------!
      !SigcD->SigcD!
      !------------!
      coupl(10,10,1)%gc=gcoup(24)*4.d0*pi/2.d0/masses(9,2)  ! sigma-t
      coupl(10,10,2)%gc=g_SigcSigcomg*g_DDomg                 ! omega-t
      coupl(10,10,2)%ka=f_SigcSigcomg/g_SigcSigcomg
      coupl(10,10,3)%gc=g_SigcSigcphi*g_DDphi                 ! phi-t
      coupl(10,10,3)%ka=f_SigcSigcphi/g_SigcSigcphi
      coupl(10,10,4)%gc=g_SigcSigcrho*g_DDrho                 ! rho-t
      coupl(10,10,4)%ka=f_SigcSigcrho/g_SigcSigcrho
      coupl(10,10,5)%gc=g_XiccSigcD**2/masses(9,2)**2          ! Xi-u

      !------------!
      !LamcD->LcDs !
      !------------!
      coupl(9,11,1)%gc=g_LamcLamcomg*g_DsDomg/masses(4,2)                 ! omega-t
      coupl(9,11,1)%ka=f_LamcLamcomg/g_LamcLamcomg
      coupl(9,11,2)%gc=g_XiccLamcDs*g_XiccLamcD/masses(9,2)    ! Xi-u
      coupl(9,11,2)%ka=f_XiccLamcDs/g_XiccLamcDs

      !------------!
      !LamcD->ScDs !
      !------------!
      coupl(9,12,1)%gc=g_LamcSigcrho*g_DsDrho/masses(3,2)                   ! rho-t
      coupl(9,12,1)%ka=f_LamcSigcrho/g_LamcSigcrho
      coupl(9,12,2)%gc=g_LamcSigcpi*g_DpiDs/masses(1,2)                     ! pi-t
      coupl(9,12,3)%gc=g_XiccLamcDs*g_XiccSigcD/masses(9,2)      ! Xi-u
      coupl(9,12,3)%ka=f_XiccLamcDs/g_XiccLamcDs

      !------------!
      !SigcD->LcDs !
      !------------!
      coupl(10,11,1)%gc=g_LamcSigcrho*g_DsDrho/masses(3,2)                 ! rho-t
      coupl(10,11,1)%ka=f_LamcSigcrho/g_LamcSigcrho
      coupl(10,11,2)%gc=g_LamcSigcpi*g_DpiDs/masses(1,2)                   ! pi-t
      coupl(10,11,3)%gc=g_XiccSigcDs*g_XiccLamcD/masses(9,2)     ! Xi-u
      coupl(10,11,3)%ka=f_XiccSigcDs/g_XiccSigcDs

      !------------!
      !SigcD->ScDs !
      !------------!
      coupl(10,12,1)%gc=g_SigcSigcomg*g_DsDomg/masses(4,2)                 ! omega-t
      coupl(10,12,1)%ka=f_SigcSigcomg/g_SigcSigcomg
      coupl(10,12,2)%gc=g_SigcSigcrho*g_DsDrho/masses(3,2)                 ! rho-t
      coupl(10,12,2)%ka=f_SigcSigcrho/g_SigcSigcrho
      coupl(10,12,3)%gc=g_SigcSigcpi*g_DpiDs/masses(1,2)                   ! pi-t
      coupl(10,12,4)%gc=g_XiccSigcDs*g_XiccSigcD/masses(9,2)     ! Xi-u
      coupl(10,12,4)%ka=f_XiccSigcDs/g_XiccSigcDs

      !------------!
      ! LcDs->LcDs !
      !------------!
      coupl(11,11,1)%gc=g_LamcLamcomg*g_DsDsomg                 ! omega-t
      coupl(11,11,1)%ka=f_LamcLamcomg/g_LamcLamcomg
      coupl(11,11,2)%gc=g_XiccLamcDs**2    ! Xi-u
      coupl(11,11,2)%ka=f_XiccLamcDs/g_XiccLamcDs

      !------------!
      ! LcDs->ScDs !
      !------------!
      coupl(11,12,1)%gc=g_LamcSigcrho*g_DsDsrho                   ! rho-t
      coupl(11,12,1)%ka=f_LamcSigcrho/g_LamcSigcrho
      coupl(11,12,2)%gc=g_LamcSigcpi*g_DspiDs/masses(1,2)                     ! pi-t
      coupl(11,12,3)%gc=g_XiccLamcDs*g_XiccSigcDs      ! Xi-u
      coupl(11,12,3)%ka=f_XiccLamcDs/g_XiccLamcDs
      coupl(11,12,3)%ka2=f_XiccSigcDs/g_XiccSigcDs

      !------------!
      ! ScDs->ScDs !
      !------------!
      coupl(12,12,1)%gc=g_SigcSigcomg*g_DsDsomg                 ! omega-t
      coupl(12,12,1)%ka=f_SigcSigcomg/g_SigcSigcomg
      coupl(12,12,2)%gc=g_SigcSigcrho*g_DsDsrho                 ! rho-t
      coupl(12,12,2)%ka=f_SigcSigcrho/g_SigcSigcrho
      coupl(12,12,3)%gc=g_SigcSigcpi*g_DspiDs/masses(1,2)                   ! pi-t
      coupl(12,12,4)%gc=g_XiccSigcDs**2     ! Xi-u
      coupl(12,12,4)%ka=f_XiccSigcDs/g_XiccSigcDs

  end subroutine  coupl_prep

  !=======================================================================!

  subroutine  dispersion_prep

      real(kind(0.d0)) :: ampi,ampi2
      real(kind(0.d0)) :: xdum(100),wdum(100)

      call gauss_dsp(nd,4.d0,50.d0,xdum,wdum)
      do i=1,nd
        t_dsp(i)=xdum(i)
        w_dsp(i)=wdum(i)
      enddo

      ampi = masses(1,2)
      ampi2=ampi*ampi
      do itp=1,nd
        t_dsp(itp)=t_dsp(itp)*ampi2
        w_dsp(itp)=w_dsp(itp)*ampi2
        imf0 (itp)=imf0 (itp)*ampi
        imf1p(itp)=imf1p(itp)/ampi
        imf1m(itp)=imf1m(itp)/ampi2
      enddo

  end subroutine  dispersion_prep

  !=======================================================================!

end module input4
