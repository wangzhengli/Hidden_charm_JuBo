!
! main program for calculating the coupled channel piN T-matrix
!
program ccmn4
  use input4
  !      use input_KY_obs  ! NEW
  use input_minuit
  use tmat4
  use output4
  !      use mpi		[UNCOMMENT THIS LINE!]
  use steuerung						!(label:maketables)

  implicit none
  integer :: k_j,iv, irles, ident   ! k_j and iv determine the quantum numbers of the channel
  ! irles: resonance number for pole search; ident: for scan complex plane
  integer :: k,n1     ! n1=n+1, n is the mesh points of momentum integral
  complex(kind(0.d0)) :: ez
  ! additional variables for MPI commands 
  real(kind(0.d0)) ::start_cpu, end_cpu
  INTEGER :: ierr,npe,myid
  integer :: read_it_save


  ! initialize MPI [UNCOMMENT THE NEXT THREE LINES!]
  !      CALL mpi_init(ierr)
  !      CALL mpi_comm_size(MPI_COMM_WORLD,npe,ierr)
  !      CALL mpi_comm_rank(MPI_COMM_WORLD,myid,ierr)
  myid=0 ! [COMMENT THIS LINE!] when used in non-parallel mode

  nread=4
  kread=5
  kwrite=6

  open(nread,file='in_fixed.txt')
  open(kread,file='cc_in_K+S+_sol_15.5.txt')
  open(kwrite,file='cc_out.txt')

  !------------------------------------------------------------!
  !  file_unit  !  file_name    !!   file_unit  !  file_name   !
  !------------------------------------------------------------!
  !  5 kread    !  cc_in.txt    !!   6 kwrite   !  cc_out.txt  !
  !  4 nread    !  in_fixed.txt !!                             !
  !  3011-3014  !  tau_bgd/     !!   3015-3018  !   tau_res/   !
  !  3021-3024  !  del_bgd/     !!   3025-3028  !   del_res/   !
  !------------------------------------------------------------!

  call readinput

  call phase_factor  !  phase factor for transition from T matrix to tau matrix in JLS basis

  !--- check those lines! ---------------
  !	countf=0						! To count how many time chi^2 was called. 
  call read_input_minuit					! Reads in:
  ! 1) in_minuit_fixed.txt, 
  ! 2) "parm" is filled with values from cc_in, read in befor.
  ! 3) "parm" is filled with values from from save_parms.txt (over-
  ! writes previously read in parameters).
  call read_data						! Read in experimental data base for hadronic reactions.        

  call write_parm                                         ! Write parameters to "Info_minuit_fit.txt" and to "save_parms.txt".

  call transfer_all_parms					! Important to call if one wants the parms from save_parms.txt 
  ! but not call minuit. Writes "parm(i)%p_start" to internal JM
  !--- check those lines! ---------------



  n1=n+1
  write(0,'(a,3i10)')' k_b,K_e,melab',k_b,k_e,melab  ! output on screen


  taumat=0.0
  del=0.0
  eta=0.0
  store_t=0.0


  read_it_save=0
  if (readit==1) then
    read_it_save=readit
    readit=0		! Allows tenporarily to access normal TNP in subroutine tmat, in order to
    ! calculate TNP at nucleon position (not saved in the tables).
  end if



  ! Renormalize once and forever at beginning of everything ! Is OK as long as TNP is not changed!
  iv=2
  k_j=1
  j1=2*k_j - 1              ! j1=2*j_physical  ! declared in input
  call rot_mat(j1)         	! argument is: j1 = 2 k_j-1 = 1
  call set_coeff_jls(j1)	! argument is: j1 = 2 k_j-1 = 1
  write(0,*)'Renormalization starts, readit is: ',readit
  write(0,*)'No Renormalization'      
  !call rnm(iv,k_j)  	! renormalize couplings and masses for nucleon pole
  ! arguments: iv=2,k_j=1
  write(0,*)'Renormalization finished.'
  !     readit=read_it_save	! After renormalization is finished, set readit as defined in the beginning.


  if (calltnptable==1) then 		!(label:maketables)
    call cpu_time(start_cpu)
    write(0,*)'Now calling make_table'
    !write(0,*)'coupl(1,1,1)%gc:',coupl(1,1,1)%gc
    write(0,*)'readit=',readit
    write(0,*)'test reading tnp tables: store_t(1,1,1,1,1,1,1)=',store_t(1,1,1,1,1,1,1)
    call make_table			!Calls the routine to make or read the table (label:maketables)
    write(0,*)'test reading tnp tables: store_t(1,1,1,1,1,1,1)=',store_t(1,1,1,1,1,1,1)
    write(0,*)'test reading tnp tables: tmatrix(1,1,1,1,1)=',tmatrix(1,1,1,1,1)
    write(0,*)'readit=',readit
    write(0,*)'End calling make_table'
    call cpu_time(end_cpu)
    write(0,*)'cpu_time make_table = ',end_cpu-start_cpu

  end if 					! Condition whether or not to call make_table  (label:maketables)


  readit=read_it_save	! After renormalization is finished, set readit as defined in the beginning.


  if (call_minuit==1) then
    call test_minuit 
  end if


  !    write(0,*)'Renormalization starts, readit is: ',readit
  !    call rnm(2,1)  	! renormalize couplings and masses for nucleon pole
  ! arguments: iv=2,k_j=1
  !   write(0,*)'Renormalization finished.'




  !==================================================================================
  !
  !     CALCULATIONS IN COMPLEX PLANE
  !
  !==================================================================================

  !      if (activate_t2==1.and.myid==6) then
  if (activate_t2==1) then
    !           if(myid==6) then
    call cpu_time(start_cpu)
    readit=0						! readit is changed to zero, because table can't be used in complex plane.
    ! Is only safety measure, should be zero anyways here! 
    rsheet=2						! Set T on second sheet, unset at end of this program part.

    if (pole_search==1) then
      do k_j=k_b,k_e  					! loop over angular momentum
        j1=2*k_j - 1              			! j1=2*j_physical  ! declared in input
        call rot_mat(j1)          			! input xg,wg,djw  ! util.f90/rot_mat  ! dmat3.f90
        call set_coeff_jls(j1)
        !    do irles=1,24
        do irles=1,2
          if (k_j_reso(irles).eq.k_j) then
            write(*,*)'Now calling polepath for irles,k_j= ',irles,k_j
            mrho_fac=2.d0
            msig_fac=445.d0/138.d0
            mdel_fac=1.d0
            if (irles== 1) then
              mrho_fac=2.d0
              msig_fac=2.d0
              mdel_fac=1.2d0
            end if    	
            if (irles== 2) mrho_fac=3.d0    	
            if (irles== 3) then
              mrho_fac=3.d0 
              msig_fac=2.d0
              mdel_fac=1.2d0
            end if    	
            if (irles== 4) mrho_fac=2.d0   	
            if (irles== 5) mrho_fac=2.d0     	
            if (irles== 6) then
              mrho_fac=5.d0 
              mdel_fac=1.5d0
            end if 		
            if (irles== 7) then
              mrho_fac=2.5d0
              mdel_fac=1.5d0
            end if  		
            if (irles== 8) then
              mrho_fac=4.5d0
              mdel_fac=1.5d0
            end if 		
            if (irles== 9) then
              mrho_fac=2.d0
              mdel_fac=1.2d0
              msig_fac=4.d0!3.22d0
            end if		
            if (irles==10) mrho_fac=3.d0  	
            if (irles==11) then
              mrho_fac=3.d0
              mdel_fac=1.5d0
            end if		
            if (irles==12) mrho_fac=3.d0		
            if (irles==13) then
              mrho_fac=2.d0
              mdel_fac=2.1d0
            end if 		
            if (irles==14) then
              mrho_fac=2.d0
              mdel_fac=1.5d0
            end if		
            if (irles==15) mrho_fac=4.5d0  		
            if (irles==16) mrho_fac=4.5d0		
            if (irles==17) mrho_fac=2.5d0		
            if (irles==18) mrho_fac=5.3d0  	
            if (irles==19) mrho_fac=2.7d0 	
            if (irles==20) then 
              mrho_fac=5.6d0 
              mdel_fac=1.5d0
            end if	
            if (irles==21) mrho_fac=5.0d0		
            if (irles==22) mrho_fac=4.5d0		
            if (irles==23) mrho_fac=5.1d0		
            if (irles==24) mrho_fac=3.d0  
            call sheet_plot(irles)				! Central call for pole search.
            mrho_fac=2.d0
            msig_fac=445.d0/138.d0! 2.d0
            mdel_fac=1.d0
            manip_v=0
            gofourthr=0
          end if						! (k_j_reso(irles).eq.k_j)
        end do						! (irles=1,9)
      end do						! (k_j=1,4)
      write(*,*)'finished calling polepath'
    end if						! (if (pole_search==1) then)
    !           end if ! myid==6

    if (scanit==1) then				! Scan of complex plane
      readit=0					
      rsheet=2						! Set T on second sheet, unset at end of this program part.
      manip_v=0
      gofourthr=0
      mrho_fac=2.d0
      msig_fac=445.d0/138.d0 ! 2.d0
      mdel_fac=1.d0
      !   if ((myid>10).and.(myid<=30)) call scan_cmplx(1,5,myid)! Arguments: Scan for k_j from... to...
      !   if ((myid>14).and.(myid<=15)) 

      do ident=6,6
        call scan_cmplx(1,5,ident)! Arguments: Scan for k_j from... to... = P13
      end do

      manip_v=0
    end if						! (if (scanit==1) then)
    rsheet=0      					! Go back to first sheet for rest of the program.

    readit=1						! For next steps, switch back to read mode..
    call cpu_time(end_cpu)
    write(0,*)'cpu_time to search for pole = ',end_cpu-start_cpu
  end if 						! activate_t2==1

  !==================================================================================
  !
  !     USUAL LOOP OVER ENERGIES FOR OUTPUT
  !
  !==================================================================================


  if(normal_ccmn==1) then 			! Whether or not to make usual loop over energy and angular momentum (label:maketables) 
    call cpu_time(start_cpu)

    !     readit=1 ! check this!


    call make_table

    write(0,*)'Now entering the normal loop over E and k_j to make and write tau matrices'
    write(0,*)'readit is', readit
    do k_j=k_b,k_e  				! loop over angular momentum
      call tau_print_prep(k_j)  ! open store for tau matrices
      call del_print_prep(k_j)  ! open store for phase shifts and inelasticities
      j1=2*k_j - 1              ! j1=2*j_physical  ! declared in input
      call rot_mat(j1)          ! input xg,wg,djw  ! util.f90/rot_mat  ! dmat3.f90
      call set_coeff_jls(j1)

      do k=2+myid,melab!,npe  ! loop over energies [UNCOMMENT, npe!]
        ez=e_cm(k)
        zrem=ez
        call mesh_q
        write(0,'(a,2i10,f15.2)')' 2j,K_ez,ez   ',j1,k,dble(ez)  ! output on screen
        !	  write(0,*)'myid is', myid
        call tmat(ez,1,4,k_j)   ! non-pole t-matrix
        write(0,*)'test reading tnp tables: store_t(1,1,1,1,1,1,1)=',store_t(1,1,1,1,1,1,1)

        write(0,*)'k_j, ez, tmatrix(n1,n1,iv,1,1) is:',k_j, ez, tmatrix(n1,n1,1,1,1) 
        call tau_matrx(k,k_j)   ! non-pole tau-matrix (on-shell)
        write(0,*)'k_j, ez, tau_matrx is:',k_j, ez, taumat(k_j,1,k,1,1)
        call phas_shif(k,k_j)   ! background phase shifts and inelasticities
        !	  call v_matrx(k,k_j)     ! on-shell potential for t and/or u channel diagrams
        !          call tau_print_prep(k_j)  ! open store for tau matrices
        call tau_print(k,k_j,19,19,1)  ! print non-pole tau-matrix on-shell -> tau_bgd/
        call del_print(k,k_j,1,1,1)  ! print background phase shifts and inelasticities -> del_bgd/
        !  1: non-pole;  2: full
        ! 1,1:    N pi  --> N pi      1,2:    N pi  --> N rho
        ! 12,12:  Lam K --> Lam K     13,13:  Sig K --> Sig K
        !	      call z_inv_topt
        !  	      call photo_print(k, k_j, 1)  ! to be used for photo-production
        do iv=1,4
          call g_res_calc(ez,iv,k_j)    ! bare vertices
          !            write(0,*)'m_bare (1,1,1):',m_bare (1,1,1)
          !            write(0,*)'g_bare (1,1,1,1):',g_bare (1,1,1,1)
          call t_fs_calc(ez,iv,k_j)     ! dressed vertices, self energy
          call tmat_s_calc(ez,iv,k_j)   ! t_pole
          call t_add(iv,k_j)            ! t=t_np+t_p
          !			call dr_vrtx_photo(k,iv,k_j)  ! prepare dressed vertices for photo-production 
        end do  ! iv=1,4
        call tau_matrx(k,k_j)   ! full tau-matrix on-shell
        call phas_shif(k,k_j)   ! full phase shifts and inelasticities
        !	  call tau_print(k,k_j,1,1,2)  ! print full tau-matrix on-shell -> tau_res/
        !         call del_print(k,k_j,1,1,2)  ! print full phase shifts and inelasticities -> del_res/
        !  	  call photo_print(k, k_j, 2)  ! to print non-pole T matrix and full T matrix
        ! to be used for photo-production         
      end do  ! end of energy loop
      call tau_print_close(k_j)
      !	  call del_print_close(k_j)
      write(0,*)'Now finishing the normal loop over E and k_j=',k_j,' to make and write tau matrices'
    end do  ! end of loop over angular momentum
    call cpu_time(end_cpu)
    write(0,*)'cpu_time = ',end_cpu-start_cpu
  end if  ! Whether or not to call normal main loop.


  write(0,*)'myid is', myid
  if(myid.EQ.1) then
    do k_j=k_b,k_e  ! loop over angular momentum
      call tau_print_prep(k_j)  ! open store for tau matrices
      call del_print_prep(k_j)  ! open store for phase shifts and inelasticities

      do k=2,melab
        call tau_print(k,k_j,1,1,2)  ! print full tau-matrix on-shell -> tau_res/
        call del_print(k,k_j,1,1,2)  ! print full phase shifts and inelasticities -> del_res/
      end do

      call tau_print_close(k_j)
      call del_print_close(k_j)
    end do

    !	  call dr_vrtx_print  ! to print dressed vertices and self energies (for photo-production)

    !	  call v_out(13,13)   ! on-shell potential for channel ic -> jc (ic=1,n_c) (13,13: SigK-SigK)
    call tau_out(1,1)   ! on-shell tau matrix for channel ic -> jc (ic=1,n_c) (1,1: Npi-Npi)
    call del_out(1,1)   ! phase shifts and inelasticities for channel ic -> jc (ic=1,n_c) (1,1: Npi-Npi)
    !	  call sig_out(1,1)   ! total cross section for channel ic -> jc (ic=1,chdim) (1,1: Npi-Npi)
    !	  call sig_out(1,7)   ! total cross section for channel ic -> jc (ic=1,chdim) (1,7: Npi-LamK)
    !	  call sig_out(1,8)   ! total cross section for channel ic -> jc (ic=1,chdim) (1,8: Npi-SigK)
    !  	  call dsdo_out(1,1)  ! differential cross section for channel ic -> jc (ic=1,chdim) (1,1: Npi-Npi)


    if (1==1) then
      call cpu_time(start_cpu)
      write(0,*) 'Now writing Obs output...'
      call write_results                                      ! Writes fit results for hadronic reactions
      call cpu_time(end_cpu)
      write(0,*)'cpu_time to write observables = ',end_cpu-start_cpu
    end if

  end if


end program ccmn4

!=======================================================================!

subroutine rnm(iv,k_j)   			! renormalization for nucleon pole 
    use input4
    use vertices
    use vertices_5h7h     			! (newres) Check if one could leave out here; doesn't make much sense.    
    !      use mpi					! parallel  
    implicit none
    complex(kind(0.d0)) 	:: ez, ezp, ezm, &
        & g2inv, gb1, gb1_c, Gam1, Gam2, Gam1_c, Gam2_c, Gam3, Gam3_c
    complex(kind(0.d0)),dimension(n_r,n_r) :: slfp, slfm, slf_prime,slf0
    real(kind(0.d0)) 		:: m_bare_2, g_bare_2, ex, exsq, f_sq, mbare_N
    integer 			:: k_j,iv    	! k_j=1 and iv=2 determine the quantum numbers of the P11 wave; 
    ! could be substituted explicitly everywhere.
    integer 			:: i_c1,i_rs_i,reno3, read_it_save
    !integer 			:: status(MPI_STATUS_SIZE) 	! Parallel

    !     read_it_save=0
    !     if (readit==1) then
    !	read_it_save=readit
    !	readit=0		! Allows tenporarily to access normal TNP in subroutine tmat, in order to
    ! calculate TNP at nucleon position (not saved in the tables).
    !     end if

    reno3=1				! Three-res (1) or two-res (0) renormalization
    i_rs_i=1					! Resonance-Nr. is 1 for the nucleon!
    i_c1=1					! channel is 1=piN for renormalization
    n1=n+1					! on-shell value for renormalization
    m_bare(2,1,1)   = mn_phys			! If multiple renormalizations are required; 
    !     reminder: input in_1.txt requires PHYSICAL nucleon mass
    g_bare(2,1,1,1) = gpin_phys		! If multiple renormalizations are required.
    m_bare_2	      = m_bare(2,1,2)		! bare mass of the P11(1710) (newres)
    g_bare_2	      = g_bare(2,1,2,1)		! coupling of the P11(1710)  (newres)


    ! if (mysubid==2) then
    write(kwrite,'(a23)')				' Before renomalization:'
    write(kwrite,'(d15.6,a10)')m_bare(2,1,1),	' mn_phys  '
    write(kwrite,'(d15.6,a10)')g_bare(2,1,1,1),	' gpin_phys'
    write(kwrite,'(d15.6,a10)')m_bare_2,		' m_bare_2 '
    write(kwrite,'(d15.6,a10)')g_bare_2,		' g_bare_2 '
    ! end if

    j1=2*k_j - 1              		! j1=2*j_physical  ! declared in input
    call rot_mat(j1)         			! argument is: j1 = 2 k_j-1 = 1
    call set_coeff_jls(j1)	

    ez=dcmplx(mn_phys,0.d0)  			! renormalization energy z = m_N (physical)
    ezp=ez+del_ez				! This and next needed for derivatives
    ezm=ez-del_ez

    !  if (mysubid==0) then			! (parallel) On myidsubid=0: z = mn_phys + Delta z
    call slf_get(slfp,ezp,iv,k_j)
    !call MPI_SEND(slfp,n_r**2,MPI_DOUBLE_COMPLEX,2,1001,fit_p_comm,ierr)
    ! else if (mysubid==1) then			! (parallel) On myidsubid=1: z = mn_phys - Delta z
    call slf_get(slfm,ezm,iv,k_j) 
    !call MPI_SEND(slfm,n_r**2,MPI_DOUBLE_COMPLEX,2,1002,fit_p_comm,ierr)  
    !   else if (mysubid==2) then			! (parallel) On myidsubid=2: z = mn_phys 
    call slf_get(slf0,ez,iv,k_j)  
    !call MPI_RECV(slfp,n_r**2,MPI_DOUBLE_COMPLEX,0,1001,fit_p_comm,status,ierr)
    !call MPI_RECV(slfm,n_r**2,MPI_DOUBLE_COMPLEX,1,1002,fit_p_comm,status,ierr)
    !  end if					! Parallelization ends; rest done on process with mysubid=2

    ! if (mysubid==2) then
    slf_prime=(slfp-slfm)/(2.d0*del_ez)
    call g_res1hp_calc(ez,iv,k_j)    	! 1/2+, here: P11, P31
    call f_ind_c_calc(iv,k_j,f_drs_c,f_ind_c,f_bar_c)
    call f_ind_a_calc(iv,k_j,f_drs_a,f_ind_a,f_bar_a)
    gb1    =f_bar_a(1,n1,1)		! Note: evaluated with g_bare(2,1,1,1)=gpin_phys
    gb1_c  =f_bar_c(1,1,n1)		! Note: evaluated with g_bare(2,1,1,1)=gpin_phys; gb1*gb1d=a_1= physical residue
    Gam1   =f_drs_a(1,n1,1)		! Note: evaluated with g_bare(2,1,1,1)=gpin_phys
    Gam1_c =f_drs_c(1,i_c1,n1)		! Note: evaluated with g_bare(2,1,1,1)=gpin_phys
    Gam2   =f_drs_a(i_c1,n1,2) 
    Gam2_c =f_drs_c(2,i_c1,n1) 
    Gam3   =f_drs_a(i_c1,n1,3)
    Gam3_c =f_drs_c(3,i_c1,n1)
    write(kwrite,'(2F15.6,2F15.6,a20)')Gam1,Gam1_c,     ' Gam1     test1     '
    write(kwrite,'(2F15.6,2F15.6,a20)')Gam2,Gam2_c,     ' Gam2     test2     '
    write(kwrite,'(2F15.6,2F15.6,a20)')Gam3,Gam3_c,     ' Gam3     test3     '
    write(kwrite,'(2F15.6,2F15.6,a20)')gb1,gb1_c,       ' gb1      test4     '
    !write(kwrite,'(2F15.6,2F15.6,a20)')slf_prime,slf0,  ' slf0     test5     '
    if (reno3==0) then
      g2inv=mn_phys-m_bare_2-slf0(2,2)       
      exsq = slf_prime(1,1)									&
          &+ slf0(1,2)/g2inv*( slf0(1,2)/g2inv*(slf_prime(2,2)-1.d0) + 2.d0*slf_prime(1,2) ) 	&
          &+ (g2inv*Gam1+Gam2*slf0(1,2))*(g2inv*Gam1_c+Gam2_c*slf0(1,2))/(g2inv**2*gb1*gb1_c)
      exsq =1.d0/exsq
      if (exsq.le.0.d0) exsq=100.d0	! chi_2 huge, fit will take other direction
      m_bare(iv,k_j,i_rs_i)=mn_phys - slf0(1,1)*exsq-(slf0(1,2)*slf0(2,1)*exsq)/(mn_phys-m_bare_2-slf0(2,2))
      ex=dsqrt(exsq)
      g_bare(2,1,1,1)=g_bare(2,1,1,1)*ex 	
    else 
      call xinvsq3r(m_bare_2, slf_prime, slf0, gb1, gb1_c, Gam1, Gam2, Gam3, Gam1_c, Gam2_c, Gam3_c, exsq, mbare_N)
      write(kwrite,'(2d15.6,a20)')exsq,mbare_N,    ' exsq,m   test6     '
      ex=dsqrt(exsq)
      g_bare(2,1,1,1)=g_bare(2,1,1,1)*ex 	
      m_bare(iv,k_j,i_rs_i) = mbare_N
    end if
    write(kwrite,'(4I2,a25)')iv,k_j,i_rs_i,i_c1,  ' iv,k_j,i_rs_i,i_c1 test7'
    write(kwrite,'(a23)')							' After renormalization:'
    write(kwrite,'(d15.6,a20)')g_bare(iv,k_j,i_rs_i,i_c1), 		' g_bare=f_b/m_pi    '
    write(kwrite,'(d15.6,a20)')g_bare(iv,k_j,i_rs_i,i_c1)*masses(1,2), 	' f_b                '
    write(kwrite,'(d15.6,a20)')ex,	 					' x                  '
    write(kwrite,'(d15.6,a20)')m_bare(iv,k_j,i_rs_i),			' m_bare             '
    f_sq= (g_bare(iv,k_j,i_rs_i,i_c1)*masses(1,2))**2/4.d0/pi
    write(kwrite,'(d15.6,a20)')f_sq,					' f_sq_b/4pi         '
    write(kwrite,'(a20)')							' end renormalization'     
    !end if 					! (mysubid==2)

    !     readit=read_it_save	! After renormalization is finished, set readit as defined in the beginning.

    !call MPI_BCAST(g_bare(iv,k_j,i_rs_i,i_c1),1,MPI_DOUBLE_PRECISION,2,fit_p_comm,ierr)
    !call MPI_BCAST(m_bare(iv,k_j,i_rs_i)     ,1,MPI_DOUBLE_PRECISION,2,fit_p_comm,ierr)
end subroutine rnm

!=======================================================================!

subroutine xinvsq3r(m_bare_2, slfprime, slf0, gb1, gb1c, Gam1, Gam2, Gam3, Gam1c, Gam2c, Gam3c, xsq, mbare_N)
    use input4
    implicit none
    complex(kind(0.d0))			:: Gam1, Gam2, Gam3, Gam1c, Gam2c, Gam3c, g2inv, g2invprime, g3inv, g3invprime, &
        &  gb1, gb1c
    complex(kind(0.d0)),dimension(n_r,n_r) 	:: slfprime, slf0
    real(kind(0.d0))				:: m_bare_2, xsq, mbare_N
    g2inv	= mn_phys-m_bare_2-slf0(2,2)
    g2invprime= 1.d0-slfprime(2,2)
    g3inv	= 1.d0-slf0(3,3)			! Adapt if different form of contact term is chosen!
    g3invprime= -slfprime(3,3)			! Adapt if different form of contact term is chosen!

    xsq =dble(													& 
        &    (g3inv**2*(Gam2*Gam2c - g2invprime*gb1*gb1c)*slf0(1,2)**2 + g2inv**2*((Gam3*Gam3c - g3invprime*gb1*gb1c)	&
        &	  * slf0(1,3)**2 + g3inv**2*(Gam1*Gam1c + gb1*gb1c*slfprime(1,1)) + g3inv*slf0(1,3)*(Gam1c*Gam3 + Gam1*Gam3c 	&
        &   + 2*gb1*gb1c*slfprime(1,3))) + slf0(2,3)**2*(-(g3invprime*gb1*gb1c*slf0(1,2)**2) - g2invprime*gb1		&
        &	  * gb1c*slf0(1,3)**2 - Gam1*Gam3c*slf0(1,2)*slf0(2,3) - Gam1*Gam2c*slf0(1,3)*slf0(2,3) + Gam1*Gam1c*slf0(2,3)**2&
        &   + Gam3*slf0(1,2)*(Gam3c*slf0(1,2) + Gam2c*slf0(1,3) - Gam1c*slf0(2,3)) + Gam2*slf0(1,3)*(Gam3c*slf0(1,2) 	&
        &   + Gam2c*slf0(1,3) - Gam1c*slf0(2,3)) + gb1*gb1c*slf0(2,3)**2*slfprime(1,1) - 2*gb1*gb1c*slf0(1,3)		&
        &   * slf0(2,3)*slfprime(1,2) - 2*gb1*gb1c*slf0(1,2)*slf0(2,3)*slfprime(1,3) + 2*gb1*gb1c*slf0(1,2)*slf0(1,3)	&
        &   * slfprime(2,3)) + g3inv*slf0(1,2)*slf0(2,3)*(Gam2c*(Gam3*slf0(1,2) + 2*Gam2*slf0(1,3) - Gam1*slf0(2,3)) 	&
        &   + Gam2*(Gam3c*slf0(1,2) - Gam1c*slf0(2,3)) - 2*gb1*gb1c*(g2invprime*slf0(1,3) + slf0(2,3)*slfprime(1,2) 	&
        &   - slf0(1,2)*slfprime(2,3))) + g2inv*(g3inv**2*slf0(1,2)*(Gam1c*Gam2 + Gam1*Gam2c + 2*gb1*gb1c*slfprime(1,2)) 	&
        &   + g3inv*(Gam2c*slf0(1,3)*(Gam3*slf0(1,2) + Gam1*slf0(2,3)) + Gam2*slf0(1,3)*(Gam3c*slf0(1,2) + Gam1c*slf0(2,3))&
        &   + slf0(2,3)*(Gam1*Gam3c*slf0(1,2) + Gam1c*(Gam3*slf0(1,2) - 2*Gam1*slf0(2,3)) + 2*gb1*gb1c*(-(slf0(2,3)	&
        &   * slfprime(1,1)) + slf0(1,3)*slfprime(1,2) + slf0(1,2)*slfprime(1,3))) + 2*gb1*gb1c*slf0(1,2)*slf0(1,3)	&
        &   * slfprime(2,3)) + slf0(1,3)*slf0(2,3)*(Gam3c*(Gam2*slf0(1,3) - Gam1*slf0(2,3)) + Gam3*(2*Gam3c*slf0(1,2) 	&
        &   + Gam2c*slf0(1,3) - Gam1c*slf0(2,3)) - 2*gb1*gb1c*(g3invprime*slf0(1,2) + slf0(2,3)*slfprime(1,3) 		&
        &	  - slf0(1,3)*slfprime(2,3)))))/(gb1*gb1c*(-(g2inv*g3inv) + slf0(2,3)**2)**2)					&
        &    )
    xsq=1.d0/xsq
    if (xsq.le.0.d0) xsq=100.d0		! chi_2 huge, fit will take other direction
    mbare_N = dble(													&
        &       mn_phys + xsq*(-slf0(1,1) + (g3inv*slf0(1,2)**2 + slf0(1,3)*(g2inv*slf0(1,3) + 2*slf0(1,2)*slf0(2,3))) 	&
        &       /(-(g2inv*g3inv) + slf0(2,3)**2))										&
        &        )
    return
end subroutine xinvsq3r

!=======================================================================!

subroutine slf_get(slf_value,ez,iv,k_j)
    use input4
    use tmat4
    use vertices
    use vertices_5h7h     				! (newres) Check if one could leave out here; doesn't make much sense.    

    implicit none
    integer :: k_j,iv    			
    complex(kind(0.d0)) :: ez
    complex(kind(0.d0)),dimension(n_r,n_r) :: slf_value

    call tmat(ez,iv,iv,k_j)  				! the non-pole t-matrix; mesh_q (non-optimized!!) has already been called 
    ! in rnm (there also: rot_mat, set_coeff_jls)
    if(k_j==1 .and. iv==2) call g_res1hp_calc(ez,iv,k_j)   ! 1/2+
    call f_ind_c_calc(iv,k_j,f_drs_c,f_ind_c,f_bar_c) ! this line and next one repeated in t_n_pn
    call f_ind_a_calc(iv,k_j,f_drs_a,f_ind_a,f_bar_a)
    call slf_calc(iv,k_j)    

    slf_value=slf  ! 

end subroutine slf_get

!=======================================================================!
!
! pole t-matrix: t_fs_calc;     f_ind_c_calc; f_ind_a_calc; slf_calc;
!                res_prop_calc; tmat_s_calc;  t_add;        g_res_calc
!
!=======================================================================!

subroutine t_fs_calc(ez,iv,k_j)  ! dressed vertex, self energy, resonance propagator
    use input4

    implicit none
    integer :: iv,k_j
    complex(kind(0.d0)) :: ez

    call f_ind_c_calc(iv,k_j,f_drs_c,f_ind_c,f_bar_c)
    call f_ind_a_calc(iv,k_j,f_drs_a,f_ind_a,f_bar_a)
    call slf_calc(iv,k_j) 
    call res_prop_calc(ez,iv,k_j) 

end subroutine t_fs_calc

!=======================================================================!

subroutine f_ind_c_calc(iv,k_j,f_dr,f_id,f_br)
    use input4

    implicit none
    integer :: iv,k_j
    integer :: i_rs
    integer :: n1
    integer :: i_c1,i_c2,i_q1,i_q2
    complex(kind(0.d0)) :: f_dr(n_r,n_c,max_q)
    complex(kind(0.d0)) :: f_br(n_r,n_c,max_q)
    complex(kind(0.d0)) :: f_id(n_r,n_c,max_q)
    n1=n+1
    f_id=0.
    do i_rs=1,max_nr(iv,k_j)
      do i_c1=1,n_c
        if (i_c1<8.or.i_c1>11) then
          do i_q1=1,n1
            do i_c2=1,n_c
              if (i_c2<8.or.i_c2>11) then
                do i_q2=1,n1
                  f_id(i_rs,i_c1,i_q1) = f_id(i_rs,i_c1,i_q1) + &
                      & f_br(i_rs,i_c2,i_q2)*u(i_c2,i_q2)*tmatrix(i_q1,i_q2,iv,i_c2,i_c1)
                end do
              end if
            end do  ! i_c2
          end do  ! i_q1
        end if
      end do  ! i_c1 
    end do  ! i_rs

    do i_rs=1,max_nr(iv,k_j)
      do i_c1=1,n_c
        do i_q1=1,n1
          f_dr(i_rs,i_c1,i_q1)=f_br(i_rs,i_c1,i_q1)+f_id(i_rs,i_c1,i_q1)
        end do  ! i_q1
      end do  ! i_c1
    end do  ! i_rs
end subroutine f_ind_c_calc

!=======================================================================!

subroutine f_ind_a_calc(iv,k_j,f_dr,f_id,f_br)
    use input4

    implicit none
    integer :: iv,k_j
    integer :: i_rs
    integer :: n1
    integer :: i_c1,i_c2,i_q1,i_q2
    complex(kind(0.d0)) :: f_dr(n_c,max_q,n_r)
    complex(kind(0.d0)) :: f_br(n_c,max_q,n_r)
    complex(kind(0.d0)) :: f_id(n_c,max_q,n_r)

    n1=n+1
    f_id=0.
    do i_rs=1,max_nr(iv,k_j)
      do i_c1=1,n_c
        if (i_c1<8.or.i_c1>11) then
          do i_q1=1,n1
            f_id(i_c1,i_q1,i_rs)=(0.d0,0.d0)
            do i_c2=1,n_c
              if (i_c2<8.or.i_c2>11) then
                do i_q2=1,n1
                  f_id(i_c1,i_q1,i_rs) = f_id(i_c1,i_q1,i_rs) + &
                      & tmatrix(i_q2,i_q1,iv,i_c1,i_c2)*u(i_c2,i_q2)*f_br(i_c2,i_q2,i_rs)
                end do  ! i_q2
              end if
            end do  ! i_c2
          end do  ! i_q1
        end if
      end do  ! i_c1 
    end do  ! i_rs

    do i_c1=1,n_c
      do i_q1=1,n1
        do i_rs=1,max_nr(iv,k_j)
          f_dr(i_c1,i_q1,i_rs)=f_br(i_c1,i_q1,i_rs)+f_id(i_c1,i_q1,i_rs)
        end do  ! i_rs
      end do  ! i_q1
    end do  ! i_c1

end subroutine f_ind_a_calc

!=======================================================================!

subroutine slf_calc(iv,k_j)
    use input4

    implicit none
    integer :: k_j,iv    ! k_j and iv determine the quantum numbers of the channel
    integer :: n1
    integer :: i_rs_a,i_rs_b
    integer :: i_c1,i_c2,i_q1,i_q2

    n1=n+1

    slf = (0.d0,0.d0)

    do i_rs_a=1,max_nr(iv,k_j)
      do i_rs_b=1,max_nr(iv,k_j)
        do i_c1=1,n_c
          if (i_c1<8.or.i_c1>11) then
            do i_q1=1,n1
              slf(i_rs_a,i_rs_b)=slf(i_rs_a,i_rs_b) + &
                  &  f_bar_c(i_rs_a,i_c1,i_q1)*u(i_c1,i_q1)*f_drs_a(i_c1,i_q1,i_rs_b) 
            end do  ! i_q1
          end if
        end do  ! i_c1
      end do  ! i_rs_b=1,max_nr(iv,k_j)
    end do  ! i_rs_a=1,max_nr(iv,k_j)

end subroutine slf_calc

!=======================================================================!

subroutine res_prop_calc(e_z,iv,k_j)
    use input4

    implicit none
    complex(kind(0.d0)) :: e_z
    integer :: k_j,iv   ! k_j and iv determine the quantum numbers of the channel
    integer :: i_rs1,i_rs2,max_res
    complex(kind(0.d0)) :: s11,s12,s21,s22,matt(3,3)

    max_res=max_nr(iv,k_j)
    res_prop_wo_det=0.d0
    res_prop=0.d0

    if(max_res==1.and.ct_on(iv,k_j)==1)then
      res_det=dcmplx(1.d0,0.d0) - slf(1,1) 
      res_prop(1,1)=1.d0/res_det
      res_prop_wo_det(1,1)=1.d0
    else
      res_det=e_z - m_bare(iv,k_j,1) - slf(1,1) 
      res_prop(1,1)=1.d0/res_det
      res_prop_wo_det(1,1)=1.d0
    end if

    if(max_res==2.and.ct_on(iv,k_j)==1)then
      !        s11=e_z - m_bare(iv,k_j,2) - slf(2,2)
      s11=dcmplx(1.d0,0.d0) - slf(2,2)
      s22=e_z - m_bare(iv,k_j,1) - slf(1,1)
      s12=slf(1,2)
      s21=slf(2,1)
      res_det=s11*s22-s12*s21
      res_prop_wo_det(1,1)=s11
      res_prop_wo_det(1,2)=s12
      res_prop_wo_det(2,1)=s21
      res_prop_wo_det(2,2)=s22
      res_prop=res_prop_wo_det/res_det
    else
      s11=e_z - m_bare(iv,k_j,2) - slf(2,2)
      !        s11=dcmplx(1.d0,0.d0) - slf(2,2)
      s22=e_z - m_bare(iv,k_j,1) - slf(1,1)
      s12=slf(1,2)
      s21=slf(2,1)
      res_det=s11*s22-s12*s21
      res_prop_wo_det(1,1)=s11
      res_prop_wo_det(1,2)=s12
      res_prop_wo_det(2,1)=s21
      res_prop_wo_det(2,2)=s22
      res_prop=res_prop_wo_det/res_det
    end if

    if(max_res==3)then				! Maximum 2 resonances plu a ct term, so no condition on ct_on here.
      matt(1,1)=e_z - m_bare(iv,k_j,1) - slf(1,1)
      matt(2,2)=e_z - m_bare(iv,k_j,2) - slf(2,2)
      matt(3,3)=dcmplx(1.d0,0.d0)      - slf(3,3)
      matt(1,2)=-slf(2,1)
      matt(1,3)=-slf(3,1)
      matt(2,1)=-slf(1,2)
      matt(2,3)=-slf(3,2)
      matt(3,1)=-slf(1,3)
      matt(3,2)=-slf(2,3)
      res_det= matt(3,1)*matt(2,2)*matt(1,3) &
          & -matt(2,1)*matt(3,2)*matt(1,3) &
          & -matt(3,1)*matt(1,2)*matt(2,3) &
          & +matt(1,1)*matt(3,2)*matt(2,3) &
          & +matt(2,1)*matt(1,2)*matt(3,3) &
          & -matt(1,1)*matt(2,2)*matt(3,3)
      res_prop_wo_det(1,1)=matt(3,2)*matt(2,3)-matt(2,2)*matt(3,3)
      res_prop_wo_det(1,2)=matt(1,2)*matt(3,3)-matt(3,2)*matt(1,3)		      
      res_prop_wo_det(1,3)=matt(2,2)*matt(1,3)-matt(1,2)*matt(2,3)		      
      res_prop_wo_det(2,1)=matt(2,1)*matt(3,3)-matt(3,1)*matt(2,3)		      
      res_prop_wo_det(2,2)=matt(3,1)*matt(1,3)-matt(1,1)*matt(3,3)		      
      res_prop_wo_det(2,3)=matt(1,1)*matt(2,3)-matt(2,1)*matt(1,3)		      
      res_prop_wo_det(3,1)=matt(3,1)*matt(2,2)-matt(2,1)*matt(3,2)		      
      res_prop_wo_det(3,2)=matt(1,1)*matt(3,2)-matt(3,1)*matt(1,2)		      
      res_prop_wo_det(3,3)=matt(2,1)*matt(1,2)-matt(1,1)*matt(2,2)
      res_prop=res_prop_wo_det/res_det		      
    end if

end subroutine res_prop_calc

!=======================================================================!

subroutine tmat_s_calc_short(iv,k_j)
    use input4
    implicit none
    integer :: k_j,iv   ! k_j and iv determine the quantum numbers of the channel
    integer :: i_c1,i_c2,i_q1,i_q2
    integer :: i_rs1,i_rs2,max_res
    integer :: n1
    tmatrix_pole_short=0.
    n1=n+1
    max_res=max_nr(iv,k_j)
    tmatrix_pole_short=dcmplx(0.d0,0.d0)
    do i_c1=1,n_c
      do i_c2=1,n_c
        do i_rs1=1,max_res
          do i_rs2=1,max_res
            tmatrix_pole_short(iv,i_c1,i_c2) = tmatrix_pole_short(iv,i_c1,i_c2)  + &
                & f_drs_a(i_c1,n1,i_rs1)*res_prop(i_rs1,i_rs2)*f_drs_c(i_rs2,i_c2,n1)
          end do
        end do
        tshort(i_c1,i_c2,iv,k_j)=tmatrix_pole_short(iv,i_c1,i_c2)+tmatrix(n1,n1,iv,i_c1,i_c2)
      end do  ! i_c2
    end do  ! i_c1
end subroutine tmat_s_calc_short

!=======================================================================!

subroutine tmat_s_calc(e_z,iv,k_j)
    use input4

    implicit none
    complex(kind(0.d0)) :: e_z
    integer :: k_j,iv   ! k_j and iv determine the quantum numbers of the channel
    integer :: i_c1,i_c2,i_q1,i_q2
    integer :: i_rs1,i_rs2,max_res
    integer :: n1

    n1=n+1
    max_res=max_nr(iv,k_j)

    do i_c1=1,n_c
      do i_c2=1,n_c
        do i_q1=1,n1
          do i_q2=1,n1
            tmatrix_pole(i_q2,i_q1,iv,i_c1,i_c2) = (0.d0, 0.d0)
            do i_rs1=1,max_res
              do i_rs2=1,max_res
                tmatrix_pole(i_q2,i_q1,iv,i_c1,i_c2) = tmatrix_pole(i_q2,i_q1,iv,i_c1,i_c2) + &
                    & f_drs_a(i_c1,i_q1,i_rs1)*res_prop(i_rs1,i_rs2)*f_drs_c(i_rs2,i_c2,i_q2)
              end do
            end do
          end do
        end do
      end do  ! i_c2
    end do  ! i_c1

end subroutine tmat_s_calc

!=======================================================================!

subroutine t_add(iv,k_j)
    use input4

    implicit none
    integer :: iv,k_j
    integer :: i1,i2
    integer :: n1,i_q1, i_q2

    n1=n+1

    do i1=1,n_c
      do i2=1,n_c
        do i_q1=1,n1
          do i_q2=1,n1
            tmatrix(i_q2,i_q1,iv,i1,i2)= tmatrix(i_q2,i_q1,iv,i1,i2)+tmatrix_pole(i_q2,i_q1,iv,i1,i2)
          end do
        end do
      end do  ! i2=1,n_c
    end do  ! i1=1,n_c

end subroutine t_add

!=======================================================================!

subroutine g_res_calc(ez,iv,k_j)
    use vertices
    use vertices_5h7h	! (newres)	  
    use input4		! We need to initialize f_bar_c and f_bar_a with zero, in case a given partial   (newres)
    ! wave has no resonance at all which happens for two cases in k_j=4!
    ! Thus, we need to make f_bar_c and f_bar_a avaliable now with "use input4"
    implicit none
    integer :: iv,k_j
    complex(kind(0.d0)) :: ez
    f_bar_c=0.		! This initializes with zero in case there is no resonance at all. (newres)
    f_bar_a=0.
    if(k_j==1 .and. iv==1) call g_res1hm_calc(ez,iv,k_j)   ! 1/2-, S11
    if(k_j==1 .and. iv==3) call g_res1hm_calc(ez,iv,k_j)   ! 1/2-, S31
    if(k_j==1 .and. iv==2) call g_res1hp_calc(ez,iv,k_j)   ! 1/2+, P11
    if(k_j==1 .and. iv==4) call g_res1hp_calc(ez,iv,k_j)   ! 1/2+, P31

    if(k_j==2 .and. iv==1) call g_res3hp_calc(ez,iv,k_j)   ! 3/2+, P13
    if(k_j==2 .and. iv==3) call g_res3hp_calc(ez,iv,k_j)   ! 3/2+, P33
    if(k_j==2 .and. iv==2) call g_res3hm_calc(ez,iv,k_j)   ! 3/2-, D13
    if(k_j==2 .and. iv==4) call g_res3hm_calc(ez,iv,k_j)   ! 3/2-, D33

    if(k_j==3 .and. iv==1) call g_res5hm_calc(ez,iv,k_j)   ! 5/2-, D15	
    if(k_j==3 .and. iv==2) call g_res5hp_calc(ez,iv,k_j)   ! 5/2+, F15	
    if(k_j==3 .and. iv==3) call g_res5hm_calc(ez,iv,k_j)   ! 5/2-, D35	
    if(k_j==3 .and. iv==4) call g_res5hp_calc(ez,iv,k_j)   ! 5/2+, F35

    if(k_j==4 .and. iv==1) call g_res7hp_calc(ez,iv,k_j)   ! 7/2+, F17	
    if(k_j==4 .and. iv==2) call g_res7hm_calc(ez,iv,k_j)   ! 7/2-, G17	
    if(k_j==4 .and. iv==3) call g_res7hp_calc(ez,iv,k_j)   ! 7/2+, F37	
    if(k_j==4 .and. iv==4) call g_res7hm_calc(ez,iv,k_j)   ! 7/2-, G37

    if(k_j==5 .and. iv==1) call g_res9hm_calc(ez,iv,k_j)   ! 9/2-, G19	
    if(k_j==5 .and. iv==2) call g_res9hp_calc(ez,iv,k_j)   ! 9/2+, H19	
    if(k_j==5 .and. iv==3) call g_res9hm_calc(ez,iv,k_j)   ! 9/2-, G39
    if(k_j==5 .and. iv==4) call g_res9hp_calc(ez,iv,k_j)   ! 9/2+, H39

end subroutine g_res_calc
