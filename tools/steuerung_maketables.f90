module	steuerung		!(label:maketables)
  implicit	none
  integer,parameter:: finepsi     = 1	! Use finite epsilon for sharp propagators (=1)
  integer,parameter:: chiral_sigma= 1	! Use chiral sigma meson in sigmaN propagator
  integer,parameter:: switchon_KY = 1	! KY propagators set to full (1) or zero (0)


  integer,parameter:: calltnptable= 1	! Whether (=1) or not (=0) to call make_table (which itself may be in read or write mode)
  integer,parameter:: normal_ccmn	= 1	! Do (=1) or don't do (=0) the usual loop over energies and angular momentum in ccmn4
  integer		 :: readit    	= 1	! Read from (=1) t_nonpole_table.dat
  integer,parameter:: writeit	= 1	! Write to  (=1) t_nonpole_table.dat
  integer,parameter:: call_minuit = 0	! Call Minuit optimization (=1) or not (=0)

  !==============================================================================================
  !
  !	Control complex energy part of the program
  !
  !==============================================================================================

  integer		 :: rsheet=0			! Riemann sheet; = 2 :amplitude on 2nd sheet
  integer,parameter:: activate_t2	= 0		! Make calculations in complex plane. (=1)
  integer,parameter:: pole_search	= 0		! Search for poles (=1)
  integer,parameter:: adapt_alpha = 1		! in rsheet=2 mode, whether or not to adapt alpha, 
  ! the rotation into complex plane
  integer,parameter:: gauss_opt	= 0		! Use optimized momentum Gauss point distribution (subroutine mesh_q)
  integer,parameter:: scanit	= 1		! Make scan of complex plane (=1)
  real(kind(0.d0)),parameter:: del_r_tol = 10.d0	! Convergence criterion in pole search.
  integer,parameter:: vary_pi_mass= 0 		! Vary the pion mass 
  integer,parameter:: max_i_mass	= 40 		! How many different pion masses to be varied 
  real(kind(0.d0)),parameter:: delta_mpion = 8.d0	! setp size [MeV] for pion mass variation

  !==============================================================================================
  !
  !	Control Fit
  !
  !==============================================================================================

  integer,parameter:: write_out	   = 1		! write output (KY, etaN observables, piN PW)
  integer,parameter:: renorma	   = 0		! Renormalize again in Fit (needed if P11 optimized and/or fit is called)
  integer,parameter:: start_w_fin15  = 1		! Start every T^P fit with pole-parm set from solution of our paper.




end module steuerung
