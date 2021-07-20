      subroutine sheet_plot(irles)			!label (polepath)
          use steuerung
	  use input4
          use tmat4
      implicit none
      real(kind(0.d0)) 		:: emin_r,emin_i,emax_r,emax_i,e_r,e_i,z_e,del_r,del_i,e_r_0,e_i_0,z_e_0, &
      				&  ee_pol_r,ee_pol_i,s_pion_sq,modre(n_c),phiphi(n_c),branch(n_c),branchtest(n_c),ell,c_fa
      integer 	       		:: nr,ni,i_r,i_i,islow,i_mass_pion,iv,k_j,ivres,kjres,irle,irles
      complex(kind(0.d0)) 	:: dt,ee,ee1,gw,r_ph_e,r_ph_m,a_12,a_32,sq_fa,ess,q_pion,k_gamma,respin,e_en,om_pi
      complex(kind(0.0d0)),dimension(n_c):: cou,ao,a1
      						 	! cou(n_c) is the output of resid: coupling strengths in the n_c channels
      							! ao is the constant term a_0 for piN --> channels 1 to n_c.
							! a1 is the following term. Can be numerically unstable!
      complex(kind(0.d0)),dimension(n_c,max_q)  :: couoff	! Coupling strength off shell; output
      complex(kind(0.d0)),dimension(2)  :: r_photo	! Residue multipole [no units]


        mass_pion= ms_b_c(1)
	mass_pion2=mass_pion
	call ms_c_def  ! masses employed in reaction channels have to be refilled.<input4_mod.f90
	! WRITE INITIAL POLE DATA FOR CHECK
	if (irles.eq.1) then
	 write(1124,*)'Check if initial pole data is correctly read:'
	 write(1124,*)'Res-No, iv, k_j, Pole position'
	 write(1124,*)'--------------------------------'
	 do irle=1,n_poles
	  write(1124,'(3i4,2f8.0,a40)')resno(irle),iv_reso(irle),k_j_reso(irle),inipole(irle),res_name(irle)
	 end do
	end if 						!(Check pole positions are read in correctly)
	
	! Define iv, k_j, pole position for the resonance No irles:
	iv_memo=iv_reso(irles)
	k_j_memo=k_j_reso(irles)	
	ee=inipole(irles)			
        ee1=ee+dcmplx(3.d0,+3.d0)
	write(1124,*)''
	write(1124,*)'====================================================='
	write(1124,*)'The ',res_name(irles)
	write(1124,*)'====================================================='
	write(1124,*)'start polesearch with the two starting points: '
	write(1124,'(2f10.1)')ee
	write(1124,'(2f10.1)')ee1
	write(1124,'(a10,g8.3)')'mrho_fac= ',mrho_fac
	write(1124,'(a10,g8.3)')'msig_fac= ',msig_fac
	write(1124,'(a10,g8.3)')'mdel_fac= ',mdel_fac
	write(1124,*)'-----------------------------------------------------'
	write(1124,'(a29,i4,a13,2i4)')'Start path for resonance No. ',irles,' with iv k_j ',iv_memo,k_j_memo
	write(1124,*)'Iteration No.       Polposition [MeV]   Convergence'
	write(1124,*)'-----------------------------------------------------'
	
!=============================================================================================
!
!	Pole search and writing results for position and residue
!
!=============================================================================================
        call csearch(gw,ee,ee1,islow)		! CHECK IF ACTIVATED OR NOT!
	write(1124,'(f5.0,2f10.1,i5)')mass_pion,ee,islow
	if (islow.lt.16) then	! Write only if converged.
	call write_polepos(irles,mass_pion,ee)
	call resid(iv_memo,k_j_memo,ee,cou,ao,a1,couoff,r_photo)							
	! NOTE: cou, ao are the coupling and constant term in the upper half plane, need to be 
	! made complex conjugate (done in subroutine when writing result).
!	ao=dcmplx(0.d0,0.d0)		! REMOVE THIS LATER
!	a1=dcmplx(0.d0,0.d0)		! REMOVE THIS LATER
	call write_pa(irles,ee,cou,ao,a1) ! Write the pole approximation and a list of residues of coupling constants
	write(1124,'(a60       )')'============================================================'  
	write(1124,'(a11,a40   )')'Resonance: ',res_name(irles)  
	write(1124,'(a50       )')'============================================================'  
	write(1124,'(a30,2g13.4)')'Pole position Re, Im [MeV]    ',conjg(ee)  
	write(1124,'(a60       )')'============================================================'  
	write(1124,'(a30,2g13.4)')'Couplings [10^-3 MeV^-1/2]    '  
	write(1124,'(a60       )')'------------------------------------------------------------'  
	write(1124,'(a30,2g13.4)')'g_piN      (Re, Im)           ',conjg(cou(1))*1000.d0
	write(1124,'(a30,2g13.4)')'g_rhoN(1)  (Re, Im)           ',-conjg(cou(2))*1000.d0
	write(1124,'(a30,2g13.4)')'g_rhoN(2)  (Re, Im)           ',-conjg(cou(3))*1000.d0
	write(1124,'(a30,2g13.4)')'g_rhoN(3)  (Re, Im)           ',-conjg(cou(4))*1000.d0
	write(1124,'(a30,2g13.4)')'g_etaN     (Re, Im)           ',conjg(cou(5))*1000.d0
	write(1124,'(a30,2g13.4)')'g_piD(1)   (Re, Im)           ',conjg(cou(6))*1000.d0
	write(1124,'(a30,2g13.4)')'g_piD(2)   (Re, Im)           ',conjg(cou(7))*1000.d0
	write(1124,'(a30,2g13.4)')'g_sigN     (Re, Im)           ',-conjg(cou(8))*1000.d0
	write(1124,'(a30,2g13.4)')'g_KLam     (Re, Im)           ',conjg(cou(12))*1000.d0
	write(1124,'(a30,2g13.4)')'g_KSig     (Re, Im)           ',conjg(cou(13))*1000.d0
        ! the followong part is for photo residues --> uncommented in this version
!	r_ph_e=	-conjg(r_photo(1))		! r_photo: Multipole residue at pole in upper half plane; 	
!	r_ph_m= -conjg(r_photo(2))		! conjg: transport to lower half plane; "-": Convention.
!!	r_ph_e= 0.015000d0*exp(-dcmplx(0.d0,1.d0)*30.d0*pi/180.d0) ! Test with values for Delta(1232) from (A1,A2)
!!	r_ph_m=-0.000811d0*exp( dcmplx(0.d0,1.d0)*35.d0*pi/180.d0) ! Check if values of Tab I of 1304.4029 come out.
!	write(1124,'(a30,2g13.4)')'|Res E| [10^-3], phi [deg]    ', &
!			& abs(r_ph_e)*1000.d0,atan2(aimag(r_ph_e),dble(r_ph_e))*180.d0/pi
!	write(1124,'(a30,2g13.4)')'|Res M| [10^-3], phi [deg]    ', &
!			& abs(r_ph_m)*1000.d0,atan2(aimag(r_ph_m),dble(r_ph_m))*180.d0/pi
!	if (iv_memo==1.or.iv_memo==3) then	! "+" Multipoles; transformation to helicity basis
!	ell=dble(k_j_memo-1)			! See Eqs. (3)-(6), 1304.4029
!	a_12=-1.d0/2.d0*((ell+2.d0)*r_ph_e+ell*r_ph_m)		! a_12,a_32: Residues helicity base
!	a_32= 1.d0/2.d0*sqrt(ell*(ell+2.d0))*(r_ph_e-r_ph_m)
!!	a_12=-1.40d0/197.3d0*exp(-dcmplx(0.d0,1.d0)*39.d0*pi/180.d0)   ! Test with Delta(1232)
!!	a_32=-2.63d0/197.3d0*exp(-dcmplx(0.d0,1.d0)*27.d0*pi/180.d0)   ! Test with Delta(1232)
!	else					! "-" Multipoles
!	ell=dble(k_j_memo)
!	a_12=-1.d0/2.d0*((ell-1.d0)*r_ph_e-(ell+1.d0)*r_ph_m)
!	a_32=-1.d0/2.d0*sqrt((ell-1.d0)*(ell+1.d0))*(r_ph_e+r_ph_m)
!	end if					! "+" or "-" Multipoles
!	write(1124,'(a30,2g13.4)')'Res A^1/2 [10^-3], phi [deg]  ',abs(a_12)*1000.d0, atan2(aimag(a_12),dble(a_12))*180.d0/pi    
!	write(1124,'(a30,2g13.4)')'Res A^3/2 [10^-3], phi [deg]  ',abs(a_32)*1000.d0, atan2(aimag(a_32),dble(a_32))*180.d0/pi   
!	if (iv_memo==1.or.iv_memo==2) then	! Isospin I=1/2; C defined below Eq. (2) of 1304.4029
!	c_fa=-sqrt(3.d0)
!	else					! Isospin I=3/2
!	c_fa=sqrt(2.d0/3.d0)
!	end if
!	ess=conjg(ee)**2			! s at pole position in lower z half plane
!	ess=dcmplx(1211.d0,-49.5d0)**2		! Test with Delta(1232)
!	q_pion = sqrt((ess-(ms_f_c(1)+ms_b_c(1))**2)*(ess-(ms_f_c(1)-ms_b_c(1))**2))/(2.d0*sqrt(ess)) ! pi c.m. momentum
!	k_gamma= (ess-ms_f_c(1)**2)/(2.d0*conjg(ee))			! gamma c.m. momentum
!	e_en   = (ess+ms_f_c(1)**2-ms_b_c(1)**2)/(2.d0*conjg(ee))	! Nucleon energy
!	om_pi  = (ess+ms_b_c(1)**2-ms_f_c(1)**2)/(2.d0*conjg(ee))	! Pion energy
! 	respin = (-1.d0)*(-1.d0)*pi*q_pion*e_en*om_pi*conjg(cou(1))**2/conjg(ee) ! one "-1": T -> tau, one "-1": Convention R_piN
!!	respin = 52.d0*exp(-dcmplx(0.d0,1.d0)*47.d0*pi/180.d0) ! Test with Delta(1232)
!	sq_fa  = q_pion/k_gamma*2.d0*pi*(2.d0*k_j_memo)*conjg(ee)/(ms_f_c(1)*respin) ! see Eq (22) of 1304.4029
!	sq_fa  = c_fa*sqrt(sq_fa)
!	a_12    = sq_fa*a_12*sqrt(1000.d0)	! sqrt(1000): MeV^-1/2 --> GeV^-1/2
!	a_32	= sq_fa*a_32*sqrt(1000.d0)
!	write(10247,'(a13,8g13.4)')res_name(irles),a_12,a_32
!	if (abs(atan2(aimag(a_12),dble(a_12))*180.d0/pi)<90.d0) then		! Next two if's: only cosmetic(keep angle small),
!	  write(1124,'(a35,2g13.4)')'|A^1/2| [10^-3 GeV^-1/2], phi [deg]', &	! as done in Tab. I of 1304.4029
!		 & abs(a_12)*1000.d0, atan2(aimag(a_12),dble(a_12))*180.d0/pi
!	else	  
!	  write(1124,'(a35,2g13.4)')'|A^1/2| [10^-3 GeV^-1/2], phi [deg]', &
!		 &-abs(a_12)*1000.d0, atan2(aimag(a_12),dble(a_12))*180.d0/pi-180.d0
!	end if
!	if (abs(atan2(aimag(a_32),dble(a_32))*180.d0/pi)<90.d0)	then 
!	  write(1124,'(a35,2g13.4)')'|A^3/2| [10^-3 GeV^-1/2], phi [deg]', &
!		 & abs(a_32)*1000.d0, atan2(aimag(a_32),dble(a_32))*180.d0/pi
!	else
!	  write(1124,'(a35,2g13.4)')'|A^3/2| [10^-3 GeV^-1/2], phi [deg]', &
!		 &-abs(a_32)*1000.d0, atan2(aimag(a_32),dble(a_32))*180.d0/pi-180.d0
!	end if
!        write(11124,'(10g13.4)')conjg(cou(1))*1000.d0,-conjg(cou(2))*1000.d0,-conjg(cou(3))*1000.d0, &
!		&-conjg(cou(4))*1000.d0,conjg(cou(5))*1000.d0,conjg(cou(6))*1000.d0,conjg(cou(7))*1000.d0,   &
!		&-conjg(cou(8))*1000.d0,conjg(cou(12))*1000.d0,conjg(cou(13))*1000.d0
	write(1124,'(a60       )')'============================================================'  
	call resibranch(ee,cou,modre,phiphi,branch,branchtest)
	write(1124,'(a40       )')'R [MeV] and phi [deg], NR '  
	write(1124,'(a50       )')'============================================================'  
	write(1124,'(a30,3f10.4)')'|R|, phi, NR piN --> piN      ',modre( 1)*aimag(ee),phiphi( 1),modre( 1)
	write(1124,'(a30,3f10.4)')'|R|, phi, NR piN --> etaN     ',modre( 5)*aimag(ee),phiphi( 5),modre( 5)
	write(1124,'(a30,3f10.4)')'|R|, phi, NR piN --> piDel (1)',modre( 6)*aimag(ee),phiphi( 6),modre( 6)
	write(1124,'(a30,3f10.4)')'|R|, phi, NR piN --> piDel (2)',modre( 7)*aimag(ee),phiphi( 7),modre( 7)
	write(1124,'(a30,3f10.4)')'|R|, phi, NR piN --> KLam     ',modre(12)*aimag(ee),phiphi(12),modre(12)
	write(1124,'(a30,3f10.4)')'|R|, phi, NR piN --> KSig     ',modre(13)*aimag(ee),phiphi(13),modre(13)
	write(1124,'(a60       )')'============================================================'  
	write(1124,'(a40       )')'Branching ratios                        '  
	write(1124,'(a60       )')'============================================================'  
	write(1124,'(a40,3f10.4)')'Gamma_piN [MeV],  G_piN/G_tot [%], Trans',2.d0*branch( 1)/100.d0*aimag(ee),branch( 1),modre(   1)*100.d0
	write(1124,'(a40,3f10.4)')'Gamma_etN [MeV],  G_etN/G_tot [%], Trans',2.d0*branch( 5)/100.d0*aimag(ee),branch( 5),modre( 5)*100.d0
	write(1124,'(a40,3f10.4)')'Gamma_piD [MeV],  G_piD/G_tot [%], Trans',2.d0*branch( 6)/100.d0*aimag(ee),branch( 6),modre( 6)*100.d0
	write(1124,'(a40,3f10.4)')'Gamma_piD [MeV],  G_piD/G_tot [%], Trans',2.d0*branch( 7)/100.d0*aimag(ee),branch( 7),modre( 7)*100.d0
	write(1124,'(a40,3f10.4)')'Gamma_KLa [MeV],  G_KLa/G_tot [%], Trans',2.d0*branch(12)/100.d0*aimag(ee),branch(12),modre( 12)*100.d0
	write(1124,'(a40,3f10.4)')'Gamma_KSi [MeV],  G_KSi/G_tot [%], Trans',2.d0*branch(13)/100.d0*aimag(ee),branch(13),modre( 13)*100.d0
	write(1124,*             )''  
	write(1124,*             )''  
	write(1124,*             )''  
	write(1124,*             )''  
	call write_resid(irles,cou,mass_pion)
	write(*,*)'Coupling strengths:'
	write(*,*)cou
	write(*,*)'a_0 is:'
	write(*,*)ao
	call write_off_shell_resid(irles,couoff,cou,ee,iv_memo,k_j_memo) ! Write away off-shell 
								!coupling strengths at pole.
	end if		! if (islow.lt.16) then


!=============================================================================================
!
!	Vary pion mass
!
!=============================================================================================
	
	if (vary_pi_mass==1) then
        do i_mass_pion=1,max_i_mass
           mass_pion=mass_pion+delta_mpion 
	   mass_pion2=mass_pion
           call ms_c_def  ! masses employed in reaction channels have to be refilled.<input4_mod.f90
	   ee1=ee+dcmplx(1.d0,1.d0)
	   ee=ee-dcmplx(1.d0,1.d0)
           call csearch(gw,ee,ee1,islow)
           write(1124,*)mass_pion,ee,islow
	   if (islow.lt.16) then	! Write only if converged.
	    call write_polepos(irles,mass_pion,ee)
!	    if (i_mass_pion.eq.max_i_mass) then
	     call resid(iv_memo,k_j_memo,ee,cou,ao,a1,couoff,r_photo)
	     call write_resid(irles,cou,mass_pion)     
!	    end if
	   end if
        end do 		! i_mass_pion=1,max_i_mass
	end if 		! vary_pi_mass==1
        write(1124,*)' '
        return
        end subroutine sheet_plot

!===================================================================================================
!===================================================================================================

	subroutine resibranch(zo,resi,modre,phiphi,branch,branchtest)
	  use input4
	implicit none
	complex(kind(0.0d0))	:: zo,resi(n_c),resi2,alp(n_c),qqcm(n_c),phasfac,ommeson,ombar
	real(kind(0.0d0))	:: modre(n_c),phiphi(n_c),branch(n_c),branchtest(n_c)
	integer 		:: i
	call qon_c(conjg(zo),qqcm)
	do i=1,n_c
	  ommeson=sqrt(qqcm(i)**2+ms_b_c(i)**2)
	  ombar  =sqrt(qqcm(i)**2+ms_f_c(i)**2)	  
	  alp(i)=sqrt(qqcm(i)*ommeson*ombar/conjg(zo))*conjg(resi(i))
	end do
	
	do i=1,n_c
	  modre(i) =pi*abs(alp(1)*alp(i))/aimag(zo)			! Normalized |residue| of piN --> MB
	  phiphi(i)=atan2(aimag(alp(1)*alp(i)),dble(alp(1)*alp(i)))*180.d0/pi-180.d0 ! Phase of residue piN --> MB
	  if (phiphi(i)<=-180.d0) phiphi(i)=phiphi(i)+360.d0
	  branch(i)=pi*abs(alp(i)**2)/aimag(zo)*100.d0			! Branching ratio
	end do
	  branchtest=0.
	end subroutine resibranch

!---------------------------------------------------------------------------------------------------

	subroutine write_polepos(irles,mapi,ee)
	  use input4
	implicit	none
	complex(kind(0.d0))::	ee
	real(kind(0.d0)) ::	mapi
	integer	::		irles,unii
	unii=1130+irles
	write(*,*)unii,ee,mapi
	write(unii,145)ee,mapi	
 145    format(15g15.6)     	
	return
	end subroutine write_polepos

!---------------------------------------------------------------------------------------------------

	subroutine write_resid(irles,cou,mapi)
	  use input4
	implicit	none
        complex(kind(0.0d0)),dimension(n_c)::	cou !cou(n_c) is the output of resid: coupling strengths in the n_c channels!	
	real(kind(0.d0)) ::	mapi
	integer	::		irles,unii
	unii=1140+irles
!	write(*,*)unii,cou,mapi
	write(unii,135)conjg(cou(1)),conjg(cou( 2)),conjg(cou( 3)),conjg(cou( 4)),conjg(cou( 5)),conjg(cou(6)),conjg(cou(7)),conjg(cou(8)), &
	              &conjg(cou(9)),conjg(cou(10)),conjg(cou(11)),conjg(cou(12)),conjg(cou(13)),mapi	
 135    format(27(15g15.6))     	
	return
	end subroutine write_resid

!---------------------------------------------------------------------------------------------------

	subroutine write_off_shell_resid(irles,couoff,cou,ee,iv,k_j)
	 use input4
 	 use cont_unstable
	implicit	none
	complex(kind(0.d0)),dimension(n_c,max_q)  :: couoff		
        complex(kind(0.0d0)),dimension(n_c) ::	cou !cou(n_c) is the output of resid: coupling strengths in the n_c channels!	
	complex(kind(0.0d0)) :: ee,discpio,disceta,discdel,aa(2),resitest(4)
	integer	:: irles, unii,i,iv,k_j
	unii=1210+irles
	write(unii,*  )'Quantities needed for Eq. (17) of photo_residue_V02.pdf'
	write(unii,*  )'(Contribution from the Non-pole part)                  '
	write(unii,*  )'======================================================='
	write(unii,*  )''
	write(unii,215)'Pole position ','[MeV]: ',conjg(ee)
	write(unii,*  )'On-shell Residue [MeV^(-1)] (piN, etaN, piD(1), piD(2) --> piN): '
	write(unii,215)conjg(cou(1)**2),conjg(cou(5)*cou(1)),conjg(cou(6)*cou(1)),conjg(cou(7)*cou(1))
	write(unii,*  )'Coupling constants x 1000 (you dont need them):'
	write(unii,215)conjg(cou(1))*1000.d0,conjg(cou(5))*1000.d0,conjg(cou(6))*1000.d0,conjg(cou(7))*1000.d0
	write(unii,  *)'Off->on-shell Residues [MeV^(-1)] (piN, etaN, piD(1), piD(2) --> piN_on): '
	write(unii,  *)'(This is also written to separate files to make readout easier)'	
	write(unii,  *)'Order: Re, Im xgaus [MeV]; Re, Im wgaus; 3 x Re, Im Residues'
	do i=1,n
	write(unii,215)   xgaus(i),wgaus(i),conjg(couoff(1,i)*cou(1)),conjg(couoff(5,i)*cou(1)),conjg(couoff(6,i)*cou(1)),conjg(couoff(7,i)*cou(1))
	write(unii+10,215)xgaus(i),wgaus(i),conjg(couoff(1,i)*cou(1)),conjg(couoff(5,i)*cou(1)),conjg(couoff(6,i)*cou(1)),conjg(couoff(7,i)*cou(1))
	end do
	write(unii,*  )'Next, we write the value of delta G from Eq. (15) from photo_residue_V02.pdf'
	write(unii,*  )'at the pole position in the lower half plane zo.'
	write(unii,*  )'They are given regardless if the resonance couples to the'
	write(unii,*  )'corresponding channels; if not, set delta G to zero!'
	write(unii,*  )'Also, set them to zero if Re(z_0)<threshold of a channel!'
	call qon_c(ee,qon)  ! on shell momenta 
	discpio=(0.d0,1.d0)*tpi*qon(1)*sqrt(qon(1)**2+ms_b_c(1)**2)*sqrt(qon(1)**2+ms_f_c(1)**2)/ee
	disceta=(0.d0,1.d0)*tpi*qon(5)*sqrt(qon(5)**2+ms_b_c(5)**2)*sqrt(qon(5)**2+ms_f_c(5)**2)/ee
	discdel=diffsupd(ee,1.d0)
	write(unii,215)'dG_piN (z_0) : Re, Im = ',conjg(discpio)
	write(unii,215)'dG_etaN(z_0) : Re, Im = ',conjg(disceta)
	write(unii,215)'dG_piD (z_0) : Re, Im = ',conjg(discdel)
	write(unii,*  )''	
	write(unii,*  )'Quantities needed for Eqs. (19,21)                     '
	write(unii,*  )'(Contribution from the Pole part)                      '
	write(unii,*  )'======================================================='
	write(unii,*  )''
	write(unii,*  )'A=A_1 [Eq. (19), 1 Reson.] and A_2 [Eq. (21), 2 Reson.]'
	call res_prop_residue(iv,k_j,aa)
	write(unii,215)'A_1 = ',aa(1)
	write(unii,215)'A_2 = ',aa(2)
	write(unii,*  )'These values can be tested: Multiplied with Gamma^c,   '
	write(unii,*  )'they must give the hadronic on-shell residues quoted   '
	write(unii,*  )'above. The relative deviations in % are                '
	write(unii,*  )'(order: see above; a NAN means the residues is zero):  '
	call test_kanzo1(aa,iv,k_j,resitest)
	write(unii,215)(abs((conjg(resitest(1))-conjg(cou(1)**2    ))/conjg(resitest(1))))*100.d0, &
	             & (abs((conjg(resitest(2))-conjg(cou(5)*cou(1)))/conjg(resitest(2))))*100.d0, &	   
	             & (abs((conjg(resitest(3))-conjg(cou(6)*cou(1)))/conjg(resitest(3))))*100.d0, &	   
	             & (abs((conjg(resitest(4))-conjg(cou(7)*cou(1)))/conjg(resitest(4))))*100.d0
	write(unii,*)'Error may become large in some occasions if residue      '
	write(unii,*)'itself is tiny. For larger residues, error is always tiny.'
 215    format(17(15g15.6))     	
	return
	end subroutine write_off_shell_resid
	
!---------------------------------------------------------------
!	NEEDED FOR OUTPUT FOR FEI AND KANZO
!---------------------------------------------------------------
        
	subroutine res_prop_residue(iv,k_j,summ)
	 use input4
	implicit none
 	integer :: i_rs1,max_res,i,n1,iv,k_j
	complex(kind(0.d0)) :: summ(2)
	
	n1=n+1
	summ=0.d0
	max_res=max_nr(iv,k_j)
	do i=1,max_res
	do i_rs1=1,max_res
	summ(i)=summ(i)+f_drs_a(1,n1,i_rs1)*res_prop_wo_det(i_rs1,i)/res_det_prime
	end do							! res_det_prime is calculated in res_auto.f90
								! Note: f_drs_a(channel=1=piN,n1=on-shell outgoing piN,i_rs1)
								! AND: Note the order of calculation in res_auto.f90:
								! It means that res_prop and f_drs_a are indeed calculated
								! at pole position, not at z_0+- delta. Fine.
	end do
	end subroutine res_prop_residue
	
!---------------------------------------------------------------

	subroutine test_kanzo1(aa,iv,k_j,resitest)
	 use input4
	implicit none
	complex(kind(0.d0)) :: aa(2),resitest(4)
	integer :: iv,k_j,n1,max_res,i_rs2
	resitest=0.d0
	n1=n+1
	max_res=max_nr(iv,k_j)

	do i_rs2=1,max_res	
	resitest(1)=resitest(1)+aa(i_rs2)*f_drs_c(i_rs2,1,n1)	! piN, etaN, piD(1), piD(2)
	resitest(2)=resitest(2)+aa(i_rs2)*f_drs_c(i_rs2,5,n1)	! are the four entries of resitest.
	resitest(3)=resitest(3)+aa(i_rs2)*f_drs_c(i_rs2,6,n1)
	resitest(4)=resitest(4)+aa(i_rs2)*f_drs_c(i_rs2,7,n1)
	end do
	
	end subroutine test_kanzo1
	
	
!---------------------------------------------------------------

	! Write the pole approximations for all channels.
	subroutine write_pa(irles,ee,cou,ao,a1)
	 use input4
	implicit	none
	integer :: irles,unii,i,zi
	real(kind(0.d0)) :: zre
	real(kind(0.d0)),dimension(n_poles) :: pa_low,pa_up
	complex(kind(0.d0)) :: ee,patemp,phasfac
	complex(kind(0.d0)),dimension(n_c):: cou,ao,a1
	complex(kind(0.d0)),dimension(n_c)::  pa	
	unii=2189+irles
	pa=dcmplx(0.d0,0.d0)

         ao=0.d0 			! Desactivate if wished!
	 a1=0.d0			! Desactivate if wished!
	  				       
	call pa_limits(pa_low,pa_up)
	do zi=1,100
	  zre=pa_low(irles)+(pa_up(irles)-pa_low(irles))/100.d0*zi
	  do i=1,n_c
	    if (i==2.or.i==3.or.i==4.or.i==8) then			! Provide a minus in case of rhoN, sigmaN final state
	    patemp=-conjg(cou(1)*cou(i)/(zre-ee)+ao(i)+a1(i)*(zre-ee)) 	! POLE APPROXIMATION MADE!
	    else								! Provide a plus in case of piN, etaN, piDelta final state
	    patemp= conjg(cou(1)*cou(i)/(zre-ee)+ao(i)+a1(i)*(zre-ee)) 	! POLE APPROXIMATION MADE!	  
	    end if
	    pa(i)= patemp*phasfac(dcmplx(zre,0.d0),i)	     		! Multiply with phase factor for each channel
	  end do ! i
	 write(unii,155)zre,pa
	 end do ! zre values
	 							! Write residues and coupling constants in a long list.
	 write(1201,155)irles,conjg(ee),conjg(cou(1)**2),-conjg(cou(1)*cou(2)),-conjg(cou(1)*cou(3)),-conjg(cou(1)*cou(4)), &
	 & conjg(cou(1)*cou(5)),conjg(cou(1)*cou(6)),conjg(cou(1)*cou(7)),-conjg(cou(1)*cou(8)),conjg(cou(1)*cou(9)),       &
	 & conjg(cou(1)*cou(10)),conjg(cou(1)*cou(11)),conjg(cou(1)*cou(12)),conjg(cou(1)*cou(13))
	 write(1202,155)irles,conjg(ee),conjg(cou(1)),-conjg(cou(2)),-conjg(cou(3)),-conjg(cou(4)), &
	 & conjg(cou(5)),conjg(cou(6)),conjg(cou(7)),-conjg(cou(8)),conjg(cou(9)),conjg(cou(10)),conjg(cou(11)), &	
	 & conjg(cou(12)),conjg(cou(13))
	 
 155    format(27(15g15.6))   
 165    format(29(15g15.6))   
 	return
	end subroutine write_pa  

!---------------------------------------------------------------
	
	! Define limits in energy  in which the pole approximation is evaluated (=ceontered around resonance position within
	! a reasonable range).
	subroutine pa_limits(pa_low,pa_upp)
	 use input4
	implicit none
	real(kind(0.d0)),dimension(n_poles) :: pa_low,pa_upp
	pa_low=0.d0
	pa_upp=0.d0
	pa_low( 1)=1465.d0 ! Limits N(1535) S11
	pa_upp( 1)=1900.d0
	pa_low( 2)=1465.d0 ! Limits N(1650) S11
	pa_upp( 2)=1900.d0
	pa_low( 3)=1100.d0 ! Limits N(1440) P11
	pa_upp( 3)=1800.d0 ! 
	pa_low( 4)=1400.d0 ! Limits N(1650) P11
	pa_upp( 4)=1850.d0
	pa_low( 5)=1500.d0 ! Limits N(1710) P11
	pa_upp( 5)=1900.d0
	pa_low( 6)=1450.d0 ! Limits D(1620) S31
	pa_upp( 6)=1800.d0
	pa_low( 7)=1500.d0 ! Limits D(1910) P31
	pa_upp( 7)=2300.d0
	pa_low( 8)=1400.d0 ! Limits N(1720) P13
	pa_upp( 8)=1900.d0
	pa_low( 9)=1300.d0 ! Limits N(1520) D13
	pa_upp( 9)=1900.d0
	pa_low(10)=1100.d0 ! Limits D(1232) P33
	pa_upp(10)=1500.d0
	pa_low(11)=1500.d0 ! Limits D(1600) P33
	pa_upp(11)=1900.d0
	pa_low(12)=1500.d0 ! Limits D(1920) P33
	pa_upp(12)=2200.d0
	pa_low(13)=1400.d0 ! Limits D(1700) D33
	pa_upp(13)=2000.d0
	pa_low(14)=1200.d0 ! Limits N(1675) D15
	pa_upp(14)=2000.d0
	pa_low(15)=1400.d0 ! Limits N(1680) F15
	pa_upp(15)=2000.d0
	pa_low(16)=1400.d0 ! Limits D(1930) D35
	pa_upp(16)=2400.d0
	pa_low(17)=1500.d0 ! Limits D(1905) F35
	pa_upp(17)=2100.d0
	pa_low(18)=1500.d0 ! Limits N(XXXX) F17
	pa_upp(18)=2200.d0
	pa_low(19)=1500.d0 ! Limits N(XXXX) G17
	pa_upp(19)=2300.d0
	pa_low(20)=1400.d0 ! Limits D(1950) F37
	pa_upp(20)=2300.d0
	pa_low(21)=1700.d0 ! Limits D(XXXX) G37
	pa_upp(21)=2500.d0
	pa_low(22)=1500.d0 ! Limits N(XXXX) G19
	pa_upp(22)=2400.d0
	pa_low(23)=1700.d0 ! Limits N(XXXX) H19
	pa_upp(23)=2400.d0
	pa_low(24)=1600.d0 ! Limits D(XXXX) G39
	pa_upp(24)=2400.d0
	return
	end subroutine pa_limits	

!=======================================================================================
	
      subroutine csearch(gwa,ka,kb,ierr)	!label (polepath)
!                Local search for complex  zero of a complex function. 
!                input: ka = starting value !will be overwritten!!!
!                       kb = value in the vicinity of ka !will be overwritten
!                output: ka= zero of the function shoot
!                        gwa=value of shoot at ka !(to control performance)
      implicit none
      complex(kind(0.d0)) 	:: gwa,ka,gwb,kb,gwc,kc,gw0,k0
      real(kind(0.d0)) 		:: gwamag,gwbmag,gw0mag,e_tol
      integer    		:: j,ierr

!         e_tol = 0.05d0	! Use with determinant search
	 e_tol = 500.d0		! Use with 1/T search
         call shoot(gwa,ka)
         gwamag=gwa*CONJG(gwa)
         gwamag=dsqrt(gwamag)
!
         call shoot(gwb,kb)
         gwbmag=gwb*CONJG(gwb)
         gwbmag=dsqrt(gwbmag)
		 j=-1
!  
! question:  gwa > gwb  ? 
         if ( gwamag.lt.gwbmag )then
             gwc=gwa
             kc =ka
             gwa=gwb
             ka =kb
             gwb=gwc
             kb =kc
         endif
!  gwa > gwb  : regula falsi 
! 
       do j=1,35  ! the local search starts.  
            ierr=j
            if(gwbmag.le.e_tol)then
                 ka=kb
                 gwa=gwb
                 exit
            end if
!           question: bad convergence??? ( if too slow, stop! )
            gw0=gwa-gwb
            gw0mag=gw0*CONJG(gw0)
            gw0mag=sqrt(gw0mag)
            if(gw0mag.le.e_tol)then
                 ka=kb
                 gwa=gwb
                 write(3,'(e12.4,a20)')gw0mag,'=gwb-gwa, STOP'
                 exit
            end if
!              convergence is fast enough, we proceed with a linear approximation
!              to the complex function and guess the next argument.
         k0=kb-gwb*(kb-ka)/(gwb-gwa)
         call shoot(gw0,k0)
         gw0mag=gw0*CONJG(gw0)
         gw0mag=dsqrt(gw0mag)
         write(1124,'(a14,i3,3f10.0)')'Iteration No. ',j,k0,gw0mag 
! the search continues. Reorder arguments.
         if(gw0mag.lt.gwbmag)then
           gwamag=gwbmag
           gwa=gwb
           ka=kb
           gwbmag=gw0mag
           gwb=gw0
           kb=k0
         else
           gwamag=gw0mag
           gwa=gw0
           ka=k0
         end if
 		 write(6,'(i3,5e16.8)') j,kb,gwb,gwbmag
       end do ! j
!
      return
      end
      
!======================================================================================
	
      subroutine shoot(fz,z) 		!label (polepath)
	  use input4
      	  use tmat4
      implicit none
      complex(kind(0.d0)) 	:: ez,fz,z
      integer 			:: iv,k_j,e_m
      iv=iv_memo
      k_j=k_j_memo
      ez=z
      zrem=ez
      call mesh_q
      ! The next few lines are redundant, but we insert them anyways to be sure
      j1=2*k_j - 1              	! j1=2*j_physical  ! declared in input
      call rot_mat(j1)          	! input xg,wg,djw  ! util.f90/rot_mat  ! dmat3.f90
      call set_coeff_jls(j1)
      call tmat(ez,1,4,k_j)
!      do e_m=1,2
        call g_res_calc(ez,iv,k_j)    
!      end do                               
      call t_fs_calc(ez,iv,k_j)    ! dressed vertices, self energy
      call tmat_s_calc(ez,iv,k_j)  ! t_pole    
      call t_add(iv,k_j)	   ! t=t_np+t_p
      if (iv==3.and.k_j==3) then	! D35 in piN-->piN is very weak, search better in rhoN(2)-->rhoN(2)
      fz=1.d0/tmatrix(n+1,n+1,iv,3,3)
      else if (iv==4.and.k_j==1) then	! P31 in piN-->piN is very weak, search better in piD(1)-->piD(1)
      fz=1.d0/tmatrix(n+1,n+1,iv,6,6)     
      else if (iv==3.and.k_j==2) then	! P33 
      fz=1.d0/tmatrix(n+1,n+1,iv,13,13)     
      else if (iv==2.and.k_j==1.and.dble(z).gt.1610.d0) then	! KL-->KL for P11(1710)
      fz=1.d0/tmatrix(n+1,n+1,iv,12,12)     
      else if (iv==2.and.k_j==1.and.dble(z).gt.1600.d0) then	! piD -->piD for P11(1750) dyn.
      fz=1.d0/tmatrix(n+1,n+1,iv,6,6)     
      else if (iv==3.and.k_j==5) then	
      fz=1.d0/tmatrix(n+1,n+1,iv,6,6)     
      else
      fz=1.d0/tmatrix(n+1,n+1,iv,1,1)
      end if
!	call  det_pole_calc(ez,iv,k_j)	! Old version from Siggi
!	fz=res_det
      end subroutine shoot
      
!====================================================================================
