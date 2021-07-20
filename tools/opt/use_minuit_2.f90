!==============================================================
!
!   This file contains some modules and subroutine for
!   the main fitting routine "test_minuit"
!
!==============================================================

	module input_minuit			! Defines variables that are needed in different subroutines and whose actual value
						! is made available through "include input_minuit".
	use input4
	implicit none
	integer :: countf			! Used to evaluate how many times the chi^2 routine has been called.
	integer,parameter  :: maxnparm = 225	! Maximum number of parameters
        integer,parameter  :: totparms = 429	! maxnparm+tuparms
	real(kind(0.d0))   :: maxcalls		! Maximum number of chi^2 calls per minuit run
	real(kind(0.d0))   :: tolerance		! tolerance (cf. p., "migrad" of manual)
	real(kind(0.d0))   :: strategy		! 0.,1., or 2.
	character(len=10)  :: fit_method	! 'migrad' or 'simplex'.
	real(kind(0.d0))   :: startfrac		! Initial step width as fraction of initial start value.
	character(len=9),dimension(4,max_j)  :: store_name	! where to read and store data and fitresults of the PW; is read in from "in_minuit_fixed.txt"
	integer,parameter  :: saidlength = 265	! Length of read in tables from SAID
	real(kind(0.d0)),dimension(4,max_j,saidlength,3) :: said 	! Array in which, for every iv and k_j and 265 energies, the SAID amplitude is stored (ecm, re tau, im tau)
	real(kind(0.d0)),dimension(4,max_j,2) :: fit_limits	! Lower and upper boud of fitted data for each PW (iv,k_j)
	integer, dimension(4,max_j) :: fit_pw	! Fit (=1) or not (=0) a given partial wave (iv,k_j).
	integer            :: jused		! Determines how many parameters are used in the fit.
!	integer, parameter :: k_e = 2		! For the moment, everything goes only up to k_j=2.
 	type parms				! Defines a data type that containes all information on a parameter.
	 character(len=10) :: p_name	     	! Name of parameter
	 real(kind(0.d0))  :: p_start	     	! Starting value of parameter
	 real(kind(0.d0))  :: p_step	     	! Starting step size (fraction of starting value)
	 real(kind(0.d0))  :: p_lo_lim	     	! lower limit within the the parameter can be varied
	 real(kind(0.d0))  :: p_up_lim	     	! upper limit within the the parameter can be varied
	 integer	   :: p_iv		! In which partial wave iv the parameter acts
	 integer           :: p_k_j		! In which partial wave k_j the parameter acts
	 integer           :: p_ires		! Resonance no in a given partial wave (so far, only S11 has two)
	 integer           :: p_ch		! channel no: 1-piN, 2-rhoN, 3-etaN, 4-piD, 5-sN, 6-omN, 7-KL, 9-KS
	 integer	   :: p_fix	     	! Whether(=1) or not (=0) the parameter is fixed
	 real(kind(0.d0))  :: p_error	     	! Parameter error as returned by mnpout
	end type parms
	type(parms) parm(totparms)		! "parm" is the array where all information on parameters is stored.

 	real(kind(0.d0)),dimension(63,3) :: etaN_data	! Experimental data pi^ --> eta ?
	real(kind(0.d0)),dimension(66,3) :: kzl_data	! Experimental data pi^-p --> K^0 Lambda
	real(kind(0.d0)),dimension(19,3) :: kzsz_data	! Experimental data pi^-p --> K^0 Sigma^0
	real(kind(0.d0)),dimension(29,3) :: kpsm_data	! Experimental data pi^-p --> K^+ Sigma^-
	real(kind(0.d0)),dimension(36,3) :: kpsp_data	! Experimental data pi^+p --> K^+ Sigma^+
	real(kind(0.d0)),dimension(10,3) :: kzl_dif_1661,kzl_dif_1683,kzl_dif_1694,kzl_dif_1724, &
							      & kzl_dif_1758,kzl_dif_1792,kzl_dif_1825,kzl_dif_1847, kzl_pol_1661,kzl_pol_1683,kzl_pol_1694
	real(kind(0.d0)),dimension(10,4) :: kzsz_dif_1724,kzsz_dif_1758, kzsz_dif_1792,kzsz_dif_1825
	real(kind(0.d0)),dimension(20,3) :: kzl_dif_1879,kzl_dif_1909,kzl_dif_1938,kzl_dif_1966, kzl_pol_1879,kzl_pol_1909,kzl_pol_1938,kzl_pol_1966
	real(kind(0.d0)),dimension(20,4) :: kzsz_dif_1879,kzsz_dif_1909, kzsz_dif_1938, kzsz_dif_1966
	real(kind(0.d0)),dimension(36,4) :: kpsp_dif_1822
	real(kind(0.d0)),dimension(37,4) :: kpsp_dif_1845,kpsp_dif_1870, kpsp_dif_1891
	real(kind(0.d0)),dimension(38,4) :: kpsp_dif_1926,kpsp_dif_1939,kpsp_dif_1970,kpsp_dif_1985,kpsp_dif_2019,kpsp_dif_2031
	real(kind(0.d0)),dimension(39,4) :: kpsp_dif_2059,kpsp_dif_2074,kpsp_dif_2106,kpsp_dif_2118,kpsp_dif_2147,kpsp_dif_2158,&
							      & kpsp_dif_2188,kpsp_dif_2202,kpsp_dif_2224,kpsp_dif_2243,kpsp_dif_2261,kpsp_dif_2282, &
							      & kpsp_dif_2304,kpsp_dif_2318,kpsp_dif_2341,kpsp_dif_2355, kzsz_pol_2208, kzsz_pol_2259, kzsz_pol_2316
	real(kind(0.d0)),dimension(14,4) :: kzl_pol_1724,kzl_pol_1758,kzl_pol_1792,kzl_pol_1825,kzl_pol_1847
	real(kind(0.d0)),dimension(5,3)  :: kzsz_pol_1725, kzsz_pol_1758,kzsz_pol_1793,kzsz_pol_1826,kzsz_pol_1879, &
							     & kzsz_pol_1909,kzsz_pol_1938, kzsz_pol_1966, kzsz_pol_2027, kzsz_pol_2059, kzsz_pol_2104, kzsz_pol_2183,&
							     & kzsz_pol_1724, kzsz_pol_1758b, kzsz_pol_1792, kzsz_pol_1825, kzsz_pol_1847, kzl_Pxdsdo_1626, kzl_Pxdsdo_1648, kzl_Pxdsdo_1672

	real(kind(0.d0)),dimension(5,4)  :: kpsp_pol_1764, kpsp_pol_1783, kpsp_pol_1789, kpsp_pol_1790, kpsp_pol_1813
	real(kind(0.d0)),dimension(17,4) :: kpsp_dif_1729
	real(kind(0.d0)),dimension(10,4) :: kpsp_dif_1700, kpsp_dif_1783, kpsp_dif_1790,kpsp_dif_1813
	real(kind(0.d0)),dimension(9,4)  :: kpsp_dif_1732
	real(kind(0.d0)),dimension(19,4) :: kpsp_dif_1757
	real(kind(0.d0)),dimension(20,4) :: kpsp_dif_1764,kpsp_dif_1789
	real(kind(0.d0)),dimension(4,4)  :: kpsp_pol_1729, kpsp_pol_1757
	real(kind(0.d0)),dimension(11,4) :: kpsp_pol_1732
	real(kind(0.d0)),dimension(18,4) :: kpsp_pol_1822,kpsp_pol_1845
	real(kind(0.d0)),dimension(19,4) :: kpsp_pol_1870,kpsp_pol_1891,kpsp_pol_1926,kpsp_pol_1939,kpsp_pol_1970,kpsp_pol_1985, &
							      & kpsp_pol_2019,kpsp_pol_2031,kpsp_pol_2106
	real(kind(0.d0)),dimension(20,4) :: kpsp_pol_2059,kpsp_pol_2074,kpsp_pol_2118,kpsp_pol_2147,kpsp_pol_2158,kpsp_pol_2188, &
							      & kpsp_pol_2202,kpsp_pol_2224,kpsp_pol_2243,kpsp_pol_2261,kpsp_pol_2282,kpsp_pol_2304, &
							      & kpsp_pol_2318,kpsp_pol_2341,kpsp_pol_2355
	real(kind(0.d0)),dimension(19,3) :: etaN_dif_1542,etaN_dif_1571,etaN_dif_1657,etaN_dif_1670,etaN_dif_1818, etaN_dif_1499K, etaN_dif_2272
	real(kind(0.d0)),dimension(18,3) :: etaN_dif_1714,etaN_dif_1764, kzl_pol_2059
	real(kind(0.d0)),dimension(20,3) :: etaN_dif_1859,etaN_dif_1900,etaN_dif_1932,etaN_dif_1509, etaN_dif_1527,  etaN_dif_1978, etaN_dif_1686, &
							      &	etaN_dif_2019, etaN_dif_2102, etaN_dif_2148,  etaN_dif_2055, etaN_dif_2183, etaN_dif_2453,  &
							      & kzsz_dif_1999, kzsz_dif_2027, kzsz_dif_2059, kzsz_dif_2104, kzsz_dif_2159, kzsz_dif_2208,&
							      & kzsz_dif_2259, kzsz_dif_2316, kzl_dif_2027, kzl_dif_2059, kzl_dif_2104, kzl_dif_2159,&
							      & kzl_dif_2183, kzl_dif_2316, kzl_dif_1632, kzl_dif_1661k, kzl_dif_1670, kzl_dif_1676, &
							      & kzl_dif_1678, kzl_dif_1681, kzl_dif_1684, kzl_dif_1686, kzl_dif_1687, kzl_dif_1689,&
							      & kzl_dif_1693, kzl_dif_1698, kzl_dif_1701, kzl_dif_1707, kzl_dif_1743, kzl_pol_2104,&
							      & kzl_pol_2159, kzl_pol_1999
	real(kind(0.d0)),dimension(17,3) :: kzl_pol_2183
	real(kind(0.d0)),dimension(8,3)  :: kzsz_dif_1934, etaN_dif_1525M
	real(kind(0.d0)),dimension(12,3) :: etaN_dif_1489,etaN_dif_1492, etaN_dif_1499,etaN_dif_1508,etaN_dif_1516,etaN_dif_1525,kzl_beta_2107, &
								& etaN_dif_1513, kzsz_dif_1930, kzsz_dif_1978, kzsz_dif_2025,  kzsz_dif_2097, kzsz_dif_2137,&
								& kzsz_dif_2180, kzsz_dif_2244, kzsz_dif_2305, kzsz_dif_2405, kzl_pol_1851, kzl_pol_1940,&
								&kzl_pol_2061, kzl_pol_2106, kzl_pol_2159b
	real(kind(0.d0)),dimension(13,3) :: etaN_dif_16kzl_pol_163309, kzl_dif_1930, kzl_dif_1978, kzl_dif_2025, kzl_dif_2097,&
								&kzl_dif_2137, kzl_dif_2180, kzl_dif_2244, kzl_dif_2305, kzl_dif_2405, etaN_dif_1609
	real(kind(0.d0)),dimension(15,3) :: etaN_dif_1507,  etaN_dif_1522, etaN_dif_1585, kzl_dif_1934
	real(kind(0.d0)),dimension(16,3) :: etaN_dif_1534, etaN_dif_1530
	real(kind(0.d0)),dimension(5,3)  :: etaN_pol_1765, etaN_pol_1819, kzl_dif_1633, kzsz_dif_1694, kzsz_pol_1694,&
								&kzsz_pol_1848, kzsz_pol_1999, kzl_pol_1633, kzl_pol_1844, kzsz_pol_2159, etaN_Pxdsdo_1739, etaN_Pxdsdo_1792
	real(kind(0.d0)),dimension(7,3)  :: etaN_pol_1860, etaN_pol_1933, etaN_pol_1979, etaN_Pxdsdo_1832, etaN_Pxdsdo_1904, etaN_Pxdsdo_1948, etaN_Pxdsdo_1988, etaN_Pxdsdo_2024, etaN_Pxdsdo_2070, etaN_Pxdsdo_2114, etaN_Pxdsdo_2148, etaN_Pxdsdo_2235, kpsm_pol_1743
	real(kind(0.d0)),dimension(6,3)  :: etaN_pol_1901, etaN_Pxdsdo_1872

	real(kind(0.d0)),dimension(8,3)	 :: kpsp_pw_S31,kpsp_pw_P31,kpsp_pw_P33,kpsp_pw_D33,kpsp_pw_D35,kpsp_pw_F35,kpsp_pw_F37
						! These variables store the pi+p --> K+S+ partial waves extracted from Candlin 1984
	real(kind(0.d0)),dimension(4,3)	 :: kpsp_beta_2021
	real(kind(0.d0)),dimension(3,3)	 :: kpsp_beta_2107, etaN_dif_1499D, etaN_dif_1507D, etaN_dif_1512D, etaN_dif_1577, etaN_dif_1516D, etaN_dif_1525D, &
					    & etaN_dif_1534D, etaN_dif_1544, etaN_dif_1587, etaN_dif_1607
	real(kind(0.d0)),dimension(11,3) :: kzl_beta_1852, kzl_beta_2062, kzl_pol_2029

	real(kind(0.d0)),dimension(10,3) :: kzl_beta_2030, kzl_beta_2160, etaN_dif_1545, etaN_dif_1575, etaN_dif_1507R, etaN_dif_1674,  etaN_dif_1729, etaN_dif_1805, etaN_dif_1897, kzsz_dif_1847, etaN_dif_1496, etaN_dif_1502,etaN_dif_1509B,  kzsz_dif_1741,&
								& kzsz_dif_1797, kzsz_dif_1819, kzsz_dif_1844, kzl_dif_1741,&
								&kzl_dif_1797, kzl_dif_1819, kzl_dif_1844, kzl_dif_1626, kzl_dif_1648,&
								&kzl_dif_1672, kzl_pol_2260, kzl_pol_1741,kzl_pol_1797, kzl_pol_1819, kzl_dif_1973, kpsm_dif_1973

	real(kind(0.d0)),dimension(10,3) :: kpsm_dif_1739, kpsm_dif_1763, kpsm_dif_1792, kpsm_dif_1818, kpsm_dif_1843, kpsm_dif_1930, etaN_dif_1512, kpsm_dif_1978,&
								& kpsm_dif_2025, kpsm_dif_2097, kpsm_dif_2137, kpsm_dif_2180, kpsm_dif_2244, kpsm_dif_2305,kpsm_dif_2405
	real(kind(0.d0)),dimension(19,3) ::  kzsz_dif_2183, kzl_dif_1999, kzl_dif_2208, kzl_dif_2259,&
								& kzl_pol_2027, kzl_pol_2208, kzl_pol_2259, kzl_pol_2316
	real(kind(0.d0)),dimension(9,3) :: kzl_beta_1941, kzl_beta_2261, etaN_dif_1498

						! Note: "parm" is a list of length "totparms" of entries of the type "parms"
	end module input_minuit

!===================================================================================================================

	subroutine read_input_minuit		! Reads in values from "in_minuit_fixed.txt"
	use input_minuit
	use input4
	implicit none
	character(len=99)::text_temp
	integer, dimension(5) :: itmp
	integer :: iv,k_j,i,j,pano
	real(kind(0.d0)),dimension(6) ::dummy
						! Initialize I/O units
	open (20,file='plot_minuit/fit_result.dat')		! For graphical output fo rthe fitresult.
	open (21,file='Info_Minuit_fit.txt')			! All the information about this simulation is written to this file
	open (22,file='Info_Minuit_stepwise.txt',status='replace') ! Will be open and closed many times in "chi_sq". Here, cut away
	write(22,'(a48)')'Function call |   chi^2      |  Parameter values'
	write(22,'(a48)')'================================================'
	close(22)						! previous versions of the file.
	open (23,file='plot_minuit/chisquare_development.dat')		! For graphical output of chi^2 as a function of chi^2 calls

	said=0.							! Initializes the storage for the SAID PW results.
	open (24,file='in_minuit_fixed.txt')
	read(24,'(a40)')text_temp;			write(21,'(a40)')text_temp
	read(24,'(a40)')text_temp;			write(21,'(a40)')text_temp
	read(24,'(a40)')text_temp;			write(21,'(a40)')text_temp
	read(24,'(a30,f10.0)')text_temp,maxcalls;	write(21,'(a30,f10.0)')text_temp,maxcalls
	read(24,'(a30,f10.0)')text_temp,tolerance;	write(21,'(a30,f10.0)')text_temp,tolerance
	read(24,'(a30,f10.0)')text_temp,strategy;	write(21,'(a30,f10.0)')text_temp,strategy
	read(24,'(a30,a7)')text_temp,fit_method;	write(21,'(a30,a7)')text_temp,fit_method
	read(24,'(a30,f10.2)')text_temp,startfrac;	write(21,'(a30,f10.0)')text_temp,startfrac
	read(24,'(a99)')text_temp;		      	write(21,'(a99)')text_temp
	!---------------------------------------------------------------------------------
	! Read the range of fitted energies.
	fit_pw=0
	read(24,'(a99)')text_temp;		      	write(21,'(a99)')text_temp
	read(24,'(a99)')text_temp;		      	write(21,'(a99)')text_temp
	do k_j=1,5
	do iv=1,4
	  read (24,'(a6,2f7.0,i3)')text_temp,fit_limits(iv,k_j,1),fit_limits(iv,k_j,2),fit_pw(iv,k_j)
	  write(21,'(a6,2f7.0,i3)')text_temp,fit_limits(iv,k_j,1),fit_limits(iv,k_j,2),fit_pw(iv,k_j)
	end do
	end do
	read(24,'(a99)')text_temp;		      	write(21,'(a99)')text_temp

	!---------------------------------------------------------------------------------
	! Read parameters
	read(24,'(a99)')text_temp;		      	!write(21,'(a99)')text_temp
	read(24,'(a99)')text_temp;		      	!write(21,'(a99)')text_temp
	read(24,'(a99)')text_temp;		      	!write(21,'(a99)')text_temp
	read(24,'(a99)')text_temp;		      	!write(21,'(a99)')text_temp
	read(24,'(a99)')text_temp;		      	!write(21,'(a99)')text_temp
	do i=1,maxnparm
	  parm(i)%p_error=0.d0
	  read (24,'(a10,4f10.3,5i4)')parm(i)%p_name,parm(i)%p_start,parm(i)%p_step,parm(i)%p_lo_lim,parm(i)%p_up_lim,parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,parm(i)%p_ch,parm(i)%p_fix
	end do


	!---------------------------------------------------------------------------------
	! Pass start values from JM :
	! Overwrite start value with JM solution, in case "parm(i)%p_start" is 1.d0:
	! Note
	! Do not add in in_minuit_fixed.txt, parm(i)%p_ch can only be 0-8, no 9-12 case
	do i=1,maxnparm
	if (parm(i)%p_start==1.d0)  then
	    if      (parm(i)%p_ch==0)then						! bare mass
	    parm(i)%p_start = m_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires)
	    else if (parm(i)%p_ch==1)then						! g_bare
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,1)
	    else if (parm(i)%p_ch==2)then
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,2)
	    else if (parm(i)%p_ch==3)then
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,5)
	    else if (parm(i)%p_ch==4)then
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,6)
	    else if (parm(i)%p_ch==5)then
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,8)
	    else if (parm(i)%p_ch==6)then
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,9)
	    else if (parm(i)%p_ch==7)then
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,12)
		  else if (parm(i)%p_ch==8)then
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,13)
	  	else if (parm(i)%p_ch==9)then
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,14)
		  else if (parm(i)%p_ch==10)then
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,15)
		  else if (parm(i)%p_ch==11)then
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,16)
			else
	    parm(i)%p_start = g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,19)
	    end if									! end channel test 0 to 8
	end if										! if start parameter overwritten (if = 1.)
	end do										! loop over i
	read(24,'(a99)')text_temp;		      	write(21,'(a99)')text_temp
	!---------------------------------------------------------------------------------

	if (1==1) then
	open(99,file='save_parms.txt')
        do i=1,totparms
	  read(99,'(a10,3e15.6)')parm(i)%p_name,parm(i)%p_start,parm(i)%p_lo_lim,parm(i)%p_up_lim
        end do
	close(99)
	end if

	!---------------------------------------------------------------------------------
	! Read name of storage units for PW (rest is thrown away).
	read(24,'(a99)')text_temp;		      	write(21,'(a99)')text_temp
	read(24,'(a99)')text_temp;		      	write(21,'(a99)')text_temp
	read(24,'(a99)')text_temp;		      	write(21,'(a99)')text_temp
        i=25
	do k_j=1,5  ! k_j=(2j+1)/2
	do iv=1,4
	  read (24,'(a9,i2,4i5)')store_name(iv,k_j),itmp(1),itmp(2),itmp(3),itmp(4),itmp(5)
	  write(21,'(a9,i2,4i5)')store_name(iv,k_j),itmp(1),itmp(2),itmp(3),itmp(4),itmp(5)
	  open(i,file='data/SAID_03_2010/'//store_name(iv,k_j))
	  open(i+21,file='plot_minuit/restore_SAID/'//store_name(iv,k_j))
	  do j=1,saidlength
	    read(i,*)said(iv,k_j,j,1),dummy(1),dummy(2),dummy(3),dummy(4),said(iv,k_j,j,2),said(iv,k_j,j,3),dummy(5),dummy(6)
	    write(i+21,*)said(iv,k_j,j,1),said(iv,k_j,j,2),said(iv,k_j,j,3)
	  end do
	  close(i)
	  close(i+21)
	  i=i+1
	end do
	end do

	end subroutine read_input_minuit



!===================================================================================================================
!
       subroutine write_parm		       ! Write the array "parm" to unit 21 for checking.
       use input_minuit
       implicit none
       integer :: i
       write(21,'(a90)')'========================================================================================='
       write(21,'(a90)')'                               Array parm is:                                            '
       write(21,'(a90)')'Name        Value       Step size   lower_limit upper_limit iv  k_j res ch  fix error    '
       write(21,'(a90)')'========================================================================================='
       do i=1,totparms
	  write(21,'(a10,4e12.3,5i4,e12.3)')parm(i)%p_name,parm(i)%p_start,parm(i)%p_step,parm(i)%p_lo_lim,parm(i)%p_up_lim, &
                                            & parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,parm(i)%p_ch,parm(i)%p_fix,parm(i)%p_error
       end do
       write(21,'(a82)')'========================================================================================='

	!---------------------------------------------------------------------------------

	if (1==1) then
	open(99,file='save_parms.txt')
       do i=1,totparms
	  write(99,'(a10,3e15.6)')parm(i)%p_name,parm(i)%p_start,parm(i)%p_lo_lim,parm(i)%p_up_lim
       end do
	close(99)
	end if
	!---------------------------------------------------------------------------------


       return
       end subroutine write_parm

!===================================================================================================================
!
	! More flexible routine to go from T to tau than programmed in output4.f90
	! Phase space factor (very simple for the effective pipiN channels)
	! Note: make shure this is in agreement with the definitions in subroutine "tau_print" (out_tfs.f90)
	function phasfac(z,i)
	 use input4
	implicit	none
	complex(kind(0.d0)) :: z,phasfac,Eon_1,Eon_2,Eon_in_1,Eon_in_2,rho_1,rho_2
	integer :: i
	real(kind(0.0d0)) :: m_1,m_2
	real(kind(0.0d0)),dimension(n_c) :: m_mes,m_bar
        complex(kind(0.d0)),dimension(n_c) :: cono
	phasfac=dcmplx(0.d0,0.d0)
	m_mes=0.
	m_bar=0.
	m_mes(1) =masses(1,2)		! Refill meson masses according to our simplified phase space factor piN
	m_mes(2) =2.d0*masses(1,2)	! rhoN, rho with mass 2mpi
	m_mes(3) =2.d0*masses(1,2)	! rhoN, rho with mass 2mpi
	m_mes(4) =2.d0*masses(1,2)	! rhoN, rho with mass 2mpi
	m_mes(5) =masses(5,2)		! eta N
	m_mes(6) =masses(1,2)		! piDelta
	m_mes(7) =masses(1,2)		! piDelta
	m_mes(8) =2.d0*masses(1,2)	! sigma N
	m_mes(12)=masses(6,2)		! Kaon mass in KLambda
	m_mes(13)=masses(6,2)		! Kaon mass in KSigma
	m_mes(14)=masses(9,2)		! D mass in DLambdac
	m_mes(15)=masses(9,2)		! D mass in DSigmac
	m_mes(16)=masses(10,2)		! Ds mass in DsLambdac
	m_mes(17)=masses(10,2)		! Ds mass in DsLambdac
	m_mes(18)=masses(10,2)		! Ds mass in DsLambdac
	m_mes(19)=masses(10,2)		! Ds mass in DsSigmac
	m_mes(20)=masses(10,2)		! Ds mass in DsSigmac
	m_mes(21)=masses(10,2)		! Ds mass in DsSigmac
	m_bar(1)=masses(1,1)		! Refill baryon masses according to our simplified phase space factor
	m_bar(2)=masses(1,1)
	m_bar(3)=masses(1,1)
	m_bar(4)=masses(1,1)
	m_bar(5)=masses(1,1)
	m_bar(6)=masses(2,1)
	m_bar(7)=masses(2,1)
	m_bar(8)=masses(1,1)
	m_bar(12)=masses(3,1)		! Lambda mass
	m_bar(13)=masses(4,1)		! Sigma mass
	m_bar(14)=masses(5,1)		! Lambdac mass
	m_bar(15)=masses(6,1)		! Sigmac mass
	m_bar(16)=masses(5,1)		! Lambdac mass
	m_bar(17)=masses(5,1)		! Lambdac mass
	m_bar(18)=masses(5,1)		! Lambdac mass
	m_bar(19)=masses(6,1)		! Sigmac mass
	m_bar(20)=masses(6,1)		! Sigmac mass
	m_bar(21)=masses(6,1)		! Sigmac mass

	qon_f_pa=0.
	call qon_c(z,qon_f_pa)
	cono=dcmplx(0.d0,0.d0)
	cono(1)=qon_f_pa(1)
	cono(2)=qon_f_pa(2)
	cono(3)=qon_f_pa(3)
	cono(4)=qon_f_pa(4)
	cono(5)=qon_f_pa(5)
	cono(6)=qon_f_pa(6)
	cono(7)=qon_f_pa(7)
	cono(8)=qon_f_pa(8)
	cono(12)=qon_f_pa(12)
	cono(13)=qon_f_pa(13)
	cono(14)=qon_f_pa(14)
	cono(15)=qon_f_pa(15)
	cono(16)=qon_f_pa(16)
	cono(17)=qon_f_pa(17)
	cono(18)=qon_f_pa(18)
	cono(19)=qon_f_pa(19)
	cono(20)=qon_f_pa(20)
	cono(21)=qon_f_pa(21)
	! Incoming: always piN:
        Eon_in_1=sqrt(cono(1)**2+m_mes(1)**2)
        Eon_in_2=sqrt(cono(1)**2+m_bar(1)**2)
	m_1=m_mes(i) 	      ! Start calculating phase space factor for channel [i]
	m_2=m_bar(i) 	      ! according to Eq. (E1) of Ashot's thesis.
        Eon_1=sqrt(cono(i)**2+m_1**2)
        Eon_2=sqrt(cono(i)**2+m_2**2)
        rho_1=Eon_in_1*Eon_in_2*cono(1)/z !Initial state is piN
        rho_2=Eon_1*Eon_2*cono(i)/z
	 if (dble(z).gt.m_1+m_2) then
	 phasfac=-pi*sqrt(rho_1*rho_2) ! PHASE SPACE FACTOR, to be improved for effective pipiN channels. ONLY IF ABOVE THRESHOLD
	 end if
	return
	end function phasfac

!=============================================================================================================================
!========NEW===ADD=====================================================================================================
!
	function phasfacnew(z,i,j)
	 use input4
	implicit	none
	complex(kind(0.d0)) :: z,phasfacnew,Eon_3,Eon_4,Eon_in_3,Eon_in_4,rho_3,rho_4
	integer :: i,j
	real(kind(0.0d0)) :: m_3,m_4
	real(kind(0.0d0)),dimension(n_c) :: m_mes,m_bar
        complex(kind(0.d0)),dimension(n_c) :: cono
	phasfacnew=dcmplx(0.d0,0.d0)
	m_mes=0.
	m_bar=0.
	m_mes(1) =masses(1,2)		! Refill meson masses according to our simplified phase space factor piN
	m_mes(2) =2.d0*masses(1,2)	! rhoN, rho with mass 2mpi
	m_mes(3) =2.d0*masses(1,2)	! rhoN, rho with mass 2mpi
	m_mes(4) =2.d0*masses(1,2)	! rhoN, rho with mass 2mpi
	m_mes(5) =masses(5,2)		! eta N
	m_mes(6) =masses(1,2)		! piDelta
	m_mes(7) =masses(1,2)		! piDelta
	m_mes(8) =2.d0*masses(1,2)	! sigma N
	m_mes(12)=masses(6,2)		! Kaon mass in KLambda
	m_mes(13)=masses(6,2)		! Kaon mass in KSigma
	m_mes(14)=masses(9,2)		! D mass in DLambdac
	m_mes(15)=masses(9,2)		! D mass in DSigmac
	m_mes(16)=masses(10,2)		! Ds mass in DsLambdac
	m_mes(17)=masses(10,2)		! Ds mass in DsLambdac
	m_mes(18)=masses(10,2)		! Ds mass in DsLambdac
	m_mes(19)=masses(10,2)		! Ds mass in DsSigmac
	m_mes(20)=masses(10,2)		! Ds mass in DsSigmac
	m_mes(21)=masses(10,2)		! Ds mass in DsSigmac
	m_bar(1)=masses(1,1)		! Refill baryon masses according to our simplified phase space factor
	m_bar(2)=masses(1,1)
	m_bar(3)=masses(1,1)
	m_bar(4)=masses(1,1)
	m_bar(5)=masses(1,1)
	m_bar(6)=masses(2,1)
	m_bar(7)=masses(2,1)
	m_bar(8)=masses(1,1)
	m_bar(12)=masses(3,1)		! Lambda mass
	m_bar(13)=masses(4,1)		! Sigma mass
	m_bar(14)=masses(5,1)		! Lambdac mass
	m_bar(15)=masses(6,1)		! Sigmac mass
	m_bar(16)=masses(5,1)		! Lambdac mass
	m_bar(17)=masses(5,1)		! Lambdac mass
	m_bar(18)=masses(5,1)		! Lambdac mass
	m_bar(19)=masses(6,1)		! Sigmac mass
	m_bar(20)=masses(6,1)		! Sigmac mass
	m_bar(21)=masses(6,1)		! Sigmac mass

	qon_f_pa=0.
	call qon_c(z,qon_f_pa)
	cono=dcmplx(0.d0,0.d0)
	cono(1)=qon_f_pa(1)
	cono(2)=qon_f_pa(2)
	cono(3)=qon_f_pa(3)
	cono(4)=qon_f_pa(4)
	cono(5)=qon_f_pa(5)
	cono(6)=qon_f_pa(6)
	cono(7)=qon_f_pa(7)
	cono(8)=qon_f_pa(8)
	cono(12)=qon_f_pa(12)
	cono(13)=qon_f_pa(13)
	cono(14)=qon_f_pa(14)
	cono(15)=qon_f_pa(15)
	cono(16)=qon_f_pa(16)
	cono(17)=qon_f_pa(17)
	cono(18)=qon_f_pa(18)
	cono(19)=qon_f_pa(19)
	cono(20)=qon_f_pa(20)
	cono(21)=qon_f_pa(21)
	! Incoming: i:
        Eon_in_3=sqrt(cono(i)**2+m_mes(i)**2)
        Eon_in_4=sqrt(cono(i)**2+m_bar(i)**2)
	m_3=m_mes(j) 	      ! Start calculating phase space factor for channel [j]
	m_4=m_bar(j) 	      ! according to Eq. (E1) of Ashot's thesis.
        Eon_3=sqrt(cono(j)**2+m_3**2)
        Eon_4=sqrt(cono(j)**2+m_4**2)
        rho_3=Eon_in_3*Eon_in_4*cono(i)/z
        rho_4=Eon_3*Eon_4*cono(j)/z
	 if (dble(z).gt.m_3+m_4) then
	 phasfacnew=-pi*sqrt(rho_3*rho_4) ! PHASE SPACE FACTOR, to be improved for effective pipiN channels. ONLY IF ABOVE THRESHOLD
	 end if
	return
end function phasfacnew

!=============================================================================================================================


	subroutine transfer_parms(parm_minuit)			! This is used in "chi_sq". It writes the current (=during fit)
	use input4						! parameter set ("parm_minuit") to those defined in "input4", which
	use input_minuit
	implicit none						! are the parameters as used by the Juelich model ("p1" to "p4").
	real(kind(0.d0)),dimension(jused) :: parm_minuit	! This list may get long and has to be defined entry by entry.
	integer :: i,j
	j=1
	do i=1,maxnparm
	if (parm(i)%p_fix==0) then
	    if      (parm(i)%p_ch==0)then
	    m_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires)    = parm_minuit(j)
	    else if (parm(i)%p_ch==1)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,1)  = parm_minuit(j)
	    else if (parm(i)%p_ch==2)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,2)  = parm_minuit(j)
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,3)  = parm_minuit(j)
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,4)  = parm_minuit(j)
	    else if (parm(i)%p_ch==3)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,5)  = parm_minuit(j)
	    else if (parm(i)%p_ch==4)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,6)  = parm_minuit(j)
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,7)  = parm_minuit(j)
	    else if (parm(i)%p_ch==5)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,8)  = parm_minuit(j)
	    else if (parm(i)%p_ch==6)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,9)  = parm_minuit(j)
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,10) = parm_minuit(j)
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,11) = parm_minuit(j)
	    else if (parm(i)%p_ch==7)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,12) = parm_minuit(j)
		  else if (parm(i)%p_ch==8)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,13) = parm_minuit(j)
	  	else if (parm(i)%p_ch==9)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,14) = parm_minuit(j)
	  	else if (parm(i)%p_ch==10)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,15) = parm_minuit(j)
		  else if (parm(i)%p_ch==11)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,16) = parm_minuit(j)
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,17) = parm_minuit(j)
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,18) = parm_minuit(j)
	    else
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,19) = parm_minuit(j)
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,20) = parm_minuit(j)
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,21) = parm_minuit(j)
	    end if
	j=j+1
	end if ! parm(i)%p_fix==0
	end do ! i=1,maxnparm
	   if (j-1/=jused) then								! CHeck if counting is OK!
	   write(21,*)'jused = ',jused,' is not equal to j-1 = ',j-1
	   end if
	return
	end subroutine transfer_parms

!=============================================================================================================================

	subroutine transfer_all_parms			! This is used in "chi_sq". It writes the current (=during fit)
	use input4						! parameter set ("parm_minuit") to those defined in "input4", which
	use input_minuit
	implicit none						! are the parameters as used by the Juelich model ("p1" to "p4").
	integer :: i
        !real(kind(0.d0)), dimension(30) :: gcoup

	do i=1,maxnparm
	    if      (parm(i)%p_ch==0)then
	    m_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires)    = parm(i)%p_start
	    else if (parm(i)%p_ch==1)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,1)  = parm(i)%p_start
	    else if (parm(i)%p_ch==2)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,2)  = parm(i)%p_start
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,3)  = parm(i)%p_start
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,4)  = parm(i)%p_start
	    else if (parm(i)%p_ch==3)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,5)  = parm(i)%p_start
	    else if (parm(i)%p_ch==4)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,6)  = parm(i)%p_start
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,7)  = parm(i)%p_start
	    else if (parm(i)%p_ch==5)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,8)  = parm(i)%p_start
	    else if (parm(i)%p_ch==6)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,9)  = parm(i)%p_start
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,10) = parm(i)%p_start
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,11) = parm(i)%p_start
	    else if (parm(i)%p_ch==7)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,12) = parm(i)%p_start
		  else if (parm(i)%p_ch==8)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,13) = parm(i)%p_start
		  else if (parm(i)%p_ch==9)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,14) = parm(i)%p_start
		  else if (parm(i)%p_ch==10)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,15) = parm(i)%p_start
		  else if (parm(i)%p_ch==11)then
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,16)  = parm(i)%p_start
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,17) = parm(i)%p_start
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,18) = parm(i)%p_start
	    else
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,19)  = parm(i)%p_start
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,20) = parm(i)%p_start
	    g_bare(parm(i)%p_iv,parm(i)%p_k_j,parm(i)%p_ires,21) = parm(i)%p_start
	    end if
	end do ! i=1,maxnparm

        coupl(1,5,1)%al1=parm(maxnparm+1)%p_start      ! pNsNuN_pNN ff
        coupl(1,5,1)%al2=parm(maxnparm+2)%p_start      ! pNsNuN_sNN ff
        coupl(1,5,2)%al1=parm(maxnparm+3)%p_start      ! pNsNtp_pNN ff
        coupl(1,5,2)%al2=parm(maxnparm+4)%p_start      ! pNsNtp_spp ff
        coupl(1,4,1)%al1=parm(maxnparm+5)%p_start      ! pNpDuN_pNN ff		     ! NOT USED, fit parm 217 instead!
        coupl(1,4,1)%al2=parm(maxnparm+6)%p_start      ! pNpDuN_pND ff
        coupl(1,4,2)%al1=parm(maxnparm+7)%p_start      ! pNpDtr_rND ff
        coupl(1,4,2)%al2=parm(maxnparm+8)%p_start      ! pNpDtr_rpp ff
        coupl(1,4,3)%al1=parm(maxnparm+9)%p_start      ! pNpDuD_pND ff
        coupl(1,4,3)%al2=parm(maxnparm+10)%p_start      ! pNpDuD_pDD ff
        coupl(4,4,1)%al1=parm(maxnparm+11)%p_start      ! pDpDuN_pND ff (only one)   ! NOT USED, fit parm 222 instead!
        coupl(4,4,2)%al1=parm(maxnparm+12)%p_start      ! pDpDtr_rDD ff
        coupl(4,4,2)%al2=parm(maxnparm+13)%p_start      ! pDpDtr_rpp ff		     ! NOT USED, fit parm 224 instead!
        coupl(4,5,1)%al1=parm(maxnparm+14)%p_start      ! pdsNtp_pND ff
        coupl(4,5,1)%al2=parm(maxnparm+15)%p_start      ! pdsNtp_spp ff
        coupl(5,5,1)%al1=parm(maxnparm+16)%p_start      ! sNsNuN_sNN ff (only one)
!         coupl(5,5,1)%al2=parm(232)%p_start	! not a new fit parameter, not used in potential.
        coupl(5,5,2)%al1=parm(maxnparm+17)%p_start      ! sNsNts_sNN ff
        coupl(5,5,2)%al2=parm(maxnparm+18)%p_start      ! sNsNts_sss ff
	lam_ff(2,1,1,1) =parm(maxnparm+19)%p_start      ! nucleon P11 s-channel FF
        gcoup_global(14)=parm(maxnparm+20)%p_start      ! NN sigma coupling; requires to call coupl_prep(gcoup_global)
        gcoup_global(16)=parm(maxnparm+21)%p_start      ! three sigma coupling; requires to call coupl_prep(gcoup_global)
	coupl(1,8,1)%al1=parm(maxnparm+22)%p_start      ! pNKStK*_pKK* ff
	coupl(1,8,1)%al2=parm(maxnparm+23)%p_start      ! pNKStK*_NSK* ff
	coupl(1,8,2)%al1=parm(maxnparm+24)%p_start      ! pNKSuS_pSS ff
	coupl(1,8,2)%al2=parm(maxnparm+25)%p_start      ! pNKSuS_NKS ff
	coupl(1,8,3)%al1=parm(maxnparm+26)%p_start      ! pNKSuL_pSL ff
	coupl(1,8,3)%al2=parm(maxnparm+27)%p_start      ! pNKSuL_NKL ff
	coupl(1,8,5)%al1=parm(maxnparm+28)%p_start      ! pNKSuS*_pSS* ff
 	coupl(1,8,5)%al2=parm(maxnparm+29)%p_start      ! pNKSuS*_NKS* ff  	     ! NOW USED in potential3_mod.f90 !!!
	coupl(1,1,1)%al1=parm(maxnparm+30)%p_start      ! pNpNuN_pNN ff (only one)   ! NOT USED, fit parm 217 instead!
	coupl(1,1,2)%al1=parm(maxnparm+31)%p_start      ! pNpNts ff     (only one)
	coupl(1,1,3)%al1=parm(maxnparm+32)%p_start      ! pNpNtr_ppr ff
	coupl(1,1,3)%al2=parm(maxnparm+33)%p_start      ! pNpNtr_rNN ff
	coupl(1,1,4)%al1=parm(maxnparm+34)%p_start      ! pNpNuD_pND ff (only one)   ! NOT USED, fit parm 225 instead!
 	coupl(1,1,4)%al2=parm(maxnparm+34)%p_start	! not a new fit parameter, NOW used in potential.
	gcoup_global(20)=parm(maxnparm+35)%p_start      ! g_SigSigsig * g_KKsig /(4pi) ! Solu 15.5: 3.154000
	coupl(8,8,1)%al1=parm(maxnparm+36)%p_start      ! KSKSts ff
	coupl(8,8,1)%al2=parm(maxnparm+37)%p_start      ! KSKSts contact ff
	coupl(8,8,2)%al1=parm(maxnparm+38)%p_start      ! KSKStom_omKK ff
	coupl(8,8,2)%al2=parm(maxnparm+39)%p_start      ! KSKStom_omSS ff
	coupl(8,8,3)%al1=parm(maxnparm+40)%p_start      ! KSKStph_phKK ff
	coupl(8,8,3)%al2=parm(maxnparm+41)%p_start      ! KSKStph_phSS ff
	coupl(8,8,4)%al1=parm(maxnparm+42)%p_start      ! KSKStr_rKK ff
	coupl(8,8,4)%al2=parm(maxnparm+43)%p_start      ! KSKStr_rSS ff
	coupl(1,7,1)%al1=parm(maxnparm+44)%p_start      ! pNKLtK*_pKK* ff  	     ! NOT USED, fit parm 238 instead!
	coupl(1,7,1)%al2=parm(maxnparm+45)%p_start      ! pNKLtK*_NLK* ff
	coupl(1,7,2)%al1=parm(maxnparm+46)%p_start      ! pNKLuS_pLS ff
	coupl(1,7,2)%al2=parm(maxnparm+47)%p_start      ! pNKLuS_NKS ff		     ! NOT USED, fit parm 241 instead!
	coupl(1,7,4)%al1=parm(maxnparm+48)%p_start      ! pNKLuS*_pLS* ff
 	coupl(1,7,4)%al2=parm(maxnparm+49)%p_start      ! pNKLuS*_NKS* ff  	     ! NOW USED in potential3_mod.f90 !!!
	coupl(7,7,1)%al1=parm(maxnparm+50)%p_start      ! KLKL_KKs ff		     ! NOT USED, fit parm 252 instead!
	coupl(7,7,1)%al2=parm(maxnparm+51)%p_start      ! KLKL_LLs ff
	coupl(7,7,2)%al1=parm(maxnparm+52)%p_start      ! KLKL_KKom ff		     ! NOT USED, fit parm 254 instead!
	coupl(7,7,2)%al2=parm(maxnparm+53)%p_start      ! KLKL_LLom ff
	coupl(7,7,3)%al1=parm(maxnparm+54)%p_start      ! KLKL_KKphi ff		     ! NOT USED, fit parm 256 instead!
	coupl(7,7,3)%al2=parm(maxnparm+55)%p_start      ! KLKL_LLphi ff
	gcoup_global(19)=parm(maxnparm+56)%p_start      ! g_LamLamsig * g_KKsig /(4pi)
	coupl(7,8,1)%al1=parm(maxnparm+57)%p_start      ! LKSK_KKrho ff		     ! NOT USED, fit parm 258 instead!
	coupl(7,8,1)%al2=parm(maxnparm+58)%p_start      ! LKSK_LSrho ff
	coupl(1,3,1)%al1=parm(maxnparm+59)%p_start      ! pNetaNuN_ pNN_ff 	     ! NOT USED, fit parm 217 instead!
	coupl(1,3,1)%al2=parm(maxnparm+60)%p_start      ! pNetaNuN_ etaNN_ff
	coupl(1,3,2)%al1=parm(maxnparm+61)%p_start      ! pNetaNta0_ petaa0_ff
	coupl(1,3,2)%al2=parm(maxnparm+62)%p_start      ! pNetaNta0_ NNa0_ff
	coupl(3,3,1)%al1=parm(maxnparm+63)%p_start      ! etaNetaNuN_ etaNN_ff	     ! NOT USED, fit parm 276 instead!
	coupl(1,2,1)%al1=parm(maxnparm+64)%p_start      ! pNrhoNuN_ piNN_ff	     ! NOT USED, fit parm 217 instead!
	coupl(1,2,1)%al2=parm(maxnparm+65)%p_start      ! pNrhoNuN_ NNrho_ff
	coupl(1,2,2)%al1=parm(maxnparm+66)%p_start      ! pirhoNN contact term
	coupl(1,2,3)%al1=parm(maxnparm+67)%p_start      ! pNrhoNtpi_ pipirho_ff
	coupl(1,2,3)%al2=parm(maxnparm+68)%p_start      ! pNrhoNtpi_piNN_ff	     ! NOT USED, fit parm 219 instead!
	coupl(1,2,4)%al1=parm(maxnparm+69)%p_start      ! pNrhoNtomega_ omegapirho_ff
	coupl(1,2,4)%al2=parm(maxnparm+70)%p_start      ! pNrhoNtomega_omegaNN_ff
	coupl(1,2,5)%al1=parm(maxnparm+71)%p_start      ! pNrhoNta1_a1pirho_ff
	coupl(1,2,5)%al2=parm(maxnparm+72)%p_start      ! pNrhoNta1_a1NN_ff
	coupl(2,2,1)%al1=parm(maxnparm+73)%p_start      ! rhoNrhoNuN_rhoNN_ff(only one)! NOT USED, fit parm 281 instead!
	coupl(2,2,2)%al1=parm(maxnparm+74)%p_start      ! rhoNrhoN contact term
	coupl(2,2,3)%al1=parm(maxnparm+75)%p_start      ! rhoNrhoNtrho_rhorhorho_ff
	coupl(2,2,3)%al2=parm(maxnparm+76)%p_start      ! rhoNrhoNtrho_rhoNN_ff	     ! NOT USED, fit parm 249 instead!
	coupl(2,2,4)%al1=parm(maxnparm+77)%p_start      ! rhoNrhoNuDelta_DeltarhoN_ff
	coupl(2,4,1)%al1=parm(maxnparm+78)%p_start      ! rhoNpiDeltatpi_rhopipi_ff
	coupl(2,4,1)%al2=parm(maxnparm+79)%p_start      ! rhoNpiDeltatpi_DeltapiN_ff ! NOT USED, fit parm 230 instead!
	coupl(2,4,2)%al1=parm(maxnparm+80)%p_start      ! rhoNpiDeltauN_rhoNDelta_ff
	coupl(2,4,2)%al2=parm(maxnparm+81)%p_start      ! rhoNpiDeltauN_NNpi_ff	     ! NOT USED, fit parm 217 instead!
        coupl(1,7,3)%al1=parm(maxnparm+82)%p_start      ! piN --> KL, kappa t channel, ff normal diagram
        coupl(1,7,3)%al2=parm(maxnparm+83)%p_start      ! piN --> KL, kappa t channel, ff contact graph
        coupl(1,8,4)%al1=parm(maxnparm+84)%p_start      ! piN --> KS, kappa t channel, ff normal diagram
        coupl(1,8,4)%al2=parm(maxnparm+85)%p_start      ! piN --> KS, kappa t channel, ff contact graph
	coupl(3,3,2)%al1=parm(maxnparm+86)%p_start      ! eta N -> etaN, f_0 exchange; ONLY one FF; %al2 never used.
	coupl(3,8,1)%al1=parm(maxnparm+87)%p_start	  ! etaNKSigKstt_etaKKSt_ff  (eta N -> KSig, K* in t)
	coupl(7,7,4)%al1=parm(maxnparm+88)%p_start	  ! KLamKLamuXi_LamXiK_ff
	coupl(7,7,5)%al1=parm(maxnparm+89)%p_start	  ! KLamKLamuXi*_LamXi*K_ff
	coupl(8,8,5)%al1=parm(maxnparm+90)%p_start	  ! KSigKSiguXi_KXiSig_ff
	coupl(8,8,6)%al1=parm(maxnparm+91)%p_start	  ! KSigKSiguXi*_KXi*Sig_ff
	coupl(3,7,2)%al1=parm(maxnparm+92)%p_start	  ! etaNKLamuLam_etaLamLam_ff
	coupl(3,8,2)%al1=parm(maxnparm+93)%p_start	  ! etaNKSiguSig_etaSigSig_ff
	coupl(3,8,3)%al1=parm(maxnparm+94)%p_start	  ! etaNKSiguSig*_etaSigSig*_ff  only on FF, %al2 never used
	g_bare(1,1,3,1) =parm(maxnparm+95)%p_start	  ! S11 piN ct. term (iv,k_j,i_rs,i_c12)
	g_bare(1,1,3,5) =parm(maxnparm+96)%p_start	  ! S11 etN ct. term (iv,k_j,i_rs,i_c12)
	g_bare(1,1,3,6) =parm(maxnparm+97)%p_start	  ! S11 piD ct. term (iv,k_j,i_rs,i_c12)
	g_bare(1,1,3,7) =parm(maxnparm+97)%p_start	  ! S11 piD ct. term (iv,k_j,i_rs,i_c12)
	g_bare(1,1,3,12)=parm(maxnparm+98)%p_start	  ! S11 Kla ct. term (iv,k_j,i_rs,i_c12)
	g_bare(1,1,3,13)=parm(maxnparm+99)%p_start	  ! S11 KSi ct. term (iv,k_j,i_rs,i_c12)
	g_bare(3,1,2,1) =parm(maxnparm+100)%p_start	  ! S31 piN ct. term (iv,k_j,i_rs,i_c12)
	g_bare(3,1,2,6) =parm(maxnparm+101)%p_start	  ! S31 piD ct. term (iv,k_j,i_rs,i_c12)
	g_bare(3,1,2,7) =parm(maxnparm+101)%p_start	  ! S31 piD ct. term (iv,k_j,i_rs,i_c12)
	g_bare(3,1,2,13)=parm(maxnparm+102)%p_start	  ! S31 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,1,3,1) =parm(maxnparm+103)%p_start	  ! P11 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,1,3,5) =parm(maxnparm+104)%p_start	  ! P11 etN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,1,3,6) =parm(maxnparm+105)%p_start	  ! P11 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,1,3,7) =parm(maxnparm+105)%p_start	  ! P11 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,1,3,12)=parm(maxnparm+106)%p_start	  ! P11 KLa ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,1,3,13)=parm(maxnparm+107)%p_start	  ! P11 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,1,2,1) =parm(maxnparm+108)%p_start	  ! P31 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,1,2,6) =parm(maxnparm+109)%p_start	  ! P31 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,1,2,7) =parm(maxnparm+109)%p_start	  ! P31 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,1,2,13)=parm(maxnparm+110)%p_start	  ! P31 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,2,3,1) =parm(maxnparm+111)%p_start	  ! P13 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,2,3,5) =parm(maxnparm+112)%p_start	  ! P13 etN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,2,3,6) =parm(maxnparm+113)%p_start	  ! P13 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,2,3,7) =parm(maxnparm+113)%p_start	  ! P13 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,2,3,12)=parm(maxnparm+114)%p_start	  ! P13 KLa ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,2,3,13)=parm(maxnparm+115)%p_start	  ! P13 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,2,2,1) =parm(maxnparm+116)%p_start	  ! D13 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,2,2,5) =parm(maxnparm+117)%p_start	  ! D13 etN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,2,2,6) =parm(maxnparm+118)%p_start	  ! D13 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,2,2,7) =parm(maxnparm+118)%p_start	  ! D13 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,2,2,12)=parm(maxnparm+119)%p_start	  ! D13 KLa ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,2,2,13)=parm(maxnparm+120)%p_start	  ! D13 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,2,3,1) =parm(maxnparm+121)%p_start	  ! P33 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,2,3,6) =parm(maxnparm+122)%p_start	  ! P33 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,2,3,7) =parm(maxnparm+122)%p_start	  ! P33 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,2,3,13)=parm(maxnparm+123)%p_start	  ! P33 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,2,2,1) =parm(maxnparm+124)%p_start	  ! D33 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,2,2,6) =parm(maxnparm+125)%p_start	  ! D33 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,2,2,7) =parm(maxnparm+125)%p_start	  ! D33 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,2,2,13)=parm(maxnparm+126)%p_start	  ! D33 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,3,2,1) =parm(maxnparm+127)%p_start	  ! D15 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,3,2,5) =parm(maxnparm+128)%p_start	  ! D15 etN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,3,2,6) =parm(maxnparm+129)%p_start	  ! D15 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,3,2,7) =parm(maxnparm+129)%p_start	  ! D15 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,3,2,12)=parm(maxnparm+130)%p_start	  ! D15 KLa ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,3,2,13)=parm(maxnparm+131)%p_start	  ! D15 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,3,3,1) =parm(maxnparm+132)%p_start	  ! F15 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,3,3,5) =parm(maxnparm+133)%p_start	  ! F15 etN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,3,3,6) =parm(maxnparm+134)%p_start	  ! F15 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,3,3,7) =parm(maxnparm+134)%p_start	  ! F15 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,3,3,12)=parm(maxnparm+135)%p_start	  ! F15 KLa ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,3,3,13)=parm(maxnparm+136)%p_start	  ! F15 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,3,2,1) =parm(maxnparm+137)%p_start	  ! D35 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,3,2,6) =parm(maxnparm+138)%p_start	  ! D35 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,3,2,7) =parm(maxnparm+138)%p_start	  ! D35 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,3,2,13)=parm(maxnparm+139)%p_start	  ! D35 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,3,2,1) =parm(maxnparm+140)%p_start	  ! F35 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,3,2,6) =parm(maxnparm+141)%p_start	  ! F35 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,3,2,7) =parm(maxnparm+141)%p_start	  ! F35 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,3,2,13)=parm(maxnparm+142)%p_start	  ! F35 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,4,2,1) =parm(maxnparm+143)%p_start	  ! F17 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,4,2,5) =parm(maxnparm+144)%p_start	  ! F17 etN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,4,2,6) =parm(maxnparm+145)%p_start	  ! F17 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,4,2,7) =parm(maxnparm+145)%p_start	  ! F17 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,4,2,12)=parm(maxnparm+146)%p_start	  ! F17 KLa ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,4,2,13)=parm(maxnparm+147)%p_start	  ! F17 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,4,2,1) =parm(maxnparm+148)%p_start	  ! G17 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,4,2,5) =parm(maxnparm+149)%p_start	  ! G17 etN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,4,2,6) =parm(maxnparm+150)%p_start	  ! G17 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,4,2,7) =parm(maxnparm+150)%p_start	  ! G17 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,4,2,12)=parm(maxnparm+151)%p_start	  ! G17 KLa ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,4,2,13)=parm(maxnparm+152)%p_start	  ! G17 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,4,2,1) =parm(maxnparm+153)%p_start	  ! F37 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,4,2,6) =parm(maxnparm+154)%p_start	  ! F37 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,4,2,7) =parm(maxnparm+154)%p_start	  ! F37 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,4,2,13)=parm(maxnparm+155)%p_start	  ! F37 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,4,2,1) =parm(maxnparm+156)%p_start	  ! G37 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,4,2,6) =parm(maxnparm+157)%p_start	  ! G37 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,4,2,7) =parm(maxnparm+157)%p_start	  ! G37 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,4,2,13)=parm(maxnparm+158)%p_start	  ! G37 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,5,2,1) =parm(maxnparm+159)%p_start	  ! G19 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,5,2,5) =parm(maxnparm+160)%p_start	  ! G19 etN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,5,2,6) =parm(maxnparm+161)%p_start	  ! G19 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,5,2,7) =parm(maxnparm+161)%p_start	  ! G19 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,5,2,12)=parm(maxnparm+162)%p_start	  ! G19 KLa ct. term (iv,k_j,i_rs,i_c12)
        g_bare(1,5,2,13)=parm(maxnparm+163)%p_start	  ! G19 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,5,2,1) =parm(maxnparm+164)%p_start	  ! H19 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,5,2,5) =parm(maxnparm+165)%p_start	  ! H19 etN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,5,2,6) =parm(maxnparm+166)%p_start	  ! H19 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,5,2,7) =parm(maxnparm+166)%p_start	  ! H19 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,5,2,12)=parm(maxnparm+167)%p_start	  ! H19 KLa ct. term (iv,k_j,i_rs,i_c12)
        g_bare(2,5,2,13)=parm(maxnparm+168)%p_start	  ! H19 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,5,2,1) =parm(maxnparm+169)%p_start	  ! G39 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,5,2,6) =parm(maxnparm+170)%p_start	  ! G39 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,5,2,7) =parm(maxnparm+170)%p_start	  ! G39 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(3,5,2,13)=parm(maxnparm+171)%p_start	  ! G39 KSi ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,5,2,1) =parm(maxnparm+172)%p_start	  ! H39 piN ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,5,2,6) =parm(maxnparm+173)%p_start	  ! H39 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,5,2,7) =parm(maxnparm+173)%p_start	  ! H39 piD ct. term (iv,k_j,i_rs,i_c12)
        g_bare(4,5,2,13)=parm(maxnparm+174)%p_start	  ! H39 KSi ct. term (iv,k_j,i_rs,i_c12)
				coupl(9,9,1)%al1=parm(maxnparm+175)%p_start      ! DLcDLc_DDs ff		     ! NOT USED, fit parm 252 instead!
				coupl(9,9,1)%al2=parm(maxnparm+176)%p_start      ! DLcDLc_LcLcs ff
				coupl(9,9,2)%al1=parm(maxnparm+177)%p_start      ! DLcDLc_DDom ff		     ! NOT USED, fit parm 254 instead!
				coupl(9,9,2)%al2=parm(maxnparm+178)%p_start      ! DLcDLc_LcLcom ff
				coupl(9,9,3)%al1=parm(maxnparm+179)%p_start      ! DLcDLc_DDphi ff		     ! NOT USED, fit parm 256 instead!
				coupl(9,9,3)%al2=parm(maxnparm+180)%p_start      ! DLcDLc_LcLcphi ff
				coupl(9,10,1)%al1=parm(maxnparm+181)%p_start      ! LcDScD_DDrho ff		     ! NOT USED, fit parm 258 instead!
				coupl(9,10,1)%al2=parm(maxnparm+182)%p_start      ! LcDScD_LcScrho ff
				coupl(10,10,1)%al1=parm(maxnparm+183)%p_start      ! DScDScts ff
				coupl(10,10,1)%al2=parm(maxnparm+184)%p_start      ! DScDScts contact ff
				coupl(10,10,2)%al1=parm(maxnparm+185)%p_start      ! DScDSctom_omDD ff
				coupl(10,10,2)%al2=parm(maxnparm+186)%p_start      ! DScDSctom_omScSc ff
				coupl(10,10,3)%al1=parm(maxnparm+187)%p_start      ! DScDSctph_phDD ff
				coupl(10,10,3)%al2=parm(maxnparm+188)%p_start      ! DScDSctph_phScSc ff
				coupl(10,10,4)%al1=parm(maxnparm+189)%p_start      ! DScDSctr_rDD ff
				coupl(10,10,4)%al2=parm(maxnparm+190)%p_start      ! DScDSctr_rScSc ff
				coupl(9,9,4)%al1=parm(maxnparm+191)%p_start	  ! DLamcDLamcuXicc_LamcXiccD_ff
				coupl(10,10,5)%al1=parm(maxnparm+192)%p_start	  ! DSigcDSigcuXicc_DXiccSigc_ff
				gcoup_global(23)=parm(maxnparm+193)%p_start      ! g_LamcLamcsig * g_DDsig /(4pi)
				gcoup_global(24)=parm(maxnparm+194)%p_start      ! g_SigcSigcsig * g_DDsig /(4pi) ! Solu 15.5: 3.154000
! add DsYc new part
				coupl(9,11,1)%al1=parm(maxnparm+195)%p_start      ! DLcDsLcto_DsDom ff
				coupl(9,11,1)%al2=parm(maxnparm+178)%p_start      ! DLcDsLcto_LcLcom ff
				coupl(9,11,2)%al1=parm(maxnparm+191)%p_start      ! DLcDsLcuX_DLamcXicc ff
				coupl(9,11,2)%al2=parm(maxnparm+203)%p_start      ! DLcDsLcuX_DsLamcXicc ff
				coupl(9,12,1)%al1=parm(maxnparm+196)%p_start      ! DLcDsSctr_DsDr ff
				coupl(9,12,1)%al2=parm(maxnparm+182)%p_start      ! DLcDsSctr_LcScr ff
				coupl(9,12,2)%al1=parm(maxnparm+197)%p_start      ! DLcDsSctp_DsDp ff
				coupl(9,12,2)%al2=parm(maxnparm+198)%p_start      ! DLcDsSctp_LcScp ff
				coupl(9,12,3)%al1=parm(maxnparm+192)%p_start      ! DLcDsScuX_DSigcXicc ff
				coupl(9,12,3)%al2=parm(maxnparm+203)%p_start      ! DLcDsScuX_DsLamcXicc ff
				coupl(10,11,1)%al1=parm(maxnparm+196)%p_start      ! DScDsLctr_DsDr ff
				coupl(10,11,1)%al2=parm(maxnparm+182)%p_start      ! DScDsLctr_LcScr ff
				coupl(10,11,2)%al1=parm(maxnparm+197)%p_start      ! DScDsLctp_DsDp ff
				coupl(10,11,2)%al2=parm(maxnparm+198)%p_start      ! DScDsLctp_LcScp ff
				coupl(10,11,3)%al1=parm(maxnparm+191)%p_start      ! DScDsLcuX_DLamcXicc ff
				coupl(10,11,3)%al2=parm(maxnparm+204)%p_start      ! DScDsLcuX_DsSigcXicc ff
				coupl(10,12,1)%al1=parm(maxnparm+195)%p_start      ! DScDsSctr_DsDom ff
				coupl(10,12,1)%al2=parm(maxnparm+186)%p_start      ! DScDsSctr_ScScom ff
				coupl(10,12,2)%al1=parm(maxnparm+196)%p_start      ! DScDsSctr_DsDr ff
				coupl(10,12,2)%al2=parm(maxnparm+190)%p_start      ! DScDsSctr_ScScr ff
				coupl(10,12,3)%al1=parm(maxnparm+197)%p_start      ! DScDsSctp_DsDp ff
				coupl(10,12,3)%al2=parm(maxnparm+199)%p_start      ! DScDsSctp_ScScp ff
				coupl(10,12,4)%al1=parm(maxnparm+192)%p_start      ! DScDsScuX_DSigcXicc ff
				coupl(10,12,4)%al2=parm(maxnparm+204)%p_start      ! DScDsScuX_DsSigcXicc ff
				coupl(11,11,1)%al1=parm(maxnparm+200)%p_start      ! DsLcDsLcto_DsDsom ff
				coupl(11,11,1)%al2=parm(maxnparm+178)%p_start      ! DsLcDsLcto_LcLcom ff
				coupl(11,11,2)%al1=parm(maxnparm+203)%p_start      ! DsLcDsLcuX_DsLamcXicc ff
				coupl(11,11,2)%al2=parm(maxnparm+203)%p_start      ! DsLcDsLcuX_DsLamcXicc ff
				coupl(11,12,1)%al1=parm(maxnparm+201)%p_start      ! DsLcDsSctr_DsDsr ff
				coupl(11,12,1)%al2=parm(maxnparm+182)%p_start      ! DsLcDsSctr_LcScr ff
				coupl(11,12,2)%al1=parm(maxnparm+202)%p_start      ! DsLcDsSctp_DsDsp ff
				coupl(11,12,2)%al2=parm(maxnparm+198)%p_start      ! DsLcDsSctp_LcScp ff
				coupl(11,12,3)%al1=parm(maxnparm+204)%p_start      ! DsLcDsScuX_DsSigcXicc ff
				coupl(11,12,3)%al2=parm(maxnparm+203)%p_start      ! DsLcDsScuX_DsLamcXicc ff
				coupl(12,12,1)%al1=parm(maxnparm+200)%p_start      ! DsScDsScto_DsDsom ff
				coupl(12,12,1)%al2=parm(maxnparm+186)%p_start      ! DsScDsScto_ScScom ff
				coupl(12,12,2)%al1=parm(maxnparm+201)%p_start      ! DsScDsSctr_DsDsr ff
				coupl(12,12,2)%al2=parm(maxnparm+190)%p_start      ! DsScDsSctr_ScScr ff
				coupl(12,12,3)%al1=parm(maxnparm+202)%p_start      ! DsScDsSctp_DsDsp ff
				coupl(12,12,3)%al2=parm(maxnparm+199)%p_start      ! DsScDsSctp_ScScp ff
				coupl(12,12,4)%al1=parm(maxnparm+204)%p_start      ! DsScDsScuX_DsSigcXicc ff
				coupl(12,12,4)%al2=parm(maxnparm+204)%p_start      ! DsScDsScuX_DsSigcXicc ff

        ! vertices with the same kinematics should have equal cut offs:

	if (1==1) then

	! pNN vertex with N in u-channel:
        coupl(1,4,1)%al1 =parm(maxnparm+1)%p_start	! this line redundant ???
	parm(maxnparm+5)%p_start=parm(maxnparm+1)%p_start

	coupl(1,1,1)%al1 =parm(maxnparm+1)%p_start
	parm(maxnparm+30)%p_start=parm(maxnparm+1)%p_start

	coupl(1,3,1)%al1 =parm(maxnparm+1)%p_start
	parm(maxnparm+59)%p_start=parm(maxnparm+1)%p_start

	coupl(1,2,1)%al1 =parm(maxnparm+1)%p_start
	parm(maxnparm+64)%p_start=parm(maxnparm+1)%p_start

	coupl(2,4,2)%al2 =parm(maxnparm+1)%p_start
	parm(maxnparm+81)%p_start=parm(maxnparm+1)%p_start

	! sKK vertex with sigma in t-channel:
	coupl(7,7,1)%al1 =parm(maxnparm+36)%p_start
	parm(maxnparm+50)%p_start=parm(maxnparm+36)%p_start

	! rNN vertex with rho in t-channel:
	coupl(2,2,3)%al2 =parm(maxnparm+33)%p_start
	parm(maxnparm+76)%p_start=parm(maxnparm+33)%p_start

	! rpp vertex with rho in t-channel:
	coupl(4,4,2)%al2 =parm(maxnparm+8)%p_start
	parm(maxnparm+13)%p_start=parm(maxnparm+8)%p_start

	! rKK vertex with rho in t-channel:
	coupl(7,8,1)%al1 =parm(maxnparm+42)%p_start
	parm(maxnparm+57)%p_start=parm(maxnparm+42)%p_start

	! pND vertex with Delta in u-channel:
	 coupl(1,1,4)%al1 =parm(maxnparm+9)%p_start
	 parm(maxnparm+34)%p_start=parm(maxnparm+9)%p_start
	 coupl(1,1,4)%al2= parm(maxnparm+9)%p_start	! THIS HAS BEEN INSERTED WITH EXPLICIT SECOND FF IN "d_f".

	! rNN vertex with N in u-channel:
	coupl(2,2,1)%al1 =parm(maxnparm+65)%p_start
	parm(maxnparm+73)%p_start=parm(maxnparm+65)%p_start

	! etaNN vertex with N in u-channel:
	coupl(3,3,1)%al1 =parm(maxnparm+60)%p_start
	parm(maxnparm+63)%p_start=parm(maxnparm+60)%p_start

	! pND vertex with N in u-channel:
	coupl(4,4,1)%al1 =parm(maxnparm+6)%p_start
	parm(maxnparm+11)%p_start=parm(maxnparm+6)%p_start

	! pNN vertex with pi in t-channel:
	coupl(1,2,3)%al2 =parm(maxnparm+3)%p_start
	parm(maxnparm+68)%p_start=parm(maxnparm+3)%p_start

	! pND vertex with pi in t-channel:
	coupl(2,4,1)%al2 =parm(maxnparm+14)%p_start
	parm(maxnparm+79)%p_start=parm(maxnparm+14)%p_start

	! KKo vertex with omega in t-channel:
	coupl(7,7,2)%al1 =parm(maxnparm+38)%p_start
	parm(maxnparm+52)%p_start=parm(maxnparm+38)%p_start

	! pKK^* vertex with K^* in t-channel:
	coupl(1,7,1)%al1 =parm(maxnparm+22)%p_start
	parm(maxnparm+44)%p_start=parm(maxnparm+22)%p_start

	! NKS vertex with Sigma in u-channel:
	coupl(1,7,2)%al2 =parm(maxnparm+25)%p_start
	parm(maxnparm+47)%p_start=parm(maxnparm+25)%p_start

	! phiKK vertex with phi in t-channel:
	coupl(7,7,3)%al1 =parm(maxnparm+40)%p_start
	parm(maxnparm+54)%p_start=parm(maxnparm+40)%p_start

        ! rpp vertex with pi in t-channel:
        coupl(2,4,1)%al1 =parm(maxnparm+67)%p_start
        parm(maxnparm+78)%p_start=parm(maxnparm+67)%p_start

        ! sNN vertex with N in u-channel:
        coupl(5,5,1)%al1 =parm(maxnparm+2)%p_start
        parm(maxnparm+16)%p_start=parm(maxnparm+2)%p_start

        ! spipi-vertex mit pi in t-channel:
        coupl(4,5,1)%al2 =parm(maxnparm+4)%p_start
        parm(maxnparm+15)%p_start=parm(maxnparm+4)%p_start

        ! kappaKpi-vertex mit kappa in t-channel:
        coupl(1,8,4)%al1 =parm(maxnparm+82)%p_start
        parm(maxnparm+84)%p_start=parm(maxnparm+82)%p_start

        ! Switched on diagram piN --> rhoN with Delta in u-channel:  / HAS NO PARAMETER THAT COULD BE SET EQUAL, THUS only coupl are set:
        coupl(1,2,6)%al1 =parm(maxnparm+9)%p_start
        coupl(1,2,6)%al2 =parm(maxnparm+77)%p_start

	!Switched on diagram piD-->piD with D in u-channel , piDD vertex with D in u-channel; only one FF
	coupl(4,4,3)%al1=parm(maxnparm+10)%p_start

	! etaN--> KLam with K* in t, etaKK* Vertex
	coupl(3,7,1)%al1=parm(maxnparm+87)%p_start

	! etaN-->KSig with K* in t, NSigK*-Vertex=parm(239)
	coupl(3,8,1)%al2=parm(maxnparm+23)%p_start

	! etaN-->KLam with K* in t. NLamK*-Vertex=parm(261)
	coupl(3,7,1)%al2=parm(maxnparm+45)%p_start

	! LamXiK-Vertex with Xi in u-channel
	coupl(7,7,4)%al2=parm(maxnparm+88)%p_start !  KLam-->KLam
	coupl(7,8,2)%al2=parm(maxnparm+88)%p_start !	LamK->SigK

	! LamXi*K vertex with Xi* in u-channel
	coupl(7,7,5)%al2=parm(maxnparm+89)%p_start !  KLam-->KLam
	coupl(7,8,3)%al2=parm(maxnparm+89)%p_start !	LamK->SigK

	! KXiSigma vertex with Xi in u-channel
	coupl(8,8,5)%al2=parm(maxnparm+90)%p_start	! KSig->KSig
	coupl(7,8,2)%al1=parm(maxnparm+90)%p_start	! KLam->KSig

	! KXi*Sig vertex with Xi* in u-channel
	coupl(8,8,6)%al2=parm(maxnparm+91)%p_start	! KSig->KSig
	coupl(7,8,3)%al1=parm(maxnparm+91)%p_start	! KLam->KSig

	! etaN-->KLam with Lam in u. NLamK-Vertex=parm(243)
	coupl(3,7,2)%al2=parm(maxnparm+27)%p_start

	! etaN->KS with Sig in u. NSK vertex=parm(241)
	 coupl(3,8,2)%al2=parm(maxnparm+25)%p_start

	! NSig*K vertex
	coupl(1,7,4)%al2=parm(maxnparm+29)%p_start 	! pNKLuS*_NKS*
	parm(maxnparm+49)%p_start=parm(maxnparm+29)%p_start
	coupl(3,8,3)%al2=parm(maxnparm+29)%p_start	! etaN-->SigK

	! LamcXiccD-Vertex with Xicc in u-channel
	coupl(9,9,4)%al2=parm(maxnparm+191)%p_start !  DLamc-->DLamc
	coupl(9,10,2)%al2=parm(maxnparm+191)%p_start !	LamcD->SigcD

	! DXiccSigmac vertex with Xicc in u-channel
	coupl(10,10,5)%al2=parm(maxnparm+192)%p_start	! DSigc->DSigc
	coupl(9,10,2)%al1=parm(maxnparm+192)%p_start	! DLamc->DSigc

	! sDD vertex with sigma in t-channel:
	coupl(10,10,1)%al1 =parm(maxnparm+175)%p_start
	parm(maxnparm+183)%p_start=parm(maxnparm+175)%p_start

	! DDo vertex with omega in t-channel:
	coupl(10,10,2)%al1 =parm(maxnparm+177)%p_start
	parm(maxnparm+185)%p_start=parm(maxnparm+177)%p_start

	! phiDD vertex with phi in t-channel:
	coupl(10,10,3)%al1 =parm(maxnparm+179)%p_start
	parm(maxnparm+187)%p_start=parm(maxnparm+179)%p_start

	! rDD vertex with rho in t-channel:
	coupl(10,10,4)%al1 =parm(maxnparm+181)%p_start
	parm(maxnparm+189)%p_start=parm(maxnparm+181)%p_start

	end if ! 1==0


	call coupl_prep(gcoup_global)	       	! Translate coupling strengths in internal "coupl" variable
						! which is used by potentials.


	return
	end subroutine transfer_all_parms
