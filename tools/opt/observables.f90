  	!=====================================================================================
	!
	!  chi_square_and_observables.f90 has been changed to observables.f90 because in this version
        !  we don't want to ft or calculate the chi^2. function chi_sq is missing here.
	!
	!=====================================================================================

! ===============================================================================================================


	subroutine sig_tot(ez,sigma_tot)			! Input: Complex energy "ez" [MeV]
								! Output: real total cross section "sigma_kl" in [mb]
	use input_minuit
	use input4
	use tmat4
	implicit none
	complex(kind(0.d0))	:: ez,phasfac,phasfacnew		! ez: complex energy
	complex(kind(0.d0)),dimension(10) :: tau
	real(kind(0.d0)), dimension(10) :: sigma_tot
	integer			:: iv,k_j,n1
	n1=n+1
        call qon_c(ez,qon)  					! on shell momenta: qon(n_c=13) is complex (initialized in "input4").

        call make_full_T(ez)

	sigma_tot=0.d0
        do k_j=k_b,k_e

	  do iv=1,2

	    tau(1)=dsqrt(2.d0)/3.d0*(tshort(1,1,iv+2,k_j)-tshort(1,1,iv,k_j))*phasfac(ez,1)  	!!!!! CHECK IF 	tshort(1,1,iv+1,k_j) Programmed should be correct		! pi-p -> pi0n
	    sigma_tot(1)=sigma_tot(1)+fpi/qon(1)**2*(2.d0*k_j)*tau(1)*conjg(tau(1))/2.d0*uf*uf*10.d0

	    tau(2)=dsqrt(2.d0/3.d0)*tshort(5,1,iv,k_j)*phasfac(ez,5)							! piN-> N eta
	    sigma_tot(2)=sigma_tot(2)+fpi/qon(1)**2*(2.d0*k_j)*tau(2)*conjg(tau(2))/2.d0*uf*uf*10.d0

	    tau(3)=dsqrt(2.d0/3.d0)*tshort(12,1,iv,k_j)*phasfac(ez,12)							! piN-> K Lambda
	    sigma_tot(3)=sigma_tot(3)+fpi/qon(1)**2*(2.d0*k_j)*tau(3)*conjg(tau(3))/2.d0*uf*uf*10.d0

	    tau(4)=dsqrt(2.d0)/3.d0*(tshort(13,1,iv,k_j)*phasfac(ez,13)+tshort(13,1,iv+2,k_j)*phasfac(ez,13))    	! pi-p -> Sig0K0
	    sigma_tot(4)=sigma_tot(4)+fpi/qon(1)**2*(2.d0*k_j)*tau(4)*conjg(tau(4))/2.d0*uf*uf*10.d0

	    tau(5)=-2.d0/3.d0*tshort(13,1,iv,k_j)*phasfac(ez,13)+1.d0/3.d0*tshort(13,1,iv+2,k_j)*phasfac(ez,13)  	! pi-p -> Sig-K+
	    sigma_tot(5)=sigma_tot(5)+fpi/qon(1)**2*(2.d0*k_j)*tau(5)*conjg(tau(5))/2.d0*uf*uf*10.d0

	    tau(6)=tshort(13,1,iv+2,k_j)*phasfac(ez,13)							       		! pi-p -> Sig+K+
	    sigma_tot(6)=sigma_tot(6)+fpi/qon(1)**2*(2.d0*k_j)*tau(6)*conjg(tau(6))/2.d0*uf*uf*10.d0
!NEW
        tau(7)=tshort(14,14,iv,k_j)*phasfacnew(ez,14,14)
        sigma_tot(7)=sigma_tot(7)+fpi/qon(14)**2*(2.d0*k_j)*tau(7)*conjg(tau(7))/2.d0*uf*uf*10.d0
        tau(8)=(1.d0)/dsqrt(3.d0)*tshort(15,14,iv,k_j)*phasfacnew(ez,14,15)
        sigma_tot(8)=sigma_tot(8)+fpi/qon(14)**2*(2.d0*k_j)*tau(8)*conjg(tau(8))/2.d0*uf*uf*10.d0
        tau(9)=(-1.d0)*dsqrt(2.d0/3.d0)*tshort(15,14,iv,k_j)*phasfacnew(ez,14,15)
        sigma_tot(9)=sigma_tot(9)+fpi/qon(14)**2*(2.d0*k_j)*tau(9)*conjg(tau(9))/2.d0*uf*uf*10.d0
        tau(10)=tshort(15,15,iv+2,k_j)*phasfacnew(ez,15,15)
        sigma_tot(10)=sigma_tot(10)+fpi/qon(15)**2*(2.d0*k_j)*tau(10)*conjg(tau(10))/2.d0*uf*uf*10.d0
!NEWforDs

	  end do						! iv
	end do							! k_j
	return

	end subroutine sig_tot

! ===============================================================================================================

	subroutine sig_tot_einzeln(ez,iv,k_j,sigma_tot)			! Input: Complex energy "ez" [MeV]
								! Output: real total cross section "sigma_kl" in [mb]
	use input_minuit
	use input4
	use tmat4
	implicit none
	complex(kind(0.d0))	:: ez,phasfac		! ez: complex energy
	complex(kind(0.d0)),dimension(6) :: tau
	real(kind(0.d0)), dimension(6) :: sigma_tot
	integer			:: iv,k_j,n1
	n1=n+1
        call qon_c(ez,qon)  					! on shell momenta: qon(n_c=13) is complex (initialized in "input4").
        call make_full_T(ez)

	sigma_tot=0.d0
!        do k_j=k_b,k_e

!	  do iv=1,2

	    if (iv==1.or.iv==2) then

	    tau(1)=dsqrt(2.d0)/3.d0*(-tshort(1,1,iv,k_j))*phasfac(ez,1)  						! pi-p -> pi0n
	    sigma_tot(1)=sigma_tot(1)+fpi/qon(1)**2*(2.d0*k_j)*tau(1)*conjg(tau(1))/2.d0*uf*uf*10.d0

	    tau(2)=dsqrt(2.d0/3.d0)*tshort(5,1,iv,k_j)*phasfac(ez,5)							! piN-> N eta
	    sigma_tot(2)=sigma_tot(2)+fpi/qon(1)**2*(2.d0*k_j)*tau(2)*conjg(tau(2))/2.d0*uf*uf*10.d0

	    tau(3)=dsqrt(2.d0/3.d0)*tshort(12,1,iv,k_j)*phasfac(ez,12)							! piN-> K Lambda
	    sigma_tot(3)=sigma_tot(3)+fpi/qon(1)**2*(2.d0*k_j)*tau(3)*conjg(tau(3))/2.d0*uf*uf*10.d0

	    tau(4)=dsqrt(2.d0)/3.d0*(tshort(13,1,iv,k_j)*phasfac(ez,13))    						! pi-p -> Sig0K0
	    sigma_tot(4)=sigma_tot(4)+fpi/qon(1)**2*(2.d0*k_j)*tau(4)*conjg(tau(4))/2.d0*uf*uf*10.d0

	    tau(5)=-2.d0/3.d0*tshort(13,1,iv,k_j)*phasfac(ez,13)  							! pi-p -> Sig-K+
	    sigma_tot(5)=sigma_tot(5)+fpi/qon(1)**2*(2.d0*k_j)*tau(5)*conjg(tau(5))/2.d0*uf*uf*10.d0

	    else


	    tau(1)=dsqrt(2.d0)/3.d0*(tshort(1,1,iv,k_j))*phasfac(ez,1)  						! pi-p -> pi0n
	    sigma_tot(1)=sigma_tot(1)+fpi/qon(1)**2*(2.d0*k_j)*tau(1)*conjg(tau(1))/2.d0*uf*uf*10.d0

	    tau(4)=dsqrt(2.d0)/3.d0*(tshort(13,1,iv,k_j)*phasfac(ez,13))    						! pi-p -> Sig0K0
	    sigma_tot(4)=sigma_tot(4)+fpi/qon(1)**2*(2.d0*k_j)*tau(4)*conjg(tau(4))/2.d0*uf*uf*10.d0

	    tau(5)=1.d0/3.d0*tshort(13,1,iv,k_j)*phasfac(ez,13)  							! pi-p -> Sig-K+
	    sigma_tot(5)=sigma_tot(5)+fpi/qon(1)**2*(2.d0*k_j)*tau(5)*conjg(tau(5))/2.d0*uf*uf*10.d0

	    tau(6)=tshort(13,1,iv,k_j)*phasfac(ez,13)							       		! pi+p -> Sig+K+
	    sigma_tot(6)=sigma_tot(6)+fpi/qon(1)**2*(2.d0*k_j)*tau(6)*conjg(tau(6))/2.d0*uf*uf*10.d0


	    end if


!	  end do						! iv
!	end do							! k_j
	return

	end subroutine sig_tot_einzeln

	!-----------------------------------------------------------------------------------------------------------

	subroutine obs_KY(thetav,ez,difsig,pola,beta)	! Input: List of angles [rad]. Complex energy "ez" [MeV]
							! Output: observables Difsig, polarization
	use input_minuit
	use input4
	use tmat4
	implicit none
	complex(kind(0.d0))	:: ez,phasfac,phasfacnew,ppkl,ppks,pppin,ppne,z1,fsaid,ppdldl,ppdlds,ppdsds		! ez: complex energy
	real(kind(0.d0)), dimension(10,50) :: difsig, pola, beta
	real(kind(0.d0)), dimension(50) :: thetav, x_kzl				! vektor mit Eintraegen winkel von pi bis tpi
	real(kind(0.d0)) :: dj, dmj,theta
	complex(kind(0.d0)), dimension(50) :: tauplus_kzl,tauminus_kzl,tauplus_kzsz,tauminus_kzsz,tauplus_kpsm,tauminus_kpsm, &
	                                     & tauplus_kpsp, tauminus_kpsp,tauminus_neta, tauplus_neta, tauminus_pin, tauplus_pin, &
																			 & tauplus_dml, tauminus_dml, tauplus_dmsp, tauminus_dmsp, tauplus_dzsz, tauminus_dzsz, &
																		 	 & tauplus_dzspp, tauminus_dzspp
  complex(kind(0.d0)), dimension(50) :: g_kzl,h_kzl,g_kzsz,h_kzsz,g_kpsm,h_kpsm,g_kpsp,h_kpsp, g_neta, h_neta, g_pin, h_pin, &
		                                   & g_dml,h_dml,g_dmsp,h_dmsp,g_dzsz,h_dzsz,g_dzspp,h_dzspp
	integer			::i,k_j,n1, iv


	n1=n+1
	z1=dcmplx(0.d0,1.d0)

        call qon_c(ez,qon)  								! on shell momenta: qon(n_c=13) is complex (initialized in "input4").
        call make_full_T(ez)

        tauplus_kzl=0.d0
        tauminus_kzl=0.d0
	tauminus_kzsz=0.d0
	tauplus_kzsz=0.d0
	tauminus_kpsm=0.d0
	tauplus_kpsm=0.d0
	tauminus_kpsp=0.d0
	tauplus_kpsp=0.d0
	tauminus_neta=0.d0
	tauplus_neta=0.d0
	tauminus_pin=0.d0
	tauplus_pin=0.d0

	tauplus_dml=0.d0
	tauminus_dml=0.d0
	tauplus_dmsp=0.d0
	tauminus_dmsp=0.d0
	tauplus_dzsz=0.d0
	tauminus_dzsz=0.d0
	tauplus_dzspp=0.d0
	tauminus_dzspp=0.d0

	g_kzl=0.d0
	h_kzl=0.d0
	g_kzsz=0.d0
	h_kzsz=0.d0
	g_kpsm=0.d0
	h_kpsm=0.d0
	g_kpsp=0.d0
	h_kpsp=0.d0
	g_neta=0.d0
	h_neta=0.d0
	g_pin=0.d0
	h_pin=0.d0

	g_dml=0.d0
	h_dml=0.d0
	g_dmsp=0.d0
	h_dmsp=0.d0
	g_dzsz=0.d0
	h_dzsz=0.d0
	g_dzspp=0.d0
	h_dzspp=0.d0

	difsig=0.d0
	pola=0.d0
	pppin=phasfac(ez,1)
	ppne=phasfac(ez,5)
	ppkl=phasfac(ez,12)
	ppks=phasfac(ez,13)

	ppdldl=phasfacnew(ez,14,14)
	ppdlds=phasfacnew(ez,14,15)
	ppdsds=phasfacnew(ez,15,15)

        do k_j=k_b,k_e



	   do i=1,50
	    if(thetav(i)/=100.d0) then
		theta=thetav(i)

             if ( k_j==1 ) then  				! J=1/2
                dj = dcos(theta/2.d0)				! Brink und Satchler
                dmj=dsin(theta/2.d0)				! Brink und Satchler
             else if ( k_j==2 ) then      			! J=3/2
 		dj=dcos(theta/2.d0)*(3.d0*(dcos(theta/2.d0))**2-2.d0)
		dmj=-dsin(theta/2.d0)*(3.d0*(dsin(theta/2.d0))**2-2.d0)
             else if ( k_j==3  )  then      			! J=5/2
		dj=dcos(theta/2.d0)*(3.d0*(dsin(theta/2.d0))**4-6.d0*(dcos(theta/2.d0))**2*(dsin(theta/2.d0))**2+(dcos(theta/2.d0))**4)
      		dmj=dsin(theta/2.d0)*(10.d0*(dsin(theta/2.d0))**4-12.d0*(dsin(theta/2.d0))**2+3.d0)		!
             else if ( k_j==4  )  then      			! J=7/2
	        dj=(dcos(theta/2.d0))**7-12.d0*(dcos(theta/2.d0))**5*(dsin(theta/2.d0))**2+ &
                & 18.d0*(dcos(theta/2.d0))**3*(dsin(theta/2.d0))**4-4.d0*(dcos(theta/2.d0))*(dsin(theta/2.d0))**6
		dmj=(-(dsin(theta/2.d0))**7+12.d0*(dcos(theta/2.d0))**2*(dsin(theta/2.d0))**5- &
		& 18.d0*(dcos(theta/2.d0))**4*(dsin(theta/2.d0))**3+4.d0*(dcos(theta/2.d0))**6*dsin(theta/2.d0))
	     else if ( k_j==5  )  then 				! J=9/2
	        dj=(dcos(theta/2.d0))**9-20.d0*(dcos(theta/2.d0))**7*(dsin(theta/2.d0))**2+60.d0*(dcos(theta/2.d0))**5*(dsin(theta/2.d0))**4- &
	        & 40.d0*(dcos(theta/2.d0))**3*(dsin(theta/2.d0))**6+5.d0*(dcos(theta/2.d0))*(dsin(theta/2.d0))**8
   		dmj=5.d0*(dcos(theta/2.d0))**8*(dsin(theta/2.d0))-40.d0*(dcos(theta/2.d0))**6*(dsin(theta/2.d0))**3+ &
		& 60.d0*(dcos(theta/2.d0))**4*(dsin(theta/2.d0))**5-20.d0*(dcos(theta/2.d0))**2*(dsin(theta/2.d0))**7+(dsin(theta/2.d0))**9
             end if

	!pi N
	tauplus_pin(i)=tauplus_pin(i)+(2.d0*k_j)*dsqrt(2.d0)/3.d0*(-tshort(1,1,1,k_j)-tshort(1,1,2,k_j)+tshort(1,1,3,k_j)+tshort(1,1,4,k_j))*pppin*dj
        tauminus_pin(i)=tauminus_pin(i)+(2.d0*k_j)*dsqrt(2.d0)/3.d0*(-tshort(1,1,1,k_j)+tshort(1,1,2,k_j)+tshort(1,1,3,k_j)-tshort(1,1,4,k_j))*pppin*dmj

	! N Eta
        tauplus_neta(i)= tauplus_neta(i)+(2.d0*k_j)*dsqrt(2.d0/3.d0)*(tshort(1,5,1,k_j)+tshort(1,5,2,k_j))*ppne*dj
        tauminus_neta(i)=tauminus_neta(i)+(2.d0*k_j)*dsqrt(2.d0/3.d0)*(tshort(1,5,1,k_j)-tshort(1,5,2,k_j))*ppne*dmj

	! Ko Lambda
	tauplus_kzl(i) = tauplus_kzl(i)+(2.d0*k_j)*dsqrt(2.d0/3.d0)*(tshort(12,1,1,k_j)+tshort(12,1,2,k_j))*ppkl*dj
	tauminus_kzl(i)= tauminus_kzl(i)+(2.d0*k_j)*dsqrt(2.d0/3.d0)*(tshort(12,1,1,k_j)-tshort(12,1,2,k_j))*ppkl*dmj

	! K0 Sigma0
	tauplus_kzsz(i)= tauplus_kzsz(i)+ (2.d0*k_j)*dsqrt(2.d0)/3.d0*(tshort(13,1,1,k_j)+tshort(13,1,2,k_j)+tshort(13,1,3,k_j)+tshort(13,1,4,k_j))*ppks*dj
	tauminus_kzsz(i)=tauminus_kzsz(i)+(2.d0*k_j)*dsqrt(2.d0)/3.d0*(tshort(13,1,1,k_j)-tshort(13,1,2,k_j)+tshort(13,1,3,k_j)-tshort(13,1,4,k_j))*ppks*dmj

	! K+ Sigma-
	tauplus_kpsm(i)=tauplus_kpsm(i)+(2.d0*k_j)*((-2.d0/3.d0)*(tshort(13,1,1,k_j)+tshort(13,1,2,k_j))+(1.d0/3.d0)*(tshort(13,1,3,k_j)+tshort(13,1,4,k_j)))*ppks*dj
	tauminus_kpsm(i)=tauminus_kpsm(i)+(2.d0*k_j)*((-2.d0/3.d0)*(tshort(13,1,1,k_j)-tshort(13,1,2,k_j))+(1.d0/3.d0)*(tshort(13,1,3,k_j)-tshort(13,1,4,k_j)))*ppks*dmj

	! K+ Sigma+
	tauplus_kpsp(i)=tauplus_kpsp(i)+(2.d0*k_j)*(tshort(13,1,3,k_j)+tshort(13,1,4,k_j))*ppks*dj
	tauminus_kpsp(i)=tauminus_kpsp(i)+(2.d0*k_j)*(tshort(13,1,3,k_j)-tshort(13,1,4,k_j))*ppks*dmj


!!!!!
		! D- Lambdac --> D- Lambdac
		tauplus_dml(i) = tauplus_dml(i)+(2.d0*k_j)*(tshort(14,14,1,k_j)+tshort(14,14,2,k_j))*ppdldl*dj				!ppdl need change
		tauminus_dml(i)= tauminus_dml(i)+(2.d0*k_j)*(tshort(14,14,1,k_j)-tshort(14,14,2,k_j))*ppdldl*dmj

		! D- Lambdac --> D- Sigmac+
		tauplus_dmsp(i)= tauplus_dmsp(i)+ (2.d0*k_j)*(1.d0)/dsqrt(3.d0)*(tshort(15,14,1,k_j)+tshort(15,14,2,k_j))*ppdlds*dj
		tauminus_dmsp(i)=tauminus_dmsp(i)+(2.d0*k_j)*(1.d0)/dsqrt(3.d0)*(tshort(15,14,1,k_j)-tshort(15,14,2,k_j))*ppdlds*dmj

		! D- Lambdac --> D0 Sigmac0
		tauplus_dzsz(i)=tauplus_dzsz(i)+(2.d0*k_j)*(-1.d0)*dsqrt(2.d0/3.d0)*(tshort(15,14,1,k_j)+tshort(15,14,2,k_j))*ppdlds*dj
		tauminus_dzsz(i)=tauminus_dzsz(i)+(2.d0*k_j)*(-1.d0)*dsqrt(2.d0/3.d0)*(tshort(15,14,1,k_j)-tshort(15,14,2,k_j))*ppdlds*dmj

		! D0 Sigmac++ --> D0 Sigmac++
		tauplus_dzspp(i)=tauplus_dzspp(i)+(2.d0*k_j)*(tshort(15,15,3,k_j)+tshort(15,15,4,k_j))*ppdsds*dj
		tauminus_dzspp(i)=tauminus_dzspp(i)+(2.d0*k_j)*(tshort(15,15,3,k_j)-tshort(15,15,4,k_j))*ppdsds*dmj
	    end if
        !write(0,*)'theta,dj,dmj,k_j',theta,dj,dmj,k_j
        !write(0,*)tshort(14,14,1,k_j),tshort(14,14,2,k_j),tshort(15,14,1,k_j),tshort(15,14,2,k_j),tshort(15,15,3,k_j),tshort(15,15,4,k_j)
	   end do !i
        !write(0,*)dj,dmj,tauplus_dml(i),tauplus_dmsp(i),tauplus_dzsz(i),tauplus_dzspp(i)
          end do !k_j

	do i=1,50
	if(thetav(i)/=100.d0) then
		theta=thetav(i)

	difsig(1,i)=dble(1.d0/qon(1)**2*1.d0/2.d0*(tauplus_pin(i)*conjg(tauplus_pin(i))+tauminus_pin(i)*conjg(tauminus_pin(i)))/2.d0*uf*uf*10.d0)   	! difsig PiN
	difsig(2,i)=dble(1.d0/qon(1)**2*1.d0/2.d0*(tauplus_neta(i)*conjg(tauplus_neta(i))+tauminus_neta(i)*conjg(tauminus_neta(i)))/2.d0*uf*uf*10.d0)   ! difsig N eta
	difsig(3,i)=dble(1.d0/qon(1)**2*1.d0/2.d0*(tauplus_kzl(i)*conjg(tauplus_kzl(i))+tauminus_kzl(i)*conjg(tauminus_kzl(i)))/2.d0*uf*uf*10.d0)   	! difsig KLambda
	difsig(4,i)=dble(1.d0/qon(1)**2*1.d0/2.d0*(tauplus_kzsz(i)*conjg(tauplus_kzsz(i))+tauminus_kzsz(i)*conjg(tauminus_kzsz(i)))/2.d0*uf*uf*10.d0)   ! difsig KzSz
	difsig(5,i)=dble(1.d0/qon(1)**2*1.d0/2.d0*(tauplus_kpsm(i)*conjg(tauplus_kpsm(i))+tauminus_kpsm(i)*conjg(tauminus_kpsm(i)))/2.d0*uf*uf*10.d0)   ! difsig KpSm
 	difsig(6,i)=dble(1.d0/qon(1)**2*1.d0/2.d0*(tauplus_kpsp(i)*conjg(tauplus_kpsp(i))+tauminus_kpsp(i)*conjg(tauminus_kpsp(i)))/2.d0*uf*uf*10.d0)   ! difsig KpSp

!NEW for DL and DS
difsig(7,i)=dble(1.d0/qon(14)**2*1.d0/2.d0*(tauplus_dml(i)*conjg(tauplus_dml(i))+tauminus_dml(i)*conjg(tauminus_dml(i)))/2.d0*uf*uf*10.d0)   	! difsig DmLambdac
difsig(8,i)=dble(1.d0/qon(14)**2*1.d0/2.d0*(tauplus_dmsp(i)*conjg(tauplus_dmsp(i))+tauminus_dmsp(i)*conjg(tauminus_dmsp(i)))/2.d0*uf*uf*10.d0)   ! difsig DmSp
difsig(9,i)=dble(1.d0/qon(14)**2*1.d0/2.d0*(tauplus_dzsz(i)*conjg(tauplus_dzsz(i))+tauminus_dzsz(i)*conjg(tauminus_dzsz(i)))/2.d0*uf*uf*10.d0)   ! difsig DzSz
difsig(10,i)=dble(1.d0/qon(15)**2*1.d0/2.d0*(tauplus_dzspp(i)*conjg(tauplus_dzspp(i))+tauminus_dzspp(i)*conjg(tauminus_dzspp(i)))/2.d0*uf*uf*10.d0)   ! difsig DzSpp



	g_pin(i)   = tauplus_pin(i)*cos(theta/2.d0)+tauminus_pin(i)*sin(theta/2.d0)
	h_pin(i)   = -z1*tauplus_pin(i)*sin(theta/2.d0)+z1*tauminus_pin(i)*cos(theta/2.d0)
	pola(1,i)= 2.d0*dble(g_pin(i)*conjg(h_pin(i)))/(g_pin(i)*conjg(g_pin(i))+h_pin(i)*conjg(h_pin(i)))          ! polarization for piN->piN
	beta(1,i)=atan2(2.d0*dimag(g_pin(i)*conjg(h_pin(i))),abs(g_pin(i))**2-abs(h_pin(i))**2)

	g_neta(i)   = tauplus_neta(i)*cos(theta/2.d0)+tauminus_neta(i)*sin(theta/2.d0)
	h_neta(i)   = -z1*tauplus_neta(i)*sin(theta/2.d0)+z1*tauminus_neta(i)*cos(theta/2.d0)
	pola(2,i)= 2.d0*dble(g_neta(i)*conjg(h_neta(i)))/(g_neta(i)*conjg(g_neta(i))+h_neta(i)*conjg(h_neta(i)))          ! polarization for piN->N Eta
	beta(2,i)=atan2(2.d0*dimag(g_neta(i)*conjg(h_neta(i))),abs(g_neta(i))**2-abs(h_neta(i))**2)

	g_kzl(i)   = tauplus_kzl(i)*dcos(theta/2.d0)+tauminus_kzl(i)*dsin(theta/2.d0)
	h_kzl(i)   = -z1*tauplus_kzl(i)*dsin(theta/2.d0)+z1*tauminus_kzl(i)*dcos(theta/2.d0)
	pola(3,i)= 2.d0*dble(g_kzl(i)*conjg(h_kzl(i)))/(g_kzl(i)*conjg(g_kzl(i))+h_kzl(i)*conjg(h_kzl(i)))          ! polarization for piN->K^0L^0
	beta(3,i)=atan2(2.d0*dimag(g_kzl(i)*conjg(h_kzl(i))),abs(g_kzl(i))**2-abs(h_kzl(i))**2)
!	beta(3,i)=2.d0*dimag(g_kzl(i)*conjg(h_kzl(i)))
!	beta(3,i)=abs(g_kzl(i))**2-abs(h_kzl(i))**2

	g_kzsz(i)=tauplus_kzsz(i)*cos(theta/2.d0)+tauminus_kzsz(i)*sin(theta/2.d0)
	h_kzsz(i)=-z1*tauplus_kzsz(i)*sin(theta/2.d0)+z1*tauminus_kzsz(i)*cos(theta/2.d0)
	pola(4,i)=2.d0*dble(g_kzsz(i)*conjg(h_kzsz(i)))/(g_kzsz(i)*conjg(g_kzsz(i))+h_kzsz(i)*conjg(h_kzsz(i)))		! polarization for piN->K^0S^0
	beta(4,i)=atan2(2.d0*dimag(g_kzsz(i)*conjg(h_kzsz(i))),abs(g_kzsz(i))**2-abs(h_kzsz(i))**2)

	g_kpsm(i)=tauplus_kpsm(i)*cos(theta/2.d0)+tauminus_kpsm(i)*sin(theta/2.d0)
	h_kpsm(i)=-z1*tauplus_kpsm(i)*sin(theta/2.d0)+z1*tauminus_kpsm(i)*cos(theta/2.d0)
	pola(5,i)=2.d0*dble(g_kpsm(i)*conjg(h_kpsm(i)))/(g_kpsm(i)*conjg(g_kpsm(i))+h_kpsm(i)*conjg(h_kpsm(i)))		! polarization for piN->K^+S^-
	beta(5,i)=atan2(2.d0*dimag(g_kpsm(i)*conjg(h_kpsm(i))),abs(g_kpsm(i))**2-abs(h_kpsm(i))**2)

	g_kpsp(i)=tauplus_kpsp(i)*cos(theta/2.d0)+tauminus_kpsp(i)*sin(theta/2.d0)
	h_kpsp(i)=-z1*tauplus_kpsp(i)*sin(theta/2.d0)+z1*tauminus_kpsp(i)*cos(theta/2.d0)
	pola(6,i)=2.d0*dble(g_kpsp(i)*conjg(h_kpsp(i)))/(g_kpsp(i)*conjg(g_kpsp(i))+h_kpsp(i)*conjg(h_kpsp(i)))		! polarization for piN->K^+S^+
	beta(6,i)=atan2(2.d0*dimag(g_kpsp(i)*conjg(h_kpsp(i))),abs(g_kpsp(i))**2-abs(h_kpsp(i))**2)


!! new
	g_dml(i)   = tauplus_dml(i)*dcos(theta/2.d0)+tauminus_dml(i)*dsin(theta/2.d0)
	h_dml(i)   = -z1*tauplus_dml(i)*dsin(theta/2.d0)+z1*tauminus_dml(i)*dcos(theta/2.d0)
	pola(7,i)= 2.d0*dble(g_dml(i)*conjg(h_dml(i)))/(g_dml(i)*conjg(g_dml(i))+h_dml(i)*conjg(h_dml(i)))          ! polarization for piN->K^0L^0
	beta(7,i)=atan2(2.d0*dimag(g_dml(i)*conjg(h_dml(i))),abs(g_dml(i))**2-abs(h_dml(i))**2)

	g_dmsp(i)=tauplus_dmsp(i)*cos(theta/2.d0)+tauminus_dmsp(i)*sin(theta/2.d0)
	h_dmsp(i)=-z1*tauplus_dmsp(i)*sin(theta/2.d0)+z1*tauminus_dmsp(i)*cos(theta/2.d0)
	pola(8,i)=2.d0*dble(g_dmsp(i)*conjg(h_dmsp(i)))/(g_dmsp(i)*conjg(g_dmsp(i))+h_dmsp(i)*conjg(h_dmsp(i)))		! polarization for piN->K^0S^0
	beta(8,i)=atan2(2.d0*dimag(g_dmsp(i)*conjg(h_dmsp(i))),abs(g_dmsp(i))**2-abs(h_dmsp(i))**2)

	g_dzsz(i)=tauplus_dzsz(i)*cos(theta/2.d0)+tauminus_dzsz(i)*sin(theta/2.d0)
	h_dzsz(i)=-z1*tauplus_dzsz(i)*sin(theta/2.d0)+z1*tauminus_dzsz(i)*cos(theta/2.d0)
	pola(9,i)=2.d0*dble(g_dzsz(i)*conjg(h_dzsz(i)))/(g_dzsz(i)*conjg(g_dzsz(i))+h_dzsz(i)*conjg(h_dzsz(i)))		! polarization for piN->K^+S^-
	beta(9,i)=atan2(2.d0*dimag(g_dzsz(i)*conjg(h_dzsz(i))),abs(g_dzsz(i))**2-abs(h_dzsz(i))**2)

	g_dzspp(i)=tauplus_dzspp(i)*cos(theta/2.d0)+tauminus_dzspp(i)*sin(theta/2.d0)
	h_dzspp(i)=-z1*tauplus_dzspp(i)*sin(theta/2.d0)+z1*tauminus_dzspp(i)*cos(theta/2.d0)
	pola(10,i)=2.d0*dble(g_dzspp(i)*conjg(h_dzspp(i)))/(g_dzspp(i)*conjg(g_dzspp(i))+h_dzspp(i)*conjg(h_dzspp(i)))		! polarization for piN->K^+S^+
	beta(10,i)=atan2(2.d0*dimag(g_dzspp(i)*conjg(h_dzspp(i))),abs(g_dzspp(i))**2-abs(h_dzspp(i))**2)

	end if
	end do !i

	return
	end subroutine obs_KY


	!----------------------------------------------------------------------------------------------------------

	subroutine scattering_length(scatleng,a_p,a_m)		! piN s-wave scattering length and p-wave scattering volumes;  in units of pion mass

	use input_minuit
	use input4
	use tmat4
	implicit none

	complex(kind(0.d0))	:: ez,phasfac
        real(kind(0.0d0)), dimension(4,2) :: scatleng, a_p,a_m
	integer		  ::  n1, iv,k_j,i,k

	n1=n+1
!	k=myid
k=1
	ez=e_cm(k)
        call qon_c(ez,qon) 						! on shell momenta: qon(n_c=13) is complex (initialized in "input4").
        call make_full_T(ez)

	scatleng=0.d0
	a_p =0.d0
	a_m =0.d0

	do k_j=1,2
	 do iv=1,4


      	if (k_j==1) then
	 if ((iv==1).or.(iv==3)) then		! S11 and S31

	  scatleng(iv,k_j)= -pi*dble(tshort(1,1,iv,k_j))*masses(1,1)*masses(1,2)/(masses(1,1)+masses(1,2))

	 else if ((iv==2).or.(iv==4)) then	! P11 and P31

	  scatleng(iv,k_j)= -pi*dble(tshort(1,1,iv,k_j))*masses(1,1)*masses(1,2)/(dble(qon(1))**2*(masses(1,1)+masses(1,2)))

	 end if

	else if (k_j==2) then
         if ((iv==1).or.(iv==3)) then		! P13 and P33

	  scatleng(iv,k_j)= -pi*dble(tshort(1,1,iv,k_j))*masses(1,1)*masses(1,2)/(dble(qon(1))**2*(masses(1,1)+masses(1,2)))

	 end if

      	end if


	 end do 	! iv
	end do		! k_j


	do k_j=1,2
      	 do i=1,2		! i=1 -> iv=1 and 3; i=2 -> iv=2 and 4
         a_p(i,k_j)=1.0d0/3.0d0*(scatleng(i,k_j)+2.0d0*scatleng(i+2,k_j))*masses(1,2)*1000.d0  ! isospin even; a_p=(1,1): s-wave scattering length; in units of m_pi^-1*10^-3
         a_m(i,k_j)=-1.0d0/3.0d0*(scatleng(i+2,k_j)-scatleng(i,k_j))*masses(1,2)*1000.d0	! isospin odd, ATTENTION with the minus sign!!!!!! b1= -a_m !!!!

	 end do ! i
	end do ! k_j


	do k_j=1,2
      	 do i=1,2		! i=1 -> iv=1 and 3; i=2 -> iv=2 and 4


      	  if ((k_j==1).and.(i==2)) then	! P11 and P31
		a_p(i,k_j)=a_p(i,k_j)*masses(1,2)**2/10.d0	!a_p(2,1) and a_m(2,1): p-wave scattering volume
		a_m(i,k_j)=a_m(i,k_j)*masses(1,2)**2/10.d0	! in units of m_pi^-3*10^-2

	  else if ((k_j==2).and.(i==1)) then ! P13 and P33
		a_p(i,k_j)=a_p(i,k_j)*masses(1,2)**2/10.d0	!a_p(1,2) and a_m(1,2): p-wave scattering volume
		a_m(i,k_j)=a_m(i,k_j)*masses(1,2)**2/10.d0	! in units of m_pi^-3*10^-2

	  end if

	 end do ! i
	end do ! k_j



	return
	end subroutine scattering_length

!-----------------------------------------------------------------------------------------------------------------------

	subroutine scattering_length_other_channels(scatleng)		! s-wave scattering length (no isospin combinations) in units of pion mass
									! no transition channels, only KLam--> KLam etc. -> ic1=ic2=ic
	use input_minuit
	use input4
	use tmat4
	implicit none

	complex(kind(0.d0))	:: ez,phasfac
        complex(kind(0.0d0)), dimension(13,4,5)	:: scatleng
	integer		  ::  n1, iv, k_j, k, ic

	n1=n+1
!	k=myid


!        check this!!!!!
!	ez=e_cm(k)
        call qon_c(ez,qon) 						! on shell momenta: qon(n_c=13) is complex (initialized in "input4").
	call ms_c_def

	scatleng=0.d0

	do ic=1,13
	 do iv=1,4	! only s-wave
	  do k_j=1,1	! only s-wave

	  scatleng(ic,iv,k_j)= -pi*tshort(ic,ic,iv,k_j)*ms_f_c(ic)*ms_b_c(ic)/(ms_f_c(ic)+ms_b_c(ic))*197.3d0	! scatleng in fm. No dble because not only piN-->piN

	  end do
	 end do
	end do


	return
	end subroutine scattering_length_other_channels

!----------------------------------------------------------------------------------------------------------------------

	subroutine make_full_T(ecm)	! CALL ONLY FOR myid/=0 ! Note: after this, one has the bare vertices at one's disposal
	use input_minuit
	use input4
	use tmat4
!        use mpi				!parallel
	implicit none
	complex(kind(0.d0))::	ecm
	integer :: iv,k_j,e_m,i_q2,n1,i_c1,i_c2,k, read_it_save

	n1=n+1

      read_it_save=0
      if (readit==1) then
	read_it_save=readit
	readit=0		! Allows tenporarily to access normal TNP in subroutine tmat, in order to
				! calculate TNP at nucleon position (not saved in the tables).
      end if

!	write(0,*)'g_bare(2,1,1,1)=',g_bare(2,1,1,1)
!	write(0,*)'m_bare(2,1,1)=',m_bare(2,1,1)
!        call rnm(2,1)  						! renormalize couplings and masses for nucleon pole
!	write(0,*)'g_bare(2,1,1,1)=',g_bare(2,1,1,1)
!	write(0,*)'m_bare(2,1,1)=',m_bare(2,1,1)

     readit=read_it_save	! After renormalization is finished, set readit as defined in the beginning.

      readit=1
!	ecm=e_cm(k)
        do k_j=k_b,k_e
          j1=2*k_j - 1              				! j1=2*j_physical  ! declared in input
          call rot_mat(j1)          				! input xg,wg,djw  ! util.f90/rot_mat  ! dmat3.f90
          call set_coeff_jls(j1)
	  zrem=ecm
	  call mesh_q

	  call tmat(ecm,1,4,k_j)				! arguments: tmat(ez,ivb,ive,k_j) UPDATE IF MORE THAN ISOSPIN 3/2 IS FITTED !
          do iv=1,4    !1,4						! UPDATE IF MORE THAN ISOSPIN 3/2 IS FITTED !
	       call g_res_calc(ecm,iv,k_j)  		! bare vertices
	    call t_fs_calc(ecm,iv,k_j)     			! dressed vertices, self energy
            call tmat_s_calc(ecm,iv,k_j)   			! t_pole
            call t_add(iv,k_j)            			! t=t_np+t_p
	    do i_c1=1,n_c
	    do i_c2=1,n_c
	    do i_q2=1,n1
	    T_transport(i_q2,i_c1,i_c2,iv,k_j)=tmatrix(i_q2,n1,iv,i_c1,i_c2)
            tshort(i_c1,i_c2,iv,k_j)=tmatrix(n1,n1,iv,i_c1,i_c2)
	    end do
	    end do
	    end do
	  end do 						! iv
	end do							! k_j
	return
	end subroutine make_full_T
