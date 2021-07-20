	subroutine	read_data
	use input_minuit
	implicit none
	integer 	 :: i, read_dsdo_kl, read_dsdo_kzsz, read_dsdo_kpsm, read_pola_kl, read_pola_kzsz, read_pola_kpsm, &
			    & read_beta_kl
	real(kind(0.d0)) :: tmp

	read_dsdo_kl   =1
	read_dsdo_kzsz =1
	read_dsdo_kpsm =1
	read_pola_kl   =1
	read_pola_kzsz =1
	read_pola_kpsm =1
	read_beta_kl   =1


	!=======================================================================
	!
	! TOTAL CROSS SECTIONS piN --> KY , piN --> eta N
	!
	!=======================================================================


 	! Read Total cross section piN--> eta N
! 	open(99,file='data/etaN_total_cs/etaN_ordered.dat')
 	open(99,file='data/etaN_total_cs/cs_sqs_piNetaN_good_SAID_DB.dat')
 	do i=1,63
 	read(99,*)etaN_data(i,1),etaN_data(i,2),etaN_data(i,3)
	! Total cs, (1): sqs [MeV], 2: sigma [mb], 3: dsigma [mb]
 	end do
 	close(99)


	! Read Total cross section piN-->KLambda
	open(99,file='data/KY_total_cs/kzl_ordered.dat')
	do i=1,66
	read(99,*)kzl_data(i,1),kzl_data(i,2),kzl_data(i,3)
	! Total cs, (1): sqs [MeV], 2: sigma [mb], 3: dsigma [mb]
	end do
	close(99)

	! Read Total cross section pi-p -->K0Sigma0
	open(99,file='data/KY_total_cs/kzsz_ordered.dat')
	do i=1,19
	read(99,*)kzsz_data(i,1),kzsz_data(i,2),kzsz_data(i,3)
	! Total cs, (1): sqs [MeV], 2: sigma [mb], 3: dsigma [mb]
	end do
	close(99)

	! Read Total cross section pi^-p --> K^+ Sigma^-
	open(99,file='data/KY_total_cs/kpsm_ordered.dat')
	do i=1,29
	read(99,*)kpsm_data(i,1),kpsm_data(i,2),kpsm_data(i,3)
	! Total cs, (1): sqs [MeV], 2: sigma [mb], 3: dsigma [mb]
	end do
	close(99)

	! Read Total cross section pi^+p --> K^+ Sigma^+
	open(99,file='data/KY_total_cs/kpsp_ordered.dat')
	do i=1,36
	read(99,*)kpsp_data(i,1),kpsp_data(i,2),kpsp_data(i,3)
	! Total cs, (1): sqs [MeV], 2: sigma [mb], 3: dsigma [mb]
	end do
	close(99)

	!=======================================================================
	!
	! DIFFERENTIAL CROSS SECTIONS , piN --> eta N, piN --> KY
	!
	!=======================================================================
	!=======================================================================
	!
	! Read differential cross section piN--> eta N
	!
	! (1): cos(theta)=-1,1, (2): dsig/domega [mb], (3): ddsig/domega
	!=======================================================================

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Parkhov05_Z=1489,19.txt')
	do i=1,12
	read(99,*)etaN_dif_1489(i,1),etaN_dif_1489(i,2),etaN_dif_1489(i,3)
	etaN_dif_1489(i,1)=acos(etaN_dif_1489(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Parkhov05_Z=1492,9.txt')
	do i=1,12
	read(99,*)etaN_dif_1492(i,1),etaN_dif_1492(i,2),etaN_dif_1492(i,3)
	etaN_dif_1492(i,1)=acos(etaN_dif_1492(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Bayadilov_EurPhysJA35_2008_Z=1496.81_mb.txt')
	do i=1,10
	read(99,*)etaN_dif_1496(i,1),etaN_dif_1496(i,2),etaN_dif_1496(i,3)
	etaN_dif_1496(i,1)=acos(etaN_dif_1496(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Morrison2000_Z=1498.txt')
	do i=1,9
	read(99,*)etaN_dif_1498(i,1),etaN_dif_1498(i,2),etaN_dif_1498(i,3)
	etaN_dif_1498(i,1)=acos(etaN_dif_1498(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Kozlenko_2001_Z=1499.89_mb.txt')
	do i=1,19
	read(99,*)etaN_dif_1499K(i,1),etaN_dif_1499K(i,2),etaN_dif_1499K(i,3)
	etaN_dif_1499K(i,1)=acos(etaN_dif_1499K(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Debenham75_Z=1499.1.txt')
	do i=1,3
	read(99,*)etaN_dif_1499D(i,1),etaN_dif_1499D(i,2),etaN_dif_1499D(i,3)
	etaN_dif_1499D(i,1)=acos(etaN_dif_1499D(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Parkhov05_Z=1499,67.txt')
	do i=1,12
	read(99,*)etaN_dif_1499(i,1),etaN_dif_1499(i,2),etaN_dif_1499(i,3)
	etaN_dif_1499(i,1)=acos(etaN_dif_1499(i,1))
	end do
	close(99)

 	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Bayadilov_EurPhysJA35_2008_Z=1502.95_mb.txt')
 	do i=1,10
 	read(99,*)etaN_dif_1502(i,1),etaN_dif_1502(i,2),etaN_dif_1502(i,3)
	etaN_dif_1502(i,1)=acos(etaN_dif_1502(i,1))
 	end do
 	close(99)

 	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Richards70_Z=1507,16.txt')
 	do i=1,10
 	read(99,*)etaN_dif_1507R(i,1),etaN_dif_1507R(i,2),etaN_dif_1507R(i,3)
	etaN_dif_1507R(i,1)=acos(etaN_dif_1507R(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Deinet69_Z=1507,09.txt')
	do i=1,15
	read(99,*)etaN_dif_1507(i,1),etaN_dif_1507(i,2),etaN_dif_1507(i,3)
	etaN_dif_1507(i,1)=acos(etaN_dif_1507(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Debenham75_Z=1507.9.txt')
	do i=1,3
	read(99,*)etaN_dif_1507D(i,1),etaN_dif_1507D(i,2),etaN_dif_1507D(i,3)
	etaN_dif_1507D(i,1)=acos(etaN_dif_1507D(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Parkhov05_Z=1508,24.txt')
	do i=1,12
	read(99,*)etaN_dif_1508(i,1),etaN_dif_1508(i,2),etaN_dif_1508(i,3)
	etaN_dif_1508(i,1)=acos(etaN_dif_1508(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Kozlenko_2001_Z=1509.08_mb.txt')
	do i=1,20
	read(99,*)etaN_dif_1509(i,1),etaN_dif_1509(i,2),etaN_dif_1509(i,3)
	etaN_dif_1509(i,1)=acos(etaN_dif_1509(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Bayadilov_EurPhysJA35_2008_Z=1509.08_mb.txt')
	do i=1,10
	read(99,*)etaN_dif_1509B(i,1),etaN_dif_1509B(i,2),etaN_dif_1509B(i,3)
	etaN_dif_1509B(i,1)=acos(etaN_dif_1509B(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Morrison2000_Z=1512.txt')
	do i=1,10
	read(99,*)etaN_dif_1512(i,1),etaN_dif_1512(i,2),etaN_dif_1512(i,3)
	etaN_dif_1512(i,1)=acos(etaN_dif_1512(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Debenham75_Z=1512.3.txt')
	do i=1,3
	read(99,*)etaN_dif_1512D(i,1),etaN_dif_1512D(i,2),etaN_dif_1512D(i,3)
	etaN_dif_1512D(i,1)=acos(etaN_dif_1512D(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Parkhov05_Z=1513,13.txt')
	do i=1,12
	read(99,*)etaN_dif_1513(i,1),etaN_dif_1513(i,2),etaN_dif_1513(i,3)
	etaN_dif_1513(i,1)=acos(etaN_dif_1513(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Debenham75_Z=1516.7.txt')
	do i=1,3
	read(99,*)etaN_dif_1516D(i,1),etaN_dif_1516D(i,2),etaN_dif_1516D(i,3)
	etaN_dif_1516D(i,1)=acos(etaN_dif_1516D(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Parkhov05_Z=1516,78.txt')
	do i=1,12
	read(99,*)etaN_dif_1516(i,1),etaN_dif_1516(i,2),etaN_dif_1516(i,3)
	etaN_dif_1516(i,1)=acos(etaN_dif_1516(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Deinet69_Z=1522,28.txt')
	do i=1,15
	read(99,*)etaN_dif_1522(i,1),etaN_dif_1522(i,2),etaN_dif_1522(i,3)
	etaN_dif_1522(i,1)=acos(etaN_dif_1522(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Morrison2000_Z=1525.txt')
	do i=1,8
	read(99,*)etaN_dif_1525M(i,1),etaN_dif_1525M(i,2),etaN_dif_1525M(i,3)
	etaN_dif_1525M(i,1)=acos(etaN_dif_1525M(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Debenham75_Z=1525.7.txt')
	do i=1,3
	read(99,*)etaN_dif_1525D(i,1),etaN_dif_1525D(i,2),etaN_dif_1525D(i,3)
	etaN_dif_1525D(i,1)=acos(etaN_dif_1525D(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Parkhov05_Z=1525,88.txt')
	do i=1,12
	read(99,*)etaN_dif_1525(i,1),etaN_dif_1525(i,2),etaN_dif_1525(i,3)
	etaN_dif_1525(i,1)=acos(etaN_dif_1525(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Kozlenko_2001_Z=1527.31_mb.txt')
	do i=1,20
	read(99,*)etaN_dif_1527(i,1),etaN_dif_1527(i,2),etaN_dif_1527(i,3)
	etaN_dif_1527(i,1)=acos(etaN_dif_1527(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Feltesse70_Z=1530,77.txt')
	do i=1,16
	read(99,*)etaN_dif_1530(i,1),etaN_dif_1530(i,2),etaN_dif_1530(i,3)
	etaN_dif_1530(i,1)=acos(etaN_dif_1530(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Deinet69_Z=1534,38.txt')
	do i=1,16
	read(99,*)etaN_dif_1534(i,1),etaN_dif_1534(i,2),etaN_dif_1534(i,3)
	etaN_dif_1534(i,1)=acos(etaN_dif_1534(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Debenham75_Z=1534.9.txt')
	do i=1,3
	read(99,*)etaN_dif_1534D(i,1),etaN_dif_1534D(i,2),etaN_dif_1534D(i,3)
	etaN_dif_1534D(i,1)=acos(etaN_dif_1534D(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Debenham75_Z=1544.1.txt')
	do i=1,3
	read(99,*)etaN_dif_1544(i,1),etaN_dif_1544(i,2),etaN_dif_1544(i,3)
	etaN_dif_1544(i,1)=acos(etaN_dif_1544(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Richards70_Z=1545,91.txt')
	do i=1,10
	read(99,*)etaN_dif_1545(i,1),etaN_dif_1545(i,2),etaN_dif_1545(i,3)
	etaN_dif_1545(i,1)=acos(etaN_dif_1545(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Richards70_Z=1575,39.txt')
	do i=1,10
	read(99,*)etaN_dif_1575(i,1),etaN_dif_1575(i,2),etaN_dif_1575(i,3)
	etaN_dif_1575(i,1)=acos(etaN_dif_1575(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Debenham75_Z=1577.7.txt')
	do i=1,3
	read(99,*)etaN_dif_1577(i,1),etaN_dif_1577(i,2),etaN_dif_1577(i,3)
	etaN_dif_1577(i,1)=acos(etaN_dif_1577(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Deinet69_Z=1585,96.txt')
	do i=1,15
	read(99,*)etaN_dif_1585(i,1),etaN_dif_1585(i,2),etaN_dif_1585(i,3)
	etaN_dif_1585(i,1)=acos(etaN_dif_1585(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Debenham75_Z=1587.6.txt')
	do i=1,3
	read(99,*)etaN_dif_1587(i,1),etaN_dif_1587(i,2),etaN_dif_1587(i,3)
	etaN_dif_1587(i,1)=acos(etaN_dif_1587(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Debenham75_Z=1607.8.txt')
	do i=1,3
	read(99,*)etaN_dif_1607(i,1),etaN_dif_1607(i,2),etaN_dif_1607(i,3)
	etaN_dif_1607(i,1)=acos(etaN_dif_1607(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Deinet69_Z=1609,17.txt')
	do i=1,13
	read(99,*)etaN_dif_1609(i,1),etaN_dif_1609(i,2),etaN_dif_1609(i,3)
	etaN_dif_1609(i,1)=acos(etaN_dif_1609(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=1657.txt')
	do i=1,19
	read(99,*)etaN_dif_1657(i,1),etaN_dif_1657(i,2),etaN_dif_1657(i,3)
	etaN_dif_1657(i,1)=acos(etaN_dif_1657(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=1670.txt')
	do i=1,19
	read(99,*)etaN_dif_1670(i,1),etaN_dif_1670(i,2),etaN_dif_1670(i,3)
	etaN_dif_1670(i,1)=acos(etaN_dif_1670(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Richards70_Z=1674.txt')
	do i=1,10
	read(99,*)etaN_dif_1674(i,1),etaN_dif_1674(i,2),etaN_dif_1674(i,3)
	etaN_dif_1674(i,1)=acos(etaN_dif_1674(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=1686.txt')
	do i=1,20
	read(99,*)etaN_dif_1686(i,1),etaN_dif_1686(i,2),etaN_dif_1686(i,3)
	etaN_dif_1686(i,1)=acos(etaN_dif_1686(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=1714.txt')
	do i=1,18
	read(99,*)etaN_dif_1714(i,1),etaN_dif_1714(i,2),etaN_dif_1714(i,3)
	etaN_dif_1714(i,1)=acos(etaN_dif_1714(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Richards70_Z=1729.txt')
	do i=1,10
	read(99,*)etaN_dif_1729(i,1),etaN_dif_1729(i,2),etaN_dif_1729(i,3)
	etaN_dif_1729(i,1)=acos(etaN_dif_1729(i,1))
	end do
	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=1764.txt')
 	do i=1,18
 	read(99,*)etaN_dif_1764(i,1),etaN_dif_1764(i,2),etaN_dif_1764(i,3)
 	etaN_dif_1764(i,1)=acos(etaN_dif_1764(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Richards70_Z=1805.txt')
	do i=1,10
	read(99,*)etaN_dif_1805(i,1),etaN_dif_1805(i,2),etaN_dif_1805(i,3)
	etaN_dif_1805(i,1)=acos(etaN_dif_1805(i,1))
	end do
	close(99)

 	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=1818.txt')
 	do i=1,19
 	read(99,*)etaN_dif_1818(i,1),etaN_dif_1818(i,2),etaN_dif_1818(i,3)
 	etaN_dif_1818(i,1)=acos(etaN_dif_1818(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=1859.txt')
 	do i=1,20
 	read(99,*)etaN_dif_1859(i,1),etaN_dif_1859(i,2),etaN_dif_1859(i,3)
 	etaN_dif_1859(i,1)=acos(etaN_dif_1859(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Richards70_Z=1897.txt')
	do i=1,10
	read(99,*)etaN_dif_1897(i,1),etaN_dif_1897(i,2),etaN_dif_1897(i,3)
	etaN_dif_1897(i,1)=acos(etaN_dif_1897(i,1))
	end do
	close(99)

 	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=1900.txt')
 	do i=1,20
 	read(99,*)etaN_dif_1900(i,1),etaN_dif_1900(i,2),etaN_dif_1900(i,3)
 	etaN_dif_1900(i,1)=acos(etaN_dif_1900(i,1))
 	end do
 	close(99)

 	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=1932.txt')
  	do i=1,20
 	read(99,*)etaN_dif_1932(i,1),etaN_dif_1932(i,2),etaN_dif_1932(i,3)
 	etaN_dif_1932(i,1)=acos(etaN_dif_1932(i,1))
 	end do
 	close(99)

 	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=1978.txt')
 	do i=1,20
 	read(99,*)etaN_dif_1978(i,1),etaN_dif_1978(i,2),etaN_dif_1978(i,3)
 	etaN_dif_1978(i,1)=acos(etaN_dif_1978(i,1))
 	end do
 	close(99)

 	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=2019.txt')
 	do i=1,20
 	read(99,*)etaN_dif_2019(i,1),etaN_dif_2019(i,2),etaN_dif_2019(i,3)
 	etaN_dif_2019(i,1)=acos(etaN_dif_2019(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=2055.txt')
 	do i=1,20
 	read(99,*)etaN_dif_2055(i,1),etaN_dif_2055(i,2),etaN_dif_2055(i,3)
 	etaN_dif_2055(i,1)=acos(etaN_dif_2055(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=2102.txt')
 	do i=1,20
 	read(99,*)etaN_dif_2102(i,1),etaN_dif_2102(i,2),etaN_dif_2102(i,3)
 	etaN_dif_2102(i,1)=acos(etaN_dif_2102(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=2148.txt')
 	do i=1,20
 	read(99,*)etaN_dif_2148(i,1),etaN_dif_2148(i,2),etaN_dif_2148(i,3)
 	etaN_dif_2148(i,1)=acos(etaN_dif_2148(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=2183.txt')
 	do i=1,20
 	read(99,*)etaN_dif_2183(i,1),etaN_dif_2183(i,2),etaN_dif_2183(i,3)
 	etaN_dif_2183(i,1)=acos(etaN_dif_2183(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=2272.txt')
 	do i=1,19
 	read(99,*)etaN_dif_2272(i,1),etaN_dif_2272(i,2),etaN_dif_2272(i,3)
 	etaN_dif_2272(i,1)=acos(etaN_dif_2272(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/dsdo_pi-p_etaN_Brown79_Z=2453.txt')
 	do i=1,20
 	read(99,*)etaN_dif_2453(i,1),etaN_dif_2453(i,2),etaN_dif_2453(i,3)
 	etaN_dif_2453(i,1)=acos(etaN_dif_2453(i,1))
 	end do
 	close(99)

	!===========================================================================
	!
	! Read differential cross section piN--> KLambda
	!
	! (1): cos(theta)=-1,1, (2): dsig/dOmega [mu b], (3): Delta dsig/dOmega  [mu b]
	!
	!===========================================================================

	if (read_dsdo_kl==1) then

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB141,29_Baker78_Z=1633_without_dTheta.txt')
	do i=1,5
	read(99,*)kzl_dif_1633(i,1),kzl_dif_1633(i,2),kzl_dif_1633(i,3)
	kzl_dif_1633(i,1)=acos(kzl_dif_1633(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB141,29_Baker78_Z=1661_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_dif_1661(i,1),kzl_dif_1661(i,2),kzl_dif_1661(i,3)
	kzl_dif_1661(i,1)=acos(kzl_dif_1661(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB141,29_Baker78_Z=1683_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_dif_1683(i,1),kzl_dif_1683(i,2),kzl_dif_1683(i,3)
	kzl_dif_1683(i,1)=acos(kzl_dif_1683(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB141,29_Baker78_Z=1694_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_dif_1694(i,1),kzl_dif_1694(i,2),kzl_dif_1694(i,3)
	kzl_dif_1694(i,1)=acos(kzl_dif_1694(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB141,29_Baker78_Z=1724_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_dif_1724(i,1),kzl_dif_1724(i,2),kzl_dif_1724(i,3)
	kzl_dif_1724(i,1)=acos(kzl_dif_1724(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB141,29_Baker78_Z=1758_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_dif_1758(i,1),kzl_dif_1758(i,2),kzl_dif_1758(i,3)
	kzl_dif_1758(i,1)=acos(kzl_dif_1758(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB141,29_Baker78_Z=1792_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_dif_1792(i,1),kzl_dif_1792(i,2),kzl_dif_1792(i,3)
	kzl_dif_1792(i,1)=acos(kzl_dif_1792(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB141,29_Baker78_Z=1825_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_dif_1825(i,1),kzl_dif_1825(i,2),kzl_dif_1825(i,3)
	kzl_dif_1825(i,1)=acos(kzl_dif_1825(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB141,29_Baker78_Z=1847_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_dif_1847(i,1),kzl_dif_1847(i,2),kzl_dif_1847(i,3)
	kzl_dif_1847(i,1)=acos(kzl_dif_1847(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=1879_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_dif_1879(i,1),kzl_dif_1879(i,2),kzl_dif_1879(i,3)
	kzl_dif_1879(i,1)=acos(kzl_dif_1879(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=1909_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_dif_1909(i,1),kzl_dif_1909(i,2),kzl_dif_1909(i,3)
	kzl_dif_1909(i,1)=acos(kzl_dif_1909(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=1938_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_dif_1938(i,1),kzl_dif_1938(i,2),kzl_dif_1938(i,3)
	kzl_dif_1938(i,1)=acos(kzl_dif_1938(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=1966_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_dif_1966(i,1),kzl_dif_1966(i,2),kzl_dif_1966(i,3)
	kzl_dif_1966(i,1)=acos(kzl_dif_1966(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=1999_without_dTheta.txt')
	do i=1,19
	read(99,*)kzl_dif_1999(i,1),kzl_dif_1999(i,2),kzl_dif_1999(i,3)
	kzl_dif_1999(i,1)=acos(kzl_dif_1999(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=2027_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_dif_2027(i,1),kzl_dif_2027(i,2),kzl_dif_2027(i,3)
	kzl_dif_2027(i,1)=acos(kzl_dif_2027(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=2059_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_dif_2059(i,1),kzl_dif_2059(i,2),kzl_dif_2059(i,3)
	kzl_dif_2059(i,1)=acos(kzl_dif_2059(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=2104_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_dif_2104(i,1),kzl_dif_2104(i,2),kzl_dif_2104(i,3)
	kzl_dif_2104(i,1)=acos(kzl_dif_2104(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=2159_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_dif_2159(i,1),kzl_dif_2159(i,2),kzl_dif_2159(i,3)
	kzl_dif_2159(i,1)=acos(kzl_dif_2159(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=2183_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_dif_2183(i,1),kzl_dif_2183(i,2),kzl_dif_2183(i,3)
	kzl_dif_2183(i,1)=acos(kzl_dif_2183(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=2208_without_dTheta.txt')
	do i=1,19
	read(99,*)kzl_dif_2208(i,1),kzl_dif_2208(i,2),kzl_dif_2208(i,3)
	kzl_dif_2208(i,1)=acos(kzl_dif_2208(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=2259_without_dTheta.txt')
	do i=1,19
	read(99,*)kzl_dif_2259(i,1),kzl_dif_2259(i,2),kzl_dif_2259(i,3)
	kzl_dif_2259(i,1)=acos(kzl_dif_2259(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NPB162,522_Saxon80_Z=2316_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_dif_2316(i,1),kzl_dif_2316(i,2),kzl_dif_2316(i,3)
	kzl_dif_2316(i,1)=acos(kzl_dif_2316(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR132,1778_Yoder63_Z=1934.36.txt')
	do i=1,15
	read(99,*)kzl_dif_1934(i,1),kzl_dif_1934(i,2),kzl_dif_1934(i,3)
	kzl_dif_1934(i,1)=acos(kzl_dif_1934(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR163,1430_Dahl67_Z=1930.49.txt')
	do i=1,13
	read(99,*)kzl_dif_1930(i,1),kzl_dif_1930(i,2),kzl_dif_1930(i,3)
	kzl_dif_1930(i,1)=acos(kzl_dif_1930(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR163,1430_Dahl67_Z=1978.31.txt')
	do i=1,13
	read(99,*)kzl_dif_1978(i,1),kzl_dif_1978(i,2),kzl_dif_1978(i,3)
	kzl_dif_1978(i,1)=acos(kzl_dif_1978(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR163,1430_Dahl67_Z=2025.01.txt')
	do i=1,13
	read(99,*)kzl_dif_2025(i,1),kzl_dif_2025(i,2),kzl_dif_2025(i,3)
	kzl_dif_2025(i,1)=acos(kzl_dif_2025(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR163,1430_Dahl67_Z=2097.62.txt')
	do i=1,13
	read(99,*)kzl_dif_2097(i,1),kzl_dif_2097(i,2),kzl_dif_2097(i,3)
	kzl_dif_2097(i,1)=acos(kzl_dif_2097(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR163,1430_Dahl67_Z=2137.39.txt')
	do i=1,13
	read(99,*)kzl_dif_2137(i,1),kzl_dif_2137(i,2),kzl_dif_2137(i,3)
	kzl_dif_2137(i,1)=acos(kzl_dif_2137(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR163,1430_Dahl67_Z=2180.74.txt')
	do i=1,13
	read(99,*)kzl_dif_2180(i,1),kzl_dif_2180(i,2),kzl_dif_2180(i,3)
	kzl_dif_2180(i,1)=acos(kzl_dif_2180(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR163,1430_Dahl67_Z=2244.22.txt')
	do i=1,13
	read(99,*)kzl_dif_2244(i,1),kzl_dif_2244(i,2),kzl_dif_2244(i,3)
	kzl_dif_2244(i,1)=acos(kzl_dif_2244(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR163,1430_Dahl67_Z=2305.96.txt')
	do i=1,13
	read(99,*)kzl_dif_2305(i,1),kzl_dif_2305(i,2),kzl_dif_2305(i,3)
	kzl_dif_2305(i,1)=acos(kzl_dif_2305(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR163,1430_Dahl67_Z=2405.38.txt')
	do i=1,13
	read(99,*)kzl_dif_2405(i,1),kzl_dif_2405(i,2),kzl_dif_2405(i,3)
	kzl_dif_2405(i,1)=acos(kzl_dif_2405(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR183,1134_Binford69_Z=1741.47.txt')
	do i=1,10
	read(99,*)kzl_dif_1741(i,1),kzl_dif_1741(i,2),kzl_dif_1741(i,3)
	kzl_dif_1741(i,1)=acos(kzl_dif_1741(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR183,1134_Binford69_Z=1797.81.txt')
	do i=1,10
	read(99,*)kzl_dif_1797(i,1),kzl_dif_1797(i,2),kzl_dif_1797(i,3)
	kzl_dif_1797(i,1)=acos(kzl_dif_1797(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR183,1134_Binford69_Z=1819.46.txt')
	do i=1,10
	read(99,*)kzl_dif_1819(i,1),kzl_dif_1819(i,2),kzl_dif_1819(i,3)
	kzl_dif_1819(i,1)=acos(kzl_dif_1819(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PR183,1134_Binford69_Z=1844.42.txt')
	do i=1,10
	read(99,*)kzl_dif_1844(i,1),kzl_dif_1844(i,2),kzl_dif_1844(i,3)
	kzl_dif_1844(i,1)=acos(kzl_dif_1844(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1632.txt')
	do i=1,20
	read(99,*)kzl_dif_1632(i,1),kzl_dif_1632(i,2),kzl_dif_1632(i,3)
	kzl_dif_1632(i,1)=acos(kzl_dif_1632(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1661.txt')
	do i=1,20
	read(99,*)kzl_dif_1661k(i,1),kzl_dif_1661k(i,2),kzl_dif_1661k(i,3)
	kzl_dif_1661k(i,1)=acos(kzl_dif_1661k(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1670.txt')
	do i=1,20
	read(99,*)kzl_dif_1670(i,1),kzl_dif_1670(i,2),kzl_dif_1670(i,3)
	kzl_dif_1670(i,1)=acos(kzl_dif_1670(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1676.txt')
	do i=1,20
	read(99,*)kzl_dif_1676(i,1),kzl_dif_1676(i,2),kzl_dif_1676(i,3)
	kzl_dif_1676(i,1)=acos(kzl_dif_1676(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1678.txt')
	do i=1,20
	read(99,*)kzl_dif_1678(i,1),kzl_dif_1678(i,2),kzl_dif_1678(i,3)
	kzl_dif_1678(i,1)=acos(kzl_dif_1678(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1681.txt')
	do i=1,20
	read(99,*)kzl_dif_1681(i,1),kzl_dif_1681(i,2),kzl_dif_1681(i,3)
	kzl_dif_1681(i,1)=acos(kzl_dif_1681(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1684.txt')
	do i=1,20
	read(99,*)kzl_dif_1684(i,1),kzl_dif_1684(i,2),kzl_dif_1684(i,3)
	kzl_dif_1684(i,1)=acos(kzl_dif_1684(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1686.txt')
	do i=1,20
	read(99,*)kzl_dif_1686(i,1),kzl_dif_1686(i,2),kzl_dif_1686(i,3)
	kzl_dif_1686(i,1)=acos(kzl_dif_1686(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1687.txt')
	do i=1,20
	read(99,*)kzl_dif_1687(i,1),kzl_dif_1687(i,2),kzl_dif_1687(i,3)
	kzl_dif_1687(i,1)=acos(kzl_dif_1687(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1689.txt')
	do i=1,20
	read(99,*)kzl_dif_1689(i,1),kzl_dif_1689(i,2),kzl_dif_1689(i,3)
	kzl_dif_1689(i,1)=acos(kzl_dif_1689(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1693.txt')
	do i=1,20
	read(99,*)kzl_dif_1693(i,1),kzl_dif_1693(i,2),kzl_dif_1693(i,3)
	kzl_dif_1693(i,1)=acos(kzl_dif_1693(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1698.txt')
	do i=1,20
	read(99,*)kzl_dif_1698(i,1),kzl_dif_1698(i,2),kzl_dif_1698(i,3)
	kzl_dif_1698(i,1)=acos(kzl_dif_1698(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1701.txt')
	do i=1,20
	read(99,*)kzl_dif_1701(i,1),kzl_dif_1701(i,2),kzl_dif_1701(i,3)
	kzl_dif_1701(i,1)=acos(kzl_dif_1701(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1707.txt')
	do i=1,20
	read(99,*)kzl_dif_1707(i,1),kzl_dif_1707(i,2),kzl_dif_1707(i,3)
	kzl_dif_1707(i,1)=acos(kzl_dif_1707(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRD11,1_Knasel75_Z=1743.txt')
	do i=1,20
	read(99,*)kzl_dif_1743(i,1),kzl_dif_1743(i,2),kzl_dif_1743(i,3)
	kzl_dif_1743(i,1)=acos(kzl_dif_1743(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRL8,332_Bertanza62_Z=1626.68.txt')
	do i=1,10
	read(99,*)kzl_dif_1626(i,1),kzl_dif_1626(i,2),kzl_dif_1626(i,3)
	kzl_dif_1626(i,1)=acos(kzl_dif_1626(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRL8,332_Bertanza62_Z=1648.45.txt')
	do i=1,10
	read(99,*)kzl_dif_1648(i,1),kzl_dif_1648(i,2),kzl_dif_1648(i,3)
	kzl_dif_1648(i,1)=acos(kzl_dif_1648(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_PRL8,332_Bertanza62_Z=1672.19.txt')
	do i=1,10
	read(99,*)kzl_dif_1672(i,1),kzl_dif_1672(i,2),kzl_dif_1672(i,3)
	kzl_dif_1672(i,1)=acos(kzl_dif_1672(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/dsdo_pi-p_K0Lam_NCA42,607_Goussu66_z=1973.58.txt')
	do i=1,10
	read(99,*)kzl_dif_1973(i,1),kzl_dif_1973(i,2),kzl_dif_1973(i,3)
	kzl_dif_1973(i,1)=acos(kzl_dif_1973(i,1))
	end do
	close(99)


	end if 		! read_dsdo_kl==1

	!=====================================================================================
	!
	! Read differential cross section piN--> KzSz
	!
	! cos(theta)=-1,1, dsig/dOmega [mu b], Delta dsigma/dOmega [mu b]
	!
	!=====================================================================================


	if (read_dsdo_kzsz==1) then

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB145,402_Baker78_Z=1694_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_dif_1694(i,1),kzsz_dif_1694(i,2),kzsz_dif_1694(i,3)
	kzsz_dif_1694(i,1)=acos(kzsz_dif_1694(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB145,402_Baker78_Z=1724_without_dTheta.txt')
	do i=1,10
	read(99,*)kzsz_dif_1724(i,1),kzsz_dif_1724(i,2),kzsz_dif_1724(i,3)
	kzsz_dif_1724(i,1)=acos(kzsz_dif_1724(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB145,402_Baker78_Z=1758_without_dTheta.txt')
	do i=1,10
	read(99,*)kzsz_dif_1758(i,1),kzsz_dif_1758(i,2),kzsz_dif_1758(i,3)
	kzsz_dif_1758(i,1)=acos(kzsz_dif_1758(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB145,402_Baker78_Z=1792_without_dTheta.txt')
	do i=1,10
	read(99,*)kzsz_dif_1792(i,1),kzsz_dif_1792(i,2),kzsz_dif_1792(i,3)
	kzsz_dif_1792(i,1)=acos(kzsz_dif_1792(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB145,402_Baker78_Z=1825_without_dTheta.txt')
	do i=1,10
	read(99,*)kzsz_dif_1825(i,1),kzsz_dif_1825(i,2),kzsz_dif_1825(i,3)
	kzsz_dif_1825(i,1)=acos(kzsz_dif_1825(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB145,402_Baker78_Z=1847_without_dTheta.txt')
	do i=1,10
	read(99,*)kzsz_dif_1847(i,1),kzsz_dif_1847(i,2),kzsz_dif_1847(i,3)
	kzsz_dif_1847(i,1)=acos(kzsz_dif_1847(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_1879.txt')
	do i=1,20
	read(99,*)kzsz_dif_1879(i,1),kzsz_dif_1879(i,2),kzsz_dif_1879(i,3)
	kzsz_dif_1879(i,1)=acos(kzsz_dif_1879(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_1909.txt')
	do i=1,20
	read(99,*)kzsz_dif_1909(i,1),kzsz_dif_1909(i,2),kzsz_dif_1909(i,3)
	kzsz_dif_1909(i,1)=acos(kzsz_dif_1909(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_1938.txt')
	do i=1,20
	read(99,*)kzsz_dif_1938(i,1),kzsz_dif_1938(i,2),kzsz_dif_1938(i,3)
	kzsz_dif_1938(i,1)=acos(kzsz_dif_1938(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_1966.txt')
	do i=1,20
	read(99,*)kzsz_dif_1966(i,1),kzsz_dif_1966(i,2),kzsz_dif_1966(i,3)
	kzsz_dif_1966(i,1)=acos(kzsz_dif_1966(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_1999.txt')
	do i=1,20
	read(99,*)kzsz_dif_1999(i,1),kzsz_dif_1999(i,2),kzsz_dif_1999(i,3)
	kzsz_dif_1999(i,1)=acos(kzsz_dif_1999(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_2027.txt')
	do i=1,20
	read(99,*)kzsz_dif_2027(i,1),kzsz_dif_2027(i,2),kzsz_dif_2027(i,3)
	kzsz_dif_2027(i,1)=acos(kzsz_dif_2027(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_2059.txt')
	do i=1,20
	read(99,*)kzsz_dif_2059(i,1),kzsz_dif_2059(i,2),kzsz_dif_2059(i,3)
	kzsz_dif_2059(i,1)=acos(kzsz_dif_2059(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_2104.txt')
	do i=1,20
	read(99,*)kzsz_dif_2104(i,1),kzsz_dif_2104(i,2),kzsz_dif_2104(i,3)
	kzsz_dif_2104(i,1)=acos(kzsz_dif_2104(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_2159.txt')
	do i=1,20
	read(99,*)kzsz_dif_2159(i,1),kzsz_dif_2159(i,2),kzsz_dif_2159(i,3)
	kzsz_dif_2159(i,1)=acos(kzsz_dif_2159(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_2183.txt')
	do i=1,19
	read(99,*)kzsz_dif_2183(i,1),kzsz_dif_2183(i,2),kzsz_dif_2183(i,3)
	kzsz_dif_2183(i,1)=acos(kzsz_dif_2183(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_2208.txt')
	do i=1,20
	read(99,*)kzsz_dif_2208(i,1),kzsz_dif_2208(i,2),kzsz_dif_2208(i,3)
	kzsz_dif_2208(i,1)=acos(kzsz_dif_2208(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_2259.txt')
	do i=1,20
	read(99,*)kzsz_dif_2259(i,1),kzsz_dif_2259(i,2),kzsz_dif_2259(i,3)
	kzsz_dif_2259(i,1)=acos(kzsz_dif_2259(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_NPB166,73_Hart80_2316.txt')
	do i=1,20
	read(99,*)kzsz_dif_2316(i,1),kzsz_dif_2316(i,2),kzsz_dif_2316(i,3)
	kzsz_dif_2316(i,1)=acos(kzsz_dif_2316(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR132,1778_Yoder63_Z=1934.36.txt')
	do i=1,8
	read(99,*)kzsz_dif_1934(i,1),kzsz_dif_1934(i,2),kzsz_dif_1934(i,3)
	kzsz_dif_1934(i,1)=acos(kzsz_dif_1934(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR163,1430_Dahl67_Z=1930.49.txt')
	do i=1,12
	read(99,*)kzsz_dif_1930(i,1),kzsz_dif_1930(i,2),kzsz_dif_1930(i,3)
	kzsz_dif_1930(i,1)=acos(kzsz_dif_1930(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR163,1430_Dahl67_Z=1978.31.txt')
	do i=1,12
	read(99,*)kzsz_dif_1978(i,1),kzsz_dif_1978(i,2),kzsz_dif_1978(i,3)
	kzsz_dif_1978(i,1)=acos(kzsz_dif_1978(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR163,1430_Dahl67_Z=2025.01.txt')
	do i=1,12
	read(99,*)kzsz_dif_2025(i,1),kzsz_dif_2025(i,2),kzsz_dif_2025(i,3)
	kzsz_dif_2025(i,1)=acos(kzsz_dif_2025(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR163,1430_Dahl67_Z=2097.62.txt')
	do i=1,12
	read(99,*)kzsz_dif_2097(i,1),kzsz_dif_2097(i,2),kzsz_dif_2097(i,3)
	kzsz_dif_2097(i,1)=acos(kzsz_dif_2097(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR163,1430_Dahl67_Z=2137.39.txt')
	do i=1,12
	read(99,*)kzsz_dif_2137(i,1),kzsz_dif_2137(i,2),kzsz_dif_2137(i,3)
	kzsz_dif_2137(i,1)=acos(kzsz_dif_2137(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR163,1430_Dahl67_Z=2180.74.txt')
	do i=1,12
	read(99,*)kzsz_dif_2180(i,1),kzsz_dif_2180(i,2),kzsz_dif_2180(i,3)
	kzsz_dif_2180(i,1)=acos(kzsz_dif_2180(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR163,1430_Dahl67_Z=2244.22.txt')
	do i=1,12
	read(99,*)kzsz_dif_2244(i,1),kzsz_dif_2244(i,2),kzsz_dif_2244(i,3)
	kzsz_dif_2244(i,1)=acos(kzsz_dif_2244(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR163,1430_Dahl67_Z=2305.96.txt')
	do i=1,12
	read(99,*)kzsz_dif_2305(i,1),kzsz_dif_2305(i,2),kzsz_dif_2305(i,3)
	kzsz_dif_2305(i,1)=acos(kzsz_dif_2305(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR163,1430_Dahl67_Z=2405.38.txt')
	do i=1,12
	read(99,*)kzsz_dif_2405(i,1),kzsz_dif_2405(i,2),kzsz_dif_2405(i,3)
	kzsz_dif_2405(i,1)=acos(kzsz_dif_2405(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR183,1134_Binford69_Z=1741.47.txt')
	do i=1,10
	read(99,*)kzsz_dif_1741(i,1),kzsz_dif_1741(i,2),kzsz_dif_1741(i,3)
	kzsz_dif_1741(i,1)=acos(kzsz_dif_1741(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR183,1134_Binford69_Z=1797.81.txt')
	do i=1,10
	read(99,*)kzsz_dif_1797(i,1),kzsz_dif_1797(i,2),kzsz_dif_1797(i,3)
	kzsz_dif_1797(i,1)=acos(kzsz_dif_1797(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR183,1134_Binford69_Z=1819.46.txt')
	do i=1,10
	read(99,*)kzsz_dif_1819(i,1),kzsz_dif_1819(i,2),kzsz_dif_1819(i,3)
	kzsz_dif_1819(i,1)=acos(kzsz_dif_1819(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K0Sig0_PR183,1134_Binford69_Z=1844.42.txt')
	do i=1,10
	read(99,*)kzsz_dif_1844(i,1),kzsz_dif_1844(i,2),kzsz_dif_1844(i,3)
	kzsz_dif_1844(i,1)=acos(kzsz_dif_1844(i,1))
	end do
	close(99)

	end if 	! read_dsdo_kzsz

	!=========================================================================================
	!
	! Read differential cross section piN--> K+S- [mu b]
	!
	!=========================================================================================

	if (read_dsdo_kpsm==1) then

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR165,1483_Doyle67_Z=1763.79.txt')
	do i=1,10
	read(99,*)kpsm_dif_1763(i,1),kpsm_dif_1763(i,2),kpsm_dif_1763(i,3)
	kpsm_dif_1763(i,1)=acos(kpsm_dif_1763(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR183,1142_Good69_Z=1739.86.txt')
	do i=1,10
	read(99,*)kpsm_dif_1739(i,1),kpsm_dif_1739(i,2),kpsm_dif_1739(i,3)
	kpsm_dif_1739(i,1)=acos(kpsm_dif_1739(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR183,1142_Good69_Z=1792.61.txt')
	do i=1,10
	read(99,*)kpsm_dif_1792(i,1),kpsm_dif_1792(i,2),kpsm_dif_1792(i,3)
	kpsm_dif_1792(i,1)=acos(kpsm_dif_1792(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR183,1142_Good69_Z=1818.44.txt')
	do i=1,10
	read(99,*)kpsm_dif_1818(i,1),kpsm_dif_1818(i,2),kpsm_dif_1818(i,3)
	kpsm_dif_1818(i,1)=acos(kpsm_dif_1818(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR183,1142_Good69_Z=1843.91.txt')
	do i=1,10
	read(99,*)kpsm_dif_1843(i,1),kpsm_dif_1843(i,2),kpsm_dif_1843(i,3)
	kpsm_dif_1843(i,1)=acos(kpsm_dif_1843(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR163,1430_Dahl67_Z=1930.49.txt')
	do i=1,10
	read(99,*)kpsm_dif_1930(i,1),kpsm_dif_1930(i,2),kpsm_dif_1930(i,3)
	kpsm_dif_1930(i,1)=acos(kpsm_dif_1930(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR163,1430_Dahl67_Z=1978.31.txt')
	do i=1,10
	read(99,*)kpsm_dif_1978(i,1),kpsm_dif_1978(i,2),kpsm_dif_1978(i,3)
	kpsm_dif_1978(i,1)=acos(kpsm_dif_1978(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR163,1430_Dahl67_Z=2025.01.txt')
	do i=1,10
	read(99,*)kpsm_dif_2025(i,1),kpsm_dif_2025(i,2),kpsm_dif_2025(i,3)
	kpsm_dif_2025(i,1)=acos(kpsm_dif_2025(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR163,1430_Dahl67_Z=2097.62.txt')
	do i=1,10
	read(99,*)kpsm_dif_2097(i,1),kpsm_dif_2097(i,2),kpsm_dif_2097(i,3)
	kpsm_dif_2097(i,1)=acos(kpsm_dif_2097(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR163,1430_Dahl67_Z=2137.39.txt')
	do i=1,10
	read(99,*)kpsm_dif_2137(i,1),kpsm_dif_2137(i,2),kpsm_dif_2137(i,3)
	kpsm_dif_2137(i,1)=acos(kpsm_dif_2137(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR163,1430_Dahl67_Z=2180.74.txt')
	do i=1,10
	read(99,*)kpsm_dif_2180(i,1),kpsm_dif_2180(i,2),kpsm_dif_2180(i,3)
	kpsm_dif_2180(i,1)=acos(kpsm_dif_2180(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR163,1430_Dahl67_Z=2244.22.txt')
	do i=1,10
	read(99,*)kpsm_dif_2244(i,1),kpsm_dif_2244(i,2),kpsm_dif_2244(i,3)
	kpsm_dif_2244(i,1)=acos(kpsm_dif_2244(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR163,1430_Dahl67_Z=2305.96.txt')
	do i=1,10
	read(99,*)kpsm_dif_2305(i,1),kpsm_dif_2305(i,2),kpsm_dif_2305(i,3)
	kpsm_dif_2305(i,1)=acos(kpsm_dif_2305(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_PR163,1430_Dahl67_Z=2405.38.txt')
	do i=1,10
	read(99,*)kpsm_dif_2405(i,1),kpsm_dif_2405(i,2),kpsm_dif_2405(i,3)
	kpsm_dif_2405(i,1)=acos(kpsm_dif_2405(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi-p_K+Sig-_NCA42,607_Goussu66_Z=1973.58.txt')
	do i=1,10
	read(99,*)kpsm_dif_1973(i,1),kpsm_dif_1973(i,2),kpsm_dif_1973(i,3)
	kpsm_dif_1973(i,1)=acos(kpsm_dif_1973(i,1))
	end do
	close(99)


	end if 		! read_dsdo_kpsm

	!==================================================================================
	!
	!	Differential cross section pi+p --> K+S+ [mu b]
	!
	!==================================================================================

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1700.txt')
	do i=1,10
 	read(99,*)kpsp_dif_1700(i,1),kpsp_dif_1700(i,3),kpsp_dif_1700(i,4)
 	kpsp_dif_1700(i,1)=acos(kpsp_dif_1700(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1729.txt')
	do i=1,17
 	read(99,*)kpsp_dif_1729(i,1),kpsp_dif_1729(i,3),kpsp_dif_1729(i,4)
 	kpsp_dif_1729(i,1)=acos(kpsp_dif_1729(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1732.txt')
	do i=1,9
 	read(99,*)kpsp_dif_1732(i,1),kpsp_dif_1732(i,3),kpsp_dif_1732(i,4)
 	kpsp_dif_1732(i,1)=acos(kpsp_dif_1732(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1757.txt')
	do i=1,19
 	read(99,*)kpsp_dif_1757(i,1),kpsp_dif_1757(i,3),kpsp_dif_1757(i,4)
 	kpsp_dif_1757(i,1)=acos(kpsp_dif_1757(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1764.txt')
	do i=1,20
 	read(99,*)kpsp_dif_1764(i,1),kpsp_dif_1764(i,3),kpsp_dif_1764(i,4)
 	kpsp_dif_1764(i,1)=acos(kpsp_dif_1764(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1783.txt')
	do i=1,10
 	read(99,*)kpsp_dif_1783(i,1),kpsp_dif_1783(i,3),kpsp_dif_1783(i,4)
 	kpsp_dif_1783(i,1)=acos(kpsp_dif_1783(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1789.txt')
	do i=1,20
 	read(99,*)kpsp_dif_1789(i,1),kpsp_dif_1789(i,3),kpsp_dif_1789(i,4)
 	kpsp_dif_1789(i,1)=acos(kpsp_dif_1789(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1790.txt')
	do i=1,10
 	read(99,*)kpsp_dif_1790(i,1),kpsp_dif_1790(i,3),kpsp_dif_1790(i,4)
 	kpsp_dif_1790(i,1)=acos(kpsp_dif_1790(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1813.txt')
	do i=1,10
 	read(99,*)kpsp_dif_1813(i,1),kpsp_dif_1813(i,3),kpsp_dif_1813(i,4)
 	kpsp_dif_1813(i,1)=acos(kpsp_dif_1813(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1822.txt')
	do i=1,36
	read(99,*)kpsp_dif_1822(i,1),kpsp_dif_1822(i,3),kpsp_dif_1822(i,4)
	kpsp_dif_1822(i,1)=acos(kpsp_dif_1822(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1845.txt')
	do i=1,37
	read(99,*)kpsp_dif_1845(i,1),kpsp_dif_1845(i,3),kpsp_dif_1845(i,4)
	kpsp_dif_1845(i,1)=acos(kpsp_dif_1845(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1870.txt')
	do i=1,37
	read(99,*)kpsp_dif_1870(i,1),kpsp_dif_1870(i,3),kpsp_dif_1870(i,4)
	kpsp_dif_1870(i,1)=acos(kpsp_dif_1870(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1891.txt')
	do i=1,37
	read(99,*)kpsp_dif_1891(i,1),kpsp_dif_1891(i,3),kpsp_dif_1891(i,4)
	kpsp_dif_1891(i,1)=acos(kpsp_dif_1891(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1926.txt')
	do i=1,38
	read(99,*)kpsp_dif_1926(i,1),kpsp_dif_1926(i,3),kpsp_dif_1926(i,4)
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1939.txt')
	do i=1,38
	read(99,*)kpsp_dif_1939(i,1),kpsp_dif_1939(i,3),kpsp_dif_1939(i,4)
	kpsp_dif_1939(i,1)=acos(kpsp_dif_1939(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1970.txt')
	do i=1,38
	read(99,*)kpsp_dif_1970(i,1),kpsp_dif_1970(i,3),kpsp_dif_1970(i,4)
	kpsp_dif_1970(i,1)=acos(kpsp_dif_1970(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_1985.txt')
	do i=1,38
	read(99,*)kpsp_dif_1985(i,1),kpsp_dif_1985(i,3),kpsp_dif_1985(i,4)
	kpsp_dif_1985(i,1)=acos(kpsp_dif_1985(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2019.txt')
	do i=1,38
	read(99,*)kpsp_dif_2019(i,1),kpsp_dif_2019(i,3),kpsp_dif_2019(i,4)
	kpsp_dif_2019(i,1)=acos(kpsp_dif_2019(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2031.txt')
	do i=1,38
	read(99,*)kpsp_dif_2031(i,1),kpsp_dif_2031(i,3),kpsp_dif_2031(i,4)
	kpsp_dif_2031(i,1)=acos(kpsp_dif_2031(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2059.txt')
	do i=1,39
	read(99,*)kpsp_dif_2059(i,1),kpsp_dif_2059(i,3),kpsp_dif_2059(i,4)
	kpsp_dif_2059(i,1)=acos(kpsp_dif_2059(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2074.txt')
	do i=1,39
	read(99,*)kpsp_dif_2074(i,1),kpsp_dif_2074(i,3),kpsp_dif_2074(i,4)
	kpsp_dif_2074(i,1)=acos(kpsp_dif_2074(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2106.txt')
	do i=1,39
	read(99,*)kpsp_dif_2106(i,1),kpsp_dif_2106(i,3),kpsp_dif_2106(i,4)
	kpsp_dif_2106(i,1)=acos(kpsp_dif_2106(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2118.txt')
	do i=1,39
	read(99,*)kpsp_dif_2118(i,1),kpsp_dif_2118(i,3),kpsp_dif_2118(i,4)
	kpsp_dif_2118(i,1)=acos(kpsp_dif_2118(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2147.txt')
	do i=1,39
	read(99,*)kpsp_dif_2147(i,1),kpsp_dif_2147(i,3),kpsp_dif_2147(i,4)
	kpsp_dif_2147(i,1)=acos(kpsp_dif_2147(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2158.txt')
	do i=1,39
	read(99,*)kpsp_dif_2158(i,1),kpsp_dif_2158(i,3),kpsp_dif_2158(i,4)
	kpsp_dif_2158(i,1)=acos(kpsp_dif_2158(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2188.txt')
	do i=1,39
	read(99,*)kpsp_dif_2188(i,1),kpsp_dif_2188(i,3),kpsp_dif_2188(i,4)
	kpsp_dif_2188(i,1)=acos(kpsp_dif_2188(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2202.txt')
	do i=1,39
	read(99,*)kpsp_dif_2202(i,1),kpsp_dif_2202(i,3),kpsp_dif_2202(i,4)
	kpsp_dif_2202(i,1)=acos(kpsp_dif_2202(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2224.txt')
	do i=1,39
	read(99,*)kpsp_dif_2224(i,1),kpsp_dif_2224(i,3),kpsp_dif_2224(i,4)
	kpsp_dif_2224(i,1)=acos(kpsp_dif_2224(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2243.txt')
	do i=1,39
	read(99,*)kpsp_dif_2243(i,1),kpsp_dif_2243(i,3),kpsp_dif_2243(i,4)
	kpsp_dif_2243(i,1)=acos(kpsp_dif_2243(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2261.txt')
	do i=1,39
	read(99,*)kpsp_dif_2261(i,1),kpsp_dif_2261(i,3),kpsp_dif_2261(i,4)
	kpsp_dif_2261(i,1)=acos(kpsp_dif_2261(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2282.txt')
	do i=1,39
	read(99,*)kpsp_dif_2282(i,1),kpsp_dif_2282(i,3),kpsp_dif_2282(i,4)
	kpsp_dif_2282(i,1)=acos(kpsp_dif_2282(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2304.txt')
	do i=1,39
	read(99,*)kpsp_dif_2304(i,1),kpsp_dif_2304(i,3),kpsp_dif_2304(i,4)
	kpsp_dif_2304(i,1)=acos(kpsp_dif_2304(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2318.txt')
	do i=1,39
	read(99,*)kpsp_dif_2318(i,1),kpsp_dif_2318(i,3),kpsp_dif_2318(i,4)
	kpsp_dif_2318(i,1)=acos(kpsp_dif_2318(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2341.txt')
	do i=1,39
	read(99,*)kpsp_dif_2341(i,1),kpsp_dif_2341(i,3),kpsp_dif_2341(i,4)
	kpsp_dif_2341(i,1)=acos(kpsp_dif_2341(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/dsdo_pi+p_K+Sig+_NPB226,1_Candlin83_2355.txt')
	do i=1,39
	read(99,*)kpsp_dif_2355(i,1),kpsp_dif_2355(i,3),kpsp_dif_2355(i,4)
	kpsp_dif_2355(i,1)=acos(kpsp_dif_2355(i,1))
	end do
	close(99)

!===============================================================================
!===============================================================================

	!=======================================================================
	!
	! POLARIZATION DATA  piN --> eta N
	!
	!=======================================================================

 	open(99,file='data/piN_etaN/pol_pi-p_etaN_Baker79_NPB156,93_Z=1765,18.txt')
 	do i=1,5
 	read(99,*)etaN_pol_1765(i,1),etaN_pol_1765(i,2),etaN_pol_1765(i,3)
 	etaN_pol_1765(i,1)=acos(etaN_pol_1765(i,1))
 	end do
 	close(99)

 	open(99,file='data/piN_etaN/pol_pi-p_etaN_Baker79_Z=1819,32.txt')
 	do i=1,5
 	read(99,*)etaN_pol_1819(i,1),etaN_pol_1819(i,2),etaN_pol_1819(i,3)
 	etaN_pol_1819(i,1)=acos(etaN_pol_1819(i,1))
 	end do
 	close(99)

 	open(99,file='data/piN_etaN/pol_pi-p_etaN_Baker79_Z=1860,43.txt')
 	do i=1,7
 	read(99,*)etaN_pol_1860(i,1),etaN_pol_1860(i,2),etaN_pol_1860(i,3)
 	etaN_pol_1860(i,1)=acos(etaN_pol_1860(i,1))
 	end do
 	close(99)

 	open(99,file='data/piN_etaN/pol_pi-p_etaN_Baker79_Z=1901,16.txt')
 	do i=1,6
 	read(99,*)etaN_pol_1901(i,1),etaN_pol_1901(i,2),etaN_pol_1901(i,3)
 	etaN_pol_1901(i,1)=acos(etaN_pol_1901(i,1))
 	end do
 	close(99)

 	open(99,file='data/piN_etaN/pol_pi-p_etaN_Baker79_Z=1933,83.txt')
 	do i=1,7
 	read(99,*)etaN_pol_1933(i,1),etaN_pol_1933(i,2),etaN_pol_1933(i,3)
 	etaN_pol_1933(i,1)=acos(etaN_pol_1933(i,1))
 	end do
 	close(99)

 	open(99,file='data/piN_etaN/pol_pi-p_etaN_Baker79_Z=1979,71.txt')
 	do i=1,7
 	read(99,*)etaN_pol_1979(i,1),etaN_pol_1979(i,2),etaN_pol_1979(i,3)
 	etaN_pol_1979(i,1)=acos(etaN_pol_1979(i,1))
 	end do
 	close(99)

	!--- P x dsdo--- etaN------------------------------------

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=1764_1739.txt')
 	do i=1,5
 	read(99,*)etaN_Pxdsdo_1739(i,1),etaN_Pxdsdo_1739(i,2),etaN_Pxdsdo_1739(i,3)
 	etaN_Pxdsdo_1739(i,1)=acos(etaN_Pxdsdo_1739(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=1819_1792.txt')
 	do i=1,5
 	read(99,*)etaN_Pxdsdo_1792(i,1),etaN_Pxdsdo_1792(i,2),etaN_Pxdsdo_1792(i,3)
  	etaN_Pxdsdo_1792(i,1)=acos(etaN_Pxdsdo_1792(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=1860_1832.txt')
 	do i=1,7
 	read(99,*)etaN_Pxdsdo_1832(i,1),etaN_Pxdsdo_1832(i,2),etaN_Pxdsdo_1832(i,3)
  	etaN_Pxdsdo_1832(i,1)=acos(etaN_Pxdsdo_1832(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=1900_1872.txt')
 	do i=1,6
 	read(99,*)etaN_Pxdsdo_1872(i,1),etaN_Pxdsdo_1872(i,2),etaN_Pxdsdo_1872(i,3)
  	etaN_Pxdsdo_1872(i,1)=acos(etaN_Pxdsdo_1872(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=1933_1904.txt')
 	do i=1,7
 	read(99,*)etaN_Pxdsdo_1904(i,1),etaN_Pxdsdo_1904(i,2),etaN_Pxdsdo_1904(i,3)
  	etaN_Pxdsdo_1904(i,1)=acos(etaN_Pxdsdo_1904(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=1979_1948.txt')
 	do i=1,7
 	read(99,*)etaN_Pxdsdo_1948(i,1),etaN_Pxdsdo_1948(i,2),etaN_Pxdsdo_1948(i,3)
  	etaN_Pxdsdo_1948(i,1)=acos(etaN_Pxdsdo_1948(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=2020_1988.txt')
 	do i=1,7
 	read(99,*)etaN_Pxdsdo_1988(i,1),etaN_Pxdsdo_1988(i,2),etaN_Pxdsdo_1988(i,3)
  	etaN_Pxdsdo_1988(i,1)=acos(etaN_Pxdsdo_1988(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=2056_2024.txt')
 	do i=1,7
 	read(99,*)etaN_Pxdsdo_2024(i,1),etaN_Pxdsdo_2024(i,2),etaN_Pxdsdo_2024(i,3)
  	etaN_Pxdsdo_2024(i,1)=acos(etaN_Pxdsdo_2024(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=2103_2070.txt')
 	do i=1,7
 	read(99,*)etaN_Pxdsdo_2070(i,1),etaN_Pxdsdo_2070(i,2),etaN_Pxdsdo_2070(i,3)
  	etaN_Pxdsdo_2070(i,1)=acos(etaN_Pxdsdo_2070(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=2149_2114.txt')
 	do i=1,7
 	read(99,*)etaN_Pxdsdo_2114(i,1),etaN_Pxdsdo_2114(i,2),etaN_Pxdsdo_2114(i,3)
  	etaN_Pxdsdo_2114(i,1)=acos(etaN_Pxdsdo_2114(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=2184_2148.txt')
 	do i=1,7
 	read(99,*)etaN_Pxdsdo_2148(i,1),etaN_Pxdsdo_2148(i,2),etaN_Pxdsdo_2148(i,3)
  	etaN_Pxdsdo_2148(i,1)=acos(etaN_Pxdsdo_2148(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_etaN/pola_times_dsdo_etaN_mb_Baker79_Z=2272_2235.txt')
 	do i=1,7
 	read(99,*)etaN_Pxdsdo_2235(i,1),etaN_Pxdsdo_2235(i,2),etaN_Pxdsdo_2235(i,3)
  	etaN_Pxdsdo_2235(i,1)=acos(etaN_Pxdsdo_2235(i,1))
 	end do
 	close(99)

	!=======================================================================
	!
	! POLARIZATION DATA  pi- p --> K0 Lambda
	!
	!=======================================================================

	if (read_pola_kl==1) then

 	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB141,29_Baker78_Z=1633_without_dTheta.txt')
 	do i=1,5
 	read(99,*)kzl_pol_1633(i,1),kzl_pol_1633(i,2),kzl_pol_1633(i,3)
 	kzl_pol_1633(i,1)=acos(kzl_pol_1633(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB141,29_Baker78_Z=1661_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_pol_1661(i,1),kzl_pol_1661(i,2),kzl_pol_1661(i,3)
	kzl_pol_1661(i,1)=acos(kzl_pol_1661(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB141,29_Baker78_Z=1683_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_pol_1683(i,1),kzl_pol_1683(i,2),kzl_pol_1683(i,3)
	kzl_pol_1683(i,1)=acos(kzl_pol_1683(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB141,29_Baker78_Z=1694_without_dTheta.txt')
	do i=1,10
	read(99,*)kzl_pol_1694(i,1),kzl_pol_1694(i,2),kzl_pol_1694(i,3)
	kzl_pol_1694(i,1)=acos(kzl_pol_1694(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB141,29_Baker78_Z=1724_without_dTheta.txt')
	do i=1,14
	read(99,*)kzl_pol_1724(i,1),kzl_pol_1724(i,2),kzl_pol_1724(i,3)
	kzl_pol_1724(i,1)=acos(kzl_pol_1724(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB141,29_Baker78_Z=1758_without_dTheta.txt')
	do i=1,14
	read(99,*)kzl_pol_1758(i,1),kzl_pol_1758(i,2),kzl_pol_1758(i,3)
	kzl_pol_1758(i,1)=acos(kzl_pol_1758(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB141,29_Baker78_Z=1792_without_dTheta.txt')
	do i=1,14
	read(99,*)kzl_pol_1792(i,1),kzl_pol_1792(i,2),kzl_pol_1792(i,3)
	kzl_pol_1792(i,1)=acos(kzl_pol_1792(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB141,29_Baker78_Z=1825_without_dTheta.txt')
	do i=1,14
	read(99,*)kzl_pol_1825(i,1),kzl_pol_1825(i,2),kzl_pol_1825(i,3)
	kzl_pol_1825(i,1)=acos(kzl_pol_1825(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB141,29_Baker78_Z=1847_without_dTheta.txt')
	do i=1,14
	read(99,*)kzl_pol_1847(i,1),kzl_pol_1847(i,2),kzl_pol_1847(i,3)
	kzl_pol_1847(i,1)=acos(kzl_pol_1847(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=1879_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_pol_1879(i,1),kzl_pol_1879(i,2),kzl_pol_1879(i,3)
	kzl_pol_1879(i,1)=acos(kzl_pol_1879(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=1909_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_pol_1909(i,1),kzl_pol_1909(i,2),kzl_pol_1909(i,3)
	kzl_pol_1909(i,1)=acos(kzl_pol_1909(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=1938_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_pol_1938(i,1),kzl_pol_1938(i,2),kzl_pol_1938(i,3)
	kzl_pol_1938(i,1)=acos(kzl_pol_1938(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=1966_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_pol_1966(i,1),kzl_pol_1966(i,2),kzl_pol_1966(i,3)
	kzl_pol_1966(i,1)=acos(kzl_pol_1966(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=1999_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_pol_1999(i,1),kzl_pol_1999(i,2),kzl_pol_1999(i,3)
	kzl_pol_1999(i,1)=acos(kzl_pol_1999(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=2027_without_dTheta.txt')
	do i=1,19
	read(99,*)kzl_pol_2027(i,1),kzl_pol_2027(i,2),kzl_pol_2027(i,3)
	kzl_pol_2027(i,1)=acos(kzl_pol_2027(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=2059_without_dTheta.txt')
	do i=1,18
	read(99,*)kzl_pol_2059(i,1),kzl_pol_2059(i,2),kzl_pol_2059(i,3)
	kzl_pol_2059(i,1)=acos(kzl_pol_2059(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=2104_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_pol_2104(i,1),kzl_pol_2104(i,2),kzl_pol_2104(i,3)
	kzl_pol_2104(i,1)=acos(kzl_pol_2104(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=2159_without_dTheta.txt')
	do i=1,20
	read(99,*)kzl_pol_2159(i,1),kzl_pol_2159(i,2),kzl_pol_2159(i,3)
	kzl_pol_2159(i,1)=acos(kzl_pol_2159(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=2183_without_dTheta.txt')
	do i=1,17
	read(99,*)kzl_pol_2183(i,1),kzl_pol_2183(i,2),kzl_pol_2183(i,3)
	kzl_pol_2183(i,1)=acos(kzl_pol_2183(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=2208_without_dTheta.txt')
	do i=1,19
	read(99,*)kzl_pol_2208(i,1),kzl_pol_2208(i,2),kzl_pol_2208(i,3)
	kzl_pol_2208(i,1)=acos(kzl_pol_2208(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=2259_without_dTheta.txt')
	do i=1,19
	read(99,*)kzl_pol_2259(i,1),kzl_pol_2259(i,2),kzl_pol_2259(i,3)
	kzl_pol_2259(i,1)=acos(kzl_pol_2259(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB162,522_Saxon80_Z=2316_without_dTheta.txt')
	do i=1,19
	read(99,*)kzl_pol_2316(i,1),kzl_pol_2316(i,2),kzl_pol_2316(i,3)
	kzl_pol_2316(i,1)=acos(kzl_pol_2316(i,1))
	end do
	close(99)

 	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB222,389_Bell83_Z=1851.49.txt')
	! here P(this experiment), not P(combined with older experiment)
 	do i=1,12
	read(99,*)kzl_pol_1851(i,1),kzl_pol_1851(i,2),kzl_pol_1851(i,3)
 	kzl_pol_1851(i,1)=acos(kzl_pol_1851(i,1))
 	end do
 	close(99)

 	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB222,389_Bell83_Z=1940.15.txt')
	! here P(this experiment), not P(combined)
 	do i=1,12
	read(99,*)kzl_pol_1940(i,1),kzl_pol_1940(i,2),kzl_pol_1940(i,3)
 	kzl_pol_1940(i,1)=acos(kzl_pol_1940(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB222,389_Bell83_Z=2029.63.txt')
	! here P(this experiment), not P(combined)
 	do i=1,11
	read(99,*)kzl_pol_2029(i,1),kzl_pol_2029(i,2),kzl_pol_2029(i,3)
 	kzl_pol_2029(i,1)=acos(kzl_pol_2029(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB222,389_Bell83_Z=2061.63.txt')
	! here P(this experiment), not P(combined)
 	do i=1,12
	read(99,*)kzl_pol_2061(i,1),kzl_pol_2061(i,2),kzl_pol_2061(i,3)
 	kzl_pol_2061(i,1)=acos(kzl_pol_2061(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB222,389_Bell83_Z=2106.52.txt')
	! here P(this experiment), not P(combined)
 	do i=1,12
	read(99,*)kzl_pol_2106(i,1),kzl_pol_2106(i,2),kzl_pol_2106(i,3)
 	kzl_pol_2106(i,1)=acos(kzl_pol_2106(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB222,389_Bell83_Z=2159.18.txt')
	! here P(this experiment), not P(combined)
 	do i=1,12
	read(99,*)kzl_pol_2159b(i,1),kzl_pol_2159b(i,2),kzl_pol_2159b(i,3)
 	kzl_pol_2159b(i,1)=acos(kzl_pol_2159b(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_NPB222,389_Bell83_Z=2260.85.txt')
	! here P(this experiment), not P(combined)
 	do i=1,10
	read(99,*)kzl_pol_2260(i,1),kzl_pol_2260(i,2),kzl_pol_2260(i,3)
 	kzl_pol_2260(i,1)=acos(kzl_pol_2260(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_PR183,1134_Binford69_Z=1741.47.txt')
	! here P(this experiment), not P(combined)
 	do i=1,10
	read(99,*)kzl_pol_1741(i,1),kzl_pol_1741(i,2),kzl_pol_1741(i,3)
 	kzl_pol_1741(i,1)=acos(kzl_pol_1741(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_PR183,1134_Binford69_Z=1797.81.txt')
	! here P(this experiment), not P(combined)
 	do i=1,10
	read(99,*)kzl_pol_1797(i,1),kzl_pol_1797(i,2),kzl_pol_1797(i,3)
 	kzl_pol_1797(i,1)=acos(kzl_pol_1797(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_PR183,1134_Binford69_Z=1819.46.txt')
	! here P(this experiment), not P(combined)
 	do i=1,10
	read(99,*)kzl_pol_1819(i,1),kzl_pol_1819(i,2),kzl_pol_1819(i,3)
 	kzl_pol_1819(i,1)=acos(kzl_pol_1819(i,1))
 	end do
 	close(99)

	open(99,file='data/piN_LamK/pol_pi-p_K0Lam_PR183,1134_Binford69_Z=1844.42.txt')
	! here P(this experiment), not P(combined)
 	do i=1,5
	read(99,*)kzl_pol_1844(i,1),kzl_pol_1844(i,2),kzl_pol_1844(i,3)
 	kzl_pol_1844(i,1)=acos(kzl_pol_1844(i,1))
 	end do
 	close(99)


	end if 	! read_pola_kl

	!==================================================================
	!  P times dsdo (pi- p --> K0 Lambda)
	!==================================================================

	open(99,file='data/piN_LamK/P_x_dsdo_PRL8,332_Bertanza62_Z=1626.68.txt')
	do i=1,5
	read(99,*)kzl_Pxdsdo_1626(i,1),kzl_Pxdsdo_1626(i,2),kzl_Pxdsdo_1626(i,3)  		! (1): cos(theta)=-1,1, (2): Pxdsdo  (3): d Pxdsdo
	kzl_Pxdsdo_1626(i,1)=acos(kzl_Pxdsdo_1626(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/P_x_dsdo_PRL8,332_Bertanza62_Z=1648.45.txt')
	do i=1,5
	read(99,*)kzl_Pxdsdo_1648(i,1),kzl_Pxdsdo_1648(i,2),kzl_Pxdsdo_1648(i,3)  		! (1): cos(theta)=-1,1, (2): Pxdsdo  (3): d Pxdsdo
	kzl_Pxdsdo_1648(i,1)=acos(kzl_Pxdsdo_1648(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/P_x_dsdo_PRL8,332_Bertanza62_Z=1672.19.txt')
	do i=1,5
	read(99,*)kzl_Pxdsdo_1672(i,1),kzl_Pxdsdo_1672(i,2),kzl_Pxdsdo_1672(i,3)  		! (1): cos(theta)=-1,1, (2): Pxdsdo  (3): d Pxdsdo
	kzl_Pxdsdo_1672(i,1)=acos(kzl_Pxdsdo_1672(i,1))
	end do
	close(99)


	!=======================================================================
	!
	! POLARIZATION DATA  pi- p --> K0 Sigma0
	!
	!=======================================================================

	if (read_pola_kzsz==1) then

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB145,402_Baker78_Z=1724_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1724(i,1),kzsz_pol_1724(i,2),kzsz_pol_1724(i,3)
	kzsz_pol_1724(i,1)=acos(kzsz_pol_1724(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB145,402_Baker78_Z=1758_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1758b(i,1),kzsz_pol_1758b(i,2),kzsz_pol_1758b(i,3)
	kzsz_pol_1758b(i,1)=acos(kzsz_pol_1758b(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB145,402_Baker78_Z=1792_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1792(i,1),kzsz_pol_1792(i,2),kzsz_pol_1792(i,3)
	kzsz_pol_1792(i,1)=acos(kzsz_pol_1792(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB145,402_Baker78_Z=1825_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1825(i,1),kzsz_pol_1825(i,2),kzsz_pol_1825(i,3)
	kzsz_pol_1825(i,1)=acos(kzsz_pol_1825(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB145,402_Baker78_Z=1847_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1847(i,1),kzsz_pol_1847(i,2),kzsz_pol_1847(i,3)
	kzsz_pol_1847(i,1)=acos(kzsz_pol_1847(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1694_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1694(i,1),kzsz_pol_1694(i,2),kzsz_pol_1694(i,3)
	kzsz_pol_1694(i,1)=acos(kzsz_pol_1694(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1725_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1725(i,1),kzsz_pol_1725(i,2),kzsz_pol_1725(i,3)
	kzsz_pol_1725(i,1)=acos(kzsz_pol_1725(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1758_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1758(i,1),kzsz_pol_1758(i,2),kzsz_pol_1758(i,3)
	kzsz_pol_1758(i,1)=acos(kzsz_pol_1758(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1793_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1793(i,1),kzsz_pol_1793(i,2),kzsz_pol_1793(i,3)
	kzsz_pol_1793(i,1)=acos(kzsz_pol_1793(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1826_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1826(i,1),kzsz_pol_1826(i,2),kzsz_pol_1826(i,3)
	kzsz_pol_1826(i,1)=acos(kzsz_pol_1826(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1848_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1848(i,1),kzsz_pol_1848(i,2),kzsz_pol_1848(i,3)
	kzsz_pol_1848(i,1)=acos(kzsz_pol_1848(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1879_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1879(i,1),kzsz_pol_1879(i,2),kzsz_pol_1879(i,3)
	kzsz_pol_1879(i,1)=acos(kzsz_pol_1879(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1909_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1909(i,1),kzsz_pol_1909(i,2),kzsz_pol_1909(i,3)
	kzsz_pol_1909(i,1)=acos(kzsz_pol_1909(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1938_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1938(i,1),kzsz_pol_1938(i,2),kzsz_pol_1938(i,3)
	kzsz_pol_1938(i,1)=acos(kzsz_pol_1938(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1966_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1966(i,1),kzsz_pol_1966(i,2),kzsz_pol_1966(i,3)
	kzsz_pol_1966(i,1)=acos(kzsz_pol_1966(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=1999_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_1999(i,1),kzsz_pol_1999(i,2),kzsz_pol_1999(i,3)
	kzsz_pol_1999(i,1)=acos(kzsz_pol_1999(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=2027_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_2027(i,1),kzsz_pol_2027(i,2),kzsz_pol_2027(i,3)
	kzsz_pol_2027(i,1)=acos(kzsz_pol_2027(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=2059_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_2059(i,1),kzsz_pol_2059(i,2),kzsz_pol_2059(i,3)
	kzsz_pol_2059(i,1)=acos(kzsz_pol_2059(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=2104_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_2104(i,1),kzsz_pol_2104(i,2),kzsz_pol_2104(i,3)
	kzsz_pol_2104(i,1)=acos(kzsz_pol_2104(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=2159_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_2159(i,1),kzsz_pol_2159(i,2),kzsz_pol_2159(i,3)
	kzsz_pol_2159(i,1)=acos(kzsz_pol_2159(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=2183_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_2183(i,1),kzsz_pol_2183(i,2),kzsz_pol_2183(i,3)
	kzsz_pol_2183(i,1)=acos(kzsz_pol_2183(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=2208_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_2208(i,1),kzsz_pol_2208(i,2),kzsz_pol_2208(i,3)
	kzsz_pol_2208(i,1)=acos(kzsz_pol_2208(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=2259_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_2259(i,1),kzsz_pol_2259(i,2),kzsz_pol_2259(i,3)
	kzsz_pol_2259(i,1)=acos(kzsz_pol_2259(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi-p_K0Sig0_NPB166,73_Hart80_Z=2316_without_dTheta.txt')
	do i=1,5
	read(99,*)kzsz_pol_2316(i,1),kzsz_pol_2316(i,2),kzsz_pol_2316(i,3)
	kzsz_pol_2316(i,1)=acos(kzsz_pol_2316(i,1))
	end do
	close(99)

	end if 	! read_pola_kzsz

	!=======================================================================
	!
	! POLARIZATION DATA  pi- p --> K0 Sigma0
	!
	!=======================================================================


	if (read_pola_kpsm==1) then

	open(99,file='data/piN_SigK/pol_pi-p_K+Sig-_PR177,2103_Edington69_Z=1743.txt')
	do i=1,7
	read(99,*)kpsm_pol_1743(i,1),kpsm_pol_1743(i,2),kpsm_pol_1743(i,3)
	kpsm_pol_1743(i,1)=acos(kpsm_pol_1743(i,1))
	end do
	close(99)

	end if

	!=======================================================================
	!
	! POLARIZATION DATA  pi+ p --> K+ Sigma+
	!
	!=======================================================================

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1729.txt')
	do i=1,4
	read(99,*)kpsp_pol_1729(i,1),kpsp_pol_1729(i,3),kpsp_pol_1729(i,4)
	kpsp_pol_1729(i,1)=acos(kpsp_pol_1729(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1732.txt')
	do i=1,11
	read(99,*)kpsp_pol_1732(i,1),kpsp_pol_1732(i,3),kpsp_pol_1732(i,4)
	kpsp_pol_1732(i,1)=acos(kpsp_pol_1732(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1757.txt')
	do i=1,4
	read(99,*)kpsp_pol_1757(i,1),kpsp_pol_1757(i,3),kpsp_pol_1757(i,4)
	kpsp_pol_1757(i,1)=acos(kpsp_pol_1757(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1764.txt')
	do i=1,5
	read(99,*)kpsp_pol_1764(i,1),kpsp_pol_1764(i,3),kpsp_pol_1764(i,4)
	kpsp_pol_1764(i,1)=acos(kpsp_pol_1764(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1783.txt')
	do i=1,5
	read(99,*)kpsp_pol_1783(i,1),kpsp_pol_1783(i,3),kpsp_pol_1783(i,4)
	kpsp_pol_1783(i,1)=acos(kpsp_pol_1783(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1789.txt')
	do i=1,5
	read(99,*)kpsp_pol_1789(i,1),kpsp_pol_1789(i,3),kpsp_pol_1789(i,4)
	kpsp_pol_1789(i,1)=acos(kpsp_pol_1789(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1790.txt')
	do i=1,5
	read(99,*)kpsp_pol_1790(i,1),kpsp_pol_1790(i,3),kpsp_pol_1790(i,4)
	kpsp_pol_1790(i,1)=acos(kpsp_pol_1790(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1813.txt')
	do i=1,5
	read(99,*)kpsp_pol_1813(i,1),kpsp_pol_1813(i,3),kpsp_pol_1813(i,4)
	kpsp_pol_1813(i,1)=acos(kpsp_pol_1813(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1822.txt')
	do i=1,18
	read(99,*)kpsp_pol_1822(i,1),kpsp_pol_1822(i,3),kpsp_pol_1822(i,4)
	kpsp_pol_1822(i,1)=acos(kpsp_pol_1822(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1845.txt')
	do i=1,18
	read(99,*)kpsp_pol_1845(i,1),kpsp_pol_1845(i,3),kpsp_pol_1845(i,4)
	kpsp_pol_1845(i,1)=acos(kpsp_pol_1845(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1870.txt')
	do i=1,19
	read(99,*)kpsp_pol_1870(i,1),kpsp_pol_1870(i,3),kpsp_pol_1870(i,4)
	kpsp_pol_1870(i,1)=acos(kpsp_pol_1870(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1891.txt')
	do i=1,19
	read(99,*)kpsp_pol_1891(i,1),kpsp_pol_1891(i,3),kpsp_pol_1891(i,4)
	kpsp_pol_1891(i,1)=acos(kpsp_pol_1891(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1926.txt')
	do i=1,19
	read(99,*)kpsp_pol_1926(i,1),kpsp_pol_1926(i,3),kpsp_pol_1926(i,4)
	kpsp_pol_1926(i,1)=acos(kpsp_pol_1926(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1939.txt')
	do i=1,19
	read(99,*)kpsp_pol_1939(i,1),kpsp_pol_1939(i,3),kpsp_pol_1939(i,4)
	kpsp_pol_1939(i,1)=acos(kpsp_pol_1939(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1970.txt')
	do i=1,19
	read(99,*)kpsp_pol_1970(i,1),kpsp_pol_1970(i,3),kpsp_pol_1970(i,4)
	kpsp_pol_1970(i,1)=acos(kpsp_pol_1970(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_1985.txt')
	do i=1,19
	read(99,*)kpsp_pol_1985(i,1),kpsp_pol_1985(i,3),kpsp_pol_1985(i,4)
	kpsp_pol_1985(i,1)=acos(kpsp_pol_1985(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2019.txt')
	do i=1,19
	read(99,*)kpsp_pol_2019(i,1),kpsp_pol_2019(i,3),kpsp_pol_2019(i,4)
	kpsp_pol_2019(i,1)=acos(kpsp_pol_2019(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2031.txt')
	do i=1,19
	read(99,*)kpsp_pol_2031(i,1),kpsp_pol_2031(i,3),kpsp_pol_2031(i,4)
	kpsp_pol_2031(i,1)=acos(kpsp_pol_2031(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2059.txt')
	do i=1,20
	read(99,*)kpsp_pol_2059(i,1),kpsp_pol_2059(i,3),kpsp_pol_2059(i,4)
	kpsp_pol_2059(i,1)=acos(kpsp_pol_2059(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2074.txt')
	do i=1,20
	read(99,*)kpsp_pol_2074(i,1),kpsp_pol_2074(i,3),kpsp_pol_2074(i,4)
	kpsp_pol_2074(i,1)=acos(kpsp_pol_2074(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2106.txt')
	do i=1,19
	read(99,*)kpsp_pol_2106(i,1),kpsp_pol_2106(i,3),kpsp_pol_2106(i,4)
	kpsp_pol_2106(i,1)=acos(kpsp_pol_2106(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2118.txt')
	do i=1,20
	read(99,*)kpsp_pol_2118(i,1),kpsp_pol_2118(i,3),kpsp_pol_2118(i,4)
	kpsp_pol_2118(i,1)=acos(kpsp_pol_2118(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2147.txt')
	do i=1,20
	read(99,*)kpsp_pol_2147(i,1),kpsp_pol_2147(i,3),kpsp_pol_2147(i,4)
	kpsp_pol_2147(i,1)=acos(kpsp_pol_2147(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2158.txt')
	do i=1,20
	read(99,*)kpsp_pol_2158(i,1),kpsp_pol_2158(i,3),kpsp_pol_2158(i,4)
	kpsp_pol_2158(i,1)=acos(kpsp_pol_2158(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2188.txt')
	do i=1,20
	read(99,*)kpsp_pol_2188(i,1),kpsp_pol_2188(i,3),kpsp_pol_2188(i,4)
	kpsp_pol_2188(i,1)=acos(kpsp_pol_2188(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2202.txt')
	do i=1,20
	read(99,*)kpsp_pol_2202(i,1),kpsp_pol_2202(i,3),kpsp_pol_2202(i,4)
	kpsp_pol_2202(i,1)=acos(kpsp_pol_2202(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2224.txt')
	do i=1,20
	read(99,*)kpsp_pol_2224(i,1),kpsp_pol_2224(i,3),kpsp_pol_2224(i,4)
	kpsp_pol_2224(i,1)=acos(kpsp_pol_2224(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2224.txt')
	do i=1,20
	read(99,*)kpsp_pol_2224(i,1),kpsp_pol_2224(i,3),kpsp_pol_2224(i,4)
	kpsp_pol_2224(i,1)=acos(kpsp_pol_2224(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2243.txt')
	do i=1,20
	read(99,*)kpsp_pol_2243(i,1),kpsp_pol_2243(i,3),kpsp_pol_2243(i,4)
	kpsp_pol_2243(i,1)=acos(kpsp_pol_2243(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2261.txt')
	do i=1,20
	read(99,*)kpsp_pol_2261(i,1),kpsp_pol_2261(i,3),kpsp_pol_2261(i,4)
	kpsp_pol_2261(i,1)=acos(kpsp_pol_2261(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2282.txt')
	do i=1,20
	read(99,*)kpsp_pol_2282(i,1),kpsp_pol_2282(i,3),kpsp_pol_2282(i,4)
	kpsp_pol_2282(i,1)=acos(kpsp_pol_2282(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2304.txt')
	do i=1,20
	read(99,*)kpsp_pol_2304(i,1),kpsp_pol_2304(i,3),kpsp_pol_2304(i,4)
	kpsp_pol_2304(i,1)=acos(kpsp_pol_2304(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2318.txt')
	do i=1,20
	read(99,*)kpsp_pol_2318(i,1),kpsp_pol_2318(i,3),kpsp_pol_2318(i,4)
	kpsp_pol_2318(i,1)=acos(kpsp_pol_2318(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2341.txt')
	do i=1,20
	read(99,*)kpsp_pol_2341(i,1),kpsp_pol_2341(i,3),kpsp_pol_2341(i,4)
	kpsp_pol_2341(i,1)=acos(kpsp_pol_2341(i,1))
	end do
	close(99)

	open(99,file='data/piN_SigK/pol_pi+p_K+Sig+_NPB226,1_Candlin83_2355.txt')
	do i=1,20
	read(99,*)kpsp_pol_2355(i,1),kpsp_pol_2355(i,3),kpsp_pol_2355(i,4)
	kpsp_pol_2355(i,1)=acos(kpsp_pol_2355(i,1))
	end do
	close(99)

	!==========================================================================
	!
	! spin-rotation parameter beta pi-p --> K0 Lambda (beta in rad)
	!
	!==========================================================================

	if(read_beta_kl==1) then

	open(99,file='data/piN_LamK/spinrot_rad_pi-p_K0Lam_NPB222,389_Bell83_Z=1852.38MeV.txt')
	do i=1,11
	read(99,*)kzl_beta_1852(i,1),kzl_beta_1852(i,2),kzl_beta_1852(i,3)
	kzl_beta_1852(i,1)=acos(kzl_beta_1852(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/spinrot_rad_pi-p_K0Lam_NPB222,389_Bell83_Z=1941.07MeV.txt')
	do i=1,9
	read(99,*)kzl_beta_1941(i,1),kzl_beta_1941(i,2),kzl_beta_1941(i,3)
	kzl_beta_1941(i,1)=acos(kzl_beta_1941(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/spinrot_rad_pi-p_K0Lam_NPB222,389_Bell83_Z=2030.57MeV.txt')
	do i=1,10
	read(99,*)kzl_beta_2030(i,1),kzl_beta_2030(i,2),kzl_beta_2030(i,3)
	kzl_beta_2030(i,1)=acos(kzl_beta_2030(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/spinrot_rad_pi-p_K0Lam_NPB222,389_Bell83_Z=2062.59MeV.txt')
	do i=1,11
	read(99,*)kzl_beta_2062(i,1),kzl_beta_2062(i,2),kzl_beta_2062(i,3)
	kzl_beta_2062(i,1)=acos(kzl_beta_2062(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/spinrot_rad_pi-p_K0Lam_NPB222,389_Bell83_Z=2107.49MeV.txt')
	do i=1,12
	read(99,*)kzl_beta_2107(i,1),kzl_beta_2107(i,2),kzl_beta_2107(i,3)
	kzl_beta_2107(i,1)=acos(kzl_beta_2107(i,1))
	end do
	close(99)

	open(99,file='data/piN_LamK/spinrot_rad_pi-p_K0Lam_NPB222,389_Bell83_Z=2160.16MeV.txt')
	do i=1,10
	read(99,*)kzl_beta_2160(i,1),kzl_beta_2160(i,2),kzl_beta_2160(i,3)
	kzl_beta_2160(i,1)=acos(kzl_beta_2160(i,1))
	end do
	close(99)

 	open(99,file='data/piN_LamK/spinrot_rad_pi-p_K0Lam_NPB222,389_Bell83_Z=2261.87MeV.txt')
 	do i=1,9
 	read(99,*)kzl_beta_2261(i,1),kzl_beta_2261(i,2),kzl_beta_2261(i,3)
 	kzl_beta_2261(i,1)=acos(kzl_beta_2261(i,1))
 	end do
 	close(99)

	end if 	! read_beta_kl

	!==========================================================================
	!
	! spin-rotation parameter beta pi+ p --> K+ Sigma+
	!
	!==========================================================================

	if (1==1) then

	open(99,file='data/spin-rotation-angle/k+s+/candlin88_ecm2021,34MeV.txt')
	do i=1,4
	read(99,*)kpsp_beta_2021(i,1),kpsp_beta_2021(i,2),kpsp_beta_2021(i,3)
	kpsp_beta_2021(i,1)=acos(kpsp_beta_2021(i,1))
	end do
	close(99)

	open(99,file='data/spin-rotation-angle/k+s+/candlin88_ecm2107,49MeV.txt')
	do i=1,3
	read(99,*)kpsp_beta_2107(i,1),kpsp_beta_2107(i,2),kpsp_beta_2107(i,3)
	kpsp_beta_2107(i,1)=acos(kpsp_beta_2107(i,1))
	end do
	close(99)

	end if

	!=======================================================================
	!
	! Partial waves pi+ p --> K+ Sigma+ from Candlin paper 1984
	!
	!=======================================================================

	open(99,file='data/PW_Candlin_pipp_toKpSp/S31.txt')
	do i=1,8
	read(99,*)kpsp_pw_S31(i,1),kpsp_pw_S31(i,2),kpsp_pw_S31(i,3)
	end do
	close(99)

	open(99,file='data/PW_Candlin_pipp_toKpSp/P31.txt')
	do i=1,8
	read(99,*)kpsp_pw_P31(i,1),kpsp_pw_P31(i,2),kpsp_pw_P31(i,3)
	end do
	close(99)

	open(99,file='data/PW_Candlin_pipp_toKpSp/P33.txt')
	do i=1,8
	read(99,*)kpsp_pw_P33(i,1),kpsp_pw_P33(i,2),kpsp_pw_P33(i,3)
	end do
	close(99)

	open(99,file='data/PW_Candlin_pipp_toKpSp/D33.txt')
	do i=1,8
	read(99,*)kpsp_pw_D33(i,1),kpsp_pw_D33(i,2),kpsp_pw_D33(i,3)
	end do
	close(99)

	open(99,file='data/PW_Candlin_pipp_toKpSp/D35.txt')
	do i=1,8
	read(99,*)kpsp_pw_D35(i,1),kpsp_pw_D35(i,2),kpsp_pw_D35(i,3)
	end do
	close(99)

	open(99,file='data/PW_Candlin_pipp_toKpSp/F35.txt')
	do i=1,8
	read(99,*)kpsp_pw_F35(i,1),kpsp_pw_F35(i,2),kpsp_pw_F35(i,3)
	end do
	close(99)

	open(99,file='data/PW_Candlin_pipp_toKpSp/F37.txt')
	do i=1,8
	read(99,*)kpsp_pw_F37(i,1),kpsp_pw_F37(i,2),kpsp_pw_F37(i,3)
	end do
	close(99)


	end subroutine read_data

!===============================================================================================================
!===============================================================================================================
!===============================================================================================================
!
!	Write Observables obtained after fitting: differential cross sections and polarizations for
!	pi N --> piN, etaN, KY
!
!===================================================================================================================

	subroutine write_results		! Make table for xmgrace with "data" and fit.
	use input_minuit
	use input4
	use tmat4
!        use mpi					!parallel
	implicit none
	complex(kind(0.d0)) :: ecm,ftheory,ftheory_kpsp,fsaid,phasfac,phasfacnew
	real(kind(0.d0))    ::rez, ref_e
	real(kind(0.d0)),dimension(10) :: sigma_tot
	real(kind(0.d0)),dimension(10,melab) :: sigma_tot_accu,sigma_tot_glob
	real(kind(0.d0)), dimension(50) :: thetav
	real(kind(0.d0)), dimension(10,50) :: difsig, pola, beta
	integer :: i,j,iv,k_j,n1,k
	real(kind(0.0d0)), dimension(4,2) :: scatleng, a_p,a_m

	n1=n+1

!	k=myid
!        ref_e=dble(e_cm(myid))

	!==========================================================================
	!
	! Total cross section piN --> MB
	!
	!==========================================================================



	sigma_tot_glob=0.
	sigma_tot_accu=0.

	open(99,file='plot_minuit/fit_result/total_cs/total_cs.dat')


	do k=2,melab
	  call sig_tot(e_cm(k),sigma_tot)		! Calculation of real cross section [mb]

	do i=1,10
	 sigma_tot_accu(i,k)=sigma_tot(i)
	end do

	end do !k

	!        CALL MPI_ALLREDUCE(sigma_tot_accu,sigma_tot_glob,6*melab,MPI_DOUBLE_PRECISION,MPI_SUM,fit_p_comm,ierr)
  !      sigma_tot_accu=sigma_tot_glob
  !	if (k==5) then
	 do i=4,melab
	  write(99,156)dble(e_cm(i)),sigma_tot_accu(7,i),sigma_tot_accu(8,i), &
        & sigma_tot_accu(9,i),sigma_tot_accu(10,i)
!sigma_tot_accu(1,i),sigma_tot_accu(2,i),sigma_tot_accu(3,i), &
	  	                !   & sigma_tot_accu(4,i),sigma_tot_accu(5,i),sigma_tot_accu(6,i), &
      !  & sigma_tot_accu(7,i),sigma_tot_accu(8,i),sigma_tot_accu(9,i),sigma_tot_accu(10,i)
				   ! 1: pi-p -> pi0n, 2: piN-> N eta, 3: pi-p> K0 Lambda,
				   ! 4: pi-p -> Sig0K0, 5: pi-p -> Sig-K+, 6: pi-p -> Sig+K+
	 end do
  	 close(99)
!	end if ! k==5

	if (1==0) then

	do k_j=k_b,k_e
	do iv =1,4
	sigma_tot_glob=0.
	sigma_tot_accu=0.
	call sig_tot_einzeln(e_cm(k),iv,k_j,sigma_tot)		! Calculation of real cross section [mb]
	do i=1,6
	 sigma_tot_accu(i,k)=sigma_tot(i)
	end do
!        CALL MPI_ALLREDUCE(sigma_tot_accu,sigma_tot_glob,6*melab,MPI_DOUBLE_PRECISION,MPI_SUM,fit_p_comm,ierr)
        sigma_tot_accu=sigma_tot_glob

  	if (k==5) then
	 open(99,file='plot_minuit/partial_cs_'//res(iv,k_j))
	 do i=4,melab
	  write(99,156)dble(e_cm(i)),sigma_tot_accu(1,i),sigma_tot_accu(2,i),sigma_tot_accu(3,i), &
	  	                   & sigma_tot_accu(4,i),sigma_tot_accu(5,i),sigma_tot_accu(6,i)
				   ! 1: pi-p -> pi0n, 2: piN-> N eta, 3: pi-p> K0 Lambda,
				   ! 4: pi-p -> Sig0K0, 5: pi-p -> Sig-K+, 6: pi-p -> Sig+K+
	 end do
  	 close(99)
	end if ! k==5


	end do
	end do


	end if ! Write total cs or not.



	!==========================================================================
	!
	! Differential cross section pi-p --> eta n
	!
	!==========================================================================

 	ecm   = dcmplx(1488.79d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1489.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1492.5d0,0.d0)
	! ! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1492.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1496.81d0,0.d0)
	! ! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1496.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1498.d0,0.d0)
	! ! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1498.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1499.27d0,0.d0)
	! ! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1499.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

	ecm   = dcmplx(1502.95d0,0.d0)
	! ! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1502.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1507.d0,0.d0)
	! ! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1507.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1509.d0,0.d0)
	! ! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1509.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1512.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1512.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1513.13d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1513.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1516.78d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1516.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1522.28d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1522.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1526.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1526.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

	ecm   = dcmplx(1527.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1527.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
	!end if

 	ecm   = dcmplx(1530.77d0,0.d0)
        ! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1530.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if

 	ecm   = dcmplx(1534.38d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1534.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if

!!	Brown data at low energies! (Don`t include in fit)
! 	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1542.dat')
!  	thetav=100.d0
!  	ecm   = dcmplx(1542.d0,0.d0)
!  	do i=1,50
!              thetav(i) = pi/50.d0*(50-i)
!  	end do
!   	call obs_KY(thetav,ecm,difsig,pola,beta)
!  	do i=1,50
!  	write(99,156)cos(thetav(i)),difsig(2,i)
!  	end do
!  	close(99)

 	ecm   = dcmplx(1545.00d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1545.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if

 	ecm   = dcmplx(1576.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1576.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if

 	ecm   = dcmplx(1587.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1587.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if

 	ecm   = dcmplx(1608.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1608.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if

!   	ecm   = dcmplx(1657.d0,0.d0)
	ecm   = dcmplx(1635.7d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1635.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
   	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

!   	ecm   = dcmplx(1670.d0,0.d0)
	ecm   = dcmplx(1648.2d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1648.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
   	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!!	end if

 	ecm   = dcmplx(1674.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1674.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if

!  	ecm   = dcmplx(1686.d0,0.d0)
 	ecm   = dcmplx(1664.36d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1664.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if

!   	ecm   = dcmplx(1714.d0,0.d0)
  	ecm   = dcmplx(1690.88d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
  	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1690.dat')
   	thetav=100.d0
	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
   	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

	ecm   = dcmplx(1729.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1729.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if

!  	ecm   = dcmplx(1764.d0,0.d0)
 	ecm   = dcmplx(1739.97d0,0.d0)
  	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1739.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
   	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

	ecm   = dcmplx(1805.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1805.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if

!   	ecm   = dcmplx(1818.d0,0.d0)
  	ecm   = dcmplx(1792.68d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1792.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
   	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

!   	ecm   = dcmplx(1859.d0,0.d0)
  	ecm   = dcmplx(1832.72d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1832.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
   	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

	ecm   = dcmplx(1897.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1897.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),difsig(2,i)
 	end do
 	close(99)
!	end if


! 	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1571.dat')
!  	thetav=100.d0
!  	ecm   = dcmplx(1571.d0,0.d0)
!  	do i=1,50
!              thetav(i) = pi/50.d0*(50-i)
!  	end do
!   	call obs_KY(thetav,ecm,difsig,pola,beta)
!  	do i=1,50
!  	write(99,156)cos(thetav(i)),difsig(2,i)
!  	end do
!  	close(99)



!   	ecm   = dcmplx(1900.d0,0.d0)
  	ecm   = dcmplx(1872.4d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1872.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
   	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

!  	ecm   = dcmplx(1932.d0,0.d0)
 	ecm   = dcmplx(1904.23d0,0.d0)
  	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1904.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
   	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

!  	ecm   = dcmplx(1978.d0,0.d0)
 	ecm   = dcmplx(1948.96d0,0.d0)
  	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
   	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1948.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

!  	ecm   = dcmplx(2019.d0,0.d0)
 	ecm   = dcmplx(1988.64d0,0.d0)
  	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
   	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN1988.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

!  	ecm   = dcmplx(2055.d0,0.d0)
 	ecm   = dcmplx(2024.01d0,0.d0)
  	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
   	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN2024.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

!  	ecm   = dcmplx(2102.d0,0.d0)
 	ecm   = dcmplx(2070.1d0,0.d0)
  	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
   	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN2070.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

!  	ecm   = dcmplx(2148.d0,0.d0)
 	ecm   = dcmplx(2114.35d0,0.d0)
  	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
   	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN2114.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

!  	ecm   = dcmplx(2183.d0,0.d0)
 	ecm   = dcmplx(2148.51d0,0.d0)
  	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
   	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN2148.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

! 	ecm   = dcmplx(2272.d0,0.d0)
	ecm   = dcmplx(2235.1d0,0.d0)
  	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
   	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN2235.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if

! 	ecm   = dcmplx(2453.d0,0.d0)
	ecm   = dcmplx(2412.08d0,0.d0)
  	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
   	open(99,file='plot_minuit/fit_result/etaN_differential_cs/dsdo_etaN2412.dat')
  	thetav=100.d0
  	do i=1,50
              thetav(i) = pi/50.d0*(50-i)
  	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
  	do i=1,50
  	write(99,156)cos(thetav(i)),difsig(2,i)
  	end do
  	close(99)
!	end if


!=================== pi N --> KL diff. cs ======================================================

!	write(0,*)'Now writing piN --> KL diff cs'

	ecm   = dcmplx(1633.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1633.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
!	end if

	ecm   = dcmplx(1661.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1661.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
!	! end if

	ecm   = dcmplx(1683.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1683.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1693.5d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1693.5.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(1694.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1694.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1724.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1724.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1758.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1758.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1792.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1792.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1825.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1825.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(1847.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1847.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1879.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1879.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1909.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1909.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1938.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1938.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1966.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1966.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1999.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1999.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2026.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2026.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(2027.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2027.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2059.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2059.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2104.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2104.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2159.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2159.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2181.5d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2181.5.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(2183.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2183.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2208.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2208.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2259.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2259.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2316.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2316.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1934.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1934.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1930.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1930.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1978.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1978.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(2025.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2025.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2097.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2097.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2137.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2137.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(2180.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2180.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2244.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2244.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2305.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2305.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2405.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL2405.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(1741.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1741.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1797.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1797.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1819.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1819.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1845.51d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1845.5.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(1844.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1844.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1632.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1632.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1670.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1670.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1676.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1676.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1678.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1678.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1681.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1681.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1684.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1684.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1686.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1686.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1687.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1687.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1689.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1689.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(1693.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1693.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1698.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1698.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1701.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1701.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1707.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1707.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1742.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1742.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(1743.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1743.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1626.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1626.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1648.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1648.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1672.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1672.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1973.58d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KL1973.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(3,i)*1000.d0
	end do
	close(99)
	! end if






!=================== pi N --> KzSz diff. cs ======================================================

!	write(0,*)'Now writing piN --> K0 Sigma0 diff cs'

	ecm   = dcmplx(1694.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1694.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1724.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1724.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1758.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1758.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1792.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1792.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1825.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1825.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

        ecm   = dcmplx(1845.51d0,0.d0)
        ! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
        open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1845.5.dat')
        thetav=100.d0
        do i=1,50
	     thetav(i) = pi/50.d0*(50-i)
        end do
        call obs_KY(thetav,ecm,difsig,pola,beta)
        do i=1,50
        write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
        end do
        close(99)
        ! end if

	ecm   = dcmplx(1879.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1879.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1909.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1909.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1938.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1938.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1966.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1966.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1999.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1999.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(2027.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
! 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2027.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2059.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2059.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2104.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2104.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2159.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2159.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2181.5d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2181.5.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(2183.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
! 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2183.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2208.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2208.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2259.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2259.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2316.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2316.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1934.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1934.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1930.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1930.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1978.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1978.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2026.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2026.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(2025.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
! 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2025.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2097.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2097.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2137.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2137.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(2180.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
! 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2180.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2244.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2244.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2305.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2305.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2405.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz2405.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1741.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1741.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1797.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1797.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1819.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1819.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1845.51d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1845.5.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(1844.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
! 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KzSz1844.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),difsig(4,i)*1000.d0
! 	end do
! 	close(99)
! 	! end if

!=================== pi N --> K+ Sigma- diff. cs===============

	ecm   = dcmplx(1763.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm1763.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1739.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm1739.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1792.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm1792.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1818.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm1818.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1843.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm1843.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1930.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm1930.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1978.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm1978.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2025.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm2025.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2097.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm2097.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2137.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm2137.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2180.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm2180.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2244.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm2244.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2305.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm2305.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2405.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm2405.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1973.58d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSm1973.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(5,i)*1000.d0
	end do
	close(99)
	! end if

!=================== pi N --> K+S+ diff. cs ======================================================

!	write(0,*)'Now writing piN --> K+ Sigma+ diff cs'

	ecm   = dcmplx(1700.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1700.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1729.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1729.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1732.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1732.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1757.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1757.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1764.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1764.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1783.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1783.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1789.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1789.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1790.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1790.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1813.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1813.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1822.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1822.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1845.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1845.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1870.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1870.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1891.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1891.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1926.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1926.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1939.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1939.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1970.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1970.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1985.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp1985.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2019.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2019.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2031.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2031.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2059.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2059.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2074.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2074.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2106.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2106.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2118.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2118.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2147.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2147.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2158.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2158.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2188.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2188.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2202.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2202.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2224.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2224.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2243.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2243.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2261.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2261.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2282.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2282.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2304.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2304.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2318.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2318.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2341.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2341.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(2355.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_differential_cs/dsdo_KpSp2355.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),difsig(6,i)*1000.d0
	end do
	close(99)
	! end if

!=================== pi N --> etaN polarization ======================================================

! 	write(0,*)'Now writing piN --> etaN polarization'

 	ecm   = dcmplx(1765.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
  	open(99,file='plot_minuit/fit_result/etaN_polarization/pola_etaN1765.dat')
  	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1819.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
  	open(99,file='plot_minuit/fit_result/etaN_polarization/pola_etaN1819.dat')
  	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1860.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
  	open(99,file='plot_minuit/fit_result/etaN_polarization/pola_etaN1860.dat')
  	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1901.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
  	open(99,file='plot_minuit/fit_result/etaN_polarization/pola_etaN1901.dat')
  	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1933.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
  	open(99,file='plot_minuit/fit_result/etaN_polarization/pola_etaN1933.dat')
  	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1979.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
  	open(99,file='plot_minuit/fit_result/etaN_polarization/pola_etaN1979.dat')
  	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)
 	end do
 	close(99)
	! end if

	!--------pola x dsdo-----etaN------------------------

 	ecm   = dcmplx(1739.97d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
   	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_1739.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1792.68d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_1792.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1832.72d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_1832.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1872.4d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_1872.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1904.23d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_1904.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1948.96d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
  	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_1948.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(1988.64d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
   	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_1988.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(2024.01d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_2024.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(2070.1d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_2070.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(2114.35d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_2114.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(2148.51d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_2148.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if

 	ecm   = dcmplx(2235.1d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/etaN_polarization/Pxdsdo_etaN_2235.dat')
  	thetav=100.d0
 	do i=1,50
	   thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(2,i)*difsig(2,i)
 	end do
 	close(99)
	! end if


!=================== pi N --> KL polarization ======================================================

!	write(0,*)'Now writing piN --> KL polarization'


	ecm   = dcmplx(1633.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1633.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1661.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1661.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1683.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1683.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1694.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1694.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1724.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1724.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1758.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1758.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1792.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1792.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1825.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1825.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1845.51d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1845.5.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(1847.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1847.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(3,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1879.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1879.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1909.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1909.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1939.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1939.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(1938.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1938.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(3,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1966.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1966.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1999.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1999.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(2027.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2027.dat')
!  	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(3,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2060.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2060.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(2059.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2059.dat')
!  	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(3,i)
! 	end do
! 	close(99)
! 	! end if

! 	ecm   = dcmplx(2104.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2104.dat')
!  	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(3,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2159.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2159.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2183.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2183.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2208.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2208.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2259.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2259.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2316.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2316.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1851.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1851.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(1940.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1940.dat')
!  	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(3,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2028.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2028.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(2029.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2029.dat')
!  	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(3,i)
! 	end do
! 	close(99)
! 	! end if

! 	ecm   = dcmplx(2061.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2061.dat')
!  	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(3,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2105.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2105.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(2106.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2106.dat')
!  	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(3,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(2159.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2159.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2260.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL2260.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1741.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1741.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1797.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1797.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1819.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1819.dat')
 	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(1844.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KL1844.dat')
!  	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(3,i)
! 	end do
! 	close(99)
! 	! end if



!=================== pi N --> K0 Sigma0 polarization ======================================================


!	write(0,*)'Now writing piN --> K0 Sigma0 polarization'

! 	ecm   = dcmplx(1724.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1724.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(4,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1792.5d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1792.5.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(1792.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1792.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(4,i)
! 	end do
! 	close(99)
! 	! end if
!
! 	ecm   = dcmplx(1825.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1825.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(4,i)
! 	end do
! 	close(99)
! 	! end if
!
 	ecm   = dcmplx(1847.5d0,0.d0)
 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1847.5.dat')
 	thetav=100.d0
 	do i=1,50
             thetav(i) = pi/50.d0*(50-i)
 	end do
  	call obs_KY(thetav,ecm,difsig,pola,beta)
 	do i=1,50
 	write(99,156)cos(thetav(i)),pola(4,i)
 	end do
 	close(99)
 	! end if

	ecm   = dcmplx(1694.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1694.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1724.5d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1724.5.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(1725.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
!  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1725.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(4,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1758.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1758.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

! 	ecm   = dcmplx(1793.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
! 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1793.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(4,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1825.5d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1825.5.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if


! 	ecm   = dcmplx(1826.d0,0.d0)
! 	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
! 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1826.dat')
! 	thetav=100.d0
! 	do i=1,50
!             thetav(i) = pi/50.d0*(50-i)
! 	end do
!  	call obs_KY(thetav,ecm,difsig,pola,beta)
! 	do i=1,50
! 	write(99,156)cos(thetav(i)),pola(4,i)
! 	end do
! 	close(99)
! 	! end if

	ecm   = dcmplx(1879.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1879.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1909.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1909.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1938.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1938.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1966.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1966.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1999.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz1999.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2027.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz2027.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2059.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz2059.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2104.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz2104.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2159.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz2159.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if


	ecm   = dcmplx(2183.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz2183.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2208.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz2208.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2259.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz2259.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2316.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KzSz2316.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(4,i)
	end do
	close(99)
	! end if


!=================== pi N --> K+S- polarization ======================================================

	ecm   = dcmplx(1743.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSm1743.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(5,i)
	end do
	close(99)
	! end if


!=================== pi N --> K+S+ polarization ======================================================

!	write(0,*)'Now writing piN --> K+ Sigma+ polarization'

	ecm   = dcmplx(1729.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1729.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1732.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1732.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1757.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1757.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1764.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1764.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1783.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1783.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1789.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1789.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1790.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
 	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1790.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1813.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
  	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1813.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1822.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1822.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1845.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1845.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1870.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1870.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1891.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1891.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1926.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1926.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1939.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1939.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1970.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1970.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(1985.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp1985.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2019.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2019.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2031.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2031.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2059.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2059.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2074.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2074.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2106.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2106.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2118.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2118.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2147.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2147.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2158.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2158.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2188.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2188.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2202.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2202.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2224.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2224.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2243.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2243.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2261.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2261.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2282.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2282.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2304.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2304.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2318.d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/pola_KpSp2318.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(6,i)
	end do
	close(99)
	! end if


	!==================================================================
	!  P times dsdo
	!==================================================================

	! Write P x dsdo  pi-p --> K???Lam

	ecm   = dcmplx(1626.68d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/P_x_dsdo_KzL1626.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)*difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1648.45d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/P_x_dsdo_KzL1648.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)*difsig(3,i)*1000.d0
	end do
	close(99)
	! end if

	ecm   = dcmplx(1672.19d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_polarization/P_x_dsdo_KzL1672.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),pola(3,i)*difsig(3,i)*1000.d0
	end do
	close(99)
	! end if


!========================================================================================================
!
!	spin-rotation parameter (K0 Lambda)
!
!========================================================================================================

	ecm   = dcmplx(1852.38d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_spin-rotation/beta_KzL1852.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),beta(3,i)	! beta in rad
	end do
	close(99)
	! end if

	ecm   = dcmplx(1941.07d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_spin-rotation/beta_KzL1941.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),beta(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2030.57d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_spin-rotation/beta_KzL2030.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),beta(3,i)
	end do
	close(99)
	!! end if

	ecm   = dcmplx(2062.59d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_spin-rotation/beta_KzL2062.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),beta(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2107.49d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_spin-rotation/beta_KzL2107.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),beta(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2160.16d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_spin-rotation/beta_KzL2160.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),beta(3,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2261.87d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_spin-rotation/beta_KzL2261.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),beta(3,i)
	end do
	close(99)
	! end if

	!Spin rotation parameter K+ Sigma+
	!----------------------------------------------------------------------------------------------------------

	ecm   = dcmplx(2021.34d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_spin-rotation/beta_KpSp2021.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),beta(6,i)
	end do
	close(99)
	! end if

	ecm   = dcmplx(2107.49d0,0.d0)
	! if (ref_e-lim_e(k-1) < dble(ecm) .and. dble(ecm) <= ref_e+lim_e(k)) then^
	open(99,file='plot_minuit/fit_result/KY_spin-rotation/beta_KpSp2107.dat')
	thetav=100.d0
	do i=1,50
            thetav(i) = pi/50.d0*(50-i)
	end do
 	call obs_KY(thetav,ecm,difsig,pola,beta)
	do i=1,50
	write(99,156)cos(thetav(i)),beta(6,i)
	end do
	close(99)
	! end if

!==========================================================================================================
	!=================== D- Lambdac --> D- Lambdac ======================================================

	!	write(0,*)'Now writing D- Lambdac --> D- Lambdac diff cs'
		ecm   = dcmplx(4300.d0,0.d0)

	 	open(99,file='plot_minuit/fit_result/DY_differential_cs/dsdo_DmL4300.dat')
		thetav=100.d0
		do i=1,50
	            thetav(i) = pi/50.d0*(50-i)
		end do
	 	call obs_KY(thetav,ecm,difsig,pola,beta)
		do i=1,50
		write(99,156)cos(thetav(i)),difsig(7,i)*1000.d0
		end do
		close(99)

	!=================== D- Lambdac --> D- Sigmac+ ======================================================

	!	write(0,*)'Now writing D- Lambdac --> D- Sigmac+ diff cs'
		ecm   = dcmplx(4500.d0,0.d0)

	 	open(99,file='plot_minuit/fit_result/DY_differential_cs/dsdo_DmSp4500.dat')
		thetav=100.d0
		do i=1,50
	            thetav(i) = pi/50.d0*(50-i)
		end do
		call obs_KY(thetav,ecm,difsig,pola,beta)
		do i=1,50
		write(99,156)cos(thetav(i)),difsig(8,i)*1000.d0
		end do
		close(99)

	!=================== D- Lambdac --> D0 Sigmac0 ======================================================

	!	write(0,*)'Now writing D- Lambdac --> D0 Sigmac0 diff cs'
		ecm   = dcmplx(4500.d0,0.d0)

		open(99,file='plot_minuit/fit_result/DY_differential_cs/dsdo_DzSz4500.dat')
		thetav=100.d0
		do i=1,50
			        thetav(i) = pi/50.d0*(50-i)
		end do
		call obs_KY(thetav,ecm,difsig,pola,beta)
		do i=1,50
		write(99,156)cos(thetav(i)),difsig(9,i)*1000.d0
		end do
		close(99)

	!=================== D0 Sigmac++ --> D0 Sigmac++ ======================================================

	!	write(0,*)'Now writing D0 Sigmac++ --> D0 Sigmac++ diff cs'
		ecm   = dcmplx(4500.d0,0.d0)

		open(99,file='plot_minuit/fit_result/DY_differential_cs/dsdo_DzSpp4500.dat')
		thetav=100.d0
		do i=1,50
			        thetav(i) = pi/50.d0*(50-i)
		end do
		call obs_KY(thetav,ecm,difsig,pola,beta)
		do i=1,50
		write(99,156)cos(thetav(i)),difsig(10,i)*1000.d0
		end do
		close(99)


!==========================================================================================================


	! Scattering length and volumes
	!------------------------------------------------------------------------------------------------------------


!	if (k==4) then

	open(99,file='scattering_length/scattering_length_and_volumes.dat')

	call scattering_length(scatleng,a_p,a_m)

!	do k_j=1,2
!     	 do i=1,2
	 		! i=1 -> iv=1 and 3; i=2 -> iv=2 and 4

!	  if ((k_j==1).and.(i==1)) then ! S-wave; in units of m_pi^-1*10^-3

	   write(99,156)'','Our fit','ChPT'
	   write(99,156)''
	   write(99,156)'a^+_0+    |', a_p(1,1) , 7.6d0
	   write(99,156)''
	   write(99,156)'a^-_0+    |', a_m(1,1) , 86.1d0
	   write(99,156)''
!	   else if ((k_j==1).and.(i==2)) then ! P11 and P31; in units of m_pi^-3*10^-2
	   write(99,156)'a^+_1-    |', a_p(2,1) , -5.52d0
	   write(99,156)''
	   write(99,156)'a^-_1-    |', a_m(2,1) , -1.36d0
	   write(99,156)''
!	   else if ((k_j==2).and.(i==1)) then ! P13 and P33; in units of m_pi^-3*10^-2
	   write(99,156)'a^+_1+    |', a_p(1,2) , 13.97d0
	   write(99,156)
	   write(99,156)'a^-_1+    |', a_m(1,2) , -8.44d0

!	  end if


!	 end do
!	end do

	close(99)

!	end if  ! k=4



!========================================================================================================================

 156    format(2(15g15.6))
	end subroutine write_results
