	module cont_unstable
	 use input4
	 use input_cont
	 	implicit none
	 contains

	!------------------------------------------
	! Sigma N propagator
	!------------------------------------------
	
	! Sigma pipi vertex
	function		versi(q,k)
	implicit		none
	complex(kind(0.0d0))::	versi,q,k
	versi=sqrt(3.d0)*gspipi*mmpi/(2.d0*pi*sqrt(sqrt(k**2+msb**2)) & 
     & *sqrt(q**2+mmpi**2))*lamsi**2/(lamsi**2+q**2)
	return
	end function
	
	! pipi loop, integrand
	function		sisinte(q,k,z,eps)
	implicit		none
	complex(kind(0.0d0))::	sisinte,q,k,z
	real(kind(0.0d0))::	eps
	sisinte=q**2*versi(q,k)**2/(z-2.d0*sqrt(q**2+mmpi**2)+ci*eps)
	return
	end function
	
	! qcm with real part in the right q half plane
	function 		qcmright(z,m1,m2)
	complex(kind(0.0d0))::	qcmright,z,tmp
	real(kind(0.0d0))::	m1,m2
	tmp=1.d0/(2.d0*z)*sqrt((z**2-(m1-m2)**2)*(z**2-(m1+m2)**2))
	if (dble(tmp).ge.0.d0) then
	qcmright=tmp
	else
	qcmright=-tmp
	end if	
	return
	end function
	
	! Simplification for qcm(mmpi,mmpi)
	function		qcmri(z)
	implicit		none
	complex(kind(0.0d0))::	qcmri,z
	qcmri=qcmright(z,mmpi,mmpi)
	return
	end function
	
	! A factor that appears in the numerical trick of integrating the pipi loop.
	function 		facsi(q,k,z)
	implicit 		none
	complex(kind(0.0d0))::	facsi,q,k,z
	facsi=q**2*versi(q,k)**2*(mmpi**2+q**2)/(q*z)
	return
	end function
	
	! Smoothened integrand (divergence subtracted)
	function		sisismooth(q,k,z,eps)
	implicit		none
	complex(kind(0.0d0))::	sisismooth,q,k,z
	real(kind(0.0d0))::	eps
	sisismooth=sisinte(q,k,z,eps) &
     &	        +facsi(qcmri(z+ci*eps),k,z+ci*eps)/(q-qcmri(z+ci*eps))	
	return
	end function
	
	! Analytic expression of integral over divergence
	function 		explsi(k,z,lala)
	implicit		none
	complex(kind(0.0d0))::	explsi,k,z
	real(kind(0.0d0))::	lala
	explsi=facsi(qcmri(z),k,z)*log((qcmri(z)-lala)/qcmri(z))
	return
	end function
	
	! Sigma self energy, evaluated with numerical trick
	function		sesigma(k,z,eps)
	implicit 		none
	real(kind(0.0d0))::	q,eps,low,up,lammsi
	real(kind(0.0d0)),dimension(1000):: xx
	complex(kind(0.0d0))::	sesigma,k,z,inte1,inte2,qcom
     	complex(kind(0.0d0)),dimension(1000) :: df 		
	integer			np,kk
	lammsi=dble(qcmri(z))+300.d0
	low=0.d0	! lower integration boundary
	up =lammsi	! uppper integration boundary
	call dsg20R(low,up,nin,xx,np) 	! xx (gauss points) and np (index of gauss points) are output.     
        do kk=1,np			! fill now the function values. Store in "df". np = 20 times n
	q=xx(kk)
	qcom=dcmplx(q,0.d0)
	df(kk)=sisismooth(qcom,k,z,eps)
	end do
        call drg20c(low,up,nin,df,inte1) ! Sum Gauss 
	low=lammsi
	up =10000.d0
	call dsg20R(low,up,nin,xx,np)    
        do kk=1,np		
	q=xx(kk)
	qcom=dcmplx(q,0.d0)
	df(kk)=sisinte(qcom,k,z,eps)
	end do
        call drg20c(low,up,nin,df,inte2) ! Sum Gauss 
	sesigma=inte1-explsi(k,z+ci*eps,lammsi)+inte2
	return
	end function
	
	! qcm, im part negative choosen
	function 		qcmneg(z,m1,m2)	! qcm, negative sheet.
	complex(kind(0.0d0))::	qcmneg,z,tmp
	real(kind(0.0d0))::	m1,m2
	tmp=1.d0/(2.d0*z)*sqrt((z**2-(m1-m2)**2)*(z**2-(m1+m2)**2))
	if (imag(tmp).lt.0.d0) then
	qcmneg=tmp
	else
	qcmneg=-tmp
	end if
	return
	end function
	
	! Explicit discontinuity of Sigma self energy
	function 		explsigmaneg(z,k)
	implicit		none	
	complex(kind(0.0d0))::	explsigmaneg,z,k,qcmnega
	qcmnega=qcmneg(z,mmpi,mmpi)
	explsigmaneg=-pi*ci*qcmnega*(qcmnega**2+mmpi**2)/z &
     &		    *versi(qcmnega,k)**2
	return
	end function
	
	! Sigma self energy with one quarter of second sheet
	function		consig(k,z,eps)
	implicit		none
	complex(kind(0.0d0))::	consig,k,z
	real(kind(0.0d0))::	eps
	if ((dble(z).ge.2.d0*mmpi).and.(imag(z).lt.0.d0)) then
	consig=sesigma(k,z,eps)+2.d0*explsigmaneg(z,k)
	else
	consig=sesigma(k,z,eps)
	end if
	return
	end function
	
	! Sigma energy as function of z_tot,k
	function		zs(z,k)
	implicit		none	
	complex(kind(0.0d0))::	zs,z,k
	zs=z+msb-sqrt(k**2+msb**2)-sqrt(mnu**2+k**2)
	return
	end function
	
	! Intgrand of sigmaN propagator on first sheet
	function		gsn1(z,k,eps)
	implicit		none
	complex(kind(0.0d0))::	gsn1,z,k
	real(kind(0.0d0))::	eps
	gsn1=1.d0/(z-sqrt(k**2+mnu**2)-sqrt(k**2+msb**2) &
     &	    -sesigma(k,zs(z,k),eps))
	return
	end function
	
	! Integrand of sigmaN propagator with analy. continued sigma self energy
	function		gsn2(z,k,eps)
	implicit		none
	complex(kind(0.0d0))::	gsn2,z,k
	real(kind(0.0d0))::	eps
	gsn2=1.d0/(z-sqrt(k**2+mnu**2)-sqrt(k**2+msb**2) &
     &	    -consig(k,zs(z,k),eps))
	return
	end function
	
	! Solution k of z=zs(z,k) [works also for rhoN, piDelta, with diff. arguments]
	function		solu3(z,msib,mnuc,zdel)
	implicit		none
	complex(kind(0.0d0))::	solu3,z,zdel
	real(kind(0.0d0))::	msib,mnuc
	solu3=sqrt((mnuc**2-(z-zdel)**2) &
     &            *(mnuc**2-(2.d0*msib+z-zdel)**2)) &
     &       /(2.d0*sqrt((msib+z-zdel)**2))
	return
	end function
	
	! sigma N propagator on the first sheet
	function 		snprop1xx(z,eps)
	implicit		none
	real(kind(0.0d0))::	low,up,k,eps,ksend	
	complex(kind(0.0d0))::	z,inte,snprop1xx
	real(kind(0.0d0)),dimension(1000)::	xx
     	complex(kind(0.0d0)),dimension(1000) :: df 	
	integer			np,kk
	ksend=dble(solu3(dcmplx(dble(z),0.d0),msb,mnu, &
     &              dcmplx(2.d0*mmpi-100.d0,0.d0)))
        low=0.d0
	up=ksend
	call dsg20R(low,up,nin2+2,xx,np)
	do kk=1,np
	k=xx(kk)
	df(kk)=k**2*gsn1(z,dcmplx(k,0.d0),eps)
	end do
        call drg20c(low,up,nin2+2,df,inte)
	snprop1xx=inte
	return
	end function
	
	! sigma N propagator on second sheet (works only in lower z half plane!)
	function 		snprop2xx(z,eps)
	implicit		none
	real(kind(0.0d0))::	low,up,eps,t	
	complex(kind(0.0d0))::	z,inte,snprop2xx,ktu,ksend, &
     &				kimzs,k0,k1,k2xx
	real(kind(0.0d0)),dimension(1000)::	xx
     	complex(kind(0.0d0)),dimension(1000) :: df 	
	integer			np,tt
	ksend=solu3(dcmplx(dble(z),0.d0),msb,mnu, &
     &              dcmplx(2.d0*mmpi-100.d0,0.d0))
        ktu  =solu3(z-200.d0*ci,         msb,mnu, &
     &              dcmplx(2.d0*mmpi,       0.d0))
     	if (dble(z).gt.reposi) then
	kimzs=-300.d0*ci+1.d0
	 if (abs(imag(z)).gt.400.d0) then 	! Allows to go to the region
	 kimzs=-700.d0*ci+1.d0			! -800 < Im z < -400
	 end if
	else
	kimzs=dcmplx(100.d0,0.d0)
	end if
        low=0.d0
	up=1.d0
	call dsg20R(low,up,nin2,xx,np)
	do tt=1,np
	t=xx(tt)
	k0  =kimzs*t
	k1  =kimzs+t*(ktu-kimzs)
	k2xx=ktu  +t*(ksend-ktu)
	df(tt)=kimzs      *gsn2(z,k0,  eps)*  k0**2 &
     &        +(ktu-kimzs)*gsn2(z,k1,  eps)*  k1**2 &
     &        +(ksend-ktu)*gsn2(z,k2xx,eps)*k2xx**2
	end do
	call drg20c(low,up,nin2,df,inte)
	snprop2xx=inte	
	return
	end function
	
	! WORKS ONLY IN THE UPPER z HALF PLANE
	function		diffsup(z,eps)
	implicit		none
	real(kind(0.0d0))::	eps
	complex(kind(0.0d0))::	diffsup,z
	diffsup=Conjg(snprop2xx(Conjg(z),eps)-snprop1xx(Conjg(z),eps))
	return
	end function
	
	!------------------------------------------
	! Rho N propagator
	!------------------------------------------
	
	! Rho pipi vertex
	function		verrho(q,k)
	implicit		none
	complex(kind(0.0d0))::	verrho,q,k
	verrho=grpipi/(2.d0*pi*sqrt(3.d0))*q &
     &  /(sqrt(sqrt(k**2+mrb**2))*sqrt(q**2+mmpi**2)) &
     &  *(larho**2+mrho**2)/(larho**2+4.d0*(q**2+mmpi**2))	
	return
	end function

	! pipi loop, integrand
	function		sisinterho(q,k,z,eps)
	implicit		none
	complex(kind(0.0d0))::	sisinterho,q,k,z
	real(kind(0.0d0))::	eps
	sisinterho &
     &  =q**2*verrho(q,k)**2/(z-2.d0*sqrt(q**2+mmpi**2)+ci*eps)
	return
	end function
	
	! A factor that appears in the numerical trick of integrating the pipi loop.
	function 		facsirho(q,k,z)
	implicit 		none
	complex(kind(0.0d0))::	facsirho,q,k,z
	facsirho=q**2*verrho(q,k)**2*(mmpi**2+q**2)/(q*z)
	return
	end function
	
	! Smoothened integrand (divergence subtracted)
	function		sirhosmooth(q,k,z,eps)
	implicit		none
	complex(kind(0.0d0))::	sirhosmooth,q,k,z
	real(kind(0.0d0))::	eps
	sirhosmooth=sisinterho(q,k,z,eps) &
     &	     +facsirho(qcmri(z+ci*eps),k,z+ci*eps)/(q-qcmri(z+ci*eps))	
	return
	end function

	! Analytic expression of integral over divergence
	function 		explsirho(k,z,lala)
	implicit		none
	complex(kind(0.0d0))::	explsirho,k,z
	real(kind(0.0d0))::	lala
	explsirho=facsirho(qcmri(z),k,z)*log((qcmri(z)-lala)/qcmri(z))
	return
	end function
	
	! Rho self energy, evaluated with numerical trick
	function		serho(k,z,eps)
	implicit 		none
	real(kind(0.0d0))::	q,eps,low,up,lammsi
	real(kind(0.0d0)),dimension(1000)::	xx
	complex(kind(0.0d0))::	serho,k,z,inte1, &
     &			inte2,qcom
     	complex(kind(0.0d0)),dimension(1000) :: 	df 		
	integer			np,kk
	lammsi=dble(qcmri(z))+300.d0
	low=0.d0	! lower integration boundary
	up =lammsi	! uppper integration boundary
	call dsg20R(low,up,nin,xx,np) 	! xx (gauss points) and np (index of gauss points) are output.     
        do kk=1,np			! fill now the function values. Store in "df". np = 20 times n
	q=xx(kk)
	qcom=dcmplx(q,0.d0)
	df(kk)=sirhosmooth(qcom,k,z,eps)
	end do
        call drg20c(low,up,nin,df,inte1) ! Sum Gauss 
	low=lammsi	! lower integration boundary
	up =10000.d0	! uppper integration boundary
	call dsg20R(low,up,nin,xx,np) 	! xx (gauss points) and np (index of gauss points) are output.     
        do kk=1,np			! fill now the function values. Store in "df". np = 20 times n
	q=xx(kk)
	qcom=dcmplx(q,0.d0)
	df(kk)=sisinterho(qcom,k,z,eps)
	end do
        call drg20c(low,up,nin,df,inte2) ! Sum Gauss 
	serho=inte1-explsirho(k,z+ci*eps,lammsi)+inte2
	return
	end function

	! Explicit discontinuity of rho self energy
	function 		explrhoneg(z,k)
	implicit		none	
	complex(kind(0.0d0))::	explrhoneg,z,k,qcmnega
	qcmnega=qcmneg(z,mmpi,mmpi)
	explrhoneg=-pi*ci*qcmnega*(qcmnega**2+mmpi**2)/z &
     &		    *verrho(qcmnega,k)**2
	return
	end function
	
	! rho self energy with one quarter of second sheet
	function		consigr(k,z,eps)
	implicit		none
	complex(kind(0.0d0))::	consigr,k,z
	real(kind(0.0d0))::	eps
	if ((dble(z).ge.2.d0*mmpi).and.(imag(z).lt.0.d0)) then
	consigr=serho(k,z,eps)+2.d0*explrhoneg(z,k)
	else
	consigr=serho(k,z,eps)
	end if
	return
	end function
	
	! Rho energy as function of z_tot,k
	function		zsr(z,k)
	implicit		none	
	complex(kind(0.0d0))::	zsr,z,k
	zsr=z+mrb-sqrt(k**2+mrb**2)-sqrt(mnu**2+k**2)
	return
	end function
	
	! Intgrand of rhoN propagator on first sheet
	function		gr1(z,k,eps)
	implicit		none
	complex(kind(0.0d0))::	gr1,z,k
	real(kind(0.0d0))::	eps
	gr1=1.d0/(z-sqrt(k**2+mnu**2)-sqrt(k**2+mrb**2) &
     &	    -serho(k,zsr(z,k),eps))
	return
	end function
	
	! Integrand of rhoN propagator with analy. continued rho self energy
	function		gsn2r(z,k,eps)
	implicit		none
	complex(kind(0.0d0))::	gsn2r,z,k
	real(kind(0.0d0))::	eps
	gsn2r=1.d0/(z-sqrt(k**2+mnu**2)-sqrt(k**2+mrb**2) &
     &	    -consigr(k,zsr(z,k),eps))
	return
	end function
	
	! rho N propagator on the first sheet
	function 		rprop1xx(z,eps)
	implicit		none
	real(kind(0.0d0))::	low,up,k,eps,ksend	
	complex(kind(0.0d0))::	z,inte,rprop1xx
	real(kind(0.0d0)),dimension(1000)::	xx
     	complex(kind(0.0d0)),dimension(1000) :: df 	
	integer			np,kk
	ksend=dble(solu3(dcmplx(dble(z),0.d0),mrb,mnu, &
     &              dcmplx(2.d0*mmpi-100.d0,0.d0)))
        low=0.d0
	up=ksend
	call dsg20R(low,up,nin2+2,xx,np)
	do kk=1,np
	k=xx(kk)
	df(kk)=k**2*gr1(z,dcmplx(k,0.d0),eps)
	end do
        call drg20c(low,up,nin2+2,df,inte)
	rprop1xx=inte
	return
	end function
	
	! rho N propagator on second sheet (works only in lower z half plane!)
	function 		rprop2xx(z,eps)
	implicit		none
	real(kind(0.0d0))::	low,up,eps,t	
	complex(kind(0.0d0))::	z,inte,rprop2xx,ktur, &
     &				ksendr,kimzr,k0r,k1r,k2xxr,kimzr2, &
     &				k0r1
	real(kind(0.0d0)),dimension(1000)::	xx
     	complex(kind(0.0d0)),dimension(1000) :: df 	
	integer			np,tt
	ksendr=solu3(dcmplx(dble(z),0.d0),mrb,mnu, &
     &              dcmplx(2.d0*mmpi-100.d0,0.d0))
        ktur  =solu3(z-200.d0*ci,         mrb,mnu, &
     &              dcmplx(2.d0*mmpi,       0.d0))
        low=0.d0
	up=1.d0
        ! IF UNEQUAL ZERO, GO TO FOURTH SHEET !!!
	if (gofourthr.eq.0) then
     	 if (dble(z).gt.repor) then
	 kimzr=-500.d0*ci+1.d0
	  if (abs(imag(z)).gt.400.d0) then 	! Allows to go to the region
	  kimzr=-700.d0*ci+1.d0			! -800 < Im z < -400 MeV
	  end if
	 else
	 kimzr=dcmplx(100.d0,0.d0)
	 end if
	 call dsg20R(low,up,nin2,xx,np)
	 do tt=1,np
	 t=xx(tt)
	 k0r  =kimzr*t
	 k1r  =kimzr+t*(ktur-kimzr)
	 k2xxr=ktur +t*(ksendr-ktur)
	 df(tt)=kimzr        *gsn2r(z,k0r,  eps)*  k0r**2 &
     &         +(ktur-kimzr) *gsn2r(z,k1r,  eps)*  k1r**2 &
     &         +(ksendr-ktur)*gsn2r(z,k2xxr,eps)*k2xxr**2
	 end do
	 call drg20c(low,up,nin2,df,inte)
	else
	 if (dble(z).gt.repor) then
	 kimzr = 500.d0*ci+1.d0
	 kimzr2= 500.d0*ci+500.d0
	 else
	 kimzr=dcmplx(-500.d0,0.d0)
	 kimzr2=-700.d0*ci-500.d0	 
	 end if
	 call dsg20R(low,up,nin2,xx,np)
	 do tt=1,np
	 t=xx(tt)
	 k0r  =kimzr*t
	 k0r1 =kimzr +t*(kimzr2-kimzr)
	 k1r  =kimzr2+t*(ktur-kimzr2)
	 k2xxr=ktur +t*(ksendr-ktur)
	 df(tt)=kimzr         *gsn2r(z,k0r,  eps)*  k0r**2 &
     &         +(kimzr2-kimzr)*gsn2r(z,k0r1, eps)*  k0r1**2 &
     &         +(ktur-kimzr2) *gsn2r(z,k1r,  eps)*  k1r**2 &
     &         +(ksendr-ktur) *gsn2r(z,k2xxr,eps)*  k2xxr**2
	 end do
	 call drg20c(low,up,nin2,df,inte)	 
	end if
	rprop2xx=inte	
	return
	end function
	
	! WORKS ONLY IN THE UPPER z HALF PLANE
	function		diffsupr(z,eps)
	implicit		none
	real(kind(0.0d0))::	eps
	complex(kind(0.0d0))::	diffsupr,z
	diffsupr=Conjg(rprop2xx(Conjg(z),eps)-rprop1xx(Conjg(z),eps))
	return
	end function
	
	!------------------------------------------
	! pi Delta propagator
	!------------------------------------------
	
	! piNDelta vertex
	function		verde(q)
	implicit		none
	complex(kind(0.0d0))::	verde,q,lamdell
	lamdell=(lamdel**4+mdel**4)/ &
     &   (lamdel**4+(sqrt(q**2+mmpi**2)+sqrt(q**2+mnu**2))**4)
	verde=sqrt(fcoud/(6.d0*pi))/mmpi*q*lamdell &
     &   *sqrt((sqrt(q**2+mnu**2)+mnu) &
     &         /(sqrt(q**2+mnu**2)*sqrt(q**2+mmpi**2)))
	return
	end function
	
	! piN loop, integrand
	function		sisintedel(q,z,eps)
	implicit		none
	complex(kind(0.0d0))::	sisintedel,q,z
	real(kind(0.0d0))::	eps
	sisintedel=q**2*verde(q)**2 &
     &  /(z-sqrt(q**2+mmpi**2)-sqrt(q**2+mnu**2)+ci*eps)
	return
	end function

	! A factor that appears in the numerical trick of integrating the piN loop.
	function 		facsidel(q,z)
	implicit 		none
	complex(kind(0.0d0))::	facsidel,q,z
	facsidel=q**2*verde(q)**2*sqrt(mmpi**2+q**2) &
     &	        *sqrt(mnu**2+q**2)/(q*z)
	return
	end function

	! Simplification for qcm(mmpi,mnu)
	function		qcmrid(z)
	implicit		none
	complex(kind(0.0d0))::	qcmrid,z
	qcmrid=qcmright(z,mmpi,mnu)
	return
	end function

	! Smoothened integrand (divergence subtracted)
	function		delsmooth(q,z,eps)
	implicit		none
	complex(kind(0.0d0))::	delsmooth,q,z
	real(kind(0.0d0))::	eps
	delsmooth=sisintedel(q,z,eps) &
     &	     +facsidel(qcmrid(z+ci*eps),z+ci*eps)/(q-qcmrid(z+ci*eps))	
	return
	end function

	! Analytic expression of integral over divergence
	function 		expldel(z,lala)
	implicit		none
	complex(kind(0.0d0))::	expldel,z
	real(kind(0.0d0))::	lala
	expldel=facsidel(qcmrid(z),z)*log((qcmrid(z)-lala)/qcmrid(z))
	return
	end function
	
	! Delta self energy, evaluated with numerical trick
	function		sedelta(z,eps)
	implicit 		none
	real(kind(0.0d0))::	q,eps,low,up,lammsi
	real(kind(0.0d0)),dimension(1000):: xx
	complex(kind(0.0d0))::	sedelta,z,inte1,inte2,qcom
     	complex(kind(0.0d0)),dimension(1000) :: df 		
	integer			np,kk
	lammsi=dble(qcmrid(z))+300.d0
	low=0.d0	! lower integration boundary
	up =lammsi	! uppper integration boundary
	call dsg20R(low,up,nin,xx,np) 	! xx (gauss points) and np (index of gauss points) are output.     
        do kk=1,np			! fill now the function values. Store in "df". np = 20 times n
	q=xx(kk)
	qcom=dcmplx(q,0.d0)
	df(kk)=delsmooth(qcom,z,eps)
	end do
        call drg20c(low,up,nin,df,inte1) ! Sum Gauss 
	low=lammsi	! lower integration boundary
	up =10000.d0	! uppper integration boundary
	call dsg20R(low,up,nin,xx,np) 	! xx (gauss points) and np (index of gauss points) are output.     
        do kk=1,np			! fill now the function values. Store in "df". np = 20 times n
	q=xx(kk)
	qcom=dcmplx(q,0.d0)
	df(kk)=sisintedel(qcom,z,eps)
	end do
        call drg20c(low,up,nin,df,inte2) ! Sum Gauss 
	sedelta=inte1-expldel(z+ci*eps,lammsi)+inte2
	return
	end function

	! Explicit discontinuity of Delta self energy
	function 		expldelneg(z)
	implicit		none	
	complex(kind(0.0d0))::	expldelneg,z,qcmnega
	qcmnega=qcmneg(z,mmpi,mnu)
	expldelneg=-pi*ci*qcmnega*sqrt(qcmnega**2+mmpi**2) &
     &		    *sqrt(qcmnega**2+mnu**2)/z*verde(qcmnega)**2
	return
	end function
	
	! Delta self energy with one quarter of second sheet
	function		consigd(z,eps)
	implicit		none
	complex(kind(0.0d0))::	consigd,z
	real(kind(0.0d0))::	eps
	if ((dble(z).ge.(mmpi+mnu)).and.(imag(z).lt.0.d0)) then
	consigd=sedelta(z,eps)+2.d0*expldelneg(z)
	else
	consigd=sedelta(z,eps)
	end if
	return
	end function

	! Delta energy as function of z_tot,k
	function		zdd(z,k)
	implicit		none	
	complex(kind(0.0d0))::	zdd,z,k
	zdd=z+mdelb-sqrt(mmpi**2+k**2)-sqrt(mdelb**2+k**2)
	return
	end function
	
	! Intgrand of piDelta propagator on first sheet
	function		gd(z,k,eps)
	implicit		none
	complex(kind(0.0d0))::	gd,z,k
	real(kind(0.0d0))::	eps
	gd=1.d0/(z-sqrt(mmpi**2+k**2)-sqrt(mdelb**2+k**2) &
     &	    -sedelta(zdd(z,k),eps))
	return
	end function
	
	! Integrand of piDelta propagator with analy. continued Delta self energy
	function		gsn2d(z,k,eps)
	implicit		none
	complex(kind(0.0d0))::	gsn2d,z,k
	real(kind(0.0d0))::	eps
	gsn2d=1.d0/(z-sqrt(mmpi**2+k**2)-sqrt(mdelb**2+k**2) &
     &	    -consigd(zdd(z,k),eps))
	return
	end function
	
	! piDelta propagator on the first sheet
	function 		dprop1xx(z,eps)
	implicit		none
	real(kind(0.0d0))::	low,up,k,eps,ksendd	
	complex(kind(0.0d0))::	z,inte,dprop1xx
	real(kind(0.0d0)),dimension(1000)::	xx
     	complex(kind(0.0d0)),dimension(1000) :: df 	
	integer			np,kk
	ksendd=dble(solu3(dcmplx(dble(z),0.d0),mdelb,mmpi, &
     &              dcmplx(mmpi+mnu-100.d0,0.d0)))
        low=0.d0
	up=ksendd
	call dsg20R(low,up,nin2+2,xx,np)
	do kk=1,np
	k=xx(kk)
	df(kk)=k**2*gd(z,dcmplx(k,0.d0),eps)
	end do
        call drg20c(low,up,nin2+2,df,inte)
	dprop1xx=inte
	return
	end function
	
	! pi Delta propagator on second sheet (works only in lower z half plane!)
	function 		dprop2xx(z,eps)
	implicit		none
	real(kind(0.0d0))::	low,up,eps,t	
	complex(kind(0.0d0))::	z,inte,dprop2xx,ktud, &
     &				ksendd,kintedd,k0d,k1d,k2xxd,kimzd,kimzd2,k0d1
	real(kind(0.0d0)),dimension(1000)::	xx
     	complex(kind(0.0d0)),dimension(1000) :: df 	
	integer			np,tt
	ksendd=dble(solu3(dcmplx(dble(z),0.d0),mdelb,mmpi, &
     &              dcmplx(mmpi+mnu-100.d0,0.d0)))
        ktud  =solu3(z-100.d0*ci,         mdelb,mmpi, &
     &              dcmplx(mmpi+mnu,       0.d0))
        low=0.d0
	up=1.d0
        ! IF UNEQUAL ZERO, GO TO FOURTH SHEET !!!
     	if (gofourthd.eq.0) then
	 if (dble(z).gt.repod) then
	 kintedd=-300.d0*ci+1.d0	! To extend to Imz =- 800 i MeV: 
					! below piD BP: don't change anything, but go only up to -700 i MeV.
					! piD_BP<Re z<1650: 1-700 i is OK
					! 1650<Re z<1903: 400-700 i is OK
					! 1903<Re z<2500: 1-700 i is OK.
					! All this is programmed here:
	   if (abs(imag(z)).gt.400.d0) then
	     if ((dble(z).gt.repod).and.(dble(z).lt.1650.d0)) then
	     kintedd=-700.d0*ci+1.d0
	     else if ((dble(z).ge.1650.d0).and.(dble(z).lt.1903.d0)) then
	     kintedd=-700.d0*ci+400.d0
	     else if ((dble(z).ge.1903.d0)) then
	     kintedd=-700.d0*ci+1.d0
	     end if
	   end if
	 else
	 kintedd=dcmplx(0.d0,0.d0)
	 end if
	 call dsg20R(low,up,nin2,xx,np)
	 do tt=1,np
	 t=xx(tt)
	 k0d  =        t*kintedd
	 k1d  =kintedd+t*(ktud-kintedd)
	 k2xxd=ktud   +t*(ksendd-ktud)
	 df(tt)=kintedd       *gsn2d(z,k0d,  eps)*  k0d**2 &
     &        +(ktud-kintedd) *gsn2d(z,k1d,  eps)*  k1d**2 &
     &        +(ksendd-ktud)  *gsn2d(z,k2xxd,eps)*k2xxd**2
	 end do
	 call drg20c(low,up,nin2,df,inte)
	else
	 if (dble(z).gt.repod) then
	 kimzd = 30.d0+80.d0*ci
	 kimzd2=100.d0+70.d0*ci 
	 else
	 kimzd= -80.d0-  1.d0*ci
	 kimzd2=-80.d0-200.d0*ci	 
	 end if
	 call dsg20R(low,up,nin2,xx,np)
	 do tt=1,np
	 t=xx(tt)
	 k0d  =kimzd*t
	 k0d1 =kimzd +t*(kimzd2-kimzd)
	 k1d  =kimzd2+t*(ktud-kimzd2)
	 k2xxd=ktud +t*(ksendd-ktud)
	 df(tt)=kimzd         *gsn2d(z,k0d,  eps)*  k0d**2 &
     &         +(kimzd2-kimzd)*gsn2d(z,k0d1, eps)*  k0d1**2 &
     &         +(ktud-kimzd2) *gsn2d(z,k1d,  eps)*  k1d**2 &
     &         +(ksendd-ktud) *gsn2d(z,k2xxd,eps)*  k2xxd**2
	 end do
	 call drg20c(low,up,nin2,df,inte)	 
	 
	 
	 
	end if
	dprop2xx=inte	
	return
	end function
	
	! WORKS ONLY IN THE UPPER z HALF PLANE
	function		diffsupd(z,eps)
	implicit		none
	real(kind(0.0d0))::	eps
	complex(kind(0.0d0))::	diffsupd,z
	diffsupd=Conjg(dprop2xx(Conjg(z),eps)-dprop1xx(Conjg(z),eps))
	return
	end function diffsupd

	!--------------------------------------------------------
	
	end module cont_unstable
