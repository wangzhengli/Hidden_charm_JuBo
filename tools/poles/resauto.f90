! SUBROUTINE TO CALCULATE QUANTITIES RELATED TO POLES AND RESIDUES.

subroutine 	resid(iv,k_j,polepo,cou,ao,a1,couoff,r_photo)
    ! Input: iv, k_j, polepos: Quantum numbers and pole position
    ! Output: cou(n_c), ao(n_c): coupling strengths and a_0 of piN --> channels 1 to n_c.
    ! a1: Following term in Laurent expansion. Numerically unstable
    ! couoff(n_c,max_q): n off shell coupling strengths of piN --> channels 1 to n_c
    ! Convention (in --> out) : tmatrix(n_piN,n_out,iv,out,piN)
    use input4
    use tmat4
    implicit				none
    real(kind(0.d0))::			pii,low,up,x,fac_ao,r,ezim
    real(kind(0.d0)),dimension(100)::	xx
    complex(kind(0.d0))::			resi,to,s,cii,fac_ex,g1,inte,inteao,zet1,zet2,intplay,aoout,ao2out,deltaz,selfprime, &
        & resana,self2prime,gagaprime,aoanal1,aoanal2,aoanal,polepo,fac_a1,intea1,secde,phasfac, &
        & res_det_plus,res_det_minus
    complex(kind(0.d0)),dimension(n_c,2,100) :: df		! First index: channel no., 2nd: channel ii or 1i, 3rd: function values at Gauss points	
    complex(kind(0.d0)),dimension(n_c,100)   :: aodf,a1df	! First index: channel no., 2nd: function values at Gauss points
    complex(kind(0.d0)),dimension(100) :: dftemp,aotemp,a1temp,dfinv 	! temporary storage of function values for subroutine "drg20c"	
    complex(kind(0.d0)),dimension(n_c,2):: c_produ
    complex(kind(0.d0)),dimension(n_c)  :: cou,ao,a1		! Coupling strengths; output. a_0 term; output
    complex(kind(0.d0)),dimension(n_c,max_q)  :: couoff	! Coupling strength off shell; output
    complex(kind(0.d0)),dimension(3,2) :: fbarea,fbarec,fdressa,fdressc 
    ! bare and dressed vertices (only piN->piN, always on-shell values)
    complex(kind(0.d0)),dimension(3,2,2) :: selfen 		! Self energy (is a 2x2 matrix) . First index: evaluated at 3 points around pole position
    complex(kind(0.d0)),dimension(3)     ::	zo123,sell
    complex(kind(0.d0)),dimension(n_c,2,100,max_q,max_q) :: dfoff	! Values T matrix for off-shell residues.
    ! First index: channel no., 
    ! 2nd: channels ii or 1i, 
    ! 3rd: function values at Gauss points,
    ! 4th and 5th: off-shell points (n+1)x(n+1)	
    complex(kind(0.d0)),dimension(n_c,2,max_q,max_q):: c_produoff
    complex(kind(0.d0)),dimension(n_c,2,3):: t3
    ! t3 stores three values (last index), T^(2)(z_0-deltaz), T^(2)(z_0), T^(2)(z_0+deltaz)
    complex(kind(0.d0)),dimension(n_c,max_q,3):: t3off
    complex(kind(0.d0)),dimension(2)     ::	rphotoup,rphotolow,r_photo
    integer	::	np,i,j,k,iv,k_j,n1,ivv,ioff,joff,nint,npoleca,quit_a1,shorter,e_m,em_i,em_e, i_c1, i_c2,i_q2

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!	END VARIABLE DEFINITIONS


    !!!!!!!!!!!!!!!!!!!!!!!!!!! 	Now some Parameters that may be changed by hand:

    nint	=1 	! nint times 20 = number of Gauss points at "xx" (n_max=5 here). 
    ! We use the file "gaussint.f" which provides a Gauss integration.
    r	=3.5d0	! Radius of circular integration contour
    npoleca	=1	! If 1: Use full T-matrix, if 0: use only Non-pole T-mtarix
    quit_a1	=1	! Set the a1 term to zero. Should be switched on (1)
    shorter =1	! If one, the residue a_-1, a_0, and a_1 are calculated using derivatives of the inverse amplitude
    !	deltaz  =dcmplx(3.5d0,0.d0) ! Step size for numerical derivative for derivative method.
    deltaz  =dcmplx(2.d0,0.d0) ! Step size for numerical derivative for derivative method.
    !!!!!!!!!!!!!!!!!!!!!!!!!!! 	Start calculation using derivatives

    !	if (iv==2.and.k_j==1) then	! Adapt Delta to optimize the result!
    !	deltaz	= dcmplx(10.d0,0.d0)	! N(1440)
    !        else if (iv==3.and.k_j==1) then
    !        deltaz  = dcmplx(2.d0,0.d0)	! Delta(1620)
    !        else if (iv==4.and.k_j==1) then
    !        deltaz  = dcmplx(3.d0,0.d0)	! Delta(1910)
    !        else if (iv==2.and.k_j==2) then
    !        deltaz  = dcmplx(17.d0,0.d0)	! N(1520)
    !        else if (iv==4.and.k_j==2) then
    !        deltaz  = dcmplx(1.5d0,0.d0)	! Delta(1700)
    !	end if

    ! Gauss point distribution and integration contour angle alpha according to pole position:
    j1=2*k_j - 1              ! j1=2*j_physical  ! declared in input
    call rot_mat(j1)          ! input xg,wg,djw  ! util.f90/rot_mat  ! dmat3.f90
    call set_coeff_jls(j1)
    zrem=polepo
    call mesh_q


    cou=0.d0
    ao=0.d0
    a1=0.d0
    couoff=0.d0

    n1 =n+1

    !==============================================
    !	IMPORTANT: WE DO HERE CHANNEL 1 --> i
    !	For output for Kanzo change this to i --> 1 in "SHORTER". 

    if (shorter.eq.1) then	


      s=polepo+deltaz
      call tmat(s,1,4,k_j)  	
      if (npoleca==1) then
        !          do e_m=1,2
        call g_res_calc(s,iv,k_j)    
        !          end do                               
        call t_fs_calc(s,iv,k_j)    
        call tmat_s_calc(s,iv,k_j)  
        call t_add(iv,k_j)      

        !------ new ----------------

        do i_c1=1,n_c
          do i_c2=1,n_c
            do i_q2=1,n1
              T_transport(i_q2,i_c1,i_c2,iv,k_j)=tmatrix(i_q2,n1,iv,i_c1,i_c2)            
            end do
          end do
        end do

        !-----------------------    

        !         do e_m=1,2
        !           call f_ind_a_photo(iv,k_j,e_m,s)      ! output: photoproduction amplitude m_gam(iv,k_j,i_c,e_m)
        !           rphotoup(e_m)=m_gam(iv,k_j,1,e_m)	  ! The one means: PION photoproduction         
        !!          rphotoup(e_m)=m_gam(iv,k_j,5,e_m)	  ! The one means: ETA photoproduction         (uncomment this line!)

        !           open(55,file='test/test_resid_rphotoup.txt')
        !               write(55,*)rphotoup(e_m), m_gam(iv,k_j,1,e_m), e_m            

        !         end do         

        !         close(55)

      end if
      res_det_plus=res_det
      do i=1,n_c
        t3(i,1,3)=tmatrix(n1,n1,iv,i,i)
        t3(i,2,3)=tmatrix(n1,n1,iv,i,1)
        do j=1,n
          t3off(i,j,3)=tmatrix(j,n1,iv,i,1)
        end do
      end do

      s=polepo-deltaz
      call tmat(s,1,4,k_j)  	
      if (npoleca==1) then
        !          do e_m=1,2
        call g_res_calc(s,iv,k_j)    
        !          end do                               
        call t_fs_calc(s,iv,k_j)   
        call tmat_s_calc(s,iv,k_j) 
        call t_add(iv,k_j)       

        !------ new ----------------

        do i_c1=1,n_c
          do i_c2=1,n_c
            do i_q2=1,n1
              T_transport(i_q2,i_c1,i_c2,iv,k_j)=tmatrix(i_q2,n1,iv,i_c1,i_c2)            
            end do
          end do
        end do

        !-----------------------    

        !         do e_m=1,2
        !           call f_ind_a_photo(iv,k_j,e_m,s)      ! output: photoproduction amplitude m_gam(iv,k_j,i_c,e_m)
        !           rphotolow(e_m)=m_gam(iv,k_j,1,e_m)	  ! The one means: PION photoproduction
        !!          rphotolow(e_m)=m_gam(iv,k_j,5,e_m)	  ! The one means: ETA photoproduction (uncomment this line!)

        !               open(55,file='test/test_resid_rphotlow.txt')
        !               write(55,*)rphotolow(e_m), m_gam(iv,k_j,1,e_m), e_m            

        !        end do                    

        !              close(55)

      end if
      res_det_minus=res_det
      do i=1,n_c
        t3(i,1,1)=tmatrix(n1,n1,iv,i,i)
        t3(i,2,1)=tmatrix(n1,n1,iv,i,1)
        do j=1,n
          t3off(i,j,1)=tmatrix(j,n1,iv,i,1)
        end do
      end do

      res_det_prime=(res_det_plus-res_det_minus)/(2.d0*deltaz)	! This is needed for the output for Kanzo.
      ! The value res_det_prime is further
      ! used in cpole2.f90
      ! Note: the next call of tmat is the last one here
      ! and the dressed vertex remains in the memory
      ! to be used also in cpole2.f90

      s=polepo			
      call tmat(s,1,4,k_j)  	
      if (npoleca==1) then
        !          do e_m=1,2
        call g_res_calc(s,iv,k_j)    
        !          end do                               
        call t_fs_calc(s,iv,k_j)   
        call tmat_s_calc(s,iv,k_j) 
        call t_add(iv,k_j)         
      end if
      do i=1,n_c
        t3(i,1,2)=tmatrix(n1,n1,iv,i,i)
        t3(i,2,2)=tmatrix(n1,n1,iv,i,1)
        do j=1,n
          t3off(i,j,2)=tmatrix(j,n1,iv,i,1)
        end do
      end do


      ! Now calculate the diagonal and off-diagonal residues and couplings strengths:

      do i=1,n_c
        if (t3(i,2,2).eq.dcmplx(0.d0,0.d0)) then	
          c_produ(i,2)     =dcmplx(0.d0,0.d0)
          test_trans_bra(i)=dcmplx(0.d0,0.d0)
        else
          c_produ(i,1)=1.d0/((1.d0/t3(i,1,3)-1.d0/t3(i,1,1))/(2.d0*deltaz))	! On-shell values
          cou(i)=           1.d0/((1.d0/t3(i,2,3)-1.d0/t3(i,2,1))/(2.d0*deltaz))/sqrt(c_produ(1,1))
          test_trans_bra(i)=1.d0/((1.d0/t3(i,2,3)-1.d0/t3(i,2,1))/(2.d0*deltaz)) ! Residue piN --> MB, for direct test of transition branching ratio
          do j=1,n								! Off shell values readily evaluated!!
            couoff(i,j)=1.d0/((1.d0/t3off(i,j,3)-1.d0/t3off(i,j,1))/(2.d0*deltaz))/sqrt(c_produ(1,1))
          end do
        end if
      end do

      ! Calculation of multipole residue and r_photo

      !        if(k_j==1.and.(iv==1.or.iv==3)) then  !S11, S31 only electric multipole
      !                em_i=1
      !                em_e=1
      !        else if(k_j==1.and.(iv==2.or.iv==4)) then   !P11, P31 only magnetic multipole
      !                em_i=2
      !                em_e=2
      !        else
      !                em_i=1
      !                em_e=2 
      !        end if     
      !        r_photo=0.
      !	do e_m=em_i,em_e
      !	  r_photo(e_m)=1.d0/((1.d0/rphotoup(e_m)-1.d0/rphotolow(e_m))/(2.d0*deltaz))
      !	end do ! e_m=em_i,em_e
      !	  r_photo=r_photo/(1000.d0*197.3d0)	! r_photo is the residue of the multipole and has no units. See factors of (1000.d0*197.3d0) in "f_ind_a_photo".
      ! Remember residue is calculated w.r.t. to E [MeV].

      !         do e_m=em_i,em_e
      !           open(55,file='test/test_resid.txt')
      !               write(55,*)r_photo(e_m), e_m            
      !         end do
      !           close(55)     

      ! Now calculate a_0 (Only piN --> MB)!

      do i=1,n_c	 
        if (t3(i,2,2).eq.dcmplx(0.d0,0.d0)) then
          secde=dcmplx(0.d0,0.d0)
        else
          secde=1.d0/deltaz**2*(1.d0/t3(i,2,3)-2.d0/t3(i,2,2)+1.d0/t3(i,2,1))
        end if
        ao(i)=-1.d0/2.d0*c_produ(i,2)**2*secde
      end do

    else		! if (shorter.eq.1) then

      !!!!!!!!!!!!!!!!!!!!!!!!!!! 	Start calculation using contour integrals
      ! We make a contour integration; contour is circle with radius r around the pole position to=polepos; 
      ! parameterization of the path: s=to+r*exp(cii*pii*x)
      low=0.d0	! lower integration boundary
      up =2.d0	! uppper integration boundary
      cii=dcmplx(0.d0,1.d0)
      to =polepo	! Pole position 
      n1 =n+1		! Index of the on-shell point 
      pii=3.14159265d0! Weird constant that appears from time to time
      ! For safety, initialize a couple of variables with zero:
      df 	=dcmplx(0.d0,0.d0)
      aodf	=dcmplx(0.d0,0.d0)
      dftemp	=dcmplx(0.d0,0.d0)
      aotemp	=dcmplx(0.d0,0.d0)
      cou	=dcmplx(0.d0,0.d0)
      couoff	=dcmplx(0.d0,0.d0)

      call dsg20R(low,up,nint,xx,np) 	! xx (gauss points) and np (index of gauss points) are output.     
      do k=1,np			! fill now the function values. Store in "df". np = 20 times nint
        write(*,*)'in loop k=1,...,np, k is (iv, k_j): ',k,iv,k_j ! For monitoring on the screen.
        x=xx(k)
        s=to+r*exp(cii*pii*x)		! Sqrt(s) as a function of parameterized path.
        fac_ex=exp(cii*pii*x)*r/2.d0 	! Factor for a_(-1)
        call tmat(s,1,4,k_j)  		! Call Non-pole t-matrix. This is what takes time.
        !	call onextra(s,k_j)		! Comment this out, because it activates the extrapolation algorithm!
        if (npoleca==1) then
          !          do e_m=1,2
          call g_res_calc(s,iv,k_j)    
          !          end do                               
          call t_fs_calc(s,iv,k_j)    ! dressed vertices, self energy
          call tmat_s_calc(s,iv,k_j)  ! t_pole    
          call t_add(iv,k_j)          ! t=t_np+t_p
        end if
        write(6000,145)x,tmatrix(n1,n1,iv,1,1) ! One can plot the integrand to check for smoothness.
        do i=1,n_c			! Store the function values for all channels i from 1 to n_c
          df(i,1,k)=tmatrix(n1,n1,iv,i,i)*fac_ex ! for (g_i)^2
          df(i,2,k)=tmatrix(n1,n1,iv,i,1)*fac_ex ! for g_1 g_i
          dfinv(k) =tmatrix(n1,n1,iv,5,1)*fac_ex ! To test residue eta N --> piN (see below)
          aodf(i,k)=tmatrix(n1,n1,iv,i,1)	  ! for a_0 in piN --> channel i
          a1df(i,k)=tmatrix(n1,n1,iv,i,1)	  ! for a_1 in piN --> channel i
          do ioff=1,n1		! Same, but for off-shell points. Additional sums over all indices -> (n+1) for each index
            do joff=1,n1
              dfoff(i,1,k,ioff,joff)=tmatrix_pole(joff,ioff,iv,i,i)*fac_ex 
              dfoff(i,2,k,ioff,joff)=tmatrix_pole(joff,ioff,iv,i,1)*fac_ex		   
            end do
          end do
        end do		
      end do		! loop over  Gauss points k=1,...,np

      do i=1,n_c	! Evaluate integral (= sum over Gauss points in "drg20c".)
        do j=1,2
          do k=1,100	! Fill dftemp, which is a 1-dim vector as required by "drg20c"
            dftemp(k)=df(i,j,k)
          end do ! k
          call drg20c(low,up,nint,dftemp,inte) 	! Residue: Calculate the integral for all channels, diagonal and off-diag	
          c_produ(i,j)=inte
          do ioff=1,n1			! Off-shell residue: Additional sums over all indices -> (n+1) for each index
            do joff=1,n1
              do k=1,100
                dftemp(k)=dfoff(i,j,k,ioff,joff)
              end do ! k
              call drg20c(low,up,nint,dftemp,inte)
              c_produoff(i,j,ioff,joff)=inte
            end do !joff
          end do !ioff
        end do ! j
      end do ! i

      ! Now, we have the products of couplings stored in "c_produ":
      ! c_produ(i,1)= (g_i)^2
      ! c_produ(i,2)= g_1 g_i
      ! The values in "c_produ(i,2)" will help us to fix the relative signs of the g_i  because so far we only have (g_i)^2. 
      ! Fixing the relative signs is now the second step:
      ! First, we chose one global sign for channel 1 (by taking the square root):
      cou(1)=sqrt(c_produ(1,1))
      ! Now, we evaluate the other couplings, sign is now fixed:
      do i=2,n_c
        cou(i)=c_produ(i,2)/cou(1) ! This gives the couplings "cou" of channels 2-n_c, including the correct relative sign.
        test_trans_bra(i)=c_produ(i,2)	
      end do

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!! 	Calculation and tests of off-shell residues  

      !Same for off-shell residues (n off shell points):
      do i=1,n_c
        do ioff=1,n
          couoff(i,ioff)=c_produoff(i,2,ioff,n1)/cou(1) ! This gives the couplings "cou" of channels 2-n_c, including the correct relative sign.
        end do
      end do
      ! Start now test of off-shell residues and other quantities:
      write(1220,*)'Test of Residues, case (iv,k_j)= ',iv,k_j
      write(1220,*)'on-shell residue piN  -> eta N:',c_produ(5,2)
      call drg20c(low,up,nint,dfinv,inte)
      write(1220,*)'on-shell residue etaN ->  pi N:',inte
      write(1220,*)'should be the same.'
      write(1220,*)
      write(1200,*)'Test if g_off factorizes correctly:'
      write(1220,*)'Check piN --> eta N, off-shell (No7) to off-shell (No10) residue'
      write(1220,*)'g_off(7) g_off(10) = ',couoff(1,7)*couoff(5,10)
      write(1220,*)'a_-1 direct calc.  = ',c_produoff(5,2,10,7)
      write(1220,*)'should be the same.'
      write(1220,*)'-----------------------------------------------'
      write(1220,*)''
      write(1220,*)'Residue sN',-conjg(cou(8))*1000.d0
      write(1220,*)'c_produ(i=8,2)',c_produ(8,2)
      write(1220,*)'c_produ(i=1,1)',c_produ(1,1)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!! 	Calculation of a_0  

      fac_ao=dcmplx(1.d0/2.d0,0.d0)	! Factor for a_0
      do i=1,n_c	! Evaluate integral (= sum over Gauss points in "drg20c".)
        aotemp=dcmplx(0.d0,0.d0)
        do k=1,100	! Fill dftemp, which is a 1-dim vector as required by "drg20c"
          aotemp(k)=aodf(i,k)
          x=xx(k)
          s=to+r*exp(cii*pii*x)		! Sqrt(s) as a function of parameterized path.
          aotemp(k)=(aotemp(k)-c_produ(i,2)/(s-to))*fac_ao
          if (i==1.and.aotemp(k).ne.0.d0) then
            write(6001,145)x,aotemp(k) ! One can plot the improved integrand for a_0 to check for smoothness.
          end if	 
        end do ! k
        call drg20c(low,up,nint,aotemp,inteao)	! a_0: 	   Calculate all a_0 for piN --> channel i
        ao(i)=inteao
      end do ! i

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!! 	Calculation of a_1  

      do i=1,n_c	! Evaluate integral (= sum over Gauss points in "drg20c".)
        a1temp=dcmplx(0.d0,0.d0)
        do k=1,100	! Fill dftemp, which is a 1-dim vector as required by "drg20c"
          a1temp(k)=a1df(i,k)
          x=xx(k)
          s=to+r*exp(cii*pii*x)		! Sqrt(s) as a function of parameterized path.
          fac_a1=exp(-cii*pii*x)/(2.d0*r) 	! Factor for a_(-1)
          a1temp(k)=(a1temp(k)-c_produ(i,2)/(s-to)-ao(i))*fac_a1
        end do ! k
        call drg20c(low,up,nint,a1temp,intea1)	! a_0: 	   Calculate all a_0 for piN --> channel i
        if (quit_a1==0) then
          a1(i)=intea1
        else
          a1(i)=dcmplx(0.d0,0.d0)
        end if			
      end do ! i

    end if ! Use derivatives of inverse amplitude or contour integral for calculation of the a's. (if (shorter.eq.1) then)
    145   format(18(15g15.6))     
end subroutine resid



!=======================================================================================
!
!	Scan of complex plane
!
!=======================================================================================

subroutine scan_cmplx(k_start,k_end,iden)
    use input4
    use tmat4
    implicit none
    integer 	    :: k_start,k_end,k_j,nscan,ivv,iden,e_m,resqs,imsqs
    !	real(kind(0.d0))    :: resqs,imsqs
    complex(kind(0.d0)) :: s

    !    iden=iden-10	! Index iden (=ident) runs now from 1 to ....

    !  k_j     iv
    if (iden==1) then 	
      open(unit=138,file='analytic/scans/S11_plane.dat')!   1       1 
      k_j=1
      ivv=1
    else if (iden==2) then      
      open(unit=138,file='analytic/scans/P11_plane.dat')!   1       2
      k_j=1
      ivv=2
    else if (iden==3) then
      open(unit=138,file='analytic/scans/S31_plane.dat')!   1       3
      k_j=1
      ivv=3
      mrho_fac=3.d0
    else if (iden==4) then
      open(unit=138,file='analytic/scans/P31_plane.dat')!   1       4
      k_j=1
      ivv=4
      mrho_fac=2.5d0
    else if (iden==5) then
      open(unit=138,file='analytic/scans/P13_plane.dat')!   2       1
      k_j=2
      ivv=1
      mrho_fac=3.0d0
    else if (iden==6) then
      open(unit=138,file='analytic/scans/D13_plane.dat')!   2       2
      k_j=2
      ivv=2
      mrho_fac=4.d0
    else if (iden==7) then
      open(unit=138,file='analytic/scans/P33_plane.dat')!   2       3
      k_j=2
      ivv=3
      mrho_fac=5.d0
    else if (iden==8) then
      open(unit=138,file='analytic/scans/D33_plane.dat')!   2       4
      k_j=2
      ivv=4
      mrho_fac=2.2d0
    else if (iden==9) then     
      open(unit=138,file='analytic/scans/D15_plane.dat')!	3	1
      k_j=3
      ivv=1
      mrho_fac=5.d0
    else if (iden==10) then
      open(unit=138,file='analytic/scans/F15_plane.dat')!	3	2
      k_j=3
      ivv=2
      mrho_fac=4.5d0
    else if (iden==11) then
      open(unit=138,file='analytic/scans/D35_plane.dat')!	3	3
      k_j=3
      ivv=3
      mrho_fac=4.5d0
    else if (iden==12) then
      open(unit=138,file='analytic/scans/F35_plane.dat')!	3	4
      k_j=3
      ivv=4
      mrho_fac=2.d0
    else if (iden==13) then
      open(unit=138,file='analytic/scans/F17_plane.dat')!	4	1
      k_j=4
      ivv=1
      mrho_fac=5.3d0
    else if (iden==14) then
      open(unit=138,file='analytic/scans/G17_plane.dat')!	4	2
      k_j=4
      ivv=2
      mrho_fac=2.7d0
    else if (iden==15) then
      open(unit=138,file='analytic/scans/F37_plane.dat')!	4	3
      k_j=4
      ivv=3
      mrho_fac=5.6d0
    else if (iden==16) then
      open(unit=138,file='analytic/scans/G37_plane.dat')!	4	4
      k_j=4
      ivv=4
      mrho_fac=5.0d0
    else if (iden==17) then
      open(unit=138,file='analytic/scans/G19_plane.dat')!	5	1
      k_j=5
      ivv=1
      mrho_fac=6.5d0
    else if (iden==18) then
      open(unit=138,file='analytic/scans/H19_plane.dat')!	5	2
      k_j=5
      ivv=2
      mrho_fac=5.d0
    else if (iden==19) then
      open(unit=138,file='analytic/scans/G39_plane.dat')!	5	3
      k_j=5
      ivv=3
      mrho_fac=3.d0
    else if (iden==20) then
      open(unit=138,file='analytic/scans/H39_plane.dat')!	5	4
      k_j=5
      ivv=4
    else
      write(0,*)'WRONG COUNTING',iden
      !	stop
    end if

    nscan=n+1
    j1=2*k_j - 1              ! j1=2*j_physical  ! declared in input
    call rot_mat(j1)          ! input xg,wg,djw  ! util.f90/rot_mat  ! dmat3.f90
    call set_coeff_jls(j1)

    do resqs=4200,4700,5
      do imsqs=0,300,5
        s=dcmplx(dble(resqs),dble(imsqs))
        zrem=s		! Now call mesh_q to adpapt angle alpha according to z:
        call mesh_q
        call tmat(s,ivv,ivv,k_j)
        !       do e_m=1,2
        !        call g_res_calc(s,ivv,k_j)    
        !       end do                               
        !        call t_fs_calc(s,ivv,k_j)     ! dressed vertices, self energy
        !        call tmat_s_calc(s,ivv,k_j)   ! t_pole  
        !        call t_add(ivv,k_j)	    ! t=t_np+t_p
        write(138,145)s, cdabs(tmatrix(nscan,nscan,ivv,14,14)), &
            &  		      cdabs(tmatrix(nscan,nscan,ivv,15,15)) , &
            &  		    	cdabs(tmatrix(nscan,nscan,ivv,16,16)) , &
            &  		    	cdabs(tmatrix(nscan,nscan,ivv,17,17)) , &
            &  		    	cdabs(tmatrix(nscan,nscan,ivv,18,18)) , &
            &  		    	cdabs(tmatrix(nscan,nscan,ivv,19,19)) , &
            &  		    	cdabs(tmatrix(nscan,nscan,ivv,20,20)) , &
            &  		    	cdabs(tmatrix(nscan,nscan,ivv,21,21))
        !            &  		    	tmatrix(nscan,nscan,ivv,8,8) , &
        !            &  		    	tmatrix(nscan,nscan,ivv,12,12) , &
        !            &  		    	tmatrix(nscan,nscan,ivv,13,13) 
        write(0,*) iden,s
      end do	! imsqs
    end do  ! resqs
    145   format(10(15g15.6))     
    return
end subroutine scan_cmplx
