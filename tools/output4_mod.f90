!
!  last updated:  July 13, 2009      F. Huang 
!

module output4
  use input4

  complex(kind(0.d0)) :: z_kanzo(n_c,max_q)  ! conversion factor: Invariant < TOPT
  complex(kind(0.d0)), dimension(max_e,n_c,n_c) :: phase_fac
  complex(kind(0.d0)), dimension(max_e,max_j,4,n_c,n_r) :: drs_vrtx
  complex(kind(0.d0)), dimension(max_e,max_j,4,n_r,n_r) :: slf_mb    ! self energy + bare mass
  integer :: tau_bgd_unit(1:4)   ! initialized in subroutine tau_print_prep
  integer :: tau_bg2_unit(1:4)   ! initialized in subroutine tau_print_prep
  integer :: tau_res_unit(1:4)   ! used inside a angular momentum loop
  integer :: del_bgd_unit(1:4)   ! initialized in subroutine del_print_prep
  integer :: del_res_unit(1:4)   ! used inside a angular momentum loop
  integer :: T_NP_unit, T_Full_unit, vertex_unit
  character*99 :: fmtstr

contains

  !=======================================================================!

  subroutine photo_prep
      implicit none

      T_NP_unit = 4001
      T_Full_unit = 4002
      vertex_unit = 4000
      open(T_NP_unit, file='photo_FSI_NP.dat')
      open(T_Full_unit, file='photo_FSI_Full.dat')
      open(vertex_unit, file='photo_vertex.dat')

  end subroutine photo_prep

  !=======================================================================!

  subroutine z_inv_topt   ! conversion factor: Invariant(Kanzo) < TOPT 
      implicit none
      integer :: ic, i, n1
      complex(kind(0.0d0)) :: m_1, m_2, xmev, om_1, om_2, n1n2_inv

      n1 = n+1
      do ic=1,n_c
        m_1=ms_f_c(ic)
        m_2=ms_b_c(ic)
        do i=1,n1
          xmev=xgau(ic,i)
          om_1=cdsqrt(xmev**2+m_1**2)
          om_2=cdsqrt(xmev**2+m_2**2)
          n1n2_inv=2.d0*cdsqrt(om_1*om_2)*rtpi3
          z_kanzo(ic,i)=n1n2_inv/cdsqrt(2.d0*m_1)
        end do ! i=1,n1
      end do ! ic=1,n_c

  end subroutine z_inv_topt


  !=======================================================================!

  subroutine photo_print(k, j, i)
      implicit none
      integer :: k, j, i, nf, n1, iv

      n1 = n + 1
      if (i==1) then
        write(T_NP_unit, '(a, i2, a, e17.8)') 'k_j =', j, '   ecm =', dreal(e_cm(k))/uf
        do nf = 1, n
          write(T_NP_unit, '(32e17.8)') (z_kanzo(1,n1)*tmatrix(nf,n1,iv,1,1)*z_kanzo(1,nf)*uf, iv=1,4), &
              & (z_kanzo(5,n1)*tmatrix(nf,n1,iv,5,1)*z_kanzo(1,nf)*uf, iv=1,4), &
              & (z_kanzo(1,n1)*tmatrix(nf,n1,iv,1,5)*z_kanzo(5,nf)*uf, iv=1,4), &
              & (z_kanzo(5,n1)*tmatrix(nf,n1,iv,5,5)*z_kanzo(5,nf)*uf, iv=1,4)
        end do
      else if (i==2) then
        write(T_Full_unit, '(a, i2, a, e17.8)') 'k_j =', j, '   ecm =', dreal(e_cm(k))/uf
        do nf = 1, n
          write(T_Full_unit, '(32e17.8)') (z_kanzo(1,n1)*tmatrix(nf,n1,iv,1,1)*z_kanzo(1,nf)*uf, iv=1,4), &
              & (z_kanzo(5,n1)*tmatrix(nf,n1,iv,5,1)*z_kanzo(1,nf)*uf, iv=1,4), &
              & (z_kanzo(1,n1)*tmatrix(nf,n1,iv,1,5)*z_kanzo(5,nf)*uf, iv=1,4), &
              & (z_kanzo(5,n1)*tmatrix(nf,n1,iv,5,5)*z_kanzo(5,nf)*uf, iv=1,4)
        end do
      end if

  end subroutine photo_print

  !=======================================================================!

  subroutine dr_vrtx_print
      implicit none
      integer :: i_rs1, i_rs2, k, k_j, iv, max_res

      do k_j = k_b, k_e
        do iv = 1, 4
          max_res=max_nr(iv,k_j)
          write(vertex_unit, '(a, i2, a, i2)') 'k_j =', k_j, '   iv =', iv
          do k = 1, melab
            write(vertex_unit,'(9e17.8)') dreal(e_cm(k))/uf, &
                & ((slf_mb(k,k_j,iv,i_rs1,i_rs2)/uf,i_rs2=1,max_res),i_rs1=1,max_res)
          end do
        end do
      end do

      write(vertex_unit, *)

      do k_j = k_b, k_e
        do iv = 1, 4
          max_res=max_nr(iv,k_j)
          write(vertex_unit, '(a, i2, a, i2)') 'k_j =', k_j, '   iv =', iv
          do k = 1, melab
            write(vertex_unit,'(9e17.8)') dreal(e_cm(k))/uf, (drs_vrtx(k,k_j,iv,1,i_rs1), i_rs1=1,max_res), &
                & (drs_vrtx(k,k_j,iv,5,i_rs1), i_rs1=1,max_res)
          end do
        end do
      end do

  end subroutine dr_vrtx_print

  !=======================================================================!

  subroutine dr_vrtx_photo(k,iv,k_j)
      implicit none
      integer :: iv, k_j, k, n1, max_res, i_rs1, i_rs2, i_c1, i_q1

      n1 = n+1
      max_res=max_nr(iv,k_j)

      do i_c1 = 1, n_c
        do i_rs1 = 1, max_res
          drs_vrtx(k,k_j,iv,i_c1,i_rs1) = f_drs_a(i_c1,n1,i_rs1)*dcmplx(0.d0,-1.d0)*z_kanzo(i_c1,n1)
        end do
      end do

      do i_rs1 = 1, max_res
        do i_rs2 = 1, max_res
          slf_mb(k, k_j, iv, i_rs1, i_rs2) = slf(i_rs1, i_rs2)
          if(i_rs1==i_rs2) slf_mb(k, k_j, iv, i_rs1, i_rs2) =   slf_mb(k, k_j, iv, i_rs1, i_rs2) &
              & + m_bare(iv,k_j,i_rs1)
        end do
      end do

  end subroutine dr_vrtx_photo

  !=======================================================================!

  subroutine phase_factor   ! to calculate the phase factor
      ! used in transition from T matrix to tau matrix in JLS basis

      implicit none
      integer :: k, ic, jc
      complex(kind(0.d0)) :: ea, eb, rhoi, rhof, ez

      do k=1,melab

        ez=e_cm(k)
        call qon_c(ez,qon)  ! on shell momenta

        do ic=1,n_c

          ea=cdsqrt(qon(ic)**2 + ms_f_c(ic)**2)
          eb=cdsqrt(qon(ic)**2 + ms_b_c(ic)**2)
          if(real(ea).lt.0.or.real(eb).lt.0)write(*,*)'error, energy not in the expected sheet'
          rhoi=qon(ic)*ea*eb/ez

          do jc=1,n_c

            ea=cdsqrt(qon(jc)**2 + ms_f_c(jc)**2)
            eb=cdsqrt(qon(jc)**2 + ms_b_c(jc)**2)
            rhof=qon(jc)*ea*eb/ez

            phase_fac(k,ic,jc)=-pi*cdsqrt(rhof*rhoi)

          end do  ! jc=1,n_c

        end do  ! ic=1,n_c

      end do  ! k=1,melab

  end subroutine phase_factor

  !=======================================================================!

  subroutine v_matrx(k,j)   ! on-shell potential for all channels

      implicit none
      integer :: k, j, ic, jc, iv, n1

      n1=n+1

      do iv=1,4
        do ic=1,n_c
          do jc=1,n_c
            ptnmat(j,iv,k,ic,jc)=vmatrix(iv,ic,jc,n1,n1)
          end do  ! jc=1,n_c
        end do  ! ic=1,n_c
      end do  ! iv=1,4

  end subroutine v_matrx

  !=======================================================================!

  subroutine tau_matrx(k,j)   ! on-shell tau-matrix for all channels

      implicit none
      integer :: k, j, ic, jc, iv, n1

      n1=n+1

      do iv=1,4
        do ic=1,n_c
          do jc=1,n_c
            taumat(j,iv,k,ic,jc)=phase_fac(k,ic,jc)*tmatrix(n1,n1,iv,ic,jc)
            !	        taumat(j,iv,k,ic,jc)=tmatrix(n1,n1,iv,ic,jc)*10000.d0                
          end do  ! jc=1,n_c
        end do  ! ic=1,n_c
      end do  ! iv=1,4

  end subroutine tau_matrx
  !=======================================================================!

  subroutine tau_matrxnp(k,j)   ! on-shell tau-matrix for all channels

      implicit none
      integer :: k, j, ic, jc, iv, n1

      n1=n+1

      do iv=1,4
        do ic=1,n_c
          do jc=1,n_c
            taumatnp(j,iv,k,ic,jc)=phase_fac(k,ic,jc)*tmatrix(n1,n1,iv,ic,jc)
            !	        taumatnp(j,iv,k,ic,jc)=tmatrix(n1,n1,iv,ic,jc)*10000.d0                
          end do  ! jc=1,n_c
        end do  ! ic=1,n_c
      end do  ! iv=1,4

  end subroutine tau_matrxnp

  !=======================================================================!

  subroutine  tau_print_prep(k_j)  ! prepare to print tau matrix

      implicit none
      integer :: k_j, iv,n1   ! k_j and iv determine the quantum numbers of the channel
      n1=n+1
      do iv=1,4

        tau_res_unit(iv)=3010+iv
        tau_bgd_unit(iv)=3014+iv
        tau_bg2_unit(iv)=3018+iv

        open(tau_res_unit(iv),file='tau_'//res(iv,k_j))    ! allocated in input/piN_pw_read
        open(tau_bgd_unit(iv),file='tau_'//bgd(iv,k_j))    ! allocated in input/piN_pw_read
        open(tau_bg2_unit(iv),file='analytic/tau_'//bgd(iv,k_j))    ! allocated in input/piN_pw_read

      end do  ! iv=1,4

  end subroutine  tau_print_prep

  !=======================================================================!

  subroutine  tau_print_close(k_j)  ! close file-units (prepare for next k_j)
      implicit none
      integer :: k_j, iv   ! k_j and iv determine the quantum numbers of the channel
      do iv=1,4
        close(tau_res_unit(iv))   ! defined in subroutine tau_print_prep
        close(tau_bgd_unit(iv))
        close(tau_bg2_unit(iv))
      end do  ! iv=1,4
  end subroutine  tau_print_close

  !=======================================================================!

  subroutine tau_print(k,j,ic,jc,iopt)   ! print on shell tau matrix for channel ic->jc

      implicit none
      integer :: k, j, ic, jc, iopt, n1
      integer :: iv, iout,iou2
      real(kind(0.d0)) :: ez
      n1=n+1
      ez=abs(e_cm(k))

      do iv=1,4

        if(iopt==1)then    
          iout=tau_bgd_unit(iv)   ! allocated in input3_mod.f90/piN_pw_read and
          iou2=tau_bg2_unit(iv)   ! 
          write(iou2,'(f10.0,12(15g15.6))')ez,tmatrix(n1,n1,iv,1,1),tmatrix(n1,n1,iv,2,2),tmatrix(n1,n1,iv,3,3),tmatrix(n1,n1,iv,4,4),tmatrix(n1,n1,iv,6,6),tmatrix(n1,n1,iv,7,7)

        else if(iopt==2)then      ! defined in tau_print_prep (above)
          iout=tau_res_unit(iv)
        else
          write(*,*)'Error, wrong tau matrix output'
          call exit
        end if

        write(iout,'(f10.0,6f18.8)')ez,taumat(j,iv,k,ic,jc),cdabs(taumat(j,iv,k,ic,jc)),tmatrix(n1,n1,iv,ic,jc),cdabs(tmatrix(n1,n1,iv,ic,jc))

      end do

  end subroutine  tau_print

  !=======================================================================!

  subroutine phas_shif(k,j)

      implicit none
      integer :: k, j, ic, jc, iv
      real(kind(0.d0)) :: del1, del2, delta
      real(kind(0.d0)) :: mindel, del_last	  

      do iv=1,4
        do ic=1,n_c
          do jc=1,n_c

            !           if(k==1)then
            !	          mindel=0.d0
            !	        else
            !	          mindel=del(j,iv,k-1,ic,jc)
            !	        end if

            del1=2.d0*real(taumat(j,iv,k,ic,jc))
            del2=1.d0-2.d0*aimag(taumat(j,iv,k,ic,jc))
            delta=0.5d0*datan2(del1,del2)*180.d0/pi


            !	if((mindel-delta)>50.d0)then
            !            delta=delta+180.d0
            !           end if

            del(j,iv,k,ic,jc) = delta
            eta(j,iv,k,ic,jc) = dsqrt(del1**2+del2**2)

          end do  ! jc=1,n_c
        end do  ! ic=1,n_c
      end do  ! iv=1,4

  end subroutine phas_shif

  !=======================================================================!

  subroutine  del_print_prep(k_j)  ! prepare to print phase shift

      implicit none
      integer :: k_j, iv   ! k_j and iv determine the quantum numbers of the channel

      do iv=1,4

        del_res_unit(iv)=3020+iv
        del_bgd_unit(iv)=3024+iv

        open(del_res_unit(iv),file='del_'//res(iv,k_j))    ! allocated in input/piN_pw_read
        open(del_bgd_unit(iv),file='del_'//bgd(iv,k_j))    ! allocated in input/piN_pw_read

      end do  ! iv=1,4

  end subroutine  del_print_prep

  !=======================================================================!

  subroutine  del_print_close(k_j)  ! close file-units (prepare for next k_j)

      implicit none
      integer :: k_j, iv   ! k_j and iv determine the quantum numbers of the channel

      do iv=1,4
        close(del_res_unit(iv))   ! defined in subroutine del_print_prep
        close(del_bgd_unit(iv))
      end do  ! iv=1,4

  end subroutine  del_print_close

  !=======================================================================!

  subroutine del_print(k,j,ic,jc,iopt)   ! print phase shifts and inelasticities

      implicit none
      integer :: k, j, ic, jc, iopt
      integer :: iv, iout
      real(kind(0.d0)) :: ez

      ez=abs(e_cm(k))

      do iv=1,4

        if(iopt==1)then
          iout=del_bgd_unit(iv)   ! allocated in input3_mod.f90/piN_pw_read and
        else if(iopt==2)then      ! defined in del_print_prep (above)
          iout=del_res_unit(iv)
        else
          write(*,*)'Error, wrong phase shift output'
          call exit
        end if

        !        write(iout,'(f10.5,2d18.8)')ez/1000.d0,del(j,iv,k,ic,jc),1.d0-eta(j,iv,k,ic,jc)**2
        write(iout,'(f10.0,2d18.8)')ez,del(j,iv,k,ic,jc),1.d0-eta(j,iv,k,ic,jc)**2

      end do

  end subroutine  del_print



  !=======================================================================!

  subroutine tau_out(ic,jc)   ! print on-shell tau-matrix to file
      implicit none

      integer :: k, j, ic, jc, iv
      real(kind(0.d0)) :: ez

      open(1,file='plot/tau_os.dat')

      do k=1,melab
        ez=e_cm(k)/1000.d0
        fmtstr='(f9.5,32f8.3)'
        write(1,fmtstr)ez,((taumat(j,iv,k,ic,jc),iv=1,4),j=k_b,k_e)
      end do

      close(1)

  end subroutine tau_out

  !=======================================================================!

  subroutine v_out(ic,jc)   ! print on-shell potential to file
      implicit none

      integer :: k, j, ic, jc, iv
      real(kind(0.d0)) :: ez, threshold

      open(1,file='plot/v_os.dat')

      threshold=dmax1(ms_f_c(ic)+ms_b_c(ic),ms_f_c(jc)+ms_b_c(jc))/1000.d0
      do k=1,melab
        ez=e_cm(k)/1000.d0
        !            fmtstr='(f9.5,<8*(k_e-k_b+1)>f9.3)'
        fmtstr='(f9.5,32f9.3)'
        if(ez.ge.threshold) write(1,fmtstr)ez,&
            &((ptnmat(j,iv,k,ic,jc)*10.d6,iv=1,4),j=k_b,k_e)
      end do

      close(1)

  end subroutine v_out

  !=======================================================================!

  subroutine del_out(ic,jc)   ! print phase shifts and inelasticities to file
      implicit none

      integer :: k, j, ic, jc, iv
      real(kind(0.d0)) :: ez

      open(1,file='plot/del_inels.dat')

      do k=1,melab
        ez=e_cm(k)/1000.d0
        !            fmtstr='(f9.5,<8*(k_e-k_b+1)>f10.3)'
        fmtstr='(f9.5,32f10.3)'
        write(1,fmtstr)ez,&
            &((del(j,iv,k,ic,jc),1.d0-eta(j,iv,k,ic,jc)**2,iv=1,4),j=k_b,k_e)
      end do

      close(1)

  end subroutine del_out

  !=======================================================================!

  subroutine sig_out(ic,jc)   ! total cross section for channel ic -> jc
      implicit none

      integer :: k, j, ic, jc, iv
      real(kind(0.d0)) :: sigpar(max_j,6), sig(3), threshold
      complex(kind(0.d0)) :: tau

      if(ic==1.and.jc==1)then  ! Npi --> Npi
        threshold=ms_f_c(1)+ms_b_c(1)
        open(1,file='plot/sig_piN_piN.txt')  ! pi-p -> pi0n
        do k=1,melab
          if(real(e_cm(k)).ge.threshold)then
            call qon_c(e_cm(k),qon)  ! on shell momenta
            sig=0.d0
            do j=k_b,k_e
              do iv=1,2
                tau=dsqrt(2.d0)/3.d0*(taumat(j,iv+2,k,1,1)-taumat(j,iv,k,1,1))  ! pi-p -> pi0n
                sigpar(j,iv)=fpi/qon(1)**2*(2.d0*j)*tau*conjg(tau)/2.d0*uf*uf*10.d0
                sig(1)=sig(1)+sigpar(j,iv)
              end do  ! iv
            end do  ! j
            write(1,'(f9.5,f12.5)')real(e_cm(k))/1000.d0,sig(1)
          end if
        end do  ! k
        close(1)
      end if

      if(ic==1.and.jc==7)then  ! Npi --> LamK
        threshold=ms_f_c(12)+ms_b_c(12)
        open(1,file='plot/sig_piN_LamK.txt')  ! pi-p -> LamK0
        do k=1,melab
          if(real(e_cm(k)).ge.threshold)then
            call qon_c(e_cm(k),qon)  ! on shell momenta
            sig=0.d0
            do j=k_b,k_e
              do iv=1,2
                tau=dsqrt(2.d0/3.d0)*taumat(j,iv,k,12,1)  ! pi-p -> LamK0
                sigpar(j,iv)=fpi/qon(1)**2*(2.d0*j)*tau*conjg(tau)/2.d0*uf*uf*10.d0
                sig(1)=sig(1)+sigpar(j,iv)
              end do  ! iv
            end do  ! j
            write(1,'(f9.5,f12.5)')real(e_cm(k))/1000.d0,sig(1)
          end if
        end do  ! k
        close(1)
      end if

      if(ic==1.and.jc==8)then  ! Npi --> SigK
        threshold=ms_f_c(13)+ms_b_c(13)
        open(1,file='plot/sig_piN_SigK.txt')  ! pi+p -> Sig+K+, pi-p -> Sig0K0, pi-p -> Sig-K+
        do k=1,melab
          if(real(e_cm(k)).ge.threshold)then
            call qon_c(e_cm(k),qon)  ! on shell momenta
            sig=0.d0
            do j=k_b,k_e
              do iv=1,2
                tau=taumat(j,iv+2,k,13,1)  ! pi+p -> Sig+K+
                sigpar(j,iv)=fpi/qon(1)**2*(2.d0*j)*tau*conjg(tau)/2.d0*uf*uf*10.d0
                sig(1)=sig(1)+sigpar(j,iv)
                tau=dsqrt(2.d0)/3.d0*(taumat(j,iv,k,13,1)+taumat(j,iv+2,k,13,1))  ! pi-p -> Sig0K0
                sigpar(j,iv)=fpi/qon(1)**2*(2.d0*j)*tau*conjg(tau)/2.d0*uf*uf*10.d0
                sig(2)=sig(2)+sigpar(j,iv)
                tau=-2.d0/3.d0*taumat(j,iv,k,13,1)+1.d0/3.d0*taumat(j,iv+2,k,13,1)  ! pi-p -> Sig-K+
                sigpar(j,iv)=fpi/qon(1)**2*(2.d0*j)*tau*conjg(tau)/2.d0*uf*uf*10.d0
                sig(3)=sig(3)+sigpar(j,iv)
              end do  ! iv
            end do  ! j
            write(1,'(f9.5,3f12.5)')real(e_cm(k))/1000.d0,sig(1),sig(2),sig(3)
          end if
        end do  ! k
        close(1)
      end if

  end subroutine sig_out

  !=======================================================================!

  subroutine scatleng_out(iv,k_j)	! Only for testing the scattering length formular


      !	use input_minuit
      use input4
      use tmat4	
      implicit none

      real(kind(0.0d0)), dimension(4,2) :: scatleng, a_p,a_m
      integer		  ::  n1, iv,k_j,i,k

      n1=n+1
      !	k=myid


      if (k==4) then
        if (iv==4) then
          open(99,file='scattering_length/epsilon_0.5MeV_P31_N=14_epsi_piNprop=5.dat')
        else if (iv==1) then
          open(99,file='scattering_length/epsilon_0.5MeV_S11_N=14_epsi_piNprop=5.dat')
        end if
        call scattering_length(scatleng,a_p,a_m)

        write(99,156)iv,k_j,scatleng(iv,k_j),qon(1),e_cm(k), 'iv, k_j, scatleng(iv,k_j)'	
        close(99)


      else if (k==5) then
        if (iv==4) then
          open(99,file='scattering_length/epsilon_1.5MeV_P31_N=14_epsi_piNprop=5.dat')
        else if (iv==1) then
          open(99,file='scattering_length/epsilon_1.5MeV_S11_N=14_epsi_piNprop=5.dat')
        end if
        call scattering_length(scatleng,a_p,a_m)

        write(99,156)iv,k_j,scatleng(iv,k_j),qon(1), e_cm(k),'iv, k_j, scatleng(iv,k_j)'	
        close(99)


      else if (k==6) then
        if (iv==4) then
          open(99,file='scattering_length/epsilon_2.5MeV_P31_N=14_epsi_piNprop=5.dat')
        else if (iv==1) then
          open(99,file='scattering_length/epsilon_2.5MeV_S11_N=14_epsi_piNprop=5.dat')
        end if
        call scattering_length(scatleng,a_p,a_m)

        write(99,156)iv,k_j,scatleng(iv,k_j),qon(1), e_cm(k), 'iv, k_j, scatleng(iv,k_j)'	
        close(99)


      else if (k==7) then
        if (iv==4) then
          open(99,file='scattering_length/epsilon_4MeV_P31_N=14_epsi_piNprop=5.dat')
        else if (iv==1) then
          open(99,file='scattering_length/epsilon_4MeV_S11_N=14_epsi_piNprop=5.dat')
        end if
        call scattering_length(scatleng,a_p,a_m)

        write(99,156)iv,k_j,scatleng(iv,k_j),qon(1), e_cm(k), 'iv, k_j, scatleng(iv,k_j)'	
        close(99)


      else if (k==8) then
        if (iv==4) then
          open(99,file='scattering_length/epsilon_8MeV_P31_N=14_epsi_piNprop=5.dat')
        else if (iv==1) then
          open(99,file='scattering_length/epsilon_8MeV_S11_N=14_epsi_piNprop=5.dat')
        end if
        call scattering_length(scatleng,a_p,a_m)

        write(99,156)iv,k_j,scatleng(iv,k_j),qon(1), e_cm(k), 'iv, k_j, scatleng(iv,k_j)'	
        close(99)

      end if

      156    format(2(15g15.6))	
  end subroutine scatleng_out

  !------------------------------------------------------------------------------------------

  subroutine a_p_m_out(i,k_j)	! Only for testing the scattering length formular


      !	use input_minuit
      use input4
      use tmat4	
      implicit none

      real(kind(0.0d0)), dimension(4,2) :: scatleng, a_p,a_m
      integer		  ::  n1, iv,k_j,i,k

      n1=n+1
      !	k=myid


      if (k==4) then


        open(99,file='scattering_length/scattering_length_and_volumes_N=14_epsi_piNprop=5.dat')

        !	do k_j=1,2
        !	do i=1,2
        call scattering_length(scatleng,a_p,a_m)

        !	do k_j=1,2
        !     	 do i=1,2		! i=1 -> iv=1 and 3; i=2 -> iv=2 and 4

        !	  write(99,156)k_j,i,a_p(i,k_j),a_m(i,k_j), 'k_j,i,a_p,a_m'
        write(99,156) a_p(1,1),a_m(1,1), 'a^+_0+ , a^-_0+'
        write(99,156) a_p(2,1),a_m(2,1), 'a^+_1- , a^-_1-'
        write(99,156) a_p(1,2),a_m(1,2), 'a^+_1+ , a^-_1+'
        !	 end do
        !	end do
        close(99)

      end if

      156    format(2(15g15.6))	
  end subroutine a_p_m_out










end module output4
