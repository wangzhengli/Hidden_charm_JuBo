!======================================================!
!   Numerical evaluation of vertex functions           !
!   Last updated:  F. Huang, Nov. 4, 2009              !
!   Resonance coupling to LamK and SigK are included.  !
!======================================================!

      module vertices_5h7h	!(newres)
	  use input4

      integer :: i_rs,i_q12,i_c12,i_c12_1,i_c12_2
      real(kind(0.d0)) :: m_1,m_2,f_iso
      real(kind(0.d0)) :: Lam,mff
      real(kind(0.d0)) :: cls_proj
      complex(kind(0.d0)) :: q_12,Eon_1,Eon_2,g_eff,cf
      complex(kind(0.d0)) :: f_vert,form,c_topt
      complex(kind(0.d0)) :: v_vert(36)  ! used locally

	  contains

!=======================================================================!

	! Erased g_res1hm_calc which was here. (newres)

!=======================================================================!

	! Erased g_res1hp_calc which was here.(newres)

!=======================================================================!

!      subroutine g_res3hm_calc(ecm,iv,k_j)  ! resonance: 3 half minus
      subroutine g_res5hp_calc(ecm,iv,k_j)  ! resonance: 5 half plus renamed! (newres)

      implicit none
      complex(kind(0.d0)) :: ecm
	  integer :: k_j,iv  ! used to determine the quantum numbers of the channel
	  integer :: n1  ! n1=n+1, n is the number of mesh points in momentum space

      n1=n+1

      f_bar_c = (0.d0,0.d0)  ! initialize resonance creation
      f_bar_a = (0.d0,0.d0)  ! initialize resonance annihilation
!      f_bar_a_photo_P=(0.d0,0.d0)
!      f_bar_a_photo_NP=(0.d0,0.d0)

      do i_rs=1,max_nr(iv,k_j)

!--------!
!  N pi  !
!--------!
        i_c12=1

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
		if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) then  ! contact term
                        g_eff= sqrt( dcmplx( g_bare(iv,k_j,i_rs,i_c12),0.d0) )
			if (g_bare(iv,k_j,i_rs,i_c12).gt.0.d0) then
				cf=dcmplx(1.d0,0.d0)
				else
				cf=dcmplx(0.d0,1.d0)
			end if
		end if
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	    Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	    f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! before D13; will become F wave now:
!           v_vert(1)=f_vert*g_eff*cls_proj			! OLD (D-wave)
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	! (newres) Here, the factor q/m_nucleon is put which makes the correct L !!!!!

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres) This line was missing before; the power of the Form factor was one, now it is higher and we have to say
	    							! this explictly!
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*cls_proj*q_12/ms_f_c(i_c12)*&
 !                                       & ((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt

          end do  ! i_q12
	    end if  ! N pi

!---------!
!  N rho  !
!---------!
        i_c12=2
        i_c12_1=i_c12+1
        i_c12_2=i_c12+2

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
	  m_2=ms_b_c(i_c12)  ! rho mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi)/3.d0

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
            Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

 	        f_vert=(0.d0,1.d0)*(-q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1)  ! D-wave, S=1/2
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

 	        f_vert=(0.d0,1.d0)*(2.*q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1)  ! D-wave, S=3/2
            v_vert(2)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

 	        f_vert=(0.d0,1.d0)*( q_12*q_12/(Eon_1 + m_1)+2.d0*Eon_2+m_2)*cdsqrt(Eon_1+m_1)  ! S-wave, S=3/2
            v_vert(3)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt( 0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12  ,i_q12) = v_vert(1)*form*c_topt  ! D-wave, L=j+1/2, S=1/2
            f_bar_c(i_rs,i_c12_1,i_q12) = v_vert(2)*form*c_topt  ! D-wave, L=j+1/2, S=3/2
            f_bar_c(i_rs,i_c12_2,i_q12) = v_vert(3)*form*c_topt  ! S-wave, L=j-3/2, S=3/2

            f_bar_a(i_c12  ,i_q12,i_rs) =-v_vert(1)*form*c_topt
!            f_bar_a_photo_P(i_c12  ,i_q12,i_rs) =-v_vert(1)*form*c_topt/(0.d0,1.d0)
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,1.d0)*((-q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1))*f_iso*cls_proj*&
!                                          & q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


            f_bar_a(i_c12_1,i_q12,i_rs) =-v_vert(2)*form*c_topt
 !           f_bar_a_photo_P(i_c12_1,i_q12,i_rs) =-v_vert(2)*form*c_topt/(0.d0,1.d0)
!            f_bar_a_photo_NP(i_c12_1,i_q12)= (0.d0,1.d0)*((2.*q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1))*f_iso*cls_proj*q_12/ms_f_c(i_c12)*&
 !                                       & ((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt

            f_bar_a(i_c12_2,i_q12,i_rs) =-v_vert(3)*form*c_topt
!            f_bar_a_photo_P(i_c12_2,i_q12,i_rs) =-v_vert(3)*form*c_topt/(0.d0,1.d0)
!            f_bar_a_photo_NP(i_c12_2,i_q12)= f_vert*f_iso*cls_proj*q_12/ms_f_c(i_c12)*&
!                                        & ((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! N rho

!---------!
!  N eta  !
!---------!
        i_c12=5

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! eta mass
          f_iso=1.d0         ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)  !CHECK

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

 	        f_vert=(q_12*q_12+Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! D-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! D-wave ! double derivative coupling
!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*cls_proj*q_12/ms_f_c(i_c12)*&
!                                        & ((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

        end if  ! N eta

!------------!
!  Delta pi  !
!------------!
        i_c12=6
        i_c12_1=i_c12+1

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Delta mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=-dsqrt(2.d0) ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=dsqrt(5.d0/3.d0)  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi)/3.d0

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	        form = form**npow_ff(iv,k_j,i_rs,i_c12)

            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

 	        f_vert=-(Eon_1-m_1)/m_1*(Eon_2+q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13, D-wave
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

 	        f_vert=(Eon_1+2.*m_1)/m_1*(Eon_2+q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  !D13, S-wave
            v_vert(2)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

	        f_bar_c(i_rs,i_c12  ,i_q12) =  v_vert(1)*form*c_topt
	        f_bar_c(i_rs,i_c12_1,i_q12) =  v_vert(2)*form*c_topt

	        f_bar_a(i_c12  ,i_q12,i_rs) =  v_vert(1)*form*c_topt
!	        f_bar_a_photo_P(i_c12  ,i_q12,i_rs) =  v_vert(1)*form*c_topt
!                f_bar_a_photo_NP(i_c12,i_q12)= (-(Eon_1-m_1)/m_1*(Eon_2+q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1) )*f_iso*&
 !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt

	        f_bar_a(i_c12_1,i_q12,i_rs) =  v_vert(2)*form*c_topt
!	        f_bar_a_photo_P(i_c12_1,i_q12,i_rs) =  v_vert(2)*form*c_topt
!                f_bar_a_photo_NP(i_c12_1,i_q12)= f_vert*f_iso*&
 !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

        end if  ! Delta pi

!---------!
!  Lam K  !
!---------!
        i_c12=12

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Lambda mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=1.d0  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
 !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! Lam K

!---------!
!  Sig K  !
!---------!
        i_c12=13

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Sigma mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=-dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
!                                     & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! Sig K

      !---------!
      !  Lamc D !
      !---------!
              i_c12=14

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Lambda mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=1.d0  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                  v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                  f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
       !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                end do  ! i_q12

      	    end if  ! Lamc D

      !---------!
      !  Sigc D !
      !---------!
              i_c12=15

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Sigma mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=-dsqrt(3.d0)  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                  v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                  f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
      !                                     & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                end do  ! i_q12

      	    end if  ! Sigc D

            !---------!
            ! Lamc Ds !
            !---------!
                    i_c12=16

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Lambda mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=1.d0  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                        v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                        f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
             !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                      end do  ! i_q12

            	    end if  ! Lamc Ds

            !---------!
            ! Sigc Ds !
            !---------!
                    i_c12=17

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Sigma mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=-dsqrt(3.d0)  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                        v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                        f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
            !                                     & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                      end do  ! i_q12

            	    end if  ! Sigc Ds

      end do  ! i_rs=1,max_nr(iv,k_j)

      end subroutine g_res5hp_calc  ! resonance: 5 half plus (newres)

!=======================================================================!

!      subroutine g_res3hp_calc(ecm,iv,k_j)  ! resonance: 3 half plus
      subroutine g_res5hm_calc(ecm,iv,k_j)  ! resonance: 5 half minus

      implicit none
      complex(kind(0.d0)) :: ecm
	  integer :: k_j,iv  ! used to determine the quantum numbers of the channel
	  integer :: n1  ! n1=n+1, n is the number of mesh points in momentum space
      complex(kind(0.d0)) :: p1_til,term1,term2

      n1=n+1

      f_bar_c = (0.d0,0.d0)  ! initialize resonance creation
      f_bar_a = (0.d0,0.d0)  ! initialize resonance annihilation
!      f_bar_a_photo_P=(0.d0,0.d0)
!      f_bar_a_photo_NP=(0.d0,0.d0)


      do i_rs=1,max_nr(iv,k_j)

!--------!
!  N pi  !
!--------!
        i_c12=1

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3)f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
		if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) then  ! contact term
                        g_eff= sqrt( dcmplx( g_bare(iv,k_j,i_rs,i_c12),0.d0) )
			if (g_bare(iv,k_j,i_rs,i_c12).gt.0.d0) then
				cf=dcmplx(1.d0,0.d0)
			else
				cf=dcmplx(0.d0,1.d0)
			end if
		end if
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	    Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	! (newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
 !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! N pi

!---------!
!  N rho  !
!---------!
        i_c12=2
        i_c12_1=i_c12+1
        i_c12_2=i_c12+2

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
	      m_2=ms_b_c(i_c12)  ! rho mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi)/3.d0

          do i_q12=1,n1
            q_12 = xgau(i_c12,i_q12)
            Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

 	        f_vert=(q_12-(Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! P-wave, S=1/2
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	! (newres)

 	        f_vert=(5.*q_12+(4.*Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! P-wave, S=3/2
            v_vert(2)=f_vert*g_eff*cls_proj/dsqrt(5.d0)*q_12/ms_f_c(i_c12)	! (newres)

 	        f_vert=(Eon_2-m_2)*q_12/(Eon_1 + m_1)*cdsqrt(Eon_1+m_1)  ! F-wave, S=3/2
            v_vert(3)=f_vert*g_eff*cls_proj/dsqrt(5.d0)*3.d0*q_12/ms_f_c(i_c12)	! (newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12  ,i_q12) = v_vert(1)*form*c_topt  ! P-wave, L=j-1/2, S=1/2
            f_bar_c(i_rs,i_c12_1,i_q12) = v_vert(2)*form*c_topt  ! P-wave, L=j-1/2, S=3/2
            f_bar_c(i_rs,i_c12_2,i_q12) = v_vert(3)*form*c_topt  ! F-wave, L=j+3/2, S=3/2

            f_bar_a(i_c12  ,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_P(i_c12  ,i_q12,i_rs) = v_vert(1)*form*c_topt
 !           f_bar_a_photo_NP(i_c12,i_q12)= ((q_12-(Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1))*f_iso*&
  !                                   & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt

            f_bar_a(i_c12_1,i_q12,i_rs) = v_vert(2)*form*c_topt
 !           f_bar_a_photo_P(i_c12_1,i_q12,i_rs) = v_vert(2)*form*c_topt
 !           f_bar_a_photo_NP(i_c12_1,i_q12)= ((5.*q_12+(4.*Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1))*f_iso*&
  !                  & cls_proj/dsqrt(5.d0)*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


            f_bar_a(i_c12_2,i_q12,i_rs) = v_vert(3)*form*c_topt
 !           f_bar_a_photo_P(i_c12_2,i_q12,i_rs) = v_vert(3)*form*c_topt
 !           f_bar_a_photo_NP(i_c12_2,i_q12)= f_vert*f_iso*&
 !                   & cls_proj/dsqrt(5.d0)*3.d0*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! N rho

!---------!
!  N eta  !
!---------!
        i_c12=5

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! eta mass
          f_iso=1.d0         ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	! (newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                                     & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

        end if  ! N eta

!------------!
!  Delta pi  !
!------------!
        i_c12=6
        i_c12_1=i_c12+1

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Delta mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=-dsqrt(2.d0) ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=dsqrt(5.d0/3.d0)  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/5.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)
            p1_til=q_12/(Eon_1+m_1)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            term1 = 4.d0*q_12**2+3.d0*Eon_1*Eon_2
            term2 = q_12*(5.d0*Eon_1+4.d0*Eon_2)
 	        f_vert=(term1*p1_til-term2)/m_1*cdsqrt(Eon_1+m_1)  ! P-wave, S=3/2
            v_vert(1)=f_vert*g_eff*cls_proj/3.d0*(0.d0,-1.d0)*q_12/ms_f_c(i_c12)	!(newres)

            term1 = q_12**2+2.*Eon_1*Eon_2
            term2 = q_12*Eon_2
 	        f_vert=(term1*p1_til-term2)/m_1*cdsqrt(Eon_1+m_1)  ! F-wave, S=3/2
            v_vert(2)=f_vert*g_eff*cls_proj*(0.d0,-1.d0)*q_12/ms_f_c(i_c12)	!(newres)

	        f_bar_c(i_rs,i_c12  ,i_q12) =  v_vert(1)*form*c_topt
	        f_bar_c(i_rs,i_c12_1,i_q12) =  v_vert(2)*form*c_topt

	        f_bar_a(i_c12  ,i_q12,i_rs) = -v_vert(1)*form*c_topt
!	        f_bar_a_photo_P(i_c12  ,i_q12,i_rs) = -v_vert(1)*form*c_topt/(0.d0,1.d0)
!                f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*(((4.d0*q_12**2+3.d0*Eon_1*Eon_2)*p1_til-(q_12*(5.d0*Eon_1+4.d0*Eon_2)))/m_1*&
!            & cdsqrt(Eon_1+m_1))*f_iso*cls_proj/3.d0*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


	        f_bar_a(i_c12_1,i_q12,i_rs) = -v_vert(2)*form*c_topt
!	        f_bar_a_photo_P(i_c12_1,i_q12,i_rs) = -v_vert(2)*form*c_topt/(0.d0,1.d0)
!                f_bar_a_photo_NP(i_c12_1,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                                     & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

        end if  ! Delta pi

!---------!
!  Lam K  !
!---------!
        i_c12=12

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Lambda mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=1.d0  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
 !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! Lam K

!---------!
!  Sig K  !
!---------!
        i_c12=13

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Sigma mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=-dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

 !           f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
 !           f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
 !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! Sig K

      !---------!
      !  Lamc D !
      !---------!
              i_c12=14

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Lambda mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=1.d0  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                  v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                  f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
       !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                end do  ! i_q12

      	    end if  ! Lamc D

      !---------!
      !  Sigc D !
      !---------!
              i_c12=15

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Sigma mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=-dsqrt(3.d0)  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                  v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                  f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

       !           f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
       !           f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
       !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                end do  ! i_q12

      	    end if  ! Sigc D

            !---------!
            ! Lamc Ds !
            !---------!
                    i_c12=16

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Lambda mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=1.d0  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                        v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                        f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
             !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                      end do  ! i_q12

            	    end if  ! Lamc Ds

            !---------!
            ! Sigc Ds !
            !---------!
                    i_c12=17

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Sigma mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=-dsqrt(3.d0)  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                        v_vert(1)=f_vert*g_eff*cls_proj*q_12/ms_f_c(i_c12)	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                        f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

             !           f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
             !           f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
             !                                    & cls_proj*q_12/ms_f_c(i_c12)*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                      end do  ! i_q12

            	    end if  ! Sigc Ds

      end do  ! i_rs=1,max_nr(iv,k_j)

      end subroutine g_res5hm_calc  ! resonance: 5 half minus

!=======================================================================!


!      subroutine g_res3hm_calc(ecm,iv,k_j)  ! resonance: 3 half minus
!      subroutine g_res5hp_calc(ecm,iv,k_j)  ! resonance: 5 half plus renamed! (newres)
      subroutine g_res7hm_calc(ecm,iv,k_j)  ! resonance: 7 half minus renamed! (newres)

      implicit none
      complex(kind(0.d0)) :: ecm
	  integer :: k_j,iv  ! used to determine the quantum numbers of the channel
	  integer :: n1  ! n1=n+1, n is the number of mesh points in momentum space

      n1=n+1

      f_bar_c = (0.d0,0.d0)  ! initialize resonance creation
      f_bar_a = (0.d0,0.d0)  ! initialize resonance annihilation
!      f_bar_a_photo_P=(0.d0,0.d0)
!      f_bar_a_photo_NP=(0.d0,0.d0)


      do i_rs=1,max_nr(iv,k_j)

!--------!
!  N pi  !
!--------!
        i_c12=1

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
		if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) then  ! contact term
			g_eff= sqrt( dcmplx( g_bare(iv,k_j,i_rs,i_c12),0.d0) )
			if (g_bare(iv,k_j,i_rs,i_c12).gt.0.d0) then
				cf=dcmplx(1.d0,0.d0)
				else
				cf=dcmplx(0.d0,1.d0)
			end if
		end if
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! before D13; will become F wave now:
!            v_vert(1)=f_vert*g_eff*cls_proj			! OLD (D-wave)
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	! (newres) Here, the factor q/m_nucleon is put which makes the correct L !!!!!

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres) This line was missing before; the power of the Form factor was one, now it is higher and we have to say
	    							! this explictly!
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
!                                     & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! N pi

!---------!
!  N rho  !
!---------!
        i_c12=2
        i_c12_1=i_c12+1
        i_c12_2=i_c12+2

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
	      m_2=ms_b_c(i_c12)  ! rho mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi)/3.d0

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
            Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

 	        f_vert=(0.d0,1.d0)*(-q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1)  ! D-wave, S=1/2
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

 	        f_vert=(0.d0,1.d0)*(2.*q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1)  ! D-wave, S=3/2
            v_vert(2)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

 	        f_vert=(0.d0,1.d0)*( q_12*q_12/(Eon_1 + m_1)+2.d0*Eon_2+m_2)*cdsqrt(Eon_1+m_1)  ! S-wave, S=3/2
            v_vert(3)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt( 0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12  ,i_q12) = v_vert(1)*form*c_topt  ! D-wave, L=j+1/2, S=1/2
            f_bar_c(i_rs,i_c12_1,i_q12) = v_vert(2)*form*c_topt  ! D-wave, L=j+1/2, S=3/2
            f_bar_c(i_rs,i_c12_2,i_q12) = v_vert(3)*form*c_topt  ! S-wave, L=j-3/2, S=3/2

            f_bar_a(i_c12  ,i_q12,i_rs) =-v_vert(1)*form*c_topt
!            f_bar_a_photo_P(i_c12  ,i_q12,i_rs) =-v_vert(1)*form*c_topt/(0.d0,1.d0)
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,1.d0)*((-q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1))*f_iso*&
!                                     & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt

            f_bar_a(i_c12_1,i_q12,i_rs) =-v_vert(2)*form*c_topt
 !           f_bar_a_photo_P(i_c12_1,i_q12,i_rs) =-v_vert(2)*form*c_topt/(0.d0,1.d0)
 !           f_bar_a_photo_NP(i_c12_1,i_q12)= (0.d0,1.d0)*((2.*q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1))*f_iso*&
 !                                    & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt

            f_bar_a(i_c12_2,i_q12,i_rs) =-v_vert(3)*form*c_topt
 !           f_bar_a_photo_P(i_c12_2,i_q12,i_rs) =-v_vert(3)*form*c_topt/(0.d0,1.d0)
 !           f_bar_a_photo_NP(i_c12_2,i_q12)= f_vert*f_iso*&
 !                                    & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! N rho

!---------!
!  N eta  !
!---------!
        i_c12=5

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! eta mass
          f_iso=1.d0         ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)  !CHECK

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

 	        f_vert=(q_12*q_12+Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! D-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! D-wave ! double derivative coupling

 !           f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
 !           f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
 !                                    & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

        end if  ! N eta

!------------!
!  Delta pi  !
!------------!
        i_c12=6
        i_c12_1=i_c12+1

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Delta mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=-dsqrt(2.d0) ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=dsqrt(5.d0/3.d0)  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi)/3.d0

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	    Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)

            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

 	        f_vert=-(Eon_1-m_1)/m_1*(Eon_2+q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13, D-wave
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

 	        f_vert=(Eon_1+2.*m_1)/m_1*(Eon_2+q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  !D13, S-wave
            v_vert(2)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

	        f_bar_c(i_rs,i_c12  ,i_q12) =  v_vert(1)*form*c_topt
	        f_bar_c(i_rs,i_c12_1,i_q12) =  v_vert(2)*form*c_topt

	        f_bar_a(i_c12  ,i_q12,i_rs) =  v_vert(1)*form*c_topt
!	        f_bar_a_photo_P(i_c12  ,i_q12,i_rs) =  v_vert(1)*form*c_topt
!                f_bar_a_photo_NP(i_c12,i_q12)= (-(Eon_1-m_1)/m_1*(Eon_2+q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1))*f_iso*&
!                                     & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt

	        f_bar_a(i_c12_1,i_q12,i_rs) =  v_vert(2)*form*c_topt
!	        f_bar_a_photo_P(i_c12_1,i_q12,i_rs) =  v_vert(2)*form*c_topt
!                f_bar_a_photo_NP(i_c12_1,i_q12)= f_vert*f_iso*&
!                                     & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt

          end do  ! i_q12

        end if  ! Delta pi

!---------!
!  Lam K  !
!---------!
        i_c12=12

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Lambda mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=1.d0  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	    f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
!                                     & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! Lam K

!---------!
!  Sig K  !
!---------!
        i_c12=13

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Sigma mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=-dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	    f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
!                                    & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! Sig K

      !---------!
      !  Lamc D !
      !---------!
              i_c12=14

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Lambda mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=1.d0  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	    f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                  v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                  f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
      !                                     & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                end do  ! i_q12

      	    end if  ! Lamc D

      !---------!
      !  Sigc D !
      !---------!
              i_c12=15

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Sigma mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=-dsqrt(3.d0)  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	    f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                  v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                  f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
      !                                    & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                end do  ! i_q12

      	    end if  ! Sigc D

            !---------!
            ! Lamc Ds !
            !---------!
                    i_c12=16

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Lambda mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=1.d0  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	    f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                        v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                        f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
            !                                     & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                      end do  ! i_q12

            	    end if  ! Lamc Ds

            !---------!
            ! Sigc Ds !
            !---------!
                    i_c12=17

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Sigma mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=-dsqrt(3.d0)  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	    f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                        v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                        f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
            !                                    & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                      end do  ! i_q12

            	    end if  ! Sigc Ds

      end do  ! i_rs=1,max_nr(iv,k_j)

      end subroutine g_res7hm_calc  ! resonance: 7 half minus

!=======================================================================!

!      subroutine g_res3hp_calc(ecm,iv,k_j)  ! resonance: 3 half plus
!      subroutine g_res5hm_calc(ecm,iv,k_j)  ! resonance: 5 half minus
      subroutine g_res7hp_calc(ecm,iv,k_j)  ! resonance: 7 half plus

      implicit none
      complex(kind(0.d0)) :: ecm
	  integer :: k_j,iv  ! used to determine the quantum numbers of the channel
	  integer :: n1  ! n1=n+1, n is the number of mesh points in momentum space
      complex(kind(0.d0)) :: p1_til,term1,term2

      n1=n+1

      f_bar_c = (0.d0,0.d0)  ! initialize resonance creation
      f_bar_a = (0.d0,0.d0)  ! initialize resonance annihilation
!      f_bar_a_photo_P=(0.d0,0.d0)
!      f_bar_a_photo_NP=(0.d0,0.d0)


      do i_rs=1,max_nr(iv,k_j)

!--------!
!  N pi  !
!--------!
        i_c12=1

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3)f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
		if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) then  ! contact term
			g_eff= sqrt( dcmplx( g_bare(iv,k_j,i_rs,i_c12),0.d0) )
			if (g_bare(iv,k_j,i_rs,i_c12).gt.0.d0) then
				cf=dcmplx(1.d0,0.d0)
				else
				cf=dcmplx(0.d0,1.d0)
			end if
		end if
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	    Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	    f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	! (newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                                     & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! N pi

!---------!
!  N rho  !
!---------!
        i_c12=2
        i_c12_1=i_c12+1
        i_c12_2=i_c12+2

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
	      m_2=ms_b_c(i_c12)  ! rho mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi)/3.d0

          do i_q12=1,n1
            q_12 = xgau(i_c12,i_q12)
            Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

 	        f_vert=(q_12-(Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! P-wave, S=1/2
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	! (newres)

 	        f_vert=(5.*q_12+(4.*Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! P-wave, S=3/2
            v_vert(2)=f_vert*g_eff*cls_proj/dsqrt(5.d0)*(q_12/ms_f_c(i_c12))**2	! (newres)

 	        f_vert=(Eon_2-m_2)*q_12/(Eon_1 + m_1)*cdsqrt(Eon_1+m_1)  ! F-wave, S=3/2
            v_vert(3)=f_vert*g_eff*cls_proj/dsqrt(5.d0)*3.d0*(q_12/ms_f_c(i_c12))**2	! (newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12  ,i_q12) = v_vert(1)*form*c_topt  ! P-wave, L=j-1/2, S=1/2
            f_bar_c(i_rs,i_c12_1,i_q12) = v_vert(2)*form*c_topt  ! P-wave, L=j-1/2, S=3/2
            f_bar_c(i_rs,i_c12_2,i_q12) = v_vert(3)*form*c_topt  ! F-wave, L=j+3/2, S=3/2

            f_bar_a(i_c12  ,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_P(i_c12  ,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= ((q_12-(Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1))*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt

            f_bar_a(i_c12_1,i_q12,i_rs) = v_vert(2)*form*c_topt
!            f_bar_a_photo_P(i_c12_1,i_q12,i_rs) = v_vert(2)*form*c_topt
!            f_bar_a_photo_NP(i_c12_1,i_q12)= ((5.*q_12+(4.*Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1))*f_iso*&
!                    & cls_proj/dsqrt(5.d0)*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt

            f_bar_a(i_c12_2,i_q12,i_rs) = v_vert(3)*form*c_topt
!            f_bar_a_photo_P(i_c12_2,i_q12,i_rs) = v_vert(3)*form*c_topt
!            f_bar_a_photo_NP(i_c12_2,i_q12)= f_vert*f_iso*&
!               & cls_proj/dsqrt(5.d0)*3.d0*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

	    end if  ! N rho

!---------!
!  N eta  !
!---------!
        i_c12=5

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! eta mass
          f_iso=1.d0         ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	! (newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                                     & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

        end if  ! N eta

!------------!
!  Delta pi  !
!------------!
        i_c12=6
        i_c12_1=i_c12+1

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Delta mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=-dsqrt(2.d0) ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=dsqrt(5.d0/3.d0)  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/5.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)
            p1_til=q_12/(Eon_1+m_1)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            term1 = 4.d0*q_12**2+3.d0*Eon_1*Eon_2
            term2 = q_12*(5.d0*Eon_1+4.d0*Eon_2)
 	        f_vert=(term1*p1_til-term2)/m_1*cdsqrt(Eon_1+m_1)  ! P-wave, S=3/2
            v_vert(1)=f_vert*g_eff*cls_proj/3.d0*(0.d0,-1.d0)*(q_12/ms_f_c(i_c12))**2	!(newres)

            term1 = q_12**2+2.*Eon_1*Eon_2
            term2 = q_12*Eon_2
 	        f_vert=(term1*p1_til-term2)/m_1*cdsqrt(Eon_1+m_1)  ! F-wave, S=3/2
            v_vert(2)=f_vert*g_eff*cls_proj*(0.d0,-1.d0)*(q_12/ms_f_c(i_c12))**2	!(newres)

	        f_bar_c(i_rs,i_c12  ,i_q12) =  v_vert(1)*form*c_topt
	        f_bar_c(i_rs,i_c12_1,i_q12) =  v_vert(2)*form*c_topt

	        f_bar_a(i_c12  ,i_q12,i_rs) = -v_vert(1)*form*c_topt
!	        f_bar_a_photo_P(i_c12  ,i_q12,i_rs) = -v_vert(1)*form*c_topt/(0.d0,1.d0)
!                f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*(((4.d0*q_12**2+3.d0*Eon_1*Eon_2)*p1_til-(q_12*(5.d0*Eon_1+4.d0*Eon_2)))/&
!             & m_1*cdsqrt(Eon_1+m_1))*f_iso*cls_proj/3.d0*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt

	        f_bar_a(i_c12_1,i_q12,i_rs) = -v_vert(2)*form*c_topt
!	        f_bar_a_photo_P(i_c12_1,i_q12,i_rs) = -v_vert(2)*form*c_topt/(0.d0,1.d0)
!                f_bar_a_photo_NP(i_c12_1,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                            & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

        end if  ! Delta pi

!---------!
!  Lam K  !
!---------!
        i_c12=12

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Lambda mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=1.d0  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! Lam K

!---------!
!  Sig K  !
!---------!
        i_c12=13

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Sigma mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=-dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


          end do  ! i_q12

	    end if  ! Sig K

      !---------!
      !  Lamc D !
      !---------!
              i_c12=14

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Lambda mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=1.d0  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                  v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                  f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
      !                                & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                end do  ! i_q12

      	    end if  ! Lamc D

      !---------!
      !  Sigc D !
      !---------!
              i_c12=15

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Sigma mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=-dsqrt(3.d0)  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                  v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                  f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
      !                                & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                end do  ! i_q12

      	    end if  ! Sigc D

            !---------!
            ! Lamc Ds !
            !---------!
                    i_c12=16

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Lambda mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=1.d0  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                        v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                        f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
            !                                & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                      end do  ! i_q12

            	    end if  ! Lamc Ds

            !---------!
            ! Sigc Ds !
            !---------!
                    i_c12=17

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Sigma mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=-dsqrt(3.d0)  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                        v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**2	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                        f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
            !                                & cls_proj*(q_12/ms_f_c(i_c12))**2*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**2*c_topt


                      end do  ! i_q12

            	    end if  ! Sigc Ds

      end do  ! i_rs=1,max_nr(iv,k_j)

!      end subroutine g_res5hm_calc  ! resonance: 5 half minus
      end subroutine g_res7hp_calc  ! resonance: 7 half plus

!=======================================================================!


!      subroutine g_res3hm_calc(ecm,iv,k_j)  ! resonance: 3 half minus
!      subroutine g_res5hp_calc(ecm,iv,k_j)  ! resonance: 5 half plus renamed! (newres)
!      subroutine g_res7hm_calc(ecm,iv,k_j)  ! resonance: 7 half minus renamed! (newres)
      subroutine g_res9hp_calc(ecm,iv,k_j)  ! resonance: 9 half plus renamed! (newres)

      implicit none
      complex(kind(0.d0)) :: ecm
	  integer :: k_j,iv  ! used to determine the quantum numbers of the channel
	  integer :: n1  ! n1=n+1, n is the number of mesh points in momentum space

      n1=n+1

      f_bar_c = (0.d0,0.d0)  ! initialize resonance creation
      f_bar_a = (0.d0,0.d0)  ! initialize resonance annihilation
!      f_bar_a_photo_P=(0.d0,0.d0)
!      f_bar_a_photo_NP=(0.d0,0.d0)


      do i_rs=1,max_nr(iv,k_j)

!--------!
!  N pi  !
!--------!
        i_c12=1

	  if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
		if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) then  ! contact term
			g_eff= sqrt( dcmplx( g_bare(iv,k_j,i_rs,i_c12),0.d0) )
			if (g_bare(iv,k_j,i_rs,i_c12).gt.0.d0) then
				cf=dcmplx(1.d0,0.d0)
				else
				cf=dcmplx(0.d0,1.d0)
			end if
		end if
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	    f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! before D13; will become F wave now:
!           v_vert(1)=f_vert*g_eff*cls_proj			! OLD (D-wave)
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	! (newres) Here, the factor q/m_nucleon is put which makes the correct L !!!!!

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres) This line was missing before; the power of the Form factor was one, now it is higher and we have to say
	    							! this explictly!
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
!                                     & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

	    end if  ! N pi

!---------!
!  N rho  !
!---------!
        i_c12=2
        i_c12_1=i_c12+1
        i_c12_2=i_c12+2

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
	      m_2=ms_b_c(i_c12)  ! rho mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi)/3.d0

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
            Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

 	        f_vert=(0.d0,1.d0)*(-q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1)  ! D-wave, S=1/2
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

 	        f_vert=(0.d0,1.d0)*(2.*q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1)  ! D-wave, S=3/2
            v_vert(2)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

 	        f_vert=(0.d0,1.d0)*( q_12*q_12/(Eon_1 + m_1)+2.d0*Eon_2+m_2)*cdsqrt(Eon_1+m_1)  ! S-wave, S=3/2
            v_vert(3)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt( 0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12  ,i_q12) = v_vert(1)*form*c_topt  ! D-wave, L=j+1/2, S=1/2
            f_bar_c(i_rs,i_c12_1,i_q12) = v_vert(2)*form*c_topt  ! D-wave, L=j+1/2, S=3/2
            f_bar_c(i_rs,i_c12_2,i_q12) = v_vert(3)*form*c_topt  ! S-wave, L=j-3/2, S=3/2

            f_bar_a(i_c12  ,i_q12,i_rs) =-v_vert(1)*form*c_topt
!            f_bar_a_photo_P(i_c12  ,i_q12,i_rs) =-v_vert(1)*form*c_topt/(0.d0,1.d0)
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,1.d0)*((-q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1))*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


            f_bar_a(i_c12_1,i_q12,i_rs) =-v_vert(2)*form*c_topt
!            f_bar_a_photo_P(i_c12_1,i_q12,i_rs) =-v_vert(2)*form*c_topt/(0.d0,1.d0)
!            f_bar_a_photo_NP(i_c12_1,i_q12)= (0.d0,1.d0)*((2.*q_12*q_12/(Eon_1 + m_1)+Eon_2-m_2)*cdsqrt(Eon_1+m_1))*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt

            f_bar_a(i_c12_2,i_q12,i_rs) =-v_vert(3)*form*c_topt
!            f_bar_a_photo_P(i_c12_2,i_q12,i_rs) =-v_vert(3)*form*c_topt/(0.d0,1.d0)
!            f_bar_a_photo_NP(i_c12_2,i_q12)= f_vert*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

	    end if  ! N rho

!---------!
!  N eta  !
!---------!
        i_c12=5

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! eta mass
          f_iso=1.d0         ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)  !CHECK

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

 	        f_vert=(q_12*q_12+Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! D-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! D-wave ! double derivative coupling

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
!                               & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

        end if  ! N eta

!------------!
!  Delta pi  !
!------------!
        i_c12=6
        i_c12_1=i_c12+1

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Delta mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=-dsqrt(2.d0) ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=dsqrt(5.d0/3.d0)  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi)/3.d0

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	        form = form**npow_ff(iv,k_j,i_rs,i_c12)

            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

 	        f_vert=-(Eon_1-m_1)/m_1*(Eon_2+q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13, D-wave
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

 	        f_vert=(Eon_1+2.*m_1)/m_1*(Eon_2+q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  !D13, S-wave
            v_vert(2)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

	        f_bar_c(i_rs,i_c12  ,i_q12) =  v_vert(1)*form*c_topt
	        f_bar_c(i_rs,i_c12_1,i_q12) =  v_vert(2)*form*c_topt

	        f_bar_a(i_c12  ,i_q12,i_rs) =  v_vert(1)*form*c_topt
!	        f_bar_a_photo_P(i_c12  ,i_q12,i_rs) =  v_vert(1)*form*c_topt
!                f_bar_a_photo_NP(i_c12,i_q12)= (-(Eon_1-m_1)/m_1*(Eon_2+q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1))*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt

	        f_bar_a(i_c12_1,i_q12,i_rs) =  v_vert(2)*form*c_topt
!	        f_bar_a_photo_P(i_c12_1,i_q12,i_rs) =  v_vert(2)*form*c_topt
!                f_bar_a_photo_NP(i_c12_1,i_q12)= f_vert*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

        end if  ! Delta pi

!---------!
!  Lam K  !
!---------!
        i_c12=12

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Lambda mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=1.d0  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

	    end if  ! Lam K

!---------!
!  Sig K  !
!---------!
        i_c12=13

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Sigma mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=-dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
            f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

	    end if  ! Sig K

      !---------!
      !  Lamc D !
      !---------!
              i_c12=14

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Lambda mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=1.d0  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                  v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                  f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
      !                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


                end do  ! i_q12

      	    end if  ! Lamc D

      !---------!
      !  Sigc D !
      !---------!
              i_c12=15

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Sigma mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=-dsqrt(3.d0)  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                  v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                  f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
      !                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


                end do  ! i_q12

      	    end if  ! Sigc D

            !---------!
            ! Lamc Ds !
            !---------!
                    i_c12=16

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Lambda mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=1.d0  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                        v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                        f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
            !                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


                      end do  ! i_q12

            	    end if  ! Lamc Ds

            !---------!
            ! Sigc Ds !
            !---------!
                    i_c12=17

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Sigma mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=-dsqrt(3.d0)  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	        f_vert=(q_12*q_12 + Eon_2*q_12*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! D13
                        v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = v_vert(1)*form*c_topt  ! S-wave
                        f_bar_a(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt  ! S-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= f_vert*f_iso*&
            !                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


                      end do  ! i_q12

            	    end if  ! Sigc Ds

      end do  ! i_rs=1,max_nr(iv,k_j)

!      end subroutine g_res7hm_calc  ! resonance: 7 half minus
      end subroutine g_res9hp_calc  ! resonance: 9 half plus

!=======================================================================!

!      subroutine g_res3hp_calc(ecm,iv,k_j)  ! resonance: 3 half plus
!      subroutine g_res5hm_calc(ecm,iv,k_j)  ! resonance: 5 half minus
!      subroutine g_res7hp_calc(ecm,iv,k_j)  ! resonance: 7 half plus
      subroutine g_res9hm_calc(ecm,iv,k_j)  ! resonance: 9 half minus

      implicit none
      complex(kind(0.d0)) :: ecm
      integer :: k_j,iv  ! used to determine the quantum numbers of the channel
      integer :: n1  ! n1=n+1, n is the number of mesh points in momentum space
      complex(kind(0.d0)) :: p1_til,term1,term2

      n1=n+1

      f_bar_c = (0.d0,0.d0)  ! initialize resonance creation
      f_bar_a = (0.d0,0.d0)  ! initialize resonance annihilation
!      f_bar_a_photo_P=(0.d0,0.d0)
!      f_bar_a_photo_NP=(0.d0,0.d0)


      do i_rs=1,max_nr(iv,k_j)

!--------!
!  N pi  !
!--------!
        i_c12=1

	  if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3)f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
		if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) then  ! contact term
			g_eff= sqrt( dcmplx( g_bare(iv,k_j,i_rs,i_c12),0.d0) )
			if (g_bare(iv,k_j,i_rs,i_c12).gt.0.d0) then
				cf=dcmplx(1.d0,0.d0)
				else
				cf=dcmplx(0.d0,1.d0)
			end if
		end if
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	    f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	! (newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

	    end if  ! N pi

!---------!
!  N rho  !
!---------!
        i_c12=2
        i_c12_1=i_c12+1
        i_c12_2=i_c12+2

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
	      m_2=ms_b_c(i_c12)  ! rho mass
          f_iso=dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi)/3.d0

          do i_q12=1,n1
            q_12 = xgau(i_c12,i_q12)
            Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

 	        f_vert=(q_12-(Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! P-wave, S=1/2
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	! (newres)

 	        f_vert=(5.*q_12+(4.*Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1)  ! P-wave, S=3/2
            v_vert(2)=f_vert*g_eff*cls_proj/dsqrt(5.d0)*(q_12/ms_f_c(i_c12))**3	! (newres)

 	        f_vert=(Eon_2-m_2)*q_12/(Eon_1 + m_1)*cdsqrt(Eon_1+m_1)  ! F-wave, S=3/2
            v_vert(3)=f_vert*g_eff*cls_proj/dsqrt(5.d0)*3.d0*(q_12/ms_f_c(i_c12))**3	! (newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12  ,i_q12) = v_vert(1)*form*c_topt  ! P-wave, L=j-1/2, S=1/2
            f_bar_c(i_rs,i_c12_1,i_q12) = v_vert(2)*form*c_topt  ! P-wave, L=j-1/2, S=3/2
            f_bar_c(i_rs,i_c12_2,i_q12) = v_vert(3)*form*c_topt  ! F-wave, L=j+3/2, S=3/2

            f_bar_a(i_c12  ,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_P(i_c12  ,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= ((q_12-(Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1))*f_iso*&
!                               & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**4*c_topt

            f_bar_a(i_c12_1,i_q12,i_rs) = v_vert(2)*form*c_topt
!            f_bar_a_photo_P(i_c12_1,i_q12,i_rs) = v_vert(2)*form*c_topt
!            f_bar_a_photo_NP(i_c12_1,i_q12)= ((5.*q_12+(4.*Eon_2+m_2)*q_12/(Eon_1 + m_1))*cdsqrt(Eon_1+m_1))*f_iso*&
!                    & cls_proj/dsqrt(5.d0)*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**4*c_topt

            f_bar_a(i_c12_2,i_q12,i_rs) = v_vert(3)*form*c_topt
!            f_bar_a_photo_P(i_c12_2,i_q12,i_rs) = v_vert(3)*form*c_topt
!            f_bar_a_photo_NP(i_c12_2,i_q12)= f_vert*f_iso*&
!               & cls_proj/dsqrt(5.d0)*3.d0*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**4*c_topt


          end do  ! i_q12

	    end if  ! N rho

!---------!
!  N eta  !
!---------!
        i_c12=5

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! nucleon mass
          m_2=ms_b_c(i_c12)  ! eta mass
          f_iso=1.d0         ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	! (newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

        end if  ! N eta

!------------!
!  Delta pi  !
!------------!
        i_c12=6
        i_c12_1=i_c12+1

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Delta mass
          m_2=ms_b_c(i_c12)  ! pion mass
          f_iso=-dsqrt(2.d0) ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=dsqrt(5.d0/3.d0)  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/5.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)
            p1_til=q_12/(Eon_1+m_1)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            term1 = 4.d0*q_12**2+3.d0*Eon_1*Eon_2
            term2 = q_12*(5.d0*Eon_1+4.d0*Eon_2)
 	        f_vert=(term1*p1_til-term2)/m_1*cdsqrt(Eon_1+m_1)  ! P-wave, S=3/2
            v_vert(1)=f_vert*g_eff*cls_proj/3.d0*(0.d0,-1.d0)*(q_12/ms_f_c(i_c12))**3	!(newres)

            term1 = q_12**2+2.*Eon_1*Eon_2
            term2 = q_12*Eon_2
 	        f_vert=(term1*p1_til-term2)/m_1*cdsqrt(Eon_1+m_1)  ! F-wave, S=3/2
            v_vert(2)=f_vert*g_eff*cls_proj*(0.d0,-1.d0)*(q_12/ms_f_c(i_c12))**3	!(newres)

	        f_bar_c(i_rs,i_c12  ,i_q12) =  v_vert(1)*form*c_topt
	        f_bar_c(i_rs,i_c12_1,i_q12) =  v_vert(2)*form*c_topt

	        f_bar_a(i_c12  ,i_q12,i_rs) = -v_vert(1)*form*c_topt
!	        f_bar_a_photo_P(i_c12  ,i_q12,i_rs) = -v_vert(1)*form*c_topt/(0.d0,1.d0)
!                f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*(((4.d0*q_12**2+3.d0*Eon_1*Eon_2)*p1_til-(q_12*(5.d0*Eon_1+4.d0*Eon_2)))/m_1*cdsqrt(Eon_1+m_1))*f_iso*&
!                           & cls_proj/3.d0*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**4*c_topt

	        f_bar_a(i_c12_1,i_q12,i_rs) = -v_vert(2)*form*c_topt
!	        f_bar_a_photo_P(i_c12_1,i_q12,i_rs) = -v_vert(2)*form*c_topt/(0.d0,1.d0)
!                f_bar_a_photo_NP(i_c12_1,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                                     & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**4*c_topt


          end do  ! i_q12

        end if  ! Delta pi

!---------!
!  Lam K  !
!---------!
        i_c12=12

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Lambda mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=1.d0  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                              & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

	    end if  ! Lam K

!---------!
!  Sig K  !
!---------!
        i_c12=13

	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

          m_1=ms_f_c(i_c12)  ! Sigma mass
          m_2=ms_b_c(i_c12)  ! kaon mass
          f_iso=-dsqrt(3.d0)  ! I=1/2
          if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

          g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
          Lam  = lam_ff(iv,k_j,i_rs,i_c12)
          mff  =   m_ff(iv,k_j,i_rs,i_c12)
          cls_proj=dsqrt(fpi/3.d0)

          do i_q12=1,n1

            q_12 = xgau(i_c12,i_q12)
	        Eon_1=cdsqrt(q_12**2+m_1**2)
            Eon_2=cdsqrt(q_12**2+m_2**2)

   	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
            v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

            form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
            c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

            f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
            f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

!            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
!            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
!                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


          end do  ! i_q12

	    end if  ! Sig K

      !---------!
      !  Lamc D !
      !---------!
              i_c12=14

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Lambda mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=1.d0  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                  v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                  f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
      !                              & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


                end do  ! i_q12

      	    end if  ! Lamc D

      !---------!
      !  Sigc D !
      !---------!
              i_c12=15

      	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                m_1=ms_f_c(i_c12)  ! Sigma mass
                m_2=ms_b_c(i_c12)  ! kaon mass
                f_iso=-dsqrt(3.d0)  ! I=1/2
                if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
      	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                mff  =   m_ff(iv,k_j,i_rs,i_c12)
                cls_proj=dsqrt(fpi/3.d0)

                do i_q12=1,n1

                  q_12 = xgau(i_c12,i_q12)
      	        Eon_1=cdsqrt(q_12**2+m_1**2)
                  Eon_2=cdsqrt(q_12**2+m_2**2)

         	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                  v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

                  form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
      	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                  c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                  f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                  f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

      !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
      !            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
      !                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


                end do  ! i_q12

      	    end if  ! Sigc D

            !---------!
            ! Lamc Ds !
            !---------!
                    i_c12=16

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Lambda mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=1.d0  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=0.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                        v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                        f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
            !                              & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


                      end do  ! i_q12

            	    end if  ! Lamc Ds

            !---------!
            ! Sigc Ds !
            !---------!
                    i_c12=17

            	    if(opt_res_c(iv,k_j,i_rs,i_c12)==1)then

                      m_1=ms_f_c(i_c12)  ! Sigma mass
                      m_2=ms_b_c(i_c12)  ! kaon mass
                      f_iso=-dsqrt(3.d0)  ! I=1/2
                      if(qn_2iso(iv,k_j)==3) f_iso=-1.d0  ! I=3/2

                      g_eff= g_bare(iv,k_j,i_rs,i_c12)*f_iso
            	  if (i_rs==max_nr(iv,k_j).and.ct_on(iv,k_j)==1) g_eff= g_eff*cf ! contact term
                      Lam  = lam_ff(iv,k_j,i_rs,i_c12)
                      mff  =   m_ff(iv,k_j,i_rs,i_c12)
                      cls_proj=dsqrt(fpi/3.d0)

                      do i_q12=1,n1

                        q_12 = xgau(i_c12,i_q12)
            	        Eon_1=cdsqrt(q_12**2+m_1**2)
                        Eon_2=cdsqrt(q_12**2+m_2**2)

               	        f_vert=q_12*cdsqrt(Eon_1+m_1)  ! P13
                        v_vert(1)=f_vert*g_eff*cls_proj*(q_12/ms_f_c(i_c12))**3	!(newres)

                        form = (Lam**4+mff**4)/(Lam**4+(Eon_1+Eon_2)**4)
            	    form = form**npow_ff(iv,k_j,i_rs,i_c12)   		! (newres)
                        c_topt=cdsqrt(0.25d0/Eon_1/Eon_2)/rtpi3

                        f_bar_c(i_rs,i_c12,i_q12) = (0.d0,-1.d0)* v_vert(1)*form*c_topt  ! P-wave
                        f_bar_a(i_c12,i_q12,i_rs) = (0.d0,1.d0)* v_vert(1)*form*c_topt   ! P-wave

            !            f_bar_a_photo_P(i_c12,i_q12,i_rs) = v_vert(1)*form*c_topt
            !            f_bar_a_photo_NP(i_c12,i_q12)= (0.d0,-1.d0)*f_vert*f_iso*&
            !                                & cls_proj*(q_12/ms_f_c(i_c12))**3*((2000.d0**4+2000.d0**4)/(2000.d0**4+(Eon_1+Eon_2)**4))**3*c_topt


                      end do  ! i_q12

            	    end if  ! Sigc Ds

      end do  ! i_rs=1,max_nr(iv,k_j)

!      end subroutine g_res5hm_calc  ! resonance: 5 half minus
!      end subroutine g_res7hp_calc  ! resonance: 7 half plus
      end subroutine g_res9hm_calc  ! resonance: 9 half minus
      end module vertices_5h7h	!(newres)
