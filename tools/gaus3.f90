      SUBROUTINE LGAUSS (XG, WG, NGMAX)
! GENERATION OF LEGENDRE-GAUSS MESH POINTS AND WEIGHTS

      IMPLICIT REAL(kind(0.0d0)) (A - H, O - Z)
      REAL(kind(0.0d0)) :: XG (199), WG (199)
      INTEGER NGMAX
      DATA EPS / 1.0d-12 /
      DIMENSION FLDR (200)

      PAI = DACOS ( - 1.0d0)
      FN = NGMAX
      DS = PAI / (FN + 0.5d0)
      S1 = - 0.25d0 * DS
      L1MAX = NGMAX + 1
      NGMAX2 = NGMAX - NGMAX / 2
      DO 100 K = 1, NGMAX2
        S1 = S1 + DS
        X0 = DCOS (S1)
        DO 50 I = 1, 999
          CALL LGDR (X0, FLDR, L1MAX)
          PNM1 = FLDR (NGMAX)
          PN = FLDR (L1MAX)
          PND = FN * (PNM1 - X0 * PN) / (1.0d0 - X0 * X0)
          X1 = X0 - PN / PND
          IF (DABS (X0 - X1) .LT. EPS) GOTO 60
          X0 = X1
   50   END DO
   60   WGHT = 2.0d0 * (1.0d0 - X1 * X1) / (FN * FN * PNM1 * PNM1)
        KMG = NGMAX - K + 1
        XG (KMG) = X1
        XG (K) = - X1
        WG (KMG) = WGHT
        WG (K) = WGHT
  100 END DO
      
	  RETURN
      END
      
!=======================================================================!

	  SUBROUTINE LGDR (X, FLDR, L1MAX)

      IMPLICIT REAL(kind(0.0d0)) (A - H, O - Z)
      DIMENSION FLDR (L1MAX)

      FLDR (1) = 1.0d0
      FLDR (2) = X
      IF (L1MAX.LE.2) RETURN
      ALDR0 = 1.0d0
      ALDR1 = X
      DO 20 L1 = 3, L1MAX
        L = L1 - 1
        FL = L
        ALDR2 = ( (2.0d0 * FL - 1.0d0) * X * ALDR1 - (FL - 1.0d0) * ALDR0) &
     &  / FL
        FLDR (L1) = ALDR2
        ALDR0 = ALDR1
        ALDR1 = ALDR2
   20 END DO
      
	  RETURN
      END

!=======================================================================!

      SUBROUTINE gauss_dsp (nd, a, b, x_dsp, w_dsp)

      INTEGER nd     ! input  number of mesh points
      REAL(kind(0.0d0)) :: a     ! input begin of integration
      REAL(kind(0.0d0)) :: b     ! input end of integration
      REAL(kind(0.0d0)) :: x_dsp (24), w_dsp (24)     ! output
      REAL(kind(0.0d0)) :: xg (199), wg (199)     ! intermediate storage
      INTEGER i

      CALL lgauss (xg, wg, nd)

      do i = 1, nd
      x_dsp (i) = 0.5d0 * (b - a) * xg (i) + 0.5d0 * (b + a)
      w_dsp (i) = 0.5d0 * (b - a) * wg (i)
      end do

      RETURN
      END
      
!=======================================================================!

	  SUBROUTINE gauss_tan (n, c, alpha, xgau, wgau)

      IMPLICIT real(kind(0.0d0)) (a - h, o - z)
      COMPLEX(kind(0.0d0)) :: xgau (99), wgau (99)     ! output
      REAL(kind(0.0d0)) :: c     ! input  scale of the momentum variables
      REAL(kind(0.0d0)) :: alpha     ! input  angle of rotated path
      INTEGER n     ! input  number of mesh points
      REAL(kind(0.0d0)) :: xg (199), wg (199)     ! intermediate storage

      CALL lgauss (xg, wg, n)
      pih = dacos (0.0d0)
      do i = 1, n
      xx = pih * (xg (i) + 1.0d0) / 2.0d0
      x_til = tan (xx) * c
      dc = 1.0d0 / dcos (xx)
      w_til = pih * c * dc * dc * wg (i) / 2.0d0
      x_til_im = - alpha * x_til
      w_til_im = - alpha * w_til
      xgau (i) = cmplx (x_til, x_til_im,kind(0.0d0))
      wgau (i) = cmplx (w_til, w_til_im,kind(0.0d0))
      end do

      RETURN
      END



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      
      SUBROUTINE gauss_tan_MOD2 (n77, c77, alpha77, xgau77, wgau77)   
      use input4
	implicit	none
!      IMPLICIT real(kind(0.0d0)) (a - h, o - z)
      real(kind(0.0d0))   ::x_til77,w_til77, x_til_im, w_til_im                         
      COMPLEX(kind(0.0d0)) :: xgau77 (99), wgau77 (99)
      REAL(kind(0.0d0)) :: c77,stretchfac
      REAL(kind(0.0d0)) :: alpha77,zreal
      INTEGER n77,i77                                                        
      REAL(kind(0.0d0)) :: xg77 (199), wg77 (199)
      zreal=dble(zrem)
!      write(*,'(a4,F6.1,a5,F5.1,a9,F5.2)')'z = ', dble(zrem),' + i ',aimag(zrem),' alpha = ', alpha77
      stretchfac=30.d0+0.26d0*(zreal-1100.d0)+0.00009d0*(zreal-1100.d0)**2
      if (stretchfac.lt.80.d0) then
      stretchfac=80.d0
      end if
                                                                       
      CALL lgauss (xg77, wg77, n77)                                            
      DO i77 = 1, n77                                                        
      x_til77 = stretchfac*(3.d0*log(2.d0+(xg77(i77))**3)-1.d0/(xg77(i77)-1.d0)-1.d0/2.d0)                                               
      w_til77 = stretchfac*(1.d0/(xg77(i77)-1.d0)**2+9.d0*xg77(i77)**2/(1.d0*(xg77(i77)**3+2.d0)))*wg77(i77)
      x_til_im = - alpha77 * x_til77                                         
      w_til_im = - alpha77 * w_til77                                         
      xgau77 (i77) = cmplx (x_til77, x_til_im,kind(0.0d0))
      wgau77 (i77) = cmplx (w_til77, w_til_im,kind(0.0d0))
      enddo                                                              
      RETURN                                                             
      END                                           
