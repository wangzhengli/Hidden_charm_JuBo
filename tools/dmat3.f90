! double precision version.
! set to F90 format by o.krehl
!     +FACT*******************************************************FACT+
!     *       CALCULATES  LN (N-FACTORIAL) FOR N=0,1,2,...,99         *
!     *       ===============================================         *
!     *       COMMON /FAK/ :   FCC(N)=LN(N|) ; N=0,1,...,99           *
!     +*************CVAMPIR************07-SEP-87**********************+

      SUBROUTINE FACT

      IMPLICIT REAL(kind(0.0d0)) (A - H, O - Z)
      PARAMETER (ZERO = 0.0d0)
      COMMON / FAK / FCC (100)

      FCC (1) = ZERO
      FCC (2) = ZERO
      DO K = 3, 100
        J = K - 1
        T = FLOAT (J)
        FCC (K) = FCC (J) + DLOG (T)
      END DO

	  RETURN
      END

!     +DMATRX***************************************************DMATRX+
!     *      CALCULATES SMALL D-FUNCTIONS AS DEFINED BY EDMONDS       *
!     *      ==================================================       *
!     *  INPUT :     BETA : ANGLE (IN RADIAN)                         *
!     *  =======     J    : 2*J                                       *
!     *  (VIA        MPR  : 2*MP                                      *
!     *   CALLING    M    : 2*M                                       *
!     *   LIST   )                                                    *
!     * ------------------------------------------------------------- *
!     *  (IN ADD.)   COMMON /FAK/ (FROM FACT)                         *
!     * ------------------------------------------------------------- *
!     *  OUTPUT :    DROT : D(J,MP,M;BETA)                            *
!     *  ========                                                     *
!     *  (VIA        IER  : 0  O.K.                                   *
!     *   CALLING           1  J<0                                    *
!     *   LIST   )          2  ABS(MP)>J OR ABS(M)>J                  *
!     *                     3  INTEGERS AND HALF-INTEGERS MIXED       *
!     *                     4  MA<MI                                  *
!     *                     5  TOO HIGH FACTORIALS REQUIRED           *
!     +*************CVAMPIR************07-SEP-87**********************+

      SUBROUTINE DMATRX (BETA, J, MPR, M, DROT, IER)

      IMPLICIT REAL(kind(0.0d0)) (A - H, O - Z)
      LOGICAL LSC, LSS
      PARAMETER (ZERO = 0.0d0, HALF = 0.5d0)
      COMMON / FAK / FCC (100)

      DROT = ZERO

      IER = 1
      IF (J.GE.0) THEN
        I1 = IABS (MPR)
        I2 = IABS (M)
        IER = 2
        IF ( (I1.LE.J) .AND. (I2.LE.J) ) THEN
          I3 = MOD (J, 2)
          IER = 3
          IF ( (MOD (I1, 2) .EQ.I3) .AND. (MOD (I2, 2) .EQ.I3) ) THEN
            I2 = (J - MPR) / 2 + 1
            I4 = (J - M) / 2 + 1
            MA = MIN (I2, I4)
            I5 = - (MPR + M) / 2
            MI = MAX (I5, 0) + 1
            IER = 4
            IF (MA.GE.MI) THEN
              I5 = - I5
              I1 = (J + MPR) / 2 + 1
              I3 = (J + M) / 2 + 1
              IP = MAX (I1, I2)
              IP = MAX (IP, I3)
              IP = MAX (IP, I4)
              IER = 5
              IF (IP.LE.100) THEN
                IER = 0
                F1 = HALF * (FCC (I1) + FCC (I2) - FCC (I3) - FCC (I4) )
                BET = HALF * BETA
                CC = DCOS (BET)
                SS = DSIN (BET)
                LSC = CC.LT.ZERO
                LSS = SS.LT.ZERO
                CC = DLOG (DABS (CC) )
                SS = DLOG (DABS (SS) )
                SUM = ZERO
                DO L = MI, MA
                  K = L - 1
                  KC = K + K + I5
                  KS = J - KC
                  F2 = FCC (I3) - FCC (I2 - K) - FCC (I5 + L) + FCC (I4)&
                  & - FCC (L) - FCC (I4 - K) + FLOAT (KC) * CC + FLOAT (KS) * SS
                  F2 = EXP (F2)
                  IP = I2 - L
                  IF (LSC) IP = IP + KC
                  IF (LSS) IP = IP + KS
                  IP = MOD (IP, 2)
                  IF (IP.NE.0) F2 = - F2
                  SUM = SUM + F2
                END DO
                F1 = EXP (F1) * SUM
                DROT = F1
              END IF
            END IF
          END IF
        END IF
      END IF

      RETURN
      END
