*******************************************************************************
*  Proposito:        Integracion numerica por Gauss (funciones reales y complejas)
*
*      SUBROUTINE DSG20R(A,B,N,X,NP)----->Realiza la particion
*                Entradas: A--->limite inferior
*                          B--->limite superior
*                          N--->Numero de puntos Gauss/20 (n=1 gauss 20 puntos
*                                                        (n=2 gauss 40 puntos,..)
*                Salidas:  X--->Particion de la variable (ha de ser dimensionada)
*                          NP-->Numero de puntos gauss (20*n)
*
*      SUBROUTINE DRG20C(A,B,N,CF,CRES)--->Integracion propiamente dicha
*                Entradas: A,B,N-->Idem antes
*                          CF----->Valor de la funcion a integrar en los
*                                  puntos X, CF(i)=funcion(x(i))
*                                  ha de ser dimensionada
*                Salidas:  CRES--->Resultado de la integral
*                                  En caso de integracion en varias variables,
*                                  CRES pasa a ser la entrada CF de la siguiente
*                                  integral.
*      SUBROUTINE  DRG20R(A,B,N,F,RES)
*                        Idem funciones reales
*	
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c begin integration subroutines		
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE DSG20R(A,B,N,X,NP)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION Y(10),X(2000)
      DATA Y/.9931285991d0,.9639719272d0,.9122344282d0,.8391169718d0,
     F .7463319064d0,.6360536807d0,.5108670019d0,.3737060887d0,
     F .2277858511d0,.0765265211d0/
      NP=20*N
      XN= N
      DINT=B - A
      DINT = DINT/XN
      DELT=DINT*0.5D0
      ORIG=A-DELT
      I1=-20
      DO 1 I=1,N
      ORIG=ORIG+DINT
      DORIG=ORIG+ORIG
      I1=I1+20
      I2=I1+21
      DO 2 J=1,10
      J1=I1+J
      J2=I2-J
      X(J1)=ORIG-DELT*Y(J)
 2    X(J2)=DORIG-X(J1)
 1    CONTINUE
      RETURN
      END
	
      SUBROUTINE DRG20C(A,B,N,CF,CRES)
      IMPLICIT REAL*8 (A,B,D-H,O-Z)
      IMPLICIT complex*16(C)
      DIMENSION W(10),CF(2000)
      DATA W/.0176140071d0,.0406014298d0,.0626720483d0,.0832767415d0,
     F .1019301198d0,.1181945319d0,.1316886384d0,.1420961093d0,
     f .1491729864d0,.1527533871d0/
      CR=(0.D0,0.D0)
      I1=-20
      DO 1 I=1,N
      I1=I1+20
      I2=I1+21
      DO 2 J=1,10
      J1=I1+J
      J2=I2-J
 2    CR=CR+W(J)*(CF(J1)+CF(J2))
 1    CONTINUE
      CRES=CR*0.5D0*(B-A)/DBLE(N)
      RETURN
      END
	
      SUBROUTINE DRG20R(A,B,N,CF,CRES)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION W(10),CF(2000)
      DATA W/.0176140071d0,.0406014298d0,.0626720483d0,.0832767415d0,
     F .1019301198d0,.1181945319d0,.1316886384d0,.1420961093d0,
     f .1491729864d0,.1527533871d0/
      CR=(0.D0,0.D0)
      I1=-20
      DO 1 I=1,N
      I1=I1+20
      I2=I1+21
      DO 2 J=1,10
      J1=I1+J
      J2=I2-J
 2    CR=CR+W(J)*(CF(J1)+CF(J2))
 1    CONTINUE
      CRES=CR*0.5D0*(B-A)/DBLE(N)
      RETURN
      END
	
         SUBROUTINE PVG48(A,B,Z,CF,X,CR,CRES)          
C   CALCULA LA PARTE PRINCIPAL PARA UN POLO SIMPLE CON UN GAUSS 48
        implicit real*8 (a,b,d,e,f,g,h,o,p,q,r,s,t,u,v,w,x,y,z)
         IMPLICIT complex*16 (C) 
         DIMENSION CF(48),CF1(48),Z(48),XD(20),XF(20) 
         IF((X-A)*(X-B).GT.0.d0) GO TO 3                  
         ROS=21.d0/abs(B-A)**2             
         A1=2.d0*X-A              
         CALL STGS20(A1,B,XD)               
         DO 1 I=1,20            
  1        XF(I)=exp(-ROS*(XD(I)-X)**2)/(XD(I)-X)  
         CALL REGS20(A1,B,XF,PV)            
         DO 2 I=1,48
  2        CF1(I)=CF(I)-EXP(-ROS*(Z(I)-X)**2)/(Z(I)-X)*CR 
         DT=abs(B-A)/48.d0/10.d0
         I0=(X-A)/(B-A)*48.d0-4.d0
         IF(I0.LT.1) I0=1
         DS=abs(B-A)
         DO 10 IP=I0,48
         DS1=DS
         DS=abs(X-Z(IP))
         IF(DS.GT.DS1) GO TO 11
         IF(DS.GT.DT) GO TO 10
         IF(IP.EQ.1) GO TO 12
         IF(IP.EQ.48) GO TO 13
         IF((dble(CF1(IP+1))-dble(CF1(IP)))*(dble(CF1(IP))-
     f        dble(CF1(IP-1))).LT.0.d0) GO TO 14
         IF((dimag(CF1(IP+1))-dimag(CF1(IP)))*(dimag(CF1(IP))-
     f        dimag(CF1(IP-1))).LT.0.d0) GO TO 14
 10        CONTINUE
         GO TO 11
 12        CF1(1)=(CF1(2)*(Z(1)-Z(3))-CF1(3)*(Z(1)-Z(2)))/(Z(2)-Z(3))
         GO TO 11
 13        CF1(48)=(CF1(47)*(Z(48)-Z(46))-CF1(46)*(Z(48)-Z(47)))/(Z(47)
     f        -Z(46))
         GO TO 11
 14        CF1(IP)=(CF1(IP+1)*(Z(IP)-Z(IP-1))-CF1(IP-1)*(Z(IP)-Z(IP+1)))
     f        /(Z(IP+1)-Z(IP-1))
 11        CONTINUE
         CALL INGS48(A,B,CF1,CRES)          
         CRES=CRES+PV*CR                    
         RETURN                 
  3        CALL INGS48(A,B,CF,CRES)         
         RETURN               
         END                 

         SUBROUTINE INGS48(X1,X2,CF,CRES)        
        implicit real*8 (a,b,d,e,f,g,h,o,p,q,r,s,t,u,v,w,x,y,z)
         IMPLICIT COMPLEX*16 (C)
          DIMENSION CF(48),W(24)             
         DATA W/.003153346052305838633d0,.007327553901276262102d0,
     f  .011477234579234539490d0,.015579315722943848728d0,.019616
     g  160457355527814d0,.023570760839324379141d0,.0274265097083
     h  56948200d0,.031167227832798088902d0,.0347772225647704388
     i  93d0,.038241351065830706317d0,.041545082943464749214d0,.0
     j  44674560856694280419d0,.047616658492490474826d0,.05035903
     k  5553854474958d0,.052890189485193667096d0,.055199503699984
     l  162868d0,.057277292100403215705d0,.059114839698395635746d0,
     m  .060704439165893880053d0,.062039423159892663904d0,.06311419
     n  2286254025657d0,.063924238584648186624d0,.06446616443590082
     l  207d0,.064737696812683922503d0/
         CRES=(0.d0,0.d0)
         DO 1 I=1,24       
         L=49-I                 
  1        CRES=CRES+W(I)*(CF(I)+CF(L))                  
         CRES=CRES*(X2-X1)*.5d0               
         RETURN 
         END

         SUBROUTINE STGS48(X1,X2,X)
        implicit real*8 (a,b,d,e,f,g,h,o,p,q,r,s,t,u,v,w,x,y,z)
         DIMENSION Y(24),X(2)
         DATA Y/.998771007252426118601d0,.993530172266350757548d0,
     f  .984124583722826857745d0,.970591592546247250461d0,.952987
     g  703160430860723d0,.931386690706554333114d0,.9058791367155
     h  69672822d0,.876572020274247885906d0,.84358826162439353071
     i  1d0,.807066204029442627083d0,.767159032515740339254d0,.72
     j  4034130923814654674d0,.677872379632663905212d0,.628867396
     k  776513623995d0,.577224726083972703818d0,.5231609747222330
     l  33678d0,.466902904750958404545d0,.408686481990716729916d0,
     l  .348755886292160738160d0,.287362487355455576736d0,.224763
     m  790394689061225d0,.161222356068891718056d0,.0970046992094
     n  62698930d0,.032380170962869362033d0/
         DORIG=X1+X2       
         ORIG=DORIG*.5d0          
         DELT=(X2-X1)*.5d0            
         DO 1 I=1,24 
         L=49-I 
         X(I)=-Y(I)*DELT+ORIG 
  1         X(L)=DORIG-X(I)
         RETURN
         END

      SUBROUTINE STGS20(XINF,XSUP,Z)  
        implicit real*8 (a,b,d,e,f,g,h,o,p,q,r,s,t,u,v,w,x,y,z)
      DIMENSION X(10),Z(2)  
      DATA X/-.9931285991d0,-.9639719272d0,-.9122344282d0,
     F -.8391169718d0,-.7463319064d0,-.6360536807d0,-.5108670019d0,
     G -.3737060887d0,-.2277858511d0,-.0765265211d0/
      DO 10 I=1,10
      Z(I)=(XSUP-XINF)*X(I)/2.d0+XSUP/2.d0+XINF/2.d0          
      L=21-I
      Z(L)=(XINF-XSUP)/2.d0*X(I)+(XSUP+XINF)/2.d0                    
   10 CONTINUE
      RETURN 
      END  

         SUBROUTINE REGS20(XINF,XSUP,F,VI)
C     REAL    GAUSS INTEGRATION  
        implicit real*8 (a,b,d,e,f,g,h,o,p,q,r,s,t,u,v,w,x,y,z)
       DIMENSION F(20)
       DIMENSION A(10)
       DATA A/.0176140071d0,.0406014298d0,.0626720483d0,.0832767415d0,
     F .1019301198d0,.1181945319d0,.1316886384d0,.1420961093d0,
     G .1491729864d0,.1527533871d0/
       SUM=0.d0 
       DO 10 I=1,10
       L=21-I 
       SUM=SUM+A(I)*(F(I)+F(L))           
   10  CONTINUE 
       VI=SUM*(XSUP-XINF)/2.d0  
       RETURN 
       END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c GAUSS INTEGRATION SUBROUTINES   - R*8 (modified by T.Inoue)
c
c STGS8(xinf,xsup,x) 
c  
c input: xinf= lower limit
c        xsup= upper limit
c output: x(8)= points where integrand should be calculated
c
c
c REGS8(XINF,XSUP,F,result)
c
c  input: xinf, xsup= idem as before
c         F(8)= function calculated in X(8)
c  output:  result
c
c INGS8(XINF,XSUP,CF,cresult)
c 
c  same as REGS8 but CF(8) and result are complex
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


        SUBROUTINE STGS8(XINF,XSUP,Z) 
        implicit none
        real*8 XINF, XSUP, X(4), Z(8) 
        integer I, L
        DATA X/-.9602898564d0,-.7966664774d0,
     &         -.5255324099d0,-.1834346424d0/  
        DO 10 I=1,4  
        Z(I)=(XSUP-XINF)*X(I)/2d0+XSUP/2d0+XINF/2d0 
        L=9-I 
        Z(L)=(XINF-XSUP)/2d0*X(I)+(XSUP+XINF)/2d0  
  10       CONTINUE   
        RETURN  
        END     

        SUBROUTINE INGS8(X1,X2,CF,CRES) 
        implicit none
        real*8 X1, X2, W(4) 
        complex*16 CF(8), CRES
        integer I, L
        DATA W/0.1012285362d0,0.2223810344d0,
     &         0.3137066458d0,0.3626837833d0/
        CRES=Dcmplx(0d0,0d0)
        DO 1 I=1,4
        L=9-I
  1    CRES=CRES+W(I)*(CF(I)+CF(L))
        CRES=CRES*(X2-X1)*.5d0
        RETURN 
        END

        SUBROUTINE REGS8(X1,X2,F,RES) 
        implicit none
        real*8 X1, X2, F(8), RES, W(4) 
        integer I, L
        DATA W/0.1012285362d0,0.2223810344d0,
     &         0.3137066458d0,0.3626837833d0/
        RES=0d0
        DO 1 I=1,4
        L=9-I
  1    RES=RES+W(I)*(F(I)+F(L))
        RES=RES*(X2-X1)*.5d0
        RETURN 
        END

cccccccc end Integration subroutines
