	module		input_cont
	
	implicit	none
!        real(kind(0.0d0)),parameter :: pi	= 	3.14159265358979323846d0 ! pi		   
        complex(kind(0.0d0)),parameter :: ci	= 	dcmplx(0.d0,1.d0)		   
        real(kind(0.0d0)),parameter :: mmpi 	=	138.d0		   ! masses(1,2)
	real(kind(0.0d0)),parameter :: gspipi 	=	9.5d0		   ! NOT DEFINED
	real(kind(0.0d0)),parameter :: lamsi 	=	1500.d0		   ! NOT DEFINED
	real(kind(0.0d0)),parameter :: msb 	=	900.1d0		   ! mbs
	real(kind(0.0d0)),parameter :: mnu 	=	938.926d0	   ! masses(1,1)
	integer,parameter :: 	       nin 	=	3	 	   ! NOT DEFINED
 	integer,parameter :: 	       nin2 	=	5		   ! NOT DEFINED
 	real(kind(0.0d0)),parameter :: reposi 	=	1814.35d0	   ! NOT DEFINED
	
	real(kind(0.0d0)),parameter :: grpipi 	=	6.03676d0	   ! sqrt(gcoup(31) 4 pi)
	real(kind(0.0d0)),parameter :: mrb 	=	911.d0		   ! mbr
	real(kind(0.0d0)),parameter :: mrho 	=	770.d0		   ! NOT DEFINED
	real(kind(0.0d0)),parameter :: larho 	=	1800.d0		   ! NOT DEFINED
 	real(kind(0.0d0)),parameter :: repor 	=	1702.115d0	   ! NOT DEFINED
	
	real(kind(0.0d0)),parameter :: fcoud 	=	0.36d0 		   ! gcoup(10)
	real(kind(0.0d0)),parameter :: lamdel 	=	1500.d0		   ! NOT DEFINED
	real(kind(0.0d0)),parameter :: mdel 	=	1232.d0		   ! NOT DEFINED
	real(kind(0.0d0)),parameter :: mdelb 	=	1415.d0		   ! mbd
 	real(kind(0.0d0)),parameter :: repod 	=	1349.074d0	   ! NOT DEFINED
	
        end module input_cont
