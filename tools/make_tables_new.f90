	subroutine make_table		!(label:maketables)
 !       use mpi				!parallel
	use input4
	use steuerung
	use tmat4
	implicit none
        integer :: k_j,iv,ic,jc,ni,nj 	! k_j and iv determine the quantum numbers of the channel, ic, jc are channel indices, ni,nj are the n1=n+1 mesh points
	integer :: k,n1   		! n1=n+1, n is the mesh points of momentum integral
      	complex(kind(0.d0)) :: ez,result
	real(kind(0.d0)):: start_cpu,end_cpu
	n1=n+1
	ramtab=0.
	readit=0			! readit is changed to zero ONLY HERE. Anything else in fitting is with readit=1
!	if (myid==root) then
!	k=1 
!	else
!	k=myid  			! parallel
!	end if
	do k_j=k_b,k_e  		! loop over angular momentum       
            write(0,*)'k_j=',k_j
          j1=2*k_j - 1              	! j1=2*j_physical  ! declared in input
          call rot_mat(j1)          	! input xg,wg,djw  ! util.f90/rot_mat  ! dmat3.f90
          call set_coeff_jls(j1)
          
          do k=1,melab  ! energy loop              
          ez=e_cm(k)
          write(0,*)'k, ez=',k, ez
	  zrem=ez        
          write(0,*)'Now calling mesh_q'
	  call mesh_q
!          write(0,*)'couple(1,1,1):', coupl(1,1,1)
!          write(0,*)'in make_tables: coupl(1,1,1)%gc=', coupl(1,1,1)%gc
	  call tmat(ez,1,4,k_j)   	! non-pole t-matrix ! UPDATE IF MORE THAN I=3/2 IS FITTED!
          write(0,*)'Now calling tmat'
	      do iv=1,4			! ADAPT TO SAVE TIME IF NOT ALL iv ARE NEEDED
	      do ic=1,n_c
	      do jc=1,n_c
	      do ni=1,n1
	      do nj=1,n1
	       result=tmatrix(nj,ni,iv,ic,jc)
	       ramtab(nj,ni,jc,ic,iv,k_j)=result
	      end do
	      end do
	      end do
	      end do
	      end do
          end do ! k, energy loop
	end do 				! end of k_j loop
	readit=1			! readit is switched back to 1 for fitting
 156    format(2(15g15.6))   
	return
	end subroutine make_table
