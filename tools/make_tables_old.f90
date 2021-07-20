	subroutine make_table		!(label:maketables)
	use input4
	use steuerung
	use tmat4
	implicit none
        integer :: k_j,iv,ic,jc,ni,nj 	! k_j and iv determine the quantum numbers of the channel, ic, jc are channel indices, ni,nj are the n1=n+1 mesh points
	integer :: k,n1   		! n1=n+1, n is the mesh points of momentum integral
	integer :: table_bgd_unit(1:4)  ! unit to write or read the table, overwritten if k_j changes!
      	complex(kind(0.d0)) :: ez
	real(kind(0.d0)):: retnp,imtnp
	n1=n+1
	store_t=0.
	write(0	,*)'Entering make_table'
	do k_j=k_b,k_e  		! loop over angular momentum
	do iv=1,4 			! Open storage to write table PW by PW. Note: this must be done inside the k_j loop!
          table_bgd_unit(iv)=3030+iv
	  open(table_bgd_unit(iv),file='t_np_tables_'//bgd(iv,k_j))    ! allocated in input/piN_pw_read    
	end do  ! iv=1,4
	if (writeit==1) then		! Wheteher or not to calculate T^NP and write it
	write(0,*)'Writing the table for k_j = ',k_j 			
	call write_info_tables		! Write esssential information about the saved table.
          j1=2*k_j - 1              	! j1=2*j_physical  ! declared in input
          call rot_mat(j1)          	! input xg,wg,djw  ! util.f90/rot_mat  ! dmat3.f90
          call set_coeff_jls(j1)
!          if(k_j==1)then
!            iv=2
!	    write(0,*)'Now calling renormalization'
!	    call rnm(iv,k_j)  		! renormalize couplings and masses for nucleon pole
!	    write(0,*)'End calling renormalization'
!          end if  ! (k_j==1)
          do k=1,melab 			! loop over energies
            ez=e_cm(k)
	    zrem=ez
	    call mesh_q
	    call tmat(ez,1,4,k_j)   	! non-pole t-matrix
	      do iv=1,4
	      do ic=1,n_c
	      do jc=1,n_c
	      do ni=1,n1
	      do nj=1,n1
	        write(table_bgd_unit(iv),156)tmatrix(nj,ni,iv,ic,jc)
	      end do
	      end do
	      end do
	      end do
	      end do	    
	    write(0,*)'Writing table for ez, k_j = ',dble(ez),k_j
           end do  			! end of energy loop
	end if				! Whether or not to calculate T^NP and write it
	if (readit==1) then		! Inside k_j loop, read the table! IMPORTANT: Parameters according to "info_table.txt" must be the same !!
	write(0,*)'Reading the table for k_j = ',k_j 			
          do k=1,melab 			! loop over energies
            ez=e_cm(k)
	    write(0,*)'Reading table for ez, k_j = ',dble(ez),k_j
	      do iv=1,4
	      do ic=1,n_c
	      do jc=1,n_c
	      do ni=1,n1
	      do nj=1,n1
	      read(table_bgd_unit(iv),156)retnp,imtnp
!	      store_t(k,k_j,iv,ic,jc,ni,nj)=retnp+dcmplx(0.d0,1.d0)*imtnp
	      store_t(nj,ni,jc,ic,k,iv,k_j)=retnp+dcmplx(0.d0,1.d0)*imtnp
	      end do
	      end do
	      end do
	      end do
	      end do
	   end do			! end of loop over energies k	    
	end if 				! Whether to read or not
	do iv=1,4 			! Close units! 
	  close(table_bgd_unit(iv))    
	end do  ! iv=1,4
	end do 				! end of k_j loop
	write(0,*)'End writing or reading the table.'
 156    format(2(15g15.6))   
	return
	end subroutine make_table
	
!----------------------------------------------------------------------------------------
	
	subroutine write_info_tables
	use input4
	use steuerung
	implicit none
	integer :: n1
	n1=n+1
	open(1701,file='t_np_tables_bgd/info_table.txt')
	write(1701,*)'Information for nonpole T table'
	write(1701,*)'-------------------------------'
	write(1701,*)'number of energies k', melab
	write(1701,*)'start k_b, end k_e of k_j: ',k_b,k_e
	write(1701,*)'number of channels n_c: ',n_c
	write(1701,*)'number n of off-shell point: ',n
	write(1701,*)'Length of table: n_c^2 (k_e-k_b+1) (times iv=4) (n+1)^2 melab',n_c**2*(k_e-k_b+1)*(n+1)**2*melab*4
	close(1701)
	return
	end subroutine write_info_tables 
	
!----------------------------------------------------------------------------------------

	subroutine restore_tnp(ez,k_j) ! Restore the nonpole T matrix (label:maketables). This subroutine is now called in the previous code, 
	use input4		       ! in subroutine "tmat" in the file "tmat4_mod.f90"
	use steuerung
	implicit none
	complex(kind(0.d0)) :: ez
	integer :: i,istore,k_j,iv,ic,jc,ni,nj,n1
	n1=n+1
	istore=0			! In the following lines: find out which energy istore from the grid of energies is just below ez which is continuous.
	do i=1,melab
	 if (dble(ez).ge.dble(e_cm(i))) then
	 istore=i
	 end if
	end do
	if ((istore==0.or.istore==melab).and.(dble(ez).ne.dble(e_cm(i)))) then
	 write(0,*)'ez is outside energy grid. Bad. This stops now'
	 write(0,*)'lowest, highest, e: ',e_cm(1),e_cm(melab),dble(ez)
	! stop
	end if
	! Now we know that we should interpolate linearly between energy istore and istore+1
	! Now, we fill up every entry of "tmatrix":
	do iv=1,4			! loop over iv quantum number
	do ic=1,n_c			! loop over outgoing channel
	do jc=1,n_c			! loop over incoming channel
	do ni=1,n1			! loop over outgoing off- and on-shell points
	do nj=1,n1			! loop over incoming off- and on-shell points
	! Linear interpolation:
       if (dble(ez)==dble(e_cm(i))) then ! Do not interpolate if energy coincides with the one save din the tables!
	tmatrix(nj,ni,iv,ic,jc)=store_t(nj,ni,jc,ic,istore,iv,k_j)
	else
	tmatrix(nj,ni,iv,ic,jc)=store_t(nj,ni,jc,ic,istore,iv,k_j) &
	& + (store_t(nj,ni,jc,ic,istore+1,iv,k_j)-store_t(nj,ni,jc,ic,istore,iv,k_j))/(e_cm(istore+1)-e_cm(istore)) &
	& * (dble(ez)-e_cm(istore))
	end if
	end do
	end do
	end do
	end do
	end do
	return
	end subroutine restore_tnp
