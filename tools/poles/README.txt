What to obey when including the code of the complex plane with the new version that has KY channels.
-------------------------

1) Indices of qon_c have changed go now through from 1 to 13, before (ccAA) 1 was piN, 2 was rhoN, 3 was etaN and so on.
2) Do not forget that tmatrix and tmatrix_pole have changed:
	OLD:	tmatrix(iv,i_c1,i_c2,i_q1,i_q2) 
	NEW: 	tmatrix(i_q2,i_q1,iv,i_c1,i_c2)
	
	OLD: 	tmatrix_pole_of(iv,i_c1,i_c2,i_q1,i_q2)
	NEW: 	DOES NOT EXIST ANY MORE
	
	OLD:    tmatrix_pole_os(iv,i_c1,i_c2)
	NEW: 	tmatrix_pole(i_q2,i_q1,iv,i1,i2)

3) Don't forget to call before tmat:
   In case k_j has changed:
        j1=2*k_j - 1              ! j1=2*j_physical  ! declared in input
	call rot_mat(j1)          ! input xg,wg,djw  ! util.f90/rot_mat  ! dmat3.f90
        call set_coeff_jls(j1)
	
	  zrem=ez
	  call mesh_q
 and finally:
	  call tmat(ez,1,4,k_j)   ! non-pole t-matrix

