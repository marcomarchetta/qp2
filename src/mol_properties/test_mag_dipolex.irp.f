program test 
 implicit none
 read_wf = .true.
 touch read_wf
 call test_numeric_m
end

subroutine test_numeric_m
implicit none 
  use bitmasks ! you need to include the bitmasks_module.f90 features
  integer :: i, j, k, kk, ispin, h, p, s1,s2,h2,p2,istate,jstate
  double precision  :: oij
  integer           :: degree_tot
  double precision  :: phase, accu
  integer, allocatable           :: occ(:,:)
  integer                        :: n_occ_ab(2)
  double precision, allocatable :: o_int(:,:), matrix(:,:)
  integer                        :: exc(0:2,2,2)
  allocate(occ(N_int*bit_kind_size,2),o_int(mo_num,mo_num),matrix(N_states,N_states))
  o_int = mo_magdip_x_becke
  matrix = 0.d0
  do i = 1, N_det
   do j = 1, N_det
    call get_excitation_degree(psi_det(1,1,i),psi_det(1,1,j),degree_tot,N_int)
    if(degree_tot == 0)then
     call bitstring_to_list_ab(psi_det(1,1,i), occ, n_occ_ab, N_int)
     oij = 0.d0
     do ispin = 1,2
      do k = 1,n_occ_ab(ispin)
       kk=occ(k,ispin)
       oij += o_int(kk,kk)
      enddo
     enddo
    else if (degree_tot == 1)then
     call get_single_excitation(psi_det(1,1,i),psi_det(1,1,j),exc,phase,N_int)
     call decode_exc(exc,degree_tot,h,p,h2,p2,s1,s2)
     oij = o_int(h,p) * phase
    endif
    if(degree_tot.le.1)then
     do istate = 1, N_states
      do jstate = 1, N_states
       matrix(jstate,istate) -= oij * psi_coef(i,istate) * psi_coef(j,jstate)
      enddo
     enddo
    endif
   enddo
  enddo

  accu = 0.d0
  do istate = 1, N_states
   do jstate = 1, N_states
    print*,istate,jstate
    print*,matrix(jstate,istate) , multi_s_x_magdipole_moment(jstate,istate) 
    accu += dabs(matrix(jstate,istate) + multi_s_x_magdipole_moment(jstate,istate) )
   enddo
  enddo
  print*,'accu = ',accu

end program 

