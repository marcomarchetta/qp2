program test 
 implicit none
 call test_numeric
end

subroutine test_numeric
implicit none 
  integer :: i, j 
  real*8  :: accu
 do i= 1, mo_num
  do j =1, mo_num
   accu+=dabs( mo_dipole_z(j,i)- mo_dipole_z_becke(j,i))
   print*,i,j,dabs( mo_dipole_z(j,i)- mo_dipole_z_becke(j,i))
  enddo
 enddo
 accu=accu/dble(mo_num*mo_num)
 print*, accu
end program 

