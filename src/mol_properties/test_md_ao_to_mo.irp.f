 program test
 implicit none
 read_wf = .true.
 touch read_wf
 call test_mag_num
end

subroutine test_mag_num


 implicit none
 integer :: i,j,ipoint
 double precision :: r(3), weight, mo_magdip_x_num(mo_num,mo_num), mo_magdip_y_num(mo_num,mo_num), mo_magdip_z_num(mo_num,mo_num)
 double precision :: accux,accuy,accuz
 mo_magdip_x_num=0.d0
 mo_magdip_y_num=0.d0
 mo_magdip_z_num=0.d0
 do i = 1, mo_num
  do j = 1, mo_num
   do ipoint = 1, n_points_final_grid
    r(1) = final_grid_points(1,ipoint)
    r(2) = final_grid_points(2,ipoint)
    r(3) = final_grid_points(3,ipoint)
    weight = final_weight_at_r_vector(ipoint)
    mo_magdip_x_num(j,i) +=  weight * ((mos_in_r_array_transp(ipoint,j)*r(2)*mos_grad_in_r_array_transp_bis(ipoint,i,3))&
                                          -(mos_in_r_array_transp(ipoint,j)*r(3)*mos_grad_in_r_array_transp_bis(ipoint,i,2 )))
    mo_magdip_y_num(j,i) +=  weight * ((mos_in_r_array_transp(ipoint,j)*r(3)*mos_grad_in_r_array_transp_bis(ipoint,i,1))&
                                          -(mos_in_r_array_transp(ipoint,j)*r(1)*mos_grad_in_r_array_transp_bis(ipoint,i,3 )))
    mo_magdip_z_num(j,i) +=  weight * ((mos_in_r_array_transp(ipoint,j)*r(1)*mos_grad_in_r_array_transp_bis(ipoint,i,2))&
                                          -(mos_in_r_array_transp(ipoint,j)*r(2)*mos_grad_in_r_array_transp_bis(ipoint,i,1 ))) 
   enddo
  enddo
 enddo
 accux=0.d0
 accuy=0.d0
 accuz=0.d0
 do i=1,mo_num
  do j=1,mo_num
  accux+=abs(mo_magdip_x_becke(j,i)-mo_magdip_x_num(j,i))
  accuy+=abs(mo_magdip_y_becke(j,i)-mo_magdip_y_num(j,i))
  accuz+=abs(mo_magdip_z_becke(j,i)-mo_magdip_z_num(j,i))
  print*,mo_magdip_z_becke(j,i)-mo_magdip_z_num(j,i) , mo_magdip_z_becke(j,i), mo_magdip_z_num(j,i)
  enddo
 enddo
print*, 'diff x', accux, 'diff y', accuy,'diff z', accuz
end subroutine
