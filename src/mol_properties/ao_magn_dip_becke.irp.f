 BEGIN_PROVIDER [ double precision, ao_magdip_x_becke, (ao_num,ao_num)]
&BEGIN_PROVIDER [ double precision, ao_magdip_y_becke, (ao_num,ao_num)]
&BEGIN_PROVIDER [ double precision, ao_magdip_z_becke, (ao_num,ao_num)]
 implicit none
 integer :: i,j,ipoint
 double precision :: r(3), weight, k 
 k=1.d0/(2.d0*137.035999177)
 ao_magdip_x_becke=0.d0
 ao_magdip_y_becke=0.d0
 ao_magdip_z_becke=0.d0
 do i = 1, ao_num
  do j = 1, ao_num
   do ipoint = 1, n_points_final_grid
    r(1) = final_grid_points(1,ipoint)
    r(2) = final_grid_points(2,ipoint)
    r(3) = final_grid_points(3,ipoint)
    weight = final_weight_at_r_vector(ipoint)
    ao_magdip_x_becke(j,i) += k* weight * ((aos_in_r_array_transp(ipoint,j)*r(2)*aos_grad_in_r_array_transp_bis(ipoint,i,3))&
                                          -(aos_in_r_array_transp(ipoint,j)*r(3)*aos_grad_in_r_array_transp_bis(ipoint,i,2 )))
    ao_magdip_y_becke(j,i) += k* weight * ((aos_in_r_array_transp(ipoint,j)*r(3)*aos_grad_in_r_array_transp_bis(ipoint,i,1))&
                                          -(aos_in_r_array_transp(ipoint,j)*r(1)*aos_grad_in_r_array_transp_bis(ipoint,i,3 )))
    ao_magdip_z_becke(j,i) += k* weight * ((aos_in_r_array_transp(ipoint,j)*r(1)*aos_grad_in_r_array_transp_bis(ipoint,i,2))&
                                          -(aos_in_r_array_transp(ipoint,j)*r(2)*aos_grad_in_r_array_transp_bis(ipoint,i,1 ))) 
   enddo
  enddo
 enddo
END_PROVIDER 
