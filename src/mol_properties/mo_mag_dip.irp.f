 BEGIN_PROVIDER [double precision, mo_magdip_x_becke, (mo_num,mo_num)]
&BEGIN_PROVIDER [double precision, mo_magdip_y_becke, (mo_num,mo_num)]
&BEGIN_PROVIDER [double precision, mo_magdip_z_becke, (mo_num,mo_num)]
  implicit none
  BEGIN_DOC
  !  Magnetic dipole moments integrals in the MO basis
  END_DOC

    call ao_to_mo( ao_magdip_x_becke, size(ao_magdip_x_becke,1),   &
                   mo_magdip_x_becke, size(mo_magdip_x_becke,1))
    call ao_to_mo( ao_magdip_y_becke, size(ao_magdip_y_becke,1),   &
                   mo_magdip_y_becke, size(mo_magdip_y_becke,1))
    call ao_to_mo( ao_magdip_z_becke, size(ao_magdip_z_becke,1),   &
                   mo_magdip_z_becke, size(mo_magdip_z_becke,1))

END_PROVIDER

