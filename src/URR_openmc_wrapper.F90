!>@brief Wrapper for OpenMC functions and variables used by PURXS
module URR_openmc_wrapper

  use error, only:&
       fatal_error
  use global, only:&
       nuclides,&
       n_materials,&
       materials
  use list_header, only:&
       ListReal,&
       ListInt
  use material_header, only:&
       Material
  use output, only:&
       write_message
  use random_lcg, only:&
       prn
  use algorithm, only:&
       binary_search
  use string, only:&
       to_str
  use URR_error, only:&
       INFO

  implicit none
  private
  public ::&
       fatal_error,&
       nuclides,&
       n_materials,&
       materials,&
       ListReal,&
       ListInt,&
       Material,&
       write_message,&
       prn,&
       binary_search,&
       to_str

end module URR_openmc_wrapper
