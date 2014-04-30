program test_m_transport_data
  use m_transport_data
  integer, parameter :: dp = kind(0.0d0)

  integer :: n, data_size
  real(dp), allocatable :: x_data(:), y_data(:)

  print *, "Testing m_transport_data.f90 implementation"

  ! Read in test data
  call TD_read_file("test_m_transport_data_input.txt", "AIR")

  ! Get some data
  call TD_get_data("Efield[V/m]_energy[eV]", x_data, y_data)

  print *, "The following data was found when searching for Efield[V/m]_energy[eV]"
  data_size = size(x_data)
  do n = 1, data_size
     print *, x_data(n), y_data(n)
  end do
  print *, ""

  ! Get some more data
  call TD_get_data("Efield[V/m]_alphaEff[1/m]", x_data, y_data)

  print *, "The following data was found when searching for Efield[V/m]_alphaEff[1/m]"
  data_size = size(x_data)
  do n = 1, data_size
     print *, x_data(n), y_data(n)
  end do

  print *, "Done, you can check test_m_transport_data_input.txt for comparison"
  print *, ""
end program test_m_transport_data
