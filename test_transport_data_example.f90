program test_transport_data_example
   use m_transport_data
   use m_lookup_table

   implicit none

   integer, parameter          :: dp         = kind(0.0d0)
   integer, parameter          :: table_size = 1000
   integer, parameter          :: test_size  = 10
   character(len=*), parameter :: td_file    = "test_td_input.txt"
   character(len=*), parameter :: gas_name   = "N2"

   integer                     :: ix
   real(dp)                    :: rand_val, rand_vals(test_size)
   real(dp)                    :: test_results(test_size)
   real(dp), parameter         :: x_min      = 0, x_max = 1e7_dp
   real(dp), allocatable       :: x_data(:), y_data(:)

   type(LT_col_t)              :: lkp_tbl

   ! Create a lookup table
   lkp_tbl = LT_create_col(x_min, x_max, table_size)

   ! Get the data from the input file
   call TD_get_td_from_file(td_file, gas_name, &
        "efield[V/m]_vs_energy[eV]", x_data, y_data)

   ! Store it in the lookup table
   call LT_set_col(lkp_tbl, x_data, y_data)

   print *, "Here are some random values from the lookup table"
   do ix = 1, test_size
      ! Get random location in range x_min : x_max
      call random_number(rand_val)
      rand_val = x_min + rand_val * (x_max - x_min)

      print *, rand_val, " : ", LT_get_col(lkp_tbl, rand_val)
   end do

   print *, "You can also get multiple at the same time"
   call random_number(rand_vals)
   rand_vals = x_min + rand_vals * (x_max - x_min)
   test_results = LT_get_col(lkp_tbl, rand_vals)

   do ix = 1, test_size
      print *, rand_vals(ix), " : ", test_results(ix)
   end do
end program test_transport_data_example