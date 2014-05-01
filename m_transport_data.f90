!> Module that provides routines for reading in arbritrary transport data

module m_transport_data

   implicit none
   private

   integer, parameter :: dp = kind(0.0d0)
   integer, parameter :: max_num_rows   = 200 !> The maximum number of rows of transport data for an entry
   integer, parameter :: nameLen        = 40
   integer, parameter :: lineLen        = 100

   public :: TD_get_td_from_file

contains

   !> Routine to read in transport data from a file
   !! Searches 'file_name' for transport 'data_name' concerning 'gas_name'
   function TD_get_td_from_file(file_name, data_name, gas_name, x_data, y_data)
      character(len=*), intent(in)         :: file_name, data_name, gas_name
      real(dp), allocatable, intent(inout) :: x_data(:), y_data(:)

      ! Temporary variables
      integer                :: gas_name_len
      integer                :: dIx
      integer                :: ioState, nL
      integer                :: n_rows
      integer, parameter     :: my_unit = 333
      character(LEN=nameLen) :: line_fmt, cur_name
      character(LEN=lineLen) :: line, prev_line
      real(dp)               :: tempArray(2, max_num_rows)

      nL = 0     ! Set the number of lines to 0
      gas_name_len = len(gas_name)

      ! Set the line format to read, only depends on lineLen currently
      write(line_fmt, FMT = "(I6)") lineLen
      line_fmt = "(A" // trim(adjustl(line_fmt)) // ")"

      ! Open 'file_name' (with error checking)
      open(my_unit, FILE = file_name, STATUS = "OLD", ACTION = "READ", ERR = 999, IOSTAT = ioState)

      ! Look for collision processes with the correct gas name in the file,
      ! which should contains entries like below:

      !     Efield[V/m]_vs_energy[eV]     [description of the type of transport data]
      !     AIR                           [the gas (mixture) name]
      !     COMMENT: Compiled by xxxxx    [possibly comments, these are optional]
      !     UPDATED: 2010-06-24 15:04:36
      !     ------------------            [at least 5 dashes]
      !     xxx       xxx                 [transport data in two column format]
      !     ...       ...
      !     xxx       xxx
      !     ------------------

      ! The outer DO loop, running until the end of the file is reached
      do
         ! Search for 'gas_name' in the file
         line = ' '
         do
            prev_line = line
            read(my_unit, FMT = line_fmt, ERR = 999, end = 666) line; nL = nL+1
            line = adjustl(line)
            if ( line(1:gas_name_len) == gas_name ) exit
         end do

         ! prev_line holds the type of transport data, see the formatting above
         if (data_name /= trim(prev_line)) cycle

         ! Now we can check whether there is a comment, while scanning lines until dashes are found,
         ! which indicate the start of the transport data
         do
            read(my_unit, FMT = line_fmt, ERR = 999, end = 555) line; nL = nL+1
            line = adjustl(line)
            if ( line(1:8) == "COMMENT:" ) cycle ! Do nothing for now
            if ( line(1:5) == "-----" ) exit
         end do

         ! Read the transport data into a temporary array
         n_rows = 0
         do
            read(my_unit, FMT = line_fmt, ERR = 999, end = 555) line; nL = nL+1
            line = adjustl(line)
            if ( line(1:5) == "-----" ) then
               exit  ! Dashes mark the end of the data
            else if (trim(line) == "" .or. line(1:1) == "#") then
               cycle ! Ignore whitespace or comments
            else if (n_rows < max_num_rows) then
               n_rows = n_rows + 1
               read(line, FMT = *, ERR = 999, end = 555) tempArray(:, n_rows)
            else
               print *, "CS_read_file error: too many rows in ", file_name, " at line ", nL
            end if
         end do

         ! Store the data in the actual table
         if (allocated(x_out)) deallocate(x_out)
         if (allocated(y_out)) deallocate(y_out)
         allocate(x_out(n_rows))
         allocate(y_out(n_rows))

         x_out = tempArray(1, 1:n_rows)
         y_out = tempArray(2, 1:n_rows)

         exit                   ! Done
      end do

555   continue ! Routine ends here if the end of "file_name" is reached erroneously
      print *, "TD_get_td_from_file error, reached end of file while searching:"
      print *, "ioState = ", ioState, " while reading from [", file_name, "] at line ", nL
      close(my_unit, ERR = 999, IOSTAT = ioState)
      return


666   continue ! Routine ends here if the end of "file_name" is reached correctly
      close(my_unit, ERR = 999, IOSTAT = ioState)
      return


999   continue ! If there was an error, the routine will end here
      print *, "TD_get_td_from_file error at line", nL
      print *, "ioState = ", ioState, " while searching [", gas_name, "] in [", file_name, "]"
      stop

   end subroutine TD_get_td_from_file

end module m_transport_data
