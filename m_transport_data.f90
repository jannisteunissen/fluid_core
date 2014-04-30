!> Module that provides routines for reading in arbritrary transport data

module m_transport_data

   implicit none
   private

   integer, parameter :: dp = kind(0.0d0)
   integer, parameter :: max_num_rows = 200 !> The maximum number of rows of transport data for an entry
   integer, parameter :: TD_nEntriesMax = 100   !> The maximum number of transport data entries
   integer, parameter :: nameLen = 40, lineLen = 100

   type TD_table_type
      real(dp), pointer      :: TD(:,:)
      integer                :: n_rows
      character(LEN=nameLen) :: gas_name
      character(LEN=nameLen) :: data_name
      character(LEN=lineLen) :: comment
   end type TD_table_type

   !> The number of transport data entries read in
   integer :: TD_nEntries = 0

   !> The actual table for storing transport data, could be allocated
   !! dynamically in the future
   type(TD_table_type) :: TD_table(TD_nEntriesMax)

   public :: TD_read_file
   public :: TD_get_data

contains

   !> Routine to read in transport data from a file
   !! Searches 'filename' for transport data concerning 'gas_name'
   subroutine TD_read_file(filename, gas_name)
      character(LEN=*), intent(IN) :: filename, gas_name

      ! Temporary variables
      integer                      :: gas_name_len
      integer                      :: dIx
      integer                      :: ioState, nL
      integer                      :: n_rows
      integer, parameter           :: my_unit = 333
      character(LEN=nameLen)       :: line_fmt, data_name
      character(LEN=lineLen)       :: line, prev_line
      real(dp)             :: tempArray(2, max_num_rows)

      nL = 0     ! Set the number of lines to 0
      gas_name_len = len(gas_name)

      ! Set the line format to read, only depends on lineLen currently
      write(line_fmt, FMT = "(I6)") lineLen
      line_fmt = "(A" // trim(adjustl(line_fmt)) // ")"

      ! Open 'filename' (with error checking)
      open(my_unit, FILE = filename, STATUS = "OLD", ACTION = "READ", ERR = 999, IOSTAT = ioState)

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
         data_name = trim(prev_line)

         ! Update the number of processes and set the gas name and data name
         TD_nEntries = TD_nEntries + 1
         dIx = TD_nEntries

         if (TD_nEntries > TD_nEntriesMax) then
            print *, "TD_read_file error, too many transport data entries"
            return
         end if

         TD_table(dIx)%gas_name  = gas_name
         TD_table(dIx)%data_name = data_name

         ! Now we can check whether there is a comment, while scanning lines until dashes are found,
         ! which indicate the start of the transport data
         do
            read(my_unit, FMT = line_fmt, ERR = 999, end = 555) line; nL = nL+1
            line = adjustl(line)
            if ( line(1:8) == "COMMENT:" ) then
               TD_table(dIx)%comment = adjustl( line(10:) )
            end if
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
               print *, "CS_read_file error: too many rows in ", filename, " at line ", nL
            end if
         end do

         ! Store the data in the actual table
         allocate( TD_table(dIx)%TD(2, n_rows) )
         TD_table(dIx)%n_rows = n_rows
         TD_table(dIx)%TD = tempArray(:, 1:n_rows)

      end do

555   continue ! Routine ends here if the end of "filename" is reached erroneously
      print *, "TD_read_file error, reached end of file while searching:"
      print *, "ioState = ", ioState, " while reading from [", filename, "] at line ", nL
      close(my_unit, ERR = 999, IOSTAT = ioState)
      return


666   continue ! Routine ends here if the end of "filename" is reached correctly
      close(my_unit, ERR = 999, IOSTAT = ioState)
      return


999   continue ! If there was an error, the routine will end here
      print *, "TD_read_file error at line", nL
      print *, "ioState = ", ioState, " while searching [", gas_name, "] in [", filename, "]"
      stop


   end subroutine TD_read_file

   !> Get the transport data with name 'data_name'
   subroutine TD_get_data(data_name, x_out, y_out)
      character(len=*), intent(in)         :: data_name
      real(dp), allocatable, intent(inout) :: x_out(:), y_out(:)
      integer                              :: n, dIx

      dIx = -1    ! Find the transport data corresponding to data_name
      do n = 1, TD_nEntries
         if (TD_table(n)%data_name == data_name) then
            dIx = n
            exit
         end if
      end do

      if (dIx == -1) then
         print *, "TD_get_data error, data with name ", data_name
         print *, "could not be found!"
         stop
      end if

      if (allocated(x_out)) deallocate(x_out)
      if (allocated(y_out)) deallocate(y_out)
      allocate( x_out(TD_table(dIx)%n_rows) )
      allocate( y_out(TD_table(dIx)%n_rows) )

      x_out = TD_table(dIx)%TD(1, :)
      y_out = TD_table(dIx)%TD(2, :)

   end subroutine TD_get_data

end module m_transport_data
