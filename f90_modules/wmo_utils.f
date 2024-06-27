c-- WMO_UTILS ------------------------------------------------------------------
c   The wmo_utils module contains methods for working WMO station ids.
c   The ids are stored in .wv/update/ndbc/id_table
c-------------------------------------------------------------------------------
        module wmo_utils

        use file_ops

        save

        integer, parameter :: WMO_array_size = 1000

        character*5  WMO_ids(WMO_array_size)

        contains


c-- LOAD_WMO_ARRAY -------------------------------------------------------------
c   Loads an array of wmo ids from the id_table		!* DEPRECATED
c   NOTE: id_table is no longer in use, GUDB is the source for WMO ids; see
c   GUDB_WMO_UTILS for the updated routine, gudb_load_wmo_array().
c-------------------------------------------------------------------------------
          subroutine load_wmo_array(err_code)		!* DEPRECATED
            integer::        err_code, munit=99
            character*100    wmo_path, wmo_file
            character*500    line

c--   Open wmo file

            wmo_path = '/project/wvutil/update/ndbc/'
            wmo_file = 'id_table'
            call open_read(munit, wmo_path, wmo_file, err_code, 6)
            if (err_code .ne. 0) return

c--   Init WMO id array

            do i=1, WMO_array_size
              WMO_ids(i) = "NULL"
            end do

c--   Loop over entries, init wmo for each and add to array

            do while (err_code .eq. 0)
              read(munit,'(a500)',iostat=err_code) line
              if (err_code .gt. 0) then
                close(munit)
                return
              else if (err_code .eq. 0) then
                read(line(1:3),'(i3.3)') index
                WMO_ids(index) = line(6:10)
              end if
            end do

            close(munit)
            err_code = 0

          end subroutine


c-- GET_WMO_ID -----------------------------------------------------------------
c   Loads the id_table and returns the WMO id for the given CDIP id.
c   Note: Call CDIP_TO_WMO if the id_table is already loaded.
c-------------------------------------------------------------------------------
          character*5 function get_wmo_id(cdip_id)
            integer::        err_code
            character*3      cdip_id

            call load_wmo_array(err_code)
            get_wmo_id = cdip_to_wmo(cdip_id)
          end function


c-- CDIP_TO_WMO ----------------------------------------------------------------
c   Returns the WMO id for the given CDIP id; the id table must be loaded 
c   before calling this function.  The WMO id will be 'NULL' if not found.
c-------------------------------------------------------------------------------
          character*5 function cdip_to_wmo(cdip_id)
            character*3      cdip_id
            read(cdip_id(1:3),'(i3.3)') index
            cdip_to_wmo = WMO_ids(index)
          end function


c-- GET_CDIP_ID ----------------------------------------------------------------
c   Loads the id_table and returns the CDIP id for the given WMO id.
c   Note: Call WMO_TO_CDIP if the id_table is already loaded.
c-------------------------------------------------------------------------------
          character*3 function get_cdip_id(wmo_id)
            integer::        err_code
            character*5      wmo_id

            call load_wmo_array(err_code)
            get_cdip_id = wmo_to_cdip(wmo_id)
          end function


c-- WMO_TO_CDIP ----------------------------------------------------------------
c   Returns the CDIP id for the given WMO id; the id table must be loaded 
c   before calling this function. The cdip id will be 'NUL' if not found
c-------------------------------------------------------------------------------
          character*3 function wmo_to_cdip(wmo_id)
            character*5      wmo_id
            wmo_to_cdip = 'NUL'
            do i=1, WMO_array_size
              if (WMO_ids(i) .eq. wmo_id) write(wmo_to_cdip(1:3),'(i3.3)') i
            end do
          end function


      end module
