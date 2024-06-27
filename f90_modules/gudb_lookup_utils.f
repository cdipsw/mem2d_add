c-- GUDB_LOOKUP_UTILS ----------------------------------------------------------
c   Routines for file management based on GUDB (e.g. tophat-based) metadata.
c   Source files are saved in .data03/SOURCE_DATA/top_YYYY and hul_YYYY dirs.
c   Tophat-sourced files include an underscore in the name, and hull-sourced
c   files include a capital X.
c-------------------------------------------------------------------------------
      module gudb_lookup_utils

        use dates
        use file_ops
        use strings

        implicit none

        save

        integer, parameter::    GLU_max_entries = 10000

        contains


c-- GUDB_IS_TOPHAT_FILENAME ----------------------------------------------------
c   Returns true if the filename is tophat id'd rather than station id'd.
c-------------------------------------------------------------------------------
          logical function gudb_is_tophat_filename(fname)
            integer           upos
            character*(*)     fname

            if (INDEX(fname, '_') .ge. 6) then
              gudb_is_tophat_filename = .true.
            else
              gudb_is_tophat_filename = .false.
            end if
          end function


c-- GUDB_GET_TOPHAT_ID_FROM_FILENAME -------------------------------------------
c   Returns a tophat id string from the filename. The filename is assumed to 
c   include an underscore at the end of the tophat string. Trailing characters
c   in the tophat id are ignored; only the numeric portion is returned.
c-------------------------------------------------------------------------------
          character*20 function gudb_get_tophat_id_from_filename(fname)
            integer           upos
            character*(*)     fname

            gudb_get_tophat_id_from_filename = ''
            upos = INDEX(fname, '_')
            if (upos .ge. 6) gudb_get_tophat_id_from_filename = gudb_trim_tophat_id(fname(3:upos))
          end function


c-- GUDB_IS_HULL_FILENAME ------------------------------------------------------
c   Returns true if the filename is hull id'd rather than station id'd.
c-------------------------------------------------------------------------------
          logical function gudb_is_hull_filename(fname)
            integer           upos
            character*(*)     fname

            if (INDEX(fname, 'X') .ge. 6) then
              gudb_is_hull_filename = .true.
            else
              gudb_is_hull_filename = .false.
            end if
          end function


c-- GUDB_GET_HULL_ID_FROM_FILENAME ---------------------------------------------
c   Returns a hull id string from the filename. The filename is assumed to 
c   include a tilda at the end of the hull string. 
c-------------------------------------------------------------------------------
          character*20 function gudb_get_hull_id_from_filename(fname)
            integer           upos
            character*(*)     fname

            gudb_get_hull_id_from_filename = ''
            upos = INDEX(fname, 'X')
            if (upos .ge. 6) gudb_get_hull_id_from_filename = fname(3:upos-1)
          end function


c-- GUDB_TRIM_TOPHAT_CHARS -----------------------------------------------------
c   Trims trailing chars from a tophat id; id must be at least 4 chars long.
c-------------------------------------------------------------------------------
          character*20 function gudb_trim_tophat_id(tophat)
            integer           epos
            character*(*)     tophat

            gudb_trim_tophat_id = ''
            epos = LEN_TRIM(tophat)
            do while (epos .gt. 1 .and. .not. string_is_numeric(tophat(epos:epos)))
              epos = epos - 1
            end do
            if (epos .ge. 4) gudb_trim_tophat_id = tophat(1:epos)
          end function


c-- GUDB_GET_TIME_FROM_FILENAME ------------------------------------------------
c   Returns a date_block set to the filetime. The filename is assumed to end
c   in a 12-digit time (YYYYMMDDhhmm).
c-------------------------------------------------------------------------------
          type(date_block) function gudb_get_time_from_filename(fname)
            integer           offset, upos
            character*14      dstring
            character*(*)     fname

            gudb_get_time_from_filename = init_date(1900,1,1,0,0,0)
            upos = INDEX(fname, '_')
            if (upos .le. 0) upos = INDEX(fname, 'X')
            offset = MIN(LEN_TRIM(fname)-upos, 14)
            dstring = fname(upos+1:upos+offset)
            dstring = complete_datestring(dstring, 'start')
            if (upos .gt. 0) gudb_get_time_from_filename = parse_datestring(dstring)
          end function


c-- GUDB_GET_STATE_STRING_FROM_LABEL -------------------------------------------
c   Returns a state string - 'MOORED', 'OFFSITE', etc - for a label (p0/1/2/3)
c-------------------------------------------------------------------------------
          character*10 function gudb_get_state_string_from_label(label, upper)
            logical,optional::    upper
            logical               upcase
            character*2           label

            upcase = .true.
            if (PRESENT(upper)) then
              if (upper .eqv. .false.) upcase = .false.
            end if

            if (label .eq. 'p1') then
              gudb_get_state_string_from_label = 'moored'
              if (upcase) gudb_get_state_string_from_label = 'MOORED'
            else if (label .eq. 'p2') then
              gudb_get_state_string_from_label = 'offsite'
              if (upcase) gudb_get_state_string_from_label = 'OFFSITE'
            else if (label .eq. 'p0') then
              gudb_get_state_string_from_label = 'predeploy'
              if (upcase) gudb_get_state_string_from_label = 'PREDEPLOY'
            else if (label .eq. 'p3') then
              gudb_get_state_string_from_label = 'recovered'
              if (upcase) gudb_get_state_string_from_label = 'RECOVERED'
            else
              gudb_get_state_string_from_label = 'undefined'
              if (upcase) gudb_get_state_string_from_label = 'UNDEFINED'
            end if
          end function


c-- GUDB_GET_FILE_PATH ---------------------------------------------------------
c   Returns the path to a tophat-id source file - is, ls, lx, df, im, iv, hv
c-------------------------------------------------------------------------------
          character*100 function gudb_get_file_path(fname, errcode)
            integer           errcode
            character*2       ftype, monthstr
            character*4       sprefix, yearstr
            character*20      id
            character*(*)     fname
            type(date_block)  ftime

            gudb_get_file_path = ''
            errcode = 0

            if (gudb_is_tophat_filename(fname)) then
              id = gudb_get_tophat_id_from_filename(fname)
              ftype = fname(1:2)
              sprefix = 'top_'
            else if (gudb_is_hull_filename(fname)) then
              id = gudb_get_hull_id_from_filename(fname)
              ftype = fname(1:2)
              sprefix = 'hul_'
            else
              errcode = 1
              return
            end if

            ftime = gudb_get_time_from_filename(fname)
            write(yearstr,'(i4)') ftime%year
            write(monthstr,'(i02.2)') ftime%month

            if (ftype .eq. 'ix') then
              gudb_get_file_path = '/project/data01/INDEX_FILES/'//sprefix//yearstr//'/'//TRIM(id)//'/'
            else
              gudb_get_file_path = '/project/data03/SOURCE_DATA/'//sprefix//yearstr//'/'//TRIM(id)//'/'//monthstr//'/'
            end if

          end function


c-- GUDB_UPDATE_INDEX_TABLE ----------------------------------------------------
c   Adds a source file name to the monthly index (ix) file.
c-------------------------------------------------------------------------------
          subroutine gudb_update_index_table(ifile, errcode, errunit)
            integer::        entries, errcode, errunit, i, temp_unit = 99
            logical          add_line
            character*(*)    ifile
            character*6      yearmo
            character*20     tophat
            character*100    ix_name, sets(GLU_max_entries)
            type(date_block) ftime

            tophat = gudb_get_tophat_id_from_filename(ifile)
            ftime = gudb_get_time_from_filename(ifile)
            write(yearmo(1:6),'(i4,i02.2)') ftime%year, ftime%month
            ix_name = 'ix'//TRIM(tophat)//'_'//yearmo
            call gudb_load_index_table(ix_name, sets, entries, errcode, errunit)
            if (errcode .ne. 0) then
              entries = 0
              errcode = 0
            end if

c--   Check if entry already exists, update it if possible

            add_line = .true.
            do i = 1, entries
              if (TRIM(sets(i)) .eq. TRIM(ifile)) add_line = .false.
            end do

c--   If needed add new line, recreate index file

            if (add_line) then
              entries = entries + 1
              write(sets(entries),'(a)') ifile
            end if
            call gudb_make_index_table(sets, entries, errcode, errunit)
          end subroutine


c-- GUDB_LOAD_INDEX_TABLE ------------------------------------------------------
c   Reads an entire ix file into an array of filename entries.
c-------------------------------------------------------------------------------
        subroutine gudb_load_index_table(ifile, sets, entries, errcode, errunit)
          character*(*)  ifile
          character*100  ixpath, ixname, sets(GLU_max_entries)
          integer::  entries, errcode, errunit, temp_unit = 99

          entries = 0
          ixname = TRIM(ifile)
          ixpath = gudb_get_file_path(ixname, errcode)
          if (errcode .ne. 0) then
            write(errunit, '(2a)') 'ERROR, index file not found for ', TRIM(ifile)
            return
          end if

          call open_read(temp_unit, ixpath, ixname, errcode, errunit)
          if (errcode .ne. 0) return
          do while (errcode .eq. 0)
            entries = entries + 1
            read(temp_unit,'(a)',iostat=errcode) sets(entries)
          end do

          entries = entries - 1
          if (errcode .eq. -1) errcode = 0    !* end-of-file, no error
          close(temp_unit)
        end subroutine


c-- GUDB_MAKE_INDEX_TABLE ------------------------------------------------------
c   Recreates an index file; helper routine for GUDB_UPDATE_INDEX_TABLE
c-------------------------------------------------------------------------------
        subroutine gudb_make_index_table(sets, entries, errcode, errunit)
          character*100    ixpath, ixname, full_name, sets(GLU_max_entries)
          integer::        entries, errunit, errcode, i, sys_code, temp_unit = 99

          ixname = 'ix'//sets(1)(3:LEN_TRIM(sets(1))-6)
          ixpath = gudb_get_file_path(ixname, errcode)
          if (errcode .ne. 0) then
            write(errunit,'(a)') 'ERROR: index table path not found'
            return
          end if

          call open_replace(temp_unit, ixpath, ixname, errcode, errunit)
          if (errcode .ne. 0) return
          do i = 1, entries
            write(temp_unit, '(a)') TRIM(sets(i))
          end do
          close(temp_unit)

          full_name = TRIM(ixpath)//TRIM(ixname)
          write(6,*) TRIM(full_name)
          sys_code = system('sort -u '//full_name//' | sort -k1.8 > '//
     *      TRIM(full_name)//'.tmp')
          sys_code = system('/usr/bin/mv '//TRIM(full_name)//
     *      '.tmp '//full_name)

        end subroutine


      end module
