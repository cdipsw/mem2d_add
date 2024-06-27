c-- FILE_OPS -------------------------------------------------------------------
c
c   The file_ops module has standardized functions for manipulating files:
c   uncompressing, opening read only or append, etc. Note that the path and
c   filename strings passed to functions are standardized to character*100.
c
c   Used by: .detox/detox, .detox/rehab, .dsk2af/dsk2af, .logfile/logfile
c            .far/diagnostics/errchk, .far/fd_to_rd
c
c-------------------------------------------------------------------------------

        module file_ops

        save

        contains


c-- OPEN_READ ------------------------------------------------------------------
c
c   Opens a compressed or uncompressed file in read-only mode. These routines
c   all look for the uncompressed file first, then for the compressed file.
c
c-------------------------------------------------------------------------------
          subroutine open_read(fileunit, path, filename, code, err_unit)

            character*100 path, filename, full_name, compressed_name
            integer code, err_unit, fileunit, system, sys_code
            logical exists

            if (filename(LEN_TRIM(filename)-1:LEN_TRIM(filename)) .eq. '.Z') 
     *        write(filename(LEN_TRIM(filename)-1:LEN_TRIM(filename)),'(a2)') '  '
            full_name = TRIM(path)//TRIM(filename)
            inquire(file=full_name,exist=exists)
            if (.not. exists) then
              compressed_name = TRIM(full_name)//'.Z'
              inquire(file=compressed_name,exist=exists)
              if (exists) sys_code = system('uncompress -f '//compressed_name)
c              if (exists) sys_code = 
c     *            system('zcat '//compressed_name//' > '//TRIM(full_name))
            end if

            if (exists) then
              open(unit=fileunit,file=TRIM(full_name),action='read',iostat=code)
              call code_check(filename,code,err_unit)
            else
              write(err_unit,'(a,a)') 'WARNING: File does not exist, filename = ',
     *          TRIM(filename)
              code = -2
            end if

          end subroutine

c-- ZCAT_READ ------------------------------------------------------------------
c   UPDATED: unix compression taken offline 10/2019. Compressed files are
c   now simply uncompressed, not zcatted.
c
c   Opens a compressed or uncompressed file in read-only mode by zcatting (or
c   catting) a copy into /tmp. PLEASE USE WITH ZCAT CLOSE!
c
c-------------------------------------------------------------------------------
          subroutine zcat_read(fileunit, path, filename, err_code, err_unit)

            character*5 cat_cmd
            character*100 path, filename, full_name, compressed_name, temp_name
            integer err_code, err_unit, fileunit, system, sys_code
            logical exists

            if (filename(LEN_TRIM(filename)-1:LEN_TRIM(filename)) .eq. '.Z')
     *        write(filename(LEN_TRIM(filename)-1:LEN_TRIM(filename)),'(a2)') '  '
            full_name = TRIM(path)//TRIM(filename)
            inquire(file=full_name,exist=exists)

            if (.not. exists) then      !* Check for compressed version
              compressed_name = TRIM(full_name)//'.Z'
              inquire(file=compressed_name,exist=exists)
              if (exists) sys_code = system('/usr/bin/uncompress '//TRIM(compressed_name))
            end if

            inquire(file=full_name,exist=exists)
            if (exists) then
              open(unit=fileunit,file=full_name,action='read',iostat=err_code)
              call code_check(filename,err_code,err_unit)
            else
              write(err_unit,'(a,a)') 'WARNING: File does not exist, filename = ',
     *          TRIM(full_name)
              err_code = -2
            end if
          end subroutine


c-- OPEN_APPEND ----------------------------------------------------------------
c
c   Opens a compressed or uncompressed file in append mode.
c   NOTE: May run a script from .f90/csh_utils to create new directories.
c
c-------------------------------------------------------------------------------
          subroutine open_append(fileunit, path, filename, code, err_unit)

            character*100 path, filename, full_name, compressed_name
            integer code, err_unit, fileunit, system, sys_code
            logical exists

            if (filename(LEN_TRIM(filename)-1:LEN_TRIM(filename)) .eq. '.Z') 
     *        write(filename(LEN_TRIM(filename)-1:LEN_TRIM(filename)),'(a2)') '  '
            sys_code = system('/project/wvutil/f90_modules/csh_utils/mkdir-p '//
     *        TRIM(path))
            full_name = TRIM(path)//TRIM(filename)

            inquire(file=full_name,exist=exists)
            if (.not. exists) then
              compressed_name = TRIM(full_name)//'.Z'
              inquire(file=compressed_name,exist=exists)
              if (exists) sys_code = system('uncompress -f '//compressed_name)
            end if

            open(unit=fileunit,file=full_name,position='append',iostat=code)
            call code_check(filename,code,err_unit)

          end subroutine


c-- OPEN_REPLACE ---------------------------------------------------------------
c
c   Removes compressed or uncompressed version of a file, then replaces it.
c   Also used to create files (and directories) that don't already exist.
c   NOTE: May run a script from .f90/csh_utils to create new directories.
c
c-------------------------------------------------------------------------------
          subroutine open_replace(fileunit, path, filename, code, err_unit)

            character*100 path, filename, full_name, compressed_name
            integer code, err_unit, fileunit, system, sys_code
            logical exists

            if (filename(LEN_TRIM(filename)-1:LEN_TRIM(filename)) .eq. '.Z') 
     *        write(filename(LEN_TRIM(filename)-1:LEN_TRIM(filename)),'(a2)') '  '
            sys_code = system('/project/wvutil/f90_modules/csh_utils/mkdir-p '//
     *        TRIM(path))
c  --CSH--  sys_code = system('if (! -e '//TRIM(path)//') mkdir -p '//
c    *        TRIM(path))
c  -BOURNE- sys_code = system('if test ! -d '//TRIM(path)//'; then mkdir -p '//
c    *        TRIM(path)//'; fi')
            full_name = TRIM(path)//TRIM(filename)
            inquire(file=full_name,exist=exists)
            if (.not. exists) then
              compressed_name = TRIM(full_name)//'.Z'
              inquire(file=compressed_name,exist=exists)
              if (exists) sys_code = system('/usr/bin/rm '//compressed_name)
            else
              sys_code = system('/usr/bin/rm '//full_name)
            end if

            open(unit=fileunit,file=full_name,iostat=code)
            call code_check(filename,code,err_unit)

          end subroutine


c-- CODE_CHECK -----------------------------------------------------------------
c 
c   Checks the iostat code and displays error messages when appropriate.
c
c-------------------------------------------------------------------------------
          subroutine code_check(filename, code, err_unit)
            character*100 filename
            integer code, err_unit
            if (code .ne. 0) then
              write(err_unit,'(a,a,a,i4)') 'ERROR: File open, filename = ',
     *          TRIM(filename),', code = ',code
            end if
          end subroutine


c-- CLOSE_COMPRESS -------------------------------------------------------------
c   UPDATE: compression disabled 10/2019
c
c   Closes and compresses a file. Checks for file conflicts, writes error
c   (or sends mail for err_unit=99) if both comp and uncomp files exist.
c
c-------------------------------------------------------------------------------
          subroutine close_compress(fileunit, path, filename, code, err_unit)

            character*100 path, filename, full_name, compressed_name
            integer code, err_unit, fileunit, system, sys_code
            logical exists

            close(fileunit, iostat=code)
            if (code .ne. 0) then
              write(err_unit,'(2a,i5)') 'ERROR - Closing unit, file + code: ', 
     *          TRIM(filename), code
              return
            end if
c           full_name = TRIM(path)//TRIM(filename)
c           compressed_name = TRIM(full_name)//'.Z'
c           inquire(file=compressed_name,exist=exists)
c           if (.not. exists) then
c             sys_code = system('/usr/bin/compress -f '//full_name)
c             code = 0
c           else
c             if (err_unit .eq. 99) then 
c               sys_code = system('echo Both compressed, uncompressed files 
c    *            exist for '//full_name// ' > message')
c               sys_code = system
c    *           ('mail -s ''FILE COMPRESSION PROBLEM'' nine_trk < message')
c               sys_code = system('/usr/bin/rm message')
c             else
c               write(err_unit,'(a,a,a)') 'COMPRESSION ERROR: both ',
c    *            'compressed, uncompressed files exist for ',full_name
c             end if
c           end if

          end subroutine

c-- ZCAT_CLOSE -----------------------------------------------------------------
c
c   Closes and removes a file after using open_read (which zcats the 
c   compressed file). 
c
c-------------------------------------------------------------------------------
          subroutine zcat_close(fileunit)

            integer fileunit

c           close(fileunit, status='delete')
            close(fileunit)

          end subroutine


c-- LOAD_FILE ------------------------------------------------------------------
c
c Load path/file into "array", and return number of items loaded.
c Returns read error code.
c
c-------------------------------------------------------------------------------
        subroutine load_file(path, file, array, num_items, code)

           character*100 path, file, array(3000)
           integer :: num_items, code, tmp_unit=99, to_screen=6

           call open_read(tmp_unit,path,file,code, to_screen)
           if ( code .ne. 0 ) return
           i = 1
           do while ( i .le. 3000 .and. code .eq. 0 )
             read(tmp_unit,'(a100)',iostat=code) array(i)
             if ( code .gt. 0 ) return
             if ( code .eq. 0 ) i = i + 1
           end do
           num_items = i - 1
           close(tmp_unit)
           code = 0

        end subroutine


c-- ARRAY2FILE ----------------------------------------------------------------
c
c Write portions of a stored array in fortran to an ascii path/file.
c If mode is "append", then appends to file, else replaces file.
c
c-------------------------------------------------------------------------------
        subroutine array2file(path, file, array, first_line, last_line, mode, code)

           character*100 path, file, array(3000)
           character*7 mode
           integer :: first_line, last_line, code, tmp_unit=99, to_screen=6

           if ( mode .eq. 'append' ) then
             call open_append(tmp_unit,path,file,code, to_screen)
           else
             call open_replace(tmp_unit,path,file,code, to_screen)
           end if

           if ( code .ne. 0 ) return
           do i=first_line, last_line
             write(tmp_unit,'(a100)') array(i)
           end do

           close(tmp_unit)

        end subroutine

      end
