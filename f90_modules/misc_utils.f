c-- MISC_UTILS -----------------------------------------------------------------
c
c   This module contains general miscellaneous utility subroutines such as
c   compressing files, displaying files, displaying help info, various
c   system commands wrapped in subroutines. Get_apid returns the ascii
c   5 char pid (useful for temporary storage files) and Mail_it, calls
c   mailx nicely.
c
c   Used by: .dsk2af, .dd/data_disseminator, .de/data_remove
c
c-------------------------------------------------------------------------------

        module misc_utils

        use strings
        use dates
        use file_ops

 
        save
   
        character*100 MU_wkg_path, MU_wkg_link
        data MU_wkg_path /'/project/spacw3/working/'/
        data MU_wkg_link /'/spacw3/working/'/

        contains


c-- COUNT_LINES ----------------------------------------------------------------
c
c Returns the number of lines in a file.
c
c-------------------------------------------------------------------------------
      integer function count_lines(path, file)

      integer :: count, read_err, unit=99
      character*100 path, file
      character*1 tmp


      count_lines = 0
      open(unit,FILE=TRIM(path)//TRIM(file),IOSTAT=read_err)
      if ( read_err .ne. 0 ) then
        count_lines = -1
      else
        do while ( read_err .eq. 0 )
          read(unit,'(a)',IOSTAT=read_err) tmp
          if ( read_err .eq. 0 ) then
            count_lines = count_lines + 1
          elseif ( read_err .gt. 0 ) then
            count_lines = -1
            return
          end if
        end do
      end if
      close(unit)

      end function


c-- MAKE_FULLPATH --------------------------------------------------------------
c
c   Makes a fullpath out of path and file. Path and/or file can be a string
c   terminated with an asterisk.
c
c-------------------------------------------------------------------------------
      character*100 function make_fullpath(path,file)

        character*100 path, file

        make_fullpath = path(1:string_end100(path))//file(1:string_end100(file))

      end function


c-- SORT_FILE ------------------------------------------------------------------
c
c   Applies the unix sort command (with options) to path and file. Returns
c   non-zero error code if problem. 
c
c   Note: if the file is unchanged upon a call to compress, an error code
c   of 3 is returned. This happens if the file cannot be reduced by compression,
c   which is not an error.
c
c-------------------------------------------------------------------------------
       integer function sort_file(path, file, options)

         character*100 path, file, options, tmp_file
         character*500 cmd
         character*5 apid
         integer state, system
 
         state = compress_state(path, file)  !* save whether compressed or not
         if ( state .lt. 1 ) then !* no file, or both compressed and uncompressed
           sort_file = state
           return
         end if

         if ( state .eq. 2 ) then  !* compressed file, so uncompress
           sort_file = uncompress_file(path, file)
           if ( sort_file .ne. 0 ) return 
         end if

         apid = get_apid()
         tmp_file = TRIM(MU_wkg_path)//'sort'//apid
         
         cmd = 'sort '//options(1:string_end100(options))
         cmd = trim(cmd)//' '//trim(make_fullpath(path,file))//' > '//tmp_file

         sort_file = system(cmd)
         if ( sort_file .ne. 0 ) return

         sort_file = move_file(tmp_file, make_fullpath(path, file))
         if ( sort_file .ne. 0 ) return

         if ( state .eq. 2 ) sort_file = compress_file(path, file)

       end function
         
           

c-- CAT_FILE -------------------------------------------------------------------
c
c Cats file to screen. Fullpath can be a string ending with an asterisk.
c If the file is compressed, cat_file calls the unix cmd zcat.
c
c-------------------------------------------------------------------------------
       integer function cat_file(fullpath)
          character fullpath*100, cmd*200
          integer system, last
          last = lnblnk(fullpath)
          if ( fullpath(last:last) .eq. 'Z' .or. 
     *         fullpath(last-1:last-1) .eq. 'Z') then
            cmd = 'zcat '//fullpath(1:string_end100(fullpath))
          else
            cmd = 'cat '//fullpath(1:string_end100(fullpath))
          end if
          cat_file = system(cmd)
       end function


c-- CAT_LIST -------------------------------------------------------------------
c
c Cats the files in fullpath (list of file names) to unit.
c Returns -1 if no data found.
c COMPILE ISSUE: Doesn't compile properly on x86 as of 10/2008
c-------------------------------------------------------------------------------
       integer function cat_list(fullpath)

          character fullpath*100, path*100, file*100, line*100
          integer :: unit=99, err_code, err_unit, read_code

          cat_list = 0

c         call split_fullname(fullpath(1:string_end100(fullpath)), path, file)
          call split_fullname(fullpath, path, file)
          call open_read(unit, path, file, err_code, err_unit)

          read(unit, '(a)', IOSTAT=read_code) line
          if ( read_code .eq. -1 ) then
            cat_list = -1
            return
          end if

          do while ( read_code .eq. 0 )
            err_code = cat_file(line)
            if ( err_code .gt. 0 ) cat_list = err_code
            read(unit, '(a100)', IOSTAT=read_code) line
          end do

          if ( read_code .gt. 0 ) cat_list = read_code

       end function

c-- COMPRESS_STATE -------------------------------------------------------------
c
c   Used by compress_file and uncompress_file.
c
c   Returns -1 if both compressed and uncompressed exist.
c   Returns  0 if neither compressed nor uncompressed exist.
c   Returns  1 if uncompressed exists.
c   Returns  2 if compressed exists.
c
c
c-------------------------------------------------------------------------------
       integer function compress_state(path, file)
          character path*100, file*100, cmd*200, full_path*100
          logical :: uncompressed = .false., compressed = .false.
          integer f_sz
          f_sz = string_end100(file)
          if ( file(f_sz-1:f_sz) .eq. '.Z' ) file = file(1:f_sz-2)
          full_path = make_fullpath(path, file)
          inquire(file=full_path,exist=uncompressed)
          inquire(file=full_path(1:lnblnk(full_path))//'.Z',exist=compressed)
          if ( compressed .and. uncompressed ) compress_state = -1
          if ( .not. compressed .and. uncompressed ) compress_state = 1
          if ( compressed .and. .not. uncompressed ) compress_state = 2
          if ( .not. compressed .and. .not. uncompressed ) compress_state = 0
       end function

c-- COMPRESS_FILE --------------------------------------------------------------
c
c   Compresses file. If already compressed does nothing. Returns non-zero
c   error code if both compressed and uncompressed exist, or the specified
c   file does not exist. Returns 3 if system command fails.
c
c-------------------------------------------------------------------------------
       integer function compress_file(path, file)

          character path*100, file*100, cmd*200, fullpath*100
          integer system, comp_state, code

          comp_state = compress_state(path,file) 

          if ( comp_state .eq. 1 ) then  !* uncompressed exists, let's compress
            cmd = 'compress '//make_fullpath(path, file)
            code = system(cmd)
            if ( code .ne. 0 ) then
              compress_file = 3
            else
              compress_file = 0
            end if
          else 
            compress_file = comp_state
          end if

       end function

c-- UNCOMPRESS_FILE ------------------------------------------------------------
c
c   Uncompresses the specified file with path. If already uncompressed
c   does nothing. Returns non zero error code if both compressed and
c   uncompressed exist, or the specified file does not exist.
c   Returns 3 if the system command fails.
c
c-------------------------------------------------------------------------------
       integer function uncompress_file(path, file)

          character path*100, file*100, cmd*200, fullpath*100
          integer code, system, comp_state

          comp_state = compress_state(path,file)

          fullpath = path(1:string_end100(path))//file(1:string_end100(file))

          if ( comp_state .eq. 2 ) then  !* compressed exists, uncompress
            cmd = 'uncompress '//path(1:string_end100(path))//file(1:string_end100(file))
            code = system(cmd)
            if ( code .ne. 0 ) then
              uncompress_file = 3
            else
              uncompress_file = 0
            end if
          else
              uncompress_file = comp_state
          end if
          
       end function


c-- REMOVE_FILE ----------------------------------------------------------------
c
c   System call to remove a file.
c
c-------------------------------------------------------------------------------
       integer function remove_file(path, file )

          character path*100, file*100, cmd*200
          integer system
          cmd = '/usr/bin/rm '//make_fullpath(path, file)
          remove_file = system(cmd)

        end function

c-- MOVE_FILE ------------------------------------------------------------------
c
c   System call to move file from fullpath1 to fullpath2, where fullpath
c   can be a string that ends with an asterisk.
c
c-------------------------------------------------------------------------------
       integer function move_file(fullpath1, fullpath2)

          character fullpath1*100, fullpath2*100, cmd*300
          integer system

          cmd = '/usr/bin/mv '//fullpath1(1:string_end100(fullpath1))//
     *          ' '//fullpath2(1:string_end100(fullpath2))
          move_file = system(cmd)

       end function

c-- COPY_FILE ------------------------------------------------------------------
c
c   System call to copy file from fullpath1 to fullpath2, where fullpath
c   can be a string that ends with an asterisk.
c   Usage: code = copy_file('/home/grant/test1.dat*','/home/grant/test2.dat*')
c
c-------------------------------------------------------------------------------
       integer function copy_file(fullpath1, fullpath2 )

          character fullpath1*100, fullpath2*100, cmd*300
          integer system

          cmd = '/usr/bin/cp '//fullpath1(1:string_end100(fullpath1))//
     *          ' '//fullpath2(1:string_end100(fullpath2))
          copy_file = system(cmd)

       end function

c-- DISPLAY_HELP ---------------------------------------------------------------
c
c Cats program headers to screen. The first two characters of each line of 
c the header must be "ch".
c
c-------------------------------------------------------------------------------
       subroutine display_help(fullpath)

          character fullpath*100, cmd*100
          integer code, system

          code = system('grep -h "^ch" '//fullpath(1:string_end100(fullpath)))

       end subroutine


c-- UNIX_LS --------------------------------------------------------------------
c
c Performs an ls -d on search_string and puts results in out_path/out_name.
c
c-------------------------------------------------------------------------------
      subroutine unix_ls(out_path, out_name, search_string, sys_code)

        character*100 out_path, out_name, search_string, full_name
        character*500 cmd
        integer sys_code, system

        full_name = TRIM(out_path)//TRIM(out_name)
        cmd = 'setenv SHELL /bin/tcsh; ls -d '//TRIM(search_string)//' > '//TRIM(full_name)
        sys_code = system(cmd)

      end subroutine


c-- RUN_CMD --------------------------------------------------------------------
c
c   Run a system call with a supplied command.
c   Usage: code = run_cmd('sort test.dat >> tmp.dat*')
c
c-------------------------------------------------------------------------------
       integer function run_cmd(cmd) 
          character cmd*500
          integer system
          run_cmd = system(cmd(1:string_end500(cmd)))
       end function

c-- RUN_CMD1 -------------------------------------------------------------------
c
c   Run a system call with 1 cmd line argument.
c   Usage: code = run_cmd1('sort*','test.dat*')
c
c-------------------------------------------------------------------------------
       integer function run_cmd1(call, arg1) 
          character fullpath*100, call*100, arg1*100, cmd*200
          integer system
          cmd = call(1:string_end100(call))//' '//
     *          arg1(1:string_end100(arg1))
          run_cmd1 = system(cmd)
       end function

c-- RUN_CMD2 -------------------------------------------------------------------
c
c   Run a system call with 2 cmd line arguments.
c   Usage: code = run_cmd2('cp*','test.dat*','test2.dat*')
c
c-------------------------------------------------------------------------------
       integer function run_cmd2(call, arg1, arg2) 
          character fullpath*100, call*100, arg1*100, arg2*100, cmd*300
          integer system
          cmd = call(1:string_end100(call))//' '//
     *          arg1(1:string_end100(arg1))//' '//
     *          arg2(1:string_end100(arg2))
          run_cmd2 = system(cmd)
       end function

c-- SPLIT_FULLNAME -------------------------------------------------------------
c
c Sets path and file given a full path filename.
c
c-------------------------------------------------------------------------------
       subroutine split_fullname(fullname, path, file)
          character*100 fullname, path, file
          integer last, pos 
          logical found
          found = .false.
          last = lnblnk(fullname)
          pos = last + 1
          do while (pos .gt. 1 .and. .not. found)
              pos = pos - 1
              if (fullname(pos:pos) .eq. '/') then
                found = .true.
              end if
          end do
          if ( .not. found .and. pos .eq. 1 ) pos = 0
          path = fullname(1:pos)
          file = fullname(pos+1:last)
      end subroutine

                
c-- GET_APID -------------------------------------------------------------------
c
c Returns pid in character*5
c
c-------------------------------------------------------------------------------
       character*5 function get_apid()

         integer s
         real rn(5)
         character symbols(62)

         data symbols
     *     /'a', 'b', 'c', 'd', 'e', 'f', 'g',
     *      'h', 'i', 'j', 'k', 'l', 'm', 'n',
     *      'o', 'p', 'q', 'r', 's', 't', 'u',
     *      'v', 'w', 'x', 'y', 'z', '0', '1',
     *      '2', '3', '4', '5', '6', '7', '8',
     *      '9', 'A', 'B', 'C', 'D', 'E', 'F',
     *      'G', 'H', 'I', 'J', 'K', 'L', 'M',
     *      'N', 'O', 'P', 'Q', 'R', 'S', 'T',
     *      'U', 'V', 'W', 'X', 'Y', 'Z'/

         do i=1,5
           call init_random_seed()
           call random_number(rn(1:5))
           s = int(rn(i)*(62+1-1))+1
           get_apid(i:i) = symbols(s)
         end do

       end function


c-- INIT_RANDOM_SEED -----------------------------------------------------------
       subroutine init_random_seed()
         integer :: i, n
         integer*8   clock
         integer, dimension(:), allocatable :: seed
         call random_seed(size=n)
         allocate(seed(n))
         call system_clock(count=clock)
         seed = clock + 37 * (/ (i - 1, i = 1, n) /)
         call random_seed(put=seed)
         deallocate(seed)
       end


c-- GET_APID_WITH_TIMESTAMP ----------------------------------------------------
c
c Returns pid with time stamp, e.g. 01234_200210241513
c
c-------------------------------------------------------------------------------
       character*20 function get_apid_with_timestamp()
           write(get_apid_with_timestamp,'(a5,a1,a14)') 
     *       get_apid(),'_',make_datestring(current_utc())
       end function


c-- MAIL_IT --------------------------------------------------------------------
c
c Uses mailx to send a 'message' to 'recipient' with 'subject'.
c Recipient can be a list, e.g. 'jot,corey,grant' or 'jot corey grant'.
c One of message or subject must have at least one non-blank character.
c Strings shorter than 100 chars should end with '*'.
c
c Example usage:
c
c    call mail_it('grant*', 'some subject with no concatenation*', ' *', code)
c 
c    or
c
c    character*100 msg, sub
c    msg = 'some message'//' with concatenation'
c    sub = 'subject with'//' concatenation'
c    call mail_it('nine_trk*', sub, msg, code)
c 
c
c-------------------------------------------------------------------------------
      subroutine mail_it(recipients, subject, message, code)
         character*100 recipients  
         character*100 subject
         character*100 message
         character*500 cmd
         integer system, code, r_len, s_len, m_len 
         real min
         s_len = min(lnblnk(subject),string_end100(subject))	!* subject line
         m_len = min(lnblnk(message),string_end100(message))
         r_len = min(lnblnk(recipients),string_end100(recipients))
         if ( (s_len .le. 1 .and. m_len .le. 1) .or. r_len .le. 1 ) then
           code = 1
         else
           cmd = 'echo "'//message(1:m_len)//'" | mailx -s "'//subject(1:s_len)//
     *           '" '//recipients(1:r_len)
           code = system(cmd)
         end if
       end subroutine

c-- UNIX -----------------------------------------------------------------------
c
c   Similar to matlab's unix command. Returns the first 500 characters of the
c   output of a unix command. Note, this command can be a little testy,
c   if it doesn't work the first time, try a slight variation of your original
c   unix command, it might just work!
c
c-------------------------------------------------------------------------------
      character*500 function unix(call, code)

        character*500 call, path, file
        character pid*5, tmp*500
        integer :: 
     *      code, call_len,  
     *      e_idx,		!* end index
     *      s_idx,		!* start index
     *      to_screen=6,  tmp_len,
     *      unit=99

c -- test if unix call is too long

        call_len = lnblnk(call)
        if ( call_len .gt. 465 ) then
          code = 1  !* call txt is too long
          return
        end if

c -- clear return character string

        do i=1,500
          unix(i:i) = ' '
        end do

c -- run the unix command and put output in path/file.

        pid = get_apid()
        path = MU_wkg_path
        file = 'tmp.'//pid
        call(1:500) = call(1:call_len)//' > '
     *                //path(1:lnblnk(path))//file(1:lnblnk(file))
        code = run_cmd(call)
        if ( code .ne. 0 ) return

c -- open output file and read contents filling output string

        call open_read(unit, path, file, code, to_screen)
        if ( code .ne. 0 ) return
        e_idx = 0
        do while ( code .eq. 0 )
          read(unit,'(a500)',iostat=code) tmp
          if ( code .eq. 0 ) then
            tmp_len = lnblnk(tmp)
            s_idx = e_idx + 1
            e_idx = tmp_len + e_idx
            if ( e_idx .gt. 500 ) then
              code = 1
              e_idx = 500
              tmp_len = e_idx - s_idx + 1
            end if
            unix(s_idx:e_idx) = tmp(1:tmp_len)
          end if
        end do
    
        close(unit)
        code = 0
        
      end function

c-- MAKE_INDEX -----------------------------------------------------------------
c
c Fills the array item_array with the output of the system command cmd_str,
c which is usually 'ls -r', or something similar. Also returns the number
c of items returned.
c
c-------------------------------------------------------------------------------
        subroutine make_index(item_dir, search_str, cmd_str, item_array, num_items, code)

           character*5 pid
           character*100 item_dir, search_str, item_array(3000)
           character*100 cmd_str, w_dir, i_file, file
           character*500 cmd
           integer :: num_items, code, system, tmp_unit=99

           pid = get_apid()
c          w_dir = MU_wkg_path
           w_dir = '/tmp/'
           i_file = 'file'//pid
           file = TRIM(w_dir)//i_file

           cmd = TRIM(cmd_str)//' '//TRIM(item_dir)//
     *           TRIM(search_str)//' > '//file

           code = system(cmd)
           if ( code .ne. 0 ) return

           call open_read(tmp_unit,w_dir,i_file,code,6)
           if ( code .ne. 0 ) return

           i = 1
           do while ( i .le. 3000 .and. code .eq. 0 )
             read(tmp_unit,'(a100)',iostat=code) item_array(i)
             if ( code .eq. 0 ) then
               last = lnblnk(item_array(i))
               if ( item_array(i)(last:last)  .eq. '*' ) then
                 item_array(i) = item_array(i)(1:last-1)
               end if
               i = i + 1
             end if
           end do
           num_items = i - 1
           code = 0

           close(tmp_unit)

           err_code = remove_file(w_dir, i_file)

        end subroutine

      
      end !* MODULE END 
        
         
         
         
          
 
