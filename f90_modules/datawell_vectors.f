c-- DATAWELL_VECTORS -----------------------------------------------------------
c
c   This module is used to read, write, and manipulate datawell
c   vectors, 10-byte arrays that are the basic transmission unit of datawell
c   directional buoys. Note that the receiver adds two bytes, the
c   error code and the counter to the 8 bytes sent by the buoy, for
c   a total of 10 bytes. The foundation of this module is the 'vector' data
c   data type, which stores the vector's ten bytes as integers along with 
c   the corresponding time.
c
c   Each vector has the following structure:
c     Byte 1: Error code                  Byte 6: z and x displacements
c     Byte 2: Counter                     Byte 7: x displacements
c     Byte 3: System (spectral) info      Byte 8: y displacements
c     Byte 4: System (spectral) info      Byte 9: y displacements, parity
c     Byte 5: z displacements             Byte 10: Parity code
c
c   Used by: .f90/buoy_utils, .f90/spectral_utils,
c            .fdiag/errchk, .fdisp/logfd
c
c-------------------------------------------------------------------------------

        module datawell_vectors

        use dates

        save

        type dw_vector
          integer error, count, sys1, sys2, zz, zx, xx, yy, yp, pp
          type(date_block) time
        end type

        contains


c-- INIT_DW_VEC ----------------------------------------------------------------
c
c   Creates a vector object from ten integers, time is NOT initialized
c
c-------------------------------------------------------------------------------
          type(dw_vector) function init_dw_vec(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
            integer a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
            init_dw_vec%error = a1
            init_dw_vec%count = a2
            init_dw_vec%sys1 = a3
            init_dw_vec%sys2 = a4
            init_dw_vec%zz = a5
            init_dw_vec%zx = a6
            init_dw_vec%xx = a7
            init_dw_vec%yy = a8
            init_dw_vec%yp = a9
            init_dw_vec%pp = a10
          end function


c-- WRITE_DW_VEC, WRITE_HEX_DW_VEC ---------------------------------------------
c
c   Outputs a vector's values in a standardized ten-column format; 
c   WRITE_HEX_VEC outputs the values in hex instead of decimal.
c   
c-------------------------------------------------------------------------------
          subroutine write_dw_vec(output_unit, vec, code)
            type(dw_vector) vec
            integer code, output_unit
            write(output_unit,'(10i5)',iostat=code) vec%error,vec%count,
     *        vec%sys1,vec%sys2,vec%zz,vec%zx,vec%xx,vec%yy,vec%yp,vec%pp
          end subroutine


          subroutine write_hex_dw_vec(output_unit, vec, code)
            type(dw_vector) vec
            integer code, output_unit
            write(output_unit,'(10z5)',iostat=code) vec%error,vec%count,
     *        vec%sys1,vec%sys2,vec%zz,vec%zx,vec%xx,vec%yy,vec%yp,vec%pp
          end subroutine


c-- READ_DW_VEC ----------------------------------------------------------------
c
c   Reads in a dw_vector from the standard ten-column format.
c
c-------------------------------------------------------------------------------
          type(dw_vector) function read_dw_vec(input_unit, code)
            integer code, input_unit
            read(input_unit,'(10i5)',iostat=code) read_dw_vec%error,
     *        read_dw_vec%count,read_dw_vec%sys1,read_dw_vec%sys2,read_dw_vec%zz,
     *        read_dw_vec%zx,read_dw_vec%xx,read_dw_vec%yy,read_dw_vec%yp,
     *        read_dw_vec%pp
          end function


c-- IS_SYNC --------------------------------------------------------------------
c
c   Checks if a dw_vector is a sync vector, the first vector on a page
c
c-------------------------------------------------------------------------------
          logical function is_sync(vec)
            type(dw_vector) vec
            if (vec%error .le. 1 .and. vec%sys1 .eq. 127 .and.
     *        vec%sys2 .eq. 255) then
              is_sync = .true.
            else
              is_sync = .false.
            end if
          end function


c-- GET_PAGE -------------------------------------------------------------------
c
c   Returns the page number of a dw_vector; the vector passed in must be the
c   the vector immediately following a sync.
c
c-------------------------------------------------------------------------------
          integer function get_page(vec)
            type(dw_vector) vec
            get_page = top(vec%sys1,4)
          end function


c-- GET_XXX, GET_YYY, GET_ZZZ --------------------------------------------------
c
c   Extract the x (N-S), y (E-W), and z (vert) displacements from any dw_vector
c
c-------------------------------------------------------------------------------
          integer function get_xxx(vec)
            type(dw_vector) vec
            get_xxx = add_sign((up(bottom(vec%zx,4),8) + vec%xx),12)
          end function


          integer function get_yyy(vec)
            type(dw_vector) vec
            get_yyy = add_sign((up(vec%yy,4) + top(vec%yp,4)),12)
          end function


          integer function get_zzz(vec)
            type(dw_vector) vec
            get_zzz = add_sign((up(vec%zz,4) + top(vec%zx,4)),12)
          end function

         
c-- UP, TOP, BOTTOM, ADD_SIGN --------------------------------------------------
c
c   Bit manipulation functions:
c     UP       - Used to raise the most significant bits of a number up the
c                appropriate number of bits (i.e. a left bit shift)
c     TOP      - Returns the specified number of bits from the top of a byte 
c     BOTTOM   - Returns the specified number of bits from the bottom of a 
c                number (any length)
c     ADD_SIGN - Signs a number (any length) based on the top bit value
c
c-------------------------------------------------------------------------------
          integer function up(val, bits)
            integer bits, val
            up = val * 2**bits
          end function

         
          integer function top(val, bits)
            integer bits, val
            top = val / 2**(8-bits)
          end function

         
          integer function bottom(val, bits)
            integer bits, val
            bottom = mod(val,2**bits)
          end function


          integer function add_sign(value, pos)
            integer pos, value
            if (value/2**(pos-1) .eq. 1) then
              add_sign = -1 * (value - 2**(pos-1))
            else
              add_sign = value
            end if
          end function


        end
