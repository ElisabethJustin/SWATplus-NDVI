      subroutine basin_output
      
      use time_module
      use hydrograph_module
      use calibration_data_module
      use output_landscape_module
      use basin_module
      
      implicit none

      integer :: ihru     !none      |counter
      integer :: iihru    !          |
      real :: const       !          |constant used for rate, days, etc
      real :: sw_init       !          |
      real :: sno_init       !          |
              
      !! zero daily basin outputs before summing
      sw_init = bwb_d%sw_init
      sno_init = bwb_d%sno_init
      bwb_d = hwbz
      bwb_d%sw_init = sw_init
      bwb_d%sno_init = sno_init
      bnb_d = hnbz
      bls_d = hlsz
      bpw_d = hpwz
      
        ! summing subbasin output for the basin
        do ihru = 1, sp_ob%hru
          iihru = lsu_elem(ihru)%obtypno
          if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
            const = lsu_elem(iihru)%bsn_frac
            if (lsu_elem(iihru)%obtyp == "hru") then
              bwb_d = bwb_d + hwb_d(iihru) * const
              bwb_d%sw_final = bwb_d%sw_final + hwb_d(iihru)%sw_final * const
              bwb_d%sno_final = bwb_d%sno_final + hwb_d(iihru)%sno_final * const
              bnb_d = bnb_d + hnb_d(iihru) * const
              bls_d = bls_d + hls_d(iihru) * const
              bpw_d = bpw_d + hpw_d(iihru) * const
            end if
          end if
        end do
          
            ! or if it is not routed and not in a subbasin
        do ihru = 1, sp_ob%hru_lte
          iihru = lsu_elem(ihru)%obtypno
          if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
            const = lsu_elem(iihru)%bsn_frac
            if (lsu_elem(iihru)%obtyp == "hlt") then
              !const = lsu_elem(iihru)%bsn_frac
              bwb_d = bwb_d + hltwb_d(iihru) * const
              bwb_d%sw_final = bwb_d%sw_final + hltwb_d(iihru)%sw_final * const
              bwb_d%sno_final = bwb_d%sno_final + hltwb_d(iihru)%sno_final * const
              bnb_d = bnb_d + hltnb_d(iihru) * const
              bls_d = bls_d + hltls_d(iihru) * const
              bpw_d = bpw_d + hltpw_d(iihru) * const
            end if 
          end if
        end do

        !! sum monthly variables
        bwb_m = bwb_m + bwb_d
        bnb_m = bnb_m + bnb_d
        bls_m = bls_m + bls_d
        bpw_m = bpw_m + bpw_d
        
!!!!! daily print - BASIN
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_bsn%d == "y") then
            bwb_d%sw = (bwb_d%sw_init + bwb_d%sw_final) / 2.
            bwb_d%snopack = (bwb_d%sno_init + bwb_d%sno_final) / 2.
            write (2050,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bwb_d  !! waterbal
            !!write (2050,101) time%mo, time%day_mo, time%yrc, bsn%name, bwb_d%surq_gen, bwb_d%latq, bwb_d%perc, bwb_d%et, bwb_d%eplant, bwb_d%esoil, bwb_d%sw, bwb_d%sw_300, bwb_d%pet, bwb_d%irr, bwb_d%surq_runon, bwb_d%latq_runon  !! waterbal  !!EB
            if (pco%csvout == "y") then 
              write (2054,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bwb_d  !! waterbal
            end if
            bwb_d%sw_init = bwb_d%sw_final
            bwb_d%sno_init = bwb_d%sno_final
          end if 
          if (pco%nb_bsn%d == "y") then
            write (2060,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bnb_d  !! nutrient bal
            if (pco%csvout == "y") then 
              write (2064,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bnb_d  !! nutrient bal
            end if
          end if
          if (pco%ls_bsn%d == "y") then
            write (2070,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bls_d  !! losses
            if (pco%csvout == "y") then 
              write (2074,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bls_d  !! losses
            end if 
          end if
          if (pco%pw_bsn%d == "y") then
            write (2080,100) time%mo, time%day_mo, time%yrc, bsn%name, bpw_d%lai, bpw_d%bioms, bpw_d%yield, bpw_d%residue, bpw_d%sol_tmp, bpw_d%strsw, bpw_d%phubase0  !! plant weather
            if (pco%csvout == "y") then 
              write (2084,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bpw_d  !! plant weather
            end if
          end if
        end if

!!!!! monthly print - BASIN
        if (time%end_mo == 1) then
          bwb_y = bwb_y + bwb_m
          bnb_y = bnb_y + bnb_m
          bls_y = bls_y + bls_m
          bpw_y = bpw_y + bpw_m
          
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          bwb_m = bwb_m // const
          bpw_m = bpw_m // const
          if (pco%wb_bsn%m == "y") then
            bwb_m%sw_final = bwb_d%sw_final
            bwb_m%sno_final = bwb_d%sno_final
            write (2051,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bwb_m
            !!write (2051,100) time%mo, time%day_mo, time%yrc, bsn%name, bwb_m%surq_gen, bwb_m%latq, bwb_m%perc, bwb_m%et, bwb_m%eplant, bwb_m%esoil, bwb_m%sw, bwb_m%sw_300, bwb_m%pet, bwb_m%irr, bwb_m%surq_runon, bwb_m%latq_runon !!EB
            if (pco%csvout == "y") then 
              write (2055,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bwb_m
            end if
            bwb_m%sw_init = bwb_m%sw_final
            bwb_m%sno_init = bwb_m%sno_final
          end if
          if (pco%nb_bsn%m == "y") then 
            write (2061,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bnb_m
            if (pco%csvout == "y") then 
              write (2065,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bnb_m
            end if 
          end if
          if (pco%ls_bsn%m == "y") then  
            write (2071,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bls_m
            if (pco%csvout == "y") then 
              write (2075,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bls_m
            end if 
          end if
          if (pco%pw_bsn%m == "y") then
            write (2081,100) time%mo, time%day_mo, time%yrc, bsn%name, bpw_m%lai, bpw_m%bioms, bpw_m%yield, bpw_m%residue, bpw_m%sol_tmp, bpw_m%strsw, bpw_m%phubase0  !!EB
            if (pco%csvout == "y") then 
              write (2085,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bpw_m
            end if 
          end if
  
          sw_init = bwb_m%sw_final
          sno_init = bwb_m%sno_final
          bwb_m = hwbz
          bwb_m%sw_init = sw_init
          bwb_m%sno_init = sno_init
          bnb_m = hnbz
          bls_m = hlsz
          bpw_m = hpwz
        end if

!!!!! yearly print - BASIN
        if (time%end_yr == 1) then
           bwb_a = bwb_a + bwb_y
           bnb_a = bnb_a + bnb_y
           bls_a = bls_a + bls_y
           bpw_a = bpw_a + bpw_y
           
           const = time%day_end_yr
           bwb_y = bwb_y // const
           bpw_y = bpw_y // const
           
           if (pco%wb_bsn%y == "y") then
             bwb_y%sw_final = bwb_d%sw_final
             bwb_y%sno_final = bwb_d%sno_final
             write (2052,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bwb_y
             !!write (2052,100) time%mo, time%day_mo, time%yrc, bsn%name, bwb_y%surq_gen, bwb_y%latq, bwb_y%perc, bwb_y%et, bwb_y%eplant, bwb_y%esoil, bwb_y%sw, bwb_y%sw_300, bwb_y%pet,  bwb_y%irr, bwb_y%surq_runon, bwb_y%latq_runon
             if (pco%csvout == "y") then 
                write (2056,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bwb_y
             end if
             bwb_y%sw_init = bwb_y%sw_final
             bwb_y%sno_init = bwb_y%sno_final
           end if
           if (pco%nb_bsn%y == "y") then
             write (2062,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bnb_y
             if (pco%csvout == "y") then 
               write (2066,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bnb_y
             end if
           end if
           if (pco%ls_bsn%y == "y") then
             write (2072,100)time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bls_y
             if (pco%csvout == "y") then 
                write (2076,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bls_y
             end if 
           end if
           if (pco%pw_bsn%y == "y") then
             write (2082,100) time%mo, time%day_mo, time%yrc, bsn%name, bpw_y%lai, bpw_y%bioms, bpw_y%yield, bpw_y%residue, bpw_y%sol_tmp, bpw_y%strsw, bpw_y%phubase0 !!EB
             if (pco%csvout == "y") then 
               write (2086,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bpw_y
             end if 
           end if
 
!!!!! zero yearly variables
          sw_init = bwb_y%sw_final
          sno_init = bwb_y%sno_final
          bwb_y = hwbz
          bwb_y%sw_init = sw_init
          bwb_y%sno_init = sno_init
          bnb_y = hnbz
          bls_y = hlsz
          bpw_y = hpwz
        end if
        
!!!!! average annual print - BASIN
      if (time%end_sim == 1 .and. pco%wb_bsn%a == "y") then
        sw_init = bwb_a%sw_init
        sno_init = bwb_a%sno_init
        bwb_a = bwb_a / time%yrs_prt
        bwb_a = bwb_a // time%days_prt
        bwb_a%sw_init = sw_init
        bwb_a%sw_final = bwb_d%sw_final
        bwb_a%sno_init = sno_init
        bwb_a%sno_final = bwb_d%sno_final

        write (2053,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bwb_a
        !!write (2053,103) time%mo, time%day_mo, time%yrc, bsn%name, bwb_a%surq_gen, bwb_a%latq, bwb_a%perc, bwb_a%et, bwb_a%eplant, bwb_a%esoil, bwb_a%sw, bwb_a%sw_300, bwb_a%pet, bwb_a%irr, bwb_a%surq_runon, bwb_a%latq_runon
        if (pco%csvout == "y") then 
          write (2057,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bwb_a, cal_sim
        end if
        bwb_a = hwbz
      end if
      if (time%end_sim == 1 .and. pco%nb_bsn%a == "y") then
        bnb_a = bnb_a / time%yrs_prt
        write (2063,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bnb_a
        if (pco%csvout == "y") then 
          write (2067,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bnb_a
        end if 
        bnb_a = hnbz
      end if
      if (time%end_sim == 1 .and. pco%ls_bsn%a == "y") then     
        bls_a = bls_a / time%yrs_prt
        write (2073,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bls_a
        if (pco%csvout == "y") then 
          write (2077,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", bsn%name, bls_a
        end if 
        bls_a = hlsz
      end if
      if (time%end_sim == 1 .and. pco%pw_bsn%a == "y") then     
        bpw_a = bpw_a / time%yrs_prt
        bpw_a = bpw_a // time%days_prt
        write (2083,100) time%mo, time%day_mo, time%yrc, bsn%name, bpw_a%lai, bpw_a%bioms, bpw_a%yield, bpw_a%residue, bpw_a%sol_tmp, bpw_a%strsw, bpw_a%phubase0 !!EB
        if (pco%csvout == "y") then 
          write (2087,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "        1", bsn%name, bpw_a
        end if 
        bpw_a = hpwz
      end if
      
      return

100   format (4i6,2a8,2x,a16,32f10.3) 
101   format (3i6,2x,a16,12f10.3)
       
      end subroutine basin_output
