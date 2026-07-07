      subroutine wallo_pou_output (ipou)

      use time_module
      use hydrograph_module
      use water_allocation_module

      implicit none

      integer, intent (in) :: ipou         !point of use number

      poum_om(ipou) = poum_om(ipou) + poud_om(ipou)

!!!!! daily print
      if (pco%water_allo%d == "y") then
        write (3110,*) time%day, time%mo, time%day_mo, time%yrc, ipou, pou(ipou)%name, &
          poud_om(ipou)%pods, poud_om(ipou)%pors

        if (pco%csvout == "y") then
          write (3114,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, ipou, pou(ipou)%name, &
            poud_om(ipou)%pods, poud_om(ipou)%pors
        end if
      end if

!!!!! monthly output accumulator
      if (time%end_mo == 1) then
        pouy_om(ipou) = pouy_om(ipou) + poum_om(ipou)

        if (pco%water_allo%m == "y") then
          write (3111,*) time%mo, time%day_mo, time%yrc, ipou, pou(ipou)%name, &
            poum_om(ipou)%pods, poum_om(ipou)%pors

          if (pco%csvout == "y") then
            write (3115,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, ipou, pou(ipou)%name, &
              poum_om(ipou)%pods, poum_om(ipou)%pors
          end if
        end if

        poum_om(ipou) = 0. * poum_om(ipou)
      end if

!!!!! yearly output accumulator
      if (time%end_yr == 1) then
        poua_om(ipou) = poua_om(ipou) + pouy_om(ipou)

        if (pco%water_allo%y == "y") then
          write (3112,*) time%mo, time%day_mo, time%yrc, ipou, pou(ipou)%name, &
            pouy_om(ipou)%pods, pouy_om(ipou)%pors

          if (pco%csvout == "y") then
            write (3116,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, ipou, pou(ipou)%name, &
              pouy_om(ipou)%pods, pouy_om(ipou)%pors
          end if
        end if

        pouy_om(ipou) = 0. * pouy_om(ipou)
      end if

!!!!! average annual output accumulator
      if (time%end_sim == 1 .and. time%yrs_prt > 0) then
        poua_om(ipou) = poua_om(ipou) / real(time%yrs_prt)

        if (pco%water_allo%a == "y") then
          write (3113,*) time%mo, time%day_mo, time%yrc, ipou, pou(ipou)%name, &
            poua_om(ipou)%pods, poua_om(ipou)%pors

          if (pco%csvout == "y") then
            write (3117,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, ipou, pou(ipou)%name, &
              poua_om(ipou)%pods, poua_om(ipou)%pors
          end if
        end if
      end if

      return

      end subroutine wallo_pou_output
