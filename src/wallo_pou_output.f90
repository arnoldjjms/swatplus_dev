      subroutine wallo_pou_output (ipou)
    
      use time_module
      use basin_module
      use hydrograph_module
      
      implicit none
      
      integer, intent (in) :: ipou         !             |

      poum_om(ipou) = poum_om(ipou) + poud_om(ipou) 
      
!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%sd_chan%d == "y") then
          write (4808,*) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,     &
                                                                                           poud_om(ipou)
           if (pco%csvout == "y") then
             write (4812,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, &
                                                                                ob(iob)%name, poud_om(ipou)
           end if
        end if
      end if

!!!!! monthly print
        if (time%end_mo == 1) then
          pouy_om(ipou) = pouy_om(ipou) + poum_om(ipou)
          
          if (pco%sd_chan%m == "y") then
          write (4808,*) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,     &
                                                                                           poum_om(ipou)
           if (pco%csvout == "y") then
             write (4812,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, &
                                                                                ob(iob)%name, poum_om(ipou)
          end if
        end if
        poud_om(ipou) = hz
        end if

!!!!! yearly print
      if (time%end_yr == 1) then
          poua_om(ipou) = poua_om(ipou) + pouy_om(ipou)
          
          if (pco%sd_chan%m == "y") then
          write (4808,*) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,     &
                                                                                           pouy_om(ipou)
           if (pco%csvout == "y") then
             write (4812,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, &
                                                                                ob(iob)%name, pouy_om(ipou)
          end if
        end if
        pouy_om(ipou) = hz
      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        poua_om(ipou) = poua_om(ipou) / time%yrs_prt
        
        if (pco%sd_chan%a == "y") then
          write (4808,*) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name,     &
                                                                                           poua_om(ipou)
           if (pco%csvout == "y") then
             write (4812,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, &
                                                                                ob(iob)%name, poua_om(ipou)
        end if
       end if
     end if 
      
      return

      end subroutine wallo_pou_output