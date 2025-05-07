      subroutine wallo_control (iwallo)
      
      use water_allocation_module
      use hydrograph_module   !, only : irrig, hz, recall
      use hru_module
      use basin_module
      use time_module
      use plant_module
      use soil_module
      use organic_mineral_mass_module
      use constituent_mass_module !rtb
      
      implicit none 

      integer, intent (inout) :: iwallo     !water allocation object number
      integer :: idmd = 0                   !water demand object number
      integer :: isrc = 0                   !source object number
      integer :: j = 0                      !hru number
      integer :: jj = 0                     !variable for passing
      integer :: irec = 0                   !recall id
      integer :: dum = 0
      real :: irr_mm = 0.                   !mm     |irrigation applied
      real :: div_total = 0.                !m3     |cumulative available diversion water
      real :: div_daily = 0.                !m3     |daily water diverted for irrigation
      

      !! zero demand, withdrawal, and unmet for entire allocation object
      wallo(iwallo)%tot = walloz
      
      !!loop through each demand object
      do idmd = 1, wallo(iwallo)%dmd_obs
               
        !! zero demand, withdrawal, and unmet for each source
        do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
          wallod_out(iwallo)%dmd(idmd)%src(isrc) = walloz
        end do
  
        !! set demand for each object
        !call wallo_demand (iwallo, idmd)
 
        !! if demand - check source availability and withdraw water
        if (wallod_out(iwallo)%dmd(idmd)%dmd_tot > 0.) then
            
          !! check if water is available from each source - set withdrawal and unmet
          wdraw_om_tot = hz
          do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
            dmd_m3 = wallod_out(iwallo)%dmd(idmd)%src(isrc)%demand
            if (dmd_m3 > 1.e-6) then
              call wallo_withdraw (iwallo, idmd, isrc)
            end if
          end do
        
          !! loop through sources again to check if compensation is allowed
          do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
            if (wallo(iwallo)%dmd(idmd)%src(isrc)%comp == "y") then
              dmd_m3 = wallo(iwallo)%dmd(idmd)%unmet_m3
              if (dmd_m3 > 1.e-6) then
                call wallo_withdraw (iwallo, idmd, isrc)
              end if
            end if
          end do
        
          !! compute total withdrawal for demand object from all sources
          wallo(iwallo)%dmd(idmd)%withdr_tot = 0.
          do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
            wallo(iwallo)%dmd(idmd)%withdr_tot = wallo(iwallo)%dmd(idmd)%withdr_tot +           &
                                                  wallod_out(iwallo)%dmd(idmd)%src(isrc)%withdr
          end do
        
          !! add water withdrawn from source to the demand object 
          select case (wallo(iwallo)%dmd(idmd)%ob_typ)
          !! irrigation transfer - set amount applied and runoff
          case ("hru")
            if (wallo(iwallo)%dmd(idmd)%withdr_tot > 0.) then
              j = wallo(iwallo)%dmd(idmd)%ob_num
              irr_mm = wallo(iwallo)%dmd(idmd)%withdr_tot / (hru(j)%area_ha * 10.)      !mm = m3 / (ha * 10.)
              irrig(j)%applied = irr_mm * wallo(iwallo)%dmd(idmd)%irr_eff * (1. - wallo(iwallo)%dmd(idmd)%surq)
              irrig(j)%runoff = wallo(iwallo)%dmd(idmd)%amount * wallo(iwallo)%dmd(idmd)%surq
              pcom(j)%days_irr = 1            ! reset days since last irrigation
              
              !! send runoff to canal?
              
              !rtb salt: irrigation salt mass accounting
              if(cs_db%num_salts > 0) then
                jj = idmd !to avoid a compiler warning
                call salt_irrig(iwallo,jj,j)
              endif
              !rtb cs: irrigation constituent mass accounting
              if(cs_db%num_cs > 0) then
                jj = idmd !to avoid a compiler warning
                call cs_irrig(iwallo,jj,j)
              endif
              
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, wallo(iwallo)%name, "IRRIGATE", phubase(j),  &
                  pcom(j)%plcur(1)%phuacc, soil(j)%sw, pl_mass(j)%tot(1)%m, soil1(j)%rsd(1)%m,           &
                  sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied
              end if
            end if
            
            case ("res")
              !! reservoir transfer - maintain reservoir levels at a specified level or required transfer
              res(j) = res(j) + wdraw_om
            
            case ("aqu")
              !! aquifer transfer - maintain aquifer levels at a specified level or required transfer
              aqu(j) = aqu(j) + wdraw_om
            
            case ("use")
              !! water use (domestic, industrial, commercial) 
              use_om_stor(j) = use_om_stor(j) + wdraw_om
              !! compute outflow and concentrations
              trt = wtp_om_treat(j)
              call wallo_treatment (iwallo, idmd)
              
              !! transfer outflow to receiving objects
              call wallo_transfer (iwallo, idmd)
            
            case ("wtp")
              !! wastewater treatment 
              wtp_om_stor(j) = wtp_om_stor(j) + wdraw_om_tot
              !! compute outflow and concentrations
              trt = use_om_efflu(j)
              call wallo_treatment (iwallo, idmd)
              
              !! transfer outflow to receiving objects
              call wallo_transfer (iwallo, idmd)
            
            case ("stor")
              !! water tower storage - don't change concentrations or compute outflow
              wtow_om_stor(j) = wtow_om_stor(j) + wdraw_om_tot
           
              !! transfer outflow to receiving objects
              call wallo_transfer (iwallo, idmd)
            
            case ("canal")
              !! canal storage - compute outflow - change concentrations?
              canal_om_stor(j) = canal_om_stor(j) + wdraw_om_tot
              !! compute losses evap and seepage
           
              !! transfer outflow to receiving objects
              call wallo_transfer (iwallo, idmd)
            
          end select
        
        end if      !if there is demand 
        
        !! sum organics 
        wdraw_om_tot = wdraw_om_tot + wdraw_om
        
        !! sum constituents
        
        !! transfer water to receiving objects
        call wallo_transfer (iwallo, idmd)
        
        !! sum demand, withdrawal, and unmet for entire allocation object
        wallo(iwallo)%tot%demand = wallo(iwallo)%tot%demand + wallod_out(iwallo)%dmd(idmd)%dmd_tot
        wallo(iwallo)%tot%withdr = wallo(iwallo)%tot%withdr + wallo(iwallo)%dmd(idmd)%withdr_tot
        wallo(iwallo)%tot%unmet = wallo(iwallo)%tot%unmet + wallo(iwallo)%dmd(idmd)%unmet_m3
        
      end do        !demand object loop
        
      return
      end subroutine wallo_control