      subroutine wallo_pou_deliv (ipou)
      
      use water_allocation_module
      use hydrograph_module
      use hru_module
      use time_module
      use sd_channel_module
      use plant_module
      use plant_data_module
      use constituent_mass_module
      use organic_mineral_mass_module
      use soil_module
      
      implicit none 

      integer, intent (in) :: ipou          !point of use number
      integer :: ipodu = 0                  !point of use daily duty number
      integer :: ipor = 0                   !point of receiving object number
      integer :: iob = 0                    !object number of channel
      integer :: j = 0                      !hru number
      integer :: jj = 0                     !
      integer :: itrn = 0                   !
      integer :: ird = 0                    !irrigation distric number number of hrus
      integer :: irdb = 0                   !irrigation farm/district number
      real :: water_avail = 0.      !m3     |water still available for irrigating each hru
      real :: dmd_m3 = 0.           !m3     |irrigation demand for the hru
      real :: rto = 0.              !ratio  |ratio of delivered vs amount in delivery object
    
      !! j is the POU number to deliver - irr delivers to all hrus with a demand
      j = pou(ipou)%typ_num
          select case (pou(ipou)%typ)
          !! irrigation transfer - set amount applied and runoff
          case ("irr")
            water_avail = poud_om(ipou)%pods%flo
            !! irrigate hru if amount water is available
            do ird = 1, pou(ipou)%irr%hru_num
              if (pou(ipou)%irr%dmd > 0.) then
                dmd_m3 = pou(ipou)%irr%dmd * hru(j)%area_ha * 10.
                if (dmd_m3 < pou(ipou)%irr%dmd) then
                  !! irrig(j)%demand,irrig(j)%applied, and irrig(j)%runoff are set in "irr_demand" action
                  pcom(j)%days_irr = 1            ! reset days since last irrigation
               
                  !! add organics and minerals to the soil
                  rto = dmd_m3 / poud_om(ipou)%pods%flo
                  !no3 = rto * poud_om(ipou)%pods%no3
                  !!add no3, nh4, solp, orgn, and orgp to soil1(j)
                  
                  !! rtb salt: irrigation salt mass accounting
                  !if(cs_db%num_salts > 0) then
                  !  jj = itrn !to avoid a compiler warning
                  !  call salt_irrig(iwallo,jj,j)
                  !endif
                  
                  !!rtb cs: irrigation constituent mass accounting
                  !if(cs_db%num_cs > 0) then
                  !  jj = itrn !to avoid a compiler warning
                  !  call cs_irrig(iwallo,jj,j)
                  !endif
              
                  !! add irrigation to yearly sum for dtbl conditioning jga6-25
                  hru(j)%irr_yr = hru(j)%irr_yr + irrig(j)%applied
            
                  if (pco%mgtout == "y") then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo, "WATER ALLO", "IRRIGATE", phubase(j),  &
                      pcom(j)%plcur(1)%phuacc, soil(j)%sw, pl_mass(j)%tot(1)%m, pl_mass(j)%rsd_tot%m,      &
                      sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied
                  end if
                else
                  irrig(j)%demand = 0.
                  irrig(j)%applied = 0.
                  irrig(j)%runoff = 0.
                end if
              end if
            end do
            
            !! divert flow into the channel in sd_channel_control3
            case ("cha")
              iob = sd_ch(j)%obj_no
              ob(iob)%trans = ob(iob)%trans + poud_om(ipou)%pods
            
            case ("res")
              !! reservoir transfer - maintain reservoir levels at a specified level or required transfer
                res(j) = res(j) + poud_om(ipou)%pods
            
            case ("aqu")
              !! aquifer transfer - maintain aquifer levels at a specified level or required transfer
              aqu(j) = aqu(j) + poud_om(ipou)%pods
              !! calculate water table depth
              
            case ("wtp")
              !! wastewater treatment 
              wtp_om_stor(j) = wtp_om_stor(j) + poud_om(ipou)%pods
              !! compute outflow and concentrations
              call wallo_treatment (ipou)
              
            case ("use")
              !! water use (domestic, industrial, commercial) 
              wuse_om_stor(j) = wuse_om_stor(j) + poud_om(ipou)%pods
              !! compute outflow and concentrations
              call wallo_use (ipou)
              
            case ("stor")
              !! water tower storage - don't change concentrations or compute outflow
              wtow_om_stor(j) = wtow_om_stor(j) + poud_om(ipou)%pods
           
            case ("can")
              !! canal storage - compute outflow - change concentrations?
              canal_om_stor(j) = canal_om_stor(j) + poud_om(ipou)%pods
              !! compute losses - evap and seepage, and outflow
              call wallo_canal (j)
              
            case ("orcv")
              !! outside receiving object
              orcv_om(j) = orcv_om(j) + poud_om(ipou)%pods
           
            end select
            
      return
      end subroutine wallo_pou_deliv