      subroutine wallo_control (ipod)
      
      use water_allocation_module
      
      implicit none 

      integer, intent (inout) :: ipod       !water allocation object number
      integer :: ipou = 0                            !point of use number
      integer :: ipodu = 0                           !point of use daily duty number
      integer :: ipor = 0                            !point of receiving object number
      
      !! withdrawfrom POD for each POU
      do ipou = 1, pod(ipod)%pous
        !! om withdrawal - pou_om(ipou)%pod(ipod)     
        call wallo_withdraw1 (ipod, ipou)
        pou(ipou)%pod(ipod)%fin = "y"
      end do
        
        !! check if all PODs are finished for the POU
        pou(ipou)%fin = "y"
        do ipod_u = 1, pou(ipou)%num_pod
          if (pou(ipou)%pod(ipod_u)%fin == "n") then
            pou(ipou)%fin = "n"
            !! check if compensation is needed for unmet duty
            exit
          end if
        end do
      
        !! deliver to PORs
        if (pou(ipou)%fin == "y") then
        !! if use or treatment compute outflow and concentrations
        select case (pou(ipou)%typ)
        case ("wtp")
          !! wastewater treatment 
          wtp_om_stor(j) = wtp_om_stor(j) + poud_om(ipou)%pors
          !! compute outflow and concentrations
          call wallo_treatment (iwallo, itrn, j)
          poud_om(ipou)%pors = wtp_om_out(j)
              
        case ("use")
          !! water use (domestic, industrial, commercial) 
          wuse_om_stor(j) = wuse_om_stor(j) + poud_om(ipou)%pors
          !! compute outflow and concentrations
          call wallo_use (iwallo, itrn, j)
          poud_om(ipou)%pors = wuse_om_out(j)
        end select
            
        !! transfer to receiving object and update water and constituent mass in receiving object
        call wallo_return (ipod, ipou)
        end if
      end do
      
    return
    end subroutine wallo_control
      
    
      subroutine wallo_return (ipod, ipou)
      
      use water_allocation_module
      
      implicit none 

      integer, intent (inout) :: ipod       !water allocation object number
      integer :: ipou = 0                            !point of use number
      integer :: ipodu = 0                           !point of use daily duty number
      integer :: ipor = 0                            !point of receiving object number
    
      integer, intent (in) :: ipod, ipou
      
      do ipor = 1, pou(ipou)%pors
      
          !! transfer water (pipes) to receiving object from all sources
          !call wallo_transfer (ipou)
        
          !! add water withdrawn from source to the receiving object - wal_omd(iwallo)%trn(itrn)%h_tot
          j = pou(ipou)%por(ipor)%num
          poud_om(ipou)%por(ipor) = poud_om(ipou)%pors * pou(ipou)%por(ipor)%frac
          
          select case (pou(ipou)%por(ipor)%typ_num)
          !! irrigation transfer - set amount applied and runoff
          case ("hru")
            !! irrigate if amount withdrawn is > 0 --> or > irrig(j)%demand
            if (pou(ipou)%por(ipor)%withdr_tot > 0.) then
              irr_mm = pou(ipou)%por(ipor)%flo / (hru(j)%area_ha * 10.)      !mm = m3 / (ha * 10.)
              irrig(j)%applied = irr_mm * pou(ipou)%por(ipor)%irr_eff * (1. - pou(ipou)%por(ipor)%surq)
              irrig(j)%runoff = pou(ipou)%por(ipor)%amount * pou(ipou)%por(ipor)%surq
              !! add irrigation water n and p to the soil when routing the soluble n and p
              irrig(j)%water = wal_omd(iwallo)%trn(itrn)%h_tot
              pcom(j)%days_irr = 1            ! reset days since last irrigation
              
              !! recode after running
              !rtb salt: irrigation salt mass accounting
              !if(cs_db%num_salts > 0) then
              !  jj = itrn !to avoid a compiler warning
              !  call salt_irrig(iwallo,jj,j)
              !endif
              !rtb cs: irrigation constituent mass accounting
              !if(cs_db%num_cs > 0) then
              !  jj = itrn !to avoid a compiler warning
              !  call cs_irrig(iwallo,jj,j)
              !endif
              
              ! add irrigation to yearly sum for dtbl conditioning jga6-25
              hru(j)%irr_yr = hru(j)%irr_yr + irrig(j)%applied
            
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, wallo(iwallo)%name, "IRRIGATE", phubase(j),  &
                  pcom(j)%plcur(1)%phuacc, soil(j)%sw, pl_mass(j)%tot(1)%m, pl_mass(j)%rsd_tot%m,      &
                  sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied
              end if
            end if
            
            !! divert flow into the channel in sd_channel_control3
            case ("cha")
              iob = sd_ch(j)%obj_no
              ob(iob)%trans = poud_om(ipou)%por(ipor)
            
            case ("res")
              !! reservoir transfer - maintain reservoir levels at a specified level or required transfer
                res(j) = res(j) + poud_om(ipou)%por(ipor)
            
            case ("aqu")
              !! aquifer transfer - maintain aquifer levels at a specified level or required transfer
              aqu(j) = aqu(j) + poud_om(ipou)%por(ipor)
              !! calculate water table depth
              
            case ("wtp")
              !! wastewater treatment 
              wtp_om_stor(j) = wtp_om_stor(j) + poud_om(ipou)%por(ipor)
              !! compute outflow and concentrations
              call wallo_treatment (iwallo, itrn, j)
              
            case ("use")
              !! water use (domestic, industrial, commercial) 
              wuse_om_stor(j) = wuse_om_stor(j) + poud_om(ipou)%por(ipor)
              !! compute outflow and concentrations
              call wallo_use (iwallo, itrn, j)
              
            case ("stor")
              !! water tower storage - don't change concentrations or compute outflow
              wtow_om_stor(j) = wtow_om_stor(j) + poud_om(ipou)%por(ipor)
           
            case ("can")
              !! canal storage - compute outflow - change concentrations?
              canal_om_stor(j) = canal_om_stor(j) + poud_om(ipou)%por(ipor)
              !! compute losses - evap and seepage, and outflow
              call wallo_canal (iwallo, itrn, j)
              
            case ("orcv")
              !! outside receiving object
              orcv_om(j) = orcv_om(j) + poud_om(ipou)%por(ipor)
           
          end select
        
      return
      end subroutine wallo_return