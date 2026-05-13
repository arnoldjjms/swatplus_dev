      subroutine wallo_treatment (ipou)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 

      integer, intent (in):: ipou       !water allocation object number
      integer :: itrt      !water treatment number
      integer :: iom                    !number of organic-mineral concentrations of water use
      
      !! treating water to wtp concentrations
      itrt = pou(ipou)%typ_num
      iom = wtp(itrt)%iorg_min
      outflo_om = wtp_om_treat(iom)
      
      !! treated outflow is a fraction of withdrawal
      outflo_om%flo = outflo_om%flo * poud_om(ipou)%pors%flo
      
      !! convert concentration to mass
      call hyd_convert_conc_to_mass (outflo_om)
      
      !! treated mass can't be higher than inflow mass
      call hyd_min (outflo_om, poud_om(ipou)%pors)
      
      wtp_om_out(itrt) = wtp_om_out(itrt) + outflo_om
      
      !! amount that is removed
      wal_tr_omd(itrt) = poud_om(ipou)%pors - wtp_om_out(itrt)
      
      !! treat constituents - convert concentration to mass
      if (cs_db%num_tot > 0) then
        call hydcsout_conc_mass (outflo_om%flo, wtp_cs_treat(itrt), outflo_cs)
      end if
      
      return
    end subroutine wallo_treatment