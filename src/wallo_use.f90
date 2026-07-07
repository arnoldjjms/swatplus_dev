      subroutine wallo_use (ipou)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 

      integer, intent (in):: ipou       !water allocation object number
      integer :: iuse                   !water use number
      integer :: iom                    !number of organic-mineral concentrations of water use
      
      !! domestic, industrial, commercial use concentrations
      iuse = pou(ipou)%typ_num
      iom = wuse(iuse)%iorg_min
      outflo_om = wuse_om_efflu(iom)
      
      !! treated outflow is currently set to inflow - no storage
      outflo_om%flo = outflo_om%flo * poud_om(ipou)%pors%flo
      wuse_om_stor(iuse) = hz
      
      !! convert concentration to mass
      call hyd_convert_conc_to_mass (outflo_om)
      wuse_om_out(iuse) = outflo_om
      
      !! amount that is added
      wal_use_omd(iuse) = wuse_om_out(iuse) - poud_om(ipou)%pors
      
      !! constituents effluent - convert concentration to mass
      if (cs_db%num_tot > 0) then
        call hydcsout_conc_mass (outflo_om%flo, wuse_cs_efflu(iuse), outflo_cs)
      end if
      
      return
    end subroutine wallo_use