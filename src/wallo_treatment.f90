      subroutine wallo_treatment (iwallo, itrt)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 

      integer, intent (in):: iwallo     !water allocation object number
      integer, intent (in) :: itrt      !water treatment plant object number
      integer :: iom                    !number of organic-mineral data concentrations of treated water 
      
      !! treating water to wtp or use concentrations
      !! treated outflow is a fraction of withdrawal
      outflo_om = wtp_om_treat(itrt)
      outflo_om%flo = outflo_om%flo * wdraw_om_tot%flo
      
      !! convert concentration to mass
      call hyd_convert_conc_to_mass (outflo_om)
      wtp_om_out(itrt) = outflo_om
      
      !! treat constituents
      !! convert concentration to mass
      call hydcsout_conc_mass (outflo_om%flo, wtp_cs_treat(itrt), outflo_cs)
      
      return
    end subroutine wallo_treatment
    
    
    subroutine wallo_use (iwallo, iuse)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 

      integer, intent (in):: iwallo         !water allocation object number
      integer, intent (in) :: iuse          !water use number
      integer :: iom                        !number of organic-mineral concentrations of water use
      
      !! treating water to wtp or use concentrations
      !! treated outflow is a fraction of withdrawal
      iom = wuse(iuse)%iorg_min
      outflo_om = wuse_om_efflu(iom)
      outflo_om%flo = outflo_om%flo * wdraw_om_tot%flo
      
      !! convert concentration to mass
      call hyd_convert_conc_to_mass (outflo_om)
      wuse_om_out(iuse) = outflo_om
      
      !! constituents effluent
      !! convert concentration to mass
      call hydcsout_conc_mass (outflo_om%flo, wuse_cs_efflu(iuse), outflo_cs)
      
      return
    end subroutine wallo_use