      subroutine ndvi_jour
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    !!EBndvi this subroutine calculates the NDVI for the HRU for the current day
!!    Interpolation du NDVI qui est lu tous les 10 jours
!!    'evol' est aussi mis a jour

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru       |none           |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Sqrt, Max, Min
!!    SWAT: Ee

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, ihru
      use time_module
      
      implicit none
      
      integer :: j                 !none          |HRU number 
      integer :: jday              !none          |jour courant (jour julien)
      integer :: jdeb              !none          |premier jour avec valeur de NDVI
      integer :: jfin              !none          |dernier jour avec valeur de NDVI   
      integer :: i                 !none          | 
      integer :: di                !none          | 
      real :: val_ndvi             !none          | 
      real :: ndvi_av              !none          | 
      real :: ndvi_ap              !none          | 

      !! initialize local variables
      j = ihru
      jday = time%day
      jdeb = hru(j)%ndvi%jdeb
      jfin = hru(j)%ndvi%jfin
      
      
      
      i = (jday-jdeb)/10 + 1
      di = mod(jday-jdeb,10)
      
      if (jday < jdeb) then
        val_ndvi = hru(j)%ndvi%val(1)    !! avant la premiere date
      else
        if (jday > jfin) then
          val_ndvi = hru(j)%ndvi%val((jfin-jdeb)/10 + 1)    !! apres la derniere date
        else
      
          if (di == 0) then
            val_ndvi = hru(j)%ndvi%val(i)     !! une date ou le NDVI est connu
          else
            val_ndvi = ( (10-di)*hru(j)%ndvi%val(i) + di*hru(j)%ndvi%val(i+1) )/10.  !!interpolation entre 2 dates
          end if
          
        end if
      end if
      
      ndvi_av = hru(j)%ndvi%ndvi_j  !!ndvi du jour précédent
      ndvi_ap = hru(j)%ndvi%val( min(i+1,(jfin-jdeb)/10 + 1) )
      
      hru(j)%ndvi%ndvi_j = val_ndvi
      
      if (ndvi_av.le.val_ndvi) then
        if (ndvi_ap.ge.val_ndvi) then
          hru(j)%ndvi%evol = 1
        else
          hru(j)%ndvi%evol = 0
        end if
      else
        if (ndvi_ap.le.val_ndvi) then
          hru(j)%ndvi%evol = 2
        else
          hru(j)%ndvi%evol = 0
        end if
      end if

      return
      end subroutine ndvi_jour
