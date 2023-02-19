      subroutine ndvi_read_init

      use input_file_module
      use time_module
      
      implicit none      
 
      character (len=80) :: header    !           |header of file
      integer :: iyr                  !           |
      integer :: eof                  !           |end of file
      logical :: i_exist              !           |check to determine if file exists
      integer :: idhru                !           |
      integer :: util                 !           |
      integer :: jdeb                 !           | 
      
      eof = 0

      !read ndvi file
      inquire (file=in_hru%hru_ndvi, exist=i_exist)
      if (i_exist .or. in_hru%hru_ndvi /= "null") then
      do
      
        open (107,file=in_hru%hru_ndvi)
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        read (107,*,iostat=eof) idhru, time%start_yr_ndvi, util, jdeb   ! start_yr_ndvi trouvé
        if (eof < 0) exit

        do while (eof == 0)
          read (107,*,iostat=eof) idhru, iyr, util, jdeb
          if (eof < 0) exit
        end do
        time%end_yr_ndvi = iyr      ! end_yr_ndvi trouvé
        
        write(*,*) 'Lecture fichier NDVI : de ', time%start_yr_ndvi,'à',time%end_yr_ndvi !!EBtest
          
        close (107)
        exit

      end do  
      endif
      
      return
      end subroutine ndvi_read_init
