  �/  O   k820309              2021.6.0    ��c                                                                                                          
       water_allocation_module.f90 WATER_ALLOCATION_MODULE                                                            #WALLOUT_ADD                                                               #WALLO_DIV_CONST                      @                               '                    #OB_TYP    #OBTYP_NUM    #FRAC                 �                                                                      �                                                              �                                              	                     @                               'H                    #NUM    #OB_TYP 	   #OBTYP_NUM 
   #DMD_TYP    #AMOUNT    #IRR_TYP    #SRC                 �                                                               �                              	                                       �                               
                               �                                                                     �                                    (          	                �                                    ,                          �                                           0                    #WATER_SOURCE_OBJECTS    p          p            p                                            @               @                '�                    #NAME    #RULE_TYP    #RES_LIM    #COMP    #DMD_OBS    #DMD                 �                                                                      �                                                                     �                                    4          	                �                                          8                           �                                    <                        �                                           @       H             #WATER_DEMAND_OBJECTS              &                                                                                                  �                        &                                           #WATER_ALLOCATION                      @                              '                    #DEMAND    #WITHDR    #UNMET                �                                              	                           {                     	                                 0.                �                                             	                           {                     	                                 0.                �                                             	                           {                     	                                 0.                                                            #SOURCE_OUTPUT                      @                               '                    #DMD_TOT    #SRC                �                                              	                           {                     	                                 0.                 �                                                               #SOURCE_OUTPUT    p          p            p                                                                                                         &                                           #WATER_ALLOCATION_OUTPUT                                                                                    &                                           #WATER_ALLOCATION_OUTPUT                                               !                                    &                                           #WATER_ALLOCATION_OUTPUT                                               "                                    &                                           #WATER_ALLOCATION_OUTPUT                      @                          #     '�                    #DAY $   #MO %   #DAY_MO &   #YRC '   #IDMD (   #DMD_TYP )   #DMD_NUM *   #SRC1_TYP +   #SRC1_NUM ,   #DMD1 -   #S1OUT .   #S1UN /   #SRC2_TYP 0   #SRC2_NUM 1   #DMD2 2   #S2OUT 3   #S2UN 4               �                              $                                                  {                                                       C  jday                                �                              %                                                 {                                                       C	 mon                                 �                              &                                                 {                                                       C day                                  �                              '                                                 {                                                       C yr                                   �                              (                                                 {                                	                       C unit                                   �                              )                                                  {                                	                       Cdmd_typ                                 �                              *            (                                     {                                	                       C dmd_num                                �                              +     
       0                                     {                                                       C src1_typ	                                �                              ,            :       	                              {                                	                       Csrc1_num                                �                              -            B       
                              {                                                       C	 demand	                                      �                              .            Q                                     {                                                       Csrc1_withdraw                                  �                              /            `                                     {                                                       C  src1_unmet                                �                              0     
       l                                     {                                                       C  src2_typ                                �                              1     
       v                                     {                                                       C src2_num                                 �                              2            �                                     {                                                       C demand                                        �                              3            �                                     {                                                       C src2_withdraw                                 �                              4            �                                     {                                                       C   src2_unmet                                                                  5     �       #WALLO_HEADER #                     @                          6     '�                    #DAY 7   #MO 8   #DAY_MO 9   #YRC :   #IDMD ;   #DMD_TYP <   #DMD_NUM =   #SRC1_TYP >   #SRC1_NUM ?   #DMD1 @   #S1OUT A   #S1UN B   #SRC2_TYP C   #SRC2_NUM D   #DMD2 E   #S2OUT F   #S2UN G               �                              7                                                  {                                	                       C	                                       �                              8                                                 {                                	                       C	                                       �                              9                                                 {                                	                       C	                                       �                              :                                                 {                                	                       C	                                       �                              ;                                                  {                                	                       C	                                       �                              <            (                                     {                                	                       C	                                       �                              =            0                                     {                                	                       C	                                       �                              >            8                                     {                                	                       C	                                       �                              ?            @       	                              {                                	                       C                                        �                              @            H       
                              {                                                       C ha_m                                       �                              A            T                                     {                                                       C   ha_m                                     �                              B            `                                     {                                                       C        ha_m                                �                              C            l                                     {                                	                       C                                        �                              D            t                                     {                                	                       C                                        �                              E            |                                     {                                                       C           ha_m                                �                              F            �                                     {                                                       C           ha_m                                �                              G            �                                     {                                                       C           ha_m                                                                H     �       #WALLO_HEADER_UNITS 6   &         @     X                                                       #WALLO1 I   #WALLO2 J   #SOURCE_OUTPUT              
                                  I                   #SOURCE_OUTPUT              
                                  J                   #SOURCE_OUTPUT    &         @     X                                                       #WALLO1 K   #CONST L   #SOURCE_OUTPUT              
                                  K                   #SOURCE_OUTPUT              
                                  L     	         �   <      fn#fn    �   Q      i@    -  U      i@ %   �  u       WATER_SOURCE_OBJECTS ,   �  P   a   WATER_SOURCE_OBJECTS%OB_TYP /   G  H   a   WATER_SOURCE_OBJECTS%OBTYP_NUM *   �  H   a   WATER_SOURCE_OBJECTS%FRAC %   �  �       WATER_DEMAND_OBJECTS )   z  H   a   WATER_DEMAND_OBJECTS%NUM ,   �  P   a   WATER_DEMAND_OBJECTS%OB_TYP /     H   a   WATER_DEMAND_OBJECTS%OBTYP_NUM -   Z  P   a   WATER_DEMAND_OBJECTS%DMD_TYP ,   �  H   a   WATER_DEMAND_OBJECTS%AMOUNT -   �  H   a   WATER_DEMAND_OBJECTS%IRR_TYP )   :  �   a   WATER_DEMAND_OBJECTS%SRC !   �  �       WATER_ALLOCATION &   �  P   a   WATER_ALLOCATION%NAME *   �  P   a   WATER_ALLOCATION%RULE_TYP )   %  H   a   WATER_ALLOCATION%RES_LIM &   m  P   a   WATER_ALLOCATION%COMP )   �  H   a   WATER_ALLOCATION%DMD_OBS %     �   a   WATER_ALLOCATION%DMD    �  �       WALLO    U	  s       SOURCE_OUTPUT %   �	  �   a   SOURCE_OUTPUT%DEMAND %   n
  �   a   SOURCE_OUTPUT%WITHDR $     �   a   SOURCE_OUTPUT%UNMET    �  S       WALLOZ (     f       WATER_ALLOCATION_OUTPUT 0   s  �   a   WATER_ALLOCATION_OUTPUT%DMD_TOT ,     �   a   WATER_ALLOCATION_OUTPUT%SRC    �  �       WALLOD_OUT    q  �       WALLOM_OUT      �       WALLOY_OUT    �  �       WALLOA_OUT    l        WALLO_HEADER !   |  �   a   WALLO_HEADER%DAY     ?  �   a   WALLO_HEADER%MO $     �   a   WALLO_HEADER%DAY_MO !   �  �   a   WALLO_HEADER%YRC "   �  �   a   WALLO_HEADER%IDMD %   M  �   a   WALLO_HEADER%DMD_TYP %     �   a   WALLO_HEADER%DMD_NUM &   �  �   a   WALLO_HEADER%SRC1_TYP &   �  �   a   WALLO_HEADER%SRC1_NUM "   c  �   a   WALLO_HEADER%DMD1 #   /  �   a   WALLO_HEADER%S1OUT "   �  �   a   WALLO_HEADER%S1UN &   �  �   a   WALLO_HEADER%SRC2_TYP &   �  �   a   WALLO_HEADER%SRC2_NUM "   R  �   a   WALLO_HEADER%DMD2 #     �   a   WALLO_HEADER%S2OUT "   �  �   a   WALLO_HEADER%S2UN    �  R       WALLO_HDR #           WALLO_HEADER_UNITS '      �   a   WALLO_HEADER_UNITS%DAY &   �   �   a   WALLO_HEADER_UNITS%MO *   �!  �   a   WALLO_HEADER_UNITS%DAY_MO '   g"  �   a   WALLO_HEADER_UNITS%YRC (   ,#  �   a   WALLO_HEADER_UNITS%IDMD +   �#  �   a   WALLO_HEADER_UNITS%DMD_TYP +   �$  �   a   WALLO_HEADER_UNITS%DMD_NUM ,   {%  �   a   WALLO_HEADER_UNITS%SRC1_TYP ,   @&  �   a   WALLO_HEADER_UNITS%SRC1_NUM (   '  �   a   WALLO_HEADER_UNITS%DMD1 )   �'  �   a   WALLO_HEADER_UNITS%S1OUT (   �(  �   a   WALLO_HEADER_UNITS%S1UN ,   `)  �   a   WALLO_HEADER_UNITS%SRC2_TYP ,   %*  �   a   WALLO_HEADER_UNITS%SRC2_NUM (   �*  �   a   WALLO_HEADER_UNITS%DMD2 )   �+  �   a   WALLO_HEADER_UNITS%S2OUT (   �,  �   a   WALLO_HEADER_UNITS%S2UN     N-  X       WALLO_HDR_UNITS    �-  {       WALLOUT_ADD #   !.  [   a   WALLOUT_ADD%WALLO1 #   |.  [   a   WALLOUT_ADD%WALLO2     �.  z       WALLO_DIV_CONST '   Q/  [   a   WALLO_DIV_CONST%WALLO1 &   �/  @   a   WALLO_DIV_CONST%CONST 