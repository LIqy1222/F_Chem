program read_wrfout
  use netcdf
  implicit none

  integer :: status, fidA

  !-维度ID
  integer :: dimID_TIME, dimID_Z, dimID_LAT, dimID_LON
  
  !-变量ID
  integer :: LAT_ID, LON_ID
  !-所需气象变量的ID
  !- geopotential位势高度的ID
  integer :: PH_ID, PHB_ID
  !- base和扰动pressure的ID
  integer :: PB_ID, P_ID

  !-网格和时间
  integer :: nx,ny,nz,nt

  !-实际变量
  real(8), dimension(:,:),   allocatable :: lat,lon
  real(8), dimension(:,:,:), allocatable :: PH, PHB, P, PB
  real(8), dimension(:,:,:), allocatable :: Z_W

  integer :: i,j,k
  !-连这些循环体涉及的小字母你都需要定义哈
  character(len=255) :: file_in

  !-以下是latlon-xy坐标的相关变量定义
  real(8), parameter :: R = 6371.0 * 1000.0  ! earth radius (m)
  real(8) :: left_bottom_lon, left_bottom_lat

  !Source grid & var & goal coordinates & point numbers define
  real(8), dimension(:,:), allocatable   :: xArray, yArray
  real(8), dimension(:,:,:), allocatable :: zArray
  real(8), dimension(:,:,:), allocatable :: pressure
  real(8), dimension(:), allocatable     :: xTarget, yTarget, zTarget
  integer,parameter                      :: n_points=279
  !插值后的变量定义
  real(8)                                :: pressure_ITP
  real(8),dimension(n_points)            :: val
 
  ! define read txt file
  character(len=8000)          ::  line
  ! define nearest points' index
  integer                     ::  ix0, ix1
  integer                     ::  iy0, iy1
  integer, dimension(4, 2)    ::  izArray   ! to save four points' iz0&iz1
  
  ! define time 
  real(8) :: start_time, end_time, elapsed_time

  !----------------下面将读取nc文件内需要的变量:lat,lon,pressure,height------------------
  ! Define the path to the NetCDF file
  file_in = './wrfout_d02_2016-12-11_00_00_00'

  ! Open the NetCDF file
  status = nf90_open(trim(file_in), 0, fidA)
  call erreur(status, .TRUE., "read_A")
  
  !- read ID of dimensions of interest and save them in -
  ! 获取维度变量ID  
  status = NF90_INQ_DIMID(fidA,"Time",dimID_TIME)
  call erreur(status,.TRUE.,"inq_dimID_TIME")
  print*,'dimID_TIME= ',dimID_TIME

  status = NF90_INQ_DIMID(fidA,"bottom_top",dimID_Z)
  call erreur(status,.TRUE.,"inq_dimID_Z")
  print*,'dimID_Z= ',dimID_Z  

  status = NF90_INQ_DIMID(fidA,"south_north",dimID_LAT)
  call erreur(status,.TRUE.,"inq_dimID_LAT")
  print*,'dimID_LAT= ',dimID_LAT

  status = NF90_INQ_DIMID(fidA,"west_east",dimID_LON)
  call erreur(status,.TRUE.,"inq_dimID_LON")
  print*,'dimID_LON= ',dimID_LON

  ! 获取维度的值:
  status = NF90_INQUIRE_DIMENSION(fidA,dimID_TIME,len=nt)
  call erreur(status,.TRUE.,"inq_dim_TIME")

  status = NF90_INQUIRE_DIMENSION(fidA,dimID_LAT,len=ny)
  call erreur(status,.TRUE.,"inq_dim_LAT")

  status = NF90_INQUIRE_DIMENSION(fidA,dimID_LON,len=nx)
  call erreur(status,.TRUE.,"inq_dim_LON")

  status = NF90_INQUIRE_DIMENSION(fidA,dimID_Z,len=nz)
  call erreur(status,.TRUE.,"inq_dim_Z")

  print*,'nx,ny,nz,nt', nx,ny,nz,nt

  !- Allocation of arrays :
  ! 需要参考wrfout最开头的dimensions来进行变量内存分配，不然会报错 Start+count exceeds dimension bound
  allocate(  LAT(nx, ny)  )
  allocate(  LON(nx, ny)  )
  !-位势高度
  allocate(  PH(nx, ny, nz+1))
  allocate(  PHB(nx, ny, nz+1))

  !-需要加和/插值计算得到的延伸变量
  allocate( Z_W(nx, ny, nz+1))
  allocate( zArray(nx, ny, nz))
  !-气压
  allocate(  P(nx, ny, nz))
  allocate(  PB(nx, ny, nz))
  !-需要加和得到的延伸变量
  allocate(  pressure(nx, ny, nz))

  !- 获取变量ID :
  status = NF90_INQ_VARID(fidA,"XLAT",LAT_ID)
  call erreur(status,.TRUE.,"inq_LAT_ID")
  !print*,'LAT_ID',LAT_ID latitude is 30th variable in the list

  status = NF90_INQ_VARID(fidA,"XLONG",LON_ID)
  call erreur(status,.TRUE.,"inq_LON_ID")

  !- 获取气象变量的ID
  !- 位势高度
  status = NF90_INQ_VARID(fidA,"PH",PH_ID)
  call erreur(status,.TRUE.,"inq_PH_ID") 
  status = NF90_INQ_VARID(fidA,"PHB",PHB_ID)
  call erreur(status,.TRUE.,"inq_PHB_ID") 
  !- 气压
  status = NF90_INQ_VARID(fidA,"PB",PB_ID)
  call erreur(status,.TRUE.,"inq_PB_ID") 
  status = NF90_INQ_VARID(fidA,"P",P_ID)
  call erreur(status,.TRUE.,"inq_P_ID") 
  !print*,'P_ID= ',P_ID

  !- 获取变量值 :
  status = NF90_GET_VAR(fidA,LAT_ID,LAT)
  call erreur(status,.TRUE.,"getvar_LAT")

  status = NF90_GET_VAR(fidA,LON_ID,LON)
  call erreur(status,.TRUE.,"getvar_LON")
  !- 位势高度
  status = NF90_GET_VAR(fidA,PH_ID,PH)
  call erreur(status,.TRUE.,"getvar_PH")
  status = NF90_GET_VAR(fidA,PHB_ID,PHB)
  call erreur(status,.TRUE.,"getvar_PHB")
  !- 气压
  status = NF90_GET_VAR(fidA,P_ID,P)
  call erreur(status,.TRUE.,"getvar_P")
  status = NF90_GET_VAR(fidA,PB_ID,PB)
  call erreur(status,.TRUE.,"getvar_PB")

  !- close netcdf file :
  status = NF90_CLOSE(fidA)
  call erreur(status,.TRUE.,"close_A")

  print*,'lat shape 198 300 ; ',       size(lat, 1), size(lat, 2)
  print*,'lon shape 198 300 ; ',       size(lon, 1), size(lon, 2)
  print*,'P   shape 198 300 30 ; ',    size(P, 1),   size(P, 2),  size(P, 3)
  print*,'PB  shape 198 300 30 ; ',    size(PB, 1),  size(PB, 2), size(PB, 3)
  print*,'PH  shape 198 300 31; ',     size(PH, 1),  size(PH, 2), size(PH, 3)
  print*,'PHB shape 198 300 31 ; ',    size(PHB, 1), size(PHB, 2),size(PHB, 3)

  !计算在W-points上的elevation
  Z_W = (PH + PHB) / 9.81
  !线性插值到theta-point高度层
  do i = 1, size(Z_W,1)
    do j =1, size(Z_W,2)
      do k =1, size(Z_W,3)-1
        zArray(i,j,k) = 0.5 *(Z_W(i,j,k)+Z_W(i,j,k+1))
      end do
    end do
  end do
  !计算在theta-points高度层上的气压
  pressure =P+PB

  !----------------下面将经纬度坐标转换成笛卡尔坐标系下的xyz坐标------------------
  !- 模拟区域左下角经纬度
  left_bottom_lon = 109.0
  left_bottom_lat = 34.0

  !- 分配x,y坐标的内存
  allocate(xArray(nx, ny))
  allocate(yArray(nx, ny))
  ! 计算经纬度到x, y坐标的转换
  do i = 1, nx
    do j =1, ny
      xArray(i,j) = (2.0d0 * 3.141592653589793d0 * R * cos(left_bottom_lat * 3.141592653589793d0 / 180.0d0)) / 360.0d0 &
            * (LON(i,j) - left_bottom_lon)
      yArray(i,j) = (2.0d0 * 3.141592653589793d0 * R) / 360.0d0 * (LAT(i,j) - left_bottom_lat)
    end do
  end do

  ! 输出结果
  print*, 'Converted x and y coordinates:'
  do i = 1, 2
    do j =1, 2
      print*, 'x = ', xArray(i,j), ' y = ', yArray(i,j)
    end do
  end do

  ! 为目标网格分配空间
  allocate(xTarget(n_points))
  allocate(yTarget(n_points))
  allocate(zTarget(n_points))

  !---------------读取目标网格（应该都是一维的）测试
  ! 打开文件并读取数据
  open(unit=10, file='x_del.txt', status='old')  ! 打开文件
  read(10,'(A)') line
  do i =1, n_points
    read(line(1+26*(i-1):26*i),*) xTarget(i)
  end do
  close(10)  ! 关闭文件

  open(unit=11, file='y_del.txt', status='old')  ! 打开文件
  read(11,'(A)') line
  do i =1, n_points
    read(line(1+26*(i-1):26*i),*) yTarget(i)
  end do
  close(11)  ! 关闭文件

  open(unit=12, file='z_del.txt', status='old')  ! 打开文件
  read(12,'(A)') line
  do i =1, n_points
    read(line(1+26*(i-1):26*i),*) zTarget(i)
  end do
  close(12)  ! 关闭文件

  !源网格的构建需要x(198,300),y(198,300),Z_theta(198,300,30),pressure(198,300,30)
  ! Call the interpolation subroutine
  ! 记录插值计算开始的时间
  call cpu_time(start_time)
  do i = 1, n_points 
    call find_near_index(xArray, yArray, zArray, &
                       xTarget(i), yTarget(i), zTarget(i), ix0, ix1,&
                       iy0, iy1, izArray)
    call TriLinear_ITP(pressure, ix0, ix1, iy0, iy1, &
                         xArray, yArray, zArray,&
                         xTarget(i), yTarget(i), zTarget(i), izArray, pressure_ITP)
    val(i) = pressure_ITP
  end do
  ! 记录插值计算结束的时间
  call cpu_time(end_time)  
  ! 计算总的时间
  elapsed_time = end_time - start_time
  print *, "Total duration for interpolating: ", elapsed_time, " s"

  ! 打开新的文件以写入结果
  open(unit=123, file='ITP_result.txt', status='replace')
  ! 写入文件标题
  write(123, *) 'x', 'y', 'z', 'val'
  ! 写入 x, y, z, val 数组的数据
  do i = 1, n_points
    write(123, '(F18.8, 8x, F18.8, 8X, F18.8, 8x, F18.8)') xTarget(i), yTarget(i), zTarget(i), val(i)
  end do
  ! 关闭文件
  close(123)
  print *, "Results had been saved to ITP_result.txt"

end program read_wrfout

subroutine erreur(iret, lstop, chaine)
  ! used to provide clear error messages :
  use netcdf
  INTEGER, INTENT(in)                     :: iret
  LOGICAL, INTENT(in)                     :: lstop
  CHARACTER(LEN=*), INTENT(in)            :: chaine
  !
  CHARACTER(LEN=80)                       :: message
  !
  IF ( iret .NE. 0 ) THEN
    WRITE(*,*) 'ROUTINE: ', TRIM(chaine)
    WRITE(*,*) 'ERROR: ', iret
    message=NF90_STRERROR(iret)
    WRITE(*,*) 'WHICH MEANS:',TRIM(message)
    IF ( lstop ) STOP
  ENDIF
  !
end subroutine erreur

subroutine find_near_index(xArray, yArray, zArray, &
                              xT, yT, zT, ix0, ix1, iy0, iy1, izArray)
  implicit none

  ! define in & out variable 
  real(8), dimension(198,300),intent(in)      ::  xArray, yArray
  real(8), dimension(198,300,30),intent(in)   ::  zArray
  real(8), intent(in)                         ::  xT, yT, zT
  integer, intent(out)                     ::  ix0, ix1
  integer, intent(out)                     ::  iy0, iy1
  integer, dimension(4, 2), intent(out)    ::  izArray   ! to save four points' iz0&iz1

  ! normal define
  integer                                  ::  i, j, k
  real(8)                                     ::  minDist, Dist  ! distance judge variable
  integer                                  ::  ix, iy, iz
  integer                                  ::  iz0, iz1
  ! initialise minDist
  minDist = 1.0e10
  print*,'zT= ',zT
  ! loop all points from wrfout, to calculate the distance between target and source grid
  do i =1,198
    do j =1,300
      dist = sqrt((xArray(i,j) - xT)**2 + (yArray(i,j) - yT)**2)
      if (dist < minDist) then
        minDist = dist
        ix = i
        iy = j
      end if
    end do
  end do
  ! found the nearst point in wrfout 
  print *, "found the nearst point in wrfout is at (", ix, ",", iy, ") with a distance of ", minDist

  ! Ensure nearby 4 points' x&y index in wrfout grid
  ! for x
  if (xT <= xArray(ix, iy)) then
    ix0 = ix -1
    ix1 = ix
  else
    ix0 = ix
    ix1 = ix + 1
  end if
  ! for y
  if (yT <= yArray(ix, iy)) then
    iy0 = iy -1
    iy1 = iy
  else
    iy0 = iy
    iy1 = iy + 1
  end if
  ! check final four indexes
  print *, "For x direction: ix0 = ", ix0, ", ix1 = ", ix1
  print *, "For y direction: iy0 = ", iy0, ", iy1 = ", iy1

  ! base on four points's x&y, find z coordinates
  do i = 1,4
    if (i == 1) then
      ix = ix0
      iy = iy0
    elseif (i == 2) then
      ix = ix0
      iy = iy1
    elseif (i == 3) then
      ix = ix1
      iy = iy0
    else
      ix = ix1
      iy = iy1
    end if

    ! initialise minDist
    minDist = 1.0e10
    do iz = 1,size(zArray,3)
      dist = abs(zArray(ix, iy, iz) - zT)
      !print*,'dist= ',dist
      if (dist < minDist) then
        minDist = dist
        !print*,zArray(ix, iy, iz)
        iz0 = iz
      end if
    end do  

    ! calculate iz0 & iz1
    if (zT <= zArray(ix, iy, iz0)) then
        iz1 = iz0
        iz0 = iz0 - 1
    else
        iz1 = iz0 + 1
    end if   

    izArray(i,1) = iz0
    izArray(i,2) = iz1
    !print*, "z0= ",zArray(ix,iy,iz0)
    !print*, "z1= ",zArray(ix,iy,iz1)
    !print *, "x= ", ix, "y= ", iy, "iz0 = ", izArray(i, 1), ", iz1 = ", izArray(i, 2)
  end do
  
  !print*,'(0,0,0)= ',xArray(ix0,iy0),'_', yArray(ix0,iy0),'_',zArray(ix0,iy0,izArray(1,1))
  !print*,'(1,0,0)= ',xArray(ix1,iy0),'_', yArray(ix1,iy0),'_',zArray(ix1,iy0,izArray(3,1))
  !print*,'(1,1,0)= ',xArray(ix1,iy1),'_', yArray(ix1,iy1),'_',zArray(ix1,iy1,izArray(4,1))
  !print*,'(0,1,0)= ',xArray(ix0,iy1),'_', yArray(ix0,iy1),'_',zArray(ix0,iy1,izArray(2,1))

  !print*,'(0,0,1)= ',xArray(ix0,iy0),'_', yArray(ix0,iy0),'_',zArray(ix0,iy0,izArray(1,2))
  !print*,'(1,0,1)= ',xArray(ix1,iy0),'_', yArray(ix1,iy0),'_',zArray(ix1,iy0,izArray(3,2))
  !print*,'(1,1,1)= ',xArray(ix1,iy1),'_', yArray(ix1,iy1),'_',zArray(ix1,iy1,izArray(4,2))
  !print*,'(0,1,1)= ',xArray(ix0,iy1),'_', yArray(ix0,iy1),'_',zArray(ix0,iy1,izArray(2,2))

end subroutine find_near_index

subroutine TriLinear_ITP(pressure, ix0, ix1, iy0, iy1, &
                         xArray, yArray, zArray,&
                         xT, yT, zT, izArray, pressure_ITP)
  implicit none

  ! define var in source grid
  real(8), dimension(198,300,30), intent(in)    ::  pressure
  ! define in & out parameters
  real(8), dimension(198,300),intent(in)        ::  xArray, yArray
  real(8), dimension(198,300,30),intent(in)     ::  zArray
  integer, intent(in)                           ::  ix0, ix1
  integer, intent(in)                           ::  iy0, iy1  
  real(8), intent(in)                           ::  xT, yT, zT 
  integer, dimension(4,2), intent(in)           ::  izArray
  real(8), intent(out)                          ::  pressure_ITP

  ! define 8 points' val values
  real(8) :: P000, P100, P010, P110, P001, P101, P011, P111

  ! define matrix
  real(8) :: A(4,4), AI(4,4) !I-INVERSE
  ! define four points
  real(8) :: px(4),  py(4)   ! four points' x & y we need
  ! define coefficient a & b
  real(8) :: alpha(4), beta(4)
  ! define solve need
  !integer :: ipiv(4), info
  ! define l & m coordinates
  real(8) :: l, m
  real(8) :: dl, dm
  
  ! daily define
  integer :: i, j

  ! Z define
  real(8) :: weight_z
  real(8) :: P1, P2 
  real(8) :: z_bottom, z_top

  px = [xArray(ix0,iy0), xArray(ix1,iy0), xArray(ix1,iy1), xArray(ix0,iy1)]
  py = [yArray(ix0,iy0), yArray(ix1,iy0), yArray(ix1,iy1), yArray(ix0,iy1)]
  print*,px
  print*,py

  ! Define the matrix A
  A = reshape([ &
      1.0, 0.0, 0.0, 0.0, &
      1.0, 1.0, 0.0, 0.0, &
      1.0, 1.0, 1.0, 1.0, &
      1.0, 0.0, 1.0, 0.0], &
      [4,4])
      
    ! Compute the inverse of matrix A 
    call matinv(A, AI)

    ! Compute the coefficients a and b by multiplying AI with px and py
    call matvecmul(AI, px, alpha)
    call matvecmul(AI, py, beta)
  ! Output the results
  print *, 'Coefficients a:', alpha
  print *, 'Coefficients b:', beta

  ! Call the subroutine to compute logical coordinates
  call XtoL(xT, yT, alpha, beta, l, m)
  ! Print the results
  print *, 'Logical coordinates: l = ', l, ', m = ', m

  ! Check if (l, m) is within the unit square [0, 1] x [0, 1]
  !if (l < 0.0d0 .or. l > 1.0d0 .or. m < 0.0d0 .or. m > 1.0d0) then
  !  print *, 'Error: (l, m) out of bounds!'
  !  stop
  !end if
  dl = l
  dm = m

  ! Perform bilinear interpolation
  !- judge z is in source grid or out first
  if (izArray(1, 1) == 0 .or. izArray(1, 1) == 30 .or. &
      izArray(2, 1) == 0 .or. izArray(2, 1) == 30 .or. &
      izArray(3, 1) == 0 .or. izArray(3, 1) == 30 .or. &
      izArray(4, 1) == 0 .or. izArray(4, 1) == 30) then
    print*,'izArray(1, 1) = ', izArray(1,1)
    print*,'izArray(2, 1) = ', izArray(2,1)
    print*,'izArray(3, 1) = ', izArray(3,1)
    print*,'izArray(4, 1) = ', izArray(4,1)
    print*,'out of grid!'

    ! set value to 4 points
    if ( izArray(1, 1) == 0 .or. izArray(2, 1) == 0 .or. &
         izArray(3, 1) == 0 .or. izArray(4, 1) == 0 ) then
      P001 = pressure(ix0, iy0, izArray(1, 2))
      P011 = pressure(ix0, iy1, izArray(2, 2))
      P101 = pressure(ix1, iy0, izArray(3, 2))
      P111 = pressure(ix1, iy1, izArray(4, 2))
    else
      P001 = pressure(ix0, iy0, izArray(1, 1))
      P011 = pressure(ix0, iy1, izArray(2, 1))
      P101 = pressure(ix1, iy0, izArray(3, 1))
      P111 = pressure(ix1, iy1, izArray(4, 1))  
      ! here is not fllow real condition, 
      ! in fact they are P000, P010, P100, P110, BUT YOU KNOW, IT IS OKAY!
    end if 

    pressure_ITP = (1.0d0 - dl) * (1.0d0 - dm) * P001+ &
        dl * (1.0d0 - dm) * P101 + &
        dl * dm * P111+ &
        (1.0d0 - dl) * dm * P011

    ! Output the interpolated value
    print *, 'OUT Z!! Interpolated value: ', pressure_ITP

  else
    ! NOW, below is not out of z boundary
    ! set values at 8 points at source grid
    ! K
    P000 = pressure(ix0, iy0, izArray(1, 1))
    P010 = pressure(ix0, iy1, izArray(2, 1))
    P100 = pressure(ix1, iy0, izArray(3, 1))
    P110 = pressure(ix1, iy1, izArray(4, 1))
    ! K+1
    P001 = pressure(ix0, iy0, izArray(1, 2))
    P011 = pressure(ix0, iy1, izArray(2, 2))
    P101 = pressure(ix1, iy0, izArray(3, 2))
    P111 = pressure(ix1, iy1, izArray(4, 2))
    
    print*,'P000= ',P000
    print*,'P010= ',P010
    print*,'P100= ',P100
    print*,'P110= ',P110
    print*,'P001= ',P001
    print*,'P011= ',P011
    print*,'P101= ',P101
    print*,'P111= ',P111

    P1 = (1.0d0 - dl) * (1.0d0 - dm) * P000+ &
        dl * (1.0d0 - dm) * P100 + &
        dl * dm * P110+ &
        (1.0d0 - dl) * dm * P010
    
    P2 =  (1.0d0 - dl) * (1.0d0 - dm) * P001+ &
        dl * (1.0d0 - dm) * P101 + &
        dl * dm * P111+ &
        (1.0d0 - dl) * dm * P011
    ! interpolate in z direction
    z_bottom = minval([zArray(ix0,iy0,izArray(1,1)),zArray(ix0,iy1,izArray(2,1)),&
               zArray(ix1,iy0,izArray(3,1)),zArray(ix1,iy1,izArray(4,1))])
    z_top    = maxval([zArray(ix0,iy0,izArray(1,2)),zArray(ix0,iy1,izArray(2,2)),&
               zArray(ix1,iy0,izArray(3,2)),zArray(ix1,iy1,izArray(4,2))])
    print*,'z_bottom= ',z_bottom
    print*,'z_top= ',z_top
    weight_z = (zT-z_bottom)/(z_top-z_bottom)
    pressure_ITP = (1-weight_z)*P1+ weight_z*P2
    ! Output the interpolated value
    !print *, 'WITHIN Z!! Interpolated value: ', pressure_ITP
  end if

end subroutine TriLinear_ITP


! Subroutine for matrix inversion using LAPACK (you need LAPACK to compile this)
subroutine matinv(A, AI)
  real(8), dimension(4,4) :: A, AI
  integer :: i, info
  real(8), dimension(4) :: work
  integer, dimension(4) :: pivots
  ! Using LAPACK function DGETRI to invert matrix A
  call dgetrf(4, 4, A, 4, pivots, info)
  if (info /= 0) then
    print *, 'Matrix inversion failed, info = ', info
    stop
  end if
  call dgetri(4, A, 4, pivots, work, 4, info)
  AI = A
end subroutine matinv

! Subroutine for matrix-vector multiplication (A * vector)
subroutine matvecmul(A, x, result)
  real(8), dimension(4,4) :: A
  real(8), dimension(4) :: x, result
  integer :: i, j
  result = 0.0
  do i = 1, 4
    do j = 1, 4
      result(i) = result(i) + A(i,j) * x(j)
    end do
  end do
end subroutine matvecmul

! Converts physical (x,y) to logical (l,m)
subroutine XtoL(x, y, alpha, beta, l, m)
  implicit none
  real(8), intent(in) :: x, y           ! Input physical coordinates
  real(8), intent(in) :: alpha(4), beta(4)     ! Coefficients for the mapping
  real(8), intent(out) :: l, m          ! Output logical coordinates
  real(8) :: aa, bb, cc, det            ! Intermediate calculations

  ! Quadratic equation coefficients: aa*mm^2 + bb*m + cc = 0
  aa = alpha(4)*beta(3) - alpha(3)*beta(4)
  bb = alpha(4)*beta(1) - alpha(1)*beta(4) + alpha(2)*beta(3) - alpha(3)*beta(2) + x*beta(4) - y*alpha(4)
  cc = alpha(2)*beta(1) - alpha(1)*beta(2) + x*beta(2) - y*alpha(2)

  ! Compute the discriminant and solve for m using the quadratic formula
  det = sqrt(bb*bb - 4.0d0*aa*cc)   ! 4.0d0 denotes double precision
  m = (-bb + det) / (2.0d0 * aa)

  ! Compute l
  l = (x - alpha(1) - alpha(3)*m) / (alpha(2) + alpha(4)*m)

end subroutine XtoL
