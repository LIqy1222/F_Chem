# -*- coding: utf-8 -*-
"""
Created on Wed Dec 11 20:15:41 2024

@author: liqianyun
"""

# load package
from netCDF4 import Dataset
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# 1.read from WRFOUT -- pay attention to your path
cd H:/02-研究生/00_fluidity/01_CaseSim_BC/data/wrfout
file_path = './wrfout_d02_2016-12-11_00_00_00.nc'  
ds = Dataset(file_path)

# 2.get lat & lon
lat = ds.variables['XLAT'][0, :, :]     # (south_north=300, west_east=198)
lon = ds.variables['XLONG'][0, :, :]    # (south_north=300, west_east=198)


# 3. get Pressure varible
# NOTE: Pressure at theta-points, but PH&PHB are at W-points, so need to transform
# 3.1 calculate the elevation at W-points & theta-points
PH        = ds.variables['PH'][0,:, :, :]
PHB       = ds.variables['PHB'][0,:, :, :]  #stagger =31 more than 30 for pressure
Z_W       = (PH+PHB)/9.81  # elevation at W_point
# interpolate elevation to cell midplane ~VERY SMART THINKING
Z_theta   = 0.5 * (Z_W[:-1, :, :]+Z_W[1:, :, :])  # (30,300,198) got it!
# 3.2 get pressure
P         = ds.variables['P'][0,:, :, :]   # perturbation pressure Pa
PB        = ds.variables['PB'][0,:, :, :]  # BASE STATE PRESSURE   Pa
pressure  = P+PB                           # PRESSURE (30,300,198)


# 4. interpolation setting
# 4.1 WRF LAT&LON to Adaptive mesh x&y
# lat&lon transform to x&y，the left corner as (0,0)
# earth radius (m)
R = 6371.0*1000.0
# left&bottom lat&lon
left_bottom_lon = 109.0
left_bottom_lat = 34.0
#calculate distance between point and (0,0) (m)
def lon_to_m(lon):
    return (2 * np.pi * R * np.cos(np.deg2rad(left_bottom_lat))) / 360 * (lon - left_bottom_lon)
def lat_to_m(lat):
    return (2 * np.pi * R) / 360 * (lat - left_bottom_lat)
# lat&lon to x&y
x = lon_to_m(lon)
y = lat_to_m(lat)


# 读取 ITP.txt 文件中的数据
cd ../../InterpolationCode
data = np.loadtxt('02_ITP_result_3d.txt',skiprows=1)  # 假设数据格式为：x, y, z, 插值结果val

# 提取 x, y, z 和插值结果
x_points = data[:, 0]
y_points = data[:, 1]
z_points = data[:, 2]
val_points = data[:, 3]


# create original points
z_coords_flat          = Z_theta .flatten()
x_coords_flat          = np.tile(x.flatten(), 30)  # change into (30 * 9944,)
y_coords_flat          = np.tile(y.flatten(), 30)  #  (30 * 9944,)
points_original        = np.column_stack((x_coords_flat, y_coords_flat, z_coords_flat))
# traverse every layer x&y, and fill z for that point, repeat z=z+1    
# flatten pressure
pressure_flat          = pressure.flatten()


# 创建三维绘图
fig = plt.figure(figsize=(12, 8))
ax = fig.add_subplot(111, projection='3d')

# 绘制源网格气压的三维散点图
# sc = ax.scatter(x_coords_flat ,y_coords_flat ,z_coords_flat , c=pressure_flat, cmap='viridis', alpha=0.5, label="Pressure")

# 绘制插值结果的三维散点图
sc= ax.scatter(x_points, y_points, z_points, c=val_points, cmap='plasma', marker='^', s=50, label="Interpolated Points")

# 添加颜色条
fig.colorbar(sc, ax=ax, label='Pressure(Pa)')

# 设置标签和标题
ax.set_xlabel('X(m)')
ax.set_ylabel('Y(m)')
ax.set_zlabel('Z(m)')
ax.set_title('Interpolation Results')

# 添加图例
ax.legend()

plt.savefig('./Interpolation_3D_distribution.png',dpi=300)
# 显示图形
plt.show()


# 创建三维绘图  - 源网格信息
fig = plt.figure(figsize=(12, 8))
ax = fig.add_subplot(111, projection='3d')

# 绘制源网格气压的三维散点图
sc = ax.scatter(x_coords_flat ,y_coords_flat ,z_coords_flat , c=pressure_flat, cmap='plasma', alpha=0.5, label="Pressure")

# 添加颜色条
fig.colorbar(sc, ax=ax, label='Pressure(Pa)')

# 设置标签和标题
ax.set_xlabel('X(m)')
ax.set_ylabel('Y(m)')
ax.set_zlabel('Z(m)')
ax.set_title('Source Grid Result')

# 添加图例
ax.legend()

plt.savefig('./SourceGrid_3D_distribution.png',dpi=300)
# 显示图形
plt.show()