# -*- coding: utf-8 -*-
"""
Created on Wed Dec 11 10:51:19 2024

@author: liqianyun
"""
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection

# 定义八个顶点的坐标，按顺序给出 (x0, y0, z0), (x1, y0, z0), ..., (x0, y1, z1)
points = np.array([
    [-500.75956505492184,  -8611.6004971993680  , 2465.3151145901375     ],  # (x0, y0, z0)
    [8867.3828598489526, -8151.7948160367187,  2453.9235825955320   ],  # (x1, y0, z0)
    [8411.6353905293054 , 1224.1689998481615 , 2369.9982421673549 ],  # (x1, y1, z0)
    [-962.13354634147902,  766.06001861600134, 2385.3855576773158 ],  # (x0, y1, z0)
    [-500.75956505492184, -8611.6004971993680,  2646.0517838812711],  # (x0, y0, z1)
    [8867.3828598489526, -8151.7948160367187,  2635.0484989546039],  # (x1, y0, z1)
    [8411.6353905293054, 1224.1689998481615, 2675.6354714723557],  # (x1, y1, z1)
    [-962.13354634147902, 766.06001861600134, 2690.3891948607356],  # (x0, y1, z1)
])

# 提取x, y, z坐标
x = points[:, 0]
y = points[:, 1]
z = points[:, 2]

# 创建3D图形并设置画布大小和分辨率
fig = plt.figure(figsize=(12, 10), dpi=300)  # 增大画布和提高分辨率
ax = fig.add_subplot(111, projection='3d')
# 绘制点
ax.scatter(x, y, z, color='r', s=50)

# 连接顶面和底面
# 连接四个点形成底面
ax.plot([x[0], x[1]], [y[0], y[1]], [z[0], z[1]], color='b')
ax.plot([x[1], x[2]], [y[1], y[2]], [z[1], z[2]], color='b')
ax.plot([x[2], x[3]], [y[2], y[3]], [z[2], z[3]], color='b')
ax.plot([x[3], x[0]], [y[3], y[0]], [z[3], z[0]], color='b')

# 连接四个点形成顶面
ax.plot([x[4], x[5]], [y[4], y[5]], [z[4], z[5]], color='b')
ax.plot([x[5], x[6]], [y[5], y[6]], [z[5], z[6]], color='b')
ax.plot([x[6], x[7]], [y[6], y[7]], [z[6], z[7]], color='b')
ax.plot([x[7], x[4]], [y[7], y[4]], [z[7], z[4]], color='b')

# 连接上下两层对应的点
ax.plot([x[0], x[4]], [y[0], y[4]], [z[0], z[4]], color='b')
ax.plot([x[1], x[5]], [y[1], y[5]], [z[1], z[5]], color='b')
ax.plot([x[2], x[6]], [y[2], y[6]], [z[2], z[6]], color='b')
ax.plot([x[3], x[7]], [y[3], y[7]], [z[3], z[7]], color='b')

# 标注点 (0, 0, 2500)
ax.scatter(0, 0, 2500, color='darkblue', s=50)
# 添加文本标注
ax.text(50, 0, 2500, 'Interpolating', color='darkblue', fontsize=12)


# 设置标签
ax.set_xlabel('X (m)')
ax.set_ylabel('Y (m)')
ax.set_zlabel('Z (m)')

plt.savefig("H:/02-研究生/00_fluidity/01_CaseSim_BC/InterpolationCode/SourceGrid.png",dpi=300)
# 显示图形
plt.show()