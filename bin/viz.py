#!/usr/bin/env python3

from math import * 
import sys
import sexpdata
from vedo import *

def fconv(xs):
    return tuple(float(x) for x in xs)

with open(sys.argv[1], 'r') as f:
    bench = sexpdata.load(f)

shapes = []

spheres = bench[0]
for s in spheres:
    x, y, z, r = tuple(float(x) for x in s)
    sp = Sphere(pos=[x, y, z], r=r, alpha=0.5)
    shapes.append(sp)

cyls = bench[2]
cyl_offsets = bench[3]
for c in cyls:
    print(c)
    (id_, (rot_x, rot_y, rot_z, r), (y, z)) = (c[0], fconv(c[1]), fconv(c[2]))
    for o in cyl_offsets:
        (id__, x) = (o[0], float(o[1]))
        if id_ == id__:
            cyl = Disc(pos=[0, 0, 0], r2=r, alpha=0.5)
            cyl.rotateZ(rot_z, rad=True)
            cyl.rotateY(rot_y + pi/2, rad=True)
            cyl.rotateX(rot_x, rad=True)
            cyl.pos([x, y, z])
            shapes.append(cyl)

cubs = bench[4]
cubx = bench[5]
cuby = bench[6]
cubz = bench[7]
for c in cubs:
    (id_, rot_x, rot_y, rot_z) = fconv(c)
    for o in cyl_offsets:
        (id__, x) = (o[0], float(o[1]))
        if id_ == id__:
            cyl = Plane(pos=[x, y, z], r2=r, alpha=0.5)
            cyl.rotateX(rot_x)
            cyl.rotateY(rot_y)
            cyl.rotateZ(rot_z)
            shapes.append(cyl)


# declare the instance of the class
vp = Plotter(shape=(2, 2), interactive=0)

# build to sphere meshes
s1 = Sphere(pos=[-3.2, 15.2, 0.8], r=0.8, alpha=0.5)
s2 = Sphere(pos=[1.2, 15.2, 0.8], r=0.8, alpha=0.5)
c1 = Disc(pos=[-4, 10, 5], r2=3.2, alpha=0.5)
c2 = Disc(pos=[17, 3.2, 0.8], r2=0.8, alpha=0.5)
c2.rotateZ(1.57)
vp.show(*shapes, at=0, axes=1)

interactive()
