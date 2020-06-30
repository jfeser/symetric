import random

def in_sphere(s, v):
    (sx, sy, sz) = s
    (x, y, z) = v
    return (sx - x) ** 2 + (sy - y) ** 2 + (sz - z) ** 2 <= 1

def in_cuboid(v):
    (x, y, z) = v
    return (-1 <= x <= 7 and -1 <= y <= 3 and -1 <= z <= 3)

s1 = (1, 1, 1)
s2 = (3, 1, 1)
s3 = (5, 1, 1)

points = []
print('(')
for i in range(100):
    x = random.uniform(-1.25, 7.25)
    y = random.uniform(-1.25, 3.25)
    z = random.uniform(-1.25, 3.25)
    points.append((x, y, z))
    print('(%f %f %f)' % (x, y, z))
print(')')

print('(')
for p in points:
    if in_cuboid(p) and not (in_sphere(s1, p) or in_sphere(s2, p) or in_sphere(s3, p)):
        print('1 ')
    else:
        print('0 ')
print(')')
