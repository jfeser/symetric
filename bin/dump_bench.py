#!/usr/bin/env python3

import re
import sys

def float_array(name, text):
    m = re.search('float\[NUM_DATA\] %s = \{(.*)\}' % name, text)
    if m is None:
        raise RuntimeError('No float array with name %s' % name)
    return [float(x) for x in m.group(1).split(',')]

def bit_array(name, text):
    m = re.search('bit\[NUM_DATA\] %s = \{(.*)\}' % name, text)
    if m is None:
        raise RuntimeError('No bit array with name %s' % name)
    return [int(x) for x in m.group(1).split(',')]

def spheres(text):
    for m in re.finditer('renderSphere\(p, (.*)\)', text):
        yield [float(x) for x in m.group(1).split(',')]

def float_inner(name, text):
    m = re.search('%s = ([-\.0-9e]*)' % name, text)
    return float(m.group(1))

def float_array_inner(name, text):
    m = re.search('%s = \{(.*?)\}' % name, text)
    return [float(x.strip()) for x in m.group(1).split(',')]

def cuboid_hints(text):
    for m in re.finditer(re.compile('CuboidHint\((.*)\)', flags=re.DOTALL), text):
        inner_text = m.group(1)
        theta_x = float_inner('theta_x', inner_text)
        theta_y = float_inner('theta_y', inner_text)
        theta_z = float_inner('theta_z', inner_text)
        xlist = float_array_inner('xlist', inner_text)
        ylist = float_array_inner('ylist', inner_text)
        zlist = float_array_inner('zlist', inner_text)
        yield (theta_x, theta_y, theta_z, xlist, ylist, zlist)

def cylinder_hints(text):
    for m in re.finditer(re.compile('CylinderHint\((.*?)\)', flags=re.DOTALL), text):
        inner_text = m.group(1)
        theta_x = float_inner('theta_x', inner_text)
        theta_y = float_inner('theta_y', inner_text)
        theta_z = float_inner('theta_z', inner_text)
        radius = float_inner('radius', inner_text)
        y = float_inner(' y', inner_text)
        z = float_inner(' z', inner_text)
        xlist = float_array_inner('xlist', inner_text)
        yield (theta_x, theta_y, theta_z, radius, y, z, xlist)

def dump_sexp(x):
    if isinstance(x, tuple):
        print('(')
        for e in x:
            dump_sexp(e)
            print(' ')
        print(')')
    else:
        print(x)

if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        text = f.read()

    xs = float_array('xs', text)
    ys = float_array('ys', text)
    zs = float_array('zs', text)
    labels = bit_array('labels', text)
    all_spheres = list(spheres(text))
    all_cuboid_hints = list(cuboid_hints(text))
    all_cylinder_hints = list(cylinder_hints(text))

    ops = ['Union', 'Inter', 'Sub']

    for s in all_spheres:
        ops.append(('Sphere', (('center', (('x', str(s[0])), ('y', str(s[1])), ('z', str(s[2])))), ('radius', str(s[2])))))

    for i, c in enumerate(all_cuboid_hints):
        ops.append(('Cuboid', (('id', str(i)), ('theta', (('x', str(c[0])), ('y', str(c[1])), ('z', str(c[2])))))))
        for x in c[3]:
            ops.append(('Offset', (('offset', str(x)), ('type_', (('id', str(i)), ('kind', 'Cuboid_x'))))))
        for y in c[4]:
            ops.append(('Offset', (('offset', str(y)), ('type_', (('id', str(i)), ('kind', 'Cuboid_y'))))))
        for z in c[5]:
            ops.append(('Offset', (('offset', str(z)), ('type_', (('id', str(i)), ('kind', 'Cuboid_z'))))))

    for i, c in enumerate(all_cylinder_hints):
        ops.append(('Cylinder', (('id', str(i)), ('theta', (('x', str(c[0])), ('y', str(c[1])), ('z', str(c[2])))), ('y', str(c[4])), ('z', str(c[5])), ('radius', str(c[3])))))
        for x in c[6]:
            ops.append(('Offset', (('offset', str(x)), ('type_', (('id', str(i)), ('kind', 'Cylinder'))))))

    ops = tuple(ops)

    input_ = tuple(zip(xs, ys, zs))
    sexp = (('ops', ops), ('input', input_), ('output', tuple(labels)))
    dump_sexp(sexp)



