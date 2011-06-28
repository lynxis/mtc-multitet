#!/usr/bin/env python
# -*- coding: utf-8 -*-

from distutils.core import setup

setup(
    name='Multitet',
    version='1.0',
    author='Ka-Ping Yee',
    author_email='<ping at zesty dot ca>',
    url='http://multitetris.com/',
    license='GPL3',
    packages=['multitet'],
    scripts=['scripts/multitet'],
    package_data={
            'multitet': ['media/*.png',],
    }
)
