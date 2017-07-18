#!/usr/bin/env python

"""
Script for generating pointwise cross sections PURXS urr.xml input file
"""

import os
import urr_xml as xml
import parse_endf_6 as endf6

urr_isotopes = xml.urr_get_args()

# URR settings
settings = xml.SettingsElement()
endf_6_filepath = os.environ.get('ENDF_6_FILEPATH')
if endf_6_filepath:
    if endf_6_filepath[-1] != '/': endf_6_filepath += '/'
    settings.endf_6_filepath(endf_6_filepath)
else:
    raise ValueError('Set ENDF_6_FILEPATH environment variable')
avg_xs_filepath = os.environ.get('AVG_XS_FILEPATH')
if avg_xs_filepath:
    if avg_xs_filepath[-1] != '/': avg_xs_filepath += '/'
    settings.avg_xs_filepath(avg_xs_filepath)
else:
    raise ValueError('Set AVG_XS_FILEPATH environment variable')
settings.formalism('SLBW')
settings.w_function_implementation('quick_w')
settings.num_s_wave(64)
settings.num_p_wave(64)
settings.num_d_wave(64)
settings.num_f_wave(64)
settings.parameter_energy_dependence('resonance')
settings.competitive_structure(True)

# URR pointwise inpu
pointwise = xml.PointwiseElement()
pointwise.source('reconstruction')
pointwise.temperature(293.6)
pointwise.min_energy_spacing(1.0e-8)
pointwise.tolerance(0.001)

# URR isotopes input
isotopes = xml.IsotopesListElement()
urr_files = endf6.urr_filenames(endf_6_filepath)
urr_files.sort()
symbols = endf6.isotope_symbols(urr_files, 'ENDFB71')
for i in range(len(urr_files)):
    include_isotope = False
    if ('all' in urr_isotopes) or (symbols[i] in urr_isotopes):
        include_isotope = True
    if include_isotope:
        isotope = xml.IsotopeElement()
        isotope.endf_6_file(urr_files[i])
        isotopes.add_isotope(isotope)

# Generate urr.xml
urr = xml.URRElement()
urr.add_element(settings)
urr.add_element(isotopes)
urr.add_element(pointwise)
urr.write_xml()
