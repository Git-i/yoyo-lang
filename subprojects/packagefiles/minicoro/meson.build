project(
  'minicoro',
  'c',
  version: '0.1.0',
  license: 'MIT',
  meson_version: '>=0.54.0',
)

minicoro_inc = include_directories('.')

minicoro_dep = declare_dependency(
  include_directories: minicoro_inc,
)

meson.override_dependency('minicoro', minicoro_dep)
