project('yoyo-lang','cpp', default_options: ['cpp_std=c++20'])

sources = [
    'src/yoyo/scanner.cpp',
    'src/yoyo/parser.cpp',
    'src/yoyo/parselets/infix_parselets.cpp',
    'src/yoyo/parselets/prefix_parselets.cpp',
]
library('yoyo', sources, include_directories: 'include/yoyo')