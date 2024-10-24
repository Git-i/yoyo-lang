project('yoyo-lang','cpp', default_options: ['cpp_std=c++20'])

sources = [
    'src/yoyo/scanner.cpp',
    'src/yoyo/parser.cpp',
    'src/yoyo/statement.cpp',
    'src/yoyo/ir_gen.cpp',
    'src/yoyo/expression_type_checker.cpp',
    'src/yoyo/expression.cpp',
    'src/yoyo/top_level_visitor.cpp',
    'src/yoyo/parselets/infix_parselets.cpp',
    'src/yoyo/parselets/prefix_parselets.cpp',
]
yoyo_lib = library('yoyo', sources, include_directories: 'include/yoyo')
yoyo_dep = declare_dependency(include_directories: 'include/yoyo', link_with: yoyo_lib, dependencies: dependency('llvm'))
parser_test = executable('parser_test', ['tests/parser_test.cpp'], dependencies: [yoyo_dep, dependency('catch2-with-main')])