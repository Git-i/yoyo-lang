project('yoyo-lang','cpp', default_options: ['cpp_std=c++20'])

sources = [
    'src/yoyo/scanner.cpp',
    'src/yoyo/parser.cpp',
    'src/yoyo/statement.cpp',
    'src/yoyo/ir_gen.cpp',
    'src/yoyo/expression_type_checker.cpp',
    'src/yoyo/expression.cpp',
    'src/yoyo/type.cpp',
    'src/yoyo/top_level_visitor.cpp',
    'src/yoyo/expression_evaluator.cpp',
    'src/yoyo/parselets/infix_parselets.cpp',
    'src/yoyo/parselets/prefix_parselets.cpp',
    'src/yoyo/engine.cpp',
    'src/yoyo/module.cpp',
    'src/yoyo/app_module.cpp',
    'src/yoyo/to_string.cpp',
    'src/yoyo/lifetime.cpp',
    'src/yoyo/cfg_node.cpp',
    'src/yoyo/overload_resolve.cpp',
    'src/yoyo/tree_cloner.cpp',
]
yoyo_lib = library('yoyo', sources, include_directories: 'include/yoyo',dependencies: dependency('llvm'))
yoyo_dep = declare_dependency(include_directories: 'include/yoyo', link_with: yoyo_lib,dependencies: dependency('llvm'))
parser_test = executable('parser_test', ['tests/parser_test.cpp'], dependencies: [yoyo_dep, dependency('catch2-with-main')])
irgen_test = executable('ir_gen_test', ['tests/ir_gen_test.cpp'], dependencies: [
                             yoyo_dep,
                             dependency('catch2-with-main'),
                             dependency('libgvc')])