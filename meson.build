project('yoyo-lang','cpp', default_options: ['cpp_std=c++20', 'b_sanitize=address'])

sources = [
    'src/yoyo/scanner.cpp',
    'src/yoyo/parser.cpp',
    'src/yoyo/statement.cpp',
    'src/yoyo/ir_gen.cpp',
    'src/yoyo/expression_type_checker.cpp',
    'src/yoyo/expression.cpp',
    'src/yoyo/type.cpp',
    'src/yoyo/parselets/infix_parselets.cpp',
    'src/yoyo/parselets/prefix_parselets.cpp',
    'src/yoyo/engine.cpp',
    'src/yoyo/module.cpp',
    'src/yoyo/lifetime.cpp',
    'src/yoyo/cfg_node.cpp',
    'src/yoyo/overload_resolve.cpp',
    'src/yoyo/tree_cloner.cpp',
    'src/yoyo/error.cpp',
    'src/yoyo/constexpr_eval.cpp',
    'src/yoyo/macro_eval.cpp',
    'src/yoyo/runtime.cpp',
    'src/yoyo/borrow_checker.cpp',
    'src/yoyo/type_checker.cpp',

    
    'include/yoyo/ast_node.h',
    'include/yoyo/cfg_node.h',
    'include/yoyo/class_entry.h',
    'include/yoyo/common.h',
    'include/yoyo/constraint.h',
    'include/yoyo/engine.h',
    'include/yoyo/error.h',
    'include/yoyo/expression.h',
    'include/yoyo/fn_type.h',
    'include/yoyo/func_sig.h',
    'include/yoyo/generic_clause.h',
    'include/yoyo/ir_gen.h',
    'include/yoyo/module.h',
    'include/yoyo/overload_details.h',
    'include/yoyo/overload_resolve.h',
    'include/yoyo/parser.h',
    'include/yoyo/precedences.h',
    'include/yoyo/scanner.h',
    'include/yoyo/source_location.h',
    'include/yoyo/statement.h',
    'include/yoyo/token.h',
    'include/yoyo/tree_cloner.h',
    'include/yoyo/type.h',
    'include/yoyo/parselets/infix_parselets.h',
    'include/yoyo/parselets/prefix_parselets.h',
    'include/yoyo/constant.h',
    'include/yoyo/runtime.h',
    'include/yoyo/borrow_checker.h',
    'include/yoyo/type_checker.h',
]
backend = get_option('executor')
deps =  [dependency('bdw-gc')]
if backend == 'llvm' or backend == 'both'
    llvm_static = get_option('llvm_static')
    deps += [dependency('llvm', static: llvm_static)]
    sources += [
        'src/yoyo/llvm/ir_gen.cpp',
        'src/yoyo/llvm/expression_evaluator.cpp',
        'src/yoyo/llvm/engine.cpp',
        'src/yoyo/llvm/module.cpp',
        'src/yoyo/to_string.cpp',
        'src/yoyo/llvm/app_module.cpp',

        'include/yoyo/llvm/llvm_irgen.h',
        'include/yoyo/llvm/llvm_engine.h',
        'include/yoyo/llvm/llvm_module.h',
        'include/yoyo/llvm/app_module.h',
    ]
endif

if backend == 'yvm' or backend == 'both'
    yoyo_vm = subproject('yoyo-vm')
    deps += [yoyo_vm.get_variable('yoyo_vm_dep'), dependency('libffi')]
    sources += [
        'include/yoyo/yvm/yvm_irgen.h',
        'include/yoyo/yvm/yvm_engine.h',
        'include/yoyo/yvm/yvm_module.h',
        'include/yoyo/yvm/native_type.h',
        'include/yoyo/yvm/unique_type_selector.h',
        'include/yoyo/yvm/app_module.h',
        'include/yoyo/yvm/fwd_decl.h',

        'src/yoyo/yvm/ir_gen.cpp',
        'src/yoyo/yvm/expression_evaluator.cpp',
        'src/yoyo/yvm/engine.cpp',
        'src/yoyo/yvm/module.cpp',
        'src/yoyo/yvm/native_type.cpp',
        'src/yoyo/yvm/unique_type_selector.cpp',
        'src/yoyo/yvm/app_module.cpp',

        
    ]
    if target_machine.system() == 'windows'
        sources += ['src/yoyo/yvm/windows_native_module.cpp']
    endif
endif
yoyo_args = ['-DYOYO_DLL']
yoyo_lib = library(
    'yoyo', 
    sources, 
    include_directories: ['include/yoyo', 'subprojects/minicoro'],
    dependencies: deps,
    cpp_args: yoyo_args)
yoyo_dep = declare_dependency(include_directories: 'include/yoyo', link_with: yoyo_lib,dependencies: deps)
parser_test = executable('parser_test', ['tests/parser_test.cpp'], dependencies: [yoyo_dep, dependency('catch2-with-main')])
irgen_deps = [
    yoyo_dep,
    dependency('catch2-with-main'),
]
irgen_args = []
gvz = dependency('libgvc', required: false)
if gvz.found()
    irgen_deps += gvz
    irgen_deps += '-DUSE_GRAPHVIZ'
endif
irgen_test = executable('ir_gen_test', ['tests/ir_gen_test.cpp'], dependencies: irgen_deps, cpp_args: irgen_args)
test('irgen', irgen_test, workdir: meson.current_source_dir() / 'tests')