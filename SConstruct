#!/usr/bin/env python
import os
import sys
from methods import print_error

def embed_file(target, source, env):
    """
    Embeds the content of <source> file into a <target> header file as a const char* constant.
    """
    target_path = str(target[0])
    source_path = str(source[0])

    # Read the content of the source file
    with open(source_path, 'r') as src_file:
        file_content = src_file.read()

    # Generate a unique header guard based on the file name
    target_file_name = os.path.basename(target_path)
    header_guard = target_file_name.replace('-', '_').replace('.', '_').upper()

    # Write the output header file
    constant_name = os.path.splitext(target_file_name)[0].replace('-', '_')
    with open(target_path, 'w') as target_file:
        target_file.write(f"""
#ifndef {header_guard}
#define {header_guard}

const char* {constant_name} = R"({file_content})";

#endif // {header_guard}
""")


libname = "godot-s7-scheme"
projectdir = "demo"

localEnv = Environment(tools=["default"], PLATFORM="")

customs = ["custom.py"]
customs = [os.path.abspath(path) for path in customs]

opts = Variables(customs, ARGUMENTS)
opts.Update(localEnv)

Help(opts.GenerateHelpText(localEnv))

env = localEnv.Clone()

submodule_initialized = False
dir_name = 'godot-cpp'
if os.path.isdir(dir_name):
    if os.listdir(dir_name):
        submodule_initialized = True

if not submodule_initialized:
    print_error("""godot-cpp is not available within this folder, as Git submodules haven't been initialized.
Run the following command to download godot-cpp:

    git submodule update --init --recursive""")
    sys.exit(1)

env = SConscript("godot-cpp/SConstruct", {"env": env, "customs": customs})
env.Append(
    CPPPATH=["src/", "s7/"],
    CPPDEFINES={
        "DISABLE_DEPRECATED": "1",
        "DISABLE_AUTOLOAD": "1",
        "WITH_C_LOADER": "0",
        "WITH_MULTITHREAD_CHECKS": "0",
        "WITH_SYSTEM_EXTRAS": "0",
        "HAVE_COMPLEX_NUMBERS": "0" if env["platform"] == "windows" else "1"
    }
)

sources = [
    Glob("src/*.cpp"),
    Glob("s7/s7.c")
]

if env["target"] in ["editor", "template_debug"]:
    try:
        doc_data = env.GodotCPPDocData("src/gen/doc_data.gen.cpp", source=Glob("doc_classes/*.xml"))
        sources.append(doc_data)
    except AttributeError:
        print("Not including class reference as we're targeting a pre-4.3 baseline.")

file = "{}{}{}".format(libname, env["suffix"], env["SHLIBSUFFIX"])
filepath = ""

if env["platform"] == "macos" or env["platform"] == "ios":
    filepath = "{}.framework/".format(env["platform"])
    file = "{}.{}.{}".format(libname, env["platform"], env["target"])

libraryfile = "bin/{}/{}{}".format(env["platform"], filepath, file)
library = env.SharedLibrary(
    libraryfile,
    source=sources,
)

copy = env.InstallAs("{}/bin/{}/{}lib{}".format(projectdir, env["platform"], filepath, file), library)

embed_scheme_repl = env.Command(
    target="src/repl/s7_scheme_repl_string.hpp",
    source="demo/addons/s7/s7_scheme_repl.scm",
    action=embed_file
)

default_args = [embed_scheme_repl, library, copy]
Default(*default_args)
