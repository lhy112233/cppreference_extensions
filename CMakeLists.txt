cmake_minimum_required(VERSION 3.20.0)

# ###############################################
# Project
# ###############################################
project(cpperference_extensions VERSION 0.0.1 LANGUAGES CXX C)

# ###############################################
# Global Default Options
# ###############################################
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# ###############################################
# Helper clangd configuration
# ###############################################
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# ###############################################
# Subdirectories
# ###############################################
add_subdirectory(src)
add_subdirectory(tests)