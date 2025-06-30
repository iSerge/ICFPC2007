cmake_minimum_required(VERSION 3.24)

project(
  rna-interactive
  VERSION 1.0
  LANGUAGES CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_BUID_TYPE Debug)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTORCC ON)

find_package(
  Qt6
  COMPONENTS Core Gui Widgets
  REQUIRED)

add_executable(rna-interactive "")

target_compile_definitions(rna-interactive PRIVATE WITH_SMART_POINTER)

target_sources(
  rna-interactive
  PRIVATE src/main.cpp
          src/mainview.h
          src/mainview.cpp
          src/mainview.ui
          src/bucket.h
          src/bucket.cpp
          src/prefixmodel.h
          src/prefixmodel.cpp
          src/rnaprocessor.h
          src/rnaprocessor.cpp)
target_compile_options(rna-interactive PRIVATE -Wall -Wextra -Wpedantic)
target_link_libraries(rna-interactive PRIVATE Qt6::Widgets Qt6::Core Qt6::Gui)
