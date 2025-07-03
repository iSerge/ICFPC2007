cmake_minimum_required(VERSION 3.24)

project(
  rnaproc
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

add_executable(rnaproc "")

target_compile_definitions(rnaproc PRIVATE WITH_SMART_POINTER)

target_sources(
  rnaproc
  PRIVATE main.cpp
          rnaprocessor.cpp
          bucket.cpp
          imageview.cpp
          rnaprocessor.h
          bucket.h
          imageview.h
          imageview.ui)
target_compile_options(rnaproc PRIVATE -Wall -Wextra -Wpedantic)
target_link_libraries(rnaproc PRIVATE Qt6::Widgets Qt6::Core Qt6::Gui)
