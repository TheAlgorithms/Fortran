cmake_minimum_required(VERSION 3.16)
project(FortranProject LANGUAGES Fortran)

add_compile_options(-Wall -Wextra -Wpedantic)

function(add_fortran_sources DIR SOURCES)
    file(GLOB_RECURSE NEW_SOURCES "${DIR}/*.f90")
    list(APPEND ${SOURCES} ${NEW_SOURCES})
    set(${SOURCES} ${${SOURCES}} PARENT_SCOPE)
endfunction()

set(MODULE_SOURCES)
add_fortran_sources(${CMAKE_SOURCE_DIR}/modules MODULE_SOURCES)

add_library(modules STATIC ${MODULE_SOURCES})

function(create_unique_name FILE_NAME OUTPUT_NAME)
    file(RELATIVE_PATH REL_PATH "${CMAKE_SOURCE_DIR}" "${FILE_NAME}")
    get_filename_component(CUR_EXT "${REL_PATH}" LAST_EXT)
    string(REPLACE "/" "_" UNIQUE_NAME "${REL_PATH}")
    string(REPLACE "${CUR_EXT}" "" UNIQUE_NAME "${UNIQUE_NAME}")
    set(${OUTPUT_NAME} ${UNIQUE_NAME} PARENT_SCOPE)
endfunction()


file(GLOB_RECURSE TEST_FILES "${CMAKE_SOURCE_DIR}/tests/*.f90")

foreach(TEST_FILE ${TEST_FILES})
    create_unique_name(${TEST_FILE} TEST_NAME)
    add_executable(${TEST_NAME} ${TEST_FILE})
    target_link_libraries(${TEST_NAME} modules)
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
endforeach()

file(GLOB_RECURSE EXAMPLE_FILES "${CMAKE_SOURCE_DIR}/examples/*.f90")

foreach(EXAMPLE_FILE ${EXAMPLE_FILES})
    create_unique_name(${EXAMPLE_FILE} EXAMPLE_NAME)
    add_executable(${EXAMPLE_NAME} ${EXAMPLE_FILE})
    target_link_libraries(${EXAMPLE_NAME} modules)
    list(APPEND EXAMPLE_NAME_LIST run_${EXAMPLE_NAME})
    add_custom_target(run_${EXAMPLE_NAME}
        COMMAND ${EXAMPLE_NAME}
        DEPENDS ${EXAMPLE_NAME}
        COMMENT "Running example: ${EXAMPLE_NAME}")
endforeach()

enable_testing()

add_custom_target(run_all_examples DEPENDS ${EXAMPLE_NAME_LIST})
