list(APPEND _utils_srcs
  binv.f
  cmpbqm.f
  readbp.f
  readmp.f
  sinv.f
  gettab.f90
  split_by_subset.f90
)

foreach(_src ${_utils_srcs})
  get_filename_component(_name "${_src}" NAME_WE)
  set( _exec "${_name}.x")
  add_executable(${_exec} ${_src})
  add_dependencies(${_exec} bufr::bufr_4_DA)
  target_link_libraries(${_exec} PRIVATE bufr::bufr_4_DA)
  list(APPEND _utils_execs ${_exec})
endforeach()

list(APPEND _debuf_srcs
  debufr.f
  debufr.c
  )

add_executable(debufr.x ${_debuf_srcs})
add_dependencies(debufr.x bufr::bufr_4_DA)
target_link_libraries(debufr.x PRIVATE bufr::bufr_4_DA)
list(APPEND _utils_execs debufr.x )


list(APPEND _readtbld_srcs
  readtbld.f90
  )

add_executable(readtbld.x ${_readtbld_srcs})
add_dependencies(readtbld.x bufr::bufr_4_DA)
target_link_libraries(readtbld.x PRIVATE bufr::bufr_4_DA)
list(APPEND _utils_execs readtbld.x )


install(TARGETS ${_utils_execs}
        RUNTIME
        DESTINATION bin
        COMPONENT utilities)
