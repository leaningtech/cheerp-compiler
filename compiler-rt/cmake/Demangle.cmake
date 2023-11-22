cmake_minimum_required(VERSION 3.13.4)
project (demangle)

include_directories(${LLVM_MAIN_SRC_DIR}/include)

add_library(demangle STATIC
  ${LLVM_MAIN_SRC_DIR}/lib/Demangle/Demangle.cpp
  ${LLVM_MAIN_SRC_DIR}/lib/Demangle/DLangDemangle.cpp
  ${LLVM_MAIN_SRC_DIR}/lib/Demangle/ItaniumDemangle.cpp
  ${LLVM_MAIN_SRC_DIR}/lib/Demangle/MicrosoftDemangle.cpp
  ${LLVM_MAIN_SRC_DIR}/lib/Demangle/MicrosoftDemangleNodes.cpp
  ${LLVM_MAIN_SRC_DIR}/lib/Demangle/RustDemangle.cpp)


install(TARGETS demangle DESTINATION "lib")
