include make.macro

OBJ_DIR = obj
LIB_DIR = lib

SRC_FOR = Initialization_module_NeXtSIM.for Command_module_for_NeXtSIM.for Physics.for Solver.for Radiation.for
SRC_F90 = neXtSIM_f2008_interface.f90
SRC_CXX = ABL.cpp

OBJ_FOR = $(SRC_FOR:%.for=$(OBJ_DIR)/%.o)
OBJ_F90 = $(SRC_F90:%.f90=$(OBJ_DIR)/%.o)
OBJ_CXX = $(SRC_CXX:%.cpp=$(OBJ_DIR)/%.o)

LIB = $(LIB_DIR)/libearthabl.a

FFLAGS += -std=legacy
F90FLAGS += -std=f2008 -lstdc++
CXXFLAGS += -lgfortran

# gfortran -std=legacy -c Initialization_module_NeXtSIM.for Command_module_for_NeXtSIM.for Physics.for Solver.for Radiation.for 
# gfortran -std=f2008 -lstdc++ -c neXtSIM_interface.f90
# g++ -lgfortran -c neXtSIM_interface.cpp


$(LIB): $(OBJ_FOR) $(OBJ_F90) $(OBJ_CXX)
	@mkdir -p $(LIB_DIR)
	ar -rv $@ $^
	ranlib $@

clean:
	rm -rf $(OBJ_DIR) $(LIB_DIR) tester.x

$(OBJ_FOR): $(OBJ_DIR)/%.o : %.for
	@mkdir -p $(OBJ_DIR)
	$(FC) -c $(FFLAGS) $< -o $@

$(OBJ_F90): $(OBJ_DIR)/%.o : %.f90
	@mkdir -p $(OBJ_DIR)
	$(FC) -c $(F90FLAGS) $< -o $@

$(OBJ_CXX): $(OBJ_DIR)/%.o : %.cpp
	@mkdir -p $(OBJ_DIR)
	$(CXX) -c $(CXXFLAGS) $< -o $@

tester.x: tester.cpp $(LIB)
	$(CXX) $(CXXFLAGS) -L./lib -learthabl $< -o $@


