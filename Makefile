# Include local defenitions
include make.macro

# Put objects and libraries in seperate directories
OBJ_DIR = obj
LIB_DIR = lib

# The source files we need
SRC_FOR = Initialization_module_NeXtSIM.for Command_module_for_NeXtSIM.for Physics.for Solver.for Radiation.for Dust_modules.for
SRC_F90 = F2008_interface.f90

# Resulting object files
OBJ_FOR = $(SRC_FOR:%.for=$(OBJ_DIR)/%.o)
OBJ_F90 = $(SRC_F90:%.f90=$(OBJ_DIR)/%.o)

# Library name
LIB = $(LIB_DIR)/libearthabl.a

# We always need those flags
# I hope this is not too compiler dependent, otherwise they should go into make.macro.
FFLAGS += -std=legacy
F90FLAGS += -std=f2008 #-lstdc++
CXXFLAGS += -lgfortran

# Rules

$(LIB): $(OBJ_FOR) $(OBJ_F90) $(OBJ_CXX)
	@mkdir -p $(LIB_DIR)
	ar -rv $@ $^
	ranlib $@

clean:
	rm -rf $(OBJ_DIR) $(LIB_DIR) tester_f.x tester_c.x

$(OBJ_FOR): $(OBJ_DIR)/%.o : %.for
	@mkdir -p $(OBJ_DIR)
	$(FC) -c $(FFLAGS) $< -o $@

$(OBJ_F90): $(OBJ_DIR)/%.o : %.f90
	@mkdir -p $(OBJ_DIR)
	$(FC) -c $(F90FLAGS) $< -o $@

tester_c.x: tester.cpp $(LIB)
	$(CXX) $(CXXFLAGS) $< -L./lib -learthabl -o $@

tester_f.x: tester.f90 $(LIB)
	$(FC) $(F90FLAGS) $< -L./lib -learthabl -o $@

test: tester_f.x tester_c.x
