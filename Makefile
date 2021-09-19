# Include local defenitions
include make.macro

# Executable names
ABL = ABL.x
IOTEST = iotest.x

# Put objects and libraries in seperate directories
OBJ_DIR = .obj

FFLAGS += -std=legacy
LDFLAGS += -ldatetime -lnetcdff -lnetcdf -lncio

# The source files for modules
SRC = mod_io.f90
S77 = Dust_modules.for Physics.for Radiation.for Solver.for

# Resulting object files
OBJ = $(SRC:%.f90=$(OBJ_DIR)/%.o)
O77 = $(S77:%.for=$(OBJ_DIR)/%.o)

$(ABL): ABL.for $(OBJ) $(O77)
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

$(OBJ): $(OBJ_DIR)/%.o : %.f90
	@mkdir -p $(OBJ_DIR)
	$(FC) -c $(F90FLAGS) $< -o $@

$(O77): $(OBJ_DIR)/%.o : %.for
	@mkdir -p $(OBJ_DIR)
	$(FC) -c $(FFLAGS) $< -o $@

$(IOTEST): iotest.for $(OBJ) $(O77)
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

test: $(IOTEST)
	./$(IOTEST)

clean:
	rm -rf $(OBJ_DIR) $(ABL) $(IOTEST)
