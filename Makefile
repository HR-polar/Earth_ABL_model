# Include local defenitions
include make.macro

# Executable names
ABL = ABL.x
IOTEST = iotest.x

# Put objects and libraries in seperate directories
OBJ_DIR = .obj

FFLAGS += -std=legacy

# The source files for modules
SRC = mod_io.f90

# Resulting object files
OBJ = $(SRC:%.f90=$(OBJ_DIR)/%.o)

$(ABL): ABL.for $(OBJ)
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

$(OBJ): $(OBJ_DIR)/%.o : %.f90
	@mkdir -p $(OBJ_DIR)
	$(FC) -c $(F90FLAGS) $< -o $@

$(IOTEST): iotest.for $(OBJ)
	$(FC) $(FFLAGS) $(LDFLAGS) $^ -o $@

clean:
	rm -rf $(OBJ_DIR) $(ABL) $(IOTEST)
