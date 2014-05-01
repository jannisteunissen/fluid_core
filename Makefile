FC 	:= gfortran
FFLAGS	:= -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow -g -O3
OBJS	:= m_transport_data.o m_time_steppers.o m_transport_schemes.o
TESTS	:= test_m_transport_data test_m_transport_schemes test_m_time_steppers

LIBS	:= fluid_core
LIBDIRS := .

%.o: 	%.f90
	$(FC) -c -o $@ $< $(FFLAGS)

%:	%.o
	$(FC) -o $@ $^ $(FFLAGS) $(addprefix -L,$(LIBDIRS)) $(addprefix -l,$(LIBS))

.PHONY: all test clean

all: 	libfluid_core.a

libfluid_core.a: $(OBJS)
	$(RM) $@
	$(AR) rcs $@ $^

$(TESTS): libfluid_core.a

test: 	$(OBJS) $(TESTS)
	$(foreach test, $(TESTS), ./$(test);)

clean:
	$(RM) *.o *.mod $(TESTS)

# Dependency information
