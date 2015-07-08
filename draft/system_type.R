
# A structure for a system type

system <- list( 
    time          = double,
    timestep      = double, 
    state         = double[],
    sys_params    = list,
    solver_params = list(sourcefile      = char[],
                         params_compiled = double[],
                         ode_options     = list),
    stability     = int)