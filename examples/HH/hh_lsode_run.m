
## HH model driver for Octave/lsode

hh_defs = "hodgkin_huxley.m";

autoload ("hodgkin_huxley", hh_defs );
autoload ("hodgkin_huxley_init", hh_defs );

hodgkin_huxley_init; 

y0 = hodgkin_huxley_init(-65);

t0 = 0.0;
t1 = 200.0;
dt = 0.001;
numpoints = (t1-t0)/dt;
t = linspace(t0,t1,numpoints);

lsode_options("relative tolerance",1e-2);
lsode_options("absolute tolerance",1e-2);
lsode_options("integration method","bdf")
y = lsode (@hodgkin_huxley, y0, t);

log = [t' y];
save "-ascii" "hodgkin_huxley.dat" log;
