
## HH model driver for Octave/lsode

hh_defs = "hodgkin_huxley.m";

autoload ("hodgkin_huxley", hh_defs );
autoload ("hodgkin_huxley_init", hh_defs );

hodgkin_huxley_init; 

y0 = hodgkin_huxley_init(-65);


function y = step (x, k)
  y = 1 ./ (1 + exp(-2 * k * x));
end

function y = stepp (x, k)
  y = 2 ./ (1 + exp(2 * k * x));
end

function i = ifn(t,k,hc,cc,t1,t2)
  i = hc + (cc - hc) .* step(t - t1,k) .* step(t2 - t,k);
endfunction

t0 = 0.0;
t1 = 500.0;
dt = 0.001;
numpoints = (t1-t0)/dt;
t = linspace(t0,t1,numpoints);

reltol = 1e-1;
abstol = 1e-1;

options = CVodeSetOptions('RelTol',reltol, 'AbsTol',abstol,
			  'MinStep',1e-7, 'MaxStep',1e-1,
			  'MaxOrder',1
			  );

function [dy flag params] = myHH(t, y, params)
  global i ik ina

  [dy flag] = hodgkin_huxley(t, y);

  i_stim = ifn(t,3,0,8.0,95,450);

  [y(1) i ik ina i_stim]
  
  dy(1) = dy(1) + (i_stim);

endfunction

CVodeInit(@myHH, 'BDF', 'Newton', t0, y0, options);


tt = t0+dt:dt:t1;

[status,t,y] = CVode(tt,'Normal');

HH_v = [t' y'];
save -ascii "HH_v.dat" HH_v;
