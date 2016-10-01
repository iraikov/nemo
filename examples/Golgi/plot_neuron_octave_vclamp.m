
CaHVA0       = load ("Golgi_Solinas08_nemo_neuron_vclamp/CaHVA.dat");
CaHVA1       = load ("Golgi_Solinas08_nemo_octave_vclamp/CaHVA.dat");

CaLVA0       = load ("Golgi_Solinas08_nemo_neuron_vclamp/CaLVA.dat");
CaLVA1       = load ("Golgi_Solinas08_nemo_octave_vclamp/CaLVA.dat");

HCN10       = load ("Golgi_Solinas08_nemo_neuron_vclamp/HCN1.dat");
HCN11       = load ("Golgi_Solinas08_nemo_octave_vclamp/HCN1.dat");

HCN20       = load ("Golgi_Solinas08_nemo_neuron_vclamp/HCN2.dat");
HCN21       = load ("Golgi_Solinas08_nemo_octave_vclamp/HCN2.dat");

KA0       = load ("Golgi_Solinas08_nemo_neuron_vclamp/KA.dat");
KA1       = load ("Golgi_Solinas08_nemo_octave_vclamp/KA.dat");

KCa0       = load ("Golgi_Solinas08_nemo_neuron_vclamp/KCa.dat");
KCa1       = load ("Golgi_Solinas08_nemo_octave_vclamp/KCa.dat");

KM0      = load ("Golgi_Solinas08_nemo_neuron_vclamp/KM.dat");
KM1      = load ("Golgi_Solinas08_nemo_octave_vclamp/KM.dat");

KV0      = load ("Golgi_Solinas08_nemo_neuron_vclamp/KV.dat");
KV1      = load ("Golgi_Solinas08_nemo_octave_vclamp/KV.dat");

SK20      = load ("Golgi_Solinas08_nemo_neuron_vclamp/SK2.dat");
SK21      = load ("Golgi_Solinas08_nemo_octave_vclamp/SK2.dat");

Na0      = load ("Golgi_Solinas08_nemo_neuron_vclamp/Na.dat");
Na1      = load ("Golgi_Solinas08_nemo_octave_vclamp/Na.dat");

NaR0      = load ("Golgi_Solinas08_nemo_neuron_vclamp/NaR.dat");
NaR1      = load ("Golgi_Solinas08_nemo_octave_vclamp/NaR.dat");

NaP0      = load ("Golgi_Solinas08_nemo_neuron_vclamp/NaP.dat");
NaP1      = load ("Golgi_Solinas08_nemo_octave_vclamp/NaP.dat");


subplot(3,4,1);
plot(CaHVA0(:,1),CaHVA0(:,2),CaHVA1(:,1),CaHVA1(:,2),'linewidth',2);
title ("CaHVA current");

subplot(3,4,2);
plot(CaLVA0(:,1),CaLVA0(:,2),CaLVA1(:,1),CaLVA1(:,2),'linewidth',2);
title ("CaLVA current");

subplot(3,4,3);
plot(HCN10(:,1),HCN10(:,2),HCN11(:,1),HCN11(:,2),'linewidth',2);
title ("HCN1 current");

subplot(3,4,4);
plot(HCN20(:,1),HCN20(:,2),HCN21(:,1),HCN21(:,2),'linewidth',2);
title ("HCN2 current");

subplot(3,4,5);
plot(KA0(:,1),KA0(:,2),KA1(:,1),KA1(:,2),'linewidth',2);
title ("KA current");

subplot(3,4,6);
plot(KCa0(:,1),KCa0(:,2),KCa1(:,1),KCa1(:,2),'linewidth',2);
title ("KCa current");

subplot(3,4,7);
plot(KM0(:,1),KM0(:,2),KM1(:,1),KM1(:,2),'linewidth',2);
title ("KM current");

subplot(3,4,8);
plot(KV0(:,1),KV0(:,2),KV1(:,1),KV1(:,2),'linewidth',2);
title ("KV current");

subplot(3,4,9);
plot(SK20(:,1),SK20(:,2),SK21(:,1),SK21(:,2),'linewidth',2);
title ("SK2 current");

subplot(3,4,10);
plot(Na0(:,1),Na0(:,2),Na1(:,1),Na1(:,2),'linewidth',2);
title ("Na current");

subplot(3,4,11);
plot(NaR0(:,1),NaR0(:,2),NaR1(:,1),NaR1(:,2),'linewidth',2);
title ("NaR current");

subplot(3,4,12);
plot(NaP0(:,1),NaP0(:,2),NaP1(:,1),NaP1(:,2),'linewidth',2);
title ("NaP current");

print  ("NEURON_Golgi_Solinas08_Neuron_Octave_Vclamp.eps", "-depsc");
