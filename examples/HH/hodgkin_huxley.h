/* This file was generated by NEMO (http://wiki.call-cc.org/nemo) version 9.0 on Thu Oct 23 23:30:27 2014 */

#include "nest.h"
#include "event.h"
#include "archiving_node.h"
#include "ring_buffer.h"
#include "connection.h"
#include "universal_data_logger.h"
#include "recordables_map.h"


#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_sf_exp.h>
#include <gsl/gsl_odeiv2.h>
#define Ith(v,i)    (v[i])



#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>



namespace nest {



  extern "C" int hodgkin_huxley_dynamics (double, const double*, double*, void*);



  class hodgkin_huxley : public Archiving_Node { 

    public:

    ~hodgkin_huxley ();
    hodgkin_huxley (const hodgkin_huxley &);
    hodgkin_huxley ();

    using Node::connect_sender;
    using Node::handle;

    port check_connection(Connection&, port);
    
    void handle(SpikeEvent &);
    void handle(CurrentEvent &);
    void handle(DataLoggingRequest &); 
    
    port connect_sender(SpikeEvent &, port);
    port connect_sender(CurrentEvent &, port);
    port connect_sender(DataLoggingRequest &, port);
    
    void get_status(DictionaryDatum &) const;
    void set_status(const DictionaryDatum &);
    
    void init_node_(const Node& proto);
    void init_state_(const Node& proto);
    void init_buffers_();
    void calibrate();
    
    void update(Time const &, const long_t, const long_t);

    /**
     * Minimal spike receptor type.
     * @note Start with 1 so we can forbid port 0 to avoid accidental
     *       creation of connections with no receptor type set.
     */
    static const port MIN_SPIKE_RECEPTOR = 1;

    /** 
     * Spike receptors.
     */
    enum SpikeSynapseTypes { 

      SUP_SPIKE_RECEPTOR

    };
  
    friend int hodgkin_huxley_dynamics (double, const double*, double*, void*);

  

  

   // The next two classes need to be friends to access the State_ class/member
   friend class RecordablesMap<hodgkin_huxley>;
   friend class UniversalDataLogger<hodgkin_huxley>;

  
   struct Parameters_ { 

    double K_erev,Na_C_alpha_h,Na_C_alpha_m,Na_A_alpha_h,Na_gbar,Na_A_alpha_m,K_gbar,K_B_alpha_n,K_e,Leak_erev,comp19_V_t,K_g,K_A_alpha_n,Na_erev,comp20_C,Na_C_beta_h,K_C_beta_n,Na_C_beta_m,Na_A_beta_m,comp19_Vrest,K_B_beta_n,Leak_gbar,Na_B_alpha_m,Na_A_beta_h,Na_e,Na_B_alpha_h,Na_g,Na_B_beta_m,K_C_alpha_n,Leak_g,K_A_beta_n,Leak_e,Na_B_beta_h;    double Vrest,V_t;
    Parameters_();
    void get(DictionaryDatum&) const;
    void set(const DictionaryDatum&);

  }; // end struct Parameters_


  struct State_ { 

      enum StateVecElems {
      NA_M60O = 3,
      K_M66O = 2,
      NA_H61O = 1,
      V = 0
      };

      double y_[4]; 

      State_(const Parameters_& p); 
      State_(const State_& s);
      State_& operator=(const State_& s);
      void get(DictionaryDatum&) const;
      void set(const DictionaryDatum&, const Parameters_&);

     int_t r_; /* refractory counter */

  }; // end struct State_

  struct Variables_ { int_t RefractoryCounts_; double U_old_; /* for spike-detection */ };

  struct Buffers_ {

    Buffers_(hodgkin_huxley&);
    Buffers_(const Buffers_&, hodgkin_huxley&);
    UniversalDataLogger<hodgkin_huxley> logger_;

    gsl_odeiv2_step*    s_;    //!< stepping function
    gsl_odeiv2_control* c_;    //!< adaptive stepsize control function
    gsl_odeiv2_evolve*  e_;    //!< evolution function
    gsl_odeiv2_system   sys_;  //!< struct describing system
    unsigned int N;  // size of state vector used by Jacobian
    double *u, *jac;  // intermediate state vectors used for Jacobian approximation



    RingBuffer currents_;

    double_t step_;           //!< step size in ms
    double   IntegrationStep_;//!< current integration time step, updated by solver

  /** 
   * Input current injected by CurrentEvent.
   * This variable is used to transport the current applied into the
   * _dynamics function computing the derivative of the state vector.
   * It must be a part of Buffers_, since it is initialized once before
   * the first simulation, but not modified before later Simulate calls.
   */
    double_t I_stim_;

    }; // end struct Buffers_

  template <State_::StateVecElems elem>
  double_t get_y_elem_() const { return S_.y_[elem]; }

  Parameters_ P_;
  State_      S_;
  Variables_  V_;
  Buffers_    B_;

  static RecordablesMap<hodgkin_huxley> recordablesMap_;

  }; // end class hodgkin_huxley


  inline port hodgkin_huxley::check_connection(Connection& c, port receptor_type)
  {
    SpikeEvent e;
    e.set_sender(*this);
    c.check_event(e);
    return c.get_target()->connect_sender(e, receptor_type);
  }


  inline port hodgkin_huxley::connect_sender(SpikeEvent&, port receptor_type)
  {
    if ( receptor_type < MIN_SPIKE_RECEPTOR || receptor_type >= SUP_SPIKE_RECEPTOR )
    {
      if ( receptor_type < 0 || receptor_type >= SUP_SPIKE_RECEPTOR )
	throw UnknownReceptorType(receptor_type, get_name());
      else
	throw IncompatibleReceptorType(receptor_type, get_name(), "SpikeEvent");
    }
    return receptor_type;
  }
 
  inline port hodgkin_huxley::connect_sender(CurrentEvent&, port receptor_type)
  {
    if (receptor_type != 0)
      throw UnknownReceptorType(receptor_type, get_name());
    return 0;
  }

  inline port hodgkin_huxley::connect_sender(DataLoggingRequest& dlr, 
				            port receptor_type)
  {
    if (receptor_type != 0)
      throw UnknownReceptorType(receptor_type, get_name());
    return B_.logger_.connect_logging_device(dlr, recordablesMap_);
  }

  inline void hodgkin_huxley::get_status(DictionaryDatum &d) const
  {
    P_.get(d);
    S_.get(d);
    Archiving_Node::get_status(d);

    (*d)[names::recordables] = recordablesMap_.get_list();

    def<double_t>(d, names::t_spike, get_spiketime_ms());

    DictionaryDatum receptor_dict_ = new Dictionary();

               
    (*d)[names::receptor_types] = receptor_dict_;
  }


  inline void hodgkin_huxley::set_status(const DictionaryDatum &d)
  {
    Parameters_ ptmp = P_;  // temporary copy in case of errors
    ptmp.set(d);                       // throws if BadProperty
    State_      stmp = S_;  // temporary copy in case of errors
    stmp.set(d, ptmp);                 // throws if BadProperty

    // We now know that (ptmp, stmp) are consistent. We do not 
    // write them back to (P_, S_) before we are also sure that 
    // the properties to be set in the parent class are internally 
    // consistent.
    Archiving_Node::set_status(d);

    // if we get here, temporaries contain consistent set of properties
    P_ = ptmp;
    S_ = stmp;

    calibrate();
  }

} // end namespace nest

