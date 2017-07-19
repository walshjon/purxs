#################################
# SYSTEM TEST CASE DESCRIPTIONS #
#################################

case-1:  default OpenMC run, no PURXS processing
case-2:  OTF,       event,      SLBW, E_n dependence,   avg competitive # OpenMC transport + PURXS processing
case-3:  OTF,       event,      SLBW, E_n dependence,   res competitive # OpenMC transport + PURXS processing
case-4:  OTF,       event,      SLBW, E_lam dependence, avg competitive # OpenMC transport + PURXS processing
case-5:  OTF,       event,      SLBW, E_lam dependence, res competitive # OpenMC transport + PURXS processing
case-6:  OTF,       simulation, SLBW, E_lam dependence, avg competitive # OpenMC transport + PURXS processing
case-7:  OTF,       simulation, SLBW, E_lam dependence, res competitive # OpenMC transport + PURXS processing
case-8:  pointwise, simulation, SLBW, E_lam dependence, avg competitive # PURXS processing-only
case-9:  pointwise, simulation, SLBW, E_lam dependence, res competitive # PURXS processing-only
case-10: tables,    generate,   SLBW, E_n dependence,   avg competitive # PURXS processing-only
case-11: tables,    generate,   SLBW, E_n dependence,   res competitive # PURXS processing-only
case-12: tables,    generate,   SLBW, E_lam dependence, avg competitive # PURXS processing-only
case-13: tables,    generate,   SLBW, E_lam dependence, res competitive # PURXS processing-only
case-14: tables,    read in,    SLBW, E_n dependence,   avg competitive # OpenMC transport + PURXS processing
case-15: tables,    read in,    SLBW, E_n dependence,   res competitive # OpenMC transport + PURXS processing
case-16: tables,    read in,    SLBW, E_lam dependence, avg competitive # OpenMC transport + PURXS processing
case-17: tables,    read in,    SLBW, E_lam dependence, res competitive # OpenMC transport + PURXS processing
