import numpy as np
import matplotlib.pyplot as plt

# Initialise model variables
water = 42000
salt = 117
water_ECF = 0.33 * water
water_ICF = 0.67 * water
osm_ECF = 34.2 * salt
osm_ICF = 8000

# Set control parameters
runtime = 86400

# Set model parameters
ktrans = 0.01
kv=0.4
R_k=2
R_n=2
AVPthre=285
Umin=400/runtime
Umax=1500/runtime
water_loss = 750
water_loss = water_loss / 86400
water_ECF_data = np.empty(runtime + 1)
water_ICF_data = np.empty(runtime + 1)
osm_ECF_data = np.empty(runtime + 1)
osm_ICF_data = np.empty(runtime + 1)
osmo_ECF_data = np.empty(runtime + 1)
osmo_ICF_data = np.empty(runtime + 1)

# Initialise recording arrays
water_ECF_data[0] = water_ECF
water_ICF_data[0] = water_ICF
osm_ECF_data[0] = osm_ECF
osm_ICF_data[0] = osm_ICF
osmo_ECF_data[0] = osm_ECF / (water_ECF / 1000)
osmo_ICF_data[0] = osm_ICF / (water_ICF / 1000)


# Run model loop
for i in range(1, runtime + 1):
    #vesopressin
    if osmo_ECF_data[i-1] > AVPthre:
        AVP = kv*(osmo_ECF_data[i-1] - AVPthre)
    else:       AVP = 0
    Rv=AVP[i]^2/(AVP**R_n+R_k**R_n)
    Uw=Umin+(1-Rv)*(Umax-Umin)

    osmo_ECF = osm_ECF / (water_ECF / 1000)
    osmo_ICF = osm_ICF / (water_ICF / 1000)
    water_trans = ktrans * (osmo_ECF - osmo_ICF)
    water_ECF = water_ECF + water_trans - water_loss - Uw
    if water_ECF < 0: water_ECF = 0
    water_ICF = water_ICF - water_trans
    osm_ECF = osm_ECF + 0
    water_ECF_data[i] = water_ECF
    water_ICF_data[i] = water_ICF
    osm_ECF_data[i] = osm_ECF
    osm_ICF_data[i] = osm_ICF
    osmo_ECF_data[i] = osmo_ECF
    osmo_ICF_data[i] = osmo_ICF
    
