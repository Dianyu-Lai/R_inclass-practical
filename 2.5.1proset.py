# task 4
import numpy as np
import matplotlib.pyplot as plt

# Initialise model variables
water = 42000
salt = 117

# Set model control parameters
runtime = 86400
K_EI = 0.1
salt_leak = 1e-8
water_loss = 750/runtime

ECF_fluid = 14000
ICF_fluid = 28000
ECF_salt = 117 #g
ECF_O = 34.2*ECF_salt
ICF_O = 34.2*ECF_salt/ECF_fluid*ICF_fluid

ECF_fluid_data = np.empty(runtime + 1)
ICF_fluid_data = np.empty(runtime + 1)
ECF_fluid_data[0] = ECF_fluid
ICF_fluid_data[0] = ICF_fluid

ECF_O_data = np.empty(runtime + 1)
ICF_O_data = np.empty(runtime + 1)
ECF_O_data[0] = ECF_O
ICF_O_data[0] = ICF_O

ECF_osmo_data = np.empty(runtime + 1)
ICF_osmo_data = np.empty(runtime + 1)
ECF_osmo_data[0] = ECF_O / (ECF_fluid / 1000)
ICF_osmo_data[0] = ICF_O / (ICF_fluid / 1000)

#salt leak out coeffecient is the same as previous one
for i in range(1, runtime + 1):
    #salt leak out of the body
    ECF_salt -= salt_leak * ECF_salt
    ECF_O = 34.2*ECF_salt
    ICF_O += salt_leak*ECF_salt * (ICF_fluid / ECF_fluid)
    
    osmo_ECF = ECF_O / (ECF_fluid / 1000)
    osmo_ICF = ICF_O / (ICF_fluid / 1000)

    transfer = K_EI * (osmo_ECF - osmo_ICF)
    ECF_fluid += transfer - water_loss
    ICF_fluid -= transfer

    ECF_O_data[i] = ECF_O
    ICF_O_data[i] = ICF_O
    ECF_osmo_data[i] = osmo_ECF
    ICF_osmo_data[i] = osmo_ICF
    ECF_fluid_data[i] = ECF_fluid
    ICF_fluid_data[i] = ICF_fluid

time=np.arange(runtime+1)
fig, axes = plt.subplots(3, 1, figsize=(10, 15))

axes[0].plot(time, ECF_fluid_data, label='ECF')
axes[0].plot(time, ICF_fluid_data, label='ICF')
axes[0].set_title("Fluid change over time")
axes[0].set_xlabel("Time (s)")
axes[0].set_ylabel("Fluid (μL)")
axes[0].legend()

axes[1].plot(time, ECF_O_data, label='ECF')
axes[1].plot(time, ICF_O_data, label='ICF')
axes[1].set_title("Solute change over time")
axes[1].set_xlabel("Time (s)")
axes[1].set_ylabel("Solute (mmol)")
axes[1].legend()

axes[2].plot(time, ECF_osmo_data, label='ECF')
axes[2].plot(time, ICF_osmo_data, label='ICF')
axes[2].set_title("Osmolarity change over time")
axes[2].set_xlabel("Time (s)")
axes[2].set_ylabel("Osmolarity (mOsm/L)")
axes[2].legend()

plt.tight_layout()
plt.show()
