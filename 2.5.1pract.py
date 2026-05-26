import numpy as np
import matplotlib.pyplot as plt

# task 1 & 2
# Initialise model variables
water = 42000
salt = 117

# Set model control parameters
runtime = 86400
K_EI = 0.1
salt_leak = 1e-4
water_loss = 750/runtime

#initialise recording arrays
salt_data = np.empty(runtime + 1)  
osmo_data = np.empty(runtime + 1)
water_data = np.empty(runtime + 1)
salt_data[0] = salt
water_data[0] = water
osmo_data[0] = (salt * 34.2) / (water / 1000)
print(f"Initial osmolarity: {osmo_data[0]:.2f} mOsm/L")

# Run model loop
for i in range(1, runtime + 1):
    
    #salt NaCl leak out of the body
    salt -= salt_leak * salt
    salt_data[i] = salt
    osmo_data[i] = (salt * 34.2) / (water / 1000)
    #water moves out due to osmosis
    water -= K_EI * (osmo_data[i-1] - osmo_data[i]) + water_loss
    water_data[i] = water

time=np.arange(runtime+1)
fig,axes=plt.subplots(3,1,figsize=(10,15))

axes[0].plot(time,salt_data)
axes[0].set_title("Salt change over time")
axes[0].set_xlabel("Time (s)")
axes[0].set_ylabel("Salt (g)")

axes[1].plot(time,water_data)
axes[1].set_title("Water change over time")
axes[1].set_xlabel("Time (s)")
axes[1].set_ylabel("Water (ml)")

axes[2].plot(time,osmo_data)
axes[2].set_title("Osmolarity change over time")
axes[2].set_xlabel("Time (s)")
axes[2].set_ylabel("Osmolarity (mOsm/L)")

plt.tight_layout()
plt.show()
