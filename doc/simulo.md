# SimulO

This document explains how to use *SimulO* on Linux in parallel.

## Procedure

- Copy all files using Globus: <https://globus.computecanada.ca/globus-app/transfer>
- Make SimulOC executable:

```bash
chmod +x SimulOC
```

- Connect to *Compute Canada*:

```bash
ssh username@cedar.computecanada.ca
```

- Enter your password

## How to run array job

### Examples

Job are run using a bash script that setup SLURM parameters. The following example will launch 8 simulations in parallel. Each task will use 100 MB of memory and up to 2 minutes CPU time.

```bash
#!/bin/bash

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# send mail to this address
#SBATCH --mail-user=youremail@email.com

#SBATCH --time=00:02:00
#SBATCH --account=def-babin
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=100
#SBATCH --array=1-8

./SimulOC Script_SimulOC.seq $SLURM_ARRAY_TASK_ID
```

The job can be launched using:

```bash
sbatch job.sh
```

The queue can be viewed using:

```bash
squeue -u username
```

One can do a simple run to have an idea on how much time should be allocated to each task:

```
Debut Simul, MaxPhoton=1000000 , MaxTime=0 , IntervalPhoton=100000
Start Simulation
progress (%)10, speed (photon/s): 27593.8189845767
progress (%)20, speed (photon/s): 27329.8715496063
progress (%)30, speed (photon/s): 27292.576419218
progress (%)40, speed (photon/s): 27201.6320979361
progress (%)50, speed (photon/s): 27268.7609075108
progress (%)60, speed (photon/s): 27251.6691647366
progress (%)70, speed (photon/s): 27277.6868521585
progress (%)80, speed (photon/s): 27284.1990382354
progress (%)90, speed (photon/s): 27230.6435508763
progress (%)100, speed (photon/s): 27220.513378887
```

Here we have a total of 1 000 000 photons that are processed as a speed of ~27 000 photons/sec. This means that this should take about ~37 seconds to run. Precise timing can be obtained using:

```bash
time ./SimulOC Script_SimulOC.seq

***************************
*          SimulO         *
*     Monte Carlo 3D      *
*      E. Leymarie        *
* Lab Oceano Villefranche *
***************************
Engine Version : 1.4.5
Path : /home/pmassicotte/Downloads/SimulOC
Open:SimulKd_ADetec.Sci
14, objects found
Debut Simul, MaxPhoton=1000000 , MaxTime=0 , IntervalPhoton=100000
Start Simulation
progress (%)10, speed (photon/s): 33422.4598930663
progress (%)20, speed (photon/s): 33557.0469798707
progress (%)30, speed (photon/s): 33538.2895472069
progress (%)40, speed (photon/s): 33604.9735360878
progress (%)50, speed (photon/s): 33464.9621845952
progress (%)60, speed (photon/s): 33123.550844648
progress (%)70, speed (photon/s): 32857.6793090442
progress (%)80, speed (photon/s): 32395.2217047989
progress (%)90, speed (photon/s): 32177.3328566308
progress (%)100, speed (photon/s): 31783.3645869718
Enregistrement Data Primitive : D0m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement Data Primitive : D1m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement Data Primitive : D2m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement Data Primitive : D3m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement Data Primitive : D4m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement Data Primitive : D5m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement Data Primitive : D6m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement Data Primitive : D7m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement Data Primitive : D8m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement Data Primitive : D9m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement Data Primitive : D10m F:1, Data : Intensité, dans : genericDataFilename3.txt
Enregistrement des résultats dans : genericDataFilename3.txt
./SimulOC Script_SimulOC.seq  31.72s user 0.03s system 99% cpu 31.750 total
```

### Ressources

- <https://support.ceci-hpc.be/doc/_contents/QuickStart/SubmittingJobs/SlurmTutorial.html>
