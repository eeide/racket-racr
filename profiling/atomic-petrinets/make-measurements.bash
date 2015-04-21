#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

old_pwd=`pwd`

#################################################################################################### Cleanup temporary resources:
my_exit(){
	cd $old_pwd
	rm rerun-measurements.bash
	exit 0
}
trap 'my_exit' 1 2 3 9 15

########################################################################################## Persist configuration in rerun script:
echo "cd ../.." > rerun-measurements.bash
echo "./make-measurements.bash << EOF" >> rerun-measurements.bash

################################################################################################################ Read parameters:
declare -a parameter_names
declare -a parameter_values
declare -a parameter_iterations
declare -a parameter_adjustments
while read -u3 line
do
	IFS='/' read -ra config_line <<< "$line"
	
	parameter_names+=( "${config_line[0]}" )
	
	read -r -p "${config_line[1]}: " choice
	if [ ! -t 0 ]; then echo "${config_line[1]}: $choice"; fi
	echo $choice >> $old_pwd/rerun-measurements.bash
	parameter_values+=( "$choice" )
	
	read -r -n1 -p " Iterate? (y/n): " choice
	if [ ! -t 0 ]; then printf " Iterate? (y/n): $choice"; fi
	echo ""
	printf $choice >> $old_pwd/rerun-measurements.bash
	case $choice in
		[y]* )	read -r -p " Number of iterations: " choice
				if [ ! -t 0 ]; then echo " Number of iterations: $choice"; fi
				echo $choice >> $old_pwd/rerun-measurements.bash
				parameter_iterations+=( "$choice" )
				read -r -p " Adjustment each iteration: " choice
				if [ ! -t 0 ]; then echo " Adjustment each iteration: $choice"; fi
				echo $choice >> $old_pwd/rerun-measurements.bash
				parameter_adjustments+=( "$choice" );;
		[n]* )	parameter_iterations+=( 1 )
				parameter_adjustments+=( 0 );;
		* ) echo "	!!! ERROR: No valid choice entered !!! "; my_exit;;
	esac
done 3< measurements.configuration

############################################################################################################# Create directories:
measurement_dir=$old_pwd/measurements/`date "+%Y-%m-%d_%H-%M-%S"`
mkdir -p $measurement_dir

################################################################################################# Finish & copy the rerun script:
echo "EOF" >> rerun-measurements.bash
chmod +x rerun-measurements.bash
cp -p rerun-measurements.bash $measurement_dir

########################################################################################################### Perform measurements:
num_parameters=${#parameter_names[@]}
current_values=parameter_values
current_iterations=( )
current_parameter=0
run=true
while $run
do
	if (( current_parameter == num_parameters ))
	then
		echo ${current_values[@]}
		if (( current_iterations[0] == parameter_iterations[0] ))
		then
			run=false
			undo=false
		fi
	else
		if undo
		then
			current_parameter=$current_parameter-1
		else
			current_values[$current_parameter]=$(( current_values[current_parameter] + parameter_adjustments[current_parameter] ))
			current_iterations[$current_parameter]=$(( current_iterations[current_parameter] + 1 ))
			current_parameter=$current_parameter+1
			current_values[$current_parameter]=$(( parameter_values[current_parameter] ))
			current_iterations[$current_parameter]=0
		fi
	fi
done

:'measure(){
	if (( current_parameter == num_parameters ))
	then
		echo ${current_values[@]}
	else
		for (( i = 1; i <= parameter_iterations[current_parameter]; i++ ))
		do
			current_values[$current_parameter]=$(( current_values[current_parameter] + parameter_adjustments[current_parameter] ))
			#current_values[$current_parameter+1]=$(( parameter_values[current_parameter + 1] ))
			current_parameter=$current_parameter+1
			measure
			current_iteration[$current_parameter]=$(( current_iteration[current_parameter] + 1 ))
		done
	fi
}
measure'

################################################################################# Finish execution & cleanup temporary resources:
my_exit
