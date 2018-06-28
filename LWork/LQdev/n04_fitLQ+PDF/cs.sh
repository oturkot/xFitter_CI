#!/bin/bash

############ DESCRIPTION ###############
# Script to submit a condor job
# Usage: cs [options] <command>
# options could be (argument should be provided after option without space, see example below):
#   -t for max. time (in seconds)
#   -m for max. memory (in KB)
#   -p (takes no argument) to prepare submission files but do not submit the job, just print the command (pretend, or dry run)
#   -n for job name
#   -o for output file names
#   -g for group ('MyProject')
#   key=value for any other option to pass to condor_q -append
# Example to submit job with name 'xfit-MyJob' for maximum 1 hour executing command "./xfitter":
# cs -nxfit-MyJob -t3600 ./xfitter
#  ../cs.sh -nxfit-LQ_~V12 -t50000 ../xfitter
# By default max. time and memory are 12h and 2GB
# Files used for submission as well as job output files are stored in directory 'condor' wich is automatically created in currecnt directory
############ END OF DESCRIPTION ###############

# split arguments into two parts (2nd part starts with first argument which does not begin with '-' and does not contain '='): 
# (1) options for submission script 
# (2) command tro run on the cluster
ArgsSub=''
ArgsExe=''
for a do
  #echo $a
  if [[ ! -z "${ArgsExe// }" ]]; then
    ArgsExe=${ArgsExe}' '$a
  elif [[ ${a:0:1} != '-' ]] && [[ *"$a"* != *"="* ]]; then
    ArgsExe=${ArgsExe}' '$a
  else
    ArgsSub=${ArgsSub}' '$a
  fi
done
#echo "ArgsSub: $ArgsSub"
#echo "ArgsExe: $ArgsExe"

# check command
# check if the command to run on the cluster was provided
if [[ -z "${ArgsExe// }" ]]; then
  echo "Error: command to run is not provided"
  echo "Usage: cs [options] <command>"
  exit 1
fi
# check whether command to run is valid
ArgsExe1=`echo $ArgsExe | awk '{print $1}'`
which ${ArgsExe1} >& /dev/null
if [ $? -ne 0 ]; then
  echo "Error: command to run is not valid, see below:"
  which ${ArgsExe1}
  exit 1
fi

# check arguments
FlagPtetend=0
#JobName=`echo $ArgsExe | sed -e 's/ /-/g' | sed -e 's/\.\///g'`
JobName=`echo $ArgsExe | sed -e 's/\.\///g' | awk '{print $1}'`
JobMem='1999'
JobTime='43199'
JobGroup=''
ExtraOptions=()
FlagOuputNameFileExplicit=0

for arg in ${ArgsSub}; do
  # check for pretend
  if [ ${arg} == "--pretend" ] || [ ${arg} == "-p" ]; then
    FlagPtetend=1
  # check for job name
  elif [[ "${arg}"* == "-n"* ]]; then
    JobName=${arg:2}
  # check for requested memory
  elif [[ "${arg}"* == "-m"* ]]; then
    JobMem=${arg:2}
  # check for requested run time
  elif [[ "${arg}"* == "-t"* ]]; then
    JobTime=${arg:2}
  # check for output file pattern
  elif [[ "${arg}"* == "-o"* ]]; then
    NameOutPattern=${arg:2}
    NameFileOutLog=${NameCondorDir}'/'${NameOutPattern}'.log'
    NameFileOutTxt=${NameCondorDir}'/'${NameOutPattern}'.txt'
    NameFileOutErr=${NameCondorDir}'/'${NameOutPattern}'.err'
    FlagOuputNameFileExplicit=1
  elif [[ *"${arg}"* == *"="* ]]; then
    ExtraOptions+=('-append '${arg})
  # check for group name (MyProject)
  elif [[ "${arg}"* == "-g"* ]]; then
    JobGroup=${arg:2}
  else
    echo "Warning: ignoring unknown argument ${arg}"
  fi
done

# create directory for intermediate and output
NameCondorDir='condor'
mkdir -p ${NameCondorDir}

if [ $FlagOuputNameFileExplicit -eq 0 ]; then
  NameFileOutLog=${NameCondorDir}'/'${JobName}'.log'
  NameFileOutTxt=${NameCondorDir}'/'${JobName}'.txt'
  NameFileOutErr=${NameCondorDir}'/'${JobName}'.err'
fi

# trick for condor: store LD_LIBRARY_PATH
export LD_LIBRARY_PATH_STORED=$LD_LIBRARY_PATH

# create script to run on  cluster side
NameScriptFile=${NameCondorDir}'/condor-run-'${JobName}'.sh'
rm -f ${NameScriptFile}

echo '#!/bin/bash' >> ${NameScriptFile}
echo '' >> ${NameScriptFile}
echo 'if [ ! -z $LD_LIBRARY_PATH_STORED ]; then' >> ${NameScriptFile}
echo '  echo "Using LD_LIBRARY_PATH_STORED"' >> ${NameScriptFile}
echo '  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH_STORED:$LD_LIBRARY_PATH' >> ${NameScriptFile}
echo 'fi' >> ${NameScriptFile}
echo '' >> ${NameScriptFile}
echo 'echo "PATH=$PATH"' >> ${NameScriptFile}
echo 'echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"' >> ${NameScriptFile}
echo 'echo "command to run: '${ArgsExe}'"' >> ${NameScriptFile}
echo '' >> ${NameScriptFile}
echo ${ArgsExe} >> ${NameScriptFile}
chmod +x ${NameScriptFile}

# create configureation submit file to submit job to the cluster
NameSubmitFile='condor/condor-submit-'${JobName}
rm -f ${NameSubmitFile}

echo 'Executable = '`pwd`'/'${NameScriptFile} >> ${NameSubmitFile}
echo 'log = '${NameFileOutLog} >> ${NameSubmitFile}
echo 'output = '${NameFileOutTxt} >> ${NameSubmitFile}
echo 'error = '${NameFileOutErr} >> ${NameSubmitFile}
echo 'Universe = vanilla' >> ${NameSubmitFile}
echo 'notification = Error' >> ${NameSubmitFile}
echo 'Initialdir = '`pwd` >> ${NameSubmitFile}
echo 'getenv=True' >> ${NameSubmitFile}
echo '+RequestRuntime = '${JobTime} >> ${NameSubmitFile}
echo 'RequestMemory = '${JobMem} >> ${NameSubmitFile}

#echo 'Requirements = ( OpSysAndVer == "CentOS7" || OpSysAndVer == "SL6")' >> ${NameSubmitFile}
echo 'Requirements = (OpSysAndVer == "SL6")' >> ${NameSubmitFile}

if [ ! -z ${JobGroup} ]; then
  echo '+MyProject = '\"${JobGroup}\" >> ${NameSubmitFile}
fi
#echo '+MyProject = "herafitter"' >> ${NameSubmitFile}
#echo '+MyProject = "zeus"' >> ${NameSubmitFile}

echo 'queue 1' >> ${NameSubmitFile}

# submit job (or print corresponding command)
com="condor_submit -batch-name ${JobName} ${ExtraOptions} ${NameSubmitFile}"
if [ ${FlagPtetend} -eq 1 ]; then
  com='echo '${com}
fi
${com}

exit $?

