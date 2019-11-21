#!/bin/bash

function evalResult {
  succ=$(grep '\[SUCCESS\]' <$file | wc -l)
  fail=$(grep '\[FAIL\]' <$file | wc -l)
  warn=$(grep '\[WARNING\]' <$file | wc -l)
  printf "\n#############\nSummary\n#############\nSucceded: $succ\nFailed: $fail\nWarnings: $warn\n"
}

DIR=$(dirname  $0)
file="/tmp/out.txt"
echo -n > $file
recordingsStorage=$DIR/test/Euler/TestData/Recordings/
dirsWithRecordings=$(cd $recordingsStorage && find . -type f -exec dirname {} \; | sort -u | cut -c 3-)
dirsArr=($dirsWithRecordings)
appModeVar="APP_MODE"
recordingsDirVar="PLAYER_RECORDINGS_DIR"
bulkPlayer="BULK_PLAYER"
echo "Recordings storage: $recordingsStorage"
printf "Dirs with recordings:\n$(echo "${dirsArr[@]}" | tr ' ' '\n' | nl -w3 -s') ')\n"
read -p 'Choose what to replay (0 for ALL): ' choice
echo "Your choice is $choice"
if [ $choice -gt 0 ] && [ $choice -le ${#dirsArr[@]} ]
then
  replayingDir=$recordingsStorage${dirsArr[$choice-1]}
  echo "Replaying: ${dirsArr[$choice-1]}"
  export "$appModeVar=$bulkPlayer"
  echo "$appModeVar is $APP_MODE"
  export "$recordingsDirVar=$replayingDir"
  echo "$recordingsDirVar is $PLAYER_RECORDINGS_DIR"
  stack exec euler-backend | tee -a $file
  evalResult
elif [ $choice -eq 0 ]
then
  echo "Replaying ALL"
  export "$appModeVar=$bulkPlayer"
  echo "$appModeVar is $APP_MODE"
  for rDir in ${dirsArr[@]}
    do
      replayingDir=$recordingsStorage$rDir
      printf "\nReplaying: $rDir\n"
      export "$recordingsDirVar=$replayingDir"
      echo "$recordingsDirVar is $PLAYER_RECORDINGS_DIR"
      printf "\n$rDir output:\n" >> $file
      stack exec euler-backend | tee -a $file
    done
    evalResult
else
  echo "Incorrect choice"
fi