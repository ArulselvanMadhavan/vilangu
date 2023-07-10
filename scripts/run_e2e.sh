#!/bin/bash

for f in $(find ./test/e2e -name '*.t'); do # run through program test_suite
    PROGRAM_FILE=$(basename $f) # get file name from path
    OUT_FILES=("${f%.t}.ll" "${f%.t}.out" "${f%.t}.err")  #get output file path
    dune exec vilangu -- $f -o ./test/e2e/
    rm -rf "${f%.t}.ast"
    rm -rf "${f%.t}.ir"
    rm -rf "${f%.t}.exe"

    for((i=0; i<${#OUT_FILES[@]}; i++)); do
        OUT_FILE=${OUT_FILES[$i]}
      if [ -f ${OUT_FILE} ]; then
        if [ -f "${OUT_FILE}.expected" ]; then # if we have expected output already
          diff "${OUT_FILE}" "${OUT_FILE}.expected" # compare output against expected output
          is_diff=$?
          if [ $is_diff -eq 1 ]; then 
            if [ "$1" == "--save" ]; then # if we want to save this output as the expected one for regression tests
              mv "${OUT_FILE}" "${OUT_FILE}.expected"
            else
              echo "Regression tests failed."
              echo $f
              exit 1 #test failed
            fi
          fi
        else
        # create expected output for regression tests in future
          cp "${OUT_FILE}" "${OUT_FILE}.expected"
        fi
        rm -rf ${OUT_FILE}
      fi
    done
done
