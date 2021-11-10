for file in Samples/*.aps
do
    echo $file ": " 
    type=$(./prologTerm $file | swipl -s check.pl -g main_stdin 2>&1)
    if [[ $type = *"void"* ]]; then
        echo "Check type OK."
    else
        echo "Check type error!"
    fi
    res=$(./eval $file)
    if [[ $res = "42" ]]; then
        echo "Evaluete OK."
    else
        echo "Evaluete error!" 
    fi
done