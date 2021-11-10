for file in Samples/*.aps
do
    echo $file ": " 
    type=$(./prologTerm $file | swipl -s check.pl -g main_stdin 2>&1)
    if [[ $type = *"void"* ]]; then
        echo "\033[32m Check OK. \033[0m"
    else
        echo "\033[31m Check error!"
    fi
    res=$(./eval $file)
    if [[ $res = "42" ]]; then
        echo "\033[32m Evaluete OK. \033[0m"
    else
        echo "\033[31m Eval Checking error!" 
    fi
done