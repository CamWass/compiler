echo "Use CTRL+BREAK to exit"
start /realtime /affinity 3 /b /wait cargo bench --bench bench_main -- --save-baseline master