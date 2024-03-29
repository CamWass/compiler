echo "Use CTRL+BREAK to exit"
start /realtime /affinity 1 /b /wait cargo bench --bench bench_main -- --baseline master 2>NUL