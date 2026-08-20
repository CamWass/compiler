echo "Use CTRL+BREAK to exit"
start /realtime /affinity 4 /b /wait cargo bench --bench bench_main -- --baseline master