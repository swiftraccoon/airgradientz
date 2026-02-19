package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"
)

// logf writes a timestamped log line to stderr.
func logf(format string, args ...any) {
	ts := time.Now().Format("2006-01-02 15:04:05")
	msg := fmt.Sprintf(format, args...)
	fmt.Fprintf(os.Stderr, "[%s] %s\n", ts, msg)
}

func main() {
	log.SetOutput(os.Stderr)
	log.SetFlags(0)

	cfg := LoadConfig()

	db, err := OpenDB(cfg.DBPath)
	if err != nil {
		logf("[server] Failed to open database: %v", err)
		os.Exit(1)
	}
	defer db.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-sigCh
		logf("[server] Received %v, shutting down", sig)
		cancel()
	}()

	poller := NewPoller(db, &cfg)
	go poller.Run(ctx)

	RunServer(ctx, db, &cfg, poller)

	logf("[server] Shut down cleanly")
}
