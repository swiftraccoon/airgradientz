package main

import (
	"context"
	"log"
	"os"
	"os/signal"
	"syscall"
)

func main() {
	log.SetOutput(os.Stderr)
	log.SetFlags(log.Ltime | log.Lmicroseconds)

	cfg := LoadConfig()

	db, err := OpenDB(cfg.DBPath)
	if err != nil {
		log.Fatalf("[server] Failed to open database: %v", err)
	}
	defer db.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-sigCh
		log.Printf("[server] Received %v, shutting down", sig)
		cancel()
	}()

	poller := NewPoller(db, &cfg)
	go poller.Run(ctx)

	RunServer(ctx, db, &cfg, poller)

	log.Print("[server] Shut down cleanly")
}
