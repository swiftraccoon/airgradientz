#![forbid(unsafe_code)]
#![deny(
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rust_2018_idioms,
    missing_debug_implementations,
    unreachable_pub
)]
#![allow(
    clippy::module_name_repetitions,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::must_use_candidate,
    clippy::redundant_pub_crate
)]

mod api;
mod config;
mod db;
mod error;
mod http;
mod json;
mod log;
mod poller;

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicI64, AtomicU64};
use std::sync::{Arc, Mutex, RwLock};
use std::thread;

use config::Config;
use log::log;
use poller::DeviceHealth;

#[derive(Debug)]
pub(crate) struct AppState {
    pub(crate) db: Mutex<rusqlite::Connection>,
    pub(crate) health: RwLock<HashMap<String, DeviceHealth>>,
    pub(crate) config: Config,
    pub(crate) shutdown: AtomicBool,
    pub(crate) started_at: i64,
    pub(crate) requests_served: AtomicU64,
    pub(crate) active_connections: AtomicI64,
}

fn main() {
    let config = Config::from_env();

    log!("[server] Opening database at {}", config.db_path);

    let conn = rusqlite::Connection::open(&config.db_path).unwrap_or_else(|e| {
        eprintln!("[server] Failed to open database: {e}");
        std::process::exit(1);
    });

    db::initialize(&conn).unwrap_or_else(|e| {
        eprintln!("[server] Failed to initialize database: {e}");
        std::process::exit(1);
    });

    let state = Arc::new(AppState {
        db: Mutex::new(conn),
        health: RwLock::new(HashMap::new()),
        config,
        shutdown: AtomicBool::new(false),
        started_at: db::now_millis(),
        requests_served: AtomicU64::new(0),
        active_connections: AtomicI64::new(0),
    });

    // Spawn poller thread
    let poller_state = Arc::clone(&state);
    thread::spawn(move || {
        poller::run(&poller_state);
    });

    // Run HTTP server on main thread (blocks)
    if let Err(e) = http::server::run(&state) {
        eprintln!("[server] Server error: {e}");
        std::process::exit(1);
    }
}
