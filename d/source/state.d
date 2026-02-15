module state;

import core.sync.mutex : Mutex;
import core.sync.rwmutex : ReadWriteMutex;
import core.atomic : atomicLoad, atomicOp, atomicStore;

import d2sqlite3 : Database;

import config : Config;
import db : initDb, nowMillis;
import poller : DeviceHealth;

class AppState {
    private Database db_;
    private Mutex dbMutex_;
    private ReadWriteMutex healthLock_;
    private DeviceHealth[string] health_;
    Config config;
    private shared(bool)* shutdownPtr_;

    long startedAt;
    shared long requestsServed;
    shared long activeConnections;

    this(Config cfg, shared(bool)* shutdownPtr) {
        config = cfg;
        shutdownPtr_ = shutdownPtr;
        dbMutex_ = new Mutex();
        healthLock_ = new ReadWriteMutex();
        db_ = initDb(cfg.dbPath);
        startedAt = nowMillis();
        atomicStore(requestsServed, 0L);
        atomicStore(activeConnections, 0L);
    }

    void incrementRequests() {
        atomicOp!"+="(requestsServed, 1L);
    }

    void incrementConnections() {
        atomicOp!"+="(activeConnections, 1L);
    }

    void decrementConnections() {
        atomicOp!"-="(activeConnections, 1L);
    }

    long getRequestsServed() {
        return atomicLoad(requestsServed);
    }

    long getActiveConnections() {
        return atomicLoad(activeConnections);
    }

    /// Execute a delegate while holding the database lock.
    T withDb(T)(scope T delegate(ref Database) dg) {
        dbMutex_.lock();
        scope(exit) dbMutex_.unlock();
        return dg(db_);
    }

    /// Execute a delegate while holding a read lock on health.
    T withHealthRead(T)(scope T delegate(const DeviceHealth[string]) dg) {
        healthLock_.reader.lock();
        scope(exit) healthLock_.reader.unlock();
        return dg(health_);
    }

    /// Execute a delegate while holding a write lock on health.
    void withHealthWrite(scope void delegate(ref DeviceHealth[string]) dg) {
        healthLock_.writer.lock();
        scope(exit) healthLock_.writer.unlock();
        dg(health_);
    }

    @property bool isShutdown() {
        return atomicLoad(*shutdownPtr_);
    }
}
