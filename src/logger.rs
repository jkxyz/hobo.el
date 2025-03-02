use env_logger::{Builder, Env};
use log::LevelFilter;
use std::io::{self, Write};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::thread;

/// A writer adapter that sends output to TCP clients
#[derive(Clone)]
struct TcpWriter {
    clients: Arc<Mutex<Vec<TcpStream>>>,
}

impl TcpWriter {
    fn new() -> Self {
        Self {
            clients: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn add_client(&self, stream: TcpStream) {
        let mut clients = self.clients.lock().unwrap();
        clients.push(stream);
    }
}

impl Write for TcpWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut clients = self.clients.lock().unwrap();
        let buf_len = buf.len();

        if clients.is_empty() {
            return Ok(buf_len); // Pretend we wrote it even if no clients
        }

        // Write to all clients, removing any that fail
        clients.retain_mut(|client| match client.write_all(buf) {
            Ok(_) => true,
            Err(_) => false,
        });

        Ok(buf_len)
    }

    fn flush(&mut self) -> io::Result<()> {
        let mut clients = self.clients.lock().unwrap();

        // Flush all clients, removing any that fail
        clients.retain_mut(|client| match client.flush() {
            Ok(_) => true,
            Err(_) => false,
        });

        Ok(())
    }
}

pub struct TcpLoggerServer {
    server_addr: SocketAddr,
    tcp_writer: TcpWriter,
}

impl TcpLoggerServer {
    pub fn new() -> io::Result<Self> {
        // Start TCP server on a random port by binding to 0
        let listener = TcpListener::bind("127.0.0.1:0")?;
        let server_addr = listener.local_addr()?;

        let tcp_writer = TcpWriter::new();
        let writer_clone = tcp_writer.clone();

        // Spawn a thread to accept new connections
        thread::spawn(move || {
            for stream in listener.incoming() {
                match stream {
                    Ok(stream) => {
                        // Set non-blocking mode to avoid hanging on writes
                        if let Err(e) = stream.set_nonblocking(true) {
                            eprintln!("Failed to set non-blocking mode: {}", e);
                            continue;
                        }

                        writer_clone.add_client(stream);
                    }
                    Err(e) => {
                        eprintln!("Error accepting connection: {}", e);
                    }
                }
            }
        });

        Ok(Self {
            server_addr,
            tcp_writer,
        })
    }

    pub fn server_addr(&self) -> SocketAddr {
        self.server_addr
    }

    pub fn init(&self, level: LevelFilter) -> Result<(), log::SetLoggerError> {
        let env = Env::default();
        let mut builder = Builder::from_env(env);

        // Create a writer for the TCP clients
        let tcp_writer = self.tcp_writer.clone();

        // Configure the builder to ONLY use our TcpWriter as the target
        // This uses env_logger's built-in formatting
        builder
            .filter_level(level)
            .target(env_logger::Target::Pipe(Box::new(tcp_writer)))
            .init();

        Ok(())
    }
}
