// [impl callconv.signature]
// [impl callconv.registers.aarch64]

#[cfg(target_arch = "aarch64")]
mod aarch64;

#[cfg(target_arch = "aarch64")]
pub use aarch64::*;

#[cfg(target_arch = "x86_64")]
compile_error!("x86_64 backend not yet implemented");
