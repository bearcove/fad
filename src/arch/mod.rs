// r[impl callconv.signature]
// r[impl callconv.registers.aarch64]
// r[impl callconv.registers.x86_64]

#[cfg(target_arch = "aarch64")]
mod aarch64;

#[cfg(target_arch = "aarch64")]
pub use aarch64::*;

#[cfg(target_arch = "x86_64")]
mod x64;

#[cfg(target_arch = "x86_64")]
pub use x64::*;
