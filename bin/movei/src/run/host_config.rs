use anyhow::{bail, Result};
use libra_crypto::{ed25519::Ed25519PrivateKey, PrivateKey};
use libra_types::account_address;
use move_core_types::account_address::AccountAddress;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
#[derive(Debug, Deserialize, Serialize)]
pub struct HostConfig {
    pub name: String,
    pub conf: Config,
}

#[derive(Debug, Deserialize, Serialize)]
pub enum Config {
    Libra(LibraHostConfig),
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LibraHostConfig {
    pub host: String,
    pub port: u16,
    pub address: AccountAddress,
    pub private_key: Ed25519PrivateKey,
}

impl Default for LibraHostConfig {
    fn default() -> Self {
        let mut buf = [0u8; Ed25519PrivateKey::LENGTH];
        buf[Ed25519PrivateKey::LENGTH - 1] = 1;
        let private_key = Ed25519PrivateKey::try_from(&buf[..]).unwrap();

        Self {
            host: "localhost".to_string(),
            port: 9000,
            address: account_address::from_public_key(&private_key.public_key()),
            private_key,
        }
    }
}

pub fn parse(data: &str) -> Result<Config> {
    let toml_value = toml::from_str::<toml::Value>(data)?;
    match toml_value {
        toml::Value::Table(mut table) => {
            let _host_type = match table.remove("type") {
                Some(t) => match t {
                    toml::Value::String(s) => s,
                    _ => bail!("string expected for type field"),
                },
                None => "libra".to_string(),
            };

            match table.remove("conf") {
                None => bail!("no conf field"),
                Some(v) => v,
            };
        }
        _ => bail!("invalid format"),
    }
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    
    #[test]
    pub fn test_config_serde() -> Result<()> {
        let c = LibraHostConfig::default();
        // let c = HostConfig {
        //     name: "test".to_string(),
        //     conf: Config::Libra(c),
        // };

        println!("{}", toml::to_string(&c)?);
        Ok(())
    }
}
