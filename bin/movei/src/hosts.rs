use anyhow::{bail, Result};
use libra_crypto::{ed25519::Ed25519PrivateKey, PrivateKey};
use libra_json_rpc::{JsonRpcAsyncClient, JsonRpcBatch, JsonRpcResponse};
use libra_types::{
    access_path::AccessPath,
    account_address::AccountAddress,
    account_state::AccountState,
    account_state_blob::AccountStateBlob,
    language_storage::TypeTag,
    on_chain_config::{OnChainConfig, VMConfig},
    transaction::{RawTransaction, Script, TransactionArgument},
    vm_error::{StatusCode, VMStatus},
};
use move_core_types::gas_schedule::CostTable;
use move_vm_state::data_cache::RemoteCache;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    convert::TryFrom,
    sync::{Mutex, RwLock},
    time::Duration,
};
use tokio::runtime::Runtime;
use vm::{errors::VMResult, transaction_metadata::TransactionMetadata};

#[derive(Debug, Deserialize, Serialize)]
pub struct Config {
    pub host: String,
    pub port: u16,
    pub address: AccountAddress,
    pub private_key: Ed25519PrivateKey,
}

impl Default for Config {
    fn default() -> Self {
        let mut buf = [0u8; Ed25519PrivateKey::LENGTH];
        buf[Ed25519PrivateKey::LENGTH - 1] = 1;
        let private_key = Ed25519PrivateKey::try_from(&buf[..]).unwrap();
        Self {
            host: "localhost".to_string(),
            port: 9000,
            address: AccountAddress::from_public_key(&private_key.public_key()),
            private_key,
        }
    }
}
pub struct LibraHost {
    pub config: Config,
    rpc_client: JsonRpcAsyncClient,
    state: LibraStateView,
    vm_config: VMConfig,
}

impl LibraHost {
    pub fn new(config: Config) -> Result<Self> {
        let client = JsonRpcAsyncClient::new(Client::new(), config.host.as_str(), config.port);
        let state = LibraStateView {
            rpc_client: client.clone(),
            cache: RwLock::new(BTreeMap::new()),
            rt: Mutex::new(Runtime::new().unwrap()),
        };

        let vm_config = match VMConfig::fetch_config(&state as &dyn RemoteCache) {
            None => bail!("no vm config found in libra host runner"),
            Some(c) => c,
        };
        let host = Self {
            config,
            rpc_client: client.clone(),
            state,
            vm_config,
        };

        Ok(host)
    }

    pub fn txn_meta(
        &self,
        script: &[u8],
        ty_args: &[TypeTag],
        args: &[TransactionArgument],
    ) -> TransactionMetadata {
        let txn = RawTransaction::new_script(
            self.config.address,
            0,
            Script::new(script.to_vec(), ty_args.to_vec(), args.to_vec()),
            100_000_000,
            1,
            Duration::from_secs(60),
        )
        .sign(
            &self.config.private_key,
            self.config.private_key.public_key(),
        )
        .unwrap();
        TransactionMetadata::new(&txn.into_inner())
    }

    pub fn gas_schedule(&self) -> &CostTable {
        &self.vm_config.gas_schedule
    }

    pub fn remote_cache(&self) -> &dyn RemoteCache {
        &self.state
    }
}

struct LibraStateView {
    rpc_client: JsonRpcAsyncClient,
    cache: RwLock<BTreeMap<AccountAddress, Option<AccountState>>>,
    rt: Mutex<Runtime>,
}

impl RemoteCache for LibraStateView {
    fn get(&self, access_path: &AccessPath) -> VMResult<Option<Vec<u8>>> {
        // query from cache first
        if let Some(s) = self.cache.read().unwrap().get(&access_path.address) {
            return Ok(s
                .as_ref()
                .and_then(|s| s.get(access_path.path.as_slice()).cloned()));
        }

        let mut batch = JsonRpcBatch::new();
        batch.add_get_account_state_with_proof_request(access_path.address, None, None);

        let response = self
            .rt
            .lock()
            .unwrap()
            .block_on(self.rpc_client.clone().execute(batch));

        let account_state = match parse_account_state(response) {
            Err(e) => {
                return Err(VMStatus::new(StatusCode::STORAGE_ERROR).with_message(format!("{}", e)))
            }
            Ok(d) => d,
        };
        let result = account_state
            .as_ref()
            .and_then(|s| s.get(access_path.path.as_slice()).cloned());
        // write to cache
        self.cache
            .write()
            .unwrap()
            .insert(access_path.address, account_state);
        return Ok(result);
    }
}

fn parse_account_state(
    batch_response: Result<Vec<Result<JsonRpcResponse>>>,
) -> Result<Option<AccountState>> {
    let resp = batch_response?.pop();
    let resp = match resp {
        None => bail!("invalid rpc response"),
        Some(d) => d?,
    };
    let blob = match resp {
        JsonRpcResponse::AccountStateWithProofResponse(view) => {
            view.blob.map(|v| v.into_bytes()).transpose()?
        }
        _ => bail!("invalid rpc response"),
    };
    let account_state = match blob {
        None => None,
        Some(s) => Some(AccountState::try_from(&AccountStateBlob::from(s))?),
    };
    Ok(account_state)
}
