use runtime_io::{blake2_256};
use rand_chacha::ChaCha8Rng;
use codec::Encode;
use crate::Trait;

pub fn random_seed<T: Trait, I: Encode>(input: I) -> [u8; 32] {
	// TODO: this randomness can be easily manipulated by block author
	// change extrinsic_index to extrinsic_data helps tiny bit but still won't be enough
	// will needs a random beacon chain to solve this issue
	let payload = (
		<system::Module<T>>::random_seed(),
		<system::Module<T>>::extrinsic_index(),
		input,
	);
	payload.using_encoded(blake2_256)
}

pub type Rng = ChaCha8Rng;
