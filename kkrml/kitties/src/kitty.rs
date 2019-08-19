use codec::{Decode, Encode};
use rand::{
	Rng,
	seq::SliceRandom,
	distributions::{Distribution, Uniform, Standard}
};
use crate::Trait;

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Eq, Clone, Copy)]
pub enum KittySex {
	Female, Male
}

impl Distribution<KittySex> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> KittySex {
        use self::KittySex::*;
		*[Female, Male].choose(rng).unwrap()
    }
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Eq, Clone, Copy)]
pub enum KittyElement {
	Natural,
	Metal,
	Wood,
	Water,
	Fire,
	Earth
}

impl Distribution<KittyElement> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> KittyElement {
        use self::KittyElement::*;
        *[
			Natural,
			Metal,
			Wood,
			Water,
			Fire,
			Earth
		].choose(rng).unwrap()
    }
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Eq, Clone, Copy)]
pub enum Rarity {
	Common,
	Rare,
	Epic,
}

impl Rarity {
	fn chance(&self) -> u8 {
		match self {
			Rarity::Common => 4,
			Rarity::Rare => 2,
			Rarity::Epic => 1
		}
	}

	fn base_value(&self) -> u8 {
		match self {
			Rarity::Common => 0,
			Rarity::Rare => 100,
			Rarity::Epic => 170,
		}
	}
}

#[cfg_attr(feature = "std", derive(Debug, PartialEq, Eq))]
#[derive(Encode, Decode)]
pub struct Kitty<T: Trait> {
	birth: T::Moment,
	generation: u8,
	appearance: [u8; 6],
	sex: KittySex,
	health: u8,
	attack: u8,
	defence: u8,
	stamina: u8,
	element: KittyElement,
}

// number of items in different rarity (common, rare, epic)
// be careful to make sure the counts never exceeds Rarity::base_value and overflow u8
const APPEARANCE_ITEM_COUNT: [(u8, u8, u8); 6] = [
	// accessories
	(16, 4, 0),
	// body
	(12, 3, 0),
	// eyes
	(12, 15, 0),
	// mouth
	(8, 2, 0),
	// fur
	(8, 2, 0),
	// zz
	(1, 1, 1),
];

fn generate_appearance(counts: &(u8, u8, u8), rng: &mut impl Rng) -> u8 {
	use Rarity::*;
	// be careful this should not overflow u8 otherise will need to upsize the type
	let common_max = counts.0 * Common.chance();
	let rare_max = counts.1 * Rare.chance();
	let epic_max = counts.2 * Epic.chance();
	let max = common_max + rare_max + epic_max;
	let value = rng.gen_range(0, max);
	if value < common_max {
		return Common.base_value() + value / Common.chance()
	}
	let value = value - common_max;
	if value < rare_max {
		return Rare.base_value() + value / Rare.chance()
	}
	let value = value - rare_max;
	Epic.base_value() + value / Epic.chance()
}

impl<T: Trait> Kitty<T> {
	pub fn new(rng: &mut impl Rng) -> Kitty<T> {
		let stats_range = Uniform::new(0, 5);
		let mut appearance = [0u8; 6];
		for i in 0..6 {
			appearance[i] = generate_appearance(&APPEARANCE_ITEM_COUNT[i], rng);
		}
		Kitty {
			birth: <timestamp::Module<T>>::now(),
			generation: 0,
			appearance,
			sex: rng.gen(),
			health: stats_range.sample(rng),
			attack: stats_range.sample(rng),
			defence: stats_range.sample(rng),
			stamina: stats_range.sample(rng),
			element: rng.gen(),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use rand_chacha::ChaCha8Rng;
	use rand::SeedableRng;
	use std::collections::HashMap;

	#[test]
	fn test_generate_appearance_probability() {
		let mut rng = ChaCha8Rng::seed_from_u64(100);
		let mut map = HashMap::<u8, u32>::new();
		// max = 3 * 4 + 2 * 2 + 1 = 17
		for _ in 0..17000 {
			let val = generate_appearance(&(3, 2, 1), &mut rng);
			match map.get_mut(&val) {
				Some(v) => *v += 1,
				None => {
					map.insert(val, 1);
				},
			};
		}
		assert_eq!(map.len(), 6);
		// common = 4
		let value = map.get(&0).unwrap();
		assert!((3800..4200).contains(value));
		let value = map.get(&1).unwrap();
		assert!((3800..4200).contains(value));
		let value = map.get(&2).unwrap();
		assert!((3800..4200).contains(value));
		// rare = 2
		let value = map.get(&100).unwrap();
		assert!((1900..2100).contains(value));
		let value = map.get(&101).unwrap();
		assert!((1900..2100).contains(value));
		// epic = 1
		let value = map.get(&170).unwrap();
		assert!((900..1100).contains(value));
	}
}
