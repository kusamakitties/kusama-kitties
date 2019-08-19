use crate::{KittiesAwakeTime, KittiesExp, Module, Trait};
use codec::{CompactAs, Decode, Encode};
use rand::{
	distributions::{Distribution, Standard, Uniform},
	seq::SliceRandom,
	Rng,
};
use sr_primitives::traits::IntegerSquareRoot;
use support::ensure;

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

// min level to breed
const BREED_LEVEL: u8 = 3;

#[cfg_attr(feature = "std", derive(Debug, Ord, PartialOrd))]
#[derive(Encode, Decode, CompactAs, Default, Copy, Clone, PartialEq, Eq)]
struct KittyIndex(u32);

impl KittyIndex {
	pub fn exp<T>(&self) -> u32 {
		Module::<T>::kitty_exp(self.0)
	}
	pub fn level<T>(&self) -> u8 {
		let exp = self.exp::<T>();
		(exp / 10).integer_sqrt()
	}
	pub fn kitty<T>(&self) -> Option<Kitty<T>> {
		Module::<T>::kitty(self.0)
	}
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Eq, Clone, Copy)]
pub enum KittySex {
	Female,
	Male,
}

impl Distribution<KittySex> for Standard {
	fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> KittySex {
		use self::KittySex::*;
		*[Female, Male].choose(rng).unwrap()
	}
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Eq, Clone, Copy)]
pub enum Element {
	Natural,
	Metal,
	Wood,
	Water,
	Fire,
	Earth,
}

impl Element {
	pub fn generating(&self) -> Option<Element> {
		use self::Element::*;
		match self {
			Natural => None,
			Metal => Some(Water),
			Wood => Some(Fire),
			Water => Some(Wood),
			Fire => Some(Earth),
			Earth => Some(Metal),
		}
	}

	pub fn overcoming(&self) -> Option<Element> {
		use self::Element::*;
		match self {
			Natural => None,
			Metal => Some(Wood),
			Wood => Some(Earth),
			Water => Some(Fire),
			Fire => Some(Metal),
			Earth => Some(Water),
		}
	}
}

impl Distribution<Element> for Standard {
	fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Element {
		use self::Element::*;
		*[Natural, Metal, Wood, Water, Fire, Earth].choose(rng).unwrap()
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
			Rarity::Epic => 1,
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
	element: Element,
	parents: Option<(KittyIndex, KittyIndex)>, // (father, mother)
}

fn generate_appearance(counts: &(u8, u8, u8), rng: &mut impl Rng) -> u8 {
	use Rarity::*;
	// be careful this should not overflow u8 otherise will need to upsize the type
	let common_max = counts.0 * Common.chance();
	let rare_max = counts.1 * Rare.chance();
	let epic_max = counts.2 * Epic.chance();
	let max = common_max + rare_max + epic_max;
	let value = rng.gen_range(0, max);
	if value < common_max {
		return Common.base_value() + value / Common.chance();
	}
	let value = value - common_max;
	if value < rare_max {
		return Rare.base_value() + value / Rare.chance();
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
			parents: None,
		}
	}

	pub fn from_parents(
		parent_id_1: KittyIndex,
		parent_id_2: KittyIndex,
		rng: &mut impl Rng,
	) -> Result<Kitty<T>, &'static str> {
		let parent1 = parent_id_1.kitty().ok_or("Invalid parent kitty 1")?;
		let parent2 = parent_id_2.kitty().ok_or("Invalid parent kitty 2")?;

		ensure!(
			parent1.generation == parent2.generation,
			"Both parents must be same generation"
		);
		ensure!(parent1.sex != parent2.sex, "Both parents must be different sex");

		let level1 = parent1.level();
		let level2 = parent2.level();

		ensure!(level1 >= BREED_LEVEL, "Parent level too low to breed");
		ensure!(level2 >= BREED_LEVEL, "Parent level too low to breed");

		let parents = if parent1.sex == KittySex::Male {
			(parent1, parent2)
		} else {
			(parent2, parent1)
		};

		let stats_range = Uniform::new(0, 5);
		let mut appearance = [0u8; 6];
		for i in 0..6 {
			appearance[i] = generate_appearance(&APPEARANCE_ITEM_COUNT[i], rng);
		}
		Ok(Kitty {
			birth: <timestamp::Module<T>>::now(),
			generation: parent1.generation.checked_add(1).ok_or("Generation overflow")?,
			appearance,
			sex: rng.gen(),
			health: stats_range.sample(rng),
			attack: stats_range.sample(rng),
			defence: stats_range.sample(rng),
			stamina: stats_range.sample(rng),
			element: rng.gen(),
			parents,
		})
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use rand::SeedableRng;
	use rand_chacha::ChaCha8Rng;
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
				}
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
