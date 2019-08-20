use crate::{Module, Trait};
use codec::{CompactAs, Decode, Encode};
use rand::{
	distributions::{Distribution, Standard, Uniform},
	seq::SliceRandom,
	Rng,
};
use sr_primitives::traits::{IntegerSquareRoot, SaturatedConversion};
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
pub struct KittyIndex(u32);

impl KittyIndex {
	pub fn next_index(&self) -> Option<KittyIndex> {
		self.0.checked_add(1).map(Self)
	}
}

#[cfg(test)]
impl From<u32> for KittyIndex {
	fn from(id: u32) -> Self {
		Self(id)
	}
}

pub struct KittyDetails<T: Trait> {
	index: KittyIndex,
	kitty: Kitty<T>,
	exp: Option<u32>,
	level: Option<u8>,
}

impl<T: Trait> KittyDetails<T> {
	pub fn from(id: KittyIndex) -> Option<KittyDetails<T>> {
		if let Some(kitty) = Module::<T>::kitty(&id) {
			Some(KittyDetails {
				index: id,
				kitty,
				exp: None,
				level: None
			})
		} else {
			None
		}
	}

	pub fn index(&self) -> KittyIndex {
		self.index
	}
	pub fn kitty(&self) -> &Kitty<T> {
		&self.kitty
	}
	pub fn exp(&mut self) -> u32 {
		if let Some(exp) = self.exp {
			exp
		} else {
			let exp = Module::<T>::kitty_exp(&self.index);
			self.exp = Some(exp);
			exp
		}
	}
	pub fn level(&mut self) -> u8 {
		if let Some(level) = self.level {
			level
		} else {
			let level = (self.exp() / 10).integer_sqrt().saturated_into();
			self.level = Some(level);
			level
		}
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
	pub birth: T::Moment,
	pub generation: u8,
	pub appearance: [u8; 6],
	pub sex: KittySex,
	pub health: u8,
	pub attack: u8,
	pub defence: u8,
	pub stamina: u8,
	pub element: Element,
	pub parents: Option<(KittyIndex, KittyIndex)>, // (father, mother)
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
		parent1: &mut KittyDetails<T>,
		parent2: &mut KittyDetails<T>,
		rng: &mut impl Rng,
	) -> Result<Kitty<T>, &'static str> {
		ensure!(
			parent1.kitty().generation == parent2.kitty().generation,
			"Both parents must be same generation"
		);
		ensure!(parent1.kitty().sex != parent2.kitty().sex, "Both parents must be different sex");

		let level1 = parent1.level();
		let level2 = parent2.level();

		ensure!(level1 >= BREED_LEVEL, "Parent level too low to breed");
		ensure!(level2 >= BREED_LEVEL, "Parent level too low to breed");

		if let (Some((p1f, p1m)), Some((p2f, p2m))) = (parent1.kitty().parents, parent2.kitty().parents) {
			// not first generation, ensure not siblings
			ensure!(p1f != p2f, "Kitties have same father");
			ensure!(p1m != p2m, "Kitties have same mother");
		}

		let parents = if parent1.kitty().sex == KittySex::Male {
			(parent1.index(), parent2.index())
		} else {
			(parent2.index(), parent1.index())
		};

		let stats_range = Uniform::new(0, 5);
		let mut appearance = [0u8; 6];
		for i in 0..6 {
			appearance[i] = generate_appearance(&APPEARANCE_ITEM_COUNT[i], rng);
		}
		Ok(Kitty {
			birth: <timestamp::Module<T>>::now(),
			generation: parent1.kitty().generation.checked_add(1).ok_or("Generation overflow")?,
			appearance,
			sex: rng.gen(),
			health: stats_range.sample(rng),
			attack: stats_range.sample(rng),
			defence: stats_range.sample(rng),
			stamina: stats_range.sample(rng),
			element: rng.gen(),
			parents: Some(parents),
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
